#include <assert.h>
#include <stdio.h>
#include "sh4_interpreter.h"
#include "sh4_server.h"

static int load_bin(const char *bin_path, uint8_t **data, int *size) {
  FILE *fp = fopen(bin_path, "rb");
  if (!fp) {
    return 0;
  }

  fseek(fp, 0, SEEK_END);
  *size = ftell(fp);
  fseek(fp, 0, SEEK_SET);

  *data = (uint8_t *)(malloc(*size));
  int n = (int)fread(*data, sizeof(uint8_t), *size, fp);
  fclose(fp);

  if (n != *size) {
    free(*data);
    return 0;
  }

  return 1;
}

static void free_bin(uint8_t *data) {
  free(data);
}

int main(int argc, char **argv) {
  if (argc < 2) {
    printf("usage: example fibonacci.bin\n");
    return EXIT_FAILURE;
  }

  // load sh4 binary
  const char *bin_path = argv[1];
  uint8_t *bin = NULL;
  int bin_size = 0;

  if (!load_bin(bin_path, &bin, &bin_size)) {
    printf("failed to load %s\n", bin_path);
    return EXIT_FAILURE;
  }

  // create sh4 context
  sh4_context_t ctx;
  ctx.physical_base = bin;
  ctx.physical_mask = 0x1fffffff;
  ctx.physical_size = bin_size;
  ctx.pc = 0x0;
  ctx.pr = 0x0;
  ctx.sr = 0x700000f0;
  ctx.fpscr = 0x00040001;
  ctx.running = 1;
  sh4_init_tables();

  // create gdb server
  gdb_target_t target;
  sh4_server_init(&target, &ctx);

  gdb_server_t *sv = gdb_server_create(&target, 24690);
  if (!sv) {
    printf("Failed to create GDB server\n");
    return EXIT_FAILURE;
  }

  // main loop
  sh4_instr_t instr;

  while (1) {
    gdb_server_pump(sv);

    // spin, waiting for the debugger to resume if the cpu isn't running
    if (!ctx.running) {
      continue;
    }

    printf("PC 0x%x\n", ctx.pc);

    uint16_t data = *(uint16_t *)&bin[ctx.pc];

    // invalid opcode, breakpoint hit
    if (!data) {
      breakpoint_t *bp = sh4_server_find_bp(&ctx, ctx.pc);
      assert(bp);

      // auto remove temporary breakpoints
      if (bp->temp) {
        sh4_server_rem_bp(&ctx, bp);
      }

      // let the client know we've stopped
      gdb_server_interrupt(sv, GDB_SIGNAL_TRAP);
      ctx.running = 0;

      continue;
    }

    // valid opcode, decode and execute
    if (!sh4_decode(data, &instr)) {
      printf("Failed to decode 0x%x\n", data);
      return EXIT_FAILURE;
    }

    instr.type->fn(&ctx, &instr);

    // move onto the next instruction
    ctx.pc += 2;
  }

  // cleanup server
  gdb_server_destroy(sv);

  // cleanup sh4 binary
  free_bin(bin);

  return EXIT_SUCCESS;
}
