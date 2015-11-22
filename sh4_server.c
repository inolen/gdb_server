#define GDB_SERVER_IMPL
#include "gdb_server.h"

#include "sh4_server.h"

static breakpoint_t *add_bp(sh4_context_t *ctx, uint32_t addr) {
  int n = ctx->num_bp;
  addr &= ctx->physical_mask;

  // resize the breakpoints array
  ctx->bp = realloc(ctx->bp, sizeof(breakpoint_t) * ++ctx->num_bp);

  // setup the new breakpoint
  breakpoint_t *bp = &ctx->bp[n];
  bp->addr = addr;
  bp->data = *(uint16_t *)&ctx->physical_base[addr];
  bp->temp = 0;

  // write out invalid opcode to the program
  *(uint16_t *)&ctx->physical_base[addr] = 0;

  return bp;
}

static breakpoint_t *add_temp_bp(sh4_context_t *ctx, uint32_t addr) {
  breakpoint_t *bp = add_bp(ctx, addr);
  bp->temp = 1;
  return bp;
}

static breakpoint_t *find_bp(sh4_context_t *ctx, uint32_t addr) {
  for (int i = 0; i < ctx->num_bp; i++) {
    breakpoint_t *bp = &ctx->bp[i];

    if (bp->addr == addr) {
      return bp;
    }
  }

  return NULL;
}

static void rem_bp(sh4_context_t *ctx, breakpoint_t *bp) {
  int n = bp - ctx->bp;
  uint32_t addr = bp->addr & ctx->physical_mask;
  uint16_t data = bp->data;

  // shift and resize the breakpoints array
  for (int i = n; i < ctx->num_bp - 1; i++) {
    ctx->bp[i] = ctx->bp[i + 1];
  }
  ctx->bp = realloc(ctx->bp, sizeof(breakpoint_t) * --ctx->num_bp);

  // swap the original instructon back
  *(uint16_t *)&ctx->physical_base[addr] = data;
}

//
// sh4 target interface
//
static uint32_t server_next_pc(sh4_context_t *ctx) {
  sh4_context_t copy;
  memcpy(&copy, ctx, sizeof(copy));

  uint32_t addr = ctx->pc & ctx->physical_mask;
  uint16_t data = *(uint16_t *)&ctx->physical_base[addr];
  sh4_instr_t instr;
  assert(sh4_decode(data, &instr));

  // if the current instruction is a branch, simulate it to get the next pc.
  // this is assuming the branch instructions won't modify any other global
  // state
  if (instr.type->flags & FLAG_BRANCH) {
    instr.type->fn(&copy, &instr);
  }

  return copy.pc + 2;
}

static void server_stop(void *data) {
  sh4_context_t *ctx = (sh4_context_t *)data;
  ctx->running = 0;
}

static void server_resume(void *data) {
  sh4_context_t *ctx = (sh4_context_t *)data;
  ctx->running = 1;
}

static void server_step(void *data) {
  sh4_context_t *ctx = (sh4_context_t *)data;
  ctx->running = 1;

  uint32_t target = server_next_pc(ctx);
  add_temp_bp(ctx, target);
}

static void server_add_bp(void *data, int type, intmax_t addr) {
  sh4_context_t *ctx = (sh4_context_t *)data;
  add_bp(ctx, addr);
}

static void server_rem_bp(void *data, int type, intmax_t addr) {
  sh4_context_t *ctx = (sh4_context_t *)data;
  breakpoint_t *bp = find_bp(ctx, addr);
  assert(bp);
  rem_bp(ctx, bp);
}

static void server_read_mem(void *data, intmax_t addr, uint8_t *buffer, int size) {
  sh4_context_t *ctx = (sh4_context_t *)data;

  addr &= ctx->physical_mask;

  int available = ctx->physical_size - addr;
  if (available < 0) {
    available = 0;
  }

  if (size > available) {
    size = available;
  }

  memcpy(buffer, &ctx->physical_base[addr], size);
}

static void server_read_reg(void *data, int n, intmax_t *value, int *size) {
  sh4_context_t *ctx = (sh4_context_t *)data;

  if (n < 16) {
    *value = ctx->r[n];
  } else if (n == 16) {
    *value = ctx->pc;
  } else if (n == 17) {
    *value = ctx->pr;
  } else if (n == 18) {
    *value = ctx->gbr;
  } else if (n == 19) {
    *value = ctx->vbr;
  } else if (n == 20) {
    *value = ctx->mach;
  } else if (n == 21) {
    *value = ctx->macl;
  } else if (n == 22) {
    *value = ctx->sr;
  } else if (n == 23) {
    *value = ctx->fpul;
  } else if (n == 24) {
    *value = ctx->fpscr;
  } else if (n < 41) {
    *value = ctx->fr[n - 25];
  } else if (n == 41) {
    *value = ctx->ssr;
  } else if (n == 42) {
    *value = ctx->spc;
  } else if (n < 51) {
    uint32_t *b0 = (ctx->sr & RB) ? ctx->ralt : ctx->r;
    *value = b0[n - 43];
  } else if (n < 59) {
    uint32_t *b1 = (ctx->sr & RB) ? ctx->r : ctx->ralt;
    *value = b1[n - 51];
  }

  *size = 4;
}

void sh4_server_init(gdb_target_t *target, sh4_context_t *ctx) {
  target->ctx = ctx;
  target->endian = GDB_LITTLE_ENDIAN;
  target->num_regs = 59;
  target->stop = &server_stop;
  target->resume = &server_resume;
  target->step = &server_step;
  target->add_bp = &server_add_bp;
  target->rem_bp = &server_rem_bp;
  target->read_reg = &server_read_reg;
  target->read_mem = &server_read_mem;
}

breakpoint_t *sh4_server_find_bp(void *data, intmax_t addr) {
  sh4_context_t *ctx = (sh4_context_t *)data;
  return find_bp(ctx, addr);
}

void sh4_server_rem_bp(void *data, breakpoint_t *bp) {
  sh4_context_t *ctx = (sh4_context_t *)data;
  rem_bp(ctx, bp);
}
