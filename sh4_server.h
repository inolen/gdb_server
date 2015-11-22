#ifndef SH4_SERVER_H
#define SH4_SERVER_H

#include "gdb_server.h"
#include "sh4_interpreter.h"

void sh4_server_init(gdb_target_t *target, sh4_context_t *ctx);
breakpoint_t *sh4_server_find_bp(void *data, intmax_t addr);
void sh4_server_rem_bp(void *data, breakpoint_t *bp);

#endif
