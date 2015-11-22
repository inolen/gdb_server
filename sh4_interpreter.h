#ifndef SH4_INTERPRETER_H
#define SH4_INTERPRETER_H

#include <stdint.h>

typedef struct {
  uint32_t addr;
  uint16_t data;
  int temp;
} breakpoint_t;

// clang-format off
#define NAMESPACE                  sh4
#define CONTEXT_EXTRA              uint8_t *physical_base; \
                                   uintptr_t physical_mask; \
                                   uintptr_t physical_size; \
                                   breakpoint_t *bp; \
                                   int num_bp;

#define I8                         int8_t
#define I16                        int16_t
#define I32                        int32_t
#define I64                        int64_t

#define DELAY_INSTR()              (ctx->delay = ctx->pc + 2)
#define NEXT_INSTR()               (ctx->pc += 2)

#define LOAD_REG_I8(n)             ((int8_t)ctx->r[n])
#define LOAD_REG_I16(n)            ((int16_t)ctx->r[n])
#define LOAD_REG_I32(n)            ((int32_t)ctx->r[n])
#define STORE_REG_I32(n, v)        (ctx->r[n] = (v))
#define STORE_REG_IMM_I32          STORE_REG_I32

#define LOAD_DBR_I32()             (ctx->dbr)
#define STORE_DBR_I32(v)           (ctx->dbr = v)
#define STORE_DBR_IMM_I32          STORE_DBR_I32

#define LOAD_GBR_I32()             (ctx->gbr)
#define STORE_GBR_I32(v)           (ctx->gbr = v)
#define STORE_GBR_IMM_I32          STORE_GBR_I32

#define LOAD_MACH_I32()            (ctx->mach)
#define STORE_MACH_I32(v)          (ctx->mach = v)
#define STORE_MACH_IMM_I32         STORE_MACH_I32

#define LOAD_MACL_I32()            (ctx->macl)
#define STORE_MACL_I32(v)          (ctx->macl = v)
#define STORE_MACL_IMM_I32         STORE_MACL_I32

#define LOAD_PR_I32()              (ctx->pr)
#define STORE_PR_I32(v)            (ctx->pr = v)
#define STORE_PR_IMM_I32           STORE_PR_I32

#define LOAD_SPC_I32()             (ctx->spc)
#define STORE_SPC_I32(v)           (ctx->spc = v)
#define STORE_SPC_IMM_I32          STORE_SPC_I32

#define LOAD_SGR_I32()             (ctx->sgr)
#define STORE_SGR_I32(v)           (ctx->sgr = v)
#define STORE_SGR_IMM_I32          STORE_SGR_I32

#define LOAD_SR_I32()              (ctx->sr)
#define STORE_SR_I32(v)            (ctx->sr = v)
#define STORE_SR_IMM_I32           STORE_SR_I32

#define LOAD_SSR_I32()             (ctx->ssr)
#define STORE_SSR_I32(v)           (ctx->ssr = v)
#define STORE_SSR_IMM_I32          STORE_SSR_I32

#define LOAD_S_I32()               (ctx->sr & S)
#define STORE_S_I32(v)             (ctx->sr = ((ctx->sr & ~S) | (v == 0 ? 0 : S)))
#define STORE_S_IMM_I32            STORE_S_I32

#define LOAD_T_I32()               (ctx->sr & T)
#define STORE_T_I32(v)             (ctx->sr = ((ctx->sr & ~T) | (v == 0 ? 0 : T)))
#define STORE_T_IMM_I32            STORE_T_I32

#define LOAD_QM_I32()              (ctx->qm)
#define STORE_QM_I32(v)            (ctx->qm = v)
#define STORE_QM_IMM_I32           STORE_QM_I32

#define LOAD_SPC_I32()             (ctx->spc)
#define STORE_SPC_I32(v)           (ctx->spc = v)
#define STORE_SPC_IMM_I32          STORE_SPC_I32

#define LOAD_VBR_I32()             (ctx->vbr)
#define STORE_VBR_I32(v)           (ctx->vbr = v)
#define STORE_VBR_IMM_I32          STORE_VBR_I32

#define LOAD_I8(addr)              (*(int8_t *)(ctx->physical_base + (addr & ctx->physical_mask)))
#define LOAD_I16(addr)             (*(int16_t *)(ctx->physical_base + (addr & ctx->physical_mask)))
#define LOAD_I32(addr)             (*(int32_t *)(ctx->physical_base + (addr & ctx->physical_mask)))
#define LOAD_I64(addr)             (*(int64_t *)(ctx->physical_base + (addr & ctx->physical_mask)))
#define STORE_I8(addr, v)          (*(int8_t *)(ctx->physical_base + (addr & ctx->physical_mask)) = (v))
#define STORE_I16(addr, v)         (*(int16_t *)(ctx->physical_base + (addr & ctx->physical_mask)) = (v))
#define STORE_I32(addr, v)         (*(int32_t *)(ctx->physical_base + (addr & ctx->physical_mask)) = (v))
#define STORE_I64(addr, v)         (*(int64_t *)(ctx->physical_base + (addr & ctx->physical_mask)) = (v))

#define SEXT_I8_I32(v)             ((int32_t)(int8_t)(v))
#define SEXT_I16_I32(v)            ((int32_t)(int16_t)(v))
#define SEXT_I32_I64(v)            ((int64_t)(int32_t)(v))

#define ZEXT_I8_I32(v)             ((uint32_t)(uint8_t)(v))
#define ZEXT_I16_I32(v)            ((uint32_t)(uint16_t)(v))
#define ZEXT_I32_I64(v)            ((uint64_t)(uint32_t)(v))

#define ADD_I8(a, b)               ((a) + (b))
#define ADD_I16                    ADD_I8
#define ADD_I32                    ADD_I8
#define ADD_I64                    ADD_I8
#define ADD_IMM_I8                 ADD_I8
#define ADD_IMM_I16                ADD_I8
#define ADD_IMM_I32                ADD_I8
#define ADD_IMM_I64                ADD_I8

#define SUB_I8(a, b)               ((a) - (b))
#define SUB_I16                    SUB_I8
#define SUB_I32                    SUB_I8
#define SUB_I64                    SUB_I8
#define SUB_IMM_I8                 SUB_I8
#define SUB_IMM_I16                SUB_I8
#define SUB_IMM_I32                SUB_I8
#define SUB_IMM_I64                SUB_I8

#define SMUL_I8(a, b)              ((int8_t)(a) * (int8_t)(b))
#define SMUL_I16(a, b)             ((int16_t)(a) * (int16_t)(b))
#define SMUL_I32(a, b)             ((int32_t)(a) * (int32_t)(b))
#define SMUL_I64(a, b)             ((int64_t)(a) * (int64_t)(b))
#define SMUL_IMM_I8                SMUL_I8
#define SMUL_IMM_I16               SMUL_I16
#define SMUL_IMM_I32               SMUL_I32
#define SMUL_IMM_I64               SMUL_I64

#define UMUL_I8(a, b)              ((uint8_t)(a) * (uint8_t)(b))
#define UMUL_I16(a, b)             ((uint16_t)(a) * (uint16_t)(b))
#define UMUL_I32(a, b)             ((uint32_t)(a) * (uint32_t)(b))
#define UMUL_I64(a, b)             ((uint64_t)(a) * (uint64_t)(b))
#define UMUL_IMM_I8                UMUL_I8
#define UMUL_IMM_I16               UMUL_I16
#define UMUL_IMM_I32               UMUL_I32
#define UMUL_IMM_I64               UMUL_I64

#define AND_I8(a, b)               ((a) & (b))
#define AND_I16                    AND_I8
#define AND_I32                    AND_I8
#define AND_I64                    AND_I8
#define AND_IMM_I8                 AND_I8
#define AND_IMM_I16                AND_I8
#define AND_IMM_I32                AND_I8
#define AND_IMM_I64                AND_I8

#define NEG_I8(a)                  (-(a))
#define NEG_I16                    NEG_I8
#define NEG_I32                    NEG_I8
#define NEG_I64                    NEG_I8

#define NOT_I8(a)                  (~(a))
#define NOT_I16                    NOT_I8
#define NOT_I32                    NOT_I8
#define NOT_I64                    NOT_I8

#define OR_I8(a, b)                ((a) | (b))
#define OR_I16                     OR_I8
#define OR_I32                     OR_I8
#define OR_I64                     OR_I8
#define OR_IMM_I8                  OR_I8
#define OR_IMM_I16                 OR_I8
#define OR_IMM_I32                 OR_I8
#define OR_IMM_I64                 OR_I8

#define XOR_I8(a, b)               ((a) ^ (b))
#define XOR_I16                    XOR_I8
#define XOR_I32                    XOR_I8
#define XOR_I64                    XOR_I8
#define XOR_IMM_I8                 XOR_I8
#define XOR_IMM_I16                XOR_I8
#define XOR_IMM_I32                XOR_I8
#define XOR_IMM_I64                XOR_I8

#define SHL_I8(v, n)               (v << n)
#define SHL_I16(v, n)              (v << n)
#define SHL_I32(v, n)              (v << n)
#define SHL_I64(v, n)              (v << n)
#define SHL_IMM_I8                 SHL_I8
#define SHL_IMM_I16                SHL_I16
#define SHL_IMM_I32                SHL_I32
#define SHL_IMM_I64                SHL_I64

#define ASHR_I8(v, n)              ((int8_t)v >> n)
#define ASHR_I16(v, n)             ((int16_t)v >> n)
#define ASHR_I32(v, n)             ((int32_t)v >> n)
#define ASHR_I64(v, n)             ((int64_t)v >> n)
#define ASHR_IMM_I8                ASHR_I8
#define ASHR_IMM_I16               ASHR_I16
#define ASHR_IMM_I32               ASHR_I32
#define ASHR_IMM_I64               ASHR_I64

#define LSHR_I8(v, n)              ((uint8_t)v >> n)
#define LSHR_I16(v, n)             ((uint16_t)v >> n)
#define LSHR_I32(v, n)             ((uint32_t)v >> n)
#define LSHR_I64(v, n)             ((uint64_t)v >> n)
#define LSHR_IMM_I8                LSHR_I8
#define LSHR_IMM_I16               LSHR_I16
#define LSHR_IMM_I32               LSHR_I32
#define LSHR_IMM_I64               LSHR_I64

#define ASHD_I32(v, n)             (((n) & 0x80000000) ? (((n) & 0x1f) ? ((v) >> -((n) & 0x1f)) : ((v) >> 31)) : ((v) << ((n) & 0x1f)))
#define LSHD_I32(v, n)             (((n) & 0x80000000) ? (((n) & 0x1f) ? ((v) >> -((n) & 0x1f)) : 0) : ((v) << ((n) & 0x1f)))

#define CMPEQ_I8(a, b)             ((a) == (b))
#define CMPEQ_I16                  CMPEQ_I8
#define CMPEQ_I32                  CMPEQ_I8
#define CMPEQ_I64                  CMPEQ_I8
#define CMPEQ_IMM_I8               CMPEQ_I8
#define CMPEQ_IMM_I16              CMPEQ_I8
#define CMPEQ_IMM_I32              CMPEQ_I8
#define CMPEQ_IMM_I64              CMPEQ_I8

#define CMPSLT_I32(a, b)           ((a) < (b))
#define CMPSLT_IMM_I32             CMPSLT_I32
#define CMPSLE_I32(a, b)           ((a) <= (b))
#define CMPSLE_IMM_I32             CMPSLE_I32
#define CMPSGT_I32(a, b)           ((a) > (b))
#define CMPSGT_IMM_I32             CMPSGT_I32
#define CMPSGE_I32(a, b)           ((a) >= (b))
#define CMPSGE_IMM_I32             CMPSGE_I32

#define CMPULT_I32(a, b)           ((uint32_t)(a) < (uint32_t)(b))
#define CMPULT_IMM_I32             CMPULT_I32
#define CMPULE_I32(a, b)           ((uint32_t)(a) <= (uint32_t)(b))
#define CMPULE_IMM_I32             CMPULE_I32
#define CMPUGT_I32(a, b)           ((uint32_t)(a) > (uint32_t)(b))
#define CMPUGT_IMM_I32             CMPUGT_I32
#define CMPUGE_I32(a, b)           ((uint32_t)(a) >= (uint32_t)(b))
#define CMPUGE_IMM_I32             CMPUGE_I32

#define SELECT_I8(c, a, b)         ((c) ? (a) : (b))
#define SELECT_I16                 SELECT_I8
#define SELECT_I32                 SELECT_I8
#define SELECT_I64                 SELECT_I8

#define TRUNCATE_I64_I32(a)        ((uint32_t)(a))

#define BRANCH_I32(d)              (ctx->pc = d)
#define BRANCH_IMM_I32             BRANCH_I32
#define BRANCH_TRUE_IMM_I32(c, d)  if (c) { ctx->pc = d; } else { NEXT_INSTR(); }
#define BRANCH_FALSE_IMM_I32(c, d) if (!c) { ctx->pc = d; } else { NEXT_INSTR(); }
// clang-format on

#include "sh4.h"

#endif
