#ifndef SH4_IMPL

#ifndef SH4_H
#define SH4_H

#include <stdint.h>

#ifndef SH4_NAMESPACE
#endif

// clang-format off
#define NAMESPACIFY_PASTE(n, v) n##_##v
#define NAMESPACIFY_EVAL(n, v)  NAMESPACIFY_PASTE(n, v)
#define NAMESPACIFY(v)          NAMESPACIFY_EVAL(NAMESPACE, v)

// types
#define OP_T                    NAMESPACIFY(op_t)
#define OPFLAG_T                NAMESPACIFY(opflag_t)
#define FN                      NAMESPACIFY(fn)
#define CONTEXT_S               NAMESPACIFY(context_s)
#define CONTEXT_T               NAMESPACIFY(context_t)
#define INSTR_TYPE_S            NAMESPACIFY(instr_type_s)
#define INSTR_TYPE_T            NAMESPACIFY(instr_type_t)
#define INSTR_S                 NAMESPACIFY(instr_s)
#define INSTR_T                 NAMESPACIFY(instr_t)

// functions
#define ARG_MASK                NAMESPACIFY(arg_mask)
#define INIT_TABLES             NAMESPACIFY(init_tables)
#define DECODE                  NAMESPACIFY(decode)
// clang-format on

// SR flags
#define T 0x00000001
#define S 0x00000002
#define I 0x000000f0
#define Q 0x00000100
#define M 0x00000200
#define FD 0x00008000
#define BL 0x10000000
#define RB 0x20000000
#define MD 0x40000000

typedef enum {
  OP_MOVI,
  OP_MOVWLPC,
  OP_MOVLLPC,
  OP_MOV,
  OP_MOVBS,
  OP_MOVWS,
  OP_MOVLS,
  OP_MOVBL,
  OP_MOVWL,
  OP_MOVLL,
  OP_MOVBM,
  OP_MOVWM,
  OP_MOVLM,
  OP_MOVBP,
  OP_MOVWP,
  OP_MOVLP,
  OP_MOVBS0D,
  OP_MOVWS0D,
  OP_MOVLSMD,
  OP_MOVBLD0,
  OP_MOVWLD0,
  OP_MOVLLDN,
  OP_MOVBS0,
  OP_MOVWS0,
  OP_MOVLS0,
  OP_MOVBL0,
  OP_MOVWL0,
  OP_MOVLL0,
  OP_MOVBS0G,
  OP_MOVWS0G,
  OP_MOVLS0G,
  OP_MOVBLG0,
  OP_MOVWLG0,
  OP_MOVLLG0,
  OP_MOVA,
  OP_MOVT,
  OP_SWAPB,
  OP_SWAPW,
  OP_XTRCT,
  OP_ADD,
  OP_ADDI,
  OP_ADDC,
  OP_ADDV,
  OP_CMPEQI,
  OP_CMPEQ,
  OP_CMPHS,
  OP_CMPGE,
  OP_CMPHI,
  OP_CMPGT,
  OP_CMPPZ,
  OP_CMPPL,
  OP_CMPSTR,
  OP_DIV0S,
  OP_DIV0U,
  OP_DIV1,
  OP_DMULS,
  OP_DMULU,
  OP_DT,
  OP_EXTSB,
  OP_EXTSW,
  OP_EXTUB,
  OP_EXTUW,
  OP_MACL,
  OP_MACW,
  OP_MULL,
  OP_MULS,
  OP_MULU,
  OP_NEG,
  OP_NEGC,
  OP_SUB,
  OP_SUBC,
  OP_SUBV,
  OP_AND,
  OP_ANDI,
  OP_ANDB,
  OP_NOT,
  OP_OR,
  OP_ORI,
  OP_ORB,
  OP_TAS,
  OP_TST,
  OP_TSTI,
  OP_TSTB,
  OP_XOR,
  OP_XORI,
  OP_XORB,
  OP_ROTL,
  OP_ROTR,
  OP_ROTCL,
  OP_ROTCR,
  OP_SHAD,
  OP_SHAL,
  OP_SHAR,
  OP_SHLD,
  OP_SHLL,
  OP_SHLR,
  OP_SHLL2,
  OP_SHLR2,
  OP_SHLL8,
  OP_SHLR8,
  OP_SHLL16,
  OP_SHLR16,
  OP_BF,
  OP_BFS,
  OP_BT,
  OP_BTS,
  OP_BRA,
  OP_BRAF,
  OP_BSR,
  OP_BSRF,
  OP_JMP,
  OP_JSR,
  OP_RTS,
  OP_CLRMAC,
  OP_CLRS,
  OP_CLRT,
  OP_LDCSR,
  OP_LDCGBR,
  OP_LDCVBR,
  OP_LDCSSR,
  OP_LDCSPC,
  OP_LDCDBR,
  OP_LDCRBANK,
  OP_LDCMSR,
  OP_LDCMGBR,
  OP_LDCMVBR,
  OP_LDCMSSR,
  OP_LDCMSPC,
  OP_LDCMDBR,
  OP_LDCMRBANK,
  OP_LDSMACH,
  OP_LDSMACL,
  OP_LDSPR,
  OP_LDSMMACH,
  OP_LDSMMACL,
  OP_LDSMPR,
  OP_MOVCAL,
  OP_NOP,
  OP_OCBI,
  OP_OCBP,
  OP_OCBWB,
  OP_PREF,
  OP_RTE,
  OP_SETS,
  OP_SETT,
  OP_SLEEP,
  OP_STCSR,
  OP_STCGBR,
  OP_STCVBR,
  OP_STCSSR,
  OP_STCSPC,
  OP_STCSGR,
  OP_STCDBR,
  OP_STCRBANK,
  OP_STCMSR,
  OP_STCMGBR,
  OP_STCMVBR,
  OP_STCMSSR,
  OP_STCMSPC,
  OP_STCMSGR,
  OP_STCMDBR,
  OP_STCMRBANK,
  OP_STSMACH,
  OP_STSMACL,
  OP_STSPR,
  OP_STSMMACH,
  OP_STSMMACL,
  OP_STSMPR,
  OP_TRAPA,
  NUM_OPCODES,
} OP_T;

typedef enum {
  FLAG_DELAYED = 0x1,
  FLAG_BRANCH = 0x2,
} OPFLAG_T;

struct CONTEXT_S;
struct INSTR_S;
typedef void (*FN)(struct CONTEXT_S *ctx, struct INSTR_S *i);

typedef struct CONTEXT_S {
  uint32_t delay, pc, spc;
  uint32_t pr;
  uint32_t gbr, vbr;
  uint32_t mach, macl;
  uint32_t r[16], ralt[8], sgr;
  uint32_t fr[16], xf[16];
  uint32_t fpul;
  uint32_t dbr;
  uint32_t sq[2][8];
  uint32_t sr, ssr;
  uint32_t qm;
  uint32_t fpscr;
  uint32_t running;
  CONTEXT_EXTRA
} CONTEXT_T;

typedef struct INSTR_TYPE_S {
  OP_T op;
  OPFLAG_T flags;
  const char *sig;
  FN fn;
  uint16_t imm_mask, imm_shift;
  uint16_t disp_mask, disp_shift;
  uint16_t rm_mask, rm_shift;
  uint16_t rn_mask, rn_shift;
  uint16_t opcode_mask;
} INSTR_TYPE_T;

typedef struct INSTR_S {
  INSTR_TYPE_T *type;
  uint16_t opcode;
  uint16_t rm;
  uint16_t rn;
  uint16_t disp;
  uint16_t imm;
} INSTR_T;

extern void INIT_TABLES();
extern int DECODE(uint16_t data, INSTR_T *instr);

#endif // #ifndef SH4_H

#else // #ifndef SH4_IMPL

#undef SH4_IMPL

#include <assert.h>
#include <string.h>
#include "sh4.h"

//
// instruction callbacks
//

#define INSTR(name) static void NAMESPACIFY(name)(CONTEXT_T * ctx, INSTR_T * i)

// MOV     #imm,Rn
INSTR(MOVI) {
  I32 v = SEXT_I8_I32(i->imm);
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// MOV.W   @(disp,PC),Rn
INSTR(MOVWLPC) {
  uint32_t addr = (i->disp * 2) + ctx->pc + 4;
  I32 v = SEXT_I16_I32(LOAD_I16(addr));
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// MOV.L   @(disp,PC),Rn
INSTR(MOVLLPC) {
  uint32_t addr = (i->disp * 4) + (ctx->pc & ~3) + 4;
  I32 v = LOAD_I32(addr);
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// MOV     Rm,Rn
INSTR(MOV) {
  I32 v = LOAD_REG_I32(i->rm);
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// MOV.B   Rm,@Rn
INSTR(MOVBS) {
  I32 addr = LOAD_REG_I32(i->rn);
  I8 v = LOAD_REG_I8(i->rm);
  STORE_I8(addr, v);
  NEXT_INSTR();
}

// MOV.W   Rm,@Rn
INSTR(MOVWS) {
  I32 addr = LOAD_REG_I32(i->rn);
  I16 v = LOAD_REG_I16(i->rm);
  STORE_I16(addr, v);
  NEXT_INSTR();
}

// MOV.L   Rm,@Rn
INSTR(MOVLS) {
  I32 addr = LOAD_REG_I32(i->rn);
  I32 v = LOAD_REG_I32(i->rm);
  STORE_I32(addr, v);
  NEXT_INSTR();
}

// MOV.B   @Rm,Rn
INSTR(MOVBL) {
  I32 v = SEXT_I8_I32(LOAD_I8(LOAD_REG_I32(i->rm)));
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// MOV.W   @Rm,Rn
INSTR(MOVWL) {
  I32 v = SEXT_I16_I32(LOAD_I16(LOAD_REG_I32(i->rm)));
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// MOV.L   @Rm,Rn
INSTR(MOVLL) {
  I32 v = LOAD_I32(LOAD_REG_I32(i->rm));
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// MOV.B   Rm,@-Rn
INSTR(MOVBM) {
  // decrease Rn by 1
  I32 addr = LOAD_REG_I32(i->rn);
  addr = SUB_IMM_I32(addr, 1);
  STORE_REG_I32(i->rn, addr);

  // store Rm at (Rn)
  I8 v = LOAD_REG_I8(i->rm);
  STORE_I8(addr, v);
  NEXT_INSTR();
}

// MOV.W   Rm,@-Rn
INSTR(MOVWM) {
  // decrease Rn by 2
  I32 addr = LOAD_REG_I32(i->rn);
  addr = SUB_IMM_I32(addr, 2);
  STORE_REG_I32(i->rn, addr);

  // store Rm at (Rn)
  I16 v = LOAD_REG_I16(i->rm);
  STORE_I16(addr, v);
  NEXT_INSTR();
}

// MOV.L   Rm,@-Rn
INSTR(MOVLM) {
  // decrease Rn by 4
  I32 addr = LOAD_REG_I32(i->rn);
  addr = SUB_IMM_I32(addr, 4);
  STORE_REG_I32(i->rn, addr);

  // store Rm at (Rn)
  I32 v = LOAD_REG_I32(i->rm);
  STORE_I32(addr, v);
  NEXT_INSTR();
}

// MOV.B   @Rm+,Rn
INSTR(MOVBP) {
  // store (Rm) at Rn
  I32 addr = LOAD_REG_I32(i->rm);
  I32 v = SEXT_I8_I32(LOAD_I8(addr));
  STORE_REG_I32(i->rn, v);

  // increase Rm by 1
  // FIXME if rm != rn???
  addr = ADD_IMM_I32(addr, 1);
  STORE_REG_I32(i->rm, addr);
  NEXT_INSTR();
}

// MOV.W   @Rm+,Rn
INSTR(MOVWP) {
  // store (Rm) at Rn
  I32 addr = LOAD_REG_I32(i->rm);
  I32 v = SEXT_I16_I32(LOAD_I16(addr));
  STORE_REG_I32(i->rn, v);

  // increase Rm by 2
  // FIXME if rm != rn???
  addr = ADD_IMM_I32(addr, 2);
  STORE_REG_I32(i->rm, addr);
  NEXT_INSTR();
}

// MOV.L   @Rm+,Rn
INSTR(MOVLP) {
  // store (Rm) at Rn
  I32 addr = LOAD_REG_I32(i->rm);
  I32 v = LOAD_I32(addr);
  STORE_REG_I32(i->rn, v);

  // increase Rm by 2
  // FIXME if rm != rn???
  addr = ADD_IMM_I32(addr, 4);
  STORE_REG_I32(i->rm, addr);
  NEXT_INSTR();
}

// MOV.B   R0,@(disp,Rn)
INSTR(MOVBS0D) {
  I32 addr = ADD_IMM_I32(LOAD_REG_I32(i->rn), (int32_t)i->disp);
  I8 v = LOAD_REG_I8(0);
  STORE_I8(addr, v);
  NEXT_INSTR();
}

// MOV.W   R0,@(disp,Rn)
INSTR(MOVWS0D) {
  I32 addr = ADD_IMM_I32(LOAD_REG_I32(i->rn), (int32_t)i->disp * 2);
  I16 v = LOAD_REG_I16(0);
  STORE_I16(addr, v);
  NEXT_INSTR();
}

// MOV.L Rm,@(disp,Rn)
INSTR(MOVLSMD) {
  I32 addr = ADD_IMM_I32(LOAD_REG_I32(i->rn), (int32_t)i->disp * 4);
  I32 v = LOAD_REG_I32(i->rm);
  STORE_I32(addr, v);
  NEXT_INSTR();
}

// MOV.B   @(disp,Rm),R0
INSTR(MOVBLD0) {
  I32 addr = ADD_IMM_I32(LOAD_REG_I32(i->rm), (int32_t)i->disp);
  I32 v = SEXT_I8_I32(LOAD_I8(addr));
  STORE_REG_I32(0, v);
  NEXT_INSTR();
}

// MOV.W   @(disp,Rm),R0
INSTR(MOVWLD0) {
  I32 addr = ADD_IMM_I32(LOAD_REG_I32(i->rm), (int32_t)i->disp * 2);
  I32 v = SEXT_I16_I32(LOAD_I16(addr));
  STORE_REG_I32(0, v);
  NEXT_INSTR();
}

// MOV.L   @(disp,Rm),Rn
INSTR(MOVLLDN) {
  I32 addr = ADD_IMM_I32(LOAD_REG_I32(i->rm), (int32_t)i->disp * 4);
  I32 v = LOAD_I32(addr);
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// MOV.B   Rm,@(R0,Rn)
INSTR(MOVBS0) {
  I32 addr = ADD_I32(LOAD_REG_I32(0), LOAD_REG_I32(i->rn));
  I8 v = LOAD_REG_I8(i->rm);
  STORE_I8(addr, v);
  NEXT_INSTR();
}

// MOV.W   Rm,@(R0,Rn)
INSTR(MOVWS0) {
  I32 addr = ADD_I32(LOAD_REG_I32(0), LOAD_REG_I32(i->rn));
  I16 v = LOAD_REG_I16(i->rm);
  STORE_I16(addr, v);
  NEXT_INSTR();
}

// MOV.L   Rm,@(R0,Rn)
INSTR(MOVLS0) {
  I32 addr = ADD_I32(LOAD_REG_I32(0), LOAD_REG_I32(i->rn));
  I32 v = LOAD_REG_I32(i->rm);
  STORE_I32(addr, v);
  NEXT_INSTR();
}

// MOV.B   @(R0,Rm),Rn
INSTR(MOVBL0) {
  I32 addr = ADD_I32(LOAD_REG_I32(0), LOAD_REG_I32(i->rm));
  I32 v = SEXT_I8_I32(LOAD_I8(addr));
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// MOV.W   @(R0,Rm),Rn
INSTR(MOVWL0) {
  I32 addr = ADD_I32(LOAD_REG_I32(0), LOAD_REG_I32(i->rm));
  I32 v = SEXT_I16_I32(LOAD_I16(addr));
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// MOV.L   @(R0,Rm),Rn
INSTR(MOVLL0) {
  I32 addr = ADD_I32(LOAD_REG_I32(0), LOAD_REG_I32(i->rm));
  I32 v = LOAD_I32(addr);
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// MOV.B   R0,@(disp,GBR)
INSTR(MOVBS0G) {
  I32 addr = ADD_IMM_I32(LOAD_GBR_I32(), (int32_t)i->disp);
  I8 v = LOAD_REG_I8(0);
  STORE_I8(addr, v);
  NEXT_INSTR();
}

// MOV.W   R0,@(disp,GBR)
INSTR(MOVWS0G) {
  I32 addr = ADD_IMM_I32(LOAD_GBR_I32(), (int32_t)i->disp * 2);
  I16 v = LOAD_REG_I16(0);
  STORE_I16(addr, v);
  NEXT_INSTR();
}

// MOV.L   R0,@(disp,GBR)
INSTR(MOVLS0G) {
  I32 addr = ADD_IMM_I32(LOAD_GBR_I32(), (int32_t)i->disp * 4);
  I32 v = LOAD_REG_I32(0);
  STORE_I32(addr, v);
  NEXT_INSTR();
}

// MOV.B   @(disp,GBR),R0
INSTR(MOVBLG0) {
  I32 addr = ADD_IMM_I32(LOAD_GBR_I32(), (int32_t)i->disp);
  I32 v = SEXT_I8_I32(LOAD_I8(addr));
  STORE_REG_I32(0, v);
  NEXT_INSTR();
}

// MOV.W   @(disp,GBR),R0
INSTR(MOVWLG0) {
  I32 addr = ADD_IMM_I32(LOAD_GBR_I32(), (int32_t)i->disp * 2);
  I32 v = SEXT_I16_I32(LOAD_I16(addr));
  STORE_REG_I32(0, v);
  NEXT_INSTR();
}

// MOV.L   @(disp,GBR),R0
INSTR(MOVLLG0) {
  I32 addr = ADD_IMM_I32(LOAD_GBR_I32(), (int32_t)i->disp * 4);
  I32 v = LOAD_I32(addr);
  STORE_REG_I32(0, v);
  NEXT_INSTR();
}

// MOVA    (disp,PC),R0
INSTR(MOVA) {
  uint32_t addr = (i->disp * 4) + (ctx->pc & ~3) + 4;
  STORE_REG_IMM_I32(0, addr);
  NEXT_INSTR();
}

// MOVT    Rn
INSTR(MOVT) {
  I32 v = LOAD_T_I32();
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// SWAP.B  Rm,Rn
INSTR(SWAPB) {
  const int nbits = 8;
  const uint32_t mask = (1u << nbits) - 1;
  I32 v = LOAD_REG_I32(i->rm);
  I32 tmp = AND_IMM_I32(XOR_I32(v, LSHR_I32(v, nbits)), mask);
  I32 res = XOR_I32(v, OR_I32(tmp, SHL_I32(tmp, nbits)));
  STORE_REG_I32(i->rn, res);
  NEXT_INSTR();
}

// SWAP.W  Rm,Rn
INSTR(SWAPW) {
  const int nbits = 16;
  const uint32_t mask = (1u << nbits) - 1;
  I32 v = LOAD_REG_I32(i->rm);
  I32 tmp = AND_IMM_I32(XOR_I32(v, LSHR_I32(v, nbits)), mask);
  I32 res = XOR_I32(v, OR_I32(tmp, SHL_I32(tmp, nbits)));
  STORE_REG_I32(i->rn, res);
  NEXT_INSTR();
}

// XTRCT   Rm,Rn
INSTR(XTRCT) {
  I32 rm = SHL_IMM_I32(AND_IMM_I32(LOAD_REG_I32(i->rm), 0x0000ffff), 16);
  I32 rn = LSHR_IMM_I32(AND_IMM_I32(LOAD_REG_I32(i->rn), 0xffff0000), 16);
  I32 v = OR_I32(rm, rn);
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 0011 nnnn mmmm 1100  1       -
// ADD     Rm,Rn
INSTR(ADD) {
  I32 v = ADD_I32(LOAD_REG_I32(i->rn), LOAD_REG_I32(i->rm));
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 0111 nnnn iiii iiii  1       -
// ADD     #imm,Rn
INSTR(ADDI) {
  I32 rn = LOAD_REG_I32(i->rn);
  int32_t imm = (int32_t)(int8_t)i->imm;
  I32 v = ADD_IMM_I32(rn, imm);
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 0011 nnnn mmmm 1110  1       carry
// ADDC    Rm,Rn
INSTR(ADDC) {
  I32 rm = LOAD_REG_I32(i->rm);
  I32 rn = LOAD_REG_I32(i->rn);
  I32 v = ADD_I32(ADD_I32(rn, rm), LOAD_T_I32());
  STORE_REG_I32(i->rn, v);

  // compute carry flag, taken from Hacker's Delight
  I32 c = OR_I32(AND_I32(rn, rm), AND_I32(OR_I32(rn, rm), NOT_I32(v)));
  STORE_T_I32(c);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 0011 nnnn mmmm 1111  1       overflow
// ADDV    Rm,Rn
INSTR(ADDV) {
  I32 rm = LOAD_REG_I32(i->rm);
  I32 rn = LOAD_REG_I32(i->rn);
  I32 v = ADD_I32(rn, rm);
  STORE_REG_I32(i->rn, v);

  // compute overflow flag, taken from Hacker's Delight
  I32 c = LSHR_IMM_I32(AND_I32(XOR_I32(v, rn), XOR_I32(v, rm)), 31);
  STORE_T_I32(c);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 1000 1000 iiii iiii  1       comparison result
// CMP/EQ #imm,R0
INSTR(CMPEQI) {
  int32_t imm = (int32_t)(int8_t)i->imm;
  I32 r0 = LOAD_REG_I32(0);
  I32 eq = CMPEQ_IMM_I32(r0, imm);
  STORE_T_I32(eq);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 0011 nnnn mmmm 0000  1       comparison result
// CMP/EQ  Rm,Rn
INSTR(CMPEQ) {
  I32 rm = LOAD_REG_I32(i->rm);
  I32 rn = LOAD_REG_I32(i->rn);
  I32 eq = CMPEQ_I32(rn, rm);
  STORE_T_I32(eq);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 0011 nnnn mmmm 0010  1       comparison result
// CMP/HS  Rm,Rn
INSTR(CMPHS) {
  I32 rm = LOAD_REG_I32(i->rm);
  I32 rn = LOAD_REG_I32(i->rn);
  I32 r = CMPUGE_I32(rn, rm);
  STORE_T_I32(r);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 0011 nnnn mmmm 0011  1       comparison result
// CMP/GE  Rm,Rn
INSTR(CMPGE) {
  I32 rm = LOAD_REG_I32(i->rm);
  I32 rn = LOAD_REG_I32(i->rn);
  I32 r = CMPSGE_I32(rn, rm);
  STORE_T_I32(r);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 0011 nnnn mmmm 0110  1       comparison result
// CMP/HI  Rm,Rn
INSTR(CMPHI) {
  I32 rm = LOAD_REG_I32(i->rm);
  I32 rn = LOAD_REG_I32(i->rn);
  I32 r = CMPUGT_I32(rn, rm);
  STORE_T_I32(r);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 0011 nnnn mmmm 0111  1       comparison result
// CMP/GT  Rm,Rn
INSTR(CMPGT) {
  I32 rm = LOAD_REG_I32(i->rm);
  I32 rn = LOAD_REG_I32(i->rn);
  I32 r = CMPSGT_I32(rn, rm);
  STORE_T_I32(r);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 0100 nnnn 0001 0001  1       comparison result
// CMP/PZ  Rn
INSTR(CMPPZ) {
  I32 rn = LOAD_REG_I32(i->rn);
  I32 r = CMPSGE_IMM_I32(rn, 0);
  STORE_T_I32(r);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 0100 nnnn 0001 0101  1       comparison result
// CMP/PL  Rn
INSTR(CMPPL) {
  I32 rn = LOAD_REG_I32(i->rn);
  I32 r = CMPSGT_IMM_I32(rn, 0);
  STORE_T_I32(r);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 0010 nnnn mmmm 1100  1       comparison result
// CMP/STR  Rm,Rn
INSTR(CMPSTR) {
  // if any diff is zero, the bytes match
  I32 rm = LOAD_REG_I32(i->rm);
  I32 rn = LOAD_REG_I32(i->rn);
  I32 diff = XOR_I32(rn, rm);
  I32 b4_eq = CMPEQ_IMM_I32(AND_I32(diff, 0xff000000), 0);
  I32 b3_eq = CMPEQ_IMM_I32(AND_I32(diff, 0x00ff0000), 0);
  I32 b2_eq = CMPEQ_IMM_I32(AND_I32(diff, 0x0000ff00), 0);
  I32 b1_eq = CMPEQ_IMM_I32(AND_I32(diff, 0x000000ff), 0);
  I32 r = OR_I32(OR_I32(OR_I32(b1_eq, b2_eq), b3_eq), b4_eq);
  STORE_T_I32(r);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 0010 nnnn mmmm 0111  1       calculation result
// DIV0S   Rm,Rn
INSTR(DIV0S) {
  I32 rm = LOAD_REG_I32(i->rm);
  I32 rn = LOAD_REG_I32(i->rn);
  I32 qm = XOR_I32(rn, rm);

  // update Q == M flag
  STORE_QM_I32(NOT_I32(qm));

  // msb of Q ^ M -> T
  I32 r = LSHR_IMM_I32(qm, 31);
  STORE_T_I32(r);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 0000 0000 0001 1001  1       0
// DIV0U
INSTR(DIV0U) {  //
  STORE_QM_IMM_I32((int32_t)0x80000000);
  STORE_T_I32(0);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 0011 nnnn mmmm 0100  1       calculation result
// DIV1 Rm,Rn
INSTR(DIV1) {
  I32 rm = LOAD_REG_I32(i->rm);
  I32 rn = LOAD_REG_I32(i->rn);

  // if Q == M, r0 = ~Rm and C = 1; else, r0 = Rm and C = 0
  I32 qm = ASHR_IMM_I32(LOAD_QM_I32(), 31);
  I32 r0 = XOR_I32(rm, qm);
  I32 carry = LSHR_IMM_I32(qm, 31);

  // initialize output bit as (Q == M) ^ Rn
  qm = XOR_I32(qm, rn);

  // shift Rn left by 1 and add T
  rn = OR_I32(SHL_IMM_I32(rn, 1), LOAD_T_I32());

  // add or subtract Rm based on r0 and C
  I32 rd = ADD_I32(ADD_I32(rn, r0), carry);
  STORE_REG_I32(i->rn, rd);

  // if C is cleared, invert output bit
  carry = LSHR_IMM_I32(
      OR_I32(AND_I32(rn, r0), AND_I32(OR_I32(rn, r0), NOT_I32(rd))), 31);
  qm = SELECT_I32(carry, qm, NOT_I32(qm));
  STORE_QM_I32(qm);

  // set T to output bit (which happens to be Q == M)
  STORE_T_I32(LSHR_IMM_I32(qm, 31));

  NEXT_INSTR();
}

// DMULS.L Rm,Rn
INSTR(DMULS) {
  I64 rm = SEXT_I32_I64(LOAD_REG_I32(i->rm));
  I64 rn = SEXT_I32_I64(LOAD_REG_I32(i->rn));
  I64 p = SMUL_I64(rm, rn);
  I32 low = TRUNCATE_I64_I32(p);
  I32 high = TRUNCATE_I64_I32(LSHR_IMM_I64(p, 32));
  STORE_MACL_I32(low);
  STORE_MACH_I32(high);
  NEXT_INSTR();
}

// DMULU.L Rm,Rn
INSTR(DMULU) {
  I64 rm = ZEXT_I32_I64(LOAD_REG_I32(i->rm));
  I64 rn = ZEXT_I32_I64(LOAD_REG_I32(i->rn));
  I64 p = UMUL_I64(rm, rn);
  I32 low = TRUNCATE_I64_I32(p);
  I32 high = TRUNCATE_I64_I32(LSHR_IMM_I64(p, 32));
  STORE_MACL_I32(low);
  STORE_MACH_I32(high);
  NEXT_INSTR();
}

// DT      Rn
INSTR(DT) {
  I32 rn = LOAD_REG_I32(i->rn);
  I32 v = SUB_IMM_I32(rn, 1);
  STORE_REG_I32(i->rn, v);
  STORE_T_I32(CMPEQ_IMM_I32(v, 0));
  NEXT_INSTR();
}

// EXTS.B  Rm,Rn
INSTR(EXTSB) {
  I8 rm = LOAD_REG_I8(i->rm);
  I32 v = SEXT_I8_I32(rm);
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// EXTS.W  Rm,Rn
INSTR(EXTSW) {
  I32 rm = LOAD_REG_I16(i->rm);
  STORE_REG_I32(i->rn, SEXT_I16_I32(rm));
  NEXT_INSTR();
}

// EXTU.B  Rm,Rn
INSTR(EXTUB) {
  I8 rm = LOAD_REG_I8(i->rm);
  I32 v = ZEXT_I8_I32(rm);
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// EXTU.W  Rm,Rn
INSTR(EXTUW) {
  I32 rm = LOAD_REG_I16(i->rm);
  I32 v = ZEXT_I16_I32(rm);
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// MAC.L   @Rm+,@Rn+
INSTR(MACL) {
  assert(0 && "MACL not implemented");
}

// MAC.W   @Rm+,@Rn+
INSTR(MACW) {
  assert(0 && "MACW not implemented");
}

// MUL.L   Rm,Rn
INSTR(MULL) {
  I32 rm = LOAD_REG_I32(i->rm);
  I32 rn = LOAD_REG_I32(i->rn);
  I32 v = SMUL_I32(rn, rm);
  STORE_MACL_I32(v);
  NEXT_INSTR();
}

// MULS    Rm,Rn
INSTR(MULS) {
  I32 rm = SEXT_I16_I32(LOAD_REG_I16(i->rm));
  I32 rn = SEXT_I16_I32(LOAD_REG_I16(i->rn));
  I32 v = SMUL_I32(rn, rm);
  STORE_MACL_I32(v);
  NEXT_INSTR();
}

// MULU    Rm,Rn
INSTR(MULU) {
  I32 rm = ZEXT_I16_I32(LOAD_REG_I16(i->rm));
  I32 rn = ZEXT_I16_I32(LOAD_REG_I16(i->rn));
  I32 v = UMUL_I32(rn, rm);
  STORE_MACL_I32(v);
  NEXT_INSTR();
}

// NEG     Rm,Rn
INSTR(NEG) {
  I32 rm = LOAD_REG_I32(i->rm);
  I32 v = NEG_I32(rm);
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// NEGC    Rm,Rn
INSTR(NEGC) {
  I32 t = LOAD_T_I32();
  I32 rm = LOAD_REG_I32(i->rm);
  I32 v = SUB_I32(NEG_I32(rm), t);
  STORE_REG_I32(i->rn, v);
  I32 c = OR_I32(t, rm);
  STORE_T_I32(c);
  NEXT_INSTR();
}

// SUB     Rm,Rn
INSTR(SUB) {
  I32 rm = LOAD_REG_I32(i->rm);
  I32 rn = LOAD_REG_I32(i->rn);
  I32 v = SUB_I32(rn, rm);
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// SUBC    Rm,Rn
INSTR(SUBC) {
  I32 rm = LOAD_REG_I32(i->rm);
  I32 rn = LOAD_REG_I32(i->rn);
  I32 v = SUB_I32(SUB_I32(rn, rm), LOAD_T_I32());
  STORE_REG_I32(i->rn, v);

  // compute carry flag, taken from Hacker's Delight
  I32 c = OR_I32(AND_I32(NOT_I32(rn), rm), AND_I32(OR_I32(NOT_I32(rn), rm), v));
  STORE_T_I32(c);

  NEXT_INSTR();
}

// SUBV    Rm,Rn
INSTR(SUBV) {
  I32 rm = LOAD_REG_I32(i->rm);
  I32 rn = LOAD_REG_I32(i->rn);
  I32 v = SUB_I32(rn, rm);
  STORE_REG_I32(i->rn, v);

  // compute overflow flag, taken from Hacker's Delight
  I32 o = LSHR_IMM_I32(AND_I32(XOR_I32(rn, rm), XOR_I32(v, rn)), 31);
  STORE_T_I32(o);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 0010 nnnn mmmm 1001  1       -
// AND     Rm,Rn
INSTR(AND) {
  I32 rn = LOAD_REG_I32(i->rn);
  I32 rm = LOAD_REG_I32(i->rm);
  I32 v = AND_I32(rn, rm);
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 1100 1001 iiii iiii  1       -
// AND     #imm,R0
INSTR(ANDI) {
  I32 r0 = LOAD_REG_I32(0);
  I32 mask = i->imm;
  I32 v = AND_I32(r0, mask);
  STORE_REG_I32(0, v);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 1100 1101 iiii iiii  1       -
// AND.B   #imm,@(R0,GBR)
INSTR(ANDB) {
  I32 r0 = LOAD_REG_I32(0);
  I32 gbr = LOAD_GBR_I32();
  I32 addr = ADD_I32(r0, gbr);
  I8 data = LOAD_I8(addr);
  I8 mask = i->imm;
  I8 v = AND_I8(data, mask);
  STORE_I8(addr, v);
  NEXT_INSTR();
}

// NOT     Rm,Rn
INSTR(NOT) {
  I32 rm = LOAD_REG_I32(i->rm);
  I32 v = NOT_I32(rm);
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// OR      Rm,Rn
INSTR(OR) {
  I32 rn = LOAD_REG_I32(i->rn);
  I32 rm = LOAD_REG_I32(i->rm);
  I32 v = OR_I32(rn, rm);
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// OR      #imm,R0
INSTR(ORI) {
  I32 r0 = LOAD_REG_I32(0);
  I32 v = OR_IMM_I32(r0, (uint32_t)i->imm);
  STORE_REG_I32(0, v);
  NEXT_INSTR();
}

// OR.B    #imm,@(R0,GBR)
INSTR(ORB) {
  I32 r0 = LOAD_REG_I32(0);
  I32 gbr = LOAD_GBR_I32();
  I32 addr = ADD_I32(r0, gbr);
  I8 v = OR_IMM_I8(LOAD_I8(addr), (uint8_t)i->imm);
  STORE_I8(addr, v);
  NEXT_INSTR();
}

// TAS.B   @Rn
INSTR(TAS) {
  I32 addr = LOAD_REG_I32(i->rn);
  I8 v = LOAD_I8(addr);
  STORE_I8(addr, OR_I8(v, (uint8_t)0x80));
  I8 r = CMPEQ_I8(v, 0);
  STORE_T_I32(r);
  NEXT_INSTR();
}

// TST     Rm,Rn
INSTR(TST) {
  I32 rm = LOAD_REG_I32(i->rm);
  I32 rn = LOAD_REG_I32(i->rn);
  I32 r = CMPEQ_IMM_I32(AND_I32(rn, rm), 0);
  STORE_T_I32(r);
  NEXT_INSTR();
}

// TST     #imm,R0
INSTR(TSTI) {
  I32 r0 = LOAD_REG_I32(0);
  uint32_t imm = (uint32_t)i->imm;
  I32 r = CMPEQ_IMM_I32(AND_I32(r0, imm), 0);
  STORE_T_I32(r);
  NEXT_INSTR();
}

// TST.B   #imm,@(R0,GBR)
INSTR(TSTB) {
  I32 r0 = LOAD_REG_I32(0);
  I32 gbr = LOAD_GBR_I32();
  I32 addr = ADD_I32(r0, gbr);
  I8 v = LOAD_I8(addr);
  I8 imm = (uint8_t)i->imm;
  I8 r = CMPEQ_IMM_I8(AND_I8(v, imm), 0);
  STORE_T_I32(r);
  NEXT_INSTR();
}

// XOR     Rm,Rn
INSTR(XOR) {
  I32 rn = LOAD_REG_I32(i->rn);
  I32 rm = LOAD_REG_I32(i->rm);
  I32 v = XOR_I32(rn, rm);
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// XOR     #imm,R0
INSTR(XORI) {
  I32 r0 = LOAD_REG_I32(0);
  uint32_t mask = (uint32_t)i->imm;
  I32 v = XOR_IMM_I32(r0, mask);
  STORE_REG_I32(0, v);
  NEXT_INSTR();
}

// XOR.B   #imm,@(R0,GBR)
INSTR(XORB) {
  I32 r0 = LOAD_REG_I32(0);
  I32 gbr = LOAD_GBR_I32();
  I32 addr = ADD_I32(r0, gbr);
  I8 v = XOR_I8(LOAD_I8(addr), (uint8_t)i->imm);
  STORE_I8(addr, v);
  NEXT_INSTR();
}

// ROTL    Rn
INSTR(ROTL) {
  I32 rn = LOAD_REG_I32(i->rn);
  I32 rn_msb = AND_I32(LSHR_IMM_I32(rn, 31), 0x1);
  I32 v = OR_I32(SHL_IMM_I32(rn, 1), rn_msb);
  STORE_REG_I32(i->rn, v);
  STORE_T_I32(rn_msb);
  NEXT_INSTR();
}

// ROTR    Rn
INSTR(ROTR) {
  I32 rn = LOAD_REG_I32(i->rn);
  I32 rn_lsb = AND_IMM_I32(rn, 0x1);
  I32 v = OR_I32(SHL_IMM_I32(rn_lsb, 31), LSHR_IMM_I32(rn, 1));
  STORE_REG_I32(i->rn, v);
  STORE_T_I32(rn_lsb);
  NEXT_INSTR();
}

// ROTCL   Rn
INSTR(ROTCL) {
  I32 rn = LOAD_REG_I32(i->rn);
  I32 rn_msb = AND_IMM_I32(LSHR_IMM_I32(rn, 31), 0x1);
  I32 v = OR_I32(SHL_IMM_I32(rn, 1), LOAD_T_I32());
  STORE_REG_I32(i->rn, v);
  STORE_T_I32(rn_msb);
  NEXT_INSTR();
}

// ROTCR   Rn
INSTR(ROTCR) {
  I32 rn = LOAD_REG_I32(i->rn);
  I32 rn_lsb = AND_IMM_I32(rn, 0x1);
  I32 v = OR_I32(SHL_IMM_I32(LOAD_T_I32(), 31), LSHR_IMM_I32(rn, 1));
  STORE_REG_I32(i->rn, v);
  STORE_T_I32(rn_lsb);
  NEXT_INSTR();
}

// SHAD    Rm,Rn
INSTR(SHAD) {
  // when Rm >= 0, Rn << Rm
  // when Rm < 0, Rn >> Rm
  // when shifting right > 32, Rn = (Rn >= 0 ? 0 : -1)
  I32 rn = LOAD_REG_I32(i->rn);
  I32 rm = LOAD_REG_I32(i->rm);
  STORE_REG_I32(i->rn, ASHD_I32(rn, rm));
  NEXT_INSTR();
}

// SHAL    Rn      (same as SHLL)
INSTR(SHAL) {
  I32 rn = LOAD_REG_I32(i->rn);
  I32 rn_msb = AND_I32(LSHR_IMM_I32(rn, 31), 0x1);
  I32 v = SHL_IMM_I32(rn, 1);
  STORE_REG_I32(i->rn, v);
  STORE_T_I32(rn_msb);
  NEXT_INSTR();
}

// SHAR    Rn
INSTR(SHAR) {
  I32 rn = LOAD_REG_I32(i->rn);
  I32 rn_lsb = AND_IMM_I32(rn, 0x1);
  I32 v = ASHR_IMM_I32(rn, 1);
  STORE_REG_I32(i->rn, v);
  STORE_T_I32(rn_lsb);
  NEXT_INSTR();
}

// SHLD    Rm,Rn
INSTR(SHLD) {
  // when Rm >= 0, Rn << Rm
  // when Rm < 0, Rn >> Rm
  // when shifting right >= 32, Rn = 0
  I32 rn = LOAD_REG_I32(i->rn);
  I32 rm = LOAD_REG_I32(i->rm);
  STORE_REG_I32(i->rn, LSHD_I32(rn, rm));
  NEXT_INSTR();
}

// SHLL    Rn      (same as SHAL)
INSTR(SHLL) {
  I32 rn = LOAD_REG_I32(i->rn);
  I32 rn_msb = AND_IMM_I32(LSHR_IMM_I32(rn, 31), 1);
  STORE_REG_I32(i->rn, SHL_IMM_I32(rn, 1));
  STORE_T_I32(rn_msb);
  NEXT_INSTR();
}

// SHLR    Rn
INSTR(SHLR) {
  I32 rn = LOAD_REG_I32(i->rn);
  I32 rn_lsb = AND_IMM_I32(rn, 0x1);
  STORE_REG_I32(i->rn, LSHR_IMM_I32(rn, 1));
  STORE_T_I32(rn_lsb);
  NEXT_INSTR();
}

// SHLL2   Rn
INSTR(SHLL2) {
  I32 rn = LOAD_REG_I32(i->rn);
  STORE_REG_I32(i->rn, SHL_IMM_I32(rn, 2));
  NEXT_INSTR();
}

// SHLR2   Rn
INSTR(SHLR2) {
  I32 rn = LOAD_REG_I32(i->rn);
  STORE_REG_I32(i->rn, LSHR_IMM_I32(rn, 2));
  NEXT_INSTR();
}

// SHLL8   Rn
INSTR(SHLL8) {
  I32 rn = LOAD_REG_I32(i->rn);
  STORE_REG_I32(i->rn, SHL_IMM_I32(rn, 8));
  NEXT_INSTR();
}

// SHLR8   Rn
INSTR(SHLR8) {
  I32 rn = LOAD_REG_I32(i->rn);
  STORE_REG_I32(i->rn, LSHR_IMM_I32(rn, 8));
  NEXT_INSTR();
}

// SHLL16  Rn
INSTR(SHLL16) {
  I32 rn = LOAD_REG_I32(i->rn);
  STORE_REG_I32(i->rn, SHL_IMM_I32(rn, 16));
  NEXT_INSTR();
}

// SHLR16  Rn
INSTR(SHLR16) {
  I32 rn = LOAD_REG_I32(i->rn);
  STORE_REG_I32(i->rn, LSHR_IMM_I32(rn, 16));
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 1000 1011 dddd dddd  3/1     -
// BF      disp
INSTR(BF) {
  I32 cond = LOAD_T_I32();
  uint32_t dest_addr = ((int8_t)i->disp * 2) + ctx->pc + 4;
  BRANCH_FALSE_IMM_I32(cond, dest_addr);
}

// code                 cycles  t-bit
// 1000 1111 dddd dddd  3/1     -
// BFS     disp
INSTR(BFS) {
  I32 cond = LOAD_T_I32();
  uint32_t dest_addr = ((int8_t)i->disp * 2) + ctx->pc + 4;
  DELAY_INSTR();
  BRANCH_FALSE_IMM_I32(cond, dest_addr);
}

// code                 cycles  t-bit
// 1000 1001 dddd dddd  3/1     -
// BT      disp
INSTR(BT) {
  I32 cond = LOAD_T_I32();
  uint32_t dest_addr = ((int8_t)i->disp * 2) + ctx->pc + 4;
  BRANCH_TRUE_IMM_I32(cond, dest_addr);
}

// code                 cycles  t-bit
// 1000 1101 dddd dddd  2/1     -
// BTS     disp
INSTR(BTS) {
  I32 cond = LOAD_T_I32();
  uint32_t dest_addr = ((int8_t)i->disp * 2) + ctx->pc + 4;
  DELAY_INSTR();
  BRANCH_TRUE_IMM_I32(cond, dest_addr);
}

// code                 cycles  t-bit
// 1010 dddd dddd dddd  2       -
// BRA     disp
INSTR(BRA) {
  int32_t disp = ((i->disp & 0xfff) << 20) >>
                 20;  // 12-bit displacement must be sign extended
  uint32_t dest_addr = (disp * 2) + ctx->pc + 4;
  DELAY_INSTR();
  BRANCH_IMM_I32(dest_addr);
}

// code                 cycles  t-bit
// 0000 mmmm 0010 0011  2       -
// BRAF    Rn
INSTR(BRAF) {
  I32 rn = LOAD_REG_I32(i->rn);
  I32 dest_addr = ADD_IMM_I32(rn, ctx->pc + 4);
  DELAY_INSTR();
  BRANCH_I32(dest_addr);
}

// code                 cycles  t-bit
// 1011 dddd dddd dddd  2       -
// BSR     disp
INSTR(BSR) {
  int32_t disp = ((i->disp & 0xfff) << 20) >>
                 20;  // 12-bit displacement must be sign extended
  uint32_t ret_addr = ctx->pc + 4;
  uint32_t dest_addr = ret_addr + disp * 2;
  DELAY_INSTR();
  STORE_PR_IMM_I32(ret_addr);
  BRANCH_IMM_I32(dest_addr);
}

// code                 cycles  t-bit
// 0000 mmmm 0000 0011  2       -
// BSRF    Rn
INSTR(BSRF) {
  I32 rn = LOAD_REG_I32(i->rn);
  uint32_t ret_addr = ctx->pc + 4;
  I32 dest_addr = ADD_IMM_I32(rn, ret_addr);
  DELAY_INSTR();
  STORE_PR_IMM_I32(ret_addr);
  BRANCH_I32(dest_addr);
}

// JMP     @Rm
INSTR(JMP) {
  I32 dest_addr = LOAD_REG_I32(i->rn);
  DELAY_INSTR();
  BRANCH_I32(dest_addr);
}

// JSR     @Rn
INSTR(JSR) {
  I32 dest_addr = LOAD_REG_I32(i->rn);
  uint32_t ret_addr = ctx->pc + 4;
  DELAY_INSTR();
  STORE_PR_IMM_I32(ret_addr);
  BRANCH_I32(dest_addr);
}

// RTS
INSTR(RTS) {
  I32 dest_addr = LOAD_PR_I32();
  DELAY_INSTR();
  BRANCH_I32(dest_addr);
}

// code                 cycles  t-bit
// 0000 0000 0010 1000  1       -
// CLRMAC
INSTR(CLRMAC) {
  STORE_MACH_IMM_I32(0);
  STORE_MACL_IMM_I32(0);
  NEXT_INSTR();
}

INSTR(CLRS) {
  STORE_S_IMM_I32(0);
  NEXT_INSTR();
}

// code                 cycles  t-bit
// 0000 0000 0000 1000  1       -
// CLRT
INSTR(CLRT) {
  STORE_T_IMM_I32(0);
  NEXT_INSTR();
}

// LDC     Rm,SR
INSTR(LDCSR) {
  I32 v = LOAD_REG_I32(i->rm);
  STORE_SR_I32(v);
  NEXT_INSTR();
}

// LDC     Rm,GBR
INSTR(LDCGBR) {
  I32 v = LOAD_REG_I32(i->rm);
  STORE_GBR_I32(v);
  NEXT_INSTR();
}

// LDC     Rm,VBR
INSTR(LDCVBR) {
  I32 v = LOAD_REG_I32(i->rm);
  STORE_VBR_I32(v);
  NEXT_INSTR();
}

// LDC     Rm,SSR
INSTR(LDCSSR) {
  I32 v = LOAD_REG_I32(i->rm);
  STORE_SSR_I32(v);
  NEXT_INSTR();
}

// LDC     Rm,SPC
INSTR(LDCSPC) {
  I32 v = LOAD_REG_I32(i->rm);
  STORE_SPC_I32(v);
  NEXT_INSTR();
}

// LDC     Rm,DBR
INSTR(LDCDBR) {
  I32 v = LOAD_REG_I32(i->rm);
  STORE_DBR_I32(v);
  NEXT_INSTR();
}

// LDC.L   Rm,Rn_BANK
INSTR(LDCRBANK) {
  //   int reg = i.rn & 0x7;
  //   I32 rm = LOAD_REG_I32(i->rm)
  //   b.StoreContext(offsetof(SH4Context, ralt) + reg * 4, rm);
  // NEXT_INSTR();
  assert(0);
}

// LDC.L   @Rm+,SR
INSTR(LDCMSR) {
  I32 addr = LOAD_REG_I32(i->rm);
  I32 v = LOAD_I32(addr);
  STORE_SR_I32(v);
  // reload Rm, sr store could have swapped banks
  addr = LOAD_REG_I32(i->rm);
  STORE_REG_I32(i->rm, ADD_I32(addr, 4));
  NEXT_INSTR();
}

// LDC.L   @Rm+,GBR
INSTR(LDCMGBR) {
  I32 addr = LOAD_REG_I32(i->rm);
  I32 v = LOAD_I32(addr);
  STORE_GBR_I32(v);
  STORE_REG_I32(i->rm, ADD_I32(addr, 4));
  NEXT_INSTR();
}

// LDC.L   @Rm+,VBR
INSTR(LDCMVBR) {
  I32 addr = LOAD_REG_I32(i->rm);
  I32 v = LOAD_I32(addr);
  STORE_VBR_I32(v);
  STORE_REG_I32(i->rm, ADD_I32(addr, 4));
  NEXT_INSTR();
}

// LDC.L   @Rm+,SSR
INSTR(LDCMSSR) {
  I32 addr = LOAD_REG_I32(i->rm);
  I32 v = LOAD_I32(addr);
  STORE_SSR_I32(v);
  STORE_REG_I32(i->rm, ADD_I32(addr, 4));
  NEXT_INSTR();
}

// LDC.L   @Rm+,SPC
INSTR(LDCMSPC) {
  I32 addr = LOAD_REG_I32(i->rm);
  I32 v = LOAD_I32(addr);
  STORE_SPC_I32(v);
  STORE_REG_I32(i->rm, ADD_I32(addr, 4));
  NEXT_INSTR();
}

// LDC.L   @Rm+,DBR
INSTR(LDCMDBR) {
  I32 addr = LOAD_REG_I32(i->rm);
  I32 v = LOAD_I32(addr);
  STORE_DBR_I32(v);
  STORE_REG_I32(i->rm, ADD_I32(addr, 4));
  NEXT_INSTR();
}

// LDC.L   @Rm+,Rn_BANK
INSTR(LDCMRBANK) {
  //   int reg = i.rn & 0x7;
  //   Value *addr = LOAD_REG_I32(i->rm);
  //   STORE_REG_I32(i->rm, b.Add(addr, b.AllocConstant(4)));
  //   Value *v = LOAD_I32(addr);
  //   b.StoreContext(offsetof(SH4Context, ralt) + reg * 4, v);
  // NEXT_INSTR();
  assert(0);
}

// LDS     Rm,MACH
INSTR(LDSMACH) {
  I32 v = LOAD_REG_I32(i->rm);
  STORE_MACH_I32(v);
  NEXT_INSTR();
}

// LDS     Rm,MACL
INSTR(LDSMACL) {
  I32 v = LOAD_REG_I32(i->rm);
  STORE_MACL_I32(v);
  NEXT_INSTR();
}

// LDS     Rm,PR
INSTR(LDSPR) {
  I32 v = LOAD_REG_I32(i->rm);
  STORE_PR_I32(v);
  NEXT_INSTR();
}

// LDS.L   @Rm+,MACH
INSTR(LDSMMACH) {
  I32 addr = LOAD_REG_I32(i->rm);
  I32 v = LOAD_I32(addr);
  STORE_MACH_I32(v);
  STORE_REG_I32(i->rm, ADD_I32(addr, 4));
  NEXT_INSTR();
}

// LDS.L   @Rm+,MACL
INSTR(LDSMMACL) {
  I32 addr = LOAD_REG_I32(i->rm);
  I32 v = LOAD_I32(addr);
  STORE_MACL_I32(v);
  STORE_REG_I32(i->rm, ADD_I32(addr, 4));
  NEXT_INSTR();
}

// LDS.L   @Rm+,PR
INSTR(LDSMPR) {
  I32 addr = LOAD_REG_I32(i->rm);
  I32 v = LOAD_I32(addr);
  STORE_PR_I32(v);
  STORE_REG_I32(i->rm, ADD_IMM_I32(addr, (int32_t)4));
  NEXT_INSTR();
}

// MOVCA.L     R0,@Rn
INSTR(MOVCAL) {
  assert(0 && "MOVCAL not implemented");
}

// NOP
INSTR(NOP) {
  NEXT_INSTR();
}

// OCBI
INSTR(OCBI) {
  NEXT_INSTR();
}

// OCBP
INSTR(OCBP) {
  NEXT_INSTR();
}

// OCBWB
INSTR(OCBWB) {
  NEXT_INSTR();
}

// PREF     @Rn
INSTR(PREF) {
  //   Value *pref = b.LoadContext(offsetof(SH4Context, Pref), VALUE_I64);
  //   Value *addr = b.ZExt(LOAD_REG_I32(i->rn), VALUE_I64);
  //   b.CallExternal2(pref, addr);
  // NEXT_INSTR();
  assert(0);
}

// RTE
INSTR(RTE) {
  I32 ssr = LOAD_SSR_I32();
  I32 spc = LOAD_SPC_I32();
  STORE_SR_I32(ssr);
  BRANCH_I32(spc);
}

// SETS
INSTR(SETS) {
  STORE_S_IMM_I32(1);
  NEXT_INSTR();
}

// SETT
INSTR(SETT) {
  STORE_T_IMM_I32(1);
  NEXT_INSTR();
}

// SLEEP
INSTR(SLEEP) {
  assert(0 && "SLEEP not implemented");
}

// STC     SR,Rn
INSTR(STCSR) {
  I32 v = LOAD_SR_I32();
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// STC     GBR,Rn
INSTR(STCGBR) {
  I32 v = LOAD_GBR_I32();
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// STC     VBR,Rn
INSTR(STCVBR) {
  I32 v = LOAD_VBR_I32();
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// STC     SSR,Rn
INSTR(STCSSR) {
  I32 v = LOAD_SSR_I32();
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// STC     SPC,Rn
INSTR(STCSPC) {
  I32 v = LOAD_SPC_I32();
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// STC     SGR,Rn
INSTR(STCSGR) {
  I32 v = LOAD_SGR_I32();
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// STC     DBR,Rn
INSTR(STCDBR) {
  I32 v = LOAD_DBR_I32();
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// STC     Rm_BANK,Rn
INSTR(STCRBANK) {
  //   int reg = i.rm & 0x7;
  //   STORE_REG_I32(i->rn, b.LoadContext(offsetof(SH4Context, ralt) + reg * 4,
  //   VALUE_I32));
  // NEXT_INSTR();
  assert(0);
}

// STC.L   SR,@-Rn
INSTR(STCMSR) {
  I32 addr = SUB_IMM_I32(LOAD_REG_I32(i->rn), 4);
  STORE_REG_I32(i->rn, addr);
  I32 v = LOAD_SR_I32();
  STORE_I32(addr, v);
  NEXT_INSTR();
}

// STC.L   GBR,@-Rn
INSTR(STCMGBR) {
  I32 addr = SUB_IMM_I32(LOAD_REG_I32(i->rn), 4);
  STORE_REG_I32(i->rn, addr);
  I32 v = LOAD_GBR_I32();
  STORE_I32(addr, v);
  NEXT_INSTR();
}

// STC.L   VBR,@-Rn
INSTR(STCMVBR) {
  I32 addr = SUB_IMM_I32(LOAD_REG_I32(i->rn), 4);
  STORE_REG_I32(i->rn, addr);
  I32 v = LOAD_VBR_I32();
  STORE_I32(addr, v);
  NEXT_INSTR();
}

// STC.L   SSR,@-Rn
INSTR(STCMSSR) {
  I32 addr = SUB_IMM_I32(LOAD_REG_I32(i->rn), 4);
  STORE_REG_I32(i->rn, addr);
  I32 v = LOAD_SSR_I32();
  STORE_I32(addr, v);
  NEXT_INSTR();
}

// STC.L   SPC,@-Rn
INSTR(STCMSPC) {
  I32 addr = SUB_IMM_I32(LOAD_REG_I32(i->rn), 4);
  STORE_REG_I32(i->rn, addr);
  I32 v = LOAD_SPC_I32();
  STORE_I32(addr, v);
  NEXT_INSTR();
}

// STC.L   SGR,@-Rn
INSTR(STCMSGR) {
  I32 addr = SUB_IMM_I32(LOAD_REG_I32(i->rn), 4);
  STORE_REG_I32(i->rn, addr);
  I32 v = LOAD_SGR_I32();
  STORE_I32(addr, v);
  NEXT_INSTR();
}

// STC.L   DBR,@-Rn
INSTR(STCMDBR) {
  I32 addr = SUB_IMM_I32(LOAD_REG_I32(i->rn), 4);
  STORE_REG_I32(i->rn, addr);
  I32 v = LOAD_DBR_I32();
  STORE_I32(addr, v);
  NEXT_INSTR();
}

// STC.L   Rm_BANK,@-Rn
INSTR(STCMRBANK) {
  //   int reg = i.rm & 0x7;
  //   Value *addr = SUB_IMM_I32(LOAD_REG_I32(i->rn), (int32_t)4);
  //   STORE_REG_I32(i->rn, addr);
  //   b.Store(addr, b.LoadContext(offsetof(SH4Context, ralt) + reg * 4,
  //   VALUE_I32));
  // NEXT_INSTR();
  assert(0);
}

// STS     MACH,Rn
INSTR(STSMACH) {
  I32 v = LOAD_MACH_I32();
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// STS     MACL,Rn
INSTR(STSMACL) {
  I32 v = LOAD_MACL_I32();
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// STS     PR,Rn
INSTR(STSPR) {
  I32 v = LOAD_PR_I32();
  STORE_REG_I32(i->rn, v);
  NEXT_INSTR();
}

// STS.L   MACH,@-Rn
INSTR(STSMMACH) {
  I32 addr = SUB_IMM_I32(LOAD_REG_I32(i->rn), 4);
  STORE_REG_I32(i->rn, addr);
  I32 v = LOAD_MACH_I32();
  STORE_I32(addr, v);
  NEXT_INSTR();
}

// STS.L   MACL,@-Rn
INSTR(STSMMACL) {
  I32 addr = SUB_IMM_I32(LOAD_REG_I32(i->rn), 4);
  STORE_REG_I32(i->rn, addr);
  I32 v = LOAD_MACL_I32();
  STORE_I32(addr, v);
  NEXT_INSTR();
}

// STS.L   PR,@-Rn
INSTR(STSMPR) {
  I32 addr = SUB_IMM_I32(LOAD_REG_I32(i->rn), 4);
  STORE_REG_I32(i->rn, addr);
  I32 v = LOAD_PR_I32();
  STORE_I32(addr, v);
  NEXT_INSTR();
}

// TRAPA   #imm
INSTR(TRAPA) {
  assert(0 && "TRAPA not implemented");
}

//
// instruction lookup table. the mask and shift fields are filled in at runtime
// during sh4_init_tables by parsing the signature field
//

// clang-format off
INSTR_TYPE_T instr_type[NUM_OPCODES] = {
  // fixed-point transfer instructions
  { OP_MOVI,      0,                          "1110nnnniiiiiiii", &NAMESPACIFY(MOVI),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVWLPC,   0,                          "1001nnnndddddddd", &NAMESPACIFY(MOVWLPC),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVLLPC,   0,                          "1101nnnndddddddd", &NAMESPACIFY(MOVLLPC),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOV,       0,                          "0110nnnnmmmm0011", &NAMESPACIFY(MOV),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVBS,     0,                          "0010nnnnmmmm0000", &NAMESPACIFY(MOVBS),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVWS,     0,                          "0010nnnnmmmm0001", &NAMESPACIFY(MOVWS),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVLS,     0,                          "0010nnnnmmmm0010", &NAMESPACIFY(MOVLS),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVBL,     0,                          "0110nnnnmmmm0000", &NAMESPACIFY(MOVBL),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVWL,     0,                          "0110nnnnmmmm0001", &NAMESPACIFY(MOVWL),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVLL,     0,                          "0110nnnnmmmm0010", &NAMESPACIFY(MOVLL),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVBM,     0,                          "0010nnnnmmmm0100", &NAMESPACIFY(MOVBM),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVWM,     0,                          "0010nnnnmmmm0101", &NAMESPACIFY(MOVWM),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVLM,     0,                          "0010nnnnmmmm0110", &NAMESPACIFY(MOVLM),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVBP,     0,                          "0110nnnnmmmm0100", &NAMESPACIFY(MOVBP),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVWP,     0,                          "0110nnnnmmmm0101", &NAMESPACIFY(MOVWP),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVLP,     0,                          "0110nnnnmmmm0110", &NAMESPACIFY(MOVLP),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVBS0D,   0,                          "10000000nnnndddd", &NAMESPACIFY(MOVBS0D),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVWS0D,   0,                          "10000001nnnndddd", &NAMESPACIFY(MOVWS0D),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVLSMD,   0,                          "0001nnnnmmmmdddd", &NAMESPACIFY(MOVLSMD),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVBLD0,   0,                          "10000100mmmmdddd", &NAMESPACIFY(MOVBLD0),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVWLD0,   0,                          "10000101mmmmdddd", &NAMESPACIFY(MOVWLD0),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVLLDN,   0,                          "0101nnnnmmmmdddd", &NAMESPACIFY(MOVLLDN),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVBS0,    0,                          "0000nnnnmmmm0100", &NAMESPACIFY(MOVBS0),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVWS0,    0,                          "0000nnnnmmmm0101", &NAMESPACIFY(MOVWS0),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVLS0,    0,                          "0000nnnnmmmm0110", &NAMESPACIFY(MOVLS0),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVBL0,    0,                          "0000nnnnmmmm1100", &NAMESPACIFY(MOVBL0),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVWL0,    0,                          "0000nnnnmmmm1101", &NAMESPACIFY(MOVWL0),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVLL0,    0,                          "0000nnnnmmmm1110", &NAMESPACIFY(MOVLL0),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVBS0G,   0,                          "11000000dddddddd", &NAMESPACIFY(MOVBS0G),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVWS0G,   0,                          "11000001dddddddd", &NAMESPACIFY(MOVWS0G),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVLS0G,   0,                          "11000010dddddddd", &NAMESPACIFY(MOVLS0G),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVBLG0,   0,                          "11000100dddddddd", &NAMESPACIFY(MOVBLG0),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVWLG0,   0,                          "11000101dddddddd", &NAMESPACIFY(MOVWLG0),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVLLG0,   0,                          "11000110dddddddd", &NAMESPACIFY(MOVLLG0),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVA,      0,                          "11000111dddddddd", &NAMESPACIFY(MOVA),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVT,      0,                          "0000nnnn00101001", &NAMESPACIFY(MOVT),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SWAPB,     0,                          "0110nnnnmmmm1000", &NAMESPACIFY(SWAPB),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SWAPW,     0,                          "0110nnnnmmmm1001", &NAMESPACIFY(SWAPW),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_XTRCT,     0,                          "0010nnnnmmmm1101", &NAMESPACIFY(XTRCT),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  // arithmetric operation instructions
  { OP_ADD,       0,                          "0011nnnnmmmm1100", &NAMESPACIFY(ADD),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_ADDI,      0,                          "0111nnnniiiiiiii", &NAMESPACIFY(ADDI),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_ADDC,      0,                          "0011nnnnmmmm1110", &NAMESPACIFY(ADDC),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_ADDV,      0,                          "0011nnnnmmmm1111", &NAMESPACIFY(ADDV),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_CMPEQI,    0,                          "10001000iiiiiiii", &NAMESPACIFY(CMPEQI),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_CMPEQ,     0,                          "0011nnnnmmmm0000", &NAMESPACIFY(CMPEQ),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_CMPHS,     0,                          "0011nnnnmmmm0010", &NAMESPACIFY(CMPHS),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_CMPGE,     0,                          "0011nnnnmmmm0011", &NAMESPACIFY(CMPGE),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_CMPHI,     0,                          "0011nnnnmmmm0110", &NAMESPACIFY(CMPHI),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_CMPGT,     0,                          "0011nnnnmmmm0111", &NAMESPACIFY(CMPGT),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_CMPPZ,     0,                          "0100nnnn00010001", &NAMESPACIFY(CMPPZ),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_CMPPL,     0,                          "0100nnnn00010101", &NAMESPACIFY(CMPPL),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_CMPSTR,    0,                          "0010nnnnmmmm1100", &NAMESPACIFY(CMPSTR),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_DIV0S,     0,                          "0010nnnnmmmm0111", &NAMESPACIFY(DIV0S),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_DIV0U,     0,                          "0000000000011001", &NAMESPACIFY(DIV0U),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_DIV1,      0,                          "0011nnnnmmmm0100", &NAMESPACIFY(DIV1),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_DMULS,     0,                          "0011nnnnmmmm1101", &NAMESPACIFY(DMULS),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_DMULU,     0,                          "0011nnnnmmmm0101", &NAMESPACIFY(DMULU),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_DT,        0,                          "0100nnnn00010000", &NAMESPACIFY(DT),        0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_EXTSB,     0,                          "0110nnnnmmmm1110", &NAMESPACIFY(EXTSB),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_EXTSW,     0,                          "0110nnnnmmmm1111", &NAMESPACIFY(EXTSW),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_EXTUB,     0,                          "0110nnnnmmmm1100", &NAMESPACIFY(EXTUB),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_EXTUW,     0,                          "0110nnnnmmmm1101", &NAMESPACIFY(EXTUW),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MACL,      0,                          "0000nnnnmmmm1111", &NAMESPACIFY(MACL),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MACW,      0,                          "0100nnnnmmmm1111", &NAMESPACIFY(MACW),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MULL,      0,                          "0000nnnnmmmm0111", &NAMESPACIFY(MULL),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MULS,      0,                          "0010nnnnmmmm1111", &NAMESPACIFY(MULS),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MULU,      0,                          "0010nnnnmmmm1110", &NAMESPACIFY(MULU),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_NEG,       0,                          "0110nnnnmmmm1011", &NAMESPACIFY(NEG),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_NEGC,      0,                          "0110nnnnmmmm1010", &NAMESPACIFY(NEGC),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SUB,       0,                          "0011nnnnmmmm1000", &NAMESPACIFY(SUB),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SUBC,      0,                          "0011nnnnmmmm1010", &NAMESPACIFY(SUBC),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SUBV,      0,                          "0011nnnnmmmm1011", &NAMESPACIFY(SUBV),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  // logic operation instructions
  { OP_AND,       0,                          "0010nnnnmmmm1001", &NAMESPACIFY(AND),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_ANDI,      0,                          "11001001iiiiiiii", &NAMESPACIFY(ANDI),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_ANDB,      0,                          "11001101iiiiiiii", &NAMESPACIFY(ANDB),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_NOT,       0,                          "0110nnnnmmmm0111", &NAMESPACIFY(NOT),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_OR,        0,                          "0010nnnnmmmm1011", &NAMESPACIFY(OR),        0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_ORI,       0,                          "11001011iiiiiiii", &NAMESPACIFY(ORI),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_ORB,       0,                          "11001111iiiiiiii", &NAMESPACIFY(ORB),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_TAS,       0,                          "0100nnnn00011011", &NAMESPACIFY(TAS),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_TST,       0,                          "0010nnnnmmmm1000", &NAMESPACIFY(TST),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_TSTI,      0,                          "11001000iiiiiiii", &NAMESPACIFY(TSTI),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_TSTB,      0,                          "11001100iiiiiiii", &NAMESPACIFY(TSTB),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_XOR,       0,                          "0010nnnnmmmm1010", &NAMESPACIFY(XOR),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_XORI,      0,                          "11001010iiiiiiii", &NAMESPACIFY(XORI),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_XORB,      0,                          "11001110iiiiiiii", &NAMESPACIFY(XORB),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  // shift instructions
  { OP_ROTL,      0,                          "0100nnnn00000100", &NAMESPACIFY(ROTL),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_ROTR,      0,                          "0100nnnn00000101", &NAMESPACIFY(ROTR),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_ROTCL,     0,                          "0100nnnn00100100", &NAMESPACIFY(ROTCL),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_ROTCR,     0,                          "0100nnnn00100101", &NAMESPACIFY(ROTCR),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SHAD,      0,                          "0100nnnnmmmm1100", &NAMESPACIFY(SHAD),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SHAL,      0,                          "0100nnnn00100000", &NAMESPACIFY(SHAL),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SHAR,      0,                          "0100nnnn00100001", &NAMESPACIFY(SHAR),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SHLD,      0,                          "0100nnnnmmmm1101", &NAMESPACIFY(SHLD),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SHLL,      0,                          "0100nnnn00000000", &NAMESPACIFY(SHLL),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SHLR,      0,                          "0100nnnn00000001", &NAMESPACIFY(SHLR),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SHLL2,     0,                          "0100nnnn00001000", &NAMESPACIFY(SHLL2),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SHLR2,     0,                          "0100nnnn00001001", &NAMESPACIFY(SHLR2),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SHLL8,     0,                          "0100nnnn00011000", &NAMESPACIFY(SHLL8),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SHLR8,     0,                          "0100nnnn00011001", &NAMESPACIFY(SHLR8),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SHLL16,    0,                          "0100nnnn00101000", &NAMESPACIFY(SHLL16),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SHLR16,    0,                          "0100nnnn00101001", &NAMESPACIFY(SHLR16),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  // branch instructions
  { OP_BF,        FLAG_BRANCH,                "10001011dddddddd", &NAMESPACIFY(BF),        0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_BFS,       FLAG_BRANCH | FLAG_DELAYED, "10001111dddddddd", &NAMESPACIFY(BFS),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_BT,        FLAG_BRANCH,                "10001001dddddddd", &NAMESPACIFY(BT),        0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_BTS,       FLAG_BRANCH | FLAG_DELAYED, "10001101dddddddd", &NAMESPACIFY(BTS),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_BRA,       FLAG_BRANCH | FLAG_DELAYED, "1010dddddddddddd", &NAMESPACIFY(BRA),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_BRAF,      FLAG_BRANCH | FLAG_DELAYED, "0000nnnn00100011", &NAMESPACIFY(BRAF),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_BSR,       FLAG_BRANCH | FLAG_DELAYED, "1011dddddddddddd", &NAMESPACIFY(BSR),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_BSRF,      FLAG_BRANCH | FLAG_DELAYED, "0000nnnn00000011", &NAMESPACIFY(BSRF),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_JMP,       FLAG_BRANCH | FLAG_DELAYED, "0100nnnn00101011", &NAMESPACIFY(JMP),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_JSR,       FLAG_BRANCH | FLAG_DELAYED, "0100nnnn00001011", &NAMESPACIFY(JSR),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_RTS,       FLAG_BRANCH | FLAG_DELAYED, "0000000000001011", &NAMESPACIFY(RTS),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  // system control instructions
  { OP_CLRMAC,    0,                          "0000000000101000", &NAMESPACIFY(CLRMAC),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_CLRS,      0,                          "0000000001001000", &NAMESPACIFY(CLRS),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_CLRT,      0,                          "0000000000001000", &NAMESPACIFY(CLRT),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDCSR,     0,                          "0100mmmm00001110", &NAMESPACIFY(LDCSR),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDCGBR,    0,                          "0100mmmm00011110", &NAMESPACIFY(LDCGBR),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDCVBR,    0,                          "0100mmmm00101110", &NAMESPACIFY(LDCVBR),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDCSSR,    0,                          "0100mmmm00111110", &NAMESPACIFY(LDCSSR),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDCSPC,    0,                          "0100mmmm01001110", &NAMESPACIFY(LDCSPC),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDCDBR,    0,                          "0100mmmm11111010", &NAMESPACIFY(LDCDBR),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDCRBANK,  0,                          "0100mmmm1nnn1110", &NAMESPACIFY(LDCRBANK),  0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDCMSR,    0,                          "0100mmmm00000111", &NAMESPACIFY(LDCMSR),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDCMGBR,   0,                          "0100mmmm00010111", &NAMESPACIFY(LDCMGBR),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDCMVBR,   0,                          "0100mmmm00100111", &NAMESPACIFY(LDCMVBR),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDCMSSR,   0,                          "0100mmmm00110111", &NAMESPACIFY(LDCMSSR),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDCMSPC,   0,                          "0100mmmm01000111", &NAMESPACIFY(LDCMSPC),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDCMDBR,   0,                          "0100mmmm11110110", &NAMESPACIFY(LDCMDBR),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDCMRBANK, 0,                          "0100mmmm1nnn0111", &NAMESPACIFY(LDCMRBANK), 0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDSMACH,   0,                          "0100mmmm00001010", &NAMESPACIFY(LDSMACH),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDSMACL,   0,                          "0100mmmm00011010", &NAMESPACIFY(LDSMACL),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDSPR,     0,                          "0100mmmm00101010", &NAMESPACIFY(LDSPR),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDSMMACH,  0,                          "0100mmmm00000110", &NAMESPACIFY(LDSMMACH),  0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDSMMACL,  0,                          "0100mmmm00010110", &NAMESPACIFY(LDSMMACL),  0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_LDSMPR,    0,                          "0100mmmm00100110", &NAMESPACIFY(LDSMPR),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_MOVCAL,    0,                          "0000nnnn11000011", &NAMESPACIFY(MOVCAL),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_NOP,       0,                          "0000000000001001", &NAMESPACIFY(NOP),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_OCBI,      0,                          "0000nnnn10010011", &NAMESPACIFY(OCBI),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_OCBP,      0,                          "0000nnnn10100011", &NAMESPACIFY(OCBP),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_OCBWB,     0,                          "0000nnnn10110011", &NAMESPACIFY(OCBWB),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_PREF,      0,                          "0000nnnn10000011", &NAMESPACIFY(PREF),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_RTE,       FLAG_BRANCH | FLAG_DELAYED, "0000000000101011", &NAMESPACIFY(RTE),       0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SETS,      0,                          "0000000001011000", &NAMESPACIFY(SETS),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SETT,      0,                          "0000000000011000", &NAMESPACIFY(SETT),      0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_SLEEP,     0,                          "0000000000011011", &NAMESPACIFY(SLEEP),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STCSR,     0,                          "0000nnnn00000010", &NAMESPACIFY(STCSR),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STCGBR,    0,                          "0000nnnn00010010", &NAMESPACIFY(STCGBR),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STCVBR,    0,                          "0000nnnn00100010", &NAMESPACIFY(STCVBR),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STCSSR,    0,                          "0000nnnn00110010", &NAMESPACIFY(STCSSR),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STCSPC,    0,                          "0000nnnn01000010", &NAMESPACIFY(STCSPC),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STCSGR,    0,                          "0000nnnn00111010", &NAMESPACIFY(STCSGR),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STCDBR,    0,                          "0000nnnn11111010", &NAMESPACIFY(STCDBR),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STCRBANK,  0,                          "0000nnnn1mmm0010", &NAMESPACIFY(STCRBANK),  0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STCMSR,    0,                          "0100nnnn00000011", &NAMESPACIFY(STCMSR),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STCMGBR,   0,                          "0100nnnn00010011", &NAMESPACIFY(STCMGBR),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STCMVBR,   0,                          "0100nnnn00100011", &NAMESPACIFY(STCMVBR),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STCMSSR,   0,                          "0100nnnn00110011", &NAMESPACIFY(STCMSSR),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STCMSPC,   0,                          "0100nnnn01000011", &NAMESPACIFY(STCMSPC),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STCMSGR,   0,                          "0100nnnn00110010", &NAMESPACIFY(STCMSGR),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STCMDBR,   0,                          "0100nnnn11110010", &NAMESPACIFY(STCMDBR),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STCMRBANK, 0,                          "0100nnnn1mmm0011", &NAMESPACIFY(STCMRBANK), 0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STSMACH,   0,                          "0000nnnn00001010", &NAMESPACIFY(STSMACH),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STSMACL,   0,                          "0000nnnn00011010", &NAMESPACIFY(STSMACL),   0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STSPR,     0,                          "0000nnnn00101010", &NAMESPACIFY(STSPR),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STSMMACH,  0,                          "0100nnnn00000010", &NAMESPACIFY(STSMMACH),  0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STSMMACL,  0,                          "0100nnnn00010010", &NAMESPACIFY(STSMMACL),  0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_STSMPR,    0,                          "0100nnnn00100010", &NAMESPACIFY(STSMPR),    0, 0, 0, 0, 0, 0, 0, 0, 0 },
  { OP_TRAPA,     0,                          "11000011iiiiiiii", &NAMESPACIFY(TRAPA),     0, 0, 0, 0, 0, 0, 0, 0, 0 },
};
// clang-format on

INSTR_TYPE_T *instr_lookup[UINT16_MAX];

//
// extract mask and shift information for a particular argument from the
// instruction signature
//
static void ARG_MASK(const char *sig, char arg, uint16_t *mask,
                     uint16_t *shift) {
  int len = (int)strlen(sig);
  if (mask) {
    *mask = 0;
  }
  if (shift) {
    *shift = 0;
  }
  for (int i = 0; i < len; i++) {
    if ((!arg && sig[i] == '1') || (arg && sig[i] == arg)) {
      if (mask) {
        *mask |= (1 << (len - i - 1));
      }
      if (shift) {
        *shift = (uint16_t)(len - i - 1);
      }
    }
  }
}

void INIT_TABLES() {
  static int initialized = 0;

  if (initialized) {
    return;
  }

  initialized = 1;

  // extract argument encoding information from signature
  for (int i = 0; i < NUM_OPCODES; i++) {
    INSTR_TYPE_T *type = &instr_type[i];
    ARG_MASK(type->sig, 'i', &type->imm_mask, &type->imm_shift);
    ARG_MASK(type->sig, 'd', &type->disp_mask, &type->disp_shift);
    ARG_MASK(type->sig, 'm', &type->rm_mask, &type->rm_shift);
    ARG_MASK(type->sig, 'n', &type->rn_mask, &type->rn_shift);
    ARG_MASK(type->sig, 0, &type->opcode_mask, NULL);
  }

  // initialize lookup table
  memset(instr_lookup, 0, sizeof(instr_lookup));

  for (int w = 0; w < 0x10000; w += 0x1000) {
    for (int x = 0; x < 0x1000; x += 0x100) {
      for (int y = 0; y < 0x100; y += 0x10) {
        for (int z = 0; z < 0x10; z++) {
          uint16_t value = w + x + y + z;

          for (int i = 0; i < NUM_OPCODES; i++) {
            INSTR_TYPE_T *type = &instr_type[i];
            uint16_t arg_mask = type->imm_mask | type->disp_mask |
                                type->rm_mask | type->rn_mask;

            if ((value & ~arg_mask) == type->opcode_mask) {
              instr_lookup[value] = type;
              break;
            }
          }
        }
      }
    }
  }
}

int DECODE(uint16_t data, INSTR_T *instr) {
  INSTR_TYPE_T *type = instr_lookup[data];

  if (!type) {
    return 0;
  }

  instr->type = type;
  instr->rm = (data & type->rm_mask) >> type->rm_shift;
  instr->rn = (data & type->rn_mask) >> type->rn_shift;
  instr->disp = (data & type->disp_mask) >> type->disp_shift;
  instr->imm = (data & type->imm_mask) >> type->imm_shift;

  return 1;
}

#endif
