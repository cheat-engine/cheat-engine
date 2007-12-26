unit Assemblerunit;

interface

uses windows,sysutils,imagehlp;

const opcodecount=1048; //I wish there was a easier way than to handcount

const invalidtoken=0;
const register8bit=1;
const register16bit=2;
const register32bit=3;
const registerMM=4;
const registerXMM=5;
const registerST=6;
const registerSreg=7;
const registerCR=8;
const registerDR=9;
const memorylocation=10;  //in case I cant find out (user forgot to say byte ptr, word ptr or dword ptr)`
const memorylocation8=11;
const memorylocation16=12;
const memorylocation32=13;
const memorylocation64=14;
const memorylocation80=15;
const memorylocation128=16;
const value=17;

//opcode part (bytes)
type Textraopcode=(eo_none,
                   eo_reg0,eo_reg1,eo_reg2,eo_reg3,eo_reg4,eo_reg5,eo_reg6,eo_reg7, // /digit
                   eo_reg, //  /r
                   eo_cb,eo_cw,eo_cd,eo_cp,
                   eo_ib,eo_iw,eo_id,
                   eo_prb,eo_prw,eo_prd,
                   eo_pi
                  );


//parameter part
type tparam=(par_noparam,
             //constant
             par_1,
             par_3,
             par_al,
             par_ax,
             par_eax,
             par_cl,
             par_dx,
             par_cs,
             par_ds,
             par_es,
             par_ss,
             par_fs,
             par_gs,
             //regs
             par_r8,
             par_r16,
             par_r32,
             par_mm,
             par_xmm,
             par_st,
             par_st0,
             par_sreg,
             par_cr,
             par_dr,
             //memorylocs
             par_m8,
             par_m16,
             par_m32,
             par_m64,
             par_m80,
             par_m128,
             par_moffs8,
             par_moffs16,
             par_moffs32,
             //regs+memorylocs
             par_rm8,
             par_rm16,
             par_rm32,
             par_r32_m16,
             par_mm_m32,
             par_mm_m64,
             par_xmm_m32,
             par_xmm_m64,
             par_xmm_m128,

            //values
             par_imm8,
             par_imm16,
             par_imm32,
             //relatives
             par_rel8,
             par_rel16,
             par_rel32);

type topcode=record
  mnemonic: string;
  opcode1,opcode2: textraopcode;
  paramtype1,paramtype2,paramtype3: tparam;
  bytes:byte;
  bt1,bt2,bt3: byte;
end;


const opcodes: array [1..opcodecount] of topcode =(
{ok}  (mnemonic:'AAA';opcode1:eo_none;paramtype1:par_noparam;bytes:1;bt1:$37), //no param
{ok}  (mnemonic:'AAD';opcode1:eo_none;paramtype1:par_noparam;bytes:2;bt1:$d5;bt2:$0a),
{ok}  (mnemonic:'AAD';opcode1:eo_ib;paramtype1:par_imm8;bytes:1;bt1:$d5),
{ok}  (mnemonic:'AAM';opcode1:eo_none;paramtype1:par_noparam;bytes:2;bt1:$d4;bt2:$0a),
{ok}  (mnemonic:'AAM';opcode1:eo_ib;paramtype1:par_imm8;bytes:1;bt1:$d4),
{ok}  (mnemonic:'AAS';opcode1:eo_none;paramtype1:par_noparam;bytes:1;bt1:$3F),
{ok}  (mnemonic:'ADC';opcode1:eo_ib;paramtype1:par_AL;paramtype2:par_imm8;bytes:1;bt1:$14),
{ok}  (mnemonic:'ADC';opcode1:eo_iw;paramtype1:par_AX;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$15),
{ok}  (mnemonic:'ADC';opcode1:eo_id;paramtype1:par_EAX;paramtype2:par_imm32;bytes:1;bt1:$15),
{ok}  (mnemonic:'ADC';opcode1:eo_reg2;opcode2:eo_ib;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$80),//verified
  (mnemonic:'ADC';opcode1:eo_reg2;opcode2:eo_iw;paramtype1:par_rm16;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$81),
  (mnemonic:'ADC';opcode1:eo_reg2;opcode2:eo_id;paramtype1:par_rm32;paramtype2:par_imm32;bytes:1;bt1:$81),
  (mnemonic:'ADC';opcode1:eo_reg2;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$83),
  (mnemonic:'ADC';opcode1:eo_reg2;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:1;bt1:$83),
  (mnemonic:'ADC';opcode1:eo_reg;paramtype1:par_rm8;paramtype2:par_r8;bytes:1;bt1:$10),
  (mnemonic:'ADC';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:2;bt1:$66;bt2:$11),
  (mnemonic:'ADC';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:1;bt1:$11),
  (mnemonic:'ADC';opcode1:eo_reg;paramtype1:par_r8;paramtype2:par_rm8;bytes:1;bt1:$12),
  (mnemonic:'ADC';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:2;bt1:$66;bt2:$13),
  (mnemonic:'ADC';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:1;bt1:$13),

  (mnemonic:'ADD';opcode1:eo_ib;paramtype1:par_AL;paramtype2:par_imm8;bytes:1;bt1:$04),
  (mnemonic:'ADD';opcode1:eo_iw;paramtype1:par_AX;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$05),
  (mnemonic:'ADD';opcode1:eo_id;paramtype1:par_EAX;paramtype2:par_imm32;bytes:1;bt1:$05),
  (mnemonic:'ADD';opcode1:eo_reg0;opcode2:eo_ib;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$80),
  (mnemonic:'ADD';opcode1:eo_reg0;opcode2:eo_iw;paramtype1:par_rm16;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$80),
  (mnemonic:'ADD';opcode1:eo_reg0;opcode2:eo_id;paramtype1:par_rm32;paramtype2:par_imm32;bytes:1;bt1:$81),
  (mnemonic:'ADD';opcode1:eo_reg0;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$83),
  (mnemonic:'ADD';opcode1:eo_reg0;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:1;bt1:$83),
  (mnemonic:'ADD';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:1;bt1:$01),
  (mnemonic:'ADD';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:2;bt1:$66;bt2:$01),
  (mnemonic:'ADD';opcode1:eo_reg;paramtype1:par_rm8;paramtype2:par_r8;bytes:1;bt1:$00),
  (mnemonic:'ADD';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:1;bt1:$03),
  (mnemonic:'ADD';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:2;bt1:$66;bt2:$03),
  (mnemonic:'ADD';opcode1:eo_reg;paramtype1:par_r8;paramtype2:par_rm8;bytes:1;bt1:$02),

  (mnemonic:'ADDPD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$58), //should be xmm1,xmm2/m128 but is also handled in all the others, in fact all other modrm types have it, hmmmmm....
  (mnemonic:'ADDPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$58), //I gues all reg,reg/mem can be handled like this. (oh well, i'm too lazy to change the code)
  (mnemonic:'ADDSD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m64;bytes:3;bt1:$f2;bt2:$0f;bt3:$58),
  (mnemonic:'ADDSS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m32;bytes:3;bt1:$f3;bt2:$0f;bt3:$58),

  (mnemonic:'AND';opcode1:eo_ib;paramtype1:par_AL;paramtype2:par_imm8;bytes:1;bt1:$24),
  (mnemonic:'AND';opcode1:eo_iw;paramtype1:par_AX;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$25),
  (mnemonic:'AND';opcode1:eo_id;paramtype1:par_EAX;paramtype2:par_imm32;bytes:1;bt1:$25),
  (mnemonic:'AND';opcode1:eo_reg4;opcode2:eo_ib;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$80),
  (mnemonic:'AND';opcode1:eo_reg4;opcode2:eo_iw;paramtype1:par_rm16;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$80),
  (mnemonic:'AND';opcode1:eo_reg4;opcode2:eo_id;paramtype1:par_rm32;paramtype2:par_imm32;bytes:1;bt1:$81),
  (mnemonic:'AND';opcode1:eo_reg4;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$83),
  (mnemonic:'AND';opcode1:eo_reg4;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:1;bt1:$83),
  (mnemonic:'AND';opcode1:eo_reg;paramtype1:par_rm8;paramtype2:par_r8;bytes:1;bt1:$20),
  (mnemonic:'AND';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:2;bt1:$66;bt2:$21),
  (mnemonic:'AND';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:1;bt1:$21),
  (mnemonic:'AND';opcode1:eo_reg;paramtype1:par_r8;paramtype2:par_rm8;bytes:1;bt1:$22),
  (mnemonic:'AND';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:2;bt1:$66;bt2:$23),
  (mnemonic:'AND';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:1;bt1:$23),

  (mnemonic:'ANDNPD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$ff),
  (mnemonic:'ANDNPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$55),

  (mnemonic:'ANDPD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$54),
  (mnemonic:'ANDPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$54),

  (mnemonic:'ARPL';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:1;bt1:$63), //eo_reg means I just need to find the reg and address
  (mnemonic:'BOUND';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:2;bt1:$66;bt2:$62),
  (mnemonic:'BOUND';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:1;bt1:$62),
  (mnemonic:'BSF';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$bc),
  (mnemonic:'BSF';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$bc),
  (mnemonic:'BSR';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$bd),
  (mnemonic:'BSR';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$bd),
  (mnemonic:'BSWAP';opcode1:eo_prd;paramtype1:par_r32;bytes:2;bt1:$0f;bt2:$c8), //eo_prd

  (mnemonic:'BT';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:3;bt1:$66;bt2:$0f;bt3:$a3),
  (mnemonic:'BT';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:2;bt1:$0f;bt2:$a3),
  (mnemonic:'BT';opcode1:eo_reg4;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$ba),
  (mnemonic:'BT';opcode1:eo_reg4;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:2;bt1:$0f;bt2:$ba),

  (mnemonic:'BTC';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:3;bt1:$66;bt2:$0f;bt3:$bb),
  (mnemonic:'BTC';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:2;bt1:$0f;bt2:$bb),
  (mnemonic:'BTC';opcode1:eo_reg7;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$ba),
  (mnemonic:'BTC';opcode1:eo_reg7;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:2;bt1:$0f;bt2:$ba),

  (mnemonic:'BTR';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:3;bt1:$66;bt2:$0f;bt3:$b3),
  (mnemonic:'BTR';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:2;bt1:$0f;bt2:$b3),
  (mnemonic:'BTR';opcode1:eo_reg6;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$ba),
  (mnemonic:'BTR';opcode1:eo_reg6;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:2;bt1:$0f;bt2:$ba),

  (mnemonic:'BTS';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:3;bt1:$66;bt2:$0f;bt3:$ab),
  (mnemonic:'BTS';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:2;bt1:$0f;bt2:$ab),
  (mnemonic:'BTS';opcode1:eo_reg5;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$ba),
  (mnemonic:'BTS';opcode1:eo_reg5;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:2;bt1:$0f;bt2:$ba),
  //no $66 $E8 because it makes the address it jumps to 16 bit
  (mnemonic:'CALL';opcode1:eo_cd;paramtype1:par_rel32;bytes:1;bt1:$e8),
  //also no $66 $ff /2
  (mnemonic:'CALL';opcode1:eo_reg2;paramtype1:par_rm32;bytes:1;bt1:$ff),
  (mnemonic:'CBW';opcode1:eo_none;paramtype1:par_noparam;bytes:2;bt1:$66;bt2:$98),
  (mnemonic:'CDQ';bytes:1;bt1:$99),
  (mnemonic:'CLC';bytes:1;bt1:$f8),
  (mnemonic:'CLD';bytes:1;bt1:$f8),
  (mnemonic:'CLFLUSH';opcode1:eo_reg7;paramtype1:par_m8;bytes:2;bt1:$0f;bt2:$ae),
  (mnemonic:'CLI';bytes:1;bt1:$fa),
  (mnemonic:'CLTS';bytes:2;bt1:$0f;bt2:$06),
  (mnemonic:'CMC';bytes:1;bt1:$f5),
  (mnemonic:'CMOVA';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$47),
  (mnemonic:'CMOVA';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$47),
  (mnemonic:'CMOVAE';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$43),
  (mnemonic:'CMOVAE';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$43),
  (mnemonic:'CMOVB';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$42),
  (mnemonic:'CMOVB';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$42),
  (mnemonic:'CMOVBE';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$46),
  (mnemonic:'CMOVBE';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$46),
  (mnemonic:'CMOVC';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$42),
  (mnemonic:'CMOVC';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$42),
  (mnemonic:'CMOVE';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$44),
  (mnemonic:'CMOVE';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$44),
  (mnemonic:'CMOVG';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$4f),
  (mnemonic:'CMOVG';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$4f),
  (mnemonic:'CMOVGE';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$4d),
  (mnemonic:'CMOVGE';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$4d),
  (mnemonic:'CMOVL';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$4c),
  (mnemonic:'CMOVL';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$4c),
  (mnemonic:'CMOVLE';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$4e),
  (mnemonic:'CMOVLE';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$4e),
  (mnemonic:'CMOVNA';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$46),
  (mnemonic:'CMOVNA';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$46),
  (mnemonic:'CMOVNAE';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$42),
  (mnemonic:'CMOVNAE';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$42),
  (mnemonic:'CMOVNB';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$43),
  (mnemonic:'CMOVNB';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$43),
  (mnemonic:'CMOVNBE';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$47),
  (mnemonic:'CMOVNBE';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$47),
  (mnemonic:'CMOVNC';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$43),
  (mnemonic:'CMOVNC';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$43),
  (mnemonic:'CMOVNE';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$45),
  (mnemonic:'CMOVNE';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$45),
  (mnemonic:'CMOVNG';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$4e),
  (mnemonic:'CMOVNG';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$4e),
  (mnemonic:'CMOVNGE';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$4c),
  (mnemonic:'CMOVNGE';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$4c),
  (mnemonic:'CMOVNL';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$4d),
  (mnemonic:'CMOVNL';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$4d),
  (mnemonic:'CMOVNLE';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$4f),
  (mnemonic:'CMOVNLE';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$4f),
  (mnemonic:'CMOVNO';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$41),
  (mnemonic:'CMOVNO';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$41),
  (mnemonic:'CMOVNP';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$4b),
  (mnemonic:'CMOVNP';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$4b),
  (mnemonic:'CMOVNS';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$49),
  (mnemonic:'CMOVNS';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$49),
  (mnemonic:'CMOVNZ';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$45),
  (mnemonic:'CMOVNZ';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$45),
  (mnemonic:'CMOVO';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$40),
  (mnemonic:'CMOVO';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$40),
  (mnemonic:'CMOVP';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$4a),
  (mnemonic:'CMOVP';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$4a),
  (mnemonic:'CMOVPE';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$4a),
  (mnemonic:'CMOVPE';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$4a),
  (mnemonic:'CMOVPO';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$4b),
  (mnemonic:'CMOVPO';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$4b),
  (mnemonic:'CMOVS';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$48),
  (mnemonic:'CMOVS';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$48),
  (mnemonic:'CMOVZ';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$44),
  (mnemonic:'CMOVZ';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$44),

  (mnemonic:'CMP';opcode1:eo_ib;paramtype1:par_AL;paramtype2:par_imm8;bytes:1;bt1:$3C),
  (mnemonic:'CMP';opcode1:eo_iw;paramtype1:par_AX;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$3D),
  (mnemonic:'CMP';opcode1:eo_id;paramtype1:par_EAX;paramtype2:par_imm32;bytes:1;bt1:$3D),
  (mnemonic:'CMP';opcode1:eo_reg7;opcode2:eo_ib;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$80),
  (mnemonic:'CMP';opcode1:eo_reg7;opcode2:eo_iw;paramtype1:par_rm16;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$81),
  (mnemonic:'CMP';opcode1:eo_reg7;opcode2:eo_id;paramtype1:par_rm32;paramtype2:par_imm32;bytes:1;bt1:$81),
  (mnemonic:'CMP';opcode1:eo_reg7;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$83),
  (mnemonic:'CMP';opcode1:eo_reg7;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:1;bt1:$83),
  (mnemonic:'CMP';opcode1:eo_reg;paramtype1:par_rm8;paramtype2:par_r8;bytes:1;bt1:$38),
  (mnemonic:'CMP';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:2;bt1:$66;bt2:$39),
  (mnemonic:'CMP';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:1;bt1:$39),
  (mnemonic:'CMP';opcode1:eo_reg;paramtype1:par_r8;paramtype2:par_rm8;bytes:1;bt1:$3A),
  (mnemonic:'CMP';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:2;bt1:$66;bt2:$3B),
  (mnemonic:'CMP';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:1;bt1:$3B),

  (mnemonic:'CMPPD';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_xmm_m128;paramtype3:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$c2),
  (mnemonic:'CMPPS';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_xmm_m128;paramtype3:par_imm8;bytes:2;bt1:$0f;bt2:$c2),

  (mnemonic:'CMPSB';bytes:1;bt1:$a6),
  (mnemonic:'CMPSD';bytes:1;bt1:$a6),
  (mnemonic:'CMPSD';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_xmm_m64;paramtype3:par_imm8;bytes:3;bt1:$f2;bt2:$0f;bt3:$c2),
  (mnemonic:'CMPSS';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_xmm_m32;paramtype3:par_imm8;bytes:3;bt1:$f3;bt2:$0f;bt3:$c2),
  (mnemonic:'CMPSW';bytes:2;bt1:$66;bt2:$a6),
  (mnemonic:'CMPXCHG';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:2;bt1:$0f;bt2:$b0),
  (mnemonic:'CMPXCHG';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:2;bt1:$66;bt2:$0f;bt3:$b1),
  (mnemonic:'CMPXCHG';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:2;bt1:$0f;bt2:$b1),
  (mnemonic:'CMPXCHG8B';opcode1:eo_reg1;paramtype1:par_m64;bytes:2;bt1:$0f;bt2:$c7), //no m64 as eo, seems it's just a /1

  (mnemonic:'COMISD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m64;bytes:3;bt1:$66;bt2:$0f;bt3:$2f),
  (mnemonic:'COMISS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m32;bytes:2;bt1:$0f;bt2:$2f),

  (mnemonic:'CPUID';bytes:2;bt1:$0f;bt2:$a2),
  (mnemonic:'CVTDQ2PD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m64;bytes:3;bt1:$f3;bt2:$0f;bt3:$e6),  //just a gues, the documentation didn't say anything about a /r, and the disassembler of delphi also doesn't recognize it
  (mnemonic:'CVTDQ2PS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$5b),
  (mnemonic:'CVTPD2DQ';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$f2;bt2:$0f;bt3:$e6),
  (mnemonic:'CVTPD2PI';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$2d),

  (mnemonic:'CVTPD2PS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$5a),
  (mnemonic:'CVTPI2PD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_mm_m64;bytes:3;bt1:$66;bt2:$0f;bt3:$2a),
  (mnemonic:'CVTPI2PS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$2a),
  (mnemonic:'CVTPS2DQ';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$5b),

  (mnemonic:'CVTPS2PD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m64;bytes:2;bt1:$0f;bt2:$5a),
  (mnemonic:'CVTPS2PI';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_xmm_m64;bytes:2;bt1:$0f;bt2:$2d),
  (mnemonic:'CVTSD2SI';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_xmm_m64;bytes:3;bt1:$f2;bt2:$0f;bt3:$2d),
  (mnemonic:'CVTSD2SS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m64;bytes:3;bt1:$f2;bt2:$0f;bt3:$5a),
  (mnemonic:'CVTSI2SD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_rm32;bytes:3;bt1:$f2;bt2:$0f;bt3:$2a),
  (mnemonic:'CVTSI2SS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_rm32;bytes:3;bt1:$f3;bt2:$0f;bt3:$2a),

  (mnemonic:'CVTSS2SD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m32;bytes:3;bt1:$f3;bt2:$0f;bt3:$5a),
  (mnemonic:'CVTSS2SI';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_xmm_m32;bytes:3;bt1:$f3;bt2:$0f;bt3:$2d),

  (mnemonic:'CVTTPD2DQ';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$e6),
  (mnemonic:'CVTTPD2PI';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$2c),

  (mnemonic:'CVTTPS2PI';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_xmm_m64;bytes:3;bt1:$0f;bt2:$2c),
  (mnemonic:'CVTTSD2SI';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_xmm_m64;bytes:3;bt1:$f2;bt2:$0f;bt3:$2c),
  (mnemonic:'CVTTSS2SI';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_xmm_m64;bytes:3;bt1:$f3;bt2:$0f;bt3:$2c),

  (mnemonic:'CWD';bytes:1;bt1:$99),
  (mnemonic:'CWDE';opcode1:eo_none;paramtype1:par_noparam;bytes:1;bt1:$98),
  (mnemonic:'DAA';bytes:1;bt1:$27),
  (mnemonic:'DAS';bytes:1;bt1:$2F),
  (mnemonic:'DEC';opcode1:eo_reg1;paramtype1:par_rm8;bytes:1;bt1:$fe),
  (mnemonic:'DEC';opcode1:eo_reg1;paramtype1:par_m16;bytes:2;bt1:$66;bt2:$ff),
  (mnemonic:'DEC';opcode1:eo_reg1;paramtype1:par_m32;bytes:1;bt1:$ff),
  (mnemonic:'DEC';opcode1:eo_prw;paramtype1:par_r16;bytes:2;bt1:$66;bt2:$48),
  (mnemonic:'DEC';opcode1:eo_prd;paramtype1:par_r32;bytes:1;bt1:$48),
  (mnemonic:'DIV';opcode1:eo_reg6;paramtype1:par_rm8;bytes:1;bt1:$f6),
  (mnemonic:'DIV';opcode1:eo_reg6;paramtype1:par_rm16;bytes:2;bt1:$66;bt2:$f7),
  (mnemonic:'DIV';opcode1:eo_reg6;paramtype1:par_rm32;bytes:1;bt1:$f7),
  (mnemonic:'DIVPD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$5e),
  (mnemonic:'DIVPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$5e),
  (mnemonic:'DIVSD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m64;bytes:3;bt1:$f2;bt2:$0f;bt3:$5e),
  (mnemonic:'DIVSS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m32;bytes:3;bt1:$f3;bt2:$0f;bt3:$5e),
  (mnemonic:'EMMS';bytes:2;bt1:$0f;bt2:$77),
  (mnemonic:'ENTER';opcode1:eo_iw;opcode2:eo_ib;paramtype1:par_imm16;paramtype2:par_imm8;bytes:1;bt1:$c8),
  (mnemonic:'F2XM1';bytes:2;bt1:$d9;bt2:$f0),
  (mnemonic:'FABS';bytes:2;bt1:$d9;bt2:$e1),
  (mnemonic:'FADD';opcode1:eo_reg0;paramtype1:par_m32;bytes:1;bt1:$d8),
  (mnemonic:'FADD';opcode1:eo_reg0;paramtype1:par_m64;bytes:1;bt1:$dc),
  (mnemonic:'FADD';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$d8;bt2:$c0),
  (mnemonic:'FADD';opcode1:eo_pi;paramtype1:par_st;paramtype2:par_st0;bytes:2;bt1:$dc;bt2:$c0),
  (mnemonic:'FADDP';opcode1:eo_pi;paramtype1:par_st;paramtype2:par_st0;bytes:2;bt1:$de;bt2:$c0),
  (mnemonic:'FADDP';opcode1:eo_pi;paramtype1:par_st;bytes:2;bt1:$de;bt2:$c0),
  (mnemonic:'FADDP';bytes:2;bt1:$de;bt2:$c1),

  (mnemonic:'FBLD';opcode1:eo_reg4;paramtype1:par_m80;bytes:1;bt1:$df),
  (mnemonic:'FBSTP';opcode1:eo_reg6;paramtype1:par_m80;bytes:1;bt1:$df),
  (mnemonic:'FCHS';bytes:2;bt1:$D9;bt2:$e0),
  (mnemonic:'FCLEX';bytes:3;bt1:$9b;bt2:$db;bt3:$e2),
  (mnemonic:'FCMOVB';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$DA;bt2:$c0),
  (mnemonic:'FCMOVBE';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$DA;bt2:$d0),
  (mnemonic:'FCMOVE';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$DA;bt2:$c8),
  (mnemonic:'FCMOVNB';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$DB;bt2:$c0),
  (mnemonic:'FCMOVNBE';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$DB;bt2:$d0),
  (mnemonic:'FCMOVNE';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$DB;bt2:$c8),
  (mnemonic:'FCMOVNU';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$DB;bt2:$d8),
  (mnemonic:'FCMOVU';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$DA;bt2:$d8),
  (mnemonic:'FCOM';opcode1:eo_reg2;paramtype1:par_m32;bytes:1;bt1:$d8),
  (mnemonic:'FCOM';opcode1:eo_reg2;paramtype1:par_m64;bytes:1;bt1:$dc),
  (mnemonic:'FCOM';opcode1:eo_pi;paramtype1:par_st;bytes:2;bt1:$d8;bt2:$d0),
  (mnemonic:'FCOM';bytes:2;bt1:$d8;bt2:$d1),
  (mnemonic:'FCOMP';opcode1:eo_reg3;paramtype1:par_m32;bytes:1;bt1:$d8),
  (mnemonic:'FCOMP';opcode1:eo_reg3;paramtype1:par_m64;bytes:1;bt1:$dc),
  (mnemonic:'FCOMP';opcode1:eo_pi;paramtype1:par_st;bytes:2;bt1:$d8;bt2:$d8),
  (mnemonic:'FCOMP';bytes:2;bt1:$d8;bt2:$d9),
  (mnemonic:'FCOMI';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$db;bt2:$f0),
  (mnemonic:'FCOMIP';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$df;bt2:$f0),

  (mnemonic:'FCOMPP';bytes:2;bt1:$de;bt2:$d9),

  (mnemonic:'FCOMPP';bytes:2;bt1:$de;bt2:$d9),
  (mnemonic:'FCOS';bytes:2;bt1:$D9;bt2:$ff),

  (mnemonic:'FDECSTP';bytes:2;bt1:$d9;bt2:$f6),

  (mnemonic:'FDIV';opcode1:eo_reg6;paramtype1:par_m32;bytes:1;bt1:$d8),
  (mnemonic:'FDIV';opcode1:eo_reg6;paramtype1:par_m64;bytes:1;bt1:$dc),
  (mnemonic:'FDIV';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$d8;bt2:$f0),
  (mnemonic:'FDIV';opcode1:eo_pi;paramtype1:par_st;paramtype2:par_st0;bytes:2;bt1:$dc;bt2:$f8),
  (mnemonic:'FDIVP';opcode1:eo_pi;paramtype1:par_st;paramtype2:par_st0;bytes:2;bt1:$de;bt2:$f8),
  (mnemonic:'FDIVP';bytes:2;bt1:$de;bt2:$f9),
  (mnemonic:'FDIVR';opcode1:eo_reg7;paramtype1:par_m32;bytes:1;bt1:$d8),
  (mnemonic:'FDIVR';opcode1:eo_reg7;paramtype1:par_m64;bytes:1;bt1:$dc),
  (mnemonic:'FDIVR';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$d8;bt2:$f8),
  (mnemonic:'FDIVR';opcode1:eo_pi;paramtype1:par_st;paramtype2:par_st0;bytes:2;bt1:$dc;bt2:$f0),
  (mnemonic:'FDIVRP';opcode1:eo_pi;paramtype1:par_st;paramtype2:par_st0;bytes:2;bt1:$de;bt2:$f0),
  (mnemonic:'FDIVRP';bytes:2;bt1:$de;bt2:$f1),
  (mnemonic:'FFREE';opcode1:eo_pi;paramtype1:par_st;bytes:2;bt1:$dd;bt2:$c0),

  (mnemonic:'FIADD';opcode1:eo_reg0;paramtype1:par_m32;bytes:1;bt1:$DA),
  (mnemonic:'FIADD';opcode1:eo_reg0;paramtype1:par_m16;bytes:1;bt1:$DE),

  (mnemonic:'FICOM';opcode1:eo_reg2;paramtype1:par_m32;bytes:1;bt1:$da),
  (mnemonic:'FICOM';opcode1:eo_reg2;paramtype1:par_m16;bytes:1;bt1:$de),
  (mnemonic:'FICOMP';opcode1:eo_reg3;paramtype1:par_m32;bytes:1;bt1:$da),
  (mnemonic:'FICOMP';opcode1:eo_reg3;paramtype1:par_m16;bytes:1;bt1:$de),

  (mnemonic:'FIDIV';opcode1:eo_reg6;paramtype1:par_m32;bytes:1;bt1:$da),
  (mnemonic:'FIDIV';opcode1:eo_reg6;paramtype1:par_m16;bytes:1;bt1:$de),

  (mnemonic:'FIDIVR';opcode1:eo_reg7;paramtype1:par_m32;bytes:1;bt1:$da),
  (mnemonic:'FIDIVR';opcode1:eo_reg7;paramtype1:par_m16;bytes:1;bt1:$de),


  (mnemonic:'FILD';opcode1:eo_reg0;paramtype1:par_m16;bytes:1;bt1:$df), //(I would have chosen to put 32 first, but I gues delphi used the same documentation as I did, cause it choose 16 as default)
  (mnemonic:'FILD';opcode1:eo_reg0;paramtype1:par_m32;bytes:1;bt1:$db),
  (mnemonic:'FILD';opcode1:eo_reg5;paramtype1:par_m64;bytes:1;bt1:$df),

  (mnemonic:'FIMUL';opcode1:eo_reg1;paramtype1:par_m32;bytes:1;bt1:$da),
  (mnemonic:'FIMUL';opcode1:eo_reg1;paramtype1:par_m16;bytes:1;bt1:$de),

  (mnemonic:'FINCSTP';bytes:2;bt1:$d9;bt2:$f7),
  (mnemonic:'FINIT';bytes:3;bt1:$9b;bt2:$db;bt3:$e3),

  (mnemonic:'FIST';opcode1:eo_reg2;paramtype1:par_m32;bytes:1;bt1:$df),
  (mnemonic:'FIST';opcode1:eo_reg2;paramtype1:par_m16;bytes:1;bt1:$db),
  (mnemonic:'FISTP';opcode1:eo_reg3;paramtype1:par_m16;bytes:1;bt1:$df),
  (mnemonic:'FISTP';opcode1:eo_reg3;paramtype1:par_m32;bytes:1;bt1:$db),
  (mnemonic:'FISTP';opcode1:eo_reg7;paramtype1:par_m64;bytes:1;bt1:$df),

  (mnemonic:'FISUB';opcode1:eo_reg4;paramtype1:par_m32;bytes:1;bt1:$da),
  (mnemonic:'FISUB';opcode1:eo_reg4;paramtype1:par_m16;bytes:1;bt1:$de),
  (mnemonic:'FISUBR';opcode1:eo_reg5;paramtype1:par_m32;bytes:1;bt1:$da),
  (mnemonic:'FISUBR';opcode1:eo_reg5;paramtype1:par_m16;bytes:1;bt1:$de),

  (mnemonic:'FLD';opcode1:eo_reg0;paramtype1:par_m32;bytes:1;bt1:$d9),
  (mnemonic:'FLD';opcode1:eo_reg0;paramtype1:par_m64;bytes:1;bt1:$dd),
  (mnemonic:'FLD';opcode1:eo_reg5;paramtype1:par_m80;bytes:1;bt1:$db),
  (mnemonic:'FLD';opcode1:eo_pi;paramtype1:par_st;bytes:2;bt1:$d9;bt2:$c0),

  (mnemonic:'FLD1';bytes:2;bt1:$d9;bt2:$e8),
  (mnemonic:'FLDCW';opcode1:eo_reg5;paramtype1:par_m16;bytes:1;bt1:$d9),
  (mnemonic:'FLDENV';opcode1:eo_reg4;paramtype1:par_m32;bytes:1;bt1:$d9),
  (mnemonic:'FLDL2E';bytes:2;bt1:$d9;bt2:$e8),
  (mnemonic:'FLDL2T';bytes:2;bt1:$d9;bt2:$e8),
  (mnemonic:'FLDLG2';bytes:2;bt1:$d9;bt2:$e8),
  (mnemonic:'FLDLN2';bytes:2;bt1:$d9;bt2:$e8),
  (mnemonic:'FLDPI';bytes:2;bt1:$d9;bt2:$e8),
  (mnemonic:'FLDZ';bytes:2;bt1:$d9;bt2:$e8),

  (mnemonic:'FMUL';opcode1:eo_reg1;paramtype1:par_m32;bytes:1;bt1:$d8),
  (mnemonic:'FMUL';opcode1:eo_reg1;paramtype1:par_m64;bytes:1;bt1:$dc),
  (mnemonic:'FMUL';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$d8;bt2:$C8),
  (mnemonic:'FMUL';opcode1:eo_pi;paramtype1:par_st;paramtype2:par_st0;bytes:2;bt1:$dc;bt2:$C8),
  (mnemonic:'FMULP';opcode1:eo_pi;paramtype1:par_st;paramtype2:par_st0;bytes:2;bt1:$de;bt2:$C8),
  (mnemonic:'FMULP';bytes:2;bt1:$de;bt2:$c9),


  (mnemonic:'FNINIT';bytes:2;bt1:$db;bt2:$e3),
  (mnemonic:'FNLEX';bytes:2;bt1:$Db;bt2:$e2),
  (mnemonic:'FNOP';bytes:2;bt1:$d6;bt2:$d0),
  (mnemonic:'FNSAVE';opcode1:eo_reg6;paramtype1:par_m32;bytes:1;bt1:$dd),

  (mnemonic:'FNSTCW';opcode1:eo_reg7;paramtype1:par_m16;bytes:1;bt1:$d9),
  (mnemonic:'FNSTENV';opcode1:eo_reg6;paramtype1:par_m32;bytes:1;bt1:$d9),

  (mnemonic:'FNSTSW';opcode1:eo_reg7;paramtype1:par_m16;bytes:1;bt1:$dd),
  (mnemonic:'FNSTSW';paramtype1:par_ax;bytes:3;bt1:$9b;bt2:$df;bt3:$df),


  (mnemonic:'FPATAN';bytes:2;bt1:$d9;bt2:$f3),
  (mnemonic:'FPREM';bytes:2;bt1:$d9;bt2:$f8),
  (mnemonic:'FPREM1';bytes:2;bt1:$d9;bt2:$f5),
  (mnemonic:'FPTAN';bytes:2;bt1:$d9;bt2:$f2),
  (mnemonic:'FRNDINT';bytes:2;bt1:$d9;bt2:$fc),
  (mnemonic:'FRSTOR';opcode1:eo_reg4;paramtype1:par_m32;bytes:1;bt1:$dd),

  (mnemonic:'FSAVE';opcode1:eo_reg6;paramtype1:par_m32;bytes:2;bt1:$9b;bt2:$dd),

  (mnemonic:'FSCALE';bytes:2;bt1:$d9;bt2:$fd),
  (mnemonic:'FSIN';bytes:2;bt1:$d9;bt2:$fe),
  (mnemonic:'FSINCOS';bytes:2;bt1:$d9;bt2:$fb),
  (mnemonic:'FSQRT';bytes:2;bt1:$d9;bt2:$fa),

  (mnemonic:'FST';opcode1:eo_reg2;paramtype1:par_m32;bytes:1;bt1:$d9),
  (mnemonic:'FST';opcode1:eo_reg2;paramtype1:par_m64;bytes:1;bt1:$dd),
  (mnemonic:'FST';opcode1:eo_pi;paramtype1:par_st;bytes:2;bt1:$dd;bt2:$d0),
  (mnemonic:'FSTCW';opcode1:eo_reg7;paramtype1:par_m16;bytes:2;bt1:$9b;bt2:$d9),
  (mnemonic:'FSTENV';opcode1:eo_reg6;paramtype1:par_m32;bytes:2;bt1:$9b;bt2:$d9),
  (mnemonic:'FSTP';opcode1:eo_reg3;paramtype1:par_m32;bytes:1;bt1:$d9),
  (mnemonic:'FSTP';opcode1:eo_reg3;paramtype1:par_m64;bytes:1;bt1:$dd),
  (mnemonic:'FSTP';opcode1:eo_reg7;paramtype1:par_m80;bytes:1;bt1:$db),
  (mnemonic:'FSTP';opcode1:eo_pi;paramtype1:par_st;bytes:2;bt1:$dd;bt2:$d8),


  (mnemonic:'FSTSW';opcode1:eo_reg7;paramtype1:par_m16;bytes:2;bt1:$9b;bt2:$dd),
  (mnemonic:'FSTSW';paramtype1:par_ax;bytes:3;bt1:$9b;bt2:$df;bt3:$e0),


  (mnemonic:'FSUB';opcode1:eo_reg4;paramtype1:par_m32;bytes:1;bt1:$d8),
  (mnemonic:'FSUB';opcode1:eo_reg4;paramtype1:par_m64;bytes:1;bt1:$dc),
  (mnemonic:'FSUB';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$d8;bt2:$e0),
  (mnemonic:'FSUB';opcode1:eo_pi;paramtype1:par_st;paramtype2:par_st0;bytes:2;bt1:$dc;bt2:$e8),
  (mnemonic:'FSUBP';opcode1:eo_pi;paramtype1:par_st;paramtype2:par_st0;bytes:2;bt1:$de;bt2:$e8),
  (mnemonic:'FSUBP';bytes:2;bt1:$de;bt2:$e9),
  (mnemonic:'FSUBPR';opcode1:eo_pi;paramtype1:par_st;paramtype2:par_st0;bytes:2;bt1:$de;bt2:$e0),
  (mnemonic:'FSUBPR';bytes:2;bt1:$de;bt2:$e1),
  (mnemonic:'FSUBR';opcode1:eo_reg5;paramtype1:par_m32;bytes:1;bt1:$d8),
  (mnemonic:'FSUBR';opcode1:eo_reg5;paramtype1:par_m64;bytes:1;bt1:$dc),
  (mnemonic:'FSUBR';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$d8;bt2:$e8),
  (mnemonic:'FSUBR';opcode1:eo_pi;paramtype1:par_st;paramtype2:par_st0;bytes:2;bt1:$dc;bt2:$e0),
  (mnemonic:'FTST';bytes:2;bt1:$d9;bt2:$e4),

  (mnemonic:'FUCOM';opcode1:eo_pi;paramtype1:par_st;bytes:2;bt1:$dd;bt2:$e0),
  (mnemonic:'FUCOM';bytes:2;bt1:$dd;bt2:$e1),
  (mnemonic:'FUCOMI';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$db;bt2:$e8),
  (mnemonic:'FUCOMIP';opcode1:eo_pi;paramtype1:par_st0;paramtype2:par_st;bytes:2;bt1:$df;bt2:$e8),
  (mnemonic:'FUCOMP';opcode1:eo_pi;paramtype1:par_st;bytes:2;bt1:$dd;bt2:$e8),
  (mnemonic:'FUCOMP';bytes:2;bt1:$dd;bt2:$e9),
  (mnemonic:'FUCOMPP';bytes:2;bt1:$da;bt2:$e9),

  (mnemonic:'FWAIT';bytes:1;bt1:$9b),
  
  (mnemonic:'FXAM';bytes:2;bt1:$d9;bt2:$e5),
  (mnemonic:'FXCH';opcode1:eo_pi;paramtype1:par_st;bytes:2;bt1:$d9;bt2:$c8),
  (mnemonic:'FXCH';bytes:2;bt1:$d9;bt2:$c9),
  (mnemonic:'FXRSTOR';opcode1:eo_reg1;paramtype1:par_m32;bytes:2;bt1:$0f;bt2:$ae),
  (mnemonic:'FXSAVE';opcode1:eo_reg0;paramtype1:par_m32;bytes:2;bt1:$0f;bt2:$ae),
  (mnemonic:'FXTRACT';bytes:2;bt1:$d9;bt2:$f4),
  (mnemonic:'FYL2X';bytes:2;bt1:$d9;bt2:$f1),
  (mnemonic:'FYL2XPI';bytes:2;bt1:$d9;bt2:$f9),

  (mnemonic:'HLT';bytes:1;bt1:$f4),

  (mnemonic:'IDIV';opcode1:eo_reg7;paramtype1:par_rm8;bytes:1;bt1:$f6),
  (mnemonic:'IDIV';opcode1:eo_reg7;paramtype1:par_rm16;bytes:2;bt1:$66;bt2:$f7),
  (mnemonic:'IDIV';opcode1:eo_reg7;paramtype1:par_rm32;bytes:1;bt1:$f7),


  (mnemonic:'IMUL';opcode1:eo_reg5;paramtype1:par_rm8;bytes:1;bt1:$f6),
  (mnemonic:'IMUL';opcode1:eo_reg5;paramtype1:par_rm16;bytes:2;bt1:$66;bt2:$f7),
  (mnemonic:'IMUL';opcode1:eo_reg5;paramtype1:par_rm32;bytes:1;bt1:$f7),

  (mnemonic:'IMUL';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$af),
  (mnemonic:'IMUL';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$af),

  (mnemonic:'IMUL';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_r16;paramtype2:par_rm16;paramtype3:par_imm8;bytes:2;bt1:$66;bt2:$6b),
  (mnemonic:'IMUL';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_r32;paramtype2:par_rm32;paramtype3:par_imm8;bytes:1;bt1:$6b),

  (mnemonic:'IMUL';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_r16;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$6b),
  (mnemonic:'IMUL';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_r32;paramtype3:par_imm8;bytes:1;bt1:$6b),

  (mnemonic:'IMUL';opcode1:eo_reg;opcode2:eo_iw;paramtype1:par_r16;paramtype2:par_rm16;paramtype3:par_imm16;bytes:2;bt1:$66;bt2:$69),
  (mnemonic:'IMUL';opcode1:eo_reg;opcode2:eo_id;paramtype1:par_r32;paramtype2:par_rm32;paramtype3:par_imm32;bytes:1;bt1:$69),

  (mnemonic:'IMUL';opcode1:eo_reg;opcode2:eo_iw;paramtype1:par_r16;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$69),
  (mnemonic:'IMUL';opcode1:eo_reg;opcode2:eo_id;paramtype1:par_r32;paramtype2:par_imm32;bytes:1;bt1:$69),

  (mnemonic:'IN';opcode1:eo_ib;paramtype1:par_al;paramtype2:par_imm8;bytes:1;bt1:$e4),
  (mnemonic:'IN';opcode1:eo_ib;paramtype1:par_ax;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$e5),
  (mnemonic:'IN';opcode1:eo_ib;paramtype1:par_eax;paramtype2:par_imm8;bytes:1;bt1:$e5),

  (mnemonic:'IN';paramtype1:par_al;paramtype2:par_dx;bytes:1;bt1:$ec),
  (mnemonic:'IN';paramtype1:par_ax;paramtype2:par_dx;bytes:2;bt1:$66;bt2:$ed),
  (mnemonic:'IN';paramtype1:par_eax;paramtype2:par_dx;bytes:1;bt1:$ed),

  (mnemonic:'INC';opcode1:eo_prw;paramtype1:par_r16;bytes:2;bt1:$66;bt2:$40),
  (mnemonic:'INC';opcode1:eo_prd;paramtype1:par_r32;bytes:1;bt1:$40),
  (mnemonic:'INC';opcode1:eo_reg0;paramtype1:par_rm8;bytes:1;bt1:$fe),
  (mnemonic:'INC';opcode1:eo_reg0;paramtype1:par_rm16;bytes:2;bt1:$66;bt2:$ff),
  (mnemonic:'INC';opcode1:eo_reg0;paramtype1:par_rm32;bytes:1;bt1:$ff),

  (mnemonic:'INSB';bytes:1;bt1:$6c),
  (mnemonic:'INSD';bytes:1;bt1:$6d),
  (mnemonic:'INSW';bytes:2;bt1:$66;bt2:$6d),

  (mnemonic:'INT';paramtype1:par_3;bytes:1;bt1:$cc),
  (mnemonic:'INT';opcode1:eo_ib;paramtype1:par_imm8;bytes:1;bt1:$cd),
  (mnemonic:'INTO';bytes:1;bt1:$ce),

  (mnemonic:'INVD';bytes:2;bt1:$0f;bt2:$08),
  (mnemonic:'INVLPG';opcode1:eo_reg7;paramtype1:par_m32;bytes:2;bt1:$0f;bt2:$01),

  (mnemonic:'IRET';bytes:2;bt1:$66;bt2:$ce),
  (mnemonic:'IRETD';bytes:1;bt1:$cf),

  (mnemonic:'JA';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$77),
  (mnemonic:'JA';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$87),
  (mnemonic:'JAE';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$73),
  (mnemonic:'JAE';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$83),
  (mnemonic:'JB';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$72),
  (mnemonic:'JB';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$82),
  (mnemonic:'JBE';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$76),
  (mnemonic:'JBE';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$86),
  (mnemonic:'JC';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$72),
  (mnemonic:'JC';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$82),

  (mnemonic:'JCXZ';opcode1:eo_cb;paramtype1:par_rel8;bytes:2;bt1:$66;bt2:$e3),
  (mnemonic:'JE';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$74),
  (mnemonic:'JE';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$84),
  (mnemonic:'JECXZ';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$e3),
  (mnemonic:'JG';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$7f),
  (mnemonic:'JG';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$8f),
  (mnemonic:'JGE';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$7d),
  (mnemonic:'JGE';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$8d),
  (mnemonic:'JL';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$7c),
  (mnemonic:'JL';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$8c),
  (mnemonic:'JLE';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$7e),
  (mnemonic:'JLE';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$8e),

  (mnemonic:'JMP';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$eb),
  (mnemonic:'JMP';opcode1:eo_cd;paramtype1:par_rel32;bytes:1;bt1:$e9),
  (mnemonic:'JMP';opcode1:eo_reg4;paramtype1:par_rm32;bytes:1;bt1:$ff),



  (mnemonic:'JNA';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$76),
  (mnemonic:'JNA';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$86),
  (mnemonic:'JNAE';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$72),
  (mnemonic:'JNAE';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$82),
  (mnemonic:'JNB';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$73),
  (mnemonic:'JNB';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$83),
  (mnemonic:'JNBE';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$77),
  (mnemonic:'JNBE';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$87),
  (mnemonic:'JNC';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$73),
  (mnemonic:'JNC';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$83),
  (mnemonic:'JNE';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$75),
  (mnemonic:'JNE';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$85),
  (mnemonic:'JNG';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$7e),
  (mnemonic:'JNG';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$8e),
  (mnemonic:'JNGE';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$7c),
  (mnemonic:'JNGE';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$8c),
  (mnemonic:'JNL';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$7d),
  (mnemonic:'JNL';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$8d),

  (mnemonic:'JNLE';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$7f),
  (mnemonic:'JNLE';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$8f),
  (mnemonic:'JNO';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$71),
  (mnemonic:'JNO';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$81),
  (mnemonic:'JNP';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$7b),
  (mnemonic:'JNP';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$8b),
  (mnemonic:'JNS';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$79),
  (mnemonic:'JNS';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$89),
  (mnemonic:'JNZ';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$75),
  (mnemonic:'JNZ';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$85),
  (mnemonic:'JO';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$70),
  (mnemonic:'JO';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$80),
  (mnemonic:'JP';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$7a),
  (mnemonic:'JP';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$8a),
  (mnemonic:'JPE';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$7a),
  (mnemonic:'JPE';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$8a),
  (mnemonic:'JPO';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$7b),
  (mnemonic:'JPO';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$8b),
  (mnemonic:'JS';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$78),
  (mnemonic:'JS';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$88),
  (mnemonic:'JZ';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$74),
  (mnemonic:'JZ';opcode1:eo_cd;paramtype1:par_rel32;bytes:2;bt1:$0f;bt2:$84),

  (mnemonic:'LAHF';bytes:1;bt1:$9f),
  (mnemonic:'LAR';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$02),
  (mnemonic:'LAR';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$02),

  (mnemonic:'LDMXCSR';opcode1:eo_reg2;paramtype1:par_m32;bytes:2;bt1:$0f;bt2:$ae),
  (mnemonic:'LDS';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_m16;bytes:2;bt1:$66;bt2:$c5),
  (mnemonic:'LDS';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_m32;bytes:1;bt1:$c5),

  (mnemonic:'LEA';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_m16;bytes:2;bt1:$66;bt2:$8d),
  (mnemonic:'LEA';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_m32;bytes:1;bt1:$8d),
  (mnemonic:'LEAVE';bytes:1;bt1:$c9),

  (mnemonic:'LES';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:2;bt1:$66;bt2:$c4),
  (mnemonic:'LES';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:1;bt1:$c4),
  (mnemonic:'LFENCE';bytes:3;bt1:$0f;bt2:$ae;bt3:$e8),

  (mnemonic:'LFS';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_m16;bytes:3;bt1:$66;bt2:$0f;bt3:$b4),
  (mnemonic:'LFS';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_m32;bytes:2;bt1:$0f;bt2:$b4),

  (mnemonic:'LGDT';opcode1:eo_reg2;paramtype1:par_m16;bytes:2;bt1:$0f;bt2:$01),
  (mnemonic:'LGDT';opcode1:eo_reg2;paramtype1:par_m32;bytes:2;bt1:$0f;bt2:$01),

  (mnemonic:'LGS';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_m16;bytes:3;bt1:$66;bt2:$0f;bt3:$b5),
  (mnemonic:'LGS';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_m32;bytes:2;bt1:$0f;bt2:$b5),

  (mnemonic:'LIDT';opcode1:eo_reg3;paramtype1:par_m16;bytes:2;bt1:$0f;bt2:$01),
  (mnemonic:'LIDT';opcode1:eo_reg3;paramtype1:par_m32;bytes:2;bt1:$0f;bt2:$01),

  (mnemonic:'LLDT';opcode1:eo_reg2;paramtype1:par_rm16;bytes:2;bt1:$0f;bt2:$00),
  (mnemonic:'LMSW';opcode1:eo_reg6;paramtype1:par_rm16;bytes:2;bt1:$0f;bt2:$01),

  (mnemonic:'LODSB';bytes:1;bt1:$ac),
  (mnemonic:'LODSD';bytes:1;bt1:$ad),
  (mnemonic:'LODSW';bytes:2;bt1:$66;bt2:$ad),

  (mnemonic:'LOOP';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$e2),
  (mnemonic:'LOOPE';opcode1:eo_cb;paramtype1:par_rel8;bytes:2;bt1:$66;bt2:$e1),
  (mnemonic:'LOOPNE';opcode1:eo_cb;paramtype1:par_rel8;bytes:2;bt1:$66;bt2:$e0),
  (mnemonic:'LOOPNZ';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$e0),
  (mnemonic:'LOOPZ';opcode1:eo_cb;paramtype1:par_rel8;bytes:1;bt1:$e1),

  (mnemonic:'LSL';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:3;bt1:$66;bt2:$0f;bt3:$03),
  (mnemonic:'LSL';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$03),

  (mnemonic:'LSS';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_m16;bytes:3;bt1:$66;bt2:$0f;bt3:$b2),
  (mnemonic:'LSS';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_m32;bytes:2;bt1:$0f;bt2:$b2),

  (mnemonic:'LTR';opcode1:eo_reg3;paramtype1:par_rm16;bytes:2;bt1:$0f;bt2:$00),

  (mnemonic:'MASKMOVDQU';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_mm;bytes:3;bt1:$66;bt2:$0f;bt3:$f7),
  (mnemonic:'MASKMOVQ';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm;bytes:2;bt1:$0f;bt2:$f7),
  (mnemonic:'MAXPD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$5f),
  (mnemonic:'MAXPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$5f),
  (mnemonic:'MAXSD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m64;bytes:3;bt1:$f2;bt2:$0f;bt3:$5f),
  (mnemonic:'MAXSS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m32;bytes:3;bt1:$f3;bt2:$0f;bt3:$5f),
  (mnemonic:'MFENCE';bytes:3;bt1:$0f;bt2:$ae;bt3:$f0),
  (mnemonic:'MINPD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$5d),
  (mnemonic:'MINPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$5d),
  (mnemonic:'MINSD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m64;bytes:3;bt1:$f2;bt2:$0f;bt3:$5d),
  (mnemonic:'MINSS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m32;bytes:3;bt1:$f3;bt2:$0f;bt3:$5d),

  (mnemonic:'MOV';opcode1:eo_id;paramtype1:par_al;paramtype2:par_moffs8;bytes:1;bt1:$a0),
  (mnemonic:'MOV';opcode1:eo_id;paramtype1:par_ax;paramtype2:par_moffs16;bytes:2;bt1:$66;bt2:$a1),
  (mnemonic:'MOV';opcode1:eo_id;paramtype1:par_eax;paramtype2:par_moffs32;bytes:1;bt1:$a1),
  (mnemonic:'MOV';opcode1:eo_id;paramtype1:par_moffs8;paramtype2:par_al;bytes:1;bt1:$a2),
  (mnemonic:'MOV';opcode1:eo_id;paramtype1:par_moffs16;paramtype2:par_ax;bytes:2;bt1:$66;bt2:$a3),
  (mnemonic:'MOV';opcode1:eo_id;paramtype1:par_moffs32;paramtype2:par_eax;bytes:1;bt1:$a3),

  (mnemonic:'MOV';opcode1:eo_reg;paramtype1:par_rm8;paramtype2:par_r8;bytes:1;bt1:$88),
  (mnemonic:'MOV';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:2;bt1:$66;bt2:$89),
  (mnemonic:'MOV';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:1;bt1:$8b), //8b prefered over 89 in case of r32,r32
  (mnemonic:'MOV';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:1;bt1:$89),
  (mnemonic:'MOV';opcode1:eo_reg;paramtype1:par_r8;paramtype2:par_rm8;bytes:1;bt1:$8a),
  (mnemonic:'MOV';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:2;bt1:$66;bt2:$8b),

  (mnemonic:'MOV';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_sreg;bytes:1;bt1:$8c),
  (mnemonic:'MOV';opcode1:eo_reg;paramtype1:par_sreg;paramtype2:par_rm16;bytes:1;bt1:$8e),



  (mnemonic:'MOV';opcode1:eo_prb;paramtype1:par_r8;paramtype2:par_imm8;bytes:1;bt1:$b0),
  (mnemonic:'MOV';opcode1:eo_prw;paramtype1:par_r16;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$b8),
  (mnemonic:'MOV';opcode1:eo_prd;paramtype1:par_r32;paramtype2:par_imm32;bytes:1;bt1:$b8),

  (mnemonic:'MOV';opcode1:eo_reg0;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$c6),
  (mnemonic:'MOV';opcode1:eo_reg0;paramtype1:par_rm16;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$c7),
  (mnemonic:'MOV';opcode1:eo_reg0;paramtype1:par_rm32;paramtype2:par_imm32;bytes:1;bt1:$c7),

  (mnemonic:'MOV';opcode1:eo_reg;paramtype1:par_CR;paramtype2:par_r32;bytes:2;bt1:$0f;bt2:$22),
  (mnemonic:'MOV';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_CR;bytes:2;bt1:$0f;bt2:$20),

  (mnemonic:'MOV';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_DR;bytes:2;bt1:$0f;bt2:$21),
  (mnemonic:'MOV';opcode1:eo_reg;paramtype1:par_DR;paramtype2:par_r32;bytes:2;bt1:$0f;bt2:$23),

  (mnemonic:'MOVAPD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$28),

  (mnemonic:'MOVAPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$28),
  (mnemonic:'MOVAPS';opcode1:eo_reg;paramtype1:par_xmm_m128;paramtype2:par_xmm;bytes:2;bt1:$0f;bt2:$29),

  (mnemonic:'MOVD';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$6e),
  (mnemonic:'MOVD';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_mm;bytes:2;bt1:$0f;bt2:$7e),

  (mnemonic:'MOVD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_rm32;bytes:3;bt1:$66;bt2:$0f;bt3:$6e),
  (mnemonic:'MOVD';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_xmm;bytes:3;bt1:$66;bt2:$0f;bt3:$7e),

  (mnemonic:'MOVDQ2Q';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_xmm;bytes:3;bt1:$f2;bt2:$0f;bt3:$d6),
  (mnemonic:'MOVDQA';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$6f),
  (mnemonic:'MOVDQA';opcode1:eo_reg;paramtype1:par_xmm_m128;paramtype2:par_xmm;bytes:3;bt1:$66;bt2:$0f;bt3:$7f),

  (mnemonic:'MOVDQU';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$f3;bt2:$0f;bt3:$6f),
  (mnemonic:'MOVDQU';opcode1:eo_reg;paramtype1:par_xmm_m128;paramtype2:par_xmm;bytes:3;bt1:$f3;bt2:$0f;bt3:$7f),

  (mnemonic:'MOVHLPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm;bytes:2;bt1:$0f;bt2:$12),

  (mnemonic:'MOVHPD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_m64;bytes:3;bt1:$66;bt2:$0f;bt3:$16),
  (mnemonic:'MOVHPD';opcode1:eo_reg;paramtype1:par_m64;paramtype2:par_xmm;bytes:3;bt1:$66;bt2:$0f;bt3:$17),

  (mnemonic:'MOVHPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_m64;bytes:2;bt1:$0f;bt2:$16),
  (mnemonic:'MOVHPS';opcode1:eo_reg;paramtype1:par_m64;paramtype2:par_xmm;bytes:2;bt1:$0f;bt2:$17),

  (mnemonic:'MOVLHPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm;bytes:2;bt1:$0f;bt3:$16),

  (mnemonic:'MOVLPD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_m64;bytes:3;bt1:$66;bt2:$0f;bt3:$12),
  (mnemonic:'MOVLPD';opcode1:eo_reg;paramtype1:par_m64;paramtype2:par_xmm;bytes:3;bt1:$66;bt2:$0f;bt3:$13),

  (mnemonic:'MOVLPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_m64;bytes:2;bt1:$0f;bt2:$12),
  (mnemonic:'MOVLPS';opcode1:eo_reg;paramtype1:par_m64;paramtype2:par_xmm;bytes:2;bt1:$0f;bt2:$13),

  (mnemonic:'MOVMSKPD';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_xmm;bytes:3;bt1:$66;bt2:$0f;bt3:$50),
  (mnemonic:'MOVMSKPS';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_xmm;bytes:2;bt1:$0f;bt2:$50),
  (mnemonic:'MOVNTDQ';opcode1:eo_reg;paramtype1:par_m128;paramtype2:par_xmm;bytes:3;bt1:$66;bt2:$0f;bt3:$e7),
  (mnemonic:'MOVNTI';opcode1:eo_reg;paramtype1:par_m32;paramtype2:par_r32;bytes:2;bt1:$0f;bt2:$c3),

  (mnemonic:'MOVNTPD';opcode1:eo_reg;paramtype1:par_m128;paramtype2:par_xmm;bytes:3;bt1:$66;bt2:$0f;bt3:$2b),
  (mnemonic:'MOVNTPS';opcode1:eo_reg;paramtype1:par_m128;paramtype2:par_xmm;bytes:2;bt1:$0f;bt2:$2b),

  (mnemonic:'MOVNTQ';opcode1:eo_reg;paramtype1:par_m64;paramtype2:par_mm;bytes:2;bt1:$0f;bt2:$e7),

  (mnemonic:'MOVQ';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$6f),
  (mnemonic:'MOVQ';opcode1:eo_reg;paramtype1:par_mm_m64;paramtype2:par_mm;bytes:2;bt1:$0f;bt2:$7f),

  (mnemonic:'MOVQ';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m64;bytes:3;bt1:$f3;bt2:$0f;bt3:$7e),
  (mnemonic:'MOVQ';opcode1:eo_reg;paramtype1:par_xmm_m64;paramtype2:par_xmm;bytes:3;bt1:$66;bt2:$0f;bt3:$d6),

  (mnemonic:'MOVQ2DQ';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_mm;bytes:3;bt1:$66;bt2:$0f;bt3:$d6),

  (mnemonic:'MOVSB';bytes:1;bt1:$a4),
  (mnemonic:'MOVSD';bytes:1;bt1:$a5),

  (mnemonic:'MOVSD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m64;bytes:3;bt1:$f2;bt2:$0f;bt3:$10),
  (mnemonic:'MOVSD';opcode1:eo_reg;paramtype1:par_xmm_m64;paramtype2:par_xmm;bytes:3;bt1:$f2;bt2:$0f;bt3:$11),

  (mnemonic:'MOVSS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m32;bytes:3;bt1:$f3;bt2:$0f;bt3:$10),
  (mnemonic:'MOVSS';opcode1:eo_reg;paramtype1:par_m32;paramtype2:par_xmm;bytes:3;bt1:$f3;bt2:$0f;bt3:$11),
  (mnemonic:'MOVSW';bytes:2;bt1:$66;bt2:$a5),

  (mnemonic:'MOVSX';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm8;bytes:3;bt1:$66;bt2:$0f;bt3:$be),
  (mnemonic:'MOVSX';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm8;bytes:2;bt1:$0f;bt2:$be),
  (mnemonic:'MOVSX';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm16;bytes:2;bt1:$0f;bt2:$bf),

  (mnemonic:'MOVUPD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$10),
  (mnemonic:'MOVUPD';opcode1:eo_reg;paramtype1:par_xmm_m128;paramtype2:par_xmm;bytes:3;bt1:$66;bt2:$0f;bt3:$11),

  (mnemonic:'MOVUPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$10),
  (mnemonic:'MOVUPS';opcode1:eo_reg;paramtype1:par_xmm_m128;paramtype2:par_xmm;bytes:2;bt1:$0f;bt2:$11),

  (mnemonic:'MOVZX';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm8;bytes:3;bt1:$66;bt2:$0f;bt3:$b6),
  (mnemonic:'MOVZX';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm8;bytes:2;bt1:$0f;bt2:$b6),
  (mnemonic:'MOVZX';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm16;bytes:2;bt1:$0f;bt2:$b7),

  (mnemonic:'MUL';opcode1:eo_reg4;paramtype1:par_rm8;bytes:1;bt1:$f6),
  (mnemonic:'MUL';opcode1:eo_reg4;paramtype1:par_rm16;bytes:2;bt1:$66;bt2:$f7),
  (mnemonic:'MUL';opcode1:eo_reg4;paramtype1:par_rm32;bytes:1;bt1:$f7),

  (mnemonic:'MULPD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$59),
  (mnemonic:'MULPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$59),
  (mnemonic:'MULSD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m64;bytes:3;bt1:$f2;bt2:$0f;bt3:$59),
  (mnemonic:'MULSS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m32;bytes:3;bt1:$f3;bt2:$0f;bt3:$59),

  (mnemonic:'NEG';opcode1:eo_reg3;paramtype1:par_rm8;bytes:1;bt1:$f6),
  (mnemonic:'NEG';opcode1:eo_reg3;paramtype1:par_rm16;bytes:2;bt1:$66;bt2:$f7),
  (mnemonic:'NEG';opcode1:eo_reg3;paramtype1:par_rm32;bytes:1;bt1:$f7),

  (mnemonic:'NOP';bytes:1;bt1:$90),  //NOP nop Nop nOp noP NoP nOp NOp nOP

  (mnemonic:'NOT';opcode1:eo_reg2;paramtype1:par_rm8;bytes:1;bt1:$f6),
  (mnemonic:'NOT';opcode1:eo_reg2;paramtype1:par_rm16;bytes:2;bt1:$66;bt2:$f7),
  (mnemonic:'NOT';opcode1:eo_reg2;paramtype1:par_rm32;bytes:1;bt1:$f7),

  (mnemonic:'OR';opcode1:eo_ib;paramtype1:par_AL;paramtype2:par_imm8;bytes:1;bt1:$0c),
  (mnemonic:'OR';opcode1:eo_iw;paramtype1:par_AX;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$0d),
  (mnemonic:'OR';opcode1:eo_id;paramtype1:par_EAX;paramtype2:par_imm32;bytes:1;bt1:$0d),
  (mnemonic:'OR';opcode1:eo_reg1;opcode2:eo_ib;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$80),
  (mnemonic:'OR';opcode1:eo_reg1;opcode2:eo_iw;paramtype1:par_rm16;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$80),
  (mnemonic:'OR';opcode1:eo_reg1;opcode2:eo_id;paramtype1:par_rm32;paramtype2:par_imm32;bytes:1;bt1:$81),
  (mnemonic:'OR';opcode1:eo_reg1;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$83),
  (mnemonic:'OR';opcode1:eo_reg1;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:1;bt1:$83),
  (mnemonic:'OR';opcode1:eo_reg;paramtype1:par_rm8;paramtype2:par_r8;bytes:1;bt1:$08),
  (mnemonic:'OR';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:2;bt1:$66;bt2:$09),
  (mnemonic:'OR';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:1;bt1:$09),
  (mnemonic:'OR';opcode1:eo_reg;paramtype1:par_r8;paramtype2:par_rm8;bytes:1;bt1:$0a),
  (mnemonic:'OR';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:2;bt1:$66;bt2:$0b),
  (mnemonic:'OR';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:1;bt1:$0b),

  (mnemonic:'ORPD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$56),
  (mnemonic:'ORPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$56),

  (mnemonic:'OUT';opcode1:eo_ib;paramtype1:par_imm8;paramtype2:par_al;bytes:1;bt1:$e6),
  (mnemonic:'OUT';opcode1:eo_ib;paramtype1:par_imm8;paramtype2:par_ax;bytes:2;bt1:$66;bt2:$e7),
  (mnemonic:'OUT';opcode1:eo_ib;paramtype1:par_imm8;paramtype2:par_eax;bytes:1;bt1:$e7),

  (mnemonic:'OUT';paramtype1:par_dx;paramtype2:par_al;bytes:1;bt1:$ee),
  (mnemonic:'OUT';paramtype1:par_dx;paramtype2:par_ax;bytes:2;bt1:$66;bt2:$ef),
  (mnemonic:'OUT';paramtype1:par_dx;paramtype2:par_eax;bytes:1;bt1:$ef),

  (mnemonic:'OUTSB';bytes:1;bt1:$6e),
  (mnemonic:'OUTSD';bytes:1;bt1:$6f),
  (mnemonic:'OUTSW';bytes:2;bt1:$66;bt2:$6f),

  (mnemonic:'PACKSSDW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$6b),
  (mnemonic:'PACKSSDW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$6b),

  (mnemonic:'PACKSSWB';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$63),
  (mnemonic:'PACKSSWB';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$63),


  (mnemonic:'PACKUSWB';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$67),
  (mnemonic:'PACKUSWB';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$67),

  (mnemonic:'PADDB';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$fc),
  (mnemonic:'PADDB';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$fc),

  (mnemonic:'PADDD';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$fe),
  (mnemonic:'PADDD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$fe),

  (mnemonic:'PADDQ';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$d4),
  (mnemonic:'PADDQ';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$d4),

  (mnemonic:'PADDSB';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$ec),
  (mnemonic:'PADDSB';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$ec),

  (mnemonic:'PADDSW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$ed),
  (mnemonic:'PADDSW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$ed),

  (mnemonic:'PADDUSB';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$dc),
  (mnemonic:'PADDUSB';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$dc),

  (mnemonic:'PADDUSW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$dd),
  (mnemonic:'PADDUSW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$dd),

  (mnemonic:'PADDW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$fd),
  (mnemonic:'PADDW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$fd),


  (mnemonic:'PAND';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$db),
  (mnemonic:'PAND';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$db),

  (mnemonic:'PANDN';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$df),
  (mnemonic:'PANDN';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$df),

  (mnemonic:'PAUSE';bytes:2;bt1:$f3;bt2:$90),

  (mnemonic:'PAVGB';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$e0),
  (mnemonic:'PAVGB';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$e0),

  (mnemonic:'PAVGW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$e3),
  (mnemonic:'PAVGW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$e3),

  (mnemonic:'PCMPEQB';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$74),
  (mnemonic:'PCMPEQB';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$74),

  (mnemonic:'PCMPEQD';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$76),
  (mnemonic:'PCMPEQD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$76),

  (mnemonic:'PCMPEQW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$75),
  (mnemonic:'PCMPEQW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$75),

  (mnemonic:'PCMPGTB';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$64),
  (mnemonic:'PCMPGTB';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$64),

  (mnemonic:'PCMPGTD';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$66),
  (mnemonic:'PCMPGTD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$66),

  (mnemonic:'PCMPGTW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$65),
  (mnemonic:'PCMPGTW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$65),

  (mnemonic:'PCPPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$53),
  (mnemonic:'PCPSS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$f3;bt2:$0f;bt3:$53),

  (mnemonic:'PEXTRW';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_r32;paramtype2:par_mm;paramtype3:par_imm8;bytes:2;bt1:$0f;bt2:$c5),
  (mnemonic:'PEXTRW';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_r32;paramtype2:par_xmm;paramtype3:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$c5),

  (mnemonic:'PINSRW';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_mm;paramtype2:par_r32_m16;paramtype3:par_imm8;bytes:2;bt1:$0f;bt2:$c4),
  (mnemonic:'PINSRW';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_r32_m16;paramtype3:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$c4),

  (mnemonic:'PMADDWD';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$f5),
  (mnemonic:'PMADDWD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$f5),

  (mnemonic:'PMAXSW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$ee),
  (mnemonic:'PMAXSW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$ee),

  (mnemonic:'PMAXUB';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$de),
  (mnemonic:'PMAXUB';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$de),

  (mnemonic:'PMINSW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$ea),
  (mnemonic:'PMINSW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$ea),

  (mnemonic:'PMINUB';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$da),
  (mnemonic:'PMINUB';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$da),

  (mnemonic:'PMOVMSKB';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_r32;paramtype2:par_mm;paramtype3:par_imm8;bytes:2;bt1:$0f;bt2:$d7),
  (mnemonic:'PMOVMSKB';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_r32;paramtype2:par_xmm;paramtype3:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$d7),

  (mnemonic:'PMULHUL';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$e4),
  (mnemonic:'PMULHUL';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$e4),

  (mnemonic:'PMULHW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$e5),
  (mnemonic:'PMULHW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$e5),

  (mnemonic:'PMULLW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$d5),
  (mnemonic:'PMULLW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$d5),

  (mnemonic:'PMULUDQ';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$f4),
  (mnemonic:'PMULUDQ';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$f4),

  (mnemonic:'POP';opcode1:eo_prw;paramtype1:par_r16;bytes:2;bt1:$66;bt2:$58),
  (mnemonic:'POP';opcode1:eo_prd;paramtype1:par_r32;bytes:1;bt1:$58),

  (mnemonic:'POP';opcode1:eo_reg0;paramtype1:par_rm16;bytes:2;bt1:$66;bt2:$8f),
  (mnemonic:'POP';opcode1:eo_reg0;paramtype1:par_rm32;bytes:1;bt1:$8f),

  (mnemonic:'POP';paramtype1:par_ds;bytes:1;bt1:$1f),
  (mnemonic:'POP';paramtype1:par_es;bytes:1;bt1:$07),
  (mnemonic:'POP';paramtype1:par_ss;bytes:1;bt1:$17),
  (mnemonic:'POP';paramtype1:par_fs;bytes:2;bt1:$0f;bt2:$a1),
  (mnemonic:'POP';paramtype1:par_gs;bytes:2;bt1:$0f;bt2:$a9),

  (mnemonic:'POPA';bytes:2;bt1:$66;bt2:$61),
  (mnemonic:'POPAD';bytes:1;bt1:$61),
  (mnemonic:'POPALL';bytes:1;bt1:$61),

  (mnemonic:'POPF';bytes:2;bt1:$66;bt2:$9d),
  (mnemonic:'POPFD';bytes:1;bt1:$9d),

  (mnemonic:'POR';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$eb),
  (mnemonic:'POR';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$eb),

  (mnemonic:'PREFETCH0';opcode1:eo_reg1;paramtype1:par_m8;bytes:2;bt1:$0f;bt2:$18),
  (mnemonic:'PREFETCH1';opcode1:eo_reg2;paramtype1:par_m8;bytes:2;bt1:$0f;bt2:$18),
  (mnemonic:'PREFETCH2';opcode1:eo_reg3;paramtype1:par_m8;bytes:2;bt1:$0f;bt2:$18),
  (mnemonic:'PREFETCHA';opcode1:eo_reg0;paramtype1:par_m8;bytes:2;bt1:$0f;bt2:$18),

  (mnemonic:'PSADBW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$f6),
  (mnemonic:'PSADBW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$f6),

  (mnemonic:'PSHUFD';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_xmm_m128;paramtype3:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$70),
  (mnemonic:'PSHUFHW';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_xmm_m128;paramtype3:par_imm8;bytes:3;bt1:$f3;bt2:$0f;bt3:$70),
  (mnemonic:'PSHUFLW';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_xmm_m128;paramtype3:par_imm8;bytes:3;bt1:$f2;bt2:$0f;bt3:$70),
  (mnemonic:'PSHUFW';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_mm;paramtype2:par_mm_m64;paramtype3:par_imm8;bytes:2;bt1:$0f;bt2:$70),


  (mnemonic:'PSLLD';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$f2),
  (mnemonic:'PSLLD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$f2),

  (mnemonic:'PSLLD';opcode1:eo_reg6;opcode2:eo_ib;paramtype1:par_mm;paramtype2:par_imm8;bytes:2;bt1:$0f;bt2:$72),
  (mnemonic:'PSLLD';opcode1:eo_reg6;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$72),

  (mnemonic:'PSLLDQ';opcode1:eo_reg7;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$73),

  (mnemonic:'PSLLQ';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$f3),
  (mnemonic:'PSLLQ';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$f3),

  (mnemonic:'PSLLQ';opcode1:eo_reg6;opcode2:eo_ib;paramtype1:par_mm;paramtype2:par_imm8;bytes:2;bt1:$0f;bt2:$73),
  (mnemonic:'PSLLQ';opcode1:eo_reg6;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$73),


  (mnemonic:'PSLLW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$f1),
  (mnemonic:'PSLLW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$f1),

  (mnemonic:'PSLLW';opcode1:eo_reg6;opcode2:eo_ib;paramtype1:par_mm;paramtype2:par_imm8;bytes:2;bt1:$0f;bt2:$71),
  (mnemonic:'PSLLW';opcode1:eo_reg6;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$71),

  (mnemonic:'PSQRTPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$52),
  (mnemonic:'PSQRTSS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m32;bytes:3;bt1:$f3;bt2:$0f;bt3:$52),


  (mnemonic:'PSRAD';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$e2),
  (mnemonic:'PSRAD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$e2),

  (mnemonic:'PSRAD';opcode1:eo_reg4;opcode2:eo_ib;paramtype1:par_mm;paramtype2:par_imm8;bytes:2;bt1:$0f;bt2:$72),
  (mnemonic:'PSRAD';opcode1:eo_reg4;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$72),

  (mnemonic:'PSRAW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$e1),
  (mnemonic:'PSRAW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$e1),

  (mnemonic:'PSRAW';opcode1:eo_reg4;opcode2:eo_ib;paramtype1:par_mm;paramtype2:par_imm8;bytes:2;bt1:$0f;bt2:$71),
  (mnemonic:'PSRAW';opcode1:eo_reg4;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$71),



  (mnemonic:'PSRLD';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$d2),
  (mnemonic:'PSRLD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$d2),

  (mnemonic:'PSRLD';opcode1:eo_reg2;opcode2:eo_ib;paramtype1:par_mm;paramtype2:par_imm8;bytes:2;bt1:$0f;bt2:$72),
  (mnemonic:'PSRLD';opcode1:eo_reg2;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$72),
  (mnemonic:'PSRLDQ';opcode1:eo_reg3;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$73),

  (mnemonic:'PSRLQ';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$d3),
  (mnemonic:'PSRLQ';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$d3),

  (mnemonic:'PSRLQ';opcode1:eo_reg2;opcode2:eo_ib;paramtype1:par_mm;paramtype2:par_imm8;bytes:2;bt1:$0f;bt2:$73),
  (mnemonic:'PSRLQ';opcode1:eo_reg2;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$73),

  (mnemonic:'PSRLW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$d1),
  (mnemonic:'PSRLW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$d1),

  (mnemonic:'PSRLW';opcode1:eo_reg2;opcode2:eo_ib;paramtype1:par_mm;paramtype2:par_imm8;bytes:2;bt1:$0f;bt2:$71),
  (mnemonic:'PSRLW';opcode1:eo_reg2;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$71),



  (mnemonic:'PSUBB';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$f8),
  (mnemonic:'PSUBB';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$f8),

  (mnemonic:'PSUBD';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$fa),
  (mnemonic:'PSUBD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$fa),

  (mnemonic:'PSUBQ';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$fb),
  (mnemonic:'PSUBQ';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$fb),

  (mnemonic:'PSUBUSB';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$d8),
  (mnemonic:'PSUBUSB';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$d8),

  (mnemonic:'PSUBUSW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$d9),
  (mnemonic:'PSUBUSW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$d9),


  (mnemonic:'PSUBW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$f9),
  (mnemonic:'PSUBW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$f9),


  (mnemonic:'PSUSB';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$e8),
  (mnemonic:'PSUSB';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$e8),

  (mnemonic:'PSUSW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$e9),
  (mnemonic:'PSUSW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$e9),


  (mnemonic:'PUNPCKHBW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$68),
  (mnemonic:'PUNPCKHBW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$68),

  (mnemonic:'PUNPCKHDQ';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$6a),
  (mnemonic:'PUNPCKHDQ';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$6a),

  (mnemonic:'PUNPCKHQDQ';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$6d),

  (mnemonic:'PUNPCKHWD';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$69),
  (mnemonic:'PUNPCKHWD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$69),

  (mnemonic:'PUNPCKLBW';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$60),
  (mnemonic:'PUNPCKLBW';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$60),

  (mnemonic:'PUNPCKLDQ';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$62),
  (mnemonic:'PUNPCKLDQ';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$62),

  (mnemonic:'PUNPCKLQDQ';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$6c),

  (mnemonic:'PUNPCKLWD';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$61),
  (mnemonic:'PUNPCKLWD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$61),

  (mnemonic:'PUSH';opcode1:eo_prw;paramtype1:par_r16;bytes:2;bt1:$66;bt2:$50),
  (mnemonic:'PUSH';opcode1:eo_prd;paramtype1:par_r32;bytes:1;bt1:$50),

  (mnemonic:'PUSH';opcode1:eo_reg6;paramtype1:par_rm16;bytes:2;bt1:$66;bt2:$ff),
  (mnemonic:'PUSH';opcode1:eo_reg6;paramtype1:par_rm32;bytes:1;bt1:$ff),

  (mnemonic:'PUSH';opcode1:eo_ib;paramtype1:par_imm8;bytes:1;bt1:$6a),
  (mnemonic:'PUSH';opcode1:eo_iw;paramtype1:par_imm16;bytes:2;bt1:$66;bt2:$68),
  (mnemonic:'PUSH';opcode1:eo_id;paramtype1:par_imm32;bytes:1;bt1:$68),

  (mnemonic:'PUSH';paramtype1:par_CS;bytes:1;bt1:$0e),
  (mnemonic:'PUSH';paramtype1:par_ss;bytes:1;bt1:$16),
  (mnemonic:'PUSH';paramtype1:par_ds;bytes:1;bt1:$1e),
  (mnemonic:'PUSH';paramtype1:par_es;bytes:1;bt1:$06),
  (mnemonic:'PUSH';paramtype1:par_fs;bytes:2;bt1:$0f;bt2:$a0),
  (mnemonic:'PUSH';paramtype1:par_gs;bytes:2;bt1:$0f;bt2:$a8),

  (mnemonic:'PUSHA';bytes:2;bt1:$66;bt2:$60),
  (mnemonic:'PUSHAD';bytes:1;bt1:$60),
  (mnemonic:'PUSHALL';bytes:1;bt1:$60),

  (mnemonic:'PUSHF';bytes:2;bt1:$66;bt2:$9c),
  (mnemonic:'PUSHFD';bytes:1;bt1:$9c),

  (mnemonic:'PXOR';opcode1:eo_reg;paramtype1:par_mm;paramtype2:par_mm_m64;bytes:2;bt1:$0f;bt2:$ef),
  (mnemonic:'PXOR';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$ef),

  (mnemonic:'RCL';opcode1:eo_reg2;paramtype1:par_rm8;paramtype2:par_1;bytes:1;bt1:$d0),
  (mnemonic:'RCL';opcode1:eo_reg2;paramtype1:par_rm8;paramtype2:par_cl;bytes:1;bt1:$d2),
  (mnemonic:'RCL';opcode1:eo_reg2;opcode2:eo_ib;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$c0),

  (mnemonic:'RCL';opcode1:eo_reg2;paramtype1:par_rm16;paramtype2:par_1;bytes:2;bt1:$66;bt2:$d1),
  (mnemonic:'RCL';opcode1:eo_reg2;paramtype1:par_rm16;paramtype2:par_cl;bytes:2;bt1:$66;bt2:$d3),
  (mnemonic:'RCL';opcode1:eo_reg2;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$c1),

  (mnemonic:'RCL';opcode1:eo_reg2;paramtype1:par_rm32;paramtype2:par_1;bytes:1;bt1:$d1),
  (mnemonic:'RCL';opcode1:eo_reg2;paramtype1:par_rm32;paramtype2:par_cl;bytes:1;bt1:$d3),
  (mnemonic:'RCL';opcode1:eo_reg2;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:1;bt1:$c1),


  (mnemonic:'RCR';opcode1:eo_reg3;paramtype1:par_rm32;paramtype2:par_1;bytes:1;bt1:$d1),
  (mnemonic:'RCR';opcode1:eo_reg3;paramtype1:par_rm32;paramtype2:par_cl;bytes:1;bt1:$d3),
  (mnemonic:'RCR';opcode1:eo_reg3;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:1;bt1:$c1),

  (mnemonic:'RCR';opcode1:eo_reg3;paramtype1:par_rm16;paramtype2:par_1;bytes:2;bt1:$66;bt2:$d1),
  (mnemonic:'RCR';opcode1:eo_reg3;paramtype1:par_rm16;paramtype2:par_cl;bytes:2;bt1:$66;bt2:$d3),
  (mnemonic:'RCR';opcode1:eo_reg3;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$c1),

  (mnemonic:'RCR';opcode1:eo_reg3;paramtype1:par_rm8;paramtype2:par_1;bytes:1;bt1:$d0),
  (mnemonic:'RCR';opcode1:eo_reg3;paramtype1:par_rm8;paramtype2:par_cl;bytes:1;bt1:$d2),
  (mnemonic:'RCR';opcode1:eo_reg3;opcode2:eo_ib;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$c0),




  (mnemonic:'RDMSR';bytes:2;bt1:$0f;bt2:$32),
  (mnemonic:'RDPMC';bytes:2;bt1:$0f;bt2:$33),
  (mnemonic:'RDTSC';bytes:2;bt1:$0f;bt2:$31),

  (mnemonic:'RET';bytes:1;bt1:$c3),
  (mnemonic:'RET';bytes:1;bt1:$cb),
  (mnemonic:'RET';opcode1:eo_iw;paramtype1:par_imm16;bytes:1;bt1:$c2),
  (mnemonic:'RETN';bytes:1;bt1:$c3),
  (mnemonic:'RETN';opcode1:eo_iw;paramtype1:par_imm16;bytes:1;bt1:$c2),




  (mnemonic:'ROL';opcode1:eo_reg0;paramtype1:par_rm32;paramtype2:par_1;bytes:1;bt1:$d1),
  (mnemonic:'ROL';opcode1:eo_reg0;paramtype1:par_rm32;paramtype2:par_cl;bytes:1;bt1:$d3),
  (mnemonic:'ROL';opcode1:eo_reg0;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:1;bt1:$c1),

  (mnemonic:'ROL';opcode1:eo_reg0;paramtype1:par_rm16;paramtype2:par_1;bytes:2;bt1:$66;bt2:$d1),
  (mnemonic:'ROL';opcode1:eo_reg0;paramtype1:par_rm16;paramtype2:par_cl;bytes:2;bt1:$66;bt2:$d3),
  (mnemonic:'ROL';opcode1:eo_reg0;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$c1),

  (mnemonic:'ROL';opcode1:eo_reg0;paramtype1:par_rm8;paramtype2:par_1;bytes:1;bt1:$d0),
  (mnemonic:'ROL';opcode1:eo_reg0;paramtype1:par_rm8;paramtype2:par_cl;bytes:1;bt1:$d2),
  (mnemonic:'ROL';opcode1:eo_reg0;opcode2:eo_ib;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$c0),

  (mnemonic:'ROR';opcode1:eo_reg1;paramtype1:par_rm32;paramtype2:par_1;bytes:1;bt1:$d1),
  (mnemonic:'ROR';opcode1:eo_reg1;paramtype1:par_rm32;paramtype2:par_cl;bytes:1;bt1:$d3),
  (mnemonic:'ROR';opcode1:eo_reg1;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:1;bt1:$c1),

  (mnemonic:'ROR';opcode1:eo_reg1;paramtype1:par_rm16;paramtype2:par_1;bytes:2;bt1:$66;bt2:$d1),
  (mnemonic:'ROR';opcode1:eo_reg1;paramtype1:par_rm16;paramtype2:par_cl;bytes:2;bt1:$66;bt2:$d3),
  (mnemonic:'ROR';opcode1:eo_reg1;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$c1),

  (mnemonic:'ROR';opcode1:eo_reg1;paramtype1:par_rm8;paramtype2:par_1;bytes:1;bt1:$d0),
  (mnemonic:'ROR';opcode1:eo_reg1;paramtype1:par_rm8;paramtype2:par_cl;bytes:1;bt1:$d2),
  (mnemonic:'ROR';opcode1:eo_reg1;opcode2:eo_ib;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$c0),



  (mnemonic:'RSM';bytes:2;bt1:$0f;bt2:$aa),


  (mnemonic:'SAHF';bytes:1;bt1:$9e),

  (mnemonic:'SAL';opcode1:eo_reg4;paramtype1:par_rm32;paramtype2:par_1;bytes:1;bt1:$d1),
  (mnemonic:'SAL';opcode1:eo_reg4;paramtype1:par_rm32;paramtype2:par_cl;bytes:1;bt1:$d3),
  (mnemonic:'SAL';opcode1:eo_reg4;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:1;bt1:$c1),

  (mnemonic:'SAL';opcode1:eo_reg4;paramtype1:par_rm16;paramtype2:par_1;bytes:2;bt1:$66;bt2:$d1),
  (mnemonic:'SAL';opcode1:eo_reg4;paramtype1:par_rm16;paramtype2:par_cl;bytes:2;bt1:$66;bt2:$d3),
  (mnemonic:'SAL';opcode1:eo_reg4;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$c1),

  (mnemonic:'SAL';opcode1:eo_reg4;paramtype1:par_rm8;paramtype2:par_1;bytes:1;bt1:$d0),
  (mnemonic:'SAL';opcode1:eo_reg4;paramtype1:par_rm8;paramtype2:par_cl;bytes:1;bt1:$d2),
  (mnemonic:'SAL';opcode1:eo_reg4;opcode2:eo_ib;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$c0),

  (mnemonic:'SAR';opcode1:eo_reg7;paramtype1:par_rm32;paramtype2:par_1;bytes:1;bt1:$d1),
  (mnemonic:'SAR';opcode1:eo_reg7;paramtype1:par_rm32;paramtype2:par_cl;bytes:1;bt1:$d3),
  (mnemonic:'SAR';opcode1:eo_reg7;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:1;bt1:$c1),

  (mnemonic:'SAR';opcode1:eo_reg7;paramtype1:par_rm16;paramtype2:par_1;bytes:2;bt1:$66;bt2:$d1),
  (mnemonic:'SAR';opcode1:eo_reg7;paramtype1:par_rm16;paramtype2:par_cl;bytes:2;bt1:$66;bt2:$d3),
  (mnemonic:'SAR';opcode1:eo_reg7;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$c1),

  (mnemonic:'SAR';opcode1:eo_reg7;paramtype1:par_rm8;paramtype2:par_1;bytes:1;bt1:$d0),
  (mnemonic:'SAR';opcode1:eo_reg7;paramtype1:par_rm8;paramtype2:par_cl;bytes:1;bt1:$d2),
  (mnemonic:'SAR';opcode1:eo_reg7;opcode2:eo_ib;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$c0),

  (mnemonic:'SBB';opcode1:eo_ib;paramtype1:par_AL;paramtype2:par_imm8;bytes:1;bt1:$1c),
  (mnemonic:'SBB';opcode1:eo_iw;paramtype1:par_AX;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$1d),
  (mnemonic:'SBB';opcode1:eo_id;paramtype1:par_EAX;paramtype2:par_imm32;bytes:1;bt1:$1d),
  (mnemonic:'SBB';opcode1:eo_reg3;opcode2:eo_ib;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$80),
  (mnemonic:'SBB';opcode1:eo_reg3;opcode2:eo_iw;paramtype1:par_rm16;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$80),
  (mnemonic:'SBB';opcode1:eo_reg3;opcode2:eo_id;paramtype1:par_rm32;paramtype2:par_imm32;bytes:1;bt1:$81),
  (mnemonic:'SBB';opcode1:eo_reg3;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$83),
  (mnemonic:'SBB';opcode1:eo_reg3;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:1;bt1:$83),
  (mnemonic:'SBB';opcode1:eo_reg;paramtype1:par_rm8;paramtype2:par_r8;bytes:1;bt1:$18),
  (mnemonic:'SBB';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:2;bt1:$66;bt2:$19),
  (mnemonic:'SBB';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:1;bt1:$19),
  (mnemonic:'SBB';opcode1:eo_reg;paramtype1:par_r8;paramtype2:par_rm8;bytes:1;bt1:$1a),
  (mnemonic:'SBB';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:2;bt1:$66;bt2:$1b),
  (mnemonic:'SBB';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:1;bt1:$1b),

  (mnemonic:'SCASB';bytes:1;bt1:$ae),
  (mnemonic:'SCASD';bytes:1;bt1:$af),
  (mnemonic:'SCASW';bytes:2;bt1:$66;bt2:$af),


  (mnemonic:'SETA';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$97),
  (mnemonic:'SETAE';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$93),
  (mnemonic:'SETB';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$92),
  (mnemonic:'SETBE';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$96),
  (mnemonic:'SETC';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$92),
  (mnemonic:'SETE';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$94),
  (mnemonic:'SETG';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$9f),
  (mnemonic:'SETGE';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$9d),
  (mnemonic:'SETL';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$9c),
  (mnemonic:'SETLE';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$9e),
  (mnemonic:'SETNA';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$96),

  (mnemonic:'SETNAE';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$92),
  (mnemonic:'SETNB';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$93),
  (mnemonic:'SETNBE';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$97),
  (mnemonic:'SETNC';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$93),
  (mnemonic:'SETNE';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$95),
  (mnemonic:'SETNG';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$9e),
  (mnemonic:'SETNGE';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$9c),
  (mnemonic:'SETNL';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$9d),
  (mnemonic:'SETNLE';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$9f),
  (mnemonic:'SETNO';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$91),
  (mnemonic:'SETNP';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$9b),

  (mnemonic:'SETNS';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$99),
  (mnemonic:'SETNZ';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$95),
  (mnemonic:'SETO';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$90),
  (mnemonic:'SETP';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$9a),
  (mnemonic:'SETPE';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$9a),
  (mnemonic:'SETPO';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$9b),
  (mnemonic:'SETS';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$98),
  (mnemonic:'SETZ';opcode1:eo_reg;paramtype1:par_rm8;bytes:2;bt1:$0f;bt2:$94),

  (mnemonic:'SFENCE';bytes:3;bt1:$0f;bt2:$ae;bt3:$f8),

  (mnemonic:'SGDT';opcode1:eo_reg0;paramtype1:par_m32;bytes:2;bt1:$0f;bt2:$01),

  (mnemonic:'SHL';opcode1:eo_reg4;paramtype1:par_rm32;paramtype2:par_1;bytes:1;bt1:$d1),
  (mnemonic:'SHL';opcode1:eo_reg4;paramtype1:par_rm16;paramtype2:par_1;bytes:2;bt1:$66;bt2:$d1),
  (mnemonic:'SHL';opcode1:eo_reg4;paramtype1:par_rm8;paramtype2:par_1;bytes:1;bt1:$d0),
  (mnemonic:'SHL';opcode1:eo_reg4;paramtype1:par_rm8;paramtype2:par_cl;bytes:1;bt1:$d2),
  (mnemonic:'SHL';opcode1:eo_reg4;opcode2:eo_ib;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$c0),

  (mnemonic:'SHL';opcode1:eo_reg4;paramtype1:par_rm16;paramtype2:par_cl;bytes:2;bt1:$66;bt2:$d3),
  (mnemonic:'SHL';opcode1:eo_reg4;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$c1),

  (mnemonic:'SHL';opcode1:eo_reg4;paramtype1:par_rm32;paramtype2:par_cl;bytes:1;bt1:$d3),
  (mnemonic:'SHL';opcode1:eo_reg4;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:1;bt1:$c1),



  (mnemonic:'SHLD';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_r16;paramtype3:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$a4),
  (mnemonic:'SHLD';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;paramtype3:par_cl;bytes:3;bt1:$66;bt2:$0f;bt3:$a5),

  (mnemonic:'SHLD';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_r32;paramtype3:par_imm8;bytes:2;bt1:$0f;bt2:$a4),
  (mnemonic:'SHLD';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;paramtype3:par_cl;bytes:2;bt1:$0f;bt2:$a5),


  (mnemonic:'SHR';opcode1:eo_reg5;paramtype1:par_rm8;paramtype2:par_1;bytes:1;bt1:$d0),
  (mnemonic:'SHR';opcode1:eo_reg5;paramtype1:par_rm8;paramtype2:par_cl;bytes:1;bt1:$d2),
  (mnemonic:'SHR';opcode1:eo_reg5;opcode2:eo_ib;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$c0),

  (mnemonic:'SHR';opcode1:eo_reg5;paramtype1:par_rm16;paramtype2:par_1;bytes:2;bt1:$66;bt2:$d1),
  (mnemonic:'SHR';opcode1:eo_reg5;paramtype1:par_rm16;paramtype2:par_cl;bytes:2;bt1:$66;bt2:$d3),
  (mnemonic:'SHR';opcode1:eo_reg5;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$c1),

  (mnemonic:'SHR';opcode1:eo_reg5;paramtype1:par_rm32;paramtype2:par_1;bytes:1;bt1:$d1),
  (mnemonic:'SHR';opcode1:eo_reg5;paramtype1:par_rm32;paramtype2:par_cl;bytes:1;bt1:$d3),
  (mnemonic:'SHR';opcode1:eo_reg5;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:1;bt1:$c1),

  (mnemonic:'SHRD';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_r32;paramtype3:par_imm8;bytes:2;bt1:$0f;bt2:$ac),
  (mnemonic:'SHRD';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;paramtype3:par_cl;bytes:2;bt1:$0f;bt2:$ac),

  (mnemonic:'SHUFPD';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_xmm_m128;paramtype3:par_imm8;bytes:3;bt1:$66;bt2:$0f;bt3:$c6),
  (mnemonic:'SHUFPS';opcode1:eo_reg;opcode2:eo_ib;paramtype1:par_xmm;paramtype2:par_xmm_m128;paramtype3:par_imm8;bytes:2;bt1:$0f;bt2:$c6),

  (mnemonic:'SIDT';opcode1:eo_reg1;paramtype1:par_m32;bytes:2;bt1:$0f;bt2:$01),
  (mnemonic:'SLDT';opcode1:eo_reg0;paramtype1:par_rm16;bytes:2;bt1:$0f;bt2:$00),

  (mnemonic:'SMSW';opcode1:eo_reg4;paramtype1:par_rm16;bytes:2;bt1:$0f;bt2:$01),

  (mnemonic:'SQRTPD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$51),
  (mnemonic:'SQRTPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$51),
  (mnemonic:'SQRTSD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m64;bytes:3;bt1:$f2;bt2:$0f;bt3:$51),
  (mnemonic:'SQRTSS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m32;bytes:3;bt1:$f2;bt2:$0f;bt3:$51),

  (mnemonic:'STC';bytes:1;bt1:$f9),
  (mnemonic:'STD';bytes:1;bt1:$fd),
  (mnemonic:'STI';bytes:1;bt1:$fb),

  (mnemonic:'STMXCSR';opcode1:eo_reg3;paramtype1:par_m32;bytes:2;bt1:$0f;bt2:$ae),

  (mnemonic:'STOSB';bytes:1;bt1:$aa),
  (mnemonic:'STOSD';bytes:1;bt1:$ab),
  (mnemonic:'STOSW';bytes:2;bt1:$66;bt2:$ab),

  (mnemonic:'STR';opcode1:eo_reg1;paramtype1:par_rm16;bytes:2;bt1:$0f;bt2:$00),


  (mnemonic:'SUB';opcode1:eo_ib;paramtype1:par_AL;paramtype2:par_imm8;bytes:1;bt1:$2c),
  (mnemonic:'SUB';opcode1:eo_iw;paramtype1:par_AX;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$2d),
  (mnemonic:'SUB';opcode1:eo_id;paramtype1:par_EAX;paramtype2:par_imm32;bytes:1;bt1:$2d),
  (mnemonic:'SUB';opcode1:eo_reg5;opcode2:eo_ib;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$80),
  (mnemonic:'SUB';opcode1:eo_reg5;opcode2:eo_iw;paramtype1:par_rm16;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$80),
  (mnemonic:'SUB';opcode1:eo_reg5;opcode2:eo_id;paramtype1:par_rm32;paramtype2:par_imm32;bytes:1;bt1:$81),
  (mnemonic:'SUB';opcode1:eo_reg5;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$83),
  (mnemonic:'SUB';opcode1:eo_reg5;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:1;bt1:$83),
  (mnemonic:'SUB';opcode1:eo_reg;paramtype1:par_rm8;paramtype2:par_r8;bytes:1;bt1:$28),
  (mnemonic:'SUB';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:2;bt1:$66;bt2:$29),
  (mnemonic:'SUB';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:1;bt1:$29),
  (mnemonic:'SUB';opcode1:eo_reg;paramtype1:par_r8;paramtype2:par_rm8;bytes:1;bt1:$2a),
  (mnemonic:'SUB';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:2;bt1:$66;bt2:$2b),
  (mnemonic:'SUB';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:1;bt1:$2b),

  (mnemonic:'SUBPD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$5c),
  (mnemonic:'SUBPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$5c),
  (mnemonic:'SUBSD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m64;bytes:3;bt1:$f2;bt2:$0f;bt3:$5c),
  (mnemonic:'SUBSS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m32;bytes:3;bt1:$f2;bt2:$0f;bt3:$5c),

  (mnemonic:'SYSENTER';bytes:2;bt1:$0f;bt2:$34),
  (mnemonic:'SYSEXIT';bytes:2;bt1:$0f;bt2:$35),


  (mnemonic:'TEST';opcode1:eo_ib;paramtype1:par_AL;paramtype2:par_imm8;bytes:1;bt1:$a8),
  (mnemonic:'TEST';opcode1:eo_iw;paramtype1:par_AX;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$a9),
  (mnemonic:'TEST';opcode1:eo_id;paramtype1:par_EAX;paramtype2:par_imm32;bytes:1;bt1:$a9),

  (mnemonic:'TEST';opcode1:eo_reg0;opcode2:eo_ib;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$f6),
  (mnemonic:'TEST';opcode1:eo_reg0;opcode2:eo_iw;paramtype1:par_rm16;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$f7),
  (mnemonic:'TEST';opcode1:eo_reg0;opcode2:eo_id;paramtype1:par_rm32;paramtype2:par_imm32;bytes:1;bt1:$f7),

  (mnemonic:'TEST';opcode1:eo_reg;paramtype1:par_rm8;paramtype2:par_r8;bytes:1;bt1:$84),
  (mnemonic:'TEST';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:2;bt1:$66;bt2:$85),
  (mnemonic:'TEST';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:1;bt1:$85),


  (mnemonic:'UCOMISD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m64;bytes:3;bt1:$66;bt2:$0f;bt3:$2e),
  (mnemonic:'UCOMISS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m32;bytes:2;bt1:$0f;bt2:$2e),

  (mnemonic:'UD2';bytes:2;bt1:$0f;bt2:$0b),

  (mnemonic:'UNPCKHPD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$15),
  (mnemonic:'UNPCKHPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$15),

  (mnemonic:'UNPCKLPD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$14),
  (mnemonic:'UNPCKLPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$14),

  (mnemonic:'VERR';opcode1:eo_reg4;paramtype1:par_rm16;bytes:2;bt1:$0f;bt2:$00),
  (mnemonic:'VERW';opcode1:eo_reg5;paramtype1:par_rm16;bytes:2;bt1:$0f;bt2:$00),

  (mnemonic:'VMCALL';bytes:3;bt1:$0f;bt2:$01;bt3:$c1),  
  (mnemonic:'VMCLEAR';opcode1:eo_reg6;paramtype1:par_m64;bytes:3;bt1:$66;bt2:$0f;bt3:$c7),
  (mnemonic:'VMLAUNCH';bytes:3;bt1:$0f;bt2:$01;bt3:$c2),
  (mnemonic:'VMPTRLD';opcode1:eo_reg6;paramtype1:par_m64;bytes:2;bt1:$0f;bt2:$c7),
  (mnemonic:'VMPTRST';opcode1:eo_reg7;paramtype1:par_m64;bytes:2;bt1:$0f;bt2:$c7),
  (mnemonic:'VMREAD';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:2;bt1:$0f;bt2:$78),
  (mnemonic:'VMRESUME';bytes:3;bt1:$0f;bt2:$01;bt3:$c3),
  (mnemonic:'VMWRITE';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:2;bt1:$0f;bt2:$79),  
  (mnemonic:'VMXOFF';bytes:3;bt1:$0f;bt2:$01;bt3:$c4),
  (mnemonic:'VMXON';opcode1:eo_reg6;paramtype1:par_m64;bytes:3;bt1:$f3;bt2:$0f;bt3:$c7),





  (mnemonic:'WAIT';bytes:1;bt1:$9b),
  (mnemonic:'WBINVD';bytes:2;bt1:$0f;bt2:$09),
  (mnemonic:'WRMSR';bytes:2;bt1:$0f;bt2:$30),

  (mnemonic:'XADD';opcode1:eo_reg;paramtype1:par_rm8;paramtype2:par_r8;bytes:2;bt1:$0f;bt2:$c0),
  (mnemonic:'XADD';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:3;bt1:$66;bt2:$0f;bt3:$c1),
  (mnemonic:'XADD';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:2;bt1:$0f;bt2:$c1),

  (mnemonic:'XCHG';opcode1:eo_prw;paramtype1:par_ax;paramtype2:par_r16;bytes:2;bt1:$66;bt2:$90),
  (mnemonic:'XCHG';opcode1:eo_prd;paramtype1:par_eax;paramtype2:par_r32;bytes:1;bt1:$90),

  (mnemonic:'XCHG';opcode1:eo_prw;paramtype1:par_r16;paramtype2:par_ax;bytes:2;bt1:$66;bt2:$90),

  (mnemonic:'XCHG';opcode1:eo_prd;paramtype1:par_r32;paramtype2:par_eax;bytes:1;bt1:$90),

  (mnemonic:'XCHG';opcode1:eo_reg;paramtype1:par_rm8;paramtype2:par_r8;bytes:1;bt1:$86),
  (mnemonic:'XCHG';opcode1:eo_reg;paramtype1:par_r8;paramtype2:par_rm8;bytes:1;bt1:$86),

  (mnemonic:'XCHG';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:2;bt1:$66;bt2:$87),
  (mnemonic:'XCHG';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:2;bt1:$66;bt2:$87),

  (mnemonic:'XCHG';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:1;bt1:$87),
  (mnemonic:'XCHG';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:1;bt1:$87),

  (mnemonic:'XLATB';bytes:1;bt1:$d7),

  (mnemonic:'XOR';opcode1:eo_ib;paramtype1:par_AL;paramtype2:par_imm8;bytes:1;bt1:$34),
  (mnemonic:'XOR';opcode1:eo_iw;paramtype1:par_AX;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$35),
  (mnemonic:'XOR';opcode1:eo_id;paramtype1:par_EAX;paramtype2:par_imm32;bytes:1;bt1:$35),
  (mnemonic:'XOR';opcode1:eo_reg6;opcode2:eo_ib;paramtype1:par_rm8;paramtype2:par_imm8;bytes:1;bt1:$80),
  (mnemonic:'XOR';opcode1:eo_reg6;opcode2:eo_iw;paramtype1:par_rm16;paramtype2:par_imm16;bytes:2;bt1:$66;bt2:$80),
  (mnemonic:'XOR';opcode1:eo_reg6;opcode2:eo_id;paramtype1:par_rm32;paramtype2:par_imm32;bytes:1;bt1:$81),
  (mnemonic:'XOR';opcode1:eo_reg6;opcode2:eo_ib;paramtype1:par_rm16;paramtype2:par_imm8;bytes:2;bt1:$66;bt2:$83),
  (mnemonic:'XOR';opcode1:eo_reg6;opcode2:eo_ib;paramtype1:par_rm32;paramtype2:par_imm8;bytes:1;bt1:$83),
  (mnemonic:'XOR';opcode1:eo_reg;paramtype1:par_rm8;paramtype2:par_r8;bytes:1;bt1:$30),
  (mnemonic:'XOR';opcode1:eo_reg;paramtype1:par_rm16;paramtype2:par_r16;bytes:2;bt1:$66;bt2:$31),
  (mnemonic:'XOR';opcode1:eo_reg;paramtype1:par_rm32;paramtype2:par_r32;bytes:1;bt1:$31),
  (mnemonic:'XOR';opcode1:eo_reg;paramtype1:par_r8;paramtype2:par_rm8;bytes:1;bt1:$32),
  (mnemonic:'XOR';opcode1:eo_reg;paramtype1:par_r16;paramtype2:par_rm16;bytes:2;bt1:$66;bt2:$33),
  (mnemonic:'XOR';opcode1:eo_reg;paramtype1:par_r32;paramtype2:par_rm32;bytes:1;bt1:$33),

  (mnemonic:'XORPD';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:3;bt1:$66;bt2:$0f;bt3:$57),
  (mnemonic:'XORPS';opcode1:eo_reg;paramtype1:par_xmm;paramtype2:par_xmm_m128;bytes:2;bt1:$0f;bt2:$57)


);

type
  PIndexArray=^TIndexArray;
  TIndex=record
    startentry: integer;
    SubIndex: PIndexArray;
    NextEntry: integer;
  end;
  TIndexArray=array [0..25] of TIndex;

type ttokens=array of string;
type TAssemblerBytes=array of byte;
function Assemble(opcode:string; address: dword;var bytes: TAssemblerBytes): boolean;
function GetOpcodesIndex(opcode: string): integer;

function tokenize(opcode:string; var tokens: ttokens): boolean;
function gettokentype(var token:string;token2: string):integer;
function getreg(reg: string;exceptonerror:boolean): integer; overload;
function getreg(reg: string): integer; overload;
function TokenToRegisterbit(token:string): integer;


var parameter1,parameter2,parameter3: integer;
    opcodenr: integer;

    assemblerindex: TIndexArray;

implementation

uses {$ifndef autoassemblerdll}cefuncproc,{$endif}symbolhandler;

function GetOpcodesIndex(opcode: string): integer;
{
will return the first entry in the opcodes list for this opcode
If not found, -1
}
var i: integer;
    index1,index2: integer;
    bestindex: PIndexArray;
    minindex,maxindex: integer;
begin
  opcode:=uppercase(opcode);
  result:=-1;
  if length(opcode)>0 then
  begin
    index1:=ord(opcode[1])-ord('A');
    if (index1<0) or (index1>25) then exit; //not alphabetical

    bestindex:=@assemblerindex[index1];
    if (bestindex[0].startentry=-1) then exit;

    if (assemblerindex[index1].SubIndex<>nil) and (length(opcode)>1) then
    begin
      index2:=ord(opcode[2])-ord('A');
      if (index2<0) or (index2>25) then exit; //not alphabetical

      bestindex:=@assemblerindex[index1].SubIndex[index2];
      if bestindex[0].startentry=-1 then exit; //no subitem2

    end;

    minindex:=bestindex[0].startentry;
    maxindex:=bestindex[0].NextEntry;

    if maxindex=-1 then
      if assemblerindex[index1].NextEntry<>-1 then
        maxindex:=assemblerindex[index1].NextEntry
      else
        maxindex:=opcodecount;

    if maxindex>opcodecount then
    begin
     // messagebox(0,pchar(opcode+':'+inttostr(maxindex)),'aaa',0);
      maxindex:=opcodecount;
    end;

    //now scan from minindex to maxindex for opcode
    for i:=minindex to maxindex do
      if opcodes[i].mnemonic=opcode then
      begin
        result:=i; //found it
        exit;
      end else if opcodes[i].mnemonic[1]<>opcode[1] then exit;

    //still here, not found, -1
  end;
end;

function isMemoryLocationDefault(parameter:string):boolean;
begin
  result:=(parameter[1]='[') and (parameter[length(parameter)]=']');
end;

procedure Add(var bytes: Tassemblerbytes; const A: array of byte);
var i: integer;
begin
  setlength(bytes,length(bytes)+length(a));
  for i:=0 to length(a)-1 do
    bytes[length(bytes)-length(a)+i]:=a[i];
end;

procedure AddWord(var Bytes: TAssemblerbytes; a: word);
begin
  add(bytes,[byte(a)]);
  add(bytes,[byte(a shr 8)]);
end;

procedure AddDword(var bytes: tassemblerbytes; a: dword);
begin
  add(bytes,[byte(a)]);
  add(bytes,[byte(a shr 8)]);
  add(bytes,[byte(a shr 16)]);
  add(bytes,[byte(a shr 24)]);
end;

procedure AddString(var bytes: Tassemblerbytes; s: string);
var i,j: integer;
begin
  j:=length(bytes);
  setlength(bytes,length(bytes)+length(s)-2); //not the quotes;

  for i:=2 to length(s)-1 do
  begin
    bytes[j]:=ord(s[i]);
    inc(j);
  end;
end;



function ValueToType(value: dword): integer;
begin
  result:=32;
  if value<=$ffff then
  begin
    result:=16;
    if value>=$8000 then result:=32;
  end;

  if value<=$ff then
  begin
    result:=8;
    if value>=$80 then result:=16;
  end;

  if result=32 then
  begin
    if integer(value)<0 then
    begin
      if integer(value)>=-128 then result:=8 else
      if integer(value)>=-32768 then result:=16;
    end;
  end;
end;

function StringValueToType(value: string): integer;
var x: dword;
    err: integer;
begin
  //this function converts a sttring to a valuetype depsnding on how it is written
  result:=0;

  val(value,x,err);
  if err>0 then exit;

  if length(value)=9 then result:=32 else
  if length(value)=5 then result:=16 else
  if length(value)=2 then result:=8;

  if result=0 then result:=ValueToType(x);
end;

function getreg(reg: string;exceptonerror:boolean): integer; overload;
begin
  result:=9;
  if (reg='EAX') or (reg='AX') or (reg='AL') or (reg='MM0') or (reg='XMM0') or (reg='ST(0)') or (reg='ST') or (reg='ES') or (reg='CR0') or (reg='DR0') then result:=0;
  if (reg='ECX') or (reg='CX') or (reg='CL') or (reg='MM1') or (reg='XMM1') or (reg='ST(1)') or (reg='CS') or (reg='CR1') or (reg='DR1') then result:=1;
  if (reg='EDX') or (reg='DX') or (reg='DL') or (reg='MM2') or (reg='XMM2') or (reg='ST(2)') or (reg='SS') or (reg='CR2') or (reg='DR2') then result:=2;
  if (reg='EBX') or (reg='BX') or (reg='BL') or (reg='MM3') or (reg='XMM3') or (reg='ST(3)') or (reg='DS') or (reg='CR3') or (reg='DR3') then result:=3;
  if (reg='ESP') or (reg='SP') or (reg='AH') or (reg='MM4') or (reg='XMM4') or (reg='ST(4)') or (reg='FS') or (reg='CR4') or (reg='DR4') then result:=4;
  if (reg='EBP') or (reg='BP') or (reg='CH') or (reg='MM5') or (reg='XMM5') or (reg='ST(5)') or (reg='GS') or (reg='CR5') or (reg='DR5') then result:=5;
  if (reg='ESI') or (reg='SI') or (reg='DH') or (reg='MM6') or (reg='XMM6') or (reg='ST(6)') or (reg='HS') or (reg='CR6') or (reg='DR6') then result:=6;
  if (reg='EDI') or (reg='DI') or (reg='BH') or (reg='MM7') or (reg='XMM7') or (reg='ST(7)') or (reg='IS') or (reg='CR7') or (reg='DR7') then result:=7;

  if (result=9) and exceptonerror then raise exception.Create('Invalid register');
end;

function getreg(reg: string): integer; overload;
begin
  result:=getreg(reg,true);
end;


function TokenToRegisterbit(token:string): integer;
begin
  result:=register32bit;

  if token='AL' then result:=register8bit else
  if token='CL' then result:=register8bit else
  if token='DL' then result:=register8bit else
  if token='BL' then result:=register8bit else
  if token='AH' then result:=register8bit else
  if token='CH' then result:=register8bit else
  if token='DH' then result:=register8bit else
  if token='BH' then result:=register8bit else

  if token='AX' then result:=register16bit else
  if token='CX' then result:=register16bit else
  if token='DX' then result:=register16bit else
  if token='BX' then result:=register16bit else
  if token='SP' then result:=register16bit else
  if token='BP' then result:=register16bit else
  if token='SI' then result:=register16bit else
  if token='DI' then result:=register16bit else

  if token='EAX' then result:=register32bit else
  if token='ECX' then result:=register32bit else
  if token='EDX' then result:=register32bit else
  if token='EBX' then result:=register32bit else
  if token='ESP' then result:=register32bit else
  if token='EBP' then result:=register32bit else
  if token='ESI' then result:=register32bit else
  if token='EDI' then result:=register32bit else

  if token='MM0' then result:=registerMM else
  if token='MM1' then result:=registerMM else
  if token='MM2' then result:=registerMM else
  if token='MM3' then result:=registerMM else
  if token='MM4' then result:=registerMM else
  if token='MM5' then result:=registerMM else
  if token='MM6' then result:=registerMM else
  if token='MM7' then result:=registerMM else

  if token='XMM0' then result:=registerXMM else
  if token='XMM1' then result:=registerXMM else
  if token='XMM2' then result:=registerXMM else
  if token='XMM3' then result:=registerXMM else
  if token='XMM4' then result:=registerXMM else
  if token='XMM5' then result:=registerXMM else
  if token='XMM6' then result:=registerXMM else
  if token='XMM7' then result:=registerXMM else

  if token='ST' then result:=registerST else
  if token='ST(0)' then result:=registerST else
  if token='ST(1)' then result:=registerST else
  if token='ST(2)' then result:=registerST else
  if token='ST(3)' then result:=registerST else
  if token='ST(4)' then result:=registerST else
  if token='ST(5)' then result:=registerST else
  if token='ST(6)' then result:=registerST else
  if token='ST(7)' then result:=registerST else

  if token='ES' then result:=registersreg else
  if token='CS' then result:=registersreg else
  if token='SS' then result:=registersreg else
  if token='DS' then result:=registersreg else
  if token='FS' then result:=registersreg else
  if token='GS' then result:=registersreg else
  if token='HS' then result:=registersreg else
  if token='IS' then result:=registersreg else

  if token='CR0' then result:=registerCR else
  if token='CR1' then result:=registerCR else
  if token='CR2' then result:=registerCR else
  if token='CR3' then result:=registerCR else
  if token='CR4' then result:=registerCR else
  if token='CR5' then result:=registerCR else
  if token='CR6' then result:=registerCR else
  if token='CR7' then result:=registerCR else

  if token='DR0' then result:=registerDR else
  if token='DR1' then result:=registerDR else
  if token='DR2' then result:=registerDR else
  if token='DR3' then result:=registerDR else
  if token='DR4' then result:=registerDR else
  if token='DR5' then result:=registerDR else
  if token='DR6' then result:=registerDR else
  if token='DR7' then result:=registerDR;

end;

function gettokentype(var token:string;token2: string):integer;
var i,err: integer;
    temp:string;
begin
  result:=0;
  if length(token)=0 then exit;

  result:=tokenToRegisterbit(token);

  temp:=ConvertHexStrToRealStr(token);
  val(temp,i,err);
  if err=0 then
  begin
    result:=value;
    token:=temp;
  end;

  //see if it is a memorylocation
  //can start with [ or ptr [

  if pos('[',token)>0 then result:=memorylocation;
  if (pos('BYTE PTR [',token)>0) then result:=memorylocation8;
  if (pos('WORD PTR [',token)>0) then result:=memorylocation16;
  if (pos('DWORD PTR [',token)>0) then result:=memorylocation32;
  if (pos('QWORD PTR [',token)>0) then result:=memorylocation64;
  if (pos('TBYTE PTR [',token)>0) then result:=memorylocation80;
  if (pos('TWORD PTR [',token)>0) then result:=memorylocation80;
  if (pos('DQWORD PTR [',token)>0) then result:=memorylocation128;

  if result=memorylocation then
  begin
    if token2='' then
    begin
      result:=memorylocation32;
      exit;
    end;

    //I need the helper param to figure it out
    case TokenToRegisterbit(token2) of
      register8bit:  result:=memorylocation8;
      registersreg,register16bit: result:=memorylocation16;

      else result:=memorylocation32;
    end;
  end;

end;

function isrm8(parametertype:integer): boolean;
begin
  if (parametertype=memorylocation8) or (parametertype=register8bit) then result:=true else result:=false;
end;

function isrm16(parametertype:integer): boolean;
begin
  if (parametertype=memorylocation16) or (parametertype=register16bit) then result:=true else result:=false;
end;

function isrm32(parametertype:integer): boolean;
begin
  if (parametertype=memorylocation32) or (parametertype=register32bit) then result:=true else result:=false;
end;

function ismm_m32(parametertype:integer): boolean;
begin
  if (parametertype=registerMM) or (parametertype=memorylocation32) then result:=true else result:=false;
end;

function ismm_m64(parametertype:integer): boolean;
begin
  if (parametertype=registerMM) or (parametertype=memorylocation64) then result:=true else result:=false;
end;

function isxmm_m32(parametertype:integer): boolean;
begin
  if (parametertype=registerXMM) or (parametertype=memorylocation32) then result:=true else result:=false;
end;

function isxmm_m64(parametertype:integer): boolean;
begin
  if (parametertype=registerXMM) or (parametertype=memorylocation64) then result:=true else result:=false;
end;

function isxmm_m128(parametertype:integer):boolean;
begin
  if (parametertype=registerxmm) or (parametertype=memorylocation128) then result:=true else result:=false;
end;

function rewrite(var token:string): boolean;
var i,j,k,err,err2: integer;
    a,b: dword;
    tokens: array of string;
    last: integer;

    symbol: PImagehlpSymbol;
    disp: dword;

    temp: string;

begin
  setlength(tokens,0);
  result:=false;
  last:=-1;

  { 5.4: special pointer notation case }
  if (length(token)>4) and (token[1]+token[2]='[[') and (token[length(token)]=']') then
  begin
    //looks like a pointer in a address specifier (idiot user detected...)


    token:='['+inttohex(symhandler.getaddressfromname(copy(token,2,length(token)-2)),8)+']';
  end;


  { 5.4 ^^^ }

  temp:='';
  i:=1;
  while i<=length(token) do
  begin
    if token[i] in ['[',']','+','-'] then
    begin
      if temp<>'' then
      begin
        setlength(tokens,length(tokens)+1);
        tokens[length(tokens)-1]:=temp;
        temp:='';
      end;
      setlength(tokens,length(tokens)+1);
      tokens[length(tokens)-1]:=token[i];
      inc(i);
      continue;
    end;
    temp:=temp+token[i];
    inc(i);
  end;

  if temp<>'' then
  begin
    setlength(tokens,length(tokens)+1);
    tokens[length(tokens)-1]:=temp;
    temp:='';
  end;

  getmem(symbol,sizeof(_Imagehlp_symbol)+200);
  try
    for i:=0 to length(tokens)-1 do
    begin
      if (length(tokens[i])>1) or (not (tokens[i][1] in ['[',']','+','-','*'])) then
      begin
        val('$'+tokens[i],j,err);
        if (err<>0) and (getreg(tokens[i],false)=9) then
        begin
          symbol.SizeOfStruct:=sizeof(_Imagehlp_symbol)+200;
          symbol.MaxNameLength:=200;

          disp:=0;

          try
            tokens[i]:=inttohex(symhandler.getaddressfromname(tokens[i]),8);
//            tokens[i]:=inttohex(symhandler.getaddressfromname(uppercase(tokens[i])),8);

          except
           // raise exception.create(tokens[i]+' is an unknown identifier');

          end;

        end;
      end;
    end;
  finally
    freemem(symbol);
  end;

  //do some calculations

  //check multiply first
  for i:=1 to length(tokens)-2 do
  begin
    if tokens[i]='*' then
    begin
      val('$'+tokens[i-1],a,err);
      val('$'+tokens[i+1],b,err2);
      if (err=0) and (err2=0) then
      begin
        a:=a*b;
        tokens[i-1]:=inttohex(a,8);
        tokens[i]:='';
        tokens[i+1]:='';
      end;
    end;
  end;

  for i:=1 to length(tokens)-2 do
  begin
    val('$'+tokens[i-1],a,err);
    val('$'+tokens[i+1],b,err2);
    if (err=0) and (err2=0) then
    begin
      case tokens[i][1] of
        '+':
        begin
          a:=a+b;
          tokens[i-1]:=inttohex(a,8);
          tokens[i]:='';
          tokens[i+1]:='';
        end;

        '-':
        begin
          a:=a-b;
          tokens[i-1]:=inttohex(a,8);
          tokens[i]:='';
          tokens[i+1]:='';
        end;
      end;
    end;
  end;


  token:='';
  for i:=0 to length(tokens)-1 do
    token:=token+tokens[i];

  setlength(tokens,0);
  result:=true;
end;


function tokenize(opcode:string; var tokens: ttokens): boolean;
var i,j,last: integer;
    spacecount: integer;
    seperatorcount: integer;

    firstquote: boolean;

begin
  setlength(tokens,0);

  while (length(opcode)>0) and ((opcode[length(opcode)]=' ') or (opcode[length(opcode)]=',')) do
    opcode:=copy(opcode,1,length(opcode)-1);

  last:=1;
  firstquote:=false;
  for i:=1 to length(opcode) do
  begin
    if (i=length(opcode)) or (opcode[i]=' ') or (opcode[i]=',') or (opcode[i]='''') then
    begin
      if not firstquote then
      begin
        setlength(tokens,length(tokens)+1);
        if i=length(opcode) then
          tokens[length(tokens)-1]:=copy(opcode,last,i-last+1)
        else
          tokens[length(tokens)-1]:=copy(opcode,last,i-last);



        tokens[length(tokens)-1]:=uppercase(tokens[length(tokens)-1]);

        if (tokens[length(tokens)-1]='DQWORD')
        or (tokens[length(tokens)-1]='TBYTE')
        or (tokens[length(tokens)-1]='TWORD')
        or (tokens[length(tokens)-1]='QWORD')
        or (tokens[length(tokens)-1]='DWORD')
        or (tokens[length(tokens)-1]='WORD')
        or (tokens[length(tokens)-1]='BYTE')
        or (tokens[length(tokens)-1]='DQWORD PTR')
        or (tokens[length(tokens)-1]='TBYTE PTR')
        or (tokens[length(tokens)-1]='TWORD PTR')
        or (tokens[length(tokens)-1]='QWORD PTR')
        or (tokens[length(tokens)-1]='DWORD PTR')
        or (tokens[length(tokens)-1]='WORD PTR')
        or (tokens[length(tokens)-1]='BYTE PTR')
        then
        begin
          setlength(tokens,length(tokens)-1)
        end
        else
        begin
          last:=i+1;

          if (length(tokens)>1) then
          begin
            //Rewrite
            rewrite(tokens[length(tokens)-1]);
          end;
        end;
                  
        if opcode[i]='''' then firstquote:=true;
      end
      else
      begin
        //inside a quote and a token seperator was encountered
        if opcode[i]='''' then //check if it is the string terminator
        begin
          firstquote:=false;
          if i=length(opcode) then
            tokens[length(tokens)-1]:=copy(opcode,last-1,i-last+2)
          else
            tokens[length(tokens)-1]:=copy(opcode,last-1,i-last+2);
          last:=i+1;
        end;
        
      end;
    end;

  end;

  i:=0;
  while i<length(tokens) do
  begin
    if (tokens[i]='') or (tokens[i]=' ') or (tokens[i]=',') then
    begin
      for j:=i to length(tokens)-2 do
        tokens[j]:=tokens[j+1];
      setlength(tokens,length(tokens)-1);
      continue;
    end;
    inc(i);
  end;

  result:=true;
end;

procedure setsibscale(var sib: byte;i:byte);
begin
  sib:=(sib and $3f) or (i shl 6);
end;

procedure setsibindex(var sib: byte; i:byte);
begin
  sib:=(sib and $c7) or (i shl 3);
end;

procedure setsibbase(var sib:byte; i:byte);
begin
  sib:=(sib and $f8) or i;
end;


procedure setrm(var modrm: byte;i: byte);
begin
  modrm:=(modrm and $f8) or i;
end;

procedure setmod(var modrm: byte;i: byte);
begin
  modrm:=(modrm and $3f) or (i shl 6);
end;

function getmod(modrm: byte):byte;
begin
  result:=modrm shr 6;
end;

procedure createsibscaleindex(var sib:byte;reg:string);
begin
  if pos('2',reg)>0 then
    setsibscale(sib,1)
  else
  if pos('4',reg)>0 then
    setsibscale(sib,2)
  else
  if pos('8',reg)>0 then
    setsibscale(sib,3)
  else setsibscale(sib,0);

  if pos('EAX',reg)>0 then setsibindex(sib,0) else
  if pos('ECX',reg)>0 then setsibindex(sib,1) else
  if pos('EDX',reg)>0 then setsibindex(sib,2) else
  if pos('EBX',reg)>0 then setsibindex(sib,3) else
  if ((reg='') or (pos('ESP',reg)>0)) then setsibindex(sib,4) else //if esp it is invalid, but if the user types it it'll compile
  if pos('EBP',reg)>0 then setsibindex(sib,5) else
  if pos('ESI',reg)>0 then setsibindex(sib,6) else
  if pos('EDI',reg)>0 then setsibindex(sib,7) else
    raise exception.Create('WTF is a '+reg);

end;



procedure setmodrm(var modrm:tassemblerbytes;address:string);
var regs: string;
    disp,test: dword;
    i,j,k,l: integer;
    start: integer;
    increase: boolean;

    go: boolean;
    splitup: array of string;
    temp:string;

    reg1,reg2: string;
    reg: array[-1..1] of string;
    found: boolean;
begin
  //first split the address string up

  go:=false;
  start:=1;
  for i:=1 to length(address) do
  begin
    if i=length(address) then
    begin
      if not (address[i] in ['+','-']) then
      begin
        setlength(splitup,length(splitup)+1);
        splitup[length(splitup)-1]:=copy(address,start,(i+1)-start);
      end;
    end
    else
    if not (address[i] in ['+','-']) then go:=true
    else
    if address[i] in ['+','-'] then
    begin
      if go then
      begin
        setlength(splitup,length(splitup)+1);
        splitup[length(splitup)-1]:=copy(address,start,i-start);
        start:=i; //copy the + or - sign
        go:=false;
      end;
    end;
  end;

  disp:=0;
  regs:='';
  for i:=0 to length(splitup)-1 do
  begin
    increase:=true;
    for j:=1 to length(splitup[i]) do
      if (splitup[i][j] in ['+','-']) then
      begin
        if splitup[i][j]='-' then increase:=not increase;
      end else
      begin
        if j=length(splitup[i]) then
          temp:=copy(splitup[i],j,length(splitup[i])-j+1)
        else
          temp:=copy(splitup[i],j,length(splitup[i])-j+1);
        break;
      end;

    if length(temp)=0 then raise exception.Create('I don''t understand what you mean with '+address);
    if temp[1]='$' then val(temp,test,j) else val('$'+temp,test,j);

    if j>0 then //a register or a stupid user
    begin
      regs:=regs+temp+'+';
    end
    else
    begin  //a value
      if increase then inc(disp,test) else dec(disp,test);
    end;


  end;
  if length(regs)>0 then regs:=copy(regs,1,length(regs)-1);

  //regs and disp are now set
  //compare the regs with posibilities     (only 1 time +, and only 1 time *)
  j:=0; k:=0;

  for i:=1 to length(regs) do
  begin
    if regs[i]='+' then inc(j);
    if regs[i]='*' then inc(k);
  end;

  if (j>1) or (k>1) then raise exception.Create('I don''t understand what you mean with '+address);

  if disp=0 then setmod(modrm[0],0) else
  if (integer(disp)>=-128) and (integeR(disp)<=127) then setmod(modrm[0],1) else setmod(modrm[0],2);
{
  begin

  if disp=0 then
  begin
    setmod(modrm[0],0);
  end else
  if disp<=255 then
  begin
    setmod(modrm[0],1);
  end else setmod(modrm[0],2);
  end;
 }

  reg1:='';
  reg2:='';
  if pos('+',regs)>0 then
  begin
    reg1:=copy(regs,1,pos('+',regs)-1);
    reg2:=copy(regs,pos('+',regs)+1,length(regs));

    k:=2;
  end else
  begin
    reg1:=regs;
    k:=1;
  end;

  reg[-1]:=reg1;
  reg[1]:=reg2;

//  if reg1<>'' then getreg(reg1);
//  if reg2<>'' then getreg(reg2);



  k:=1;



  if (reg1<>'') and (reg2='') and (pos('*',reg1)>0) then
  begin
    setmod(modrm[0],0);
    setrm(modrm[0],4);
    setlength(modrm,2);
    setsibbase(modrm[1],5);
    createsibscaleindex(modrm[1],reg[-1]);
    adddword(modrm,disp);
    found:=true;

  end;

  if (reg[k]='') and (reg[-k]='') then
  begin
    //no registers, just a address
    setrm(modrm[0],5);
    setmod(modrm[0],0);
    adddword(modrm,disp);
    found:=true;
  end;

  if (reg[k]='EAX') or (reg[-k]='EAX') then
  begin
    if reg[-k]='EAX' then k:=-k;

    if (reg[-k]<>'') then //sib needed
    begin
      setrm(modrm[0],4);
      setlength(modrm,2);
      setsibbase(modrm[1],0);
      createsibscaleindex(modrm[1],reg[-k]);
    end else setrm(modrm[0],0); //no sib needed
    found:=true;
  end;
  
  if (reg[k]='ECX') or (reg[-k]='ECX') then
  begin
    if reg[-k]='ECX' then k:=-k;

    if (reg[-k]<>'') then //sib needed
    begin
      setrm(modrm[0],4);
      setlength(modrm,2);
      setsibbase(modrm[1],1);
      createsibscaleindex(modrm[1],reg[-k]);
    end else setrm(modrm[0],1); //no sib needed
    found:=true;
  end;

  if (reg[k]='EDX') or (reg[-k]='EDX') then
  begin
    if reg[-k]='EDX' then k:=-k;

    if (reg[-k]<>'') then //sib needed
    begin
      setrm(modrm[0],4);
      setlength(modrm,2);
      setsibbase(modrm[1],2);
      createsibscaleindex(modrm[1],reg[-k]);
    end else setrm(modrm[0],2); //no sib needed
    found:=true;
  end;

  if (reg[k]='EBX') or (reg[-k]='EBX') then
  begin
    if reg[-k]='EBX' then k:=-k;

    if (reg[-k]<>'') then //sib needed
    begin
      setrm(modrm[0],4);
      setlength(modrm,2);
      setsibbase(modrm[1],3);
      createsibscaleindex(modrm[1],reg[-k]);
    end else setrm(modrm[0],3); //no sib needed
    found:=true;
  end;


  if (reg[k]='ESP') or (reg[-k]='ESP') then //nope...
  begin
    if reg[-k]='ESP' then k:=-k;


    setrm(modrm[0],4);
    setlength(modrm,2);
    setsibbase(modrm[1],4);
    createsibscaleindex(modrm[1],reg[-k]);
    found:=true;
  end;

  if (reg[k]='EBP') or (reg[-k]='EBP') then
  begin
    if reg[-k]='EBP' then k:=-k;

    if disp=0 then setmod(modrm[0],1);

    if (reg[-k]<>'') then //sib needed
    begin
      setrm(modrm[0],4);
      setlength(modrm,2);
      setsibbase(modrm[1],5);
      createsibscaleindex(modrm[1],reg[-k]);
    end else setrm(modrm[0],5); //no sib needed
    found:=true;
  end;

  if (reg[k]='ESI') or (reg[-k]='ESI') then
  begin
    if reg[-k]='ESI' then k:=-k;

    if (reg[-k]<>'') then //sib needed
    begin
      setrm(modrm[0],4);
      setlength(modrm,2);
      setsibbase(modrm[1],6);
      createsibscaleindex(modrm[1],reg[-k]);
    end else setrm(modrm[0],6); //no sib needed
    found:=true;
  end;

  if (reg[k]='EDI') or (reg[-k]='EDI') then
  begin
    if reg[-k]='EDI' then k:=-k;

    if (reg[-k]<>'') then //sib needed
    begin
      setrm(modrm[0],4);
      setlength(modrm,2);
      setsibbase(modrm[1],7);
      createsibscaleindex(modrm[1],reg[-k]);
    end else setrm(modrm[0],7); //no sib needed
    found:=true;
  end;

  if not found then raise exception.create('Invalid address');


  i:=getmod(modrm[0]);
  if i=1 then add(modrm,[byte(disp)]);
  if i=2 then adddword(modrm,disp);

 // hgihkjjkjj
end;

function createModRM(var bytes: tassemblerbytes;reg:integer;param: string):boolean;
var address: string;
    modrm: tassemblerbytes;
    i,j: integer;
begin
  setlength(modrm,1);
  modrm[0]:=0;
  result:=false;

  address:=copy(param,pos('[',param)+1,pos(']',param)-pos('[',param)-1);
  if address='' then
  begin
    //register //modrm c0 to ff
    setmod(modrm[0],3);
    if (param='EAX') or (param='AX') or (param='AL') or (param='MM0') or (param='XMM0') then setrm(modrm[0],0) else
    if (param='ECX') or (param='CX') or (param='CL') or (param='MM1') or (param='XMM1') then setrm(modrm[0],1) else
    if (param='EDX') or (param='DX') or (param='DL') or (param='MM2') or (param='XMM2') then setrm(modrm[0],2) else
    if (param='EBX') or (param='BX') or (param='BL') or (param='MM3') or (param='XMM3') then setrm(modrm[0],3) else
    if (param='ESP') or (param='SP') or (param='AH') or (param='MM4') or (param='XMM4') then setrm(modrm[0],4) else
    if (param='EBP') or (param='BP') or (param='CH') or (param='MM5') or (param='XMM5') then setrm(modrm[0],5) else
    if (param='ESI') or (param='SI') or (param='DH') or (param='MM6') or (param='XMM6') then setrm(modrm[0],6) else
    if (param='EDI') or (param='DI') or (param='BH') or (param='MM7') or (param='XMM7') then setrm(modrm[0],7) else
    raise exception.Create('I don''t understand what you mean with '+param);
  end else setmodrm(modrm,address);

  //setreg
  if reg>7 then raise exception.Create('The assembler tried to set a register value that is too high');
  if reg=-1 then reg:=0;
  modrm[0]:=modrm[0]+(reg shl 3);

  j:=length(bytes);
  setlength(bytes,length(bytes)+length(modrm));
  for i:=0 to length(modrm)-1 do
    bytes[j+i]:=modrm[i];


  result:=true;
end;

procedure addopcode(var bytes:tassemblerbytes;i:integer);
begin
  add(bytes,[opcodes[i].bt1]);
  if opcodes[i].bytes>1 then add(bytes,[opcodes[i].bt2]);
  if opcodes[i].bytes=3 then add(bytes,[opcodes[i].bt3]);
end;

function eoToReg(eo:TExtraOpcode):integer;
begin
  result:=-1;
  case eo of
    eo_reg0: result:=0;
    eo_reg1: result:=1;
    eo_reg2: result:=2;
    eo_reg3: result:=3;
    eo_reg4: result:=4;
    eo_reg5: result:=5;
    eo_reg6: result:=6;
    eo_reg7: result:=7;
  end;
end;

function Assemble(opcode:string; address: dword;var bytes: TAssemblerBytes): boolean;
var tokens: ttokens;
    i,j,k,l: integer;
    v,v2: dword;
    mnemonic,nroftokens: integer;
    paramtype1,paramtype2,paramtype3: integer;
    parameter1,parameter2,parameter3: string;
    vtype,v2type: integer;

    first,last: integer;
    startoflist,endoflist: integer;

    tempstring: string;
begin
  {$ifdef checkassembleralphabet}
  for i:=2 to opcodecount do
    if opcodes[i].mnemonic<opcodes[i-1].mnemonic then raise exception.Create('FUCK YOU! THE PROGRAMMER WAS STUPID ENOUGH TO MESS THIS PART UP IN PART '+IntToStr(i)+' '+opcodes[i-1].mnemonic+'<'+opcodes[i].mnemonic);
  {$endif}


  result:=false;

  tokenize(opcode,tokens);

  nroftokens:=length(tokens);

  if nroftokens=0 then exit;

  if tokens[0]='DB' then
  begin
    for i:=1 to nroftokens-1 do
    begin
      if tokens[i][1]='''' then //string
      begin
        //find the original non uppercase stringpos in the opcode
        j:=pos(tokens[i],uppercase(opcode));

        if j>0 then
        begin
          tempstring:=copy(opcode,j,length(tokens[i]));
          addstring(bytes,tempstring);
        end
        else addstring(bytes,tokens[i]); //lets try to save face...
      end
      else
        add(bytes,[strtoint('$'+tokens[i])]);
    end;

    result:=true;
    exit;
  end;

  if tokens[0]='DW' then
  begin
    for i:=1 to nroftokens-1 do
      addword(bytes,strtoint('$'+tokens[i]));
    result:=true;
    exit;
  end;

  if tokens[0]='DD' then
  begin
    for i:=1 to nroftokens-1 do
      adddword(bytes,strtoint('$'+tokens[i]));
    result:=true;
    exit;
  end;


  mnemonic:=-1;
  for i:=0 to length(tokens)-1 do
  begin
    if not ((tokens[i]='LOCK') or (tokens[i]='REP') or (tokens[i]='REPNE') or (tokens[i]='REPE')) then
    begin
      mnemonic:=i;
      break;
    end;
  end;

  if mnemonic=-1 then exit;

  setlength(bytes,mnemonic);
  for i:=0 to mnemonic-1 do
  begin
    if tokens[i]='REP' then bytes[i]:=$f3 else
    if tokens[i]='REPNE' then bytes[i]:=$f2 else
    if tokens[i]='REPE' then bytes[i]:=$f3 else
    if tokens[i]='LOCK' then bytes[i]:=$f0;
  end;


  //this is just to speed up the finding of the right opcode
  //I could have done a if mnemonic=... then ... else if mnemonic=... then ..., but that would be slow, VERY SLOW

  if (nroftokens-1)>=mnemonic+1 then parameter1:=tokens[mnemonic+1] else parameter1:='';
  if (nroftokens-1)>=mnemonic+2 then parameter2:=tokens[mnemonic+2] else parameter2:='';
  if (nroftokens-1)>=mnemonic+3 then parameter3:=tokens[mnemonic+3] else parameter3:='';

  paramtype1:=gettokentype(parameter1,parameter2);
  paramtype2:=gettokentype(parameter2,parameter1);
  paramtype3:=gettokentype(parameter3,'');

  if (paramtype1>=memorylocation) and (paramtype1<=memorylocation128) then
  begin
    if pos('ES:',parameter1)>0 then
    begin
      setlength(bytes,length(bytes)+1);
      bytes[length(bytes)-1]:=$26;
    end;

    if pos('CS:',parameter1)>0 then
    begin
      setlength(bytes,length(bytes)+1);
      bytes[length(bytes)-1]:=$2e;
    end;

    if pos('SS:',parameter1)>0 then
    begin
      setlength(bytes,length(bytes)+1);
      bytes[length(bytes)-1]:=$36;
    end;

    if pos('DS:',parameter1)>0 then
    begin
      setlength(bytes,length(bytes)+1);
      bytes[length(bytes)-1]:=$3e;
    end;

    if pos('FS:',parameter1)>0 then
    begin
      setlength(bytes,length(bytes)+1);
      bytes[length(bytes)-1]:=$64;
    end;

    if pos('GS:',parameter1)>0 then
    begin
      setlength(bytes,length(bytes)+1);
      bytes[length(bytes)-1]:=$65;
    end;
  end;

  if (paramtype2>=memorylocation) and (paramtype2<=memorylocation128) then
  begin
    if pos('ES:',parameter2)>0 then
    begin
      setlength(bytes,length(bytes)+1);
      bytes[length(bytes)-1]:=$26;
    end;

    if pos('CS:',parameter2)>0 then
    begin
      setlength(bytes,length(bytes)+1);
      bytes[length(bytes)-1]:=$2e;
    end;

    if pos('SS:',parameter2)>0 then
    begin
      setlength(bytes,length(bytes)+1);
      bytes[length(bytes)-1]:=$36;
    end;

    if pos('DS:',parameter2)>0 then
    begin
      setlength(bytes,length(bytes)+1);
      bytes[length(bytes)-1]:=$3e;
    end;

    if pos('FS:',parameter2)>0 then
    begin
      setlength(bytes,length(bytes)+1);
      bytes[length(bytes)-1]:=$64;
    end;

    if pos('GS:',parameter2)>0 then
    begin
      setlength(bytes,length(bytes)+1);
      bytes[length(bytes)-1]:=$65;
    end;
  end;


  v:=0;
  vtype:=0;
  v2type:=0;

  if paramtype1=value then
  begin
    v:=StrToInt(parameter1);
    vtype:=StringValueToType(parameter1);
  end;

  if paramtype2=value then if v=0 then
  begin
    v:=StrToInt(parameter2);
    vtype:=StringValueToType(parameter2);
  end
  else
  begin
    v2:=StrToInt(parameter2);
    v2type:=StringValueToType(parameter2);
  end;

  if paramtype3=value then if v=0 then
  begin
    v:=StrToInt(parameter3);
    vtype:=StringValueToType(parameter3);
  end
  else
  begin
    v2:=StrToInt(parameter3);
    v2type:=StringValueToType(parameter3);
  end;

  result:=false;
  //this is just a test to see if I can do the assembler stuff a little easier

  {
  //sorted scan (nice and fast)
  first:=1;
  last:=opcodecount;

  j:=first+((last-first) div 2);
  k:=j-1;
  dec(k);

  while (first<last) do
  begin
    j:=first+((last-first) div 2);

    if k=j then
    begin
      if k=first then
      begin
        first:=last;
        j:=last;
      end;

      if k=last then
      begin
        last:=first;
        j:=last;
      end;
    end;

    k:=j;

    if opcodes[j].mnemonic=tokens[mnemonic] then break;

    if opcodes[j].mnemonic>tokens[mnemonic] then last:=j
                                            else first:=j;
  end;

  if opcodes[j].mnemonic<>tokens[mnemonic] then exit;

  while (j>=1) and (opcodes[j].mnemonic=tokens[mnemonic]) do dec(j);
  inc(j);
     }


  j:=GetOpcodesIndex(tokens[mnemonic]); //index scan, better than sorted
  if j=-1 then exit;

  startoflist:=j;
  endoflist:=startoflist;

  while opcodes[endoflist].mnemonic=tokens[mnemonic] do inc(endoflist);
  dec(endoflist);


  while j<=opcodecount do
  begin
    if opcodes[j].mnemonic<>tokens[mnemonic] then exit;


    //no param
    if (opcodes[j].paramtype1=par_noparam) and (parameter1='') then
    begin
      //no param
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          //no_param,no_param,no_param

          if (opcodes[j].opcode1=eo_none) and (opcodes[j].opcode1=eo_none) then
          begin
            //eo_none,eo_none--no_param,no_param,no_param
            addopcode(bytes,j);
            result:=true;
            exit;
          end;
        end;
      end;
    end;


    if (opcodes[j].paramtype1=par_imm8) and (paramtype1=value) then
    begin
      //imm8,
      if (opcodes[j].paramtype2=par_al) and (parameter2='AL') then
      begin
        //imm8,al
        addopcode(bytes,j);
        add(bytes,[v]);
        result:=true;
        exit;
      end;

      if (opcodes[j].paramtype2=par_ax) and (parameter2='AX') then
      begin
        //imm8,ax /?
        addopcode(bytes,j);
        add(bytes,[v]);
        result:=true;
        exit;
      end;

      if (opcodes[j].paramtype2=par_eax) and (parameter2='EAX') then
      begin
        //imm8,eax
        addopcode(bytes,j);
        add(bytes,[v]);
        result:=true;
        exit;
      end;



      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        if vtype=16 then
        begin
          //see if there is also a 'opcode imm16' variant
          k:=startoflist;
          while (k<=opcodecount) and (opcodes[j].mnemonic=tokens[mnemonic]) do
          begin
            if (opcodes[k].paramtype1=par_imm16) then
            begin
              addopcode(bytes,k);
              addword(bytes,v);
              result:=true;
              exit;
            end;

            inc(k);
          end;
        end;

        if vtype=32 then
        begin
          //see if there is also a 'opcode imm32' variant
          k:=startoflist;
          while (k<=opcodecount) and (opcodes[j].mnemonic=tokens[mnemonic]) do
          begin
            if (opcodes[k].paramtype1=par_imm32) then
            begin
              addopcode(bytes,k);
              adddword(bytes,v);
              result:=true;
              exit;
            end;

            inc(k);
          end;
        end;


        //op imm8
        addopcode(bytes,j);
        add(bytes,[byte(v)]);
        result:=true;
        exit;
      end;
    end;

    if (opcodes[j].paramtype1=par_imm16) and (paramtype1=value) then
    begin
      //imm16,
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        //imm16
        addopcode(bytes,j);
        addword(bytes,v);
        result:=true;
        exit;
      end;


      if (opcodes[j].paramtype2=par_imm8) and (paramtype2=value) then
      begin
        //imm16,imm8,
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          addword(bytes,v);
          add(bytes,[v2]);
          result:=true;
          exit;
        end;
      end;
    end;

    if (opcodes[j].paramtype1=par_moffs8) and ((paramtype1=memorylocation8) or (ismemorylocationdefault(parameter1)  ))  then
    begin
      if (opcodes[j].paramtype2=par_al) and (parameter2='AL') then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          k:=pos('[',parameter1);
          l:=pos(']',parameter1);
          val('$'+copy(parameter1,k+1,l-k-1),v,k);
          if k=0 then
          begin
            //verified, it doesn't have a registerbase in it
            addopcode(bytes,j);
            adddword(bytes,v);
            result:=true;
            exit;
          end;
        end;
      end;
    end;

    if (opcodes[j].paramtype1=par_moffs16) and ((paramtype1=memorylocation16) or (ismemorylocationdefault(parameter1)  ))  then
    begin
      if (opcodes[j].paramtype2=par_ax) and (parameter2='AX') then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          k:=pos('[',parameter1);
          l:=pos(']',parameter1);
          val('$'+copy(parameter1,k+1,l-k-1),v,k);
          if k=0 then
          begin
            //verified, it doesn't have a registerbase in it
            addopcode(bytes,j);
            adddword(bytes,v);
            result:=true;
            exit;
          end;
        end;
      end;
    end;

    if (opcodes[j].paramtype1=par_moffs32) and (paramtype1=memorylocation32)  then
    begin
      if (opcodes[j].paramtype2=par_eax) and (parameter2='EAX') then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          k:=pos('[',parameter1);
          l:=pos(']',parameter1);
          val('$'+copy(parameter1,k+l,l-k-1),v,k);
          if k=0 then
          begin
            //verified, it doesn't have a registerbase in it
            addopcode(bytes,j);
            adddword(bytes,v);
            result:=true;
            exit;
          end;
        end;
      end;
    end;



    if (opcodes[j].paramtype1=par_3) and (paramtype1=value) and (v=3) then
    begin
      //int 3
      addopcode(bytes,j);
      result:=true;
      exit;
    end;

    if (opcodes[j].paramtype1=PAR_AL) and (parameter1='AL') then
    begin
      //AL,

      if (opcodes[j].paramtype2=par_dx) and (parameter2='DX') then
      begin
        //opcode al,dx
        addopcode(bytes,j);
        result:=true;
        exit;
      end;

      if (opcodes[j].paramtype2=par_imm8) and (paramtype2=value) then
      begin
        //AL,imm8
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          if (opcodes[j].opcode1=eo_ib) and (opcodes[j].opcode2=eo_none) then
          begin
            //verified: AL,imm8
            addopcode(bytes,j);
            add(bytes,[byte(v)]);
            result:=true;
            exit;
          end;
        end;

      end;

      if (opcodes[j].paramtype2=par_moffs8) and ((paramtype2=memorylocation8) or (ismemorylocationdefault(parameter2)  ))  then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          k:=pos('[',parameter2);
          l:=pos(']',parameter2);
          val('$'+copy(parameter2,k+l,l-k-1),v,k);
          if k=0 then
          begin
            //verified, it doesn't have a registerbase in it
            addopcode(bytes,j);
            adddword(bytes,v);
            result:=true;
            exit;
          end;


        end;
      end;
    end;

    if (opcodes[j].paramtype1=PAR_AX) and (parameter1='AX') then
    begin
      //AX,
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        //opcode AX
        addopcode(bytes,j);
        result:=true;
        exit;
      end;

      if (opcodes[j].paramtype2=par_dx) and (parameter2='DX') then
      begin
        //opcode ax,dx
        addopcode(bytes,j);
        result:=true;
        exit;
      end;

      //r16
      if (opcodes[j].paramtype2=par_r16) and (paramtype2=register16bit) then
      begin
        //eax,r32
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          //r32,eax
          if opcodes[j].opcode1=eo_prw then
          begin
            //opcode+rd
            addopcode(bytes,j);
            inc(bytes[length(bytes)-1],getreg(parameter2));
            result:=true;
            exit;
          end;
        end;
      end;


      if (opcodes[j].paramtype2=par_imm16) and (paramtype2=value) then
      begin
        //AX,imm16
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          //params confirmed it is a ax,imm16
          if (opcodes[j].opcode1=eo_iw) and (opcodes[j].opcode2=eo_none) then
          begin
            addopcode(bytes,j);
            addword(bytes,word(v));
            result:=true;
            exit;
          end;
        end;
      end;

      if (opcodes[j].paramtype2=par_moffs16) and ((paramtype2=memorylocation16) or (ismemorylocationdefault(parameter2)  ))  then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          k:=pos('[',parameter2);
          l:=pos(']',parameter2);
          val('$'+copy(parameter2,k+l,l-k-1),v,k);
          if k=0 then
          begin
            //verified, it doesn't have a registerbase in it
            addopcode(bytes,j);
            adddword(bytes,v);
            result:=true;
            exit;
          end;
        end;
      end;

    end;

    if (opcodes[j].paramtype1=PAR_EAX) and (parameter1='EAX') then
    begin
      //eAX,
      if (opcodes[j].paramtype2=par_dx) and (parameter2='DX') then
      begin
        //opcode eax,dx
        addopcode(bytes,j);
        result:=true;
        exit;
      end;

      //r32
      if (opcodes[j].paramtype2=par_r32) and (paramtype2=register32bit) then
      begin
        //eax,r32
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          //r32,eax
          if opcodes[j].opcode1=eo_prd then
          begin
            //opcode+rd
            addopcode(bytes,j);
            inc(bytes[length(bytes)-1],getreg(parameter2));
            result:=true;
            exit;
          end;
        end;
      end;

      if (opcodes[j].paramtype2=par_imm8) and (paramtype2=value) then
      begin
        //eax,imm8
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          add(bytes,[v]);
          result:=true;
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_imm32) and (paramtype2=value) then
      begin
        //EAX,imm32
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          if (opcodes[j].opcode1=eo_id) and (opcodes[j].opcode2=eo_none) then
          begin
            addopcode(bytes,j);
            adddword(bytes,v);
            result:=true;
            exit;
          end;
        end;
      end;

      if (opcodes[j].paramtype2=par_moffs32) and ((paramtype2=memorylocation32) or (ismemorylocationdefault(parameter2)  ))  then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          k:=pos('[',parameter2);
          l:=pos(']',parameter2);
          val('$'+copy(parameter2,k+1,l-k-1),v,k);
          if k=0 then
          begin
            //verified, it doesn't have a registerbase in it
            addopcode(bytes,j);
            adddword(bytes,v);
            result:=true;
            exit;
          end;
        end;
      end;

    end;

    if (opcodes[j].paramtype1=par_dx) and (parameter1='DX') then
    begin
      if (opcodes[j].paramtype2=par_al) and (parameter2='AL') then
      begin
        addopcode(bytes,j);
        result:=true;
        exit;
      end;

      if (opcodes[j].paramtype2=par_ax) and (parameter2='AX') then
      begin
        addopcode(bytes,j);
        result:=true;
        exit;
      end;

      if (opcodes[j].paramtype2=par_eax) and (parameter2='EAX') then
      begin
        addopcode(bytes,j);
        result:=true;
        exit;
      end;
    end;

    if (opcodes[j].paramtype1=par_cs) and (parameter1='CS') then
    begin
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        addopcode(bytes,j);
        result:=true;
        exit;
      end;
    end;

    if (opcodes[j].paramtype1=par_ds) and (parameter1='DS') then
    begin
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        addopcode(bytes,j);
        result:=true;
        exit;
      end;
    end;

    if (opcodes[j].paramtype1=par_es) and (parameter1='ES') then
    begin
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        addopcode(bytes,j);
        result:=true;
        exit;
      end;
    end;

    if (opcodes[j].paramtype1=par_SS) and (parameter1='SS') then
    begin
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        addopcode(bytes,j);
        result:=true;
        exit;
      end;
    end;

    if (opcodes[j].paramtype1=par_fs) and (parameter1='FS') then
    begin
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        addopcode(bytes,j);
        result:=true;
        exit;
      end;
    end;

    if (opcodes[j].paramtype1=par_gs) and (parameter1='GS') then
    begin
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        addopcode(bytes,j);
        result:=true;
        exit;
      end;
    end;





    //r8,
    if (opcodes[j].paramtype1=par_r8) and (paramtype1=register8bit) then
    begin
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        //opcode r8
        if opcodes[j].opcode1=eo_prb then
        begin
          //opcode+rd
          addopcode(bytes,j);
          inc(bytes[length(bytes)-1],getreg(parameter1));
          result:=true;
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_imm8) and (paramtype2=value) then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          if opcodes[j].opcode1=eo_prb then
          begin
            addopcode(bytes,j);
            inc(bytes[length(bytes)-1],getreg(parameter1));
            add(bytes,[v]);
            result:=true;
            exit;
          end;
        end;
      end;

      if (opcodes[j].paramtype2=par_rm8) and (isrm8(paramtype2)) then
      begin
        //r8,rm8
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;
      end;
    end;

    if (opcodes[j].paramtype1=par_r16) and (paramtype1=register16bit) then
    begin
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        //opcode r16
        if opcodes[j].opcode1=eo_prw then
        begin
          //opcode+rw
          addopcode(bytes,j);
          inc(bytes[length(bytes)-1],getreg(parameter1));
          result:=true;
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_ax) and (parameter2='AX') then
      begin
        //r16,ax,
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          //r16,ax
          if opcodes[j].opcode1=eo_prw then
          begin
            //opcode+rd
            addopcode(bytes,j);
            inc(bytes[length(bytes)-1],getreg(parameter1));
            result:=true;
            exit;
          end;
        end;
      end;


      if (opcodes[j].paramtype2=par_imm8) and (paramtype2=value) then
      begin
        if (opcodes[j].opcode1=eo_reg) and (opcodes[j].opcode2=eo_ib) then
        begin
          if vtype>8 then
          begin
            //search for r16/imm16
            k:=startoflist;
            while (k<=opcodecount) and (opcodes[k].mnemonic=tokens[mnemonic]) do
            begin
              if (opcodes[k].paramtype1=par_r16) and
                 (opcodes[k].paramtype2=par_imm16) then
              begin
                if (opcodes[k].opcode1=eo_reg) and (opcodes[j].opcode2=eo_ib) then
                begin
                  addopcode(bytes,k);
                  result:=createmodrm(bytes,getreg(parameter1),parameter1);
                  addword(bytes,v);
                  exit;
                end;
              end;
              inc(k);
            end;

          end;


          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          add(bytes,[v]);
        end;
      end;

      if (opcodes[j].paramtype2=par_imm16) and (paramtype2=value) then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          if opcodes[j].opcode1=eo_prw then
          begin
            addopcode(bytes,j);
            inc(bytes[length(bytes)-1],getreg(parameter1));
            addword(bytes,v);
            result:=true;
            exit;
          end;
        end;
      end;


      if (opcodes[j].paramtype2=par_rm16) and (isrm16(paramtype2)) then
      begin
        //r16,r/m16
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;

        if (opcodes[j].paramtype3=par_imm8) and (paramtype3=value) then
        begin
          if (opcodes[j].opcode2=eo_ib) then
          begin
            //r16,r/m16,imm8
            if vtype>8 then
            begin
              //see if there is a //r16,r/m16,imm16
              k:=startoflist;
              while (k<=opcodecount) and (opcodes[k].mnemonic=tokens[mnemonic]) do
              begin
                if (opcodes[k].paramtype1=par_r16) and
                   (opcodes[k].paramtype2=par_rm16) and
                   (opcodes[k].paramtype3=par_imm16) then
                begin
                  addopcode(bytes,k);
                  result:=createmodrm(bytes,getreg(parameter1),parameter2);
                  addword(bytes,v);
                  exit;
                end;
                inc(k);
              end;
            end;

            addopcode(bytes,j);
            result:=createmodrm(bytes,getreg(parameter1),parameter2);
            add(bytes,[v]);
            exit;
          end;
        end;
      end;
    end;

    if (opcodes[j].paramtype1=par_r32) and (paramtype1=register32bit) then
    begin
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        //opcode r32
        if opcodes[j].opcode1=eo_prd then
        begin
          //opcode+rd
          addopcode(bytes,j);
          inc(bytes[length(bytes)-1],getreg(parameter1));
          result:=true;
          exit;
        end
        else
        begin
          //reg0..reg7
          addopcode(bytes,j);
          result:=createmodrm(bytes,eotoreg(opcodes[j].opcode1),parameter1);
          exit;
        end;
      end;




      //eax
      if (opcodes[j].paramtype2=par_eax) and (parameter2='EAX') then
      begin
        //r32,eax,
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          //r32,eax
          if opcodes[j].opcode1=eo_prd then
          begin
            //opcode+rd
            addopcode(bytes,j);
            inc(bytes[length(bytes)-1],getreg(parameter1));
            result:=true;
            exit;
          end;
        end;
      end;


      if (opcodes[j].paramtype2=par_mm) and (paramtype2=registermm) then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;

        if (opcodes[j].paramtype3=par_imm8) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          add(bytes,[v]);
          exit;
        end;

      end;


      if (opcodes[j].paramtype2=par_xmm) and (paramtype2=registerxmm) then
      begin
        //r32,xmm,
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter2),parameter1);
          exit;
        end;

        if (opcodes[j].paramtype3=par_imm8) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter2),parameter1);
          add(bytes,[v]);
          exit;
        end;

      end;

      if (opcodes[j].paramtype2=par_cr) and (paramtype2=registercr) then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter2),parameter1);
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_dr) and (paramtype2=registerdr) then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter2),parameter1);
          exit;
        end;
      end;


      if (opcodes[j].paramtype2=par_xmm_m32) and (isxmm_m32(paramtype2)) then
      begin
        //xmm,xmm/m32
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_xmm_m64) and (isxmm_m64(paramtype2) or ((paramtype2=memorylocation32) and (parameter2[1]='['))) then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_xmm_m128) and (isxmm_m64(paramtype2) or ((paramtype2=memorylocation32) and (parameter2[1]='['))) then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_m32) and (paramtype2=memorylocation32) then
      begin
        //r32,m32,
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          //r32,m32
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_rm8) and (isrm8(paramtype2) or (ismemorylocationdefault(parameter2))) then
      begin
        //r32,rm8
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_rm16) and (isrm16(paramtype2) or (ismemorylocationdefault(parameter2))) then
      begin
        //r32,rm16
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_rm32) and (isrm32(paramtype2)) then
      begin
        //r32,r/m32
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;

        if (opcodes[j].paramtype3=par_imm8) and (paramtype3=value) then
        begin
          if (opcodes[j].opcode2=eo_ib) then
          begin
            if vtype>8 then
            begin
              k:=startoflist;
              while (k<=endoflist) and (opcodes[k].mnemonic=tokens[mnemonic]) do
              begin
                if (opcodes[k].paramtype1=par_r32) and
                   (opcodes[k].paramtype2=par_rm32) and
                   (opcodes[k].paramtype3=par_imm32) then
                begin
                  addopcode(bytes,k);
                  result:=createmodrm(bytes,getreg(parameter1),parameter2);
                  adddword(bytes,v);
                  exit;
                end;
                inc(k);
              end;
            end;


            //r32,r/m32,imm8
            addopcode(bytes,j);
            result:=createmodrm(bytes,getreg(parameter1),parameter2);
            add(bytes,[v]);
            exit;
          end;
        end;

      end;

      if (opcodes[j].paramtype2=par_imm32) and (paramtype2=value) then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          if opcodes[j].opcode1=eo_prd then
          begin
            addopcode(bytes,j);
            inc(bytes[length(bytes)-1],getreg(parameter1));
            adddword(bytes,v);
            result:=true;
            exit;
          end;
        end;
      end;


      if (opcodes[j].paramtype2=par_imm8) and (paramtype2=value) then
      begin
        addopcode(bytes,j);
        createmodrm(bytes,getreg(parameter1),parameter2);
        add(bytes,[byte(v)]);
        result:=true;
        exit;
      end;

    end;


    if (opcodes[j].paramtype1=par_sreg) and (paramtype2=registersreg) then
    begin
      if (opcodes[j].paramtype2=par_rm16) and (isrm16(paramtype2)) then
      begin
        //sreg,rm16
        addopcode(bytes,j);
        result:=createmodrm(bytes,getreg(parameter1),parameter2);
        exit;
      end;
    end;

    if (opcodes[j].paramtype1=par_cr) and (paramtype1=registercr) then
    begin
      if (opcodes[j].paramtype2=par_r32) and (paramtype2=register32bit) then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;
      end;
    end;

    if (opcodes[j].paramtype1=par_dr) and (paramtype1=registerdr) then
    begin
      if (opcodes[j].paramtype2=par_r32) and (paramtype2=register32bit) then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;
      end;
    end;


    //rm8,
    if (opcodes[j].paramtype1=par_rm8) and (isrm8(paramtype1) ) then
    begin
      //r/m8,
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        //opcode r/m8
        addopcode(bytes,j);
        result:=createmodrm(bytes,eotoreg(opcodes[j].opcode1),parameter1);
        exit;
      end;

      if (opcodes[j].paramtype2=par_1) and (paramtype2=value) and (v=1) then
      begin
        addopcode(bytes,j);
        result:=createmodrm(bytes,eotoreg(opcodes[j].opcode1),parameter1);
        exit;
      end;

      if (opcodes[j].paramtype2=par_cl) and (parameter2='CL') then
      begin
        addopcode(bytes,j);
        result:=createmodrm(bytes,eotoreg(opcodes[j].opcode1),parameter1);
        exit;
      end;


      if (opcodes[j].paramtype2=par_imm8) and (paramtype2=value) then
      begin
        //r/m8,imm8,
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          //verified it IS r/m8,imm8
          addopcode(bytes,j);
          createmodrm(bytes,eoToReg(opcodes[j].opcode1),parameter1);
          add(bytes,[byte(v)]);
          result:=true;
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_r8) and (paramtype2=register8bit) then
      begin
        // r/m8,r8
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter2),parameter1);
          exit;
        end;
      end;
    end;


    if (opcodes[j].paramtype1=par_rm16) and (isrm16(paramtype1)) then
    begin
      //r/m16,
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        //opcode r/m16
        addopcode(bytes,j);
        result:=createmodrm(bytes,eotoreg(opcodes[j].opcode1),parameter1);
        exit;
      end;

      if (opcodes[j].paramtype2=par_1) and (paramtype2=value) and (v=1) then
      begin
        addopcode(bytes,j);
        result:=createmodrm(bytes,eotoreg(opcodes[j].opcode1),parameter1);
        exit;
      end;

      if (opcodes[j].paramtype2=par_imm8) and (paramtype2=value) then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          if vtype=16 then
          begin
            //perhaps there is a r/m16,imm16
            k:=startoflist;
            while k<=endoflist do
            begin
              if opcodes[k].mnemonic<>tokens[mnemonic] then break; //nope, so continue with r/m,imm16
              if ((opcodes[k].paramtype1=par_rm16) and (opcodes[k].paramtype2=par_imm16)) and ((opcodes[k].paramtype3=par_noparam) and (parameter3='')) then
              begin
                //yes, there is
                //r/m16,imm16
                addopcode(bytes,k);
                createmodrm(bytes,eoToReg(opcodes[k].opcode1),parameter1);
                add(bytes,[byte(v)]);
                result:=true;
                exit;
              end;
              inc(k);
            end;
          end;
          //nope, so it IS r/m16,8
          addopcode(bytes,j);
          createmodrm(bytes,eoToReg(opcodes[j].opcode1),parameter1);
          addword(bytes,word(v));
          result:=true;
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_imm16) and (paramtype2=value) then
      begin
        //r/m16,imm
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          if vtype=8 then
          begin
            //see if there is a r/m16,imm8 (or if this is the one) (optimisation)
            k:=startoflist;
            while k<=endoflist do
            begin
              if opcodes[k].mnemonic<>tokens[mnemonic] then break; //nope, so continue with r/m,imm16
              if ((opcodes[k].paramtype1=par_rm16) and (opcodes[k].paramtype2=par_imm8)) and ((opcodes[k].paramtype3=par_noparam) and (parameter3='')) then
              begin
                //yes, there is
                //r/m16,imm8
                addopcode(bytes,k);
                createmodrm(bytes,eoToReg(opcodes[k].opcode1),parameter1);
                add(bytes,[byte(v)]);
                result:=true;
                exit;
              end;
              inc(k);
            end;
          end;

          addopcode(bytes,j);
          createmodrm(bytes,eoToReg(opcodes[j].opcode1),parameter1);
          addword(bytes,word(v));
          result:=true;
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_r16) and (paramtype2=register16bit) then
      begin
        //r/m16,r16,

        if (opcodes[j].paramtype3=par_cl) and (parameter3='CL') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter2),parameter1);
          exit;
        end;

        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter2),parameter1);
          exit;
        end;

        if (opcodes[j].paramtype3=par_imm8) and (paramtype3=value) then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter2),parameter1);
          add(bytes,[v]);
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_sreg) and (paramtype2=registersreg) then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          //r/m16,sreg
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter2),parameter1);
          exit;
        end;
      end;
    end;

    if (opcodes[j].paramtype1=par_rm32) and (isrm32(paramtype1)) then
    begin
      //r/m32,
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        //no 2nd parameter so it is 'opcode r/m32'
        addopcode(bytes,j);
        result:=createmodrm(bytes,eoToReg(opcodes[j].opcode1),parameter1);
        exit;
      end;

      if (opcodes[j].paramtype2=par_1) and (paramtype2=value) and (v=1) then
      begin
        addopcode(bytes,j);
        result:=createmodrm(bytes,eotoreg(opcodes[j].opcode1),parameter1);
        exit;
      end;      

      if (opcodes[j].paramtype2=par_imm8) and (paramtype2=value) then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          if vtype=32 then
          begin
            //the user requests a 32-bit value, so see if there is also a r/m32,imm32
            k:=startoflist;
            while k<=endoflist do
            begin
              if opcodes[k].mnemonic<>tokens[mnemonic] then break; //nope, so continue with r/m,imm16
              if ((opcodes[k].paramtype1=par_rm32) and (opcodes[k].paramtype2=par_imm32)) and ((opcodes[k].paramtype3=par_noparam) and (parameter3='')) then
              begin
                //yes, there is
                addopcode(bytes,k);
                createmodrm(bytes,eoToReg(opcodes[k].opcode1),parameter1);
                add(bytes,[byte(v)]);
                result:=true;
                exit;
              end;
              inc(k);
            end;
          end;

          //r/m32,imm8
          addopcode(bytes,j);
          createmodrm(bytes,eoToReg(opcodes[j].opcode1),parameter1);
          add(bytes,[byte(v)]);
          result:=true;
          exit;
        end;
      end;


      if (opcodes[j].paramtype2=par_imm32) and (paramtype2=value) then
      begin
        //r/m32,imm
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          if vtype=8 then
          begin
            //see if there is a r/m32,imm8 (or if this is the one) (optimisation)
            k:=startoflist;
            while k<=endoflist do
            begin
              if opcodes[k].mnemonic<>tokens[mnemonic] then break; //nope, so continue with r/m,imm16
              if ((opcodes[k].paramtype1=par_rm32) and (opcodes[k].paramtype2=par_imm8)) and ((opcodes[k].paramtype3=par_noparam) and (parameter3='')) then
              begin
                //yes, there is
                addopcode(bytes,k);
                createmodrm(bytes,eoToReg(opcodes[k].opcode1),parameter1);
                add(bytes,[byte(v)]);
                result:=true;
                exit;
              end;
              inc(k);
            end;
          end;
          //no there's none
          addopcode(bytes,j);
          createmodrm(bytes,eoToReg(opcodes[j].opcode1),parameter1);
          adddword(bytes,v);
          result:=true;
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_cl) and (parameter2='CL') then
      begin
        addopcode(bytes,j);
        result:=createmodrm(bytes,eotoreg(opcodes[j].opcode1),parameter1);
        exit;
      end;


      if (opcodes[j].paramtype2=par_r32) and (paramtype2=register32bit) then
      begin
        //r/m32,r32
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter2),parameter1);
          exit;
        end;

        if (opcodes[j].paramtype3=par_cl) and (parameter3='CL') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter2),parameter1);
          exit;
        end;

        if (opcodes[j].paramtype3=par_imm8) and (paramtype3=value) then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter2),parameter1);
          add(bytes,[v]);
          exit;
        end;
      end;
    end;

    if (opcodes[j].paramtype1=par_mm) and (paramtype1=registermm) then
    begin
      //mm,xxxxx
      if (opcodes[j].paramtype2=par_mm) and (paramtype2=registermm) then
      begin
        addopcode(bytes,j);
        result:=createmodrm(bytes,getreg(parameter1),parameter2);
        exit;
      end;

      if (opcodes[j].paramtype2=par_xmm) and (paramtype2=registerxmm) then
      begin
        //mm,xmm
        addopcode(bytes,j);
        result:=createmodrm(bytes,getreg(parameter1),parameter2);
        exit;
      end;

      if (opcodes[j].paramtype2=par_r32_m16) and ( (paramtype1=register32bit) or (paramtype2=memorylocation16) or ismemorylocationdefault(parameter2) ) then
      begin
        //mm,r32/m16,
        if (opcodes[j].paramtype3=par_imm8) and (paramtype3=value) then
        begin
          //imm8
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;
      end;



      if (opcodes[j].paramtype2=par_xmm_m32) and (isxmm_m32(paramtype2)) then
      begin
        //mm,xmm/m32
        addopcode(bytes,j);
        result:=createmodrm(bytes,getreg(parameter1),parameter2);
        exit;
      end;

      if (opcodes[j].paramtype2=par_xmm_m64) and (isxmm_m64(paramtype2) or ((paramtype2=memorylocation32) and (parameter2[1]='['))) then
      begin
        //mm,xmm/m64
        addopcode(bytes,j);
        result:=createmodrm(bytes,getreg(parameter1),parameter2);
        exit;
      end;

      if (opcodes[j].paramtype2=par_xmm_m128) and (isxmm_m128(paramtype2) or ((paramtype2=memorylocation32) and (parameter2[1]='['))) then
      begin
        //mm,xmm/m128
        addopcode(bytes,j);
        result:=createmodrm(bytes,getreg(parameter1),parameter2);
        exit;
      end;
    end;

    if (opcodes[j].paramtype1=par_xmm) and (paramtype1=registerxmm) then
    begin
      //XMM,
      if (opcodes[j].paramtype2=par_imm8) and (paramtype2=value) then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,eotoreg(opcodes[j].opcode1),parameter1);
          add(bytes,[v]);
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_mm) and (paramtype2=registermm) then
      begin
        //xmm,xmm
        addopcode(bytes,j);
        result:=createmodrm(bytes,getreg(parameter1),parameter2);
        exit;
      end;


      if (opcodes[j].paramtype2=par_xmm) and (paramtype2=registerxmm) then
      begin
        //xmm,xmm
        addopcode(bytes,j);
        result:=createmodrm(bytes,getreg(parameter1),parameter2);
        exit;
      end;

      if (opcodes[j].paramtype2=par_m64) and ((paramtype2=memorylocation64) or (ismemorylocationdefault(parameter2))) then
      begin
        addopcode(bytes,j);
        result:=createmodrm(bytes,getreg(parameter1),parameter2);
        exit;
      end;

      if (opcodes[j].paramtype2=par_rm32) and (isrm32(paramtype2)) then
      begin
        //xmm,rm32,
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          //xmm,rm32
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_mm_m64) and (ismm_m64(paramtype2) or ((paramtype2=memorylocation32) and (parameter2[1]='['))) then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          //xmm,mm/m64
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_xmm_m32) and isxmm_m32(paramtype2) then
      begin
        //even if the user didn't intend for it to be xmm,m64 it will be, that'll teach the lazy user to forget opperand size
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          //xmm,xmm/m32
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;

        if (opcodes[j].paramtype3=par_imm8) and (paramtype3=value) then
        begin
          addopcode(bytes,j);
          createmodrm(bytes,getreg(parameter1),parameter2);
          add(bytes,[v]);
          result:=true;
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_xmm_m64) and (isxmm_m64(paramtype2) or ((paramtype2=memorylocation32) and (parameter2[1]='[')))  then
      begin
        //even if the user didn't intend for it to be xmm,m64 it will be, that'll teach the lazy user to forget opperand size
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          //xmm,xmm/m64
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;

        if (opcodes[j].paramtype3=par_imm8) and (paramtype3=value) then
        begin
          addopcode(bytes,j);
          createmodrm(bytes,getreg(parameter1),parameter2);
          add(bytes,[v]);
          result:=true;
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_xmm_m128) and (isxmm_m128(paramtype2) or ((paramtype2=memorylocation32) and (parameter2[1]='[')))  then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          //xmm,xmm/m128
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter1),parameter2);
          exit;
        end;

        if (opcodes[j].paramtype3=par_imm8) and (paramtype3=value) then
        begin
          addopcode(bytes,j);
          createmodrm(bytes,getreg(parameter1),parameter2);
          add(bytes,[v]);
          result:=true;
          exit;
        end;
      end;

    end;

    //m8
    if (opcodes[j].paramtype1=par_m8) and ((paramtype1=memorylocation8) or ismemorylocationdefault(parameter1)) then
    begin
      //m8,xxx
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        //m8
        //                                 //check if it is especially designed to be 32 bit, or if it is a default anser
        //verified, it is a 8 bit location, and if it was detected as 8 it was due to defaulting to 32
        addopcode(bytes,j);
        result:=createmodrm(bytes,eotoreg(opcodes[j].opcode1),parameter1);
        exit;
      end;
    end;

    //m16
    if (opcodes[j].paramtype1=par_m16) and (paramtype1=memorylocation16) then //no check if it is 16 if it is a [xxx], default is 32
    begin
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        //opcode+rd
        addopcode(bytes,j);
        result:=createmodrm(bytes,eotoreg(opcodes[j].opcode1),parameter1);
        exit;
      end;
    end;

    //m32
    if (opcodes[j].paramtype1=par_m32) and (paramtype1=memorylocation32) then //no check if it is 16 if it is a [xxx], default is 32
    begin
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        addopcode(bytes,j);
        result:=createmodrm(bytes,eotoreg(opcodes[j].opcode1),parameter1);
        exit;
      end;

      if (opcodes[j].paramtype2=par_r32) then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,eotoreg(opcodes[j].opcode1),parameter1);
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_xmm) and ((paramtype2=registerxmm) or ismemorylocationdefault(parameter2)  ) then
      begin
        if (opcodes[j].paramtype3=par_noparam) or (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter2),parameter1);
          exit;
        end;
      end;      
    end;


    if (opcodes[j].paramtype1=par_m64) and ((paramtype1=memorylocation64) or (paramtype1=memorylocation32)  ) then
    begin
      //m64,
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        //m64
        //                                   //check if it is especially designed to be 32 bit, or if it is a default answer (so CAN be 64)
        if (paramtype1=memorylocation64) or (parameter1[1]='[') then
        begin
          //verified, it is a 64 bit location, and if it was detected as 32 it was due to defaulting to 32
          addopcode(bytes,j);
          result:=createmodrm(bytes,eotoreg(opcodes[j].opcode1),parameter1);
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_xmm) and ((paramtype2=registerxmm) or ismemorylocationdefault(parameter2)  ) then
      begin
        if (opcodes[j].paramtype3=par_noparam) or (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter2),parameter1);
          exit;
        end;
      end;

      if (opcodes[j].paramtype2=par_xmm) and ((paramtype2=registerxmm) or ismemorylocationdefault(parameter2)  ) then
      begin
        if (opcodes[j].paramtype3=par_noparam) or (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter2),parameter1);
          exit;
        end;
      end;

    end;

    if (opcodes[j].paramtype1=par_m80) and ((paramtype1=memorylocation80) or ((paramtype1=memorylocation32) and (parameter1[1]='[')))  then
    begin
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        addopcode(bytes,j);
        result:=createmodrm(bytes,eotoreg(opcodes[j].opcode1),parameter1);
        exit;
      end;
    end;

    if (opcodes[j].paramtype1=par_m128) and ((paramtype1=memorylocation128) or (ismemorylocationdefault(parameter1))) then
    begin
      if (opcodes[j].paramtype2=par_xmm) and (paramtype2=registerxmm) then
      begin
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          addopcode(bytes,j);
          result:=createmodrm(bytes,getreg(parameter2),parameter1);
          exit;
        end;
      end;
    end;

    if (opcodes[j].paramtype1=par_rel8) and (paramtype1=value) then
    begin
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        //rel8
        if parameter1[1] in ['-','+'] then
        begin
          if vtype>8 then
          begin
            //see if there is a 32 bit equivalent opcode (notice I dont do rel 16 because that'll completly screw up eip)
            k:=startoflist;
            while (k<=opcodecount) and (opcodes[k].mnemonic=tokens[mnemonic]) do
            begin
              if (opcodes[k].paramtype1=par_rel32) and (opcodes[k].paramtype2=par_noparam) then
              begin
                //yes, there is a 32 bit version
                addopcode(bytes,k);
                adddword(bytes,v);
                result:=true;
                exit;
              end;
              inc(k);
            end;

          end;


          addopcode(bytes,j);
          add(bytes,[v]);
          result:=true;
          exit;
        end
        else
        begin
          //user typed in a direct address
          if valueTotype(v-address-(opcodes[j].bytes+1))>8 then
          begin
            //the user tried to find a relative address out of it's reach
            //see if there is a 32 bit version of the opcode
            k:=startoflist;
            while (k<=opcodecount) and (opcodes[k].mnemonic=tokens[mnemonic]) do
            begin
              if (opcodes[k].paramtype1=par_rel32) and (opcodes[k].paramtype2=par_noparam) then
              begin
                //yes, there is a 32 bit version
                addopcode(bytes,k);
                adddword(bytes,v-address-(opcodes[k].bytes+4));
                result:=true;
                exit;
              end;
              inc(k);
            end;
          end
          else
          begin
            addopcode(bytes,j);

            add(bytes,[v-address-(opcodes[j].bytes+1)]);
            result:=true;
            exit;
          end;
        end;

      end;
    end;

    if (opcodes[j].paramtype1=par_rel32) and (paramtype1=value) then
    begin
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        if parameter1[1] in ['-','+'] then
        begin
          //opcode rel32
          addopcode(bytes,j);
          adddword(bytes,v);
          result:=true;
          exit;
        end else
        begin
          //user typed in a direct address
          addopcode(bytes,j);

          adddword(bytes,v-address-(opcodes[j].bytes+4));
          result:=true;
          exit;
        end;
      end;
    end;


    if (opcodes[j].paramtype1=par_st0) and ((parameter1='ST(0)') or (parameter1='ST')) then
    begin
      //st(0),
      if (opcodes[j].paramtype2=par_st) and (paramtype2=registerst) then
      begin
        //st(0),st(x),
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          if (opcodes[j].opcode1=eo_pi) then
          begin
            //opcode+i
            addopcode(bytes,j);
            inc(bytes[length(bytes)-1],getreg(parameter2));
            result:=true;
            exit;
          end;

        end;
      end;
    end;

    if (opcodes[j].paramtype1=par_st) and (paramtype1=registerst) then
    begin
      //st(0),
      if (opcodes[j].paramtype2=par_noparam) and (parameter2='') then
      begin
        addopcode(bytes,j);
        inc(bytes[length(bytes)-1],getreg(parameter1));
        result:=true;
        exit;
      end;

      if (opcodes[j].paramtype2=par_st0) and ((parameter2='ST(0)') or (parameter2='ST')) then
      begin
        //st(0),st(x),
        if (opcodes[j].paramtype3=par_noparam) and (parameter3='') then
        begin
          if (opcodes[j].opcode1=eo_pi) then
          begin
            //opcode+i
            addopcode(bytes,j);
            inc(bytes[length(bytes)-1],getreg(parameter1));
            result:=true;
            exit;
          end;

        end;
      end;
    end;





    inc(j);
  end;

end;


var i,j,k,l,m: integer;
    lastentry: integer=1;
    lastentry2: integer=1;
    lastindex: PIndexArray;

initialization
//setup the index for the assembler
  for i:=0 to 25 do
  begin
    assemblerindex[i].startentry:=-1;
    assemblerindex[i].NextEntry:=-1;
    assemblerindex[i].SubIndex:=nil;
    for j:=lastentry to opcodecount do
    begin

      if ord(opcodes[j].mnemonic[1])=(ord('A')+i) then
      begin
        //found the first entry with this as first character
        if lastindex<>nil then
          lastindex[0].nextentry:=j;

        lastindex:=@assemblerindex[i];
        assemblerindex[i].startentry:=j;
        assemblerindex[i].SubIndex:=nil; //default initialization
        lastentry:=j;
        break;
      end;

      if ord(opcodes[j].mnemonic[1])>(ord('A')+i) then
        break; //passed it

    end;

  end;

  if assemblerindex[25].startentry<>-1 then
    assemblerindex[25].NextEntry:=opcodecount;

  //fill in the subindexes
  for i:=0 to 25 do
  begin
    if assemblerindex[i].startentry<>-1 then
    begin
      //initialize subindex
      getmem(assemblerindex[i].SubIndex,26*sizeof(tindex));
      for j:=0 to 25 do
      begin
        assemblerindex[i].SubIndex[j].startentry:=-1;
        assemblerindex[i].SubIndex[j].NextEntry:=-1;
        assemblerindex[i].SubIndex[j].SubIndex:=nil;
      end;

      lastindex:=nil;
      if (assemblerindex[i].NextEntry=-1) then //last one in the list didn't get a assignment
        assemblerindex[i].NextEntry:=opcodecount+1;

      for j:=0 to 25 do
      begin
        for k:=assemblerindex[i].startentry to assemblerindex[i].NextEntry-1 do
        begin
          if ord(opcodes[k].mnemonic[2])=(ord('A')+j) then
          begin
            if lastindex<>nil then
              lastindex[0].nextentry:=k;

            lastindex:=@assemblerindex[i].SubIndex[j];
            assemblerindex[i].SubIndex[j].startentry:=k;
            break;
          end;
        end;
      end;

    end;
  end;


end.
