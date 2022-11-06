//copyright Cheat Engine 2022. All rights reserved
unit DisAssemblerARM32;

{$mode objfpc}{$H+}
{$WARN 3177 off : Some fields coming after "$1" were not initialized}
interface

//last update at :a7

uses
  Classes, SysUtils, LastDisassembleData;



type
  TInstructionGroupPointerType=(igpGroup, igpInstructions);
  TInstructionGroup=record
    mask: DWORD;
    value: DWORD;
    list: pointer;
    listType: TInstructionGroupPointerType;
    name: string;
  end;
  TInstructionGroupArray=array of TInstructionGroup;
  PInstructionGroupArray=^TInstructionGroupArray;


  TIndexedParameter=(ind_no, ind_index, ind_stop, ind_stopexp, ind_single, ind_singleexp);
  TArm32ParameterType=(pt_rreg,
                       pt_rreg_ex, //register with a !
                       pt_coproc, //p0,p1, p2, ...
                       pt_crreg,
                       pt_signedrreg, //can come with a + or - in front
                       pt_shift5,
                       pt_shifttype, //extra is offset of register (usually offset+3)
                       pt_const,
                       pt_neglabel, //(const)label that points backwards from PC
                       pt_poslabel,
                       pt_label, //same as neg/pos but extra field has the signed bit
                       pt_signed_label,  //maxval determines the msb, and extra the amount of bits to shift (2)
                       pt_signed_imm8_shl2, //imm8 with a U bit in extra
                       pt_signed_imm12, //imm12 with a U bit in extra.  (if U=0 then nagative, else positive)

                       pt_imm16_4_12, //imm16 value split up in 2 blocks.  block 1 is 4 bits, block2=12 bits
                       pt_imm16_12_4,
                       pt_imm,  //normal imm value, no split etc...     (bitsize is determined by maxval)
                       pt_imm8LH, //imm8 value split up in 2 blocks of 4 bits. (offset is encoded using bytes. byte 0 is l offset, byte 1 is h offset)  Extra is the "add" bit
                       pt_imm8LH_label, //same as imm8 but shows the destinationaddress based on the pc reg
                       pt_ap_reg,
                       pt_cpsr_reg,
                       pt_spsr_reg,
                       pt_apsr, //just the text apsr
                       pt_spsr, //spsr ^



                       pt_bankedreg,
                       pt_bankedreg_spsr,
                       pt_rotatex8,
                       pt_reglist16, //bitlist for registers. extra contaisn the minimum amount of bits that must be set to be allowed for encoding
                       pt_reglist16_x, //same as pt_reglist16 but has a ^ at the end
                       pt_reglist_reg //bitlist with just 1 register encoded in a register
                       );



  TAParameters=record
    ptype: TArm32ParameterType;
    offset: dword; //binary position  (in case of imm2/pt_reglist_*: offset is a 32-bit bitmask and assumed concatenated from left to right)
    maxval: dword;
    extra:  qword; //extra data for paramtypes
    optional: boolean;
    defvalue: integer; //in case of optional
    index: TIndexedParameter;
  end;

  TAParametersList=array of TAParameters;

  TInstructionUse=(iuBoth=0, iuAssembler=1, iuDisassembler=2);

  TOpcodeAdditions=(opa_s20, //opcode followed by S if bit20 is 1  (ANDSEQ)
                    opa_cond //opcode followed by conditional (EQ, NZ, etc...)


                    );

  POpcodeArray=^topcodearray;
  TOpcode=record
    mnemonic: string;
    additions: set of TOpcodeAdditions;
    params: TAParametersList;
    mask: DWORD;
    value:DWORD;
    use: TInstructionUse;
    alt: popcodearray;
  end;
  POpcode=^TOpcode;
  TOpcodeArray=array of TOpcode;

  EInvalidInstruction=class(Exception);

  TArm32ParameterTypes=set of TArm32ParameterType;

  TArm32Instructionset=object
  private
    address: dword;
    opcode: uint32;

    procedure InitARM32Support;

    function GetIMM2Value(mask: dword): dword;
    function GetIMM2_8Value(mask: dword): qword;
    procedure SetIMM2Value(mask: dword; v: dword);

    function ParseParametersForDisassembler(plist: TAParametersList): boolean;
    function ScanOpcodeList(const list: topcodearray): boolean;
    function ScanGroupList(const list: TInstructionGroupArray): boolean;
    //assembler
    function ParseParameterForAssembler(param:TAParameters; paramstr: string): boolean;
    function GuessTypes(param: string): TArm32ParameterTypes;
  public
    LastDisassembleData: TLastDisassembleData;
    function disassemble(var DisassembleAddress: ptruint{$ifdef armdev}; _opcode: dword{$endif}): string;
    function assemble(_address: ptruint; instruction: string): DWORD;
  end;

  {$ifdef armdev}
  procedure GetArmInstructionsAssemblerListDebug(r: tstrings);
  function test(d: dword):string;
  {$endif}




implementation

{$ifndef armdev}
uses math, NewKernelHandler,ProcessHandlerUnit,StringHashList;
{$else}
uses StringHashList, math, windows, Rtti, RttiUtils, TypInfo;
{$endif}

const
  ArmRegisters : array [0..15] of string=('R0','R1','R2','R3','R4','R5','R6','R7','R8','R9','R10','FP','IP','SP','LR','PC');
  ArmConditions: array [0..15] of string=('EQ','NE','CS', 'CC', 'MI', 'PL', 'VS', 'VC', 'HI', 'LS', 'GE', 'LT', 'GT', 'LE', '','NV');



//  ArmInstructionsBase: array [0..6] of  ...
//    0:ArmInstructionsUnused [0..0]
//    1:ArmInstructionsSystem: ...
//    2:ArmInstructionsDataProcessing: .8..

//example:
//191c: 4a c9 46 f9   ldr     x10, [x10, #3472]
//f946c94a=11111001010001101100100101001010  = Load/store register (unsigned immediate)   , unsigned offset variant
//LDR <Xt>, [<Xn|SP>{, #<pimm>}]

//


  ArmInstructionsDataProcessingImmediate: array of TOpcode=(
    (mnemonic:'ADR';  additions:[opa_cond];          params:((ptype:pt_rreg; offset:12),                           (ptype:pt_neglabel; offset:0; maxval: $fff; extra:0; optional:true)); mask: %00001111111111110000000000000000; value: %00000010010011110000000000000000),
    (mnemonic:'ADR';  additions:[opa_cond];          params:((ptype:pt_rreg; offset:12),                           (ptype:pt_poslabel; offset:0; maxval: $fff; extra:0; optional:true)); mask: %00001111111111110000000000000000; value: %00000010100011110000000000000000),

    (mnemonic:'AND';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_const; offset:0; maxval: 4095; extra:0; optional:true));    mask: %00001111111000000000000000000000; value: %00000010000000000000000000000000),
    (mnemonic:'EOR';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_const; offset:0; maxval: 4095; extra:0; optional:true));    mask: %00001111111000000000000000000000; value: %00000010001000000000000000000000),
    (mnemonic:'SUB';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_const; offset:0; maxval: 4095; extra:0; optional:true));    mask: %00001111111000000000000000000000; value: %00000010010000000000000000000000),
    (mnemonic:'RSB';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_const; offset:0; maxval: 4095; extra:0; optional:true));    mask: %00001111111000000000000000000000; value: %00000010011000000000000000000000),

    (mnemonic:'ADD';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_const; offset:0; maxval: 4095; extra:0; optional:true));    mask: %00001111111000000000000000000000; value: %00000010100000000000000000000000),
    (mnemonic:'ADC';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_const; offset:0; maxval: 4095; extra:0; optional:true));    mask: %00001111111000000000000000000000; value: %00000010101000000000000000000000),
    (mnemonic:'SBC';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_const; offset:0; maxval: 4095; extra:0; optional:true));    mask: %00001111111000000000000000000000; value: %00000010110000000000000000000000),
    (mnemonic:'RSC';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_const; offset:0; maxval: 4095; extra:0; optional:true));    mask: %00001111111000000000000000000000; value: %00000010111000000000000000000000),

    (mnemonic:'TST';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_const; offset:0; maxval: 4095; extra:0; optional:true));                                        mask: %00001111111100001111000000000000; value: %00000011000100000000000000000000),
    (mnemonic:'TEQ';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_const; offset:0; maxval: 4095; extra:0; optional:true));                                        mask: %00001111111100001111000000000000; value: %00000011001100000000000000000000),
    (mnemonic:'CMP';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_const; offset:0; maxval: 4095; extra:0; optional:true));                                        mask: %00001111111100001111000000000000; value: %00000011010100000000000000000000),
    (mnemonic:'CMN';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_const; offset:0; maxval: 4095; extra:0; optional:true));                                        mask: %00001111111100001111000000000000; value: %00000011011100000000000000000000),

    (mnemonic:'ORR';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_const; offset:0; maxval: 4095; extra:0; optional:true));    mask: %00001111111000000000000000000000; value: %00000011100000000000000000000000),
    (mnemonic:'MOV';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_const; offset:0; maxval: 4095; extra:0; optional:true));                               mask: %00001111111011110000000000000000; value: %00000011101000000000000000000000),
    (mnemonic:'BIC';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_const; offset:0; maxval: 4095; extra:0; optional:true));    mask: %00001111111000000000000000000000; value: %00000011110000000000000000000000),
    (mnemonic:'MVN';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_const; offset:0; maxval: 4095; extra:0; optional:true));                               mask: %00001111111011110000000000000000; value: %00000011111000000000000000000000)

  );

  ArmInstructionsDataProcessingRegister: array of TOpcode=(
    (mnemonic:'AND';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111000000000000000010000; value: %00000000000000000000000000000000),
    (mnemonic:'EOR';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111000000000000000010000; value: %00000000001000000000000000000000),
    (mnemonic:'SUB';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111000000000000000010000; value: %00000000010000000000000000000000),
    (mnemonic:'RSB';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111000000000000000010000; value: %00000000011000000000000000000000),
    (mnemonic:'ADD';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111000000000000000010000; value: %00000000100000000000000000000000),
    (mnemonic:'ADC';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111000000000000000010000; value: %00000000101000000000000000000000),
    (mnemonic:'SBC';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111000000000000000010000; value: %00000000110000000000000000000000),
    (mnemonic:'RSC';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111000000000000000010000; value: %00000000111000000000000000000000),

    (mnemonic:'TST';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111100001111000000010000; value: %00000001000100000000000000000000),
    (mnemonic:'TEQ';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111100001111000000010000; value: %00000001001100000000000000000000),
    (mnemonic:'CMP';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111100001111000000010000; value: %00000001010100000000000000000000),
    (mnemonic:'CMN';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111100001111000000010000; value: %00000001011100000000000000000000),

    (mnemonic:'ORR';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111000000000000000010000; value: %00000001100000000000000000000000),

    (mnemonic:'MOV';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0));                                                                 mask: %00001111111011110000111111110000; value: %00000001101000000000000000000000),
    (mnemonic:'LSL';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111011110000000001110000; value: %00000001101000000000000000000000),
    (mnemonic:'LSR';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111011110000000001110000; value: %00000001101000000000000000100000),
    (mnemonic:'ASR';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111011110000000001110000; value: %00000001101000000000000001000000),
    (mnemonic:'RRX';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0));                                                                 mask: %00001111111011110000111111110000; value: %00000001101000000000000001100000),
    (mnemonic:'ROR';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111011110000000001110000; value: %00000001101000000000000001100000),

    (mnemonic:'BIC';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true)); mask: %00001111111000000000000000010000; value: %00000001110000000000000000000000),
    (mnemonic:'MVN';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true));                            mask: %00001111111011110000000000010000; value: %00000001111000000000000000000000)
  );



  ArmInstructionsSaturatingAddonAndSubtraction: array of TOpcode=(
    (mnemonic:'QADD';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:16)); mask: %00001111111100000000111111110000; value: %00000001000000000000000001010000),
    (mnemonic:'QSUB';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:16)); mask: %00001111111100000000111111110000; value: %00000001001000000000000001010000),
    (mnemonic:'QDADD'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:16)); mask: %00001111111100000000111111110000; value: %00000001010000000000000001010000),
    (mnemonic:'QDSUB'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:16)); mask: %00001111111100000000111111110000; value: %00000001011000000000000001010000)
  );

  ArmInstructionsMiscellaneousInstuctions: array of TOpcode=(
    (mnemonic:'MRS';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_bankedreg; offset:16; maxval:%11111; extra:8));      mask: %00001111111100000000111011111111; value: %00000001000000000000001000000000),
    (mnemonic:'MRS';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_bankedreg_spsr; offset:16; maxval:%11111; extra:8)); mask: %00001111111100000000111011111111; value: %00000001010000000000001000000000),

    (mnemonic:'MSR';  additions:[opa_cond];  params:((ptype:pt_bankedreg; offset:16; maxval:%11111; extra:8),(ptype:pt_rreg; offset:0; maxval:%11111; extra:8));      mask: %00001111111100001111111011110000; value: %00000001001000001111001000000000),
    (mnemonic:'MSR';  additions:[opa_cond];  params:((ptype:pt_bankedreg_spsr; offset:16; maxval:%11111; extra:8),(ptype:pt_rreg; offset:0; maxval:%11111; extra:8)); mask: %00001111111100001111111011110000; value: %00000001011000001111001000000000),

    (mnemonic:'MRS';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_apsr; offset:16; maxval:%11111; extra:8));      mask: %00001111111111110000111111111111; value: %00000001000011110000000000000000),
    (mnemonic:'MRS';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_spsr; offset:16; maxval:%11111; extra:8));      mask: %00001111111111110000111111111111; value: %00000001010011110000000000000000),

    (mnemonic:'MSR';  additions:[opa_cond];  params:((ptype:pt_ap_reg; offset:18; maxval:3), (ptype:pt_rreg; offset:0));    mask: %00001111111100111111111111110000; value: %00000001001000001111000000000000),
    (mnemonic:'MSR';  additions:[opa_cond];  params:((ptype:pt_cpsr_reg; offset:16; maxval:15), (ptype:pt_rreg; offset:0)); mask: %00001111111100001111111111110000; value: %00000001001000001111000000000000),
    (mnemonic:'MSR';  additions:[opa_cond];  params:((ptype:pt_spsr_reg; offset:16; maxval:15), (ptype:pt_rreg; offset:0)); mask: %00001111111100001111111111110000; value: %00000001011000001111000000000000),

    (mnemonic:'BX';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:0));                                             mask: %00001111111111111111111111110000; value: %00000001001011111111111100010000),
    (mnemonic:'CLZ'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12), (ptype:pt_rreg; offset:0));                 mask: %00001111111111110000111111110000; value: %00000001011011110000111100010000),
    (mnemonic:'BXJ'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:0));                                             mask: %00001111111111111111111111110000; value: %00000001001011111111111100100000),
    (mnemonic:'BLX'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:0));                                             mask: %00001111111111111111111111110000; value: %00000001001011111111111100110000),
    (mnemonic:'ERET';additions:[opa_cond];  params:();                                                                      mask: %00001111111111111111111111111111; value: %00000001011000000000000001101110),
    (mnemonic:'BKPT';additions:[opa_cond];  params:((ptype:pt_imm16_4_12; offset:0; maxval:65535; extra:8));                mask: %00001111111100000000000011110000; value: %00000001001000000000000001110000),
    (mnemonic:'HVC'; additions:[opa_cond];  params:((ptype:pt_imm16_4_12; offset:0; maxval:65535; extra:8));                mask: %00001111111100000000000011110000; value: %00000001010000000000000001110000),
    (mnemonic:'SMC'; additions:[opa_cond];  params:((ptype:pt_imm; offset:0; maxval:15));                                  mask: %00001111111111111111111111110000; value: %00000001011000000000000001110000)
  );


  ArmInstructionsHalfWordMultiplyAndMultiplyAccumulate: array of TOpcode=(
    (mnemonic:'SMLABB';additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8),(ptype:pt_rreg; offset:12)); mask: %00001111111100000000000011110000; value: %00000001000000000000000010000000),
    (mnemonic:'SMLATB';additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8),(ptype:pt_rreg; offset:12)); mask: %00001111111100000000000011110000; value: %00000001000000000000000010100000),
    (mnemonic:'SMLABT';additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8),(ptype:pt_rreg; offset:12)); mask: %00001111111100000000000011110000; value: %00000001000000000000000011000000),
    (mnemonic:'SMLATT';additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8),(ptype:pt_rreg; offset:12)); mask: %00001111111100000000000011110000; value: %00000001000000000000000011100000),

    (mnemonic:'SMLAWB';additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8),(ptype:pt_rreg; offset:12)); mask: %00001111111100000000000011110000; value: %00000001001000000000000010000000),
    (mnemonic:'SMLAWT';additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8),(ptype:pt_rreg; offset:12)); mask: %00001111111100000000000011110000; value: %00000001001000000000000011000000),

    (mnemonic:'SMULWB';additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                            mask: %00001111111100001111000011110000; value: %00000001001000000000000010100000),
    (mnemonic:'SMULWT';additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                            mask: %00001111111100001111000011110000; value: %00000001001000000000000011100000),

    (mnemonic:'SMLALBB';additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8)); mask: %00001111111100000000000011110000; value: %00000001010000000000000010000000),
    (mnemonic:'SMLALTB';additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8)); mask: %00001111111100000000000011110000; value: %00000001010000000000000010100000),
    (mnemonic:'SMLALBT';additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8)); mask: %00001111111100000000000011110000; value: %00000001010000000000000011000000),
    (mnemonic:'SMLALTT';additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8)); mask: %00001111111100000000000011110000; value: %00000001010000000000000011100000),


    (mnemonic:'SMULBB';additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                            mask: %00001111111100001111000011110000; value: %00000001011000000000000010000000),
    (mnemonic:'SMULTB';additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                            mask: %00001111111100001111000011110000; value: %00000001011000000000000010100000),
    (mnemonic:'SMULBT';additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                            mask: %00001111111100001111000011110000; value: %00000001011000000000000011000000),
    (mnemonic:'SMULTT';additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                            mask: %00001111111100001111000011110000; value: %00000001011000000000000011100000)
  );

  ArmInstructionsMultiplyAndMultiplyAccumulate: array of TOpcode=(
    (mnemonic:'MUL';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                            mask: %00001111111000001111000011110000; value: %00000000000000000000000010010000),
    (mnemonic:'MLA';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8),(ptype:pt_rreg; offset:12)); mask: %00001111111000000000000011110000; value: %00000000001000000000000010010000),
    (mnemonic:'UMAAL';additions:[opa_cond];          params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8)); mask: %00001111111100000000000011110000; value: %00000000010000000000000010010000),
    (mnemonic:'MLS';  additions:[opa_cond];          params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8),(ptype:pt_rreg; offset:12)); mask: %00001111111100000000000011110000; value: %00000000011000000000000010010000),
    (mnemonic:'UMULL';additions:[opa_s20,opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8)); mask: %00001111111000000000000011110000; value: %00000000100000000000000010010000),
    (mnemonic:'UMLAL';additions:[opa_s20,opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8)); mask: %00001111111000000000000011110000; value: %00000000101000000000000010010000),
    (mnemonic:'SMULL';additions:[opa_s20,opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8)); mask: %00001111111000000000000011110000; value: %00000000110000000000000010010000),
    (mnemonic:'SMLAL';additions:[opa_s20,opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8)); mask: %00001111111000000000000011110000; value: %00000000111000000000000010010000)
  );

  ArmInstructionsSynchronizationPrimitives: array of TOpcode=(
    (mnemonic:'SWP';   additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:16; maxval:15; extra:0; optional:false; defvalue:0; index: ind_single)); mask: %00001111111100000000111111110000; value: %00000001000000000000000010010000),
    (mnemonic:'SWPB';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:16; maxval:15; extra:0; optional:false; defvalue:0; index: ind_single)); mask: %00001111111100000000111111110000; value: %00000001010000000000000010010000),
    (mnemonic:'STREX'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:16; maxval:15; extra:0; optional:false; defvalue:0; index: ind_single)); mask: %00001111111100000000111111110000; value: %00000001100000000000111110010000),
    (mnemonic:'LDREX'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16; maxval:15; extra:0; optional:false; defvalue:0; index: ind_single));                           mask: %00001111111100000000111111111111; value: %00000001100100000000111110011111),

    (mnemonic:'STREXD';additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:16; maxval:15; extra:0; optional:false; defvalue:0; index: ind_single));  mask: %00001111111100000000111111110000; value: %00000001101000000000111110010000),
    (mnemonic:'LDREXD';additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:16; maxval:15; extra:0; optional:false; defvalue:0; index: ind_single)); mask: %00001111111100000000111111111111; value: %00000001101100000000111110011111),

    (mnemonic:'STREXB';additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:16; maxval:15; extra:0; optional:false; defvalue:0; index: ind_single)); mask: %00001111111100000000111111110000; value: %00000001110000000000111110010000),
    (mnemonic:'LDREXB';additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16; maxval:15; extra:0; optional:false; defvalue:0; index: ind_single));                           mask: %00001111111100000000111111111111; value: %00000001110100000000111110011111),

    (mnemonic:'STREXH';additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:16; maxval:15; extra:0; optional:false; defvalue:0; index: ind_single)); mask: %00001111111100000000111111110000; value: %00000001111000000000111110010000),
    (mnemonic:'LDREXH';additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16; maxval:15; extra:0; optional:false; defvalue:0; index: ind_single));                           mask: %00001111111100000000111111111111; value: %00000001111100000000111110011111)

  );

  ArmInstructionsExtraLoadStoreInstructions: array of TOpcode=(
    (mnemonic:'STRH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23));                                                mask: %00001111011100000000111111110000; value: %00000000000000000000000010110000), //post-index (index=false, wback=true (p=0))
    (mnemonic:'STRH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23; optional:false; defvalue:0; index: ind_stop));    mask: %00001111011100000000111111110000; value: %00000001000000000000000010110000), //offset (index=true, wback=false)
    (mnemonic:'STRH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23; optional:false; defvalue:0; index: ind_stopexp)); mask: %00001111011100000000111111110000; value: %00000001001000000000000010110000), //pre-index (index=true, wback=true (w=1))

    (mnemonic:'LDRH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23));                                                mask: %00001111011100000000111111110000; value: %00000000000100000000000010110000),
    (mnemonic:'LDRH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23; optional:false; defvalue:0; index: ind_stop));    mask: %00001111011100000000111111110000; value: %00000001000100000000000010110000),
    (mnemonic:'LDRH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23; optional:false; defvalue:0; index: ind_stopexp)); mask: %00001111011100000000111111110000; value: %00000001001100000000000010110000),


    (mnemonic:'STRH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0));                          mask: %00001111011100000000000011110000; value: %00000000010000000000000010110000),
    (mnemonic:'STRH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0; index: ind_stop));          mask: %00001111011100000000000011110000; value: %00000001010000000000000010110000),
    (mnemonic:'STRH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0; index: ind_stopexp));       mask: %00001111011100000000000011110000; value: %00000001011000000000000010110000),

    (mnemonic:'LDRH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15),(ptype:pt_imm8LH_label; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0));                                                                            mask: %00001111011111110000000011110000; value: %00000000010111110000000010110000),


    (mnemonic:'LDRH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0));                          mask: %00001111011100000000000011110000; value: %00000000010100000000000010110000),
    (mnemonic:'LDRH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0; index: ind_stop));          mask: %00001111011100000000000011110000; value: %00000001010100000000000010110000),
    (mnemonic:'LDRH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0; index: ind_stopexp));       mask: %00001111011100000000000011110000; value: %00000001011100000000000010110000),

    (mnemonic:'LDRD';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23));                                                mask: %00001111011100000000111111110000; value: %00000000000000000000000011010000),
    (mnemonic:'LDRD';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23; optional:false; defvalue:0; index: ind_stop));    mask: %00001111011100000000111111110000; value: %00000001000000000000000011010000),
    (mnemonic:'LDRD';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23; optional:false; defvalue:0; index: ind_stopexp)); mask: %00001111011100000000111111110000; value: %00000001001000000000000011010000),

    (mnemonic:'LDRSB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23));                                                mask: %00001111011100000000000011110000; value: %00000000000100000000000011010000),
    (mnemonic:'LDRSB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23; optional:false; defvalue:0; index: ind_stop));    mask: %00001111011100000000000011110000; value: %00000001000100000000000011010000),
    (mnemonic:'LDRSB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23; optional:false; defvalue:0; index: ind_stopexp)); mask: %00001111011100000000000011110000; value: %00000001001100000000000011010000),

    (mnemonic:'LDRD';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_imm8LH_label; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0));                    mask: %00001111011111110000000011110000; value: %00000001010011110000000011010000),

    (mnemonic:'LDRD';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0));                    mask: %00001111011100000000111111110000; value: %00000000010000000000000011010000),
    (mnemonic:'LDRD';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0; index: ind_stop));    mask: %00001111011100000000111111110000; value: %00000001010000000000000011010000),
    (mnemonic:'LDRD';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0; index: ind_stopexp)); mask: %00001111011100000000111111110000; value: %00000001011000000000000011010000),



    (mnemonic:'LDRSB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm8LH_label; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0));              mask: %00001111011111110000000011110000; value: %00000000010111110000000011010000),

    (mnemonic:'LDRSB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0));                    mask: %00001111011100000000000011110000; value: %00000000010100000000000011010000),
    (mnemonic:'LDRSB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0; index: ind_stop));    mask: %00001111011100000000000011110000; value: %00000001010100000000000011010000),
    (mnemonic:'LDRSB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0; index: ind_stopexp)); mask: %00001111011100000000000011110000; value: %00000001011100000000000011010000),



    (mnemonic:'STRD';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23));                                                mask: %00001111011100000000111111110000; value: %00000000000000000000000011110000),
    (mnemonic:'STRD';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23; optional:false; defvalue:0; index: ind_stop));    mask: %00001111011100000000111111110000; value: %00000001000000000000000011110000),
    (mnemonic:'STRD';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23; optional:false; defvalue:0; index: ind_stopexp)); mask: %00001111011100000000111111110000; value: %00000001001000000000000011110000),


    (mnemonic:'LDRSH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23));                                                mask: %00001111011100000000111111110000; value: %00000000000100000000000011110000),
    (mnemonic:'LDRSH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23; optional:false; defvalue:0; index: ind_stop));    mask: %00001111011100000000111111110000; value: %00000001000100000000000011110000),
    (mnemonic:'LDRSH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23; optional:false; defvalue:0; index: ind_stopexp)); mask: %00001111011100000000111111110000; value: %00000001001100000000000011110000),

    (mnemonic:'STRD';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0));                          mask: %00001111011100000000000011110000; value: %00000000010000000000000011110000),
    (mnemonic:'STRD';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0; index: ind_stop));          mask: %00001111011100000000000011110000; value: %00000001010000000000000011110000),
    (mnemonic:'STRD';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0; index: ind_stopexp));       mask: %00001111011100000000000011110000; value: %00000001011000000000000011110000),


    (mnemonic:'LDRSH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm8LH_label; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0));              mask: %00001111011111110000000011110000; value: %00000000010111110000000011110000),

    (mnemonic:'LDRSH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0));                    mask: %00001111011100000000000011110000; value: %00000000010100000000000011110000),
    (mnemonic:'LDRSH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0; index: ind_stop));    mask: %00001111011100000000000011110000; value: %00000001010100000000000011110000),
    (mnemonic:'LDRSH';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0; index: ind_stopexp)); mask: %00001111011100000000000011110000; value: %00000001011100000000000011110000)


  );

  ArmInstructionsExtraLoadStoreInstructionsUnprivileged: array of TOpcode=(
    (mnemonic:'STRHT';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0));  mask: %00001111011100000000000011110000; value: %00000000011000000000000010110000),
    (mnemonic:'STRHT';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23));                                      mask: %00001111011100000000111111110000; value: %00000000001000000000000010110000),
    (mnemonic:'LDRHT';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0));  mask: %00001111011100000000000011110000; value: %00000000011100000000000010110000),
    (mnemonic:'LDRHT';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23));                                      mask: %00001111011100000000111111110000; value: %00000000001100000000000010110000),
    (mnemonic:'LDRSBT'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0));  mask: %00001111011100000000000011110000; value: %00000000011100000000000011010000),
    (mnemonic:'LDRSBT'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23));                                      mask: %00001111011100000000111111110000; value: %00000000001100000000000011010000),
    (mnemonic:'LDRSHT'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm8LH; offset:0 or (8 shl 8); maxval:15; extra:23; optional:true; defvalue:0));  mask: %00001111011100000000000011110000; value: %00000000011100000000000011110000),
    (mnemonic:'LDRSHT'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23));                                      mask: %00001111011100000000111111110000; value: %00000000001100000000000011110000)
  );





  ArmInstructionsDataProcessingRegisterShiftedRegister: array of TOpcode=(
    (mnemonic:'AND';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shifttype; offset:5; maxval: 3; extra: 8)); mask: %00001111111000000000000010010000; value: %00000000000000000000000000010000),
    (mnemonic:'EOR';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shifttype; offset:5; maxval: 3; extra: 8)); mask: %00001111111000000000000010010000; value: %00000000001000000000000000010000),
    (mnemonic:'SUB';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shifttype; offset:5; maxval: 3; extra: 8)); mask: %00001111111000000000000010010000; value: %00000000010000000000000000010000),
    (mnemonic:'RSB';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shifttype; offset:5; maxval: 3; extra: 8)); mask: %00001111111000000000000010010000; value: %00000000011000000000000000010000),
    (mnemonic:'ADD';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shifttype; offset:5; maxval: 3; extra: 8)); mask: %00001111111000000000000010010000; value: %00000000100000000000000000010000),
    (mnemonic:'ADC';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shifttype; offset:5; maxval: 3; extra: 8)); mask: %00001111111000000000000010010000; value: %00000000101000000000000000010000),
    (mnemonic:'SBC';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shifttype; offset:5; maxval: 3; extra: 8)); mask: %00001111111000000000000010010000; value: %00000000110000000000000000010000),
    (mnemonic:'RSC';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shifttype; offset:5; maxval: 3; extra: 8)); mask: %00001111111000000000000010010000; value: %00000000111000000000000000010000),

    (mnemonic:'TST';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shifttype; offset:5; maxval: 3; extra: 8));                                     mask: %00001111111100001111000010010000; value: %00000001000100000000000000010000),
    (mnemonic:'TEQ';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shifttype; offset:5; maxval: 3; extra: 8));                                     mask: %00001111111100001111000010010000; value: %00000001001100000000000000010000),
    (mnemonic:'CMP';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shifttype; offset:5; maxval: 3; extra: 8));                                     mask: %00001111111100001111000010010000; value: %00000001010100000000000000010000),
    (mnemonic:'CMN';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shifttype; offset:5; maxval: 3; extra: 8));                                     mask: %00001111111100001111000010010000; value: %00000001011100000000000000010000),

    (mnemonic:'ORR';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shifttype; offset:5; maxval: 3; extra: 8)); mask: %00001111111000000000000010010000; value: %00000001100000000000000000010000),
    (mnemonic:'LSL';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                                                      mask: %00001111111011110000000011110000; value: %00000001101000000000000000010000),
    (mnemonic:'LSR';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                                                      mask: %00001111111011110000000011110000; value: %00000001101000000000000000110000),
    (mnemonic:'ASR';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                                                      mask: %00001111111011110000000011110000; value: %00000001101000000000000001010000),
    (mnemonic:'ROR';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                                                      mask: %00001111111011110000000011110000; value: %00000001101000000000000001110000),

    (mnemonic:'BIC';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shifttype; offset:5; maxval: 3; extra: 8)); mask: %00001111111000000000000010010000; value: %00000001110000000000000000010000),
    (mnemonic:'MVN';  additions:[opa_s20, opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shifttype; offset:5; maxval: 3; extra: 8)); mask: %00001111111000000000000010010000; value: %00000001111000000000000000010000)
  );



  ArmInstructionsMSRImmediateAndHints: array of TOpcode=(
    (mnemonic:'NOP';  additions:[opa_cond]; params:(); mask: %00001111111111111111111111111111; value: %00000011001000001111000000000000),
    (mnemonic:'YIELD';additions:[opa_cond]; params:(); mask: %00001111111111111111111111111111; value: %00000011001000001111000000000001),
    (mnemonic:'WFE';  additions:[opa_cond]; params:(); mask: %00001111111111111111111111111111; value: %00000011001000001111000000000010),
    (mnemonic:'WFI';  additions:[opa_cond]; params:(); mask: %00001111111111111111111111111111; value: %00000011001000001111000000000011),
    (mnemonic:'SEV';  additions:[opa_cond]; params:(); mask: %00001111111111111111111111111111; value: %00000011001000001111000000000100),
    (mnemonic:'CSDB'; additions:[opa_cond]; params:(); mask: %00001111111111111111111111111111; value: %00000011001000001111000000010100),

    (mnemonic:'DBG';  additions:[opa_cond]; params:((ptype:pt_imm; offset:0; maxval:15)); mask: %00001111111111111111111111110000; value: %00000011001000001111000011110000),

    (mnemonic:'MSR';  additions:[opa_cond]; params:((ptype:pt_ap_reg; offset:18; maxval:3), (ptype:pt_const; offset:0; maxval: 4095));    mask: %00001111111100111111000000000000; value: %00000011001000001111000000000000),
    (mnemonic:'MSR';  additions:[opa_cond]; params:((ptype:pt_cpsr_reg; offset:16; maxval:15), (ptype:pt_const; offset:0; maxval: 4095)); mask: %00001111111100001111000000000000; value: %00000011001000001111000000000000),
    (mnemonic:'MSR';  additions:[opa_cond]; params:((ptype:pt_spsr_reg; offset:16; maxval:15), (ptype:pt_const; offset:0; maxval: 4095)); mask: %00001111111100001111000000000000; value: %00000011011000001111000000000000)

  );

  ArmInstructionsLoadStoreWordAndUnsignedByte: array of TOpcode=(

    (mnemonic:'PUSH'; additions:[opa_cond]; params:((ptype:pt_reglist_reg; offset:12; maxval:15));                                                                                                                                                                                       mask: %00001111111111110000111111111111; value: %00000101001011010000000000000100),
    (mnemonic:'POP';  additions:[opa_cond]; params:((ptype:pt_reglist_reg; offset:12; maxval:15));                                                                                                                                                                                       mask: %00001111111111110000111111111111; value: %00000100100111010000000000000100),



    (mnemonic:'STR';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signed_imm12; offset:0; maxval:4095; extra:23));                                                 mask: %00001111011100000000000000000000; value: %00000100000000000000000000000000),
    (mnemonic:'STR';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm12; offset:0; maxval:4095; extra:23; optional:true; defvalue:0; index: ind_stop));      mask: %00001111011100000000000000000000; value: %00000101000000000000000000000000),
    (mnemonic:'STR';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm12; offset:0; maxval:4095; extra:23; optional:false; defvalue:0; index: ind_stopexp));  mask: %00001111011100000000000000000000; value: %00000101001000000000000000000000),

    (mnemonic:'STR';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23),(ptype:pt_shift5; offset:5; maxval:63; extra:0; optional:true; defvalue:0));                                                                mask: %00001111011100000000000000010000; value: %00000110000000000000000000000000),
    (mnemonic:'STR';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23;optional:false; defvalue:0;index: ind_index),(ptype:pt_shift5; offset:5; maxval:63; extra:0; optional:true; defvalue:0; index: ind_stop));    mask: %00001111011100000000000000010000; value: %00000111000000000000000000000000),
    (mnemonic:'STR';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23;optional:false; defvalue:0;index: ind_index),(ptype:pt_shift5; offset:5; maxval:63; extra:0; optional:true; defvalue:0; index: ind_stopexp)); mask: %00001111011100000000000000010000; value: %00000111001000000000000000000000),


    (mnemonic:'STRT';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signed_imm12; offset:0; maxval:4095)); mask: %00001111011100000000000000000000; value: %00000100001000000000000000000000),
    (mnemonic:'STRT';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23),(ptype:pt_shift5; offset:5; maxval:63; extra:0; optional:true; defvalue:0));                                                               mask: %00001111011100000000000000000000; value: %00000110001000000000000000000000),


    (mnemonic:'LDR';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signed_imm12; offset:0; maxval:4095; extra:23));                                                mask: %00001111011100000000000000000000; value: %00000100000100000000000000000000),
    (mnemonic:'LDR';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm12; offset:0; maxval:4095; extra:23; optional:true; defvalue:0; index: ind_stop));     mask: %00001111011100000000000000000000; value: %00000101000100000000000000000000),
    (mnemonic:'LDR';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm12; offset:0; maxval:4095; extra:23; optional:false; defvalue:0; index: ind_stopexp)); mask: %00001111011100000000000000000000; value: %00000101001100000000000000000000),

    (mnemonic:'LDR';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_label; offset:0;  maxval:4095; extra:23; optional:false; defvalue:0));  mask: %00001111011111110000000000000000; value: %00000101000111110000000000000000),


    (mnemonic:'LDR';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23),(ptype:pt_shift5; offset:5; maxval:63; extra:0; optional:true; defvalue:0));                                                                mask: %00001111011100000000000000010000; value: %00000110000100000000000000000000),
    (mnemonic:'LDR';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23;optional:false; defvalue:0;index: ind_index),(ptype:pt_shift5; offset:5; maxval:63; extra:0; optional:true; defvalue:0; index: ind_stop));    mask: %00001111011100000000000000010000; value: %00000111000100000000000000000000),
    (mnemonic:'LDR';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23;optional:false; defvalue:0;index: ind_index),(ptype:pt_shift5; offset:5; maxval:63; extra:0; optional:true; defvalue:0; index: ind_stopexp)); mask: %00001111011100000000000000010000; value: %00000111001100000000000000000000),

    (mnemonic:'LDRT';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signed_imm12; offset:0; maxval:4095));                                                                                                                                                mask: %00001111011100000000000000000000; value: %00000100001100000000000000000000),
    (mnemonic:'LDRT';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23),(ptype:pt_shift5; offset:5; maxval:63; extra:0; optional:true; defvalue:0));                                                               mask: %00001111011100000000000000000000; value: %00000110001100000000000000000000),


    //
    (mnemonic:'STRB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signed_imm12; offset:0; maxval:4095; extra:23));                                                                                                                                      mask: %00001111011100000000000000000000; value: %00000100010000000000000000000000),
    (mnemonic:'STRB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm12; offset:0; maxval:4095; extra:23; optional:true; defvalue:0; index: ind_stop));                                                                                           mask: %00001111011100000000000000000000; value: %00000101010000000000000000000000),
    (mnemonic:'STRB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm12; offset:0; maxval:4095; extra:23; optional:false; defvalue:0; index: ind_stopexp));                                                                                       mask: %00001111011100000000000000000000; value: %00000101011000000000000000000000),

    (mnemonic:'STRB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23),(ptype:pt_shift5; offset:5; maxval:63; extra:0; optional:true; defvalue:0));                                                               mask: %00001111011100000000000000010000; value: %00000110010000000000000000000000),
    (mnemonic:'STRB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23;optional:false; defvalue:0;index: ind_index),(ptype:pt_shift5; offset:5; maxval:63; extra:0; optional:true; defvalue:0; index: ind_stop));   mask: %00001111011100000000000000010000; value: %00000111010000000000000000000000),
    (mnemonic:'STRB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23;optional:false; defvalue:0;index: ind_index),(ptype:pt_shift5; offset:5; maxval:63; extra:0; optional:true; defvalue:0; index: ind_stopexp));mask: %00001111011100000000000000010000; value: %00000111011000000000000000000000),


    (mnemonic:'STRBT';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signed_imm12; offset:0; maxval:4095));                                                                                                                                               mask: %00001111011100000000000000000000; value: %0000010011000000000000000000000),
    (mnemonic:'STRBT';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23),(ptype:pt_shift5; offset:5; maxval:63; extra:0; optional:true; defvalue:0));                                                              mask: %00001111011100000000000000000000; value: %00000110011000000000000000000000),

    (mnemonic:'LDRB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signed_imm12; offset:0; maxval:4095; extra:23));                                                                                                                                      mask: %00001111011100000000000000000000; value: %00000100010100000000000000000000),
    (mnemonic:'LDRB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm12; offset:0; maxval:4095; extra:23; optional:true; defvalue:0; index: ind_stop));                                                                                           mask: %00001111011100000000000000000000; value: %00000101010100000000000000000000),
    (mnemonic:'LDRB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm12; offset:0; maxval:4095; extra:23; optional:false; defvalue:0; index: ind_stopexp));                                                                                       mask: %00001111011100000000000000000000; value: %00000101011100000000000000000000),

    (mnemonic:'LDRB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_label; offset:0;  maxval:4095; extra:23; optional:false; defvalue:0));                                                                                                                                                                                                               mask: %00001111011111110000000000000000; value: %00000101010111110000000000000000),


    (mnemonic:'LDRB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23),(ptype:pt_shift5; offset:5; maxval:63; extra:0; optional:true; defvalue:0));                                                               mask: %00001111011100000000000000010000; value: %00000110010100000000000000000000),
    (mnemonic:'LDRB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23;optional:false; defvalue:0;index: ind_index),(ptype:pt_shift5; offset:5; maxval:63; extra:0; optional:true; defvalue:0; index: ind_stop));   mask: %00001111011100000000000000010000; value: %00000111010100000000000000000000),
    (mnemonic:'LDRB';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23;optional:false; defvalue:0;index: ind_index),(ptype:pt_shift5; offset:5; maxval:63; extra:0; optional:true; defvalue:0; index: ind_stopexp));mask: %00001111011100000000000000010000; value: %00000111011100000000000000000000),

    (mnemonic:'LDRBT';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signed_imm12; offset:0; maxval:4095));                                                                                                                                               mask: %00001111011100000000000000000000; value: %00000100011100000000000000000000),
    (mnemonic:'LDRBT';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signedrreg; offset:0; maxval:15; extra:23),(ptype:pt_shift5; offset:5; maxval:63; extra:0; optional:true; defvalue:0));                                                              mask: %00001111011100000000000000000000; value: %00000110011100000000000000000000)

  );

  ArmInstructionsParallelAdditionAndSubtractionSigned: array of TOpcode=(
    (mnemonic:'SADD16'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110000100000000111100010000),
    (mnemonic:'SASX';   additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110000100000000111100110000),
    (mnemonic:'SSAX';   additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110000100000000111101010000),
    (mnemonic:'SSUB16'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110000100000000111101110000),
    (mnemonic:'SADD8';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110000100000000111110010000),
    (mnemonic:'SSUB8';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110000100000000111111110000),

    (mnemonic:'QADD16'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110001000000000111100010000),
    (mnemonic:'QASX';   additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110001000000000111100110000),
    (mnemonic:'QSAX';   additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110001000000000111101010000),
    (mnemonic:'QSUB16'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110001000000000111101110000),
    (mnemonic:'QADD8';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110001000000000111110010000),
    (mnemonic:'QSUB8';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110001000000000111111110000),

    (mnemonic:'SHADD16'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110001100000000111100010000),
    (mnemonic:'SHASX';   additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110001100000000111100110000),
    (mnemonic:'SHSAX';   additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110001100000000111101010000),
    (mnemonic:'SHSUB16'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110001100000000111101110000),
    (mnemonic:'SHADD8';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110001100000000111110010000),
    (mnemonic:'SHSUB8';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110001100000000111111110000)

  );

  ArmInstructionsParallelAdditionAndSubtractionUnsigned: array of TOpcode=(
    (mnemonic:'UADD16'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110010100000000111100010000),
    (mnemonic:'UASX';   additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110010100000000111100110000),
    (mnemonic:'USAX';   additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110010100000000111101010000),
    (mnemonic:'USUB16'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110010100000000111101110000),
    (mnemonic:'UADD8';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110010100000000111110010000),
    (mnemonic:'USUB8';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));   mask: %00001111111100000000111111110000; value: %00000110010100000000111111110000),

    (mnemonic:'UQADD16'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));  mask: %00001111111100000000111111110000; value: %00000110011000000000111100010000),
    (mnemonic:'UQASX';   additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));  mask: %00001111111100000000111111110000; value: %00000110011000000000111100110000),
    (mnemonic:'UQSAX';   additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));  mask: %00001111111100000000111111110000; value: %00000110011000000000111101010000),
    (mnemonic:'UQSUB16'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));  mask: %00001111111100000000111111110000; value: %00000110011000000000111101110000),
    (mnemonic:'UQADD8';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));  mask: %00001111111100000000111111110000; value: %00000110011000000000111110010000),
    (mnemonic:'UQSUB8';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));  mask: %00001111111100000000111111110000; value: %00000110011000000000111111110000),

    (mnemonic:'UHADD16'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));  mask: %00001111111100000000111111110000; value: %00000110011100000000111100010000),
    (mnemonic:'UHASX';   additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));  mask: %00001111111100000000111111110000; value: %00000110011100000000111100110000),
    (mnemonic:'UHSAX';   additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));  mask: %00001111111100000000111111110000; value: %00000110011100000000111101010000),
    (mnemonic:'UHSUB16'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));  mask: %00001111111100000000111111110000; value: %00000110011100000000111101110000),
    (mnemonic:'UHADD8';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));  mask: %00001111111100000000111111110000; value: %00000110011100000000111110010000),
    (mnemonic:'UHSUB8';  additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));  mask: %00001111111100000000111111110000; value: %00000110011100000000111111110000)

  );

  ArmInstructionsPackingUnpackingSaturationAndReversal: array of TOpcode=(
    (mnemonic:'PKHBT'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true));   mask: %00001111111100000000000001110000; value: %00000110100000000000000000010000),
    (mnemonic:'PKHTB'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_shift5; offset:5; maxval: 63; extra:0; optional:true));   mask: %00001111111100000000000001110000; value: %00000110100000000000000001010000),
    (mnemonic:'SXTB16'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rotatex8; offset:10; maxval: 3; extra:0; optional:true));                           mask: %00001111111111110000001111110000; value: %00000110100011110000000001110000),
    (mnemonic:'SXTAB16'; additions:[opa_cond];params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rotatex8; offset:10; maxval: 3; extra:0; optional:true)); mask: %00001111111100000000001111110000; value: %00000110100000000000000001110000),
    (mnemonic:'SEL'; additions:[opa_cond];    params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0));                                                                   mask: %00001111111100000000111111110000; value: %00000110100000000000111110110000),
    (mnemonic:'SSAT16'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_imm; offset:16; maxval: 31), (ptype:pt_rreg; offset:0));                                                       mask: %00001111111100000000111111110000; value: %00000110101000000000111100110000),
    (mnemonic:'SSAT'; additions:[opa_cond];   params:((ptype:pt_rreg; offset:12),(ptype:pt_imm; offset:16; maxval: 31), (ptype:pt_rreg; offset:0), (ptype:pt_shift5; offset:5));                          mask: %00001111111000000000000000110000; value: %00000110101000000000000000010000),
    (mnemonic:'SXTB'; additions:[opa_cond];   params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rotatex8; offset:10; maxval: 3; extra:0; optional:true));                           mask: %00001111111111110000001111110000; value: %00000110101011110000000001110000),
    (mnemonic:'SXTAB'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rotatex8; offset:10; maxval: 3; extra:0; optional:true)); mask: %00001111111100000000001111110000; value: %00000110101000000000000001110000),
    (mnemonic:'REV'; additions:[opa_cond];    params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0));                                                                                              mask: %00001111111111110000111111110000; value: %00000110101111110000111110110000),

    (mnemonic:'SXTH'; additions:[opa_cond];   params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rotatex8; offset:10; maxval: 3; extra:0; optional:true));                           mask: %00001111111111110000001111110000; value: %00000110101111110000000001110000),
    (mnemonic:'SXTAH'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rotatex8; offset:10; maxval: 3; extra:0; optional:true)); mask: %00001111111100000000001111110000; value: %00000110101100000000000001110000),
    (mnemonic:'REV16'; additions:[opa_cond];    params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0));                                                                                            mask: %00001111111111110000111111110000; value: %00000110101111110000111100110000),


    (mnemonic:'UXTB16'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rotatex8; offset:10; maxval: 3; extra:0; optional:true));                           mask: %00001111111111110000001111110000; value: %00000110110011110000000001110000),
    (mnemonic:'UXTAB16'; additions:[opa_cond];params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rotatex8; offset:10; maxval: 3; extra:0; optional:true)); mask: %00001111111100000000001111110000; value: %00000110110000000000000001110000),

    (mnemonic:'USAT16'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_imm; offset:16; maxval: 32),(ptype:pt_rreg; offset:0));                                                        mask: %00001111111100000000111111110000; value: %00000110111000000000111100110000),
    (mnemonic:'USAT'; additions:[opa_cond];   params:((ptype:pt_rreg; offset:12),(ptype:pt_imm; offset:16; maxval: 31), (ptype:pt_rreg; offset:0), (ptype:pt_shift5; offset:5));                          mask: %00001111111000000000000000110000; value: %00000110111000000000000000010000),

    (mnemonic:'UXTB'; additions:[opa_cond];   params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rotatex8; offset:10; maxval: 3; extra:0; optional:true));                           mask: %00001111111111110000001111110000; value: %00000110111011110000000001110000),
    (mnemonic:'UXTAB'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rotatex8; offset:10; maxval: 3; extra:0; optional:true)); mask: %00001111111100000000001111110000; value: %00000110111000000000000001110000),
    (mnemonic:'RBIT'; additions:[opa_cond];   params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0));                                                                                              mask: %00001111111111110000111111110000; value: %00000110111111110000111100110000),

    (mnemonic:'UXTH'; additions:[opa_cond];   params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rotatex8; offset:10; maxval: 3; extra:0; optional:true));                           mask: %00001111111111110000001111110000; value: %00000110111111110000000001110000),
    (mnemonic:'UXTAH'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rotatex8; offset:10; maxval: 3; extra:0; optional:true)); mask: %00001111111100000000001111110000; value: %00000110111100000000000001110000),
    (mnemonic:'REVSH'; additions:[opa_cond];    params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0));                                                                                            mask: %00001111111111110000111111110000; value: %00000110111111110000111110110000)


  );

  ArmInstructionsSignedMultiplySignedAndUnsignedDivide: array of TOpcode=(
    (mnemonic:'SMUAD';additions:[opa_cond];   params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                                                         mask: %00001111111100001111000011110000; value: %00000111000000000000000000010000),
    (mnemonic:'SMUADX';additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                                                         mask: %00001111111100001111000011110000; value: %00000111000000000000000000110000),
    (mnemonic:'SMLAD';additions:[opa_cond];   params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8),(ptype:pt_rreg; offset:12));                              mask: %00001111111100000000000011110000; value: %00000111000000000000000000010000),
    (mnemonic:'SMLADX';additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8),(ptype:pt_rreg; offset:12));                              mask: %00001111111100000000000011110000; value: %00000111000000000000000000110000),

    (mnemonic:'SDIV';additions:[opa_cond];    params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                                                         mask: %00001111111100001111000011110000; value: %00000111000100001111000000010000),
    (mnemonic:'UDIV';additions:[opa_cond];    params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                                                         mask: %00001111111100001111000011110000; value: %00000111001100001111000000010000),

    (mnemonic:'SMLALD';additions:[opa_cond];   params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                             mask: %00001111111100000000000011110000; value: %00000111010000000000000000010000),
    (mnemonic:'SMLALDX';additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                             mask: %00001111111100000000000011110000; value: %00000111010000000000000000110000),

    (mnemonic:'SMLSLD';additions:[opa_cond];   params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                             mask: %00001111111100000000000011110000; value: %00000111010000000000000001010000),
    (mnemonic:'SMLSLDX';additions:[opa_cond];  params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                             mask: %00001111111100000000000011110000; value: %00000111010000000000000001110000),


    (mnemonic:'SMMUL';additions:[opa_cond];   params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                                                         mask: %00001111111100001111000011110000; value: %00000111010100000000000000010000),
    (mnemonic:'SMMULX';additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                                                         mask: %00001111111100001111000011110000; value: %00000111010100000000000000110000),

    (mnemonic:'SMMLA';additions:[opa_cond];    params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8),(ptype:pt_rreg; offset:12));                             mask: %00001111111100000000000011110000; value: %00000111010100000000000001010000),
    (mnemonic:'SMMLAX';additions:[opa_cond];   params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8),(ptype:pt_rreg; offset:12));                             mask: %00001111111100000000000011110000; value: %00000111010100000000000001110000),

    (mnemonic:'SMMLS';additions:[opa_cond];   params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8), (ptype:pt_rreg; offset:12));                             mask: %00001111111100000000000011110000; value: %00000111010100000000000001010000),
    (mnemonic:'SMMLSX';additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8), (ptype:pt_rreg; offset:12));                             mask: %00001111111100000000000011110000; value: %00000111010100000000000001110000)


  );


  ArmInstructionsGroupMediaInstructions: array of TOpcode=(
    (mnemonic:'USAD8'; additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8));                                                         mask: %00001111111100001111000011110000; value: %00000111100000001111000000010000),
    (mnemonic:'USADA8';additions:[opa_cond];  params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_rreg; offset:8),(ptype:pt_rreg; offset:12));                              mask: %00001111111100000000000011110000; value: %00000111100000000000000000010000),
    (mnemonic:'SBFX';additions:[opa_cond];    params:((ptype:pt_rreg; offset:16),(ptype:pt_rreg; offset:0),(ptype:pt_imm; offset:7; maxval:%11111),(ptype:pt_imm; offset:16; maxval:%11111));  mask: %00001111111000000000000001110000; value: %00000111101000000000000001010000),
    (mnemonic:'BFC';additions:[opa_cond];     params:((ptype:pt_rreg; offset:12),(ptype:pt_imm; offset:7; maxval:%11111),(ptype:pt_imm; offset:16; maxval:%11111));                            mask: %00001111111000000000000001111111; value: %00000111101000000000000000011111),
    (mnemonic:'BFI';additions:[opa_cond];     params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_imm; offset:7; maxval:%11111),(ptype:pt_imm; offset:16; maxval:%11111));  mask: %00001111111000000000000001110000; value: %00000111110000000000000000010000),
    (mnemonic:'UBFX';additions:[opa_cond];    params:((ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:0),(ptype:pt_imm; offset:7; maxval:%11111),(ptype:pt_imm; offset:16; maxval:%11111));  mask: %00001111111000000000000001110000; value: %00000111111000000000000001010000),
    (mnemonic:'UDF';additions:[];             params:((ptype:pt_imm16_4_12; offset:0; maxval:65535));                                                                                          mask: %11111111111100000000000011110000; value: %11100111111100000000000011110000)

  );

  ArmInstructionsBranchBranchWithLinkAndBlockDataTransfer: array of TOpcode=(
    (mnemonic:'STMDA'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16; offset:0));     mask: %00001111111100000000000000000000; value: %00001000000000000000000000000000),
    (mnemonic:'STMDA'; additions:[opa_cond]; params:((ptype:pt_rreg_ex; offset:16),(ptype:pt_reglist16; offset:0));  mask: %00001111111100000000000000000000; value: %00001000001000000000000000000000),

    (mnemonic:'LDMDA'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16; offset:0));     mask: %00001111111100000000000000000000; value: %00001000000100000000000000000000),
    (mnemonic:'LDMDA'; additions:[opa_cond]; params:((ptype:pt_rreg_ex; offset:16),(ptype:pt_reglist16; offset:0));  mask: %00001111111100000000000000000000; value: %00001000001100000000000000000000),

    (mnemonic:'STM'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16; offset:0));       mask: %00001111111100000000000000000000; value: %00001000100000000000000000000000),
    (mnemonic:'STM'; additions:[opa_cond]; params:((ptype:pt_rreg_ex; offset:16),(ptype:pt_reglist16; offset:0));    mask: %00001111111100000000000000000000; value: %00001000101000000000000000000000),

    (mnemonic:'LDM'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16; offset:0));       mask: %00001111111100000000000000000000; value: %00001000100100000000000000000000),
    (mnemonic:'LDM'; additions:[opa_cond]; params:((ptype:pt_rreg_ex; offset:16),(ptype:pt_reglist16; offset:0));    mask: %00001111111100000000000000000000; value: %00001000101100000000000000000000),

    (mnemonic:'POP';  additions:[opa_cond]; params:((ptype:pt_reglist16; offset:0; maxval:65535; extra:2));          mask: %00001111111111110000000000000000; value: %00001000101111010000000000000000),
    (mnemonic:'PUSH'; additions:[opa_cond]; params:((ptype:pt_reglist16; offset:0; maxval:65535; extra:2));          mask: %00001111111111110000000000000000; value: %00001001001011010000000000000000),

    (mnemonic:'STMDB'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16; offset:0));     mask: %00001111111100000000000000000000; value: %00001001000000000000000000000000),
    (mnemonic:'STMDB'; additions:[opa_cond]; params:((ptype:pt_rreg_ex; offset:16),(ptype:pt_reglist16; offset:0));  mask: %00001111111100000000000000000000; value: %00001001001000000000000000000000),

    (mnemonic:'LDMDB'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16; offset:0));     mask: %00001111111100000000000000000000; value: %00001001000100000000000000000000),
    (mnemonic:'LDMDB'; additions:[opa_cond]; params:((ptype:pt_rreg_ex; offset:16),(ptype:pt_reglist16; offset:0));  mask: %00001111111100000000000000000000; value: %00001001001100000000000000000000),

    (mnemonic:'STMIB'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16; offset:0));     mask: %00001111111100000000000000000000; value: %00001001100000000000000000000000),
    (mnemonic:'STMIB'; additions:[opa_cond]; params:((ptype:pt_rreg_ex; offset:16),(ptype:pt_reglist16; offset:0));  mask: %00001111111100000000000000000000; value: %00001001101000000000000000000000),

    (mnemonic:'LDMIB'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16; offset:0));     mask: %00001111111100000000000000000000; value: %00001001100100000000000000000000),
    (mnemonic:'LDMIB'; additions:[opa_cond]; params:((ptype:pt_rreg_ex; offset:16),(ptype:pt_reglist16; offset:0));  mask: %00001111111100000000000000000000; value: %00001001101100000000000000000000),

    //                                                                                                            //               pu                                        pu
    (mnemonic:'STMDA'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100000000000000000000; value: %00001000010000000000000000000000),
    (mnemonic:'STMED'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100000000000000000000; value: %00001000010000000000000000000000),
    (mnemonic:'STMDB'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100000000000000000000; value: %00001001010000000000000000000000),
    (mnemonic:'STMFD'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100000000000000000000; value: %00001001010000000000000000000000),
    (mnemonic:'STMIA'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100000000000000000000; value: %00001000110000000000000000000000),
    (mnemonic:'STMEA'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100000000000000000000; value: %00001000110000000000000000000000),
    (mnemonic:'STMIB'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100000000000000000000; value: %00001001110000000000000000000000),
    (mnemonic:'STMFA'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100000000000000000000; value: %00001001110000000000000000000000),

    (mnemonic:'LDMDA'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100001000000000000000; value: %00001000010100000000000000000000),
    (mnemonic:'LDMFA'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100001000000000000000; value: %00001000010100000000000000000000),
    (mnemonic:'LDMDB'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100001000000000000000; value: %00001001010100000000000000000000),
    (mnemonic:'LDMEA'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100001000000000000000; value: %00001001010100000000000000000000),
    (mnemonic:'LDMIA'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100001000000000000000; value: %00001000110100000000000000000000),
    (mnemonic:'LDMFD'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100001000000000000000; value: %00001000110100000000000000000000),
    (mnemonic:'LDMIB'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100001000000000000000; value: %00001001110100000000000000000000),
    (mnemonic:'LDMED'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100001000000000000000; value: %00001001110100000000000000000000),

    (mnemonic:'LDMDA'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100001000000000000000; value: %00001000010100001000000000000000),
    (mnemonic:'LDMFA'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100001000000000000000; value: %00001000010100001000000000000000),
    (mnemonic:'LDMDB'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100001000000000000000; value: %00001001010100001000000000000000),
    (mnemonic:'LDMEA'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100001000000000000000; value: %00001001010100001000000000000000),
    (mnemonic:'LDMIA'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100001000000000000000; value: %00001000110100001000000000000000),
    (mnemonic:'LDMFD'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100001000000000000000; value: %00001000110100001000000000000000),
    (mnemonic:'LDMIB'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100001000000000000000; value: %00001001110100001000000000000000),
    (mnemonic:'LDMED'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:16),(ptype:pt_reglist16_x; offset:0));   mask: %00001111111100001000000000000000; value: %00001001110100001000000000000000),

    (mnemonic:'LDMDA'; additions:[opa_cond]; params:((ptype:pt_rreg_ex; offset:16),(ptype:pt_reglist16_x; offset:0));Mask: %00001111111100001000000000000000; value: %00001000011100001000000000000000),
    (mnemonic:'LDMFA'; additions:[opa_cond]; params:((ptype:pt_rreg_ex; offset:16),(ptype:pt_reglist16_x; offset:0));mask: %00001111111100001000000000000000; value: %00001000011100001000000000000000),
    (mnemonic:'LDMDB'; additions:[opa_cond]; params:((ptype:pt_rreg_ex; offset:16),(ptype:pt_reglist16_x; offset:0));mask: %00001111111100001000000000000000; value: %00001001011100001000000000000000),
    (mnemonic:'LDMEA'; additions:[opa_cond]; params:((ptype:pt_rreg_ex; offset:16),(ptype:pt_reglist16_x; offset:0));mask: %00001111111100001000000000000000; value: %00001001011100001000000000000000),
    (mnemonic:'LDMIA'; additions:[opa_cond]; params:((ptype:pt_rreg_ex; offset:16),(ptype:pt_reglist16_x; offset:0));mask: %00001111111100001000000000000000; value: %00001000111100001000000000000000),
    (mnemonic:'LDMFD'; additions:[opa_cond]; params:((ptype:pt_rreg_ex; offset:16),(ptype:pt_reglist16_x; offset:0));mask: %00001111111100001000000000000000; value: %00001000111100001000000000000000),
    (mnemonic:'LDMIB'; additions:[opa_cond]; params:((ptype:pt_rreg_ex; offset:16),(ptype:pt_reglist16_x; offset:0));mask: %00001111111100001000000000000000; value: %00001001111100001000000000000000),
    (mnemonic:'LDMED'; additions:[opa_cond]; params:((ptype:pt_rreg_ex; offset:16),(ptype:pt_reglist16_x; offset:0));mask: %00001111111100001000000000000000; value: %00001001111100001000000000000000),


    (mnemonic:'B';   additions:[opa_cond]; params:((ptype:pt_signed_label; offset:0; maxval: $FFFFFF; extra:0 ));    mask: %00001111000000000000000000000000; value: %00001010000000000000000000000000),
    (mnemonic:'BL';  additions:[opa_cond]; params:((ptype:pt_signed_label; offset:0; maxval: $FFFFFF; extra:0 ));    mask: %00001111000000000000000000000000; value: %00001011000000000000000000000000),
    (mnemonic:'BL';  additions:[opa_cond]; params:((ptype:pt_signed_label; offset:0; maxval: $FFFFFF; extra:0 ));    mask: %00001111000000000000000000000000; value: %00001011000000000000000000000000)


  );


  ArmInstructionsGroupCoprocessorInstructionsAndSupervisorCall: array of TOpcode=(


    (mnemonic:'SVC'; additions:[opa_cond]; params:((ptype:pt_imm; offset:0; maxval: $FFFFFF));    mask: %00001111000000000000000000000000; value: %00001111000000000000000000000000),
                                                                                                                                                                                                                                                                                                                       //      xxxxxxxPUDW0                              xxxxxxxPUDW
    (mnemonic:'STC'; additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm; offset:0; maxval:255));                                                                       mask: %00001111111100000000000000000000; value: %00001100000000000000000000000000), //unindexed (p=0, w=0, U=1)
    (mnemonic:'STC'; additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signed_imm8_shl2; offset:0; maxval:255;extra:23));                                                 mask: %00001111011100000000000000000000; value: %00001100001000000000000000000000), //post-indexed (p=0, w=1, U=*)
    (mnemonic:'STC'; additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm8_shl2; offset:0; maxval:$FF; extra:23; optional:false; defvalue:0; index: ind_stopexp)); mask: %00001111011100000000000000000000; value: %00001101001000000000000000000000), //pre-index (p=1, W=1, U=*)
    (mnemonic:'STC'; additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm8_shl2; offset:0; maxval:$FF; extra:23; optional:false; defvalue:0; index: ind_stop));    mask: %00001111011100000000000000000000; value: %00001101000000000000000000000000), //offset (p=1 w=0)

    (mnemonic:'STCL';additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm; offset:0; maxval:255));                                                                       mask: %00001111111100000000000000000000; value: %00001100010000000000000000000000), //unindexed (p=0, w=0, U=1)
    (mnemonic:'STCL';additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signed_imm8_shl2; offset:0; maxval:255;extra:23));                                                 mask: %00001111011100000000000000000000; value: %00001100011000000000000000000000), //post-indexed (p=0, w=1, U=*)
    (mnemonic:'STCL';additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm8_shl2; offset:0; maxval:$FF; extra:23; optional:false; defvalue:0; index: ind_stopexp)); mask: %00001111011100000000000000000000; value: %00001101011000000000000000000000), //pre-index (p=1, W=1, U=*)
    (mnemonic:'STCL';additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm8_shl2; offset:0; maxval:$FF; extra:23; optional:false; defvalue:0; index: ind_stop));    mask: %00001111011100000000000000000000; value: %00001101010000000000000000000000), //offset (p=1 w=0)

                                                                                                                                                                                                                                                                                                                      //      xxxxxxxPUDW1                              xxxxxxxPUDW1
    (mnemonic:'LDC'; additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm; offset:0; maxval:255));                                                                       mask: %00001111111100000000000000000000; value: %00001100000100000000000000000000), //unindexed (p=0, w=0, U=1)
    (mnemonic:'LDC'; additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signed_imm8_shl2; offset:0; maxval:255;extra:23));                                                 mask: %00001111011100000000000000000000; value: %00001100001100000000000000000000), //post-indexed (p=0, w=1, U=*)
    (mnemonic:'LDC'; additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm8_shl2; offset:0; maxval:$FF; extra:23; optional:false; defvalue:0; index: ind_stopexp)); mask: %00001111011100000000000000000000; value: %00001101001100000000000000000000), //pre-index (p=1, W=1, U=*)
    (mnemonic:'LDC'; additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm8_shl2; offset:0; maxval:$FF; extra:23; optional:false; defvalue:0; index: ind_stop));    mask: %00001111011100000000000000000000; value: %00001101000100000000000000000000), //offset (p=1 w=0)

    (mnemonic:'LDCL';additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm; offset:0; maxval:255));                                                                       mask: %00001111111100000000000000000000; value: %00001100010100000000000000000000), //unindexed (p=0, w=0, U=1)
    (mnemonic:'LDCL';additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signed_imm8_shl2; offset:0; maxval:255;extra:23));                                                 mask: %00001111011100000000000000000000; value: %00001100011100000000000000000000), //post-indexed (p=0, w=1, U=*)
    (mnemonic:'LDCL';additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm8_shl2; offset:0; maxval:$FF; extra:23; optional:false; defvalue:0; index: ind_stopexp)); mask: %00001111011100000000000000000000; value: %00001101011100000000000000000000), //pre-index (p=1, W=1, U=*)
    (mnemonic:'LDCL';additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm8_shl2; offset:0; maxval:$FF; extra:23; optional:false; defvalue:0; index: ind_stop));    mask: %00001111011100000000000000000000; value: %00001101010100000000000000000000), //offset (p=1 w=0)

    //LDC (literal)


    (mnemonic:'LDC'; additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_label; offset:0; maxval:$ff; extra:23));                                                                                                                                                          mask: %00001111011111110000000000000000; value: %00001101000111110000000000000000), //normal    (p=1, w=0, U=*)
    (mnemonic:'LDC'; additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single), (ptype:pt_imm; offset:12; maxval:$ff));                                                                     mask: %00001111111111110000000000000000; value: %00001100100111110000000000000000), //unindexed (p=0, w=0, U=1)
    (mnemonic:'LDCL';additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_label; offset:0; maxval:$ff; extra:23));                                                                                                                                                          mask: %00001111011111110000000000000000; value: %00001101001111110000000000000000), //normal    (p=1, w=0, U=*)
    (mnemonic:'LDCL';additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single), (ptype:pt_imm; offset:12; maxval:$ff));                                                                     mask: %00001111111111110000000000000000; value: %00001100101111110000000000000000), //unindexed (p=0, w=0, U=1)
    (mnemonic:'MCRR';  additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_imm; offset:4; maxval:$f),(ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_crreg; offset:0));                                                                                                                mask: %00001111111100000000000000000000; value: %00001100010000000000000000000000),
    (mnemonic:'MRRC'; additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_imm; offset:4; maxval:$f),(ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_crreg; offset:0));                                                                                                                 mask: %00001111111100000000000000000000; value: %00001100010100000000000000000000),
    (mnemonic:'CDP';  additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_imm; offset:4; maxval:$f),(ptype:pt_crreg; offset:12),(ptype:pt_crreg; offset:16),(ptype:pt_crreg; offset:0),(ptype:pt_imm; offset:5; maxval:7));                                                                            mask: %00001111000000000000000000010000; value: %00001110000000000000000000000000),
    (mnemonic:'MCR';  additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_imm; offset:4; maxval:7),(ptype:pt_crreg; offset:12),(ptype:pt_crreg; offset:16),(ptype:pt_crreg; offset:0),(ptype:pt_imm; offset:5; maxval:7));                                                                             mask: %00001111000100000000000000010000; value: %00001110000000000000000000010000),
    (mnemonic:'MRC';  additions:[opa_cond]; params:((ptype:pt_coproc; offset:8), (ptype:pt_imm; offset:4; maxval:7),(ptype:pt_crreg; offset:12),(ptype:pt_crreg; offset:16),(ptype:pt_crreg; offset:0),(ptype:pt_imm; offset:5; maxval:7));                                                                             mask: %00001111000100000000000000010000; value: %00001110000100000000000000010000)
  );

  ArmInstructionsUnconditionalInstructions: array of TOpcode=(
                                                                                                                       //        PU1W0                                     PU1W
    (mnemonic:'SRSDA';  additions:[]; params:((ptype:pt_rreg; offset:16), (ptype:pt_imm; offset:0; maxval:$1f));   mask: %11111111111111111111111111100000; value: %11111000010011010000010100000000), //DA P=0, U=0
    (mnemonic:'SRSDB';  additions:[]; params:((ptype:pt_rreg; offset:16), (ptype:pt_imm; offset:0; maxval:$1f));   mask: %11111111111111111111111111100000; value: %11111001010011010000010100000000), //DB P=1, U=0
    (mnemonic:'SRSIA';  additions:[]; params:((ptype:pt_rreg; offset:16), (ptype:pt_imm; offset:0; maxval:$1f));   mask: %11111111111111111111111111100000; value: %11111000110011010000010100000000), //DB P=0, U=1
    (mnemonic:'SRSIB';  additions:[]; params:((ptype:pt_rreg; offset:16), (ptype:pt_imm; offset:0; maxval:$1f));   mask: %11111111111111111111111111100000; value: %11111001110011010000010100000000), //DB P=1, U=1

    (mnemonic:'SRSDA'; additions:[]; params:((ptype:pt_rreg_ex; offset:16), (ptype:pt_imm; offset:0; maxval:$1f)); mask: %11111111111111111111111111100000; value: %11111000011011010000010100000000), //DA P=0, U=0
    (mnemonic:'SRSDB'; additions:[]; params:((ptype:pt_rreg_ex; offset:16), (ptype:pt_imm; offset:0; maxval:$1f)); mask: %11111111111111111111111111100000; value: %11111001011011010000010100000000), //DB P=1, U=0
    (mnemonic:'SRSIA'; additions:[]; params:((ptype:pt_rreg_ex; offset:16), (ptype:pt_imm; offset:0; maxval:$1f)); mask: %11111111111111111111111111100000; value: %11111000111011010000010100000000), //DB P=0, U=1
    (mnemonic:'SRSIB'; additions:[]; params:((ptype:pt_rreg_ex; offset:16), (ptype:pt_imm; offset:0; maxval:$1f)); mask: %11111111111111111111111111100000; value: %11111001111011010000010100000000), //DB P=1, U=1

      //                                                                                                                      XXXX100PU0W1
    (mnemonic:'RFEDA';  additions:[]; params:((ptype:pt_rreg; offset:16));   mask: %11111111111100001111111111111111; value: %11111000000100000000101000000000), //DA P=0, U=0
    (mnemonic:'RFEDB';  additions:[]; params:((ptype:pt_rreg; offset:16));   mask: %11111111111100001111111111111111; value: %11111001000100000000101000000000), //DB P=1, U=0
    (mnemonic:'RFEIA';  additions:[]; params:((ptype:pt_rreg; offset:16));   mask: %11111111111100001111111111111111; value: %11111000100100000000101000000000), //DB P=0, U=1
    (mnemonic:'RFEIB';  additions:[]; params:((ptype:pt_rreg; offset:16));   mask: %11111111111100001111111111111111; value: %11111001100100000000101000000000), //DB P=1, U=1

    (mnemonic:'RFEDA'; additions:[]; params:((ptype:pt_rreg_ex; offset:16)); mask: %11111111111100001111111111111111; value: %11111000001100000000101000000000), //DA P=0, U=0
    (mnemonic:'RFEDB'; additions:[]; params:((ptype:pt_rreg_ex; offset:16)); mask: %11111111111100001111111111111111; value: %11111001001100000000101000000000), //DB P=1, U=0
    (mnemonic:'RFEIA'; additions:[]; params:((ptype:pt_rreg_ex; offset:16)); mask: %11111111111100001111111111111111; value: %11111000101100000000101000000000), //DB P=0, U=1
    (mnemonic:'RFEIB'; additions:[]; params:((ptype:pt_rreg_ex; offset:16)); mask: %11111111111100001111111111111111; value: %11111001101100000000101000000000), //DB P=1, U=1


    (mnemonic:'BLX'; additions:[]; params:((ptype:pt_signed_label; offset:0; maxval: $FFFFFF; extra:0 ));    mask: %11111111100000000000000000000000; value: %11111010000000000000000000000000),
    (mnemonic:'BLX'; additions:[]; params:((ptype:pt_signed_label; offset:0; maxval: $FFFFFF; extra:1 ));    mask: %11111111100000000000000000000000; value: %11111011000000000000000000000000),


    (mnemonic:'STC2';additions:[]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm; offset:0; maxval:255));                                                                       mask: %11111111111100000000000000000000; value: %11111100000000000000000000000000), //unindexed (p=0, w=0, U=1)
    (mnemonic:'STC2';additions:[]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signed_imm8_shl2; offset:0; maxval:255;extra:23));                                                 mask: %11111111011100000000000000000000; value: %11111100001000000000000000000000), //post-indexed (p=0, w=1, U=*)
    (mnemonic:'STC2';additions:[]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm8_shl2; offset:0; maxval:$FF; extra:23; optional:false; defvalue:0; index: ind_stopexp)); mask: %11111111011100000000000000000000; value: %11111101001000000000000000000000), //pre-index (p=1, W=1, U=*)
    (mnemonic:'STC2';additions:[]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm8_shl2; offset:0; maxval:$FF; extra:23; optional:false; defvalue:0; index: ind_stop));    mask: %11111111011100000000000000000000; value: %11111101000000000000000000000000), //offset (p=1 w=0)

    (mnemonic:'STC2L';additions:[];params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm; offset:0; maxval:255));                                                                       mask: %11111111111100000000000000000000; value: %11111100010000000000000000000000), //unindexed (p=0, w=0, U=1)
    (mnemonic:'STC2L';additions:[];params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signed_imm8_shl2; offset:0; maxval:255;extra:23));                                                 mask: %11111111011100000000000000000000; value: %11111100011000000000000000000000), //post-indexed (p=0, w=1, U=*)
    (mnemonic:'STC2L';additions:[];params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm8_shl2; offset:0; maxval:$FF; extra:23; optional:false; defvalue:0; index: ind_stopexp)); mask: %11111111011100000000000000000000; value: %11111101011000000000000000000000), //pre-index (p=1, W=1, U=*)
    (mnemonic:'STC2L';additions:[];params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm8_shl2; offset:0; maxval:$FF; extra:23; optional:false; defvalue:0; index: ind_stop));    mask: %11111111011100000000000000000000; value: %11111101010000000000000000000000), //offset (p=1 w=0)


    (mnemonic:'LDC2';additions:[]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm; offset:0; maxval:255));                                                                       mask: %11111111111100000000000000000000; value: %11111100000000000000000000000000), //unindexed (p=0, w=0, U=1)
    (mnemonic:'LDC2';additions:[]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signed_imm8_shl2; offset:0; maxval:255;extra:23));                                                 mask: %11111111011100000000000000000000; value: %11111100001000000000000000000000), //post-indexed (p=0, w=1, U=*)
    (mnemonic:'LDC2';additions:[]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm8_shl2; offset:0; maxval:$FF; extra:23; optional:false; defvalue:0; index: ind_stopexp)); mask: %11111111011100000000000000000000; value: %11111101001000000000000000000000), //pre-index (p=1, W=1, U=*)
    (mnemonic:'LDC2';additions:[]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm8_shl2; offset:0; maxval:$FF; extra:23; optional:false; defvalue:0; index: ind_stop));    mask: %11111111011100000000000000000000; value: %11111101000000000000000000000000), //offset (p=1 w=0)

    (mnemonic:'LDC2L';additions:[];params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_imm; offset:0; maxval:255));                                                                       mask: %11111111111100000000000000000000; value: %11111100010000000000000000000000), //unindexed (p=0, w=0, U=1)
    (mnemonic:'LDC2L';additions:[];params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single),(ptype:pt_signed_imm8_shl2; offset:0; maxval:255;extra:23));                                                 mask: %11111111011100000000000000000000; value: %11111100011000000000000000000000), //post-indexed (p=0, w=1, U=*)
    (mnemonic:'LDC2L';additions:[];params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm8_shl2; offset:0; maxval:$FF; extra:23; optional:false; defvalue:0; index: ind_stopexp)); mask: %11111111011100000000000000000000; value: %11111101011000000000000000000000), //pre-index (p=1, W=1, U=*)
    (mnemonic:'LDC2L';additions:[];params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index),(ptype:pt_signed_imm8_shl2; offset:0; maxval:$FF; extra:23; optional:false; defvalue:0; index: ind_stop));    mask: %11111111011100000000000000000000; value: %11111101010000000000000000000000), //offset (p=1 w=0)


    (mnemonic:'LDC2'; additions:[]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_label; offset:0; maxval:$ff; extra:23));                                                                                                                                                         mask: %11111111011111110000000000000000; value: %11111101000111110000000000000000), //normal    (p=1, w=0, U=*)
    (mnemonic:'LDC2'; additions:[]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single), (ptype:pt_imm; offset:12; maxval:$ff));                                                                    mask: %11111111111111110000000000000000; value: %11111100100111110000000000000000), //unindexed (p=0, w=0, U=1)

    (mnemonic:'LDC2L';additions:[]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_label; offset:0; maxval:$ff; extra:23));                                                                                                                                                         mask: %11111111011111110000000000000000; value: %11111101001111110000000000000000), //normal    (p=1, w=0, U=*)
    (mnemonic:'LDC2L';additions:[]; params:((ptype:pt_coproc; offset:8), (ptype:pt_crreg; offset:12),(ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_single), (ptype:pt_imm; offset:12; maxval:$ff));                                                                    mask: %11111111111111110000000000000000; value: %11111100101111110000000000000000), //unindexed (p=0, w=0, U=1)

    (mnemonic:'MCRR2'; additions:[]; params:((ptype:pt_coproc; offset:8), (ptype:pt_imm; offset:4; maxval:$f),(ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_crreg; offset:0));                                                                                                                mask: %11111111111100000000000000000000; value: %11111100010000000000000000000000),
    (mnemonic:'MRRC2';  additions:[]; params:((ptype:pt_coproc; offset:8), (ptype:pt_imm; offset:4; maxval:$f),(ptype:pt_rreg; offset:12),(ptype:pt_rreg; offset:16),(ptype:pt_crreg; offset:0));                                                                                                               mask: %11111111111100000000000000000000; value: %11111100010100000000000000000000),
    (mnemonic:'CDP2'; additions:[]; params:((ptype:pt_coproc; offset:8), (ptype:pt_imm; offset:4; maxval:$f),(ptype:pt_crreg; offset:12),(ptype:pt_crreg; offset:16),(ptype:pt_crreg; offset:0),(ptype:pt_imm; offset:5; maxval:7));                                                                            mask: %11111111000000000000000000010000; value: %11111110000000000000000000000000),
    (mnemonic:'MCR2'; additions:[]; params:((ptype:pt_coproc; offset:8), (ptype:pt_imm; offset:4; maxval:7),(ptype:pt_crreg; offset:12),(ptype:pt_crreg; offset:16),(ptype:pt_crreg; offset:0),(ptype:pt_imm; offset:5; maxval:7));                                                                             mask: %11111111000100000000000000010000; value: %11111110000000000000000000010000),

    (mnemonic:'MRC2'; additions:[]; params:((ptype:pt_coproc; offset:8), (ptype:pt_imm; offset:4; maxval:7),(ptype:pt_crreg; offset:12),(ptype:pt_crreg; offset:16),(ptype:pt_crreg; offset:0),(ptype:pt_imm; offset:5; maxval:7));                                                                             mask: %11111111000100000000000000010000; value: %11111110000100000000000000010000)
  );

  ArmInstructionsMemoryHintsAdvancedSIMDAndMisc: array of TOpcode=(
    (mnemonic:'CPS';  additions:[];  params:((ptype:pt_imm; offset:0; maxval:$1f));              mask: %11111111111100011111111000100000; value: %11110001000000000000000000000000),
    (mnemonic:'SETEND'; additions:[]; params:((ptype:pt_imm; offset:0; maxval:$1f));             mask: %11111111111111111111110111111111; value: %11110001000000010000000000000000),
    (mnemonic:'PLI'; additions:[]; params:((ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_signed_imm12; offset:0; maxval:$fff; extra:23; optional:false; defvalue:0; index: ind_stop));  mask: %11111111011100001111000000000000; value: %11110100010100001111000000000000),
    (mnemonic:'PLD'; additions:[]; params:((ptype:pt_label; offset:0; maxval:$fff; extra:23));   mask: %11111111011111111111000000000000; value: %11110101010111111111000000000000),
    (mnemonic:'PLD'; additions:[]; params:((ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_signed_imm12; offset:0; maxval:$fff; extra:23; optional:false; defvalue:0; index: ind_stop));  mask: %11111111001100001111000000000000; value: %11110101000100001111000000000000),
    (mnemonic:'PLDW';additions:[]; params:((ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_signed_imm12; offset:0; maxval:$fff; extra:23; optional:false; defvalue:0; index: ind_stop));  mask: %11111111001100001111000000000000; value: %11110101010100001111000000000000),

    (mnemonic:'CLREX';additions:[]; params:();  mask: %11111111111111111111111111111111; value: %11110101011111111111000000011111),

    (mnemonic:'DSB';  additions:[];  params:((ptype:pt_imm; offset:0; maxval:$f));               mask: %11111111111111111111111111110000; value: %11110101011111111111000001000000),
    (mnemonic:'DMB';  additions:[];  params:((ptype:pt_imm; offset:0; maxval:$f));               mask: %11111111111111111111111111110000; value: %11110101011111111111000001010000),
    (mnemonic:'ISB';  additions:[];  params:((ptype:pt_imm; offset:0; maxval:$f));               mask: %11111111111111111111111111110000; value: %11110101011111111111000001100000),

    (mnemonic:'PLI'; additions:[]; params:((ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_signedrreg; offset:0;  maxval:15; extra:23; optional:false; defvalue:0; index: ind_index), (ptype:pt_shift5; offset:5; maxval: 63; extra:23; optional:false; defvalue:0; index: ind_stop));  mask: %11111111011100001111000000010000; value: %11110110010100001111000000000000),
    (mnemonic:'PLD'; additions:[]; params:((ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_signedrreg; offset:0;  maxval:15; extra:23; optional:false; defvalue:0; index: ind_index), (ptype:pt_shift5; offset:5; maxval: 63; extra:23; optional:false; defvalue:0; index: ind_stop));  mask: %11111111011100001111000000010000; value: %11110111000100001111000000000000),
    (mnemonic:'PLDW';additions:[]; params:((ptype:pt_rreg; offset:16;  maxval:15; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_signedrreg; offset:0;  maxval:15; extra:23; optional:false; defvalue:0; index: ind_index), (ptype:pt_shift5; offset:5; maxval: 63; extra:23; optional:false; defvalue:0; index: ind_stop));  mask: %11111111011100001111000000010000; value: %11110111010100001111000000000000)


  );

  ArmInstructionsDataProcessingAndMisc: array of TOpcode=(
    (mnemonic:'MOVW';  additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_imm16_12_4; offset:16; maxval: $ffff; extra:16)); mask: %00001111111100000000000000000000; value: %00000011000000000000000000000000),
    (mnemonic:'MOVT'; additions:[opa_cond]; params:((ptype:pt_rreg; offset:12),(ptype:pt_imm16_12_4; offset:16; maxval: $ffff; extra:16));  mask: %00001111111100000000000000000000; value: %00000011010000000000000000000000)

  );

  ArmInstructionsAdvancedSIMDDataProcessing: array of TOpcode=();
  ArmInstructionsAdvancedSIMDElementOrStructureLoadStore: array of TOpcode=();

  ArmInstructions8_16_and_32_bit_TransferBetweenARMCoreAndExtensionRegisters: array of TOpcode=();
  ArmInstructionsFloatingPointDataProcessing: array of TOpcode=();
  ArmInstructions64BitTransfersBetweenARMCoreAndExtensionRegisters: array of TOpcode=();
  ArmInstructionsExtensionRegisterLoadStoreInstruction: array of TOpcode=();






//--------------------------- instruction groups----------------------------//

  ArmGroupMemoryHintsAdvancedSIMDAndMisc:  array of TInstructionGroup=(
    (mask:%11111111000100000000000000000000; value: %11110111000100000000000000000000; list: @ArmInstructionsAdvancedSIMDElementOrStructureLoadStore; listType: igpInstructions; name:'ArmInstructionsAdvancedSIMDElementOrStructureLoadStore'),
    (mask:%11111110000000000000000000000000; value: %11110010000000000000000000000000; list: @ArmInstructionsAdvancedSIMDDataProcessing; listType: igpInstructions; name:'ArmInstructionsAdvancedSIMDDataProcessing'),
    (mask:%00000000000000000000000000000000; value: %00000000000000000000000000000000; list: @ArmInstructionsMemoryHintsAdvancedSIMDAndMisc; listType: igpInstructions; name:'ArmInstructionsUnconditionalInstructions')
  );

  ArmGroupUnconditionalInstructions: array of TInstructionGroup=(
    (mask:%11111000000000000000000000000000; value: %11110000000000000000000000000000; list: @ArmGroupMemoryHintsAdvancedSIMDAndMisc; listType: igpGroup; name:'ArmGroupMemoryHintsAdvancedSIMDAndMisc'),
    (mask:%00000000000000000000000000000000; value: %00000000000000000000000000000000; list: @ArmInstructionsUnconditionalInstructions; listType: igpInstructions; name:'ArmInstructionsUnconditionalInstructions')
  );

  ArmGroupDataProcessingAndMisc: array of TInstructionGroup=(
    (mask:%00001111100100000000000000000000; value: %00000011000000000000000000000000; list: @ArmInstructionsDataProcessingAndMisc; listType: igpInstructions; name:'ArmInstructionsDataProcessingAndMisc'),

    (mask:%00001111100100000000000011110000; value: %00000001000000000000000001010000; list: @ArmInstructionsSaturatingAddonAndSubtraction; listType: igpInstructions; name:'ArmInstructionsSaturatingAddonAndSubtraction'),
    (mask:%00001111100100000000000010000000; value: %00000001000000000000000000000000; list: @ArmInstructionsMiscellaneousInstuctions; listType: igpInstructions; name:'ArmInstructionsMiscellaneousInstuctions'),
    (mask:%00001111100100000000000010010000; value: %00000001000000000000000010000000; list: @ArmInstructionsHalfWordMultiplyAndMultiplyAccumulate; listType: igpInstructions; name:'ArmInstructionsHalfWordMultiplyAndMultiplyAccumulate'),



    (mask:%00001111000000000000000011110000; value: %00000000000000000000000010010000; list: @ArmInstructionsMultiplyAndMultiplyAccumulate; listType: igpInstructions; name:'ArmInstructionsMultiplyAndMultiplyAccumulate'),
    (mask:%00001111000000000000000011110000; value: %00000001000000000000000010010000; list: @ArmInstructionsSynchronizationPrimitives; listType: igpInstructions; name:'ArmInstructionsSynchronizationPrimitives'),


    (mask:%00001110000000000000000010010000; value: %00000000000000000000000010010000; list: @ArmInstructionsExtraLoadStoreInstructions; listType: igpInstructions; name:'ArmInstructionsExtraLoadStoreInstructions'),
    (mask:%00001111001000000000000011110000; value: %00000000001000000000000010110000; list: @ArmInstructionsExtraLoadStoreInstructionsUnprivileged; listType: igpInstructions; name:'ArmInstructionsExtraLoadStoreInstructionsUnprivileged'),
    (mask:%00001111001100000000000011010000; value: %00000000001100000000000011010000; list: @ArmInstructionsExtraLoadStoreInstructionsUnprivileged; listType: igpInstructions; name:'ArmInstructionsExtraLoadStoreInstructionsUnprivileged2'),

    (mask:%00001111001000000000000011110000; value: %00000001000000000000000000000000; list: @ArmInstructionsExtraLoadStoreInstructions; listType: igpInstructions; name:'ArmInstructionsExtraLoadStoreInstructions'),
    (mask:%00001111001000000000000011010000; value: %00000001000000000000000000000000; list: @ArmInstructionsExtraLoadStoreInstructions; listType: igpInstructions; name:'ArmInstructionsExtraLoadStoreInstructions2'),

    (mask:%00001110000000000000000010010000; value: %00000000000000000000000000010000; list: @ArmInstructionsDataProcessingRegisterShiftedRegister; listType: igpInstructions; name:'ArmInstructionsDataProcessingRegisterShiftedRegister'),
    (mask:%00001110000000000000000000010000; value: %00000000000000000000000000000000; list: @ArmInstructionsDataProcessingRegister; listType: igpInstructions; name:'ArmInstructionsDataProcessingRegister'),
    (mask:%00001110000000000000000000000000; value: %00000010000000000000000000000000; list: @ArmInstructionsDataProcessingImmediate; listType: igpInstructions; name:'ArmInstructionsDataProcessingImmediate'), //not 10xx0


    (mask:%00001111101100000000000000000000; value: %00000011001000000000000000000000; list: @ArmInstructionsMSRImmediateAndHints; listType: igpInstructions; name:'ArmInstructionsMSRImmediateAndHints')




  );




  ArmGroupMediaInstructions: array of TInstructionGroup=(
    (mask:%00001111110000000000000000010000; value: %00000110000000000000000000010000; list: @ArmInstructionsParallelAdditionAndSubtractionSigned; listType: igpInstructions; name:'ArmInstructionsParallelAdditionAndSubtractionSigned'),
    (mask:%00001111110000000000000000010000; value: %00000110010000000000000000010000; list: @ArmInstructionsParallelAdditionAndSubtractionUnsigned; listType: igpInstructions; name:'ArmInstructionsParallelAdditionAndSubtractionUnsigned'),
    (mask:%00001111100000000000000000010000; value: %00000110100000000000000000010000; list: @ArmInstructionsPackingUnpackingSaturationAndReversal; listType: igpInstructions; name:'ArmInstructionsPackingUnpackingSaturationAndReversal'),
    (mask:%00001111100000000000000000010000; value: %00000111000000000000000000010000; list: @ArmInstructionsSignedMultiplySignedAndUnsignedDivide; listType: igpInstructions; name:'ArmInstructionsSignedMultiplySignedAndUnsignedDivide'),
    (mask:%00000000000000000000000000000000; value: %00000000000000000000000000000000; list: @ArmInstructionsGroupMediaInstructions; listType: igpInstructions; name:'ArmInstructionsGroupMediaInstructions') //everything else in this group
  );




  ArmGroupCoprocessorInstructionsAndSupervisorCall: array of TInstructionGroup=(
    (mask:%00001111000000000000111000010000; value: %00001110000000000000101000010000; list: @ArmInstructions8_16_and_32_bit_TransferBetweenARMCoreAndExtensionRegisters; listType: igpInstructions; name: 'ArmInstructions8_16_and_32_bit_TransferBetweenARMCoreAndExtensionRegisters'),
    (mask:%00001111000000000000111000010000; value: %00001110000000000000101000000000; list: @ArmInstructionsFloatingPointDataProcessing; listType: igpInstructions; name: 'ArmInstructionsFloatingPointDataProcessing'),

    (mask:%00001111111000000000111000000000; value: %00001100000000000000101000000000; list: @ArmInstructions64BitTransfersBetweenARMCoreAndExtensionRegisters; listType: igpInstructions; name: 'ArmInstructions64BitTransfersBetweenARMCoreAndExtensionRegisters'),
    (mask:%00001110000000000000111000000000; value: %00001100000000000000101000000000; list: @ArmInstructionsExtensionRegisterLoadStoreInstruction; listType: igpInstructions; name: 'ArmInstructionsExtensionRegisterLoadStoreInstruction'),
    (mask:%00000000000000000000000000000000; value: %00000000000000000000000000000000; list: @ArmInstructionsGroupCoprocessorInstructionsAndSupervisorCall; listType: igpInstructions; name:'ArmInstructionsGroupCoprocessorInstructionsAndSupervisorCall') //everything else in this group
  );

 //
  ArmGroupBase: array of TInstructionGroup=(
    (mask:%11110000000000000000000000000000; value: %11110000000000000000000000000000; list: @ArmGroupUnconditionalInstructions; listType: igpGroup; name: 'ArmGroupUnconditionalInstructions'),
    (mask:%00001100000000000000000000000000; value: %00000000000000000000000000000000; list: @ArmGroupDataProcessingAndMisc; listType: igpGroup; name: 'ArmGroupDataProcessingAndMisc'),
    (mask:%00001110000000000000000000000000; value: %00000100000000000000000000000000; list: @ArmInstructionsLoadStoreWordAndUnsignedByte; listType: igpInstructions; name: 'ArmInstructionsLoadStoreWordAndUnsignedByte'),
    (mask:%00001110000000000000000000010000; value: %00000110000000000000000000000000; list: @ArmInstructionsLoadStoreWordAndUnsignedByte; listType: igpInstructions; name: 'ArmInstructionsLoadStoreWordAndUnsignedByte 2'),
    (mask:%00001100000000000000000000010000; value: %00000110000000000000000000010000; list: @ArmGroupMediaInstructions; listType: igpGroup; name: 'ArmGroupMediaInstructions'),
    (mask:%00001100000000000000000000000000; value: %00001000000000000000000000000000; list: @ArmInstructionsBranchBranchWithLinkAndBlockDataTransfer; listType: igpInstructions; name: 'ArmInstructionsBranchBranchWithLinkAndBlockDataTransfer'),
    (mask:%00001100000000000000000000000000; value: %00001100000000000000000000000000; list: @ArmGroupCoprocessorInstructionsAndSupervisorCall; listType: igpGroup; name: 'ArmGroupCoprocessorInstructionsAndSupervisorCall')



  );

var
  ArmInstructionsAssemblerList: TStringHashList;

  tlbilist: TStringHashList;


{$ifdef armdev}
procedure DebugOutputOpcode(opcode: POpcode);
var
  s: string;
  i: integer;
  ti: PTypeInfo;
  tn: string;

  insideIndex: boolean;
begin
  s:='';
  insideIndex:=false;
  for i:=0 to length(opcode^.params)-1 do
  begin
    tn:='';
    if (insideIndex=false) and (opcode^.params[i].index<>ind_no) then
    begin
      insideindex:=true;
      tn:=tn+'[';
    end;

    ti:=TypeInfo(TArm32ParameterType);
    tn:=tn+GetEnumName(ti, integer(opcode^.params[i].ptype));
    {
    if (insideIndex) and (opcode^.params[i].index in [ind_no, ind_stop, ind_stopexp, ind_single, ind_singleexp]) then
    begin
      insideindex:=false;
      tn:=tn+']';

      if opcode^.params[i].index in [ind_stopexp, ind_singleexp] then
        tn:=tn+'!';
    end; }

    if i>0 then s:=s+', ';
    s:=s+tn;
  end;


  if insideindex then
    s:=s+']';

  outputdebugstring(pchar(opcode^.mnemonic+'('+s+')'));
end;
{$endif}



function SignExtend(value: qword; mostSignificantBit: integer): qword; inline;
{
Signextends a given offset. mostSignificant bit defines what bit determines if it should be sign extended or not
}
begin
  if (value and (1 shl mostSignificantBit))<>0 then //needs to be sign extended
      value:=value or ((qword($ffffffffffffffff) shl mostSignificantBit));

  result:=value;
end;

function TArm32Instructionset.GetIMM2Value(mask: dword): dword;
//scan the bitmask for 1's and convert them a value
var
  i: integer;
  startbit: integer;
  bitcount: integer;

begin
  //scan for the first bit
  result:=0;
  startbit:=-1;
  for i:=0 to 31 do
  begin
    if (mask and (1 shl i)) <> 0 then
    begin
      //found the start
      startbit:=i;
      break;
    end;
  end;

  if startbit=-1 then exit;

  bitcount:=0;
  for i:=startbit to 31 do
  begin
    if (mask and (1 shl i))<>0 then
    begin
      if (opcode and (1 shl i))<>0 then
        result:=result or (1 shl bitcount);

      inc(bitcount);
    end;
  end;
end;

function TArm32Instructionset.GetIMM2_8Value(mask: dword): qword;
//scan the bitmask for 1's and convert them a value (there are 8 bits in this mask)
var
  v: byte;
  r: qword;
  i,j: integer;
begin
  result:=0;
  v:=GetIMM2Value(mask);
  for i:=0 to 7 do
  begin
    for j:=i*8 to (i+1)*8-1 do
      result:=result or (((v shr i) and 1) shl j);
  end;


end;

function highestbit(v: dword): integer;
var i: integer;
begin
  case v of
        $ff: exit(8);
       $1ff: exit(9);
      $3fff: exit(14);
      $7fff: exit(15);
      $ffff: exit(16);
     $1ffff: exit(17);
     $3ffff: exit(18);
     $7ffff: exit(19);
    $ffffff: exit(23);

    $7ffffff: exit(26);
    else
    begin
      result:=0;
      for i:=31 downto 0 do
        if (v and (1 shl i))<>0 then exit(i);
    end;
  end;
end;

function ones(len: integer): qword;
begin
  result:=qword(qword(1) shl len)-1;
end;

function zeroextend(v: qword; bitlen: integer): qword;
begin
  result:=v and ones(bitlen);
end;

function ror(v: qword; esize: integer; r: integer): qword;
var a,b: qword;
begin
  a:=v shl (esize-r) and ones(esize);
  b:=v shr r and ones(esize);

  result:=(a or b) and ones(esize);

end;

function replicate(v: qword; sourceSize: integer; destinationSize: integer): qword;
var
  repval: qword;
  times: integer;
  i: integer;
  r: qword;
begin
  if sourcesize=0 then exit(0);

  repval:=v and zeroextend(v, sourcesize);

  times:=destinationsize div sourcesize;
  result:=0;
  for i:=1 to times do
    result:=(result shl sourcesize) or repval;

  result:=result or repval;
end;

function armbitmask(v: qword; datasize: integer): qword;
var
  d: bitpacked record
    imms: 0..$3f;
    immr: 0..$3f;
    immN: 0..1;
  end absolute v;

  len,len2: integer;
  levels,level2: integer;

  s: integer;
  r: integer;
  diff: integer;
  esize: integer;
  welem: qword;
  telem: qword;

  _d: integer;

  scratch: bitpacked record
    v: 0..$3f;
    tmask_and, wmask_and: 0..$3f;
    tmask_or, wmask_or: 0..$3f;
    levels: 0..$3f;
  end;

  tmask: qword;
  wmask: qword;
begin
  result:=0;
  scratch.v:=d.imms;
  scratch.v:=not scratch.v;


  len:=highestbit((d.immn shl 6) or (scratch.v));
  if len<1 then exit(0);

  scratch.v:=ones(len);
  levels:=scratch.v;

  s:=d.imms and levels;
  r:=d.immr and levels;
  diff:=s-r;

  esize:=1 shl len;

  _d:=diff and (ones(len-1));

  welem:=zeroextend(ones(s+1), esize);
  telem:=zeroextend(ones(_d+1), esize);

  wmask:=replicate(ror(welem, esize, r), esize, datasize);
  tmask:=replicate(telem, esize, datasize);

  exit(wmask);
end;

function bhsd(encoding2: integer): string;
begin
  case encoding2 and 3 of
    %00: exit('B');
    %01: exit('H');
    %10: exit('S');
    %11: exit('D');
  end;
end;

function bhsd2(encoding2: integer): string;
begin
  case encoding2 and 3 of
    %01: exit('B');
    %10: exit('H');
    %11: exit('S');
  end;
end;

function getVectorSize2FromString(s: string): integer;
begin
  result:=-1;
  case s of
    '4H': exit(%000);
    '2S': exit(%001);
    '1D': exit(%010);
    '8H': exit(%100);
    '4S': exit(%101);
    '2D': exit(%110);
  end
end;

function getVectorSizeFromString(s: string):integer;
begin
  result:=-1;
  case s of
    '8B': exit(%000);
    '4H': exit(%001);
    '2S': exit(%010);
    '1D': exit(%011);
    '16B': exit(%100);
    '8H': exit(%101);
    '4S': exit(%110);
    '2D': exit(%111);
  end;
end;

function getVectorSizeString(encoding3: integer): string;
begin
  case encoding3 of
    %000: result:='8B';
    %001: result:='4H';
    %010: result:='2S';
    %011: result:='1D';
    %100: result:='16B';
    %101: result:='8H';
    %110: result:='4S';
    %111: result:='2D';
    else result:='invalid';
  end
end;

function getVectorSizeString2(encoding3: integer): string;
begin
  case encoding3 of
    %000: result:='4H';
    %001: result:='2S';
    %010: result:='1D';
    %100: result:='8H';
    %101: result:='4S';
    %110: result:='2D';
    else result:='invalid';
  end
end;

function getVectorRegisterString(regnr: integer; encoding3: integer): string;
begin
  result:='V'+inttostr(regnr)+'.'+getVectorSizeString(encoding3);
end;

function getVectorRegisterListString(startreg: integer; encoding3: integer; len: integer):string;
var s: string;
begin
  s:=getVectorSizeString(encoding3);
  result:='{V'+inttostr(startreg)+'.'+s;
  if len>1 then
    result:=result+'-V'+inttostr((startreg+len-1) mod 32)+'.'+s;

  result:=result+'}';
end;

function floatToFP8(f: single): byte;
var
  fi: single;
  fib: bitpacked record
    frac: 0..$3FFFFF;
    exp: 0..255;
    sign: 0..1;
  end absolute fi;

  rb: byte;
  r: bitpacked record
    frac: 0..$f;
    exp: 0..7;
    sign: 0..1;
  end absolute rb;

begin
  fi:=f;

  rb:=0;
  r.sign:=fib.sign;
  r.exp:=fib.Exp;
  r.frac:=fib.frac shr 18;

  result:=rb;

end;

function fp8tofloat(v: byte): single;
var
  n: integer;
  e: integer;
  f: integer;
  sign: integer;
  exp: integer;
  frac: integer;

  r: single;
  fr: bitpacked record
    sign: 1..1;
    exp: 0..255;
    frac: 0..$3FFFFF;
  end absolute r;

begin
  r:=0;
  n:=32;
  e:=8;
  f:=n-e-1;

  fr.sign:=(v shr 7) and 1;
  fr.exp:=(not((v shr 6) and 1) shr 7) or (replicate((v shr 6) and 1, 1,e-3) shl 2) or ((v shr 4) and 3);
  fr.frac:=(v and $f) shl 18;

  result:=r;

end;

function shiftTypeToInteger(s: string): integer;
begin
  if s='LSL' then exit(0);
  if s='LSR' then exit(1);
  if s='ASR' then exit(2);
  if s='ROR' then exit(3);
  result:=-1;
end;

function getShiftTypeString(t: integer): string;
begin
  case t and $3 of
    0: exit('LSL');
    1: exit('LSR');
    2: exit('ASR');
    3: exit('ROR');
  end;
end;

function ARMRegisterStringToInteger(s: string):integer;
begin
  result:=-1;
  if length(s)<2 then exit;
  s:=uppercase(s);

  try
    if s[1]='R' then exit(strtoint(s.Substring(1)));
  except
    exit;
  end;

  if s='FP' then exit(11);
  if s='IP' then exit(12);
  if s='SP' then exit(13);
  if s='LR' then exit(14);
  if s='PC' then exit(15);
end;

function TArm32Instructionset.ParseParametersForDisassembler(plist: TAParametersList): boolean;
var
  i,j: integer;
  v,v2,v3: dword;
  qv,qv2: qword;

  p,s: string;

  insideIndex: boolean;
  first: boolean;
begin
  result:=true;
  insideIndex:=false;



  for i:=0 to length(plist)-1 do
  begin

    p:='';
    case plist[i].ptype of
      pt_rreg, pt_rreg_ex, pt_signedrreg:
      begin
        v:=(opcode shr plist[i].offset) and 15;
        p:=ArmRegisters[v];

        case plist[i].ptype of
          pt_rreg_ex: p:=p+'!';
          pt_signedrreg: if (opcode and (1 shl plist[i].extra))=0 then p:='-'+p;
        end;
      end;

      pt_coproc:
      begin
        v:=(opcode shr plist[i].offset) and 15;
        p:=v.ToString;
      end;

      pt_crreg:
      begin
        v:=(opcode shr plist[i].offset) and 15;
        p:='CR'+v.ToString;
      end;

      pt_signed_imm8_shl2:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        v:=v shl 2;
        p:=v.ToHexString(1);
        if (opcode and (1 shl plist[i].extra))=0 then
          p:='-'+p;

        p:='#'+p;
      end;

      pt_signed_imm12:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        p:=v.ToHexString(1);
        if (opcode and (1 shl plist[i].extra))=0 then
          p:='-'+p;

        p:='#'+p;
      end;


      pt_shift5: //shift and type are next to eachother. Offset is the type offset
      begin
        v:=(opcode shr plist[i].offset) and  $7ff;
        v2:=v and $3;
        v:=v shr 2;

        case v2 of //type
          0: if v=0 then continue else p:='LSL #'+inttohex(v,1);
          1: if v=0 then p:='LSR #20' else p:='LSR #'+inttohex(v,1);
          2: if v=0 then p:='ASR #20' else p:='ASR #'+inttohex(v,1);
          3: if v=0 then p:='RRX' else p:='ROR #'+inttohex(v,1);
        end;
      end;

      pt_shifttype:
      begin
        v:=(opcode shr plist[i].offset) and $3;
        v2:=(opcode shr plist[i].extra) and $f;
        p:=getShiftTypeString(v)+' '+ArmRegisters[v2];
      end;

      pt_const:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        v2:=v and $ff; //unrotated value
        v3:=(v shr 8);

        //p:=v2.ToHexString+' ROR '+v3.ToHexString;

        v:=RorDWord(v2,v3*2);
        p:='#'+v.ToHexString(1);
      end;

      pt_signed_label:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        v:=SignExtend(v, highestbit(plist[i].maxval));
        v:=v shl 2;

        if plist[i].extra<>0 then
          v:=v or 2;

        v:=v+address+8; //accept it...

        v2:=v; //needed when compiling in -O1 else v has some extra bits set

        p:=IntToHex(v,8);

//        p:=v.ToHexString(8);
      end;

      pt_label:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        v2:=v and $ff; //unrotated value
        v3:=(v shr 8);
        v:=RorDWord(v2,v3*2);

        if ((opcode shr plist[i].extra) and 1)=0 then
          v:=dword(address)-v
        else
          v:=dword(address)+v;

        p:=v.ToHexString(8);
      end;

      pt_neglabel:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        v2:=v and $ff; //unrotated value
        v3:=(v shr 8);
        v:=RorDWord(v2,v3*2);
        v:=address-v+8;
        p:=v.ToHexString(8);
      end;

      pt_poslabel:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        v2:=v and $ff; //unrotated value
        v3:=(v shr 8);
        v:=RorDWord(v2,v3*2);
        v:=address+v+8;
        p:=v.ToHexString(8);
      end;

      pt_imm8LH, pt_imm8LH_label:
      begin

        v:=(opcode shr pbyte(@plist[i].offset)[0]) and $f;
        v2:=(opcode shr pbyte(@plist[i].offset)[1]) and $f;
        v:=v or (v2 shl 4);

        if plist[i].ptype=pt_imm8LH_label then
        begin
          if ((opcode shr plist[i].extra) and 1)=0 then
            v:=address-v+8
          else
            v:=address+v+8;

          p:=v.ToHexString(1);
        end
        else
        begin
          p:=v.ToHexString(1);

          if ((opcode shr plist[i].extra) and 1)=0 then
            p:='-'+p;
        end;
      end;

      pt_imm:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        p:=v.ToHexString(1);
      end;

      pt_imm16_4_12:
      begin
        v:=(opcode shr plist[i].offset) and $f;
        v2:=(opcode shr plist[i].extra) and $fff;

        v:=(v2 shl 4) or v;
        p:=v.ToHexString(1);
      end;

      pt_imm16_12_4:
      begin
        v:=(opcode shr plist[i].offset) and $fff;
        v2:=(opcode shr plist[i].extra) and $f;

        v:=(v2 shl 12) or v;
        p:=v.ToHexString(1);
      end;


      pt_apsr: p:='apsr';
      pt_spsr: p:='spsr';


      pt_ap_reg:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;

        if plist[i].maxval=3 then
        begin
          case v of
            1: p:='apsr_g';
            2: p:='apsr_nzcvq';
            3: p:='apsr_nzcvqg';
          end;
        end;
      end;

      pt_cpsr_reg:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        p:='cpsr_'+inttohex(v,1);
      end;

      pt_spsr_reg:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        p:='spsr_'+inttohex(v,1);
      end;

      pt_bankedreg:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        v2:=(v shr 3) and 3;
        case v2 of
          0: p:='r'+inttostr(v and %11)+'_usr';
          1: p:='r'+inttostr(v and %11)+'_fiq';
          else
            p:='nyi'; //todo: implement these when people want it....

        end;
      end;

      pt_bankedreg_spsr:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        v:=v or (((opcode shr plist[i].extra) and 1) shl 4);
        case v of
          %01110: p:='SPSR_fiq';
          %10000: p:='SPSR_irq';
          %10010: p:='SPSR_svc';
          %10100: p:='SPSR_abt';
          %10110: p:='SPSR_und';
          %11100: p:='SPSR_mon';
          %11110: p:='SPSR_hyp';
          else exit(false);
        end;

      end;

      pt_rotatex8:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        case v of
          0: p:='';
          1: p:='ROR #8';
          2: p:='ROR #10';
          3: p:='ROR #18';
        end;
      end;

      pt_reglist16, pt_reglist16_x: //16 bit registerlist
      begin
        p:='{';
        first:=true;
        for j:=0 to 15 do
        begin
          if ((opcode shr plist[i].offset) and (1 shl j))<>0 then
          begin
            if not first then
              p:=p+', '
            else
              first:=false;

            p:=p+ArmRegisters[j];
          end;
        end;
        p:=p+'}';

        if plist[i].ptype=pt_reglist16_x then
          p:=p+'^';
      end;

      pt_reglist_reg:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        p:='{'+ArmRegisters[v]+'}';
      end;


    end;


    if i>0 then
      LastDisassembleData.parameters:=LastDisassembleData.parameters+', ';


    if (not insideIndex) and (plist[i].index in [ind_index, ind_single, ind_singleexp]) then
    begin
      LastDisassembleData.parameters:=LastDisassembleData.parameters+'[';
      insideindex:=true;
    end;

    LastDisassembleData.parameters:=LastDisassembleData.parameters+p;


    if insideindex and (plist[i].index in [ind_single, ind_singleexp, ind_stop, ind_stopexp, ind_index]) then
    begin

      if (plist[i].index<>ind_index) or (i=length(plist)-1) then
      begin
        LastDisassembleData.parameters:=LastDisassembleData.parameters+']';
        if plist[i].index in [ind_singleexp, ind_stopexp] then
          LastDisassembleData.parameters:=LastDisassembleData.parameters+'!';

        insideindex:=false;
      end;
    end;
  end;

end;

function TArm32Instructionset.ScanOpcodeList(const list: topcodearray): boolean;
var i: integer;
begin
  result:=false;
  for i:=0 to length(list)-1 do
  begin
    if ((opcode and list[i].mask)=list[i].value) and (list[i].use in [iuBoth, iuDisassembler]) then
    begin
      if list[i].alt<>nil then
      begin
        result:=ScanOpcodeList(list[i].alt^);
        if result then exit;
      end;

      {$ifdef armdev}
      DebugOutputOpcode(@list[i]);
      {$endif}

      LastDisassembleData.opcode:=list[i].mnemonic;

      if (opa_s20 in list[i].additions) and ((list[i].mask and $100000) = 0) and ((opcode and $100000)=$100000) then
        LastDisassembleData.opcode:=LastDisassembleData.opcode+LastDisassembleData.opcode+'S';

      if (opa_cond in list[i].additions) and ((list[i].mask and ($f shr 28)) = 0) then
        LastDisassembleData.opcode:=LastDisassembleData.opcode+ArmConditions[opcode shr 28];



      //parse the parameters
      if ParseParametersForDisassembler(list[i].params) then
        exit(true);
    end;
  end;
end;

function TArm32Instructionset.ScanGroupList(const list: TInstructionGroupArray):boolean;
var i: integer;
begin
  result:=false;
  for i:=0 to length(list)-1 do
  begin
    if (opcode and list[i].mask)=list[i].value then
    begin
      if list[i].listType=igpGroup then
        result:=ScanGroupList(PInstructionGroupArray(list[i].list)^)
      else
        result:=ScanOpcodeList(POpcodeArray(list[i].list)^);

      if result then //unlike arm64, ARM32 is a chaotic pile of doodoo with many overlaps
        exit;
    end;
  end;
end;



function TArm32Instructionset.disassemble(var DisassembleAddress: ptruint{$ifdef armdev}; _opcode: dword{$endif}): string;
var
  x: ptruint;
  i: integer;
begin
  InitARM32Support;

  address:=DisassembleAddress;
  x:=0;
  setlength(LastDisassembleData.Bytes,4);

  {$ifdef armdev}
  PDWORD(@LastDisassembleData.Bytes[0])^:=_opcode;
  opcode:=_opcode;
  x:=4;
  {$else}
  readprocessmemory(processhandle, pointer(address), @LastDisassembleData.Bytes[0], 4, x);
  opcode:=pdword(@LastDisassembleData.Bytes[0])^;
  {$endif}

  LastDisassembleData.opcode:='UNDEFINED';
  Lastdisassembledata.parameters:='';
  LastDisassembleData.address:=address;
  LastDisassembleData.SeperatorCount:=0;
  LastDisassembleData.prefix:='';
  LastDisassembleData.PrefixSize:=0;
  LastDisassembleData.opcode:='';
  LastDisassembleData.parameters:='';
  lastdisassembledata.isjump:=false;
  lastdisassembledata.iscall:=false;
  lastdisassembledata.isret:=false;
  lastdisassembledata.isconditionaljump:=false;
  lastdisassembledata.modrmValueType:=dvtNone;
  lastdisassembledata.parameterValueType:=dvtNone;
  lastdisassembledata.Disassembler:=dcArm;

  ScanGroupList(ArmGroupBase);


  result:=inttohex(LastDisassembleData.address,8);
  result:=result+' - ';
  if x>0 then
  begin
    for i:=0 to length(LastDisassembleData.bytes)-1 do
      result:=result+inttohex(LastDisassembleData.Bytes[i],2)+' ';
  end
  else
  begin
    for i:=0 to length(LastDisassembleData.bytes)-1 do
      result:=result+'?? ';
  end;

  result:=result+' - ';
  result:=result+LastDisassembleData.opcode;
  result:=result+' ';
  result:=result+LastDisassembleData.parameters;

  inc(DisassembleAddress,4);
end;




function TArm32Instructionset.GuessTypes(param: string): TArm32ParameterTypes;
var
  li: longint;
  i64: int64;
  i: integer;
  s: string;
  f: single;
  v: dword;
  isforcedSigned: boolean;
  hasExp: boolean;
begin
  //pt_creg?
  result:=[];
  hasExp:=false;

  param:=uppercase(trim(param));
  if length(param)=0 then exit;


  i:=ARMRegisterStringToInteger(param);
  if i<>-1 then
    result:=[pt_rreg, pt_signedrreg];

  if param[1] in ['+','-'] then
  begin
    param:=copy(param,2);
    isforcedSigned:=true;
  end;

  if TryStrToUInt('$'+param,v) then
    result:=result+[pt_const];

  v:=0;

  case param[1] of
    '{':
    begin
      if param.EndsWith('}') then
        result:=result+[pt_reglist16, pt_reglist_reg]
      else
      if param.endswith('}^') then
        result:=result+[pt_reglist16_x];
    end;

    'R': //Rxx /ROR/RRX
    begin
      s:=copy(param,2);
      if s.EndsWith('!') then
      begin
        s:=copy(s,1,length(s)-1);
        hasExp:=true;
      end;

      if TryStrToInt(s,li) then
      begin
        if (li>=0) and (li<=15) then
        begin
          if hasexp then
            result:=result+[pt_rreg_ex]
          else
            result:=result+[pt_rreg, pt_signedrreg];
          exit;
        end;
      end;

      if (param.Substring(0,3)='ROR') or (param.Substring(0,3)='RRX') then
        result:=result+[pt_shift5, pt_shifttype, pt_rotatex8];
    end;

    'L': //LSL,LSR
    begin
      if (param.Substring(0,3)='LSL') or (param.Substring(0,3)='LSR') then
        result:=result+[pt_shift5, pt_shifttype];

    end;

    'A': //ASR/APSR
    begin
      if (param.Substring(0,3)='ASR') then
        result:=result+[pt_shift5,pt_shifttype];

      if param.StartsWith('APSR_') then
        result:=result+[pt_ap_reg];
    end;

    'C': //CPSR
    begin
      if param.StartsWith('CPSR_') then
        result:=result+[pt_cpsr_reg];
    end;

    'S': //SPSR
    begin
      if param.StartsWith('SPSR_') then
        result:=result+[pt_spsr_reg];
    end;

    '#':
    begin
      if TryStrToUInt('$'+param.Substring(1),v) then
        result:=result+[pt_const];
    end;
  end;

  if pt_const in result then
  begin
    //could be a label (v is filled in)
    if v<address then
      result:=result+[pt_neglabel]
    else
      result:=result+[pt_poslabel];

    result:=result+[pt_label, pt_imm, pt_imm8LH, pt_imm8LH_label, pt_signed_imm12, pt_imm16_4_12, pt_imm16_12_4];
  end;

end;


procedure TArm32Instructionset.SetIMM2Value(mask: dword; v: dword);
//scan the bitmask for 1's and convert them a value
var
  i: integer;
  bitcount: integer;

begin
  //scan for the first bit
  bitcount:=0;
  for i:=0 to 31 do
  begin
    if (mask and (1 shl i)) <> 0 then
    begin
      opcode:=opcode or (v shl bitcount);
      inc(bitcount);
    end;
  end;

end;

function TArm32Instructionset.ParseParameterForAssembler(param:TAParameters; paramstr: string): boolean;
var
  s,s2,s3: string;
  qv,qv2: qword;
  v,v2,v3: dword;
  sv: integer;

  i,j,k: integer;
  b: boolean;

  reglist: array of string;

begin
  result:=false;
  if paramstr='' then exit;

  case param.ptype of
    pt_rreg:
    begin
      j:=ARMRegisterStringToInteger(paramstr);
      if j=-1 then exit;


      opcode:=opcode or (j shl param.offset);
    end;


    pt_rreg_ex:
    begin
      if paramstr.EndsWith('!')=false then exit;

      paramstr:=copy(paramstr,1,length(paramstr)-1);
      j:=ARMRegisterStringToInteger(paramstr);
      if j=-1 then exit;

      opcode:=opcode or (j shl param.offset);
    end;

    pt_signedrreg:
    begin
      v:=1;
      if paramstr[1] in ['-','+'] then
      begin
        if paramstr[1]='-' then
          v:=0; //subtract
        paramstr:=copy(paramstr,2);
      end;

      j:=ARMRegisterStringToInteger(paramstr);
      if j=-1 then exit;

      opcode:=opcode or (j shl param.offset);

      opcode:=opcode or (v shl param.extra);
    end;

    pt_shift5:
    begin
      if paramstr.Substring(0,3)='RRX' then opcode:=opcode or (3 shl param.offset) else
      begin
        i:=paramstr.IndexOf('#');
        if i=-1 then exit;

        s:=trim(paramstr.Substring(0,i-1));
        s2:=paramstr.Substring(i+1);

        case s of
          'LSL': ;
          'LSR': opcode:=opcode or (1 shl (param.offset+2));
          'ASR': opcode:=opcode or (2 shl (param.offset+2));
          'ROR': opcode:=opcode or (3 shl (param.offset+2));
          else exit; //invalid
        end;

        i:=strtoint('$'+s2);
        i:=i and 63;

        opcode:=opcode or (i shl param.offset);
      end;
    end;

    pt_shifttype:
    begin
      i:=uppercase(paramstr).IndexOf(' R');
      if i=-1 then exit;

      s:=trim(paramstr.Substring(0,i-1));
      s2:=trim(paramstr.Substring(i+1));

      i:=shiftTypeToInteger(s);
      if i=-1 then exit;



      j:=ARMRegisterStringToInteger(s2);
      if j=-1 then exit;

      opcode:=opcode or (i shl param.offset);
      opcode:=opcode or (j shl param.extra);
    end;

    pt_signed_imm12:
    begin
      v:=1;

      if trim(paramstr).StartsWith('#') then
        paramstr:=trim(paramstr.Substring(1));

      if paramstr.StartsWith('-') then
      begin
        v:=0;
        paramstr:=trim(copy(paramstr,2));
      end;

      i:=strtoint('$'+paramstr);
      if i>param.maxval then exit;
      opcode:=opcode or (i shl param.offset);
      opcode:=opcode or (v shl param.extra);
    end;

    pt_imm:
    begin
      if trim(paramstr).StartsWith('#') then
        paramstr:=paramstr.Substring(1);

      v:=StrToInt('$'+paramstr);
      if v>param.maxval then exit;

      opcode:=opcode or (v shl param.offset);
    end;

    pt_const:
    begin
      if trim(paramstr).StartsWith('#') then
        paramstr:=paramstr.Substring(1);

      v:=StrToInt('$'+paramstr);
      j:=-1;
      v3:=$ffffffff;
      for i:=0 to 15 do //try 16 combinations
      begin
        v2:=RolDWord(v,i*2);
        if (v2<=255) and (v2<v3) then
        begin
          j:=i;
          v3:=v2;
        end;
      end;

      if j=-1 then exit; //not valid

      opcode:=opcode or (v3 shl param.offset);
      opcode:=opcode or (j shl (param.offset+8));
    end;

    pt_label:
    begin
      if trim(paramstr).StartsWith('#') then
        paramstr:=paramstr.Substring(1);

      v:=StrToInt('$'+paramstr);
      if v>address then //positive
      begin
        opcode:=opcode or (1 shl param.extra); //mark as positive
        v:=v-address;
      end
      else
        v:=address-v; //negative

      j:=-1;
      v3:=$ffffffff;
      for i:=0 to 15 do //try 16 combinations
      begin
        v2:=RolDWord(v,i*2);
        if (v2<=255) and (v2<v3) then
        begin
          j:=i;
          v3:=v2;
        end;
      end;

      if j=-1 then exit; //not valid

      opcode:=opcode or (v3 shl param.offset);
      opcode:=opcode or (j shl (param.offset+8));
    end;


    pt_neglabel:
    begin
      if trim(paramstr).StartsWith('#') then
        paramstr:=paramstr.Substring(1);

      v:=StrToInt('$'+paramstr);
      v:=address-v;
      j:=-1;
      v3:=$ffffffff;
      for i:=0 to 15 do //try 16 combinations
      begin
        v2:=RolDWord(v,i*2);
        if (v2<=255) and (v2<v3) then
        begin
          j:=i;
          v3:=v2;
        end;
      end;

      if j=-1 then exit; //not valid

      opcode:=opcode or (v3 shl param.offset);
      opcode:=opcode or (j shl (param.offset+8));
    end;

    pt_poslabel:
    begin
      if trim(paramstr).StartsWith('#') then
        paramstr:=paramstr.Substring(1);

      v:=StrToInt('$'+paramstr);
      v:=v-address;
      j:=-1;
      v3:=$ffffffff;
      for i:=0 to 15 do //try 16 combinations
      begin
        v2:=RolDWord(v,i*2);
        if (v2<=255) and (v2<v3) then
        begin
          j:=i;
          v3:=v2;
        end;
      end;

      if j=-1 then exit; //not valid

      opcode:=opcode or (v3 shl param.offset);
      opcode:=opcode or (j shl (param.offset+8));
    end;

    pt_imm8LH:
    begin
      if trim(paramstr).StartsWith('#') then
        paramstr:=trim(paramstr).Substring(1);

      if paramstr.startswith('-') then
      begin
        paramstr:=trim(paramstr).Substring(1);
      end
      else //positive
        opcode:=opcode or (1 shl param.extra);

      v:=StrToInt('$'+paramstr);

      opcode:=opcode or ((v and $f) shl pbyte(@param.offset)[0]);
      opcode:=opcode or (((v shr 4) and $f) shl pbyte(@param.offset)[1]);
    end;

    pt_imm8LH_label:
    begin
      v:=StrToInt('$'+paramstr);
      if v>=address then
      begin
        opcode:=opcode or (1 shl param.extra); //add=1
        v:=v-address;
      end
      else
        v:=address-v;

      if v>255 then exit;

      opcode:=opcode or ((v and $f) shl pbyte(@param.offset)[0]);
      opcode:=opcode or (((v shr 4) and $f) shl pbyte(@param.offset)[1]);
    end;

    pt_ap_reg:
    begin
      if paramstr='APSR_G' then
        v:=1
      else
      if paramstr='APSR_NZCVQ' then
        v:=2
      else
      if paramstr='APSR_NZCVQG' then
        v:=3
      else
        exit;

      opcode:=opcode or ((v and 3) shl param.offset);
    end;

    pt_spsr_reg:
    begin
      if paramstr.StartsWith('SPSR_') then
        paramstr:=paramstr.Substring(5)
      else
        exit;

      v:=strtoint('$'+paramstr);
      opcode:=opcode or ((v and param.maxval) shl param.offset);
    end;

    pt_cpsr_reg:
    begin
      if paramstr.StartsWith('CPSR_') then
        paramstr:=paramstr.Substring(5)
      else
        exit;

      v:=strtoint('$'+paramstr);
      opcode:=opcode or ((v and param.maxval) shl param.offset);
    end;

    pt_apsr: if paramstr<>'APSR' then exit;
    pt_spsr: if paramstr<>'SPSR' then exit;

    pt_rotatex8:
    begin
      if trim(paramstr).StartsWith('#') then
        paramstr:=trim(paramstr).Substring(1);

      v:=strtoint('$'+paramstr);
      case v of
        0: ;
        8: opcode:=opcode or (1 shl param.offset);
        16: opcode:=opcode or (2 shl param.offset);
        24: opcode:=opcode or (3 shl param.offset);
        else exit;
      end;
    end;

    pt_reglist16,
    pt_reglist16_x:
    begin
      paramstr:=trim(paramstr);
      if paramstr.StartsWith('{')=false then exit;
      if (param.ptype=pt_reglist16_x) and (not paramstr.EndsWith('}^')) then exit;
      if (param.ptype=pt_reglist16) and (not paramstr.EndsWith('}')) then exit;

      paramstr:=copy(paramstr,2,length(paramstr)-2);
      reglist:=paramstr.Split(',');

      if param.extra<>0 then
      begin
        if length(reglist)<param.extra then exit; //not allowed, needs more registers
      end;

      for i:=0 to length(reglist)-1 do
      begin
        reglist[i]:=trim(reglist[i]);
        if reglist[i].StartsWith('R') then
        begin
          j:=reglist[i].Substring(1).ToInteger;
          if (j>=0) and (j<=15) then
            opcode:=(opcode or (1 shl j)) shl param.offset
          else
            exit;
        end
        else
        begin
          if reglist[i]='FP' then
            opcode:=(opcode or (1 shl 11)) shl param.offset
          else
          if reglist[i]='IP' then
            opcode:=(opcode or (1 shl 12)) shl param.offset
          else
          if reglist[i]='SP' then
            opcode:=(opcode or (1 shl 13)) shl param.offset
          else
          if reglist[i]='LR' then
            opcode:=(opcode or (1 shl 14)) shl param.offset
          else
          if reglist[i]='PC' then
            opcode:=(opcode or (1 shl 15)) shl param.offset
          else
            exit; //unknown/unsupported reg
        end;
      end;

    end;

    pt_reglist_reg:
    begin
      paramstr:=copy(paramstr,2,length(paramstr)-2);
      reglist:=paramstr.Split(',');
      if length(reglist)>1 then exit;
      i:=ARMRegisterStringToInteger(reglist[0]);
      if i>param.maxval then exit;
      opcode:=opcode or (i shl param.offset);
    end;


  end;
  exit(true);
end;


function TArm32Instructionset.assemble(_address: ptruint; instruction: string): dword;
//raises EInvalidInstruction if it can't be assembled
var
  opcodestring,parameterstring: string;
  opcodeend: string;
  parameterstringsplit: array of string;
  i: integer;
  listindex: integer;
  selectedopcode: POpcode;
  parameters: array of record
    str: string;
    possibletypes: TArm32ParameterTypes;
    index: integer;
  end;
  inindex: boolean;

  preindexed: boolean;

  match: boolean;
  condition: integer;
begin
  InitARM32Support;

  result:=0;
  parameters:=[];

  i:=pos(' ', instruction);
  if i>0 then
  begin
    opcodestring:=copy(instruction,1,i-1);
    parameterstring:=trim(copy(instruction,i+1));
  end
  else
  begin
    opcodestring:=instruction;
    parameterstring:='';
  end;

  opcodeend:=uppercase(copy(opcodestring, length(opcodestring)-1,2));
  condition:=14; //no condition

  if ArmInstructionsAssemblerList.Find(opcodestring)=-1 then
  begin
    for i:=0 to 13 do
      if opcodeend=ArmConditions[i] then
      begin
        opcodestring:=copy(opcodestring,1,length(opcodestring)-2);
        condition:=i;
        break;
      end;
  end;




  if pos(',',parameterstring)>0 then
  begin
    parameterstringsplit:=parameterstring.Split([','],'{','}');
    setlength(parameters, length(parameterstringsplit));
  end
  else
  begin
    if parameterstring='' then
    begin
      setlength(parameters,0);
      parameterstringsplit:=[];
    end
    else
    begin
      setlength(parameters,1);
      parameterstringsplit:=[parameterstring];
    end;
  end;

  preindexed:=false;
  inindex:=false;

  self.address:=_address;



  for i:=0 to length(parameterstringsplit)-1 do
  begin
    if inindex then
      parameters[i].index:=1
    else
      parameters[i].index:=0;

    parameters[i].possibletypes:=[];
    parameters[i].str:=trim(parameterstringsplit[i]);

    if parameters[i].str[1]='[' then
    begin
      parameters[i].index:=1;
      parameters[i].str:=trim(copy(parameters[i].str,2));
      inindex:=true;
    end;

    if parameters[i].str.EndsWith(']!') then
    begin
      parameters[i].str:=copy(parameters[i].str,1,length(parameters[i].str)-2);
      inindex:=false;
      preindexed:=true;
    end;

    if parameters[i].str.EndsWith(']') then
    begin
      parameters[i].str:=copy(parameters[i].str,1,length(parameters[i].str)-1);
      inindex:=false;
    end;

    parameters[i].possibletypes:=GuessTypes(parameters[i].str);
  end;



  listindex:=ArmInstructionsAssemblerList.Find(opcodestring);
  if listindex=-1 then
  begin
    if opcodestring.EndsWith('S') then
    begin
      //flag adjusting
      opcodestring:=copy(opcodestring,1,length(opcodestring)-1);
      listindex:=ArmInstructionsAssemblerList.Find(opcodestring);
    end;

    if listindex=-1 then //invalid
      exit;
  end;


  while (listindex>0) and (uppercase(ArmInstructionsAssemblerList.List[listindex-1]^.Key)=uppercase(opcodestring)) do
    dec(listindex); //find the actual start of the list

  while (listindex<ArmInstructionsAssemblerList.Count) and (uppercase(ArmInstructionsAssemblerList.List[listindex]^.Key)=uppercase(opcodestring)) do
  begin
    //check if this entry matches the parameters given
    selectedopcode:=POpcode(ArmInstructionsAssemblerList.List[listindex]^.Data);

    {$ifdef armdev}
    DebugOutputOpcode(selectedopcode);
    {$endif}



    if length(parameters)>length(selectedopcode^.params) then
    begin
      inc(listindex);
      continue; //don't bother.  (the other way is possible though if optional parameters are present)
    end;

    match:=true;
    //first a quick check to see if the parameters match type and count
    for i:=0 to length(selectedopcode^.params)-1 do
    begin
      if i<length(parameters) then
      begin
        if (not (selectedopcode^.params[i].ptype in parameters[i].possibletypes))  or
           ((parameters[i].index<>0) and (selectedopcode^.params[i].index=ind_no)) or
           ((parameters[i].index=0) and (selectedopcode^.params[i].index<>ind_no)) or
           ((selectedopcode^.params[i].index in [ind_singleexp,ind_stopexp]) and (not preindexed) ) or
           ((selectedopcode^.params[i].index in [ind_single,ind_stop]) and (preindexed))
        then
        begin
          match:=false;
          break;
        end;
      end
      else if (not selectedopcode^.params[i].optional) then
      begin //there is supposed to be another non-optional parameter
        match:=false;
        break;
      end;
    end;

    if match then
    begin
      //all good, try to assemble it
      opcode:=selectedopcode^.value;

      if (opa_cond in selectedopcode^.additions) and ((selectedopcode^.mask and ($f shl 28))=0) then //conditional field is modifiable, apply the condition value
        opcode:=opcode or (condition shl 28);

      //try to apply the parameters
      for i:=0 to length(parameters)-1 do
      begin
        try
          if ParseParameterForAssembler(selectedopcode^.params[i], parameters[i].str)=false then
          begin
            //the given parameter could not be parsed using the given type after all.
            match:=false;
            break;
          end;
        except
          match:=false;
          break;
        end;
      end;

      //check for extra rules?
    end;

    if match then
    begin
      result:=opcode;
      exit; //still a match, so use this
    end;
    opcode:=0;
    inc(listindex);
  end;

  raise EInvalidInstruction.create('Invalid instruction');
end;

{$ifdef armdev}
procedure GetArmInstructionsAssemblerListDebug(r: tstrings);
var i,j: integer;
  x: string;


  d: TArm32Instructionset;
begin
  d.InitARM32Support;

  for i:=0 to ArmInstructionsAssemblerList.Count-1 do
  begin
    x:='';
    for j:=0 to length(popcode(ArmInstructionsAssemblerList.List[i]^.Data)^.params)-1 do
    begin
      if popcode(ArmInstructionsAssemblerList.List[i]^.Data)^.params[j].optional then
        x:='(has optional field)';
    end;
    r.add(inttostr(i)+'='+ArmInstructionsAssemblerList.List[i]^.Key+' - '+inttohex(ptruint(ArmInstructionsAssemblerList.List[i]^.Data),8)+'  '+x);
  end;

end;
{$endif}


//todo maybe: first sort each table by mask popcnt
procedure FillArmInstructionsAssemblerListWithOpcodeArray(const list: TOpcodeArray);
var i: integer;
begin
  for i:=length(list)-1 downto 0 do
  begin
    if list[i].use=iuDisassembler then continue;

    ArmInstructionsAssemblerList.Add(list[i].mnemonic,@list[i]);
  end;
end;

procedure FillArmInstructionsAssemblerListWithInstructionGroupArray(const list: TInstructionGroupArray);
var i: integer;
begin
  for i:=0 to length(list)-1 do
  begin
    if list[i].listType=igpGroup then
      FillArmInstructionsAssemblerListWithInstructionGroupArray(PInstructionGroupArray(list[i].list)^)
    else
      FillArmInstructionsAssemblerListWithOpcodeArray(POpcodeArray(list[i].list)^);
  end;

end;

procedure InitializeArmInstructionsAssemblerList;
begin
  ArmInstructionsAssemblerList:=TStringHashList.Create(false);
  FillArmInstructionsAssemblerListWithInstructionGroupArray(ArmGroupBase);
end;


procedure TArm32Instructionset.InitARM32Support;
const initialized: boolean=false;
begin
  if not initialized then
  begin
    InitializeArmInstructionsAssemblerList;
    initialized:=true;
  end;
end;

{$ifdef armdev}
function test(d: dword):string;
var
  v: dword;
  p: string;
  qv: qword;
begin
  qv:=0;
  v:=$ffffffff;
  v:=v+qv+1;

  v:=v and $ffffffff;
  p:=IntToHex(dword(v),8);
  result:=p;
end;
{$endif}

end.

