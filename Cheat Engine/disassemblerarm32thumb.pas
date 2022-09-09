//copyright Cheat Engine 2022. All rights reserved
unit disassemblerArm32Thumb;

{$mode objfpc}{$H+}
{$WARN 3177 off : Some fields coming after "$1" were not initialized}
interface

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
  TThumbParameterType=(pt_rreg3, //3 bit to define a reg in r0 to r7
                       pt_rreg3_same, //no-encoding, but must make sure that the given register is the same as already encoded at the given offset
                       pt_rreg3_exp, //same as rreg3 but with a !
                       pt_rreg3_1, //4 bit arm reg encoded as 3 bits and the last bit is in the extra field
                       pt_rreg4, //4 bit arm reg

                       pt_spreg, //just the sp reg, has no encoding bits
                       pt_imm_val0, //just #0
                       pt_imm,
                       pt_imm5_1_shl1_label,
                       pt_imm_shl2_poslabel, //positive label
                       pt_imm_shl2,
                       pt_simm_shl1_label,
                       pt_reglist13,
                       pt_reglist8,
                       pt_reglist8_exclude_rreg3, //reglist, but if extra offset in extra contains an rreg specified in this entry, it's invalid
                       pt_reglist8_withExtra //the pbyte(@extra)[0]=extra registernr, pbyte(@extra)[1]=bitposition for 1/0
                       );



  TAParameters=record
    ptype: TThumbParameterType;
    offset: dword; //binary position  (in case of imm2/pt_reglist_*: offset is a 32-bit bitmask and assumed concatenated from left to right)
    maxval: dword;
    extra:  qword; //extra data for paramtypes
    optional: boolean;
    defvalue: integer; //in case of optional
    index: TIndexedParameter;
  end;

  TAParametersList=array of TAParameters;

  TInstructionUse=(iuBoth=0, iuAssembler=1, iuDisassembler=2);

  TOpcodeAdditions=(
                    opa_ITCond_S, //if/then block condition, else S
                    opa_tcond8 //opcode followed by conditional (EQ, NZ, etc...) bit 8 contains the condition


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

  TThumbParameterTypes=set of TThumbParameterType;

  TThumbInstructionset=object
  private
    address: dword;
    opcode: uint32;
    size: integer;

    procedure InitThumbSupport;

    function ParseParametersForDisassembler(plist: TAParametersList): boolean;
    function ScanOpcodeList(const list: topcodearray): boolean;
    function ScanGroupList(const list: TInstructionGroupArray): boolean;
    //assembler
    function ParseParameterForAssembler(param:TAParameters; paramstr: string): boolean;
    function GuessTypes(param: string): TThumbParameterTypes;
  public
    LastDisassembleData: TLastDisassembleData;
    function disassemble(var DisassembleAddress: ptruint{$ifdef armdev}; _opcode: dword{$endif}): string;
    function assemble(_address: ptruint; instruction: string): DWORD;
  end;

  {$ifdef armdev}
  procedure GetThumbInstructionsAssemblerListDebug(r: tstrings);
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


  ThumbInstructionsBase16: array of TOpcode=(
    (mnemonic:'LDR';  additions:[];  params:((ptype:pt_rreg3; offset:8; maxval:7), (ptype:pt_imm_shl2_poslabel; offset:0; maxval:255));          mask: %1111100000000000; value: %0100100000000000),
    (mnemonic:'ADR';  additions:[];  params:((ptype:pt_rreg3; offset:8; maxval:7), (ptype:pt_imm_shl2_poslabel; offset:0; maxval:255));          mask: %1111100000000000; value: %1010000000000000),
    (mnemonic:'ADD';  additions:[];  params:((ptype:pt_rreg3; offset:8; maxval:7), (ptype:pt_spreg), (ptype:pt_imm_shl2; offset:0; maxval:255)); mask: %1111100000000000; value: %1010100000000000),
    (mnemonic:'STM';  additions:[];  params:((ptype:pt_rreg3_exp; offset:8; maxval:7), (ptype:pt_reglist13; offset:0));                          mask: %1111100000000000; value: %1100000000000000),
    (mnemonic:'LDM';  additions:[];  params:((ptype:pt_rreg3_exp; offset:8; maxval:7), (ptype:pt_reglist8_exclude_rreg3; offset:0;maxval:0; extra:8));   mask: %1111100000000000; value: %1100100000000000),
    (mnemonic:'B';    additions:[];  params:((ptype:pt_simm_shl1_label; offset:0; maxval:$7ff));                                                 mask: %1111100000000000; value: %1110000000000000)
  );

  ThumbInstructionsMiscellaneous16BitInstructions: array of TOpcode=(
    //1011************
    (mnemonic:'ADD';  additions:[];  params:((ptype:pt_spreg), (ptype:pt_spreg), (ptype:pt_imm_shl2; offset:0; maxval:127));  mask: %1111111110000000; value: %1011000000000000),
    (mnemonic:'SUB';  additions:[];  params:((ptype:pt_spreg), (ptype:pt_spreg), (ptype:pt_imm_shl2; offset:0; maxval:127));  mask: %1111111110000000; value: %1011000010000000),

    (mnemonic:'CBZ'; additions:[];params:((ptype:pt_rreg3;offset:0),(ptype:pt_imm5_1_shl1_label;offset:3;maxval:$1f;extra:9));mask: %1111110100000000; value: %1011000100000000),
    (mnemonic:'CBNZ';additions:[];params:((ptype:pt_rreg3;offset:0),(ptype:pt_imm5_1_shl1_label;offset:3;maxval:$1f;extra:9));mask: %1111110100000000; value: %1011100100000000),

    (mnemonic:'PUSH'; additions:[];  params:((ptype:pt_reglist8_withExtra; offset:0; maxval:0; extra:$080e));                 mask: %1111111000000000; value: %1011010000000000),
    (mnemonic:'SETEND'; additions:[];  params:((ptype:pt_imm; offset:3; maxval:1));                                           mask: %1111111111110111; value: %1011011001010000),

    (mnemonic:'CPS'; additions:[];  params:();                                                                                mask: %1111111111101000; value: %1011011001100000),//todo: fill in

    (mnemonic:'REV';  additions:[];params:((ptype:pt_rreg3;offset:0),(ptype:pt_rreg3;offset:3));                              mask: %1111111111000000; value: %1011101000000000),
    (mnemonic:'REV16';additions:[];params:((ptype:pt_rreg3;offset:0),(ptype:pt_rreg3;offset:3));                              mask: %1111111111000000; value: %1011101001000000),
    (mnemonic:'REVSH';additions:[];params:((ptype:pt_rreg3;offset:0),(ptype:pt_rreg3;offset:3));                              mask: %1111111111000000; value: %1011101011000000),

    (mnemonic:'POP'; additions:[];  params:((ptype:pt_reglist8_withExtra; offset:0; maxval:0; extra:$080f));                  mask: %1111111000000000; value: %1011110000000000),

    (mnemonic:'BKPT'; additions:[]; params:((ptype:pt_imm; offset:0; maxval:255));                                            mask: %1111111100000000; value: %1011111000000000)


    //if then and hints

//    (mnemonic:'IT'; additions:[];

  );

  ThumbInstructionsShiftAddSubtractMoveAndCompare16:array of TOpcode=(
    //00**************
    (mnemonic:'MOVS'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                                    mask: %1111111111000000; value: %0000000000000000),
    (mnemonic:'LSL';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_imm; offset:5; maxval: 63));  mask: %1111100000000000; value: %0000000000000000),
    (mnemonic:'LSR';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_imm; offset:5; maxval: 63));  mask: %1111100000000000; value: %0000100000000000),
    (mnemonic:'ASR';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_imm; offset:5; maxval: 63));  mask: %1111100000000000; value: %0001000000000000),
    (mnemonic:'ADD';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_rreg3; offset:6));            mask: %1111111000000000; value: %0001100000000000),
    (mnemonic:'SUB';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_rreg3; offset:6));            mask: %1111111000000000; value: %0001111000000000),
    (mnemonic:'MOV';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:8), (ptype:pt_imm; offset:0; maxval: 255));                             mask: %1111100000000000; value: %0010000000000000),
    (mnemonic:'CMP';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:8), (ptype:pt_imm; offset:0; maxval: 255));                             mask: %1111100000000000; value: %0010100000000000),
    (mnemonic:'ADD';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_imm; offset:6; maxval: 7));   mask: %1111111000000000; value: %0001110000000000),
    (mnemonic:'ADD';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:8), (ptype:pt_imm; offset:6; maxval: 255));                             mask: %1111100000000000; value: %0011000000000000),
    (mnemonic:'SUB';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_imm; offset:6; maxval: 7));   mask: %1111111000000000; value: %0001111000000000),
    (mnemonic:'SUB';  additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:8), (ptype:pt_imm; offset:6; maxval: 255));                             mask: %1111100000000000; value: %0011100000000000)
  );

  ThumbInstructionsDataProcessing16:array of TOpcode=(
    //010000**********
    (mnemonic:'AND'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100000000000000),
    (mnemonic:'EOR'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100000001000000),
    (mnemonic:'LSL'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100000010000000),
    (mnemonic:'LSR'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100000011000000),
    (mnemonic:'ASR'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100000100000000),
    (mnemonic:'ADC'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100000101000000),
    (mnemonic:'SBC'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100000110000000),
    (mnemonic:'ROR'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100000111000000),
    (mnemonic:'TST'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100001000000000),
    (mnemonic:'RSB'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_imm_val0));            mask: %1111111111000000; value: %0100001001000000),
    (mnemonic:'CMP'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100001010000000),
    (mnemonic:'CMN'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100001011000000),
    (mnemonic:'ORR'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100001100000000),
    (mnemonic:'MUL'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100001101000000),
    (mnemonic:'BIC'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3), (ptype:pt_rreg3_same; offset:0));mask: %1111111111000000; value: %0100001110000000),
    (mnemonic:'MVN'; additions:[opa_ITCond_S];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3));                                 mask: %1111111111000000; value: %0100001111000000)
  );

  ThumbInstructionsSpecialDataInstructionsAndBranchAndExchange16:array of TOpcode=(
    //010001**********;
    (mnemonic:'ADD';  additions:[];  params:((ptype:pt_rreg3_1; offset:0; maxval:15; extra:7), (ptype:pt_rreg4; offset:3));     mask: %1111111100000000; value: %0100010000000000),
    (mnemonic:'CMP';  additions:[];  params:((ptype:pt_rreg3_1; offset:0; maxval:15; extra:7), (ptype:pt_rreg4; offset:3));     mask: %1111111100000000; value: %0100010100000000),
    (mnemonic:'MOV';  additions:[];  params:((ptype:pt_rreg3_1; offset:0; maxval:15; extra:7), (ptype:pt_rreg4; offset:3));     mask: %1111111100000000; value: %0100011000000000),
    (mnemonic:'BX';   additions:[];  params:((ptype:pt_rreg4; offset:3));                                                       mask: %1111111110000111; value: %0100011100000000),
    (mnemonic:'BLX';  additions:[];  params:((ptype:pt_rreg4; offset:3));                                                       mask: %1111111110000111; value: %0100011110000000)
  );

  ThumbInstructionsLoadStoreSingleDataItem:array of TOpcode=(
    //0101************
    (mnemonic:'STR';  additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_rreg3; offset:6; maxval:7; extra:0; optional:false; defvalue:0; index: ind_stop));   mask: %1111111000000000; value: %0101000000000000),
    (mnemonic:'STRH'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_rreg3; offset:6; maxval:7; extra:0; optional:false; defvalue:0; index: ind_stop));   mask: %1111111000000000; value: %0101001000000000),
    (mnemonic:'STRB'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_rreg3; offset:6; maxval:7; extra:0; optional:false; defvalue:0; index: ind_stop));   mask: %1111111000000000; value: %0101010000000000),
    (mnemonic:'LDRSB';additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_rreg3; offset:6; maxval:7; extra:0; optional:false; defvalue:0; index: ind_stop));   mask: %1111111000000000; value: %0101011000000000),
    (mnemonic:'LDR';  additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_rreg3; offset:6; maxval:7; extra:0; optional:false; defvalue:0; index: ind_stop));   mask: %1111111000000000; value: %0101100000000000),
    (mnemonic:'LDRH'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_rreg3; offset:6; maxval:7; extra:0; optional:false; defvalue:0; index: ind_stop));   mask: %1111111000000000; value: %0101101000000000),
    (mnemonic:'LDRB'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_rreg3; offset:6; maxval:7; extra:0; optional:false; defvalue:0; index: ind_stop));   mask: %1111111000000000; value: %0101110000000000),
    (mnemonic:'LDRSH';additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_rreg3; offset:6; maxval:7; extra:0; optional:false; defvalue:0; index: ind_stop));   mask: %1111111000000000; value: %0101111000000000),

    //011*************
    (mnemonic:'STR';  additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_imm_shl2;offset:6; maxval:$1f; extra:0; optional:true; defvalue:0; index: ind_stop));mask: %1111100000000000; value: %0110000000000000),
    (mnemonic:'LDR';  additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_imm_shl2;offset:6; maxval:$1f; extra:0; optional:true; defvalue:0; index: ind_stop));mask: %1111100000000000; value: %0110100000000000),

    (mnemonic:'STRB'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_imm_shl2;offset:6; maxval:$1f; extra:0; optional:true; defvalue:0; index: ind_stop));mask: %1111100000000000; value: %0111000000000000),
    (mnemonic:'LDRB'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_imm_shl2;offset:6; maxval:$1f; extra:0; optional:true; defvalue:0; index: ind_stop));mask: %1111100000000000; value: %0111100000000000),

    //100*************
    (mnemonic:'STRH'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_imm_shl2;offset:6; maxval:$1f; extra:0; optional:true; defvalue:0; index: ind_stop));mask: %1111100000000000; value: %1000000000000000),
    (mnemonic:'LDRH'; additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_imm_shl2;offset:6; maxval:$1f; extra:0; optional:true; defvalue:0; index: ind_stop));mask: %1111100000000000; value: %1000100000000000),

    (mnemonic:'STR';  additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_imm_shl2;offset:6; maxval:$1f; extra:0; optional:true; defvalue:0; index: ind_stop));mask: %1111100000000000; value: %1001000000000000),
    (mnemonic:'LDR';  additions:[];  params:((ptype:pt_rreg3; offset:0), (ptype:pt_rreg3; offset:3; maxval:7; extra:0; optional:false; defvalue:0; index: ind_index), (ptype:pt_imm_shl2;offset:6; maxval:$1f; extra:0; optional:true; defvalue:0; index: ind_stop));mask: %1111100000000000; value: %1001100000000000)


  );

//--------------------------- instruction groups----------------------------//



 //



  ThumbGroupConditionalBranchAndSupervisorCall: array of TInstructionGroup=();
  //    (mnemonic:'B';    additions:[opa_tcond8]; params:((ptype:pt_simm_shl1_label; offset:0; maxval:$7ff)); mask: %1111000000000000; value: %1101000000000000)



  ThumbGroupBase16: array of TInstructionGroup=(
    (mask:%1111110000000000; value: %0100000000000000; list: @ThumbInstructionsDataProcessing16; listType: igpInstructions; name: 'ThumbInstructionsDataProcessing'),
    (mask:%1111110000000000; value: %0100010000000000; list: @ThumbInstructionsSpecialDataInstructionsAndBranchAndExchange16; listType: igpInstructions; name: 'ThumbGroupSpecialDataInstructionsAndBranchAndExchange'),
    (mask:%1111100000000000; value: %0100100000000000; list: @ThumbInstructionsBase16; listType: igpInstructions; name: 'ThumbInstructionsBase16'), //LDR
    (mask:%1111100000000000; value: %1010000000000000; list: @ThumbInstructionsBase16; listType: igpInstructions; name: 'ThumbInstructionsBase16'), //ADR
    (mask:%1111100000000000; value: %1010100000000000; list: @ThumbInstructionsBase16; listType: igpInstructions; name: 'ThumbInstructionsBase16'), //ADD
    (mask:%1111100000000000; value: %1100000000000000; list: @ThumbInstructionsBase16; listType: igpInstructions; name: 'ThumbInstructionsBase16'), //STM
    (mask:%1111100000000000; value: %1100100000000000; list: @ThumbInstructionsBase16; listType: igpInstructions; name: 'ThumbInstructionsBase16'), //LDM
    (mask:%1111100000000000; value: %1110000000000000; list: @ThumbInstructionsBase16; listType: igpInstructions; name: 'ThumbInstructionsBase16'), //B


    (mask:%1111000000000000; value: %1011000000000000; list: @ThumbInstructionsMiscellaneous16BitInstructions; listType: igpInstructions; name: 'ThumbInstructionsMiscellaneous16BitInstructions'),


    (mask:%1111000000000000; value: %1101000000000000; list: @ThumbGroupConditionalBranchAndSupervisorCall; listType: igpGroup; name: 'ThumbGroupConditionalBranchAndSupervisorCall'),


    (mask:%1111000000000000; value: %0101000000000000; list: @ThumbInstructionsLoadStoreSingleDataItem; listType: igpInstructions; name: 'ThumbInstructionsLoadStoreSingleDataItem'),
    (mask:%1110000000000000; value: %0110000000000000; list: @ThumbInstructionsLoadStoreSingleDataItem; listType: igpInstructions; name: 'ThumbInstructionsLoadStoreSingleDataItem'),
    (mask:%1110000000000000; value: %1000000000000000; list: @ThumbInstructionsLoadStoreSingleDataItem; listType: igpInstructions; name: 'ThumbInstructionsLoadStoreSingleDataItem'),


    (mask:%1100000000000000; value: %0000000000000000; list: @ThumbInstructionsShiftAddSubtractMoveAndCompare16; listType: igpInstructions; name: 'ThumbInstructionsShiftAddSubtractMoveAndCompare')
  );

  ThumbGroupBase32: array of TInstructionGroup=(
    //(mask:%00000000000000000000000000000000; value: %11110000000000000000000000000000; list: @ThumbGroupUnconditionalInstructions; listType: igpGroup; name: 'ThumbGroupUnconditionalInstructions'),
  );

  ThumbGroupBase: array of TInstructionGroup=(
    (mask:%00000000000000001111100000000000; value: %00000000000000001110100000000000; list: @ThumbGroupBase32; listType: igpGroup; name: 'ThumbGroupBase32'),
    (mask:%00000000000000001111100000000000; value: %00000000000000001111000000000000; list: @ThumbGroupBase32; listType: igpGroup; name: 'ThumbGroupBase32'),
    (mask:%00000000000000001111100000000000; value: %00000000000000001111100000000000; list: @ThumbGroupBase32; listType: igpGroup; name: 'ThumbGroupBase32'),
    (mask:%00000000000000000000000000000000; value: %00000000000000000000000000000000; list: @ThumbGroupBase16; listType: igpGroup; name: 'ThumbGroupBase16')

  );

var
  ThumbInstructionsAssemblerList: TStringHashList;

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

    ti:=TypeInfo(TThumbParameterType);
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
  if sourcesize=0 then exit;

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

function TThumbInstructionset.ParseParametersForDisassembler(plist: TAParametersList): boolean;
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
      pt_rreg3, pt_rreg3_exp:
      begin
        v:=(opcode shr plist[i].offset) and 7;
        p:=ArmRegisters[v];

        if plist[i].ptype=pt_rreg3_exp then p:=p+'!';
      end;

      pt_rreg3_1:
      begin
        v:=(opcode shr plist[i].offset) and 7;
        v:=v or (((opcode shr plist[i].extra) and 1) shl 3);
        p:=ArmRegisters[v];
      end;

      pt_rreg4:
      begin
        v:=(opcode shr plist[i].offset) and $f;
        p:=ArmRegisters[v];
      end;

      pt_spreg: p:='SP';
      pt_imm_val0: p:='#0';

      pt_imm:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        p:='#'+v.ToHexString(1);
      end;

      pt_simm_shl1_label:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        v:=SignExtend(v,7);
        v:=v shl 1;

        v:=address+v+2*size;
        p:=v.ToHexString(1);
      end;

      pt_imm5_1_shl1_label:
      begin
        v:=(opcode shr plist[i].offset) and $1f;
        v:=v or (((opcode shr plist[i].extra) and 1) shl 5);
        v:=v shl 1;

        v:=address+v+2*size;
        p:=v.ToHexString(1);
      end;

      pt_imm_shl2_poslabel:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        v:=v shl 2;
        v:=(address and $fffffffc)+v+2*size;
        p:=v.ToHexString(1);
      end;

      pt_imm_shl2:
      begin
        v:=(opcode shr plist[i].offset) and plist[i].maxval;
        v:=v shl 2;
        p:='#'+v.ToHexString(1);
      end;

      pt_reglist8,
      pt_reglist8_withExtra:
      begin
        p:='{';
        first:=true;
        for j:=0 to 7 do
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

        if plist[i].ptype=pt_reglist8_withExtra then
        begin
          if ((opcode shr (pbyte(@plist[i].extra)[1])) and 1)=1 then
          begin
            j:=pbyte(@plist[i].extra)[0];
            p:=p+', '+ArmRegisters[j];
          end;
        end;
        p:=p+'}';
      end;

      pt_reglist8_exclude_rreg3:
      begin
        p:='{';
        first:=true;
        v:=opcode shr plist[i].extra and 7;

        for j:=0 to 7 do
        begin
          if ((opcode shr plist[i].offset) and (1 shl j))<>0 then
          begin
            if j=v then exit(false); //invalid bit set

            if not first then
              p:=p+', '
            else
              first:=false;

            p:=p+ArmRegisters[j];
          end;
        end;
        p:=p+'}';
      end;

      pt_reglist13:
      begin
        p:='{';
        first:=true;
        for j:=0 to 12 do
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

function TThumbInstructionset.ScanOpcodeList(const list: topcodearray): boolean;
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

      if (opa_tcond8 in list[i].additions) then
        LastDisassembleData.opcode:=LastDisassembleData.opcode+ArmConditions[(opcode shr 8) and $f];



      //parse the parameters
      if ParseParametersForDisassembler(list[i].params) then
        exit(true);
    end;
  end;
end;

function TThumbInstructionset.ScanGroupList(const list: TInstructionGroupArray):boolean;
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

      if result then //unlike arm64, Thumb is a chaotic pile of doodoo with many overlaps
        exit;
    end;
  end;
end;



function TThumbInstructionset.disassemble(var DisassembleAddress: ptruint{$ifdef armdev}; _opcode: dword{$endif}): string;
var
  x: ptruint;
  i: integer;
begin
  InitThumbSupport;

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

  case (opcode and %1111100000000000) shr 11 of
    %11101,
    %11110,
    %11111:
    begin
      size:=4;
      ScanGroupList(ThumbGroupBase32);
    end
    else
    begin
      setlength(LastDisassembleData.Bytes,2);
      size:=2;
      opcode:=opcode and $ffff;
      ScanGroupList(ThumbGroupBase16);
    end;
  end;

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




function TThumbInstructionset.GuessTypes(param: string): TThumbParameterTypes;
var
  li: longint;
  i64: int64;
  i: integer;
  s: string;
  f: single;
  v: dword;
  isforcedSigned: boolean;
  hasExp: boolean;

  notlabel: boolean;
begin
  //pt_creg?
  result:=[];
  param:=trim(param);
  if length(param)=0 then exit;


  i:=ARMRegisterStringToInteger(param);
  if i<>-1 then
  begin
    if i<=7 then
      result:=result+[pt_rreg3, pt_rreg3_same, pt_rreg4,pt_rreg3_1]
    else
      result:=result+[pt_rreg4, pt_rreg3_1];

    if i=13 then
      result:=result+[pt_spreg];
  end;


  if (param[1]='#') then
  begin
    param:=param.Substring(1);
    notlabel:=true;   //not a label when # is used
  end
  else
    notlabel:=false;


  if (param[1]='-') then
  begin
    if TryStrToUInt('-$'+param.Substring(1),v) then
      result:=result+[pt_imm_shl2, pt_imm];
  end
  else
  begin
    if TryStrToUInt('$'+param,v) then
    begin
      result:=result+[pt_imm_shl2, pt_imm];

      if v=0 then
        result:=result+[pt_imm_val0];


      if notlabel=false then
        result:=result+[pt_imm_shl2_poslabel, pt_simm_shl1_label, pt_imm5_1_shl1_label];
    end;
  end;

  case param[1] of
    '{':
    begin
      if param.EndsWith('}') then
        result:=result+[pt_reglist13, pt_reglist8_exclude_rreg3, pt_reglist8, pt_reglist8_withExtra];
    end;
  end;

end;

function TThumbInstructionset.ParseParameterForAssembler(param:TAParameters; paramstr: string): boolean;
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
    pt_rreg3, pt_rreg3_exp:
    begin
      if param.ptype=pt_rreg3_exp then
      begin
        if paramstr.EndsWith('!') then
        begin
          paramstr:=paramstr.Substring(0,length(paramstr)-2);
        end
        else
          exit;
      end;


      j:=ARMRegisterStringToInteger(paramstr);
      if j=-1 then exit;
      if j>7 then exit;

      opcode:=opcode or (j shl param.offset);
    end;

    pt_rreg3_same:
    begin
      j:=ARMRegisterStringToInteger(paramstr);
      if (opcode shr param.offset) and $7<>j then exit;
    end;

    pt_rreg3_1:
    begin
      j:=ARMRegisterStringToInteger(paramstr);
      if j=-1 then exit;
      if j>15 then exit;
      opcode:=opcode or ((j shl param.offset) and 7);
      opcode:=opcode or ((j shr 3) shl param.extra);

    end;

    pt_rreg4:
    begin
      j:=ARMRegisterStringToInteger(paramstr);
      if j=-1 then exit;
      if j>15 then exit;
      opcode:=opcode or (j shl param.offset);
    end;





    pt_spreg: if paramstr<>'SP' then exit;

    pt_imm_val0:
    begin
      if trim(paramstr).StartsWith('#') then
        paramstr:=trim(paramstr).Substring(1);

      v:=StrToInt('$'+paramstr);
      if v<>0 then exit;
    end;


    pt_imm:
    begin
      if trim(paramstr).StartsWith('#') then
        paramstr:=trim(paramstr).Substring(1);

      v:=StrToInt('$'+paramstr);
      opcode:=opcode or ((v and param.maxval) shl param.offset);
    end;

    pt_simm_shl1_label:
    begin
      if trim(paramstr).StartsWith('#') then
        paramstr:=trim(paramstr).Substring(1);

      v:=StrToInt('$'+paramstr);
      v:=v-address;
      if (v and 1)<>0 then exit;
      v:=v shr 1;


      opcode:=opcode or ((v and param.maxval) shl param.offset);
    end;

    pt_imm5_1_shl1_label:
    begin
      if trim(paramstr).StartsWith('#') then
        paramstr:=trim(paramstr).Substring(1);

      v:=StrToInt('$'+paramstr);
      v:=v-(address+2*size);
      if (v and 1)<>0 then exit;
      v:=v shr 1;


      opcode:=opcode or ((v and $1f) shl param.offset);
      opcode:=opcode or (((v shr 5) and 1) shl param.extra);

    end;

    pt_imm_shl2_poslabel:
    begin
      if trim(paramstr).StartsWith('#') then
        paramstr:=trim(paramstr).Substring(1);

      v:=StrToInt('$'+paramstr);
      v:=v-address;
      if (v and 3)<>0 then exit;
      v:=v shr 2;
      opcode:=opcode or ((v and param.maxval) shl param.offset);
    end;

    pt_imm_shl2:
    begin
      if trim(paramstr).StartsWith('#') then
        paramstr:=trim(paramstr).Substring(1);

      opcode:=opcode or (1 shl param.extra);

      v:=StrToInt('$'+paramstr);

      opcode:=opcode or ((v and param.maxval) shl param.offset);
    end;

    pt_reglist8,
    pt_reglist8_withExtra:
    begin
      paramstr:=trim(paramstr);
      if paramstr.StartsWith('{')=false then exit;

      paramstr:=copy(paramstr,2,length(paramstr)-2);
      reglist:=paramstr.Split(',');

      for i:=0 to length(reglist)-1 do
      begin
        reglist[i]:=trim(reglist[i]);
        if reglist[i].StartsWith('R') then
        begin
          j:=reglist[i].Substring(1).ToInteger;
          if (j>=0) and (j<=7) then
            opcode:=(opcode or (1 shl j)) shl param.offset
          else
            exit;
        end
        else
        begin
          if (param.ptype=pt_reglist8_withExtra) then
          begin
            if reglist[i]<>ArmRegisters[pbyte(param.extra)[0]] then exit(false);
            opcode:=opcode or (1 shl pbyte(param.extra)[1]);
          end
          else
            exit;
        end;
      end;
    end;

    pt_reglist8_exclude_rreg3:
    begin
      paramstr:=trim(paramstr);
      if paramstr.StartsWith('{')=false then exit;

      paramstr:=copy(paramstr,2,length(paramstr)-2);
      reglist:=paramstr.Split(',');

      k:=(opcode shr param.extra) and 7;


      for i:=0 to length(reglist)-1 do
      begin
        reglist[i]:=trim(reglist[i]);
        if reglist[i].StartsWith('R') then
        begin
          j:=reglist[i].Substring(1).ToInteger;

          //the previous param field is already set, check it

          if j=k then exit; //not allowed for this encoding

          if (j>=0) and (j<=7) then
            opcode:=(opcode or (1 shl j)) shl param.offset
          else
            exit;
        end;
      end;
    end;


    pt_reglist13:
    begin
      paramstr:=trim(paramstr);
      if paramstr.StartsWith('{')=false then exit;

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
            opcode:=(opcode or (1 shl 12)) shl param.offset;
        end;
      end;
    end;


  end;
  exit(true);
end;


function TThumbInstructionset.assemble(_address: ptruint; instruction: string): dword;
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
    possibletypes: TThumbParameterTypes;
    index: integer;
  end;
  inindex: boolean;

  preindexed: boolean;

  match: boolean;
  condition: integer;
begin
  InitThumbSupport;

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

  if ThumbInstructionsAssemblerList.Find(opcodestring)=-1 then
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



  listindex:=ThumbInstructionsAssemblerList.Find(opcodestring);
  if listindex=-1 then
  begin
    if opcodestring.EndsWith('S') then
    begin
      //flag adjusting
      opcodestring:=copy(opcodestring,1,length(opcodestring)-1);
      listindex:=ThumbInstructionsAssemblerList.Find(opcodestring);
    end;

    if listindex=-1 then //invalid
      exit;
  end;


  while (listindex>0) and (uppercase(ThumbInstructionsAssemblerList.List[listindex-1]^.Key)=uppercase(opcodestring)) do
    dec(listindex); //find the actual start of the list

  while (listindex<ThumbInstructionsAssemblerList.Count) and (uppercase(ThumbInstructionsAssemblerList.List[listindex]^.Key)=uppercase(opcodestring)) do
  begin
    //check if this entry matches the parameters given
    selectedopcode:=POpcode(ThumbInstructionsAssemblerList.List[listindex]^.Data);

    {$ifdef armdev}
    DebugOutputOpcode(selectedopcode);
    {$endif}

    if length(parameters)>length(selectedopcode^.params) then
    begin
      inc(listindex);
      continue; //don't bother.  (the other way is possible though if optional parameters are present)
    end;

    if (selectedopcode^.mask shr 16)=0 then
      size:=2
    else
      size:=4;

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

      if (opa_tcond8 in selectedopcode^.additions) then //conditional field is modifiable, apply the condition value
        opcode:=opcode or (condition shl 8);

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
procedure GetThumbInstructionsAssemblerListDebug(r: tstrings);
var i,j: integer;
  x: string;


  d: TThumbInstructionset;
begin
  d.InitThumbSupport;

  for i:=0 to ThumbInstructionsAssemblerList.Count-1 do
  begin
    x:='';
    for j:=0 to length(popcode(ThumbInstructionsAssemblerList.List[i]^.Data)^.params)-1 do
    begin
      if popcode(ThumbInstructionsAssemblerList.List[i]^.Data)^.params[j].optional then
        x:='(has optional field)';
    end;
    r.add(inttostr(i)+'='+ThumbInstructionsAssemblerList.List[i]^.Key+' - '+inttohex(ptruint(ThumbInstructionsAssemblerList.List[i]^.Data),8)+'  '+x);
  end;

end;
{$endif}


//todo maybe: first sort each table by mask popcnt
procedure FillThumbInstructionsAssemblerListWithOpcodeArray(const list: TOpcodeArray);
var i: integer;
begin
  for i:=length(list)-1 downto 0 do
  begin
    if list[i].use=iuDisassembler then continue;

    ThumbInstructionsAssemblerList.Add(list[i].mnemonic,@list[i]);
  end;
end;

procedure FillThumbInstructionsAssemblerListWithInstructionGroupArray(const list: TInstructionGroupArray);
var i: integer;
begin
  for i:=0 to length(list)-1 do
  begin
    if list[i].listType=igpGroup then
      FillThumbInstructionsAssemblerListWithInstructionGroupArray(PInstructionGroupArray(list[i].list)^)
    else
      FillThumbInstructionsAssemblerListWithOpcodeArray(POpcodeArray(list[i].list)^);
  end;

end;

procedure InitializeThumbInstructionsAssemblerList;
begin
  ThumbInstructionsAssemblerList:=TStringHashList.Create(false);
  FillThumbInstructionsAssemblerListWithInstructionGroupArray(ThumbGroupBase);
end;


procedure TThumbInstructionset.InitThumbSupport;
const initialized: boolean=false;
begin
  if not initialized then
  begin
    InitializeThumbInstructionsAssemblerList;
    initialized:=true;
  end;
end;

end.

