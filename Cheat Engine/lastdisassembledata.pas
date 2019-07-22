unit LastDisassembleData;

{$mode delphi}

interface

uses
  Classes, SysUtils, commonTypeDefs;

type
  TDisAssemblerValueType=(dvtNone=0, dvtAddress=1, dvtValue=2);

  TDisassemblerClass=(dcX86, dcArm, dcThumb);


  TLastDisassembleData=record
    address: PtrUint;
    prefix: string;
    prefixsize: integer;
    opcode: string;
    parameters: string;
    description: string;
    commentsoverride: string;
    Bytes: array of byte;
    SeperatorCount: integer;
    Seperators: Array [0..5] of integer; //an index in the byte array describing the seperators (prefix/instruction/modrm/sib/extra)
    modrmValueType: TDisAssemblerValueType;
    modrmValue: ptrUint;
    parameterValueType: TDisAssemblerValueType;
    parameterValue: ptrUint;
  //  ValueType: TValueType; //if it's not unknown the value type will say what type of value it is (e.g for the FP types)

    datasize: integer;
    isfloat: boolean; //True if the data it reads/writes is a float (only when sure)
    isfloat64: boolean;
    iscloaked: boolean;

    hasSib: boolean;
    sibIndex: integer;
    sibScaler: integer;

    isjump: boolean; //set for anything that can change eip/rip
    iscall: boolean; //set if it's a call
    isret: boolean; //set if it's a ret
    isconditionaljump: boolean; //set if it's only effective when an conditon is met
    willJumpAccordingToContext: boolean; //only valid if a context was provided with the disassembler and isconditionaljump is true

    riprelative: integer; //0 or contains the offset where the rip relative part of the code is

    Disassembler: TDisassemblerClass;

    //todo: add an isreader/iswriter
    //and what registers/flags it could potentially access/modify
  end;

  PLastDisassembleData=^TLastDisassembleData;

implementation

end.

