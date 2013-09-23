unit LastDisassembleData;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TDisAssemblerValueType=(dvtNone=0, dvtAddress=1, dvtValue=2);



  TLastDisassembleData=record
    address: PtrUint;
    prefix: string;
    prefixsize: integer;
    opcode: string; //and sadly undone because I want to allow the user to change the string... pchar; //replaced string with pchar so it now only contains a pointer. Faster. string; //the result without bytes
    parameters: string;
    description: string;
    Bytes: array of byte;
    SeperatorCount: integer;
    Seperators: Array [0..5] of integer; //an index in the byte array describing the seperators (prefix/instruction/modrm/sib/extra)
    modrmValueType: TDisAssemblerValueType;
    modrmValue: ptrUint;
    parameterValueType: TDisAssemblerValueType;
    parameterValue: ptrUint;
  //  ValueType: TValueType; //if it's not unknown the value type will say what type of value it is (e.g for the FP types)

    hasSib: boolean;
    sibIndex: integer;
    sibScaler: integer;

    isjump: boolean; //set for anything that can change eip/rip
    iscall: boolean; //set if it's a call
    isret: boolean; //set if it's a ret
    isconditionaljump: boolean; //set if it's only effective when an conditon is met
  end;

implementation

end.

