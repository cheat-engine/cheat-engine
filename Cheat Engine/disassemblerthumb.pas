unit DisassemblerThumb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TThumbDisassembler=object
  private
    opcode: uint16;
    opcode2: uint16; //for 32-bit thumb2
  public
//    LastDisassembleData: TLastDisassembleData;
    function disassemble(var address: ptrUint): string;
  end;

implementation

function TThumbDisassembler.disassemble(var address: ptruint): string;
begin

end;

end.

