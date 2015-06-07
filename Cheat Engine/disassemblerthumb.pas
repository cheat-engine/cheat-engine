unit DisassemblerThumb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TThumbDisassembler=object
  private
    opcode: uint16;
  public
//    LastDisassembleData: TLastDisassembleData;
    function disassemble(var address: ptrUint): string;
  end;

implementation

function TThumbDisassembler.disassemble(var address: ptruint): string;
begin

end;

end.

