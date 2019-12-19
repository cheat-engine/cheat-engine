unit vextypedef;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TVEX3Byte=bitpacked record
    mmmmm: 0..31;
    B: 0..1;
    X: 0..1;
    R: 0..1;
    pp: 0..3;
    L: 0..1;
    vvvv: 0..15;
    W: 0..1;
  end;
  PVEX3Byte=^TVEX3Byte;

  TVEX2Byte=bitpacked record
    pp: 0..3;
    L: 0..1;
    vvvv: 0..15;
    R: 0..1;
  end;
  PVex2Byte=^TVex2Byte;

implementation

end.

