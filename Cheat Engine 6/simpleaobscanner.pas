unit simpleaobscanner;

{$MODE Delphi}

{
Will search for an array of bytes and returns as soon as it finds it
}

interface

uses LCLIntf, memscan, CEFuncProc;

function findaob(aobstring: string): dword;

implementation

function findaob(aobstring: string): dword;
{scans the game's memory for aobstring and returns the pointer if found. returns 0 if not found}
var
  ms: tmemscan;
  x: ptruint;
begin
  ms:=tmemscan.create(nil);
  ms.onlyone:=true;

  ms.firstscan(soExactValue, vtByteArray, rtRounded, aobstring, '', 0, $7fffffff, false, true, true, false, false, false, nil, cstNone);

  ms.waittilldone; //wait till it's finished scanning
  if ms.GetOnlyOneResult(x) then
    result:=x
  else
    result:=0;
end;

end.
