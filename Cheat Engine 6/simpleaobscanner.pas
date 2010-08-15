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
  max: ptrUint;
begin
  ms:=tmemscan.create(nil);
  ms.onlyone:=true;
  {$ifdef cpu64}
  if processhandler.is64Bit then
    max:=qword($7fffffffffffffff)
  else
  {$else}
    max:=$7fffffff;
  {$endif}

  ms.firstscan(soExactValue, vtByteArray, rtRounded, aobstring, '', 0, max, false, true, true, false, false, false,false);

  ms.waittilldone; //wait till it's finished scanning
  if ms.GetOnlyOneResult(x) then
    result:=x
  else
    result:=0;
end;

end.
