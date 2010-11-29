unit simpleaobscanner;

{$MODE Delphi}

{
Will search for an array of bytes and returns as soon as it finds it
}

interface

uses LCLIntf, memscan, CEFuncProc;

function findaob(aobstring: string): ptruint;

implementation

uses NewKernelHandler;

function findaob(aobstring: string): ptruint;
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
  {$endif}
  begin
    if Is64bitOS then
      max:=$ffffffff
    else
      max:=$7fffffff;
  end;

  ms.firstscan(soExactValue, vtByteArray, rtRounded, aobstring, '', 0, max, false, true, true, false, false, false,false);

  ms.waittilldone; //wait till it's finished scanning
  if ms.GetOnlyOneResult(x) then
    result:=x
  else
    result:=0;

  ms.free;
end;

end.
