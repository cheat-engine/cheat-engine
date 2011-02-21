unit simpleaobscanner;

{$MODE Delphi}

{
Will search for an array of bytes and returns as soon as it finds it
}

interface

uses LCLIntf, memscan, sysutils, CEFuncProc, classes, foundlisthelper;


function findaob(aobstring: string): ptruint;
function getaoblist(aobstring: string; list: tstrings):boolean;

implementation

uses NewKernelHandler;


function getaoblist(aobstring: string; list: tstrings{; protectionflags: TProtectionflags; alignmenttype: TFastScanMethod; alignmentparam: string }):boolean;
var
  ms: tmemscan;
  x: ptruint;
  max: ptrUint;

  foundlist: Tfoundlist;
  count: integer;
  i: integer;
begin
  list.clear;
  ms:=tmemscan.create(nil);
  foundlist:=tfoundlist.create(nil, ms);

  {$ifdef cpu64}
  if processhandler.is64Bit then
  begin
    {$ifdef darwin}
      max:=qword($ffffffffffffffff)
    {$else}
      max:=qword($7fffffffffffffff)
    {$endif}

  end
  else
  {$endif}
  begin
    if Is64bitOS then
      max:=$ffffffff
    else
      max:=$7fffffff;
  end;

  ms.firstscan(soExactValue, vtByteArray, rtRounded, aobstring, '', 0, max, false, true,  false, false, false,false, fsmAligned);

  ms.waittilldone; //wait till it's finished scanning


  count:=foundlist.Initialize(8,nil);

  for i:=0 to count-1 do
  begin
    x:=foundlist.GetAddress(i);
    list.Add(inttohex(x,8));
  end;

  foundlist.Deinitialize;

  foundlist.free;
  ms.free;

  result:=list.count>0;
end;


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

  ms.firstscan(soExactValue, vtByteArray, rtRounded, aobstring, '', 0, max, false, true, false, false, false,false, fsmAligned);

  ms.waittilldone; //wait till it's finished scanning
  if ms.GetOnlyOneResult(x) then
    result:=x
  else
    result:=0;

  ms.free;
end;

end.
