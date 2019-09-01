unit simpleaobscanner;

{$MODE Delphi}

{
Will search for an array of bytes and returns as soon as it finds it
}

interface

uses LCLIntf, memscan, sysutils, CEFuncProc, classes, foundlisthelper, commonTypeDefs;


function findaobInModule(modulename: string; aobstring: string; protectionflags: string=''; alignmenttype: TFastScanMethod=fsmNotAligned; alignmentparam: string=''; isUnique: boolean=false): ptruint;
function findaob(aobstring: string; protectionflags: string=''; alignmenttype: TFastScanMethod=fsmNotAligned; alignmentparam: string=''; isUnique: boolean=false): ptruint;
function getaoblist(aobstring: string; list: tstrings; protectionflags: string=''; alignmenttype: TFastScanMethod=fsmNotAligned; alignmentparam: string=''):boolean;

implementation

uses NewKernelHandler, symbolhandler, symbolhandlerstructs, ProcessHandlerUnit;


function getaoblist(aobstring: string; list: tstrings; protectionflags: string=''; alignmenttype: TFastScanMethod=fsmNotAligned; alignmentparam: string=''):boolean;
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
  ms.parseProtectionflags(protectionflags);

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

  ms.firstscan(soExactValue, vtByteArray, rtRounded, aobstring, '', 0, max, true,  false, false, false, alignmenttype, alignmentparam);
  ms.waittillreallydone; //wait till it's finished scanning AND saving


  count:=foundlist.Initialize(vtByteArray,nil);

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

function AsyncAOBScan(modulename: string; aobstring: string; protectionflags: string=''; alignmenttype: TFastScanMethod=fsmNotAligned; alignmentparam: string=''; isunique: boolean=false): TMemScan;
//starts a scan and returns a memscan object.
//It's recommended to use finishAOBScan(ms) to get the address and free the object
var
  ms: tmemscan;
  minaddress: ptruint;
  maxaddress: ptrUint;
  mi: TModuleinfo;
begin
  result:=nil;
  ms:=tmemscan.create(nil);
  ms.parseProtectionflags(protectionflags);
  ms.onlyone:=true;

  if modulename='' then
  begin
    minaddress:=0;
    {$ifdef cpu64}
    if processhandler.is64Bit then
      maxaddress:=qword($7fffffffffffffff)
    else
    {$endif}
    begin
      if Is64bitOS then
        maxaddress:=$ffffffff
      else
        maxaddress:=$7fffffff;
    end;
  end
  else
  begin
    //get the modulebase and modulesize and use those as min and max
    if symhandler.getmodulebyname(modulename, mi) then
    begin
      minaddress:=mi.baseaddress;
      maxaddress:=mi.baseaddress+mi.basesize;
    end
    else
      exit;
  end;

  ms.firstscan(soExactValue, vtByteArray, rtRounded, aobstring, '', minaddress, maxaddress, true, false, false, false, fsmNotAligned);
  result:=ms;
end;

function FinishAOBScan(ms: TMemscan): integer;
{scans the game's memory for aobstring and returns the pointer if found. returns 0 if not found}
var x: ptruint;
begin
  result:=0;
  ms.waittilldone; //wait till it's finished scanning
  if ms.GetOnlyOneResult(x) then
    result:=x;

  ms.free;
end;

function findaobInModule(modulename: string; aobstring: string; protectionflags: string=''; alignmenttype: TFastScanMethod=fsmNotAligned; alignmentparam: string=''; isUnique: boolean=false): ptruint;
var ms: TMemscan;
begin
  ms:=AsyncAOBScan(modulename, aobstring, protectionflags, alignmenttype, alignmentparam, isUnique);
  result:=finishAOBScan(ms);
end;

function findaob(aobstring: string; protectionflags: string=''; alignmenttype: TFastScanMethod=fsmNotAligned; alignmentparam: string=''; isunique: boolean=false): ptruint;
begin
  result:=findaobInModule('', aobstring, protectionflags, alignmenttype, alignmentparam);
end;


end.
