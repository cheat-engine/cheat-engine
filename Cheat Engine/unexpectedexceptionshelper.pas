unit UnexpectedExceptionsHelper;

{$mode delphi}

interface

uses
  jwawindows, windows, Classes, SysUtils;

type
  TUnexpectedExceptionAction=(ueaIgnore, ueaBreak, ueaBreakIfInRegion);
var
  UnexpectedExceptionAction: TUnexpectedExceptionAction=ueaIgnore;
  function IsInUnexpectedExceptionRegion(Address: ptruint): boolean;
  procedure AddUnexpectedExceptionRegion(Address: ptruint; size: integer);
  function RemoveUnexpectedExceptionRegion(Address: ptruint; size: integer): boolean;
  procedure getUnexpectedExceptionRegionList(l: tstrings);
  procedure ClearUnexpectedExceptionRegions;

  function IsIgnoredExceptionCode(code: DWORD): boolean;
  function AddIgnoredExceptionCode(code: DWORD): boolean;
  function RemoveIgnoredExceptionCode(code: DWORD): boolean;
  procedure getIgnoredExceptionCodeList(l:  tstrings);

  procedure SaveIgnoredExceptionCodeList;
  procedure LoadIgnoredExceptionCodeList;

  function ExceptionCodeToString(code: dword): string;


implementation

uses syncobjs, CEFuncProc, commonTypeDefs, maps, registry,
  frmExceptionRegionListUnit, frmExceptionIgnoreListUnit;

var
  UnexpectedExceptionListCS: TCriticalsection;
  UnexpectedExceptionList: Array of TMemoryRegionSmall;

  IgnoredExceptionCodeListCS: TCriticalsection;
  IgnoredExceptionCodeList: TMap;

procedure ClearUnexpectedExceptionRegions;
begin
  UnexpectedExceptionListCS.enter;
  setlength(UnexpectedExceptionList,0);
  UnexpectedExceptionListCS.leave;
end;

function IsInUnexpectedExceptionRegion(Address: ptruint): boolean;
var i: integer;
begin
  UnexpectedExceptionListCS.enter;

  try
    for i:=0 to length(UnexpectedExceptionList)-1 do
      if InRangeX(address, UnexpectedExceptionList[i].address, UnexpectedExceptionList[i].address+UnexpectedExceptionList[i].size) then
        exit(true);
  finally
    UnexpectedExceptionListCS.leave;
  end;
end;

procedure getUnexpectedExceptionRegionList(l: tstrings);
var i: integer;
begin
  l.Clear;
  UnexpectedExceptionListCS.enter;
  try
    for i:=0 to length(UnexpectedExceptionList)-1 do
      l.add(inttohex(UnexpectedExceptionList[i].Address,8)+' - '+inttohex(UnexpectedExceptionList[i].Address+UnexpectedExceptionList[i].size,8));
  finally
    UnexpectedExceptionListCS.leave;
  end;
end;

procedure AddUnexpectedExceptionRegion(Address: ptruint; size: integer);
var i: integer;
begin
  UnexpectedExceptionListCS.enter;
  try
    for i:=0 to length(UnexpectedExceptionList)-1 do
    begin
      if (address=UnexpectedExceptionList[i].address) and (size=UnexpectedExceptionList[i].size) then
        exit; //already in the list
    end;

    setlength(UnexpectedExceptionList,length(UnexpectedExceptionList)+1);
    UnexpectedExceptionList[length(UnexpectedExceptionList)-1].address:=address;
    UnexpectedExceptionList[length(UnexpectedExceptionList)-1].size:=size;
  finally
    UnexpectedExceptionListCS.leave;
  end;

  if (GetCurrentThreadId=MainThreadID) and (frmExceptionRegionList<>nil) then
    frmExceptionRegionList.UpdateList;
end;

function RemoveUnexpectedExceptionRegion(Address: ptruint; size: integer):boolean;
var i,j: integer;
begin
  result:=false;
  UnexpectedExceptionListCS.enter;
  try
    for i:=0 to length(UnexpectedExceptionList)-1 do
      if (address=UnexpectedExceptionList[i].address) and ((size=0) or (size=UnexpectedExceptionList[i].size)) then
      begin
        for j:=i to length(UnexpectedExceptionList)-2 do
          UnexpectedExceptionList[i]:=UnexpectedExceptionList[i+1];

        setlength(UnexpectedExceptionList, length(UnexpectedExceptionList)-1);
        result:=true;
        exit;
      end;
  finally
    UnexpectedExceptionListCS.leave;
  end;

  if (GetCurrentThreadId=MainThreadID) and (frmExceptionRegionList<>nil) then
    frmExceptionRegionList.UpdateList;
end;


function IsIgnoredExceptionCode(code: DWORD): boolean;
begin
  IgnoredExceptionCodeListCS.enter;
  try
    result:=IgnoredExceptionCodeList.HasId(code);
  finally
    IgnoredExceptionCodeListCS.Leave;
  end;
end;

function AddIgnoredExceptionCode(code: DWORD): boolean;
var t: byte=1;
begin
  result:=false;
  IgnoredExceptionCodeListCS.enter;
  try
    if IsIgnoredExceptionCode(code)=false then
    begin
      IgnoredExceptionCodeList.Add(code,t);
      result:=true;
    end;
  finally
    IgnoredExceptionCodeListCS.Leave;
  end;

  if (GetCurrentThreadId=MainThreadID) and (frmExceptionIgnoreList<>nil) then
    frmExceptionIgnoreList.UpdateList;
end;

function RemoveIgnoredExceptionCode(code: DWORD): boolean;
begin
  result:=false;
  IgnoredExceptionCodeListCS.enter;
  try
    if IgnoredExceptionCodeList.HasId(code) then
    begin
      IgnoredExceptionCodeList.Delete(code);
      result:=true;
    end;
  finally
    IgnoredExceptionCodeListCS.Leave;
  end;

  if (GetCurrentThreadId=MainThreadID) and (frmExceptionIgnoreList<>nil) then
    frmExceptionIgnoreList.UpdateList;
end;

procedure getIgnoredExceptionCodeList(l:  tstrings);
var
  mi: TMapIterator;
  c: dword;
begin
  l.clear;
  IgnoredExceptionCodeListCS.Enter;
  try
    mi:=TMapIterator.Create(IgnoredExceptionCodeList);
    mi.First;
    while not mi.EOM do
    begin
      mi.GetID(c);
      l.Add(ExceptionCodeToString(c));
      mi.next;
    end;
  finally
    IgnoredExceptionCodeListCS.leave;
  end;

end;

procedure SaveIgnoredExceptionCodeList;
var
  reg: tregistry;
  l: tstringlist;
  i: integer;
begin
  reg:=Tregistry.Create;
  try
    if reg.OpenKey('\Software\Cheat Engine\Ignored Exceptions\',true) then
    begin
      try
        l:=tstringlist.create;

        reg.GetValueNames(l);

        for i:=0 to l.count-1 do
          reg.DeleteValue(l[i]);

        l.clear;


        getIgnoredExceptionCodeList(l);
        for i:=0 to l.count-1 do
          reg.WriteBool(l[i],true);

      except
      end;

      l.free;
    end;
  finally
    reg.free;
  end;
end;

procedure LoadIgnoredExceptionCodeList;
var
  reg: tregistry;
  l: tstringlist;
  i: integer;
begin
  reg:=Tregistry.Create;
  try
    if reg.OpenKey('\Software\Cheat Engine\Ignored Exceptions\',false) then
    begin
      l:=tstringlist.create;

      try
        reg.GetValueNames(l);

        for i:=0 to l.count-1 do
        begin
          try
            AddIgnoredExceptionCode(strtoint('0x'+copy(l[i],1,8)));
          except
          end;
        end;
      except
        OutputDebugString('No exceptioncode ignore list');
      end;

    end;
  finally
    reg.free;
  end;

end;


function ExceptionCodeToString(code: dword): string;
begin
  result:=inttohex(code,8);
  case code of
    EXCEPTION_ACCESS_VIOLATION: result:=result+' (EXCEPTION_ACCESS_VIOLATION)';
    EXCEPTION_SINGLE_STEP: result:=result+' (EXCEPTION_SINGLE_STEP)';
    STATUS_WX86_SINGLE_STEP: result:=result+' (STATUS_WX86_SINGLE_STEP)';
    EXCEPTION_BREAKPOINT: result:=result+' (EXCEPTION_BREAKPOINT)';
    STATUS_WX86_BREAKPOINT: result:=result+' (STATUS_WX86_BREAKPOINT)';
    EXCEPTION_ILLEGAL_INSTRUCTION: result:=result+' (EXCEPTION_ILLEGAL_INSTRUCTION)';
    EXCEPTION_PRIV_INSTRUCTION: result:=result+' (EXCEPTION_ILLEGAL_INSTRUCTION)';
  end;
end;


initialization
  UnexpectedExceptionListCS:=Tcriticalsection.create;
  IgnoredExceptionCodeListCS:=TCriticalSection.create;
  IgnoredExceptionCodeList:=TMap.Create(itu4,1);

  LoadIgnoredExceptionCodeList;

finalization
  SaveIgnoredExceptionCodeList;

  UnexpectedExceptionListCS.free;
  IgnoredExceptionCodeListCS.free;





end.

