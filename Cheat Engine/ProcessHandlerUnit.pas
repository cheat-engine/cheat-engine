unit ProcessHandlerUnit;

{$MODE Delphi}

{
Will handle all process specific stuff like openening and closing a process
The ProcessHandler variable will be in cefuncproc, but a tabswitch to another
process will set it to the different tab's process
}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  {$ifndef jni}LCLIntf, {$endif}
  newkernelhandler, classes, sysutils;

type
  TSystemArchitecture=(archX86=0, archArm=1);
  TOperatingsystemABI=(abiWindows=0, abiSystemV=1);

type TProcessHandler=class
  private
    fis64bit: boolean;
    fprocesshandle: THandle;
    fpointersize: integer;
    fSystemArchitecture: TSystemArchitecture;
    fOSABI: TOperatingsystemABI;  //for c-code
    fHexDigitPreference: integer;
    procedure setIs64bit(state: boolean);
    procedure setProcessHandle(processhandle: THandle);
  public
    processid: dword;


    procedure Open;
    function isNetwork: boolean;  //perhaps name it isLinux ?
    procedure overridePointerSize(newsize: integer);

    property is64Bit: boolean read fIs64Bit write setIs64bit;
    property pointersize: integer read fPointersize;
    property processhandle: THandle read fProcessHandle write setProcessHandle;
    property SystemArchitecture: TSystemArchitecture read fSystemArchitecture write fSystemArchitecture;
    property OSABI: TOperatingsystemABI read fOSABI;
    property hexdigitpreference: integer read fHexDigitPreference;
end;


var processhandler: TProcessHandler;

function processhandle: THandle; inline;
function processid: dword; inline;


implementation

{$ifdef jni}
uses networkinterface, networkInterfaceApi;
{$else}
uses LuaHandler, mainunit, networkinterface, networkInterfaceApi, ProcessList, lua, FileUtil;
{$endif}

procedure TProcessHandler.overridePointerSize(newsize: integer);
begin
  fpointersize:=newsize;
end;

function TProcessHandler.isNetwork: boolean;
begin
  result:=(((processhandle shr 24) and $ff)=$ce) and (getConnection<>nil);
end;

procedure TProcessHandler.setIs64bit(state: boolean);
begin
  fis64bit:=state;
  if state then
    fpointersize:=8
  else
    fpointersize:=4;

  fhexdigitpreference:=fpointersize*2;
end;

procedure TProcessHandler.setProcessHandle(processhandle: THandle);
var
  c: TCEConnection;
  arch: integer;
  abi: integer;
begin
  //outputdebugstring('TProcessHandler.setProcessHandle');
  if (fprocesshandle<>0) and (fprocesshandle<>getcurrentprocess) and (processhandle<>getcurrentprocess) then
  begin
    try
      closehandle(fprocesshandle);
    except //debugger issue
    end;
    fprocesshandle:=0;
  end;

  fprocesshandle:=processhandle;

  c:=getConnection;
  if c<>nil then
  begin
    arch:=c.getArchitecture(fprocesshandle);
    case arch of
      0:   //i386
      begin
        fSystemArchitecture:=archX86;
        setIs64Bit(false);
      end;

      1: //x86_64
      begin
        fSystemArchitecture:=archX86;
        setIs64Bit(true);
      end;

      2: //arm
      begin
        fSystemArchitecture:=archArm;
        setIs64Bit(false);
      end;

      3: //arm64
      begin
        fSystemArchitecture:=archArm;
        setIs64Bit(true);
      end;
    end;

    abi:=c.GetABI;
    case abi of
      0: fOSABI:=abiWindows;
      1: fOSABI:=abiSystemV;
    end;

  end
  else
  begin
    //outputdebugstring('setProcessHandle not windows');

    {$ifdef darwin}
    if MacIsArm64 then  //rosetta2 or I finally ported it to full armv8
      fSystemArchitecture:=archArm;
    {$else}
    fSystemArchitecture:=archX86;
    {$endif}
    {$ifdef windows}
    fOSABI:=abiWindows;
    {$else}
    fOSABI:=abiSystemV;
    {$endif}

    setIs64Bit(newkernelhandler.Is64BitProcess(fProcessHandle));
  end;

  {$ifdef ARMTEST}
  fSystemArchitecture:=archArm;
  fOSABI:=abiSystemV;
  setIs64Bit(false);
  {$endif}

  if processhandle<>0 then
  begin
    outputdebugstring('calling open');
    open;
  end;

end;

procedure TProcessHandler.Open;
var mn: string;
begin
  outputdebugstring('TProcessHandler.Open');
  //GetFirstModuleNa
  {$ifndef jni}

  if processid<>0 then
  begin
    mn:=GetFirstModuleName(processid);

    lua_pushstring(luavm, pchar(extractfilename(mn)));
    lua_setglobal(luavm, 'process');
  end;


  LUA_functioncall('onOpenProcess', [ptruint(processid)]);   //todo: Change to a callback array/list
  {$endif}
end;

function processhandle: THandle; inline;
begin
  result:=processhandler.processhandle;
end;

function processid: dword; inline;
begin
  result:=processhandler.processid;
end;

initialization
  processhandler:=TProcessHandler.create;

end.


