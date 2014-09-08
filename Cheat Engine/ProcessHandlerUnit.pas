unit ProcessHandlerUnit;

{$MODE Delphi}

{
Will handle all process specific stuff like openening and closing a process
The ProcessHandler variable will be in cefuncproc, but a tabswitch to another
process will set it to the different tab's process
}

interface

uses {$ifndef jni}LCLIntf, {$endif}newkernelhandler, classes;

type
  TSystemArchitecture=(archX86=0, archArm=1);

type TProcessHandler=class
  private
    fis64bit: boolean;
    fprocesshandle: THandle;
    fpointersize: integer;
    fSystemArchitecture: TSystemArchitecture;
    procedure setIs64bit(state: boolean);
    procedure setProcessHandle(processhandle: THandle);
  public
    processid: dword;


    procedure Open;
    function isNetwork: boolean;  //perhaps name it isLinux ?
    property is64Bit: boolean read fIs64Bit;
    property pointersize: integer read fPointersize;
    property processhandle: THandle read fProcessHandle write setProcessHandle;
    property SystemArchitecture: TSystemArchitecture read fSystemArchitecture;
end;


var processhandler: TProcessHandler;

function processhandle: THandle; inline;
function processid: dword; inline;


implementation

{$ifdef jni}
uses networkinterface, networkInterfaceApi;
{$else}
uses LuaHandler, mainunit, networkinterface, networkInterfaceApi;
{$endif}

function TProcessHandler.isNetwork: boolean;
begin
  result:=(((processhandle shr 24) and $ff)=$ce) and (getConnection<>nil);
end;

procedure TProcessHandler.setIs64bit(state: boolean);
begin
  fis64bit:=state;
  if state then
  begin
    fpointersize:=8;
  end
  else
  begin
    fpointersize:=4;
  end;
end;

procedure TProcessHandler.setProcessHandle(processhandle: THandle);
var c: TCEConnection;
  arch: integer;
begin
  fprocesshandle:=processhandle;

  c:=getConnection;
  if c<>nil then
  begin
    arch:=c.getArchitecture;
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

      3: //arm64 (untested, not seen yet)
      begin
        fSystemArchitecture:=archArm;
        setIs64Bit(true);
      end;
    end;

  end
  else
  begin
    fSystemArchitecture:=archX86;
    setIs64Bit(newkernelhandler.Is64BitProcess(fProcessHandle));
  end;

  {$ifdef ARMTEST}
  fSystemArchitecture:=archArm;
  setIs64Bit(false);
  {$endif}

  if processhandle<>0 then
    open;

    {if GetCurrentThreadId<>MainThreadID then
        TThread.Synchronize(nil, open) }




    //(synchronize(open));
//  if (mainform<>nil) and (mainform.addresslist<>nil) then
//    mainform.addresslist.needsToReinterpret:=true;
end;

procedure TProcessHandler.Open;
begin
  {$ifndef jni}
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

