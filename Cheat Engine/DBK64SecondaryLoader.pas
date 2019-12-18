unit DBK64SecondaryLoader;

{$mode delphi}

interface

{$IFDEF windows}
uses
  jwawindows, windows, Classes, SysUtils, cefuncproc, NewKernelHandler, dialogs;

function SecondaryDriverLoad: THandle;
function SecondaryDeviceIoControl(dwIoControlCode: DWORD; lpInBuffer: Pointer; nInBufferSize: DWORD; lpOutBuffer: Pointer; nOutBufferSize: DWORD; var lpBytesReturned: DWORD; lpOverlapped: POverlapped): BOOL; stdcall;
{$ENDIF}

implementation

{$IFDEF windows}
uses dbk32functions, vmxfunctions, ManualModuleLoader, ctypes, Globals;

resourcestring
  rsSeemsLikeDbvmIsntLoadedAfterAll = 'seems like dbvm isn''t loaded after all';
  rsSuccessTheDriverHasBeenLoaded = 'Success. The driver has been loaded thanks to dbvm';
  rsTheDriverFailedToInitialize = 'The driver failed to initialize';
  rsModuleLoaderDailedToMDbk64sysToMemoryap = 'ModuleLoader failed to map dbk64.sys to memory';
  rsErrorWhileTryingToLoadTheDriverAtPart = 'Error while trying to load the driver at part ';

const IRP_MJ_CREATE                   =$00;
const IRP_MJ_CREATE_NAMED_PIPE        =$01;
const IRP_MJ_CLOSE                    =$02;
const IRP_MJ_READ                     =$03;
const IRP_MJ_WRITE                    =$04;
const IRP_MJ_QUERY_INFORMATION        =$05;
const IRP_MJ_SET_INFORMATION          =$06;
const IRP_MJ_QUERY_EA                 =$07;
const IRP_MJ_SET_EA                   =$08;
const IRP_MJ_FLUSH_BUFFERS            =$09;
const IRP_MJ_QUERY_VOLUME_INFORMATION =$0a;
const IRP_MJ_SET_VOLUME_INFORMATION   =$0b;
const IRP_MJ_DIRECTORY_CONTROL        =$0c;
const IRP_MJ_FILE_SYSTEM_CONTROL      =$0d;
const IRP_MJ_DEVICE_CONTROL           =$0e;
const IRP_MJ_INTERNAL_DEVICE_CONTROL  =$0f;
const IRP_MJ_SHUTDOWN                 =$10;
const IRP_MJ_LOCK_CONTROL             =$11;
const IRP_MJ_CLEANUP                  =$12;
const IRP_MJ_CREATE_MAILSLOT          =$13;
const IRP_MJ_QUERY_SECURITY           =$14;
const IRP_MJ_SET_SECURITY             =$15;
const IRP_MJ_POWER                    =$16;
const IRP_MJ_SYSTEM_CONTROL           =$17;
const IRP_MJ_DEVICE_CHANGE            =$18;
const IRP_MJ_QUERY_QUOTA              =$19;
const IRP_MJ_SET_QUOTA                =$1a;
const IRP_MJ_PNP                      =$1b;

const IRP_MJ_MAXIMUM_FUNCTION = $1b;


type DRIVER_OBJECT=record
  _Type: cshort;
  Size: cshort;
  DeviceObject: pointer;
  Flags: ULONG;

  DriverStart: pointer;
  DriverSize: ULONG;
  DriverSection: PVOID;
  DriverExtension: pointer;
  DriverName: UNICODE_STRING ;
  HardwareDatabase: PUNICODE_STRING;
  FastIoDispatch: pointer;

  DriverInit: pointer;
  DriverStartIo: pointer;
  DriverUnload: pointer;
  MajorFunction: array [0..IRP_MJ_MAXIMUM_FUNCTION] of pointer;

end;

var dobject: DRIVER_OBJECT;

function SecondaryDriverLoad: THandle;
var ml: TModuleLoader;
  r: integer;
  part: integer;
begin
  part:=0;
  result:=INVALID_HANDLE_VALUE;


  //ShowMessage('SecondaryDriverLoad');

  try
    part:=1;
   // ShowMessage('Part 1');

    //load the 64 bit driver
    if dbvm_version =0 then
    begin
      showmessage(rsSeemsLikeDbvmIsntLoadedAfterAll);
      exit;
    end;

    part:=2;
    //ShowMessage('Part 2');

    ml:=TModuleLoader.create(CheatEngineDir+'dbk64.sys');

    part:=3;
   // ShowMessage('Part 3');
    if ml.loaded then
    begin
      part:=4;
      ZeroMemory(@dobject, sizeof(dobject));
      r:=dbvm_executeDriverEntry(pointer(ml.entrypoint), @dobject,nil);
      part:=5;
      if r=0 then
      begin
        result:=$fff00fff;
        part:=6;
        ShowMessage(rsSuccessTheDriverHasBeenLoaded);
      end
      else
        showMessage(rsTheDriverFailedToInitialize);
      part:=7;
    end
    else
      showmessage(rsModuleLoaderDailedToMDbk64sysToMemoryap);

    part:=8;
  except
    on e: exception do
      showmessage(rsErrorWhileTryingToLoadTheDriverAtPart+inttostr(part)+': '+e.message);
  end;
end;

function SecondaryDeviceIoControl(dwIoControlCode: DWORD; lpInBuffer: Pointer; nInBufferSize: DWORD; lpOutBuffer: Pointer; nOutBufferSize: DWORD; var lpBytesReturned: DWORD; lpOverlapped: POverlapped): BOOL; stdcall;
begin
  outputdebugstring('SecondaryDeviceIoControl: dwIoControlCode='+inttohex(dwIoControlCode,1));

  result:=dbvm_executeDispatchIoctl(pointer(dobject.MajorFunction[IRP_MJ_DEVICE_CONTROL]), @dobject, dwIoControlCode, lpInBuffer, nInBufferSize, lpOutBuffer, nOutBufferSize, @lpBytesReturned);
  if result then
    outputdebugstring('Returned true')
  else
    outputdebugstring('Returned false');
end;
{$ENDIF}

end.

