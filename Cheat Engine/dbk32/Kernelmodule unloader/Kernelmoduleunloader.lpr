program Kernelmoduleunloader;

{$MODE Delphi}

uses
  windows,
  {winsvc,}
  SysUtils,
  multicpuexecution in '..\multicpuexecution.pas';

{$R ic.res}

const FILE_ANY_ACCESS=0;
const FILE_SPECIAL_ACCESS=FILE_ANY_ACCESS;
const FILE_READ_ACCESS=$0001;
const FILE_WRITE_ACCESS=$0002;

const METHOD_BUFFERED=    0;
const METHOD_IN_DIRECT=   1;
const METHOD_OUT_DIRECT=  2;
const METHOD_NEITHER=     3;
const FILE_DEVICE_UNKNOWN=$00000022;
const IOCTL_UNKNOWN_BASE=FILE_DEVICE_UNKNOWN;

type TIsWow64Process=function (processhandle: THandle; var isWow: BOOL): BOOL; stdcall;


var kernel32dll: thandle;
    IsWow64Process: TIsWow64Process;
    iswow64: bool;


var
  hSCManager: SC_HANDLE;
  hservice:   SC_HANDLE;
  hDevice:    THANDLE;
  cc:         dword;
  x:          dword;
  servicestatus:_service_status;
  ok,ok2:     boolean;
  count:      integer;
  setup: boolean;


function noIsWow64(processhandle: THandle; var isWow: BOOL): BOOL; stdcall;
begin
  if @isWow<>nil then
    isWow:=false;
    
  result:=false;
end;

  
function UpperCase(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then Dec(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

function CTL_CODE(DeviceType, Func, Method, Access : integer) : integer;
begin
  Result := (DeviceType shl 16) or (Access shl 14) or (Func shl 2) or Method;
end;


function disableGlobalDebug(parameters: pointer): BOOL; stdcall;
{
Sets the global debug flag to 0 for the current cpu
}
var state: BOOL;
begin
  state:=false;
  cc:=CTL_CODE(IOCTL_UNKNOWN_BASE, $0830 {IOCTL_CE_SETGLOBALDEBUGSTATE}, METHOD_BUFFERED, FILE_READ_ACCESS or FILE_WRITE_ACCESS);
  result:=deviceiocontrol(hdevice,cc,@state,sizeof(state),nil,0,x,nil);
  if not result then
  begin
    if not setup then messagebox(0,'Failure stopping the debugging','driver error',mb_ok or MB_ICONERROR);
    ExitProcess(1);
  end;
end;

function disableInterruptHooks(parameters: pointer): BOOL; stdcall;
{
Sets the global debug flag to 0 for the current cpu
}
var state: BOOL;
begin
  state:=false;
  cc:=CTL_CODE(IOCTL_UNKNOWN_BASE, $083b {IOCTL_CE_UNHOOKALLINTERRUPTS}, METHOD_BUFFERED, FILE_READ_ACCESS or FILE_WRITE_ACCESS);
  result:=deviceiocontrol(hdevice,cc,nil,0,nil,0,x,nil);
  if not result then
  begin
    if not setup then messagebox(0,'Failure stopping the debugging','driver error',mb_ok or MB_ICONERROR);
    ExitProcess(1);
  end;
end;

var f,driverdat: textfile;
    s,s2: string;
    i: integer;


    dataloc: string;
    apppath: pchar;

{$R *.res}

begin
  kernel32dll:=loadlibrary('kernel32.dll');
  loadlibrary('user32.dll');
  loadlibrary('comctl32.dll');
  IsWow64Process:=GetProcAddress(kernel32dll, 'IsWow64Process');
  if not assigned(IsWow64Process) then IsWow64Process:=noIsWow64;

  IsWow64Process(getcurrentprocess,iswow64);

  outputdebugstring('Kernelmodule unloader');
  if iswow64 then
    outputdebugstring('Running in wow64');

  setup:=false;
  if ParamCount>0 then
  begin
    for i:=1 to paramcount do
    begin
      s:=paramstr(i);
      if uppercase(s)='/SETUP' then
        setup:=true;
    end;
  end;


  count:=0;
  ok:=false;

  if setup then
    outputdebugstring('Setup. So do not show messages')
  else
    outputdebugstring('Setup is false');

  while (not ok) and (count<5) do
  begin
    outputdebugstring('attempting to unload');

    hSCManager := OpenSCManager(nil, nil, GENERIC_READ or GENERIC_WRITE);
    if hscmanager<>0 then
    begin
      outputdebugstring('SCManager opened');
      hservice:=OpenService(hSCManager, 'DBKDRVR', SERVICE_ALL_ACCESS);
      if hservice<>0 then
      begin
        outputdebugstring('Opened service DBKDRVR');
        hDevice := FileCreate('\\.\DBKDRVR'); { *Converted from CreateFile*  }

        if hdevice<>INVALID_HANDLE_VALUE then
        begin
          //unhook (in case it was protecting something)
          cc:=CTL_CODE(IOCTL_UNKNOWN_BASE, $080e {unprotect}, METHOD_BUFFERED, FILE_READ_ACCESS or FILE_WRITE_ACCESS);
          ok:=deviceiocontrol(hdevice,cc,@x,4,@x,4,x,nil);
          FileClose(hdevice); { *Converted from CloseHandle*  }
        end else ok:=false;

        ok:=ControlService(hService, SERVICE_CONTROL_STOP, serviceStatus);
        ok2:=DeleteService(hService);
      end;

      hservice:=OpenService(hSCManager, 'DRIVER1111', SERVICE_ALL_ACCESS);
      if hservice<>0 then
      begin
        outputdebugstring('Opened service DRIVER1111');
        hDevice := FileCreate('\\.\DRIVER1111'); { *Converted from CreateFile*  }

        if hdevice<>INVALID_HANDLE_VALUE then
        begin
          //unhook (in case it was protecting something)
          cc:=CTL_CODE(IOCTL_UNKNOWN_BASE, $080e {unprotect}, METHOD_BUFFERED, FILE_READ_ACCESS or FILE_WRITE_ACCESS);
          ok:=deviceiocontrol(hdevice,cc,@x,4,@x,4,x,nil);
          FileClose(hdevice); { *Converted from CloseHandle*  }
        end else ok:=false;

        ok:=ControlService(hService, SERVICE_CONTROL_STOP, serviceStatus);
        ok2:=DeleteService(hService);
      end;


      hService := OpenService(hSCManager, 'CEDRIVER50', SERVICE_ALL_ACCESS);
      if hservice<>0 then
      begin
        outputdebugstring('Opened service CEDRIVER50');
        hDevice := FileCreate('\\.\CEDRIVER50'); { *Converted from CreateFile*  }

        if hdevice<>INVALID_HANDLE_VALUE then
        begin
          //unhook (in case it was protecting something)
          cc:=CTL_CODE(IOCTL_UNKNOWN_BASE, $080e {unprotect}, METHOD_BUFFERED, FILE_READ_ACCESS or FILE_WRITE_ACCESS);
          ok:=deviceiocontrol(hdevice,cc,@x,4,@x,4,x,nil);
          if not ok then
          begin
            if not setup then messagebox(0,'The driver was found and present. But it can''t unload itself right now','driver error',mb_ok or MB_ICONERROR);
          end;

          FileClose(hdevice); { *Converted from CloseHandle*  }
        end else ok:=false;

        ControlService(hService, SERVICE_CONTROL_STOP, serviceStatus);
        ok:=DeleteService(hService);

        CloseServiceHandle(hservice);
      end;


      hService := OpenService(hSCManager, 'CEDRIVER51', SERVICE_ALL_ACCESS);
      if hservice<>0 then
      begin
        outputdebugstring('Opened service CEDRIVER51');
        hDevice := FileCreate('\\.\CEDRIVER51'); { *Converted from CreateFile*  }

        if hdevice<>INVALID_HANDLE_VALUE then
        begin
          //unhook (in case it was protecting something)
          cc:=CTL_CODE(IOCTL_UNKNOWN_BASE, $080e {unprotect}, METHOD_BUFFERED, FILE_READ_ACCESS or FILE_WRITE_ACCESS);
          ok:=deviceiocontrol(hdevice,cc,@x,4,@x,4,x,nil);
          if not ok then
          begin
            if not setup then messagebox(0,'The driver was found and present. But it can''t unload itself right now','driver error',mb_ok or MB_ICONERROR);
          end;

          FileClose(hdevice); { *Converted from CloseHandle*  }
        end else ok:=false;

        ControlService(hService, SERVICE_CONTROL_STOP, serviceStatus);
        ok:=DeleteService(hService);

        CloseServiceHandle(hservice);
      end;

      hService := OpenService(hSCManager, 'CEDRIVER52', SERVICE_ALL_ACCESS);
      if hservice<>0 then
      begin
        outputdebugstring('Opened service CEDRIVER52');
        hDevice := FileCreate('\\.\CEDRIVER52'); { *Converted from CreateFile*  }

        if hdevice<>INVALID_HANDLE_VALUE then
        begin
          //unhook (in case it was protecting something)
          cc:=CTL_CODE(IOCTL_UNKNOWN_BASE, $080e {unprotect}, METHOD_BUFFERED, FILE_READ_ACCESS or FILE_WRITE_ACCESS);
          ok:=deviceiocontrol(hdevice,cc,@x,4,@x,4,x,nil);
          if not ok then
          begin
            if not setup then messagebox(0,'The driver was found and present. But it can''t unload itself right now','driver error',mb_ok or MB_ICONERROR);
          end;

          FileClose(hdevice); { *Converted from CloseHandle*  }
        end else ok:=false;

        ControlService(hService, SERVICE_CONTROL_STOP, serviceStatus);
        ok:=DeleteService(hService);

        CloseServiceHandle(hservice);
      end;


      hService := OpenService(hSCManager, 'CEDRIVER55', SERVICE_ALL_ACCESS);
      if hservice<>0 then
      begin
        outputdebugstring('Opened service CEDRIVER52');
        hDevice := FileCreate('\\.\CEDRIVER52'); { *Converted from CreateFile*  }

        if hdevice<>INVALID_HANDLE_VALUE then
        begin
          //unhook (in case it was protecting something)
          outputdebugstring('Calling disableglobaldebug');
          foreachcpu(disableGlobalDebug,nil);

          outputdebugstring('calling disableInterruptHooks');
          foreachcpu(disableInterruptHooks,nil);

          FileClose(hdevice); { *Converted from CloseHandle*  }
        end else ok:=false;

        ControlService(hService, SERVICE_CONTROL_STOP, serviceStatus);
        ok:=DeleteService(hService);

        CloseServiceHandle(hservice);
      end;


      try
        s:='ULTIMAP2';
        getmem(apppath,250);
        GetModuleFileName(0,apppath,250);

        if iswow64 then
          dataloc:=extractfilepath(apppath)+'driver64.dat'
        else
          dataloc:=extractfilepath(apppath)+'driver.dat';

        if FileExists(dataloc) then
        begin
          try
            assignfile(driverdat,dataloc);
            reset(driverdat);
            readln(driverdat,s2);  //dbk servicename
            readln(driverdat,s2);  //processeventname
            readln(driverdat,s2);  //threadeventname
            readln(driverdat,s2);  //sysfile
            readln(driverdat,s2);  //vmx_p1_txt
            readln(driverdat,s2);  //vmx_p2_txt
            readln(driverdat,s);  //ultimapservicename
            closefile(driverdat);
          except
          end;
        end;
      finally
        freemem(apppath);
      end;

      hService := OpenService(hSCManager, pchar(s), SERVICE_ALL_ACCESS);
      if hservice<>0 then
      begin
        outputdebugstring(pchar('Opened service '+s));
        hDevice := CreateFile(pchar('\\.\'+s),
                      GENERIC_READ or GENERIC_WRITE,
                      FILE_SHARE_READ or FILE_SHARE_WRITE,
                      nil,
                      OPEN_EXISTING,
                      FILE_FLAG_OVERLAPPED,
                      0);

        if hdevice<>INVALID_HANDLE_VALUE then
        begin
          FileClose(hdevice);
        end;

        if ControlService(hService, SERVICE_CONTROL_STOP, serviceStatus)=false then
          outputdebugstring('Failed to stop')
        else
          outputdebugstring('Stopped the service');

        if DeleteService(hService)=false then
          OutputDebugString('Failed to delete')
        else
          outputdebugstring('Delete the service');

        CloseServiceHandle(hservice);
      end;

      try
        s:='CEDRIVER60';
        getmem(apppath,250);
        GetModuleFileName(0,apppath,250);

        if iswow64 then
          dataloc:=extractfilepath(apppath)+'driver64.dat'
        else
          dataloc:=extractfilepath(apppath)+'driver.dat';

        if FileExists(dataloc) then
        begin
          assignfile(driverdat,dataloc);
          reset(driverdat);
          readln(driverdat,s);
          closefile(driverdat);
        end;
      finally
        freemem(apppath);
      end;

      hService := OpenService(hSCManager, pchar(s), SERVICE_ALL_ACCESS);
      if hservice<>0 then
      begin
        outputdebugstring(pchar('Opened service '+s));
        hDevice := CreateFile(pchar('\\.\'+s),
                      GENERIC_READ or GENERIC_WRITE,
                      FILE_SHARE_READ or FILE_SHARE_WRITE,
                      nil,
                      OPEN_EXISTING,
                      FILE_FLAG_OVERLAPPED,
                      0);

        if hdevice<>INVALID_HANDLE_VALUE then
        begin
          //unhook (in case it was protecting something)
          outputdebugstring('Calling disableglobaldebug');
          foreachcpu(disableGlobalDebug,nil);

          outputdebugstring('calling disableInterruptHooks');
          foreachcpu(disableInterruptHooks,nil);

          FileClose(hdevice); { *Converted from CloseHandle*  }
        end else ok:=false;

        ControlService(hService, SERVICE_CONTROL_STOP, serviceStatus);
        ok:=DeleteService(hService);

        CloseServiceHandle(hservice);
      end else
      if count=0 then
      begin
        outputdebugstring('count=0');
        if not setup then
        begin
         // outputdebugstring('showing message that the driver isn''t in the registry');
          messageboxA(0,'Failed to find the driver in the registry','driver error',mb_ok);
         // outputdebugstring('AFTER the messagebox');
        end
        else outputdebugstring('setup=true');
        exit;
      end;

      CloseServiceHandle(hSCManager);
    end;

    if not ok then
    begin
      sleep(1000);
      inc(count);
    end;
  end;


  outputdebugstring('near the end');
  if not setup then
  begin
    if ok or ok2 then
      messagebox(0,'The driver is successfully unloaded.','dbk32.sys unloaded',MB_ICONINFORMATION or MB_OK)
    else
      messagebox(0,'The driver failed to unload or is already unloaded. If you think it''s still loaded then reboot and run the unloader again.','DBK32.sys unloader',MB_ICONERROR or MB_OK)
  end;
end.
