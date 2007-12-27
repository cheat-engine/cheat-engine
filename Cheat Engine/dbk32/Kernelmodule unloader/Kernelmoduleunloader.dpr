program Kernelmoduleunloader;

uses
  windows, winsvc,SysUtils;

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


var
  hSCManager: SC_HANDLE;
  hservice:   SC_HANDLE;
  hDevice:    THANDLE;
  cc:         dword;
  x:          dword;
  servicestatus:_service_status;
  ok,ok2:     boolean;
  count:      integer;

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

var f,driverdat: textfile;
    s: string;
    i: integer;

    setup: boolean;
    dataloc: string;
    apppath: pchar;
begin
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

  while (not ok) and (count<5) do
  begin
    hSCManager := OpenSCManager(nil, nil, GENERIC_READ or GENERIC_WRITE);
    if hscmanager<>0 then
    begin
      hservice:=OpenService(hSCManager, 'DBKDRVR', SERVICE_ALL_ACCESS);
      if hservice<>0 then
      begin
        hDevice := CreateFile('\\.\DBKDRVR',
                      GENERIC_READ or GENERIC_WRITE,
                      FILE_SHARE_READ or FILE_SHARE_WRITE,
                      nil,
                      OPEN_EXISTING,
                      FILE_FLAG_OVERLAPPED,
                      0);

        if hdevice<>INVALID_HANDLE_VALUE then
        begin
          //unhook (in case it was protecting something)
          cc:=CTL_CODE(IOCTL_UNKNOWN_BASE, $080e {unprotect}, METHOD_BUFFERED, FILE_READ_ACCESS or FILE_WRITE_ACCESS);
          ok:=deviceiocontrol(hdevice,cc,@x,4,@x,4,x,nil);
          closehandle(hdevice);
        end else ok:=false;

        ok:=ControlService(hService, SERVICE_CONTROL_STOP, serviceStatus);
        ok2:=DeleteService(hService);
      end;

      hservice:=OpenService(hSCManager, 'DRIVER1111', SERVICE_ALL_ACCESS);
      if hservice<>0 then
      begin
        hDevice := CreateFile('\\.\DRIVER1111',
                      GENERIC_READ or GENERIC_WRITE,
                      FILE_SHARE_READ or FILE_SHARE_WRITE,
                      nil,
                      OPEN_EXISTING,
                      FILE_FLAG_OVERLAPPED,
                      0);

        if hdevice<>INVALID_HANDLE_VALUE then
        begin
          //unhook (in case it was protecting something)
          cc:=CTL_CODE(IOCTL_UNKNOWN_BASE, $080e {unprotect}, METHOD_BUFFERED, FILE_READ_ACCESS or FILE_WRITE_ACCESS);
          ok:=deviceiocontrol(hdevice,cc,@x,4,@x,4,x,nil);
          closehandle(hdevice);
        end else ok:=false;

        ok:=ControlService(hService, SERVICE_CONTROL_STOP, serviceStatus);
        ok2:=DeleteService(hService);
      end;


      hService := OpenService(hSCManager, 'CEDRIVER50', SERVICE_ALL_ACCESS);
      if hservice<>0 then
      begin
        hDevice := CreateFile('\\.\CEDRIVER50',
                      GENERIC_READ or GENERIC_WRITE,
                      FILE_SHARE_READ or FILE_SHARE_WRITE,
                      nil,
                      OPEN_EXISTING,
                      FILE_FLAG_OVERLAPPED,
                      0);

        if hdevice<>INVALID_HANDLE_VALUE then
        begin
          //unhook (in case it was protecting something)
          cc:=CTL_CODE(IOCTL_UNKNOWN_BASE, $080e {unprotect}, METHOD_BUFFERED, FILE_READ_ACCESS or FILE_WRITE_ACCESS);
          ok:=deviceiocontrol(hdevice,cc,@x,4,@x,4,x,nil);
          if not ok then
          begin
            if not setup then messagebox(0,'The driver was found and present. But it can''t unload itself right now','driver error',mb_ok or MB_ICONERROR);
          end;

          closehandle(hdevice);
        end else ok:=false;

        ControlService(hService, SERVICE_CONTROL_STOP, serviceStatus);
        ok:=DeleteService(hService);

        CloseServiceHandle(hservice);
      end;


      hService := OpenService(hSCManager, 'CEDRIVER51', SERVICE_ALL_ACCESS);
      if hservice<>0 then
      begin
        hDevice := CreateFile('\\.\CEDRIVER51',
                      GENERIC_READ or GENERIC_WRITE,
                      FILE_SHARE_READ or FILE_SHARE_WRITE,
                      nil,
                      OPEN_EXISTING,
                      FILE_FLAG_OVERLAPPED,
                      0);

        if hdevice<>INVALID_HANDLE_VALUE then
        begin
          //unhook (in case it was protecting something)
          cc:=CTL_CODE(IOCTL_UNKNOWN_BASE, $080e {unprotect}, METHOD_BUFFERED, FILE_READ_ACCESS or FILE_WRITE_ACCESS);
          ok:=deviceiocontrol(hdevice,cc,@x,4,@x,4,x,nil);
          if not ok then
          begin
            if not setup then messagebox(0,'The driver was found and present. But it can''t unload itself right now','driver error',mb_ok or MB_ICONERROR);
          end;

          closehandle(hdevice);
        end else ok:=false;

        ControlService(hService, SERVICE_CONTROL_STOP, serviceStatus);
        ok:=DeleteService(hService);

        CloseServiceHandle(hservice);
      end;

      hService := OpenService(hSCManager, 'CEDRIVER52', SERVICE_ALL_ACCESS);
      if hservice<>0 then
      begin
        hDevice := CreateFile('\\.\CEDRIVER52',
                      GENERIC_READ or GENERIC_WRITE,
                      FILE_SHARE_READ or FILE_SHARE_WRITE,
                      nil,
                      OPEN_EXISTING,
                      FILE_FLAG_OVERLAPPED,
                      0);

        if hdevice<>INVALID_HANDLE_VALUE then
        begin
          //unhook (in case it was protecting something)
          cc:=CTL_CODE(IOCTL_UNKNOWN_BASE, $080e {unprotect}, METHOD_BUFFERED, FILE_READ_ACCESS or FILE_WRITE_ACCESS);
          ok:=deviceiocontrol(hdevice,cc,@x,4,@x,4,x,nil);
          if not ok then
          begin
            if not setup then messagebox(0,'The driver was found and present. But it can''t unload itself right now','driver error',mb_ok or MB_ICONERROR);
          end;

          closehandle(hdevice);
        end else ok:=false;

        ControlService(hService, SERVICE_CONTROL_STOP, serviceStatus);
        ok:=DeleteService(hService);

        CloseServiceHandle(hservice);
      end;


      try
        s:='CEDRIVER52';
        getmem(apppath,250);
        GetModuleFileName(0,apppath,250);

        dataloc:=extractfilepath(apppath)+'driver.dat';
        if fileexists(dataloc) then
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
          cc:=CTL_CODE(IOCTL_UNKNOWN_BASE, $080e {unprotect}, METHOD_BUFFERED, FILE_READ_ACCESS or FILE_WRITE_ACCESS);
          ok:=deviceiocontrol(hdevice,cc,@x,4,@x,4,x,nil);
          if not ok then
          begin
            if not setup then messagebox(0,'The driver was found and present. But it can''t unload itself right now','driver error',mb_ok or MB_ICONERROR);
//            closehandle(hdevice);
//            CloseServiceHandle(hservice);
//            CloseServiceHandle(hSCManager);
//            exit;
          end;

          closehandle(hdevice);
        end else ok:=false;

        ControlService(hService, SERVICE_CONTROL_STOP, serviceStatus);
        ok:=DeleteService(hService);

        CloseServiceHandle(hservice);
      end else
      if count=0 then
      begin
        if not setup then messagebox(0,'Failed to find the driver in the registry','driver error',mb_ok or MB_ICONERROR);
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

  if not setup then
  begin
    if ok or ok2 then
      messagebox(0,'The driver is successfully unloaded.','dbk32.sys unloaded',MB_ICONINFORMATION or MB_OK)
    else
      messagebox(0,'The driver failed to unload or is already unloaded. If you think it''s still loaded then reboot and run the unloader again.','DBK32.sys unloader',MB_ICONERROR or MB_OK)
  end;
end.
