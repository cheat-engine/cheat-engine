unit ProcessList;

{$mode delphi}

interface

uses
  {$ifdef windows}jwawindows, windows, {$endif}
  {$ifdef darwin}macport,{$endif}
  cefuncproc, LazUTF8, Classes, SysUtils{$ifndef JNI}, StdCtrls{$endif},
  ProcessHandlerUnit {$ifdef JNI},unixporthelper{$endif},newkernelhandler;

{$ifndef jni}
procedure GetProcessList(ProcessList: TListBox; NoPID: boolean=false); overload;
{$endif}
procedure GetProcessList(ProcessList: TStrings; NoPID: boolean=false; noProcessInfo: boolean=false); overload;
procedure sanitizeProcessList(processlist: TStrings);
procedure cleanProcessList(processlist: TStrings);


function GetFirstModuleName(processid: dword): string;


//global vars refering to the processlist
var
  GetProcessIcons: Boolean;
  ProcessesCurrentUserOnly: boolean;

implementation

uses Globals, commonTypeDefs, networkInterfaceApi
  {$ifdef darwin}
  , macportdefines //must be at the end
  {$endif}
  ;

resourcestring
    rsICanTGetTheProcessListYouArePropablyUsingWindowsNT = 'I can''t get the process list. You are propably using windows NT. Use the window list instead!';


function GetFirstModuleName(processid: dword): string;
var
  SNAPHandle: THandle;
  check: boolean;
  ModuleEntry: MODULEENTRY32;
begin
  result:='';
  SNAPHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE{$ifdef darwin}or TH32CS_SNAPMODULEFIRSTONLY{$endif},processid);
  if SNAPHandle<>0 then
  begin
    ModuleEntry.dwSize:=sizeof(moduleentry);
    if Module32First(snaphandle,ModuleEntry) then
      result:=moduleentry.szExePath
    else
      result:='';

    closehandle(SNAPHandle);
  end;
end;


procedure sanitizeProcessList(processlist: TStrings);
var
  i: integer;
{$IFDEF WINDOWS}
  ProcessListInfo: PProcessListInfo;
  {$ENDIF}
begin
  {$ifdef windows}
  for i:=0 to processlist.count-1 do
    if processlist.Objects[i]<>nil then
    begin
      ProcessListInfo:= pointer( processlist.Objects[i]);

      if (ProcessListInfo^.processIcon<>0) and (ProcessListInfo^.processIcon<>HWND(-1)) then
      begin
        if ProcessListInfo^.processID<>GetCurrentProcessId then
          DestroyIcon(ProcessListInfo^.processIcon);
      end;



      freemem(ProcessListInfo);

      processlist.Objects[i]:=nil;
    end;
  {$endif}
end;

procedure cleanProcessList(processlist: TStrings);
begin
 // OutputDebugString('cleanProcessList()');
  sanitizeProcessList(processlist);
  processlist.clear;
end;

procedure GetProcessList(ProcessList: TStrings; NoPID: boolean=false; noProcessInfo: boolean=false);
var SNAPHandle: THandle;
    ProcessEntry: PROCESSENTRY32;
    Check: Boolean;
    {$IFDEF WINDOWS}
    lwindir: string;
    HI: HICON;
    ProcessListInfo: PProcessListInfo;
    {$ENDIF}
    i,j: integer;
    s,s2: string;


begin
  cleanProcessList(ProcessList);

  {$ifdef darwin}
  if getconnection=nil then
  begin
    macport.GetProcessList(processlist);
    exit;
  end;
  {$endif}

  {$ifdef windows}


  lwindir:=lowercase(windowsdir);
  ProcessListInfo:=nil;
  HI:=0;

  j:=0;

//  OutputDebugString('GetProcessList()');
  {$endif}




 // OutputDebugString('Calling CreateToolhelp32Snapshot');
  SNAPHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);

 // OutputDebugString('SNAPHandle='+inttohex(SNAPHandle,8));

  If SnapHandle<>0 then
  begin
  //  OutputDebugString('SnapHandle>0');

    ZeroMemory(@ProcessEntry, sizeof(ProcessEntry));

    //OutputDebugString('Setting up processentry');




    if not assigned(Process32First) then
    begin
    //  OutputDebugString('Process32First was not assigned');
      exit;
    end;

   // OutputDebugString('Calling Process32First');

   // OutputDebugString('Setting up ProcessEntry dwSize');
    ProcessEntry.dwSize:=SizeOf(ProcessEntry);

    {$ifdef windows}
    if getconnection<>nil then
      noProcessInfo:=true;
    {$else}
    noProcessInfo:=true;
    {$endif}

    Check:=Process32First(SnapHandle,ProcessEntry);
    while check do
    begin

      //s:=GetFirstModuleName(processentry.th32ProcessID);

{$ifdef windows}

      if (not ProcessesCurrentUserOnly) or (GetUserNameFromPID(processentry.th32ProcessID)=username) then
      {$endif}
      begin
        if processentry.th32ProcessID<>0 then
        begin

          {$ifdef windows}
          if noprocessinfo=false then
          begin
            // get some processinfo
            getmem(ProcessListInfo,sizeof(TProcessListInfo));
            ProcessListInfo.processID:=processentry.th32ProcessID;
            ProcessListInfo.processIcon:=0;
            ProcessListInfo.winhandle:=0;
          end;
          {$endif}

          if noPID then
            s:=''
          else
            s:=IntTohex(processentry.th32ProcessID,8)+'-';

          s:=s+ExtractFilename(WinCPToUTF8(processentry.szExeFile));

{$ifdef windows}
          if noprocessinfo then
            ProcessList.Add(s)
          else
            ProcessList.AddObject(s, TObject(ProcessListInfo));
{$else}
          ProcessList.Add(s)
{$endif}
        end;
      end;


      check:=Process32Next(SnapHandle,ProcessEntry);
    end;

    closehandle(snaphandle);
  end
  else
  begin
  //  OutputDebugString('Apparently the handle is smaller than 0...');
    {$ifdef windows}
    raise exception.Create(rsICanTGetTheProcessListYouArePropablyUsingWindowsNT);
    {$endif}
  end;
end;

{$ifndef JNI}
procedure GetProcessList(ProcessList: TListBox; NoPID: boolean=false);
{$IFDEF WINDOWS}
var sl: tstringlist;
    i: integer;
    pli: PProcessListInfo;
{$ENDIF}
begin
  {$ifdef darwin}
  macport.GetProcessList(processlist.Items);
  {$endif}

  {$ifdef windows}
  sl:=tstringlist.create;
  try
    processlist.Sorted:=false;
    for i:=0 to processlist.Items.count-1 do
      if processlist.Items.Objects[i]<>nil then
      begin
        pli:=pointer(processlist.Items.Objects[i]);
        if (pli^.processIcon<>0) and (pli^.processIcon<>HWND(-1)) then
        begin
          if pli^.processID<>GetCurrentProcessId then
            DestroyIcon(pli^.processIcon);
        end;
        freemem(pli);
        processlist.Items.Objects[i]:=nil;
      end;

    processlist.Items.Clear;


    GetProcessList(sl, NoPID);
    processlist.Items.AddStrings(sl);
  finally
    sl.free;
  end;
  {$endif}
end;
{$endif}

end.

