unit ProcessList;

{$mode delphi}

interface

uses
  {$ifdef windows}jwawindows, windows, cefuncproc, LazUTF8, {$endif}Classes, SysUtils{$ifndef JNI}, StdCtrls{$endif}, ProcessHandlerUnit {$ifndef windows},unixporthelper{$endif},newkernelhandler;

{$ifndef jni}
procedure GetProcessList(ProcessList: TListBox; NoPID: boolean=false); overload;
{$endif}
procedure GetProcessList(ProcessList: TStrings; NoPID: boolean=false; noProcessInfo: boolean=false); overload;
procedure sanitizeProcessList(processlist: TStrings);
procedure cleanProcessList(processlist: TStrings);

{$ifdef windows}
function GetFirstModuleName(processid: dword): string;
{$endif}

//global vars refering to the processlist
var
  GetProcessIcons: Boolean;
  ProcessesWithIconsOnly: boolean;
  ProcessesCurrentUserOnly: boolean;

implementation

uses Globals, networkInterfaceApi;

resourcestring
    rsICanTGetTheProcessListYouArePropablyUsingWindowsNT = 'I can''t get the process list. You are propably using windows NT. Use the window list instead!';

type TProcessListInfo=record
  processID: dword;
  processIcon: HICON;
end;
PProcessListInfo=^TProcessListInfo;

{$ifdef windows}
function GetFirstModuleName(processid: dword): string;
var
  SNAPHandle: THandle;
  check: boolean;
  ModuleEntry: MODULEENTRY32;
begin
  SNAPHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,processid);
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
{$endif}

procedure sanitizeProcessList(processlist: TStrings);
var
  i: integer;
  ProcessListInfo: PProcessListInfo;
begin
  for i:=0 to processlist.count-1 do
    if processlist.Objects[i]<>nil then
    begin
      ProcessListInfo:= pointer( processlist.Objects[i]);
{$ifdef windows}
      if ProcessListInfo.processIcon>0 then
        DestroyIcon(ProcessListInfo.processIcon);
{$endif}
      freemem(ProcessListInfo);

      processlist.Objects[i]:=nil;
    end;
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
    HI: HICON;
    ProcessListInfo: PProcessListInfo;
    i,j: integer;
    s: string;
begin

  ProcessListInfo:=nil;
  HI:=0;

  j:=0;

//  OutputDebugString('GetProcessList()');

  cleanProcessList(ProcessList);


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


    if getconnection<>nil then
      noProcessInfo:=true;

    Check:=Process32First(SnapHandle,ProcessEntry);
    while check do
    begin
{$ifdef windows}
      if (noprocessinfo=false) and getprocessicons then
      begin
        s:='';


        HI:=ExtractIcon(hinstance,ProcessEntry.szExeFile,0);
        if HI=0 then
        begin
          i:=getlasterror;

          //alternative method:
          if (processentry.th32ProcessID>0) and (uppercase(copy(ExtractFileName(ProcessEntry.szExeFile), 1,3))<>'AVG') then //february 2014: AVG freezes processes that do createtoolhelp32snapshot on it's processes for several seconds. AVG has multiple processes...
          begin
            s:=GetFirstModuleName(processentry.th32ProcessID);
           // OutputDebugString(s);
            HI:=ExtractIcon(hinstance,pchar(s),0);
          end;
        end;

      end;

      if (noprocessinfo) or (not (ProcessesWithIconsOnly and (hi=0))) and ((not ProcessesCurrentUserOnly) or (GetUserNameFromPID(processentry.th32ProcessID)=username)) then
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
            ProcessListInfo.processIcon:=HI;
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
var sl: tstringlist;
    i: integer;
    pli: PProcessListInfo;
begin
  sl:=tstringlist.create;
  try
    processlist.Sorted:=false;
    for i:=0 to processlist.Items.count-1 do
      if processlist.Items.Objects[i]<>nil then
      begin
        pli:=pointer(processlist.Items.Objects[i]);
        if pli.processIcon>0 then
          DestroyIcon(pli.processIcon);
        freemem(pli);
      end;

    processlist.Items.Clear;


    GetProcessList(sl, NoPID);
    processlist.Items.AddStrings(sl);
  finally
    sl.free;
  end;
end;
{$endif}

end.

