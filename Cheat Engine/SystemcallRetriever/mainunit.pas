unit mainunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,debugger,newkernelhandler, ExtCtrls, ComCtrls,tlhelp32;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Image1: TImage;
    ProgressBar1: TProgressBar;
    Timer2: TTimer;
    listbox1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    activelinkoffset: dword;
    processnameoffset: dword;
    debugportoffset: dword;
    debugger: tdebugger;
    sdtshadow: dword;
    procedure GetPEProcessData;
    procedure Done(var m:tmessage); message wm_user+1;
  public
    { Public declarations }
  end;

type tsharedmem=record
  Infunction:boolean;
  RetrieverWindowHandle: thandle;
end;

var
  Form1: TForm1;

  sharedmemmapping: thandle;
  sharedmem: ^tsharedmem;
  paramlist: dword;
  processhandle:thandle;

  phase: integer;

implementation

{$R *.dfm}
uses resultwindowunit;

function GetSystemType: Integer;  //from Stuart Johnson with a little change by me
const
 { operating system constants }

 cOsUnknown = -1;
 cOsWin95 = 0;
 cOsWin98 = 1;
 cOsWin98SE = 2;
 cOsWinME = 3;
 cOsWinNT = 4;
 cOsWin2000 = 5;
 cOsWinXP = 6;
 cOsNewer = 7;

var
 osVerInfo : TOSVersionInfo;
 majorVer, minorVer : Integer;

begin
{ set operating system type flag }
 osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
 if GetVersionEx(osVerInfo) then
   begin
     majorVer := osVerInfo.dwMajorVersion;
     minorVer := osVerInfo.dwMinorVersion;
     case osVerInfo.dwPlatformId of
       VER_PLATFORM_WIN32_NT : { Windows NT/2000 }
         begin
           if majorVer <= 4 then
             result := cOsWinNT
           else
             if (majorVer = 5) AND (minorVer= 0) then
               result := cOsWin2000
             else
               if (majorVer = 5) AND (minorVer = 1) then
                 result := cOsWinXP
             else if (majorver = 5) then result:=cOsNewer
           else
           result := cOsUnknown;
         end; {case }
     VER_PLATFORM_WIN32_WINDOWS : { Windows 9x/ME }
       begin
         if (majorVer = 4) AND (minorVer = 0) then
           result := cOsWin95
         else
           if (majorVer = 4) AND (minorVer = 10) then
             begin
               if osVerInfo.szCSDVersion[1] = 'A' then
                 result := cOsWin98SE
               else
                  result := cOsWin98;
               end {if Version = 'A'}
             else
               if (majorVer = 4) AND (minorVer = 90) then
                 result := cOsWinME
               else
                  result := cOsUnknown;
       end; {case VER_PLATFORM_WIN32_WINDOWS}
     else
      result := cOsUnknown;
   end;
 end
else
  result := cOsUnknown;
end;


procedure TForm1.Done(var m:tmessage);
var callnumbersfile:tfilestream;
    i:integer;
    NtUserBuildHwndListCallnumber: DWORD;
    NtUserQueryWindowCallnumber:DWORD;
    NtUserFindWindowExCallnumber:DWORD;
    NtUserGetForegroundWindowCallnumber:DWORD;

    PossibleNtUserBuildHwndListCallnumbers:array of dword;
    PossibleNtUserQueryWindowCallnumbers:array of dword;
    PossibleNtUserFindWindowExCallnumbers:array of dword;
    PossibleNtUserGetForegroundWindowCallnumbers:array of dword;
    input:string;
    question:string;
    buf: byte;
    temp:dword;
    ar:dword;
    winversion:_osversioninfoa;
begin
  timer1.Enabled:=false;
  timer2.Enabled:=false;
  progressbar1.Position:=0;

  if m.WParam=1 then //it was a cancel, it took too long so it might have been crashed
  begin
    terminateprocess(debuggedprocesshandle,0);
    debugger.Terminate;
    //showmessage('First part failed, but let''s hope I managed to get the data I needed');
  end;

  if phase<=2 then //prepare for next debugging sesion
  begin
    inc(phase);
    debugger:=tdebugger.Create(false);
    timer1.Enabled:=true;
    timer2.Enabled:=true;
  end
  else
  begin
    //this was the last one , now lets check everything

    //NtUserBuildHwndListCallnumber
    setlength(PossibleNtUserBuildHwndListCallnumbers,0);
    for i:=0 to length(callnumbers[0])-1 do
    begin
      //search the list for a callnumber that takes $1c parameters (if there are more ask)
      if callnumbers[0][i].parametercount=$1c then
      begin
        setlength(PossibleNtUserBuildHwndListCallnumbers,length(PossibleNtUserBuildHwndListCallnumbers)+1);
        PossibleNtUserBuildHwndListCallnumbers[length(PossibleNtUserBuildHwndListCallnumbers)-1]:=callnumbers[0][i].callnumber;
      end;
    end;

    if length(PossibleNtUserBuildHwndListCallnumbers)=1 then
    begin
      NtUserBuildHwndListCallnumber:=PossibleNtUserBuildHwndListCallnumbers[0]
    end
    else //not 1, not 0, so multiple
    begin
      input:='0';
      question:='Multiple systemcalls where recorded with the same ammount of parameters. Select the right one. (default=0)';
      for i:=0 to length(PossibleNtUserBuildHwndListCallnumbers)-1 do
        question:=question+#13#10+IntToStr(i)+':'+IntTohex(PossibleNtUserBuildHwndListCallnumbers[i],4);

      if inputquery('Systemcall retriever error',question ,input) then
      begin
        try
          i:=StrToInt(input);
          if i>=length(PossibleNtUserBuildHwndListCallnumbers) then raise exception.Create('I can''t do that dave');

          NtUserBuildHwndListCallnumber:=PossibleNtUserBuildHwndListCallnumbers[i];
        except
          NtUserBuildHwndListCallnumber:=0;
        end;
      end
      else //choose the first one
        NtUserBuildHwndListCallnumber:=PossibleNtUserBuildHwndListCallnumbers[0];

    end;

    //NtUserqueryWindow
    setlength(PossibleNtUserQueryWindowCallnumbers,0);
    for i:=0 to length(callnumbers[1])-1 do
    begin
      //search the list for a callnumber that takes $08 parameters (if there are more ask)
      if callnumbers[1][i].parametercount=$08 then
      begin
        setlength(PossibleNtUserQueryWindowCallnumbers,length(PossibleNtUserQueryWindowCallnumbers)+1);
        PossibleNtUserQueryWindowCallnumbers[length(PossibleNtUserQueryWindowCallnumbers)-1]:=callnumbers[1][i].callnumber;
      end;
    end;

    if length(PossibleNtUserQueryWindowCallnumbers)=1 then
    begin
      NtUserQueryWindowCallnumber:=PossibleNtUserQueryWindowCallnumbers[0]
    end
    else //not 1, not 0, so multiple
    begin
      input:='0';
      question:='Multiple systemcalls where recorded with the same ammount of parameters. Select the right one. (default=0)';
      for i:=0 to length(PossibleNtUserQueryWindowCallnumbers)-1 do
        question:=question+#13#10+IntToStr(i)+':'+IntTohex(PossibleNtUserQueryWindowCallnumbers[i],4);

      if inputquery('Systemcall retriever error',question ,input) then
      begin
        try
          i:=StrToInt(input);
          if i>=length(PossibleNtUserQueryWindowCallnumbers) then raise exception.Create('I can''t do that dave');

          NtUserQueryWindowCallnumber:=PossibleNtUserQueryWindowCallnumbers[i];
        except
          NtUserQueryWindowCallnumber:=0;
        end;
      end
      else //choose the first one
        NtUserQueryWindowCallnumber:=PossibleNtUserQueryWindowCallnumbers[0];

    end;

    //NtUserFindWindowEx
    setlength(PossibleNtUserFindWindowExCallnumbers,0);
    for i:=0 to length(callnumbers[2])-1 do
    begin
      //search the list for a callnumber that takes $14 parameters (if there are more ask)
      if callnumbers[2][i].parametercount=$14 then
      begin
        setlength(PossibleNtUserFindWindowExCallnumbers,length(PossibleNtUserFindWindowExCallnumbers)+1);
        PossibleNtUserFindWindowExCallnumbers[length(PossibleNtUserFindWindowExCallnumbers)-1]:=callnumbers[2][i].callnumber;
      end;
    end;

    if length(PossibleNtUserFindWindowExCallnumbers)=1 then
    begin
      NtUserFindWindowExCallnumber:=PossibleNtUserFindWindowExCallnumbers[0]
    end
    else //not 1, not 0, so multiple
    begin
      input:='0';
      question:='Multiple systemcalls where recorded with the same ammount of parameters. Select the right one. (default=0)';
      for i:=0 to length(PossibleNtUserFindWindowExCallnumbers)-1 do
        question:=question+#13#10+IntToStr(i)+':'+IntTohex(PossibleNtUserFindWindowExCallnumbers[i],4);

      if inputquery('Systemcall retriever error',question ,input) then
      begin
        try
          i:=StrToInt(input);
          if i>=length(PossibleNtUserFindWindowExCallnumbers) then raise exception.Create('I can''t do that dave');

          NtUserFindWindowExCallnumber:=PossibleNtUserFindWindowExCallnumbers[i];
        except
          NtUserFindWindowExCallnumber:=0;
        end;
      end
      else //choose the first one
        NtUserFindWindowExCallnumber:=PossibleNtUserFindWindowExCallnumbers[0];

    end;

    //NtUserGetForegroundWindow
    setlength(PossibleNtUserGetForegroundWindowCallnumbers,0);
    for i:=0 to length(callnumbers[3])-1 do
    begin
      //search the list for a callnumber that takes 0 parameters (if there are more ask)
      if callnumbers[3][i].parametercount=0 then
      begin
        setlength(PossibleNtUserGetForegroundWindowCallnumbers,length(PossibleNtUserGetForegroundWindowCallnumbers)+1);
        PossibleNtUserGetForegroundWindowCallnumbers[length(PossibleNtUserGetForegroundWindowCallnumbers)-1]:=callnumbers[3][i].callnumber;
      end;
    end;

    if length(PossibleNtUserGetForegroundWindowCallnumbers)=1 then
    begin
      NtUserGetForegroundWindowCallnumber:=PossibleNtUserGetForegroundWindowCallnumbers[0]
    end
    else //not 1, not 0, so multiple
    begin
      input:='0';
      question:='Multiple systemcalls where recorded with the same ammount of parameters. Select the right one. (default=0)';
      for i:=0 to length(PossibleNtUserGetForegroundWindowCallnumbers)-1 do
        question:=question+#13#10+IntToStr(i)+':'+IntTohex(PossibleNtUserGetForegroundWindowCallnumbers[i],4);

      if inputquery('Systemcall retriever error',question ,input) then
      begin
        try
          i:=StrToInt(input);
          if i>=length(PossibleNtUserGetForegroundWindowCallnumbers) then raise exception.Create('I can''t do that dave');

          NtUserGetForegroundWindowCallnumber:=PossibleNtUserGetForegroundWindowCallnumbers[i];
        except
          NtUserGetForegroundWindowCallnumber:=0;
        end;
      end
      else //choose the first one
        NtUserGetForegroundWindowCallnumber:=PossibleNtUserGetForegroundWindowCallnumbers[0];

    end;


    winversion.dwOSVersionInfoSize:=sizeof(winversion);
    getversionex(winversion);


    if (debugportoffset=0) and (getsystemtype=6) then
    begin
      if messagedlg('It seems the debugport wasn''t found. But I see windows XP is used. So it should be safe to assume that the debugport offset is 188. Do you want to fill that in?',mtConfirmation,[mbyes,mbno],0)=mryes then
        debugportoffset:=188;
    end;

    with tresultwindow.create(self) do
    begin
      //fill the window with the collected data
      edit1.text:=inttostr(NtUserBuildHwndListCallnumber);
      edit2.text:=inttostr(NtUserQueryWindowCallnumber);
      edit3.text:=inttostr(NtUserFindWindowExCallnumber);
      edit4.text:=inttostr(NtUserGetForegroundWindowCallnumber);

      edit5.text:=inttostr(activelinkoffset);
      edit6.text:=inttostr(processnameoffset);
      edit7.text:=inttostr(debugportoffset);

      if showmodal =mrok then
      begin
        NtUserBuildHwndListCallnumber:=strtoint(edit1.text);
        NtUserQueryWindowCallnumber:=strtoint(edit2.text);
        NtUserFindWindowExCallnumber:=strtoint(edit3.text);
        NtUserGetForegroundWindowCallnumber:=strtoint(edit4.text);
        activelinkoffset:=strtoint(edit5.text);
        processnameoffset:=strtoint(edit6.text);
        debugportoffset:=strtoint(edit7.text);
      end;
      //fill the data with the data from the window
    end;


    callnumbersfile:=tfilestream.Create(extractfilepath(application.ExeName)+'kerneldata.dat',fmcreate,fmsharedenynone);
    try
      callnumbersfile.WriteBuffer(winversion.dwMajorVersion,4);
      callnumbersfile.WriteBuffer(winversion.dwMinorVersion,4);
      callnumbersfile.WriteBuffer(winversion.dwBuildNumber,4);
      callnumbersfile.WriteBuffer(winversion.szCSDVersion,128);

      callnumbersfile.WriteBuffer(NtUserBuildHwndListCallnumber,4);
      callnumbersfile.WriteBuffer(NtUserQueryWindowCallnumber,4);
      callnumbersfile.WriteBuffer(NtUserFindWindowExCallnumber,4);
      callnumbersfile.WriteBuffer(NtUserGetForegroundWindowCallnumber,4);

      callnumbersfile.WriteBuffer(activelinkoffset,4);
      callnumbersfile.WriteBuffer(processnameoffset,4);
      callnumbersfile.WriteBuffer(debugportoffset,4);
    finally
      callnumbersfile.Free;
    end;


    application.Terminate;

  end;

end;

procedure TForm1.GetPEProcessData;
var ths: thandle;
    pe32: tagProcessentry32;
    process1name: string;
    process1processid: dword;
    process1peprocess:dword;
    process1buffer: array [0..512] of byte;
    process2name: string;
    process2processid: dword;
    process2peprocess:dword;
    process2buffer: array [0..512] of byte;
    process3name: string;
    process3processid: dword;
    process3peprocess:dword;
    process3buffer: array [0..512] of byte;
    nobr: dword;
    a,b,c: boolean;
    p,p2: ^dword;
    offset: dword;
    temp:dword;
    currentchar: byte;
    i: integer;
    tempname: pchar;

    idleprocesspeprocess: dword;
    idleprocessbuffer: array [0..512] of byte;
    idleprocesswithdebuggerbuffer: array [0..512] of byte;

    processhandle1,processhandl2: thandle;
    si:_startupinfoa;
    pi:_process_information;
    si2:_startupinfoa;
    pi2:_process_information;

begin

  listbox1.lines.Add('Get Processlist');
  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  try

    pe32.dwSize:=sizeof(pe32);
    if process32first(ths,pe32) then
    begin
      if pe32.th32ProcessID<>0 then
      begin
        process1name:=pe32.szExeFile;
        process1processid:=pe32.th32ProcessID;
        process1peprocess:=getpeprocess(process1processid);
      end
      else
      if process32next(ths,pe32) then
      begin
        process1name:=pe32.szExeFile;
        process1processid:=pe32.th32ProcessID;
        process1peprocess:=getpeprocess(process1processid);
      end else
      begin
        listbox1.Lines.Add('Couldn''t get a valid process list(1). No kerneldata can be gathered');
        exit;
      end;

      if process32next(ths,pe32) then
      begin
        process2name:=pe32.szExeFile;
        process2processid:=pe32.th32ProcessID;
        process2peprocess:=getpeprocess(process2processid);
      end else
      begin
        listbox1.Lines.Add('Couldn''t get a valid process list(2). No kerneldata can be gathered');
        exit;
      end;

      if process32next(ths,pe32) then
      begin
        process3name:=pe32.szExeFile;
        process3processid:=pe32.th32ProcessID;
        process3peprocess:=getpeprocess(process3processid);
      end else
      begin
        listbox1.Lines.Add('Couldn''t get a valid process list(3). No kerneldata can be gathered');
        exit;
      end;

      listbox1.Lines.Add('Reading the PEProcess structures');
      a:=ReadProcessmemory(processhandle,pointeR(process1peprocess),@process1buffer[0],512,nobr);
      b:=ReadProcessmemory(processhandle,pointeR(process2peprocess),@process2buffer[0],512,nobr);
      c:=ReadProcessmemory(processhandle,pointeR(process3peprocess),@process3buffer[0],512,nobr);

      if not (a and b and c) then
      begin
        listbox1.Lines.Add('The PEProcess structures could not be read');
        exit;
      end;

      listbox1.Lines.Add('Finding the offset for the name');
      currentchar:=1;
      for i:=0 to 511 do
      begin
        if uppercase(chr(process1buffer[i]))=uppercase(process1name[currentchar]) then
        begin
          inc(currentchar);
          if currentchar>length(process1name) then
          begin
            listbox1.Lines.add('name of the process was found at offset '+IntToHex(i-currentchar+2,3));
            processnameoffset:=i-currentchar+2;
            break;
          end;
        end else currentchar:=1;
      end;

      if processnameoffset=0 then
      begin
        listbox1.Lines.Add('The name of the process couldn''t be found in the peprocess structure...');
        exit;
      end;

      listbox1.Lines.Add('Confirming the offset');

      if length(process2name)<=14 then
      begin
        getmem(tempname,length(process2name)+1);
        try
          copymemory(tempname,@process2buffer[processnameoffset],length(process2name));
          tempname[length(process2name)]:=#0;
          if uppercase(tempname)=uppercase(process2name) then
            listbox1.Lines.Add('The offset is correct(1)')
          else
          begin
            processnameoffset:=0;
            listbox1.Lines.Add('The offset doesn''t seem to be correct(1). ');
            exit;
          end;
        finally
          freemem(tempname);
        end;
      end else listbox1.Lines.Add('Skipped compare of process2 because of a too big name');

      if length(process3name)<=14 then
      begin
        getmem(tempname,length(process3name)+1);
        try
          copymemory(tempname,@process3buffer[processnameoffset],length(process3name));
          tempname[length(process3name)]:=#0;
          if uppercase(tempname)=uppercase(process3name) then
            listbox1.Lines.Add('The offset is correct(2)')
          else
          begin
            processnameoffset:=0;
            listbox1.Lines.Add('The offset doesn''t seem to be correct(2). ');
            exit;
          end;
        finally
          freemem(tempname);
        end;
      end else listbox1.Lines.Add('Skipped compare of process3 because of a too big name');


      listbox1.Lines.Add('');
      listbox1.Lines.Add('Finding out the activeprocess offset');
      p:=@process1buffer;
      offset:=0;
      //scan till p^ finds (process2peprocess+offset)
      while offset<2048 do
      begin
        if (p^=process2peprocess+offset) then
        begin
          activelinkoffset:=offset;
          listbox1.Lines.Add('The activeprocess linked list is propably at '+IntToHex(offset,3));
          break;
        end;
        inc(p);
        inc(offset,4);
      end;

      if activelinkoffset=0 then
      begin
        listbox1.Lines.Add('I couldn''t find a activelistprocess structure...');
        exit;
      end;

      temp:=pdword(@process2buffer[activelinkoffset+4])^-activelinkoffset;
      listbox1.Lines.Add('According to process2 the previous peprocess in the list is '+IntToHex(temp,8));
      listbox1.Lines.Add('PEProcess of process1 is at '+IntToHex(process1peprocess,8));

      if temp=process1peprocess then
        listbox1.Lines.Add('This seems to be alright')
      else
      begin
        listbox1.Lines.Add('This isn''t what I was expecting...');
        activelinkoffset:=0;
        exit;
      end;

      listbox1.Lines.Add('');
      listbox1.Lines.Add('Going to figure out the debugport');

      //spawn a process. (no debuggng)
      zeromemory(@si,sizeof(si));
      zeromemory(@pi,sizeof(pi));
      if CreateProcess('emptyprocess.exe',nil,nil,nil,false,0,nil, nil, si, pi) then
      begin
        //create another one so it doesnt find a the processlist/threadlist as a valid entry in win2k
        zeromemory(@si2,sizeof(si));
        zeromemory(@pi2,sizeof(pi));
        CreateProcess('emptyprocess.exe',nil,nil,nil,false,0,nil, nil, si2, pi2);


        try
          idleprocesspeprocess:=getpeprocess(pi.dwProcessId);
          if not readprocessmemory(processhandle,pointeR(idleprocesspeprocess),@idleprocessbuffer[0],512,nobr) then
          begin
            listbox1.Lines.Add('The peprocess structure of the idle process couldn''t be read');
            exit;
          end;

          listbox1.Lines.Add('The peprocess structure of the idleprocess has been stored. Now going to attach a debugger');


          if debugactiveprocess(pi.dwProcessId) then
          begin
            sleep(1000);

            if not readprocessmemory(processhandle,pointeR(idleprocesspeprocess),@idleprocesswithdebuggerbuffer[0],512,nobr) then
            begin
              listbox1.Lines.Add('Failed to get the peprocess structure of the idle process AFTER the debugger was attached. (What kind of shit is this?)');
              exit;
            end;

            listbox1.Lines.Add('Obtained a copy of the peprocess structure WITH and WITHOUT a debugger. Now going to do a compare and hopefully find the debugport');
            offset:=0;
            p:=@idleprocessbuffer[0];
            p2:=@idleprocesswithdebuggerbuffer[0];

            while offset<512 do
            begin
              if (p^=0) and (p2^>$80000000) then
              begin
                debugportoffset:=offset;
                break;
              end;

              inc(p);
              inc(p2);
              inc(offset,4);
            end;

            if debugportoffset=0 then
            begin
              listbox1.Lines.Add('Failed to find the debugport offset');
              exit;
            end;

            Listbox1.Lines.Add('I believe the debugport offset is at '+IntToHex(debugportoffset,3));

          end
          else
          begin
            listbox1.Lines.Add('Failed to attach the debugger');
            exit;
          end;



        finally
          terminateprocess(pi2.hProcess,0);

        end;
      end
      else listbox1.Lines.Add('Failed to spawn the idle process');



    end else
    begin
      listbox1.Lines.Add('Couldn''t get a process list. No kerneldata can be gathered');
      exit;
    end;
  finally
    closehandle(ths);
  end;


  if paramcount>0 then
  begin
    if ParamStr(1)='O' then
      listbox1.Lines.SaveToFile('c:\kernellog.txt');
  end;

  exit;
end;

procedure TForm1.FormCreate(Sender: TObject);
var ar:dword;
begin
  if Messagedlg('This application will try to find out some information about your current system and may crash if something unexpected happens. It''s best to save your data before going further. Continue?',mtConfirmation,[mbyes,mbno],0)=mrno then terminateprocess(getcurrentprocess,1);
  listbox1.Lines.Add('Initializing data structures');
  phase:=0;
  ok:=false;

  SharedMemMapping:=CreateFileMapping($FFFFFFFF,nil,PAGE_READWRITE,0,sizeof(tsharedmem),'SystemCallInfo');
  SharedMem:=MapViewOfFile(SharedMemMapping,FILE_MAP_ALL_ACCESS,0,0,0);

  if sharedmem=nil then
  begin
    showmessage('The systemcallnumber retriever failed to initialize');
    terminateprocess(getcurrentprocess,2);
  end
  else
  begin
   sharedmem^.Infunction:=false;
   sharedmem^.RetrieverWindowHandle:=handle;
  end;

  Listbox1.Lines.Add('Opening the windows kernel');
  UseDBKOpenProcess;
  UseDBKReadWriteMemory;

  processhandle:=OpenProcess(process_all_access,true,getcurrentprocessid);
  if processhandle=0 then showmessage('this process couldn''t be opened');

  sdtshadow:=GetSdtshadow;
  if sdtshadow=0 then
  begin
    listbox1.Lines.Add('No SDTShadow found. So no window stealth possible');
  end else
  begin
    Paramlist:=sdtshadow+12;
    if (paramlist=12) or (not ReadProcessMemory(processhandle,pointer(paramlist),@paramlist,4,ar)) then
      listbox1.Lines.Add('The SDTShadow table that was reported to be found isn''t valid. No stealth...');
  end;


end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  postmessage(sharedmem^.RetrieverWindowHandle,wm_user+1,1,0);
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  progressbar1.StepIt;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  //get peprocessinfo
  application.ProcessMessages;
  GetPEProcessData;

  if sdtshadow<>0 then
  begin
    listbox1.Lines.Add('Going to retrieve some callnumbers used inside windows.');
    listbox1.Lines.Add('This will take a while and you may see the progressbar at the bottom go completly full 4 times');

    debugger:=tdebugger.Create(false);
    timer1.Enabled:=true;
  end else listbox1.Lines.Add('Done! You can close this window now')   
end;

end.
