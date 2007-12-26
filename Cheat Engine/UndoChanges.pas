unit UndoChanges;

interface

uses windows,SysUtils,classes,dialogs,controls,cefuncproc,newkernelhandler,syncobjs;

const undoversion=1;

type TUndoChangesThread=class(TThread)
  private
    procedure ShutdownProtection;
    procedure ShowModification;
    procedure HideModification;
    function  CheckForChanges:boolean;
  public
    procedure execute; override;

end;

type tregiondata=record
  dllnr: byte;
  address: dword;
  size: dword;
end;
type bytearray=array of byte;

function CheckForChanges:boolean;

var undothread:TUndoChangesThread;
    firstrun:boolean;

implementation
uses mainunit,formsettingsunit;

var checkedokonce: boolean;
    protectionloaded: boolean;
    loadmem: TCriticalSection;


    OldKernelDLLBase,NewKernelDLLBase: DWORD; //needed to make offset adjustments if somehow the addresses got changed. (0% chance this happens, except a windows update, but lets do it anyhow)
    OldNTDLLBase,NewNTDLLBase: DWORD;
    Olduser32dllbase,newuser32dllbase:DWORD;

    KernelDllRegions: array of TRegiondata;
    Ntdllregions: array of tregiondata;
    user32DLLregions: array of tregiondata;

    KernelMemory: array of bytearray;
    ntdllmemory:  array of bytearray;
    user32memory: array of bytearray;

    OpenProcessPos:dword;
    DebugActiveProcessPos:dword;
    SuspendThreadPos:dword;
    ResumeThreadPos:dword;
    ReadProcessMemoryPos:dword;
    WriteProcessMemoryPos:dword;
    NtOpenProcessPos:dword;
    SetWindowsHookExAPos:dword;

function InitMem:boolean;
var savedata: tfilestream;
    x: dword;
    i:integer;
begin
  try
    LoadMem.enter;
    try
      if firstrun then //load the original memory
      begin
        NewUser32DLLBase:=dword(LoadLibrary('user32.dll'));
        NewKernelDLLBase:=dword(LoadLibrary('Kernel32.dll'));
        NewNtDLLBase:=dword(LoadLibrary('ntdll.dll'));

        Freelibrary(NewKernelDLLBase); //decrease the reference pointer. (always a good habbit, but not really needed)
        FreeLibrary(NewNtdllbase);
        FreeLibrary(NewNtDLLBase);

        savedata:=tfilestream.Create(cheatenginedir+'CEProtect.dat',fmOpenRead);
        try
          savedata.ReadBuffer(x,4);
          if x<>undoversion then
          begin
            formsettings.cbUndoMemoryChanges.checked:=false;
            raise exception.Create('The ceprotect.dat file was created with a different version of Cheat Engine. Re-enable the undo memory changes option in CE to recreate the CEProtect.dat file');
          end;

          savedata.ReadBuffer(OldKernelDLLBase,4);
          savedata.ReadBuffer(OldNTDLLBase,4);
          savedata.ReadBuffer(OldUser32DLLBase,4);

          savedata.ReadBuffer(x,4);
          setlength(kerneldllregions,x);
          savedata.ReadBuffer(kerneldllregions[0],x*sizeof(tregiondata));

          setlength(kernelmemory,x);
          for i:=0 to x-1 do
          begin
            setlength(kernelmemory[i],kerneldllregions[i].size);
            savedata.ReadBuffer(kernelmemory[i][0],kerneldllregions[i].size);
          end;

          savedata.ReadBuffer(x,4);
          setlength(ntdllregions,x);
          savedata.ReadBuffer(ntdllregions[0],x*sizeof(tregiondata));

          setlength(ntdllmemory,x);
          for i:=0 to x-1 do
          begin
            setlength(ntdllmemory[i],ntdllregions[i].size);
            savedata.ReadBuffer(ntdllmemory[i][0],ntdllregions[i].size);
          end;

          savedata.ReadBuffer(x,4);
          setlength(user32dllregions,x);
          savedata.ReadBuffer(user32dllregions[0],x*sizeof(tregiondata));

          setlength(user32memory,x);
          for i:=0 to x-1 do
          begin
            setlength(user32memory[i],user32dllregions[i].size);
            savedata.ReadBuffer(user32memory[i][0],user32dllregions[i].size);
          end;


          savedata.ReadBuffer(OpenProcessPos,4);
          savedata.ReadBuffer(DebugActiveProcessPos,4);
          savedata.ReadBuffer(SuspendThreadPos,4);
          savedata.ReadBuffer(ResumeThreadPos,4);
          savedata.ReadBuffer(ReadProcessMemoryPos,4);
          savedata.ReadBuffer(WriteProcessMemoryPos,4);
          savedata.ReadBuffer(NtOpenProcessPos,4);
          savedata.Readbuffer(SetWindowsHookExAPos,4);
        finally
          savedata.free;
        end;

        firstrun:=false;
      end;

    finally
      LoadMem.leave;
    end;


    //if it managed to get to here I gues nothing went wrong
    protectionloaded:=true;
  except

  end;

  result:=protectionloaded;
end;

function UndoMemoryChanges: boolean;
var i: integer;
    x,y,ar,buf: dword;
    p: thandle;
    t: byte;
begin
  result:=true;

  //kernel
  for i:=0 to length(kerneldllregions)-1 do
  begin
    x:=kerneldllregions[i].address;

    if formsettings.cbForceUndo.checked then
    begin
      //open cheat engine using the kernel mode driver
      p:=KernelOpenProcess(process_all_access,false,getcurrentprocessid);

      x:=kerneldllregions[i].address;

      while x<kerneldllregions[i].address+kerneldllregions[i].size do
      begin
        try
          MakeWritable(x,4096,true);
          //this page should be writable now, so lets write to it

          CopyMemory(pointer(x),@kernelmemory[i][x-kerneldllregions[i].address],4096)
        except
          //error? ehrm, thats a bug and should never occur
          inc(x,4096);
          continue;
        end;

        inc(x,4096);
      end;


      closehandle(p);
    end
    else
    begin
      //normal virtualprotect method     (wont work on newer nprotect games)
      x:=kerneldllregions[i].address;

      VirtualProtect(pointer(x),kerneldllregions[i].size,page_execute_readwrite,ar);
      try
        CopyMemory(pointer(x),@kernelmemory[i][0],kerneldllregions[i].size);
      except
        result:=false;

      end;
    end;
  end;

  //ntdll
  for i:=0 to length(ntdllregions)-1 do
  begin
    x:=ntdllregions[i].address;

    if formsettings.cbForceUndo.checked then
    begin
      //open cheat engine using the nt mode driver
      p:=KernelOpenProcess(process_all_access,false,getcurrentprocessid);

      x:=ntdllregions[i].address;

      while x<ntdllregions[i].address+ntdllregions[i].size do
      begin
        try
          //read a byte from this page (so it's paged)
          if pbyte(x)^=$cc then sleep(0);

          //now make that address writable
          y:=((x div $1000) *4)+$c0000000;
          if KernelReadProcessMemory(p,pointer(y),@buf,4,ar) then
          begin
            //make it writable without copy-on-write. Copy-on0write should already have taken place when it got modified in the first place. (and if it isn't this'll fix ALL the processes)
            buf:=(buf or $2);
            KernelWriteProcessMemory(p,pointer(y),@buf,4,ar);
          end;

          //this page should be writable now, so lets write to it

          CopyMemory(pointer(x),@ntdllmemory[i][x-ntdllregions[i].address],4096)
        except
          //error? ehrm, thats a bug and should never occur
          inc(x,4096);
          continue;
        end;

        inc(x,4096);
      end;


      closehandle(p);
    end
    else
    begin
      //normal virtualprotect method     (wont work on newer nprotect games)
      x:=ntdllregions[i].address;

      VirtualProtect(pointer(x),ntdllregions[i].size,page_execute_readwrite,ar);
      try
        CopyMemory(pointer(x),@ntdllmemory[i][0],ntdllregions[i].size);
      except
        result:=false;

      end;
    end;
  end;


  //user32
  for i:=0 to length(user32dllregions)-1 do
  begin
    x:=(newuser32dllbase-Olduser32dllbase)+user32dllregions[i].address;

    if formsettings.cbForceUndo.checked then
    begin
      //open cheat engine using the user32 mode driver
      p:=kernelOpenProcess(process_all_access,false,getcurrentprocessid);

      x:=(newuser32dllbase-Olduser32dllbase)+user32dllregions[i].address;

      while x<((newuser32dllbase-Olduser32dllbase)+user32dllregions[i].address)+user32dllregions[i].size do
      begin
        try
          //read a byte from this page (so it's paged)
          if pbyte(x)^=$cc then sleep(0);

          //now make that address writable
          y:=((x div $1000) *4)+$c0000000;
          if kernelReadProcessMemory(p,pointer(y),@buf,4,ar) then
          begin
            //make it writable without copy-on-write. Copy-on0write should already have taken place when it got modified in the first place. (and if it isn't this'll fix ALL the processes)
            buf:=(buf or $2);
            kernelWriteProcessMemory(p,pointer(y),@buf,4,ar);
          end;

          //this page should be writable now, so lets write to it

          CopyMemory(pointer(x),@user32memory[i][x-user32dllregions[i].address],4096)
        except
          //error? ehrm, thats a bug and should never occur
          inc(x,4096);
          continue;
        end;

        inc(x,4096);
      end;


      closehandle(p);
    end
    else
    begin
      //normal virtualprotect method     (wont work on newer nprotect games)
      x:=(newuser32dllbase-Olduser32dllbase)+user32dllregions[i].address;

      VirtualProtect(pointer(x),user32dllregions[i].size,page_execute_readwrite,ar);
      try
        CopyMemory(pointer(x),@user32memory[i][0],user32dllregions[i].size);
      except
        result:=false;

      end;
    end;
  end;

  if result then
    mainform.LabelModifiedmem.Caption:='A modification has been found in the memory and Cheat Engine fixed it'
  else
    mainform.LabelModifiedmem.Caption:='A modification has been found in the memory but Cheat Engine failed to fixed it';

  mainform.LabelModifiedmem.Visible:=true;
  mainform.Timer4.Enabled:=true;
end;

function CheckForChanges: boolean;
var kernel32,ntdll: thandle;
    newmem: array [0..19] of byte;

    i: integer;
    found:boolean;
begin
  //call from mainthread
  result:=true;
  InitMem;    //make sure the memory to compare is loaded

  //check the following apis (first 20 bytes):  (thats faster than complete memory compare each time)

  if oldkerneldllbase<>newkerneldllbase then exit;
  if oldntdllbase<>newntdllbase then exit;


  Try
    //check OpenProcess
    copyMemory(@newmem[0],pointer(OpenProcesspos),20);
    found:=false;
    for i:=0 to length(kerneldllregions)-1 do
      if (OpenProcessPos>=kerneldllregions[i].address) and (OpenProcessPos<kerneldllregions[i].address+kerneldllregions[i].size) then
      begin
        found:=true;

        //found the region to check
        //find the offset into the array
        if not Comparemem(@kernelmemory[i][(OpenProcessPos-kerneldllregions[i].address)],@newmem[0],20) then
        begin
          undoMemorychanges;
          exit;
        end;

      end;

    if not found then exit; //error


    //check DebugActiveProcess
    copyMemory(@newmem[0],pointer(DebugActiveProcesspos),20);
    found:=false;
    for i:=0 to length(kerneldllregions)-1 do
      if (DebugActiveProcessPos>=kerneldllregions[i].address) and (DebugActiveProcessPos<kerneldllregions[i].address+kerneldllregions[i].size) then
      begin
        found:=true;
        //found the region to check
        //find the offset into the array
        if not Comparemem(@kernelmemory[i][(DebugActiveProcessPos-kerneldllregions[i].address)],@newmem[0],20) then
        begin
          undoMemorychanges;
          exit;
        end;

      end;

    if not found then exit; //error


    //check SuspendThread
    copyMemory(@newmem[0],pointer(SuspendThreadpos),20);
    found:=false;
    for i:=0 to length(kerneldllregions)-1 do
      if (SuspendThreadPos>=kerneldllregions[i].address) and (SuspendThreadPos<kerneldllregions[i].address+kerneldllregions[i].size) then
      begin
        found:=true;
        //found the region to check
        //find the offset into the array
        if not Comparemem(@kernelmemory[i][(SuspendThreadPos-kerneldllregions[i].address)],@newmem[0],20) then
        begin
          undoMemorychanges;
          exit;
        end;

      end;

    if not found then exit; //error

    //check ResumeThread
    copyMemory(@newmem[0],pointer(ResumeThreadpos),20);
    found:=false;
    for i:=0 to length(kerneldllregions)-1 do
      if (ResumeThreadPos>=kerneldllregions[i].address) and (ResumeThreadPos<kerneldllregions[i].address+kerneldllregions[i].size) then
      begin
        found:=true;
        //found the region to check
        //find the offset into the array
        if not Comparemem(@kernelmemory[i][(ResumeThreadPos-kerneldllregions[i].address)],@newmem[0],20) then
        begin
          undoMemorychanges;
          exit;
        end;

      end;

    if not found then exit; //error


    //check ReadProcessMemory
    copyMemory(@newmem[0],pointer(ReadProcessMemorypos),20);
    found:=false;
    for i:=0 to length(kerneldllregions)-1 do
      if (ReadProcessMemoryPos>=kerneldllregions[i].address) and (ReadProcessMemoryPos<kerneldllregions[i].address+kerneldllregions[i].size) then
      begin
        found:=true;
        //found the region to check
        //find the offset into the array
        if not Comparemem(@kernelmemory[i][(ReadProcessMemoryPos-kerneldllregions[i].address)],@newmem[0],20) then
        begin
          undoMemorychanges;
          exit;
        end;

      end;

    if not found then exit; //error

    //check WriteProcessMemory
    copyMemory(@newmem[0],pointer(WriteProcessMemorypos),20);
    found:=false;
    for i:=0 to length(kerneldllregions)-1 do
      if (WriteProcessMemoryPos>=kerneldllregions[i].address) and (WriteProcessMemoryPos<kerneldllregions[i].address+kerneldllregions[i].size) then
      begin
        found:=true;
        //found the region to check
        //find the offset into the array
        if not Comparemem(@kernelmemory[i][(WriteProcessMemoryPos-kerneldllregions[i].address)],@newmem[0],20) then
        begin
          undoMemorychanges;
          exit;
        end;

      end;

    if not found then exit; //error

    //check NtOpenProcess (ntdll)
    copyMemory(@newmem[0],pointer(NtOpenProcesspos),20);
    found:=false;
    for i:=0 to length(ntdllregions)-1 do
      if (NtOpenProcessPos>=ntdllregions[i].address) and (NtOpenProcessPos<ntdllregions[i].address+ntdllregions[i].size) then
      begin
        found:=true;
        //found the region to check
        //find the offset into the array
        if not Comparemem(@ntdllmemory[i][(NtOpenProcessPos-ntdllregions[i].address)],@newmem[0],20) then
        begin
          undoMemorychanges;
          exit;
        end;

      end;

    if not found then exit; //error


    //check SetWindowsHookEx (user32)
    copyMemory(@newmem[0],pointer(SetWindowsHookExApos+(Newuser32dllbase-olduser32dllbase)),20);
    found:=false;
    for i:=0 to length(user32dllregions)-1 do
      if (SetWindowsHookExAPos>=user32dllregions[i].address) and (SetWindowsHookExAPos<user32dllregions[i].address+user32dllregions[i].size) then
      begin
        found:=true;
        //found the region to check
        //find the offset into the array
        if not Comparemem(@user32memory[i][(SetWindowsHookExAPos-user32dllregions[i].address)],@newmem[0],20) then
        begin
          undoMemorychanges;
          exit;
        end;

      end;

    if not found then exit; //error


    result:=false;
  except

  end;
end;

procedure TUndoChangesThread.ShutdownProtection;
begin
  //shutdown the undothread and exit this procedure
  formsettings.cbUndoMemoryChanges.Checked:=false;
  if undothread<>nil then
  begin
    undothread.terminate;
    undothread:=nil;
  end;
end;

function TUndoChangesThread.CheckForChanges:boolean;
{result=true if there was a change}
begin
  result:=false;
  //check the following apis (first 20 bytes):  (thats faster than complete memory compare each time)
  //NtOpenProcess
  //DebugActiveProcess
  //SuspendThread
  //OpenProcess
  //SetWindowsHookEx
  //ReadProcessMemory
  //WriteProcessMemory

  if not InitMem then
  begin
    MessageBox(0,'The protection file couldn''t be loaded. The protector will not run','CE Protector Thread',MB_OK or MB_ICONERROR);
    synchronize(shutdownprotection);
    exit;
  end;

  if result then
  begin
    if not checkedokonce then
    begin
      //WARNING!!!! This is the first time this scan is done and the memory isn't what it should be.
      //This can be caused by 2 thinga
      //1: A anti-cheat or another hook changed the memory
      //2: The user had a update
      if MessageBox(0,'Warning! The memory of at least one key memory location is not what it should be. This can be due to a anti-cheat, a virus, or a windows update.'+#13#10+'Do you want to restore the memory to what it was suposed to be? (Click yes for CE to restore the memory, no to disable the memory restore function)','CE Protector Thread',MB_YESNO	or MB_ICONWARNING ) =IDNO	then
      begin
        synchronize(shutdownprotection);
        exit;
      end;
    end;

    //if it found something wrong it'll write over ALL the memory
  end;

  if not result then checkedokonce:=true;
end;

procedure TUndoChangesThread.ShowModification;
begin
  mainform.LabelModifiedmem.visible:=true;
end;

procedure TUndoChangesThread.HideModification;
begin
  mainform.LabelModifiedmem.visible:=false;
end;


procedure TUndoChangesThread.execute;
begin
  while not terminated do
  begin
    if (CheckForChanges) and (not terminated) then
    begin
      synchronize(ShowModification);
      sleep(2000);
      synchronize(HideModification);
      continue;
    end;
    if not terminated then sleep(5000); //check ebvery 5 seconds for changes
  end;
end;

initialization
  checkedokonce:=false;//i'm not 100% if declared variables are set to 0 (false) or not so let's be sure
  undothread:=nil;
  firstrun:=true;
  protectionloadeD:=false;
  Loadmem:=tcriticalsection.Create;
end.
