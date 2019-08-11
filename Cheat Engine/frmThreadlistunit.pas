unit frmThreadlistunit;

{$MODE Delphi}
{$warn 4056 off}
{$warn 4082 off}

interface

uses
  jwawindows, windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, StdCtrls, LResources,cefuncproc, CEDebugger, debugHelper,
  newkernelhandler, networkInterface, networkInterfaceApi;




type

  { TfrmThreadlist }

  TfrmThreadlist = class(TForm)
    tlImageList: TImageList;
    lblIsWOW64: TLabel;
    MenuItem1: TMenuItem;
    miCopyValueToClipboard: TMenuItem;
    miClearDebugRegisters: TMenuItem;
    miFreezeThread: TMenuItem;
    miResumeThread: TMenuItem;
    PopupMenu1: TPopupMenu;
    miBreak: TMenuItem;
    threadTreeview: TTreeView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure miBreakClick(Sender: TObject);
    procedure miClearDebugRegistersClick(Sender: TObject);
    procedure miCopyValueToClipboardClick(Sender: TObject);
    procedure miFreezeThreadClick(Sender: TObject);
    procedure miResumeThreadClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure threadTreeviewDblClick(Sender: TObject);
    procedure threadTreeviewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure FillThreadlist;
  end;

var
  frmThreadlist: TfrmThreadlist;

implementation

uses debugeventhandler, frmstacktraceunit, DebuggerInterfaceAPIWrapper,
  ProcessHandlerUnit, Parsers, symbolhandler, Clipbrd;

resourcestring
  rsPleaseFirstAttachTheDebuggerToThisProcess = 'Please first attach the debugger to this process';
  rsCouldnTObtainContext = 'Couldn''t obtain context';
  rsCouldnTOpenHandle = 'Couldn''t open handle';
  rsTLChangeValue = 'Change value';
  rsTLWhatShouldTheNewValueOfThisRegisterBe = 'What should the new value of this register be?';
  rsTLFailedErrorcode = 'failed. Errorcode=';
  rsTLExecute = 'Execute';
  rsTLWrite = 'Write';
  rsTLIO = 'I/O';
  rsTLAccess = 'Access';
  rsTL1Bytes = '1 byte';
  rsTL2Bytes = '2 bytes';
  rsTL8Bytes = '8 bytes';
  rsTL4Bytes = '4 bytes';
  rsCurrent  = 'Current';

procedure TfrmThreadlist.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
  frmthreadlist:=nil;
end;

procedure TfrmThreadlist.FormCreate(Sender: TObject);
begin
  fillthreadlist;

  LoadFormPosition(self);
end;

procedure TfrmThreadlist.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(self);
end;

procedure TfrmThreadlist.FormShow(Sender: TObject);
begin
  lblIsWOW64.visible:={$ifdef cpu64}processhandler.is64Bit=false{$else}false{$endif};
end;

procedure TfrmThreadlist.MenuItem1Click(Sender: TObject);
var
  c: TCONTEXT;
{$ifdef cpu64}
  c32: TContext32;
{$endif}
  i: integer;
  s: TTreeNode;
  threadlist: tlist;

  tid: dword;
  th: THandle;
begin

  zeromemory(@c, sizeof(c));
  c.ContextFlags:=CONTEXT_FULL;
  s:=threadTreeview.Selected;
  if s<>nil then
  begin


    while s.level>0 do
      s:=s.Parent;

    tid:=integer(s.data);

    if frmstacktrace=nil then
      frmstacktrace:=tfrmstacktrace.create(application);


    if debuggerthread<>nil then
    begin
      threadlist:=debuggerthread.lockThreadlist;
      try
        for i:=0 to threadlist.Count-1 do
        begin
          if TDebugThreadHandler(threadlist[i]).ThreadId=tid then
          begin
            {$ifdef cpu64}
            if processhandler.is64Bit=false then
            begin
              ZeroMemory(@c32, sizeof(c32));
              c32.ContextFlags:=CONTEXT_FULL;
              Wow64GetThreadContext(TDebugThreadHandler(threadlist[i]).handle, c32);
              c.Rip:=c32.Eip;
              c.Rsp:=c32.esp;
              c.Rbp:=c32.ebp;
            end
            else
            {$endif}
            GetThreadContext(TDebugThreadHandler(threadlist[i]).handle, c);
            break;
          end;
        end;
      finally
        debuggerthread.unlockThreadlist;
      end;
    end
    else
    begin
      th:=OpenThread(THREAD_GET_CONTEXT, false, tid);

      if th<>0 then
      begin
        {$ifdef cpu64}
        if processhandler.is64Bit=false then
        begin
          ZeroMemory(@c32, sizeof(c32));
          c32.ContextFlags:=CONTEXT_FULL; ;
          Wow64GetThreadContext(th, c32);
          c.Rip:=c32.Eip;
          c.Rsp:=c32.esp;
          c.Rbp:=c32.ebp;
        end
        else
        {$endif}
        GetThreadContext(th, c);

        closehandle(th);
      end;
    end;


    frmstacktrace.stacktrace(th, c);

    frmstacktrace.Show;
  end;




end;

procedure TFrmthreadlist.FillThreadlist;
type
  TExpandedInfo=record
    tid: dword;
    selectedIndex: integer; //mostly -1
  end;
  PExpandedInfo=^TExpandedInfo;

var
  i,j: integer;
  lastselected: integer;
  threadlist: tlist;
  li: TListitem;

  ths: THandle;
  te32: TThreadEntry32;
  p: PSystemProcesses;
  needed: dword;

  pp: PSystemProcesses;

  s: string;

  n:TTreenode;
  expandedList: TList;
  expinfo: PExpandedInfo;
begin
  expandedList:=TList.create;
  n:=threadtreeview.Items.GetFirstNode;
  while n<>nil do
  begin
    if n.Expanded then
    begin
      getmem(expinfo,sizeof(TExpandedInfo));
      expinfo^.tid:=integer(n.data);
      expinfo^.selectedIndex:=-1;

      for i:=0 to n.Count-1 do
        if n[i].Selected then
        begin
          expinfo^.selectedIndex:=i;
          break;
        end;

      expandedlist.Add(expinfo);
    end;

    n:=n.GetNextSibling;
  end;


  threadTreeview.BeginUpdate;
  threadTreeview.Items.Clear;

  if debuggerthread<>nil then
  begin
    threadlist:=debuggerthread.lockThreadlist;
    try
      for i:=0 to threadlist.Count-1 do
      begin
        s:=inttohex(TDebugThreadHandler(threadlist[i]).ThreadId,1);
        if debuggerthread.isWaitingToContinue and (debuggerthread.CurrentThread.ThreadId=TDebugThreadHandler(threadlist[i]).ThreadId) then
          s:=s+' ('+rsCurrent+')';

        n:=threadTreeview.Items.Add(nil,s);
        n.Data:=pointer(TDebugThreadHandler(threadlist[i]).ThreadId);
      end;

    finally
      debuggerthread.unlockThreadlist;
    end;
  end
  else
  begin
    //get the list using thread32first/next
    ths:=CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD,0);
    if ths<>INVALID_HANDLE_VALUE then
    begin
      zeromemory(@te32,sizeof(te32));
      te32.dwSize:=sizeof(te32);
      if Thread32First(ths, te32) then
      repeat
        if te32.th32OwnerProcessID=processid then
        begin
          n:=threadTreeview.Items.add(nil,inttohex(te32.th32ThreadID,1));
          n.data:=pointer(te32.th32ThreadID);
        end;

      until Thread32Next(ths, te32)=false;
      closehandle(ths);
    end;
  end;

  for i:=0 to threadTreeview.Items.Count-1 do
    threadTreeview.Items[i].HasChildren:=true;

  //expand the selected items
  for i:=0 to expandedlist.Count-1 do
  begin
    expinfo:=expandedlist.items[i];

    n:=threadtreeview.Items.GetFirstNode;
    while n<>nil do
    begin
      if integer(n.data)=expinfo^.tid then
      begin
        n.Expand(true);
        if (expinfo^.selectedIndex<>-1) and (expinfo^.selectedIndex<n.Count) then
          n[expinfo^.selectedIndex].Selected:=true;
      end;

      n:=n.GetNextSibling;
    end;

    freemem(expinfo);
  end;

  expandedlist.free;


  {if threadtreeview.Selected<>nil then
    threadtreeview.selected.MakeVisible;}

  threadTreeview.EndUpdate;
end;

procedure TfrmThreadlist.miBreakClick(Sender: TObject);
var threadlist: tlist;
i: integer;
begin
  if debuggerthread<>nil then
  begin
    if (threadTreeview.Selected<>nil) and (threadTreeview.selected.Level=0) then
    begin
      threadlist:=debuggerthread.lockThreadlist;
      try
        for i:=0 to threadlist.Count-1 do
        begin
          if TDebugThreadHandler(threadlist[i]).ThreadId=integer(threadTreeview.selected.data) then
          begin
            TDebugThreadHandler(threadlist[i]).breakThread;
            break;
          end;
        end;
      finally
        debuggerthread.unlockThreadlist;
      end;

    end;

  end
  else
    raise exception.create(rsPleaseFirstAttachTheDebuggerToThisProcess);

end;

procedure TfrmThreadlist.miClearDebugRegistersClick(Sender: TObject);
var threadlist: tlist;
i,j: integer;
s: ttreenode;
begin
  if debuggerthread<>nil then
  begin
    for j:=0 to threadTreeview.SelectionCount-1 do
    begin
      s:=threadTreeview.Selections[j];

      // if (threadTreeview.Selected<>nil) then
      begin
        //s:=threadTreeview.Selected;
        while s.level>0 do
          s:=s.parent;


        threadlist:=debuggerthread.lockThreadlist;
        try
          for i:=0 to threadlist.Count-1 do
          begin
            if TDebugThreadHandler(threadlist[i]).ThreadId=integer(s.data) then
            begin
              TDebugThreadHandler(threadlist[i]).clearDebugRegisters;
              break;
            end;
          end;
        finally
          debuggerthread.unlockThreadlist;
        end;
      end;

    end;

  end
  else
    raise exception.create(rsPleaseFirstAttachTheDebuggerToThisProcess);
end;

procedure TfrmThreadlist.miCopyValueToClipboardClick(Sender: TObject);
var
  s: string;
  i: integer;

  r: tstringlist;
begin
  if (threadTreeview.Selected<>nil) and (threadtreeview.Selected.Level=1) then
  begin
    if threadTreeview.SelectionCount>1 then
    begin
      r:=tstringlist.create;

      for i:=0 to threadTreeview.Items.count-1 do
        if threadTreeview.items[i].Selected then
          r.add(threadTreeview.items[i].text);

      Clipboard.AsText:=r.Text;
      r.free;
    end
    else
    begin
      s:=threadTreeview.Selected.text;
      i:=pos('=',s);
      if i<>-1 then
      begin
        s:=copy(s,i+1,length(s));
        Clipboard.AsText:=s;
      end;
    end;
  end;
end;

procedure TfrmThreadlist.miFreezeThreadClick(Sender: TObject);
var
  i: integer;
  s: TTreeNode;
  threadlist: tlist;

  tid: dword;
  th: THandle;
begin
  s:=threadTreeview.Selected;
  if s<>nil then
  begin

    while s.level>0 do
      s:=s.Parent;

    tid:=integer(s.data);

    if debuggerthread<>nil then
    begin
      threadlist:=debuggerthread.lockThreadlist;
      try
        for i:=0 to threadlist.Count-1 do
        begin
          if TDebugThreadHandler(threadlist[i]).ThreadId=tid then
          begin
            SuspendThread(TDebugThreadHandler(threadlist[i]).handle);
            exit;
          end;
        end;
      finally
        debuggerthread.unlockThreadlist;
      end;
    end;

    begin
      th:=OpenThread(THREAD_SUSPEND_RESUME, false, tid);

      if th<>0 then
      begin
        SuspendThread(th);
        closehandle(th);
      end;
    end;

  end;
end;

procedure TfrmThreadlist.miResumeThreadClick(Sender: TObject);
var
  i: integer;
  s: TTreeNode;
  threadlist: tlist;

  tid: dword;
  th: Thandle;
begin
  s:=threadTreeview.Selected;
  if s<>nil then
  begin

    while s.level>0 do
      s:=s.Parent;

    tid:=integer(s.data);

    if debuggerthread<>nil then
    begin
      threadlist:=debuggerthread.lockThreadlist;
      try
        for i:=0 to threadlist.Count-1 do
        begin
          if TDebugThreadHandler(threadlist[i]).ThreadId=tid then
          begin
            ResumeThread(TDebugThreadHandler(threadlist[i]).handle);
            exit;
          end;
        end;
      finally
        debuggerthread.unlockThreadlist;
      end;
    end;

    begin
      th:=OpenThread(THREAD_SUSPEND_RESUME, false, tid);

      if th<>0 then
      begin
        ResumeThread(th);
        closehandle(th);
      end;
    end;

  end;
end;

procedure TfrmThreadlist.PopupMenu1Popup(Sender: TObject);
begin
  miCopyValueToClipboard.visible:=(threadTreeview.Selected<>nil) and (threadtreeview.Selected.Level=1);
end;

procedure TfrmThreadlist.threadTreeviewDblClick(Sender: TObject);
var s: TTreeNode;
  th: thandle;
  cp: pcontext;



  regnr: integer;

  regaddress: PPtrUInt;

  v: ptruint;

  input: string;
  tid: dword;

  ai: integer;
  x: boolean;
  {$ifdef cpu64}
  use32bitcontext: boolean;
  c32: TContext32;
  {$endif}

  isCurrentDebuggedThread: boolean;
begin
  //change the selected register
  isCurrentDebuggedThread:=false;

  s:=threadTreeview.Selected;
  if (s<>nil) and (s.level=1) then //selected a registers
  begin
    regnr:=s.Index;
    ai:=s.AbsoluteIndex;

    while s.level>0 do
      s:=s.Parent;

    tid:=integer(s.data);

    if (tid=GetCurrentThreadId) then exit; //don't accidentally freeze the ce main thread

    isCurrentDebuggedThread:=(debuggerthread<>nil) and debuggerthread.isWaitingToContinue and (debuggerthread.CurrentThread.ThreadId=tid);

    if isCurrentDebuggedThread then
      th:=debuggerthread.CurrentThread.handle
    else
      th:=OpenThread(THREAD_SUSPEND_RESUME or THREAD_GET_CONTEXT or THREAD_SET_CONTEXT or THREAD_QUERY_INFORMATION, false, tid);

    if (th<>0) then
    begin
      if not isCurrentDebuggedThread then
        suspendthread(th);

      x:=false;

      if isCurrentDebuggedThread then
        cp:=debuggerthread.CurrentThread.context
      else
      begin
        getmem(cp,sizeof(TCONTEXT)+4096);
        ZeroMemory(cp, sizeof(TCONTEXT)+4096);
      end;

      try
        {$ifdef cpu64}
        use32bitcontext:=(not processhandler.is64Bit) and (ssctrl in GetKeyShiftState);


        if use32bitcontext then
        begin
          //override, the user wants the 32-bit context

          ZeroMemory(@c32, sizeof(c32));
          c32.ContextFlags:=CONTEXT_ALL;
          x:=WOW64GetThreadContext(th, c32);
          if x then
          begin
            //convert
            cp^.Dr0:=c32.Dr0;
            cp^.Dr1:=c32.Dr1;
            cp^.Dr2:=c32.Dr2;
            cp^.Dr3:=c32.Dr3;
            cp^.Dr6:=c32.Dr6;
            cp^.Dr7:=c32.Dr7;
            cp^.Rax:=c32.eax;
            cp^.Rbx:=c32.ebx;
            cp^.Rcx:=c32.ecx;
            cp^.Rdx:=c32.edx;
            cp^.Rsi:=c32.esi;
            cp^.Rdi:=c32.edi;
            cp^.Rbp:=c32.ebp;
            cp^.Rsp:=c32.esp;
            cp^.Rip:=c32.eip;


          end;
        end
        else
        {$endif}
        begin
          if isCurrentDebuggedThread=false then
          begin
            cp^.ContextFlags:=CONTEXT_ALL or CONTEXT_EXTENDED_REGISTERS;
            x:=GetThreadContext(th, cp^);
          end
          else
            x:=true;
        end;



        if x then
        begin
          case regnr of
            0: regaddress:=@cp^.Dr0;
            1: regaddress:=@cp^.Dr1;
            2: regaddress:=@cp^.Dr2;
            3: regaddress:=@cp^.Dr3;
            4: regaddress:=@cp^.Dr6;
            5: regaddress:=@cp^.Dr7;

            6: regaddress:=@cp^.{$ifdef cpu64}rax{$else}eax{$endif};
            7: regaddress:=@cp^.{$ifdef cpu64}rbx{$else}ebx{$endif};
            8: regaddress:=@cp^.{$ifdef cpu64}rcx{$else}ecx{$endif};
            9: regaddress:=@cp^.{$ifdef cpu64}rdx{$else}edx{$endif};
            10: regaddress:=@cp^.{$ifdef cpu64}rsi{$else}esi{$endif};
            11: regaddress:=@cp^.{$ifdef cpu64}rdi{$else}edi{$endif};
            12: regaddress:=@cp^.{$ifdef cpu64}rbp{$else}ebp{$endif};
            13: regaddress:=@cp^.{$ifdef cpu64}rsp{$else}esp{$endif};
            14: regaddress:=@cp^.{$ifdef cpu64}rip{$else}eip{$endif};

            {$ifdef cpu64}
            15: regaddress:=@cp^.r8;
            16: regaddress:=@cp^.r9;
            17: regaddress:=@cp^.r10;
            18: regaddress:=@cp^.r11;
            19: regaddress:=@cp^.r12;
            20: regaddress:=@cp^.r13;
            21: regaddress:=@cp^.r14;
            22: regaddress:=@cp^.r15;
            {$endif}
            else
              raise exception.create('Invalid register');
          end;

          if processhandler.is64Bit then
            v:=regaddress^
          else
            v:=pdword(regaddress)^;

          input:=inttohex(v,8);
          InputQuery(rsTLChangeValue,rsTLWhatShouldTheNewValueOfThisRegisterBe, input);

          v:=symhandler.getAddressFromName(input);

          if processhandler.is64Bit then
            regaddress^:=v
          else
            pdword(regaddress)^:=v;

          {$ifdef cpu64}
          if use32bitcontext then
          begin
            c32.ContextFlags:=CONTEXT_ALL;
            c32.Dr0:=cp^.Dr0;
            c32.Dr1:=cp^.Dr1;
            c32.Dr2:=cp^.Dr2;
            c32.Dr3:=cp^.Dr3;
            c32.Dr6:=cp^.Dr6;
            c32.Dr7:=cp^.Dr7;
            c32.eax:=cp^.Rax;
            c32.ebx:=cp^.rbx;
            c32.ecx:=cp^.rcx;
            c32.edx:=cp^.rdx;
            c32.esi:=cp^.rsi;
            c32.edi:=cp^.rdi;
            c32.ebp:=cp^.rbp;
            c32.esp:=cp^.rsp;
            c32.eip:=cp^.rip;

            if WOW64SetThreadContext(th, c32)=false then
              showmessage(rsTLFailedErrorcode+inttostr(GetLastError));
          end
          else
          {$endif}
          begin
            if isCurrentDebuggedThread then
            begin
              debuggerthread.CurrentThread.setContext;
              debuggerthread.CurrentThread.fillContext;


            end
            else
            begin
              cp^.ContextFlags:=CONTEXT_ALL or CONTEXT_EXTENDED_REGISTERS;
              if SetThreadContext(th, cp^)=false then
                showmessage(rsTLFailedErrorcode+inttostr(GetLastError));
            end;
          end;
        end;

        if isCurrentDebuggedThread=false then
        begin
          resumethread(th);
          closehandle(th);
        end;


        s.Collapse(true);
        s.DeleteChildren;
        s.HasChildren:=true;
        s.Expand(true);


        //threadTreeviewExpanding(threadTreeview, s,x);

        //threadTreeview.Items.SelectOnlyThis(threadTreeview.Items[ai]);
        threadTreeview.Selected:=threadTreeview.Items[ai];
        threadTreeview.selected.MakeVisible;
      finally
        if isCurrentDebuggedThread=false then
          freemem(cp);
      end;
    end;
  end;
end;





procedure TfrmThreadlist.threadTreeviewExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
function rw2str(x: byte): string;
begin
  result:='';
  case x of
    0: result:=rsTLExecute;
    1: result:=rsTLWrite;
    2: result:=rsTLIO;
    3: result:=rsTLAccess;
  end;
end;

function len2str(x: byte): string;
begin
  result:='';
  case x of
    0: result:=rsTL1Bytes;
    1: result:=rsTL2Bytes;
    2: result:=rsTL8Bytes;
    3: result:=rsTL4Bytes;
  end;
end;

var
  c: TContext;
  ca: TARMCONTEXT;
  tid: dword;
  th: thandle;

  prefix: char;
  s: string;
  rw: byte;
  len: byte;

  drinfo: string;

  ldtentry: TLDTEntry;
  i: integer;
  x: boolean;

  tempp: ptruint;

  {$ifdef cpu64}
  use32bitcontext: boolean;
  c32: TContext32;
  {$endif}

  cenet: TCEconnection;

  tbi: THREAD_BASIC_INFORMATION;
begin
  drinfo:=' ';

  if node.level=0 then
  begin
    //extract thread info
    if node.HasChildren then
      Node.DeleteChildren;

    tid:=integer(node.Data);

    cenet:=getConnection;
    if cenet<>nil then
    begin
      if debuggerthread<>nil then
      begin
        if processhandler.SystemArchitecture=archArm then
        begin
          GetThreadContextArm(tid, ca);
          with threadTreeview.items do
          begin
            AddChild(node,'R0='+inttohex(ca.R0,8));
            AddChild(node,'R1='+inttohex(ca.R1,8));
            AddChild(node,'R2='+inttohex(ca.R2,8));
            AddChild(node,'R3='+inttohex(ca.R3,8));
            AddChild(node,'R4='+inttohex(ca.R4,8));
            AddChild(node,'R5='+inttohex(ca.R5,8));
            AddChild(node,'R6='+inttohex(ca.R6,8));
            AddChild(node,'R7='+inttohex(ca.R7,8));
            AddChild(node,'R8='+inttohex(ca.R8,8));
            AddChild(node,'R9='+inttohex(ca.R9,8));
            AddChild(node,'R10='+inttohex(ca.R10,8));
            AddChild(node,'FP='+inttohex(ca.FP,8));
            AddChild(node,'IP='+inttohex(ca.IP,8));
            AddChild(node,'SP='+inttohex(ca.SP,8));
            AddChild(node,'LR='+inttohex(ca.LR,8));
            AddChild(node,'PC='+inttohex(ca.PC,8));
            AddChild(node,'CPSR='+inttohex(ca.CPSR,8));
            AddChild(node,'ORIG_R0='+inttohex(ca.ORIG_R0,8));

          end;

        end
        else
        begin
          GetThreadContext(tid, c);

          if processhandler.is64Bit then
            prefix:='r'
          else
            prefix:='e';

          //no dr access yet for x86
          with threadTreeview.items do
          begin
            AddChild(node,prefix+'ax='+inttohex(c.{$ifdef cpu64}rax{$else}eax{$endif},8));
            AddChild(node,prefix+'bx='+inttohex(c.{$ifdef cpu64}rbx{$else}ebx{$endif},8));
            AddChild(node,prefix+'cx='+inttohex(c.{$ifdef cpu64}rcx{$else}ecx{$endif},8));
            AddChild(node,prefix+'dx='+inttohex(c.{$ifdef cpu64}rdx{$else}edx{$endif},8));
            AddChild(node,prefix+'si='+inttohex(c.{$ifdef cpu64}rsi{$else}esi{$endif},8));
            AddChild(node,prefix+'di='+inttohex(c.{$ifdef cpu64}rdi{$else}edi{$endif},8));
            AddChild(node,prefix+'bp='+inttohex(c.{$ifdef cpu64}rbp{$else}ebp{$endif},8));
            AddChild(node,prefix+'sp='+inttohex(c.{$ifdef cpu64}rsp{$else}esp{$endif},8));
            AddChild(node,prefix+'ip='+inttohex(c.{$ifdef cpu64}rip{$else}eip{$endif},8));

            {$ifdef cpu64}
            if processhandler.is64bit then
            begin
              AddChild(node,'r8='+inttohex(c.r8,8));
              AddChild(node,'r9='+inttohex(c.r9,8));
              AddChild(node,'r10='+inttohex(c.r10,8));
              AddChild(node,'r11='+inttohex(c.r11,8));
              AddChild(node,'r12='+inttohex(c.r12,8));
              AddChild(node,'r13='+inttohex(c.r13,8));
              AddChild(node,'r14='+inttohex(c.r14,8));
              AddChild(node,'r15='+inttohex(c.r15,8));
            end;
            {$endif}

            threadTreeview.items.AddChild(node,'cs='+inttohex(c.SegCs,8));
            {$ifdef cpu64}
            //context has room for some extra data
            AddChild(node,'fsbase='+inttohex(c.P2Home,8));
            AddChild(node,'gsbase='+inttohex(c.P3Home,8));
            {$endif}



          end;

        end;

      end
      else
      begin
        //threads can only be inspected when debugging (for now)
        beep;
        allowexpansion:=false;
        exit;
      end;
    end
    else
    begin

      th:=OpenThread(THREAD_QUERY_INFORMATION or THREAD_GET_CONTEXT, false, tid);
      if th<>0 then
      begin
        zeromemory(@c,SizeOf(c));
        c.ContextFlags:=CONTEXT_ALL or CONTEXT_EXTENDED_REGISTERS;

        {$ifdef cpu64}
        use32bitcontext:=(not processhandler.is64Bit) and (ssctrl in GetKeyShiftState);

        if use32bitcontext then
        begin
          //override, the user wants the 32-bit context

          ZeroMemory(@c32, sizeof(c32));
          c32.ContextFlags:=CONTEXT_ALL;
          x:=WOW64GetThreadContext(th, c32);
          if x then
          begin
            //convert
            c.Dr0:=c32.Dr0;
            c.Dr1:=c32.Dr1;
            c.Dr2:=c32.Dr2;
            c.Dr3:=c32.Dr3;
            c.Dr6:=c32.Dr6;
            c.Dr7:=c32.Dr7;
            c.Rax:=c32.eax;
            c.Rbx:=c32.ebx;
            c.Rcx:=c32.ecx;
            c.Rdx:=c32.edx;
            c.Rsi:=c32.esi;
            c.Rdi:=c32.edi;
            c.Rbp:=c32.ebp;
            c.Rsp:=c32.esp;
            c.Rip:=c32.eip;
            c.SegCs:=c32.SegCs;
            c.SegGs:=c32.seggs;
            c.segfs:=c32.SegFs;
          end;
        end
        else
        {$endif}
        x:=getThreadContext(th, c);

        if x then
        begin
          threadTreeview.items.AddChild(node,'dr0='+inttohex(c.Dr0,{$ifdef cpu64}16{$else}8{$endif}));
          threadTreeview.items.AddChild(node,'dr1='+inttohex(c.Dr1,{$ifdef cpu64}16{$else}8{$endif}));
          threadTreeview.items.AddChild(node,'dr2='+inttohex(c.Dr2,{$ifdef cpu64}16{$else}8{$endif}));
          threadTreeview.items.AddChild(node,'dr3='+inttohex(c.Dr3,{$ifdef cpu64}16{$else}8{$endif}));
          threadTreeview.items.AddChild(node,'dr6='+inttohex(c.Dr6,{$ifdef cpu64}16{$else}8{$endif}));

          s:='dr7='+inttohex(c.Dr7,{$ifdef cpu64}16{$else}8{$endif});

          if c.dr7 and 1=1 then
          begin
            rw:=(c.dr7 shr 16) and 3;
            len:=(c.dr7 shr 18) and 3;

            drinfo:=drinfo+'1('+rw2str(rw)+' - '+len2str(len)+') ';
          end;

          if c.dr7 and 4=4 then
          begin
            rw:=(c.dr7 shr 20) and 3;
            len:=(c.dr7 shr 22) and 3;

            drinfo:=drinfo+'2('+rw2str(rw)+' - '+len2str(len)+') ';
          end;

          if c.dr7 and 16=16 then
          begin
            s:=s+'3(';
            rw:=(c.dr7 shr 24) and 3;
            len:=(c.dr7 shr 26) and 3;

            drinfo:=drinfo+'3('+rw2str(rw)+' - '+len2str(len)+') ';
          end;

          if c.dr7 and 64=64 then
          begin

            rw:=(c.dr7 shr 28) and 3;
            len:=(c.dr7 shr 30) and 3;

            drinfo:=drinfo+'4('+rw2str(rw)+' - '+len2str(len)+') ';
          end;



          if drinfo<>'' then
            s:=s+' :'+drinfo;

          threadTreeview.items.AddChild(node,s);

          if processhandler.is64Bit then
            prefix:='r'
          else
            prefix:='e';

          threadTreeview.items.AddChild(node,prefix+'ax='+inttohex(c.{$ifdef cpu64}rax{$else}eax{$endif},8));
          threadTreeview.items.AddChild(node,prefix+'bx='+inttohex(c.{$ifdef cpu64}rbx{$else}ebx{$endif},8));
          threadTreeview.items.AddChild(node,prefix+'cx='+inttohex(c.{$ifdef cpu64}rcx{$else}ecx{$endif},8));
          threadTreeview.items.AddChild(node,prefix+'dx='+inttohex(c.{$ifdef cpu64}rdx{$else}edx{$endif},8));
          threadTreeview.items.AddChild(node,prefix+'si='+inttohex(c.{$ifdef cpu64}rsi{$else}esi{$endif},8));
          threadTreeview.items.AddChild(node,prefix+'di='+inttohex(c.{$ifdef cpu64}rdi{$else}edi{$endif},8));
          threadTreeview.items.AddChild(node,prefix+'bp='+inttohex(c.{$ifdef cpu64}rbp{$else}ebp{$endif},8));
          threadTreeview.items.AddChild(node,prefix+'sp='+inttohex(c.{$ifdef cpu64}rsp{$else}esp{$endif},8));
          threadTreeview.items.AddChild(node,prefix+'ip='+inttohex(c.{$ifdef cpu64}rip{$else}eip{$endif},8));

          {$ifdef cpu64}
          if processhandler.is64bit then
          begin
            threadTreeview.items.AddChild(node,'r8='+inttohex(c.r8,8));
            threadTreeview.items.AddChild(node,'r9='+inttohex(c.r9,8));
            threadTreeview.items.AddChild(node,'r10='+inttohex(c.r10,8));
            threadTreeview.items.AddChild(node,'r11='+inttohex(c.r11,8));
            threadTreeview.items.AddChild(node,'r12='+inttohex(c.r12,8));
            threadTreeview.items.AddChild(node,'r13='+inttohex(c.r13,8));
            threadTreeview.items.AddChild(node,'r14='+inttohex(c.r14,8));
            threadTreeview.items.AddChild(node,'r15='+inttohex(c.r15,8));
          end;
          {$endif}

          threadTreeview.items.AddChild(node,'cs='+inttohex(c.SegCs,8));


          i:=NtQueryInformationThread(th, ThreadBasicInformation, @tbi, sizeof(tbi), @x);
          if i=0 then
            threadTreeview.items.AddChild(node,'TEB='+inttohex(qword(tbi.TebBaseAddress),8));

          if processhandler.is64Bit=false then
          begin
            if GetThreadSelectorEntry(th, c.segFs, ldtentry) then
              threadTreeview.items.AddChild(node,'fsbase='+inttohex(ldtentry.BaseLow+ldtentry.HighWord.Bytes.BaseMid shl 16+ldtentry.HighWord.Bytes.BaseHi shl 24,8));

            if GetThreadSelectorEntry(th, c.SegGs, ldtentry) then
              threadTreeview.items.AddChild(node,'gsbase='+inttohex(ldtentry.BaseLow+ldtentry.HighWord.Bytes.BaseMid shl 16+ldtentry.HighWord.Bytes.BaseHi shl 24,8));
          end;

        end
        else threadTreeview.items.AddChild(node, rsCouldnTObtainContext);
        closehandle(th);
      end else
        threadTreeview.items.AddChild(node, rsCouldnTOpenHandle);

    end;

    AllowExpansion:=true;
  end
  else
    AllowExpansion:=false;
end;

initialization
  {$i frmThreadlistunit.lrs}

end.
