unit frmThreadlistunit;

{$MODE Delphi}

interface

uses
  jwawindows, windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, StdCtrls, LResources,cefuncproc, CEDebugger, debugHelper,
  newkernelhandler, networkInterface, networkInterfaceApi;




type

  { TfrmThreadlist }

  TfrmThreadlist = class(TForm)
    lblIsWOW64: TLabel;
    MenuItem1: TMenuItem;
    miClearDebugRegisters: TMenuItem;
    miFreezeThread: TMenuItem;
    miResumeThread: TMenuItem;
    PopupMenu1: TPopupMenu;
    miBreak: TMenuItem;
    threadTreeview: TTreeView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure miBreakClick(Sender: TObject);
    procedure miClearDebugRegistersClick(Sender: TObject);
    procedure miFreezeThreadClick(Sender: TObject);
    procedure miResumeThreadClick(Sender: TObject);
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
  ProcessHandlerUnit, Parsers;

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

procedure TfrmThreadlist.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
  frmthreadlist:=nil;
end;

procedure TfrmThreadlist.FormCreate(Sender: TObject);
begin
  fillthreadlist;
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

    tid:=strtoint('$'+s.Text);

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

var i: integer;
    lastselected: integer;
    threadlist: tlist;
    li: TListitem;

    ths: THandle;
    te32: TThreadEntry32;
    p: PSystemProcesses;
    needed: dword;

    pp: PSystemProcesses;
begin
  if threadTreeview.Selected<>nil then
    lastselected:=threadTreeview.selected.index
  else
    lastselected:=-1;

  threadTreeview.BeginUpdate;
  threadTreeview.Items.Clear;

  if debuggerthread<>nil then
  begin
    threadlist:=debuggerthread.lockThreadlist;
    try
      for i:=0 to threadlist.Count-1 do
        threadTreeview.Items.Add(nil,inttohex(TDebugThreadHandler(threadlist[i]).ThreadId,1));

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
          threadTreeview.Items.add(nil,inttohex(te32.th32ThreadID,1));

      until Thread32Next(ths, te32)=false;
      closehandle(ths);
    end;



  end;

  for i:=0 to threadTreeview.Items.Count-1 do
    threadTreeview.Items[i].HasChildren:=true;

  if (lastselected<>-1) and (threadTreeview.Items.Count>lastselected) then
    threadTreeview.Items[lastselected].Selected:=true;

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
          if TDebugThreadHandler(threadlist[i]).ThreadId=strtoint('$'+threadTreeview.selected.Text) then
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
            if TDebugThreadHandler(threadlist[i]).ThreadId=strtoint('$'+s.Text) then
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

    tid:=strtoint('$'+s.Text);

    if debuggerthread<>nil then
    begin
      threadlist:=debuggerthread.lockThreadlist;
      try
        for i:=0 to threadlist.Count-1 do
        begin
          if TDebugThreadHandler(threadlist[i]).ThreadId=tid then
          begin
            SuspendThread(TDebugThreadHandler(threadlist[i]).handle);
            break;
          end;
        end;
      finally
        debuggerthread.unlockThreadlist;
      end;
    end
    else
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

    tid:=strtoint('$'+s.Text);

    if debuggerthread<>nil then
    begin
      threadlist:=debuggerthread.lockThreadlist;
      try
        for i:=0 to threadlist.Count-1 do
        begin
          if TDebugThreadHandler(threadlist[i]).ThreadId=tid then
          begin
            SuspendThread(TDebugThreadHandler(threadlist[i]).handle);
            break;
          end;
        end;
      finally
        debuggerthread.unlockThreadlist;
      end;
    end
    else
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

procedure TfrmThreadlist.threadTreeviewDblClick(Sender: TObject);
var s: TTreeNode;
  th: thandle;
  c: tcontext;


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
begin
  //change the selected register


  s:=threadTreeview.Selected;
  if (s<>nil) and (s.level=1) then //selected a registers
  begin
    regnr:=s.Index;
    ai:=s.AbsoluteIndex;

    while s.level>0 do
      s:=s.Parent;

    tid:=strtoint('$'+s.Text);




    th:=OpenThread(THREAD_SUSPEND_RESUME or THREAD_GET_CONTEXT or THREAD_SET_CONTEXT or THREAD_QUERY_INFORMATION, false, tid);

    if th<>0 then
    begin
      suspendthread(th);

      x:=false;
      ZeroMemory(@c, sizeof(c));

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


        end;
      end
      else
      {$endif}
      begin

        c.ContextFlags:=CONTEXT_ALL or CONTEXT_EXTENDED_REGISTERS;
        x:=GetThreadContext(th, c);
      end;



      if x then
      begin
        case regnr of
          0: regaddress:=@c.Dr0;
          1: regaddress:=@c.Dr1;
          2: regaddress:=@c.Dr2;
          3: regaddress:=@c.Dr3;
          4: regaddress:=@c.Dr6;
          5: regaddress:=@c.Dr7;

          6: regaddress:=@c.{$ifdef cpu64}rax{$else}eax{$endif};
          7: regaddress:=@c.{$ifdef cpu64}rbx{$else}ebx{$endif};
          8: regaddress:=@c.{$ifdef cpu64}rcx{$else}ecx{$endif};
          9: regaddress:=@c.{$ifdef cpu64}rdx{$else}edx{$endif};
          10: regaddress:=@c.{$ifdef cpu64}rsi{$else}esi{$endif};
          11: regaddress:=@c.{$ifdef cpu64}rdi{$else}edi{$endif};
          12: regaddress:=@c.{$ifdef cpu64}rbp{$else}ebp{$endif};
          13: regaddress:=@c.{$ifdef cpu64}rsp{$else}esp{$endif};
          14: regaddress:=@c.{$ifdef cpu64}rip{$else}eip{$endif};

          {$ifdef cpu64}
          15: regaddress:=@c.r8;
          16: regaddress:=@c.r9;
          17: regaddress:=@c.r10;
          18: regaddress:=@c.r11;
          19: regaddress:=@c.r12;
          20: regaddress:=@c.r13;
          21: regaddress:=@c.r14;
          22: regaddress:=@c.r15;
          {$endif}
          else
            regaddress:=@v; //in case of bugs
        end;

        if processhandler.is64Bit then
          v:=regaddress^
        else
          v:=pdword(regaddress)^;

        input:=inttohex(v,8);
        InputQuery(rsTLChangeValue,rsTLWhatShouldTheNewValueOfThisRegisterBe, input);

        v:=StrToQWordEx('$'+input);

        if processhandler.is64Bit then
          regaddress^:=v
        else
          pdword(regaddress)^:=v;

        {$ifdef cpu64}
        if use32bitcontext then
        begin
          c32.ContextFlags:=CONTEXT_ALL;
          c32.Dr0:=c.Dr0;
          c32.Dr1:=c.Dr1;
          c32.Dr2:=c.Dr2;
          c32.Dr3:=c.Dr3;
          c32.Dr6:=c.Dr6;
          c32.Dr7:=c.Dr7;
          c32.eax:=c.Rax;
          c32.ebx:=c.rbx;
          c32.ecx:=c.rcx;
          c32.edx:=c.rdx;
          c32.esi:=c.rsi;
          c32.edi:=c.rdi;
          c32.ebp:=c.rbp;
          c32.esp:=c.rsp;
          c32.eip:=c.rip;

          if WOW64SetThreadContext(th, c32)=false then
            showmessage(rsTLFailedErrorcode+inttostr(GetLastError));
        end
        else
        {$endif}
        begin
          c.ContextFlags:=CONTEXT_ALL or CONTEXT_EXTENDED_REGISTERS;
          if SetThreadContext(th, c)=false then
            showmessage(rsTLFailedErrorcode+inttostr(GetLastError));
        end;
      end;

      resumethread(th);
      closehandle(th);



      threadTreeviewExpanding(threadTreeview, s,x);


      threadTreeview.Items.SelectOnlyThis(threadTreeview.Items[ai]);
      threadTreeview.Selected:=threadTreeview.Items[ai];



    end;


  end;




  //suspend the thread
  //get the current register value
  //show and edit
  //convert back to integer
  //resume thread

end;





procedure TfrmThreadlist.threadTreeviewExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
function rw2str(x: byte): string;
begin
  case x of
    0: result:=rsTLExecute;
    1: result:=rsTLWrite;
    2: result:=rsTLIO;
    3: result:=rsTLAccess;
  end;
end;

function len2str(x: byte): string;
begin
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

  {$ifdef cpu64}
  use32bitcontext: boolean;
  c32: TContext32;
  {$endif}

  cenet: TCEconnection;
begin
  if node.level=0 then
  begin
    //extract thread info
    if node.HasChildren then
      Node.DeleteChildren;

    tid:=strtoint('$'+Node.text);

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
            rw:=(c.dr7 shr 16) and 3;
            len:=(c.dr7 shr 18) and 3;

            drinfo:=drinfo+'2('+rw2str(rw)+' - '+len2str(len)+') ';
          end;

          if c.dr7 and 16=16 then
          begin
            s:=s+'3(';
            rw:=(c.dr7 shr 16) and 3;
            len:=(c.dr7 shr 18) and 3;

            drinfo:=drinfo+'3('+rw2str(rw)+' - '+len2str(len)+') ';
          end;

          if c.dr7 and 64=64 then
          begin

            rw:=(c.dr7 shr 16) and 3;
            len:=(c.dr7 shr 18) and 3;

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


          if GetThreadSelectorEntry(th, c.segFs, ldtentry) then
            threadTreeview.items.AddChild(node,'fsbase='+inttohex(ldtentry.BaseLow+ldtentry.HighWord.Bytes.BaseMid shl 16+ldtentry.HighWord.Bytes.BaseHi shl 24,8));

          if GetThreadSelectorEntry(th, c.SegGs, ldtentry) then
            threadTreeview.items.AddChild(node,'gsbase='+inttohex(ldtentry.BaseLow+ldtentry.HighWord.Bytes.BaseMid shl 16+ldtentry.HighWord.Bytes.BaseHi shl 24,8));


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
