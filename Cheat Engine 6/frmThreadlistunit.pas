unit frmThreadlistunit;

{$MODE Delphi}

interface

uses
  jwawindows, windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, StdCtrls, LResources,cefuncproc, CEDebugger, debugHelper, newkernelhandler;

type

  { TfrmThreadlist }

  TfrmThreadlist = class(TForm)
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Break1: TMenuItem;
    threadTreeview: TTreeView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Break1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
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

uses debugeventhandler;

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

procedure TFrmthreadlist.FillThreadlist;
var i: integer;
    lastselected: integer;
    threadlist: tlist;
    li: TListitem;

    ths: THandle;
    te32: TThreadEntry32;
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

procedure TfrmThreadlist.Break1Click(Sender: TObject);
var threadlist: tlist;
i: integer;
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

end;

procedure TfrmThreadlist.MenuItem1Click(Sender: TObject);
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
            TDebugThreadHandler(threadlist[i]).clearDebugRegisters;
            break;
          end;
        end;
      finally
        debuggerthread.unlockThreadlist;
      end;
    end;

  end;
end;

procedure TfrmThreadlist.threadTreeviewExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var tid: dword;
th: thandle;
c: TContext;
prefix: char;
begin
  if node.level=0 then
  begin
    //extract thread info
    if node.HasChildren then
      Node.DeleteChildren;

    tid:=strtoint('$'+Node.text);
    th:=OpenThread(THREAD_QUERY_INFORMATION or THREAD_GET_CONTEXT, false, tid);
    if th<>0 then
    begin
      zeromemory(@c,SizeOf(c));
      c.ContextFlags:=CONTEXT_ALL or CONTEXT_EXTENDED_REGISTERS;
      if GetThreadContext(th, c) then
      begin
        threadTreeview.items.AddChild(node,'dr0='+inttohex(c.Dr0,{$ifdef cpu64}16{$else}8{$endif}));
        threadTreeview.items.AddChild(node,'dr1='+inttohex(c.Dr1,{$ifdef cpu64}16{$else}8{$endif}));
        threadTreeview.items.AddChild(node,'dr2='+inttohex(c.Dr2,{$ifdef cpu64}16{$else}8{$endif}));
        threadTreeview.items.AddChild(node,'dr3='+inttohex(c.Dr3,{$ifdef cpu64}16{$else}8{$endif}));
        threadTreeview.items.AddChild(node,'dr6='+inttohex(c.Dr6,{$ifdef cpu64}16{$else}8{$endif}));
        threadTreeview.items.AddChild(node,'dr7='+inttohex(c.Dr7,{$ifdef cpu64}16{$else}8{$endif}));

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
      end
      else threadTreeview.items.AddChild(node, 'Couldn''t obtain context');
      closehandle(th);
    end else
      threadTreeview.items.AddChild(node, 'Couldn''t open handle');

    AllowExpansion:=true;
  end
  else
    AllowExpansion:=false;
end;

initialization
  {$i frmThreadlistunit.lrs}

end.
