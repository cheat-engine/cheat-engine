unit ThreadlistExFRM;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  jwawindows, windows,
  {$endif}
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,NewKernelHandler,{tlhelp32,}CEFuncProc,
  ExtCtrls, LResources;

type
  TfrmThreadlistEx = class(TForm)
    TreeView1: TTreeView;
    Panel1: TPanel;
    Button1: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure updatelist;
  end;

var
  frmThreadlistEx: TfrmThreadlistEx;

implementation

uses dbk32functions, ProcessHandlerUnit;

procedure TfrmThreadListEx.updatelist;
var ths: thandle;
    te32: THREADENTRY32;
    th: thandle;
    cont: _context;

    x: ttreenode;
    ok: boolean;
begin
  treeview1.Items.Clear;
  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD,processid);
  try
    te32.dwSize:=sizeof(te32);
    if Thread32First(ths,te32) then
    repeat
      if te32.th32OwnerProcessID=processid then
      begin
        x:=treeview1.Items.Add(nil,inttohex(te32.th32ThreadID,8){$IFDEF windows} +' ('+inttohex(getpethread(te32.th32ThreadID),8)+')' {$ENDIF});

        if dword(getcurrentthreadid)<>te32.th32ThreadID then
        begin
          th:=openthread({$IFDEF windows} STANDARD_RIGHTS_REQUIRED or windows.synchronize or {$ENDIF} $3ff,true,te32.th32ThreadID);
          try
            suspendthread(th);
            cont.ContextFlags:=CONTEXT_FULL or CONTEXT_DEBUG_REGISTERS;
            ok:=getthreadcontext(th,cont);
            resumethread(th);

            if ok then
            begin
              treeview1.Items.AddChild(x,'EAX='+inttohex(cont.{$ifdef cpu64}rax{$else}Eax{$endif},8));
              treeview1.Items.AddChild(x,'EBX='+inttohex(cont.{$ifdef cpu64}rbx{$else}Ebx{$endif},8));
              treeview1.Items.AddChild(x,'ECX='+inttohex(cont.{$ifdef cpu64}rcx{$else}Ecx{$endif},8));
              treeview1.Items.AddChild(x,'EDX='+inttohex(cont.{$ifdef cpu64}rdx{$else}Edx{$endif},8));
              treeview1.Items.AddChild(x,'ESI='+inttohex(cont.{$ifdef cpu64}rsi{$else}Esi{$endif},8));
              treeview1.Items.AddChild(x,'EDI='+inttohex(cont.{$ifdef cpu64}rdi{$else}Edi{$endif},8));
              treeview1.Items.AddChild(x,'EBP='+inttohex(cont.{$ifdef cpu64}rbp{$else}Ebp{$endif},8));
              treeview1.Items.AddChild(x,'ESP='+inttohex(cont.{$ifdef cpu64}rsp{$else}Esp{$endif},8));
              treeview1.Items.AddChild(x,'EIP='+inttohex(cont.{$ifdef cpu64}rip{$else}Eip{$endif},8));
              treeview1.Items.AddChild(x,'DR0='+inttohex(cont.Dr0,8));
              treeview1.Items.AddChild(x,'DR1='+inttohex(cont.Dr1,8));
              treeview1.Items.AddChild(x,'DR2='+inttohex(cont.Dr2,8));
              treeview1.Items.AddChild(x,'DR3='+inttohex(cont.Dr3,8));
              treeview1.Items.AddChild(x,'DR6='+inttohex(cont.Dr6,8));
              treeview1.Items.AddChild(x,'DR7='+inttohex(cont.Dr7,8));
              treeview1.Items.AddChild(x,'EFLAGS='+inttohex(cont.EFlags,8));
            end;
          finally
            closehandle(th);
          end;
        end;
      end;

    until Thread32next(ths,te32)=false;

  finally
    closehandle(ths);
  end;
end;

procedure TfrmThreadlistEx.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
  frmThreadlistEx:=nil;
end;

procedure TfrmThreadlistEx.Button1Click(Sender: TObject);
begin
  close;
end;

initialization
  {$i ThreadlistExFRM.lrs}

end.
