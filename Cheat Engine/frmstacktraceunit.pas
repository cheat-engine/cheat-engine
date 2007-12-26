unit frmstacktraceunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,cefuncproc, ComCtrls,imagehlp,debugger;

type
  TfrmStacktrace = class(TForm)
    ListView1: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }

  public
    { Public declarations }
    procedure stacktrace(threadhandle:thandle;context:_context);
  end;

var
  frmStacktrace: TfrmStacktrace;

implementation

{$R *.dfm}

procedure TfrmStacktrace.stacktrace(threadhandle:thandle;context:_context);
var stackframe: PStackframe;
    cxt:_context;
    a,b,c,d: dword;
    sa,sb,sc,sd:string;
begin
  cxt:=context;

  getmem(stackframe,sizeof(tstackframe));
  zeromemory(stackframe,sizeof(tstackframe));
  stackframe^.AddrPC.Offset:=context.eip;
  stackframe^.AddrPC.mode:=AddrModeFlat;

  stackframe^.AddrStack.Offset:=context.Esp;
  stackframe^.AddrStack.Mode:=addrmodeflat;

  stackframe^.AddrFrame.Offset:=context.Ebp;
  stackframe^.AddrFrame.Mode:=addrmodeflat;

  listview1.items.clear;
  while stackwalk(IMAGE_FILE_MACHINE_I386,processhandle,threadhandle,stackframe,@cxt, nil,SymFunctionTableAccess,SymGetModuleBase,nil) do
  begin
    listview1.Items.Add.Caption:=inttohex(stackframe^.AddrPC.Offset,8);
    listview1.items[listview1.Items.Count-1].SubItems.add(inttohex(stackframe^.AddrStack.Offset,8));
    listview1.items[listview1.Items.Count-1].SubItems.add(inttohex(stackframe^.AddrFrame.Offset,8));
    listview1.items[listview1.Items.Count-1].SubItems.add(inttohex(stackframe^.AddrReturn.Offset,8));

    a:=stackframe^.Params[0];
    b:=stackframe^.Params[1];
    c:=stackframe^.Params[2];
    d:=stackframe^.Params[3];

    if integer(a)>$400000 then sa:='0x'+inttohex(a,8) else sa:=inttostr(integer(a));
    if integer(b)>$400000 then sb:='0x'+inttohex(b,8) else sb:=inttostr(integer(b));
    if integer(c)>$400000 then sc:='0x'+inttohex(c,8) else sc:=inttostr(integer(c));
    if integer(d)>$400000 then sd:='0x'+inttohex(d,8) else sd:=inttostr(integer(d));

    listview1.items[listview1.Items.Count-1].SubItems.add(sa+','+sb+','+sc+','+sd+',...');
  end;

end;

procedure TfrmStacktrace.FormCreate(Sender: TObject);
begin
  stacktrace(debuggerthread.pausedthreadhandle,debuggerthread.context);
end;

procedure TfrmStacktrace.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  frmstacktrace:=nil;
  action:=cafree;
end;

end.
