unit formTracer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,debugger,newkernelhandler;

type
  TfrmTracer = class(TForm)
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTracer: TfrmTracer;

implementation

{$R *.dfm}

procedure TfrmTracer.FormCreate(Sender: TObject);
var tcount: integer;
    tcounts: string;
begin
  //set a breakpoint and when that breakpoint gets hit trace a number of instructions
  tcounts:=1000;

  if inputquery('Cheat Engine Trace','Trace count',tcounts) then
  begin
    try
      tcount:=strtoint(trcounts);
    except
      raise exception.Create('That isn''t a valid count');
    end;
    
    if startdebuggerifneeded then
    begin
      debugger.traceaddress:=dselected;
      debugger.tracecount:=tcount;

      togglebreakpoint(dselected);
      updatedisassemblerview;
    end;
  end;
end;

end.
