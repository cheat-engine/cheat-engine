unit frmBreakThreadUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,debugger;

type
  Tfrmbreakthread = class(TForm)
    Label1: TLabel;
    Threadlistbox: TListBox;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    threadhandle: thandle;
  end;

var
  frmbreakthread: Tfrmbreakthread;

implementation

{$R *.dfm}

procedure Tfrmbreakthread.Button1Click(Sender: TObject);
begin
  if threadlistbox.ItemIndex<>-1 then
  begin
    threadhandle:=debuggerthread.threadlist[threadlistbox.ItemIndex,1];
    modalresult:=mrok;
  end
  else raise exception.Create('Please select a thread before clicking on OK');
end;

procedure Tfrmbreakthread.FormCreate(Sender: TObject);
var i: integer;
begin
  threadlistbox.clear;
  //fill the threadlistbox
  with debuggerthread do
  begin
    for i:=0 to length(threadlist)-1 do
      threadlistbox.Items.Add(inttohex(threadlist[i,0],8));
  end;
end;

end.
