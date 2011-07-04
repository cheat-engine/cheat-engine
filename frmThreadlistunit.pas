unit frmThreadlistunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, StdCtrls,debugger;

type
  TfrmThreadlist = class(TForm)
    PopupMenu1: TPopupMenu;
    Break1: TMenuItem;
    threadlistview: TListView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Break1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure FillThreadlist;
  end;

var
  frmThreadlist: TfrmThreadlist;

implementation

{$R *.dfm}

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
begin
  lastselected:=threadlistview.ItemIndex;

  threadlistview.Clear;
  //fill the threadlistbox
  with debuggerthread do
  begin
    for i:=0 to length(threadlist)-1 do
    begin
      threadlistview.Items.Add.caption:=inttohex(threadlist[i,0],8);
      threadlistview.Items[threadlistview.Items.Count-1].SubItems.add(inttohex(threadlist[i,2],8));
      threadlistview.Items[threadlistview.Items.Count-1].SubItems.add(inttohex(threadlist[i,3],8));
    end;
  end;

  if (lastselected<>-1) and (threadlistview.Items.Count>lastselected) then
    threadlistview.ItemIndex:=lastselected;
end;

procedure TfrmThreadlist.Break1Click(Sender: TObject);
begin
  if threadlistview.ItemIndex<>-1 then
    breakthread(debuggerthread.threadlist[threadlistview.ItemIndex,1]);
end;

end.
