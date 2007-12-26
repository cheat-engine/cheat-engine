unit frmCreatedProcessListUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,cefuncproc;

type
  TfrmCreatedProcessList = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCreatedProcessList: TfrmCreatedProcessList;

implementation

{$R *.dfm}

uses debugger,mainunit;

procedure TfrmCreatedProcessList.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
  frmCreatedProcesslist:=nil;
end;

procedure TfrmCreatedProcessList.Button1Click(Sender: TObject);
var i,j: integer;
begin
  i:=listbox1.ItemIndex;

  if i>=0 then
  begin
    processid:=StrToInt('$'+listbox1.Items[i]);
    try
      for j:=0 to length(debuggerthread.Newprocesses)-1 do
      begin
        if debuggerthread.Newprocesses[j].processid=processid then
        begin
          processhandle:=debuggerthread.Newprocesses[j].processhandle;
          break;
        end;
      end;

      listbox1.Repaint;
      mainform.ProcessLabel.caption:=listbox1.Items[i];
    except

    end;
  end;


end;

procedure TfrmCreatedProcessList.ListBox1DrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Bitmap: TBitmap;      { temporary variable for the item’s bitmap }
  Offset: Integer;      { text offset width }
  origcolor: tcolor;
begin
  origcolor:=clBlack;

  with listbox1.Canvas do  { draw on control canvas, not on the form }
  begin
    FillRect(Rect);       { clear the rectangle }
    Offset := 2;          { provide default offset }
    Bitmap := TBitmap(listbox1.Items.Objects[Index]); { get the bitmap }
    if Bitmap <> nil then
    begin
      Draw(Rect.Left + Offset, Rect.Top, Bitmap); {render bitmap}
      Offset := Bitmap.width + 6;    { add four pixels between bitmap and text}
    end;

    if processid=StrToInt('$'+listbox1.Items[index]) then
      listbox1.Canvas.Font.Style:=[fsBold]
    else
      listbox1.Canvas.Font.Style:=[];

    TextOut(Rect.Left + Offset, Rect.Top, listbox1.Items[Index]);  { display the text }
  end;
end;

procedure TfrmCreatedProcessList.FormShow(Sender: TObject);
begin
  left:=mainform.Left+mainform.Width;
  top:=mainform.Top;
end;

end.
