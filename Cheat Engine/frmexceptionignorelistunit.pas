unit frmExceptionIgnoreListUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, betterControls;

type

  { TfrmExceptionIgnoreList }

  TfrmExceptionIgnoreList = class(TForm)
    btnAdd: TButton;
    edtExceptionCode: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lbExceptionCodeList: TListBox;
    miDelete: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    procedure btnAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbExceptionCodeListDblClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    { private declarations }
    loadedPosition: boolean;
  public
    { public declarations }
    procedure updateList;
  end;

var
  frmExceptionIgnoreList: TfrmExceptionIgnoreList;

implementation

{$R *.lfm}

uses UnexpectedExceptionsHelper, CEFuncProc, math;

{ TfrmExceptionIgnoreList }

procedure TfrmExceptionIgnoreList.FormCreate(Sender: TObject);
begin
  loadedPosition:=LoadFormPosition(self);
end;

procedure TfrmExceptionIgnoreList.btnAddClick(Sender: TObject);
var code: dword;
begin
  try
    code:=strtoint(edtExceptionCode.text);
  except
    code:=strtoint('$'+edtExceptionCode.text);
  end;
  AddIgnoredExceptionCode(code);
end;

procedure TfrmExceptionIgnoreList.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(self);
end;

procedure TfrmExceptionIgnoreList.FormShow(Sender: TObject);
begin
  updateList;

  edtExceptionCode.Width:=btnAdd.Width+2;

  if not loadedPosition then
  begin
    clientwidth:=max(clientwidth, canvas.TextWidth(label1.Caption)*2);
    clientheight:=max(clientheight, canvas.TextHeight('jf')*20);

  end;
end;

procedure TfrmExceptionIgnoreList.lbExceptionCodeListDblClick(Sender: TObject);
var
  i: integer;
  s: string;
  c: dword;
begin
  if lbExceptionCodeList.itemindex<>-1 then
  begin
    s:=lbExceptionCodeList.items[lbExceptionCodeList.itemindex];
    s:=copy(s,1,8);
    c:=strtoint('$'+s);
    RemoveIgnoredExceptionCode(c);
  end;
end;

procedure TfrmExceptionIgnoreList.PopupMenu1Popup(Sender: TObject);
begin
  midelete.visible:=lbExceptionCodeList.itemindex<>-1;
end;

procedure TfrmExceptionIgnoreList.updateList;
begin
  getIgnoredExceptionCodeList(lbExceptionCodeList.Items);
end;

end.

