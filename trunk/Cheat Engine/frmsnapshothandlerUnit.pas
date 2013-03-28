unit frmsnapshothandlerUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus;

type

  { TfrmSnapshotHandler }

  TfrmSnapshotHandler = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem8: TMenuItem;
    OpenDialog1: TOpenDialog;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ScrollBar1: TScrollBar;
    Splitter1: TSplitter;
    procedure MenuItem4Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure loadsnapshots(list: TStrings);
    procedure initialize(path: string; count: integer);
  end;

var
  frmSnapshotHandler: TfrmSnapshotHandler;

implementation

{$R *.lfm}

procedure TfrmSnapshotHandler.loadsnapshots(list: TStrings);
begin

end;

procedure TfrmSnapshotHandler.ScrollBar1Change(Sender: TObject);
begin

end;

procedure TfrmSnapshotHandler.MenuItem4Click(Sender: TObject);
begin

end;

procedure TfrmSnapshotHandler.initialize(path: string; count: integer);
begin

end;

end.

