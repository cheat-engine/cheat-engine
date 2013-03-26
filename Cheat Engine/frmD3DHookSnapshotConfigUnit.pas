unit frmD3DHookSnapshotConfigUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls;

type

  { TfrmD3DHookSnapshotConfig }

  TfrmD3DHookSnapshotConfig = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    DirectoryEdit1: TDirectoryEdit;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmD3DHookSnapshotConfig: TfrmD3DHookSnapshotConfig;

implementation

{$R *.lfm}

end.

