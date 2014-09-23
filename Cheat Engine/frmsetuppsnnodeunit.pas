unit frmSetupPSNNodeUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmSetupPSNNode }

  TfrmSetupPSNNode = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    cbPriority: TComboBox;
    edtThreadCount: TEdit;
    Edit10: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    lblPublicName: TLabel;
    lblThreadCount: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblPriority: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    procedure Edit8Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmSetupPSNNode: TfrmSetupPSNNode;

implementation

{$R *.lfm}

{ TfrmSetupPSNNode }

procedure TfrmSetupPSNNode.FormCreate(Sender: TObject);
begin

end;

procedure TfrmSetupPSNNode.Edit8Change(Sender: TObject);
begin

end;

end.

