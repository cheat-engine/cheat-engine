unit frmExeTrainerGeneratorUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TfrmExeTrainerGenerator }

  TfrmExeTrainerGenerator = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Image1: TImage;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure FormActivate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmExeTrainerGenerator: TfrmExeTrainerGenerator;

implementation

{ TfrmExeTrainerGenerator }

procedure TfrmExeTrainerGenerator.FormActivate(Sender: TObject);
begin

end;

initialization
  {$I frmExeTrainerGeneratorUnit.lrs}

end.

