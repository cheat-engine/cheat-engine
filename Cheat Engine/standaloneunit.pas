unit standaloneunit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, CEFuncProc,tlhelp32, Menus, Buttons, ExtCtrls,
  ExtDlgs,ShellApi;

type
  TStandAlone = class(TForm)
    Button1: TButton;
    Button2: TButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    GroupBox1: TGroupBox;
    RadioButton2: TRadioButton;
    RadioButton1: TRadioButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure RadioButton4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    filename: string;
  end;

var
  StandAlone: TStandAlone;

implementation

uses MainUnit, standaloneexample,
   formPatcherMaker,
  formMemoryModifier,opensave;

{$R *.DFM}

procedure TStandAlone.Button1Click(Sender: TObject);
var i: integer;
begin
  if radiobutton3.checked then
  begin
    if opendialog1.Execute then
    begin
      LoadExe(opendialog1.filename);
      close;
    end;
    exit;
  end;

  if radiobutton1.checked then
  begin
    hide;
    frmPatcherMaker:=TfrmPatcherMaker.create(self);
    frmPatcherMaker.showmodal;
  end
  else
  begin
    //memory changer
    hide;
    frmMemoryModifier:=TFrmMemoryModifier.create(self);
    frmMemoryModifier.showmodal;
  end;

  modalresult:=mrok;
end;

procedure TStandAlone.RadioButton4Click(Sender: TObject);
begin
  groupbox1.Enabled:=radiobutton4.Checked;
  radiobutton2.Enabled:=radiobutton4.checked;
  radiobutton1.Enabled:=radiobutton4.checked;
end;

end.
