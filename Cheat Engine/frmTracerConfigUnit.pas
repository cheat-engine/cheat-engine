unit frmTracerConfigUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TfrmTracerConfig }

  TfrmTracerConfig = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    cbDereferenceAddresses: TCheckBox;
    cbSaveStack: TCheckBox;
    cbStepOver: TCheckBox;
    cbSkipSystemModules: TCheckBox;
    edtMaxTrace: TEdit;
    edtCondition: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    rbBreakOnAccess: TRadioButton;
    rbBreakOnWrite: TRadioButton;
  private
    { private declarations }
    fDataTrace: boolean;
    procedure setDataTrace(state: boolean);
  public
    { public declarations }
    property DataTrace: boolean read fDataTrace write setDataTrace;
  end; 

var frmTracerConfig:TfrmTracerConfig;

implementation

{ TfrmTracerConfig }

procedure TfrmTracerConfig.setDataTrace(state: boolean);
begin
  rbBreakOnAccess.visible:=state;
  rbBreakOnWrite.visible:=state;

  if state then
    btnOk.top:=rbBreakOnAccess.top+rbBreakOnAccess.Height+5
  else
    btnOk.top:=cbSkipSystemModules.top+cbSkipSystemModules.Height+5;

  btnCancel.top:=btnOk.top;
  ClientHeight:=btnOK.top+btnOK.Height+3;
end;



initialization
  {$I frmTracerConfigUnit.lrs}

end.

