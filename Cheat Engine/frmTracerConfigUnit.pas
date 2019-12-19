unit frmTracerConfigUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, debuggertypedefinitions;

type

  { TfrmTracerConfig }

  TfrmTracerConfig = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    cbDereferenceAddresses: TCheckBox;
    cbSaveStack: TCheckBox;
    cbStepOver: TCheckBox;
    cbSkipSystemModules: TCheckBox;
    edtStartCondition: TEdit;
    edtMaxTrace: TEdit;
    edtStopCondition: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    rbBPHardware: TRadioButton;
    rbBPSoftware: TRadioButton;
    rbBPException: TRadioButton;
    rbBPDBVM: TRadioButton;
    rbBreakOnAccess: TRadioButton;
    rbBreakOnWrite: TRadioButton;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    fDataTrace: boolean;
    procedure setDataTrace(state: boolean);
    function getBreakpointmethod: TBreakpointmethod;
    procedure setBreakPointMethod(m: TBreakpointmethod);
  public
    { public declarations }
    property DataTrace: boolean read fDataTrace write setDataTrace;
    property breakpointmethod: TBreakpointMethod read getBreakpointMethod write setBreakpointMethod;
  end; 

var frmTracerConfig:TfrmTracerConfig;

implementation

{ TfrmTracerConfig }

uses NewKernelHandler, DebugHelper, debuggerinterface, DebuggerInterfaceAPIWrapper,
  formsettingsunit;

function TfrmTracerConfig.getBreakpointmethod: TBreakpointmethod;
begin
  result:=bpmDebugRegister;

  if rbBPHardware.checked then
    result:=bpmDebugRegister
  else
  if rbBPSoftware.checked then
  begin
    if datatrace=false then
      result:=bpmInt3
    else
      result:=bpmDebugRegister;
  end
  else
  if rbBPException.checked then
    result:=bpmException
  else
  if rbBPDBVM.checked then
    result:=bpmDBVM;
end;

procedure TfrmTracerConfig.setBreakPointMethod(m: TBreakpointmethod);
begin
  case m of
    bpmDebugRegister: rbBPHardware.checked:=true;
    bpmInt3: rbBPSoftware.checked:=true;
    bpmException: rbBPException.checked:=true;
    bpmDBVM: rbBPDBVM.checked:=true;
  end;
end;



procedure TfrmTracerConfig.FormCreate(Sender: TObject);
begin
  if hasEPTSupport=false then
    rbBPDBVM.visible:=false;

  rbBPHardware.enabled:=true;
  rbBPSoftware.enabled:=((CurrentDebuggerInterface<>nil) and (dbcSoftwareBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities)) or (formsettings.cbKDebug.Checked=false);
  rbBPException.enabled:=((CurrentDebuggerInterface<>nil) and (dbcExceptionBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities)) or (formsettings.cbKDebug.Checked=false);
  rbBPDBVM.enabled:=((CurrentDebuggerInterface<>nil) and (dbcDBVMBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities)) or (formsettings.cbKDebug.Checked);
end;

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

  rbBPSoftware.Enabled:=(not state) and ((CurrentDebuggerInterface=nil) or (dbcSoftwareBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities));

  if state and rbBPSoftware.checked then
    rbBPHardware.checked:=true;

  fDataTrace:=state;
end;



initialization
  {$I frmTracerConfigUnit.lrs}

end.

