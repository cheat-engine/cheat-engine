unit frmTracerConfigUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, debuggertypedefinitions, betterControls;

type

  { TfrmTracerConfig }

  TfrmTracerConfig = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    cbDereferenceAddresses: TCheckBox;
    cbSaveStack: TCheckBox;
    cbStepOver: TCheckBox;
    cbSkipSystemModules: TCheckBox;
    cbDBVMBreakAndTrace: TCheckBox;
    cbDBVMTriggerCOW: TCheckBox;
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
    procedure cbDBVMBreakAndTraceChange(Sender: TObject);
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
  formsettingsunit, DBVMDebuggerInterface;

function TfrmTracerConfig.getBreakpointmethod: TBreakpointmethod;
begin
  if (CurrentDebuggerInterface<>nil) and (CurrentDebuggerInterface is TDBVMDebugInterface) then
    exit(bpmDBVMNative);


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

  rbBPHardware.enabled:=((CurrentDebuggerInterface<>nil) and CurrentDebuggerInterface.usesDebugRegisters) or (formsettings.cbUseDBVMDebugger.checked=false);
  rbBPSoftware.enabled:=((CurrentDebuggerInterface<>nil) and (dbcSoftwareBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities)) and (formsettings.cbKDebug.Checked=false);
  rbBPException.enabled:=((CurrentDebuggerInterface<>nil) and (dbcExceptionBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities)) and (formsettings.cbKDebug.Checked=false);
  rbBPDBVM.enabled:=((CurrentDebuggerInterface<>nil) and (dbcDBVMBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities)) or (formsettings.cbKDebug.Checked);

  cbDBVMBreakAndTrace.visible:=isDBVMCapable;
  cbDBVMBreakAndTrace.enabled:=isDBVMCapable;

  if (CurrentDebuggerInterface=nil) and isRunningDBVM then //no debugger running, go for dbvm by default
    cbDBVMBreakAndTrace.Checked:=true;

  if (CurrentDebuggerInterface<>nil) and (CurrentDebuggerInterface is TDBVMDebugInterface) or (formSettings.cbUseDBVMDebugger.checked) then
    groupbox1.visible:=false;
end;

procedure TfrmTracerConfig.cbDBVMBreakAndTraceChange(Sender: TObject);
begin

  if cbDBVMBreakAndTrace.Checked then
  begin
    cbDBVMTriggerCOW.visible:=true;
    cbDereferenceAddresses.enabled:=false;
    cbStepOver.enabled:=false;
    cbSkipSystemModules.enabled:=false;
    groupbox1.enabled:=false;
    rbBPHardware.enabled:=false;


    rbBPSoftware.enabled:=false;
    rbBPException.enabled:=false;
    rbBPDBVM.enabled:=false;
    Label3.enabled:=false;
    edtStartCondition.enabled:=false;

    Label2.enabled:=false;
    edtStopCondition.enabled:=false;
  end
  else
  begin
    cbDBVMTriggerCOW.visible:=formSettings.cbUseDBVMDebugger.checked;
    cbDereferenceAddresses.enabled:=true;
    cbStepOver.enabled:=true;
    cbSkipSystemModules.enabled:=true;
    groupbox1.enabled:=true;
    rbBPHardware.enabled:=true;

    rbBPSoftware.enabled:=((CurrentDebuggerInterface<>nil) and (dbcSoftwareBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities)) or (formsettings.cbKDebug.Checked=false);
    rbBPException.enabled:=((CurrentDebuggerInterface<>nil) and (dbcExceptionBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities)) or (formsettings.cbKDebug.Checked=false);
    rbBPDBVM.enabled:=((CurrentDebuggerInterface<>nil) and (dbcDBVMBreakpoint in CurrentDebuggerInterface.DebuggerCapabilities)) or (formsettings.cbKDebug.Checked);
    label3.enabled:=true;
    edtStartCondition.enabled:=true;

    label2.enabled:=false;
    edtStopCondition.enabled:=true;
  end;

end;

procedure TfrmTracerConfig.setDataTrace(state: boolean);
begin
  rbBreakOnAccess.visible:=state;
  rbBreakOnWrite.visible:=state;

  if state then
  begin
    btnOk.top:=rbBreakOnAccess.top+rbBreakOnAccess.Height+5;
    cbDBVMBreakAndTrace.visible:=false;
    cbDBVMBreakAndTrace.Checked:=false;
  end
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

