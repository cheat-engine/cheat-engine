program Trainerwithassembler;

uses
  Forms,
  PatcherUnit in 'PatcherUnit.pas' {frmPatcher},
  settingsunit in 'settingsunit.pas',
  frmMainUnit in 'frmMainUnit.pas' {frmMain},
  MemoryTrainerUnit in 'MemoryTrainerUnit.pas' {frmMemoryTrainer},
  ExtraTrainerComponents in '..\ExtraTrainerComponents.pas',
  Userdefinedformunit in 'Userdefinedformunit.pas' {Userdefinedform},
  HotkeyHandler in '..\HotkeyHandler.pas',
  CEFuncProc in '..\CEFuncProc.pas',
  reinit in '..\reinit.pas',
  Assemblerunit in '..\Assemblerunit.pas',
  NewKernelHandler in '..\NewKernelHandler.pas',
  Filehandler in '..\Filehandler.pas',
  symbolhandler in '..\symbolhandler.pas',
  frmautoinjectunit in '..\frmautoinjectunit.pas' {frmAutoInject},
  autoassembler in '..\autoassembler.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.ShowMainForm:=false;

  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
