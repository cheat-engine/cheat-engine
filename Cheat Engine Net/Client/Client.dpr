program Client;

uses
  Forms,
  CEClient in 'CEClient.pas' {ConnectForm},
  Unit2 in 'Unit2.pas' {MainForm},
  processwindow in 'processwindow.pas' {ProcesWindow},
  Unit3 in 'Unit3.pas' {Waitform},
  changetimerunit in 'changetimerunit.pas' {UpdateTimerForm},
  addformunit in 'addformunit.pas' {Addform},
  commentsunit in 'commentsunit.pas' {Comments},
  Standaloneunit in 'Standaloneunit.pas' {Standalone},
  Changeoffsetunit in '..\..\Changeoffsetunit.pas' {ChangeOffset},
  Exampletrainerstyle3Unit in '..\..\Exampletrainerstyle3Unit.pas' {ExampleTrainerStyle3},
  exampletrainerstyle1unit in '..\..\exampletrainerstyle1unit.pas' {Exampletrainerstyle1},
  exampletrainerstyle2unit in '..\..\exampletrainerstyle2unit.pas' {Exampletrainerstyle2},
  formsettingsunit in '..\..\formsettingsunit.pas' {formSettings},
  addressparser in '..\..\addressparser.pas',
  formScanningUnit in 'formScanningUnit.pas' {FormScanning},
  OpenSave in '..\..\OpenSave.pas',
  Valuechange in '..\..\Valuechange.pas' {ValueChangeForm},
  TypePopup in '..\..\TypePopup.pas' {TypeForm},
  aboutunit in '..\..\aboutunit.pas' {About},
  tlgUnit in '..\..\tlgUnit.pas' {tlg},
  reinit in '..\..\reinit.pas',
  NetApis in 'NetApis.pas',
  MemoryBrowserFormUnit in '..\..\MemoryBrowserFormUnit.pas' {MemoryBrowser},
  Assemblerunit in '..\..\Assemblerunit.pas',
  disassembler in '..\..\disassembler.pas',
  frmLoadMemoryunit in '..\..\frmLoadMemoryunit.pas' {frmLoadMemory},
  frmSaveMemoryRegionUnit in '..\..\frmSaveMemoryRegionUnit.pas' {frmSaveMemoryRegion},
  inputboxtopunit in '..\..\inputboxtopunit.pas' {InputboxTop},
  frmFillMemoryUnit in '..\..\frmFillMemoryUnit.pas' {frmFillMemory},
  FoundCodeUnit in '..\..\FoundCodeUnit.pas' {FoundCodeDialog},
  debugger in 'debugger.pas',
  AdvancedOptionsUnit in '..\..\AdvancedOptionsUnit.pas' {AdvancedOptions},
  formFoundcodeListExtraUnit in '..\..\formFoundcodeListExtraUnit.pas' {FormFoundCodeListExtra},
  frmautoinjectunit in '..\..\frmautoinjectunit.pas' {frmAutoInject},
  formAddToCodeList in '..\..\formAddToCodeList.pas' {frmAddToCodeList},
  frmFindCodeInFileUnit in '..\..\frmFindCodeInFileUnit.pas' {formFindCodeInFile},
  MainUnit2 in '..\..\MainUnit2.pas',
  frmCodecaveScannerUnit in '..\..\frmCodecaveScannerUnit.pas' {frmCodecaveScanner},
  CEFuncProc in '..\..\CEFuncProc.pas',
  symbolhandler in 'symbolhandler.pas',
  APIhooktemplatesettingsfrm in '..\..\APIhooktemplatesettingsfrm.pas' {frmAPIhookTemplateSettings},
  autoassembler in '..\..\autoassembler.pas',
  frmDisassemblyscanunit in '..\..\frmDisassemblyscanunit.pas' {frmDisassemblyscan};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Cheat Engine 5.2 Client';
  Application.CreateForm(TConnectForm, ConnectForm);
  Application.CreateForm(TUpdateTimerForm, UpdateTimerForm);
  Application.CreateForm(TComments, Comments);
  Application.CreateForm(TStandalone, Standalone);
  Application.CreateForm(TChangeOffset, ChangeOffset);
  Application.CreateForm(TExampleTrainerStyle3, ExampleTrainerStyle3);
  Application.CreateForm(TExampletrainerstyle1, Exampletrainerstyle1);
  Application.CreateForm(TExampletrainerstyle2, Exampletrainerstyle2);
  Application.CreateForm(TformSettings, formSettings);
  Application.CreateForm(TValueChangeForm, ValueChangeForm);
  Application.CreateForm(TTypeForm, TypeForm);
  Application.CreateForm(TAbout, About);
  Application.CreateForm(Ttlg, tlg);
  Application.CreateForm(TfrmSaveMemoryRegion, frmSaveMemoryRegion);
  Application.CreateForm(TInputboxTop, InputboxTop);
  Application.CreateForm(TfrmFillMemory, frmFillMemory);
  Application.CreateForm(TfrmLoadMemory, frmLoadMemory);
  Application.CreateForm(TAdvancedOptions, AdvancedOptions);
  Application.CreateForm(TFormFoundCodeListExtra, FormFoundCodeListExtra);
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TMemoryBrowser, MemoryBrowser);
  Application.CreateForm(TfrmCodecaveScanner, frmCodecaveScanner);
  Application.Run;
end.
