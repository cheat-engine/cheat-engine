{$ifndef ceasinjectabledll}
program CheatEngine;
{$else}
{$R 'trainer.res' 'trainer.rc'}

library CheatEngine;
{$endif}

{$R 'trainer.res' 'trainer.rc'}
{$R manifest.res} 

uses
  Forms,
  windows,
  registry,
  sysutils,
  MainUnit in 'MainUnit.pas' {MainForm},
  ProcessWindowUnit in 'ProcessWindowUnit.pas' {ProcessWindow},
  CEFuncProc in 'CEFuncProc.pas',
  MemoryBrowserFormUnit in 'MemoryBrowserFormUnit.pas' {MemoryBrowser},
  TypePopup in 'TypePopup.pas' {TypeForm},
  Valuechange in 'Valuechange.pas' {ValueChangeForm},
  AddAddress in 'AddAddress.pas' {AddForm},
  HotKeys in 'HotKeys.pas' {HotKeyForm},
  standaloneunit in 'standaloneunit.pas' {StandAlone},
  aboutunit in 'aboutunit.pas' {About},
  CommentsUnit in 'CommentsUnit.pas' {Comments},
  formsettingsunit in 'formsettingsunit.pas' {formSettings},
  disassembler in 'disassembler.pas',
  Changeoffsetunit in 'Changeoffsetunit.pas' {ChangeOffset},
  Debugger in 'Debugger.pas',
  FoundCodeUnit in 'FoundCodeUnit.pas' {FoundCodeDialog},
  addressparser in 'addressparser.pas',
  AdvancedOptionsUnit in 'AdvancedOptionsUnit.pas' {AdvancedOptions},
  inputboxtopunit in 'inputboxtopunit.pas' {InputboxTop},
  FindWindowUnit in 'FindWindowUnit.pas' {FindWindow},
  formChangedAddresses in 'formChangedAddresses.pas' {frmChangedAddresses},
  frmFindCodeInFileUnit in 'frmFindCodeInFileUnit.pas' {formFindCodeInFile},
  formAddToCodeList in 'formAddToCodeList.pas' {frmAddToCodeList},
  formPatcherMaker in 'formPatcherMaker.pas' {frmPatcherMaker},
  formPatcherMaker2 in 'formPatcherMaker2.pas' {frmPatcherMaker2},
  formPatcherMaker3 in 'formPatcherMaker3.pas' {frmPatcherMaker3},
  formPatchEdit in 'formPatchEdit.pas' {frmPatchEdit},
  formMemoryModifier in 'formMemoryModifier.pas' {frmMemoryModifier},
  formMemoryTrainerUnit in 'formMemoryTrainerUnit.pas' {frmMemoryTrainerPreview},
  formMemoryTrainerAddEntry in 'formMemoryTrainerAddEntry.pas' {FrmMemoryTrainerAddEntry},
  FrmMemoryTrainerAddEntry2Unit in 'FrmMemoryTrainerAddEntry2Unit.pas' {FrmMemoryTrainerAddEntry2},
  formScanningUnit in 'formScanningUnit.pas' {formScanning},
  formhotkeyunit in 'formhotkeyunit.pas' {FormHotkey},
  formDifferentBitSizeUnit in 'formDifferentBitSizeUnit.pas' {formDifferentBitSize},
  Assemblerunit in 'Assemblerunit.pas',
  OpenSave in 'OpenSave.pas',
  Handles in 'HANDLES.PAS',
  MemoryTrainerDesignUnit in 'MemoryTrainerDesignUnit.pas' {frmTrainerDesigner},
  MemoryTrainerDesignControlsUnit in 'MemoryTrainerDesignControlsUnit.pas' {frmTrainerDesignControls},
  MemoryTrainerDesignObjectViewUnit in 'MemoryTrainerDesignObjectViewUnit.pas' {frmTrainerDesignObjectView},
  ExtraTrainerComponents in 'ExtraTrainerComponents.pas',
  frmstacktraceunit in 'frmstacktraceunit.pas' {frmStacktrace},
  frmThreadlistunit in 'frmThreadlistunit.pas' {frmThreadlist},
  frmBreakThreadUnit in 'frmBreakThreadUnit.pas' {frmbreakthread},
  frmSaveMemoryRegionUnit in 'frmSaveMemoryRegionUnit.pas' {frmSaveMemoryRegion},
  frmLoadMemoryunit in 'frmLoadMemoryunit.pas' {frmLoadMemory},
  FormDebugStringsUnit in 'FormDebugStringsUnit.pas' {FormDebugStrings},
  formFoundcodeListExtraUnit in 'formFoundcodeListExtraUnit.pas' {FormFoundCodeListExtra},
  formAddressChangeUnit in 'formAddressChangeUnit.pas' {formAddressChange},
  formmemoryregionsunit in 'formmemoryregionsunit.pas' {FormMemoryRegions},
  formPointerOrPointeeUnit in 'formPointerOrPointeeUnit.pas' {formPointerOrPointee},
  frmExcludeHideUnit in 'frmExcludeHideUnit.pas' {frmExcludeHide},
  frmFillMemoryUnit in 'frmFillMemoryUnit.pas' {frmFillMemory},
  tlgUnit in 'tlgUnit.pas' {tlg},
  frmDissectwindowUnit in 'frmDissectwindowUnit.pas' {frmdissectWindow},
  frmCapturedTimersUnit in 'frmCapturedTimersUnit.pas' {frmCapturedTimers},
  frmHeapsUnit in 'frmHeapsUnit.pas' {frmHeaps},
  frmEnumerateDLLsUnit in 'frmEnumerateDLLsUnit.pas' {frmEnumerateDLLs},
  frmDirectXUnit in 'frmDirectXUnit.pas' {frmDirectX},
  frmCreatedProcessListUnit in 'frmCreatedProcessListUnit.pas' {frmCreatedProcessList},
  NewKernelHandler in 'NewKernelHandler.pas',
  HotkeyHandler in 'hotkeyhandler.pas',
  frmautoinjectunit in 'frmautoinjectunit.pas' {frmAutoInject},
  frmFindstaticsUnit in 'frmFindstaticsUnit.pas' {frmFindStatics},
  UndoChanges in 'UndoChanges.pas',
  MainUnit2 in 'MainUnit2.pas',
  frmCodecaveScannerUnit in 'frmCodecaveScannerUnit.pas' {frmCodecaveScanner},
  KernelDebugger in 'KernelDebugger.pas',
  Filehandler in 'Filehandler.pas',
  frmModifyRegistersUnit in 'frmModifyRegistersUnit.pas' {frmModifyRegisters},
  frmBreakpointlistunit in 'frmBreakpointlistunit.pas' {frmBreakpointlist},
  frmProcessWatcherUnit in 'frmProcessWatcherUnit.pas' {frmProcessWatcher},
  ModuleSafetyUnit in 'ModuleSafetyUnit.pas' {frmModuleSafety},
  ChangeValuefrm in 'ChangeValuefrm.pas' {frmChangeValue},
  formProcessInfo in 'formProcessInfo.pas' {frmProcessInfo},
  DissectCodeThread in 'DissectCodeThread.pas',
  DissectCodeunit in 'DissectCodeunit.pas' {frmDissectCode},
  unrandomizer in 'unrandomizer.pas',
  frmProcesswatcherExtraUnit in 'frmProcesswatcherExtraUnit.pas' {frmProcessWatcherExtra},
  pointerscannerfrm in 'pointerscannerfrm.pas' {frmPointerScanner},
  SyncObjs2 in 'SyncObjs2.pas',
  framememorybrowser2unit in 'framememorybrowser2unit.pas' {frameMemoryBrowser2: TFrame},
  APIhooktemplatesettingsfrm in 'APIhooktemplatesettingsfrm.pas' {frmAPIhookTemplateSettings},
  Structuresfrm in 'Structuresfrm.pas' {frmStructures},
  StructuresAddElementfrm in 'StructuresAddElementfrm.pas' {frmStructuresAddElement},
  symbolhandler in 'symbolhandler.pas',
  symbolconfigunit in 'symbolconfigunit.pas' {frmSymbolhandler},
  hypermode in 'hypermode.pas',
  savedisassemblyfrm in 'savedisassemblyfrm.pas' {frmSavedisassembly},
  ThreadlistExFRM in 'ThreadlistExFRM.pas' {frmThreadlistEx},
  autoassembler in 'autoassembler.pas',
  PasteTableentryFRM in 'PasteTableentryFRM.pas' {frmPasteTableentry},
  InjectedpointerscanornotFRM in 'InjectedpointerscanornotFRM.pas' {frmInjectedpointerscanornot},
  VirtualMemory in 'VirtualMemory.pas',
  injectedpointerscanunit in 'injectedpointerscanunit.pas',
  PointerscannerSettingsFrm in 'PointerscannerSettingsFrm.pas' {frmPointerScannerSettings},
  frmDisassemblyscanunit in 'frmDisassemblyscanunit.pas' {frmDisassemblyscan},
  DriverList in 'DriverList.pas' {frmDriverlist},
  plugin in 'plugin.pas',
  pluginexports in 'pluginexports.pas',
  ServiceDescriptorTables in 'ServiceDescriptorTables.pas' {frmServiceDescriptorTables},
  SaveFirstScan in 'SaveFirstScan.pas',
  firstscanhandler in 'firstscanhandler.pas',
  foundlisthelper in 'foundlisthelper.pas',
  frmTracerUnit in 'frmTracerUnit.pas' {frmTracer},
  frmGDTunit in 'frmGDTunit.pas' {frmGDTinfo},
  frmIDTunit in 'frmIDTunit.pas' {frmIDT},
  PEInfounit in 'PEInfounit.pas' {frmPEInfo},
  underc in 'underc.pas',
  RichEditHighlight in 'RichEditHighlight.pas',
  psvAutoAssembler in 'psvAutoAssembler.pas',
  psvCPlusPlus in 'psvCPlusPlus.pas',
  psvRichSyntax in 'psvRichSyntax.pas',
  memscan in 'memscan.pas',
  PEInfoFunctions in 'PEInfoFunctions.pas',
  frmFunctionlistUnit in 'frmFunctionlistUnit.pas' {frmFunctionList},
  FileMapping in 'FileMapping.pas',
  FormsExtra in 'FormsExtra.pas',
  speedhack2 in 'speedhack2.pas',
  frmAAEditPrefsUnit in 'frmAAEditPrefsUnit.pas' {frmAAEditPrefs},
  MenuItemExtra in 'MenuItemExtra.pas',
  ProcessHandlerUnit in 'ProcessHandlerUnit.pas',
  AccessCheck in 'AccessCheck.pas',
  frameHotkeyConfigUnit in 'frameHotkeyConfigUnit.pas' {frameHotkeyConfig: TFrame},
  htmlHelp in 'htmlHelp.pas',
  SynHighlighterAA in 'SynEdit\Source\SynHighlighterAA.pas',
  SynHighlighterCpp in 'SynEdit\Source\SynHighlighterCPP.pas',
  frmFloatingPointPanelUnit in 'frmFloatingPointPanelUnit.pas' {frmFloatingPointPanel},
  frmRegistersunit in 'frmRegistersunit.pas' {Registers},
  dbvmPhysicalMemoryHandler in 'dbvmPhysicalMemoryHandler.pas',
  frmCScriptUnit in 'frmCScriptUnit.pas' {frmCScript},
  stealthedit in 'stealthedit.pas',
  disassemblerviewunit in 'disassemblerviewunit.pas',
  disassemblerviewlinesunit in 'disassemblerviewlinesunit.pas',
  frmMemoryAllocHandlerUnit in 'frmMemoryAllocHandlerUnit.pas' {frmMemoryAllocHandler},
  stacktrace2 in 'stacktrace2.pas',
  byteinterpreter in 'byteinterpreter.pas',
  simpleaobscanner in 'simpleaobscanner.pas',
  circularBuffer in 'circularBuffer.pas',
  frmRescanPointerUnit in 'frmRescanPointerUnit.pas' {frmRescanPointer},
  pointervaluelist in 'pointervaluelist.pas',
  rescanhelper in 'rescanhelper.pas',
  ValueFinder in 'ValueFinder.pas',
  PointerscanresultReader in 'PointerscanresultReader.pas',
  bigmemallochandler in 'bigmemallochandler.pas';

//  frmOpenGLUnit in 'frmOpenGLUnit.pas' {frmOpenGL};

{$R *.RES}

begin
  Application.Initialize;
  getcedir;
  setlanguage;
  Application.Title := 'Cheat Engine 5.6';
  Application.HelpFile := 'CheatEngine.chm';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TfrmPasteTableentry, frmPasteTableentry);
  Application.CreateForm(TformSettings, formSettings);
  Application.CreateForm(TMemoryBrowser, MemoryBrowser);
  Application.CreateForm(TTypeForm, TypeForm);
  Application.CreateForm(TValueChangeForm, ValueChangeForm);
  Application.CreateForm(THotKeyForm, HotKeyForm);
  Application.CreateForm(TStandAlone, StandAlone);
  Application.CreateForm(TComments, Comments);
  Application.CreateForm(TAdvancedOptions, AdvancedOptions);
  Application.CreateForm(TFormDebugStrings, FormDebugStrings);
  Application.CreateForm(TfrmdissectWindow, frmdissectWindow);
  Application.CreateForm(TfrmChangeValue, frmChangeValue);
  Application.CreateForm(TfrmCapturedTimers, frmCapturedTimers);
  Application.CreateForm(TfrmCScript, frmCScript);
  Application.CreateForm(TfrmFloatingPointPanel, frmFloatingPointPanel);
  initcetitle;
  Application.Run;
end.

