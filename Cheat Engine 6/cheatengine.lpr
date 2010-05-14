program cheatengine;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, bogus, MainUnit, CEDebugger, NewKernelHandler, CEFuncProc,
  ProcessHandlerUnit, symbolhandler, Assemblerunit, hypermode, byteinterpreter,
  addressparser, autoassembler, ProcessWindowUnit, MainUnit2, Filehandler,
  dbvmPhysicalMemoryHandler, frameHotkeyConfigUnit, formsettingsunit,
  HotkeyHandler, formhotkeyunit, AdvancedOptionsUnit, inputboxtopunit, plugin,
  pluginexports, tlgUnit, aboutunit, frmProcesswatcherExtraUnit,
  frmProcessWatcherUnit, ModuleSafetyUnit, frmExcludeHideUnit,
  ConfigUnrandomizerFrm, AddAddress, HotKeys, TypePopup, CommentsUnit,
  FoundCodeUnit, foundlisthelper, unrandomizer, SaveFirstScan, firstscanhandler,
  memscan, formScanningUnit, KernelDebugger, formDifferentBitSizeUnit,
  formAddressChangeUnit, Changeoffsetunit, speedhack2, formPointerOrPointeeUnit,
  AccessCheck, formmemoryregionsunit, OpenSave, formProcessInfo,
  frmautoinjectunit, frmCScriptUnit, underc, MenuItemExtra,
  MemoryBrowserFormUnit, disassemblerviewlinesunit, disassemblerviewunit,
  PasteTableentryFRM, frmBreakpointlistunit, DissectCodeThread, DissectCodeunit,
  Valuechange, FindWindowUnit, stacktrace2, frmstacktraceunit,
  frmBreakThreadUnit, FormDebugStringsUnit, frmDissectwindowUnit,
  frmCapturedTimersUnit, frmEnumerateDLLsUnit, frmThreadlistunit,
  frmMemoryAllocHandlerUnit, circularBuffer, PEInfoFunctions, PEInfounit,
  FileMapping, frmFindstaticsUnit, frmModifyRegistersUnit, frmHeapsUnit,
  savedisassemblyfrm, frmSaveMemoryRegionUnit, frmLoadMemoryunit,
  formAddToCodeList, frmFillMemoryUnit, frmCodecaveScannerUnit,
  frmFunctionlistUnit, symbolconfigunit, frmFloatingPointPanelUnit,
  frmTracerUnit, DriverList, frmRegistersunit, formChangedAddresses, frmGDTunit,
  frmIDTunit, frmDisassemblyscanunit, frmReferencedStringsUnit,
  StructuresAddElementfrm, Structuresfrm, PointerscannerSettingsFrm,
  simpleaobscanner, PointerscanresultReader, pointervaluelist, rescanhelper,
  pointerscannerfrm, VirtualMemory, ValueFinder, frmRescanPointerUnit,
  SyncObjs2, ManualModuleLoader, SynHighlighterAA, APIhooktemplatesettingsfrm,
  frmAAEditPrefsUnit, disassembler, hexviewunit, guisafecriticalsection,
  DebugHelper, debugeventhandler, formFoundcodeListExtraUnit,
  debuggertypedefinitions, sfloat, addresslist, addresslisthandlerunit,
  MemoryRecordUnit, ThreadlistExFRM, windows7taskbar, tablist;

{$R cheatengine.res}
{$R manifest.res}


begin
  Application.Title:='Cheat Engine 6.0';
  Application.Initialize;
  getcedir;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TMemorybrowser, MemoryBrowser);
  Application.CreateForm(TformSettings, formSettings);
  Application.CreateForm(TAdvancedOptions, AdvancedOptions);
  Application.CreateForm(TComments, Comments);
  Application.CreateForm(TTypeForm, TypeForm);
  initcetitle;
  Application.Run;
end.

