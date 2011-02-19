program cheatengine;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  sysutils, Forms, bogus, MainUnit, CEDebugger, NewKernelHandler, CEFuncProc,
  ProcessHandlerUnit, symbolhandler, Assemblerunit, hypermode, byteinterpreter,
  addressparser, autoassembler, ProcessWindowUnit, MainUnit2, Filehandler,
  dbvmPhysicalMemoryHandler, frameHotkeyConfigUnit, formsettingsunit,
  HotkeyHandler, formhotkeyunit, AdvancedOptionsUnit, inputboxtopunit, plugin,
  pluginexports, tlgUnit, aboutunit, frmProcesswatcherExtraUnit,
  frmProcessWatcherUnit, ModuleSafetyUnit, frmExcludeHideUnit,
  ConfigUnrandomizerFrm, AddAddress, HotKeys, TypePopup, CommentsUnit,
  FoundCodeUnit, foundlisthelper, unrandomizer, SaveFirstScan, savedscanhandler,
  memscan, formScanningUnit, KernelDebugger, formDifferentBitSizeUnit,
  formAddressChangeUnit, Changeoffsetunit, speedhack2, formPointerOrPointeeUnit,
  AccessCheck, formmemoryregionsunit, OpenSave, formProcessInfo,
  frmautoinjectunit, MenuItemExtra, MemoryBrowserFormUnit,
  disassemblerviewlinesunit, disassemblerviewunit, PasteTableentryFRM,
  frmBreakpointlistunit, DissectCodeThread, DissectCodeunit, Valuechange,
  FindWindowUnit, stacktrace2, frmstacktraceunit, frmBreakThreadUnit,
  FormDebugStringsUnit, frmDissectwindowUnit, frmCapturedTimersUnit,
  frmEnumerateDLLsUnit, frmThreadlistunit, frmMemoryAllocHandlerUnit,
  circularBuffer, PEInfoFunctions, PEInfounit, FileMapping, frmFindstaticsUnit,
  frmModifyRegistersUnit, frmHeapsUnit, savedisassemblyfrm,
  frmSaveMemoryRegionUnit, frmLoadMemoryunit, formAddToCodeList,
  frmFillMemoryUnit, frmCodecaveScannerUnit, frmSelectionlistunit,
  symbolconfigunit, frmFloatingPointPanelUnit, frmTracerUnit, DriverList,
  frmRegistersunit, formChangedAddresses, frmGDTunit, frmIDTunit,
  frmDisassemblyscanunit, frmReferencedStringsUnit, StructuresAddElementfrm,
  Structuresfrm, PointerscannerSettingsFrm, simpleaobscanner,
  PointerscanresultReader, pointervaluelist, rescanhelper, pointerscannerfrm,
  VirtualMemory, ValueFinder, frmRescanPointerUnit, SyncObjs2,
  ManualModuleLoader, SynHighlighterAA, APIhooktemplatesettingsfrm,
  frmAAEditPrefsUnit, disassembler, hexviewunit, guisafecriticalsection,
  DebugHelper, debugeventhandler, formFoundcodeListExtraUnit,
  debuggertypedefinitions, sfloat, addresslist, MemoryRecordUnit,
  ThreadlistExFRM, windows7taskbar, tablist, frmStructuresConfigUnit,
  VEHDebugger, VEHDebugSharedMem, DebuggerInterface, WindowsDebugger,
  DebuggerInterfaceAPIWrapper, frmDebugEventsUnit, changelist, tableconverter,
  DBK32functions, debug, multicpuexecution, vmxfunctions, frmPagingUnit,
  bigmemallochandler, KernelDebuggerInterface, CustomTypeHandler, LuaHandler,
  frmLuaEngineUnit, frmMemviewPreferencesUnit, frmBreakpointConditionUnit,
  frmTracerConfigUnit, frmStackViewUnit, luaJit, ScrollBoxEx, fileaccess,
  ceguicomponents, formdesignerunit, LuaCaller, 
LuaSyntax, cesupport, trainergenerator, genericHotkey,
frmExeTrainerGeneratorUnit, luafile, xmplayer_server, xmplayer_defines;

{$R cheatengine.res}
{$R manifest.res}

{$ifdef cpu32}
{$SetPEFlags $20}
{$endif}



begin
  Application.Title:='Cheat Engine 6.1';
  Application.Initialize;
  getcedir;
  symhandlerInitialize;

 // LRSTranslator := TPoTranslator.Create(FindLocaleFileName('.po'));
  //TranslateUnitResourceStrings('MainUnit',
  Application.ShowMainForm:=false;

  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TMemoryBrowser, MemoryBrowser);
  Application.CreateForm(TformSettings, formSettings);
  Application.CreateForm(TAdvancedOptions, AdvancedOptions);
  Application.CreateForm(TComments, Comments);
  Application.CreateForm(TTypeForm, TypeForm);
  initcetitle;

  if paramcount >= 1 then
  begin
    try
      mainform.visible:=uppercase(ExtractFileExt(paramstr(1)))<>'.CETRAINER';
      LoadTable(paramstr(1),false);
    except
    end;
  end
  else
    mainform.visible:=true;

  Application.CreateForm(TfrmExeTrainerGenerator, frmExeTrainerGenerator);
  Application.Run;
end.

