unit MainUnit2;

{$MODE Delphi}

//this unit is used by both the network client and the main program (USERINTERFACE)

interface

uses windows, dialogs,forms,classes,LCLIntf, LCLProc, sysutils,registry,ComCtrls, menus,
     formsettingsunit, cefuncproc,AdvancedOptionsUnit, MemoryBrowserFormUnit,
     memscan,plugin, hotkeyhandler,frmProcessWatcherunit, newkernelhandler;


resourcestring
  cename = 'Cheat Engine 6.2';
  rsPleaseWait = 'Please Wait!';

procedure UpdateToolsMenu;
procedure LoadSettingsFromRegistry;
procedure initcetitle;

function getVarType: TVariableType;
function getVarType2: TVariableType;

function GetScanType: TScanOption;
function GetScanType2: TScanOption;


const beta=' Alpha 3'; //empty this for a release

var
  CEnorm:string;
  CERegion: string;
  CESearch: string;
  CERegionSearch: string;
  CEWait: string;

resourcestring
  strStart='Start';
  strStop='Stop';
  strOK='OK';
  strBug='BUG!';
  strAutoAssemble='Assembler';

  strAddressHasToBeReadable='The address has to be readable if you want to use this function';
  strNewScan='New Scan';
  strFirstScan='First Scan';
  strNoDescription='No description';

  strNeedNewerWindowsVersion='This function only works in Windows 2000+ (perhaps also NT but not tested)';

  //scantypes
  strexact='Exact';
  strexactvalue='Exact Value';
  strbiggerThan='Bigger than...';
  strSmallerThan='Smaller than...';
  strIncreasedValue='Increased value';
  strIncreasedValueBy='Increased value by ...';
  strDecreasedValue='Decreased value';
  strDecreasedValueBy='Decreased value by ...';
  strValueBetween='Value between...';

  strChangedValue='Changed value';
  strUnchangedValue='Unchanged value';
  strUnknownInitialValue='Unknown initial value';
  strCompareToFirstScan='Compare to first scan';
  strCompareToLastScan='Compare to last scan';
  strCompareToSavedScan='Compare to saved scan';

  strFailedToInitialize='Failed to initialize the debugger';
  strtoolong='Too long';
  rsUseTheGameApplicationForAWhile = 'Use the game/application for a while and make the address you''re watching change. The list will be filled with addresses that contain code '
    +'that change the watched address.';
  rsSelectAnItemFromTheListForASmallDescription = 'Select an item from the list for a small description';
  rsNoHotkey = 'No hotkey';
  rsEnableDisableSpeedhack = 'Enable/Disable speedhack.';



  var
    speedhackspeed1: tspeedhackspeed;
    speedhackspeed2: tspeedhackspeed;
    speedhackspeed3: tspeedhackspeed;
    speedhackspeed4: tspeedhackspeed;
    speedhackspeed5: tspeedhackspeed;

    speedupdelta: single;
    slowdowndelta: single;

implementation


uses KernelDebugger,mainunit;

function GetScanType2: TScanOption;
{
not needed anymore
}
begin
  result:=GetScanType;
end;

function GetScanType: TScanOption;
begin
  result:=soExactValue;
  with mainform do
  begin
    if not (getvartype in [vtBinary,vtString,vtByteArray]) then //not binary, string or bytearray
    begin
      if not nextscanbutton.enabled then
      begin
        //first scan
        case scantype.ItemIndex of
          0: result:=soExactValue;
          1: result:=soBiggerThan;
          2: result:=soSmallerThan;
          3: result:=soValueBetween;
          4: result:=soUnknownValue;
        end;
      end
      else
      begin
        //next scan
        case scantype.itemindex of
          0: result:=soExactValue;
          1: result:=soBiggerThan;
          2: result:=soSmallerThan;
          3: result:=soValueBetween;
          4: result:=soIncreasedValue;
          5: result:=soIncreasedValueBy;
          6: result:=soDecreasedValue;
          7: result:=soDecreasedValueBy;
          8: result:=soChanged;
          9: result:=soUnchanged;

        end;
      end;
    end;
  end;
end;


function getVarType2: TVariableType; //obsolete
begin
  result:=getVarType;

end;

function getVarType: TVariableType;
begin
  if mainform.vartype.itemindex>=11 then
    result:=vtCustom
  else
  case mainform.VarType.ItemIndex of
    0: result:=vtBinary; //binary
    1: result:=vtByte; //byte
    2: result:=vtWord; //2 bytes
    3: result:=vtDword; //4 bytes
    4: result:=vtQword; //8 bytes
    5: result:=vtSingle; //float
    6: result:=vtDouble; //double
    7: result:=vtString; //text
    8: result:=vtByteArray; //array of byte
    9: result:=vtAll; //all, only for new memscan
    10: result:=vtGrouped; //grouped, only for memscan
  end;
end;

procedure UpdateToolsMenu;
var i: integer;
    mi: tmenuitem;
begin

  with formsettings do
  begin
    mainform.ools1.Clear;
    mainform.ools1.Visible:=lvtools.Items.Count>0;
    for i:=0 to lvTools.Items.Count-1 do
    begin

      mi:=tmenuitem.Create(mainform);

      mi.Caption:=lvTools.Items[i].Caption;
      mi.ShortCut:=TShortCut(ptrUint(lvTools.Items[i].data));
      mi.Tag:=i;
      mi.OnClick:=mainform.OnToolsClick;
      mainform.ools1.Add(mi);
    end;


  end;
end;

procedure LoadSettingsFromRegistry;
var reg : TRegistry;
    i,j: integer;
    temphotkeylist: array [0..30] of cefuncproc.tkeycombo;
    found:boolean;
    names: TStringList;
    li: tlistitem;
    s,s2: string;
begin
  ZeroMemory(@temphotkeylist, 31*sizeof(cefuncproc.tkeycombo));
  if formsettings=nil then exit;

  try
    reg:=Tregistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey('\Software\Cheat Engine',false) then
      begin

        with formsettings do
        begin
          LoadingSettingsFromRegistry:=true;

          if reg.ValueExists('Saved Stacksize') then
            savedStackSize:=reg.ReadInteger('Saved Stacksize');

          edtStacksize.text:=inttostr(savedStackSize);

          if reg.valueexists('Show processlist in mainmenu') then
            cbShowProcesslist.checked:=reg.readbool('Show processlist in mainmenu');

          mainform.Process1.visible:=cbShowProcesslist.checked;

          if reg.ValueExists('Ask if table has lua script') then
            cbAskIfTableHasLuascript.Checked:=reg.ReadBool('Ask if table has lua script');

          if reg.ValueExists('Always run script') then
            cbAlwaysRunScript.Checked:=reg.ReadBool('Always run script');

          if reg.ValueExists('All includes custom types') then
            cbAllIncludesCustomType.checked:=reg.readbool('All includes custom types');


          if reg.ValueExists('Show all windows on taskbar') then
          begin
            if reg.ReadBool('Show all windows on taskbar') then
            begin
              cbShowallWindows.checked:=true;
              Application.TaskBarBehavior:=tbMultiButton;
            end
            else
            begin
              cbShowallWindows.checked:=false;
              Application.TaskBarBehavior:=tbSingleButton;
            end;
          end;


          if reg.ValueExists('Undo') then
            cbshowundo.checked:=reg.ReadBool('Undo');


          if reg.ValueExists('ScanThreadpriority') then
            combothreadpriority.itemindex:=reg.ReadInteger('ScanThreadpriority');

          case combothreadpriority.itemindex of
            0: scanpriority:=tpIdle;
            1: scanpriority:=tpLowest;
            2: scanpriority:=tpLower;
            3: scanpriority:=tpLower;
            4: scanpriority:=tpNormal;
            5: scanpriority:=tpHigher;
            6: scanpriority:=tpHighest;
            7: scanpriority:=tpTimeCritical;
          end;

          mainform.UndoScan.visible:=cbshowundo.checked;

          {$ifndef net}
          if reg.ValueExists('hotkey poll interval') then
            hotkeyPollInterval:=reg.ReadInteger('hotkey poll interval')
          else
            hotkeyPollInterval:=100;

          if reg.ValueExists('Time between hotkeypress') then
            hotkeyIdletime:=reg.ReadInteger('Time between hotkeypress')
          else
            hotkeyIdletime:=100;

          frameHotkeyConfig.edtKeypollInterval.text:=inttostr(hotkeyPollInterval);
          frameHotkeyConfig.edtHotkeyDelay.text:=inttostr(hotkeyIdletime);

          SuspendHotkeyHandler;

          if reg.ValueExists('Speedhack 1 speed') then
            speedhackspeed1.speed:=reg.ReadFloat('Speedhack 1 speed')
          else
            speedhackspeed1.speed:=1;


          if reg.ValueExists('Speedhack 2 speed') then
            speedhackspeed2.speed:=reg.ReadFloat('Speedhack 2 speed')
          else
            speedhackspeed2.speed:=1;



          if reg.ValueExists('Speedhack 3 speed') then
            speedhackspeed3.speed:=reg.ReadFloat('Speedhack 3 speed')
          else
            speedhackspeed3.speed:=1;


          if reg.ValueExists('Speedhack 4 speed') then
            speedhackspeed4.speed:=reg.ReadFloat('Speedhack 4 speed')
          else
            speedhackspeed4.speed:=1;



          if reg.ValueExists('Speedhack 5 speed') then
            speedhackspeed5.speed:=reg.ReadFloat('Speedhack 5 speed')
          else
            speedhackspeed5.speed:=1;



          if reg.ValueExists('Increase Speedhack delta') then
            speedupdelta:=reg.ReadFloat('Increase Speedhack delta')
          else
            speedupdelta:=1;

          if reg.ValueExists('Decrease Speedhack delta') then
            slowdowndelta:=reg.ReadFloat('Decrease Speedhack delta')
          else
            slowdowndelta:=1;


          if reg.ValueExists('Show Cheat Engine Hotkey') then
            reg.ReadBinaryData('Show Cheat Engine Hotkey',temphotkeylist[0][0],10) else  mainform.label7.Caption:='';

          if reg.ValueExists('Pause process Hotkey') then
            reg.ReadBinaryData('Pause process Hotkey',temphotkeylist[1][0],10);

          if reg.ValueExists('Toggle speedhack Hotkey') then
            reg.ReadBinaryData('Toggle speedhack Hotkey',temphotkeylist[2][0],10);

          if reg.ValueExists('Set Speedhack speed 1 Hotkey') then
            reg.ReadBinaryData('Set Speedhack speed 1 Hotkey',temphotkeylist[3][0],10);

          if reg.ValueExists('Set Speedhack speed 2 Hotkey') then
            reg.ReadBinaryData('Set Speedhack speed 2 Hotkey',temphotkeylist[4][0],10);

          if reg.ValueExists('Set Speedhack speed 3 Hotkey') then
            reg.ReadBinaryData('Set Speedhack speed 3 Hotkey',temphotkeylist[5][0],10);

          if reg.ValueExists('Set Speedhack speed 4 Hotkey') then
            reg.ReadBinaryData('Set Speedhack speed 4 Hotkey',temphotkeylist[6][0],10);

          if reg.ValueExists('Set Speedhack speed 5 Hotkey') then
            reg.ReadBinaryData('Set Speedhack speed 5 Hotkey',temphotkeylist[7][0],10);

          if reg.ValueExists('Increase Speedhack speed') then
            reg.ReadBinaryData('Increase Speedhack speed',temphotkeylist[8][0],10);

          if reg.ValueExists('Decrease Speedhack speed') then
            reg.ReadBinaryData('Decrease Speedhack speed',temphotkeylist[9][0],10);

          if reg.ValueExists('Binary Hotkey') then
            reg.ReadBinaryData('Binary Hotkey',temphotkeylist[10][0],10);

          if reg.ValueExists('Byte Hotkey') then
            reg.ReadBinaryData('Byte Hotkey',temphotkeylist[11][0],10);

          if reg.ValueExists('2 Bytes Hotkey') then
            reg.ReadBinaryData('2 Bytes Hotkey',temphotkeylist[12][0],10);

          if reg.ValueExists('4 Bytes Hotkey') then
            reg.ReadBinaryData('4 Bytes Hotkey',temphotkeylist[13][0],10);

          if reg.ValueExists('8 Bytes Hotkey') then
            reg.ReadBinaryData('8 Bytes Hotkey',temphotkeylist[14][0],10);

          if reg.ValueExists('Float Hotkey') then
            reg.ReadBinaryData('Float Hotkey',temphotkeylist[15][0],10);

          if reg.ValueExists('Double Hotkey') then
            reg.ReadBinaryData('Double Hotkey',temphotkeylist[16][0],10);

          if reg.ValueExists('Text Hotkey') then
            reg.ReadBinaryData('Text Hotkey',temphotkeylist[17][0],10);

          if reg.ValueExists('Array of Byte Hotkey') then
            reg.ReadBinaryData('Array of Byte Hotkey',temphotkeylist[18][0],10);

          if reg.ValueExists('New Scan Hotkey') then
            reg.ReadBinaryData('New Scan Hotkey',temphotkeylist[19][0],10);

          if reg.ValueExists('New Scan-Exact Value') then
            reg.ReadBinaryData('New Scan-Exact Value',temphotkeylist[20][0],10);

          if reg.ValueExists('Unknown Initial Value Hotkey') then
            reg.ReadBinaryData('Unknown Initial Value Hotkey',temphotkeylist[21][0],10);

          if reg.ValueExists('Next Scan-Exact Value') then
            reg.ReadBinaryData('Next Scan-Exact Value',temphotkeylist[22][0],10);

          if reg.ValueExists('Increased Value Hotkey') then
            reg.ReadBinaryData('Increased Value Hotkey',temphotkeylist[23][0],10);

          if reg.ValueExists('Decreased Value Hotkey') then
            reg.ReadBinaryData('Decreased Value Hotkey',temphotkeylist[24][0],10);

          if reg.ValueExists('Changed Value Hotkey') then
            reg.ReadBinaryData('Changed Value Hotkey',temphotkeylist[25][0],10);

          if reg.ValueExists('Unchanged Value Hotkey') then
            reg.ReadBinaryData('Unchanged Value Hotkey',temphotkeylist[26][0],10);

          if reg.ValueExists('Same as first scan Hotkey') then
            reg.ReadBinaryData('Same as first scan Hotkey',temphotkeylist[27][0],10);

          if reg.ValueExists('Undo Last scan Hotkey') then
            reg.ReadBinaryData('Undo Last scan Hotkey',temphotkeylist[28][0],10);

          if reg.ValueExists('Cancel scan Hotkey') then
            reg.ReadBinaryData('Cancel scan Hotkey',temphotkeylist[29][0],10);

          if reg.ValueExists('Debug->Run Hotkey') then
            reg.ReadBinaryData('Debug->Run Hotkey',temphotkeylist[30][0],10);



          //fill the hotkeylist
          for i:=0 to 30 do
          begin
            found:=false;

            for j:=0 to length(hotkeythread.hotkeylist)-1 do
            begin
              if (hotkeythread.hotkeylist[j].id=i) and (hotkeythread.hotkeylist[j].handler2) then
              begin
                //found it
                hotkeythread.hotkeylist[j].keys:=temphotkeylist[i];
                found:=true;
                break;
              end;
            end;

            if not found then //add it
            begin
              j:=length(hotkeythread.hotkeylist);
              setlength(hotkeythread.hotkeylist,j+1);
              hotkeythread.hotkeylist[j].keys:=temphotkeylist[i];
              hotkeythread.hotkeylist[j].windowtonotify:=mainform.Handle;
              hotkeythread.hotkeylist[j].id:=i;
              hotkeythread.hotkeylist[j].handler2:=true;
            end;

            checkkeycombo(temphotkeylist[i]);
          end;

          if temphotkeylist[1][0]<>0 then
            advancedoptions.pausehotkeystring:='('+ConvertKeyComboToString(temphotkeylist[1])+')'
          else
            advancedoptions.pausehotkeystring:=' ('+rsNoHotkey+')';



          if temphotkeylist[2][0]<>0 then
            mainform.cbSpeedhack.Hint:=rsEnableDisableSpeedhack+' ('+
              ConvertKeyComboToString(temphotkeylist[2])+')'
          else
            mainform.cbSpeedhack.Hint:=rsEnableDisableSpeedhack+' (No hotkey)';


          ResumeHotkeyHandler;

          {$endif}

          if reg.ValueExists('Buffersize') then
            buffersize:=reg.readInteger('Buffersize')
          else
            buffersize:=512;




          try EditBufSize.text:=IntToStr(buffersize) except EditBufSize.Text:='512'; end;
          buffersize:=buffersize*1024;
          {$ifdef net} mainform.buffersize:=buffersize; {$endif}


          if reg.ValueExists('Center on popup') then
            formsettings.cbCenterOnPopup.checked:=reg.readbool('Center on popup');

          if reg.ValueExists('Update interval') then
            mainform.updatetimer.Interval:=reg.readInteger('Update interval');

          if reg.ValueExists('Freeze interval') then
            mainform.freezetimer.Interval:=reg.readInteger('Freeze interval');

          formsettings.EditUpdateInterval.text:=IntToStr(mainform.updatetimer.Interval);
          formsettings.EditFreezeInterval.text:=IntToStr(mainform.freezetimer.Interval);

          {$ifdef net}
          //also get the update interval for network
          try i:=reg.readInteger('Network Update Interval'); except end;
          try formsettings.EditNetworkUpdateInterval.Text:=IntToStr(i); except end;
          {$endif}

          if reg.ValueExists('Show values as signed') then
            cbShowAsSigned.checked:=reg.readbool('Show values as signed');

          if reg.ValueExists('AutoAttach') then
            EditAutoAttach.Text:=reg.ReadString('AutoAttach');

          if reg.ValueExists('Always AutoAttach') then
            cbAlwaysAutoAttach.checked:=reg.readbool('Always AutoAttach');


          if reg.ValueExists('Replace incomplete opcodes with NOPS') then
            replacewithnops.checked:=reg.readBool('Replace incomplete opcodes with NOPS');

          if reg.ValueExists('Override existing bp''s') then
            cbOverrideExistingBPs.checked:=reg.readBool('Override existing bp''s');

          BPOverride:=cbOverrideExistingBPs.checked;



          if reg.ValueExists('Ask for replace with NOPS') then
            askforreplacewithnops.checked:=reg.readBool('Ask for replace with NOPS');

          try cbFastscan.checked:=reg.ReadBool('Fastscan on by default'); except end;
          try checkbox1.Checked:=reg.readbool('Use Anti-debugdetection'); except end;
          try cbhandlebreakpoints.Checked:=reg.ReadBool('Handle unhandled breakpoints'); except end;

          if cbFastscan.Checked then mainform.cbFastscan.Checked:=true else mainform.cbFastScan.Checked:=false;

          try cbsimplecopypaste.checked:=reg.readbool('Simple copy/paste'); except end;


          try rbDebugAsBreakpoint.Checked:=reg.readbool('Hardware breakpoints'); except end;
          try rbInt3AsBreakpoint.checked:=not reg.readbool('Hardware breakpoints'); except end;

          try cbUpdatefoundList.Checked:=reg.readbool('Update Foundaddress list'); except end;

          if reg.ValueExists('Update Foundaddress list Interval') then
            try mainform.UpdateFoundlisttimer.interval:=reg.readInteger('Update Foundaddress list Interval'); except end;

          editUpdatefoundInterval.Text:=IntToStr(mainform.UpdateFoundlisttimer.interval);

          if reg.ValueExists('Save window positions') then
            cbSaveWindowPos.checked:=reg.ReadBool('Save window positions');

          if reg.ValueExists('Show main menu') then
            cbShowMainMenu.Checked:=reg.ReadBool('Show main menu');

          if reg.ValueExists('Get process icons') then
            cbProcessIcons.Checked:=reg.ReadBool('Get process icons');
          GetProcessIcons:=cbProcessIcons.Checked;


          if reg.ValueExists('Only show processes with icon') then
            cbProcessIconsOnly.checked:=reg.ReadBool('Only show processes with icon');

          if reg.ValueExists('Pointer appending') then
            cbOldPointerAddMethod.checked:=reg.ReadBool('Pointer appending');

          cbProcessIconsOnly.Enabled:=cbProcessIcons.Checked;
          ProcessesWithIconsOnly:=cbProcessIconsOnly.Checked;


          if reg.ValueExists('skip PAGE_NOCACHE') then
            cbSkip_PAGE_NOCACHE.Checked:=reg.readbool('skip PAGE_NOCACHE');
            
          Skip_PAGE_NOCACHE:=cbSkip_PAGE_NOCACHE.Checked;


          if reg.ValueExists('Hide all windows') then
            cbHideAllWindows.Checked:=reg.ReadBool('Hide all windows');

          if reg.ValueExists('Really hide all windows') then
            temphideall:=reg.ReadBool('Really hide all windows');
            
          onlyfront:=not formsettings.temphideall;

          if reg.ValueExists('MEM_PRIVATE') then
            cbMemPrivate.Checked:=reg.ReadBool('MEM_PRIVATE');

          if reg.ValueExists('MEM_IMAGE') then
            cbMemImage.Checked:=reg.ReadBool('MEM_IMAGE');

          if reg.ValueExists('MEM_MAPPED') then
            cbMemMapped.Checked:=reg.ReadBool('MEM_MAPPED');

          Scan_MEM_PRIVATE:=cbMemPrivate.checked;
          Scan_MEM_IMAGE:=cbMemImage.Checked;
          Scan_MEM_MAPPED:=cbMemMapped.Checked;

          if reg.ValueExists('Can Step Kernelcode') then
            cbCanStepKernelcode.checked:=reg.ReadBool('Can Step Kernelcode');



          try cbKernelQueryMemoryRegion.checked:=reg.ReadBool('Use dbk32 QueryMemoryRegionEx'); except end;
          try cbKernelReadWriteProcessMemory.checked:=reg.ReadBool('Use dbk32 ReadWriteProcessMemory'); except end;
          try cbKernelOpenProcess.checked:=reg.ReadBool('Use dbk32 OpenProcess'); except end;

          {$ifndef net}


          try unrandomizersettings.defaultreturn:=reg.ReadInteger('Unrandomizer: default value'); except end;
          try unrandomizersettings.incremental:=reg.ReadBool('Unrandomizer: incremental'); except end;

          if reg.ValueExists('ModuleList as Denylist') then
            DenyList:=reg.ReadBool('ModuleList as Denylist')
          else
            denylist:=true;
            
          if reg.ValueExists('Global Denylist') then
            DenyListGlobal:=reg.ReadBool('Global Denylist')
          else
            denylistglobal:=false;

          if reg.ValueExists('ModuleListSize') then
            ModuleListSize:=reg.ReadInteger('ModuleListSize')
          else
            modulelistsize:=0;
            
          if modulelist<>nil then freemem(modulelist);
          getmem(modulelist,modulelistsize);

          if reg.ValueExists('Module List') then
            reg.ReadBinaryData('Module List',ModuleList^,ModuleListSize);



          if reg.ValueExists('Don''t use tempdir') then
            cbDontUseTempDir.checked:=reg.ReadBool('Don''t use tempdir');

          if reg.ValueExists('Scanfolder') then
            edtTempScanFolder.text:=reg.ReadString('Scanfolder');

          dontusetempdir:=cbDontusetempdir.checked;
          tempdiralternative:=edtTempScanFolder.text;


          if reg.ValueExists('Use Processwatcher') then
            cbProcessWatcher.checked:=reg.readBool('Use Processwatcher');

          if reg.ValueExists('Use VEH Debugger') then
            cbUseVEHDebugger.Checked:=reg.ReadBool('Use VEH Debugger');

          if reg.ValueExists('Use Windows Debugger') then
            cbUseWindowsDebugger.checked:=reg.ReadBool('Use Windows Debugger');

          if reg.ValueExists('Use Kernel Debugger') then
            cbKdebug.checked:=reg.ReadBool('Use Kernel Debugger');



          try cbGlobalDebug.checked:=reg.ReadBool('Use Global Debug Routines'); except end;

          if reg.ValueExists('Show tools menu') then
            cbShowTools.Checked:=reg.ReadBool('Show tools menu');

          mainform.ools1.Visible:=cbShowTools.Checked;



          if cbKernelQueryMemoryRegion.checked then UseDBKQueryMemoryRegion else DontUseDBKQueryMemoryRegion;
          if cbKernelReadWriteProcessMemory.checked then UseDBKReadWriteMemory else DontUseDBKReadWriteMemory;
          if cbKernelOpenProcess.Checked then UseDBKOpenProcess else DontUseDBKOpenProcess;

          if cbProcessWatcher.Checked then
            if (frmProcessWatcher=nil) then //propably yes
              frmProcessWatcher:=tfrmprocesswatcher.Create(mainform); //start the process watcher

          {$endif}





        end;


      end;

      {$ifndef net}
      formsettings.lvtools.Clear;
      if Reg.OpenKey('\Software\Cheat Engine\Tools',false) then
      begin
        names:=TStringList.create;
        try
          reg.GetValueNames(names);
          names.sort;
          for i:=0 to names.count-1 do
          begin
            try
              if names[i][10]='A' then //tools name
                s:=reg.ReadString(names[i]);

              if names[i][10]='B' then //tools app
                s2:=reg.ReadString(names[i]);

              if names[i][10]='C' then //tools shortcut
              begin
                //A is already handled (sorted) so s contaisn the name
                li:=formsettings.lvtools.items.add;
                li.caption:=s;
                li.Data:=pointer(ptrUint(reg.readinteger(names[i])));

                li.SubItems.Add(s2);
                li.SubItems.Add(ShortCutToText(ptrUint(li.data)));

              end;
            except
            end;
          end;
        finally
          names.free;
        end;
      end;
      UpdateToolsMenu;



      if Reg.OpenKey('\Software\Cheat Engine\Plugins'{$ifdef cpu64}+'64'{$else}+'32'{$endif},false) then
      begin
        names:=TStringList.create;
        try
          reg.GetValueNames(names);
          names.Sort;

          for i:=0 to names.Count-1 do
          begin
            try
              if names[i][10]='A' then //plugin dll
              begin
                s:=reg.ReadString(names[i]);
                j:=pluginhandler.LoadPlugin(s);
              end;

              if (j>=0) and (names[i][10]='B') then //enabled or not
              begin
                if reg.ReadBool(names[i]) then
                  pluginhandler.EnablePlugin(j);
              end;
            except

            end;


          end;

          pluginhandler.FillCheckListBox(formsettings.clbPlugins);
        finally
          names.Free;
        end;
      end;
      {$endif}


      formsettings.LoadingSettingsFromRegistry:=false;

    finally
      reg.CloseKey;
    end;
  except

  end;

  {$ifdef net}
  MemoryBrowser.Kerneltools1.visible:=false;
  {$else}
  MemoryBrowser.Kerneltools1.Enabled:=DBKLoaded;
  {$endif}


  mainform.autoattachlist.Delimiter:=';';
  mainform.autoattachlist.DelimitedText:=formsettings.EditAutoAttach.Text;


  if formsettings.cbShowMainMenu.Checked then
    mainform.Menu:=mainform.MainMenu1
  else
    mainform.Menu:=nil;

end;

procedure initcetitle;
begin
  CEnorm:=cename+BETA;  //.';
  Application.Title:=CENorm;


  CERegion:=cenorm+' - '+rsPleaseWait;
  CESearch:=CERegion;
  CERegionSearch:= CERegion;
  CEWait:= ceregion;
  mainform.Caption:=CENorm;
end;

end.
















