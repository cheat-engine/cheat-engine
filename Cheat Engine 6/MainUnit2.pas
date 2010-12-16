unit MainUnit2;

{$MODE Delphi}

//this unit is used by both the network client and the main program (USERINTERFACE)

interface

uses windows, dialogs,forms,classes,LCLIntf, LCLProc, sysutils,registry,ComCtrls, menus,
     formsettingsunit, cefuncproc,AdvancedOptionsUnit, MemoryBrowserFormUnit,
     memscan,plugin, mainunit,hotkeyhandler,frmProcessWatcherunit, newkernelhandler;


procedure UpdateToolsMenu;
procedure HandleautoAttachString;
procedure LoadSettingsFromRegistry;
procedure initcetitle;
function GetScanType: Integer;
function getVarType: Integer;
function GetScanType2: TScanOption;
function getVarType2: TVariableType;

const beta=' Beta'; //empty this for a release
const cename='Cheat Engine 6.0';

var
  CEnorm:string = cename+beta;
  CERegion:string = cename+beta+' - Please Wait!';
  CESearch:string = cename+beta+' - Please Wait!';
  CERegionSearch:string = cename+beta+' - Please Wait!';
  CEWait:string= cename+beta+' - Please Wait!';

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
  strSameAsFirstScan='Same as first scan';

  strFailedToInitialize='Failed to initialize the debugger';
  strtoolong='Too long';



  var
    speedhackspeed1: tspeedhackspeed;
    speedhackspeed2: tspeedhackspeed;
    speedhackspeed3: tspeedhackspeed;
    speedhackspeed4: tspeedhackspeed;
    speedhackspeed5: tspeedhackspeed;

    speedupdelta: single;
    slowdowndelta: single;

implementation

uses KernelDebugger;

function GetScanType2: TScanOption;
{
Determine the current scanoption.
If it's a custom type vatriable, it's always of type custom
}
begin
  if getVarType2=vtAutoAssembler then result:=soCustom
  else
  case GetScanType of
    exact_value:       result:=soExactValue;
    biggerthan:        result:=soBiggerThan;
    smallerthan:       result:=soSmallerThan;
    valueBetween:      result:=soValueBetween;
    Advanced_scan:     result:=soUnknownValue;

    Increased_value:   result:=soIncreasedValue;
    Increased_value_by:result:=soIncreasedValueBy;
    Decreased_value:   result:=soDecreasedValue;
    Decreased_value_by:result:=soDecreasedValueBy;
    Changed_value:     result:=soChanged;
    Unchanged_value:   result:=soUnchanged;
    SameAsFirst:       result:=soSameAsFirst;
    else result:=soExactValue;
  end;

end;

function GetScanType: Integer;
begin
  with mainform do
  begin
    result:=exact_value;

    if getvartype in [0,1,2,3,4,6,9] then
    begin
      if not nextscanbutton.enabled then
      begin
        //first scan
        case scantype.ItemIndex of
          0: result:=exact_value;
          1: result:=biggerthan;
          2: result:=smallerthan;
          3: result:=valuebetween;
          4: result:=Advanced_scan;
        end;
      end
      else
      begin
        //next scan
        case scantype.itemindex of
          0: result:=exact_value;
          1: result:=biggerthan;
          2: result:=smallerthan;
          3: result:=valuebetween;
          4: result:=increased_value;
          5: result:=increased_value_by;
          6: result:=decreased_value;
          7: result:=decreased_value_by;
          8: result:=changed_value;
          9: result:=unchanged_value;
          10: result:=sameasfirst;
        end;
      end;
    end;
  end;
end;


function getVarType2: TVariableType;
var i: integer;
begin
  i:=getVarType;

  if i>=10 then
    result:=vtCustom
  else
  case i of
    5: result:=vtBinary;
    0: result:=vtByte;
    1: result:=vtWord;
    2: result:=vtDword;
    6: result:=vtQword;
    3: result:=vtSingle;
    4: result:=vtDouble;
    7: result:=vtString;
    8: result:=vtByteArray;
    9: result:=vtAll;
  end;


end;

function getVarType: Integer;
begin
  {
Bit = 5
Byte =0
2 Bytes =1
4 Bytes =2
8 Bytes =6
Float =3
Double =4
Text = 7
}
  result:=-1;
  with mainform do
  begin
    if vartype.itemindex>=10 then
      result:=10 //"custom"
    else
    case VarType.ItemIndex of
      0: result:=5; //binary
      1: result:=0; //byte
      2: result:=1; //2 bytes
      3: result:=2; //4 bytes
      4: result:=6; //8 bytes
      5: result:=3; //float
      6: result:=4; //double
      7: result:=7; //text
      8: result:=8; //array of byte
      9: result:=9; //all, only for new memscan


    end;
  end;
end;

procedure HandleautoAttachString;
var s: string;
    s2: string;
    i: integer;
begin

  mainform.autoattachlist.clear;
  s:=formsettings.EditAutoAttach.Text;
  s2:='';
  for i:=1 to length(s) do
  begin
    if s[i]=';' then
    begin
      s2:=trim(s2);
      if s2<>'' then
        mainform.autoattachlist.Add(s2);

      s2:='';
      continue;
    end;
    s2:=s2+s[i];
  end;

  s2:=trim(s2);
  if s2<>'' then
    mainform.autoattachlist.Add(s2);

  mainform.AutoAttachTimer.Enabled:=mainform.autoattachlist.Count>0;
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
    modifier: dword;
    key: dword;
    hotkey: string;
    i,j: integer;
    go: boolean;
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

          if reg.ValueExists('Ask if table has lua script') then
            cbAskIfTableHasLuascript.Checked:=reg.ReadBool('Ask if table has lua script');

          if reg.ValueExists('Always run script') then
            cbAlwaysRunScript.Checked:=reg.ReadBool('Always run script');


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

          if reg.ValueExists('Speedhack 1 sleeptime') then
            speedhackspeed1.sleeptime:=reg.ReadInteger('Speedhack 1 sleeptime')
          else
            speedhackspeed1.sleeptime:=3;

          if reg.ValueExists('Speedhack 2 speed') then
            speedhackspeed2.speed:=reg.ReadFloat('Speedhack 2 speed')
          else
            speedhackspeed2.speed:=1;

          if reg.ValueExists('Speedhack 2 sleeptime') then
            speedhackspeed2.sleeptime:=reg.ReadInteger('Speedhack 2 sleeptime')
          else
            speedhackspeed2.sleeptime:=3;

          if reg.ValueExists('Speedhack 3 speed') then
            speedhackspeed3.speed:=reg.ReadFloat('Speedhack 3 speed')
          else
            speedhackspeed3.speed:=1;

          if reg.ValueExists('Speedhack 3 sleeptime') then
            speedhackspeed3.sleeptime:=reg.ReadInteger('Speedhack 3 sleeptime')
          else
            speedhackspeed3.sleeptime:=3;

          if reg.ValueExists('Speedhack 4 speed') then
            speedhackspeed4.speed:=reg.ReadFloat('Speedhack 4 speed')
          else
            speedhackspeed4.speed:=1;

          if reg.ValueExists('Speedhack 4 sleeptime') then
            speedhackspeed4.sleeptime:=reg.ReadInteger('Speedhack 4 sleeptime')
          else
            speedhackspeed4.sleeptime:=3;

          if reg.ValueExists('Speedhack 5 speed') then
            speedhackspeed5.speed:=reg.ReadFloat('Speedhack 5 speed')
          else
            speedhackspeed5.speed:=1;

          if reg.ValueExists('Speedhack 5 sleeptime') then
            speedhackspeed5.sleeptime:=reg.ReadInteger('Speedhack 5 sleeptime')
          else
            speedhackspeed5.sleeptime:=3;

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

          if reg.ValueExists('Speedhack speed 1') then
            reg.ReadBinaryData('Speedhack speed 1',Speedhackspeed1,sizeof(tspeedhackspeed))
          else
            speedhackspeed1.speed:=2;

          if reg.ValueExists('Speedhack speed 2') then
              reg.ReadBinaryData('Speedhack speed 2',Speedhackspeed2,sizeof(tspeedhackspeed))
            else
              speedhackspeed2.speed:=2;

          if reg.ValueExists('Speedhack speed 3') then
              reg.ReadBinaryData('Speedhack speed 3',Speedhackspeed3,sizeof(tspeedhackspeed))
            else
              speedhackspeed3.speed:=2;

          if reg.ValueExists('Speedhack speed 4') then
              reg.ReadBinaryData('Speedhack speed 4',Speedhackspeed4,sizeof(tspeedhackspeed))
            else
              speedhackspeed4.speed:=2;


          if reg.ValueExists('Speedhack speed 5') then
              reg.ReadBinaryData('Speedhack speed 5',Speedhackspeed5,sizeof(tspeedhackspeed))
            else
              speedhackspeed5.speed:=2;

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




          if temphotkeylist[0][0]<>0 then
            hotkey:=ConvertKeyComboToString(temphotkeylist[0])
          else
            hotkey:='no';



          if temphotkeylist[1][0]<>0 then
            advancedoptions.pausehotkeystring:='('+ConvertKeyComboToString(temphotkeylist[1])+')'
          else
            advancedoptions.pausehotkeystring:=' (No hotkey)';



          if temphotkeylist[2][0]<>0 then
            mainform.cbSpeedhack.Hint:='Enable/Disable speedhack. ('+ConvertKeyComboToString(temphotkeylist[2])+')'
          else
            mainform.cbSpeedhack.Hint:='Enable/Disable speedhack. (No hotkey)';


          ResumeHotkeyHandler;

          {$endif}

          if reg.ValueExists('Buffersize') then
            buffersize:=reg.readInteger('Buffersize')
          else
            buffersize:=512;



          try EditBufSize.text:=IntToStr(buffersize) except EditBufSize.Text:='512'; end;
          buffersize:=buffersize*1024;
          {$ifdef net} mainform.buffersize:=buffersize; {$endif}


          try formsettings.cbCenterOnPopup.checked:=reg.readbool('Center on popup'); except end;
          try mainform.updatetimer.Interval:=reg.readInteger('Update interval'); except end;
          try mainform.freezetimer.Interval:=reg.readInteger('Freeze interval'); except end;
          formsettings.EditUpdateInterval.text:=IntToStr(mainform.updatetimer.Interval);
          formsettings.EditFreezeInterval.text:=IntToStr(mainform.freezetimer.Interval);

          {$ifdef net}
          //also get the update interval for network
          try i:=reg.readInteger('Network Update Interval'); except end;
          try formsettings.EditNetworkUpdateInterval.Text:=IntToStr(i); except end;
          {$endif}

          try cbShowAsSigned.checked:=reg.readbool('Show values as signed'); except end;

          if reg.ValueExists('AutoAttach') then
            EditAutoAttach.Text:=reg.ReadString('AutoAttach');

          if reg.ValueExists('Always AutoAttach') then
            cbAlwaysAutoAttach.checked:=reg.readbool('Always AutoAttach');


          if reg.ValueExists('Replace incomplete opcodes with NOPS') then
            replacewithnops.checked:=reg.readBool('Replace incomplete opcodes with NOPS');

          try askforreplacewithnops.checked:=reg.readBool('Ask for replace with NOPS'); except end;
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


      if Reg.OpenKey('\Software\Cheat Engine\plugins',false) then
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

              if names[i][10]='B' then //enabled or not
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

  HandleAutoAttachString;

  if formsettings.cbShowMainMenu.Checked then
    mainform.Menu:=mainform.MainMenu1
  else
    mainform.Menu:=nil;

end;

procedure initcetitle;
begin
  CEnorm:=cename+BETA;  //.';
  Application.Title:=CENorm;


  CERegion:=cenorm+' - Please Wait!';
  CESearch:=CERegion;
  CERegionSearch:= CERegion;
  CEWait:= ceregion;
  mainform.Caption:=CENorm;
end;

end.
















