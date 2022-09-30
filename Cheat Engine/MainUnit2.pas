unit MainUnit2;

{$MODE Delphi}
{$MACRO ON}

//this unit is used by both the network client and the main program (USERINTERFACE)

interface

uses
     {$ifdef darwin}
     macport, macexceptiondebuggerinterface,
     {$endif}
     {$ifdef windows}
     windows,
     {$endif}
     dialogs,forms,classes,LCLIntf, LCLProc, sysutils,registry,ComCtrls, menus,
     formsettingsunit, cefuncproc,AdvancedOptionsUnit, MemoryBrowserFormUnit,
     memscan,plugin, hotkeyhandler,frmProcessWatcherUnit, newkernelhandler,
     debuggertypedefinitions, commonTypeDefs, betterControls;

const ceversion=7.42;
{$ifdef altname}  //i'd use $MACRO ON but fpc bugs out
  strCheatEngine='Runtime Modifier'; //if you change this, also change it in first.pas
  strCheatTable='Code Table';   //because it contains code.... duh.....
  strCheatTableLower='code table';
  strCheat='Modification';
  strTrainer='Modifier';
  strTrainerLower='modifier';
  strMyCheatTables='My Mod Tables';
  strSpeedHack='Speedmodifier';
{$else}
  strCheatEngine='Cheat Engine';
  strCheatTable='Cheat Table';
  strCheatTableLower='cheat table';
  strCheat='Cheat';
  strTrainer='Trainer';
  strTrainerLower='trainer';
  strMyCheatTables='My Cheat Tables';
  strSpeedHack='Speedhack';
{$endif}

resourcestring
  cename = strCheatEngine+' 7.4.2';
  rsCheatEngine = strCheatEngine;
  rsPleaseWait = 'Please Wait!';

procedure UpdateToolsMenu;
procedure LoadSettingsFromRegistry(skipPlugins: boolean=false; skipkernelapply: boolean=false);
procedure initcetitle;


const beta=''; //empty this for a release

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
  strIgnoreValue='Ignore value';
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
  rsM2NoHotkey = ' (No hotkey)';
  rsWontHaveAnyEffectUntilYouOpenANewProcess = '(Won''t have any effect until you (re)open a process)';
  rsDBVMMissedEntries = 'Missed %d entries due to a too small buffer or slow copy operation';


  var
    speedhackspeed1: tspeedhackspeed;
    speedhackspeed2: tspeedhackspeed;
    speedhackspeed3: tspeedhackspeed;
    speedhackspeed4: tspeedhackspeed;
    speedhackspeed5: tspeedhackspeed;

    speedupdelta: single;
    slowdowndelta: single;

implementation


uses KernelDebugger,mainunit, DebugHelper, CustomTypeHandler, ProcessList, Globals,
     frmEditHistoryUnit, DBK32functions, frameHotkeyConfigUnit, UnexpectedExceptionsHelper,
     TypInfo, StdCtrls;

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

procedure LoadSettingsFromRegistry(skipPlugins: boolean=false; skipkernelapply: boolean=false);
var reg : TRegistry;
    i,j: integer;
    temphotkeylist: array [0..cehotkeycount-1] of commontypedefs.tkeycombo;
    found:boolean;
    names: TStringList;
    li: tlistitem;
    s,s2: string;
begin
  ZeroMemory(@temphotkeylist, cehotkeycount*sizeof(commontypedefs.tkeycombo));
  if formsettings=nil then exit;

  s:=formsettings.ClassName;
  if s<>'TformSettings' then exit;

  try
    reg:=Tregistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey('\Software\'+strCheatEngine,false) then
      begin

        with formsettings do
        begin
          LoadingSettingsFromRegistry:=true;

          if reg.ValueExists('Disable DarkMode Support') then
            cbDisableDarkModeSupport.checked:=reg.ReadBool('Disable DarkMode Support');

          if reg.ValueExists('Saved Stacksize') then
            savedStackSize:=reg.ReadInteger('Saved Stacksize');

          edtStacksize.text:=inttostr(savedStackSize);

          if reg.valueexists('Show processlist in mainmenu') then
            cbShowProcesslist.checked:=reg.readbool('Show processlist in mainmenu');

          mainform.Process1.visible:=cbShowProcesslist.checked;


          if reg.ValueExists('LuaScriptAction') then
          begin
            i:=reg.ReadInteger('LuaScriptAction');
            case i of
              0: miLuaExecAlways.checked:=true;
              1: miLuaExecSignedOnly.checked:=true;
              2: miLuaExecAsk.checked:=true;
              3: miLuaExecNever.checked:=true;
            end;
          end
          else
            miLuaExecSignedOnly.checked:=true;


          if reg.ValueExists('AllByte') then cbAllByte.checked:=reg.readBool('AllByte') else cbAllByte.checked:=false;
          if reg.ValueExists('AllWord') then cbAllWord.checked:=reg.readBool('AllWord') else cbAllWord.checked:=false;
          if reg.ValueExists('AllDWord') then cbAllDword.checked:=reg.readBool('AllDWord') else cbAllDWord.checked:=true;
          if reg.ValueExists('AllQWord') then cbAllQword.checked:=reg.readBool('AllQWord') else cbAllQWord.checked:=false;
          if reg.ValueExists('AllFloat') then cbAllSingle.checked:=reg.readBool('AllFloat') else cbAllSingle.checked:=true;
          if reg.ValueExists('AllDouble') then cbAllDouble.checked:=reg.readBool('AllDouble') else cbAllDouble.checked:=true;
          if reg.ValueExists('AllCustom') then cbAllCustom.checked:=reg.readBool('AllCustom') else cbAllDouble.checked:=false;

          ScanAllTypes:=[];
          if cbAllByte.checked then ScanAllTypes:=ScanAllTypes+[vtByte];
          if cbAllWord.checked then ScanAllTypes:=ScanAllTypes+[vtWord];
          if cbAllDword.checked then ScanAllTypes:=ScanAllTypes+[vtDword];
          if cbAllQword.checked then ScanAllTypes:=ScanAllTypes+[vtQword];
          if cbAllSingle.checked then ScanAllTypes:=ScanAllTypes+[vtSingle];
          if cbAllDouble.checked then ScanAllTypes:=ScanAllTypes+[vtDouble];
          if cbAllCustom.checked then ScanAllTypes:=ScanAllTypes+[vtCustom];


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

          if reg.ValueExists('hotkey poll interval') then
            hotkeyPollInterval:=reg.ReadInteger('hotkey poll interval')
          else
            hotkeyPollInterval:=100;

          if reg.ValueExists('Time between hotkeypress') then
            hotkeyIdletime:=reg.ReadInteger('Time between hotkeypress')
          else
            hotkeyIdletime:=350;

          frameHotkeyConfig.edtKeypollInterval.text:=inttostr(hotkeyPollInterval);
          frameHotkeyConfig.edtHotkeyDelay.text:=inttostr(hotkeyIdletime);



          if reg.ValueExists('Speedhack 1 speed') then
            speedhackspeed1.speed:={$ifdef windows}reg.ReadFloat('Speedhack 1 speed'){$else}strtofloat(reg.ReadString('Speedhack 1 speed')){$endif} //readString as there is a stack corruption in readFloat on unix
          else
            speedhackspeed1.speed:=1;

          if reg.ValueExists('Speedhack 1 disablewhenreleased') then
            speedhackspeed1.disablewhenreleased:=reg.ReadBool('Speedhack 1 disablewhenreleased');


          if reg.ValueExists('Speedhack 2 speed') then
            speedhackspeed2.speed:={$ifdef windows}reg.ReadFloat('Speedhack 2 speed'){$else}strtofloat(reg.ReadString('Speedhack 2 speed')){$endif}
          else
            speedhackspeed2.speed:=1;

          if reg.ValueExists('Speedhack 2 disablewhenreleased') then
            speedhackspeed2.disablewhenreleased:=reg.ReadBool('Speedhack 2 disablewhenreleased');



          if reg.ValueExists('Speedhack 3 speed') then
            speedhackspeed3.speed:={$ifdef windows}reg.ReadFloat('Speedhack 3 speed'){$else}strtofloat(reg.ReadString('Speedhack 3 speed')){$endif}
          else
            speedhackspeed3.speed:=1;

          if reg.ValueExists('Speedhack 3 disablewhenreleased') then
            speedhackspeed3.disablewhenreleased:=reg.ReadBool('Speedhack 3 disablewhenreleased');

          if reg.ValueExists('Speedhack 4 speed') then
            speedhackspeed4.speed:={$ifdef windows}reg.ReadFloat('Speedhack 4 speed'){$else}strtofloat(reg.ReadString('Speedhack 4 speed')){$endif}
          else
            speedhackspeed4.speed:=1;

          if reg.ValueExists('Speedhack 4 disablewhenreleased') then
            speedhackspeed4.disablewhenreleased:=reg.ReadBool('Speedhack 4 disablewhenreleased');


          if reg.ValueExists('Speedhack 5 speed') then
            speedhackspeed5.speed:={$ifdef windows}reg.ReadFloat('Speedhack 5 speed'){$else}strtofloat(reg.ReadString('Speedhack 4 speed')){$endif}
          else
            speedhackspeed5.speed:=1;

          if reg.ValueExists('Speedhack 5 disablewhenreleased') then
            speedhackspeed5.disablewhenreleased:=reg.ReadBool('Speedhack 5 disablewhenreleased');



          if reg.ValueExists('Increase Speedhack delta') then
            speedupdelta:={$ifdef windows}reg.ReadFloat('Increase Speedhack delta'){$else}strtofloat(reg.ReadString('Increase Speedhack delta')){$endif}
          else
            speedupdelta:=1;

          if reg.ValueExists('Decrease Speedhack delta') then
            slowdowndelta:={$ifdef windows}reg.ReadFloat('Decrease Speedhack delta'){$else}strtofloat(reg.ReadString('Decrease Speedhack delta')){$endif}
          else
            slowdowndelta:=1;



          SuspendHotkeyHandler;




          if reg.ValueExists('Attach to foregroundprocess Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Attach to foregroundprocess Hotkey',temphotkeylist[0][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Attach to foregroundprocess Hotkey')),pchar(@temphotkeylist[0][0]),10);
            {$endif}


          if reg.ValueExists('Show '+strCheatEngine+' Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Show '+strCheatEngine+' Hotkey',temphotkeylist[1][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Show '+strCheatEngine+' Hotkey')),pchar(@temphotkeylist[1][0]),10);
            {$endif}

          if reg.ValueExists('Pause process Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Pause process Hotkey',temphotkeylist[2][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Pause process Hotkey')),pchar(@temphotkeylist[2][0]),10);
            {$endif}



          if reg.ValueExists('Toggle speedhack Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Toggle speedhack Hotkey',temphotkeylist[3][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Toggle speedhack Hotkey')),pchar(@temphotkeylist[3][0]),10);
            {$endif}

          if reg.ValueExists('Set Speedhack speed 1 Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Set Speedhack speed 1 Hotkey',temphotkeylist[4][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Set Speedhack speed 1 Hotkey')),pchar(@temphotkeylist[4][0]),10);
            {$endif}

          speedhackspeed1.keycombo:=temphotkeylist[4];

          if reg.ValueExists('Set Speedhack speed 2 Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Set Speedhack speed 2 Hotkey',temphotkeylist[5][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Set Speedhack speed 2 Hotkey')),pchar(@temphotkeylist[5][0]),10);
            {$endif}

          speedhackspeed2.keycombo:=temphotkeylist[5];

          if reg.ValueExists('Set Speedhack speed 3 Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Set Speedhack speed 3 Hotkey',temphotkeylist[6][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Set Speedhack speed 3 Hotkey')),pchar(@temphotkeylist[6][0]),10);
            {$endif}

          speedhackspeed3.keycombo:=temphotkeylist[6];

          if reg.ValueExists('Set Speedhack speed 4 Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Set Speedhack speed 4 Hotkey',temphotkeylist[7][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Set Speedhack speed 4 Hotkey')),pchar(@temphotkeylist[7][0]),10);
            {$endif}

          speedhackspeed4.keycombo:=temphotkeylist[7];

          if reg.ValueExists('Set Speedhack speed 5 Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Set Speedhack speed 5 Hotkey',temphotkeylist[8][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Set Speedhack speed 5 Hotkey')),pchar(@temphotkeylist[8][0]),10);
            {$endif}

          speedhackspeed5.keycombo:=temphotkeylist[8];

          if reg.ValueExists('Increase Speedhack speed') then
            {$ifdef windows}
            reg.ReadBinaryData('Increase Speedhack speed',temphotkeylist[9][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Increase Speedhack speed')),pchar(@temphotkeylist[9][0]),10);
            {$endif}

          if reg.ValueExists('Decrease Speedhack speed') then
            {$ifdef windows}
            reg.ReadBinaryData('Decrease Speedhack speed',temphotkeylist[10][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Decrease Speedhack speed')),pchar(@temphotkeylist[10][0]),10);
            {$endif}

          if reg.ValueExists('Binary Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Binary Hotkey',temphotkeylist[11][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Binary Hotkey')),pchar(@temphotkeylist[11][0]),10);
            {$endif}

          if reg.ValueExists('Byte Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Byte Hotkey',temphotkeylist[12][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Byte Hotkey')),pchar(@temphotkeylist[12][0]),10);
            {$endif}

          if reg.ValueExists('2 Bytes Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('2 Bytes Hotkey',temphotkeylist[13][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('2 Bytes Hotkey')),pchar(@temphotkeylist[13][0]),10);
            {$endif}

          if reg.ValueExists('4 Bytes Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('4 Bytes Hotkey',temphotkeylist[14][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('4 Bytes Hotkey')),pchar(@temphotkeylist[14][0]),10);
            {$endif}

          if reg.ValueExists('8 Bytes Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('8 Bytes Hotkey',temphotkeylist[15][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('8 Bytes Hotkey')),pchar(@temphotkeylist[15][0]),10);
            {$endif}

          if reg.ValueExists('Float Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Float Hotkey',temphotkeylist[16][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Float Hotkey')),pchar(@temphotkeylist[16][0]),10);
            {$endif}

          if reg.ValueExists('Double Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Double Hotkey',temphotkeylist[17][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Double Hotkey')),pchar(@temphotkeylist[17][0]),10);
            {$endif}

          if reg.ValueExists('Text Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Text Hotkey',temphotkeylist[18][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Text Hotkey')),pchar(@temphotkeylist[18][0]),10);
            {$endif}

          if reg.ValueExists('Array of Byte Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Array of Byte Hotkey',temphotkeylist[19][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Array of Byte Hotkey')),pchar(@temphotkeylist[19][0]),10);
            {$endif}

          if reg.ValueExists('New Scan Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('New Scan Hotkey',temphotkeylist[20][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('New Scan Hotkey')),pchar(@temphotkeylist[20][0]),10);
            {$endif}

          if reg.ValueExists('New Scan-Exact Value') then
            {$ifdef windows}
            reg.ReadBinaryData('New Scan-Exact Value',temphotkeylist[21][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('New Scan-Exact Value')),pchar(@temphotkeylist[21][0]),10);
            {$endif}

          if reg.ValueExists('Unknown Initial Value Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Unknown Initial Value Hotkey',temphotkeylist[22][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Unknown Initial Value Hotkey')),pchar(@temphotkeylist[22][0]),10);
            {$endif}

          if reg.ValueExists('Next Scan-Exact Value') then
            {$ifdef windows}
            reg.ReadBinaryData('Next Scan-Exact Value',temphotkeylist[23][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Next Scan-Exact Value')),pchar(@temphotkeylist[23][0]),10);
            {$endif}

          if reg.ValueExists('Increased Value Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Increased Value Hotkey',temphotkeylist[24][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Increased Value Hotkey')),pchar(@temphotkeylist[24][0]),10);
            {$endif}

          if reg.ValueExists('Decreased Value Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Decreased Value Hotkey',temphotkeylist[25][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Decreased Value Hotkey')),pchar(@temphotkeylist[25][0]),10);
            {$endif}

          if reg.ValueExists('Changed Value Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Changed Value Hotkey',temphotkeylist[26][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Changed Value Hotkey')),pchar(@temphotkeylist[26][0]),10);
            {$endif}

          if reg.ValueExists('Unchanged Value Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Unchanged Value Hotkey',temphotkeylist[27][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Unchanged Value Hotkey')),pchar(@temphotkeylist[27][0]),10);
            {$endif}

          if reg.ValueExists('Same as first scan Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Same as first scan Hotkey',temphotkeylist[28][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Same as first scan Hotkey')),pchar(@temphotkeylist[28][0]),10);
            {$endif}

          if reg.ValueExists('Undo Last scan Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Undo Last scan Hotkey',temphotkeylist[29][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Undo Last scan Hotkey')),pchar(@temphotkeylist[29][0]),10);
            {$endif}

          if reg.ValueExists('Cancel scan Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Cancel scan Hotkey',temphotkeylist[30][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Cancel scan Hotkey')),pchar(@temphotkeylist[30][0]),10);
            {$endif}

          if reg.ValueExists('Debug->Run Hotkey') then
            {$ifdef windows}
            reg.ReadBinaryData('Debug->Run Hotkey',temphotkeylist[31][0],10);
            {$else}
            HexToBin(pchar(reg.ReadString('Debug->Run Hotkey')),pchar(@temphotkeylist[31][0]),10);
            {$endif}

          //fill the hotkeylist

          if hotkeythread<>nil then
          begin
            for i:=0 to cehotkeycount-1 do
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
          end;

          if temphotkeylist[2][0]<>0 then
            advancedoptions.pausehotkeystring:='('+ConvertKeyComboToString(temphotkeylist[2])+')'
          else
            advancedoptions.pausehotkeystring:=' ('+rsNoHotkey+')';



          if temphotkeylist[3][0]<>0 then
            mainform.cbSpeedhack.Hint:=rsEnableDisableSpeedhack+' ('+
              ConvertKeyComboToString(temphotkeylist[3])+')'
          else
            mainform.cbSpeedhack.Hint:=rsEnableDisableSpeedhack+rsM2NoHotkey;


          ResumeHotkeyHandler;

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

          if reg.ValueExists('Skip PDB') then
            cbSkipPDB.checked:=reg.readBool('Skip PDB');

          skippdb:=cbSkipPDB.checked;

          if reg.valueExists('Use Intel PT For Debug') then
            cbUseIntelPT.checked:=reg.readBool('Use Intel PT For Debug');
          useintelptfordebug:=cbUseIntelPT.checked;

          if reg.valueExists('Hide IPT Capability') then
            cbHideIPTCapability.checked:=reg.readbool('Hide IPT Capability');
          hideiptcapability:=cbHideIPTCapability.checked;


          if reg.ValueExists('Log IPT buffers inside FindWhat results') then
            cbRecordIPTForFindWhatRoutines.checked:=reg.ReadBool('Log IPT buffers inside FindWhat results');
          inteliptlogfindwhatroutines:=cbRecordIPTForFindWhatRoutines.checked;

          if reg.ValueExists('Max IPT Size') then
             cbIPTTraceSize.ItemIndex:=reg.readinteger('Max IPT Size');
          maxiptconfigsize:=cbIPTTraceSize.ItemIndex;


          if reg.ValueExists('Replace incomplete opcodes with NOPS') then
            replacewithnops.checked:=reg.readBool('Replace incomplete opcodes with NOPS');

          if reg.ValueExists('Override existing bp''s') then
            cbOverrideExistingBPs.checked:=reg.readBool('Override existing bp''s');

          BPOverride:=cbOverrideExistingBPs.checked;



          if reg.ValueExists('Ask for replace with NOPS') then
            askforreplacewithnops.checked:=reg.readBool('Ask for replace with NOPS');

          if reg.ValueExists('Fastscan on by default') then
            cbFastscan.checked:=reg.ReadBool('Fastscan on by default');

          if reg.ValueExists('Use Anti-debugdetection') then
            checkbox1.Checked:=reg.readbool('Use Anti-debugdetection');

          if reg.ValueExists('Handle unhandled breakpoints') then
            cbhandlebreakpoints.Checked:=reg.ReadBool('Handle unhandled breakpoints');

          if cbFastscan.Checked then mainform.cbFastscan.Checked:=true else mainform.cbFastScan.Checked:=false;

          if reg.ValueExists('Simple copy/paste') then
            cbsimplecopypaste.checked:=reg.readbool('Simple copy/paste');

          if reg.ValueExists('Hardware breakpoints') then
            rbDebugAsBreakpoint.Checked:=reg.readbool('Hardware breakpoints');

          if reg.ValueExists('Software breakpoints') then
            rbInt3AsBreakpoint.checked:=reg.readbool('Software breakpoints')
          else
            rbDebugAsBreakpoint.checked:=true;

          if reg.ValueExists('Exception breakpoints') then
            rbPageExceptions.checked:=reg.ReadBool('Exception breakpoints');

          if rbDebugAsBreakpoint.checked then
            preferedBreakpointMethod:=bpmDebugRegister
          else
          if rbInt3AsBreakpoint.checked then
            preferedBreakpointMethod:=bpmInt3
          else
          if rbPageExceptions.checked then
            preferedBreakpointMethod:=bpmException;

          if reg.ValueExists('Update Foundaddress list') then
            cbUpdatefoundList.Checked:=reg.readbool('Update Foundaddress list');

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

          if reg.ValueExists('Pointer appending') then
            cbOldPointerAddMethod.checked:=reg.ReadBool('Pointer appending');


          if reg.ValueExists('skip PAGE_NOCACHE') then
            cbSkip_PAGE_NOCACHE.Checked:=reg.readbool('skip PAGE_NOCACHE');
            
          Skip_PAGE_NOCACHE:=cbSkip_PAGE_NOCACHE.Checked;

          if reg.ValueExists('skip PAGE_WRITECOMBINE') then
            cbSkip_PAGE_WRITECOMBINE.Checked:=reg.readbool('skip PAGE_WRITECOMBINE');

          Skip_PAGE_WRITECOMBINE:=cbSkip_PAGE_WRITECOMBINE.Checked;

          if reg.ValueExists('Save memoryregion scansettings') then
          begin
            cbSaveMemoryregionScanSettings.checked:=reg.readbool('Save memoryregion scansettings');
            if cbSaveMemoryregionScanSettings.checked then
            begin
              //load from the registry if available
              if reg.ValueExists('scan CopyOnWrite') then
              begin
                i:=reg.ReadInteger('scan CopyOnWrite');
                mainform.cbCopyOnWrite.State:=TCheckBoxState(i);
              end;

              if reg.ValueExists('scan Executable') then
              begin
                i:=reg.ReadInteger('scan Executable');
                mainform.cbExecutable.State:=TCheckBoxState(i);
              end;

              if reg.ValueExists('scan Writable') then
              begin
                i:=reg.ReadInteger('scan Writable');
                mainform.cbWritable.State:=TCheckBoxState(i);
              end;
            end;
          end;

          if reg.ValueExists('Pause when scanning on by default') then
            cbPauseWhenScanningOnByDefault.Checked:=reg.readbool('Pause when scanning on by default');

          MainForm.cbPauseWhileScanning.Checked:=cbPauseWhenScanningOnByDefault.checked;

          if reg.ValueExists('Repeat Delay') then
            Globals.repeatDelay:=reg.ReadInteger('Repeat Delay');

          formsettings.edtRepeatDelay.text:=inttostr(Globals.repeatDelay);


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





          if reg.ValueExists('Unrandomizer: default value') then
            unrandomizersettings.defaultreturn:=reg.ReadInteger('Unrandomizer: default value');

          if reg.ValueExists('Unrandomizer: incremental') then
            unrandomizersettings.incremental:=reg.ReadBool('Unrandomizer: incremental');

          {$ifdef windows}
          if reg.ValueExists('Use dbk32 QueryMemoryRegionEx') then
            cbKernelQueryMemoryRegion.checked:=reg.ReadBool('Use dbk32 QueryMemoryRegionEx');

          if reg.ValueExists('Use dbk32 ReadWriteProcessMemory') then
            cbKernelReadWriteProcessMemory.checked:=reg.ReadBool('Use dbk32 ReadWriteProcessMemory');

          if reg.ValueExists('Use dbk32 OpenProcess') then
            cbKernelOpenProcess.checked:=reg.ReadBool('Use dbk32 OpenProcess');

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
            
          if modulelist<>nil then freememandnil(modulelist);
          getmem(modulelist,modulelistsize);

          if reg.ValueExists('Module List') then
            reg.ReadBinaryData('Module List',ModuleList^,ModuleListSize);
          {$endif}


          if reg.ValueExists('Don''t use tempdir') then
            cbDontUseTempDir.checked:=reg.ReadBool('Don''t use tempdir');

          edtTempScanFolder.text:='';
          if reg.ValueExists('Scanfolder') then
            edtTempScanFolder.text:=reg.ReadString('Scanfolder');

          dontusetempdir:=cbDontusetempdir.checked;
          tempdiralternative:=edtTempScanFolder.text;

          if tempdiralternative='' then
            dontusetempdir:=false;


          if reg.ValueExists('Use Processwatcher') then
            cbProcessWatcher.checked:=reg.readBool('Use Processwatcher');

          {$ifdef windows}
          if reg.ValueExists('Use VEH Debugger') then
            cbUseVEHDebugger.Checked:=reg.ReadBool('Use VEH Debugger');

          if reg.ValueExists('VEH Real context on thread creation event') then
            cbVEHRealContextOnThreadCreation.checked:=reg.ReadBool('VEH Real context on thread creation event');

          VEHRealContextOnThreadCreation:=cbVEHRealContextOnThreadCreation.checked;


          if reg.ValueExists('Use Windows Debugger') then
            cbUseWindowsDebugger.checked:=reg.ReadBool('Use Windows Debugger');

          if reg.ValueExists('Use Kernel Debugger') then
            cbKdebug.checked:=reg.ReadBool('Use Kernel Debugger');

          if reg.ValueExists('Use DBVM Debugger') then
            cbUseDBVMDebugger.checked:=reg.ReadBool('Use DBVM Debugger');

          if reg.ValueExists('DBVMBP Trigger COW') then
            dbvmbp_options.TriggerCOW:=reg.ReadBool('DBVMBP Trigger COW')
          else
            dbvmbp_options.TriggerCOW:=true;

          if reg.ValueExists('DBVMBP This Process Only') then
            dbvmbp_options.TargetedProcessOnly:=reg.ReadBool('DBVMBP This Process Only');

          if reg.ValueExists('DBVMBP Kernelmode') then
            dbvmbp_options.KernelmodeBreaks:=reg.readBool('DBVMBP Kernelmode')
          else
            dbvmbp_options.KernelmodeBreaks:=true;



          cbDBVMDebugTriggerCOW.checked:=dbvmbp_options.TriggerCOW;
          cbDBVMDebugTargetedProcessOnly.checked:=dbvmbp_options.TargetedProcessOnly;
          cbDBVMDebugKernelmodeBreaks.checked:=dbvmbp_options.KernelmodeBreaks;

          {$endif}

          if reg.ValueExists('Wait After Gui Update') then
            waitafterguiupdate:=reg.ReadBool('Wait After Gui Update');
          cbWaitAfterGuiUpdate.checked:=waitafterguiupdate;

          {$ifdef darwin}
          cbUseMacDebugger.checked:=true;

          if reg.ValueExists('Use TaskLevel debugger') then
            useTaskLevelDebug:=reg.ReadBool('Use TaskLevel debugger');

          {$endif}


          if reg.ValueExists('Unexpected Breakpoint Behaviour') then
          begin
            case reg.ReadInteger('Unexpected Breakpoint Behaviour') of
              0:
              begin
                miUnexpectedBreakpointsIgnore.checked:=true;
                UnexpectedExceptionAction:=ueaIgnore;
              end;

              1:
              begin
                miUnexpectedBreakpointsBreak.checked:=true;
                UnexpectedExceptionAction:=ueaBreak;
              end;

              2:
              begin
                miUnexpectedBreakpointsBreakWhenInsideRegion.checked;
                UnexpectedExceptionAction:=ueaBreakIfInRegion;
              end;
            end;
          end;


          if reg.ValueExists('Use Global Debug Routines') then
            cbGlobalDebug.checked:=reg.ReadBool('Use Global Debug Routines');

          if reg.ValueExists('Show tools menu') then
            cbShowTools.Checked:=reg.ReadBool('Show tools menu');

          mainform.ools1.Visible:=cbShowTools.Checked;

          {$ifdef windows}

          if skipkernelapply=false then
          begin
            if cbKernelQueryMemoryRegion.checked then UseDBKQueryMemoryRegion else DontUseDBKQueryMemoryRegion;
            if cbKernelReadWriteProcessMemory.checked then UseDBKReadWriteMemory else DontUseDBKReadWriteMemory;
            if cbKernelOpenProcess.Checked then UseDBKOpenProcess else DontUseDBKOpenProcess;
          end;

          if cbProcessWatcher.Checked then
            if (frmProcessWatcher=nil) then //probably yes
              frmProcessWatcher:=tfrmprocesswatcher.Create(mainform); //start the process watcher

          {$endif}


          if reg.ValueExists('WriteLogging') then
            cbWriteLoggingOn.checked:=reg.ReadBool('WriteLogging');

          if reg.ValueExists('WriteLoggingSize') then
          begin
            edtWriteLogSize.text:=inttostr(reg.ReadInteger('WriteLoggingSize'));
            setMaxWriteLogSize(reg.ReadInteger('WriteLoggingSize'));
          end;

          logWrites:=cbWriteLoggingOn.checked;

          if reg.ValueExists('Never Change Protection') then
            cbNeverChangeProtection.checked:=reg.ReadBool('Never Change Protection');

          SkipVirtualProtectEx:=cbNeverChangeProtection.checked;

          if reg.ValueExists('Always Force Load') then
            cbAlwaysForceLoad.checked:=reg.ReadBool('Always Force Load');

          alwaysforceload:=cbAlwaysForceLoad.checked;


          if reg.ValueExists('Show Language MenuItem') then
            cbShowLanguageMenuItem.Checked:=reg.ReadBool('Show Language MenuItem');

          MainForm.miLanguages.Visible:=cbShowLanguageMenuItem.Checked and (lbLanguages.Count>1);

          if reg.ValueExists('DPI Aware') then
            cbDPIAware.Checked:=reg.readBool('DPI Aware');

          if reg.ValueExists('Override Default Font') then
            cbOverrideDefaultFont.Checked:=reg.readbool('Override Default Font');

          {$ifdef windows}
          {$ifdef privatebuild}
          if reg.ValueExists('DoNotOpenProcessHandles') then
            cbDontOpenHandle.Checked:=reg.readbool('DoNotOpenProcessHandles');

          DoNotOpenProcessHandles:=cbDontOpenHandle.checked;

          if reg.ValueExists('ProcessWatcherOpensHandles') then
            cbProcessWatcherOpensHandles.Checked:=reg.readbool('ProcessWatcherOpensHandles');

          ProcessWatcherOpensHandles:=cbProcessWatcherOpensHandles.checked;

          if reg.ValueExists('useapctoinjectdll') then
            cbInjectDLLWithAPC.Checked:=reg.readbool('useapctoinjectdll');

          useapctoinjectdll:=cbInjectDLLWithAPC.checked;
          {$else}
          DoNotOpenProcessHandles:=false;
          ProcessWatcherOpensHandles:=false;
          useapctoinjectdll:=false;
          {$endif}
          {$endif}

          if reg.ValueExists('Always Sign Table') then
            cbAlwaysSignTable.Checked:=reg.readBool('Always Sign Table');

          if reg.ValueExists('Always Ask For Password') then
            cbAlwaysAskForPassword.Checked:=reg.readBool('Always Ask For Password');


          if reg.ValueExists('collectgarbage passive') then
            cbLuaPassiveGarbageCollection.checked:=reg.ReadBool('collectgarbage passive');

          if reg.ValueExists('collectgarbage active') then
            cbLuaGarbageCollectAll.checked:=reg.ReadBool('collectgarbage active');

          if reg.ValueExists('collectgarbage timer') then
            edtLuaCollectTimer.text:=inttostr(reg.ReadInteger('collectgarbage timer'));

          if reg.ValueExists('collectgarbage only when bigger') then
            cbLuaOnlyCollectWhenLarger.checked:=reg.ReadBool('collectgarbage only when bigger');

          if reg.ValueExists('collectgarbage minsize') then
            edtLuaMinCollectSize.text:=inttostr(reg.ReadInteger('collectgarbage minsize'));

          if cbLuaGarbageCollectAll.checked then
          begin
            mainform.tLuaGCActive.interval:=strtoint(edtLuaCollectTimer.text)*1000;

            if cbLuaOnlyCollectWhenLarger.checked then
              luagc_MinSize:=strtoint(edtLuaMinCollectSize.text)
            else
              luagc_MinSize:=0;
          end;
          mainform.tLuaGCActive.enabled:=cbLuaGarbageCollectAll.checked;
          mainform.tLuaGCPassive.enabled:=cbLuaPassiveGarbageCollection.checked;


          if reg.ValueExists('use thread to freeze') then
          begin
            cbUseThreadForFreeze.checked:=reg.ReadBool('use thread to freeze');
            mainform.UseThreadToFreeze:=cbUseThreadForFreeze.checked;
          end;
        end;


      end;

      {$ifndef net}
      formsettings.lvtools.Clear;
      if Reg.OpenKey('\Software\'+strCheatEngine+'\Tools',false) then
      begin
        names:=TStringList.create;
        try
          reg.GetValueNames(names); //fpc 3.2.0 can raise an exception here as well
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
        except
        end;
        freeandnil(names);
      end;
      UpdateToolsMenu;




      if (not skipPlugins) and (Reg.OpenKey('\Software\'+strCheatEngine+'\Plugins'{$ifdef cpu64}+'64'{$else}+'32'{$endif},false)) then
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


        finally
          names.Free;
        end;
      end;

      pluginhandler.FillCheckListBox(formsettings.clbPlugins);
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
  MemoryBrowser.Kerneltools1.Enabled:={$ifdef windows}DBKLoaded or isRunningDBVM{$else}false{$endif};
  MemoryBrowser.miCR3Switcher.visible:=MemoryBrowser.Kerneltools1.Enabled;
  {$endif}



  if mainform.autoattachlist<>nil then
  begin
    mainform.autoattachlist.Delimiter:=';';
    mainform.autoattachlist.DelimitedText:=formsettings.EditAutoAttach.Text;
  end;


  if formsettings.cbShowMainMenu.Checked then
    mainform.Menu:=mainform.MainMenu1
  else
    mainform.Menu:=nil;

end;

procedure initcetitle;
begin
  CEnorm:=cename+BETA;  //.';

{$ifdef XDEBUG}
  CEnorm:=CENorm+' Debug Build';
{$endif}
{$ifdef darwin}
  CEnorm:=CENorm+' MacOS version';
{$endif}

  Application.Title:=CENorm;


  CERegion:=cenorm+' - '+rsPleaseWait;
  CESearch:=CERegion;
  CERegionSearch:= CERegion;
  CEWait:= ceregion;
  {$ifdef darwin}
  {$ifdef CPUX86_64}
  if MacIsArm64 then
    CENorm:=CENorm+' on Rosetta';
  {$endif}
  {$endif}
  mainform.Caption:=CENorm;
end;

initialization
  OutputDebugString('MainUnit2');

end.
















