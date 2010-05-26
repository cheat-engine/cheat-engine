unit formsettingsunit;

{$MODE Delphi}

interface

uses
  windows, LCLProc, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,registry, Menus,ComCtrls,CEFuncProc,ExtCtrls,{tlhelp32,}CheckLst,
  Buttons, LResources, frameHotkeyConfigUnit,

  kerneldebugger,plugin,NewKernelHandler,CEDebugger,hotkeyhandler, debugHelper,
  formhotkeyunit;


type Tpathspecifier=class(TObject)
  public
    path: string;
end;

type

  { TformSettings }

  TformSettings = class(TForm)
    defaultbuffer: TPopupMenu;
    Default1: TMenuItem;
    pnlConfig: TPanel;
    tvMenuSelection: TTreeView;
    pcSetting: TPageControl;
    GeneralSettings: TTabSheet;
    ScanSettings: TTabSheet;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    cbShowUndo: TCheckBox;
    cbShowAdvanced: TCheckBox;
    cbCenterOnPopup: TCheckBox;
    EditUpdateInterval: TEdit;
    EditFreezeInterval: TEdit;
    GroupBox1: TGroupBox;
    cbShowAsSigned: TCheckBox;
    cbBinariesAsDecimal: TCheckBox;
    cbsimplecopypaste: TCheckBox;
    EditNetworkUpdateInterval: TEdit;
    cbUpdatefoundList: TCheckBox;
    editUpdatefoundInterval: TEdit;
    cbHideAllWindows: TCheckBox;
    btnExcludeProcesses: TButton;
    EditAutoAttach: TEdit;
    cbAlwaysAutoAttach: TCheckBox;
    cbSaveWindowPos: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    Label15: TLabel;
    Label21: TLabel;
    combothreadpriority: TComboBox;
    cbFastscan: TCheckBox;
    cbSkip_PAGE_NOCACHE: TCheckBox;
    cbLowMemoryUsage: TCheckBox;
    cbMemImage: TCheckBox;
    cbMemMapped: TCheckBox;
    cbMemPrivate: TCheckBox;
    EditBufsize: TEdit;
    Plugins: TTabSheet;
    CodeFinder: TTabSheet;
    Assembler: TTabSheet;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    rbDebugRegisters: TRadioButton;
    rdWriteExceptions: TRadioButton;
    CheckBox1: TCheckBox;
    cbHandleBreakpoints: TCheckBox;
    cbShowDisassembler: TCheckBox;
    cbShowDebugoptions: TCheckBox;
    replacewithnops: TCheckBox;
    askforreplacewithnops: TCheckBox;
    rbDebugAsBreakpoint: TRadioButton;
    rbInt3AsBreakpoint: TRadioButton;
    cbBreakOnAttach: TCheckBox;
    Extra: TTabSheet;
    TauntOldOsUser: TLabel;
    GroupBox3: TGroupBox;
    cbKernelQueryMemoryRegion: TCheckBox;
    cbKernelReadWriteProcessMemory: TCheckBox;
    cbKernelOpenProcess: TCheckBox;
    cbProcessWatcher: TCheckBox;
    cbKdebug: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    cbGlobalDebug: TCheckBox;
    tsHotkeys: TTabSheet;
    OpenDialog1: TOpenDialog;
    Unrandomizer: TTabSheet;
    Label5: TLabel;
    edtDefault: TEdit;
    cbIncremental: TCheckBox;
    Panel6: TPanel;
    AboutLabel: TLabel;
    Button2: TButton;
    Button1: TButton;
    frameHotkeyConfig: TframeHotkeyConfig;
    cbProcessIcons: TCheckBox;
    cbProcessIconsOnly: TCheckBox;
    tsTools: TTabSheet;
    Panel2: TPanel;
    cbShowTools: TCheckBox;
    Panel3: TPanel;
    edtApplicationTool: TEdit;
    btnSetToolShortcut: TButton;
    Panel5: TPanel;
    Panel4: TPanel;
    btnToolNew: TButton;
    btnToolDelete: TButton;
    lvTools: TListView;
    lblApplicationTool: TLabel;
    lblShortcut: TLabel;
    lblShortcutText: TLabel;
    lblToolsName: TLabel;
    edtToolsName: TEdit;
    OpenButton: TSpeedButton;
    OpenDialog2: TOpenDialog;
    cbShowMainMenu: TCheckBox;
    cbOldPointerAddMethod: TCheckBox;
    Panel7: TPanel;
    Button5: TButton;
    Button4: TButton;
    Panel8: TPanel;
    Label22: TLabel;
    clbPlugins: TCheckListBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure checkThreadClick(Sender: TObject);
    procedure EditBufSizeKeyPress(Sender: TObject; var Key: Char);
    procedure Default1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbShowDisassemblerClick(Sender: TObject);
    procedure pcSettingChange(Sender: TObject);
    procedure replacewithnopsClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure cbUpdatefoundListClick(Sender: TObject);
    procedure AboutLabelClick(Sender: TObject);
    procedure cbHideAllWindowsClick(Sender: TObject);
    procedure btnExcludeProcessesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbKernelQueryMemoryRegionClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbKdebugClick(Sender: TObject);
    procedure cbProcessWatcherClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure tvMenuSelectionChange(Sender: TObject; Node: TTreeNode);
    procedure Panel6Resize(Sender: TObject);
    procedure cbProcessIconsClick(Sender: TObject);
    procedure tvMenuSelectionCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure btnSetToolShortcutClick(Sender: TObject);
    procedure cbShowToolsClick(Sender: TObject);
    procedure btnToolNewClick(Sender: TObject);
    procedure lvToolsClick(Sender: TObject);
    procedure edtApplicationToolChange(Sender: TObject);
    procedure btnToolDeleteClick(Sender: TObject);
    procedure edtToolsNameChange(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
  private
    { Private declarations }
    tempstatePopupHide:word;
    temppopupmodifier:dword;
    tempstatePause:word;
    tempPausemodifier:dword;
    tempstateSpeedhack:word;
    tempSpeedhackmodifier:dword;

    systemcallretrieverexecuted:boolean;
    systemcallretrieverhandle: thandle;

    tempmodulelist: pchar;
    tempmodulelistsize: integer;
    tempdenylist: boolean;
    tempdenylistglobal: boolean;

    deletedmodules: tstringlist;

    procedure SetAssociations;
    procedure startsystemcallretrieverifneeded; overload;
    procedure startsystemcallretrieverifneeded(why:string); overload;
  public
    { Public declarations }
    

    tempdonthidelist: array of string;
    temphideall: boolean;
    laststatePopupHide:word;
    lastpopupmodifier:dword;
    laststatePause:word;
    lastPausemodifier:dword;
    laststateSpeedhack:word;
    lastSpeedhackmodifier:dword;
    Loadingsettingsfromregistry: boolean;

    unrandomizersettings: record
                            defaultreturn: integer;
                            incremental: boolean;
                          end;
  end;

var
  formSettings: TformSettings;



  {$ifdef net}
  IsDebuggerPresentLocation: integer=0;
  {$endif}

implementation

uses
aboutunit,

{$ifdef net}
Unit2;
{$else}
MainUnit,
MainUnit2,
frmExcludeHideUnit, {
MemoryBrowserFormUnit,}
ModuleSafetyUnit,
frmProcessWatcherUnit,
ConfigUnrandomizerFrm;
{$endif}




procedure decimal(var key: char);
begin
  case key of
    chr(8)   : ;
    chr(16)  : ;
    '0'..'9' : ;
    else key:=chr(0);
  end;
end;


procedure TFormSettings.SetAssociations;
begin

end;


resourcestring
  strProcessWatcherWillPreventUnloader='Enabling the process watcher will prevent the unloader from working';
procedure TformSettings.Button1Click(Sender: TObject);
var processhandle2: Thandle;
    reg: TRegistry;
    bufsize: integer;
    newmax: integer;
    i,j,error: integer;
    ec:dword;
    found:boolean;

    networkupdateinterval,updateinterval,freezeinterval,FoundInterval: integer;

    dllpath: Tpathspecifier;
begin

{$ifndef net}

  if cbProcessWatcher.checked and (frmprocesswatcher=nil) then
  begin
    if messagedlg(strProcessWatcherWillPreventUnloader,mtwarning,[mbok,mbcancel],0)=mrcancel then
      exit
    else
    begin
      loaddbk32;
      frmprocesswatcher:=tfrmprocesswatcher.Create(mainform); //start the process watcher
    end;
  end;


  if systemcallretrieverexecuted then
  begin
    if GetExitCodeProcess(systemcallretrieverhandle,ec) then
    begin
      if ec=STILL_ACTIVE then
         if messagedlg('It''s best to wait till the systemcall retriever is done. Continue anyhow?',mtconfirmation,[mbyes,mbno],0)=mrno then exit;

      closehandle(systemcallretrieverhandle);
      systemcallretrieverexecuted:=false;
    end;
  end;

{$endif}

  if not ((cbMemPrivate.checked) or (cbMemImage.Checked) or (cbMemMapped.Checked)) then
    if messagedlg('You haven''t selected any memory type. This will result in Cheat Engine finding NO memory! Are you stupid?',mtWarning,[mbyes,mbno],0)<>mryes then exit;


  val(editUpdatefoundInterval.Text,foundinterval,error);
  if (error<>0) or (foundinterval<=0) then raise exception.Create(editUpdatefoundInterval.Text+' is not a valid interval');

  val(editupdateinterval.text,updateinterval,error);
  if (error<>0) or (updateinterval<=0) then raise exception.Create(editupdateinterval.text+' is not a valid interval');

  val(editnetworkupdateinterval.text,networkupdateinterval,error);
  if (error<>0) or (networkupdateinterval<=0) then raise exception.Create(editnetworkupdateinterval.text+' is not a valid interval');

  val(editfreezeinterval.text,freezeinterval,error);
  if (error<>0) or (updateinterval<=0) then raise exception.Create(editfreezeinterval.text+' is not a valid interval');


  try bufsize:=StrToInt(editbufsize.text); except bufsize:=1024; end;

  if bufsize=0 then raise exception.create('The scanbuffer size has to be greater than 0');

  {$ifdef net}
  mainform.buffersize:=bufsize*1024;
  {$else}
  buffersize:=bufsize*1024;
  {$endif}

  mainform.UndoScan.visible:={$ifdef net}false{$else}cbshowundo.checked{$endif};
  mainform.advancedbutton.Visible:=cbShowAdvanced.checked;


  //save to the registry
  reg:=Tregistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Cheat Engine',true) then
    begin
      //write the settings
      reg.WriteBool('Undo',cbshowundo.checked);
      reg.WriteBool('Advanced',cbShowAdvanced.checked);
      reg.WriteInteger('ScanThreadpriority',combothreadpriority.itemindex);
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


      reg.WriteInteger('Buffersize',bufsize);
      reg.WriteInteger('Maxresults',newmax);
      reg.WriteBool('UseDebugRegs',rbDebugRegisters.checked);
      reg.writebool('Show Disassembler',cbShowDisassembler.checked);
      reg.WriteBool('Center on popup',cbCenterOnPopup.checked);
      reg.WriteInteger('Update interval',updateinterval);
      reg.WriteInteger('Freeze interval',freezeinterval);
      reg.writebool('Show values as signed',cbShowAsSigned.checked);
      reg.writebool('Handle binarys as decimals',cbBinariesAsDecimal.checked);
      reg.WriteInteger('Network Update Interval',Networkupdateinterval);
      reg.WriteBool('Show debugger options',cbShowDebugOptions.checked);
      reg.WriteBool('Replace incomplete opcodes with NOPS',replacewithnops.checked);
      reg.WriteBool('Ask for replace with NOPS',askforreplacewithnops.checked);
      reg.WriteBool('Use Anti-debugdetection',checkbox1.checked);
      reg.WriteBool('Handle unhandled breakpoints',cbhandlebreakpoints.Checked);
      reg.WriteBool('Fastscan on by default',cbFastscan.checked);

      reg.WriteBool('Hardware breakpoints',rbDebugAsBreakpoint.checked);
      reg.WriteBool('Update Foundaddress list',cbUpdatefoundList.checked);
      reg.WriteInteger('Update Foundaddress list Interval',foundinterval);

      reg.WriteBool('Simple copy/paste',cbsimplecopypaste.checked);
      reg.WriteString('AutoAttach',EditAutoAttach.text);
      reg.writebool('Always AutoAttach', cbAlwaysAutoAttach.checked);


      {$ifndef net}
      mainform.UpdateFoundlisttimer.Interval:=foundinterval;
      {$endif}

      reg.WriteBool('Save window positions',cbSaveWindowPos.checked);
      reg.WriteBool('Show main menu',cbShowMainMenu.Checked);
      reg.WriteBool('Get process icons',cbProcessIcons.Checked);
      GetProcessIcons:=cbProcessIcons.Checked;

      reg.WriteBool('Only show processes with icon',cbProcessIconsOnly.Checked);
      ProcessesWithIconsOnly:=cbProcessIconsOnly.Checked;

      reg.WriteBool('Pointer appending', cbOldPointerAddMethod.checked);

      reg.writebool('skip PAGE_NOCACHE',cbSkip_PAGE_NOCACHE.Checked);
      reg.WriteBool('Break when debuging',cbBreakOnAttach.Checked);
      reg.WriteBool('Hide all windows',cbHideAllWindows.checked);
      reg.WriteBool('Really hide all windows',temphideall);


      //save donthidelist
      {$ifndef net}
      setlength(donthidelist,length(tempdonthidelist));
      for i:=0 to length(tempdonthidelist)-1 do
      begin
        donthidelist[i]:=tempdonthidelist[i];
        reg.writestring('Do not hide '+IntToStr(i),tempdonthidelist[i]);
      end;

      //end
      reg.writestring('Do not hide '+IntToStr(length(tempdonthidelist)),'');
      reg.WriteBool('MEM_PRIVATE',cbMemPrivate.checked);
      reg.WriteBool('MEM_IMAGE',cbMemImage.Checked);
      reg.WriteBool('MEM_MAPPED',cbMemMapped.Checked);
      onlyfront:=not temphideall;
      reg.WriteBool('Low Memory Usage',cbLowMemoryUsage.checked);

      //check the module list

      if frmModuleSafety<>nil then //modified
      begin
        freemem(modulelist);
        modulelist:=tempmodulelist;
        modulelistsize:=tempmodulelistsize;
        tempmodulelist:=nil;
        denylist:=tempdenylist;
        denylistglobal:=tempdenylistglobal;

        reg.WriteBinaryData('Module List',ModuleList^,modulelistsize);
        reg.writeInteger('modulelistsize',modulelistsize);
        reg.WriteBool('Global Denylist',DenyListGlobal);
        reg.WriteBool('ModuleList as Denylist',DenyList);
      end;


      try
        reg.WriteInteger('hotkey poll interval',strtoint(frameHotkeyConfig.edtKeypollInterval.text));
        hotkeyPollInterval:=strtoint(frameHotkeyConfig.edtKeypollInterval.text);
      except
        raise exception.Create('the value for the keypoll interval ('+frameHotkeyConfig.edtKeypollInterval.text+' is invalid');
      end;

      try
        reg.WriteInteger('Time between hotkeypress',strtoint(frameHotkeyConfig.edtHotkeyDelay.text));
        hotkeyIdletime:=strtoint(frameHotkeyConfig.edtHotkeyDelay.text);
      except
        raise exception.Create('the value for the wait between hotkey presses ('+frameHotkeyConfig.edtHotkeyDelay.text+' is invalid');
      end;




        //save the hotkeylist
        reg.WriteBinaryData('Show Cheat Engine Hotkey',frameHotkeyConfig.newhotkeys[0][0],10);
        reg.WriteBinaryData('Pause process Hotkey',frameHotkeyConfig.newhotkeys[1][0],10);
        reg.WriteBinaryData('Toggle speedhack Hotkey',frameHotkeyConfig.newhotkeys[2][0],10);


        reg.WriteFloat('Speedhack 1 speed',frameHotkeyConfig.newspeedhackspeed1.speed);
        reg.WriteInteger('Speedhack 1 sleeptime',frameHotkeyConfig.newspeedhackspeed1.sleeptime);
        reg.WriteFloat('Speedhack 2 speed',frameHotkeyConfig.newspeedhackspeed2.speed);
        reg.WriteInteger('Speedhack 2 sleeptime',frameHotkeyConfig.newspeedhackspeed2.sleeptime);
        reg.WriteFloat('Speedhack 3 speed',frameHotkeyConfig.newspeedhackspeed3.speed);
        reg.WriteInteger('Speedhack 3 sleeptime',frameHotkeyConfig.newspeedhackspeed3.sleeptime);
        reg.WriteFloat('Speedhack 4 speed',frameHotkeyConfig.newspeedhackspeed4.speed);
        reg.WriteInteger('Speedhack 4 sleeptime',frameHotkeyConfig.newspeedhackspeed4.sleeptime);
        reg.WriteFloat('Speedhack 5 speed',frameHotkeyConfig.newspeedhackspeed5.speed);
        reg.WriteInteger('Speedhack 5 sleeptime',frameHotkeyConfig.newspeedhackspeed5.sleeptime);

        mainunit2.speedhackspeed1:=frameHotkeyConfig.newspeedhackspeed1;
        mainunit2.speedhackspeed2:=frameHotkeyConfig.newspeedhackspeed2;
        mainunit2.speedhackspeed3:=frameHotkeyConfig.newspeedhackspeed3;
        mainunit2.speedhackspeed4:=frameHotkeyConfig.newspeedhackspeed4;
        mainunit2.speedhackspeed5:=frameHotkeyConfig.newspeedhackspeed5;

        reg.WriteBinaryData('Set Speedhack speed 1 Hotkey',frameHotkeyConfig.newhotkeys[3][0],10);
        reg.WriteBinaryData('Set Speedhack speed 2 Hotkey',frameHotkeyConfig.newhotkeys[4][0],10);
        reg.WriteBinaryData('Set Speedhack speed 3 Hotkey',frameHotkeyConfig.newhotkeys[5][0],10);
        reg.WriteBinaryData('Set Speedhack speed 4 Hotkey',frameHotkeyConfig.newhotkeys[6][0],10);
        reg.WriteBinaryData('Set Speedhack speed 5 Hotkey',frameHotkeyConfig.newhotkeys[7][0],10);

        reg.WriteBinaryData('Increase Speedhack speed',frameHotkeyConfig.newhotkeys[8][0],10);
        reg.WriteFloat('Increase Speedhack delta',frameHotkeyConfig.speedupdelta);

        reg.WriteBinaryData('Decrease Speedhack speed',frameHotkeyConfig.newhotkeys[9][0],10);
        reg.WriteFloat('Decrease Speedhack delta',frameHotkeyConfig.slowdowndelta);

        mainunit2.speedupdelta:=frameHotkeyConfig.speedupdelta;
        mainunit2.slowdowndelta:=frameHotkeyConfig.slowdowndelta;

        reg.WriteBinaryData('Binary Hotkey',frameHotkeyConfig.newhotkeys[10][0],10);
        reg.WriteBinaryData('Byte Hotkey',frameHotkeyConfig.newhotkeys[11][0],10);
        reg.WriteBinaryData('2 Bytes Hotkey',frameHotkeyConfig.newhotkeys[12][0],10);
        reg.WriteBinaryData('4 Bytes Hotkey',frameHotkeyConfig.newhotkeys[13][0],10);
        reg.WriteBinaryData('8 Bytes Hotkey',frameHotkeyConfig.newhotkeys[14][0],10);
        reg.WriteBinaryData('Float Hotkey',frameHotkeyConfig.newhotkeys[15][0],10);
        reg.WriteBinaryData('Double Hotkey',frameHotkeyConfig.newhotkeys[16][0],10);
        reg.WriteBinaryData('Text Hotkey',frameHotkeyConfig.newhotkeys[17][0],10);
        reg.WriteBinaryData('Array of Byte Hotkey',frameHotkeyConfig.newhotkeys[18][0],10);
        reg.WriteBinaryData('New Scan Hotkey',frameHotkeyConfig.newhotkeys[19][0],10);
        reg.WriteBinaryData('New Scan-Exact Value',frameHotkeyConfig.newhotkeys[20][0],10);
        reg.WriteBinaryData('Unknown Initial Value Hotkey',frameHotkeyConfig.newhotkeys[21][0],10);
        reg.WriteBinaryData('Next Scan-Exact Value',frameHotkeyConfig.newhotkeys[22][0],10);
        reg.WriteBinaryData('Increased Value Hotkey',frameHotkeyConfig.newhotkeys[23][0],10);
        reg.WriteBinaryData('Decreased Value Hotkey',frameHotkeyConfig.newhotkeys[24][0],10);
        reg.WriteBinaryData('Changed Value Hotkey',frameHotkeyConfig.newhotkeys[25][0],10);
        reg.WriteBinaryData('Unchanged Value Hotkey',frameHotkeyConfig.newhotkeys[26][0],10);
        reg.WriteBinaryData('Same as first scan Hotkey',frameHotkeyConfig.newhotkeys[27][0],10);

        reg.WriteBinaryData('Undo Last scan Hotkey',frameHotkeyConfig.newhotkeys[28][0],10);
        reg.WriteBinaryData('Cancel scan Hotkey',frameHotkeyConfig.newhotkeys[29][0],10);
        reg.WriteBinaryData('Debug->Run Hotkey',frameHotkeyConfig.newhotkeys[30][0],10);


        //apply these hotkey changes
        for i:=0 to 30 do
        begin
          found:=false;

          for j:=0 to length(hotkeythread.hotkeylist)-1 do
          begin
            if (hotkeythread.hotkeylist[j].id=i) and (hotkeythread.hotkeylist[j].handler2) then
            begin
              //found it
              hotkeythread.hotkeylist[j].keys:=frameHotkeyConfig.newhotkeys[i];
              found:=true;
              break;
            end;
          end;

          if not found then //add it
          begin
            j:=length(hotkeythread.hotkeylist);
            setlength(hotkeythread.hotkeylist,j+1);
            hotkeythread.hotkeylist[j].keys:=frameHotkeyConfig.newhotkeys[i];
            hotkeythread.hotkeylist[j].windowtonotify:=mainform.Handle;
            hotkeythread.hotkeylist[j].id:=i;
            hotkeythread.hotkeylist[j].handler2:=true;
          end;

          checkkeycombo(frameHotkeyConfig.newhotkeys[i]);
        end;
 


      {$endif}

      reg.WriteBool('Use dbk32 QueryMemoryRegionEx',cbKernelQueryMemoryRegion.checked);
      reg.WriteBool('Use dbk32 ReadWriteProcessMemory',cbKernelReadWriteProcessMemory.checked);
      reg.WriteBool('Use dbk32 OpenProcess',cbKernelOpenProcess.checked);

      reg.WriteBool('Use Processwatcher',cbProcessWatcher.checked);
      reg.WriteBool('Use Kernel Debugger',cbKdebug.checked);
      reg.WriteBool('Use Global Debug Routines',cbGlobalDebug.checked);

      if (cbGlobalDebug.checked) then
        kdebugger.GlobalDebug:=cbGlobalDebug.checked;



      unrandomizersettings.defaultreturn:=strtoint(edtdefault.Text);
      unrandomizersettings.incremental:=cbincremental.Checked;
      reg.WriteInteger('Unrandomizer: default value',unrandomizersettings.defaultreturn);
      reg.WriteBool('Unrandomizer: incremental',unrandomizersettings.incremental);

      reg.writebool('Show tools menu', cbShowTools.checked);
      mainform.ools1.Visible:=cbShowTools.checked;

    end;

{$ifndef net}
    //save the tools hotkeys
    reg.DeleteKey('\Software\Cheat Engine\Tools');
    if Reg.OpenKey('\Software\Cheat Engine\Tools',true) then
    begin
      for i:=0 to lvTools.Items.Count-1 do
      begin
        reg.WriteString(format('%.8x A',[i]),lvTools.Items[i].caption);
        reg.WriteString(format('%.8x B',[i]),lvTools.Items[i].subitems[0]);
        reg.WriteInteger(format('%.8x C',[i]),dword(lvTools.Items[i].data));
      end;
    end;
    UpdateToolsMenu;

    for i:=0 to deletedmodules.Count-1 do
    begin
      j:=pluginhandler.GetPluginID(deletedmodules[i]);
      if j<>-1 then
        pluginhandler.DisablePlugin(j);
    end;

    //save the plugins
    reg.DeleteKey('\Software\Cheat Engine\Plugins');
    if Reg.OpenKey('\Software\Cheat Engine\Plugins',true) then
    begin
      for i:=0 to clbplugins.Count-1 do
      begin
        dllpath:=Tpathspecifier(clbplugins.Items.Objects[i]);

        reg.WriteString(format('%.8x A',[i]),dllpath.path);
        reg.WriteBool(format('%.8x B',[i]),clbplugins.Checked[i]);
      end;
    end;

    for i:=0 to clbplugins.Count-1 do
    begin
      dllpath:=Tpathspecifier(clbplugins.Items.Objects[i]);
      j:=pluginhandler.GetPluginID(dllpath.path);

      if j=-1 then //not loaded yet
        j:=pluginhandler.LoadPlugin(dllpath.path);

      if clbplugins.Checked[i] then
      begin
        //at least load it if it is loadable

        pluginhandler.EnablePlugin(j);
      end
      else
        pluginhandler.DisablePlugin(j);
    end;
{$endif}



  finally
    reg.CloseKey;
    reg.free;
  end;

  SetAssociations;


  {$ifndef net}
  mainform.FreezeTimer.Interval:=freezeinterval;
  mainform.UpdateTimer.Interval:=updateinterval;
  {$else}
  mainform.FreezeTimer.Interval:=freezeinterval;
  mainform.UpdateTimer.Interval:=networkupdateinterval;
  {$endif}

  Skip_PAGE_NOCACHE:=cbSkip_PAGE_NOCACHE.Checked;

  {$ifndef net}
  Scan_MEM_PRIVATE:=cbMemPrivate.checked;
  Scan_MEM_IMAGE:=cbMemImage.Checked;
  Scan_MEM_MAPPED:=cbMemMapped.Checked;
  {$endif}

  laststatePopupHide:=tempstatepopuphide;
  lastpopupmodifier:=temppopupmodifier;
  laststatePause:=tempstatepause;
  lastPausemodifier:=temppausemodifier;
  laststateSpeedhack:=tempstatespeedhack;
  lastSpeedhackmodifier:=tempspeedhackmodifier;

  handleautoattachstring;

  if cbShowMainMenu.Checked then
    mainform.Menu:=mainform.MainMenu1
  else
    mainform.Menu:=nil;

  modalresult:=mrok;

end;

procedure TformSettings.Button2Click(Sender: TObject);
begin

end;

procedure TformSettings.checkThreadClick(Sender: TObject);
begin

end;

procedure TformSettings.EditBufSizeKeyPress(Sender: TObject;
  var Key: Char);
begin
  decimal(key);
end;

procedure TformSettings.Default1Click(Sender: TObject);
begin
  editbufsize.Text:='512';
end;

procedure TformSettings.FormShow(Sender: TObject);
  function CheckAssociation(ext: string):boolean;
  var  reg: TRegistry;
       temp: string;
  begin
    reg := TRegistry.Create;
    reg.RootKey := HKEY_CLASSES_ROOT;
    reg.LazyWrite := false;

    try
      reg.OpenKey(ext+'\shell\open\command',false);
    except
      result:=false;
      reg.free;
      exit;
    end;

    temp:=reg.ReadString('');
    if temp<>application.ExeName+' "%1"' then
    begin
      result:=false;
      reg.CloseKey;
      reg.free;
      exit;
    end;

    result:=true;
  end;
  var reg: TRegistry;
  i: integer;
begin

  tempstatepopuphide:=laststatePopupHide;
  temppopupmodifier:=lastpopupmodifier;
  tempstatepause:=laststatePause;
  temppausemodifier:=lastPausemodifier;
  tempstatespeedhack:=laststateSpeedhack;
  tempspeedhackmodifier:=lastSpeedhackmodifier;

  {$ifndef net}
  setlength(tempdonthidelist,length(donthidelist));
  for i:=0 to length(donthidelist)-1 do
    tempdonthidelist[i]:=donthidelist[i];
  {$endif net}


  if IsDebuggerPresentLocation=0 then
  begin
    checkbox1.Enabled:=false;
  end;

  {$ifndef net}
  if debuggerthread<>nil then
  begin
    rbDebugAsBreakpoint.Enabled:=false;
    rbInt3AsBreakpoint.Enabled:=false;
  end
  else
  begin
    rbDebugAsBreakpoint.Enabled:=true;
    rbInt3AsBreakpoint.Enabled:=true;
  end;
  {$endif}

  label1.Enabled:=not mainform.nextscanbutton.enabled;
  editbufsize.enabled:=not mainform.nextscanbutton.enabled;

  //load the settings from the register and apply them to this window


  {$ifndef net}
  if GetSystemType<3 then //not a supported os for hardware breakpoints
  begin
    rdWriteExceptions.Checked:=true;
    rbDebugRegisters.Enabled:=false;

    rbDebugAsBreakpoint.enabled:=false;
    rbInt3AsBreakpoint.enabled:=true;
    label6.Enabled:=false;
    label7.Enabled:=false;
  end;
  {$endif}

  //fill hotkey list
  for i:=0 to length(hotkeythread.hotkeylist)-1 do
    if hotkeythread.hotkeylist[i].handler2 then
      framehotkeyconfig.newhotkeys[hotkeythread.hotkeylist[i].id]:=hotkeythread.hotkeylist[i].keys;

  framehotkeyconfig.newspeedhackspeed1:=speedhackspeed1;
  framehotkeyconfig.newspeedhackspeed2:=speedhackspeed2;
  framehotkeyconfig.newspeedhackspeed3:=speedhackspeed3;
  framehotkeyconfig.newspeedhackspeed4:=speedhackspeed4;
  framehotkeyconfig.newspeedhackspeed5:=speedhackspeed5;

  framehotkeyconfig.speedupdelta:=speedupdelta;
  framehotkeyconfig.slowdowndelta:=slowdowndelta;

end;

procedure TformSettings.cbShowDisassemblerClick(Sender: TObject);
begin
  if cbshowdisassembler.Checked then
  begin
    cbShowDebugOptions.Enabled:=true;
    ReplaceWithNops.enabled:=true;

    askforreplacewithnops.Enabled:=replacewithnops.Checked;
  end
  else
  begin
    cbShowDebugOptions.Enabled:=false;
    ReplaceWithNops.enabled:=false;
    askforreplacewithnops.Enabled:=false;
  end;
end;

procedure TformSettings.pcSettingChange(Sender: TObject);
begin

end;

procedure TformSettings.replacewithnopsClick(Sender: TObject);
begin
  askforreplacewithnops.Enabled:=replacewithnops.Checked;
end;

procedure TformSettings.CheckBox1Click(Sender: TObject);
begin

end;

procedure TformSettings.CheckBox2Click(Sender: TObject);
begin

end;

procedure TformSettings.cbUpdatefoundListClick(Sender: TObject);
begin
  if cbUpdatefoundList.Checked then
  begin
    label18.Enabled:=true;
    editUpdatefoundInterval.Enabled:=true;
    Label19.Enabled:=true;
  end
  else
  begin
    label18.Enabled:=false;
    editUpdatefoundInterval.Enabled:=false;
    Label19.Enabled:=false;
  end;
end;

procedure TformSettings.AboutLabelClick(Sender: TObject);
begin

  with tabout.create(self) do
  begin
    showmodal;
    free;
  end;

end;

procedure TformSettings.cbHideAllWindowsClick(Sender: TObject);
begin
  btnExcludeProcesses.enabled:=cbHideallWindows.Checked;
end;

procedure TformSettings.btnExcludeProcessesClick(Sender: TObject);
begin
  {$ifndef net}

  with tfrmExcludeHide.create(self) do
  begin
    showmodal;
    free;
  end;
  {$endif}
end;

procedure TformSettings.FormCreate(Sender: TObject);
var i: integer;

begin

  aboutlabel.left:=aboutlabel.parent.ClientWidth-aboutlabel.width;
  aboutlabel.top:=aboutlabel.parent.clientheight-aboutlabel.height;

  //set the default popup
  laststatePopupHide:=vk_next;
  lastpopupmodifier:=MOD_CONTROL or MOD_ALT;

  laststatepause:=ord('P');
  lastpausemodifier:=MOD_CONTROL or MOD_ALT;

  laststateSpeedhack:=ord('S');
  laststateSpeedhack:=MOD_CONTROL or MOD_ALT;

  deletedmodules:=TStringlist.Create;


  //64-bit check
  if is64bitos then
  begin
    TauntOldOsUser.Visible:=true;
    TauntOldOsUser.Caption:='Please boot with unsigned drivers allowed(F8 during boot), or sign the driver yourself';

    cbKernelQueryMemoryRegion.enabled:=false; //currently no 64-bit paging support (yet)

    cbKdebug.Enabled:=isDBVMCapable;
    cbKdebug.Caption:=cbKdebug.Caption+' (Requires DBVM)';
    if not cbKdebug.Enabled then
      cbKdebug.checked:=false;
  end;


  //make the tabs invisible

  for i:=0 to pcSetting.PageCount-1 do
    pcSetting.Pages[i].TabVisible:=false;

  pcSetting.ActivePageIndex:=0;

  tvMenuSelection.FullExpand;
end;

procedure TformSettings.cbKernelQueryMemoryRegionClick(Sender: TObject);
begin
  if (cbkdebug.checked) or (cbKernelQueryMemoryRegion.Checked) or (cbKernelReadWriteProcessMemory.Checked) then
  begin
    cbKernelOpenProcess.Checked:=true;
    cbKernelOpenProcess.Enabled:=false;
  end
  else cbKernelOpenProcess.Enabled:=true;

end;

procedure TformSettings.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
{$ifndef net}
  deletedmodules.Clear;
{$endif}
end;

procedure TformSettings.startsystemcallretrieverifneeded;
begin
  startsystemcallretrieverifneeded('');
end;

procedure TformSettings.startsystemcallretrieverifneeded(why:string);
var
  proc_info: TProcessInformation;
  startinfo: TStartupInfo;
  ec: dword;
begin
  if systemcallretrieverexecuted then
  begin
    //check if it's still running
    if GetExitCodeProcess(systemcallretrieverhandle,ec) then
      if ec=STILL_ACTIVE then exit; //still running
  end;

  //start the systemsignalretriever if there's no callnumbers.dat
  if not fileexists(cheatenginedir+'kerneldata.dat') then
  begin
    if why<>'' then
      if messagedlg(why,mtconfirmation,[mbyes,mbno],0)<>mryes then exit;

    zeromemory(@proc_info,sizeof(proc_info));
    zeromemory(@startinfo,sizeof(startinfo));
    startinfo.cb:=sizeof(startinfo);

    if Createprocess(nil,'systemcallretriever.exe',nil,nil,false,NORMAL_PRIORITY_CLASS,nil,nil,startinfo,proc_info) then
    begin
      systemcallretrieverexecuted:=true;
      systemcallretrieverhandle:=proc_info.hProcess;
    end;
  end;

end;

procedure TformSettings.cbKdebugClick(Sender: TObject);
begin
  if (cbkdebug.checked) or (cbKernelQueryMemoryRegion.Checked) or (cbKernelReadWriteProcessMemory.Checked) then
  begin
    cbKernelOpenProcess.Checked:=true;
    cbKernelOpenProcess.Enabled:=false;
    cbGlobalDebug.enabled:=true;
  end
  else
  begin
    cbKernelOpenProcess.Enabled:=true;
    cbGlobalDebug.enabled:=false;
  end;

  if cbkdebug.Checked then
  begin
    cbKernelOpenProcess.enabled:=false;
    cbKernelOpenProcess.Checked:=true;
    cbProcessWatcher.Enabled:=false;
    cbProcesswatcher.Checked:=true;

    cbProcessWatcherClick(cbProcessWatcher);
  end else cbprocesswatcher.enableD:=true;
end;

procedure TformSettings.cbProcessWatcherClick(Sender: TObject);
var
  proc_info: TProcessInformation;
  startinfo: TStartupInfo;
begin
  if cbprocesswatcher.checked then
    startsystemcallretrieverifneeded('To get more detailed information about processes, like the processname, it''s recommended to run the kerneldataretriever program. Do you want to run it ?');
end;

procedure TformSettings.Button4Click(Sender: TObject);
var pluginname: string;
    fullpath: Tpathspecifier;
    i: integer;
    s: string;
begin
{$ifndef net}

  if opendialog1.Execute then
  begin
    s:=uppercase(ExtractFileName(opendialog1.FileName));
    for i:=0 to clbplugins.count-1 do
      if uppercase(extractfilename(Tpathspecifier(clbplugins.Items.Objects[i]).path))=s then
        raise exception.Create('This plugin is already loaded');

    pluginname:=pluginhandler.GetPluginName(opendialog1.FileName);
    fullpath:=Tpathspecifier.Create;
    fullpath.path:=opendialog1.filename;
    clbPlugins.Items.AddObject(extractfilename(opendialog1.FileName)+':'+pluginname,fullpath);
  end;
{$endif}
end;

procedure TformSettings.Button5Click(Sender: TObject);
var modulename: string;
begin
{$ifndef net}

  if clbplugins.ItemIndex<>-1 then
  begin
    modulename:=extractfilename(Tpathspecifier(clbplugins.items.Objects[clbplugins.ItemIndex]).path);
    deletedmodules.add(modulename);

    Tpathspecifier(clbPlugins.Items.Objects[clbplugins.ItemIndex]).Free;
    clbPlugins.Items.Delete(clbplugins.ItemIndex);
  end;
{$endif}
end;

procedure TformSettings.tvMenuSelectionChange(Sender: TObject;
  Node: TTreeNode);
begin
  if node.Level=0 then //main settings
  begin
    pcSetting.ActivePageIndex:=node.Index;
  end;

  if node.level=1 then
  begin
    if node.Parent.Index=0 then
    begin
      if node.Index=0 then //tools menu
      begin
        pcSetting.ActivePage:=tsTools;
      end;
    end;

  end;
end;

procedure TformSettings.Panel6Resize(Sender: TObject);
begin
  button1.Left:=button1.parent.ClientWidth div 2 - button1.Width - 10;
  button2.Left:=button2.parent.ClientWidth div 2 + 10;  
end;

procedure TformSettings.cbProcessIconsClick(Sender: TObject);
begin
  cbProcessIconsOnly.Enabled:=cbProcessIcons.Checked;
  if not cbProcessIcons.Checked then cbProcessIconsOnly.Checked:=false;
end;

procedure TformSettings.tvMenuSelectionCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  AllowCollapse:=false;
end;

procedure TformSettings.btnSetToolShortcutClick(Sender: TObject);
var x: tshortcut;
begin

  if lvtools.Selected=nil then exit;

  with TFormHotkey.Create(self) do
  begin
    if ShowModal=mrok then
    begin
      x:=key;

      if (modifier and MOD_ALT)>0 then
        x:=x or scAlt;

      if (modifier and MOD_CONTROL)>0 then
        x:=x or scCtrl;

      if (modifier and MOD_SHIFT)>0 then
        x:=x or scShift;

      lblShortcutText.caption:=ShortCutToText(x);
      lvtools.Selected.Data:=pointer(x);
      lvtools.Selected.SubItems[1]:=lblShortcutText.caption;
    end;

    free;
  end;
end;

procedure TformSettings.cbShowToolsClick(Sender: TObject);
begin
  lvTools.enabled:=cbShowTools.Checked;
  lblToolsName.enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);
  edtToolsName.enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);
  lblApplicationTool.enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);
  edtApplicationTool.enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);
  OpenButton.Enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);
  lblShortcut.enabled:= cbShowTools.Checked and (lvtools.Selected<>nil);
  lblShortcutText.enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);
  btnSetToolShortcut.enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);
  btnToolNew.enabled:=cbShowTools.Checked;
  btnToolDelete.Enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);

  if (lvtools.Selected<>nil) then
  begin
    edtToolsName.Text:=lvtools.Selected.Caption;
    edtApplicationTool.Text:=lvtools.Selected.SubItems[0];
    lblShortcutText.caption:=lvtools.Selected.SubItems[1];
  end;

end;

procedure TformSettings.btnToolNewClick(Sender: TObject);
var li:tlistitem;
begin
  li:=lvTools.Items.Add;
  li.Data:=nil;
  li.Caption:='No Name';
  li.SubItems.Add('');
  li.SubItems.Add('');
  li.Selected:=true;
  lvTools.OnClick(lvTools);


  edtToolsName.SetFocus;
  edtToolsName.SelectAll;
end;

procedure TformSettings.lvToolsClick(Sender: TObject);
begin
  lblToolsName.enabled:=lvtools.Selected<>nil;
  edtToolsName.enabled:=lvtools.Selected<>nil;
  lblApplicationTool.enabled:=lvtools.Selected<>nil;
  edtApplicationTool.enabled:=lvtools.Selected<>nil;
  lblShortcut.enabled:= lvtools.Selected<>nil;
  lblShortcutText.enabled:=lvtools.Selected<>nil;
  btnSetToolShortcut.enabled:=lvtools.Selected<>nil;
  btnToolDelete.Enabled:=lvtools.Selected<>nil;
  OpenButton.Enabled:=cbShowTools.Checked and (lvtools.Selected<>nil);

  if lvtools.Selected<>nil then
  begin
    edtToolsName.Text:=lvtools.Selected.Caption;
    edtApplicationTool.Text:=lvtools.Selected.SubItems[0];
    lblShortcutText.caption:=lvtools.Selected.SubItems[1];
    edtToolsName.SetFocus;
  end;
end;

procedure TformSettings.edtApplicationToolChange(Sender: TObject);
begin
  lvtools.Selected.subitems[0]:=edtApplicationTool.text;
end;

procedure TformSettings.btnToolDeleteClick(Sender: TObject);
begin
  if lvTools.Selected<>nil then
    lvTools.Selected.Delete;

  lvTools.OnClick(lvTools); //refresh
end;

procedure TformSettings.edtToolsNameChange(Sender: TObject);
begin
  lvtools.Selected.Caption:=edtToolsName.text;
end;

procedure TformSettings.OpenButtonClick(Sender: TObject);
begin
  if opendialog2.Execute then
    edtApplicationTool.Text:=opendialog2.FileName;
end;

initialization
  {$i formsettingsunit.lrs}

end.


