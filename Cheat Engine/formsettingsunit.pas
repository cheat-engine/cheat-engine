unit formsettingsunit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,registry, Menus,ComCtrls,cefuncproc,ExtCtrls,tlhelp32,CheckLst
  {$ifndef net}
  ,plugin,newkernelhandler,debugger,hotkeyhandler, frameHotkeyConfigUnit;
  {$else}
  ,netapis;

  {$endif}

type Tpathspecifier=class(TObject)
  public
    path: string;
end;

type
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
    cbOldSpeedhack: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    Label15: TLabel;
    Label21: TLabel;
    checkThread: TCheckBox;
    combothreadpriority: TComboBox;
    cbFastscan: TCheckBox;
    cbSkip_PAGE_NOCACHE: TCheckBox;
    cbLowMemoryUsage: TCheckBox;
    cbMemImage: TCheckBox;
    cbMemMapped: TCheckBox;
    cbMemPrivate: TCheckBox;
    cbEnableHyperscanWhenPossible: TCheckBox;
    EditBufsize: TEdit;
    FileAssocs: TTabSheet;
    Plugins: TTabSheet;
    CodeFinder: TTabSheet;
    Assembler: TTabSheet;
    Label10: TLabel;
    cbGH: TCheckBox;
    cbCT3: TCheckBox;
    cbCT2: TCheckBox;
    cbCET: TCheckBox;
    cbCT: TCheckBox;
    Label22: TLabel;
    Button4: TButton;
    Button5: TButton;
    clbPlugins: TCheckListBox;
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
    CheckBox2: TCheckBox;
    rbDebugAsBreakpoint: TRadioButton;
    rbInt3AsBreakpoint: TRadioButton;
    cbBreakOnAttach: TCheckBox;
    Extra: TTabSheet;
    TauntOldOsUser: TLabel;
    GroupBox3: TGroupBox;
    cbKernelQueryMemoryRegion: TCheckBox;
    cbKernelReadWriteProcessMemory: TCheckBox;
    cbKernelOpenProcess: TCheckBox;
    Panel1: TPanel;
    Label25: TLabel;
    cbStealth: TCheckBox;
    cbProtectMe: TCheckBox;
    btnMoreStealth: TButton;
    cbUndoMemoryChanges: TCheckBox;
    cbForceUndo: TCheckBox;
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
    procedure Button1Click(Sender: TObject);
    procedure checkThreadClick(Sender: TObject);
    procedure EditBufSizeKeyPress(Sender: TObject; var Key: Char);
    procedure Default1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbShowDisassemblerClick(Sender: TObject);
    procedure replacewithnopsClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure cbUpdatefoundListClick(Sender: TObject);
    procedure AboutLabelClick(Sender: TObject);
    procedure cbHideAllWindowsClick(Sender: TObject);
    procedure btnExcludeProcessesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbKernelQueryMemoryRegionClick(Sender: TObject);
    procedure cbUndoMemoryChangesClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbProtectMeClick(Sender: TObject);
    procedure cbKdebugClick(Sender: TObject);
    procedure cbProcessWatcherClick(Sender: TObject);
    procedure btnMoreStealthClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure tvMenuSelectionChange(Sender: TObject; Node: TTreeNode);
    procedure Panel6Resize(Sender: TObject);
    procedure cbProcessIconsClick(Sender: TObject);
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
Mainunit2,
frmExcludeHideUnit,
MemoryBrowserFormUnit,
ModuleSafetyUnit,
frmProcessWatcherUnit,
ConfigUnrandomizerFrm;
{$endif}


{$R *.DFM}


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
  procedure Associate(ext: string);
  var  reg: TRegistry;
       preCE: string;
  begin
    reg := TRegistry.Create;
    reg.RootKey := HKEY_CLASSES_ROOT;
    reg.LazyWrite := false;

    reg.OpenKey(ext+'\shell\open\command',true);

    preCE:=reg.ReadString('preCE');
    if preCE='' then //no preCE item yet
      preCE:=reg.ReadString('');
    reg.writestring('preCE',preCE);

    reg.WriteString('',application.ExeName+' "%1"');
    reg.CloseKey;

    reg.OpenKey(ext+'\DefaultIcon', true);

    preCE:=reg.ReadString('preCE');
    if preCE='' then //no preCE item yet
      preCE:=reg.ReadString('');
    reg.writestring('preCE',preCE);

    reg.WriteString('',application.ExeName+',0');
    reg.CloseKey;

    reg.free;
  end;

  procedure UnAssociate(ext: string);
  var  reg: TRegistry;
       delete: boolean;
       preCE:string;
  begin
    reg := TRegistry.Create;
    reg.RootKey := HKEY_CLASSES_ROOT;
    reg.LazyWrite := false;

    delete:=false;

    try
      reg.OpenKey(ext+'\shell\open\command',false);
      preCE:=reg.ReadString('preCE');
      if preCE='' then delete:=true else
      begin
        reg.writestring('',preCE);
        reg.DeleteValue('preCE');
      end;

      reg.CloseKey;
    except
      delete:=true;
    end;

    try
      reg.OpenKey(ext+'\DefaultIcon', false);
      preCE:=reg.ReadString('preCE');
      if preCE='' then delete:=true else
      begin
        reg.writestring('',preCE);
        reg.DeleteValue('preCE');
      end;

      reg.closekey;
    except
      delete:=true;
    end;

    reg.DeleteKey(ext);
    reg.free;
  end;

begin
  if cbCT.checked then Associate('.CT') else unassociate('.CT');
  if cbCT2.checked then Associate('.CT2') else unassociate('.CT2');
  if cbCT3.checked then Associate('.CT3') else unassociate('.CT3');
  if cbCET.checked then Associate('.CET') else unassociate('.CET');
  if cbGH.checked then Associate('.GH') else unassociate('.GH');
end;


procedure TformSettings.Button1Click(Sender: TObject);
resourcestring
  strProcessWatcherWillPreventUnloader='Enabling the process watcher will prevent the unloader from working';
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
      reg.WriteBool('SeperateThread',checkThread.checked);
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
      reg.WriteBool('Use old speedhack',cbOldSpeedhack.checked);
      reg.WriteBool('Get process icons',cbProcessIcons.Checked);
      GetProcessIcons:=cbProcessIcons.Checked;

      reg.WriteBool('Only show processes with icon',cbProcessIconsOnly.Checked);
      ProcessesWithIconsOnly:=cbProcessIconsOnly.Checked;

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
      reg.writebool('Use Hyperscan if posible',cbEnableHyperscanWhenPossible.checked);

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
        reg.WriteBinaryData('Undo Last scan Hotkey',frameHotkeyConfig.newhotkeys[27][0],10);
        reg.WriteBinaryData('Cancel scan Hotkey',frameHotkeyConfig.newhotkeys[28][0],10);


        //apply these hotkey changes
        for i:=0 to 28 do
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

      reg.WriteBool('StealthOnExecute',cbStealth.Checked);
      reg.WriteBool('Protect CE',cbProtectMe.Checked);

      reg.WriteBool('Undo memory changes',cbUndoMemoryChanges.checked);
      reg.WriteBool('Undo memory changes:Force writable',cbForceUndo.checked);

      reg.WriteBool('Use dbk32 QueryMemoryRegionEx',cbKernelQueryMemoryRegion.checked);
      reg.WriteBool('Use dbk32 ReadWriteProcessMemory',cbKernelReadWriteProcessMemory.checked);
      reg.WriteBool('Use dbk32 OpenProcess',cbKernelOpenProcess.checked);

      reg.WriteBool('Use Processwatcher',cbProcessWatcher.checked);
      reg.WriteBool('Use Kernel Debugger',cbKdebug.checked);
      reg.WriteBool('Use Global Debug Routines',cbGlobalDebug.checked);
      if assigned(newkernelhandler.SetGlobalDebugState) then
        newkernelhandler.SetGlobalDebugState(cbGlobalDebug.checked);


      unrandomizersettings.defaultreturn:=strtoint(edtdefault.Text);
      unrandomizersettings.incremental:=cbincremental.Checked;
      reg.WriteInteger('Unrandomizer: default value',unrandomizersettings.defaultreturn);
      reg.WriteBool('Unrandomizer: incremental',unrandomizersettings.incremental);

    end;

{$ifndef net}
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

  modalresult:=mrok;
end;

procedure TformSettings.checkThreadClick(Sender: TObject);
begin
  if checkthread.checked then
  begin
    label3.Enabled:=true;
    combothreadpriority.enabled:=true;
  end
  else
  begin
    label3.Enabled:=false;
    combothreadpriority.enabled:=false;
  end;
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
    checkbox2.Enabled:=false;
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


  cbCT.checked:=CheckAssociation('.CT');
  cbCET.checked:=CheckAssociation('.CET');
  cbCT2.checked:=CheckAssociation('.CT2');
  cbCT3.checked:=CheckAssociation('.CT3');
  cbGH.checked:=CheckAssociation('.GH');

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

  cbOldSpeedhack.enabled:=not mainform.cbSpeedhack.Checked; //don't change it while  you're using the speedhack



  //fill hotkey list
  for i:=0 to length(hotkeythread.hotkeylist)-1 do
    if hotkeythread.hotkeylist[i].handler2 then
      framehotkeyconfig.newhotkeys[hotkeythread.hotkeylist[i].id]:=hotkeythread.hotkeylist[i].keys;

  framehotkeyconfig.newspeedhackspeed1:=speedhackspeed1;
  framehotkeyconfig.newspeedhackspeed1:=speedhackspeed2;
  framehotkeyconfig.newspeedhackspeed1:=speedhackspeed3;
  framehotkeyconfig.newspeedhackspeed1:=speedhackspeed4;
  framehotkeyconfig.newspeedhackspeed1:=speedhackspeed5;

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

procedure TformSettings.replacewithnopsClick(Sender: TObject);
begin
  askforreplacewithnops.Enabled:=replacewithnops.Checked;
end;

procedure TformSettings.CheckBox1Click(Sender: TObject);
begin
  checkbox2.Checked:=checkbox1.checked;
end;

procedure TformSettings.CheckBox2Click(Sender: TObject);
begin
  Checkbox1.Checked:=checkbox2.Checked;
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


  {$ifdef net}

  tabsheet7.TabVisible:=false;
  
  button3.visible:=false;
  cbHideAllWindows.visible:=false;
  btnExcludeProcesses.visible:=false;
  cbCenterOnPopup.visible:=false;

  cbHideAllWindows.Visible:=false;
  btnExcludeProcesses.visible:=false;
  cbCenterOnPopup.visible:=false;
  cbUpdatefoundList.Visible:=false;
  cbShowAdvanced.Visible:=false;
  cbShowUndo.Visible:=false;

  cbfastscan.Visible:=false;
  cbEnableHyperscanWhenPossible.Visible:=false;
  cbLowMemoryUsage.Visible:=false;

  label21.Visible:=false;
  cbMemprivate.Visible:=false;
  cbMemImage.Visible:=false;
  cbMemMapped.Visible:=false;

  label2.visible:=false;
  checkthread.Visible:=false;
  label3.Visible:=false;
  combothreadpriority.Visible:=false;

  checkbox1.visible:=false;
  checkbox2.visible:=false;

  cbshowdebugoptions.enableD:=false;
  rbDebugAsBreakpoint.Enabled:=false;
  rbInt3AsBreakpoint.Enabled:=false;
  cbBreakOnAttach.Enabled:=false;
  cbBreakOnAttach.Visible:=false;
  checkbox2.Visible:=false;
  checkbox1.Visible:=false;
  cbstealth.Visible:=false;
  cbprotectme.visible:=false;

  label25.Visible:=false;

  cbUndoMemoryChanges.Visible:=false;
  cbForceUndo.Visible:=false;
  panel1.Visible:=false;


  formsettings.Height:=formsettings.Height+24;
  pagecontrol1.Height:=pagecontrol1.Height+24;
  button1.top:=button1.top+24;
  button2.top:=button2.top+24;


  label16.Visible:=true;
  EditNetworkUpdateInterval.Visible:=true;
  Label17.Visible:=true;

  Label13.top:=label13.top+24;
  editUpdateInterval.top:=editUpdateInterval.top+24;
  label17.Top:=label17.Top+24;

  label14.top:=label14.top+24;
  editFreezeInterval.top:=editFreezeInterval.Top+24;
  label12.Top:=label12.top+24;
  cbprocesswatcher.Visible:=false;
  cbKdebug.Visible:=false;

  btnUnrandomizerconfig.Visible:=false;
  {$else}
  cbstealth.Visible:=fileexists(cheatenginedir+'stealth.dll');
  {$endif}


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
    TauntOldOsUser.Caption:='These functions can only be used on a 32-bit Operating System';
    TauntOldOsUser.Visible:=true;
  end;


  //make the tabs invisible
  for i:=0 to pcSetting.PageCount-1 do
    pcSetting.Pages[i].TabVisible:=false;

  pcSetting.ActivePageIndex:=0;
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

procedure TformSettings.cbUndoMemoryChangesClick(Sender: TObject);
begin
  if (cbUndoMemoryChanges.checked) and (not LoadingSettingsFromRegistry) then
    cbUndoMemoryChanges.checked:=Messagedlg('Are you sure the memory is clean from any alterations?',mtconfirmation,[mbyes,mbno],0)=mryes;

  cbforceundo.Enabled:=cbUndoMemoryChanges.checked;
  if not cbforceundo.enabled then cbforceundo.Checked:=false;
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

procedure TformSettings.cbProtectMeClick(Sender: TObject);
begin
  btnmorestealth.enabled:=cbProtectme.checked;

  if cbprotectme.Checked then
    startsystemcallretrieverifneeded;
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

procedure TformSettings.btnMoreStealthClick(Sender: TObject);
var i: integer;
    p:pbyte;
    p2: pchar;
begin
{$ifndef net}
  if frmModuleSafety=nil then
  begin
    //fill the list with all the modules
    frmModuleSafety:=TfrmModuleSafety.create(self);

    with frmModuleSafety do
    begin
      rbDenyList.checked:=DenyList;
      rbAllowList.Checked:=not Denylist;
      cbGlobalDeny.Checked:=DenyListGlobal;

      p:=modulelist;
      p2:=modulelist;
      i:=0;

      while i<modulelistsize do
      begin
        if p^=0 then
        begin
          ListBox1.Items.Add(p2);
          p2:=pointer(p);
        end;

        inc(p);
        inc(i);
      end;
    end;
  end;

  frmModuleSafety.showmodal;

  //now fill the tempmodulelist with the data of the list
  //get the size
  if tempmodulelist<>nil then freemem(tempmodulelist);
  tempmodulelistsize:=0;
  for i:=0 to frmmodulesafety.ListBox1.items.count-1 do
    inc(tempmodulelistsize,length(frmmodulesafety.ListBox1.Items[i])+1); //+1 because of the 0 terminator

  getmem(tempmodulelist,tempmodulelistsize);
  p:=pointer(tempmodulelist);

  for i:=0 to frmmodulesafety.ListBox1.items.count-1 do
  begin
    copymemory(p,@frmmodulesafety.ListBox1.items[i][1],length(frmmodulesafety.ListBox1.items[i])+1);
    inc(p,length(frmmodulesafety.ListBox1.items[i])+1);
  end;

  tempdenylist:=frmModuleSafety.rbDenyList.checked;
  tempDenyListGlobal:=frmModuleSafety.cbGlobalDeny.Checked;
{$endif}
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

end.


