unit trainergenerator;

{$mode delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, ceguicomponents, lclintf, StdCtrls, EditBtn, ExtCtrls, ExtDlgs,
  ComCtrls, Buttons, Menus, ExtraTrainerComponents, CEFuncProc, HotkeyHandler,
  HotKeys, symbolhandler, luacaller, formdesignerunit, opensave, luafile,
  frmAdConfigUnit, cesupport, IconStuff, memoryrecordunit, frmSelectionlistunit,
  MainUnit2, lua, luahandler, commonTypeDefs, math, CECustomButton, betterControls;

type
  TTrainerForm=class(TCEForm)
    public
      defaultTrainer: boolean;
  end; //so it's uniquely identifyable

  { TfrmTrainerGenerator }
  TfrmTrainerGenerator = class(TForm)
    btnAddSounds: TButton;
    btnDelete: TButton;
    btnAddHotkey: TButton;
    btnDesignForm: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button5: TButton;
    cbActivateSound: TComboBox;
    cbCanResize: TCheckBox;
    cbConfigD3DHook: TButton;
    cbDeactivateSound: TComboBox;
    cbOutput: TComboBox;
    cbPlaySoundOnAction: TCheckBox;
    cbPlayXM: TCheckBox;
    cbPopupOnKeypress: TCheckBox;
    cbProtect: TCheckBox;
    cbStopPlaying: TCheckBox;
    cbSupportCheatEngine: TCheckBox;
    cbUseD3DHook: TCheckBox;
    comboProcesslist: TComboBox;
    CTSaveDialog: TSaveDialog;
    edtCaption: TEdit;
    edtFreezeInterval: TEdit;
    edtPopupHotkey: TEdit;
    fnXM: TFileNameEdit;
    GroupBox2: TGroupBox;
    tgImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblActivateSound: TLabel;
    lblDeactivateSound: TLabel;
    lvCheats: TListView;
    mAbout: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miEditHotkey: TMenuItem;
    odWave: TOpenDialog;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    PopupMenu1: TPopupMenu;
    CETRAINERSaveDialog: TSaveDialog;
    EXESaveDialog: TSaveDialog;
    PopupMenu2: TPopupMenu;
    rbStopWhenAttached: TRadioButton;
    rbStopWhenFocusLost: TRadioButton;
    sbPlayActivate: TSpeedButton;
    sbPlayDeactivate: TSpeedButton;
    sbPlayStopXM: TSpeedButton;
    spbDown: TSpeedButton;
    spbUp: TSpeedButton;
    procedure btnDeleteClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure btnAddSoundsClick(Sender: TObject);
    procedure cbPlaySoundOnActionChange(Sender: TObject);
    procedure cbConfigD3DHookClick(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure btnDesignFormClick(Sender: TObject);
    procedure btnAddHotkeyClick(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure cbCanResizeChange(Sender: TObject);
    procedure cbOutputChange(Sender: TObject);
    procedure cbOutputSelect(Sender: TObject);
    procedure cbPlayXMChange(Sender: TObject);
    procedure cbStopPlayingChange(Sender: TObject);
    procedure cbSupportCheatEngineChange(Sender: TObject);
    procedure cbUseD3DHookChange(Sender: TObject);
    procedure edtCaptionChange(Sender: TObject);
    procedure edtPopupHotkeyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FileNameEdit1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvCheatsDblClick(Sender: TObject);
    procedure lvCheatsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure miEditHotkeyClick(Sender: TObject);
    procedure Panel2Resize(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
    procedure sbPlayActivateClick(Sender: TObject);
    procedure sbPlayDeactivateClick(Sender: TObject);
    procedure sbPlayStopXMClick(Sender: TObject);
    procedure spbDownClick(Sender: TObject);
    procedure spbUpClick(Sender: TObject);
  private
    { private declarations }
    popupkeys: TKeycombo;
    restoretimer: ttimer;
    adconfig: TfrmAdConfig;

    playbitmap: TBitmap;
    stopbitmap: TBitmap;
    shown: boolean;

    procedure editHotkey(m: Tmemoryrecord; hotkey: TMemoryrecordhotkey);
    procedure AddHotkey(hk: TMemoryrecordHotkey);
    procedure buildcheatlist;
    procedure fillHotkeyList;
    procedure FillSound;
    procedure generateScript;
    procedure generateScript2;
    procedure RestoreSupportCE(sender: tobject);

    procedure RefreshHotkeyItem(li: TListitem);
  public
    trainerform: TTrainerForm;
    extrapanel: TCEPanel;
    //cheatpanel: TCEPanel;
    aboutbutton: TCECustomButton;
    image: TCEImage;
    closebutton: TCECustomButton;
    seperator: TCESplitter;

    hotkeylabel, descriptionlabel: tcelabel;
    { public declarations }

    canceled: boolean;
  end;

procedure FillSoundList(list: Tstrings);

var
  frmTrainerGenerator: TfrmTrainerGenerator;

implementation

uses mainunit, frmD3DTrainerGeneratorOptionsunit, xmplayer_server,
  ProcessHandlerUnit, ProcessList, DPIHelper;

{ TfrmTrainerGenerator }
resourcestring
  rsDeActive = '(De)active';
  rsUnFreeze = '(Un)Freeze';
  rsSet = 'Set';
  rsIncrease = 'Increase';
  rsDecrease = 'Decrease';
  rsDoSomethingWith = 'Do something with';
  rsButAllowIncrease = 'but allow increase';
  rsButAllowDecrease = 'but allow decrease';
  rsTo = 'to';
  rsBy = 'by';
  rsOnCloseWarning = 'This form had an onClose event. Good thing this was only a stub, else '+strCheatEngine+' would have terminated';
  rsAlreadyATrainerFormDefined =
      'There is already a trainer form defined. '
    +'Continuing will erase the current trainerscript and cheats in the '
    +'trainer and replace them with the current hotkeys defined in your '
    +'current cheat table (Layout and images will remain unchanged). Continue ?';
  rsNoCheatPanel = 'The current trainer form does not have a panel '
    +'named ''CHEATPANEL'' so can not be reused by the automated trainer '
    +'generator.%sDo you want to start from scratch? (If you want to create a '
    +'trainer from your current script you can just save your table as .EXE '
    +'instead of using the automated trainer generator)';
  rsAreYouSure = 'Are you sure?';
  rsInvalidTrainerScript = 'The current lua script only has a half '
    +'TRAINERGENERATORSTART/TRAINERGENERATORSTOP block. Please fix this '
    +'first (Removing is the easiest option)';
  rsTipYouDontHaveToUseTheTrainerGeneratorIfYouDontWantTo = 'Tip: You don''t '
    +'have to use the trainer generator if you don''t want to. You can just '
    +'save your table as .EXE or CETRAINER';
  rsGoBackToGeneratedDesigner = 'Go back to generated designer';
  rsDesignUserinterfaceManually = 'Design userinterface manually';
  rsCheatEntries = 'Cheat Entries';
  rsSelectTheCheatEntryYouWantToSetTheHotkeyFor = 'Select the cheat entry you '
    +'want to set the hotkey for';
  rsYouNeedACheatTableWithCheatEntries = 'You need a cheat table with cheat '
    +'entries';
  rsDonTSupportCheatEngineOrYourself = 'Don''t support '+strCheatEngine+' (or '
    +'yourself)';
  rsThankYou = 'Thank you! :)';
  rsAaaaw = 'aaaaw :(';
  rsAutogenwarningPart1 = 'This is autogenerated code. Changing code in this block will';
  rsAutoGenWarningPart2 = 'get erased and rewritten if you regenerate the trainer code';
  rsPleaseSelectTypeInaProcessname = 'Please select/type in a processname';
  rsAbout = 'About';
  rsHotkey = 'Hotkey';
  rsEffect = 'Effect';
  rsClose = 'Close';

procedure TfrmTrainerGenerator.RefreshHotkeyItem(li: TListitem);
var hk: TMemoryrecordhotkey;
  mr: TMemoryrecord;
begin
  hk:=TMemoryrecordhotkey(li.data);
  mr:=hk.owner;
  li.caption:=ConvertKeyComboToString(hk.keys);
  li.SubItems.clear;

  if hk.description='' then
  begin
    //try to guess that it does
    case hk.action of
      mrhToggleActivation: li.SubItems.Add(rsDeActive+' '+mr.description);
      mrhToggleActivationAllowIncrease: li.SubItems.Add(rsUnFreeze+' '+ mr.description+' '+rsButAllowIncrease);
      mrhToggleActivationAllowDecrease: li.SubItems.Add(rsUnFreeze+' '+ mr.description+' '+rsButAllowDecrease);
      mrhSetValue: li.SubItems.Add(rsSet+' '+mr.description+' '+rsTo+' '+ hk.value);
      mrhIncreaseValue: li.SubItems.Add(rsIncrease+' '+mr.description+' '+rsBy+ ' '+  hk.value);
      mrhDecreaseValue: li.SubItems.Add(rsDecrease+' '+mr.description+' '+rsBy+ ' '+  hk.value);
      else
        li.SubItems.Add(rsDoSomethingWith+' '+mr.description);
    end;
  end else
    li.SubItems.Add(hk.description);
end;

procedure TfrmTrainerGenerator.AddHotkey(hk: TMemoryrecordHotkey);
var li: TListitem;
  mr: TMemoryRecord;
begin
  li:=lvCheats.Items.Add;
  li.Data:=hk;

  RefreshHotkeyItem(li);
end;

procedure TfrmTrainerGenerator.buildcheatlist;
var cheatpanel: TScrollBox;
  i: integer;
  currentcheat, lastcheat: TCheat;
  hk: TMemoryRecordHotkey;
begin
  cheatpanel:=TScrollBox(trainerform.FindComponent('CHEATPANEL'));

  if cheatpanel<>nil then
  begin
    //clear the old list (only the TCheat objects)
    i:=0;
    while i<cheatpanel.ControlCount do
    begin
      if cheatpanel.controls[i].Tag<>0 then
        cheatpanel.Controls[i].Free
      else
        inc(i);
    end;

    currentCheat:=nil;
    for i:=0 to lvCheats.Items.Count-1 do
    begin
      lastCheat:=currentCheat;
      currentcheat:=tcheat.create(trainerform);
      currentcheat.parent:=cheatpanel;
      currentcheat.name:='CHEAT'+inttostr(i);
      currentcheat.cheatnr:=i;
      currentcheat.tag:=i+1;
      currentcheat.AutoSize:=true;

      if lastcheat=nil then
      begin
        //top
        currentcheat.left:=scalex(10,96);
        currentcheat.top:=scaley(40,96);
      end
      else
      begin
        //next one
        currentcheat.top:=lastcheat.Top+lastcheat.height+scaley(10,96);
        currentcheat.left:=lastcheat.left;
      end;

      currentcheat.hotkeyleft:=hotkeylabel.left-currentcheat.left;
      currentcheat.descriptionleft:=descriptionlabel.left-currentcheat.left;

      currentcheat.width:=cheatpanel.clientwidth-currentcheat.Left-scalex(2,96);
      currentcheat.anchors:=currentcheat.anchors+[akRight];

      currentcheat.Hotkey:=lvCheats.Items[i].Caption;
      currentcheat.Description:=lvCheats.Items[i].SubItems[0];



    end;



  end;
end;

procedure TfrmTrainerGenerator.FillHotkeyList;
var i,j: integer;
  mr: TMemoryRecord;
  h: TMemoryRecordHotkey;
begin
  lvCheats.Clear;

  for i:=0 to mainform.addresslist.Count-1 do
  begin
    mr:=mainform.addresslist.MemRecItems[i];

    if mr.hasHotkeys then
    begin
      for j:=0 to mr.HotkeyCount-1 do
        AddHotkey(mr.Hotkey[j]);
    end;

  end;
end;

procedure TfrmTrainerGenerator.FormCreate(Sender: TObject);
var i,j: integer;
  f: TCEForm;

  temp: TObject;
  mr: Tmemoryrecord;

  cheatnr: integer;
  currentcheat: tcheat;
  lastcheat: tcheat;
  r: integer;

  reusedWindow: boolean;
  fname: string;

  hotkeynamename, memrecname: string;

  cheatpanel: TScrollBox;
begin

  //get the processlist
  GetProcessList(comboProcesslist.Items, true);

  //find the current process in the processlist
  {$IFDEF WINDOWS}
  for i:=0 to comboProcesslist.Items.Count-1 do
    if PProcessListInfo(comboProcesslist.Items.Objects[i]).processID=processid then
    begin
      //found it
      comboProcesslist.ItemIndex:=i;
      break;
    end;
  {$ENDIF}

  //first check if there is already a trainerform
  reusedWindow:=false;
  for i:=0 to mainform.LuaForms.count-1 do
  begin
    temp:=TObject(mainform.luaforms[i]) ;
    if (temp is TTrainerform) then
    begin
      r:=messagedlg(rsAlreadyATrainerFormDefined, mtConfirmation, [mbok, mbcancel], 0);

      if r=mrok then
      begin
        trainerform:=TTrainerForm(mainform.luaforms[i]);

        extrapanel:=TCEPanel(trainerform.FindComponent('EXTRAPANEL'));
        cheatpanel:=TScrollBox(trainerform.FindComponent('CHEATPANEL'));
        aboutbutton:=TCECustomButton(trainerform.FindComponent('ABOUTBUTTON'));
        image:=TCEImage(trainerform.FindComponent('IMAGE'));
        closebutton:=TCECustomButton(trainerform.FindComponent('CLOSEBUTTON'));
        seperator:=TCESplitter(trainerform.FindComponent('SEPERATOR'));

        hotkeylabel:=TCELabel(trainerform.FindComponent('HOTKEYLABEL'));
        descriptionlabel:=TCELabel(trainerform.FindComponent('DESCRIPTIONLABEL'));

        if seperator<>nil then
          seperator.Enabled:=true; //in case the script disabled it


        if cheatpanel=nil then
        begin
          if messagedlg(Format(rsNoCheatPanel, [#13#10]), mtError, [mbyes, mbno], 0)=mryes then
            trainerform:=nil;

        end;


        reusedWindow:=true;
      end
      else
      begin
        canceled:=true;
        exit;
      end;
      break;
    end;


  end;

  if trainerform=nil then
  begin
    //create it
    trainerform:=TTrainerForm.CreateNew(nil);
    trainerform.AutoSize:=false;
    trainerform.defaultTrainer:=true;
    trainerform.DesignTimePPI:=screen.PixelsPerInch;

    trainerform.width:=scalex(350,96);
    trainerform.height:=scaley(240,96);

    mainform.luaforms.add(trainerform);

    //now initialize the form to it's default
    trainerform.name:='CETrainer';
    trainerform.Position:=poScreenCenter;

    seperator:=TCESplitter.create(trainerform);
    seperator.Align:=alLeft;
    seperator.name:='SEPERATOR';
    seperator.parent:=trainerform;

    extrapanel:=Tcepanel.create(trainerform);
    extrapanel.align:=alleft;
    extrapanel.width:=100;
    extrapanel.name:='EXTRAPANEL';
    extrapanel.caption:='';
    extrapanel.bevelinner:=bvLowered;
    extrapanel.bevelouter:=bvLowered;
    extrapanel.parent:=trainerform;

    cheatpanel:=TScrollBox.create(trainerform);
    cheatpanel.BorderStyle:=bsNone;
    cheatpanel.name:='CHEATPANEL';
    cheatpanel.caption:='';
    cheatpanel.parent:=trainerform;
    cheatpanel.AnchorSideTop.Control:=trainerform;
    cheatpanel.AnchorSideTop.side:=asrTop;
    cheatpanel.AnchorSideLeft.Control:=extrapanel;
    cheatpanel.AnchorSideLeft.side:=asrRight;
    cheatpanel.AnchorSideRight.Control:=trainerform;
    cheatpanel.AnchorSideRight.side:=asrRight;



    aboutbutton:=TCECustomButton.create(trainerform);
    aboutbutton.autosize:=true;
    aboutbutton.name:='ABOUTBUTTON';
    aboutbutton.caption:=rsAbout;
    aboutbutton.align:=albottom;
    aboutbutton.Parent:=extrapanel;
    aboutbutton.RoundingX:=ScaleX(10,96);
    aboutbutton.RoundingY:=ScaleX(10,96);
    aboutbutton.DrawFocusRect:=false;

    with TLuaCaller.create do
    begin
      luaroutine:='AboutClick';
      aboutbutton.onclick:=NotifyEvent;
    end;


    image:=TCEImage.create(trainerform);
    image.name:='IMAGE';
    image.align:=alclient;
    image.stretch:=true;
    image.parent:=extrapanel;

    //these are part of the cheatpanel which has ben destroyed or just created
    hotkeylabel:=Tcelabel.create(trainerform);
    hotkeylabel.name:='HOTKEYLABEL';
    hotkeylabel.caption:=rsHotkey;
    hotkeylabel.left:=scalex(10,96);
    hotkeylabel.top:=scaley(10,96);
    hotkeylabel.parent:=cheatpanel;

    descriptionlabel:=Tcelabel.create(trainerform);
    descriptionlabel.name:='DESCRIPTIONLABEL';
    descriptionlabel.caption:=rsEffect;
    descriptionlabel.left:=100; //gets adjusted automatically
    descriptionlabel.top:=hotkeylabel.top;
    descriptionlabel.parent:=cheatpanel;


    closebutton:=TCECustomButton.create(trainerform);
    closebutton.name:='CLOSEBUTTON';
    closebutton.caption:=rsClose;
//    closebutton.top:=cheatpanel.clientheight - closebutton.height-8;
//    closebutton.left:=cheatpanel.clientwidth div 2 - closebutton.width div 2;

    closebutton.AnchorSideLeft.Control:=cheatpanel;
    closebutton.AnchorSideLeft.Side:=asrCenter;

    closebutton.AnchorSideBottom.Control:=trainerform;
    closebutton.AnchorSideBottom.Side:=asrBottom;
    closebutton.RoundingX:=ScaleX(10,96);
    closebutton.RoundingY:=ScaleX(10,96);
    closebutton.parent:=trainerform;
    closebutton.autosize:=true;
    closebutton.DrawFocusRect:=false;
    closebutton.anchors:=[akLeft, akBottom];

    cheatpanel.AnchorSideBottom.Control:=closebutton;
    cheatpanel.AnchorSideBottom.side:=asrTop;
    cheatpanel.anchors:=[akTop, akLeft, akRight, akBottom];



    with TLuaCaller.create do
    begin
      luaroutine:='CloseClick';
      closebutton.onclick:=NotifyEvent;
      trainerform.OnClose:=CloseEvent; //same routine
    end;

//    LUA_DoScript('function CloseClick(s) showMessage('''+rsOnCloseWarning+''') return caHide end');
    LUA_DoScript('function CloseClick(s) showMessage('''+rsOnCloseWarning+''') return caHide end');
  end;






  fillHotkeyList;
  buildcheatlist;
  FillSound;

  playbitmap:=TBitmap.Create;
  stopbitmap:=TBitmap.Create;
  tgImageList.GetBitmap(0, playbitmap);
  tgImageList.GetBitmap(1, stopbitmap);

  sbPlayStopXM.Glyph:=playbitmap;
end;

procedure TfrmTrainerGenerator.FormShow(Sender: TObject);
var
  br: trect;
begin
  if not shown then
  begin
    DPIHelper.AdjustSpeedButtonSize(spbUp);
    DPIHelper.AdjustSpeedButtonSize(spbDown);
    DPIHelper.AdjustSpeedButtonSize(sbPlayActivate);
    DPIHelper.AdjustSpeedButtonSize(sbPlayDeactivate);
    DPIHelper.AdjustSpeedButtonSize(sbPlayStopXM);
    DPIHelper.AdjustComboboxSize(cbOutput, self.canvas);
    DPIHelper.AdjustComboboxSize(cbActivateSound, self.canvas);
    DPIHelper.AdjustComboboxSize(cbDeactivateSound, self.canvas);

    shown:=true;
  end;

  if trainerform<>nil then
  begin
    trainerform.show;
    if LCLIntf.GetWindowRect(frmTrainerGenerator.handle, br)>0 then
    begin
      trainerform.left:=br.Right+5;
      trainerform.top:=br.top;
    end;
  end;

  Constraints.MinHeight:=panel9.top+button5.top+button5.height+label6.height+5;
end;

procedure TfrmTrainerGenerator.lvCheatsDblClick(Sender: TObject);
begin
  miEditHotkey.Click;
end;

procedure TfrmTrainerGenerator.lvCheatsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  btnDelete.enabled:=selected;


  if selected then
  begin
    spbDown.enabled:=item.index<lvcheats.items.count-1;
    spbUp.enabled:=item.index>0;

  end
  else
  begin
    spbUp.enabled:=false;
    spbDown.enabled:=false;
  end;


end;

procedure TfrmTrainerGenerator.MenuItem1Click(Sender: TObject);
begin
  buildcheatlist;
end;

procedure TfrmTrainerGenerator.MenuItem2Click(Sender: TObject);
begin
  generateScript
end;

procedure TfrmTrainerGenerator.miEditHotkeyClick(Sender: TObject);
var mh: TMemoryrecordhotkey;
  mr: TMemoryrecord;
begin
  if lvcheats.selected<>nil then
  begin
    mh:=TMemoryrecordhotkey(lvcheats.selected.data);
    mr:=mh.owner;
    editHotkey(mr,mh);

    RefreshHotkeyItem(lvcheats.selected);
  end;
end;


procedure TfrmTrainerGenerator.Panel2Resize(Sender: TObject);
begin
  lvCheats.Column[1].Width:=lvCheats.clientwidth-lvCheats.Column[0].Width-3;
end;

procedure TfrmTrainerGenerator.RadioButton2Change(Sender: TObject);
begin

end;

procedure TfrmTrainerGenerator.sbPlayActivateClick(Sender: TObject);
begin
  LUA_DoScript('playSound(findTableFile([['+cbActivateSound.Text+']]))');
end;

procedure TfrmTrainerGenerator.sbPlayDeactivateClick(Sender: TObject);
begin
  LUA_DoScript('playSound(findTableFile([['+cbDeactivateSound.Text+']]))');
end;

procedure TfrmTrainerGenerator.sbPlayStopXMClick(Sender: TObject);
begin
  if sbPlayStopXM.tag=0 then //not yet playing
  begin
    xmplayer.playXM(fnXM.FileName);
    sbPlayStopXM.glyph:=stopbitmap;
    sbPlayStopXM.tag:=1;
  end
  else
  begin
    xmplayer.stop;
    sbPlayStopXM.glyph:=playbitmap;
    sbPlayStopXM.tag:=0;
  end;
end;

procedure TfrmTrainerGenerator.spbDownClick(Sender: TObject);
var
  i: integer;

  temphotkey: string;
  tempdescription: string;
  tempdata: pointer;

begin
  if lvcheats.ItemIndex<>-1 then
  begin
    if lvcheats.ItemIndex<lvcheats.items.count-1 then
    begin
      temphotkey:=lvcheats.items[lvcheats.ItemIndex].Caption;
      tempdescription:=lvcheats.items[lvcheats.ItemIndex].SubItems[0];
      tempdata:=lvcheats.items[lvcheats.ItemIndex].data;

      lvcheats.items[lvcheats.ItemIndex].Caption:=lvcheats.items[lvcheats.ItemIndex+1].Caption;
      lvcheats.items[lvcheats.ItemIndex].subitems[0]:=lvcheats.items[lvcheats.ItemIndex+1].subitems[0];
      lvcheats.items[lvcheats.ItemIndex].data:=lvcheats.items[lvcheats.ItemIndex+1].data;

      lvcheats.items[lvcheats.ItemIndex+1].Caption:=temphotkey;
      lvcheats.items[lvcheats.ItemIndex+1].subitems[0]:=tempdescription;
      lvcheats.items[lvcheats.ItemIndex+1].data:=tempdata;

      lvcheats.itemindex:=lvcheats.itemindex+1;

      buildcheatlist;
    end;

  end;
end;

procedure TfrmTrainerGenerator.spbUpClick(Sender: TObject);
var
  i: integer;

  temphotkey: string;
  tempdescription: string;
  tempdata: pointer;

begin
  if lvcheats.ItemIndex>=1 then
  begin
    temphotkey:=lvcheats.items[lvcheats.ItemIndex-1].Caption;
    tempdescription:=lvcheats.items[lvcheats.ItemIndex-1].SubItems[0];
    tempdata:=lvcheats.items[lvcheats.ItemIndex-1].data;

    lvcheats.items[lvcheats.ItemIndex-1].Caption:=lvcheats.items[lvcheats.ItemIndex].Caption;
    lvcheats.items[lvcheats.ItemIndex-1].subitems[0]:=lvcheats.items[lvcheats.ItemIndex].subitems[0];
    lvcheats.items[lvcheats.ItemIndex-1].data:=lvcheats.items[lvcheats.ItemIndex].data;

    lvcheats.items[lvcheats.ItemIndex].Caption:=temphotkey;
    lvcheats.items[lvcheats.ItemIndex].subitems[0]:=tempdescription;
    lvcheats.items[lvcheats.ItemIndex].data:=tempdata;

    lvcheats.itemindex:=lvcheats.itemindex-1;

    buildcheatlist;
  end;
end;

procedure TfrmTrainerGenerator.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  try
    xmplayer.stop; //can raise an not initialized exception
  except
    //deal with it by ignoring it
  end;

  if not cbSupportCheatEngine.checked then
    cbSupportCheatEngine.checked:=true;

  cleanProcessList(comboProcesslist.items);

  closeaction:=cafree;
  frmTrainerGenerator:=nil;

  if trainerform<>nil then
  begin
    if (btnDesignForm.Tag=1) and (formdesigner<>nil) then
      formdesigner.Close;

    trainerform.hide;
  end;
end;

procedure TfrmTrainerGenerator.cbConfigD3DHookClick(Sender: TObject);
begin
  frmD3DTrainerGeneratorOptions.show;
end;

procedure TfrmTrainerGenerator.Button3Click(Sender: TObject);
begin
  zeromemory(@popupkeys,sizeof(TKeyCombo));
  edtPopupHotkey.Text:=ConvertKeyComboToString(popupkeys);
  edtPopupHotkey.SetFocus;
end;

procedure FillSoundList(list: Tstrings);
var
  i: integer;
  riff: pchar;
begin
  getmem(riff,5);
  for i:=0 to mainform.LuaFiles.Count-1 do
  begin
    if mainform.LuaFiles[i].stream.size>4 then
    begin
      CopyMemory(riff, mainform.LuaFiles[i].stream.Memory,4);
      riff[4]:=#0;
      if riff='RIFF' then //good enough (could still be wrong, but better than random)
        list.add(mainform.LuaFiles[i].name);
    end;
  end;

  FreeMemAndNil(riff);

  for i:=0 to mainform.InternalLuaFiles.Count-1 do
  begin
    if list.IndexOf(mainform.InternalLuaFiles[i].name)=-1 then //not overriden
      list.add(mainform.InternalLuaFiles[i].name);
  end;
end;

procedure TfrmTrainerGenerator.FillSound;
var i: integer;
  s: tstringlist;

  riff: pchar;

  oldcbActivateSound: string;
  oldcbDeactivateSound:string;
begin


  s:=tstringlist.create;

  FillSoundList(s);

  oldcbActivateSound:=cbActivateSound.text;
  oldcbDeactivateSound:=cbDeactivateSound.text;


  cbActivateSound.Items.Assign(s);
  cbActivateSound.DropDownCount:=max(16, s.count);

  cbDeactivateSound.Items.Assign(s);
  cbDeactivateSound.DropDownCount:=max(16, s.count);


  cbActivateSound.itemindex:=cbActivateSound.Items.IndexOf(oldcbActivateSound);
  cbDeactivateSound.itemindex:=cbDeactivateSound.Items.IndexOf(oldcbDeactivateSound);

  s.free;
end;

procedure TfrmTrainerGenerator.btnAddSoundsClick(Sender: TObject);
var
  i: integer;
  s: tmemorystream;
  lf: TLuafile;
begin
  odwave.InitialDir:=GetCEdir;
  if odwave.execute then
  begin
    for i:=0 to odwave.Files.Count-1 do
    begin
      s := TMemorystream.Create;
      try
        s.LoadFromFile(odwave.files[i]);
        lf := TLuaFile.Create(extractfilename(odwave.files[i]), s);

        MainForm.LuaFiles.Add(lf);
      finally
        s.free;
      end;
    end;


    FillSound;
  end;
end;

procedure TfrmTrainerGenerator.cbPlaySoundOnActionChange(Sender: TObject);
begin
  lblActivateSound.enabled:=cbPlaySoundOnAction.checked;
  lblDeactivateSound.enabled:=cbPlaySoundOnAction.checked;
  cbActivateSound.enabled:=cbPlaySoundOnAction.checked;
  cbDeactivateSound.enabled:=cbPlaySoundOnAction.checked;
  sbPlayActivate.enabled:=cbPlaySoundOnAction.checked;
  sbPlayDeactivate.enabled:=cbPlaySoundOnAction.checked;

  btnAddSounds.enabled:=cbPlaySoundOnAction.checked;
end;





procedure TfrmTrainerGenerator.Button1Click(Sender: TObject);
var
  i: integer;


  nextpos: integer;
  maxheight: integer;
  p: TImage;

//  iconlist: array of Hicon;

  z: Ticon;

  e: THandle;
  hr: THandle;

  r: pointer;
  aaa: TFPResourceHMODULE;
  wee: HGlobal;

  rs: TResourceStream;

  size: integer;

  modulehandle: THandle;

  m: tmemorystream;
begin

  trainerform.icon:=pickIcon;


end;

procedure TfrmTrainerGenerator.btnDeleteClick(Sender: TObject);
var hk : TMemoryRecordHotkey;
begin
  if lvCheats.Selected<>nil then
  begin
    if messagedlg(rsAreYouSure, mtConfirmation, [mbyes, mbno], 0)=mryes then
    begin
      hk:=TMemoryRecordHotkey(lvCheats.selected.data);
      hk.Free;

      lvCheats.Selected.Delete;

      buildcheatlist;
    end;
  end;
end;

procedure TfrmTrainerGenerator.Button2Click(Sender: TObject);
begin
  image:=TCEImage(trainerform.FindComponent('IMAGE'));//in case the image object got replaced
  if openpicturedialog1.execute then
    image.Picture.LoadFromFile(openpicturedialog1.FileName);
end;

procedure TfrmTrainerGenerator.generateScript2;
begin

end;

procedure TfrmTrainerGenerator.generateScript;
var generated: tstringlist;
  start,stop: integer;
  i: integer;
  l: tstrings;

  keyparams: string;

  f: TMemorystream;
  s: string;

  currentcheat: TCheat;
  currenthk: TMemoryRecordHotkey;
  currentmr: TMemoryrecord;
  fname: string;

  memrecname,hotkeyname: string;
  screwit: tstringlist;

  checked, unchecked: tbitmap;
  CHECKBOXIMAGE_UNCHECKED: integer;
  CHECKBOXIMAGE_CHECKED: integer;
  d3dfontlabel: tcelabel;

  cheatpanel: TCEPanel;
begin

  trainerform.active:=false;
  trainerform.SaveCurrentStateasDesign;


  //add to the script routine an auto attach registration
  if comboprocesslist.text<>'' then
    comboProcesslist.text
  else
    raise exception.create(rsPleaseSelectTypeInaProcessname);

  //erase the --TRAINERGENERATOR part of the luascript
  l:=mainform.frmLuaTableScript.assemblescreen.Lines;
  start:=l.IndexOf('--TRAINERGENERATORSTART--');
  stop:=l.IndexOf('--TRAINERGENERATORSTOP--');

  if ((start=-1) or (stop=-1)) and (start<>stop) then
    raise exception.create(rsInvalidTrainerScript);

  if start<>-1 then
    for i:=start to stop do
      l.Delete(start);

  //now write
  l.add('--TRAINERGENERATORSTART--');
  l.add('--'+rsAutogenwarningPart1);
  l.add('--'+rsAutoGenWarningPart2);
  l.add('');
  l.add('--Uncomment the following line if this is a Cheat Table format trainer and you don''t want CE to show (Tip, save as .CETRAINER alternatively)');
  l.add('--hideAllCEWindows()');
  l.add('');
  l.add('RequiredCEVersion='+floattostr(ceversion));
  l.add('if (getCEVersion==nil) or (getCEVersion()<RequiredCEVersion) then');
  l.add('  messageDialog(''Please install '+strCheatEngine+' ''..RequiredCEVersion, mtError, mbOK)');
  l.add('  closeCE()');
  l.add('end');

  try


    if cbUseD3DHook.checked then
      l.add('d3dcheats={}  --table containing the information to build the cheat lines for d3d');


    cheatpanel:=TCEPanel(trainerform.FindComponent('CHEATPANEL'));
    if cheatpanel<>nil then
    begin



      //create the routines for these cheats
      l.add('addresslist=getAddressList()');

      //fill the memrec list
      for i:=0 to mainform.addresslist.count-1 do
        if mainform.addresslist.MemRecItems[i].hasHotkeys then
          l.add('memrec'+inttostr( mainform.addresslist.MemRecItems[i].id)+'=addresslist.getMemoryRecordByID('+inttostr(mainform.addresslist.MemRecItems[i].id)+')');

      l.add('');

      //fill the hotkey list
      for i:=0 to lvCheats.Items.Count-1 do
      begin
        currenthk:=TMemoryRecordHotkey(lvcheats.Items[i].Data);
        currentmr:=currenthk.owner;

        memrecname:='memrec'+inttostr(currentmr.id);
        hotkeyname:=memrecname+'_hotkey'+inttostr(currenthk.id);
        l.add(hotkeyname+'='+memrecname+'.getHotkeyByID('+inttostr(currenthk.id)+')');
      end;
      l.add('');

      //now go through the actual cheatlist on the form itself and give it it's functions


      for i:=0 to cheatpanel.ControlCount-1 do
      begin

        if cheatpanel.Controls[i] is TCheat then
        begin
          currentcheat:=TCheat(cheatpanel.Controls[i]);


          currenthk:=TMemoryRecordHotkey(lvcheats.Items[currentcheat.cheatnr].Data);
          currentmr:=currenthk.owner;

          if cbUseD3DHook.checked then
          begin
            l.add('d3dcheats['+inttostr(currentcheat.cheatnr+1)+']={}');
            l.add('d3dcheats['+inttostr(currentcheat.cheatnr+1)+'].description=[['+currentcheat.Description+']]');
            l.add('d3dcheats['+inttostr(currentcheat.cheatnr+1)+'].hotkeys=[['+currentcheat.Hotkey+']]');
            l.add('d3dcheats['+inttostr(currentcheat.cheatnr+1)+'].top='+inttostr(currentcheat.Top));
            l.add('d3dcheats['+inttostr(currentcheat.cheatnr+1)+'].left='+inttostr(currentcheat.Left));
            l.add('d3dcheats['+inttostr(currentcheat.cheatnr+1)+'].memrecid='+inttostr(currentmr.id));
            l.add('d3dcheats['+inttostr(currentcheat.cheatnr+1)+'].hotkeyid='+inttostr(currenthk.id));

          end;


          //get the memrecname
          memrecname:='memrec'+inttostr(currentmr.id);


          //get the hotkey name
          hotkeyname:=memrecname+'_hotkey'+inttostr(currenthk.id);


          case currenthk.action of
            mrhToggleActivation,
            mrhToggleActivationAllowIncrease,
            mrhToggleActivationAllowDecrease:
            begin
              //constantly enabled
              fname:='onPostHotkey'+inttostr(currentcheat.cheatnr);
              l.Add('function '+fname+'(Hotkey)');
              l.add('  --Executed after the "toggle*" cheat got executed');
              l.add('  local memrec=Hotkey.Owner');
              l.add('  local isActive=memrec.Active --get the state after the hotkey got triggered');
              l.add('  '+trainerform.name+'.'+currentcheat.name+'.setActive(isActive) --gui update, nothing else');
              l.add('  if gPlaySoundOnAction then');
              l.add('    if isActive then');
              l.add('      playSound(gActivateSound)');
              l.add('    else');
              l.add('      playSound(gDeactivateSound)');
              l.add('    end');
              l.add('  end');
              if cbUseD3DHook.checked and (frmD3DTrainerGeneratorOptions<>nil) and (frmD3DTrainerGeneratorOptions.cbHasCheckbox.checked) then
              begin
                l.add('  ');
                l.add('  local newcbtexture=nil');
                l.add('  if isActive then');
                l.add('    newcbtexture=CheckedTexture');
                l.add('  else');
                l.add('    newcbtexture=UncheckedTexture');
                l.add('  end');
                l.add('  d3dcheats['+inttostr(currentcheat.cheatnr+1)+'].CheckboxSprite.Texture=newcbtexture');
              end;
              l.add('end');
              l.add('');


              l.add(hotkeyname+'.onPostHotkey='+fname);
              l.add('');
            end;

            else
            begin
              //one time only
              fname:='onHotkey'+inttostr(currentcheat.cheatnr);

              l.Add('function '+fname+'(Hotkey)');
              l.add('  --Executed before the hotkey is handled');
              if currentcheat.HasEditBox then
              begin
                l.add('  local memrec=Hotkey.Owner');
                l.add('  memrec.Value='+trainerform.name+'.'+currentcheat.name+'.Editvalue --this will also update the ''frozen'' value');
              end;
              l.add('  '+trainerform.name+'.'+currentcheat.name+'.setActive(true, 1500)');
              l.add('  if gPlaySoundOnAction then');

              if currenthk.action=mrhDeactivate then
                l.add('    playSound(gDeactivateSound)')
              else
                l.add('    playSound(gActivateSound)');

              l.add('  end');
              l.add('end');
              l.add('');
              l.add(hotkeyname+'.onHotkey='+fname);

            end;


          end;
        end;
      end;
    end
    else //the user deleted the cheat panel
      showmessage(rsTipYouDontHaveToUseTheTrainerGeneratorIfYouDontWantTo);

    seperator:=TCESplitter(trainerform.FindComponent('SEPERATOR'));
    if seperator<>nil then
      l.Add(trainerform.name+'.SEPERATOR.Visible=false');


    l.add('');
    l.add('getAutoAttachList().add("'+comboProcesslist.text+'")');


    if (cbPopupOnKeypress.checked) and (edtPopupHotkey.text<>'') then
    begin
      l.add('function popupTrainerHotkeyFunction()');
      l.add('  '+trainerform.Name+'.show()');
      l.add('end');


      keyparams:='';
      for i:=0 to length(popupkeys)-1 do
      begin
        if popupkeys[i]=0 then break;
        keyparams:=keyparams+inttostr(popupkeys[i])+',';
      end;

      if keyparams<>'' then
        keyparams:=copy(keyparams, 1,length(keyparams)-1);

      l.add('createHotkey(popupTrainerHotkeyFunction, '+keyparams+')');
      l.add('getFreezeTimer().Interval='+edtFreezeInterval.text);
    end;

    if cbPlaySoundOnAction.checked then
    begin
      l.add('gPlaySoundOnAction=true');
      l.add('gActivateSound=findTableFile([['+cbActivateSound.text+']])');
      l.add('gDeactivateSound=findTableFile([['+cbDeactivateSound.text+']])');
    end
    else
      l.add('gPlaySoundOnAction=false');

    l.add(trainerform.Name+'.fixDPI() --remove this if you have already taken care of DPI issues yourself');
    l.add(trainerform.Name+'.show()');

    if mAbout.lines.count>0 then
    begin
      l.add('function AboutClick()');
      l.add('  showMessage(gAboutText)');
      l.add('end');

      s:='';

      screwit:=tstringlist.create;


      screwit.AddStrings(mabout.lines);
      if screwit.Count>0 then
      begin
        screwit[0]:='gAboutText=[['+screwit[0];
        screwit[screwit.count-1]:=screwit[screwit.count-1]+']]';
        l.AddStrings(screwit);
      end
      else
        l.add('gAboutText=""');

      screwit.free;

      l.add('');
    end;


    l.add('function CloseClick()');
    l.add('  --called by the close button onClick event, and when closing the form');
    l.add('  closeCE()');
    l.add('  return caFree --onClick doesn''t care, but onClose would like a result');
    l.add('end');

    l.add('');




    if cbPlayXM.checked then
    begin
      f:=TMemoryStream.Create;
      f.LoadFromFile(fnXM.FileName);
      //check if there is already a TRAINERXM file
      //if so, delete

      for i:=0 to mainform.LuaFiles.count-1 do
        if mainform.LuaFiles[i].name='TRAINERXM' then
        begin
          mainform.LuaFiles[i].free;
          mainform.LuaFiles.Delete(i);
          break;
        end;

      mainform.LuaFiles.add(TLuafile.create('TRAINERXM',f));
      f.free;

      l.add('');
      l.add('XMFILE=findTableFile(''TRAINERXM'')');
      l.add('xmplayer.playXM(XMFILE)');
      l.add('');

      if rbStopWhenAttached.checked then
      begin


        l.add('function onOpenProcess_xmplayer(processid)');
        l.add('  xmplayer.stop()');
        l.add('  if xmplayer_originalOnOpenProcess~=nil then');
        l.add('    xmplayer_originalOnOpenProcess(processid)');
        l.add('  end');
        l.add('end');

        l.add('xmplayer_originalOnOpenProcess=onOpenProcess');
        l.add('onOpenProcess=onOpenProcess_xmplayer');
      end
      else
      begin
        l.add('function focusCheck(sender)');
        l.add('  if ('+trainerform.Name+'.isForegroundWindow('+trainerform.Name+')) then');
        l.add('    if (xmplayer.isPlaying()==false) then');
        l.add('      xmplayer.resume()');
        l.add('    end');
        l.add('  else');
        l.add('    if (xmplayer.isPlaying()) then');
        l.add('      xmplayer.pause()');
        l.add('    end');
        l.add('  end');
        l.add('end');

        l.add('');
        l.add('focusTimer=createTimer(nil)');
        l.add('focusTimer.onTimer=focusCheck');
        l.add('focusTimer.Interval=250');
        l.add('focusTimer.Enabled=true');
        l.add('');
      end;


    end;

    if not cbSupportCheatEngine.checked then
    begin
      if adconfig<>nil then
      begin

        l.add('supportCheatEngine('+trainerform.name+', '+BoolToStr(adconfig.cbCanClose.checked,'true','false')+', '+adconfig.edtWidth.text+', '+adconfig.edtHeight.text+', '+inttostr(adconfig.adposition)+', '+QuotedStr(adconfig.ownurl)+', '+QuotedStr(adconfig.extraparam)+', '+inttostr(adconfig.percentage)+')');
        l.add('--Thank you from Dark Byte--');
      end;
    end;


    if cbUseD3DHook.checked then
    begin
      //create a label with the specified font in the trainer form

      l.add('');
      l.add('--    Direct 3D Hook Function   --');
      l.add('');

      if frmD3DTrainerGeneratorOptions<>nil then
      begin

        d3dfontlabel:=TCELabel(trainerform.FindComponent('D3DFONTLABEL'));
        if d3dfontlabel=nil then
        begin
          d3dfontlabel:=tcelabel.create(trainerform);
          d3dfontlabel.parent:=trainerform;
          d3dfontlabel.name:='D3DFONTLABEL';
        end;

        d3dfontlabel.Font.assign(frmD3DTrainerGeneratorOptions.lblTextColor.Font);
        d3dfontlabel.Visible:=false;

        trainerform.SaveCurrentStateasDesign;


        l.add('D3DHook={} --config options');
        l.add('D3DHook.oldOnOpenProcess=onOpenProcess  --easy and compatible way for different scripts to make use of onOpenProcess');
        l.add('D3DHook.transparency='+inttostr(frmD3DTrainerGeneratorOptions.TrackBar1.Position));
        l.add('D3DHook.textFont='+trainerform.name+'.D3DFONTLABEL.font');

        //l.add('D3DHook.allowDrag='+BoolToStr(frmD3DTrainerGeneratorOptions.cbAllowDrag.checked, 'true','false'));
        l.add('D3DHook.showHotkeys='+BoolToStr(frmD3DTrainerGeneratorOptions.cbShowHotkeys.checked, 'true', 'false'));
        l.add('D3DHook.hasCheckbox='+BoolToStr(frmD3DTrainerGeneratorOptions.cbHasCheckbox.checked, 'true', 'false'));
        l.add('D3DHook.stretch='+BoolToStr(frmD3DTrainerGeneratorOptions.cbStretch.checked, 'true',' false'));
        l.add('D3DHook.distanceBetweenLines='+frmD3DTrainerGeneratorOptions.edtDistanceBetweenLines.text);
        l.add('D3DHook.distanceFromTop='+frmD3DTrainerGeneratorOptions.edtDistanceFromTop.text);
        l.add('D3DHook.distanceFromBorder='+frmD3DTrainerGeneratorOptions.edtDistanceFromBorder.text);

        if frmD3DTrainerGeneratorOptions.rbTopLeft.checked then
          l.add('D3DHook.position=1')
        else
        if frmD3DTrainerGeneratorOptions.rbTopRight.checked then
          l.add('D3DHook.position=2')
        else
        if frmD3DTrainerGeneratorOptions.rbBottomLeft.checked then
          l.add('D3DHook.position=3')
        else
        if frmD3DTrainerGeneratorOptions.rbBottomRight.checked then
          l.add('D3DHook.position=4')
        else
        if frmD3DTrainerGeneratorOptions.rbTopRight.checked then
          l.add('D3DHook.position=5');


        l.add('');
        l.add('function D3DHook.UpdatePosition()');
        l.add('  if D3DHook.position==2 then --Top Right');
        l.add('    SetD3DMenuPosition(h.Width-BackgroundSprite.Width, 0)');
        l.add('  elseif D3DHook.position==3 then --Bottom Left');
        l.add('    SetD3DMenuPosition(0, h.Height-BackgroundSprite.Height)');
        l.add('  elseif D3DHook.position==4 then --Bottom Right');
        l.add('    SetD3DMenuPosition(h.Width-BackgroundSprite.Width, h.Height-BackgroundSprite.Height)');
        l.add('  elseif D3DHook.position==5 then --Center');
        l.add('    SetD3DMenuPosition((h.Width / 2)-(BackgroundSprite.Width / 2), (h.Height / 2)-(BackgroundSprite.Height/2))');
        l.add('  end');
        l.add('end');
        l.add('');
        l.add('function onOpenProcess()');
        l.add('  if (D3DHook.oldOnOpenProcess~=nil) then');
        l.add('    D3DHook.oldOnOpenProcess() --call the original onOpenProcess if needed');
        l.add('  end');
        l.add('  h=createD3DHook()');
        l.add('  if (h~=nil) then');

        if frmD3DTrainerGeneratorOptions.cbHasCheckbox.checked then
        begin
          //create a checkbox luafile resource if it doesn't exist yet

          //check if there is already a CHECKBOXIMAGE_CHECKED/UNCHECKED

          CHECKBOXIMAGE_UNCHECKED:=-1;
          CHECKBOXIMAGE_CHECKED:=-1;

          for i:=0 to mainform.LuaFiles.count-1 do
          begin
            if mainform.LuaFiles[i].name='CHECKBOXIMAGE_UNCHECKED' then
              CHECKBOXIMAGE_UNCHECKED:=i;

            if mainform.LuaFiles[i].name='CHECKBOXIMAGE_CHECKED' then
              CHECKBOXIMAGE_CHECKED:=i;
          end;

          if (CHECKBOXIMAGE_UNCHECKED=-1) or (CHECKBOXIMAGE_CHECKED=-1) then
          begin
            //if none, or only one is defined create new checkbox images

            //first cleanup. At max only one gets deleted so no need to worry about the index changing

            if CHECKBOXIMAGE_UNCHECKED<>-1 then //destroy the old one
            begin
              MainForm.LuaFiles[CHECKBOXIMAGE_UNCHECKED].Free;
              MainForm.LuaFiles.Delete(CHECKBOXIMAGE_UNCHECKED);
            end;

            if CHECKBOXIMAGE_CHECKED<>-1 then //destroy the old one
            begin
              MainForm.LuaFiles[CHECKBOXIMAGE_CHECKED].Free;
              MainForm.LuaFiles.Delete(CHECKBOXIMAGE_CHECKED);
            end;

            checked:=frmD3DTrainerGeneratorOptions.imgChecked.Picture.Bitmap;
            unchecked:=frmD3DTrainerGeneratorOptions.imgUnchecked.Picture.Bitmap;


            f:=TMemoryStream.create;
            checked.SaveToStream(f);

            mainform.LuaFiles.Add(TLuafile.create('CHECKBOXIMAGE_CHECKED', f));
            f.free;

            f:=TMemoryStream.create;
            unchecked.SaveToStream(f);
            mainform.LuaFiles.Add(TLuafile.create('CHECKBOXIMAGE_UNCHECKED', f));
            f.free;
          end;


          l.add('    --First get a "Picture" object to the checkbox images');
          l.add('    CheckedPicture=createPicture()');
          l.add('    UncheckedPicture=createPicture()');

          l.add('    CheckedPicture.loadFromStream(findTableFile("CHECKBOXIMAGE_CHECKED").Stream)');
          l.add('    UncheckedPicture.loadFromStream(findTableFile("CHECKBOXIMAGE_UNCHECKED").Stream)');

          l.add('    --create the textures for the the checked and unchecked checkbox with these pictures');
          l.add('    CheckedTexture=h.createTexture(CheckedPicture)');
          l.add('    UncheckedTexture=h.createTexture(UncheckedPicture)');
          l.add('    CheckedPicture.destroy() --Not needed anymore');
          l.add('    CheckedPicture=nil');
          l.add('    UncheckedPicture.destroy() ');
          l.add('    UncheckedPicture=nil');
        end;


        //save the background image as a luafile
        f:=TMemoryStream.create;
        frmD3DTrainerGeneratorOptions.imgPreview.Picture.SaveToStream(f);
        for i:=0 to mainform.LuaFiles.count-1 do
          if MainForm.LuaFiles[i].name='D3DTRAINERBACKGROUND' then
          begin
            mainform.LuaFiles[i].Free;
            mainform.LuaFiles.delete(i);
            break;
          end;
        mainform.LuaFiles.Add(TLuafile.create('D3DTRAINERBACKGROUND', f));
        f.free;



        l.add('    --create the texture for the background');
        l.add('    BackgroundPicture=createPicture()');
        l.add('    BackgroundPicture.loadFromStream(findTableFile("D3DTRAINERBACKGROUND").Stream)');
        l.add('    BackgroundTexture=h.createTexture(BackgroundPicture)');
        l.add('    BackgroundPicture.destroy() --Not needed anymore (The texture has everything we need)');
        l.add('    BackgroundPicture=nil');
        l.add('');
        l.add('    BackgroundSprite=h.createSprite(BackgroundTexture)');
        l.add('    BackgroundSprite.Alphablend=1.0-D3DHook.transparency / 100  --alphablend takes a value between 0.0 and 1.0 where 1.0 is fully visible, and transparency is a percentage from 0 to 100 where 100 is invisible');
        l.add('');
        l.add('    --create the cheat entry lines');
        l.add('    for i,info in ipairs(d3dcheats) do');
        l.add('      local pic=createPicture()');
        l.add('      local text=info.description');
        l.add('      if D3DHook.showHotkeys then --add the hotkey as well');
        l.add('        text=text.." ("..info.hotkeys..")"');
        l.add('      end');
        l.add('      pic.Bitmap.Canvas.Font.assign(D3DHook.textFont)');
        l.add('      local width=pic.Bitmap.Canvas.getTextWidth(text)');
        l.add('      local height=pic.Bitmap.Canvas.getTextHeight(text)');

        l.add('      pic.Bitmap.Canvas.Brush.Color=0x010101');
        l.add('      pic.Bitmap.Width=width');
        l.add('      pic.Bitmap.Height=height');
        l.add('      pic.Bitmap.Canvas.textOut(0,0,text)');
        l.add('      info.TextTexture=h.createTexture(pic, 0x010101)');
        l.add('      pic.destroy()');
        l.add('');
        l.add('      info.TextSprite=h.createSprite(info.TextTexture)');
        l.add('      info.TextSprite.Alphablend=1.0-D3DHook.transparency / 100');
        l.add('      if D3DHook.hasCheckbox then');
        l.add('        info.CheckboxSprite=h.createSprite(UncheckedTexture)');
        l.add('        info.CheckboxSprite.Alphablend=1.0-D3DHook.transparency / 100');
        l.add('      end');
        l.add('');

        l.add('    end');  //end of for loop
        l.add('');
        l.add('    D3DHook.UpdatePosition()');
        l.add('    --create a timer to update the position');
        l.add('    local t=createTimer()');
        l.add('    t.OnTimer=D3DHook.UpdatePosition');
        l.add('    t.Interval=2000 --every 2 seconds');
        l.add('    t.Enabled=true');
        l.add('');
        l.add('    if D3DHook.hasCheckbox then');
        l.add('      h.OnClick=D3DHookSpriteClick');
        l.add('    end');
        l.add('  end'); //end of h~=nil
        l.add('end');
        l.add('');
        l.add('function D3DHookSpriteClick(d3dhook_sprite, x, y)');
        l.add('  for i,info in ipairs(d3dcheats) do');
        l.add('    if (d3dhook_sprite==info.CheckboxSprite) or (d3dhook_sprite==info.TextSprite) then');
        l.add('      --clicked on a cheat entry. Execute the hotkey event');
        l.add('      local mr=getAddressList().getMemoryRecordByID(info.memrecid)');
        l.add('      mr.getHotkeyByID(info.hotkeyid).doHotkey() --execute the hotkey event');
        l.add('      break');
        l.add('    end');
        l.add('  end');
        l.add('end');
        l.add('');


        l.add('function SetD3DMenuPosition(x,y)');
        l.add('  --Sets up the trainer window and position');
        l.add('  --set the background position and go through the d3dcheats. Optionally also stretching the background sprite');
        l.add('  local maxX=0');
        l.add('  local startY=y');
        l.add('');
        l.add('  BackgroundSprite.X=math.floor(x)');
        l.add('  BackgroundSprite.Y=math.floor(y)');
        l.add('');
        l.add('  x=x+D3DHook.distanceFromBorder');
        l.add('  y=y+D3DHook.distanceFromTop');
        l.add('  for i,info in ipairs(d3dcheats) do');
        l.add('    local _x=x');
        l.add('    local lineheight=info.TextSprite.Height');
        l.add('');
        l.add('    if D3DHook.hasCheckbox then');
        l.add('      if info.CheckboxSprite.Height>lineheight then');
        l.add('        lineheight=info.CheckboxSprite.Height');
        l.add('      end');
        l.add('      info.CheckboxSprite.X=math.floor(_x)');
        l.add('      _x=_x+info.CheckboxSprite.Width+2');
        l.add('      info.CheckboxSprite.Y=math.floor(y+ (lineheight / 2) - (info.CheckboxSprite.Height /2))');
        l.add('    end');
        l.add('    info.TextSprite.X=math.floor(_x)');
        l.add('    info.TextSprite.Y=math.floor(y+ (lineheight / 2) - (info.TextSprite.Height /2))');
        l.add('');
        l.add('    if maxX<info.TextSprite.X+info.TextSprite.Width+4 then');
        l.add('      maxX=info.TextSprite.X+info.TextSprite.Width+4');
        l.add('    end');
        l.add('    y=y+lineheight+D3DHook.distanceBetweenLines');
        l.add('  end');
        l.add('');
        l.add('  if D3DHook.stretch then');
        l.add('    BackgroundSprite.Width=maxX-x+D3DHook.distanceFromBorder');
        l.add('    BackgroundSprite.Height=y-startY+D3DHook.distanceFromBorder');
        l.add('  end');
        l.add('end');


        if frmD3DTrainerGeneratorOptions.cbUseD3DKeys.checked then
        begin
          l.add('');
          l.add('D3DHook.visible=true');
          l.add('function D3DToggleMenuVisible()');
          l.add('  D3DHook.visible=not D3DHook.visible');
          l.add('');
          l.add('  BackgroundSprite.visible=D3DHook.visible');
          l.add('  for i,info in ipairs(d3dcheats) do');
          l.add('    if D3DHook.hasCheckbox then');
          l.add('      info.CheckboxSprite.visible=D3DHook.visible');
          l.add('    end');
          l.add('    info.TextSprite.visible=D3DHook.visible');
          l.add('  end');
          l.add('end');
          s:='';
          //build a key list
          for i:=0 to length(frmD3DTrainerGeneratorOptions.d3dkeys)-1 do
          begin
            if frmD3DTrainerGeneratorOptions.d3dkeys[i]=0 then break;
            s:=s+', '+inttostr(frmD3DTrainerGeneratorOptions.d3dkeys[i]);
          end;

          l.add('createHotkey(D3DToggleMenuVisible'+s+')');

        end;

      end
      else
        l.add('--WTF?--');



    end;


  finally
    l.add('--TRAINERGENERATORSTOP--');
  end;
end;

procedure TfrmTrainerGenerator.Button5Click(Sender: TObject);
var f: string;
    protect: boolean;
begin
  protect:=false;
  generateScript;

//  generateScript2;


  case cbOutput.ItemIndex of
    0:
    begin
      MainForm.miShowLuaScript.click;
      exit;
    end;

    1:
    begin
      if not EXESaveDialog.Execute then exit;
      f:=EXESaveDialog.FileName;

      if uppercase(ExtractFileName(f))=uppercase(comboProcesslist.Text) then
      begin
        messagedlg('You may not name your trainer .EXE the same as the process you wish to cheat on', mtError, [mbok],0);
        exit;
      end;

      protect:=true;
    end;

    2:
    begin
      if not CETRAINERSaveDialog.Execute then exit;
      f:=CETRAINERSaveDialog.FileName;
      protect:=cbProtect.checked;
    end;

    3:
    begin
      if not CTSaveDialog.Execute then exit;
      f:=CTSaveDialog.FileName;
      protect:=cbProtect.checked;
    end;

    else
      raise exception.create('Invalid option');

  end;


  savetable(f, protect);  //always protect

end;

procedure TfrmTrainerGenerator.btnDesignFormClick(Sender: TObject);
var i: integer;
    currentcheat: TCheat;
begin


  if btnDesignForm.tag=0 then
  begin
    mainform.createFormdesigner;

    formdesigner.designForm(trainerform);

    formdesigner.show;

    trainerform.show;

    edtCaption.enabled:=false;

    btnDesignForm.caption:=rsGoBackToGeneratedDesigner;
    btnDesignForm.tag:=1;
  end
  else
  begin
    if formdesigner<>nil then
      formdesigner.Close;

    btnDesignForm.tag:=0;

    btnDesignForm.caption:=rsDesignUserinterfaceManually;
  end;




end;

procedure TfrmTrainerGenerator.editHotkey(m: Tmemoryrecord; hotkey: TMemoryrecordhotkey);
var
  i,j: integer;
  oldlist: tlist;
  found: boolean;

  hkf: THotkeyform;
begin
  oldlist:=tlist.create;

  for i:=0 to m.HotkeyCount-1 do
    oldlist.Add(m.Hotkey[i]);

  hkf:=THotKeyForm.Create(self);
  with hkf do
  begin
    memrec:=m;
    memrec.beginEdit;

    if hotkey=nil then
      btnCreateHotkey.Click
    else
    begin
      //select the editable hotkey
      for i:=0 to hkf.ListView1.Items.Count-1 do
        if hkf.ListView1.items[i].Data=hotkey then
        begin
          //found it
          hkf.ListView1.Selected:=hkf.ListView1.items[i];
          hkf.ListView1.ItemIndex:=i;
          hkf.ListView1.Items[i].Selected:=true;
          hkf.btnEditHotkey.Click;
          break;
        end;
    end;

    showmodal;
  end;

  //recheck the hotkey list and check which ones have been added OR deleted

  //remove the entries that got deleted
  for i:=0 to oldlist.count-1 do
  begin
    found:=false;
    for j:=0 to m.HotkeyCount-1 do
      if oldlist[i]=m.Hotkey[i] then
      begin
        found:=true;
        break;
      end;

    if not found then
    begin
      //delete from lvcheats
      for j:=0 to lvCheats.items.count-1 do
        if lvcheats.items[j].Data=oldlist[i] then
        begin
          //found it
          lvCheats.Items.Delete(j);
          break;
        end;

    end;
  end;

  //now add the entries that are not present in the oldlist
  for i:=0 to m.Hotkeycount-1 do
  begin
    if oldlist.IndexOf(m.Hotkey[i])=-1 then //not in the list
      AddHotkey(m.Hotkey[i]);
  end;


  oldlist.free;

  //now update the hotkey and description texts
  for i:=0 to lvCheats.items.count-1 do
    RefreshHotkeyItem(lvCheats.items[i]);

  //and update the trainerform
  buildcheatlist;
end;

procedure TfrmTrainerGenerator.btnAddHotkeyClick(Sender: TObject);
var l: TfrmSelectionList;
  s: tstringlist;
  i,j: integer;


  m: TMemoryRecord;
  found: boolean;
begin
  // add hotkey
  if mainform.addresslist.Count>0 then
  begin
    s:=tstringlist.create;
    for i:=0 to mainform.addresslist.Count-1 do
      s.add(mainform.addresslist.MemRecItems[i].Description);

    l:=TfrmSelectionList.create(self,s);


    l.Caption:=rsCheatEntries;
    l.label1.caption:=rsSelectTheCheatEntryYouWantToSetTheHotkeyFor;
    l.itemindex:=0;

    if (l.showmodal=mrok) and (l.itemindex<>-1) then
      edithotkey(mainform.addresslist.MemRecItems[l.itemindex], nil);


    s.free;


  end
  else
    raise exception.create(rsYouNeedACheatTableWithCheatEntries);
end;

procedure TfrmTrainerGenerator.Button8Click(Sender: TObject);
begin

end;

procedure TfrmTrainerGenerator.cbCanResizeChange(Sender: TObject);
begin
  if cbCanResize.checked then
    trainerform.BorderStyle:=bsSizeable
  else
    trainerform.BorderStyle:=bsSingle;
end;

procedure TfrmTrainerGenerator.cbOutputChange(Sender: TObject);
begin

end;

procedure TfrmTrainerGenerator.cbOutputSelect(Sender: TObject);
var oldprotect: boolean;
begin
  oldprotect:=cbProtect.enabled and cbProtect.checked and (cbOutput.itemindex=1);

  cbProtect.enabled:=cbOutput.itemindex=1;
  cbProtect.checked:=(cbOutput.itemindex=0) or oldprotect;
end;

procedure TfrmTrainerGenerator.cbPlayXMChange(Sender: TObject);
begin
  fnXM.enabled:=cbPlayXM.checked;
  cbStopPlaying.enabled:=cbPlayXM.checked;
  rbStopWhenAttached.enabled:=cbPlayXM.checked and cbStopPlaying.checked;
  rbStopWhenFocusLost.enabled:=cbPlayXM.checked and cbStopPlaying.checked;

  sbPlayStopXM.enabled:=cbPlayXM.checked;
end;

procedure TfrmTrainerGenerator.cbStopPlayingChange(Sender: TObject);
begin
  rbStopWhenAttached.enabled:=cbPlayXM.checked and cbStopPlaying.checked;
  rbStopWhenFocusLost.enabled:=cbPlayXM.checked and cbStopPlaying.checked;
end;

procedure TfrmTrainerGenerator.RestoreSupportCE(sender: tobject);
begin
  cbSupportCheatEngine.caption:=rsDonTSupportCheatEngineOrYourself;
  if restoretimer<>nil then
    restoretimer.enabled:=false;
end;

procedure TfrmTrainerGenerator.cbSupportCheatEngineChange(Sender: TObject);
begin
  //Guilt procedure
  if not cbSupportCheatEngine.checked then
  begin
    cbSupportCheatEngine.caption:=rsThankYou;
    //show the ad config window

    if adwindow=nil then
       adwindow:=TADWindow.Create2(application, true);

    adwindow.Width:=468;
    adwindow.height:=60;
    adwindow.show;
    adwindow.AttachToForm(trainerform);
    adwindow.setPosition(akBottom);




    if adconfig=nil then
      adconfig:=TfrmAdConfig.create(self);

    if left>adconfig.width then
    begin
      adconfig.left:=left-adconfig.width-20;
      adconfig.top:=top;
    end
    else
    begin
      adconfig.Left:=left;
      adconfig.top:=top+height;
    end;

    adconfig.show;

    adwindow.optional:='designer=1';
    adwindow.LoadAd;

  end
  else
  begin
    cbSupportCheatEngine.caption:=rsAaaaw;
    if adwindow<>nil then
    begin
      adwindow.AttachToForm(nil);
      adwindow.hide;
    end;

    if adconfig<>nil then
      adconfig.hide;
  end;


  if restoretimer=nil then
  begin
    restoretimer:=TTimer.create(self);
    restoretimer.ontimer:=RestoreSupportCE;
    restoretimer.interval:=1500;
  end;

  restoretimer.enabled:=true;
end;

procedure TfrmTrainerGenerator.cbUseD3DHookChange(Sender: TObject);
begin
  cbConfigD3DHook.enabled:=cbUseD3DHook.checked;

  if cbConfigD3DHook.enabled and (frmD3DTrainerGeneratorOptions=nil) then
    frmD3DTrainerGeneratorOptions:=tfrmD3DTrainerGeneratorOptions.create(application);

  if (cbConfigD3DHook.enabled=false) and frmD3DTrainerGeneratorOptions.visible then
    frmD3DTrainerGeneratorOptions.hide;

end;

procedure TfrmTrainerGenerator.edtCaptionChange(Sender: TObject);
begin
  trainerform.caption:=edtcaption.text;
end;

procedure TfrmTrainerGenerator.edtPopupHotkeyKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var i: integer;
begin
  if popupkeys[4]=0 then
  begin
    for i:=0 to 4 do
      if popupkeys[i]=0 then
      begin
        popupkeys[i]:=key;
        break;
      end else
      if popupkeys[i]=key then break;
  end;

  edtPopupHotkey.Text:=ConvertKeyComboToString(popupkeys);

  key:=0;
end;

procedure TfrmTrainerGenerator.FileNameEdit1Change(Sender: TObject);
begin

end;

initialization
  RegisterClass(TTrainerForm);

  {$I trainergenerator.lrs}

end.

