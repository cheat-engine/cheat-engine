unit trainergenerator;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, ceguicomponents, lclintf, StdCtrls, EditBtn, ExtCtrls, ExtDlgs,
  ComCtrls, Buttons, Menus, ExtraTrainerComponents, cefuncproc, HotkeyHandler,
  HotKeys, symbolhandler, luacaller, formdesignerunit, opensave, luafile,
  frmAdConfigUnit, cesupport, IconStuff, memoryrecordunit, frmSelectionlistunit;

type
  TTrainerForm=class(TCEForm)
    public
      defaultTrainer: boolean;
  end; //so it's uniquely identifyable

  { TfrmTrainerGenerator }
  TfrmTrainerGenerator = class(TForm)
    btnDesignForm: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    btnDelete: TButton;
    btnAddHotkey: TButton;
    cbBeepOnAction: TCheckBox;
    cbCanResize: TCheckBox;
    cbPlayXM: TCheckBox;
    cbPopupOnKeypress: TCheckBox;
    cbProtect: TCheckBox;
    cbStopPlaying: TCheckBox;
    cbSupportCheatEngine: TCheckBox;
    CTSaveDialog: TSaveDialog;
    cbOutput: TComboBox;
    comboProcesslist: TComboBox;
    edtCaption: TEdit;
    edtFreezeInterval: TEdit;
    edtPopupHotkey: TEdit;
    fnXM: TFileNameEdit;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lvCheats: TListView;
    mAbout: TMemo;
    MenuItem1: TMenuItem;
    miEditHotkey: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PopupMenu1: TPopupMenu;
    rbStopWhenAttached: TRadioButton;
    rbStopWhenFocusLost: TRadioButton;
    CETRAINERSaveDialog: TSaveDialog;
    EXESaveDialog: TSaveDialog;
    spbDown: TSpeedButton;
    spbUp: TSpeedButton;
    procedure btnDeleteClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
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
    procedure miEditHotkeyClick(Sender: TObject);
    procedure Panel2Resize(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
    procedure spbDownClick(Sender: TObject);
    procedure spbUpClick(Sender: TObject);
  private
    { private declarations }
    popupkeys: TKeycombo;
    restoretimer: ttimer;
    adconfig: TfrmAdConfig;

    procedure editHotkey(m: Tmemoryrecord; hotkey: TMemoryrecordhotkey);
    procedure AddHotkey(hk: TMemoryrecordHotkey);
    procedure buildcheatlist;
    procedure fillHotkeyList;
    procedure generateScript;
    procedure RestoreSupportCE(sender: tobject);

    procedure RefreshHotkeyItem(li: TListitem);
  public
    trainerform: TTrainerForm;
    extrapanel: TCEPanel;
    cheatpanel: TCEPanel;
    aboutbutton: TCEButton;
    image: TCEImage;
    closebutton: TCEButton;
    seperator: TCESplitter;

    hotkeylabel, descriptionlabel: tcelabel;
    { public declarations }

    canceled: boolean;
  end; 

var
  frmTrainerGenerator: TfrmTrainerGenerator;

implementation

uses mainunit;

{ TfrmTrainerGenerator }

procedure TfrmTrainerGenerator.RefreshHotkeyItem(li: TListitem);
var hk: TMemoryrecordhotkey;
  mr: TMemoryrecord;
begin
  hk:=TMemoryrecordhotkey(li.data);
  mr:=hk.owner;
  li.caption:=ConvertKeyComboToString(hk.keys);


  if hk.description='' then
  begin
    //try to guess that it does
    case hk.action of
      mrhToggleActivation: li.SubItems.Add('(De)active '+mr.description);
      mrhToggleActivationAllowIncrease: li.SubItems.Add('(Un)Freeze '+mr.description+' but allow increase');
      mrhToggleActivationAllowDecrease: li.SubItems.Add('(Un)Freeze '+mr.description+' but allow decrease');
      mrhSetValue: li.SubItems.Add('Set '+mr.description+' to '+hk.value);
      mrhIncreaseValue: li.SubItems.Add('Increase '+mr.description+' by '+hk.value);
      mrhDecreaseValue: li.SubItems.Add('Decrease '+mr.description+' by '+hk.value);
      else
        li.SubItems.Add('Do something with '+mr.description);
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
var cheatpanel: TCEPanel;
  i: integer;
  currentcheat, lastcheat: TCheat;

  hk: TMemoryRecordHotkey;
begin
  cheatpanel:=TCEPanel(trainerform.FindComponent('CHEATPANEL'));

  if cheatpanel<>nil then
  begin
    //clear the old list (onloy the TCheat objects)
    i:=0;
    while i<cheatpanel.ControlCount do
    begin
      if cheatpanel.controls[i] is tcheat then
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

      if lastcheat=nil then
      begin
        //top
        currentcheat.left:=10;
        currentcheat.top:=40;
      end
      else
      begin
        //next one
        currentcheat.top:=lastcheat.Top+lastcheat.height+10;
        currentcheat.left:=lastcheat.left;
      end;

      currentcheat.hotkeyleft:=hotkeylabel.left-currentcheat.left;
      currentcheat.descriptionleft:=descriptionlabel.left-currentcheat.left;

      currentcheat.width:=cheatpanel.clientwidth-currentcheat.Left-2;
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

  mr: Tmemoryrecord;

  cheatnr: integer;
  currentcheat: tcheat;
  lastcheat: tcheat;
  r: integer;

  reusedWindow: boolean;
  fname: string;

  hotkeynamename, memrecname: string;
begin

  //get the processlist
  GetProcessList(comboProcesslist.Items, true);

  //find the current process in the processlist
  for i:=0 to comboProcesslist.Items.Count-1 do
    if PProcessListInfo(comboProcesslist.Items.Objects[i]).processID=processid then
    begin
      //found it
      comboProcesslist.ItemIndex:=i;
      break;
    end;

  //first check if there is already a trainerform
  reusedWindow:=false;
  for i:=0 to mainform.LuaForms.count-1 do
  begin
    if (TObject(mainform.luaforms[i]) is TTrainerform) then
    begin
      r:=messagedlg('There is already a trainer form defined. Continuing will erase the current trainerscript and cheats in the trainer and replace them with the current hotkeys defined in your current cheat table (Layout and images will remain unchanged). Continue ?', mtConfirmation, [mbok, mbcancel],0);

      if r=mrok then
      begin
        trainerform:=TTrainerForm(mainform.luaforms[i]);

        extrapanel:=TCEPanel(trainerform.FindComponent('EXTRAPANEL'));
        cheatpanel:=TCEPanel(trainerform.FindComponent('CHEATPANEL'));
        aboutbutton:=TCEButton(trainerform.FindComponent('ABOUTBUTTON'));
        image:=TCEImage(trainerform.FindComponent('IMAGE'));
        closebutton:=TCEButton(trainerform.FindComponent('CLOSEBUTTON'));
        seperator:=TCESplitter(trainerform.FindComponent('SEPERATOR'));

        if seperator<>nil then
          seperator.Enabled:=true; //in case the script disabled it


        if cheatpanel=nil then
        begin
          if messagedlg('The current trainer form does not have a panel named ''CHEATPANEL'' so can not be reused by the automated trainer generator.'+#13#10+'Do you want to start from scratch? (If you want to create a trainer from your current script you can just save your table as .EXE instead of using the automated trainer generator)', mtError, [mbyes, mbno], 0)=mryes then
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


    cheatpanel:=Tcepanel.create(trainerform);
    cheatpanel.align:=alclient;
    cheatpanel.name:='CHEATPANEL';
    cheatpanel.caption:='';
    cheatpanel.parent:=trainerform;




    hotkeylabel:=Tcelabel.create(trainerform);
    hotkeylabel.name:='HOTKEYLABEL';
    hotkeylabel.caption:='Hotkey';
    hotkeylabel.left:=10;
    hotkeylabel.top:=10;
    hotkeylabel.parent:=cheatpanel;

    descriptionlabel:=Tcelabel.create(trainerform);
    descriptionlabel.name:='DESCRIPTIONLABEL';
    descriptionlabel.caption:='Effect';
    descriptionlabel.left:=100;
    descriptionlabel.top:=hotkeylabel.top;
    descriptionlabel.parent:=cheatpanel;


    aboutbutton:=TCEButton.create(trainerform);
    aboutbutton.name:='ABOUTBUTTON';
    aboutbutton.caption:='About';
    aboutbutton.align:=albottom;
    aboutbutton.Parent:=extrapanel;
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

    closebutton:=TCEButton.create(trainerform);
    closebutton.name:='CLOSEBUTTON';
    closebutton.caption:='Close';
    closebutton.top:=cheatpanel.clientheight - closebutton.height-8;
    closebutton.left:=cheatpanel.clientwidth div 2 - closebutton.width div 2;
    closebutton.parent:=cheatpanel;

    closebutton.anchors:=[akBottom];

    with TLuaCaller.create do
    begin
      luaroutine:='CloseClick';
      closebutton.onclick:=NotifyEvent;
      trainerform.OnClose:=CloseEvent; //same routine
    end;


  end;

  fillHotkeyList;
  buildcheatlist;

  {
  Complete rewrite to make it better understandable for the braindead zombies that are used to ce 5.6.1

  code was:

  //fill in the processlist and select the currently opened process

  GetProcessList(comboProcesslist.Items, true);

  //find the current process in the processlist
  for i:=0 to comboProcesslist.Items.Count-1 do
    if PProcessListInfo(comboProcesslist.Items.Objects[i]).processID=processid then
    begin
      //found it
      comboProcesslist.ItemIndex:=i;
      break;
    end;


  functions:=tstringlist.create;
  init:=tstringlist.create;
  init.add('--Please note that this is an autogenerated trainer');
  init.add('--The reason the generated code is a bit bloated is because it doesn''t make');
  init.add('--use of memrec_onActive/onDeactive so it doesn''t interfere with user scripts');
  init.add('--And of course: It is autogenerated. Don''t expect too much of it');
  init.add('');
  init.add('--If you write your own script manually you can use as many shortcuts as you like');
  init.add('');
  init.add('addresslist=getAddressList()');


  //first check if there is already a trainerform
  reusedWindow:=false;
  for i:=0 to mainform.LuaForms.count-1 do
  begin
    if (TObject(mainform.luaforms[i]) is TTrainerform) then
    begin
      r:=messagedlg('There is already a trainer form defined. Do you want to reuse this window?', mtConfirmation, [mbyes,mbno,mbcancel],0);

      if r=mrCancel then
      begin
        canceled:=true;
        exit;
      end;

      if r=mryes then
      begin
        trainerform:=TTrainerForm(mainform.luaforms[i]);

        extrapanel:=TCEPanel(trainerform.FindComponent('EXTRAPANEL'));
        cheatpanel:=TCEPanel(trainerform.FindComponent('CHEATPANEL'));
        aboutbutton:=TCEButton(trainerform.FindComponent('ABOUTBUTTON'));
        image:=TCEImage(trainerform.FindComponent('IMAGE'));
        closebutton:=TCEButton(trainerform.FindComponent('CLOSEBUTTON'));
        seperator:=TCESplitter(trainerform.FindComponent('SEPERATOR'));

        if seperator<>nil then
          seperator.Enabled:=true; //in case the script disabled it

        reusedWindow:=true;
      end
      else
      begin
        TTrainerForm(mainform.luaforms[i]).free;
        mainform.LuaForms.Delete(i);
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


    cheatpanel:=Tcepanel.create(trainerform);
    cheatpanel.align:=alclient;
    cheatpanel.name:='CHEATPANEL';
    cheatpanel.caption:='';
    cheatpanel.parent:=trainerform;




    hotkeylabel:=Tcelabel.create(trainerform);
    hotkeylabel.name:='HOTKEYLABEL';
    hotkeylabel.caption:='Hotkey';
    hotkeylabel.left:=10;
    hotkeylabel.top:=10;
    hotkeylabel.parent:=cheatpanel;

    descriptionlabel:=Tcelabel.create(trainerform);
    descriptionlabel.name:='DESCRIPTIONLABEL';
    descriptionlabel.caption:='Effect';
    descriptionlabel.left:=100;
    descriptionlabel.top:=hotkeylabel.top;
    descriptionlabel.parent:=cheatpanel;


    aboutbutton:=TCEButton.create(trainerform);
    aboutbutton.name:='ABOUTBUTTON';
    aboutbutton.caption:='About';
    aboutbutton.align:=albottom;
    aboutbutton.Parent:=extrapanel;
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

    closebutton:=TCEButton.create(trainerform);
    closebutton.name:='CLOSEBUTTON';
    closebutton.caption:='Close';
    closebutton.top:=cheatpanel.clientheight - closebutton.height-8;
    closebutton.left:=cheatpanel.clientwidth div 2 - closebutton.width div 2;
    closebutton.parent:=cheatpanel;

    closebutton.anchors:=[akBottom];

    with TLuaCaller.create do
    begin
      luaroutine:='CloseClick';
      closebutton.onclick:=NotifyEvent;
      trainerform.OnClose:=CloseEvent; //same routine
    end;
  end;




  if seperator<>nil then
    init.Add('control_setVisible('+trainerform.name+'_SEPERATOR, false)');


  if (cheatpanel<>nil) and ((not reusedWindow) or (MessageDlg('Do you want to refill the cheat list?', mtconfirmation, [mbyes, mbno],0)=mryes)) then
  begin
    //whipe out the old cheats
    while cheatpanel.ComponentCount>0 do
      cheatpanel.Components[0].free;

    //configure it based on the current addresslist
    cheatnr:=0;
    lastcheat:=nil;
    for i:=0 to mainform.addresslist.Count-1 do
    begin
      mr:=mainform.addresslist.MemRecItems[i];

      if mr.hasHotkeys then
      begin
        memrecname:='memrec'+inttostr(mr.id);
        init.add(memrecname+'=addresslist_getMemoryRecordByID(addresslist,'+inttostr(mr.id)+')');
      end;

      //add the hotkeys (could be multiple)

      for j:=0 to mr.Hotkeycount-1 do
      begin
        //add it
        hotkeynamename:=memrecname+'_hotkey'+inttostr(mr.hotkey[j].id);
        init.add(hotkeynamename+'=memoryrecord_getHotkeyByID('+memrecname+','+inttostr(mr.hotkey[j].id)+')');

        currentcheat:=tcheat.create(trainerform);
        currentcheat.parent:=cheatpanel;
        currentcheat.name:='CHEAT'+inttostr(cheatnr);

        if lastcheat=nil then
        begin
          currentcheat.left:=10;
          currentcheat.top:=40;
        end
        else
        begin
          currentcheat.top:=lastcheat.Top+lastcheat.height+10;
          currentcheat.left:=lastcheat.left;
        end;

        currentcheat.hotkeyleft:=hotkeylabel.left-currentcheat.left;
        currentcheat.descriptionleft:=descriptionlabel.left-currentcheat.left;

        currentcheat.width:=cheatpanel.clientwidth-currentcheat.Left;
        currentcheat.anchors:=currentcheat.anchors+[akRight];

        currentcheat.Hotkey:=ConvertKeyComboToString(mr.hotkey[j].keys);
        if mr.hotkey[j].description='' then
        begin
          //try to guess that it does
          case mr.hotkey[j].action of
            mrhToggleActivation: currentcheat.description:='(De)active '+mr.description;
            mrhToggleActivationAllowIncrease: currentcheat.description:='(Un)Freeze '+mr.description+' but allow increase';
            mrhToggleActivationAllowDecrease: currentcheat.description:='(Un)Freeze '+mr.description+' but allow decrease';
            mrhSetValue: currentcheat.description:='Set '+mr.description+' to '+mr.hotkey[j].value;
            mrhIncreaseValue: currentcheat.description:='Increase '+mr.description+' by '+mr.hotkey[j].value;
            mrhDecreaseValue: currentcheat.description:='Decrease '+mr.description+' by '+mr.hotkey[j].value;
            else
              currentcheat.description:='Do something with '+mr.description;
          end;
        end
        else
          currentcheat.Description:=mr.hotkey[j].description;




        case mr.hotkey[j].action of
          mrhToggleActivation,
          mrhToggleActivationAllowIncrease,
          mrhToggleActivationAllowDecrease:
          begin
            //constantly enabled
            fname:='onPostHotkey'+inttostr(cheatnr);
            functions.Add('function '+fname+'(Hotkey)');
            functions.add('--executed after the "toggle*" cheat got executed so');
            functions.add('  local memrec=memoryrecordhotkey_getOwner(Hotkey)');
            functions.add('  local isActive=memoryrecord_isActive(memrec'+inttostr(mr.id)+') --get the state after the hotkey got triggered');
            functions.add('  cheatcomponent_setActive('+trainerform.name+'_CHEAT'+inttostr(cheatnr)+', isActive)');
            functions.add('  if gBeepOnAction then');
            functions.add('    beep()');
            functions.add('  end');
            functions.add('end');
            functions.add('');


            init.add('memoryrecordhotkey_onPostHotkey('+hotkeynamename+','+fname+')');
          end;

          else
          begin
            //one time only
            fname:='onHotkey'+inttostr(cheatnr);
            functions.Add('function '+fname+'(Hotkey)');
            functions.add('  cheatcomponent_setActive(CHEAT'+inttostr(cheatnr)+', isActive, 1500)');
            functions.add('  if gBeepOnAction then');
            functions.add('    beep()');
            functions.add('  end');
            functions.add('end');
            functions.add('');

            init.add('memoryrecordhotkey_afterHotkey('+hotkeynamename+','+fname+')');
          end;


        end;

        lastcheat:=currentcheat;
        inc(cheatnr);

      end;
    end;

    if cheatnr=0 then
      showmessage('The current cheat table has no hotkeys assigned. Only cheat entries with hotkeys assigned will be used');
  end;

  edtCaptionChange(edtCaption);
  trainerform.SaveCurrentStateasDesign;
  }
end;

procedure TfrmTrainerGenerator.FormShow(Sender: TObject);
var
  br: trect;
begin
  if trainerform<>nil then
  begin
    trainerform.show;
    if LCLIntf.GetWindowRect(frmTrainerGenerator.handle, br)>0 then
    begin
      trainerform.left:=br.Right+5;
      trainerform.top:=br.top;
    end;
  end;
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

procedure TfrmTrainerGenerator.miEditHotkeyClick(Sender: TObject);
var mh: TMemoryrecordhotkey;
  mr: TMemoryrecord;
begin
  if lvcheats.selected<>nil then
  begin
    mh:=TMemoryrecordhotkey(lvcheats.selected.data);
    mr:=mh.owner;
    editHotkey(mr,mh);

  end;
end;


procedure TfrmTrainerGenerator.Panel2Resize(Sender: TObject);
begin
  lvCheats.Column[1].Width:=lvCheats.clientwidth-lvCheats.Column[0].Width-3;
end;

procedure TfrmTrainerGenerator.RadioButton2Change(Sender: TObject);
begin

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
  if not cbSupportCheatEngine.checked then
    cbSupportCheatEngine.checked:=true;

  cleanProcessList(comboProcesslist.items);

  closeaction:=cafree;
  frmTrainerGenerator:=nil;

  if trainerform<>nil then
    trainerform.hide;
end;

procedure TfrmTrainerGenerator.Button4Click(Sender: TObject);
begin




end;

procedure TfrmTrainerGenerator.Button3Click(Sender: TObject);
begin
  zeromemory(@popupkeys,sizeof(TKeyCombo));
  edtPopupHotkey.Text:=ConvertKeyComboToString(popupkeys);
  edtPopupHotkey.SetFocus;
end;





procedure TfrmTrainerGenerator.Button1Click(Sender: TObject);
var hi: HICON;
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
    if messagedlg('Are you sure?', mtConfirmation, [mbyes,mbno],0)=mryes then
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
begin
  trainerform.active:=false;
  trainerform.SaveCurrentStateasDesign;


  //add to the script routine an auto attach registration
  if comboprocesslist.text<>'' then
    comboProcesslist.text
  else
    raise exception.create('Please select/type in a processname');

  //erase the --TRAINERGENERATOR part of the luascript
  l:=mainform.frmLuaTableScript.assemblescreen.Lines;
  start:=l.IndexOf('--TRAINERGENERATORSTART--');
  stop:=l.IndexOf('--TRAINERGENERATORSTOP--');

  if ((start=-1) or (stop=-1)) and (start<>stop) then
    raise exception.create('The current lua script only has a half TRAINERGENERATORSTART/TRAINERGENERATORSTOP block. Please fix this first (Removing is the easiest option)');

  if start<>-1 then
    for i:=start to stop do
      l.Delete(start);

  //now write
  l.add('--TRAINERGENERATORSTART--');
  try

    cheatpanel:=TCEPanel(trainerform.FindComponent('CHEATPANEL'));
    if cheatpanel<>nil then
    begin
      //create the routines for these cheats
      l.add('addresslist=getAddressList()');

      //fill the memrec list
      for i:=0 to mainform.addresslist.count-1 do
        if mainform.addresslist.MemRecItems[i].hasHotkeys then
          l.add('memrec'+inttostr( mainform.addresslist.MemRecItems[i].id)+'=addresslist_getMemoryRecordByID(addresslist,'+inttostr(mainform.addresslist.MemRecItems[i].id)+')');

      l.add('');

      //fill the hotkey list
      for i:=0 to lvCheats.Items.Count-1 do
      begin
        currenthk:=TMemoryRecordHotkey(lvcheats.Items[i].Data);
        currentmr:=currenthk.owner;

        memrecname:='memrec'+inttostr(currentmr.id);
        hotkeyname:=memrecname+'_hotkey'+inttostr(currenthk.id);
        l.add(hotkeyname+'=memoryrecord_getHotkeyByID('+memrecname+','+inttostr(currenthk.id)+')');
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
              l.add('--executed after the "toggle*" cheat got executed so');
              l.add('  local memrec=memoryrecordhotkey_getOwner(Hotkey)');
              l.add('  local isActive=memoryrecord_isActive(memrec) --get the state after the hotkey got triggered');
              l.add('  cheatcomponent_setActive('+trainerform.name+'_'+currentcheat.name+', isActive)');
              l.add('  if gBeepOnAction then');
              l.add('    beep()');
              l.add('  end');
              l.add('end');
              l.add('');


              l.add('memoryrecordhotkey_onPostHotkey('+hotkeyname+','+fname+')');
            end;

            else
            begin
              //one time only
              fname:='onHotkey'+inttostr(currentcheat.cheatnr);
              l.Add('function '+fname+'(Hotkey)');
              l.add('  cheatcomponent_setActive('+trainerform.name+'_'+currentcheat.name+', isActive, 1500)');
              l.add('  if gBeepOnAction then');
              l.add('    beep()');
              l.add('  end');
              l.add('end');
              l.add('');

              l.add('memoryrecordhotkey_afterHotkey('+hotkeyname+','+fname+')');
            end;



          end;
        end;
      end;
    end
    else
      showmessage('Tip: You don''t have to use the trainer generator if you don''t want to. You can just save your table as .EXE or CETRAINER');

    seperator:=TCESplitter(trainerform.FindComponent('SEPERATOR'));
    if seperator<>nil then
      l.Add('control_setVisible('+trainerform.name+'_SEPERATOR, false)');


    l.add('');
    l.add('strings_add(getAutoAttachList(), "'+comboProcesslist.text+'")');


    if (cbPopupOnKeypress.checked) and (edtPopupHotkey.text<>'') then
    begin
      l.add('function popupTrainerHotkeyFunction()');
      l.add('  form_show('+trainerform.Name+')');
      l.add('end');


      keyparams:='';
      for i:=0 to length(popupkeys)-1 do
      begin
        if popupkeys[i]=0 then break;
        keyparams:=keyparams+inttostr(popupkeys[i])+',';
      end;

      if keyparams<>'' then
        keyparams:=copy(keyparams, 1,length(keyparams)-1);

      l.add('registerHotkey(popupTrainerHotkeyFunction, '+keyparams+')');
      l.add('timer_setInterval(getFreezeTimer(),'+edtFreezeInterval.text+')');
    end;

    if cbBeepOnAction.checked then
      l.add('gBeepOnAction=true')
    else
      l.add('gBeepOnAction=false');

    l.add('form_show('+trainerform.Name+')');

    if mAbout.lines.count>0 then
    begin
      l.add('function AboutClick()');
      l.add('  showMessage(gAboutText)');
      l.add('end');

      if mabout.lines.Count>0 then
        s:=mAbout.lines[0]
      else
        s:='';

      l.add('gAboutText=[['+s);

      for i:=1 to mabout.lines.Count-1 do
      begin
        if i<mabout.lines.Count-1 then
          l.add(mAbout.lines[i])
        else
          l.add(mAbout.lines[i]+']]');
      end;

      l.add('');
    end;


    l.add('function CloseClick()');
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
        if TLuafile(mainform.LuaFiles[i]).name='TRAINERXM' then
        begin
          TLuafile(mainform.LuaFiles[i]).free;
          mainform.LuaFiles.Delete(i);
          break;
        end;

      mainform.LuaFiles.add(TLuafile.create('TRAINERXM',f));
      f.free;

      l.add('');
      l.add('XMFILE=findTableFile(''TRAINERXM'')');
      l.add('xmplayer_playXM(XMFILE)');
      l.add('');

      if rbStopWhenAttached.checked then
      begin
        l.add('function onOpenProcess(processid)');
        l.add('  xmplayer_stop()');
        l.add('end');
      end
      else
      begin
        l.add('function focusCheck(sender)');
        l.add('  if (form_isForegrounwindow('+trainerform.Name+')) then');
        l.add('    if (xmplayer_isPlaying()==false) then');
        l.add('      xmplayer_resume()');
        l.add('    end');
        l.add('  else');
        l.add('    if (xmplayer_isPlaying()) then');
        l.add('      xmplayer_pause()');
        l.add('    end');
        l.add('  end');
        l.add('end');

        l.add('');
        l.add('focusTimer=createTimer()');
        l.add('timer_onTimer(focuscheck)');
        l.add('timer_setInterval(focustimer, 250)');
        l.add('control_setEnabled(focustimer, true)');
      end;


    end;

    if not cbSupportCheatEngine.checked then
    begin
      if adconfig<>nil then
      begin
        l.add('supportCheatEngine('+trainerform.name+','+BoolToStr(adconfig.cbCanClose.checked,'true','false')+','+adconfig.edtWidth.text+','+adconfig.edtHeight.text+','+inttostr(adconfig.adposition)+', '+adconfig.ownurl+','+adconfig.extraparam+','+inttostr(adconfig.percentage)+')');
        l.add('--Thank you from Dark Byte--');
      end;
    end;

  finally
    l.add('--TRAINERGENERATORSTOP--');
  end;
end;

procedure TfrmTrainerGenerator.Button5Click(Sender: TObject);
var f: string;
    protect: boolean;
begin
  generateScript;

  case cbOutput.ItemIndex of
    0:
    begin
      if not EXESaveDialog.Execute then exit;
      f:=EXESaveDialog.FileName;
      protect:=true;
    end;

    1:
    begin
      if not CETRAINERSaveDialog.Execute then exit;
      f:=CETRAINERSaveDialog.FileName;
      protect:=cbProtect.checked;
    end;

    2:
    begin
      if not CTSaveDialog.Execute then exit;
      f:=CTSaveDialog.FileName;
      protect:=cbProtect.checked;
    end;

  end;


  savetable(f, protect);  //always protect

end;

procedure TfrmTrainerGenerator.btnDesignFormClick(Sender: TObject);
begin
  if btnDesignForm.tag=0 then
  begin
    mainform.createFormdesigner;

    formdesigner.designForm(trainerform);
    formdesigner.show;

    trainerform.show;

    edtCaption.enabled:=false;

    btnDesignForm.caption:='Go back to generated designer';
  end
  else
  begin
    btnDesignForm.tag:=0;
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


    l.Caption:='Cheat Entries';
    l.label1.caption:='Select the cheat entry you want to set the hotkey for';
    l.itemindex:=0;

    if (l.showmodal=mrok) and (l.itemindex<>-1) then
      edithotkey(mainform.addresslist.MemRecItems[l.itemindex], nil);


    s.free;


  end
  else
    raise exception.create('You need a cheat table with cheat entries');
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

end;

procedure TfrmTrainerGenerator.cbStopPlayingChange(Sender: TObject);
begin
  rbStopWhenAttached.enabled:=cbPlayXM.checked and cbStopPlaying.checked;
  rbStopWhenFocusLost.enabled:=cbPlayXM.checked and cbStopPlaying.checked;
end;

procedure TfrmTrainerGenerator.RestoreSupportCE(sender: tobject);
begin
  cbSupportCheatEngine.caption:='Don''t support Cheat Engine (or yourself)';
  if restoretimer<>nil then
    restoretimer.enabled:=false;
end;

procedure TfrmTrainerGenerator.cbSupportCheatEngineChange(Sender: TObject);
begin
  //Guilt procedure
  if not cbSupportCheatEngine.checked then
  begin
    cbSupportCheatEngine.caption:='Thank you! :)';
    //show the ad config window

    if adwindow=nil then
       adwindow:=TADWindow.CreateNew(trainerform, true);

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
    cbSupportCheatEngine.caption:='aaaaw :(';
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

