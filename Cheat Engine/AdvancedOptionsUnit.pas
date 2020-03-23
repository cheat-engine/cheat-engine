unit AdvancedOptionsUnit;

{$MODE Delphi}

{
This unit could use a big update
}
interface

uses
  {$ifdef darwin}
  macport, LCLType, math,
  {$endif}
  {$ifdef windows}
  jwawindows, windows,
  {$endif}

  symbolhandler,symbolhandlerstructs,LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,CEDebugger, Menus,CEFuncProc, ExtCtrls,disassembler,
  SyncObjs,registry, ComCtrls, LResources,NewKernelHandler{$ifdef windows},win32proc{$endif};



type

  { TAdvancedOptions }

  TCodeRecord=class
  private
  public
    before: array of byte;
    actualopcode: array of byte;
    after: array of byte;
    changed:boolean;
    symbolname: string;
  end;

  TCodeListEntry=class
  private
  public
    code: TCodeRecord; //nil if header

    color: TColor;
    constructor create;
    destructor destroy; override;
  end;

  TAdvancedOptions = class(TForm)
    aoImageList: TImageList;
    ColorDialog1: TColorDialog;
    miNewGroup: TMenuItem;
    miSetColor: TMenuItem;
    N4: TMenuItem;
    miDBVMFindWhatCodeAccesses: TMenuItem;
    PopupMenu2: TPopupMenu;
    miReplaceWithNops: TMenuItem;
    miRestoreWithOriginal: TMenuItem;
    Remove1: TMenuItem;
    Rename1: TMenuItem;
    Findoutwhatthiscodechanges1: TMenuItem;
    Openthedisassemblerhere1: TMenuItem;
    OpenDialog1: TOpenDialog;
    N1: TMenuItem;
    N2: TMenuItem;
    SaveDialog1: TSaveDialog;
    Replaceall1: TMenuItem;
    Timer1: TTimer;
    Panel1: TPanel;
    Button1: TButton;
    Panel2: TPanel;
    Pausebutton: TSpeedButton;
    Label1: TLabel;
    N3: TMenuItem;
    lvCodelist: TListView;
    procedure Button3Click(Sender: TObject);
    procedure lvCodelistAdvancedCustomDraw(Sender: TCustomListView;
      const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure lvCodelistAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure lvCodelistDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lvCodelistDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lvCodelistResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvCodelistStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure miNewGroupClick(Sender: TObject);
    procedure miSetColorClick(Sender: TObject);
    procedure miDBVMFindWhatCodeAccessesClick(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure miRestoreWithOriginalClick(Sender: TObject);
    procedure miReplaceWithNopsClick(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure Findoutwhatthiscodechanges1Click(Sender: TObject);
    procedure Rename1Click(Sender: TObject);
    procedure Findthiscodeinsideabinaryfile1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure PausebuttonClick(Sender: TObject);
    procedure PausebuttonMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Replaceall1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure lvCodelistDblClick(Sender: TObject);
    procedure lvCodelistCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    { Private declarations }
    plabel:string;

    red: boolean;
    oldpausestate: boolean;
    loadedFormPosition: boolean;

    CurrentlyDraggedOverItem: TListItem;
    CurrentlyDraggedOverBefore, CurrentlyDraggedOverAfter: boolean;

    procedure hotkey(var Message: TMessage); {$ifdef windows}message WM_HOTKEY;{$endif}
    function getCount: integer;
    function getCode(index: integer): TCodeRecord;
    function getEntry(index: integer): TCodeListEntry;
    function getSelected(index: integer): boolean;
    procedure setSelected(index: integer; state: boolean);
  public
    { Public declarations }
    pausehotkeystring: string;
    pausedbyhotkey: boolean;

    reader: boolean;
    function AddToCodeList(address: ptrUint; sizeofopcode: integer;changed: boolean; multiadd: boolean=false):boolean;
    procedure clear;

  published
    property count: integer read getCount;
    property code[index: integer]: TCodeRecord read getCode;  //todo: change to code later
    property entries[index: integer]: TCodeListEntry read getEntry;  //todo: change to code later
    property selected[index: integer]: boolean read getSelected write setSelected;
    property CodeList2: TListView read lvCodelist; //backward compatibility
  end;

procedure unpause;

var
  AdvancedOptions: TAdvancedOptions;

resourcestring
  strnotreadable='This address is not readable';
  strNotWhatitshouldbe='The memory at this address is''nt what it should be! Continue?';


implementation

uses MainUnit, MemoryBrowserFormUnit,
  inputboxtopunit,

  formChangedAddresses,
  formhotkeyunit,
  {frmDissectwindowUnit,
  frmCapturedTimersUnit,
  frmDirectXUnit,
  frmFindCodeInFileUnit,
  standaloneunit,}
  formsettingsunit,
  MainUnit2,
  ProcessHandlerUnit,
  {$ifdef windows}
  DBK32functions,
  {$endif}
  globals;


resourcestring
  stralreadyinthelist = 'This byte is already part of another opcode already present in the list';
  strPartOfOpcodeInTheList='At least one of these bytes is already in the list';
  strAddressAlreadyInTheList='This address is already in the list';
  strCECode='Code:';
  strNameCECode='What name do you want to give this code?';
  strChangeOf='Change of ';
  strCode='Code :';

constructor TCodeListEntry.create;
begin
  color:=clWindowText;
end;

destructor TCodeListEntry.destroy;
begin
  if code<>nil then
    freeandnil(code);
  inherited destroy;
end;


procedure unpause;
begin
  if advancedoptions<>nil then
  begin
    if advancedoptions.Pausebutton.Down then
    begin
      advancedoptions.Pausebutton.Down:=false;
      advancedoptions.Pausebutton.Click;
    end;

  end;
end;


procedure TAdvancedOptions.hotkey(var Message: TMessage);
begin
  if Message.wparam=0 then  //pause
  begin
    pausebutton.down:=not pausebutton.down;
    pausebutton.Click;
  end;
end;


function TAdvancedOptions.getCount: integer;
begin
  result:=lvCodelist.Items.Count;
end;

function TAdvancedOptions.getCode(index: integer): TCodeRecord;
begin
  result:=nil;
  if entries[index]=nil then exit;
  result:=entries[index].code;
end;

function TAdvancedOptions.getEntry(index: integer): TCodeListEntry;
begin
  result:=nil;
  if (index<0) or (index>=count) then exit;

  result:=TCodeListEntry(lvCodelist.Items[index].data);
end;

function TAdvancedOptions.getSelected(index: integer): boolean;
begin
  if (index<0) or (index>=count) then exit(false);
  result:=lvCodelist.Items[index].Selected;
end;

procedure TAdvancedOptions.setSelected(index: integer; state: boolean);
begin
  if (index<0) or (index>=count) then exit;
  lvCodelist.Items[index].Selected:=state;
end;

procedure TAdvancedOptions.clear;
var i: integer;
begin
  for i:=0 to lvCodelist.Items.count-1 do
    TCodeListEntry(lvCodelist.items[i].data).Free;

  lvCodelist.Clear;
end;

function TAdvancedOptions.AddToCodeList(address: ptrUint; sizeofopcode: integer;changed: boolean; multiadd: boolean=false):boolean;

var i: integer;
    bread: PtrUInt;
    toread,toread2: dword;
    backupbytes: array[0..4] of byte;
    ignore: string;
    address2:ptrUint;

    starta,stopa,startb,stopb: ptrUint;
    modulename,modulebaseaddress:ptrUint;

    ths: thandle;
    me32:MODULEENTRY32;
    x: pchar;
    canceled: boolean;
    D,newstring: string;
    li: tlistitem;

    e: TCodeListEntry;
begin
  //check if the address is already in the list


  for i:=0 to lvCodelist.Items.Count-1 do
  begin
    //if (code[i].Address=address) then raise exception.create(strAddressAlreadyInTheList);

    //I want to see if address to address+sizeofopcode-1 is overlapping with addresses[i] to length(actualopcode[i])-1

    e:=TCodeListEntry(lvCodelist.Items[i].Data);
    if e=nil then raise exception.create('Codelist entry '+inttostr(i)+' is not initialized'); //don't translate

    if e.code<>nil then
    begin
      try
        starta:=symhandler.getAddressFromName(e.code.symbolname);
      except
        continue;
      end;

      stopa:=starta+length(e.code.actualopcode)-1;

      startb:=address;
      stopb:=address+sizeofopcode-1;

      if ((starta>startb) and (starta<stopb)) or
         ((startb>starta) and (startb<stopa)) then
        if sizeofopcode=1 then
          raise exception.Create(stralreadyinthelist)
        else
          raise exception.Create(strPartOfOpcodeInTheList);
    end;
  end;


  address2:=address;
  d:=disassemble(address2,ignore);
  splitDisassembledString(d, false, ignore,ignore,d,ignore);

  if changed then
    newstring:=strChangeOf+d
  else
    newstring:=strCode+d;

  if not multiadd then
  begin
    newstring:=Inputboxtop(strCECode,strNameCECode, newstring,true,canceled)
  end
  else
  begin
    canceled:=false;
  end;

  result:=not canceled;

  if not result then exit;

  if newstring='' then newstring:=strNoDescription;

  e:=TCodeListEntry.Create;


  //before
  bread:=0;
  toread:=5;
  toread2:=5;
  e.code:=TCodeRecord.Create;

  while bread<toread do
  begin
    toread:=toread2;
    readprocessmemory(processhandle,pointer(address-5+(5-toread)),addr(backupbytes[0]),toread,bread);
    if bread=toread then
    begin
      setlength(e.code.before,toread);
      for i:=0 to toread-1 do e.code.before[i]:=backupbytes[i];
    end;
    dec(toread2);
  end;

  //actualopcode

  setlength(e.code.actualopcode,sizeofopcode);
  readprocessmemory(processhandle,pointer(address),addr(e.code.actualopcode[0]),sizeofopcode,bread);

  //after
  readprocessmemory(processhandle,pointer(address+sizeofopcode),@backupbytes[0],5,bread);

  setlength(e.code.after,bread);
  for i:=0 to bread-1 do e.code.after[i]:=backupbytes[i];

  e.code.changed:=changed;

  if ssctrl in GetKeyShiftState then
    e.code.symbolname:=symhandler.getNameFromAddress(address,true,true)
  else
    e.code.symbolname:=symhandler.getNameFromAddress(address,false,true);

  li:=lvCodelist.Items.Add;
  li.Caption:=e.code.symbolname;
  li.SubItems.Add(newstring);
  li.Data:=e;

  mainform.editedsincelastsave:=true;
end;


procedure TAdvancedOptions.lvCodelistResize(Sender: TObject);
begin

end;

procedure TAdvancedOptions.Button3Click(Sender: TObject);
begin

end;

procedure TAdvancedOptions.lvCodelistAdvancedCustomDraw(
  Sender: TCustomListView; const ARect: TRect; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin

end;

procedure TAdvancedOptions.lvCodelistAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  c: TColor;
  linerect: trect;
  topfix: integer;
begin
  sender.Canvas.Font.Style:=sender.canvas.font.style-[fsBold];
  sender.Canvas.Font.Color:=TCodeListEntry(item.data).color;

  if (code[item.index]<>nil) and code[item.index].changed then
  begin
    sender.Canvas.Font.Color:=clred;
    sender.Canvas.Font.Style:=sender.canvas.font.style+[fsBold];
  end;

  if CurrentlyDraggedOverItem=item then
  begin
    linerect:=item.DisplayRect(drSelectBounds);

    if CurrentlyDraggedOverBefore then
    begin
      if item.index=0 then
        topfix:=1
      else
        topfix:=0;

      sender.Canvas.Line(0,max(0,linerect.top-1)+topfix,linerect.right,max(0,linerect.top-1)+topfix)
    end
    else
      sender.Canvas.Line(0,linerect.bottom-1,linerect.right,linerect.bottom-1);
  end;
end;

procedure TAdvancedOptions.lvCodelistDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  r: trect;
  selectedItems: tlist;
  i: integer;
  newindex: integer;

  target: TListItem;
  from: TListItem;

  fi, ti: integer;
begin
  if source = sender then
  begin
    CurrentlyDraggedOverItem:=lvCodeList.GetItemAt(x,y);
    if CurrentlyDraggedOverItem<>nil then
    begin
      r:=CurrentlyDraggedOverItem.DisplayRect(drSelectBounds);
      CurrentlyDraggedOverBefore:=y<r.top+(r.Height div 2);
      CurrentlyDraggedOverAfter:=y>r.top+(r.height div 2);

      selectedItems:=tlist.create;
      for i:=0 to lvCodelist.Items.Count-1 do
      begin
        if lvcodelist.Items[i].Selected then
          selecteditems.Add(lvcodelist.Items[i]);
      end;

      target:=CurrentlyDraggedOverItem;


      for i:=0 to selecteditems.Count-1 do
      begin
        from:=tlistitem(selecteditems[i]);
        fi:=from.index;
        ti:=CurrentlyDraggedOverItem.index;

        if from.Index<target.index then
        begin
          if CurrentlyDraggedOverBefore then
            lvCodelist.Items.Move(fi,max(0, ti-1))
          else
            lvCodelist.Items.Move(fi,ti);
        end
        else
        if from.Index=target.index then
        begin
          //nothing
        end
        else
        begin
          if CurrentlyDraggedOverBefore then
            lvCodelist.Items.Move(fi,ti)
          else
            lvCodelist.Items.Move(fi,min(count-1,ti+1));
        end;
      end;



      lvCodelist.ClearSelection;
      for i:=0 to selecteditems.Count-1 do
        tlistitem(selectedItems[i]).Selected:=true;

    end;

  end;

  CurrentlyDraggedOverItem:=nil;
  refresh;
end;

procedure TAdvancedOptions.lvCodelistDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  t: integer;
  r,r2: trect;

begin
  if source=sender then
  begin
    CurrentlyDraggedOverItem:=lvCodeList.GetItemAt(x,y);
    CurrentlyDraggedOverBefore:=false;
    CurrentlyDraggedOverAfter:=false;

    if CurrentlyDraggedOverItem<>nil then
    begin
      r:=CurrentlyDraggedOverItem.DisplayRect(drSelectBounds);

      CurrentlyDraggedOverBefore:=y<r.top+(r.Height div 2);
      CurrentlyDraggedOverAfter:=y>r.top+(r.height div 2);
    end;

    accept:=true;
    lvCodelist.refresh;
  end
  else
  begin
    accept:=false;
    CurrentlyDraggedOverItem:=nil;
  end;
end;

procedure TAdvancedOptions.FormDestroy(Sender: TObject);
begin
  saveformposition(self);
end;

procedure TAdvancedOptions.FormResize(Sender: TObject);
begin

end;

procedure TAdvancedOptions.FormShow(Sender: TObject);
begin
  if loadedFormPosition=false then
  begin
    Position := podesigned;
    Left := mainform.left - Width;
    Top := mainform.Top + mainform.Height - Height;

    if (Left < 0) or (Top + Height > screen.Height) then
    begin
      left := 0;
      Top := screen.Height - advancedoptions.Height;
    end;

    loadedFormPosition:=true;
  end;
end;

procedure TAdvancedOptions.lvCodelistStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
//
end;

procedure TAdvancedOptions.miNewGroupClick(Sender: TObject);
var
  groupname: string;
  groupcount: integer;
  li: TListitem;
  i: integer;
begin
  groupname:='';
  groupcount:=0;
  for i:=0 to count-1 do
    if code[i]=nil then
      inc(groupcount);

  groupname:='Group '+inttostr(groupcount);

  if InputQuery('Code List','New group name:',groupname) then
  begin
    li:=lvCodelist.Items.Add;
    li.data:=TCodeListEntry.create;
    li.caption:=groupname;
  end;
end;

procedure TAdvancedOptions.miSetColorClick(Sender: TObject);
var i: integer;
begin
  if lvCodelist.Selected=nil then exit;

  colordialog1.color:=TCodeListEntry(lvCodelist.selected.Data).color;
  if colordialog1.execute then
  begin
    for i:=0 to count-1 do
      entries[i].color:=colordialog1.Color;
  end;


  lvCodelist.Refresh;
end;

procedure TAdvancedOptions.miDBVMFindWhatCodeAccessesClick(Sender: TObject);
begin
  {$ifdef windows}
  try
    if code[lvCodelist.ItemIndex]<>nil then
      MemoryBrowser.DBVMFindWhatThisCodeAccesses(symhandler.getAddressFromName(code[lvCodelist.ItemIndex].symbolname));
  except
  end;
  {$endif}
end;

resourcestring
  strFindWhatCodeaccesses='Find out what addresses this code accesses';
  strFindWhatCodeReads='Find out what addresses this code reads from';
  strFindWhatCodeWrites='Find out what addresses this code writes to';
procedure TAdvancedOptions.PopupMenu2Popup(Sender: TObject);
var offset: ptrUint;
    opcode,desc: string;
    fb,nb: integer;
    seperator: integer;

    mi: tmoduleinfo;

begin

  miSetColor.visible:=lvCodelist.ItemIndex<>-1;

  if (count=0) or (lvCodelist.ItemIndex=-1) or (code[lvCodelist.itemindex]=nil) then
  begin
    miReplaceWithNops.enabled:=false;
    miRestoreWithOriginal.enabled:=false;
    rename1.enabled:=false;
    remove1.enabled:=false;
    Openthedisassemblerhere1.enabled:=false;
    Findoutwhatthiscodechanges1.enabled:=false;
    Replaceall1.enabled:=false;


  end else
  begin
    rename1.enabled:=true;
    remove1.enabled:=true;

    Replaceall1.enabled:=true;
    Openthedisassemblerhere1.enabled:=true;

    if code[lvCodelist.itemindex].changed then
    begin
      miReplaceWithNops.enabled:=false;
      miRestoreWithOriginal.enabled:=true;
      Findoutwhatthiscodechanges1.enabled:=false;
    end else
    begin
      miReplaceWithNops.enabled:=true;
      miRestoreWithOriginal.enabled:=false;

      //disassemble this address, and see if it a writer or reader
      //if neither grey it out

      try
        offset:=symhandler.getAddressFromName(code[lvCodelist.itemindex].symbolname, false);
        opcode:=disassemble(offset,desc);
      except
        Findoutwhatthiscodechanges1.enabled:=false;
        exit;
      end;

      Findoutwhatthiscodechanges1.Caption:=strFindWhatCodeAccesses;
      Findoutwhatthiscodechanges1.enabled:=false;
      fb:=pos('[',opcode);
      if fb>0 then
      begin
        nb:=pos(']',opcode);
        if nb>fb then //just a simple check to verify the opcode is ok
        begin
          seperator:=pos(',',opcode);
          if seperator>-1 then
          begin
            if seperator<fb then //reader
            begin
              reader:=true;
              FindOutWhatThisCodeChanges1.caption:=strFindWhatCodeReads
            end
            else
            begin
              reader:=false;
              FindOutWhatThisCodeChanges1.caption:=strfindwhatcodewrites;
            end;


            Findoutwhatthiscodechanges1.enabled:=true;
          end;
        end;
      end;





      Findoutwhatthiscodechanges1.enabled:=true;
    end;
  end;

  miDBVMFindWhatCodeAccesses.Enabled:={$ifdef windows}isIntel and isDBVMCapable and Findoutwhatthiscodechanges1.enabled{$else}false{$endif};
  miDBVMFindWhatCodeAccesses.Caption:='DBVM '+Findoutwhatthiscodechanges1.Caption;

  //OutputDebugString('popupmenu2');
end;

resourcestring strcouldntrestorecode='Error when trying to restore this code!';
               strnotthesame='The memory at this address isn''t what it should be! Continue?';
procedure TAdvancedOptions.miRestoreWithOriginalClick(Sender: TObject);
var i,j: integer;
    a,address: ptrUint;
    lengthactualopcode: dword;
    written: PtrUInt;
    original: dword;
    x: dword;
    br: PtrUInt;
    temp: array of byte;
    temp2: array of byte;

    vpe: boolean;

begin
  for i:=0 to lvCodelist.items.Count-1 do
  begin
    if not Selected[i] then continue;
    if code[i]=nil then continue; //header

    try
      address:=symhandler.getAddressFromName(code[i].symbolname);
    except
      continue;
    end;


    lengthactualopcode:=length(code[i].actualopcode);
    //read the current list, if it isnt a NOP or the actualopcode give a warning
    setlength(temp,lengthactualopcode);
    setlength(temp2,lengthactualopcode);
    for j:=0 to lengthactualopcode-1 do
      temp[j]:=$90;

    readprocessmemory(processhandle,pointer(Address),@temp2[0],lengthactualopcode,br);


    if br<>lengthactualopcode then
      raise exception.Create(strNotReadable);

    //check if it is a nop field
    if not comparemem(@temp[0],@temp2[0],lengthactualopcode) then
    begin
      //NO????????

      //then check if it is the actual opcode, and there was a bug
      if not comparemem(@temp[0],@code[i].actualopcode[0],lengthactualopcode) then
      begin
        //It's also not the original opcode? WTF, This dude must be braindeath...
        if messagedlg(strnotthesame,mtWarning,[mbyes,mbno],0)=mrno then exit;
      end
      else
      begin
        code[i].changed:=false;
        lvCodelist.Repaint;
        exit;
      end;
    end;


    //set to read and write
    vpe:=(SkipVirtualProtectEx=false) and VirtualProtectEx(processhandle,pointer(Address),length(code[i].actualopcode),PAGE_EXECUTE_READWRITE,original);  //I want to execute this, read it and write it. (so, full access)

    //write
    written:=0;
    writeprocessmemory(processhandle,pointer(Address),@code[i].actualopcode[0],length(code[i].actualopcode),written);
    if written<>lengthactualopcode then
    begin
      messagedlg(strCouldntrestorecode,mtWarning,[MBok],0);
      if vpe then
        VirtualProtectEx(processhandle,pointer(Address),lengthactualopcode,original,x);
      exit;
    end;

    //set back
    if vpe then VirtualProtectEx(processhandle,pointer(Address),lengthactualopcode,original,x);

    {$ifdef windows}
    FlushInstructionCache(processhandle,pointer(Address),lengthactualopcode);
    {$endif}

    code[i].changed:=false;
  end;

  lvCodelist.Repaint;

end;

resourcestring
strcouldntwrite='The memory at this address couldn''t be written';
rsDelete = 'Delete';
rsNewName = 'New name';
rsGiveTheNewNameOfThisEntry = 'Give the new name of this entry';
rsResumeTheGame = 'Resume the game';
rsPaused = 'paused';
rsPauseTheGame = 'Pause the game';
rsTheMemoryAtThisAddressCouldnTBeWritten = 'The memory at this address couldn''t be written';
rsAreYouSureYouWishToDeleteTheseEntries = 'Are you sure you wish to delete these entries?';


procedure TAdvancedOptions.miReplaceWithNopsClick(Sender: TObject);
var codelength: integer;
    written: PtrUInt;
    i,index: integer;
    nops: array of byte;
    a: ptrUint;
    b: ptruint;
    original: dword;
    vpe: boolean;

begin
  //search dselected in the addresslist
  for index:=0 to lvCodelist.items.Count-1 do
  begin
    if not Selected[index] then continue;
    if code[index]=nil then continue;

    try
      a:=symhandler.getAddressFromName(code[index].symbolname);
    except
      continue;
    end;

    codelength:=length(code[index].actualopcode);

    //read the opcode currently at the address
    setlength(nops,codelength);
    readprocessmemory(processhandle,pointer(a),@nops[0],codelength,b);
    if b<>dword(codelength) then
      raise exception.Create(strNotReadable);

    //compare it with what is in the actualopcode array
    if not comparemem(@nops[0],@code[index].actualopcode[0],codelength) then
      if messagedlg(strNotWhatitshouldbe,mtWarning,[mbyes,mbno],0)=mrno then exit;



    for i:=0 to codelength-1 do
      nops[i]:=$90;  // $90=nop

   // get old security and set new security
    vpe:=(SkipVirtualProtectEx=false) and VirtualProtectEx(processhandle,pointer(a),codelength,PAGE_EXECUTE_READWRITE,original);  //I want to execute this, read it and write it. (so, full access)

    writeprocessmemory(processhandle,pointer(a),@nops[0],codelength,written);
    if written<>dword(codelength) then
    begin
      messagedlg(strcouldntwrite,mtError,[mbok],0);
      exit;
    end;


    //set old security back
    if vpe then
      VirtualProtectEx(processhandle,pointer(a),codelength,original,original);  //ignore a

    {$ifdef windows}
    FlushInstructionCache(processhandle,pointer(a),codelength);
    {$endif}

    code[index].changed:=true;
  end;
  lvCodelist.Repaint;
end;

procedure TAdvancedOptions.Remove1Click(Sender: TObject);
var i,j,index: integer;
  multidelete: boolean;
begin
  if lvCodelist.selected=nil then exit;

  multidelete:=lvCodelist.SelCount>1;
  if multidelete then
  begin
    if dialogs.messagedlg(rsAreYouSureYouWishToDeleteTheseEntries, mtConfirmation, [mbyes, mbno], 0) <> mryes then exit
  end
  else
  begin
    if dialogs.messagedlg(rsDelete+' '+lvCodelist.selected.SubItems[0]+' ?', mtConfirmation, [mbyes, mbno], 0) <> mryes then exit;
  end;

  lvCodelist.Items.BeginUpdate;
  try

    for index:=count-1 downto 0 do
    begin
      if lvCodelist.items[index].Selected then
      begin
        entries[index].destroy;
        lvCodelist.Items.Delete(index);
      end;
    end;


  finally
    lvCodelist.Items.endUpdate;
  end;

end;

procedure TAdvancedOptions.Findoutwhatthiscodechanges1Click(
  Sender: TObject);
begin
  try
    if code[lvCodelist.ItemIndex]<>nil then
      MemoryBrowser.FindWhatThisCodeAccesses(symhandler.getAddressFromName(code[lvCodelist.ItemIndex].symbolname));
  except
  end;
end;

procedure TAdvancedOptions.Rename1Click(Sender: TObject);
var index: integer;
begin
  index:=lvCodelist.ItemIndex;
  lvCodelist.Items[index].SubItems[0]:=inputbox(rsNewName, rsGiveTheNewNameOfThisEntry, lvCodelist.Items[index].SubItems[0]);
end;

procedure TAdvancedOptions.Findthiscodeinsideabinaryfile1Click(
  Sender: TObject);
begin

end;

procedure TAdvancedOptions.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TAdvancedOptions.SaveButtonClick(Sender: TObject);
begin

end;

procedure TAdvancedOptions.PausebuttonClick(Sender: TObject);
var i: integer;
    ct: _Context;

    down: boolean;
    x: dword;
begin
  {$ifdef windows}
  down:=pausebutton.down;
  if down=oldpausestate then exit;

  try
    if processhandle=0 then
    begin
      pausebutton.down:=false;
      exit;
    end;


    if down then
    begin
      if processid=getcurrentprocessid then
      begin
        pausebutton.down:=false;
        exit;
      end;

      if (assigned(ntsuspendprocess)) then
      begin
       // OutputDebugString('Calling ntsuspendProcess');
        if IsValidHandle(processhandle) then
        begin
          x:=ntsuspendProcess(processhandle);
          if (x<>0) and (DBKLoaded) then DBKSuspendProcess(processid);
        end
        else
          if DBKLoaded then
            DBKSuspendProcess(processid);
      end;

       pausebutton.Hint:=rsResumeTheGame+pausehotkeystring;

      red:=false;
      mainform.ProcessLabel.font.Color:=clred;

      plabel:=mainform.ProcessLabel.Caption;
      mainform.ProcessLabel.Caption:=mainform.ProcessLabel.Caption+' ('+rsPaused+')';
      timer1.Enabled:=true;
    end
    else
    if (not down) then
    begin
      //resume
      if assigned(ntresumeprocess) then
      begin
        if IsValidHandle(processhandle) then
        begin
          x:=ntresumeprocess(processhandle);
          if (x<>0) and (DBKLoaded) then DBKResumeProcess(processid);
        end
        else
          if DBKLoaded then
            DBKResumeProcess(processid);
      end;

      pausebutton.Hint:=rsPauseTheGame+pausehotkeystring;

      timer1.Enabled:=false;
      mainform.ProcessLabel.Font.Color:=clMenuText;
      mainform.ProcessLabel.Caption:=plabel;
    end;

  finally
    oldpausestate:=pausebutton.down;
  end;
   {$endif}
end;

procedure TAdvancedOptions.PausebuttonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if pausebutton.Down then
    pausebutton.hint:=rsResumeTheGame+pausehotkeystring
  else
    pausebutton.hint:=rsPauseTheGame+pausehotkeystring;
end;

procedure TAdvancedOptions.Replaceall1Click(Sender: TObject);
var codelength: integer;
    written: PtrUInt;
    j,i,index: integer;
    nops: array of byte;
    a: ptrUint;
    b: PtrUInt;
    original: dword;
    mi: TModuleInfo;
    vpe: boolean;
begin
  //search dselected in the addresslist
  for j:=0 to lvCodelist.Items.Count-1 do
  begin
    index:=j;
    if code[index]=nil then continue;
    if code[index].changed then continue;

    try
      a:=symhandler.getAddressFromName(code[index].symbolname);
    except
      continue;
    end;
    codelength:=length(code[index].actualopcode);

    //read the opcode currently at the address
    setlength(nops,codelength);
    readprocessmemory(processhandle,pointer(a),@nops[0],codelength,b);
    if b<>dword(codelength) then
      raise exception.Create(strNotReadable);

    //compare it with what is in the actualopcode array
    if not comparemem(@nops[0],@code[index].actualopcode[0],codelength) then
      if messagedlg(strNotWhatitshouldbe,mtWarning,[mbyes,mbno],0)=mrno then exit;

    //-------



    for i:=0 to codelength-1 do
      nops[i]:=$90;  //  $90=nop

   // get old security and set new security
    vpe:=(SkipVirtualProtectEx=false) and VirtualProtectEx(processhandle,pointer(a),codelength,PAGE_EXECUTE_READWRITE,original);  //I want to execute this, read it and write it. (so, full access)

    writeprocessmemory(processhandle,pointer(a),@nops[0],codelength,written);
    if written<>dword(codelength) then
    begin
      messagedlg(rsTheMemoryAtThisAddressCouldnTBeWritten, mtError, [mbok], 0);
      exit;
    end;


    //set old security back
    if vpe then
      VirtualProtectEx(processhandle,pointer(a),codelength,original,original);  //ignore a

    {$ifdef windows}
    FlushInstructionCache(processhandle,pointer(a),codelength);
    {$endif}

    code[index].changed:=true;
  end;
  lvCodelist.Repaint;
end;

procedure TAdvancedOptions.Timer1Timer(Sender: TObject);
begin
  if red then
  begin
    mainform.ProcessLabel.Font.Color:=clred;
    red:=false;
  end
  else
  begin
    mainform.ProcessLabel.Font.Color:=clGreen;
    reD:=true;
  end;
end;

resourcestring
  StrSelectExeFor3D='Select the executable of the Direct-3D game';
  rsAOErrorWhileTryingToCreateTheSharedKeyStructureEtc = 'Error while trying to create the shared key structure! (Which efficiently renders this whole feature useless)';
  rsAOCheatEngineFailedToGetIntoTheConfigOfSelectedProgram = 'Cheat Engine failed to get into the config of the selected program.';
  rsAOYouCanOnlyLoadExeFiles = 'You can only load EXE files';

procedure TAdvancedOptions.Button4Click(Sender: TObject);
begin

end;

procedure TAdvancedOptions.FormCreate(Sender: TObject);
var x: array of integer;
begin
  {$ifdef windows}
  {$ifdef cpu64}
    //lazarus bug bypass
    if WindowsVersion=wvVista then
      lvCodelist.OnCustomDrawItem:=nil;
  {$endif}
  {$else}
  Pausebutton.visible:=false;
  {$endif}

 // pausebutton.Left:=savebutton.Left;

  setlength(x,0);
  loadedFormPosition:=loadformposition(self,x);
end;

procedure TAdvancedOptions.Button2Click(Sender: TObject);
begin

end;

procedure TAdvancedOptions.Panel1Resize(Sender: TObject);
begin

end;

procedure TAdvancedOptions.lvCodelistDblClick(Sender: TObject);
var mi: TModuleInfo;
begin
  try
    if code[lvCodelist.itemindex]<>nil then
    begin
      memorybrowser.disassemblerview.SelectedAddress:=symhandler.getAddressFromName(code[lvCodelist.itemindex].symbolname);

      if memorybrowser.Height<(memorybrowser.Panel1.Height+100) then memorybrowser.height:=memorybrowser.Panel1.Height+100;
      memorybrowser.panel1.visible:=true;
      memorybrowser.show;
    end;

  except
  end;
end;

procedure TAdvancedOptions.lvCodelistCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
{var
  c: TColor;
  linerect: trect; }
begin
  {sender.Canvas.Font.Style:=sender.canvas.font.style-[fsBold];
  sender.Canvas.Font.Color:=TCodeListEntry(item.data).color;

  if (code[item.index]<>nil) and code[item.index].changed then
  begin
    sender.Canvas.Font.Color:=clred;
    sender.Canvas.Font.Style:=sender.canvas.font.style+[fsBold];
  end;

  if CurrentlyDraggedOverItem=item then
  begin
    linerect:=item.DisplayRect(drSelectBounds);

    if CurrentlyDraggedOverBefore then
      sender.Canvas.Line(0,max(0,linerect.top-1),linerect.right,max(0,linerect.top-1))
    else
      sender.Canvas.Line(0,linerect.bottom-1,linerect.right,linerect.bottom-1);
  end;}
end;

initialization
  {$i AdvancedOptionsUnit.lrs}

end.
