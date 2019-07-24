unit AdvancedOptionsUnit;

{$MODE Delphi}

{
This unit could use a big update
}
interface

uses
  jwawindows, windows, symbolhandler,symbolhandlerstructs,{tlhelp32,}LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,CEDebugger, Menus,CEFuncProc, ExtCtrls,disassembler,
  SyncObjs,registry, ComCtrls, LResources,NewKernelHandler{$ifdef windows},win32proc{$endif};



type

  { TAdvancedOptions }

  TAdvancedOptions = class(TForm)
    aoImageList: TImageList;
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
    Codelist2: TListView;
    procedure Button3Click(Sender: TObject);
    procedure Codelist2Resize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
    procedure Codelist2DblClick(Sender: TObject);
    procedure Panel2Resize(Sender: TObject);
    procedure Codelist2CustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    { Private declarations }
    plabel:string;

    red: boolean;
    oldpausestate: boolean;
    loadedFormPosition: boolean;
    procedure hotkey(var Message: TMessage); message WM_HOTKEY;
  public
    { Public declarations }
    pausehotkeystring: string;
    pausedbyhotkey: boolean;

    reader: boolean;
    numberofcodes: integer;
    code: array of record
            before: array of byte;
            actualopcode: array of byte;
            after: array of byte;
            changed:boolean;
//            modulename: string;
//            offset: dword;
            symbolname: string;
//            Address: ptrUint; //in case module offsets dont work
          end;

    function AddToCodeList(address: ptrUint; sizeofopcode: integer;changed: boolean; multiadd: boolean=false):boolean;
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
  processhandlerunit,
  DBK32functions,
  globals;




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


resourcestring
  stralreadyinthelist = 'This byte is already part of another opcode already present in the list';
  strPartOfOpcodeInTheList='At least one of these bytes is already in the list';
  strAddressAlreadyInTheList='This address is already in the list';
  strCECode='Code:';
  strNameCECode='What name do you want to give this code?';
  strChangeOf='Change of ';
  strCode='Code :';
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
begin
  //check if the address is already in the list
  for i:=0 to numberofcodes-1 do
  begin
    //if (code[i].Address=address) then raise exception.create(strAddressAlreadyInTheList);

    //I want to see if address to address+sizeofopcode-1 is overlapping with addresses[i] to length(actualopcode[i])-1
    try
      starta:=symhandler.getAddressFromName(code[i].symbolname);
    except
      continue;
    end;

    stopa:=starta+length(code[i].actualopcode)-1;

    startb:=address;
    stopb:=address+sizeofopcode-1;

    if ((starta>startb) and (starta<stopb)) or
       ((startb>starta) and (startb<stopa)) then
      if sizeofopcode=1 then
        raise exception.Create(stralreadyinthelist)
      else
        raise exception.Create(strPartOfOpcodeInTheList);

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


  inc(numberofcodes);
  setlength(advancedoptions.code,numberofcodes);

  //before
  bread:=0;
  toread:=5;
  toread2:=5;
  while bread<toread do
  begin
    toread:=toread2;
    readprocessmemory(processhandle,pointer(address-5+(5-toread)),addr(backupbytes[0]),toread,bread);
    if bread=toread then
    begin

      setlength(AdvancedOptions.code[numberofcodes-1].before,toread);
      for i:=0 to toread-1 do AdvancedOptions.code[numberofcodes-1].before[i]:=backupbytes[i];
    end;
    dec(toread2);
  end;

  //actualopcode

  setlength(AdvancedOptions.code[numberofcodes-1].actualopcode,sizeofopcode);
  readprocessmemory(processhandle,pointer(address),addr(AdvancedOptions.code[numberofcodes-1].actualopcode[0]),sizeofopcode,bread);

  //after
  readprocessmemory(processhandle,pointer(address+sizeofopcode),@backupbytes[0],5,bread);

  setlength(AdvancedOptions.code[numberofcodes-1].after,bread);
  for i:=0 to bread-1 do AdvancedOptions.code[numberofcodes-1].after[i]:=backupbytes[i];

  code[numberofcodes-1].changed:=changed;

  if ssctrl in GetKeyShiftState then
    code[numberofcodes-1].symbolname:=symhandler.getNameFromAddress(address,true,true)
  else
    code[numberofcodes-1].symbolname:=symhandler.getNameFromAddress(address,false,true);

  li:=codelist2.Items.Add;
  li.Caption:=code[numberofcodes-1].symbolname;

  li.SubItems.Add(newstring);

  mainform.editedsincelastsave:=true;
end;


procedure TAdvancedOptions.Codelist2Resize(Sender: TObject);
begin

end;

procedure TAdvancedOptions.Button3Click(Sender: TObject);
begin

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

procedure TAdvancedOptions.miDBVMFindWhatCodeAccessesClick(Sender: TObject);
begin
  try
    MemoryBrowser.DBVMFindWhatThisCodeAccesses(symhandler.getAddressFromName(code[codelist2.ItemIndex].symbolname));
  except
  end;
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
  if (codelist2.Items.Count=0) or (codelist2.ItemIndex=-1) then
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

    if code[codelist2.itemindex].changed then
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
        offset:=symhandler.getAddressFromName(code[codelist2.itemindex].symbolname, false);
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

  miDBVMFindWhatCodeAccesses.Enabled:=isIntel and isDBVMCapable and Findoutwhatthiscodechanges1.enabled;
  miDBVMFindWhatCodeAccesses.Caption:='DBVM '+Findoutwhatthiscodechanges1.Caption;
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
  for i:=0 to codelist2.items.Count-1 do
  begin
    if not codelist2.Items[i].Selected then continue;

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
        codelist2.Repaint;
        exit;
      end;
    end;


    //set to read and write
    vpe:=(SkipVirtualProtectEx=false) and VirtualProtectEx(processhandle,pointer(Address),length(code[i].actualopcode),PAGE_EXECUTE_READWRITE,original);  //I want to execute this, read it and write it. (so, full access)

    //write
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

    FlushInstructionCache(processhandle,pointer(Address),lengthactualopcode);

    code[i].changed:=false;
  end;

  codelist2.Repaint;

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
  for index:=0 to codelist2.items.Count-1 do
  begin
    if not codelist2.items[index].Selected then continue;

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

    FlushInstructionCache(processhandle,pointer(a),codelength);

    code[index].changed:=true;
  end;
  codelist2.Repaint;
end;

procedure TAdvancedOptions.Remove1Click(Sender: TObject);
var i,j,index: integer;
  multidelete: boolean;
begin
  multidelete:=codelist2.SelCount>1;
  if multidelete then
    if messagedlg(rsAreYouSureYouWishToDeleteTheseEntries, mtConfirmation, [mbyes, mbno], 0) <> mryes then exit;


  codelist2.Items.BeginUpdate;
  try
    while codelist2.SelCount>0 do
    begin
      index:=codelist2.Selected.Index;
      if (index=-1) or (codelist2.Items.Count=0) then exit;

      if not multidelete then
        if messagedlg(rsDelete+' '+codelist2.Items[index].SubItems[0]+' ?', mtConfirmation, [mbyes, mbno], 0) <> mryes then exit;


      setlength(code[index].before,0);
      setlength(code[index].actualopcode,0);
      setlength(code[index].after,0);

      for i:=index to numberofcodes-2 do
      begin
        code[i].before:=code[i+1].before;
        code[i].actualopcode:=code[i+1].actualopcode;
        code[i].after:=code[i+1].after;
        code[i].symbolname:=code[i+1].symbolname;
        code[i].changed:=code[i+1].changed;
      end;

      dec(numberofcodes);
      setlength(code,numberofcodes);

      codelist2.Items.Delete(index);
    end;

  finally
    codelist2.Items.endUpdate;
  end;

end;

procedure TAdvancedOptions.Findoutwhatthiscodechanges1Click(
  Sender: TObject);
begin

  try
    MemoryBrowser.FindWhatThisCodeAccesses(symhandler.getAddressFromName(code[codelist2.ItemIndex].symbolname));
  except
  end;
end;

procedure TAdvancedOptions.Rename1Click(Sender: TObject);
var index: integer;
begin
  index:=codelist2.ItemIndex;
  codelist2.Items[index].SubItems[0]:=inputbox(rsNewName, rsGiveTheNewNameOfThisEntry, codelist2.Items[index].SubItems[0]);
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
  for j:=0 to codelist2.Items.Count-1 do
  begin
    index:=j;
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

    FlushInstructionCache(processhandle,pointer(a),codelength);

    code[index].changed:=true;
  end;
  codelist2.Repaint;
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
      codelist2.OnCustomDrawItem:=nil;
  {$endif}
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

procedure TAdvancedOptions.Codelist2DblClick(Sender: TObject);
var mi: TModuleInfo;
begin

  if codelist2.itemindex<>-1 then
  begin
    try
      memorybrowser.disassemblerview.SelectedAddress:=symhandler.getAddressFromName(code[codelist2.itemindex].symbolname);

      if memorybrowser.Height<(memorybrowser.Panel1.Height+100) then memorybrowser.height:=memorybrowser.Panel1.Height+100;
      memorybrowser.panel1.visible:=true;
      memorybrowser.show;

    except
    end;
  end;

end;

procedure TAdvancedOptions.Panel2Resize(Sender: TObject);
begin
  label1.left:=(panel2.Width div 2)-(label1.Width div 2);
end;

procedure TAdvancedOptions.Codelist2CustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if code[item.index].changed then sender.Canvas.Font.Color:=clred;
end;

initialization
  {$i AdvancedOptionsUnit.lrs}

end.
