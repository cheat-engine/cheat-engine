unit AdvancedOptionsUnit;

interface

uses
  symbolhandler,tlhelp32,Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,debugger, Menus,cefuncproc, ExtCtrls,disassembler,
  SyncObjs,registry, ComCtrls{$ifdef net},netapis{$else},newkernelhandler{$endif};



type
  TAdvancedOptions = class(TForm)
    PopupMenu2: TPopupMenu;
    CC1: TMenuItem;
    CC2: TMenuItem;
    Remove1: TMenuItem;
    Rename1: TMenuItem;
    Findoutwhatthiscodechanges1: TMenuItem;
    Openthedisassemblerhere1: TMenuItem;
    codelist: TListBox;
    Findthiscodeinsideabinaryfile1: TMenuItem;
    OpenDialog1: TOpenDialog;
    N1: TMenuItem;
    N2: TMenuItem;
    SaveDialog1: TSaveDialog;
    Replaceall1: TMenuItem;
    Timer1: TTimer;
    Panel1: TPanel;
    Button1: TButton;
    Button4: TButton;
    Button2: TButton;
    Panel2: TPanel;
    Pausebutton: TSpeedButton;
    SaveButton: TSpeedButton;
    Label1: TLabel;
    procedure codelistDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormShow(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure CC2Click(Sender: TObject);
    procedure CC1Click(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure Findoutwhatthiscodechanges1Click(Sender: TObject);
    procedure Rename1Click(Sender: TObject);
    procedure Openthedisassemblerhere1Click(Sender: TObject);
    procedure Findthiscodeinsideabinaryfile1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure PausebuttonClick(Sender: TObject);
    procedure PausebuttonMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure codelistMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Replaceall1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure codelistKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    plabel:string;

    red: boolean;

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
            modulename: string;
            offset: dword;
            Address: dword; //in case module offsets dont work
          end;

    function AddToCodeList(address: Dword; sizeofopcode: integer; changed:boolean):boolean;
    procedure UpdateAdvancedOptions;
  end;

procedure unpause;

var
  AdvancedOptions: TAdvancedOptions;

resourcestring
  strnotreadable='This address is not readable';
  strNotWhatitshouldbe='The memory at this address is''nt what it should be! Continue?';

implementation

uses {$ifdef net}ceclient,unit2{$else}MainUnit{$endif},
  inputboxtopunit,
  {$ifndef net}
  formChangedAddresses,
  formhotkeyunit,
  frmDissectwindowUnit,
  frmCapturedTimersUnit,
  frmDirectXUnit,
//  frmOpenglUnit,
  {$endif}
  MemoryBrowserFormUnit,
  frmFindCodeInFileUnit,
  standaloneunit,
  formsettingsunit,
  mainunit2;



{$R *.dfm}

procedure unpause;
begin
  if advancedoptions.Pausebutton.Down then
  begin
    advancedoptions.Pausebutton.Down:=false;
    advancedoptions.Pausebutton.Click;
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

procedure TadvancedOptions.UpdateAdvancedOptions;
begin

end;


function TAdvancedOptions.AddToCodeList(address: dword; sizeofopcode: integer;changed: boolean):boolean;
resourcestring
  stralreadyinthelist = 'This byte is already part of another opcode already present in the list';
  strPartOfOpcodeInTheList='At least one of these bytes is already in the list';
  strAddressAlreadyInTheList='This address is already in the list';
  strCECode='Cheat Engine code:';
  strNameCECode='What name do you want to give this code?';
  strChangeOf='Change of ';
  strCode='Code :';
var i: integer;
    bread: dword;
    toread,toread2: dword;
    backupbytes: array[0..4] of byte;
    ignore: string;
    address2:dword;

    starta,stopa,startb,stopb: dword;
    modulename,modulebaseaddress:dword;

    ths: thandle;
    me32:MODULEENTRY32;
    x: pchar;
    canceled: boolean;
    newstring: string;
begin
  //check if the address is already in the list
  for i:=0 to numberofcodes-1 do
  begin
    if (code[i].Address=address) then raise exception.create(strAddressAlreadyInTheList);

    //I want to see if address to address+sizeofopcode-1 is overlapping with addresses[i] to length(actualopcode[i])-1
    starta:=code[i].Address;
    stopa:=code[i].Address+length(code[i].actualopcode)-1;

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


  if changed then newstring:=Inputboxtop(strCECode,strNameCECode, strChangeOf+disassemble(address2,ignore),true,canceled)
             else newstring:=Inputboxtop(strCECode,strNameCECode, strCode+disassemble(address2,ignore),true,canceled);

  result:=not canceled;

  if not result then exit;


  if newstring='' then
    codelist.items.add(strNoDescription)
  else
    codelist.Items.Add(newstring);

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
  code[numberofcodes-1].Address:=address;
  code[numberofcodes-1].modulename:='';
  code[numberofcodes-1].offset:=0;

  //get the module this code is in
  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,processid);
  me32.dwSize:=sizeof(MODULEENTRY32);
  if ths<>0 then
  begin
    try
      if module32first(ths,me32) then
      repeat
        if (address>=dword(me32.modBaseAddr)) and (address<dword(me32.modBaseAddr)+me32.modBaseSize) then
        begin
          x:=me32.szExePath;
          code[numberofcodes-1].modulename:=extractfilename(x);
          code[numberofcodes-1].offset:=address-dword(me32.modBaseAddr);
          break;
        end;
      until not module32next(ths,me32);
    finally
      closehandle(ths);
    end;
  end;
end;


procedure TAdvancedOptions.codelistDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
{copy/paste from example in help}
var
  Bitmap: TBitmap;      { temporary variable for the item’s bitmap }
  Offset: Integer;      { text offset width }
  origcolor: tcolor;
begin
  origcolor:=clBlack;

  with codelist.Canvas do  { draw on control canvas, not on the form }
  begin
    FillRect(Rect);       { clear the rectangle }
    Offset := 2;          { provide default offset }
    Bitmap := TBitmap(codelist.Items.Objects[Index]); { get the bitmap }
    if Bitmap <> nil then
    begin
      Draw(Rect.Left + Offset, Rect.Top, Bitmap); {render bitmap}
      Offset := Bitmap.width + 6;    { add four pixels between bitmap and text}
    end;

    if (code[index].changed) then
    begin
      origcolor:=font.color;
      font.Color:=clRed;
    end;
    TextOut(Rect.Left + Offset, Rect.Top, codelist.Items[Index]);  { display the text }

    if (code[index].changed) then font.color:=origcolor;
  end;
end;

procedure TAdvancedOptions.FormShow(Sender: TObject);
begin

  UpdateAdvancedOptions;
end;

procedure TAdvancedOptions.PopupMenu2Popup(Sender: TObject);
var offset: dword;
    opcode,desc: string;
    fb,nb: integer;
    seperator: integer;

    mi: tmoduleinfo;
resourcestring
  strFindWhatCodeaccesses='Find out what addresses this code accesses';
  strFindWhatCodeReads='Find out what addresses this code reads from';
  strFindWhatCodeWrites='Find out what addresses this code writes to';
begin
  if codelist.ItemIndex=-1 then
  begin
    cc1.visible:=false;
    cc2.visible:=false;
    rename1.visible:=false;
    remove1.Visible:=false;
    Openthedisassemblerhere1.Visible:=false;
    Findoutwhatthiscodechanges1.visible:=false;
    Findthiscodeinsideabinaryfile1.Visible:=false;
  end else
  begin
    rename1.visible:=true;
    remove1.visible:=true;

    Openthedisassemblerhere1.visible:=true;
    Findthiscodeinsideabinaryfile1.Visible:=true;

    if code[codelist.itemindex].modulename<>'' then
    begin
      symhandler.getmodulebyname(code[codelist.itemindex].modulename,mi);
      code[codelist.itemindex].Address:=mi.baseaddress+code[codelist.itemindex].offset;
    end;

    if code[codelist.itemindex].changed then
    begin
      cc1.visible:=false;
      cc2.visible:=true;
      Findoutwhatthiscodechanges1.visible:=false;
    end else
    begin
      cc1.visible:=true;
      cc2.visible:=false;

      //disassemble this address, and see if it a writer or reader
      //if neither grey it out
      offset:=code[codelist.itemindex].Address;
      opcode:=disassemble(offset,desc);

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


      Findoutwhatthiscodechanges1.visible:=true;
    end;
  end;

  {$ifdef net}
  Findoutwhatthiscodechanges1.Visible:=false;


  {$endif}

end;

procedure TAdvancedOptions.CC2Click(Sender: TObject);
var i: integer;
    a,original,written: dword;
    lengthactualopcode: dword;
    temp: array of byte;
    temp2: array of byte;

resourcestring strcouldntrestorecode='Error when trying to restore this code!';
               strnotthesame='The memory at this address isn''t what it should be! Continue?';
begin
  i:=codelist.ItemIndex;

  lengthactualopcode:=length(code[i].actualopcode);
  //read the current list, if it isnt a NOP or the actualopcode give a warning
  setlength(temp,lengthactualopcode);
  setlength(temp2,lengthactualopcode);
  for i:=0 to lengthactualopcode-1 do
    temp[i]:=$90;

  i:=codelist.itemindex;
  readprocessmemory(processhandle,pointer(code[i].Address),@temp2[0],lengthactualopcode,original);
  if original<>lengthactualopcode then
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
      codelist.Repaint;
      exit;
    end;
  end;


  //set to read and write
  VirtualProtectEx(processhandle,pointer(code[i].Address),length(code[i].actualopcode),PAGE_EXECUTE_READWRITE,original);  //I want to execute this, read it and write it. (so, full access)

  //write
  writeprocessmemory(processhandle,pointer(code[i].Address),@code[i].actualopcode[0],length(code[i].actualopcode),written);
  if written<>lengthactualopcode then
  begin
    messagedlg(strCouldntrestorecode,mtWarning,[MBok],0);
    VirtualProtectEx(processhandle,pointer(code[i].Address),lengthactualopcode,original,a);  //ignore a
    exit;
  end;

  //set back
  VirtualProtectEx(processhandle,pointer(code[i].Address),lengthactualopcode,original,a);  //ignore a
  FlushInstructionCache(processhandle,pointer(code[i].Address),lengthactualopcode);

  code[i].changed:=false;

  codelist.Repaint;

end;

procedure TAdvancedOptions.CC1Click(Sender: TObject);
var codelength: integer;
    written: dword;
    i,index: integer;
    nops: array of byte;
    a,b: dword;
    original: dword;
resourcestring strcouldntwrite='The memory at this address couldn''t be written';
begin
  //search dselected in the addresslist

  index:=codelist.itemindex;
  a:=code[index].Address;
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
    nops[i]:=$90;  //$90=nop

 // get old security and set new security
  VirtualProtectEx(processhandle,pointer(a),codelength,PAGE_EXECUTE_READWRITE,original);  //I want to execute this, read it and write it. (so, full access)

  writeprocessmemory(processhandle,pointer(a),@nops[0],codelength,written);
  if written<>dword(codelength) then
  begin
    messagedlg(strcouldntwrite,mtError,[mbok],0);
    exit;
  end;


  //set old security back
  VirtualProtectEx(processhandle,pointer(a),codelength,original,a);  //ignore a

  FlushInstructionCache(processhandle,pointer(a),codelength);

  code[index].changed:=true;
  codelist.Repaint;
end;

procedure TAdvancedOptions.Remove1Click(Sender: TObject);
var i,index: integer;
begin

  index:=codelist.ItemIndex;
  if messagedlg('Delete '+codelist.Items[index]+' ?',mtConfirmation,[mbyes,mbno],0) = mrno then exit;


  setlength(code[index].before,0);
  setlength(code[index].actualopcode,0);
  setlength(code[index].after,0);

  for i:=index to numberofcodes-2 do
  begin
    code[i].before:=code[i+1].before;
    code[i].actualopcode:=code[i+1].actualopcode;
    code[i].after:=code[i+1].after;
    code[i].Address:=code[i+1].Address;
    code[i].changed:=code[i+1].changed;
    code[i].modulename:=code[i+1].modulename;
    code[i].offset:=code[i+1].offset;
  end;

  dec(numberofcodes);
  setlength(code,numberofcodes);  

  codelist.Items.Delete(index);
end;

procedure TAdvancedOptions.Findoutwhatthiscodechanges1Click(
  Sender: TObject);
{var temp: dword;
    int3: byte;
    original: dword;}
var i: integer;
begin
{$ifndef net}
  //New method:
  if not startdebuggerifneeded then exit;
  if debuggerthread.userisdebugging then raise exception.create('You can''t use this function while you are debugging the application yourself. (Close the memory view window)');

  frmChangedAddresses:=TfrmChangedAddresses.Create(self);
  debuggerthread.Suspend;

  debuggerthread.DRRegs.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
  debuggerthread.DRRegs.Dr0:=code[codelist.ItemIndex].Address;
  debuggerthread.DRRegs.Dr7:=reg0set or reg1set or reg2set or reg3set;

  for i:=0 to length(debuggerthread.threadlist)-1 do
  begin
    suspendthread(debuggerthread.threadlist[i][1]);
    if not setthreadcontext(debuggerthread.threadlist[i][1],debuggerthread.DRRegs) then showmessage('failed 1');
    resumethread(debuggerthread.threadlist[i][1]);
  end;


  debuggerthread.breakpointaddress:=code[codelist.ItemIndex].Address;
  debuggerthread.breakpointset:=true;

  debuggerthread.Resume;
  frmChangedAddresses.showmodal;

  {$endif}
end;

procedure TAdvancedOptions.Rename1Click(Sender: TObject);
var index: integer;
begin
  index:=codelist.ItemIndex;
  codelist.Items[index]:=inputbox('New name','Give the new name of this entry',codelist.Items[index]);
end;

procedure TAdvancedOptions.Openthedisassemblerhere1Click(Sender: TObject);
begin
  memorybrowser.Disassembleraddress:=code[codelist.itemindex].Address;
  if memorybrowser.Height<(memorybrowser.Panel1.Height+100) then memorybrowser.height:=memorybrowser.Panel1.Height+100;
  memorybrowser.panel1.visible:=true;
  memorybrowser.show;
end;

procedure TAdvancedOptions.Findthiscodeinsideabinaryfile1Click(
  Sender: TObject);
begin
  formFindCodeInFile:=TformFindCodeInFile.create(self);
  if formFindcodeInfile.ok then formFindCodeInFile.showmodal;
end;

procedure TAdvancedOptions.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TAdvancedOptions.SaveButtonClick(Sender: TObject);
begin
  StandAlone.filename:=SaveDialog1.filename;
  standAlone.showmodal;
end;

procedure TAdvancedOptions.PausebuttonClick(Sender: TObject);
var i: integer;
    ct: _Context;
begin
  {$ifndef net}

  if hypermode<>nil then
  begin
    pausebutton.down:=not pausebutton.down;
    exit;
  end;

  if processhandle=0 then
  begin
    pausebutton.down:=false;
    exit;
  end;


  if pausebutton.down then
  begin
    if processid=getcurrentprocessid then
    begin
      pausebutton.down:=false;
      exit;
    end;


    if (debuggerthread=nil) and (@ntsuspendprocess<>nil) then
    begin
      if ntsuspendprocess(processhandle)<>0 then //failed somehow, try the debugger from now on
      begin
        @ntsuspendprocess:=nil;
        pausebuttonclick(sender);
        exit;
      end;
    end
    else
    begin
      if not startdebuggerifneeded(not pausedbyhotkey) then
      begin
        pausebutton.Down:=false;
        exit;
      end;



      debuggerthread.Suspend;
      for i:=0 to length(debuggerthread.threadlist)-1 do
        suspendthread(debuggerthread.threadlist[i][1]);
    end;
    pausebutton.Hint:='Resume the game'+pausehotkeystring;
    pausebutton.down:=true;

    red:=false;
    mainform.ProcessLabel.font.Color:=clred;

    plabel:=mainform.ProcessLabel.Caption;
    mainform.ProcessLabel.Caption:=mainform.ProcessLabel.Caption+' (paused)';
    timer1.Enabled:=true;
  end
  else
  begin
    //resume
    if (debuggerthread=nil) and (@ntresumeprocess<>nil) then
    begin
      ntresumeprocess(processhandle);
    end
    else
    begin
      for i:=length(debuggerthread.threadlist)-1 downto 0 do
        resumethread(debuggerthread.threadlist[i][1]);

      debuggerthread.Resume;
    end;

    pausebutton.Hint:='Pause the game'+pausehotkeystring;

    timer1.Enabled:=false;
    mainform.ProcessLabel.Font.Color:=clMenuText;
    mainform.ProcessLabel.Caption:=plabel;


    pausebutton.Down:=false;
  end;

  {$else}
  //network version
  if pausebutton.down then
  begin
    if not startdebuggerifneeded then
    begin
      pausebutton.Down:=false;
      exit;
    end;

    output[0]:=CS_SuspenProcess;
    sendbuf(1);
    pausebutton.Hint:='Resume the game'+pausehotkeystring;
    pausebutton.down:=true;
  end
  else
  begin
    output[0]:=CS_ResumeProcess;
    sendbuf(1);
    pausebutton.Hint:='Pause the game';
    pausebutton.Down:=false;
  end;
{$endif}
end;

procedure TAdvancedOptions.PausebuttonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if pausebutton.Down then
    pausebutton.hint:='Resume the game'+pausehotkeystring
  else
    pausebutton.hint:='Pause the game'+pausehotkeystring;
end;

procedure TAdvancedOptions.codelistMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var tp: tpoint;
begin
  tp.x:=x;
  tp.y:=y;
  if codelist.ItemAtPos(tp,true)<>-1 then codelist.ItemIndex:=codelist.ItemAtPos(tp,true);
end;

procedure TAdvancedOptions.Replaceall1Click(Sender: TObject);
var codelength: integer;
    written: dword;
    j,i,index: integer;
    nops: array of byte;
    a,b: dword;
    original: dword;
begin
  //search dselected in the addresslist
  for j:=0 to codelist.Items.Count-1 do
  begin
    index:=j;
    if code[index].changed then continue;

    a:=code[index].Address;
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
      nops[i]:=$90;  //$90=nop

   // get old security and set new security
    VirtualProtectEx(processhandle,pointer(a),codelength,PAGE_EXECUTE_READWRITE,original);  //I want to execute this, read it and write it. (so, full access)

    writeprocessmemory(processhandle,pointer(a),@nops[0],codelength,written);
    if written<>dword(codelength) then
    begin
      messagedlg('The memory at this address couldn''t be written',mtError,[mbok],0);
      exit;
    end;


    //set old security back
    VirtualProtectEx(processhandle,pointer(a),codelength,original,a);  //ignore a

    FlushInstructionCache(processhandle,pointer(a),codelength);

    code[index].changed:=true;
  end;
  codelist.Repaint;
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

procedure TAdvancedOptions.Button4Click(Sender: TObject);
var i:integer;
    fname,expectedFilename: string;
    oldtitle: string;
resourcestring StrSelectExeFor3D='Select the executable of the Direct-3D game';
begin
  {$ifndef net}
  oldtitle:=opendialog1.Title;
  opendialog1.Title:=StrSelectExeFor3D;

  if Opendialog1.Execute then
  begin
    hyperscanview.HookDirect3d:=true;
    hyperscanview.asktocontinue:=true;

    KeysFileMapping:=CreateFileMapping($FFFFFFFF,nil,PAGE_READWRITE,0,sizeof(tkeys),'CEKEYS');
    if KeysFileMapping=0 then
      raise exception.Create('Error while trying to create the shared key structure! (Which efficiently renders this whole feature useless)');

    keys:=MapViewOfFile(KeysFileMapping,FILE_MAP_ALL_ACCESS,0,0,0);
    if keys=nil then
    begin
      closehandle(KeysFileMapping);
      raise exception.Create('Cheat Engine failed to get into the config of the selected program.');
    end;

    keys.configured:=false;


    HyperscanView.HookDirect3d:=true;
    HyperscanView.HookOpenGL:=false;


    unpause;
    detachIfPossible;
    if Uppercase(extractfileext(opendialog1.FileName))<>'.EXE' then raise Exception.Create('You can only load EXE files');

    Debuggerthread:=TDebugger.MyCreate(opendialog1.FileName);

    while (debuggerthread<>nil) and (debuggerthread.attaching) do sleep(1);

    mainForm.ProcessLabel.caption:=IntToHex(processid,8)+'-'+ExtractFileName(opendialog1.FileName);


    mainform.debugproc:=true;

    if formsettings.cbBreakOnAttach.checked then
      memorybrowser.show;

    mainform.enablegui(false);

    with TFrmDirectx.Create(self) do show;
  end;

  opendialog1.title:=oldtitle;
  {$endif}
end;

procedure TAdvancedOptions.FormCreate(Sender: TObject);
begin
  {$ifdef net}
  button4.Visible:=false;
  savebutton.Visible:=false;
  pausebutton.Left:=savebutton.Left;
  {$endif}
end;

procedure TAdvancedOptions.Button2Click(Sender: TObject);
var i:integer;
    fname,expectedFilename: string;
    oldtitle: string;
resourcestring StrSelectExeForOpenGL3D='Select the executable of the OpenGL game';
begin
  {$ifndef net}
  oldtitle:=opendialog1.Title;
  opendialog1.Title:=StrSelectExeForOpenGL3D;

  if Opendialog1.Execute then
  begin
    hyperscanview.HookDirect3d:=true;
    hyperscanview.asktocontinue:=true;

    KeysFileMapping:=CreateFileMapping($FFFFFFFF,nil,PAGE_READWRITE,0,sizeof(tkeys2),'CEKEYS2');
    if KeysFileMapping=0 then
      raise exception.Create('Error while trying to create the shared key structure! (Which efficiently renders this whole feature useless)');

    keys2:=MapViewOfFile(KeysFileMapping,FILE_MAP_ALL_ACCESS,0,0,0);
    if keys2=nil then
    begin
      closehandle(KeysFileMapping);
      raise exception.Create('Cheat Engine failed to get into the config of the selected program.');
    end;

    keys2.configured:=false;

    HyperscanView.HookDirect3d:=false;
    HyperscanView.HookOpenGL:=true;

    unpause;
    detachIfPossible;
    if Uppercase(extractfileext(opendialog1.FileName))<>'.EXE' then raise Exception.Create('You can only load EXE files');

    Debuggerthread:=TDebugger.MyCreate(opendialog1.FileName);

    while (debuggerthread<>nil) and (debuggerthread.attaching) do sleep(1);

    mainForm.ProcessLabel.caption:=IntToHex(processid,8)+'-'+ExtractFileName(opendialog1.FileName);


    mainform.debugproc:=true;

    if formsettings.cbBreakOnAttach.checked then
      memorybrowser.show;

    mainform.enablegui(false);

//    with TfrmOpenGL.Create(self) do show;
  end;

  opendialog1.title:=oldtitle;
  {$endif}
end;

procedure TAdvancedOptions.Panel1Resize(Sender: TObject);
begin
  button1.Left:=panel1.Width div 2 - button1.width div 2;
end;

procedure TAdvancedOptions.codelistKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=vk_delete then
    Remove1.Click;
end;

end.
