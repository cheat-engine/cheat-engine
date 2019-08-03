unit savedisassemblyfrm;

{$MODE Delphi}

interface

uses
  LCLIntf, LResources, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, symbolhandler, symbolhandlerstructs, disassembler,
  StdCtrls, ComCtrls, ActnList, Clipbrd, ExtCtrls, strutils, Parsers;

type
  TfrmSavedisassembly = class;
  TSaveDisassemblyThread=class(TThread)
  public
    progressbar: tprogressbar;
    startaddress: ptrUint;
    stopaddress: ptrUint;
    address: boolean;
    bytes: boolean;
    opcode: boolean;
    comment: boolean;
    copymode: boolean;
    filename: string;
    form: TfrmSavedisassembly;
    procedure execute; override;
  end;

  { TfrmSavedisassembly }

  TfrmSavedisassembly = class(TForm)
    Button1: TButton;
    cbAddress: TCheckBox;
    cbBytes: TCheckBox;
    cbOpcode: TCheckBox;
    cbComment: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    SaveDialog1: TSaveDialog;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    SaveDisassemblyThread: TSaveDisassemblyThread;
    FCopyMode: boolean;
    procedure setCopyMode(mode: boolean);
  public
    { Public declarations }
    property copymode: boolean read FCopyMode write setCopyMode;
    procedure waittilldone;
  end;

implementation

uses MemoryBrowserFormUnit, disassemblerComments, ProcessHandlerUnit;

resourcestring
  rsCopyDisassembledOutput = 'Copy disassembled output';
  rsCopy = 'Copy';
  rsSaveDisassembledOutput = 'Save disassembled output';
  rsSave = 'Save';
  rsStopCopying = 'Stop copying';
  rsStopSaving = 'Stop saving';

procedure TSaveDisassemblyThread.execute;
var oldaddress, currentaddress: ptrUint;
    f: textfile;
    temps: string;
    temps2: string;
    addresspart, bytepart, opcodepart, specialpart: string;
    i: integer;
    cpbuf: tstringlist;
    y,z:string;
    mi: TModuleInfo;

    disassembler: TDisassembler;
    desc: string;

    addresslength: integer;
begin
  disassembler:=TDisassembler.Create;
  disassembler.showmodules:=memorybrowser.Showmoduleaddresses1.checked;
  disassembler.showsymbols:=memorybrowser.Showsymbols1.Checked;

  currentaddress:=startaddress;

  if copymode then
  begin
    cpbuf:=tstringlist.Create;

  end
  else
  begin
    cpbuf:=nil;
    assignfile(f,filename);
    rewrite(f);
  end;

  i:=0;
  temps:='';
  temps2:='';

  addresslength:=0;

  while (not terminated) and (currentaddress<=stopaddress) do
  begin
    oldaddress:=currentaddress;
    temps2:=disassembler.disassemble(currentaddress, desc); //contains the addresspart, bytepart and opcode part
    splitDisassembledString(temps2,true,addresspart,bytepart,opcodepart,specialpart);

    if disassembler.showsymbols then
    begin
      addresspart:=symhandler.getNameFromAddress(oldaddress);
    end
    else
    if disassembler.showmodules then
    begin
      //replace the address part with a modulename+offset when possible
      if symhandler.getmodulebyaddress(oldaddress,mi) then
      begin
        y:=inttohex(oldaddress,8);
        z:=mi.modulename+'+'+inttohex(oldaddress-mi.baseaddress,4);

        opcodepart:=stringreplace(opcodepart,y,z,[rfReplaceAll]);
        addresspart:=stringreplace(addresspart,y,z,[rfReplaceAll]);
      end;
    end;

    if addresslength=0 then
      addresslength:=length(addresspart)+1;

    if dassemblercomments<>nil then
      specialpart:=dassemblercomments.comments[oldaddress]
    else
      specialpart:='';

    if specialpart='' then
      specialpart:=disassembler.DecodeLastParametersToString;


    if address then
    begin
      temps:=PadRight(addresspart, addresslength);
      if bytes or opcode then temps:=temps+'- ';
    end;

    if bytes then
    begin
      temps:=temps+padright(bytepart, 21);

      if opcode then
        temps:=temps+' - '

    end;

    if opcode then temps:=temps+opcodepart;

    if (comment) and (specialpart<>'') then temps:=temps+' { '+specialpart+' }';

    if (address or opcode or comment) or (currentaddress>stopaddress) then
    begin
      //each line for address/opcode/comment, and only one time at the and for bytes only
      if copymode then
      begin
        //save to clipboard
        cpbuf.Add(temps);
      end
      else
        writeln(f,temps); //write to file

      temps:=''; //erase the current data
    end;

    progressbar.position:=trunc(currentaddress/stopaddress*1000);

  end;
  if copymode then
  begin
    clipboard.AsText:=cpbuf.GetText;
    cpbuf.free;
  end
  else
  begin
    flush(f);
    closefile(f);
  end;

  disassembler.free;

  if not terminated then postmessage(form.handle,wm_close,0,0);

end;

procedure TfrmSavedisassembly.setCopyMode(mode: boolean);
begin
  FCopyMode:=mode;

  if mode then
  begin
    //configure for copy mode, so instead of saving to file save to clipboard
    caption:=rsCopyDisassembledOutput;
    button1.caption:=rsCopy;
  end
  else
  begin

    caption:=rsSaveDisassembledOutput;
    button1.Caption:=rsSave;
  end;
end;

procedure TfrmSavedisassembly.Button1Click(Sender: TObject);
var startaddress,stopaddress: ptrUint;
begin
  if SaveDisassemblyThread<>nil then
  begin
    SaveDisassemblyThread.Terminate;
    SaveDisassemblyThread.WaitFor;
    freeandnil(SaveDisassemblyThread);
    if FCopyMode then
      button1.Caption:=rsCopy
    else
      button1.Caption:=rsSave;
    exit;
  end;


  try
    startaddress:=StrToQWordEx('$'+edit1.Text);
  except
    startaddress:=symhandler.getAddressFromName(edit1.Text);
  end;

  try
    stopaddress:=StrToQWordEx('$'+edit2.text);
  except
    stopaddress:=symhandler.getAddressFromName(edit2.text);
  end;

  if startaddress>stopaddress then
  begin  //xor swap
    startaddress:=startaddress xor stopaddress;
    stopaddress:=stopaddress xor startaddress;
    startaddress:=startaddress xor stopaddress;
  end;


  if (FCopyMode) or savedialog1.Execute then
  begin
    SaveDisassemblyThread:=TSaveDisassemblyThread.Create(true);
    SaveDisassemblyThread.address:=cbAddress.checked;
    SaveDisassemblyThread.bytes:=cbBytes.Checked;
    SaveDisassemblyThread.opcode:=cbOpcode.Checked;
    SaveDisassemblyThread.comment:=cbComment.Checked;
    SaveDisassemblyThread.startaddress:=startaddress;
    SaveDisassemblyThread.stopaddress:=stopaddress;
    SaveDisassemblyThread.filename:=savedialog1.FileName;
    SaveDisassemblyThread.copymode:=fcopymode;

    SaveDisassemblyThread.form:=self;

    progressbar1.Min:=0;
    progressbar1.Max:=1000;
    progressbar1.Position:=0;
    if not progressbar1.Visible then progressbar1.Visible:=true;

    SaveDisassemblyThread.progressbar:=progressbar1;

    if fcopymode then
      button1.caption:=rsStopCopying
    else
      button1.caption:=rsStopSaving;
      
    SaveDisassemblyThread.start;

  end;
end;

procedure TfrmSavedisassembly.actCancelExecute(Sender: TObject);
begin
  close;
end;

procedure TfrmSavedisassembly.FormDestroy(Sender: TObject);
begin
  if SaveDisassemblyThread<>nil then
  begin
    SaveDisassemblyThread.Terminate;
    SaveDisassemblyThread.WaitFor;
    freeandnil(SaveDisassemblyThread);
  end;
end;

procedure TfrmSavedisassembly.FormShow(Sender: TObject);
begin
    if processhandler.is64bit then
    begin
      //init just once if needed
      if (edit1.Text = '') or (edit2.Text = '') then  // if not initialized
       begin
          edit2.text:='7FFFFFFFFFFFFFFF';
          edit1.Text:='0000000000000000';
       end;
    end
    else
    begin
       //init just once if needed
       if (edit1.Text = '') or (edit2.Text = '') then  // if not initialized
       begin
          edit2.text:='7FFFFFFF';
          edit1.Text:='00000000';
       end;
    end;
  edit1.Constraints.MinWidth:=canvas.GetTextWidth('XXXXXXXXXXXXXXXX');
  edit2.Constraints.MinWidth:=edit1.Constraints.MinWidth;
end;

procedure TfrmSavedisassembly.waittilldone;
begin
  if SaveDisassemblyThread<>nil then
    SaveDisassemblyThread.WaitFor;
end;

initialization
  {$i savedisassemblyfrm.lrs}

end.
