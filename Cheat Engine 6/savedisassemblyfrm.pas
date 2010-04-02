unit savedisassemblyfrm;

{$MODE Delphi}

interface

uses
  LCLIntf, LResources, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,symbolhandler,disassembler, StdCtrls, ComCtrls, ActnList, Clipbrd;

type
  TfrmSavedisassembly = class;
  TSaveDisassemblyThread=class(TThread)
  public
    progressbar: tprogressbar;
    start: ptrUint;
    stop: ptrUint;
    address: boolean;
    bytes: boolean;
    opcode: boolean;
    copymode: boolean;
    filename: string;
    form: TfrmSavedisassembly;
    procedure execute; override;
  end;

  TfrmSavedisassembly = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    SaveDialog1: TSaveDialog;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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


procedure TSaveDisassemblyThread.execute;
var oldaddress, currentaddress: ptrUint;
    f: textfile;
    temps: string;
    temps2: string;
    addresspart, bytepart, opcodepart, specialpart: string;
    i: integer;
    clip: tclipboard;
    cpbuf: tstringlist;
    y,z:string;
    mi: TModuleInfo;
begin
  currentaddress:=start;

  if copymode then
  begin
    clip:=tclipboard.Create;
    cpbuf:=tstringlist.Create;

  end
  else
  begin
    assignfile(f,filename);
    rewrite(f);
  end;

  i:=0;

  while (not terminated) and (currentaddress<=stop) do
  begin
    oldaddress:=currentaddress;
    temps:=disassemble(currentaddress); //contains the addresspart, bytepart and opcode part
    splitDisassembledString(temps,true,addresspart,bytepart,opcodepart,specialpart);

    if symhandler.showmodules then
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

    if specialpart<>'' then
      opcodepart:=opcodepart+' : '+specialpart;

    temps:='';
    if address then
    begin
      temps:=addresspart;
      if bytes or opcode then temps:=temps+' - ';
    end;

    if bytes then
    begin
      temps:=temps+bytepart;
      if opcode then
      begin
        temps:=temps+' ';
        if address then
          while length(temps)<(11+(9*3)) do temps:=temps+' '
        else
          while length(temps)<(9*3) do temps:=temps+' ';

        temps:=temps+'- '
      end;
    end;

    if opcode then temps:=temps+opcodepart;

    if copymode then
    begin
      //save to clipboard
      cpbuf.Add(temps);
    end
    else
      writeln(f,temps); //write to file

    if (i mod 10=0) and (currentaddress<$7fffffff) then
      progressbar.position:=currentaddress;
  end;

  if copymode then
  begin
    clip.SetTextBuf(cpbuf.GetText);
    clip.Free;
    cpbuf.free;
  end
  else
  begin
    flush(f);
    closefile(f);
  end;

  if not terminated then postmessage(form.handle,wm_close,0,0);

end;

procedure TfrmSavedisassembly.setCopyMode(mode: boolean);
begin
  FCopyMode:=mode;

  if mode then
  begin
    //configure for copy mode, so instead of saving to file save to clipboard
    caption:='Copy disassembled output';
    button1.caption:='Copy';
  end
  else
  begin

    caption:='Save disassembled output';
    button1.Caption:='Save';
  end;
end;

procedure TfrmSavedisassembly.Button1Click(Sender: TObject);
var start,stop: ptrUint;
begin
  if SaveDisassemblyThread<>nil then
  begin
    SaveDisassemblyThread.Terminate;
    SaveDisassemblyThread.WaitFor;
    freeandnil(SaveDisassemblyThread);
    if FCopyMode then
      button1.Caption:='Copy'
    else
      button1.Caption:='Save';
    exit;
  end;


  start:=symhandler.getAddressFromName(edit1.Text);
  stop:=symhandler.getAddressFromName(edit2.text);


  if (FCopyMode) or savedialog1.Execute then
  begin
    SaveDisassemblyThread:=TSaveDisassemblyThread.Create(true);
    SaveDisassemblyThread.address:=checkbox1.checked;
    SaveDisassemblyThread.bytes:=checkbox2.Checked;
    SaveDisassemblyThread.opcode:=checkbox3.Checked;
    SaveDisassemblyThread.start:=start;
    SaveDisassemblyThread.stop:=stop;
    SaveDisassemblyThread.filename:=savedialog1.FileName;
    SaveDisassemblyThread.copymode:=fcopymode;
    SaveDisassemblyThread.form:=self;

    if (start<$7fffffff) and (stop<$7fffffff) then
    begin
      progressbar1.Max:=stop;
      progressbar1.Min:=start;
      progressbar1.Position:=start;
      if not progressbar1.Visible then progressbar1.Visible:=true;
    end
    else
      progressbar1.Visible:=false;

    SaveDisassemblyThread.progressbar:=progressbar1;

    if fcopymode then
      button1.caption:='Stop copying'
    else
      button1.caption:='Stop saving';
      
    SaveDisassemblyThread.resume;

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

procedure TfrmSavedisassembly.waittilldone;
begin
  if SaveDisassemblyThread<>nil then
    SaveDisassemblyThread.WaitFor;
end;

initialization
  {$i savedisassemblyfrm.lrs}

end.
