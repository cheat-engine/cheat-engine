unit PatcherUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,settingsunit, ExtDlgs;

type TPatch=record
  address: Dword;
  mem: array of byte;
  filename: string;
end;

type
  TfrmPatcher = class(TForm)
    Button1: TButton;
    Messages: TMemo;
    OpenDialog1: TOpenDialog;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    Numberofpatches: integer;
    patches: array of TPatch;
    undoinfo: array of TPatch; //Tpatch can also be used to undo
  public
    { Public declarations }
  end;

var
  frmPatcher: TfrmPatcher;

implementation

{$R *.dfm}

procedure TfrmPatcher.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  application.Terminate;
end;

procedure TfrmPatcher.FormCreate(Sender: TObject);
var i: integer;
    temp: dword;
    x: pchar;
begin
  //fill in the data
  trainerfile.ReadBuffer(temp,4);
  Numberofpatches:=temp;

  setlength(patches,temp);

  for i:=0 to Numberofpatches-1 do
  begin
    trainerfile.ReadBuffer(temp,4);
    patches[i].address:=temp;

    trainerfile.ReadBuffer(temp,4);
    setlength(patches[i].mem,temp);
    trainerfile.ReadBuffer(pointer(patches[i].mem)^,temp);

    trainerfile.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainerfile.ReadBuffer(pointer(x)^,temp);
    x[temp]:=#0;

    patches[i].filename:=x;
    freemem(x);

  end;

  trainerfile.ReadBuffer(temp,4);
  getmem(x,temp+1);
  trainerfile.ReadBuffer(pointer(x)^,temp);
  x[temp]:=#0;
  messages.Lines.Text:=x;
  freemem(x);

  trainerfile.ReadBuffer(temp,4);
  getmem(x,temp+1);
  trainerfile.ReadBuffer(pointer(x)^,temp);
  x[temp]:=#0;
  caption:=x;
  application.Title:=x;
  freemem(x);



  trainerfile.free;

  //now scan for the files, if they dont excist ask the user to tell where the files are (if the user chooses cancel quit)
  for i:=0 to numberofpatches-1 do
  begin
    if not fileexists(patches[i].filename) then
    begin
      opendialog1.filename:=patches[i].filename;
      opendialog1.Title:='Please tell me where '+patches[i].filename+' is';
      if opendialog1.Execute then
        patches[i].filename:=opendialog1.FileName
      else
      begin
        application.Terminate;
        exit;
      end;
    end;
  end;

  //check if there is undo information in the same location as the patcher
  if fileexists(changefileext(application.ExeName,'.undo')) then
  begin
    //undo file exists so
    button1.Caption:='Undo';
  end;
end;

procedure TfrmPatcher.Button1Click(Sender: TObject);
var i,j: integer;
    tempfile: TFileStream;
    undofile: TFileStream;
    temp: dword;
    x: pchar;

    mem: TMemorystream;
begin
  messages.clear;
  if Button1.caption='Undo' then
  begin
    //restore the file(s) with the .undo file
    messages.lines.add('restoring file(s)...');
    undofile:=tfilestream.create(changefileext(application.ExeName,'.undo'),fmOpenRead or fmShareDenyNone);

    //fill the patches list with undo info
    for i:=0 to numberofpatches-1 do
    begin
      undofile.ReadBuffer(pointer(patches[i].mem)^,length(patches[i].mem));
      undofile.ReadBuffer(temp,4);
      getmem(x,temp+1);
      undofile.ReadBuffer(pointer(x)^,temp);
      x[temp]:=#0;
      patches[i].filename:=x;
      freemem(x);

      if not fileexists(patches[i].filename) then
      begin
        opendialog1.title:='I''ve seen to have misplaced '+patches[i].filename+' . Please tell me where it is!';
        opendialog1.FileName:=patches[i].filename;
        if opendialog1.execute then patches[i].filename:=opendialog1.filename else
        begin
          showmessage('Couldn''t undo the patching!');
          application.Terminate;
          exit;
        end;
      end;

      tempfile:=TFilestream.create(patches[i].filename,fmOpenReadWrite or fmShareDenyNone);
      tempfile.Position:=patches[i].address;
      tempfile.WriteBuffer(pointer(patches[i].mem)^,length(patches[i].mem));
      tempfile.free;
    end;

    undofile.free;
    deletefile(changefileext(application.ExeName,'.undo'));
    showmessage('The unpatching has finished!');
    application.terminate;
  end
  else
  begin
    //verify the files,make the undo info and then patch
    messages.lines.add('verifying the file(s)');
    setlength(undoinfo,numberofpatches);

    for i:=0 to numberofpatches-1 do
    begin
      tempfile:=TFilestream.Create(patches[i].filename,fmOpenReadWrite or fmshareDenynone);
      tempfile.Position:=patches[i].address;

      setlength(undoinfo[i].mem,length(patches[i].mem));
      tempfile.ReadBuffer(pointer(undoinfo[i].mem)^,length(patches[i].mem));

      //check if the bytes I read are what they are suposed to be
      if not comparemem(@undoinfo[i].mem[0],@patches[i].mem[0],length(patches[i].mem)) then
      begin
        //check if it was suposed to be checked
        for j:=0 to length(patches[i].mem)-1 do
          if patches[i].mem[j]<>$90 then
          begin
            messagedlg('The file '+patches[i].filename+' is not the right version',mtError,[mbok],0);
            application.Terminate;
            exit;
          end;
      end;
      tempfile.free;
    end;

    messages.lines.add('Verified');
    messages.Lines.Add('Generating undo file');
    //if all the files got verified then make the undo file and patch
    undofile:=tfilestream.create(changefileext(application.ExeName,'.undo'),fmCreate or fmShareDenyNone);

    for i:=0 to numberofpatches-1 do
    begin
      undofile.WriteBuffer(pointer(undoinfo[i].mem)^,length(undoinfo[i].mem));

      temp:=length(patches[i].filename);
      undofile.WriteBuffer(temp,4);
      x:=pchar(patches[i].filename);
      undofile.writeBuffer(pointer(x)^,temp);
    end;

    undofile.free;

    messages.Lines.add('Undo file created');
    messages.Lines.add('Applying patches to file(s)');

    //and now the patching
    for i:=0 to numberofpatches-1 do
    begin
      mem:=TMemorystream.Create;  //I know, slow, but each time I do a normal write the file gets reverted when I leave the procedure (ok, fixed cause I tested it with rthe regenerating winmine.exe file in c:\windows\system32, but this works too....)
      mem.LoadFromFile(patches[i].filename);
      mem.Position:=patches[i].address;

      for j:=0 to length(patches[i].mem)-1 do
        patches[i].mem[j]:=$90;

      mem.WriteBuffer(pointer(patches[i].mem)^,length(patches[i].mem));
      mem.SaveToFile(patches[i].filename);
      mem.Free;
    end;
    Messages.lines.add('Patching successfull. Have a nice day!');
    showmessage('The patching has finished! To undo the changes start the patcher again and click Undo');
    application.terminate;
  end;
end;

end.
