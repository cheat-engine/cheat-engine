unit frmExeTrainerGeneratorUnit;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, ExtCtrls,
  dialogs, StdCtrls, cefuncproc, IconStuff, zstream;

type

  { TfrmExeTrainerGenerator }

  TfrmExeTrainerGenerator = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbVEHDebug: TCheckBox;
    cbKernelDebug: TCheckBox;
    cbXMPlayer: TCheckBox;
    cbSpeedhack: TCheckBox;
    comboCompression: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    rb32: TRadioButton;
    rb64: TRadioButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    saving: boolean;
    archive: Tcompressionstream;
    _archive: TMemoryStream;

    updatehandle: thandle;
    procedure addFile(filename: string);
  public
    { public declarations }
    filename: string;
  end; 

var
  frmExeTrainerGenerator: TfrmExeTrainerGenerator;

implementation

{ TfrmExeTrainerGenerator }

uses MainUnit,ceguicomponents, opensave;

procedure TfrmExeTrainerGenerator.FormActivate(Sender: TObject);
begin

end;

var roti: integer;
function rot: string;
begin
  roti:=(roti+1) mod 8;
  case roti of
    0: result:='-';
    1: result:='\';
    2: result:='|';
    3: result:='/';
    4: result:='-';
    5: result:='\';
    6: result:='|';
    7: result:='/';
  end;

end;

procedure TfrmExeTrainerGenerator.addFile(filename: string);
var
  f: tmemorystream;
  currentfile: string;

  size: dword;
  i: qword;
  block: integer;
begin
  f:=TMemoryStream.create;
  try


    f.LoadFromFile(filename);
    f.position:=0;
    size:=f.size;

    //write the filename
    currentfile:=extractfilename(filename);
    size:=length(currentfile);
    archive.write(size, sizeof(size));
    archive.write(currentfile[1], size);

    //write the size, and the file itself
    size:=f.size;
    archive.Write(size, sizeof(size));

    i:=f.size;
    while i>0 do
    begin
      block:=min(256*1024, i);
      archive.CopyFrom(f, block);
      dec(i,block);

      button2.caption:='Saving...'+rot;
      application.ProcessMessages;
    end;
  finally
    f.free;
  end;
end;


procedure TfrmExeTrainerGenerator.Button2Click(Sender: TObject);
var DECOMPRESSOR: TMemorystream;
  CETRAINER: string;
  icon: tmemorystream;

  z: ticon;

  ii: PICONDIR;
  gii: PGRPICONDIR absolute ii;

  compression: Tcompressionlevel;
begin
  CETRAINER:=ExtractFilePath(filename)+'CET_TRAINER.CETRAINER';
  SaveTable(CETRAINER, true);

  button2.caption:='Saving...'+rot;
  button2.enabled:=false;
  saving:=true;

  application.ProcessMessages;
  try
    if CopyFile(cheatenginedir+'standalonephase1.dat', filename) then
    begin
      updatehandle:=BeginUpdateResourceA(pchar(filename), false);
      if updatehandle<>0 then
      begin
        _archive:=TMemorystream.create; //create the archive

        case comboCompression.itemindex of
          0: compression:=clnone;
          1: compression:=clfastest;
          2: compression:=cldefault;
          3: compression:=clmax;
        end;

        archive:=Tcompressionstream.create(compression, _archive, true);


        decompressor:=TMemorystream.create;
        decompressor.LoadFromFile(cheatenginedir+'standalonephase2.dat');

        addfile(CETRAINER);
        deletefile(cetrainer);

        if rb32.checked then
        begin
          addfile(cheatenginedir+'cheatengine-i386.exe');
          addfile(cheatenginedir+'lua5.1-32.dll');

          if cbSpeedhack.checked then
            addfile(cheatenginedir+'speedhack-i386.dll');

          if cbvehdebug.checked then
            addfile(cheatenginedir+'vehdebug-i386.dll');

          if cbKernelDebug.checked then
            addfile(cheatenginedir+'dbk32.sys');


        end
        else
        begin
          addfile(cheatenginedir+'cheatengine-x86_64.exe');
          addfile(cheatenginedir+'lua5.1-64.dll');

          if cbSpeedhack.checked then
            addfile(cheatenginedir+'speedhack-x86_64.dll');

          if cbvehdebug.checked then
            addfile(cheatenginedir+'vehdebug-x86_64.dll');

          if cbKernelDebug.checked then
            addfile(cheatenginedir+'dbk64.sys');
        end;

        if cbXMPlayer.checked then
          addfile(cheatenginedir+'xmplayer.exe');


        archive.free;


        if not UpdateResourceA(updatehandle, RT_RCDATA, 'ARCHIVE', 0, _archive.memory, _archive.size) then
          raise exception.create('failure on writing ARCHIVE:'+inttostr(getlasterror()));

        if not UpdateResourceA(updatehandle, RT_RCDATA, 'DECOMPRESSOR', 0, decompressor.memory, decompressor.size) then
          raise exception.create('failure on writing DECOMPRESSOR:'+inttostr(getlasterror()));

        icon:=tmemorystream.create;
        try
          image1.picture.icon.SaveToStream(icon);
         // sizeof(TBitmapInfoHeader)

          //GetIconInfo();

          z:=TIcon.create;
         // z.LoadFromFile('F:\svn\favicon.ico');
          //z.SaveToStream(icon);

          ii:=icon.memory;

          if ii.idType=1 then
          begin
            if ii.idCount>0 then
            begin
              //update the icon
              if not updateResourceA(updatehandle,pchar(RT_ICON),MAKEINTRESOURCE(1),1033, pointer(ptruint(icon.Memory)+ii.icondirentry[0].dwImageOffset), ii.icondirentry[0].dwBytesInRes) then
                raise exception.create('icon update error2');

              //update the group
              gii.idCount:=1;
              gii.icondirentry[0].id:=1;
              if not updateResourceA(updatehandle,pchar(RT_GROUP_ICON),MAKEINTRESOURCE(101),1033, gii, sizeof(TGRPICONDIR)+sizeof(TGRPICONDIRENTRY)) then
                raise exception.create('icon update error3');


            end;
          end;
        finally
          icon.free;

        end;




        EndUpdateResource(updatehandle, false);
      end else raise exception.create('Failure opening the trainer for resource updates');
    end;

  finally
    if _archive<>nil then
      freeandnil(_archive);

    saving:=false;
    button2.enabled:=true;

    showmessage('The trainer has been successfully generated');
  end;
end;

procedure TfrmExeTrainerGenerator.Button1Click(Sender: TObject);
begin
  image1.picture.icon:=pickIcon;
end;

procedure TfrmExeTrainerGenerator.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  closeaction:=cafree;
  frmExeTrainerGenerator:=nil;
end;

procedure TfrmExeTrainerGenerator.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  canclose:=not saving;
end;

procedure TfrmExeTrainerGenerator.FormCreate(Sender: TObject);
var s: string;
  i: integer;
begin
  //scan the current script for markers that might indicate a used feature
  s:=lowercase(mainform.frmLuaTableScript.assemblescreen.Text);

  cbSpeedhack.checked:=pos('speedhack_',s)>0;
  cbXMPlayer.checked:=pos('xmplayer_initialize',s)>0;


  if mainform.LuaForms.count=1 then  //if there is only one form use that icon as default
    image1.Picture.Icon:=TCEForm(mainform.LuaForms[0]).icon
  else   //else check if there is a TRAINERFORM
  for i:=0 to mainform.LuaForms.count-1 do
    if TCEForm(mainform.LuaForms[i]).Name='TRAINERFORM' then
    begin
      //use the icon from this form
      image1.Picture.Icon:=TCEForm(mainform.LuaForms[i]).icon;
      break;
    end;

end;

initialization
  {$I frmExeTrainerGeneratorUnit.lrs}

end.

