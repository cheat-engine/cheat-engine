unit frmExeTrainerGeneratorUnit;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  ExtCtrls, dialogs, StdCtrls, ComCtrls, Menus, cefuncproc, IconStuff, zstream,
  registry, MainUnit2, symbolhandler, lua, lualib, lauxlib;


type
  TFileData=class
    filepath: string;
    filename: string;
    folder: string;
  end;



  { TfrmExeTrainerGenerator }

  TfrmExeTrainerGenerator = class(TForm)
    Button1: TButton;
    btnGenerateTrainer: TButton;
    btnAddFile: TButton;
    btnRemoveFile: TButton;
    Button3: TButton;
    cbKernelDebug: TCheckBox;
    cbSpeedhack: TCheckBox;
    cbVEHDebug: TCheckBox;
    cbModPlayer: TCheckBox;
    cbD3DHook: TCheckBox;
    cbDotNet: TCheckBox;
    comboCompression: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    ListView1: TListView;
    miEditFolder: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    pmFiles: TPopupMenu;
    cbTiny: TRadioButton;
    cbGigantic: TRadioButton;
    rb32: TRadioButton;
    rb64: TRadioButton;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure btnAddFileClick(Sender: TObject);
    procedure btnRemoveFileClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnGenerateTrainerClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure cbTrainersizeChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure miEditFolderClick(Sender: TObject);
    procedure pmFilesPopup(Sender: TObject);
  private
    { private declarations }
    saving: boolean;
    archive: Tcompressionstream;
    _archive: TMemoryStream;

    updatehandle: thandle;
    filecount: integer;

    addedFiles: tstringlist;

    procedure addFile(filename: string; folder: string='');
  public
    { public declarations }
    filename: string;
    procedure addFiletoList(fn: string);
    procedure addDirToList(dir: string);
  end; 

var
  frmExeTrainerGenerator: TfrmExeTrainerGenerator;

  exeTrainerFeatures: array of record
    featurename: string;
    functionid: integer;
    cb: TCheckBox;
  end;

implementation

{ TfrmExeTrainerGenerator }

uses MainUnit,ceguicomponents, opensave, Globals, LuaHandler;

resourcestring
  rsSaving = 'Saving...';
  rsGenerate = 'Generate';
  rsupdateFailed='Failure updating the trainer resource information: %d';
  rsFailureOnWriting = 'failure on writing';
  rsIconUpdateError = 'icon update error';
  rsFailureOpeningTheTrainerForResourceUpdates = 'Failure opening the trainer '
    +'for resource updates. Make sure you do not watch the creation';
  rsTheTrainerHasBeenSuccessfullyGenerated = 'The trainer has been '
    +'successfully generated';
  rsNone = 'None';
  rsFastest = 'Fastest';
  rsDefault = 'Default';
  rsMax = 'Max';
  rsNewFoldername = 'New foldername';
  rsCETrainerMaker = 'CE trainer maker';
  rsARCHIVE = ' ARCHIVE:';
  rsDECOMPRESSOR = ' DECOMPRESSOR:';
  rsInvalidIcon = '(Invalid icon)';
  rsInvalidIconType = '(Invalid icon type)';
  rsTrainerFileMissing = 'Copying %s.dat to %s failed. Please make sure the '
    +'file still exists';
  
procedure TfrmExeTrainerGenerator.FormActivate(Sender: TObject);
begin

end;

var roti: integer;
function rot: string;
begin
  result:='';
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

procedure TfrmExeTrainerGenerator.addFile(filename: string; folder: string='');
var
  f: tmemorystream;
  currentfile: string;

  size: dword;
  i: qword;
  block: integer;
begin
  folder:=trim(folder);
  if (folder<>'') and ((folder[1]='\') or (folder[1]='/')) then
    folder:='';

  if addedfiles.IndexOf(folder+filename)<>-1 then exit; //it was already added earlier by the user

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

    //write the relative folder
    size:=length(folder);
    archive.write(size, sizeof(size));
    archive.write(folder[1], size);

    //write the size, and the file itself
    size:=f.size;
    archive.Write(size, sizeof(size));

    i:=f.size;
    while i>0 do
    begin
      block:=min(256*1024, i);
      archive.CopyFrom(f, block);
      dec(i,block);

      btnGenerateTrainer.caption:=rsSaving+rot;
      application.ProcessMessages;
    end;
    inc(filecount);

    addedfiles.add(folder+filename);
  finally
    f.free;
    btnGenerateTrainer.caption:=rsGenerate;
  end;
end;


procedure TfrmExeTrainerGenerator.btnGenerateTrainerClick(Sender: TObject);
var DECOMPRESSOR: TMemorystream;
  CETRAINER: string;
  icon: tmemorystream;

  z: ticon;

  ii: PICONDIR;
  gii: PGRPICONDIR absolute ii;

  compression: Tcompressionlevel;
  i,j,t,t2,ltop: integer;

  tiny: boolean;

  basefile: string;

  filepath: string;
  relpath: string;

begin

  addedfiles:=tstringlist.create;

  tiny:=cbTiny.Checked;

  CETRAINER:=ExtractFilePath(filename)+'CET_TRAINER.CETRAINER';

  if tiny then
  begin
    //temporarily insert this in front of the lua script
    MainForm.frmLuaTableScript.assemblescreen.BeginUpdate;
    MainForm.frmLuaTableScript.assemblescreen.Lines.Insert(0, 'RequiredCEVersion='+floattostr(ceversion));
    MainForm.frmLuaTableScript.assemblescreen.Lines.Insert(1, 'if (getCEVersion==nil) or (getCEVersion()<RequiredCEVersion) then');
    MainForm.frmLuaTableScript.assemblescreen.Lines.Insert(2, '  messageDialog(''Please install Cheat Engine ''..RequiredCEVersion, mtError, mbOK)');
    MainForm.frmLuaTableScript.assemblescreen.Lines.Insert(3, '  closeCE()');
    MainForm.frmLuaTableScript.assemblescreen.Lines.Insert(4, 'end');
  end;


  try
    SaveTable(CETRAINER, true);
  finally
    if tiny then
    begin
      //undo that addition
      for i:=0 to 4 do
        MainForm.frmLuaTableScript.assemblescreen.Lines.Delete(0);

      MainForm.frmLuaTableScript.assemblescreen.EndUpdate;
    end;
  end;



  btnGenerateTrainer.caption:=rsSaving+rot;
  btnGenerateTrainer.enabled:=false;
  saving:=true;

  application.ProcessMessages;
  try
    if tiny then basefile:='tiny' else basefile:='standalonephase1';

    if CopyFile(cheatenginedir+basefile+'.dat', filename) then
    begin
      updatehandle:=BeginUpdateResourceA(pchar(filename), false);
      if updatehandle<>0 then
      begin
        _archive:=TMemorystream.create; //create the archive

        if not tiny then
        begin
          //all files go into a compressed archive

          filecount:=0;
          _archive.WriteBuffer(filecount, sizeof(filecount)); //allocate space for the filecount  (omg trainers will be thirtytwo bits longer!)

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

          //first the custom files (this way you can override files with your own from other folders)
          for i:=0 to listview1.Items.Count-1 do
            addfile(TFileData(listview1.items[i].data).filepath, TFileData(listview1.items[i].data).folder);

          addfile(cheatenginedir+'defines.lua');

          if rb32.checked then
          begin
            addfile(cheatenginedir+'cheatengine-i386.exe');

            addfile(cheatenginedir+'lua53-32.dll');
            addfile(cheatenginedir+'win32\dbghelp.dll','win32');

            if cbSpeedhack.checked then
              addfile(cheatenginedir+'speedhack-i386.dll');

            if cbvehdebug.checked then
              addfile(cheatenginedir+'vehdebug-i386.dll');

            if cbKernelDebug.checked then
            begin
              addfile(cheatenginedir+'dbk32.sys');
              addfile(cheatenginedir+'dbk64.sys');
              addfile(cheatenginedir+'cheatengine-i386.exe.sig');
            end;

            if cbModPlayer.checked then
              addfile(cheatenginedir+'libmikmod32.dll');

          end
          else
          begin
            addfile(cheatenginedir+'cheatengine-x86_64.exe');
            addfile(cheatenginedir+'lua53-64.dll');

            if cbSpeedhack.checked then
              addfile(cheatenginedir+'speedhack-x86_64.dll');

            if cbvehdebug.checked then
              addfile(cheatenginedir+'vehdebug-x86_64.dll');

            if cbKernelDebug.checked then
            begin
              addfile(cheatenginedir+'dbk64.sys');
              addfile(cheatenginedir+'cheatengine-x86_64.exe.sig');
            end;

            if cbModPlayer.checked then
              addfile(cheatenginedir+'libmikmod64.dll');

          end;

          if cbDotNet.checked then
          begin
            addfile(cheatenginedir+'DotNetDataCollector32.exe');
            addfile(cheatenginedir+'DotNetDataCollector64.exe');
          end;

          if cbD3DHook.checked then
          begin
            addfile(cheatenginedir+'overlay.fx');
            if rb32.checked then
            begin
              addfile(cheatenginedir+'d3dhook.dll');
              addfile(cheatenginedir+'ced3d9hook.dll');
              addfile(cheatenginedir+'ced3d10hook.dll');
              addfile(cheatenginedir+'ced3d11hook.dll');
            end
            else
            begin
              addfile(cheatenginedir+'d3dhook64.dll');
              addfile(cheatenginedir+'ced3d9hook64.dll');
              addfile(cheatenginedir+'ced3d10hook64.dll');
              addfile(cheatenginedir+'ced3d11hook64.dll');
            end;
          end;

          //do the exe trainer features
          for i:=0 to length(exeTrainerFeatures)-1 do
            if (exeTrainerFeatures[i].cb<>nil) and exeTrainerFeatures[i].cb.checked then
            begin
              try
                ltop:=lua_gettop(luavm);
                lua_rawgeti(luavm, LUA_REGISTRYINDEX, exeTrainerFeatures[i].functionid) ;
                if lua_pcall(luavm, 0,1,0)=0 then
                begin
                  if lua_istable(luavm,-1) then
                  begin
                    t:=lua_gettop(luavm);
                    for j:=1 to lua_objlen(luavm, t) do
                    begin
                      lua_pushinteger(luavm, j);
                      lua_gettable(luavm, t);
                      if lua_istable(luavm, -1) then
                      begin
                        t2:=lua_gettop(Luavm);
                        lua_pushstring(Luavm,'PathToFile');
                        lua_gettable(Luavm, t2);
                        if lua_isstring(Luavm, -1) then
                          filepath:=Lua_ToString(LuaVM,-1)
                        else
                          filepath:='';

                        lua_pop(luavm,1);

                        if filepath<>'' then
                        begin
                          lua_pushstring(Luavm,'RelativePath');
                          lua_gettable(Luavm, t2);
                          if lua_isstring(Luavm, -1) then
                            relpath:=Lua_ToString(LuaVM,-1)
                          else
                            relpath:='';

                          lua_pop(luavm,1);

                          addFile(filepath,relpath);
                        end;

                      end;
                      lua_pop(luavm,1);
                    end;
                  end;
                end;

              finally
                lua_settop(luavm,ltop);
              end;
            end;

          archive.free;

          pinteger(_archive.Memory)^:=filecount;  //fill in the count (uncompressed)


        end
        else
          _archive.LoadFromFile(CETRAINER); //tiny version has the .cetrainer only


        {_Archive.SaveToFile('c:\bla.dat');}

        try

          if not UpdateResourceA(updatehandle, RT_RCDATA, 'ARCHIVE', 0, _archive.memory, _archive.size) then
            raise exception.create(rsFailureOnWriting+rsARCHIVE+inttostr(
              getlasterror()));

          if not tiny then
          begin
            //tiny has no decompressor
            if not UpdateResourceA(updatehandle, RT_RCDATA, 'DECOMPRESSOR', 0, decompressor.memory, decompressor.size) then
              raise exception.create(rsFailureOnWriting+rsDECOMPRESSOR+inttostr(
                getlasterror()));
          end;

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
                  raise exception.create(rsIconUpdateError+' 2');

                //update the group
                gii.idCount:=1;
                gii.icondirentry[0].id:=1;
                if not updateResourceA(updatehandle,pchar(RT_GROUP_ICON),MAKEINTRESOURCE(101),1033, gii, sizeof(TGRPICONDIR)+sizeof(TGRPICONDIRENTRY)) then
                  raise exception.create(rsIconUpdateError+' 3');


              end
              else
                raise exception.create(rsIconUpdateError+' 4 '+rsInvalidIcon);
            end
            else
              raise exception.create(rsIconUpdateError+' 5 '+rsInvalidIconType);
          finally
            icon.free;

          end;

        finally
          if EndUpdateResource(updatehandle, false)=false then
            raise exception.create(format(rsUpdateFailed,[getLastError]));
        end;
      end else raise exception.create(
        rsFailureOpeningTheTrainerForResourceUpdates);

      showmessage(rsTheTrainerHasBeenSuccessfullyGenerated);
    end
    else
      showMessage(Format(rsTrainerFileMissing, [cheatenginedir+basefile, filename]));


  finally
    if _archive<>nil then
      freeandnil(_archive);

    saving:=false;
    btnGenerateTrainer.enabled:=true;


    if addedfiles<>nil then
      freeandnil(addedfiles);
  end;
end;

procedure TfrmExeTrainerGenerator.addDirToList(dir: string);
var dirinfo: TSearchRec;
  r: integer;
begin
  ZeroMemory(@DirInfo,sizeof(TSearchRec));

  while dir[length(dir)]=pathdelim do //cut of \
    dir:=copy(dir,1,length(dir)-1);

  {$warn 5044 off}

  r := FindFirst(dir + pathdelim+'*.*', FaAnyfile, DirInfo);
  while (r = 0) do
  begin
    if (DirInfo.Attr and FaVolumeId <> FaVolumeID) then
    begin
      if ((DirInfo.Attr and FaDirectory) <> FaDirectory) then
        addFiletoList(dir + pathdelim + DirInfo.Name)
      else
      begin
        if (DirInfo.Name[1]<>'.') then
          addDirToList(dir + pathdelim + DirInfo.Name);
      end;
    end;

    r := FindNext(DirInfo);
  end;

  {$warn 5044 on}
  FindClose(DirInfo);
end;


procedure TfrmExeTrainerGenerator.addFiletoList(fn: string);
var f: TFiledata;
  d: string;
  li: TListItem;
begin
  f:=tfiledata.create;
  f.filepath:=fn;
  f.filename:=extractfilename(fn);

  d:=ExtractFilePath(fn);
  d:=ExtractRelativepath(cheatenginedir, d);
  if (pos(':', d)>0) or (pos('..', d)>0) then
    d:='';

  f.folder:=d;

  li:=listview1.Items.Add;
  li.caption:=f.filename;
  li.SubItems.Add(d);
  li.Data:=f;
end;

procedure TfrmExeTrainerGenerator.Button3Click(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
    addDirToList(SelectDirectoryDialog1.FileName);
end;

procedure TfrmExeTrainerGenerator.cbTrainersizeChange(Sender: TObject);
begin
  groupbox1.enabled:=cbGigantic.checked;
  GroupBox2.enabled:=cbGigantic.checked;
  rb32.enabled:=cbGigantic.Checked;
  rb64.enabled:=cbGigantic.checked;
  cbSpeedhack.enabled:=cbGigantic.Checked;
  cbVEHDebug.enabled:=cbGigantic.checked;
  cbModPlayer.Enabled:=cbGigantic.checked;
  cbKernelDebug.enabled:=cbGigantic.Checked;

  label1.enabled:=cbGigantic.checked;
  comboCompression.enabled:=cbGigantic.checked;

  GroupBox3.enabled:=cbGigantic.checked;
  ListView1.enabled:=cbGigantic.checked;
  Button3.enabled:=cbGigantic.checked;

  btnAddFile.enabled:=cbGigantic.checked;
  btnRemoveFile.enabled:=listview1.Selected<>nil;

end;

procedure TfrmExeTrainerGenerator.Button1Click(Sender: TObject);
begin
  image1.picture.icon:=pickIcon;
end;

procedure TfrmExeTrainerGenerator.btnAddFileClick(Sender: TObject);
var i: integer;
begin
  if opendialog1.execute then
  begin
    for i:=0 to opendialog1.Files.count-1 do
      addFileToList(opendialog1.Files[i]);
  end;
end;

procedure TfrmExeTrainerGenerator.btnRemoveFileClick(Sender: TObject);
var i: integer;
begin
  i:=0;
  while i<listview1.items.count do
  begin
    if listview1.Items[i].Selected then
    begin
      TFileData(listview1.items[i].data).free;
      listview1.items.Delete(i);
    end
    else
      inc(i);
  end;

end;

procedure TfrmExeTrainerGenerator.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var i: integer;
begin
  closeaction:=cafree;
  frmExeTrainerGenerator:=nil;

  for i:=0 to ListView1.Items.Count-1 do
    TFiledata(listview1.items[i].Data).free;

  for i:=0 to length(exeTrainerFeatures)-1 do
    if exeTrainerFeatures[i].cb<>nil then
      freeandnil(exeTrainerFeatures[i].cb);
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
  comboCompression.Items.Clear;
  with comboCompression.Items do
  begin
    add(rsNone);
    add(rsFastest);
    add(rsDefault);
    add(rsMax);
  end;
  comboCompression.itemindex:=3;


  OpenDialog1.InitialDir:=CheatEngineDir;
  SelectDirectoryDialog1.InitialDir:=CheatEngineDir;

  //scan the current script for markers that might indicate a used feature
  s:=lowercase(mainform.frmLuaTableScript.assemblescreen.Text);

  cbSpeedhack.checked:=pos('speedhack_',s)>0;
  cbModPlayer.checked:=(pos('xmplayer_',s)>0) or (pos('xmplayer.',s)>0);
  cbKernelDebug.checked:=pos('dbk_',s)>0;
  cbD3DHook.checked:=pos('created3dhook',s)>0;
  cbDotNet.checked:=symhandler.hasDotNetAccess or (pos('dotnet',s)>0);


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

  for i:=0 to length(exeTrainerFeatures)-1 do
    if exeTrainerFeatures[i].featurename<>'' then
    begin
      exeTrainerFeatures[i].cb:=TCheckBox.Create(self);
      exeTrainerFeatures[i].cb.Caption:=exeTrainerFeatures[i].featurename;
      exeTrainerFeatures[i].cb.Parent:=GroupBox2;
    end;
end;

procedure TfrmExeTrainerGenerator.FormShow(Sender: TObject);
var
  i,s:integer;
  cbi: TComboboxInfo;
  extrasize: integer;
begin
  i:=max(max(button3.Width, btnAddFile.Width), btnRemoveFile.Width);

  button3.width:=i;
  btnAddFile.Width:=i;
  btnRemoveFile.Width:=i;
  groupbox3.Constraints.MinHeight:=panel1.height;

  cbi.cbSize:=sizeof(cbi);
  if GetComboBoxInfo(comboCompression.handle, @cbi) then
    extrasize:=cbi.rcButton.Right-cbi.rcButton.Left+cbi.rcItem.Left
  else
    extrasize:=16;

  s:=0;
  for i:=0 to comboCompression.Items.Count-1 do
    s:=max(s, canvas.GetTextWidth(comboCompression.items[i]));

  comboCompression.Constraints.MinWidth:=s+extrasize;
end;

procedure TfrmExeTrainerGenerator.ListView1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TfrmExeTrainerGenerator.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  btnRemoveFile.enabled:=listview1.Selected<>nil;
end;

procedure TfrmExeTrainerGenerator.miEditFolderClick(Sender: TObject);
var z: TFiledata;
begin
  if listview1.Selected<>nil then
  begin
    z:=TFiledata(listview1.Selected.data);
    InputQuery(rsNewFoldername, rsCETrainerMaker, z.folder);
    listview1.Selected.SubItems[0]:=z.folder;
  end;
end;

procedure TfrmExeTrainerGenerator.pmFilesPopup(Sender: TObject);
begin
  miEditFolder.enabled:=ListView1.Selected<>nil;

end;

initialization
  {$I frmExeTrainerGeneratorUnit.lrs}

end.

