unit frmautoinjectunit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, CEFuncProc, StrUtils, types, ComCtrls, LResources,
  NewKernelHandler, SynEdit, SynHighlighterCpp, SynHighlighterAA, disassembler,
  MainUnit2, Assemblerunit, autoassembler, symbolhandler, SynEditSearch,
  MemoryRecordUnit, tablist, customtypehandler, registry;


type TCallbackRoutine=procedure(memrec: TMemoryRecord; script: string; changed: boolean) of object;
type TCustomCallbackRoutine=procedure(ct: TCustomType; script:string; changed: boolean; lua: boolean) of object;

type TScripts=array of record
                script: string;
                filename: string;
                undoscripts: array [0..4] of record
                               oldscript: string;
                               startpos: integer;
                             end;
                currentundo: integer;
              end;

type
  TfrmAutoInject = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Panel1: TPanel;
    Button1: TButton;
    Load1: TMenuItem;
    Panel2: TPanel;
    Save1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Exit1: TMenuItem;
    Assigntocurrentcheattable1: TMenuItem;
    emplate1: TMenuItem;
    Codeinjection1: TMenuItem;
    CheatTablecompliantcodee1: TMenuItem;
    APIHook1: TMenuItem;
    SaveAs1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Coderelocation1: TMenuItem;
    New1: TMenuItem;
    N2: TMenuItem;
    Syntaxhighlighting1: TMenuItem;
    closemenu: TPopupMenu;
    Close1: TMenuItem;
    Inject1: TMenuItem;
    Injectincurrentprocess1: TMenuItem;
    Injectintocurrentprocessandexecute1: TMenuItem;
    Find1: TMenuItem;
    Paste1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    Undo1: TMenuItem;
    N6: TMenuItem;
    FindDialog1: TFindDialog;
    undotimer: TTimer;
    View1: TMenuItem;
    AAPref1: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Codeinjection1Click(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure CheatTablecompliantcodee1Click(Sender: TObject);
    procedure assemblescreenChange(Sender: TObject);
    procedure Assigntocurrentcheattable1Click(Sender: TObject);
    procedure APIHook1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure assemblescreenKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Coderelocation1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure Syntaxhighlighting1Click(Sender: TObject);
    procedure TabControl1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure Close1Click(Sender: TObject);
    procedure Injectincurrentprocess1Click(Sender: TObject);
    procedure Injectintocurrentprocessandexecute1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure AAPref1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
  private
    { Private declarations }

    AAHighlighter: TSynAASyn;
    CPPHighlighter: TSynCppSyn;
    assembleSearch: TSynEditSearch;

    oldtabindex: integer;
    scripts: TScripts;
             
    selectedtab: integer;

    fluamode: boolean;
    fCustomTypeScript: boolean;

    procedure setluamode(state: boolean);
    procedure injectscript(createthread: boolean);
    procedure tlistOnTabChange(sender: TObject; oldselection: integer);
    procedure setCustomTypeScript(x: boolean);

  public
    { Public declarations }

    assemblescreen: TSynEdit;
    tlist: TTablist;

    editscript: boolean;
    editscript2: boolean;
    memrec: TMemoryRecord;

    customtype: TCustomType;

    callbackroutine: TCallbackroutine;
    CustomTypeCallback: TCustomCallbackroutine;
    injectintomyself: boolean;
    property luamode: boolean read fluamode write setluamode;
    property CustomTypeScript: boolean read fCustomTypeScript write setCustomTypeScript;
  end;


procedure Getjumpandoverwrittenbytes(address,addressto: ptrUINT; jumppart,originalcodepart: tstrings);
procedure generateAPIHookScript(script: tstrings; address: string; addresstogoto: string; addresstostoreneworiginalfunction: string=''; nameextension:string='0');


implementation


uses frmAAEditPrefsUnit,MainUnit,memorybrowserformunit,APIhooktemplatesettingsfrm;

procedure TfrmAutoInject.setCustomTypeScript(x: boolean);
begin
  fCustomTypeScript:=x;
  if x then
    editscript:=true;
end;

procedure TfrmAutoInject.setluamode(state: boolean);
begin
{$ifndef standalonetrainerwithassembler}
  fluamode:=state;
  if state then
  begin
    assemblescreen.Highlighter:=nil;

    //change gui to c++ style
    button1.Caption:='Execute script';
    opendialog1.DefaultExt:='LUA';
    opendialog1.Filter:='LUA Script (*.LUA)|*.LUA|All Files ( *.* )|*.*';
    savedialog1.DefaultExt:='LUA';
    savedialog1.Filter:='LUA Script (*.LUA)|*.LUA|All Files ( *.* )|*.*';
    Assigntocurrentcheattable1.visible:=false;
    emplate1.Visible:=false;
    caption:='LUA Script engine';
    inject1.Visible:=true;
    helpcontext:=19; //c-script help
  end
  else
  begin
    assemblescreen.Highlighter:=AAHighlighter;


    //change gui to autoassembler style
    button1.caption:='Write code';
    opendialog1.DefaultExt:='CEA';
    opendialog1.Filter:='Cheat Engine Assembly (*.CEA)|*.CEA|All Files ( *.* )|*.*';
    savedialog1.DefaultExt:='CES';
    savedialog1.Filter:='Cheat Engine Assembly (*.CEA)|*.CEA|All Files ( *.* )|*.*';
    Assigntocurrentcheattable1.Visible:=true;
    emplate1.Visible:=true;
    caption:='Auto assembler';
    inject1.Visible:=false;
    helpcontext:=18; //auto asm help
  end;
{$endif}
end;


procedure TfrmAutoInject.Button1Click(Sender: TObject);
var
    a,b: integer;

    aa: TCEAllocArray;

    //variables for injectintomyself:
    check: boolean;
    registeredsymbols: TStringlist;
begin
{$ifndef standalonetrainerwithassembler}
  registeredsymbols:=tstringlist.Create;
  registeredsymbols.CaseSensitive:=false;
  registeredsymbols.Duplicates:=dupIgnore;

  if luamode then
  begin
    //no implementation
    if editscript then
    begin
      modalresult:=mrok; //not modal anymore, but can still be used to pass info
      if editscript2 or CustomTypeScript then close;
    end;
  end
  else
  begin
    if editscript then
    begin
      //check if both scripts are valid before allowing the edit

      setlength(aa,1);
      getenableanddisablepos(assemblescreen.Lines,a,b);
      if not CustomTypeScript then
        if (a=-1) and (b=-1) then raise exception.create('The code needs an [ENABLE] and a [DISABLE] section if you want to use this script as a table entry');


      check:=autoassemble(assemblescreen.lines,false,true,true,injectintomyself,aa,registeredsymbols) and
             autoassemble(assemblescreen.lines,false,false,true,injectintomyself,aa,registeredsymbols);

      if check then
      begin
        modalresult:=mrok; //not modal anymore, but can still be used to pass info
        if editscript2 or CustomTypeScript then close; //can only be used when not modal
      end
      else
      begin
        if messagedlg('Not all code is injectable. Are you sure you wan''t to edit it to this?',mtWarning,[mbyes,mbno],0)=mryes then
        begin
          modalresult:=mrok; //not modal anymore, but can still be used to pass info
          if editscript2 or CustomTypeScript then close;
        end;
      end;
    end else autoassemble(assemblescreen.lines,true);
  end;
  registeredsymbols.free;
{$endif}
end;

procedure TfrmAutoInject.Load1Click(Sender: TObject);
begin
{$ifndef standalonetrainerwithassembler}

  if opendialog1.Execute then
  begin

    assemblescreen.Lines.Clear;
    assemblescreen.Lines.LoadFromFile(opendialog1.filename);
    savedialog1.FileName:=opendialog1.filename;
    assemblescreen.AfterLoadFromFile;

  end;
{$endif}
end;

procedure TfrmAutoInject.Save1Click(Sender: TObject);
var f: tfilestream;
    s: string;
begin
{$ifndef standalonetrainerwithassembler}

  if (savedialog1.filename='') and (not savedialog1.Execute) then exit;   //filename was empty and the user clicked cancel

  f:=tfilestream.Create(savedialog1.filename,fmcreate);
  try
    s:=assemblescreen.text;
    f.Write(s[1],length(assemblescreen.text));

    assemblescreen.MarkTextAsSaved;

  finally
    f.Free;
  end;
{$endif}
end;

procedure TfrmAutoInject.Exit1Click(Sender: TObject);
begin
  close;
end;

procedure TfrmAutoInject.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
{$ifndef standalonetrainerwithassembler}

  if not editscript then
  begin
    action:=cafree;
  end
  else
  begin
    try
      if editscript2 then
      begin
        //call finish routine with script

        if modalresult=mrok then
          callbackroutine(memrec, assemblescreen.text,true)
        else
          callbackroutine(memrec, assemblescreen.text,false);

        action:=cafree;
      end
      else
      if CustomTypeScript then
      begin

        if modalresult=mrok then
          CustomTypeCallback(customtype, assemblescreen.text,true,luamode)
        else
          CustomTypeCallback(customtype, assemblescreen.text,false,luamode);

        action:=cafree;
      end;

    except
      on e: exception do
      begin
        modalresult:=mrNone;
        raise exception.create(e.message);
      end;
    end;
  end;
{$endif}
end;

procedure TfrmAutoInject.Codeinjection1Click(Sender: TObject);
function inttostr(i:int64):string;
begin
  if i=0 then result:='' else result:=sysutils.IntToStr(i);
end;

var address: string;
    originalcode: array of string;
    originalbytes: array of byte;
    codesize: integer;
    a: ptrUint;
    br: dword;
    c: ptrUint;
    x: string;
    i,j,k: integer;
    injectnr: integer;

    enablepos: integer;
    disablepos: integer;
    enablecode: tstringlist;
    disablecode: tstringlist;
begin
{$ifndef standalonetrainerwithassembler}

  a:=memorybrowser.disassemblerview.SelectedAddress;

  address:=inttohex(a,8);
  if inputquery('Code inject template','On what address do you want the jump?',address) then
  begin
    try
      a:=strtoint64('$'+address);
    except

      a:=symhandler.getaddressfromname(address);

    end;

    c:=a;

    injectnr:=0;
    for i:=0 to assemblescreen.Lines.Count-1 do
    begin
      j:=pos('alloc(newmem',lowercase(assemblescreen.lines[i]));
      if j<>0 then
      begin
        x:=copy(assemblescreen.Lines[i],j+12,length(assemblescreen.Lines[i]));
        x:=copy(x,1,pos(',',x)-1);
        try
          k:=strtoint(x);
          if injectnr<=k then
            injectnr:=k+1;
        except
          inc(injectnr);
        end;
      end;
    end;


    //disassemble the old code
    setlength(originalcode,0);
    codesize:=0;

    while codesize<5 do
    begin
      setlength(originalcode,length(originalcode)+1);
      originalcode[length(originalcode)-1]:=disassemble(c,x);
      i:=posex('-',originalcode[length(originalcode)-1]);
      i:=posex('-',originalcode[length(originalcode)-1],i+1);
      originalcode[length(originalcode)-1]:=copy(originalcode[length(originalcode)-1],i+2,length(originalcode[length(originalcode)-1]));
      codesize:=c-a;
    end;

    setlength(originalbytes,codesize);
    ReadProcessMemory(processhandle, pointer(a), @originalbytes[0], codesize, br);

    enablecode:=tstringlist.Create;
    disablecode:=tstringlist.Create;
    try
      with enablecode do
      begin
        add('alloc(newmem'+inttostr(injectnr)+',2048) //2kb should be enough');
        add('label(returnhere'+inttostr(injectnr)+')');
        add('label(originalcode'+inttostr(injectnr)+')');
        add('label(exit'+inttostr(injectnr)+')');
        add('');
        add('newmem'+inttostr(injectnr)+': //this is allocated memory, you have read,write,execute access');
        add('//place your code here');

        add('');
        add('originalcode'+inttostr(injectnr)+':');
        for i:=0 to length(originalcode)-1 do
          add(originalcode[i]);
        add('');
        add('exit'+inttostr(injectnr)+':');
        add('jmp returnhere'+inttostr(injectnr)+'');

        add('');
        add(address+':');
        add('jmp newmem'+inttostr(injectnr)+'');
        while codesize>5 do
        begin
          add('nop');
          dec(codesize);
        end;

        add('returnhere'+inttostr(injectnr)+':');
        add('');
      end;

      with disablecode do
      begin
        add('dealloc(newmem'+inttostr(injectnr)+')');
        add(address+':');
        for i:=0 to length(originalcode)-1 do
          add(originalcode[i]);
        x:='db';
        for i:=0 to length(originalbytes)-1 do
          x:=x+' '+inttohex(originalbytes[i],2);
        add('//Alt: '+x);
      end;

      getenableanddisablepos(assemblescreen.lines,enablepos,disablepos);
      //skip first comment(s)
      if enablepos>=0 then
      begin
        while enablepos<assemblescreen.lines.Count-1 do
        begin
          if pos('//',trim(assemblescreen.Lines[enablepos+1]))=1 then inc(enablepos) else break;
        end;
      end;

      for i:=enablecode.Count-1 downto 0 do
        assemblescreen.Lines.Insert(enablepos+1,enablecode[i]);

      getenableanddisablepos(assemblescreen.lines,enablepos,disablepos);
      //skip first comment(s)
      if disablepos>=0 then
      begin
        while disablepos<assemblescreen.lines.Count-1 do
        begin
          if pos('//',trim(assemblescreen.Lines[disablepos+1]))=1 then inc(enablepos) else break;
            inc(disablepos);
        end;
        //only if there actually is a disable section place this code
        for i:=disablecode.Count-1 downto 0 do
          assemblescreen.Lines.Insert(disablepos+1,disablecode[i]);
      end;
    finally
      enablecode.free;
      disablecode.Free;
    end;

  end;

{$endif}
end;

procedure TfrmAutoInject.Panel1Resize(Sender: TObject);
begin
  button1.Left:=panel1.Width div 2-button1.Width div 2;
end;


procedure TfrmAutoInject.CheatTablecompliantcodee1Click(Sender: TObject);
var e,d: integer;
begin
{$ifndef standalonetrainerwithassembler}

  getenableanddisablepos(assemblescreen.lines,e,d);

  if e=-1 then //-2 is 2 or more, so bugged, and >=0 is has one
  begin
    assemblescreen.Lines.Insert(0,'[ENABLE]');
    assemblescreen.Lines.Insert(1,'//code from here to ''[DISABLE]'' will be used to enable the cheat');
    assemblescreen.Lines.Insert(2,'');
  end;

  if d=-1 then
  begin
    assemblescreen.Lines.Add(' ');
    assemblescreen.Lines.Add(' ');
    assemblescreen.Lines.Add('[DISABLE]');
    assemblescreen.Lines.Add('//code from here till the end of the code will be used to disable the cheat');
  end;
{$endif}  
end;

procedure TfrmAutoInject.assemblescreenChange(Sender: TObject);
{$ifndef standalonetrainerwithassembler}
{
var
  TempMS: TMemoryStream;
  FSyntax: TpsvAARTF;
  FSyntax2: TpsvCppRTF;
  pos, top: Integer;
  OnChange: TNotifyEvent;
  }
{$endif}
begin

{$ifndef standalonetrainerwithassembler}
{  undotimer.enabled:=false;
  undotimer.enabled:=true; //if no change for 2 seconds the script gets stored

  if (Length(assemblescreen.Text) <= 0) then
    exit;


  assemblescreen.Lines.BeginUpdate;
  pos := assemblescreen.selstart;
  top := SendMessage(assemblescreen.Handle, EM_GETFIRSTVISIBLELINE, 0, 0);
  OnChange := assemblescreen.OnChange;
  TempMS := TMemoryStream.Create;

  assemblescreen.OnChange := nil;



  try
    if fcplusplus then
      FSyntax2 := TpsvCppRTF.Create
    else
      FSyntax := TpsvAARTF.Create;
      
    try

      try

        if cplusplus then
        begin
          FSyntax2.SetText(assemblescreen.Text);
          FSyntax2.ConvertToRTFStream(TempMS);
        end
        else
        begin
          FSyntax.SetText(assemblescreen.Text);
          FSyntax.ConvertToRTFStream(TempMS);
        end;

        TempMS.Position := 0;
        assemblescreen.PlainText := False;


        assemblescreen.Lines.LoadFromStream(TempMS);
        SendMessage(assemblescreen.Handle, EM_LINESCROLL, 0, top);
        
      finally
        if fcplusplus then
          fsyntax2.free
        else
          FSyntax.Free;
      end;


    except
      assemblescreen.SelAttributes := assemblescreen.DefAttributes;
    end;

  finally
    assemblescreen.PlainText := True;
    assemblescreen.SelStart := Pos;

    TempMS.Free;
    assemblescreen.Lines.EndUpdate;
    assemblescreen.OnChange := OnChange;
  end;   }
{$endif}

end;



procedure TfrmAutoInject.Assigntocurrentcheattable1Click(Sender: TObject);
var a,b: integer;
    aa:TCEAllocArray;
    registeredsymbols: TStringlist;
begin
{$ifndef standalonetrainerwithassembler}
  {$ifndef net}

  registeredsymbols:=tstringlist.Create;
  registeredsymbols.CaseSensitive:=false;
  registeredsymbols.Duplicates:=dupIgnore;
  
  try
    setlength(aa,0);
    getenableanddisablepos(assemblescreen.Lines,a,b);
    if (a=-1) and (b=-1) then raise exception.create('The code needs a [ENABLE] and a [DISABLE] section if you want to add it to a table');

    if autoassemble(assemblescreen.lines,false,true,true,false,aa,registeredsymbols) and
       autoassemble(assemblescreen.lines,false,false,true,false,aa,registeredsymbols) then
    begin
      //add a entry with type 255
      mainform.AddAutoAssembleScript(assemblescreen.text);


    end
    else showmessage('Failed to add to table. Not all code is injectable');
  finally
    registeredsymbols.Free;
  end;
  {$endif}
  {$endif}
end;

procedure Getjumpandoverwrittenbytes(address,addressto: ptrUint; jumppart,originalcodepart: tstrings);
//pre: jumppart and originalcodepart are declared objects
var x,y: ptrUint;
    z: string;
    i: integer;
    ab: TAssemblerBytes;
    jumpsize: integer;
begin
{$ifndef standalonetrainerwithassembler}
  Assemble('jmp '+inttohex(addressto,8),address,ab);
  jumpsize:=length(ab);

  x:=address;
  y:=address;

  while x-y<jumpsize do
  begin
    z:=disassemble(x);
    z:=copy(z,pos('-',z)+1,length(z));
    z:=copy(z,pos('-',z)+1,length(z));

    originalcodepart.add(z);
  end;

  jumppart.Add('jmp '+inttohex(addressto,8));

  for i:=jumpsize to x-y-1 do
    jumppart.Add('nop');
{$endif}
end;


procedure generateAPIHookScript(script: tstrings; address: string; addresstogoto: string; addresstostoreneworiginalfunction: string=''; nameextension:string='0');
var originalcode: array of string;
    originaladdress: array of ptrUint;
    i,j: integer;
    codesize: integer;
    a,b: ptrUint;
    br: dword;
    x: string;

    enablepos,disablepos: integer;
    disablescript: tstringlist;
    enablescript: tstringlist;

    originalcodebuffer: Pbytearray;
    ab: TAssemblerBytes;

    jumpsize: integer;
    tempaddress: ptrUint;

    specifier: array of ptrUint;
    specifiernr: integer;
    s,s2: string;

    d: TDisassembler;

    originalcodestart: integer;
begin
{$ifndef standalonetrainerwithassembler}
  //disassemble the old code
  d:=TDisassembler.Create;
  d.showmodules:=false;
  d.showsymbols:=false;

  setlength(specifier,0);
  setlength(originalcode,0);
  setlength(ab,0);
  specifiernr:=0;


  try
    a:=symhandler.getAddressFromName(address);
  except
    on e: exception do
      raise exception.create(address+':'+e.message);
  end;

  try
    b:=symhandler.getAddressFromName(addresstogoto);
  except
    on e: exception do
      raise exception.create(addresstogoto+':'+e.message);
  end;

  if processhandler.is64bit then
  begin
    //check if there is a region I can make use of for a jump trampoline
    if FindFreeBlockForRegion(a,2048)=nil then
    begin
      Assemble('jmp '+inttohex(b,8),a,ab);
      jumpsize:=length(ab);
    end
    else
      jumpsize:=5;
  end
  else
    jumpsize:=5;



  disablescript:=tstringlist.Create;
  enablescript:=tstringlist.Create;

  codesize:=0;
  b:=a;
  while codesize<jumpsize do
  begin
    setlength(originalcode,length(originalcode)+1);
    setlength(originaladdress,length(originalcode));

    originaladdress[length(originaladdress)-1]:=a;
    originalcode[length(originalcode)-1]:=d.disassemble(a,x);
    i:=posex('-',originalcode[length(originalcode)-1]);
    i:=posex('-',originalcode[length(originalcode)-1],i+1);
    originalcode[length(originalcode)-1]:=copy(originalcode[length(originalcode)-1],i+2,length(originalcode[length(originalcode)-1]));

    codesize:=a-b;
  end;

  getmem(originalcodebuffer,codesize);
  if ReadProcessMemory(processhandle,pointer(b), originalcodebuffer, codesize, br) then
  begin
    disablescript.Add(address+':');
    x:='db';

    for i:=0 to br-1 do
      x:=x+' '+inttohex(originalcodebuffer[i],2);

    disablescript.Add(x);      
  end;

  freemem(originalcodebuffer);



  with enablescript do
  begin
    if not processhandler.is64bit then
      add('alloc(originalcall'+nameextension+',2048) //2kb should be enough')
    else
    begin
      add('alloc(originalcall'+nameextension+',2048,'+address+') //2kb should be enough');
      add('alloc(jumptrampoline'+nameextension+',64,'+address+') //special jump trampoline in the current region (64-bit)');
      add('label(jumptrampoline'+nameextension+'address)');
    end;

    add('label(returnhere'+nameextension+')');
    add('');
    if addresstostoreneworiginalfunction<>'' then
    begin
      add(addresstostoreneworiginalfunction+':');
      if processhandler.is64Bit then
        add('dq originalcall'+nameextension)
      else
        add('dd originalcall'+nameextension);
    end;
    add('');
    add('originalcall'+nameextension+':');

    originalcodestart:=enablescript.Count;

    for i:=0 to length(originalcode)-1 do
    begin
      if hasAddress(originalcode[i], tempaddress, nil ) then
      begin
        if InRangeX(tempaddress, b,b+codesize) then
        begin
          s2:='specifier'+nameextension+inttostr(specifiernr);
          setlength(specifier,length(specifier)+1);
          specifier[specifiernr]:=tempaddress;

          Insert(0,'label('+s2+')');
          if has4ByteHexString(originalcode[i], s) then //should be yes
          begin
            s:=copy(s,2,length(s)-1);

            originalcode[i]:=StringReplace(originalcode[i],s,s2,[rfIgnoreCase]);
          end;

          inc(specifiernr);
        end;
      end;
      add(originalcode[i]);
    end;

    //now find the originalcode line that belongs to the specifier
    inc(originalcodestart,specifiernr);
    for i:=0 to length(specifier)-1 do
    begin
      for j:=0 to length(originaladdress)-1 do
      begin
        if specifier[i]=originaladdress[j] then
        begin
          enablescript[originalcodestart+j]:='specifier'+nameextension+inttostr(i)+':'+enablescript[originalcodestart+j]
        end;
      end;
    end;

    i:=0;

    while i<enablescript.count do
    begin
      j:=pos(':',enablescript[i]);

      if j>0 then
      begin
        s:=enablescript[i];
        s2:=copy(s,j+1,length(s));
        delete(i);
        Insert(i,copy(s,1,j));
        inc(i);
        Insert(i,s2);
      end;

      inc(i);
    end;


    add('jmp returnhere'+nameextension+'');

    add('');

    if processhandler.is64bit then
    begin
      add('jumptrampoline'+nameextension+':');
      add('jmp [jumptrampoline'+nameextension+'address]');
      add('jumptrampoline'+nameextension+'address:');
      add('dq '+addresstogoto);
      add('');
    end;


    add(address+':');
    if processhandler.is64bit then
      add('jmp jumptrampoline'+nameextension)
    else
      add('jmp '+addresstogoto);

    while codesize>jumpsize do
    begin
      add('nop');
      dec(codesize);
    end;

    add('returnhere'+nameextension+':');

    add('');
  end;


  getenableanddisablepos(script,enablepos,disablepos);

  if disablepos<>-1 then
  begin
    for i:=0 to disablescript.Count-1 do
      script.Insert(disablepos+i+1,disablescript[i]);
  end;

  getenableanddisablepos(script,enablepos,disablepos); //idiots putting disable first 

  if enablepos<>-1 then
  begin
    for i:=0 to enablescript.Count-1 do
      script.Insert(enablepos+i+1,enablescript[i]);
  end
  else
    script.AddStrings(enablescript);

  disablescript.free;
  enablescript.free;

  d.free;
{$endif}
end;



procedure TfrmAutoInject.APIHook1Click(Sender: TObject);
function inttostr(i:int64):string;
begin
  if i=0 then result:='' else result:=sysutils.IntToStr(i);
end;

var address: string;

    a: ptrUint;
    x: string;
    i,j,k: integer;

    injectnr: integer;

begin
{$ifndef standalonetrainerwithassembler}

  a:=memorybrowser.disassemblerview.SelectedAddress;

  address:=inttohex(a,8);

  with tfrmapihooktemplatesettings.create(self) do
//  if inputquery('Give the address of the api you want to hook',address) and inputquery('Give the address of the replacement function',address) then
  begin
    try
      injectnr:=0;
      for i:=0 to assemblescreen.Lines.Count-1 do
      begin
        j:=pos('alloc(newmem',lowercase(assemblescreen.lines[i]));
        if j<>0 then
        begin
          x:=copy(assemblescreen.Lines[i],j+12,length(assemblescreen.Lines[i]));
          x:=copy(x,1,pos(',',x)-1);
          try
            k:=strtoint(x);
            if injectnr<=k then
              injectnr:=k+1;
          except
            inc(injectnr);
          end;
        end;
      end;

      edit1.text:=address;
      if showmodal<>mrok then exit;


      generateAPIHookScript(assemblescreen.Lines,edit1.Text, edit2.Text, edit3.Text, inttostr(injectnr)); 


    finally
      free;
    end;
  end;

{$endif}
end;

procedure TfrmAutoInject.SaveAs1Click(Sender: TObject);
begin
  if savedialog1.Execute then
    save1.Click;    
end;

procedure TfrmAutoInject.FormShow(Sender: TObject);
begin
{$ifndef standalonetrainerwithassembler}

  if editscript then
    button1.Caption:=strOK;

  assemblescreen.SetFocus;
{$endif}
end;

procedure TfrmAutoInject.assemblescreenKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
{   if (ssCtrl in Shift) and (key=ord('A'))  then
   begin
     TMemo(Sender).SelectAll;
     Key := 0;
   end; }
end;

procedure TfrmAutoInject.Coderelocation1Click(Sender: TObject);
var starts,stops: string;
    start,stop,current: ptrUint;
    x: ptrUint;
    i,j: integer;

    labels: tstringlist;
    output: tstringlist;
    s: string;

    a,b: string;
    prev: ptrUint;

    ok: boolean;

begin
{$ifndef standalonetrainerwithassembler}

  starts:=inttohex(memorybrowser.disassemblerview.SelectedAddress,8);
  stops:=inttohex(memorybrowser.disassemblerview.SelectedAddress+128,8);

  if inputquery('Start address:','Code relocation template',starts) then
  begin
    start:=strtoint64('$'+starts);
    if inputquery('End address (last bytes are included if necesary)','Code relocation template',stops) then
    begin
      stop:=strtoint64('$'+stops);

      output:=tstringlist.Create;
      labels:=tstringlist.create;
      labels.Duplicates:=dupIgnore;
      labels.Sorted:=true;
      
      output.add('alloc(newmem,'+inttostr(abs(integer(stop-start))*2)+')');
      output.add('');
      output.add('newmem:');


      try
        current:=start;

        while current<stop do
        begin
          prev:=current;
          s:=disassemble(current);
          i:=posex('-',s);
          i:=posex('-',s,i+1);
          s:=copy(s,i+2,length(s));

          i:=pos(' ',s);
          a:=copy(s,1,i-1);
          b:=copy(s,i+1,length(s));


          if length(a)>1 then
          begin
            if (lowercase(a)='loop') or (lowercase(a[1])='j') or (lowercase(a)='call') then
            begin
              try
                x:=symhandler.getAddressFromName(b);
                if (x>=start) and (x<=stop) then
                begin
                  labels.Add('orig_'+inttohex(x,8));
                  s:=a+' orig_'+inttohex(x,8);
                end;
              except
                //nolabel
              end;
            end;
          end;

          output.add('orig_'+inttohex(prev,8)+':');
          output.add(s);
        end;

        labels.Sort;
        //now clean up output so that the result is a readable program
        for i:=0 to labels.Count-1 do
          output.Insert(2+i,'label('+labels[i]+')');

        output.Insert(2+labels.Count,'');

        i:=2+labels.Count+1;
        while i<output.Count do
        begin
          if pos('orig_',output[i])>0 then
          begin
            //determine if it's valid or not
            ok:=false;
            for j:=0 to labels.Count-1 do
              if labels[j]+':'=output[i] then
              begin
                ok:=true;
                break;
              end;

            if not ok then
              output.Delete(i)
            else
            begin
              output.Insert(i,'');
              inc(i,2);
            end;
          end
          else inc(i);
        end;

        assemblescreen.Lines.AddStrings(output);

      finally
        output.free;
      end;

    end;

  end;
{$endif}
end;

procedure TfrmAutoInject.New1Click(Sender: TObject);
var i: integer;
begin
{$ifndef standalonetrainerwithassembler}

  scripts[length(scripts)-1].script:=assemblescreen.Text;
  setlength(scripts,length(scripts)+1);

  scripts[length(scripts)-1].script:='';
  scripts[length(scripts)-1].undoscripts[0].oldscript:='';
  scripts[length(scripts)-1].currentundo:=0;

  assemblescreen.Text:='';


  if length(scripts)=2 then //first time new
  begin
    tlist.AddTab('Script 1');
    tlist.Visible:=true;
  end;

  i:=tlist.AddTab('Script '+inttostr(length(scripts)));
  tlist.SelectedTab:=i;
  oldtabindex:=i;
{$endif}
end;

procedure tfrmautoinject.tlistOnTabChange(sender: TObject; oldselection: integer);
begin
{$ifndef standalonetrainerwithassembler}

  scripts[oldselection].script:=assemblescreen.text;
  scripts[oldselection].filename:=opendialog1.FileName;

  assemblescreen.text:=scripts[tlist.SelectedTab].script;
  opendialog1.FileName:=scripts[tlist.SelectedTab].filename;

  oldtabindex:=tlist.SelectedTab;

  assemblescreen.ClearUndo;

{$endif}
end;

procedure TfrmAutoInject.FormCreate(Sender: TObject);
var x: array of integer;
    reg: tregistry;
begin
{$ifndef standalonetrainerwithassembler}

  setlength(scripts,1);
  scripts[0].currentundo:=0;
  oldtabindex:=0;
{  assemblescreen.SelStart:=0;
  assemblescreen.SelLength:=0; }


  AAHighlighter:=TSynAASyn.Create(self);
  CPPHighlighter:=TSynCppSyn.create(self);
  assembleSearch:=TSyneditSearch.Create;

  tlist:=TTablist.Create(self);
  tlist.height:=20;
  tlist.Align:=alTop;
  tlist.Visible:=false;
  tlist.OnTabChange:=tlistOnTabChange;

  tlist.Parent:=panel2;


  assemblescreen:=TSynEdit.Create(self);
  assemblescreen.Highlighter:=AAHighlighter;
  assemblescreen.Options:=SYNEDIT_DEFAULT_OPTIONS - [eoScrollPastEol]+[eoTabIndent];
  assemblescreen.WantTabs:=true;
  assemblescreen.TabWidth:=4;


  assemblescreen.Gutter.MarksPart.Visible:=false;
  assemblescreen.Gutter.Visible:=true;
  assemblescreen.Gutter.LineNumberPart.Visible:=true;
  assemblescreen.Gutter.LeftOffset:=1;
  assemblescreen.Gutter.RightOffset:=1;

  assemblescreen.Align:=alClient;
  assemblescreen.PopupMenu:=PopupMenu1;
  assemblescreen.Parent:=panel2;

  setlength(x,0);
  loadformposition(self,x);

  reg:=tregistry.create;
  try
    if reg.OpenKey('\Software\Cheat Engine\Auto Assembler\',false) then
    begin
      if reg.valueexists('Font.name') then
        assemblescreen.Font.Name:=reg.readstring('Font.name');

      if reg.valueexists('Font.size') then
        assemblescreen.Font.size:=reg.ReadInteger('Font.size');

      if reg.valueexists('Show Line Numbers') then
        assemblescreen.Gutter.linenumberpart.visible:=reg.ReadBool('Show Line Numbers');

      if reg.valueexists('Show Gutter') then
        assemblescreen.Gutter.Visible:=reg.ReadBool('Show Gutter');

      if reg.valueexists('smart tabs') then
        if reg.ReadBool('smart tabs') then assemblescreen.Options:=assemblescreen.options+[eoSmartTabs];

      if reg.valueexists('tabs to spaces') then
        if reg.ReadBool('tabs to spaces') then assemblescreen.Options:=assemblescreen.options+[eoTabsToSpaces];

      if reg.valueexists('tab width') then
        assemblescreen.tabwidth:=reg.ReadInteger('tab width');
    end;

  finally
    reg.free;
  end;

{$endif}
end;

procedure TfrmAutoInject.TabControl1Change(Sender: TObject);
begin

end;

procedure TfrmAutoInject.Syntaxhighlighting1Click(Sender: TObject);
begin
{$ifndef standalonetrainerwithassembler}

  Syntaxhighlighting1.checked:=not Syntaxhighlighting1.checked;
  if Syntaxhighlighting1.checked then //enable
    assemblescreen.Highlighter:=AAHighlighter
  else //disabl
    assemblescreen.Highlighter:=nil;

{$endif}
end;

procedure TfrmAutoInject.TabControl1ContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  //selectedtab:=TabControl1.IndexOfTabAt(mousepos.x,mousepos.y);
  //closemenu.Popup(mouse.CursorPos.X,mouse.cursorpos.Y);   
end;

procedure TfrmAutoInject.Close1Click(Sender: TObject);
var i: integer;
begin
{$ifndef standalonetrainerwithassembler}


  if messagedlg('Are you sure you want to close '+tlist.TabText[selectedtab]+' ?',mtConfirmation,[mbyes,mbno],0)=mryes then
  begin
    scripts[oldtabindex].script:=assemblescreen.text; //save current script
    tlist.RemoveTab(selectedtab);

    for i:=selectedtab to length(scripts)-2 do
      scripts[i]:=scripts[i+1];

    setlength(scripts,length(scripts)-1);

    if oldtabindex=selectedtab then //it was the current one
    begin
      oldtabindex:=length(scripts)-1;
      tlist.SelectedTab:=oldtabindex;
      assemblescreen.text:=scripts[oldtabindex].script;
      assemblescreen.OnChange(assemblescreen);
    end;

    if (length(scripts)=1) then
    begin
      tlist.RemoveTab(0);
      tlist.Visible:=false;
    end;
//    tabcontrol1.tabs[selectedtab]

  end;
{$endif}
end;

procedure TfrmAutoInject.injectscript(createthread: boolean);
var i: integer;
    setenvscript: tstringlist;
    CEAllocArray: TCEAllocArray;
    callscriptscript: tstringlist;

    totalmem: dword;
    totalwritten: dword;
    address: pointer;
    mi: TModuleInfo;
    hasjustloadedundercdll: boolean;

    aawindowwithstub: tfrmautoinject;
   // setenv_done: dword;
//    setenv_done_value: dword;
    s: string;

    ignore: dword;
    th: thandle;
begin
{$ifndef standalonetrainerwithassembler}

  //this will inject the script dll and generate a assembler script the user can use to call the script
  //first set the environment var for uc_home
  s:=assemblescreen.text;
  if not symhandler.getmodulebyname('undercdll.dll',mi) then
  begin
    //dll was not loaded yet

    setenvscript:=tstringlist.Create;

    with setenvscript do
    begin
      add('[enable]');
      Add('alloc(envname,8)');
      add('alloc(envvar,512)');
      add('alloc(myscript,512)');

      add('envname:');
      add('db ''UC_HOME'',0');
      add('envvar:');
      add('db '''+cheatenginedir+''' ,0');
      add('myscript:');
      add('push envvar');
      add('push envname');
      add('call SetEnvironmentVariableA');
      add('ret');

      //cleanup part:
      add('[disable]');
      add('dealloc(myscript)');
      add('dealloc(envvar)');
      add('dealloc(envname)');
    end;

    setlength(CEAllocArray,1);
    if autoassemble(setenvscript,false,true,false,false,CEAllocArray) then //enabled
    begin
      for i:=0 to length(ceallocarray)-1 do
        if ceallocarray[i].varname='myscript' then
        begin
          th:=createremotethread(processhandle,nil,0,pointer(ceallocarray[i].address),nil,0,ignore);
          if th<>0 then
            waitforsingleobject(th,4000); //4 seconds max
            
          break;
        end;



      //wait done
      autoassemble(setenvscript,false,false,false,false,CEAllocArray); //disable for the deallocs
    end;

    setenvscript.free;


    injectdll(cheatenginedir+'undercdll.dll','');
    symhandler.reinitialize;
    hasjustloadedundercdll:=true;
  end else hasjustloadedundercdll:=false;

  //now allocate memory for the script and write it to there
  totalmem:=length(assemblescreen.text);
  address:=VirtualAllocEx(processhandle,nil,totalmem+512,mem_commit,page_execute_readwrite);
  if address=nil then raise exception.create('Failed allocating memory for the script');
  if not WriteProcessMemory(processhandle,address,@s[1],totalmem,totalwritten) then
    raise exception.create('failed writing the script to the process');



  callscriptscript:=tstringlist.create;
  try
    with callscriptscript do
    begin
      add('label(result)');
      add(inttohex((ptrUint(address)+totalmem+$20) - (ptrUint(address) mod $10),8)+':');
      add('pushfd');
      add('pushad');
      add('push '+inttohex(ptrUint(address),8));
      add('call underc_executescript');
      add('mov [result],eax');
      add('popad');
      add('popfd');
      add('mov eax,[result]');
      add('ret');
      add('result:');
      add('dd 0');
    end;

    if hasjustloadedundercdll then
    begin
      //lets wait before injecting the callscript script
      symhandler.waitforsymbolsloaded;
      if not symhandler.getmodulebyname('undercdll.dll',mi) then
        raise exception.Create('Failure loading undercdll');
    end;
    if not autoassemble(callscriptscript,false,true,false,false,CEAllocArray) then raise exception.Create('Failed creating calling stub for script located at address '+inttohex(ptrUint(address),8));
  finally
    callscriptscript.free;
  end;

  aawindowwithstub:=tfrmautoinject.create(memorybrowser);
  with aawindowwithstub.assemblescreen.Lines do
  begin
    if createthread then
    begin
      add('createthread(myscript)');
      add('alloc(myscript,256)');
      add('myscript:');
    end;

    add('//Call this code to execute the script from assembler');
    add('call '+inttohex((ptrUint(address)+totalmem+$20) - (ptrUint(address) mod $10),8));
    add('');
    add('//eax==0 when successfully executed');
    add('//''call underc_geterror'' to get a pointer to the last generated error buffer');

    if createthread then
      add('ret //interesing thing with createthread is that the return param points to exitthread');
  end;
  aawindowwithstub.show;

{$endif}
end;



procedure TfrmAutoInject.Injectincurrentprocess1Click(Sender: TObject);
begin
  injectscript(false);


end;

procedure TfrmAutoInject.Injectintocurrentprocessandexecute1Click(
  Sender: TObject);
begin
  injectscript(true);
end;

procedure TfrmAutoInject.Cut1Click(Sender: TObject);
begin
  assemblescreen.CutToClipboard;
end;

procedure TfrmAutoInject.Copy1Click(Sender: TObject);
begin
  assemblescreen.CopyToClipboard;
end;

procedure TfrmAutoInject.Paste1Click(Sender: TObject);
begin
  assemblescreen.PasteFromClipboard;
end;

procedure TfrmAutoInject.Find1Click(Sender: TObject);
begin
  finddialog1.Execute;

end;

procedure TfrmAutoInject.FindDialog1Find(Sender: TObject);
begin
  //scan the text for the given text
  assemblescreen.SearchReplace(finddialog1.FindText,'',[]);
end;

//follow is just a emergency fix since undo is messed up. At least it's better than nothing
procedure TfrmAutoInject.AAPref1Click(Sender: TObject);
var reg: tregistry;
begin
  with TfrmAAEditPrefs.create(self) do
  begin
    try
      if execute(assemblescreen) then
      begin
        //save these settings
        reg:=tregistry.create;
        try
          if reg.OpenKey('\Software\Cheat Engine\Auto Assembler\',true) then
          begin
            reg.WriteString('Font.name', assemblescreen.Font.Name);
            reg.WriteInteger('Font.size', assemblescreen.Font.size);
            //assemblescreen.Font.

            reg.WriteBool('Show Line Numbers', assemblescreen.Gutter.linenumberpart.visible);
            reg.WriteBool('Show Gutter', assemblescreen.Gutter.Visible);

            reg.WriteBool('smart tabs', eoSmartTabs in assemblescreen.Options);
            reg.WriteBool('tabs to spaces', eoTabsToSpaces in assemblescreen.Options);
          end;

        finally
          reg.free;
        end;
      end;
    finally
      free;
    end;
  end;
end;

procedure TfrmAutoInject.FormDestroy(Sender: TObject);
begin
  //if editscript or editscript2 then
  begin
    saveformposition(self,[]); 

  end;
end;

procedure TfrmAutoInject.Undo1Click(Sender: TObject);
begin
  assemblescreen.Undo;
end;

initialization
  {$i frmautoinjectunit.lrs}

end.












