unit frmautoinjectunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus,cefuncproc,  StrUtils, types,
  ComCtrls,
  {$ifdef net}
  netapis,
  {$else}
  newkernelhandler,
  {$endif}
  {$ifndef standalonetrainerwithassembler}
  disassembler,
  mainunit2,
  psvAutoAssembler,
  psvCPlusPlus,
  underc,
  SynEdit, SynEditSearch, SynHighlighterAA, SynHighlighterCPP,
  {$endif}
  assemblerunit, autoassembler, symbolhandler;



type
  TfrmAutoInject = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Panel1: TPanel;
    Button1: TButton;
    Load1: TMenuItem;
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
    TabControl1: TTabControl;
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
  private
    { Private declarations }
    {$ifndef standalonetrainerwithassembler}
    AAHighlighter: TSynAASyn;
    CPPHighlighter: TSynCppSyn;
    assembleSearch: TSynEditSearch;
    {$endif}

    updating: boolean;
    pagecontrol: tpagecontrol;
    oldtabindex: integer;
    scripts: array of record
               script: string;
               filename: string;
               undoscripts: array [0..4] of record
                              oldscript: string;
                              startpos: integer;
                            end;
               currentundo: integer;
             end;
             
    selectedtab: integer;

    fcplusplus: boolean;

    undolist: array [0..5] of string;
    procedure setcplusplus(state: boolean);
    procedure injectscript(createthread: boolean);
  public
    { Public declarations }
    {$ifndef standalonetrainerwithassembler}
    assemblescreen: TSynEdit;
    {$endif}
    editscript: boolean;
    editscript2: boolean;
    callbackroutine: procedure(script: string; changed: boolean) of object;
    injectintomyself: boolean;
    property cplusplus: boolean read fcplusplus write setcplusplus; 
  end;

  procedure Getjumpandoverwrittenbytes(address,addressto: dword; jumppart,originalcodepart: tstrings);
  procedure generateAPIHookScript(script: tstrings; address: string; addresstogoto: string; addresstostoreneworiginalfunction: string=''; nameextension:string='0');

implementation

{$R *.dfm}

{$ifndef standalonetrainerwithassembler}
uses frmAAEditPrefsUnit,memorybrowserformunit,APIhooktemplatesettingsfrm,{$ifdef net}unit2{$else}mainunit{$endif};
{$endif}

procedure TfrmAutoInject.setcplusplus(state: boolean);
begin
{$ifndef standalonetrainerwithassembler}
  fcplusplus:=state;
  if state then
  begin
    assemblescreen.Highlighter:=CPPHighlighter;

    //change gui to c++ style
    button1.Caption:='Execute script';
    opendialog1.DefaultExt:='CEC';
    opendialog1.Filter:='Cheat Engine Script (*.CEC)|*.CEC|All Files (*.*)|*.*';
    savedialog1.DefaultExt:='CEC';
    savedialog1.Filter:='Cheat Engine Script (*.CEC)|*.CEC|All Files (*.*)|*.*';
    Assigntocurrentcheattable1.visible:=false;
    emplate1.Visible:=false;
    caption:='Script engine';
    inject1.Visible:=true;
    helpcontext:=19; //c-script help
  end
  else
  begin
    assemblescreen.Highlighter:=AAHighlighter;

    //change gui to autoassembler style
    button1.caption:='Write code';
    opendialog1.DefaultExt:='CEA';
    opendialog1.Filter:='Cheat Engine Assembly (*.CEA)|*.CEA|All Files (*.*)|*.*';
    savedialog1.DefaultExt:='CES';
    savedialog1.Filter:='Cheat Engine Assembly (*.CEA)|*.CEA|All Files (*.*)|*.*';
    Assigntocurrentcheattable1.Visible:=true;
    emplate1.Visible:=true;
    caption:='Auto assembler';
    inject1.Visible:=false;
    helpcontext:=18; //auto asm help
  end;
{$endif}
end;


procedure TfrmAutoInject.Button1Click(Sender: TObject);
var enable,disable: integer;
    a,b: integer;

    aa: TCEAllocArray;
    i: integer;

    //variables for injectintomyself:
    check: boolean;
    oldProcessID: dword;
    oldProcessHandle: thandle;
begin
{$ifndef standalonetrainerwithassembler}
  if cplusplus then
  begin
    //scriptengine stuff
    if not editscript and scriptengine.beginScript then
    begin
      try
       // for i:=0 to assemblescreen.Lines.Count-1 do
          if not scriptengine.execute_command(assemblescreen.text) then
            raise exception.Create('Error interpreting script:'+scriptengine.getError);



      finally
        scriptengine.endScript;
      end;

      showmessage('script executed');
    end;


  end
  else
  begin
    if editscript then
    begin
      //check if both scripts are valid before allowing the edit

      setlength(aa,1);
      getenableanddisablepos(assemblescreen.Lines,a,b);
      if (a=-1) and (b=-1) then raise exception.create('The code needs an [ENABLE] and a [DISABLE] section if you want to use this script as a table entry');

      if injectintomyself then
      begin
        //save the current process and target CE
        oldProcessID:=processid;
        oldProcessHandle:=processhandle;
        processid:=Getcurrentprocessid;
        processhandle:=getcurrentprocess;
      end;

      check:=autoassemble(assemblescreen.lines,false,true,true,false,aa) and
             autoassemble(assemblescreen.lines,false,false,true,false,aa);

      if injectintomyself then
      begin
        //restore back to original process
        processid:=oldProcessID;
        processhandle:=oldProcessHandle;
      end;

      if check then
      begin
        modalresult:=mrok; //not modal anymore, but can still be used to pass info
        if editscript2 then close; //can only be used when not modal
      end
      else
      begin
        if messagedlg('Not all code is injectable. Are you sure you wan''t to edit it to this?',mtWarning,[mbyes,mbno],0)=mryes then
        begin
          modalresult:=mrok; //not modal anymore, but can still be used to pass info
          if editscript2 then close;
        end;
      end;
    end else autoassemble(assemblescreen.lines,true);
  end;
{$endif}
end;

procedure TfrmAutoInject.Load1Click(Sender: TObject);
begin
{$ifndef standalonetrainerwithassembler}
  if opendialog1.Execute then
  begin
    assemblescreen.Clear;
    assemblescreen.Lines.LoadFromFile(opendialog1.filename);
    savedialog1.FileName:=opendialog1.filename;
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
    if editscript2 then
    begin
      //call finish routine with script

      if modalresult=mrok then
        callbackroutine(assemblescreen.text,true)
      else
        callbackroutine(assemblescreen.text,false);

      action:=cafree;
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
    addressdw: dword;
    originalcode: array of string;
    originalbytes: array of byte;
    codesize: integer;
    a,b: dword;
    x: string;
    i,j,k: integer;
    prev_usesymbols: boolean;
    injectnr: integer;

    enablepos: integer;
    disablepos: integer;
    enablecode: tstringlist;
    disablecode: tstringlist;
begin
{$ifndef standalonetrainerwithassembler}
  a:=memorybrowser.dselected;

  address:=inttohex(a,8);
  if inputquery('On what address do you want the jump?','Code inject template',address) then
  begin
    try
      a:=strtoint('$'+address);
    except

      a:=symhandler.getaddressfromname(address);

    end;

    b:=a;

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
      originalcode[length(originalcode)-1]:=disassemble(a,x);
      i:=posex('-',originalcode[length(originalcode)-1]);
      i:=posex('-',originalcode[length(originalcode)-1],i+1);
      originalcode[length(originalcode)-1]:=copy(originalcode[length(originalcode)-1],i+2,length(originalcode[length(originalcode)-1]));
      codesize:=a-b;
    end;

    setlength(originalbytes,codesize);
    ReadProcessMemory(processhandle, pointer(b), @originalbytes[0], codesize, a);

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
        add(address+':');
        add('jmp newmem'+inttostr(injectnr)+'');
        while codesize>5 do
        begin
          add('nop');
          dec(codesize);
        end;

        add('returnhere'+inttostr(injectnr)+':');
        add('');
        add('newmem'+inttostr(injectnr)+': //this is allocated memory, you have read,write,execute access');
        add('//place your code here');

        add('');
        add('');
        add('originalcode'+inttostr(injectnr)+':');
        for i:=0 to length(originalcode)-1 do
          add(originalcode[i]);
        add('');
        add('exit'+inttostr(injectnr)+':');
        add('jmp returnhere'+inttostr(injectnr)+'');
      end;

      with disablecode do
      begin
        add('dealloc(newmem'+inttostr(injectnr)+')');
        add(address+':');
        for i:=0 to length(originalcode)-1 do
          add(originalcode[i]);
        x:='db';
        for i:=0 to codesize-1 do
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
begin
{$ifndef standalonetrainerwithassembler}
  assemblescreen.Lines.Insert(0,'[ENABLE]');
  assemblescreen.Lines.Insert(1,'//code from here to ''[DISABLE]'' will be used to enable the cheat');
  assemblescreen.Lines.Insert(2,'');

  assemblescreen.Lines.Add(' ');
  assemblescreen.Lines.Add(' ');
  assemblescreen.Lines.Add('[DISABLE]');
  assemblescreen.Lines.Add('//code from here till the end of the code will be used to disable the cheat');
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
begin
{$ifndef standalonetrainerwithassembler}
  {$ifndef net}
  setlength(aa,1);
  getenableanddisablepos(assemblescreen.Lines,a,b);
  if (a=-1) and (b=-1) then raise exception.create('The code needs a [ENABLE] and a [DISABLE] section if you want to add it to a table');

  if autoassemble(assemblescreen.lines,false,true,true,false,aa) and
     autoassemble(assemblescreen.lines,false,false,true,false,aa) then
  begin
    //add a entry with type 255
    mainform.AddAutoAssembleScript(assemblescreen.text);


  end
  else showmessage('Failed to add to table. Not all code is injectable');
  {$endif}
  {$endif}
end;

procedure Getjumpandoverwrittenbytes(address,addressto: dword; jumppart,originalcodepart: tstrings);
//pre: jumppart and originalcodepart are declared objects
var x,y: dword;
    z: string;
    i: integer;
begin
{$ifndef standalonetrainerwithassembler}
  x:=address;
  y:=address;

  while x-y<5 do
  begin
    z:=disassemble(x);
    z:=copy(z,pos('-',z)+1,length(z));
    z:=copy(z,pos('-',z)+1,length(z));

    originalcodepart.add(z);
  end;

  jumppart.Add('jmp '+inttohex(addressto,8));

  for i:=5 to x-y-1 do
    jumppart.Add('nop');
{$endif}
end;


procedure generateAPIHookScript(script: tstrings; address: string; addresstogoto: string; addresstostoreneworiginalfunction: string=''; nameextension:string='0');
var originalcode: array of string;
    i: integer;
    codesize: integer;
    a,b: dword;
    x: string;
begin
{$ifndef standalonetrainerwithassembler}
  //disassemble the old code
  setlength(originalcode,0);



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

  codesize:=0;
  b:=a;
  while codesize<5 do
  begin
    setlength(originalcode,length(originalcode)+1);
    originalcode[length(originalcode)-1]:=disassemble(a,x);
    i:=posex('-',originalcode[length(originalcode)-1]);
    i:=posex('-',originalcode[length(originalcode)-1],i+1);
    originalcode[length(originalcode)-1]:=copy(originalcode[length(originalcode)-1],i+2,length(originalcode[length(originalcode)-1]));
    codesize:=a-b;
  end;

  with script do
  begin
    add('alloc(originalcall'+nameextension+',2048) //2kb should be enough');
    add('label(returnhere'+nameextension+')');
    add('');
    if addresstostoreneworiginalfunction<>'' then
    begin
      add(addresstostoreneworiginalfunction+':');
      add('dd originalcall'+nameextension);
    end;
    add('');
    add('originalcall'+nameextension+':');

    for i:=0 to length(originalcode)-1 do
      add(originalcode[i]);
    add('jmp returnhere'+nameextension+'');

    add('');
    
    add(address+':');
    add('jmp '+addresstogoto);
    while codesize>5 do
    begin
      add('nop');
      dec(codesize);
    end;
    add('returnhere'+nameextension+':');

    add('');



  end;
{$endif}
end;



procedure TfrmAutoInject.APIHook1Click(Sender: TObject);
function inttostr(i:int64):string;
begin
  if i=0 then result:='' else result:=sysutils.IntToStr(i);
end;

var address: string;
    addressdw: dword;
    originalcode: array of string;
    codesize: integer;
    a,b,c: dword;
    x: string;
    i,j,k: integer;
    prev_usesymbols: boolean;
    injectnr: integer;

begin
{$ifndef standalonetrainerwithassembler}
  a:=memorybrowser.dselected;

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
  if editscript then button1.Caption:=strOK;

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
    start,stop,current,x: dword;
    i,j: integer;

    labels: tstringlist;
    output: tstringlist;
    s: string;

    a,b: string;
    prev: dword;

    ok: boolean;

begin
{$ifndef standalonetrainerwithassembler}
  starts:=inttohex(memorybrowser.dselected,8);
  stops:=inttohex(memorybrowser.dselected+128,8);

  if inputquery('Start address:','Code relocation template',starts) then
  begin
    start:=strtoint('$'+starts);
    if inputquery('End address (last bytes are included if necesary)','Code relocation template',stops) then
    begin
      stop:=strtoint('$'+stops);

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
begin
{$ifndef standalonetrainerwithassembler}
  scripts[length(scripts)-1].script:=assemblescreen.Text;
  setlength(scripts,length(scripts)+1);

  scripts[length(scripts)-1].script:='';
  scripts[length(scripts)-1].undoscripts[0].oldscript:='';
  scripts[length(scripts)-1].currentundo:=0;

  assemblescreen.Text:='';

  if length(scripts)=2 then //first time new
    tabcontrol1.Tabs.Add('Script 1');

  tabcontrol1.Tabs.Add('Script '+inttostr(length(scripts)));
  tabcontrol1.TabIndex:=length(scripts)-1;
  oldtabindex:=tabcontrol1.TabIndex;
{$endif}
end;

procedure TfrmAutoInject.FormCreate(Sender: TObject);
begin
{$ifndef standalonetrainerwithassembler}
  setlength(scripts,1);
  scripts[0].currentundo:=0;
  oldtabindex:=0;
{  assemblescreen.SelStart:=0;
  assemblescreen.SelLength:=0; }

  AAHighlighter:=TSynAASyn.Create(self);
  CPPHighlighter:=TSynCppSyn.create(self);
  assembleSearch:=TSyneditSearch.Create(self);


  assemblescreen:=TSynEdit.Create(self);
  assemblescreen.Highlighter:=AAHighlighter;
  assemblescreen.SearchEngine:=assembleSearch;
  assemblescreen.Options:=SYNEDIT_DEFAULT_OPTIONS;
  assemblescreen.WantTabs:=true;
  assemblescreen.TabWidth:=2;


  assemblescreen.Gutter.Visible:=true;
  assemblescreen.Gutter.ShowLineNumbers:=true;
  assemblescreen.Gutter.LeftOffset:=1;
  assemblescreen.Gutter.RightOffset:=1;
  assemblescreen.Gutter.DigitCount:=3;

  assemblescreen.Align:=alClient;
  assemblescreen.PopupMenu:=PopupMenu1;
  assemblescreen.Parent:=tabcontrol1;
{$endif}
end;

procedure TfrmAutoInject.TabControl1Change(Sender: TObject);
begin
{$ifndef standalonetrainerwithassembler}
  scripts[oldtabindex].script:=assemblescreen.text;
  scripts[oldtabindex].filename:=opendialog1.FileName;

  assemblescreen.text:=scripts[TabControl1.TabIndex].script;
  opendialog1.FileName:=scripts[TabControl1.TabIndex].filename;

  oldtabindex:=tabcontrol1.TabIndex;
{$endif}
end;

procedure TfrmAutoInject.Syntaxhighlighting1Click(Sender: TObject);
var s: string;
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
  if messagedlg('Are you sure you want to close '+TabControl1.Tabs[selectedtab]+' ?',mtConfirmation,[mbyes,mbno],0)=mryes then
  begin
    scripts[oldtabindex].script:=assemblescreen.text; //save current script
    tabcontrol1.Tabs.Delete(selectedtab);

    for i:=selectedtab to length(scripts)-2 do
      scripts[i]:=scripts[i+1];

    setlength(scripts,length(scripts)-1);

    if oldtabindex=selectedtab then //it was the current one
    begin
      oldtabindex:=length(scripts)-1;
      tabcontrol1.TabIndex:=oldtabindex;
      assemblescreen.text:=scripts[oldtabindex].script;
      assemblescreen.OnChange(assemblescreen);
    end;

    if (length(scripts)=1) then
      tabcontrol1.Tabs.Delete(0);

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
    setenv_done: dword;
    setenv_done_value: dword;
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
      add(inttohex((dword(address)+totalmem+$20) - (dword(address) mod $10),8)+':');
      add('pushfd');
      add('pushad');
      add('push '+inttohex(dword(address),8));
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
    if not autoassemble(callscriptscript,false,true,false,false,CEAllocArray) then raise exception.Create('Failed creating calling stub for script located at address '+inttohex(dword(address),8));
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
    add('call '+inttohex((dword(address)+totalmem+$20) - (dword(address) mod $10),8));
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
{$ifndef standalonetrainerwithassembler}
  assemblescreen.CutToClipboard;
{$endif}
end;

procedure TfrmAutoInject.Copy1Click(Sender: TObject);
begin
{$ifndef standalonetrainerwithassembler}
  assemblescreen.CopyToClipboard;
{$endif}
end;

procedure TfrmAutoInject.Paste1Click(Sender: TObject);
begin
{$ifndef standalonetrainerwithassembler}
  assemblescreen.PasteFromClipboard;
{$endif}
end;

procedure TfrmAutoInject.Find1Click(Sender: TObject);
begin
  finddialog1.Execute;

end;

procedure TfrmAutoInject.FindDialog1Find(Sender: TObject);
var start,l: integer;
    p: integer;
begin
{$ifndef standalonetrainerwithassembler}
  //scan the text for the given text
  start:=assemblescreen.selstart;
  l:=length(assemblescreen.text)-start;


  assemblescreen.SearchReplace(finddialog1.FindText,'',[]);
{$endif}
end;

//follow is just a emergency fix since undo is messed up. At least it's better than nothing
procedure TfrmAutoInject.AAPref1Click(Sender: TObject);
begin
{$ifndef standalonetrainerwithassembler}
  with TfrmAAEditPrefs.create(self) do
  begin
    try
      if execute(assemblescreen) then
      begin
        //save these settings
      end;
    finally
      free;
    end;
  end;
{$endif}
end;

end.












