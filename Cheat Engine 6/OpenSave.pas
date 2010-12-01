unit OpenSave;

{$MODE Delphi}

          //should be called loadsave but oh well...

interface


uses windows, forms, MainUnit,LCLIntf,{standaloneunit,}SysUtils,AdvancedOptionsUnit,CommentsUnit,
     CEFuncProc,classes,{formmemorymodifier,formMemoryTrainerUnit,}shellapi,
     {MemoryTrainerDesignUnit,}StdCtrls,{ExtraTrainerComponents,}Graphics,Controls, tableconverter,
     ExtCtrls,Dialogs,NewKernelHandler, hotkeyhandler, structuresfrm, comctrls,dom, xmlread,xmlwrite;


var CurrentTableVersion: dword=10;
procedure SaveTable(Filename: string);
procedure LoadTable(Filename: string;merge: boolean);
procedure SaveCEM(Filename:string;address:ptrUint; size:dword);

{procedure LoadExe(filename: string);}


//procedure SaveCTEntryToXMLNode(i: integer; Entries: TDOMNode);


{type TCEPointer=record
  Address: Dword;  //only used when last pointer in list
  offset: dword;
end;}

type
  MemoryRecordV10 = record
        Description : string;
        Address : ptrUint;
        interpretableaddress: string;
        VarType : byte;
        unicode : boolean;
        IsPointer: Boolean;
        pointers: array of TCEPointer;
        Bit     : Byte;
        bitlength: integer;
        Frozen : boolean;
        FrozenValue : Int64;
        OldValue: string;   //not saved
        Frozendirection: integer; //0=always freeze,1=only freeze when going up,2=only freeze when going down
        Group:  Byte;
        ShowAsHex: boolean;
        autoassemblescript: string;
        allocs: TCEAllocArray;
  end;

type
  MemoryRecordV6 = record
        Description : string;
        Address : dword;
        interpretableaddress: string;
        VarType : byte;
        unicode : boolean;
        IsPointer: Boolean;
        pointers: array of TCEPointer;
        Bit     : Byte;
        bitlength: integer;
        Frozen : boolean;
        FrozenValue : Int64;
        OldValue: string;   //not saved
        Frozendirection: integer; //0=always freeze,1=only freeze when going up,2=only freeze when going down
        Group:  Byte;
        ShowAsHex: boolean;
        autoassemblescript: string;
        allocs: TCEAllocArray;
  end;

  MemoryRecordV5 = record
        Description : string;
        Address : dword;
        VarType : byte;
        Unicode : boolean;
        IsPointer: Boolean;
        pointers: array of TCEPointer;
        Bit     : Byte;
        bitlength: integer;
        Frozen : boolean;
        FrozenValue : Int64;
        Frozendirection: integer; //0=always freeze,1=only freeze when going up,2=only freeze when going down
        Group:  Byte;
        ShowAsHex: boolean;
  end;

  MemoryRecordV4 = record
        Description : string[50];
        Address : dword;
        VarType : byte;
        IsPointer: Boolean;
        pointers: array of TCEPointer;
        Bit     : Byte;
        bitlength: integer;
        Frozen : boolean;
        FrozenValue : Int64;
        Frozendirection: integer; //0=always freeze,1=only freeze when going up,2=only freeze when going down
        Group:  Byte;
        ShowAsHex: boolean;
  end;


  MemoryRecordV3 = record
        Description : string[50];
        Address : dword;
        VarType : byte;
        IsPointer: Boolean;
        pointers: array of TCEPointer;
        Bit     : Byte;
        bitlength: integer;
        Frozen : boolean;
        FrozenValue : Int64;
        Frozendirection: integer; //0=always freeze,1=only freeze when going up,2=only freeze when going down
        Group:  Byte;
  end;

  MemoryRecordV2 = record
        Description : string[50];
        Address : dword;
        VarType : byte;
        Bit     : Byte;
        bitlength: integer;
        Frozen : boolean;
        FrozenValue : Int64;
        Frozendirection: integer; //0=always freeze,1=only freeze when going up,2=only freeze when going down
        Group:  Byte;
  end;

  MemoryRecordV1 = record
        Description : string[50];
        Address : dword;
        VarType : byte;
        Bit     : Byte;
        bitlength: integer;
        Frozen : boolean;
        FrozenValue : Int64;
        Group:  Byte;
        x,y: dword;
  end;

type
  MemoryRecordcet3 = record
        Description : string[50];
        Address : dword;
        VarType : byte;
        Bit     : Byte;
        Frozen : boolean;
        FrozenValue : Int64;
        Group:  Byte;
  end;

type
  MemoryRecordOld = record
        Description : string[50];
        Address : dword;
        VarType : byte;
        Frozen : boolean;
        FrozenValue : Dword;
  end;

procedure LoadStructFromXMLNode(var struct: TbaseStructure; Structure: TDOMNode);

procedure SaveStructToXMLNode(struct: TbaseStructure; Structures: TDOMNode);

{$ifdef net}
var processhandle: thandle;
{$endif}

resourcestring strunknowncomponent='There is a unknown component in the trainer! compnr=';

implementation

uses mainunit2, symbolhandler, LuaHandler;



resourcestring
  strCorruptIcon='The icon has been corrupted';
  strCantLoadFilepatcher='The file patcher can''t be loaded by Cheat Engine!';
  strNotACETrainer='This is not a trainer made by Cheat Engine (If it is a trainer at all!)';
  strUnknownTrainerVersion='This version of Cheat Engine doesn''t know how to read this trainer! Trainerversion=';
  strCantLoadProtectedfile='This trainer is protected from being opened by CE. Now go away!!!';

procedure LoadStructFromXMLNode(var struct: TbaseStructure; Structure: TDOMNode);
var tempnode: TDOMNode;
    elements: TDOMNode;
    element: TDOMNode;
    i: integer;
    currentOffset: dword;
    findoffset: boolean;
    doc: TDOMDocument;
begin
  doc:=Structure.OwnerDocument;

  currentoffset:=0;

  if structure.NodeName='Structure' then
  begin
    tempnode:=structure.FindNode('Name');
    if tempnode<>nil then
      struct.name:=tempnode.TextContent;

    elements:=structure.FindNode('Elements');
    setlength(struct.structelement, elements.ChildNodes.Count);



    for i:=0 to length(struct.structelement)-1 do
    begin
      element:=elements.ChildNodes[i];
      findoffset:=true;
      tempnode:=element.FindNode('Offset');
      if tempnode<>nil then
      begin
        try
          struct.structelement[i].offset:=strtoint(tempnode.textcontent);
          findoffset:=false; //the offset was fetched properly, no need to calculate it
        except

        end;
      end;

      if findoffset then //it couldn't be read out
        struct.structelement[i].offset:=currentoffset;  //calculated offset

      tempnode:=element.FindNode('Description');
      if tempnode<>nil then
        struct.structelement[i].description:=tempnode.TextContent;

      tempnode:=element.FindNode('PointerTo');
      struct.structelement[i].pointerto:=(tempnode<>nil) and (tempnode.TextContent='1');

      tempnode:=element.FindNode('PointerToSize');
      if tempnode<>nil then
        struct.structelement[i].pointertosize:=strtoint(tempnode.TextContent);

      tempnode:=element.FindNode('Structurenr');
      if tempnode<>nil then
        struct.structelement[i].structurenr:=strtoint(tempnode.TextContent);

      tempnode:=element.FindNode('Bytesize');
      if tempnode<>nil then
        struct.structelement[i].Bytesize:=strtoint(tempnode.TextContent);


      currentoffset:=struct.structelement[i].offset+struct.structelement[i].Bytesize;
    end;
  end;

  sortStructure(struct);
end;


procedure LoadXML(doc: TXMLDocument; merge: boolean);
var newrec: MemoryRecordV6;
    CheatTable: TDOMNode;
    Entries, Codes, Symbols, Comments, luascript: TDOMNode;
    CheatEntry, CodeEntry, SymbolEntry: TDOMNode;
    Structures, Structure: TDOMNode;
    Offsets: TDOMNode;

    tempnode, tempnode2: TDOMNode;
    i,j: integer;
    addrecord: boolean;

    tempbefore: array of byte;
    tempactual: array of byte;
    tempafter: array of byte;
    tempaddress: ptrUint;
    tempdescription,tempmodulename: string;
    tempoffset: dword;

    symbolname: string;
    address: ptrUint;
    li: tlistitem;
begin

  try



    CheatTable:=doc.FindNode('CheatTable');
    if CheatTable<>nil then
    begin
      Entries:=CheatTable.FindNode('CheatEntries');
      Codes:=CheatTable.FindNode('CheatCodes');
      Symbols:=CheatTable.FindNode('UserdefinedSymbols');
      Structures:=CheatTable.FindNode('Structures');
      Comments:=CheatTable.FindNode('Comments');
      LuaScript:=CheatTable.FindNode('LuaScript');
    end;

    if entries<>nil then
      mainform.addresslist.loadTableXMLFromNode(entries);


    if codes<>nil then
    begin
      for i:=0 to codes.ChildNodes.Count-1 do
      begin
        CodeEntry:=codes.ChildNodes[i];

        if CodeEntry.NodeName='CodeEntry' then
        begin
          tempnode:=CodeEntry.FindNode('Description');
          if tempnode<>nil then
            tempdescription:=tempnode.TextContent
          else
            tempdescription:='...';

          tempaddress:=0;
          tempnode:=CodeEntry.FindNode('Address');
          if tempnode<>nil then
          begin
            try
              tempaddress:=strtoint64('$'+tempnode.TextContent);
            except
            end;
          end;

          tempnode:=CodeEntry.FindNode('ModuleName');
          if tempnode<>nil then
            tempmodulename:=tempnode.TextContent
          else
            tempmodulename:='';

          tempoffset:=0;
          tempnode:=CodeEntry.FindNode('ModuleNameOffset');
          if tempnode<>nil then
          begin
            try
              tempoffset:=strtoint('$'+tempnode.TextContent);
            except

            end;
          end;

          tempnode:=CodeEntry.FindNode('Before');
          if tempnode<>nil then
          begin
            setlength(tempbefore,tempnode.ChildNodes.Count);
            for j:=0 to tempnode.ChildNodes.Count-1 do
            begin
              try
                tempbefore[j]:=strtoint('$'+tempnode.ChildNodes[j].TextContent);
              except

              end;
            end;
          end else setlength(tempbefore,0);

          tempnode:=CodeEntry.FindNode('Actual');
          if tempnode<>nil then
          begin
            setlength(tempactual,tempnode.ChildNodes.Count);
            for j:=0 to tempnode.ChildNodes.Count-1 do
            begin
              try
                tempactual[j]:=strtoint('$'+tempnode.ChildNodes[j].TextContent);
              except

              end;
            end;
          end else setlength(tempactual,0);

          tempnode:=CodeEntry.FindNode('After');
          if tempnode<>nil then
          begin
            setlength(tempafter,tempnode.ChildNodes.Count);
            for j:=0 to tempnode.ChildNodes.Count-1 do
            begin
              try
                tempafter[j]:=strtoint('$'+tempnode.ChildNodes[j].TextContent);
              except

              end;
            end;
          end else setlength(tempafter,0);



          with advancedoptions do
          begin
            inc(numberofcodes);
            setlength(code,numberofcodes);

            setlength(code[numberofcodes-1].before,length(tempbefore));
            for j:=0 to length(tempbefore)-1 do
              code[numberofcodes-1].before[j]:=tempbefore[j];

            setlength(code[numberofcodes-1].actualopcode,length(tempactual));
            for j:=0 to length(tempactual)-1 do
              code[numberofcodes-1].actualopcode[j]:=tempactual[j];

            setlength(code[numberofcodes-1].after,length(tempafter));
            for j:=0 to length(tempafter)-1 do
              code[numberofcodes-1].after[j]:=tempafter[j];

            code[numberofcodes-1].Address:=tempaddress;
            code[numberofcodes-1].modulename:=tempmodulename;
            code[numberofcodes-1].offset:=tempoffset;

            li:=codelist2.Items.Add;
            if code[numberofcodes-1].modulename<>'' then
              li.Caption:=code[numberofcodes-1].modulename+'+'+inttohex(code[numberofcodes-1].offset,1)
            else
              li.Caption:=inttohex(tempaddress,8);

            li.SubItems.Add(tempdescription);
          end;

        end;
      end;
    end;

    if symbols<>nil then
    begin
      for i:=0 to symbols.ChildNodes.Count-1 do
      begin
        SymbolEntry:=symbols.ChildNodes[i];
        if SymbolEntry.NodeName='SymbolEntry' then
        begin
          tempnode:=SymbolEntry.FindNode('Name');
          if tempnode<>nil then
            symbolname:=tempnode.TextContent
          else
            symbolname:='...';

          address:=0;
          tempnode:=SymbolEntry.FindNode('Address');
          if tempnode<>nil then
          begin
            try
              symhandler.DeleteUserdefinedSymbol(symbolname);
              symhandler.AddUserdefinedSymbol(tempnode.TextContent,symbolname);
            except

            end;
          end;


        end;
      end;
    end;

    if Structures<>nil then
    begin
      setlength(definedstructures, Structures.ChildNodes.Count);
      for i:=0 to Structures.ChildNodes.Count-1 do
      begin
        Structure:=Structures.ChildNodes[i];
        LoadStructFromXMLNode(definedstructures[i], Structure);
      end;
    end;

    if comments<>nil then
      Commentsunit.Comments.Memo1.text:=comments.TextContent;

    if luaScript<>nil then
      Commentsunit.Comments.mLuaScript.text:=luaScript.TextContent;

    if Commentsunit.Comments.mLuaScript.text<>'' then
    begin
      try
        LUA_DoScript(Commentsunit.Comments.mLuaScript.text);
      except
        on e: exception do
        begin
          raise Exception.create('Error executing this table''s lua script: '+e.message);
        end;
      end;
    end;

  finally

  end;

end;


procedure SaveCEM(Filename:string;address:ptrUint; size:dword);
var memfile: TFilestream;
    buf: pointer;
    temp:dword;
    a: qword;
begin
  memfile:=Tfilestream.Create(filename,fmCreate);
  buf:=nil;
  try
    getmem(buf,size);
    if readprocessmemory(processhandle,pointer(address),buf,size,temp) then
    begin
      memfile.WriteBuffer(pchar('CHEATENGINE')^,11);
      temp:=2; //version
      memfile.WriteBuffer(temp,4);
      a:=address;
      memfile.WriteBuffer(a,8);
      memfile.WriteBuffer(buf^,size);
    end else messagedlg('The region at '+IntToHex(address,8)+' was partially or completly unreadable',mterror,[mbok],0);
  finally
    memfile.free;
    freemem(buf);
  end;
end;

procedure LoadCEM(filename:string);
var memfile: TFilestream;
    check: pchar;
    mem: pointer;
    temp,ar:dword;
    a:dword;
begin
  check:=nil;
  try
    memfile:=Tfilestream.Create(filename,fmopenread);
    getmem(check,12);
    memfile.ReadBuffer(check^,11);
    check[11]:=#0;
    if check='CHEATENGINE' then
    begin
      memfile.ReadBuffer(temp,4);
      if temp<>1 then raise exception.Create('The version of '+filename+' is incompatible with this CE version');
      memfile.ReadBuffer(temp,4);
      //temp=startaddress

      getmem(mem,memfile.Size-memfile.Position);
      memfile.ReadBuffer(mem^,memfile.Size-memfile.Position);


      RewriteCode(processhandle,temp,mem,memfile.Size-memfile.Position);
    end else raise exception.Create(filename+' doesn''t contain needed information where to place the memory');
  finally
    freemem(check);
    memfile.free;
  end;
end;



procedure LoadCT(filename: string; merge: boolean);
var ctfile: TFilestream;
    version: dword;
    x: pchar;
    f: TSearchRec;

    doc: TXMLDocument;
begin
  ctfile:=nil;
  doc:=nil;
  ctfile:=Tfilestream.Create(filename,fmopenread or fmsharedenynone);
  try
    x:=nil;
    getmem(x,12);
    ctfile.ReadBuffer(x^,11);
    x[11]:=#0;  //write a 0 terminator
    freeandnil(ctfile);



    if x[0]<>'<' then //not xml
      doc:=ConvertCheatTableToXML(filename);

    try
      if doc=nil then
        ReadXMLFile(doc, filename);

      LoadXML(doc, merge);
    except
      raise exception.Create('This is not a valid cheat table');
    end;

  finally
    if x<>nil then
      freemem(x);

    if ctfile<>nil then
      ctfile.free;

    if doc<>nil then
      doc.free;
  end;


end;


procedure LoadTable(Filename: string;merge: boolean);
var
    actualread: Integer;
    i,j: Integer;

    oldrec: MemoryrecordOld;
    ct3rec: MemoryRecordCET3;

    NewRec: MemoryRecordV2;
    NewRec2: array [0..255] of MemoryrecordV2;
    Extension: String;
    Str: String;
    records: Dword;
    x: Pchar;
    charstoread: byte;

    nrofbytes:  byte;
    doc: TXMLDocument;
begin

  Extension:=uppercase(extractfileext(filename));

  if not merge then
  begin
    //delete everything

    with advancedoptions do
    begin
      for i:=0 to numberofcodes-1 do
      begin
        setlength(code[i].before,0);
        setlength(code[i].before,0);
        setlength(code[i].actualopcode,0);
        setlength(code[i].after,0);
      end;

      Codelist2.Clear;
      setlength(code,0);
      numberofcodes:=0;
    end;

    mainform.addresslist.clear;
    Comments.Memo1.Text:='';
  end;

  {if Extension='.AMT' then LoadAMT(filename,merge) else
  if Extension='.GH' then LoadGH(filename,merge) else
  if Extension='.CET' then LoadCET(filename,merge) else
  if Extension='.CT2' then LoadCT2(filename,merge) else
  if Extension='.CT3' then LoadCT3(filename,merge) else}
  if Extension='.CT' then LoadCT(filename,merge) else
  if Extension='.XML' then
  begin
    try
      ReadXMLFile(doc, filename);
    except
      raise exception.create('This is not a valid xml file');
    end;
    LoadXML(doc,merge);
    doc.free;
  end else
  raise exception.create('Unknown extention');

  mainform.editedsincelastsave:=false;

  if comments.memo1.Lines.Count>0 then
    mainform.Commentbutton.font.style:=mainform.Commentbutton.font.style+[fsBold]
  else
    mainform.Commentbutton.font.style:=mainform.Commentbutton.font.style-[fsBold];
end;



procedure SaveStructToXMLNode(struct: TbaseStructure; Structures: TDOMnode);
var structure: TDOMnode;
    elements: TDOMnode;
    element: TDOMnode;
    i: integer;
    doc: TDOMDocument;
begin
  doc:=Structures.OwnerDocument;
  structure:=structures.AppendChild(doc.CreateElement('Structure'));
  structure.AppendChild(doc.CreateElement('Name')).TextContent:=struct.name;
  elements:=structure.AppendChild(doc.CreateElement('Elements'));



  for i:=0 to length(struct.structelement)-1 do
  begin
    element:=elements.AppendChild(doc.CreateElement('Element'));
    element.AppendChild(doc.CreateElement('Offset')).TextContent:=inttostr(struct.structelement[i].offset);
    element.AppendChild(doc.CreateElement('Description')).TextContent:=struct.structelement[i].description;

    if struct.structelement[i].pointerto then
    begin
      element.AppendChild(doc.CreateElement('PointerTo')).TextContent:='1';
      element.AppendChild(doc.CreateElement('PointerToSize')).TextContent:=inttostr(struct.structelement[i].pointertosize);
    end;

    element.AppendChild(doc.CreateElement('Structurenr')).TextContent:=inttostr(struct.structelement[i].structurenr);
    element.AppendChild(doc.CreateElement('Bytesize')).TextContent:=inttostr(struct.structelement[i].bytesize);

  end;

end;

procedure SaveXML(Filename: string);
var doc: TXMLDocument;
    CheatTable: TDOMNode;
    Entries,Codes,Symbols, Structures, Comment,luascript: TDOMNode;
    CheatRecord, CodeRecords, CodeRecord, SymbolRecord: TDOMNode;
    CodeBytes: TDOMNode;
    Offsets: TDOMNode;

    CodeList: TDOMNode;
    Pointers: TDOMNode;
    i,j: integer;

    sl: tstringlist;
    extradata: ^TUDSEnum;
    x: dword;

begin
  doc:=TXMLDocument.Create;
  CheatTable:=doc.AppendChild(doc.CreateElement('CheatTable'));
  TDOMElement(CheatTable).SetAttribute('CheatEngineTableVersion',IntToStr(CurrentTableVersion));

  entries:=CheatTable.AppendChild(doc.CreateElement('CheatEntries'));

  mainform.addresslist.saveTableXMLToNode(entries);

  if advancedoptions.numberofcodes>0 then
  begin
    CodeRecords:=CheatTable.AppendChild(doc.CreateElement('CheatCodes'));


    for i:=0 to AdvancedOptions.numberofcodes-1 do
    begin
      CodeRecord:=CodeRecords.AppendChild(doc.CreateElement('CodeEntry'));
      CodeRecord.AppendChild(doc.CreateElement('Description')).TextContent:=advancedoptions.codelist2.Items[i].SubItems[0];
      CodeRecord.AppendChild(doc.CreateElement('Address')).TextContent:=inttohex(advancedoptions.code[i].address,8);
      CodeRecord.AppendChild(doc.CreateElement('ModuleName')).TextContent:=advancedoptions.code[i].modulename;
      CodeRecord.AppendChild(doc.CreateElement('ModuleNameOffset')).TextContent:=inttohex(advancedoptions.code[i].offset,1);

      //before
      CodeBytes:=CodeRecord.AppendChild(doc.CreateElement('Before'));
      for j:=0 to length(advancedoptions.code[i].before)-1 do
        CodeBytes.AppendChild(doc.CreateElement('Byte')).TextContent:=inttohex(advancedoptions.code[i].before[j],2);

      //actual
      CodeBytes:=CodeRecord.AppendChild(doc.CreateElement('Actual'));
      for j:=0 to length(advancedoptions.code[i].actualopcode)-1 do
        CodeBytes.AppendChild(doc.CreateElement('Byte')).TextContent:=inttohex(advancedoptions.code[i].actualopcode[j],2);

      //after
      CodeBytes:=CodeRecord.AppendChild(doc.CreateElement('After'));
      for j:=0 to length(advancedoptions.code[i].after)-1 do
        CodeBytes.AppendChild(doc.CreateElement('Byte')).TextContent:=inttohex(advancedoptions.code[i].after[j],2);

    end;
  end;

  sl:=tstringlist.Create;
  try
    symbols:=CheatTable.AppendChild(doc.CreateElement('UserdefinedSymbols'));
    symhandler.EnumerateUserdefinedSymbols(sl);
    if sl.count>0 then
    begin
      for i:=0 to sl.count-1 do
      begin
        extradata:=pointer(sl.Objects[i]);
        SymbolRecord:=symbols.AppendChild(doc.CreateElement('SymbolEntry'));
        SymbolRecord.AppendChild(doc.CreateElement('name')).TextContent:=sl[i];

        SymbolRecord.AppendChild(doc.CreateElement('Address')).TextContent:=extradata.addressstring;
      end;
    end;
  finally
    sl.free;
  end;

  if length(definedstructures)>0 then
  begin
    Structures:=CheatTable.AppendChild(doc.CreateElement('Structures'));
    for i:=0 to length(definedstructures)-1 do
      SaveStructToXMLNode(definedstructures[i],Structures);
  end;

  if comments.memo1.Lines.Count>0 then
  begin
    comment:=CheatTable.AppendChild(doc.CreateElement('Comments'));
    comment.TextContent:=comments.Memo1.text;
  end;

  if comments.mLuaScript.lines.count>0 then
  begin
    luascript:=CheatTable.AppendChild(doc.CreateElement('LuaScript'));
    luascript.TextContent:=comments.mLuaScript.text;
  end;
  WriteXMLFile(doc, filename);

  doc.Free;

end;

procedure SaveTable(Filename: string);
begin

  try
    if Uppercase(extractfileext(filename))<>'.EXE' then
    begin
      SaveXML(filename);
    end
    else
    begin
//trainer maker
    end;
    mainform.editedsincelastsave:=false;
  finally

  end;

end;


end.



