unit OpenSave;

{$MODE Delphi}

          //should be called loadsave but oh well...

interface


uses
     {$ifdef darwin}
     macport,
     {$endif}
     {$ifdef windows}
     windows,
     {$endif}
     forms, LCLIntf,registry, SysUtils,AdvancedOptionsUnit,CommentsUnit,
     CEFuncProc,classes,{formmemorymodifier,formMemoryTrainerUnit,}{$ifdef windows}shellapi,{$endif}
     {MemoryTrainerDesignUnit,}StdCtrls,{ExtraTrainerComponents,}Graphics,Controls,
     tableconverter, ExtCtrls,Dialogs,NewKernelHandler, hotkeyhandler, structuresfrm,
     StructuresFrm2, comctrls,dom, xmlread,xmlwrite, FileUtil, ceguicomponents,
     zstream, luafile, disassemblerComments, commonTypeDefs, lazutf8, betterControls;

const _CurrentTableVersion=44;

var CurrentTableVersion: dword=_CurrentTableVersion;
    lastLoadedTableVersion: dword=_CurrentTableVersion;
    iscetrainer: integer=0;

procedure protecttrainer(filename: string);
procedure unprotecttrainer(filename: string; stream: TStream);
procedure SaveTable(Filename: string; protect: boolean=false; dontDeactivateDesignerForms: boolean=true);
procedure LoadTable(Filename: string;merge: boolean);
procedure SaveCEM(Filename:string;address:ptrUint; size:dword);
procedure LoadXML(doc: TXMLDocument; merge: boolean; isTrainer: boolean=false);
procedure SaveXML(doc: TXMLDocument; dontDeactivateDesignerForms: boolean=false; skipsign: boolean=false); overload;
procedure SaveXML(filename: string; dontDeactivateDesignerForms: boolean=false; skipsign: boolean=false); overload;



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

  {
procedure LoadStructFromXMLNode(var struct: TbaseStructure; Structure: TDOMNode); //obsolete

procedure SaveStructToXMLNode(struct: TbaseStructure; Structures: TDOMNode);   //obsolete
     }

{$ifdef net}
var processhandle: thandle;
{$endif}

resourcestring
  strunknowncomponent='There is a unknown component in the trainer! compnr=';


implementation

uses MainUnit, mainunit2, symbolhandler, symbolhandlerstructs, LuaHandler,
     formsettingsunit {$ifdef windows},frmExeTrainerGeneratorUnit, trainergenerator{$endif},
     ProcessHandlerUnit, parsers{$ifdef windows},feces{$endif} ,asktorunluascript;



resourcestring
  strCorruptIcon='The icon has been corrupted';
  strCantLoadFilepatcher='The file patcher can''t be loaded by '+strCheatEngine+'!';
  strNotACETrainer='This is not a '+strTrainer+' made by '+strCheatEngine+' (If it is a '+strTrainer+' at all!)';
  strUnknownTrainerVersion='This version of '+strCheatEngine+' doesn''t know how to read this '+strTrainer+'! '+strTrainer+'version=';
  strCantLoadProtectedfile='This '+strTrainer+' is protected from being opened by '+strCheatEngine+'. Now go away!!!';
  rsThisTableContainsALuaScriptDoYouWantToRunIt = 'This table contains a lua script. Do you want to run it?';
  rsErrorExecutingThisTableSLuaScript = 'Error executing this table''s lua script: %s';
  rsErrorExecutingThisTableSLuaScriptEntry = 'Error executing this table''s lua script named %s: %s';
  rsTheRegionAtWasPartiallyOrCompletlyUnreadable = 'The region at %s was partially or completely unreadable';
  rsTheVersionOfIsIncompatibleWithThisCEVersion = 'The version of %s is incompatible with this '+strCheatEngine+' version';
  rsDoesnTContainNeededInformationWhereToPlaceTheMemor = '%s doesn''t contain needed information where to place the memory';
  rsThisIsNotAValidCheatTable = 'This is not a valid '+strCheatTableLower;
  rsThisIsNotAValidXmlFile = 'This is not a valid xml file';
  rsUnknownExtention = 'Unknown extension';
  rsYouCanOnlyProtectAFileIfItHasAnCETRAINERExtension = 'You can only protect a file if it has an .CETRAINER extension';
  rsErrorSaving = 'Error saving...';
  rsAskIfStupid = 'Generating a '+strtrainerlower+' with the current state of the cheat '
    +'table will likely result in a completely useless '+strtrainerlower+' that does '
    +'nothing. Are you sure?';
  rsOSThereIsANewerVersionifCheatEngineOutEtc = 'There is a newer version of '+strCheatEngine+' out. It''s recommended to use that version instead';
  rsOSThisCheatTableIsCorrupt = 'This '+strCheatTableLower+' is corrupt';
  rsInvalidLuaForTrainer = 'The lua script in this '+strTrainerLower+' has some issues and will therefore not load';


type
  THintWindowX=class(THintWindow)
  private
    i:TImage;
  protected
    procedure DoCreate; override;
    procedure DoDestroy; override;
  end;

  THintWindowXClass = class of THintWindowX;

  TImageHint=class
  public
    procedure signatureShowHint(Sender: TObject; HintInfo: PHintInfo);
  end;

var sigimage: TPicture;

procedure THintWindowX.DoDestroy;
begin
  if i<>nil then
    freeandnil(i);
end;

procedure THintWindowX.DoCreate;
begin
  inherited docreate;
  i:=TImage.Create(self);
  i.Picture.Assign(sigimage);
  i.AutoSize:=true;
  i.top:=0;
  i.left:=0;
  i.parent:=self;

  autosize:=true;
end;

procedure TImageHint.signatureShowHint(Sender: TObject; HintInfo: PHintInfo);
var p: tpoint;
begin
  hintinfo.HintWindowClass:=THintWindowX;
  hintinfo.HintColor:=clWhite;
  p.x:=MainForm.lblSigned.Left+MainForm.lblSigned.width div 2;
  p.y:=0;
  p:=MainForm.panel4.ClientToScreen(p);

  p.y:=p.y-sigimage.height;
  p.x:=p.x-sigimage.Width div 2;
  hintinfo.HintPos:=p;
end;

var imagehint: TImageHint;

procedure LoadXML(doc: TXMLDocument; merge: boolean; isTrainer: boolean=false);
var
    CheatTable: TDOMNode;
    Files, Forms, Entries, Codes, Symbols, Comments, luascript, luascriptentry, DComments: TDOMNode;
    CodeEntry, SymbolEntry: TDOMNode;
    Structures, Structure: TDOMNode;

    filenode: TDOMNode;
    form: TDOmNode;
    f: TCEForm;
    lf: TLUAFile;

    tempnode: TDOMNode;
    i,j: integer;
    s: string;

    isCodeListGroupHeader: boolean;

    tempbefore: array of byte;
    tempactual: array of byte;
    tempafter: array of byte;
    tempaddress: ptrUint;
    tempdescription,tempmodulename: string;
    tempoffset: dword;
    tempsymbolname: string;

    symbolname: string;
    li: tlistitem;

    r: integer;
    reg: Tregistry;
    version: integer;

    tempstruct: TDissectedStruct;
    svstring: string;
    sv: integer;

    signed: boolean=false;
    signedstring: string='';

    ask: TfrmLuaScriptQuestion;
    image: tpicture;
    imagepos: integer=0;

    cle: TCodeListEntry;
    EntryColor: TColor;

    hasLuaScript: boolean=false;
    usesScriptEntries: boolean=false;
    combinedLuaScript: tstringlist;
    currentLuaScript: string;
begin
  LUA_DoScript('tableIsLoading=true');
  LUA_functioncall('onTableLoad',[true]);

  try
    signed:=false;
    image:=nil;
    Forms:=nil;
    Entries:=nil;
    Codes:=nil;
    Symbols:=nil;
    Structures:=nil;
    Comments:=nil;
    LuaScript:=nil;


    if not merge then
    begin
      i:=0;
      while i<mainform.LuaForms.count do
      begin
        //only delete if it's a table form
        if TCEForm(mainform.Luaforms[i]).DoNotSaveInTable=false then
        begin
          TCEForm(mainform.Luaforms[i]).free;
          mainform.Luaforms.Delete(i);
        end
        else
          inc(i);
      end;

      for i:=0 to mainform.LuaFiles.count-1 do
        mainform.LuaFiles[i].free;

      mainform.LuaFiles.clear;

      //cleanup structures
      while frmStructures2.Count>0 do
        TfrmStructures2(frmStructures2[0]).Free;

      while DissectedStructs.Count>0 do
        TDissectedStruct(DissectedStructs[0]).free;

      mainform.addresslist.clear;
    end;


    //first load the form. If the lua functions are not loaded it's no biggy, the events just don't do anything then
    //and just assume that the loading of the lua script initializes the objects accordingly

    CheatTable:=doc.FindNode('CheatTable');
    if CheatTable<>nil then
    begin

      signed:={$ifdef windows}isProperlySigned(TDOMElement(cheattable), signedstring, imagepos, image){$else}false{$endif};

      try
        tempnode:=CheatTable.Attributes.GetNamedItem('CheatEngineTableVersion');
      except
        tempnode:=nil;
      end;

      if tempnode<>nil then
      begin
        try
          version:=strtoint(tempnode.TextContent);
          if (version>CurrentTableVersion) then
            showmessage(rsOSThereIsANewerVersionifCheatEngineOutEtc);

          lastLoadedTableVersion:=version;
        except
          showmessage(rsOSThisCheatTableIsCorrupt);
        end;
      end;


      Files:=CheatTable.FindNode('Files');
      Forms:=CheatTable.FindNode('Forms');
      Entries:=CheatTable.FindNode('CheatEntries');
      Codes:=CheatTable.FindNode('CheatCodes');
      Symbols:=CheatTable.FindNode('UserdefinedSymbols');
      Structures:=CheatTable.FindNode('Structures');
      Comments:=CheatTable.FindNode('Comments');
      LuaScript:=CheatTable.FindNode('LuaScript');
      DComments:=CheatTable.FindNode('DisassemblerComments');
    end;

    if Files<>nil then
    begin
      for i:=0 to files.Childnodes.count-1 do
      begin
        filenode:=files.ChildNodes.item[i];
        lf:=TLuafile.createFromXML(filenode);
        mainform.LuaFiles.add(lf);
      end;
    end;


    if Forms<>nil then
    begin
      for i:=0 to forms.ChildNodes.Count-1 do
      begin
        form:=forms.ChildNodes.Item[i];

        if (form.Attributes<>nil) and (form.Attributes.GetNamedItem('Class')<>nil) and (uppercase(form.Attributes.GetNamedItem('Class').TextContent)='TTRAINERFORM') then
          {$ifdef windows}
          f:=TTrainerForm.CreateNew(nil)
          {$else}
          raise exception.create('This CE version has no trainer support yet')
          {$endif}
        else
          f:=TCEform.createnew(nil);

        f.LoadFromXML(form);

        mainform.LuaForms.Add(f);
      end;
    end;

    if mainform.miResyncFormsWithLua<>nil then
      mainform.miResyncFormsWithLua.click;


    if entries<>nil then
      mainform.addresslist.loadTableXMLFromNode(entries);


    if codes<>nil then
    begin
      for i:=0 to codes.ChildNodes.Count-1 do
      begin
        CodeEntry:=codes.ChildNodes[i];



        if CodeEntry.NodeName='CodeEntry' then
        begin
          isCodeListGroupHeader:=false;
          entrycolor:=clWindowText;

          if (CodeEntry.Attributes<>nil) then
          begin
            if (CodeEntry.Attributes.GetNamedItem('GroupHeader')<>nil) then isCodeListGroupHeader:=CodeEntry.Attributes.GetNamedItem('GroupHeader').TextContent='1';
            if (CodeEntry.Attributes.GetNamedItem('Color')<>nil) then entrycolor:=strtoint('$'+CodeEntry.Attributes.GetNamedItem('Color').TextContent);

            if (entrycolor=graphics.clWindowText) or
               (entrycolor=graphics.clDefault)
            then //default color (wasn't supposed to be saved)
              entrycolor:=clWindowtext;
          end;




          tempnode:=CodeEntry.FindNode('Description');
          if tempnode<>nil then
            tempdescription:=tempnode.TextContent
          else
            tempdescription:='...';

          if isCodeListGroupHeader=false then
          begin
            tempnode:=CodeEntry.FindNode('AddressString');
            if tempnode<>nil then
              tempsymbolname:=tempnode.TextContent
            else
              tempsymbolname:='';

            tempaddress:=0;
            tempnode:=CodeEntry.FindNode('Address');
            if tempnode<>nil then
            begin
              try
                tempaddress:=StrToQWordEx('$'+tempnode.TextContent);
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
          end;

          with advancedoptions do
          begin
            cle:=TCodeListEntry.create;
            cle.color:=entrycolor;

            if isCodeListGroupHeader=false then
            begin
              cle.code:=TAdvancedOptionsCodeRecord.Create;


              setlength(cle.code.before,length(tempbefore));
              for j:=0 to length(tempbefore)-1 do
                cle.code.before[j]:=tempbefore[j];

              setlength(cle.code.actualopcode,length(tempactual));
              for j:=0 to length(tempactual)-1 do
                cle.code.actualopcode[j]:=tempactual[j];

              setlength(cle.code.after,length(tempafter));
              for j:=0 to length(tempafter)-1 do
                cle.code.after[j]:=tempafter[j];

              if tempsymbolname<>'' then
                cle.code.symbolname:=tempsymbolname
              else
              begin
                if tempmodulename='' then
                  cle.code.symbolname:=inttohex(tempaddress,8)
                else
                  cle.code.symbolname:=tempmodulename+'+'+inttohex(tempoffset,1);
              end;
            end;

            li:=lvcodelist.Items.Add;
            li.data:=cle;
            if isCodeListGroupHeader then
              li.Caption:=tempdescription
            else
            begin
              li.Caption:=cle.code.symbolname;
              li.SubItems.Add(tempdescription);
            end;
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
      svstring:=TDOMElement(structures).GetAttribute('StructVersion');
      if svstring='' then
        sv:=1
      else
        sv:=StrToInt(svstring);


      for i:=0 to Structures.ChildNodes.Count-1 do
      begin
        Structure:=Structures.ChildNodes[i];
        if sv>=2 then
          tempstruct:=TDissectedStruct.createFromXMLNode(structure)
        else  //v1 structure(pre 6.2). Needs conversion
          tempstruct:=TDissectedStruct.createFromOutdatedXMLNode(structure);

        tempstruct.addToGlobalStructList;
      end;


      //fill in the structure references
      for i:=0 to DissectedStructs.count-1 do
        TDissectedStruct(DissectedStructs[i]).fillDelayLoadedChildstructs;
    end;



    //disassemblercomments
    if DComments<>nil then
      dassemblercomments.loadFromXMLNode(dcomments);


    Commentsunit.Comments.Memo1.clear;
    if comments<>nil then
    begin
      s:='';
      for i:=1 to length(comments.textcontent) do
      begin
        if not (comments.textcontent[i] in [#13,#10]) then
          s:=s+comments.textcontent[i]
        else
        begin
          if s<>'' then
          begin
            //new line
            Commentsunit.Comments.Memo1.Lines.add(s);
            s:='';
          end;
        end;
      end;
      if s<>'' then
        Commentsunit.Comments.Memo1.Lines.add(s);
    end;

    mainform.frmLuaTableScript.TabCount:=1;
    mainform.frmLuaTableScript.assemblescreen.Text:='';

    combinedLuaScript:=tstringlist.create;


    if luaScript<>nil then
    begin
      if luascript.HasChildNodes then
      begin
        i:=0;
        luascriptentry:=luascript.FirstChild;
        while luascriptentry<>nil do
        begin
          if luascriptentry.NodeName='LuaScriptEntry' then
          begin
            usesScriptEntries:=true;
            mainform.frmLuaTableScript.TabCount:=i+1;
            mainForm.frmLuaTableScript.TabScript[i]:=ansitoutf8(luascriptentry.TextContent);

            if (luascriptentry.Attributes.GetNamedItem('Name')<>nil) then
            begin
              s:=luascriptentry.Attributes.GetNamedItem('Name').TextContent;
              mainForm.frmLuaTableScript.tablist.TabText[i]:=s;
            end;

            if mainForm.frmLuaTableScript.TabScript[i]<>'' then
              hasLuaScript:=true;

            combinedLuaScript.Add('---------- : '+mainForm.frmLuaTableScript.tablist.TabText[i]+' : --------');
            combinedLuaScript.AddText(mainForm.frmLuaTableScript.TabScript[i]);
            combinedLuaScript.Add('');
            combinedLuaScript.Add('');

            inc(i);
          end;

          luascriptentry:=luascriptentry.NextSibling;
        end;
      end;

      if usesScriptEntries=false then
      begin
        mainform.frmLuaTableScript.assemblescreen.Text:=ansitoutf8(luascript.TextContent);
        if mainform.frmLuaTableScript.assemblescreen.Text<>'' then
          hasLuaScript:=true;

        combinedLuaScript.AddText(mainform.frmLuaTableScript.assemblescreen.Text);
      end;
    end;



    if hasluascript then
    begin
      if (not isTrainer) and (iscetrainer=0) then
      begin
        reg:=TRegistry.Create;
        try
          Reg.RootKey := HKEY_CURRENT_USER;

          if Reg.OpenKey('\Software\'+strCheatEngine,false) then   //fill it from the registry (in case it's loaded before the settings are loaded)
          begin
            if reg.ValueExists('LuaScriptAction') then
              i:=reg.ReadInteger('LuaScriptAction')
            else
              i:=1;

            case i of
              0: r:=mryes;
              1,2:
              begin
                if (i=1) and signed then r:=mryes else
                begin
                  ask:=TfrmLuaScriptQuestion.Create(application);
                  ask.script.Lines.Text:=combinedLuaScript.text;
                  ask.LuaScriptAction:=i;
                  r:=ask.showmodal;

                  reg.WriteInteger('LuaScriptAction', ask.LuaScriptAction);


                  ask.free;
                end;
              end;

              3: r:=mrno;
            end;
          end;


        finally
          reg.free;
        end;

      end
      else
        r:=mryes; //cetrainers always execute the script

      if r=mryes then
      begin
        try
          for i:=0 to mainform.frmLuaTableScript.TabCount-1 do
          begin
            if mainform.frmLuaTableScript.TabCount>1 then
              currentLuaScript:=mainform.frmLuaTableScript.tablist.TabText[i];

            LUA_DoScript(mainform.frmLuaTableScript.TabScript[i]);
          end;
        except
          on e: exception do
          begin
              //raise Exception.create(Format(rsErrorExecutingThisTableSLuaScript, [e.message]))

            if isTrainer then
            begin
              MessageDlg(rsInvalidLuaForTrainer,mtError,[mbok],0);
              Application.Terminate;
              exit;
              //ExitProcess(123);
            end
            else
            begin
              if mainform.frmLuaTableScript.TabCount>1 then
                MessageDlg(Format(rsErrorExecutingThisTableSLuaScriptEntry, [currentLuaScript, e.message]), mtError, [mbok],0)
              else
                MessageDlg(Format(rsErrorExecutingThisTableSLuaScript, [e.message]), mtError, [mbok],0);
            end;

          end;
        end;
      end;


    end;

    combinedLuaScript.free;

    //default view
    mainform.lblSigned.Anchors:=[];
    mainform.lblSigned.AnchorSideTop.control:=mainform.Panel4;
    mainform.lblSigned.AnchorSideTop.side:=asrCenter;
    mainform.lblSigned.AnchorSideLeft.control:=mainform.Panel4;
    mainform.lblSigned.AnchorSideLeft.Side:=asrCenter;
    mainform.lblSigned.Anchors:=[akTop,akLeft];

    if signed then
    begin
      mainform.lblSigned.Caption:=signedstring;
      mainform.lblSigned.Visible:=true;

      if image<>nil then
      begin
        if MainForm.imgSignature=nil then
          MainForm.imgSignature:=TImage.create(mainform);

        MainForm.imgSignature.Name:='imgSignature';
        MainForm.imgSignature.AutoSize:=true;
        MainForm.imgSignature.Anchors:=[];

        MainForm.imgSignature.Parent:=MainForm.panel4;



        case imagepos of
          1:
          begin
            //behind it (centered)
            MainForm.imgSignature.AnchorSideTop.Control:=mainform.panel4;
            MainForm.imgSignature.AnchorSideTop.Side:=asrCenter;
            MainForm.imgSignature.AnchorSideLeft.Control:=mainform.panel4;
            MainForm.imgSignature.AnchorSideLeft.Side:=asrCenter;
            MainForm.imgSignature.Anchors:=[akTop, akLeft];
          end;

          2:
          begin
            //above it
            MainForm.imgSignature.AnchorSideTop.Control:=mainform.panel4;
            MainForm.imgSignature.AnchorSideTop.Side:=asrTop;
            MainForm.imgSignature.AnchorSideLeft.Control:=mainform.panel4;
            MainForm.imgSignature.AnchorSideLeft.Side:=asrCenter;
            MainForm.imgSignature.Anchors:=[akTop, akLeft];

            mainform.lblSigned.Anchors:=[];
            mainform.lblSigned.AnchorSideTop.control:=MainForm.imgSignature;
            mainform.lblSigned.AnchorSideTop.side:=asrBottom;
            mainform.lblSigned.Anchors:=[akTop,akLeft];
          end;

          3:
          begin
            //below it

            mainform.lblSigned.Anchors:=[];
            mainform.lblSigned.AnchorSideTop.control:=MainForm.panel4;
            mainform.lblSigned.AnchorSideTop.side:=asrTop;
            mainform.lblSigned.Anchors:=[akTop,akLeft];

            MainForm.imgSignature.AnchorSideTop.Control:=mainform.lblSigned;
            MainForm.imgSignature.AnchorSideTop.Side:=asrBottom;
            MainForm.imgSignature.AnchorSideLeft.Control:=mainform.lblSigned;
            MainForm.imgSignature.AnchorSideLeft.Side:=asrCenter;
            MainForm.imgSignature.Anchors:=[akTop, akLeft];
          end;

          4:
          begin
            //left of the label
            MainForm.imgSignature.AnchorSideTop.Control:=mainform.panel4;
            MainForm.imgSignature.AnchorSideTop.Side:=asrCenter;
            MainForm.imgSignature.AnchorSideRight.Control:=mainform.lblSigned;
            MainForm.imgSignature.AnchorSideRight.Side:=asrLeft;
            MainForm.imgSignature.Anchors:=[akTop, akRight];
          end;

          5:
          begin
            //right of the label
            MainForm.imgSignature.AnchorSideTop.Control:=mainform.panel4;
            MainForm.imgSignature.AnchorSideTop.Side:=asrCenter;
            MainForm.imgSignature.AnchorSideLeft.Control:=mainform.lblSigned;
            MainForm.imgSignature.AnchorSideLeft.Side:=asrRight;
            MainForm.imgSignature.Anchors:=[akTop, akLeft];
          end;

          6:
          begin
            //no text, image only (hint shows text)
            MainForm.imgSignature.AnchorSideTop.Control:=mainform.panel4;
            MainForm.imgSignature.AnchorSideTop.Side:=asrTop;
            MainForm.imgSignature.AnchorSideLeft.Control:=mainform.panel4;
            MainForm.imgSignature.AnchorSideLeft.Side:=asrCenter;
            MainForm.imgSignature.Anchors:=[akTop, akLeft];

            MainForm.imgSignature.Hint:=signedstring;
            MainForm.imgSignature.ShowHint:=true;

            mainform.lblSigned.Visible:=false;
          end;

          7:
          begin
            //no image, text only (mouseenter pops up image, mouseleave hides it)
            MainForm.imgSignature.visible:=false;

            if imagehint=nil then
              imagehint:=TImageHint.create;

            if sigimage=nil then
              sigimage:=tpicture.Create;

            sigimage.Assign(image);


            mainform.lblSigned.OnShowHint:=imagehint.signatureShowHint;
            mainform.lblSigned.Hint:='Do not look at this, it''s ugly';
            mainform.lblSigned.ShowHint:=true;


          end;

        end;


        MainForm.imgSignature.Picture.Assign(image);

        freeandnil(image);

        mainform.lblSigned.BringToFront;

      end
      else
      begin
        if MainForm.imgSignature<>nil then
          freeandnil(MainForm.imgSignature);
      end;
    end
    else
    begin
      mainform.lblSigned.Visible:=false;
      if mainform.imgSignature<>nil then
        freeandnil(mainform.imgSignature);
    end;



  finally
    LUA_DoScript('tableIsLoading=false');

    LUA_functioncall('onTableLoad',[false]);

    if image<>nil then
      freeandnil(image);
  end;

end;


procedure SaveCEM(Filename:string;address:ptrUint; size:dword);
var memfile: TFilestream;
    buf: pointer;
    temp:ptruint;
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
    end else messagedlg(Format(rsTheRegionAtWasPartiallyOrCompletlyUnreadable, [IntToHex(address, 8)]), mterror, [mbok], 0);
  finally
    freeandnil(memfile);
    freememandnil(buf);
  end;
end;

procedure LoadCEM(filename:string);
var memfile: TFilestream;
    check: pchar;
    mem: pointer;
    temp:dword;
    size: dword;
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
      if temp<>1 then raise exception.Create(Format(rsTheVersionOfIsIncompatibleWithThisCEVersion, [filename]));
      memfile.ReadBuffer(temp,4);
      //temp=startaddress

      size:=memfile.Size-memfile.Position;
      getmem(mem,size);
      memfile.ReadBuffer(mem^,size);


      RewriteCode(processhandle,temp,mem,size);
    end else raise exception.Create(Format(rsDoesnTContainNeededInformationWhereToPlaceTheMemor, [filename]));
  finally
    freememandnil(check);
    freeandnil(memfile);

  end;
end;



procedure LoadCT(filename: string; merge: boolean);
var ctfile: TFilestream=nil;
    x: pchar=nil;
    doc: TXMLDocument=nil;
    unprotectedstream: TMemorystream=nil;

    isProtected: boolean;
begin
  x:=nil;
  isProtected:=false;
  unprotectedstream:=nil;
  ctfile:=nil;
  doc:=nil;
  ctfile:=Tfilestream.Create(filename,fmopenread or fmsharedenynone);

  mainform.addresslist.Items.BeginUpdate;
  try
    getmem(x,12);
    ctfile.ReadBuffer(x^,11);
    x[11]:=#0;  //write a 0 terminator
    freeandnil(ctfile);


    if uppercase(copy(x, 1,5))<>'<?XML' then
    begin
      //not xml

      if X='CHEATENGINE' then
      begin
         doc:=ConvertCheatTableToXML(filename)
      end
      else
      begin
        //protected
        iscetrainer:=1;
        isProtected:=true;
        unprotectedstream:=tmemorystream.create;

        unprotecttrainer(filename, unprotectedstream);
        unprotectedstream.Position:=0;

        ReadXMLFile(doc, unprotectedstream);
      end;
    end;


    try
      if doc=nil then
        ReadXMLFile(doc, filename);

      LoadXML(doc, merge, uppercase(extractfileext(utf8tosys(filename)))='.CETRAINER');
      if isProtected then //I know, this protection is pathetic for anyone that can compile ce. But as I said, this is just to stop the ultimate lazy guy from just editing the .CETRAINER file and changing the name
        mainform.isProtected:=true;
    except
      on e: exception do
        raise exception.Create(rsThisIsNotAValidCheatTable + ' ('+e.message+')');
    end;

  finally
    if x<>nil then
    begin
      freememandnil(x);

    end;

    if ctfile<>nil then
      freeandnil(ctfile);

    if doc<>nil then
      freeandnil(doc);

    if unprotectedstream<>nil then
      freeandnil(unprotectedstream);

    mainform.addresslist.Items.EndUpdate;
  end;
end;


procedure LoadTable(Filename: string;merge: boolean);
var
    i: Integer;
    Extension: String;
    doc: TXMLDocument;
    workdir: string;
begin
  if mainform.addresslist=nil then exit;

  if fileexists(filename)=false then
    filename:=UTF8ToSys(filename); //fix chinese problems I hope


  SetCurrentDir(ExtractFilePath(filename)); //in case it's a table with 'external' files
  Extension:=uppercase(extractfileext(filename));

  if not merge then
  begin
    //delete everything
    advancedoptions.clear;
    mainform.addresslist.clear;
    Comments.Memo1.Text:='';
  end;

  {if Extension='.AMT' then LoadAMT(filename,merge) else
  if Extension='.GH' then LoadGH(filename,merge) else
  if Extension='.CET' then LoadCET(filename,merge) else
  if Extension='.CT2' then LoadCT2(filename,merge) else
  if Extension='.CT3' then LoadCT3(filename,merge) else}
  if Extension='.CETRAINER' then LoadCT(filename, false) else
  if Extension='.CT' then LoadCT(filename,merge) else
  if Extension='.XML' then
  begin
    try
      ReadXMLFile(doc, filename);
    except
      raise exception.create(rsThisIsNotAValidXmlFile);
    end;
    LoadXML(doc,merge);
    doc.free;
  end else
  raise exception.create(rsUnknownExtention);

  mainform.editedsincelastsave:=false;

  if (comments.memo1.Lines.Count>0) then
    mainform.Commentbutton.font.style:=mainform.Commentbutton.font.style+[fsBold]
  else
    mainform.Commentbutton.font.style:=mainform.Commentbutton.font.style-[fsBold];

  try
    mainform.autoattachcheck; //check if it added an auto attach check and see if it's currently running
  except
  end;
end;

procedure SaveXML(doc: TXMLDocument; dontDeactivateDesignerForms: boolean=false; skipsign: boolean=false);
var
  CheatTable: TDOMElement;
  Files, Forms,Entries,Symbols, Structures, Comment,luascript, luascriptentry, dcomments: TDOMNode;
  CodeRecords, CodeRecord, SymbolRecord: TDOMNode;
  CodeBytes: TDOMNode;

  i,j: integer;

  sl: tstringlist;
  extradata: ^TUDSEnum;

  a: TDOMAttr;
begin
  CheatTable:=TDOMElement(doc.AppendChild(TDOMNode(doc.CreateElement('CheatTable'))));
  TDOMElement(CheatTable).SetAttribute('CheatEngineTableVersion',IntToStr(CurrentTableVersion));

  if mainform.LuaForms.count>0 then
  begin
    Forms:=CheatTable.AppendChild(doc.CreateElement('Forms'));
    for i:=0 to mainform.LuaForms.count-1 do
      if TCEForm(mainform.LuaForms[i]).DoNotSaveInTable=false then //only save forms that belong to the table
        TCEForm(mainform.LuaForms[i]).savetoxml(forms, dontDeactivateDesignerForms);
  end;

  if mainform.LuaFiles.count>0 then
  begin
    Files:=CheatTable.AppendChild(doc.CreateElement('Files'));
    for i:=0 to mainform.Luafiles.count-1 do
      mainform.LuaFiles[i].savetoxml(files);
  end;

  entries:=CheatTable.AppendChild(doc.CreateElement('CheatEntries'));

  mainform.addresslist.saveTableXMLToNode(entries);

  if advancedoptions.count>0 then
  begin
    CodeRecords:=CheatTable.AppendChild(doc.CreateElement('CheatCodes'));


    for i:=0 to AdvancedOptions.count-1 do
    begin
      CodeRecord:=CodeRecords.AppendChild(doc.CreateElement('CodeEntry'));

      if (TCodeListEntry(advancedoptions.lvCodelist.Items[i].data).color<>clWindowtext) and
         (TCodeListEntry(advancedoptions.lvCodelist.Items[i].data).color<>graphics.clDefault)
      then //don't save the color if it's the default color
      begin
        a:=doc.CreateAttribute('Color');
        a.TextContent:=inttohex(TCodeListEntry(advancedoptions.lvCodelist.Items[i].data).color,8);
        CodeRecord.Attributes.SetNamedItem(a);
      end;

      if AdvancedOptions.code[i]=nil then
      begin
        a:=doc.CreateAttribute('GroupHeader');
        a.TextContent:='1';
        CodeRecord.Attributes.SetNamedItem(a);

//        CodeRecord.Attributes.SetNamedItem(doc.CreateAttribute('GroupHeader')).TextContent:='1';
        CodeRecord.AppendChild(doc.CreateElement('Description')).TextContent:=advancedoptions.lvCodelist.Items[i].Caption;
      end
      else
      begin
        CodeRecord.AppendChild(doc.CreateElement('Description')).TextContent:=advancedoptions.lvCodelist.Items[i].SubItems[0];
        CodeRecord.AppendChild(doc.CreateElement('AddressString')).TextContent:=advancedoptions.code[i].symbolname;

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
        if extradata.doNotSave=false then
        begin
          SymbolRecord:=symbols.AppendChild(doc.CreateElement('SymbolEntry'));
          SymbolRecord.AppendChild(doc.CreateElement('Name')).TextContent:=sl[i];
          SymbolRecord.AppendChild(doc.CreateElement('Address')).TextContent:=extradata.addressstring;
        end;
      end;
    end;
  finally
    sl.free;
  end;

  {
  //old pre 6.2
  if length(definedstructures)>0 then
  begin
    Structures:=CheatTable.AppendChild(doc.CreateElement('Structures'));
    for i:=0 to length(definedstructures)-1 do
      SaveStructToXMLNode(definedstructures[i],Structures);
  end;
  }
  if DissectedStructs.Count>0 then
  begin
    Structures:=CheatTable.AppendChild(doc.CreateElement('Structures'));
    TDOMElement(Structures).SetAttribute('StructVersion', inttostr(structureversion));
    for i:=0 to DissectedStructs.Count-1 do
      TDissectedStruct(DissectedStructs[i]).WriteToXMLNode(Structures);
  end;

  if comments.memo1.Lines.Count>0 then
  begin
    comment:=CheatTable.AppendChild(doc.CreateElement('Comments'));
    comment.TextContent:=comments.Memo1.text;
  end;


  if mainform.frmLuaTableScript.assemblescreen.lines.count>0 then
  begin
    luascript:=CheatTable.AppendChild(doc.CreateElement('LuaScript'));

    if mainform.frmLuaTableScript.TabCount=1 then
      luascript.TextContent:=Utf8ToAnsi(mainform.frmLuaTableScript.assemblescreen.text)
    else
    begin
      //multiple lua scripts
      for i:=0 to mainform.frmLuaTableScript.TabCount-1 do
      begin
        luascriptentry:=luascript.AppendChild(doc.CreateElement('LuaScriptEntry'));

        TDOMElement(luascriptentry).SetAttribute('Name', mainform.frmLuaTableScript.tablist.TabText[i]);
        luascriptentry.TextContent:=Utf8ToAnsi(mainform.frmLuaTableScript.TabScript[i]);
      end;
    end;

    mainform.frmLuaTableScript.assemblescreen.MarkTextAsSaved;
  end;

  //disassemblercomments
  if dassemblercomments.count>0 then
  begin
    dcomments:=CheatTable.AppendChild(doc.CreateElement('DisassemblerComments'));
    dassemblercomments.saveToXMLNode(dcomments);
  end;

  {$ifdef windows}
  if (not skipsign) and cansigntables and formsettings.cbAlwaysSignTable.checked then
    signTable(cheattable);
  {$endif}

end;

procedure SaveXML(Filename: string; dontDeactivateDesignerForms: boolean=false; skipsign: boolean=false);
var doc: TXMLDocument;
begin
  doc:=TXMLDocument.Create;
  SaveXML(doc, dontDeactivateDesignerForms, skipsign);
  WriteXMLFile(doc, filename);
  doc.Free;
end;

procedure SaveTable(Filename: string; protect: boolean=false; dontDeactivateDesignerForms: boolean=true);
begin
  try
    if Uppercase(utf8tosys(extractfileext(filename)))<>'.EXE' then
    begin
      if protect and (Uppercase(utf8tosys(extractfileext(filename)))<>'.CETRAINER') then raise exception.create(rsYouCanOnlyProtectAFileIfItHasAnCETRAINERExtension);

      if protect and (MainForm.LuaForms.Count=0) and (mainform.frmLuaTableScript.assemblescreen.Text='') then
        if MessageDlg(rsAskIfStupid, mtWarning, [mbyes, mbno], 0)<>mryes
          then exit;


      SaveXML(utf8tosys(filename), dontDeactivateDesignerForms);
      if protect then
        protecttrainer(utf8tosys(filename));
    end
    else
    begin
      //trainer maker
      //show the trainer exegenerator form
      {$ifdef windows}

      if (MainForm.LuaForms.Count=0) and (mainform.frmLuaTableScript.assemblescreen.Text='') then
        if MessageDlg(rsAskIfStupid, mtWarning, [mbyes, mbno], 0)<>mryes
          then exit;

      if frmExeTrainerGenerator=nil then
        frmExeTrainerGenerator:=TfrmExeTrainerGenerator.create(application);

      frmExeTrainerGenerator.filename:=filename;
      frmExeTrainerGenerator.showmodal;
      {$else}
      raise exception.create('Not yet implemented');
      {$endif}
    end;
    mainform.editedsincelastsave:=false;
  finally

  end;

end;

procedure unprotecttrainer(filename: string; stream: TStream);
var f: TMemorystream;
 m: pbytearray;
 p: pchar;

 i: integer;
 k: byte;

 d: Tdecompressionstream;

 b: pointer;
 size: integer;

 ActuallyRead: integer;
begin
  f:=tmemorystream.create;
  f.loadfromfile(filename);

  m:=f.Memory;

  for i:=2 to f.size-1 do
    m[i]:=m[i] xor m[i-2];

  for i:=f.Size-2 downto 0 do
    m[i]:=m[i] xor m[i+1];

  k:=$ce;
  for i:=0 to f.size-1 do
  begin
    m[i]:=m[i] xor k;
    inc(k);
  end;




  getmem(p,6);
  copymemory(p,m,5);
  p[5]:=#0;
  if p='CHEAT' then
  begin
    //new storage method

    f.Position:=5;  //skip "CHEAT" header

    d:=Tdecompressionstream.create(f,true);

    i:=d.read(size, sizeof(size));
    if i=sizeof(size) then
    begin
      getmem(b,size);
      try
        i:=d.read(b^, size);
        if i>0 then
        begin
          stream.Write(b^, i);
        end;
      finally
        freemem(b);
        b:=nil;
      end;
    end;
  end
  else
  begin
    //no CHEAT header, so old bad storage method

    d:=Tdecompressionstream.create(f,true);

    size:=1024;
    getmem(b, size);
    try
      ActuallyRead:=1024;
      while ActuallyRead>0 do
      begin
        ActuallyRead:=d.read(b^, size);
        if ActuallyRead>0 then
          stream.Write(b^, ActuallyRead);
      end;


    finally
      freememandnil(b);

    end;
  end;

  if d<>nil then
    freeandnil(d);

  if f<>nil then
    freeandnil(f);
end;

procedure protecttrainer(filename: string);
{
this is the super mega protector routine for the trainer
Yeah, it's pathetic, but it keeps the retarded noobs out that don't know how to
read code and only know how to copy/paste
}
var f,f2: tmemorystream;
  m: PByteArray;
  i: integer;
  k: byte;
  c: Tcompressionstream;
  s: string;

begin
  if Uppercase(extractfileext(filename))<>'.CETRAINER' then raise exception.create(rsErrorSaving);

  f:=tmemorystream.create;
  f.LoadFromFile(filename);
  f2:=tmemorystream.create;

  s:='CHEAT';
  f2.Write(s[1], 5);

  c:=Tcompressionstream.create(clmax, f2,true);

  i:=f.size;
  c.write(i, sizeof(i));
  c.write(f.Memory^, f.size);
  freeandnil(c);
  freeandnil(f);


  k:=$ce;
  m:=f2.Memory;

  for i:=0 to f2.Size-1 do
  begin
    m[i]:=(m[i] xor k);
    inc(k);
  end;

  for i:=0 to f2.Size-2 do
    m[i]:=m[i] xor m[i+1];

  for i:=f2.Size-1 downto 2 do
    m[i]:=m[i] xor m[i-2];


  f2.SaveToFile(filename);

  freeandnil(f2);
end;


end.



