unit OpenSave;

{$MODE Delphi}

          //should be called loadsave but oh well...

interface


uses windows, forms, MainUnit,LCLIntf,{standaloneunit,}SysUtils,AdvancedOptionsUnit,CommentsUnit,
     CEFuncProc,classes,{formmemorymodifier,formMemoryTrainerUnit,}shellapi,
     {MemoryTrainerDesignUnit,}StdCtrls,{ExtraTrainerComponents,}Graphics,Controls,
     ExtCtrls,Dialogs,NewKernelHandler, hotkeyhandler, structuresfrm,{, XMLDoc, XMLIntf, KIcon,} comctrls,dom, xmlread,xmlwrite;


var CurrentTableVersion: dword=10;
procedure SaveTable(Filename: string);
procedure LoadTable(Filename: string;merge: boolean);
procedure SaveCEM(Filename:string;address,size:dword);

procedure LoadExe(filename: string);


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

uses mainunit2, symbolhandler;


{$ifndef net}
procedure LoadTrainer8(trainer:TFilestream);
var temp: dword;
    tempb: boolean;
    tempc: tcolor;
    tempi: integer;
    tempbc: tbevelcut;
    (*tempbk: tbevelkind; *)
    tempcursor: tcursor;
    i,j,k:integer;
    x: pchar;

    image: pointer;
    temps: string;
    filecount: integer;
    f: tfilestream;
//    trainerdata1: array of TTrainerData1;
begin
  f:=nil;
  filecount:=0;
 (*
  with frmMemoryModifier do
  begin
    //size of trainerdata
    trainer.ReadBuffer(temp,4);
    setlength(frmMemoryModifier.trainerdata,temp);

    for i:=0 to length(trainerdata)-1 do
    begin
      //description
      trainer.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainer.ReadBuffer(x^,temp);
      x[temp]:=#0;
      trainerdata[i].description:=x;
      freemem(x);

      //hotkeytext
      trainer.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainer.ReadBuffer(x^,temp);
      x[temp]:=#0;
      trainerdata[i].hotkeytext:=x;
      freemem(x);

      trainer.readbuffer(trainerdata[i].hotkey,sizeof(trainerdata[i].hotkey));

      trainer.ReadBuffer(temp,4);
      setlength(trainerdata[i].codeentrys,temp);

      //opcodes of this cheat
      for j:=0 to length(trainerdata[i].codeentrys)-1 do
      begin
        //address
        trainer.ReadBuffer(trainerdata[i].codeentrys[j].address,4);

        //modulename
        trainer.ReadBuffer(temp,4);
        getmem(x,temp+1);
        trainer.ReadBuffer(x^,temp);
        x[temp]:=#0;
        trainerdata[i].codeentrys[j].modulename:=x;
        freemem(x);

        //module offset
        trainer.ReadBuffer(trainerdata[i].codeentrys[j].moduleoffset,4);

        //original opcode
        trainer.ReadBuffer(temp,4);
        setlength(trainerdata[i].codeentrys[j].originalopcode,temp);
        trainer.ReadBuffer(pointer(trainerdata[i].codeentrys[j].originalopcode)^,temp);
      end;

      //address entrys
      trainer.ReadBuffer(temp,4);
      setlength(trainerdata[i].addressentrys,temp);
      for j:=0 to length(trainerdata[i].addressentrys)-1 do
      begin
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].address,sizeof(trainerdata[i].addressentrys[j].address));

        //interpretable address
        trainer.ReadBuffer(temp,4);
        getmem(x,temp+1);
        trainer.ReadBuffer(x^,temp);
        x[temp]:=#0;
        trainerdata[i].addressentrys[j].interpretableaddress:=x;
        freemem(x);

        trainer.ReadBuffer(trainerdata[i].addressentrys[j].ispointer,sizeof(trainerdata[i].addressentrys[j].ispointer));

        trainer.ReadBuffer(tempi,4);
        setlength(trainerdata[i].addressentrys[j].pointers,tempi);

        for k:=0 to tempi-1 do
        begin
          trainer.readBuffer(trainerdata[i].addressentrys[j].pointers[k].address,sizeof(trainerdata[i].addressentrys[j].pointers[k].address));

          //interpretable address
          trainer.ReadBuffer(temp,4);
          getmem(x,temp+1);
          trainer.ReadBuffer(x^,temp);
          x[temp]:=#0;
          trainerdata[i].addressentrys[j].pointers[k].interpretableaddress:=x;
          freemem(x);

          trainer.readBuffer(trainerdata[i].addressentrys[j].pointers[k].offset,sizeof(trainerdata[i].addressentrys[j].pointers[k].offset));
        end;


        trainer.ReadBuffer(trainerdata[i].addressentrys[j].bit,sizeof(trainerdata[i].addressentrys[j].bit));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].memtyp,sizeof(trainerdata[i].addressentrys[j].memtyp));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].frozen,sizeof(trainerdata[i].addressentrys[j].frozen));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].frozendirection,sizeof(trainerdata[i].addressentrys[j].frozendirection));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].setvalue,sizeof(trainerdata[i].addressentrys[j].setvalue));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].userinput,sizeof(trainerdata[i].addressentrys[j].userinput));

        trainer.ReadBuffer(temp,4);
        getmem(x,temp+1);
        trainer.ReadBuffer(x^,temp);
        x[temp]:=#0;
        trainerdata[i].addressentrys[j].value:=x;
        freemem(x);


       // trainer.Readbuffer(trainerdata[i].addressentrys[j].value,50);
        if trainerdata[i].addressentrys[j].userinput then
        begin
          trainerdata[i].hasedit:=true;
          trainerdata[i].editvalue:=trainerdata[i].addressentrys[j].value;
        end;

        //interpretable address
        trainer.ReadBuffer(temp,4);
        getmem(x,temp+1);
        trainer.ReadBuffer(x^,temp);
        x[temp]:=#0;
        trainerdata[i].addressentrys[j].autoassemblescript:=x;
        freemem(x);
      end;
    end;

    //title
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edittitle.Text:=x;
    freemem(x);

    //launch filename
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edit2.Text:=x;
    freemem(x);

    //autolaunch
    trainer.ReadBuffer(tempb,sizeof(tempb));
    checkbox2.Checked:=tempb;

    //popup on keypress
    trainer.ReadBuffer(tempb,sizeof(tempb));
    checkbox1.Checked:=tempb;

    //process name
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    combobox1.Text:=x;
    freemem(x);

    //hotkeytext
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edithotkey.Text:=x;
    freemem(x);

    //hotkey+shiftstate
    trainer.ReadBuffer(popuphotkey,sizeof(popuphotkey));

    //abouttext
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    memo1.Text:=x;
    freemem(x);

    //freeze interval
    trainer.ReadBuffer(tempi,sizeof(tempi));
    editFreezeInterval.text:=inttostr(tempi);

    trainer.ReadBuffer(temp,4);
    if temp=$666 then
    begin
      //default
      //leftside image
      trainer.ReadBuffer(temp,4); //size of extension
      getmem(x,temp+1);
      trainer.ReadBuffer(x^,temp);
      x[temp]:=#0;
      leftimageext:=x;

      temps:=tempdir+inttostr(getcurrentprocessid)+'-'+inttostr(filecount)+x;
      freemem(x);

      f:=tfilestream.Create(temps, fmCreate or fmShareDenyNone);
      inc(filecount);

      //size
      trainer.ReadBuffer(temp,4);
      getmem(x,temp);
      trainer.ReadBuffer(x^,temp);
      f.WriteBuffer(x^,temp);
      freemem(x);
      f.free;

      if leftimage<>nil then
        freeandnil(leftimage);

      if temp<>0 then //not size 0
      begin
        frmMemorytrainerpreview.Image1.Picture.LoadFromFile(temps);
        leftimage:=tmemorystream.Create;
        leftimage.LoadFromFile(temps);
      end;

      DeleteFile(temps);

      //windowwidth
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Width));
      frmMemorytrainerpreview.Width:=temp;

      //windowheight
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.height));
      frmMemorytrainerpreview.height:=temp;

      //leftsidewidth
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Panel1.Width));
      frmMemorytrainerpreview.Panel1.Width:=temp;

      //leftsideheight
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Panel1.height));
      frmMemorytrainerpreview.Panel1.height:=temp;
    end
    else
    begin
      //user defined
      frmMemoryModifier.dontshowdefault:=true;       //obsolete
      frmMemoryModifier.Button7.Click;

      //windowwidth
      trainer.readbuffer(temp,4);
      frmTrainerDesigner.Width:=temp;

      //windowheight
      trainer.readbuffer(temp,4);
      frmTrainerDesigner.height:=temp;


      while true do
      begin
        trainer.ReadBuffer(temp,4);
        case temp of
          0: begin
               //tbutton
               with tbutton2.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 //caption
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 caption:=x;
                 freemem(x);

                 //wordwrap
                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 wordwrap:=tempb;

                 //onclick
                 trainer.ReadBuffer(temp,sizeof(tag));
                 tag:=temp;
                 parent:=frmTrainerDesigner;

                 //command
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 command:=x;
                 freemem(x);

                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          1: begin
               //cheatlist
               with tcheatlist.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 activationcolor:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 color:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 textcolor:=tempc;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 hotkeyleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 descriptionleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editwidth:=tempi;

                 trainer.ReadBuffer(tempbc,sizeof(tbevelcut));
                 bevelinner:=tempbc;
                 trainer.ReadBuffer(tempbc,sizeof(tbevelcut));
                 bevelouter:=tempbc;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 bevelwidth:=tempi;
                 trainer.ReadBuffer(tempbk,sizeof(tbevelkind));
                 bevelkind:=tempbk;

                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 hascheckbox:=tempb;
                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 beepOnActivate:=tempb;                 
                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 showhotkeys:=tempb;

                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          2: begin
               //tcheat
               with tcheat.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(cheatnr,sizeof(integer));
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 activationcolor:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 color:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 textcolor:=tempc;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 hotkeyleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 descriptionleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editwidth:=tempi;

                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 hascheckbox:=tempb;
                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 beeponactivate:=tempb;                 
                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 showhotkey:=tempb;

                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          3: begin
               //timage
               with timage2.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(tempcursor,sizeof(tcursor));
                 cursor:=tempcursor;

                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 stretch:=tempb;
                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 transparent:=tempb;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 tag:=tempi;

                 //-

                 trainer.ReadBuffer(temp,4); //size of extension
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);  //extension
                 x[temp]:=#0;
                 extension:=x;

                 temps:=tempdir+inttostr(getcurrentprocessid)+'-'+inttostr(filecount)+x;
                 freemem(x);

                 f:=tfilestream.Create(temps, fmCreate or fmShareDenyNone);
                 inc(filecount);

                 trainer.ReadBuffer(temp,4); //size of imagedata
                 getmem(x,temp);
                 trainer.ReadBuffer(x^,temp);
                 f.WriteBuffer(x^,temp);
                 freemem(x);
                 f.free;

                 Picture.LoadFromFile(temps);


                 imagedata:=tmemorystream.Create;
                 imagedata.LoadFromFile(temps);

                 DeleteFile(temps);
                 //-


                 //command
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 command:=x;
                 freemem(x);

                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          4: begin
               with tlabel2.Create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 //caption
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 caption:=x;
                 freemem(x);

                 //wordwrap
                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 wordwrap:=tempb;

                 //color
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 font.Color:=tempc;

                 //command
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 command:=x;
                 freemem(x);

                 //cursor
                 trainer.ReadBuffer(tempcursor,sizeof(tcursor));
                 cursor:=tempcursor;

                 //tag
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 tag:=tempi;

                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 if tempb then
                   Font.Style:=[fsUnderline]
                 else
                   Font.Style:=[];

                   
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;
          $ffffffff: break;
          else raise exception.Create(strunknowncomponent+Inttostr(temp));
        end;
      end;
    end;
  end;

  //fill in the list of cheats
  for i:=0 to length(frmMemoryModifier.trainerdata)-1 do
  begin
    frmMemoryModifier.recordview.Items.Add.caption:=frmMemoryModifier.trainerdata[i].description;
    frmMemoryModifier.recordview.Items[frmMemoryModifier.recordview.Items.count-1].SubItems.add(frmMemoryModifier.trainerdata[i].hotkeytext);
  end;

  frmMemoryTrainerPreview.UpdateScreen;
  if frmtrainerdesigner<>nil then frmtrainerdesigner.updatecheats   *)
end;


procedure LoadTrainer7(trainer:TFilestream);
var temp: dword;
    tempb: boolean;
    tempc: tcolor;
    tempi: integer;
    tempbc: tbevelcut;
  (*  tempbk: tbevelkind; *)
    tempcursor: tcursor;
    i,j,k:integer;
    x: pchar;

    image: pointer;
//    trainerdata1: array of TTrainerData1;
begin
  (*
  with frmMemoryModifier do
  begin
    //size of trainerdata
    trainer.ReadBuffer(temp,4);
    setlength(frmMemoryModifier.trainerdata,temp);

    for i:=0 to length(trainerdata)-1 do
    begin
      //description
      trainer.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainer.ReadBuffer(x^,temp);
      x[temp]:=#0;
      trainerdata[i].description:=x;
      freemem(x);

      //hotkeytext
      trainer.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainer.ReadBuffer(x^,temp);
      x[temp]:=#0;
      trainerdata[i].hotkeytext:=x;
      freemem(x);

      trainer.readbuffer(trainerdata[i].hotkey,sizeof(trainerdata[i].hotkey));

      trainer.ReadBuffer(temp,4);
      setlength(trainerdata[i].codeentrys,temp);

      //opcodes of this cheat
      for j:=0 to length(trainerdata[i].codeentrys)-1 do
      begin
        //address
        trainer.ReadBuffer(trainerdata[i].codeentrys[j].address,4);

        //modulename
        trainer.ReadBuffer(temp,4);
        getmem(x,temp+1);
        trainer.ReadBuffer(x^,temp);
        x[temp]:=#0;
        trainerdata[i].codeentrys[j].modulename:=x;
        freemem(x);

        //module offset
        trainer.ReadBuffer(trainerdata[i].codeentrys[j].moduleoffset,4);

        //original opcode
        trainer.ReadBuffer(temp,4);
        setlength(trainerdata[i].codeentrys[j].originalopcode,temp);
        trainer.ReadBuffer(pointer(trainerdata[i].codeentrys[j].originalopcode)^,temp);
      end;

      //address entrys
      trainer.ReadBuffer(temp,4);
      setlength(trainerdata[i].addressentrys,temp);
      for j:=0 to length(trainerdata[i].addressentrys)-1 do
      begin
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].address,sizeof(trainerdata[i].addressentrys[j].address));

        //interpretable address
        trainer.ReadBuffer(temp,4);
        getmem(x,temp+1);
        trainer.ReadBuffer(x^,temp);
        x[temp]:=#0;
        trainerdata[i].addressentrys[j].interpretableaddress:=x;
        freemem(x);

        trainer.ReadBuffer(trainerdata[i].addressentrys[j].ispointer,sizeof(trainerdata[i].addressentrys[j].ispointer));

        trainer.ReadBuffer(tempi,4);
        setlength(trainerdata[i].addressentrys[j].pointers,tempi);

        for k:=0 to tempi-1 do
        begin
          trainer.readBuffer(trainerdata[i].addressentrys[j].pointers[k].address,sizeof(trainerdata[i].addressentrys[j].pointers[k].address));

          //interpretable address
          trainer.ReadBuffer(temp,4);
          getmem(x,temp+1);
          trainer.ReadBuffer(x^,temp);
          x[temp]:=#0;
          trainerdata[i].addressentrys[j].pointers[k].interpretableaddress:=x;
          freemem(x);

          trainer.readBuffer(trainerdata[i].addressentrys[j].pointers[k].offset,sizeof(trainerdata[i].addressentrys[j].pointers[k].offset));
        end;


        trainer.ReadBuffer(trainerdata[i].addressentrys[j].bit,sizeof(trainerdata[i].addressentrys[j].bit));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].memtyp,sizeof(trainerdata[i].addressentrys[j].memtyp));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].frozen,sizeof(trainerdata[i].addressentrys[j].frozen));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].frozendirection,sizeof(trainerdata[i].addressentrys[j].frozendirection));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].setvalue,sizeof(trainerdata[i].addressentrys[j].setvalue));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].userinput,sizeof(trainerdata[i].addressentrys[j].userinput));

        trainer.ReadBuffer(temp,4);
        getmem(x,temp+1);
        trainer.ReadBuffer(x^,temp);
        x[temp]:=#0;
        trainerdata[i].addressentrys[j].value:=x;
        freemem(x);


       // trainer.Readbuffer(trainerdata[i].addressentrys[j].value,50);
        if trainerdata[i].addressentrys[j].userinput then
        begin
          trainerdata[i].hasedit:=true;
          trainerdata[i].editvalue:=trainerdata[i].addressentrys[j].value;
        end;

        //interpretable address
        trainer.ReadBuffer(temp,4);
        getmem(x,temp+1);
        trainer.ReadBuffer(x^,temp);
        x[temp]:=#0;
        trainerdata[i].addressentrys[j].autoassemblescript:=x;
        freemem(x);
      end;
    end;

    //title
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edittitle.Text:=x;
    freemem(x);

    //launch filename
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edit2.Text:=x;
    freemem(x);

    //autolaunch
    trainer.ReadBuffer(tempb,sizeof(tempb));
    checkbox2.Checked:=tempb;

    //popup on keypress
    trainer.ReadBuffer(tempb,sizeof(tempb));
    checkbox1.Checked:=tempb;

    //process name
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    combobox1.Text:=x;
    freemem(x);

    //hotkeytext
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edithotkey.Text:=x;
    freemem(x);

    //hotkey+shiftstate
    trainer.ReadBuffer(popuphotkey,sizeof(popuphotkey));

    //abouttext
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    memo1.Text:=x;
    freemem(x);

    //freeze interval
    trainer.ReadBuffer(tempi,sizeof(tempi));
    editFreezeInterval.text:=inttostr(tempi);

    trainer.ReadBuffer(temp,4);
    if temp=$666 then
    begin
      //default
      //leftside image
      trainer.ReadBuffer(temp,4);  //size of the image
      if temp>0 then
      begin
        //getmem(image,temp);
        //trainer.ReadBuffer(image^,temp);
        frmMemorytrainerpreview.Image1.Picture.Bitmap.LoadFromStream(trainer);

        leftimageext:='.bmp';
        if leftimage<>nil then
          leftimage.free;
        leftimage:=tmemorystream.Create;
        frmMemorytrainerpreview.Image1.Picture.Bitmap.SaveToStream(leftimage);
      end;

      //windowwidth
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Width));
      frmMemorytrainerpreview.Width:=temp;

      //windowheight
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.height));
      frmMemorytrainerpreview.height:=temp;

      //leftsidewidth
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Panel1.Width));
      frmMemorytrainerpreview.Panel1.Width:=temp;

      //leftsideheight
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Panel1.height));
      frmMemorytrainerpreview.Panel1.height:=temp;
    end
    else
    begin
      //user defined
      frmMemoryModifier.dontshowdefault:=true;       //obsolete
      frmMemoryModifier.Button7.Click;

      //windowwidth
      trainer.readbuffer(temp,4);
      frmTrainerDesigner.Width:=temp;

      //windowheight
      trainer.readbuffer(temp,4);
      frmTrainerDesigner.height:=temp;


      while true do
      begin
        trainer.ReadBuffer(temp,4);
        case temp of
          0: begin
               //tbutton
               with tbutton2.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 //caption
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 caption:=x;
                 freemem(x);

                 //wordwrap
                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 wordwrap:=tempb;

                 //onclick
                 trainer.ReadBuffer(temp,sizeof(tag));
                 tag:=temp;
                 parent:=frmTrainerDesigner;

                 //command
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 command:=x;
                 freemem(x);

                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          1: begin
               //cheatlist
               with tcheatlist.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 activationcolor:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 color:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 textcolor:=tempc;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 hotkeyleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 descriptionleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editwidth:=tempi;

                 trainer.ReadBuffer(tempbc,sizeof(tbevelcut));
                 bevelinner:=tempbc;
                 trainer.ReadBuffer(tempbc,sizeof(tbevelcut));
                 bevelouter:=tempbc;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 bevelwidth:=tempi;
                 trainer.ReadBuffer(tempbk,sizeof(tbevelkind));
                 bevelkind:=tempbk;

                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 hascheckbox:=tempb;
                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 showhotkeys:=tempb;

                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          2: begin
               //tcheat
               with tcheat.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(cheatnr,sizeof(integer));
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 activationcolor:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 color:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 textcolor:=tempc;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 hotkeyleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 descriptionleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editwidth:=tempi;

                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 hascheckbox:=tempb;
                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 showhotkey:=tempb;

                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          3: begin
               //timage
               with timage2.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(tempcursor,sizeof(tcursor));
                 cursor:=tempcursor;

                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 stretch:=tempb;
                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 transparent:=tempb;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 tag:=tempi;

                 trainer.ReadBuffer(temp,4);
                 if temp>0 then
                 begin
                   picture.Bitmap.LoadFromStream(trainer);
                   extension:='.bmp';
                   imagedata:=tmemorystream.Create;
                   picture.Bitmap.SaveToStream(imagedata);

                 end;

                 //command
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 command:=x;
                 freemem(x);

                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          4: begin
               with tlabel2.Create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 //caption
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 caption:=x;
                 freemem(x);

                 //wordwrap
                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 wordwrap:=tempb;

                 //color
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 font.Color:=tempc;

                 //command
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 command:=x;
                 freemem(x);

                 //cursor
                 trainer.ReadBuffer(tempcursor,sizeof(tcursor));
                 cursor:=tempcursor;

                 //tag
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 tag:=tempi;

                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 if tempb then
                   Font.Style:=[fsUnderline]
                 else
                   Font.Style:=[];

                   
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;
          $ffffffff: break;
          else raise exception.Create(strunknowncomponent+Inttostr(temp));
        end;
      end;
    end;
  end;

  //fill in the list of cheats
  for i:=0 to length(frmMemoryModifier.trainerdata)-1 do
  begin
    frmMemoryModifier.recordview.Items.Add.caption:=frmMemoryModifier.trainerdata[i].description;
    frmMemoryModifier.recordview.Items[frmMemoryModifier.recordview.Items.count-1].SubItems.add(frmMemoryModifier.trainerdata[i].hotkeytext);
  end;

  frmMemoryTrainerPreview.UpdateScreen;
  if frmtrainerdesigner<>nil then frmtrainerdesigner.updatecheats
  *)
end;


procedure LoadTrainer6(trainer:TFilestream);
var temp: dword;
    tempb: boolean;
    tempc: tcolor;
    tempi: integer;
    tempbc: tbevelcut;
 (*   tempbk: tbevelkind; *)
    tempcursor: tcursor;
    i,j,k:integer;
    x: pchar;

    image: pointer;
    laststate: word;
    lastshiftstate:word;

    keycombo: tkeycombo;
//    trainerdata1: array of TTrainerData1;
begin
  (*
  with frmMemoryModifier do
  begin
    //size of trainerdata
    trainer.ReadBuffer(temp,4);
    setlength(frmMemoryModifier.trainerdata,temp);

    for i:=0 to length(trainerdata)-1 do
    begin
      //description
      trainer.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainer.ReadBuffer(x^,temp);
      x[temp]:=#0;
      trainerdata[i].description:=x;
      freemem(x);

      //hotkeytext
      trainer.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainer.ReadBuffer(x^,temp);
      x[temp]:=#0;
      trainerdata[i].hotkeytext:=x;
      freemem(x);

      //read and convert
      trainer.ReadBuffer(laststate,sizeof(laststate));
      trainer.ReadBuffer(lastshiftstate,sizeof(lastshiftstate));
      ConvertOldHotkeyToKeyCombo(lastshiftstate, laststate, trainerdata[i].hotkey);



      trainer.ReadBuffer(temp,4);
      setlength(trainerdata[i].codeentrys,temp);

      //opcodes of this cheat
      for j:=0 to length(trainerdata[i].codeentrys)-1 do
      begin
        //address
        trainer.ReadBuffer(trainerdata[i].codeentrys[j].address,4);

        //modulename
        trainer.ReadBuffer(temp,4);
        getmem(x,temp+1);
        trainer.ReadBuffer(x^,temp);
        x[temp]:=#0;
        trainerdata[i].codeentrys[j].modulename:=x;
        freemem(x);

        //module offset
        trainer.ReadBuffer(trainerdata[i].codeentrys[j].moduleoffset,4);

        //original opcode
        trainer.ReadBuffer(temp,4);
        setlength(trainerdata[i].codeentrys[j].originalopcode,temp);
        trainer.ReadBuffer(pointer(trainerdata[i].codeentrys[j].originalopcode)^,temp);
      end;

      //address entrys
      trainer.ReadBuffer(temp,4);
      setlength(trainerdata[i].addressentrys,temp);
      for j:=0 to length(trainerdata[i].addressentrys)-1 do
      begin
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].address,sizeof(trainerdata[i].addressentrys[j].address));

        //interpretable address
        trainer.ReadBuffer(temp,4);
        getmem(x,temp+1);
        trainer.ReadBuffer(x^,temp);
        x[temp]:=#0;
        trainerdata[i].addressentrys[j].interpretableaddress:=x;
        freemem(x);

        trainer.ReadBuffer(trainerdata[i].addressentrys[j].ispointer,sizeof(trainerdata[i].addressentrys[j].ispointer));

        trainer.ReadBuffer(tempi,4);
        setlength(trainerdata[i].addressentrys[j].pointers,tempi);

        for k:=0 to tempi-1 do
        begin
          trainer.readBuffer(trainerdata[i].addressentrys[j].pointers[k].address,sizeof(trainerdata[i].addressentrys[j].pointers[k].address));

          //interpretable address
          trainer.ReadBuffer(temp,4);
          getmem(x,temp+1);
          trainer.ReadBuffer(x^,temp);
          x[temp]:=#0;
          trainerdata[i].addressentrys[j].pointers[k].interpretableaddress:=x;
          freemem(x);

          trainer.readBuffer(trainerdata[i].addressentrys[j].pointers[k].offset,sizeof(trainerdata[i].addressentrys[j].pointers[k].offset));
        end;


        trainer.ReadBuffer(trainerdata[i].addressentrys[j].bit,sizeof(trainerdata[i].addressentrys[j].bit));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].memtyp,sizeof(trainerdata[i].addressentrys[j].memtyp));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].frozen,sizeof(trainerdata[i].addressentrys[j].frozen));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].frozendirection,sizeof(trainerdata[i].addressentrys[j].frozendirection));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].setvalue,sizeof(trainerdata[i].addressentrys[j].setvalue));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].userinput,sizeof(trainerdata[i].addressentrys[j].userinput));

        trainer.ReadBuffer(temp,4);
        getmem(x,temp+1);
        trainer.ReadBuffer(x^,temp);
        x[temp]:=#0;
        trainerdata[i].addressentrys[j].value:=x;
        freemem(x);


       // trainer.Readbuffer(trainerdata[i].addressentrys[j].value,50);
        if trainerdata[i].addressentrys[j].userinput then
        begin
          trainerdata[i].hasedit:=true;
          trainerdata[i].editvalue:=trainerdata[i].addressentrys[j].value;
        end;

        //interpretable address
        trainer.ReadBuffer(temp,4);
        getmem(x,temp+1);
        trainer.ReadBuffer(x^,temp);
        x[temp]:=#0;
        trainerdata[i].addressentrys[j].autoassemblescript:=x;
        freemem(x);
      end;
    end;

    //title
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edittitle.Text:=x;
    freemem(x);

    //launch filename
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edit2.Text:=x;
    freemem(x);

    //autolaunch
    trainer.ReadBuffer(tempb,sizeof(tempb));
    checkbox2.Checked:=tempb;

    //popup on keypress
    trainer.ReadBuffer(tempb,sizeof(tempb));
    checkbox1.Checked:=tempb;

    //process name
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    combobox1.Text:=x;
    freemem(x);

    //hotkeytext
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edithotkey.Text:=x;
    freemem(x);

    //hotkey+shiftstate
    trainer.ReadBuffer(laststate,sizeof(laststate));
    trainer.ReadBuffer(lastshiftstate,sizeof(lastshiftstate));
    ConvertOldHotkeyToKeyCombo(lastshiftstate, laststate, popuphotkey);
    

    //abouttext
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    memo1.Text:=x;
    freemem(x);

    //freeze interval
    trainer.ReadBuffer(tempi,sizeof(tempi));
    editFreezeInterval.text:=inttostr(tempi);

    trainer.ReadBuffer(temp,4);
    if temp=$666 then
    begin
      //default
      //leftside image
      trainer.ReadBuffer(temp,4);  //size of the image
      if temp>0 then
      begin
        //getmem(image,temp);
        //trainer.ReadBuffer(image^,temp);
        frmMemorytrainerpreview.Image1.Picture.Bitmap.LoadFromStream(trainer);
         leftimageext:='.bmp';
         if leftimage<>nil then
           leftimage.free;
         leftimage:=tmemorystream.Create;
         frmMemorytrainerpreview.Image1.Picture.Bitmap.SaveToStream(leftimage);

      end;

      //windowwidth
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Width));
      frmMemorytrainerpreview.Width:=temp;

      //windowheight
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.height));
      frmMemorytrainerpreview.height:=temp;

      //leftsidewidth
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Panel1.Width));
      frmMemorytrainerpreview.Panel1.Width:=temp;

      //leftsideheight
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Panel1.height));
      frmMemorytrainerpreview.Panel1.height:=temp;
    end
    else
    begin
      //user defined
      frmMemoryModifier.dontshowdefault:=true;       //obsolete
      frmMemoryModifier.Button7.Click;

      //windowwidth
      trainer.readbuffer(temp,4);
      frmTrainerDesigner.Width:=temp;

      //windowheight
      trainer.readbuffer(temp,4);
      frmTrainerDesigner.height:=temp;


      while true do
      begin
        trainer.ReadBuffer(temp,4);
        case temp of
          0: begin
               //tbutton
               with tbutton2.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 //caption
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 caption:=x;
                 freemem(x);

                 //wordwrap
                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 wordwrap:=tempb;

                 //onclick
                 trainer.ReadBuffer(temp,sizeof(tag));
                 tag:=temp;
                 parent:=frmTrainerDesigner;

                 //command
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 command:=x;
                 freemem(x);

                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          1: begin
               //cheatlist
               with tcheatlist.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 activationcolor:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 color:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 textcolor:=tempc;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 hotkeyleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 descriptionleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editwidth:=tempi;

                 trainer.ReadBuffer(tempbc,sizeof(tbevelcut));
                 bevelinner:=tempbc;
                 trainer.ReadBuffer(tempbc,sizeof(tbevelcut));
                 bevelouter:=tempbc;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 bevelwidth:=tempi;
                 trainer.ReadBuffer(tempbk,sizeof(tbevelkind));
                 bevelkind:=tempbk;

                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 hascheckbox:=tempb;
                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 showhotkeys:=tempb;

                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          2: begin
               //tcheat
               with tcheat.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(cheatnr,sizeof(integer));
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 activationcolor:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 color:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 textcolor:=tempc;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 hotkeyleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 descriptionleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editwidth:=tempi;

                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 hascheckbox:=tempb;
                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 showhotkey:=tempb;

                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          3: begin
               //timage
               with timage2.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(tempcursor,sizeof(tcursor));
                 cursor:=tempcursor;

                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 stretch:=tempb;
                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 transparent:=tempb;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 tag:=tempi;

                 trainer.ReadBuffer(temp,4);
                 if temp>0 then
                 begin
                   picture.Bitmap.LoadFromStream(trainer);
                   extension:='.bmp';
                   imagedata:=tmemorystream.Create;
                   picture.Bitmap.SaveToStream(leftimage);
                 end;

                 //command
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 command:=x;
                 freemem(x);

                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          4: begin
               with tlabel2.Create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 //caption
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 caption:=x;
                 freemem(x);

                 //wordwrap
                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 wordwrap:=tempb;

                 //color
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 font.Color:=tempc;

                 //command
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 command:=x;
                 freemem(x);

                 //cursor
                 trainer.ReadBuffer(tempcursor,sizeof(tcursor));
                 cursor:=tempcursor;

                 //tag
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 tag:=tempi;

                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 if tempb then
                   Font.Style:=[fsUnderline]
                 else
                   Font.Style:=[];

                   
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;
          $ffffffff: break;
          else raise exception.Create(strunknowncomponent+Inttostr(temp));
        end;
      end;
    end;
  end;

  //fill in the list of cheats
  for i:=0 to length(frmMemoryModifier.trainerdata)-1 do
  begin
    frmMemoryModifier.recordview.Items.Add.caption:=frmMemoryModifier.trainerdata[i].description;
    frmMemoryModifier.recordview.Items[frmMemoryModifier.recordview.Items.count-1].SubItems.add(frmMemoryModifier.trainerdata[i].hotkeytext);
  end;

  frmMemoryTrainerPreview.UpdateScreen;
  if frmtrainerdesigner<>nil then frmtrainerdesigner.updatecheats
  *)
end;


procedure LoadTrainer5(trainer:TFilestream);
var temp: dword;
    tempb: boolean;
    tempc: tcolor;
    tempi: integer;
    tempbc: tbevelcut;
    (*tempbk: tbevelkind; *)
    tempcursor: tcursor;
    i,j,k:integer;
    x: pchar;

    image: pointer;
    laststate: word;
    lastshiftstate: word;
//    trainerdata1: array of TTrainerData1;
begin
  (*
  with frmMemoryModifier do
  begin
    //size of trainerdata
    trainer.ReadBuffer(temp,4);
    setlength(frmMemoryModifier.trainerdata,temp);

    for i:=0 to length(trainerdata)-1 do
    begin
      //description
      trainer.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainer.ReadBuffer(x^,temp);
      x[temp]:=#0;
      trainerdata[i].description:=x;
      freemem(x);

      //hotkeytext
      trainer.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainer.ReadBuffer(x^,temp);
      x[temp]:=#0;
      trainerdata[i].hotkeytext:=x;
      freemem(x);

      trainer.ReadBuffer(laststate,2);
      trainer.ReadBuffer(lastshiftstate,sizeof(lastshiftstate));
      ConvertOldHotkeyToKeyCombo(lastshiftstate, laststate, trainerdata[i].hotkey);


      trainer.ReadBuffer(temp,4);
      setlength(trainerdata[i].codeentrys,temp);

      //opcodes of this cheat
      for j:=0 to length(trainerdata[i].codeentrys)-1 do
      begin
        //address
        trainer.ReadBuffer(trainerdata[i].codeentrys[j].address,4);

        //original opcode
        trainer.ReadBuffer(temp,4);
        setlength(trainerdata[i].codeentrys[j].originalopcode,temp);
        trainer.ReadBuffer(pointer(trainerdata[i].codeentrys[j].originalopcode)^,temp);
      end;

      //address entrys
      trainer.ReadBuffer(temp,4);
      setlength(trainerdata[i].addressentrys,temp);
      for j:=0 to length(trainerdata[i].addressentrys)-1 do
      begin
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].address,sizeof(trainerdata[i].addressentrys[j].address));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].ispointer,sizeof(trainerdata[i].addressentrys[j].ispointer));

        trainer.ReadBuffer(tempi,4);
        setlength(trainerdata[i].addressentrys[j].pointers,tempi);

        for k:=0 to tempi-1 do
        begin
          trainer.readBuffer(trainerdata[i].addressentrys[j].pointers[k].address,sizeof(trainerdata[i].addressentrys[j].pointers[k].address));
          trainer.readBuffer(trainerdata[i].addressentrys[j].pointers[k].offset,sizeof(trainerdata[i].addressentrys[j].pointers[k].offset));
        end;


        trainer.ReadBuffer(trainerdata[i].addressentrys[j].bit,sizeof(trainerdata[i].addressentrys[j].bit));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].memtyp,sizeof(trainerdata[i].addressentrys[j].memtyp));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].frozen,sizeof(trainerdata[i].addressentrys[j].frozen));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].frozendirection,sizeof(trainerdata[i].addressentrys[j].frozendirection));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].setvalue,sizeof(trainerdata[i].addressentrys[j].setvalue));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].userinput,sizeof(trainerdata[i].addressentrys[j].userinput));

        trainer.ReadBuffer(temp,4);
        getmem(x,temp+1);
        trainer.ReadBuffer(x^,temp);
        x[temp]:=#0;
        trainerdata[i].addressentrys[j].value:=x;
        freemem(x);


       // trainer.Readbuffer(trainerdata[i].addressentrys[j].value,50);
        if trainerdata[i].addressentrys[j].userinput then
        begin
          trainerdata[i].hasedit:=true;
          trainerdata[i].editvalue:=trainerdata[i].addressentrys[j].value;
        end;

      end;
    end;

    //title
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edittitle.Text:=x;
    freemem(x);

    //launch filename
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edit2.Text:=x;
    freemem(x);

    //autolaunch
    trainer.ReadBuffer(tempb,sizeof(tempb));
    checkbox2.Checked:=tempb;

    //popup on keypress
    trainer.ReadBuffer(tempb,sizeof(tempb));
    checkbox1.Checked:=tempb;

    //process name
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    combobox1.Text:=x;
    freemem(x);

    //hotkeytext
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edithotkey.Text:=x;
    freemem(x);

    //hotkey+shiftstate
    trainer.ReadBuffer(laststate,2);
    trainer.ReadBuffer(lastshiftstate,sizeof(lastshiftstate));
    ConvertOldHotkeyToKeyCombo(lastshiftstate, laststate, popuphotkey);


    //abouttext
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    memo1.Text:=x;
    freemem(x);

    //freeze interval
    trainer.ReadBuffer(tempi,sizeof(tempi));
    editFreezeInterval.text:=inttostr(tempi);

    trainer.ReadBuffer(temp,4);
    if temp=$666 then
    begin
      //default
      //leftside image
      trainer.ReadBuffer(temp,4);  //size of the image
      if temp>0 then
      begin
        //getmem(image,temp);
        //trainer.ReadBuffer(image^,temp);
        frmMemorytrainerpreview.Image1.Picture.Bitmap.LoadFromStream(trainer);
      end;

      //windowwidth
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Width));
      frmMemorytrainerpreview.Width:=temp;

      //windowheight
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.height));
      frmMemorytrainerpreview.height:=temp;

      //leftsidewidth
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Panel1.Width));
      frmMemorytrainerpreview.Panel1.Width:=temp;

      //leftsideheight
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Panel1.height));
      frmMemorytrainerpreview.Panel1.height:=temp;
    end
    else
    begin
      //user defined
      frmMemoryModifier.dontshowdefault:=true;       //obsolete
      frmMemoryModifier.Button7.Click;

      //windowwidth
      trainer.readbuffer(temp,4);
      frmTrainerDesigner.Width:=temp;

      //windowheight
      trainer.readbuffer(temp,4);
      frmTrainerDesigner.height:=temp;


      while true do
      begin
        trainer.ReadBuffer(temp,4);
        case temp of
          0: begin
               //tbutton
               with tbutton2.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 //caption
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 caption:=x;
                 freemem(x);

                 //wordwrap
                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 wordwrap:=tempb;

                 //onclick
                 trainer.ReadBuffer(temp,sizeof(tag));
                 tag:=temp;
                 parent:=frmTrainerDesigner;

                 //command
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 command:=x;
                 freemem(x);

                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          1: begin
               //cheatlist
               with tcheatlist.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 activationcolor:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 color:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 textcolor:=tempc;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 hotkeyleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 descriptionleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editwidth:=tempi;

                 trainer.ReadBuffer(tempbc,sizeof(tbevelcut));
                 bevelinner:=tempbc;
                 trainer.ReadBuffer(tempbc,sizeof(tbevelcut));
                 bevelouter:=tempbc;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 bevelwidth:=tempi;
                 trainer.ReadBuffer(tempbk,sizeof(tbevelkind));
                 bevelkind:=tempbk;
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          2: begin
               //tcheat
               with tcheat.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(cheatnr,sizeof(integer));
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 activationcolor:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 color:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 textcolor:=tempc;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 hotkeyleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 descriptionleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editwidth:=tempi;
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          3: begin
               //timage
               with timage2.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(tempcursor,sizeof(tcursor));
                 cursor:=tempcursor;

                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 stretch:=tempb;
                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 transparent:=tempb;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 tag:=tempi;

                 trainer.ReadBuffer(temp,4);
                 if temp>0 then
                 begin
                   picture.Bitmap.LoadFromStream(trainer);
                 end;

                 //command
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 command:=x;
                 freemem(x);

                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          4: begin
               with tlabel2.Create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 //caption
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 caption:=x;
                 freemem(x);

                 //wordwrap
                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 wordwrap:=tempb;

                 //color
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 font.Color:=tempc;

                 //command
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 command:=x;
                 freemem(x);

                 //cursor
                 trainer.ReadBuffer(tempcursor,sizeof(tcursor));
                 cursor:=tempcursor;

                 //tag
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 tag:=tempi;

                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 if tempb then
                   Font.Style:=[fsUnderline]
                 else
                   Font.Style:=[];

                   
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;
          $ffffffff: break;
          else raise exception.Create(strunknowncomponent+Inttostr(temp));
        end;
      end;
    end;
  end;

  //fill in the list of cheats
  for i:=0 to length(frmMemoryModifier.trainerdata)-1 do
  begin
    frmMemoryModifier.recordview.Items.Add.caption:=frmMemoryModifier.trainerdata[i].description;
    frmMemoryModifier.recordview.Items[frmMemoryModifier.recordview.Items.count-1].SubItems.add(frmMemoryModifier.trainerdata[i].hotkeytext);
  end;

  frmMemoryTrainerPreview.UpdateScreen;
  if frmtrainerdesigner<>nil then frmtrainerdesigner.updatecheats
  *)
end;


procedure LoadTrainer4(trainer:TFilestream);
var temp: dword;
    tempb: boolean;
    tempc: tcolor;
    tempi: integer;
    tempbc: tbevelcut;
  (*  tempbk: tbevelkind;  *)
    tempcursor: tcursor;
    i,j,k:integer;
    x: pchar;

    image: pointer;
    laststate: word;
    lastshiftstate: word;     
//    trainerdata1: array of TTrainerData1;
begin
  (*
  with frmMemoryModifier do
  begin
    //size of trainerdata
    trainer.ReadBuffer(temp,4);
    setlength(frmMemoryModifier.trainerdata,temp);

    for i:=0 to length(trainerdata)-1 do
    begin
      //description
      trainer.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainer.ReadBuffer(x^,temp);
      x[temp]:=#0;
      trainerdata[i].description:=x;
      freemem(x);

      //hotkeytext
      trainer.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainer.ReadBuffer(x^,temp);
      x[temp]:=#0;
      trainerdata[i].hotkeytext:=x;
      freemem(x);

      trainer.ReadBuffer(laststate,2);
      trainer.ReadBuffer(lastshiftstate,sizeof(lastshiftstate));
      ConvertOldHotkeyToKeyCombo(lastshiftstate, laststate, trainerdata[i].hotkey);


      trainer.ReadBuffer(temp,4);
      setlength(trainerdata[i].codeentrys,temp);

      //opcodes of this cheat
      for j:=0 to length(trainerdata[i].codeentrys)-1 do
      begin
        //address
        trainer.ReadBuffer(trainerdata[i].codeentrys[j].address,4);

        //original opcode
        trainer.ReadBuffer(temp,4);
        setlength(trainerdata[i].codeentrys[j].originalopcode,temp);
        trainer.ReadBuffer(pointer(trainerdata[i].codeentrys[j].originalopcode)^,temp);
      end;

      //address entrys
      trainer.ReadBuffer(temp,4);
      setlength(trainerdata[i].addressentrys,temp);
      for j:=0 to length(trainerdata[i].addressentrys)-1 do
      begin
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].address,sizeof(trainerdata[i].addressentrys[j].address));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].ispointer,sizeof(trainerdata[i].addressentrys[j].ispointer));

        trainer.ReadBuffer(tempi,4);
        setlength(trainerdata[i].addressentrys[j].pointers,tempi);

        for k:=0 to tempi-1 do
        begin
          trainer.readBuffer(trainerdata[i].addressentrys[j].pointers[k].address,sizeof(trainerdata[i].addressentrys[j].pointers[k].address));
          trainer.readBuffer(trainerdata[i].addressentrys[j].pointers[k].offset,sizeof(trainerdata[i].addressentrys[j].pointers[k].offset));
        end;


        trainer.ReadBuffer(trainerdata[i].addressentrys[j].bit,sizeof(trainerdata[i].addressentrys[j].bit));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].memtyp,sizeof(trainerdata[i].addressentrys[j].memtyp));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].frozen,sizeof(trainerdata[i].addressentrys[j].frozen));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].frozendirection,sizeof(trainerdata[i].addressentrys[j].frozendirection));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].setvalue,sizeof(trainerdata[i].addressentrys[j].setvalue));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].userinput,sizeof(trainerdata[i].addressentrys[j].userinput));

        trainer.ReadBuffer(temp,4);
        getmem(x,temp+1);
        trainer.ReadBuffer(x^,temp);
        x[temp]:=#0;
        trainerdata[i].addressentrys[j].value:=x;
        freemem(x);


       // trainer.Readbuffer(trainerdata[i].addressentrys[j].value,50);
        if trainerdata[i].addressentrys[j].userinput then
        begin
          trainerdata[i].hasedit:=true;
          trainerdata[i].editvalue:=trainerdata[i].addressentrys[j].value;
        end;

      end;
    end;

    //title
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edittitle.Text:=x;
    freemem(x);

    //launch filename
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edit2.Text:=x;
    freemem(x);

    //autolaunch
    trainer.ReadBuffer(tempb,sizeof(tempb));
    checkbox2.Checked:=tempb;

    //popup on keypress
    trainer.ReadBuffer(tempb,sizeof(tempb));
    checkbox1.Checked:=tempb;

    //process name
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    combobox1.Text:=x;
    freemem(x);

    //hotkeytext
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edithotkey.Text:=x;
    freemem(x);

    //hotkey+shiftstate
    trainer.ReadBuffer(laststate,2);
    trainer.ReadBuffer(lastshiftstate,sizeof(lastshiftstate));
    ConvertOldHotkeyToKeyCombo(lastshiftstate, laststate, popuphotkey);


    //abouttext
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    memo1.Text:=x;
    freemem(x);

    trainer.ReadBuffer(temp,4);
    if temp=$666 then
    begin
      //default
      //leftside image
      trainer.ReadBuffer(temp,4);  //size of the image
      if temp>0 then
      begin
        //getmem(image,temp);
        //trainer.ReadBuffer(image^,temp);
        frmMemorytrainerpreview.Image1.Picture.Bitmap.LoadFromStream(trainer);
      end;

      //windowwidth
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Width));
      frmMemorytrainerpreview.Width:=temp;

      //windowheight
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.height));
      frmMemorytrainerpreview.height:=temp;

      //leftsidewidth
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Panel1.Width));
      frmMemorytrainerpreview.Panel1.Width:=temp;

      //leftsideheight
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Panel1.height));
      frmMemorytrainerpreview.Panel1.height:=temp;
    end
    else
    begin
      //user defined
      frmMemoryModifier.dontshowdefault:=true;       //obsolete
      frmMemoryModifier.Button7.Click;

      //windowwidth
      trainer.readbuffer(temp,4);
      frmTrainerDesigner.Width:=temp;

      //windowheight
      trainer.readbuffer(temp,4);
      frmTrainerDesigner.height:=temp;


      while true do
      begin
        trainer.ReadBuffer(temp,4);
        case temp of
          0: begin
               //tbutton
               with tbutton2.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 //caption
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 caption:=x;
                 freemem(x);

                 //onclick
                 trainer.ReadBuffer(temp,sizeof(tag));
                 tag:=temp;
                 parent:=frmTrainerDesigner;

                 //command
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 command:=x;
                 freemem(x);

                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          1: begin
               //cheatlist
               with tcheatlist.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 activationcolor:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 color:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 textcolor:=tempc;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 hotkeyleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 descriptionleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editwidth:=tempi;

                 trainer.ReadBuffer(tempbc,sizeof(tbevelcut));
                 bevelinner:=tempbc;
                 trainer.ReadBuffer(tempbc,sizeof(tbevelcut));
                 bevelouter:=tempbc;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 bevelwidth:=tempi;
                 trainer.ReadBuffer(tempbk,sizeof(tbevelkind));
                 bevelkind:=tempbk;
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          2: begin
               //tcheat
               with tcheat.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(cheatnr,sizeof(integer));
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 activationcolor:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 color:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 textcolor:=tempc;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 hotkeyleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 descriptionleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editwidth:=tempi;
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          3: begin
               //timage
               with timage2.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(tempcursor,sizeof(tcursor));
                 cursor:=tempcursor;

                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 stretch:=tempb;
                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 transparent:=tempb;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 tag:=tempi;

                 trainer.ReadBuffer(temp,4);
                 if temp>0 then
                 begin
                   picture.Bitmap.LoadFromStream(trainer);
                 end;

                 //command
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 command:=x;
                 freemem(x);

                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          4: begin
               with tlabel2.Create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 //caption
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 caption:=x;
                 freemem(x);

                 //wordwrap
                 trainer.ReadBuffer(tempb,sizeof(boolean));
                 wordwrap:=tempb;

                 //color
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 font.Color:=tempc;

                 //command
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 command:=x;
                 freemem(x);

                 //cursor
                 trainer.ReadBuffer(tempcursor,sizeof(tcursor));
                 cursor:=tempcursor;

                 //tag
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 tag:=tempi;

                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 if tempb then
                   Font.Style:=[fsUnderline]
                 else
                   Font.Style:=[];

                   
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;
          $ffffffff: break;
          else raise exception.Create(strunknowncomponent+Inttostr(temp));
        end;
      end;
    end;
  end;

  //fill in the list of cheats
  for i:=0 to length(frmMemoryModifier.trainerdata)-1 do
  begin
    frmMemoryModifier.recordview.Items.Add.caption:=frmMemoryModifier.trainerdata[i].description;
    frmMemoryModifier.recordview.Items[frmMemoryModifier.recordview.Items.count-1].SubItems.add(frmMemoryModifier.trainerdata[i].hotkeytext);
  end;

  frmMemoryTrainerPreview.UpdateScreen;
  if frmtrainerdesigner<>nil then frmtrainerdesigner.updatecheats     *)
end;

procedure LoadTrainer3(trainer:tfilestream);
var temp: dword;
    tempb: boolean;
    tempc: tcolor;
    tempi: integer;
    tempbc: tbevelcut;
    (*tempbk: tbevelkind;  *)
    tempcursor: tcursor;
    i,j,k:integer;
    x: pchar;

    image: pointer;
    laststate: word;
    lastshiftstate: word; 
//    trainerdata1: array of TTrainerData1;
begin
  (*
  with frmMemoryModifier do
  begin
    //size of trainerdata
    trainer.ReadBuffer(temp,4);
    setlength(frmMemoryModifier.trainerdata,temp);

    for i:=0 to length(trainerdata)-1 do
    begin
      //description
      trainer.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainer.ReadBuffer(x^,temp);
      x[temp]:=#0;
      trainerdata[i].description:=x;
      freemem(x);

      //hotkeytext
      trainer.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainer.ReadBuffer(x^,temp);
      x[temp]:=#0;
      trainerdata[i].hotkeytext:=x;
      freemem(x);

      trainer.ReadBuffer(laststate,2);
      trainer.ReadBuffer(lastshiftstate,sizeof(lastshiftstate));
      ConvertOldHotkeyToKeyCombo(lastshiftstate, laststate, trainerdata[i].hotkey);


      trainer.ReadBuffer(temp,4);
      setlength(trainerdata[i].codeentrys,temp);

      //opcodes of this cheat
      for j:=0 to length(trainerdata[i].codeentrys)-1 do
      begin
        //address
        trainer.ReadBuffer(trainerdata[i].codeentrys[j].address,4);

        //original opcode
        trainer.ReadBuffer(temp,4);
        setlength(trainerdata[i].codeentrys[j].originalopcode,temp);
        trainer.ReadBuffer(pointer(trainerdata[i].codeentrys[j].originalopcode)^,temp);
      end;

      //address entrys
      trainer.ReadBuffer(temp,4);
      setlength(trainerdata[i].addressentrys,temp);
      for j:=0 to length(trainerdata[i].addressentrys)-1 do
      begin
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].address,sizeof(trainerdata[i].addressentrys[j].address));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].ispointer,sizeof(trainerdata[i].addressentrys[j].ispointer));

        trainer.ReadBuffer(temp,4);
        setlength(trainerdata[i].addressentrys[j].pointers,temp);

        trainer.WriteBuffer(temp,4);
        for k:=0 to temp-1 do
        begin
          trainer.readBuffer(trainerdata[i].addressentrys[j].pointers[k].address,sizeof(trainerdata[i].addressentrys[j].pointers[k].address));
          trainer.readBuffer(trainerdata[i].addressentrys[j].pointers[k].offset,sizeof(trainerdata[i].addressentrys[j].pointers[k].offset));
        end;


        trainer.ReadBuffer(trainerdata[i].addressentrys[j].bit,sizeof(trainerdata[i].addressentrys[j].bit));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].memtyp,sizeof(trainerdata[i].addressentrys[j].memtyp));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].frozen,sizeof(trainerdata[i].addressentrys[j].frozen));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].frozendirection,sizeof(trainerdata[i].addressentrys[j].frozendirection));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].setvalue,sizeof(trainerdata[i].addressentrys[j].setvalue));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].userinput,sizeof(trainerdata[i].addressentrys[j].userinput));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].value,50);
        if trainerdata[i].addressentrys[j].userinput then
        begin
          trainerdata[i].hasedit:=true;
          trainerdata[i].editvalue:=trainerdata[i].addressentrys[j].value;
        end;

      end;
    end;

    //title
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edittitle.Text:=x;
    freemem(x);

    //launch filename
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edit2.Text:=x;
    freemem(x);

    //autolaunch
    trainer.ReadBuffer(tempb,sizeof(tempb));
    checkbox2.Checked:=tempb;

    //popup on keypress
    trainer.ReadBuffer(tempb,sizeof(tempb));
    checkbox1.Checked:=tempb;

    //process name
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    combobox1.Text:=x;
    freemem(x);

    //hotkeytext
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edithotkey.Text:=x;
    freemem(x);

    //hotkey+shiftstate
    trainer.ReadBuffer(laststate,2);
    trainer.ReadBuffer(lastshiftstate,sizeof(lastshiftstate));
    ConvertOldHotkeyToKeyCombo(lastshiftstate, laststate, popuphotkey);


    //abouttext
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    memo1.Text:=x;
    freemem(x);

    trainer.ReadBuffer(temp,4);
    if temp=$666 then
    begin
      //default
      //leftside image
      trainer.ReadBuffer(temp,4);  //size of the image
      if temp>0 then
      begin
        //getmem(image,temp);
        //trainer.ReadBuffer(image^,temp);
        frmMemorytrainerpreview.Image1.Picture.Bitmap.LoadFromStream(trainer);
      end;

      //windowwidth
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Width));
      frmMemorytrainerpreview.Width:=temp;

      //windowheight
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.height));
      frmMemorytrainerpreview.height:=temp;

      //leftsidewidth
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Panel1.Width));
      frmMemorytrainerpreview.Panel1.Width:=temp;

      //leftsideheight
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Panel1.height));
      frmMemorytrainerpreview.Panel1.height:=temp;
    end
    else
    begin
      //user defined
      frmMemoryModifier.dontshowdefault:=true;       //obsolete
      frmMemoryModifier.Button7.Click;

      //windowwidth
      trainer.readbuffer(temp,4);
      frmTrainerDesigner.Width:=temp;

      //windowheight
      trainer.readbuffer(temp,4);
      frmTrainerDesigner.height:=temp;


      while true do
      begin
        trainer.ReadBuffer(temp,4);
        case temp of
          0: begin
               //tbutton
               with tbutton.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 //caption
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 caption:=x;
                 freemem(x);

                 trainer.ReadBuffer(temp,sizeof(tag));
                 tag:=temp;
                 parent:=frmTrainerDesigner;

                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          1: begin
               //cheatlist
               with tcheatlist.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 activationcolor:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 color:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 textcolor:=tempc;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 hotkeyleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 descriptionleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editwidth:=tempi;

                 trainer.ReadBuffer(tempbc,sizeof(tbevelcut));
                 bevelinner:=tempbc;
                 trainer.ReadBuffer(tempbc,sizeof(tbevelcut));
                 bevelouter:=tempbc;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 bevelwidth:=tempi;
                 trainer.ReadBuffer(tempbk,sizeof(tbevelkind));
                 bevelkind:=tempbk;
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          2: begin
               //tcheat
               with tcheat.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(cheatnr,sizeof(integer));
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 activationcolor:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 color:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 textcolor:=tempc;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 hotkeyleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 descriptionleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editwidth:=tempi;
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          3: begin
               //timage
               with timage.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(tempcursor,sizeof(tcursor));
                 cursor:=tempcursor;

                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 stretch:=tempb;
                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 transparent:=tempb;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 tag:=tempi;

                 trainer.ReadBuffer(temp,4);
                 if temp>0 then
                 begin
                   picture.Bitmap.LoadFromStream(trainer);
                 end;

                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          4: begin
               with tlabel.Create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 //caption
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 caption:=x;
                 freemem(x);

                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 font.Color:=tempc;
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;                 
               end;
             end;
          $ffffffff: break;
          else raise exception.Create(strunknowncomponent+Inttostr(temp));
        end;
      end;
    end;
  end;

  //fill in the list of cheats
  for i:=0 to length(frmMemoryModifier.trainerdata)-1 do
  begin
    frmMemoryModifier.recordview.Items.Add.caption:=frmMemoryModifier.trainerdata[i].description;
    frmMemoryModifier.recordview.Items[frmMemoryModifier.recordview.Items.count-1].SubItems.add(frmMemoryModifier.trainerdata[i].hotkeytext);
  end;

  frmMemoryTrainerPreview.UpdateScreen;
  if frmtrainerdesigner<>nil then frmtrainerdesigner.updatecheats

  //freemem(x); *)
end;

procedure LoadTrainer2(trainer:tfilestream);
{type TcodeEntry1 = record
  address: dword;
  originalopcode: array of byte;
end;

type TAddressEntry1 = record
  address: dword;
  bit: byte;
  memtyp: integer;
  frozen: boolean;
  setvalue: boolean;
  userinput: boolean;
  value: string[50];
end;

type Ttrainerdata1 = record
  description: string;
  hotkeytext: string;
  hotkey: word;
  hotshift: word;
  hasedit: boolean;
  editvalue: string;

  codeentrys: array of TCodeEntry;
  addressentrys: array of TAddressEntry;
end;
 }
var temp: dword;
    tempb: boolean;
    tempc: tcolor;
    tempi: integer;
    tempbc: tbevelcut;
  (*  tempbk: tbevelkind;  *)
    tempcursor: tcursor;
    i,j:integer;
    x: pchar;

    image: pointer;
    laststate: word;
    lastshiftstate: word;
//    trainerdata1: array of TTrainerData1;
begin
 (* with frmMemoryModifier do
  begin
    //size of trainerdata
    trainer.ReadBuffer(temp,4);
    setlength(frmMemoryModifier.trainerdata,temp);

    for i:=0 to length(trainerdata)-1 do
    begin
      //description
      trainer.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainer.ReadBuffer(x^,temp);
      x[temp]:=#0;
      trainerdata[i].description:=x;
      freemem(x);

      //hotkeytext
      trainer.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainer.ReadBuffer(x^,temp);
      x[temp]:=#0;
      trainerdata[i].hotkeytext:=x;
      freemem(x);

      trainer.ReadBuffer(laststate,2);
      trainer.ReadBuffer(lastshiftstate,sizeof(lastshiftstate));
      ConvertOldHotkeyToKeyCombo(lastshiftstate, laststate, trainerdata[i].hotkey);


      trainer.ReadBuffer(temp,4);
      setlength(trainerdata[i].codeentrys,temp);

      //opcodes of this cheat
      for j:=0 to length(trainerdata[i].codeentrys)-1 do
      begin
        //address
        trainer.ReadBuffer(trainerdata[i].codeentrys[j].address,4);

        //original opcode
        trainer.ReadBuffer(temp,4);
        setlength(trainerdata[i].codeentrys[j].originalopcode,temp);
        trainer.ReadBuffer(pointer(trainerdata[i].codeentrys[j].originalopcode)^,temp);
      end;

      //address entrys
      trainer.ReadBuffer(temp,4);
      setlength(trainerdata[i].addressentrys,temp);
      for j:=0 to length(trainerdata[i].addressentrys)-1 do
      begin
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].address,sizeof(trainerdata[i].addressentrys[j].address));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].bit,sizeof(trainerdata[i].addressentrys[j].bit));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].memtyp,sizeof(trainerdata[i].addressentrys[j].memtyp));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].frozen,sizeof(trainerdata[i].addressentrys[j].frozen));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].frozendirection,sizeof(trainerdata[i].addressentrys[j].frozendirection));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].setvalue,sizeof(trainerdata[i].addressentrys[j].setvalue));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].userinput,sizeof(trainerdata[i].addressentrys[j].userinput));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].value,50);
        if trainerdata[i].addressentrys[j].userinput then
        begin
          trainerdata[i].hasedit:=true;
          trainerdata[i].editvalue:=trainerdata[i].addressentrys[j].value;
        end;

      end;
    end;

    //title
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edittitle.Text:=x;
    freemem(x);

    //launch filename
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edit2.Text:=x;
    freemem(x);

    //autolaunch
    trainer.ReadBuffer(tempb,sizeof(tempb));
    checkbox2.Checked:=tempb;

    //popup on keypress
    trainer.ReadBuffer(tempb,sizeof(tempb));
    checkbox1.Checked:=tempb;

    //process name
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    combobox1.Text:=x;
    freemem(x);

    //hotkeytext
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edithotkey.Text:=x;
    freemem(x);

    trainer.ReadBuffer(laststate,2);
    trainer.ReadBuffer(lastshiftstate,sizeof(lastshiftstate));
    ConvertOldHotkeyToKeyCombo(lastshiftstate, laststate, popuphotkey);


    //abouttext
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    memo1.Text:=x;
    freemem(x);

    trainer.ReadBuffer(temp,4);
    if temp=$666 then
    begin
      //default
      //leftside image
      trainer.ReadBuffer(temp,4);  //size of the image
      if temp>0 then
      begin
        //getmem(image,temp);
        //trainer.ReadBuffer(image^,temp);
        frmMemorytrainerpreview.Image1.Picture.Bitmap.LoadFromStream(trainer);
      end;

      //windowwidth
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Width));
      frmMemorytrainerpreview.Width:=temp;

      //windowheight
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.height));
      frmMemorytrainerpreview.height:=temp;

      //leftsidewidth
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Panel1.Width));
      frmMemorytrainerpreview.Panel1.Width:=temp;

      //leftsideheight
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Panel1.height));
      frmMemorytrainerpreview.Panel1.height:=temp;
    end
    else
    begin
      //user defined
      frmMemoryModifier.dontshowdefault:=true;       //obsolete
      frmMemoryModifier.Button7.Click;

      //windowwidth
      trainer.readbuffer(temp,4);
      frmTrainerDesigner.Width:=temp;

      //windowheight
      trainer.readbuffer(temp,4);
      frmTrainerDesigner.height:=temp;


      while true do
      begin
        trainer.ReadBuffer(temp,4);
        case temp of
          0: begin
               //tbutton
               with tbutton.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 //caption
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 caption:=x;
                 freemem(x);

                 trainer.ReadBuffer(temp,sizeof(tag));
                 tag:=temp;
                 parent:=frmTrainerDesigner;

                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          1: begin
               //cheatlist
               with tcheatlist.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 activationcolor:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 color:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 textcolor:=tempc;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 hotkeyleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 descriptionleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editwidth:=tempi;

                 trainer.ReadBuffer(tempbc,sizeof(tbevelcut));
                 bevelinner:=tempbc;
                 trainer.ReadBuffer(tempbc,sizeof(tbevelcut));
                 bevelouter:=tempbc;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 bevelwidth:=tempi;
                 trainer.ReadBuffer(tempbk,sizeof(tbevelkind));
                 bevelkind:=tempbk;
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          2: begin
               //tcheat
               with tcheat.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(cheatnr,sizeof(integer));
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 activationcolor:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 color:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 textcolor:=tempc;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 hotkeyleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 descriptionleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editwidth:=tempi;
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          3: begin
               //timage
               with timage.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(tempcursor,sizeof(tcursor));
                 cursor:=tempcursor;

                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 stretch:=tempb;
                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 transparent:=tempb;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 tag:=tempi;

                 trainer.ReadBuffer(temp,4);
                 if temp>0 then
                 begin
                   picture.Bitmap.LoadFromStream(trainer);
                 end;

                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          4: begin
               with tlabel.Create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 //caption
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 caption:=x;
                 freemem(x);

                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 font.Color:=tempc;
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;                 
               end;
             end;
          $ffffffff: break;
          else raise exception.Create(strunknowncomponent+Inttostr(temp));
        end;
      end;
    end;
  end;

  //fill in the list of cheats
  for i:=0 to length(frmMemoryModifier.trainerdata)-1 do
  begin
    frmMemoryModifier.recordview.Items.Add.caption:=frmMemoryModifier.trainerdata[i].description;
    frmMemoryModifier.recordview.Items[frmMemoryModifier.recordview.Items.count-1].SubItems.add(frmMemoryModifier.trainerdata[i].hotkeytext);
  end;

  frmMemoryTrainerPreview.UpdateScreen;
  if frmtrainerdesigner<>nil then frmtrainerdesigner.updatecheats

  //freemem(x);   *)
end;


procedure LoadTrainer1(trainer:tfilestream);
{type TcodeEntry1 = record
  address: dword;
  originalopcode: array of byte;
end;

type TAddressEntry1 = record
  address: dword;
  bit: byte;
  memtyp: integer;
  frozen: boolean;
  setvalue: boolean;
  userinput: boolean;
  value: string[50];
end;

type Ttrainerdata1 = record
  description: string;
  hotkeytext: string;
  hotkey: word;
  hotshift: word;
  hasedit: boolean;
  editvalue: string;

  codeentrys: array of TCodeEntry;
  addressentrys: array of TAddressEntry;
end;
 }
var temp: dword;
    tempb: boolean;
    tempc: tcolor;
    tempi: integer;
    tempbc: tbevelcut;
    (*tempbk: tbevelkind; *)
    tempcursor: tcursor;
    i,j:integer;
    x: pchar;

    image: pointer;
    laststate: word;
    lastshiftstate: word;
//    trainerdata1: array of TTrainerData1;
begin
  (*
  with frmMemoryModifier do
  begin
    //size of trainerdata
    trainer.ReadBuffer(temp,4);
    setlength(frmMemoryModifier.trainerdata,temp);

    for i:=0 to length(trainerdata)-1 do
    begin
      //description
      trainer.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainer.ReadBuffer(x^,temp);
      x[temp]:=#0;
      trainerdata[i].description:=x;
      freemem(x);

      //hotkeytext
      trainer.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainer.ReadBuffer(x^,temp);
      x[temp]:=#0;
      trainerdata[i].hotkeytext:=x;
      freemem(x);

      trainer.ReadBuffer(laststate,2);
      trainer.ReadBuffer(lastshiftstate,sizeof(lastshiftstate));
      ConvertOldHotkeyToKeyCombo(lastshiftstate, laststate, trainerdata[i].hotkey);


      trainer.ReadBuffer(temp,4);
      setlength(trainerdata[i].codeentrys,temp);

      //opcodes of this cheat
      for j:=0 to length(trainerdata[i].codeentrys)-1 do
      begin
        //address
        trainer.ReadBuffer(trainerdata[i].codeentrys[j].address,4);

        //original opcode
        trainer.ReadBuffer(temp,4);
        setlength(trainerdata[i].codeentrys[j].originalopcode,temp);
        trainer.ReadBuffer(pointer(trainerdata[i].codeentrys[j].originalopcode)^,temp);
      end;

      //address entrys
      trainer.ReadBuffer(temp,4);
      setlength(trainerdata[i].addressentrys,temp);
      for j:=0 to length(trainerdata[i].addressentrys)-1 do
      begin
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].address,sizeof(trainerdata[i].addressentrys[j].address));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].bit,sizeof(trainerdata[i].addressentrys[j].bit));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].memtyp,sizeof(trainerdata[i].addressentrys[j].memtyp));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].frozen,sizeof(trainerdata[i].addressentrys[j].frozen));
        trainerdata[i].addressentrys[j].frozendirection:=0;
        trainer.Readbuffer(trainerdata[i].addressentrys[j].setvalue,sizeof(trainerdata[i].addressentrys[j].setvalue));
        trainer.ReadBuffer(trainerdata[i].addressentrys[j].userinput,sizeof(trainerdata[i].addressentrys[j].userinput));
        trainer.Readbuffer(trainerdata[i].addressentrys[j].value,50);
        if trainerdata[i].addressentrys[j].userinput then
        begin
          trainerdata[i].hasedit:=true;
          trainerdata[i].editvalue:=trainerdata[i].addressentrys[j].value;
        end;

      end;
    end;

    //title
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edittitle.Text:=x;
    freemem(x);

    //launch filename
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edit2.Text:=x;
    freemem(x);

    //autolaunch
    trainer.ReadBuffer(tempb,sizeof(tempb));
    checkbox2.Checked:=tempb;

    //popup on keypress
    trainer.ReadBuffer(tempb,sizeof(tempb));
    checkbox1.Checked:=tempb;

    //process name
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    combobox1.Text:=x;
    freemem(x);

    //hotkeytext
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    edithotkey.Text:=x;
    freemem(x);

    //hotkey+shiftstate     (convert)
    trainer.ReadBuffer(laststate,2);
    trainer.ReadBuffer(lastshiftstate,sizeof(lastshiftstate));
    ConvertOldHotkeyToKeyCombo(lastshiftstate, laststate, popuphotkey);


    //abouttext
    trainer.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainer.ReadBuffer(x^,temp);
    x[temp]:=#0;
    memo1.Text:=x;
    freemem(x);

    trainer.ReadBuffer(temp,4);
    if temp=$666 then
    begin
      //default
      //leftside image
      trainer.ReadBuffer(temp,4);  //size of the image
      if temp>0 then
      begin
        //getmem(image,temp);
        //trainer.ReadBuffer(image^,temp);
        frmMemorytrainerpreview.Image1.Picture.Bitmap.LoadFromStream(trainer);
      end;

      //windowwidth
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Width));
      frmMemorytrainerpreview.Width:=temp;

      //windowheight
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.height));
      frmMemorytrainerpreview.height:=temp;

      //leftsidewidth
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Panel1.Width));
      frmMemorytrainerpreview.Panel1.Width:=temp;

      //leftsideheight
      trainer.readbuffer(temp,sizeof(frmMemorytrainerpreview.Panel1.height));
      frmMemorytrainerpreview.Panel1.height:=temp;
    end
    else
    begin
      //user defined
      frmMemoryModifier.dontshowdefault:=true;       //obsolete
      frmMemoryModifier.Button7.Click;

      //windowwidth
      trainer.readbuffer(temp,4);
      frmTrainerDesigner.Width:=temp;

      //windowheight
      trainer.readbuffer(temp,4);
      frmTrainerDesigner.height:=temp;


      while true do
      begin
        trainer.ReadBuffer(temp,4);
        case temp of
          0: begin
               //tbutton
               with tbutton.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 //caption
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 caption:=x;
                 freemem(x);

                 trainer.ReadBuffer(temp,sizeof(tag));
                 tag:=temp;
                 parent:=frmTrainerDesigner;

                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          1: begin
               //cheatlist
               with tcheatlist.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 activationcolor:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 color:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 textcolor:=tempc;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 hotkeyleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 descriptionleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editwidth:=tempi;

                 trainer.ReadBuffer(tempbc,sizeof(tbevelcut));
                 bevelinner:=tempbc;
                 trainer.ReadBuffer(tempbc,sizeof(tbevelcut));
                 bevelouter:=tempbc;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 bevelwidth:=tempi;
                 trainer.ReadBuffer(tempbk,sizeof(tbevelkind));
                 bevelkind:=tempbk;
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          2: begin
               //tcheat
               with tcheat.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(cheatnr,sizeof(integer));
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 activationcolor:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 color:=tempc;
                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 textcolor:=tempc;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 hotkeyleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 descriptionleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editleft:=tempi;
                 trainer.ReadBuffer(tempi,sizeof(integer));
                 editwidth:=tempi;
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          3: begin
               //timage
               with timage.create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 trainer.ReadBuffer(tempcursor,sizeof(tcursor));
                 cursor:=tempcursor;

                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 stretch:=tempb;
                 trainer.ReadBuffer(tempb,sizeof(tempb));
                 transparent:=tempb;

                 trainer.ReadBuffer(tempi,sizeof(integer));
                 tag:=tempi;

                 trainer.ReadBuffer(temp,4);
                 if temp>0 then
                 begin
                   picture.Bitmap.LoadFromStream(trainer);
                 end;

                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;
               end;
             end;

          4: begin
               with tlabel.Create(frmTrainerDesigner) do
               begin
                 trainer.ReadBuffer(temp,sizeof(integer));
                 left:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 top:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 width:=temp;
                 trainer.ReadBuffer(temp,sizeof(integer));
                 height:=temp;

                 //caption
                 trainer.ReadBuffer(temp,4);
                 getmem(x,temp+1);
                 trainer.ReadBuffer(x^,temp);
                 x[temp]:=#0;
                 caption:=x;
                 freemem(x);

                 trainer.ReadBuffer(tempc,sizeof(tcolor));
                 font.Color:=tempc;
                 parent:=frmTrainerDesigner;
                 onmousedown:=frmTrainerDesigner.MouseDown;
                 onmousemove:=frmTrainerDesigner.MouseMove;
                 onmouseup:=frmTrainerDesigner.MouseUp;                 
               end;
             end;
          $ffffffff: break;
          else raise exception.Create(strunknowncomponent+Inttostr(temp));
        end;
      end;
    end;
  end;

  //fill in the list of cheats
  for i:=0 to length(frmMemoryModifier.trainerdata)-1 do
  begin
    frmMemoryModifier.recordview.Items.Add.caption:=frmMemoryModifier.trainerdata[i].description;
    frmMemoryModifier.recordview.Items[frmMemoryModifier.recordview.Items.count-1].SubItems.add(frmMemoryModifier.trainerdata[i].hotkeytext);
  end;

  frmMemoryTrainerPreview.UpdateScreen;
  if frmtrainerdesigner<>nil then frmtrainerdesigner.updatecheats  

  //freemem(x);  *)
end;

resourcestring
  strCorruptIcon='The icon has been corrupted';
  strCantLoadFilepatcher='The file patcher can''t be loaded by Cheat Engine!';
  strNotACETrainer='This is not a trainer made by Cheat Engine (If it is a trainer at all!)';
  strUnknownTrainerVersion='This version of Cheat Engine doesn''t know how to read this trainer! Trainerversion=';
  strCantLoadProtectedfile='This trainer is protected from being opened by CE. Now go away!!!';
procedure LoadExe(filename: string);

var trainer: tfilestream;
    temp: dword;
    hi: hicon;
begin
  (*
  if frmMemoryModifier<>nil then
    frmMemoryModifier.Close;

  frmMemoryModifier:=TFrmMemoryModifier.create(nil);
  frmMemoryModifier.SaveDialog1.filename:=filename;
  frmMemoryModifier.show;

  //extract icon
  hi:=ExtractIcon(hinstance,pchar(filename),0);
  if (hi=0) or (hi=1) then
  begin
    hi:=0;
  end;

  frmMemoryModifier.Icon.Picture.Icon.Handle:=hi;
  frmMemoryTrainerPreview.Icon:=frmMemoryModifier.Icon.Picture.Icon;
  frmMemoryModifier.CurrentIcon.LoadFromHandle(hi);


  trainer:=tfilestream.Create(filename,fmopenread);
  try
    //load stuff from the trainer
    trainer.Position:=80;
    trainer.ReadBuffer(temp,4); //go to start of trainerstuff

    if temp>trainer.Size then raise exception.Create(strNotACETrainer);
    trainer.Position:=temp;

    //first check it is a suported trainer
    trainer.ReadBuffer(temp,4);
    if temp=$111111 then raise exception.Create(strCantLoadFilepatcher);
    if temp=$22322 then raise exception.Create(strCantLoadProtectedfile);
    if temp<>$22222 then raise exception.Create(strNotACETrainer);

    //trainerversion
    trainer.ReadBuffer(temp,4);
    case temp of
      1: LoadTrainer1(trainer);
      2: LoadTrainer2(trainer);
      3: LoadTrainer3(trainer);
      4: LoadTrainer4(trainer);
      5: LoadTrainer5(trainer);
      6: LoadTrainer6(trainer);
      7: LoadTrainer7(trainer);
      8: LoadTrainer8(trainer);
      else raise exception.Create(strUnknownTrainerVersion+IntToStr(temp));
    end;

    if frmTrainerDesigner<>nil then
      frmTrainerDesigner.Icon:=frmMemoryModifier.Icon.Picture.Icon;

  finally
    trainer.Free;
  end;
   *)
end;
{$endif}


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


procedure LoadXML(filename: string; merge: boolean);
var newrec: MemoryRecordV6;
    doc: TXMLDocument;
    CheatTable: TDOMNode;
    Entries, Codes, Symbols, Comments: TDOMNode;
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
  doc:=nil;
  try

    ReadXMLFile(doc, filename);

    CheatTable:=doc.FindNode('CheatTable');
    if CheatTable<>nil then
    begin
      Entries:=CheatTable.FindNode('CheatEntries');
      Codes:=CheatTable.FindNode('CheatCodes');
      Symbols:=CheatTable.FindNode('UserdefinedSymbols');
      Structures:=CheatTable.FindNode('Structures');
      Comments:=CheatTable.FindNode('Comments');
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
    begin
      Commentsunit.Comments.Memo1.Lines.Add(filename);
      Commentsunit.Comments.Memo1.Lines.Add('---');

      for i:=0 to comments.ChildNodes.Count-1 do
      begin
        if comments.ChildNodes[i].NodeName='Comment' then
          Commentsunit.Comments.Memo1.Lines.Add(comments.ChildNodes[i].TextContent);
      end;
    end;
  finally
    if doc<>nil then
      doc.free;
  end;

end;





procedure LoadV6(filename: string; ctfile: tfilestream;merge: boolean);
var newrec: MemoryRecordV6;
    records, subrecords, pointers: dword;
    i,j,k: integer;
    addrecord: boolean;
    temp:dword;
    tableversion: integer;

    x: pchar;
    nrofbytes:  byte;
    tempbefore: array of byte;
    tempactual: array of byte;
    tempafter: array of byte;
    tempaddress: dword;
    tempdescription,tempmodulename: string;
    tempoffset: dword;

    address: dword;
    symbolname: string;
    addressstring: string;
    li: tlistitem;
begin
  (*

    ctfile.ReadBuffer(records,4);

    for i:=0 to records-1 do
    begin
      //ctfile.ReadBuffer(newrec,sizeof(MemoryRecordV2));
      ctfile.ReadBuffer(j,sizeof(integer));
      getmem(x,j+1);
      ctfile.readbuffer(x^,j);
      x[j]:=#0;
      newrec.description:=x;
      freemem(x);

      newrec.Address:=0;
      ctfile.ReadBuffer(newrec.Address,sizeof(dword));

      //interpretableaddress
      ctfile.ReadBuffer(j,sizeof(integer));
      getmem(x,j+1);
      ctfile.readbuffer(x^,j);
      x[j]:=#0;
      newrec.interpretableaddress:=x;
      freemem(x);

      ctfile.ReadBuffer(newrec.VarType,sizeof(byte));
      ctfile.ReadBuffer(newrec.unicode,sizeof(byte));
      ctfile.ReadBuffer(newrec.Bit,sizeof(byte));
      ctfile.ReadBuffer(newrec.bitlength,sizeof(integer));
      ctfile.ReadBuffer(newrec.Group,sizeof(byte));
      ctfile.ReadBuffer(newrec.showashex,sizeof(boolean));
      ctfile.ReadBuffer(newrec.ispointer,sizeof(boolean));

      ctfile.ReadBuffer(temp,sizeof(temp));
      setlength(newrec.pointers,temp);

      for j:=0 to temp-1 do
      begin
        newrec.pointers[j].address:=0;
        ctfile.ReadBuffer(newrec.pointers[j].address,sizeof(dword));
        ctfile.ReadBuffer(newrec.pointers[j].offset,sizeof(integer));

        //interpretableaddress for pointer
        ctfile.ReadBuffer(k,sizeof(k));
        getmem(x,k+1);
        ctfile.readbuffer(x^,k);
        x[k]:=#0;
        newrec.pointers[j].interpretableaddress:=x;
        freemem(x);

      end;

      ctfile.ReadBuffer(j,sizeof(j));
      getmem(x,j+1);
      ctfile.readbuffer(x^,j);
      x[j]:=#0;
      newrec.autoassemblescript:=x;
      freemem(x);

      
      addrecord:=true;
      if merge then
        //find it in the current list, if it is in, dont add
        for j:=0 to mainform.NumberOfRecords-1 do
          if (mainform.memrec[j].Address=newrec.Address) and (mainform.memrec[j].VarType=newrec.VarType) then
          begin
            if (newrec.VarType=5) then
              if (newrec.Bit<>mainform.memrec[j].bit) or (newrec.bitlength<>mainform.memrec[j].bitlength) then continue;
            addrecord:=false;
            break;
          end;

      if addrecord then
      begin
        with mainform do
        begin
          inc(numberofrecords);
          reservemem;
          memrec[numberofrecords-1].Description:=newrec.Description;
          memrec[numberofrecords-1].Address:=newrec.Address;
          memrec[numberofrecords-1].interpretableaddress:=newrec.interpretableaddress;
          memrec[numberofrecords-1].VarType:=newrec.VarType;
          memrec[numberofrecords-1].unicode:=newrec.Unicode;
          memrec[numberofrecords-1].Group:=newrec.Group;
          memrec[numberofrecords-1].Bit:=newrec.Bit;
          memrec[numberofrecords-1].bitlength:=newrec.bitlength;
          memrec[numberofrecords-1].Frozen:=false;
          memrec[numberofrecords-1].FrozenValue:=0;
          memrec[numberofrecords-1].Frozendirection:=0;
          memrec[numberofrecords-1].ShowAsHex:=newrec.showashex;
          memrec[numberofrecords-1].autoassemblescript:=newrec.autoassemblescript;

          {$ifndef net} //no pointer handling for the client/server yet
          memrec[numberofrecords-1].IsPointer:=newrec.IsPointer;
          setlength(memrec[numberofrecords-1].pointers,length(newrec.pointers));
          for j:=0 to length(newrec.pointers)-1 do
          begin
            memrec[numberofrecords-1].pointers[j].Address:=newrec.pointers[j].Address;
            memrec[numberofrecords-1].pointers[j].offset:=newrec.pointers[j].offset;
            memrec[numberofrecords-1].pointers[j].Interpretableaddress:=newrec.pointers[j].Interpretableaddress;
          end;
          {$endif}
        end;
      end;
    end;


    ctfile.ReadBuffer(records,4);
    for i:=0 to records-1 do
    begin
      ctfile.ReadBuffer(tempaddress,4);

      ctfile.ReadBuffer(nrofbytes,1);
      getmem(x,nrofbytes+1);
      ctfile.ReadBuffer(pointer(x)^,nrofbytes);
      x[nrofbytes]:=#0;
      tempmodulename:=x;
      freemem(x);
      ctfile.ReadBuffer(tempoffset,4);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempbefore,nrofbytes);
      ctfile.ReadBuffer(pointer(tempbefore)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempactual,nrofbytes);
      ctfile.ReadBuffer(pointer(tempactual)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempafter,nrofbytes);
      ctfile.ReadBuffer(pointer(tempafter)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      getmem(x,nrofbytes+1);
      ctfile.ReadBuffer(pointer(x)^,nrofbytes);
      x[nrofbytes]:=#0;
      tempdescription:=x;
      freemem(x);

      addrecord:=true;

{      if merge then
      begin
        for j:=0 to advancedoptions.numberofcodes-1 do
          if (advancedoptions.addresses[j]=tempaddress) then
          begin
            addrecord:=false;
            break;
          end;

      end; }

      if addrecord then
      begin
        with advancedoptions do
        begin
          inc(numberofcodes);
          setlength(code,numberofcodes);

          setlength(code[numberofcodes-1].before,length(tempbefore));
          for k:=0 to length(tempbefore)-1 do
            code[numberofcodes-1].before[k]:=tempbefore[k];

          setlength(code[numberofcodes-1].actualopcode,length(tempactual));
          for k:=0 to length(tempactual)-1 do
            code[numberofcodes-1].actualopcode[k]:=tempactual[k];

          setlength(code[numberofcodes-1].after,length(tempafter));
          for k:=0 to length(tempafter)-1 do
            code[numberofcodes-1].after[k]:=tempafter[k];

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

    i:=ctfile.Position;
    ctfile.Position:=11;
    ctfile.ReadBuffer(tableversion,4);
    ctfile.Position:=i;

    if tableversion>=7 then
    begin
      //version 7 also contains some stuff about symbols
      ctfile.ReadBuffer(records,sizeof(dword));
      for i:=0 to records-1 do
      begin
        ctfile.ReadBuffer(address,sizeof(dword));
        ctfile.ReadBuffer(j,sizeof(integer));

        getmem(x,j+1);
        try
          ctfile.ReadBuffer(x^,j);
          x[j]:=#0;
          symbolname:=x;
        finally
          freemem(x);
        end;

        if tableversion>=9 then //version 9 adds the addressstring
        begin
          ctfile.ReadBuffer(j,sizeof(j));
          getmem(x,j+1);
          try
            ctfile.ReadBuffer(x^,j);
            x[j]:=#0;
            addressstring:=x;
          finally
            freemem(x);
          end;


          try
            symhandler.DeleteUserdefinedSymbol(symbolname);
            symhandler.AddUserdefinedSymbol(addressstring,symbolname);
          except

          end;
        end
        else
        begin
          //before version 9
          try
            symhandler.DeleteUserdefinedSymbol(symbolname);
            symhandler.AddUserdefinedSymbol(inttohex(address,8),symbolname);
          except

          end;
        end;
      end;
    end;

    if tableversion>=8 then
    begin
      //version 8 added structure data
      ctfile.ReadBuffer(records,4);
      setlength(definedstructures,records);
      for i:=0 to records-1 do
      begin
        ctfile.ReadBuffer(j,sizeof(j));
        getmem(x,j+1);
        ctfile.readbuffer(x^,j);
        x[j]:=#0;
        definedstructures[i].name:=x;
        freemem(x);

        ctfile.ReadBuffer(subrecords,4);
        setlength(definedstructures[i].structelement,subrecords);
        for j:=0 to subrecords-1 do
        begin
          ctfile.ReadBuffer(k,sizeof(k));
          getmem(x,k+1);
          ctfile.readbuffer(x^,k);
          x[k]:=#0;
          definedstructures[i].structelement[j].description:=x;
          freemem(x);

          ctfile.ReadBuffer(definedstructures[i].structelement[j].pointerto,sizeof(boolean));
          ctfile.ReadBuffer(definedstructures[i].structelement[j].pointertoSize,sizeof(dword));
          ctfile.ReadBuffer(definedstructures[i].structelement[j].structurenr,sizeof(integer));
          ctfile.ReadBuffer(definedstructures[i].structelement[j].bytesize,sizeof(dword));
          if tableversion>=9 then //version 9 added offsets
            ctfile.ReadBuffer(definedstructures[i].structelement[j].offset,sizeof(dword));

        end;
      end;
    end;

    //comments
    if merge then comments.Memo1.Lines.Add(filename);
    i:=ctfile.Size-ctfile.Position;
    getmem(x,i+1);
    ctfile.readbuffer(x^,i);

    x[i]:=chr(0);
    comments.Memo1.Text:=comments.Memo1.Text+x;

    freemem(x);

  end;  *)
end;


procedure LoadV5(filename: string; ctfile: tfilestream;merge: boolean);
var newrec: MemoryRecordV5;
    records,pointers: dword;
    i,j,k: integer;
    addrecord: boolean;
    temp:dword;

    x: pchar;
    nrofbytes:  byte;
    tempbefore: array of byte;
    tempactual: array of byte;
    tempafter: array of byte;
    tempaddress: dword;
    tempdescription: string;
    li: tlistitem;
begin
  (*
    ctfile.ReadBuffer(records,4);
    for i:=0 to records-1 do
    begin
      //ctfile.ReadBuffer(newrec,sizeof(MemoryRecordV2));
      ctfile.ReadBuffer(j,sizeof(j));
      getmem(x,j+1);
      ctfile.readbuffer(x^,j);
      x[j]:=#0;
      newrec.description:=x;
      freemem(x);
      
      ctfile.ReadBuffer(newrec.Address,sizeof(newrec.Address));
      ctfile.ReadBuffer(newrec.VarType,sizeof(newrec.VarType));
      ctfile.ReadBuffer(newrec.unicode,sizeof(newrec.VarType));
      ctfile.ReadBuffer(newrec.Bit,sizeof(newrec.Bit));
      ctfile.ReadBuffer(newrec.bitlength,sizeof(newrec.bitlength));
      ctfile.ReadBuffer(newrec.Group,sizeof(newrec.Group));
      ctfile.ReadBuffer(newrec.showashex,sizeof(newrec.showashex));
      ctfile.ReadBuffer(newrec.ispointer,sizeof(newrec.ispointer));

      ctfile.ReadBuffer(temp,sizeof(temp));
      setlength(newrec.pointers,temp);

      for j:=0 to temp-1 do
      begin
        ctfile.ReadBuffer(newrec.pointers[j].address,sizeof(newrec.pointers[j].address));
        ctfile.ReadBuffer(newrec.pointers[j].offset,sizeof(newrec.pointers[j].offset));
      end;

      addrecord:=true;
      if merge then
        //find it in the current list, if it is in, dont add
        for j:=0 to mainform.NumberOfRecords-1 do
          if (mainform.memrec[j].Address=newrec.Address) and (mainform.memrec[j].VarType=newrec.VarType) then
          begin
            if (newrec.VarType=5) then
              if (newrec.Bit<>mainform.memrec[j].bit) or (newrec.bitlength<>mainform.memrec[j].bitlength) then continue;
            addrecord:=false;
            break;
          end;

      if addrecord then
      begin
        with mainform do
        begin
          inc(numberofrecords);
          reservemem;
          memrec[numberofrecords-1].Description:=newrec.Description;
          memrec[numberofrecords-1].Address:=newrec.Address;
          memrec[numberofrecords-1].VarType:=newrec.VarType;
          memrec[numberofrecords-1].unicode:=newrec.Unicode;
          memrec[numberofrecords-1].Group:=newrec.Group;
          memrec[numberofrecords-1].Bit:=newrec.Bit;
          memrec[numberofrecords-1].bitlength:=newrec.bitlength;
          memrec[numberofrecords-1].Frozen:=false;
          memrec[numberofrecords-1].FrozenValue:=0;
          memrec[numberofrecords-1].Frozendirection:=0;
          memrec[numberofrecords-1].ShowAsHex:=newrec.showashex;

          {$ifndef net} //no pointer handling for the client/server yet
          memrec[numberofrecords-1].IsPointer:=newrec.IsPointer;
          setlength(memrec[numberofrecords-1].pointers,length(newrec.pointers));
          for j:=0 to length(newrec.pointers)-1 do
          begin
            memrec[numberofrecords-1].pointers[j].Address:=newrec.pointers[j].Address;
            memrec[numberofrecords-1].pointers[j].offset:=newrec.pointers[j].offset;
          end;
          {$endif}
        end;
      end;
    end;


    ctfile.ReadBuffer(records,4);
    for i:=0 to records-1 do
    begin
      ctfile.ReadBuffer(tempaddress,4);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempbefore,nrofbytes);
      ctfile.ReadBuffer(pointer(tempbefore)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempactual,nrofbytes);
      ctfile.ReadBuffer(pointer(tempactual)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempafter,nrofbytes);
      ctfile.ReadBuffer(pointer(tempafter)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      getmem(x,nrofbytes+1);
      ctfile.ReadBuffer(pointer(x)^,nrofbytes);
      x[nrofbytes]:=#0;
      tempdescription:=x;
      freemem(x);

      addrecord:=true;

{      if merge then
      begin
        for j:=0 to advancedoptions.numberofcodes-1 do
          if (advancedoptions.addresses[j]=tempaddress) then
          begin
            addrecord:=false;
            break;
          end;

      end; }

      if addrecord then
      begin
        with advancedoptions do
        begin
          inc(numberofcodes);
          setlength(code,numberofcodes);

          setlength(code[numberofcodes-1].before,length(tempbefore));
          for k:=0 to length(tempbefore)-1 do
            code[numberofcodes-1].before[k]:=tempbefore[k];

          setlength(code[numberofcodes-1].actualopcode,length(tempactual));
          for k:=0 to length(tempactual)-1 do
            code[numberofcodes-1].actualopcode[k]:=tempactual[k];

          setlength(code[numberofcodes-1].after,length(tempafter));
          for k:=0 to length(tempafter)-1 do
            code[numberofcodes-1].after[k]:=tempafter[k];

          code[numberofcodes-1].Address:=tempaddress;

          li:=codelist2.Items.Add;
          if code[numberofcodes-1].modulename<>'' then
            li.Caption:=code[numberofcodes-1].modulename+'+'+inttohex(code[numberofcodes-1].offset,1)
          else
            li.Caption:=inttohex(tempaddress,8);

          li.SubItems.Add(tempdescription);
        end;
      end;

    end;

    //comments
    if merge then comments.Memo1.Lines.Add(filename);
    i:=ctfile.Size-ctfile.Position;
    getmem(x,i+1);
    ctfile.readbuffer(x^,i);

    x[i]:=chr(0);
    comments.Memo1.Text:=comments.Memo1.Text+x;

    freemem(x);
    *)
end;

procedure LoadV4(filename: string; ctfile: tfilestream;merge: boolean);
var newrec: MemoryRecordV4;
    records,pointers: dword;
    i,j,k: integer;
    addrecord: boolean;
    temp:dword;

    x: pchar;
    nrofbytes:  byte;
    tempbefore: array of byte;
    tempactual: array of byte;
    tempafter: array of byte;
    tempaddress: dword;
    tempdescription: string;
    li: tlistitem;
begin
  (*
    ctfile.ReadBuffer(records,4);
    for i:=0 to records-1 do
    begin
      //ctfile.ReadBuffer(newrec,sizeof(MemoryRecordV2));

      ctfile.ReadBuffer(newrec.Description,sizeof(newrec.Description));
      ctfile.ReadBuffer(newrec.Address,sizeof(newrec.Address));
      ctfile.ReadBuffer(newrec.VarType,sizeof(newrec.VarType));
      ctfile.ReadBuffer(newrec.Bit,sizeof(newrec.Bit));
      ctfile.ReadBuffer(newrec.bitlength,sizeof(newrec.bitlength));
      ctfile.ReadBuffer(newrec.Group,sizeof(newrec.Group));
      ctfile.ReadBuffer(newrec.showashex,sizeof(newrec.showashex));
      ctfile.ReadBuffer(newrec.ispointer,sizeof(newrec.ispointer));

      ctfile.ReadBuffer(temp,sizeof(temp));
      setlength(newrec.pointers,temp);

      for j:=0 to temp-1 do
      begin
        ctfile.ReadBuffer(newrec.pointers[j].address,sizeof(newrec.pointers[j].address));
        ctfile.ReadBuffer(newrec.pointers[j].offset,sizeof(newrec.pointers[j].offset));
      end;

      addrecord:=true;
      //goodbye merge, just add

      if addrecord then
      begin
        with mainform do
        begin
          inc(numberofrecords);
          reservemem;
          memrec[numberofrecords-1].Description:=newrec.Description;
          memrec[numberofrecords-1].Address:=newrec.Address;
          memrec[numberofrecords-1].VarType:=newrec.VarType;
          memrec[numberofrecords-1].unicode:=false;
          memrec[numberofrecords-1].Group:=newrec.Group;
          memrec[numberofrecords-1].Bit:=newrec.Bit;
          memrec[numberofrecords-1].bitlength:=newrec.bitlength;
          memrec[numberofrecords-1].Frozen:=false;
          memrec[numberofrecords-1].FrozenValue:=0;
          memrec[numberofrecords-1].Frozendirection:=0;
          memrec[numberofrecords-1].ShowAsHex:=newrec.showashex;

          {$ifndef net} //no pointer handling for the client/server yet
          memrec[numberofrecords-1].IsPointer:=newrec.IsPointer;
          setlength(memrec[numberofrecords-1].pointers,length(newrec.pointers));
          for j:=0 to length(newrec.pointers)-1 do
          begin
            memrec[numberofrecords-1].pointers[j].Address:=newrec.pointers[j].Address;
            memrec[numberofrecords-1].pointers[j].offset:=newrec.pointers[j].offset;
          end;
          {$endif}
        end;
      end;
    end;


    ctfile.ReadBuffer(records,4);
    for i:=0 to records-1 do
    begin
      ctfile.ReadBuffer(tempaddress,4);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempbefore,nrofbytes);
      ctfile.ReadBuffer(pointer(tempbefore)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempactual,nrofbytes);
      ctfile.ReadBuffer(pointer(tempactual)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempafter,nrofbytes);
      ctfile.ReadBuffer(pointer(tempafter)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      getmem(x,nrofbytes+1);
      ctfile.ReadBuffer(pointer(x)^,nrofbytes);
      x[nrofbytes]:=#0;
      tempdescription:=x;
      freemem(x);

      addrecord:=true;

      if merge then
      begin
        for j:=0 to advancedoptions.numberofcodes-1 do
          if (advancedoptions.code[j].Address=tempaddress) then
          begin
            addrecord:=false;
            break;
          end;

      end;

      if addrecord then
      begin
        with advancedoptions do
        begin
          inc(numberofcodes);
          setlength(code,numberofcodes);

          setlength(code[numberofcodes-1].before,length(tempbefore));
          for k:=0 to length(tempbefore)-1 do
            code[numberofcodes-1].before[k]:=tempbefore[k];

          setlength(code[numberofcodes-1].actualopcode,length(tempactual));
          for k:=0 to length(tempactual)-1 do
            code[numberofcodes-1].actualopcode[k]:=tempactual[k];

          setlength(code[numberofcodes-1].after,length(tempafter));
          for k:=0 to length(tempafter)-1 do
            code[numberofcodes-1].after[k]:=tempafter[k];

          code[numberofcodes-1].Address:=tempaddress;

          li:=codelist2.Items.Add;
          if code[numberofcodes-1].modulename<>'' then
            li.Caption:=code[numberofcodes-1].modulename+'+'+inttohex(code[numberofcodes-1].offset,1)
          else
            li.Caption:=inttohex(tempaddress,8);

          li.SubItems.Add(tempdescription);
        end;
      end;

    end;

    //comments
    if merge then comments.Memo1.Lines.Add(filename);
    i:=ctfile.Size-ctfile.Position;
    getmem(x,i+1);
    ctfile.readbuffer(x^,i);

    x[i]:=chr(0);
    comments.Memo1.Text:=comments.Memo1.Text+x;

    freemem(x);
    *)
end;


procedure LoadV3(filename: string; ctfile: tfilestream;merge: boolean);
var newrec: MemoryRecordV3;
    records,pointers: dword;
    i,j,k: integer;
    addrecord: boolean;
    temp:dword;

    x: pchar;
    nrofbytes:  byte;
    tempbefore: array of byte;
    tempactual: array of byte;
    tempafter: array of byte;
    tempaddress: dword;
    tempdescription: string;
    li: tlistitem;
begin
  (*
    ctfile.ReadBuffer(records,4);
    for i:=0 to records-1 do
    begin
      //ctfile.ReadBuffer(newrec,sizeof(MemoryRecordV2));

      ctfile.ReadBuffer(newrec.Description,sizeof(newrec.Description));
      ctfile.ReadBuffer(newrec.Address,sizeof(newrec.Address));
      ctfile.ReadBuffer(newrec.VarType,sizeof(newrec.VarType));
      ctfile.ReadBuffer(newrec.Bit,sizeof(newrec.Bit));
      ctfile.ReadBuffer(newrec.bitlength,sizeof(newrec.bitlength));
      ctfile.ReadBuffer(newrec.Group,sizeof(newrec.Group));
      ctfile.ReadBuffer(newrec.ispointer,sizeof(newrec.ispointer));

      ctfile.ReadBuffer(temp,sizeof(temp));
      setlength(newrec.pointers,temp);

      for j:=0 to temp-1 do
      begin
        ctfile.ReadBuffer(newrec.pointers[j].address,sizeof(newrec.pointers[j].address));
        ctfile.ReadBuffer(newrec.pointers[j].offset,sizeof(newrec.pointers[j].offset));
      end;

      addrecord:=true;
      if merge then
        //find it in the current list, if it is in, dont add
        for j:=0 to mainform.NumberOfRecords-1 do
          if (mainform.memrec[j].Address=newrec.Address) and (mainform.memrec[j].VarType=newrec.VarType) then
          begin
            if (newrec.VarType=5) then
              if (newrec.Bit<>mainform.memrec[j].bit) or (newrec.bitlength<>mainform.memrec[j].bitlength) then continue;
            addrecord:=false;
            break;
          end;

      if addrecord then
      begin
        with mainform do
        begin
          inc(numberofrecords);
          reservemem;
          memrec[numberofrecords-1].Description:=newrec.Description;
          memrec[numberofrecords-1].Address:=newrec.Address;
          memrec[numberofrecords-1].VarType:=newrec.VarType;
          memrec[numberofrecords-1].unicode:=false;
          memrec[numberofrecords-1].Bit:=newrec.Bit;
          memrec[numberofrecords-1].bitlength:=newrec.bitlength;
          memrec[numberofrecords-1].Frozen:=false;
          memrec[numberofrecords-1].FrozenValue:=0;
          memrec[numberofrecords-1].Frozendirection:=0;
          memrec[numberofrecords-1].ShowAsHex:=false;

          {$ifndef net}
          memrec[numberofrecords-1].IsPointer:=newrec.IsPointer;
          setlength(memrec[numberofrecords-1].pointers,length(newrec.pointers));
          for j:=0 to length(newrec.pointers)-1 do
          begin
            memrec[numberofrecords-1].pointers[j].Address:=newrec.pointers[j].Address;
            memrec[numberofrecords-1].pointers[j].offset:=newrec.pointers[j].offset;
          end;
          {$endif}
        end;
      end;
    end;


    ctfile.ReadBuffer(records,4);
    for i:=0 to records-1 do
    begin
      ctfile.ReadBuffer(tempaddress,4);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempbefore,nrofbytes);
      ctfile.ReadBuffer(pointer(tempbefore)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempactual,nrofbytes);
      ctfile.ReadBuffer(pointer(tempactual)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempafter,nrofbytes);
      ctfile.ReadBuffer(pointer(tempafter)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      getmem(x,nrofbytes+1);
      ctfile.ReadBuffer(pointer(x)^,nrofbytes);
      x[nrofbytes]:=#0;
      tempdescription:=x;
      freemem(x);

      addrecord:=true;

      if merge then
      begin
        for j:=0 to advancedoptions.numberofcodes-1 do
          if (advancedoptions.code[j].Address=tempaddress) then
          begin
            addrecord:=false;
            break;
          end;

      end;

      if addrecord then
      begin
        with advancedoptions do
        begin
          inc(numberofcodes);
          setlength(code,numberofcodes);

          setlength(code[numberofcodes-1].before,length(tempbefore));
          for k:=0 to length(tempbefore)-1 do
            code[numberofcodes-1].before[k]:=tempbefore[k];

          setlength(code[numberofcodes-1].actualopcode,length(tempactual));
          for k:=0 to length(tempactual)-1 do
            code[numberofcodes-1].actualopcode[k]:=tempactual[k];

          setlength(code[numberofcodes-1].after,length(tempafter));
          for k:=0 to length(tempafter)-1 do
            code[numberofcodes-1].after[k]:=tempafter[k];

          code[numberofcodes-1].Address:=tempaddress;

          li:=codelist2.Items.Add;
          if code[numberofcodes-1].modulename<>'' then
            li.Caption:=code[numberofcodes-1].modulename+'+'+inttohex(code[numberofcodes-1].offset,1)
          else
            li.Caption:=inttohex(tempaddress,8);

          li.SubItems.Add(tempdescription);
        end;
      end;

    end;

    //comments
    if merge then comments.Memo1.Lines.Add(filename);
    i:=ctfile.Size-ctfile.Position;
    getmem(x,i+1);
    ctfile.readbuffer(x^,i);

    x[i]:=chr(0);
    comments.Memo1.Text:=comments.Memo1.Text+x;

    freemem(x);
    *)
end;


procedure LoadV2(filename: string; ctfile: tfilestream;merge: boolean);
var newrec: MemoryRecordV2;
    records: dword;
    i,j,k: integer;
    addrecord: boolean;

    x: pchar;
    nrofbytes:  byte;
    tempbefore: array of byte;
    tempactual: array of byte;
    tempafter: array of byte;
    tempaddress: dword;
    tempdescription: string;
    li: tlistitem;
begin
  (*
    ctfile.ReadBuffer(records,4);
    for i:=0 to records-1 do
    begin
      ctfile.ReadBuffer(newrec,sizeof(MemoryRecordV2));

      addrecord:=true;
      if merge then
        //find it in the current list, if it is in, dont add
        for j:=0 to mainform.NumberOfRecords-1 do
          if (mainform.memrec[j].Address=newrec.Address) and (mainform.memrec[j].VarType=newrec.VarType) then
          begin
            if (newrec.VarType=5) then
              if (newrec.Bit<>mainform.memrec[j].bit) or (newrec.bitlength<>mainform.memrec[j].bitlength) then continue;
            addrecord:=false;
            break;
          end;

      if addrecord then
      begin
        with mainform do
        begin
          
          inc(numberofrecords);
          reservemem;
          memrec[numberofrecords-1].Description:=newrec.Description;
          memrec[numberofrecords-1].Address:=newrec.Address;
          memrec[numberofrecords-1].VarType:=newrec.VarType;
          memrec[numberofrecords-1].unicode:=false;
          memrec[numberofrecords-1].Bit:=newrec.Bit;
          memrec[numberofrecords-1].bitlength:=newrec.bitlength;
          memrec[numberofrecords-1].Frozen:=false;
          memrec[numberofrecords-1].FrozenValue:=0;
          memrec[numberofrecords-1].Frozendirection:=0;
          memrec[numberofrecords-1].ShowAsHex:=false;
        end;
      end;
    end;


    ctfile.ReadBuffer(records,4);
    for i:=0 to records-1 do
    begin
      ctfile.ReadBuffer(tempaddress,4);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempbefore,nrofbytes);
      ctfile.ReadBuffer(pointer(tempbefore)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempactual,nrofbytes);
      ctfile.ReadBuffer(pointer(tempactual)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempafter,nrofbytes);
      ctfile.ReadBuffer(pointer(tempafter)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      getmem(x,nrofbytes+1);
      ctfile.ReadBuffer(pointer(x)^,nrofbytes);
      x[nrofbytes]:=#0;
      tempdescription:=x;
      freemem(x);

      addrecord:=true;

      if merge then
      begin
        for j:=0 to advancedoptions.numberofcodes-1 do
          if (advancedoptions.code[j].Address=tempaddress) then
          begin
            addrecord:=false;
            break;
          end;

      end;

      if addrecord then
      begin
        with advancedoptions do
        begin
          inc(numberofcodes);
          setlength(code,numberofcodes);

          setlength(code[numberofcodes-1].before,length(tempbefore));
          for k:=0 to length(tempbefore)-1 do
            code[numberofcodes-1].before[k]:=tempbefore[k];

          setlength(code[numberofcodes-1].actualopcode,length(tempactual));
          for k:=0 to length(tempactual)-1 do
            code[numberofcodes-1].actualopcode[k]:=tempactual[k];

          setlength(code[numberofcodes-1].after,length(tempafter));
          for k:=0 to length(tempafter)-1 do
            code[numberofcodes-1].after[k]:=tempafter[k];

          code[numberofcodes-1].Address:=tempaddress;

          li:=codelist2.Items.Add;
          if code[numberofcodes-1].modulename<>'' then
            li.Caption:=code[numberofcodes-1].modulename+'+'+inttohex(code[numberofcodes-1].offset,1)
          else
            li.Caption:=inttohex(tempaddress,8);

          li.SubItems.Add(tempdescription);
        end;
      end;

    end;

    //comments
    if merge then comments.Memo1.Lines.Add(filename);
    i:=ctfile.Size-ctfile.Position;
    getmem(x,i+1);
    ctfile.readbuffer(x^,i);

    x[i]:=chr(0);
    comments.Memo1.Text:=comments.Memo1.Text+x;

    freemem(x);
    *)
end;

procedure LoadV1(filename: string; ctfile: tfilestream;merge: boolean);
var newrec: MemoryRecordV1;
    records: dword;
    i,j,k: integer;
    addrecord: boolean;

    x: pchar;
    nrofbytes:  byte;
    tempbefore: array of byte;
    tempactual: array of byte;
    tempafter: array of byte;
    tempaddress: dword;
    tempdescription: string;
    li: tlistitem;
begin
  (*
    ctfile.ReadBuffer(records,4);
    for i:=0 to records-1 do
    begin
      ctfile.ReadBuffer(newrec,sizeof(MemoryRecordV1));

      addrecord:=true;
      if merge then
        //find it in the current list, if it is in, dont add
        for j:=0 to mainform.NumberOfRecords-1 do
          if (mainform.memrec[j].Address=newrec.Address) and (mainform.memrec[j].VarType=newrec.VarType) then
          begin
            if (newrec.VarType=5) then
              if (newrec.Bit<>mainform.memrec[j].bit) or (newrec.bitlength<>mainform.memrec[j].bitlength) then continue;
            addrecord:=false;
            break;
          end;

      if addrecord then
      begin
        with mainform do
        begin
          inc(numberofrecords);
          reservemem;
          memrec[numberofrecords-1].Description:=newrec.Description;
          memrec[numberofrecords-1].Address:=newrec.Address;
          memrec[numberofrecords-1].VarType:=newrec.VarType;
          memrec[numberofrecords-1].unicode:=false;
          memrec[numberofrecords-1].Bit:=newrec.Bit;
          memrec[numberofrecords-1].bitlength:=newrec.bitlength;
          memrec[numberofrecords-1].Frozen:=false;
          memrec[numberofrecords-1].FrozenValue:=0;
          memrec[numberofrecords-1].Frozendirection:=0;
          memrec[numberofrecords-1].ShowAsHex:=false;
        end;
      end;
    end;


    ctfile.ReadBuffer(records,4);
    for i:=0 to records-1 do
    begin
      ctfile.ReadBuffer(tempaddress,4);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempbefore,nrofbytes);
      ctfile.ReadBuffer(pointer(tempbefore)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempactual,nrofbytes);
      ctfile.ReadBuffer(pointer(tempactual)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      setlength(tempafter,nrofbytes);
      ctfile.ReadBuffer(pointer(tempafter)^,nrofbytes);

      ctfile.ReadBuffer(nrofbytes,1);
      getmem(x,nrofbytes+1);
      ctfile.ReadBuffer(pointer(x)^,nrofbytes);
      x[nrofbytes]:=#0;
      tempdescription:=x;
      freemem(x);

      addrecord:=true;

      if merge then
      begin
        for j:=0 to advancedoptions.numberofcodes-1 do
          if (advancedoptions.code[j].Address=tempaddress) then
          begin
            addrecord:=false;
            break;
          end;

      end;

      if addrecord then
      begin
        with advancedoptions do
        begin
          inc(numberofcodes);
          setlength(code,numberofcodes);

          setlength(code[numberofcodes-1].before,length(tempbefore));
          for k:=0 to length(tempbefore)-1 do
            code[numberofcodes-1].before[k]:=tempbefore[k];

          setlength(code[numberofcodes-1].actualopcode,length(tempactual));
          for k:=0 to length(tempactual)-1 do
            code[numberofcodes-1].actualopcode[k]:=tempactual[k];

          setlength(code[numberofcodes-1].after,length(tempafter));
          for k:=0 to length(tempafter)-1 do
            code[numberofcodes-1].after[k]:=tempafter[k];

          code[numberofcodes-1].Address:=tempaddress;

          li:=codelist2.Items.Add;
          if code[numberofcodes-1].modulename<>'' then
            li.Caption:=code[numberofcodes-1].modulename+'+'+inttohex(code[numberofcodes-1].offset,1)
          else
            li.Caption:=inttohex(tempaddress,8);

          li.SubItems.Add(tempdescription);
        end;
      end;

    end;

    //comments
    if merge then comments.Memo1.Lines.Add(filename);
    i:=ctfile.Size-ctfile.Position;
    getmem(x,i+1);
    ctfile.readbuffer(x^,i);

    x[i]:=chr(0);
    comments.Memo1.Text:=comments.Memo1.Text+x;

    freemem(x);
    *)
end;

procedure SaveCEM(Filename:string;address,size:dword);
var memfile: TFilestream;
    buf: pointer;
    temp:dword;
begin
  memfile:=Tfilestream.Create(filename,fmCreate);
  buf:=nil;
  try
    getmem(buf,size);
    if readprocessmemory(processhandle,pointer(address),buf,size,temp) then
    begin
      memfile.WriteBuffer(pchar('CHEATENGINE')^,11);
      temp:=1; //version
      memfile.WriteBuffer(temp,4);
      memfile.WriteBuffer(address,4);
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
begin
  ctfile:=nil;
  ctfile:=Tfilestream.Create(filename,fmopenread);
  try
    x:=nil;
    getmem(x,12);
    ctfile.ReadBuffer(x^,11);
    x[11]:=#0;  //write a 0 terminator

    if x<>'CHEATENGINE' then
    begin
      if x[0]='<' then //xml ? (in case of ce6.0+)
      begin
        freeandnil(ctfile);

        try
          LoadXML(filename, merge);
        except
          raise exception.Create('This is not a valid cheat table');
        end;
        exit;
      end;

      if messagedlg('This is NOT a valid Cheat Engine table. Are you sure you want to load it?',mtWarning, [mbyes,mbno],0) = mrno then exit;
    end;

    //still here, so an older version of cheat engine (predating 6.0)
    ctfile.ReadBuffer(version,4);
    if version>CurrentTableVersion then
      raise exception.Create('This table was made with a newer version of Cheat Engine and isn''t supported. Download the latest version from the Cheat Engine website');

    //now load the table loader for each supported version
    case version of
      1: LoadV1(filename,ctfile,merge);
      2: LoadV2(filename,ctfile,merge);
      3: LoadV3(filename,ctfile,merge);
      4: LoadV4(filename,ctfile,merge);
      5: LoadV5(filename,ctfile,merge);
      6,7,8,9: LoadV6(filename,ctfile,merge);
      else raise exception.Create('This table was made with a version of Cheat Engine that isn''t supported anymore! (The table is propably messed up)');
    end;

    //see if there are filename.m* files
    zeromemory(@f,sizeof(f));
    if findfirst(filename+'.m*',faAnyFile,f)=0 then
    begin
      if messagedlg('Some memory files where detected for this table. Do you want to load them now?',mtConfirmation,[mbyes,mbno],0)=mryes then
      begin
        loadcem(extractfilepath(filename)+f.Name);
        while findnext(f)=0 do loadcem(extractfilepath(filename)+f.Name);
      end;
    end;
  finally
    if x<>nil then
      freemem(x);

    if ctfile<>nil then
      ctfile.free;
  end;


end;

procedure LoadCT3(filename: string; merge: boolean);
var loadfile: File;
    i,j,k: integer;
    actualread: dword;
    Controle: String[6];
    records:  dword;
    ct3rec:   MemoryRecordcet3;

    addrecord: boolean;
    x: pchar;

    nrofbytes:  byte;
    tempbefore: array of byte;
    tempactual: array of byte;
    tempafter: array of byte;
    tempaddress: dword;
    tempdescription: string;
    li: tlistitem;
begin
  (*
  assignfile(LoadFile,filename);
  reset(LoadFile,1);


  //check version
  blockread(LoadFile,controle,sizeof(controle),actualread);
  if controle<>'CET3' then
  begin
    closefile(LoadFile);
    Raise Exception.Create('Invalid Cheat Engine table');
  end;

  blockread(Loadfile,records,4,actualread);

  i:=0;
  actualread:=sizeof(MemoryRecordCET3);
  while (i<records) and (actualread=sizeof(MemoryRecordCET3)) do
  begin
    blockread(LoadFile,ct3rec,sizeof(MemoryRecordCET3),actualread);
    if actualread=sizeof(MemoryRecordCET3) then
    begin
      addrecord:=true;
      if merge then
        //find it in the current list, if it is in, dont add
        for j:=0 to mainform.NumberOfRecords-1 do
          if (mainform.memrec[j].Address=ct3rec.Address) and (mainform.memrec[j].VarType=ct3rec.VarType) then
          begin
            if (ct3rec.VarType=5) then
              if (ct3rec.Bit<>mainform.memrec[j].bit) then continue;

            addrecord:=false;
            break;
          end;

      if addrecord then
      begin
        with mainform do
        begin
          inc(numberofrecords);
          reservemem;
          memrec[numberofrecords-1].Description:=ct3rec.Description;
          memrec[numberofrecords-1].Address:=ct3rec.Address;
          memrec[numberofrecords-1].VarType:=ct3rec.VarType;
          memrec[numberofrecords-1].Bit:=ct3rec.Bit;
          memrec[numberofrecords-1].bitlength:=1;
          memrec[numberofrecords-1].Frozen:=false;
          memrec[numberofrecords-1].FrozenValue:=0;
        end;
      end;


    end;

    inc(i);
  end;

  blockread(LoadFile,controle,sizeof(controle),actualread);
  if (actualread>0) then
  begin
    if (controle<>'CET3AA') then
    begin
      //old version of CT3
      if merge then comments.Memo1.Lines.Add(filename);

      seek(loadfile,filepos(loadfile)-sizeof(controle));
      i:=filesize(loadfile)-filepos(Loadfile);
      getmem(x,i+1);
      blockread(loadfile,x^,i,actualread);

      x[i]:=chr(0);
      comments.Memo1.Text:=comments.Memo1.Text+x;
      freemem(x);

      closefile(loadfile);
      exit;
    end;



    blockread(Loadfile,records,4,actualread);

    for i:=0 to records-1 do
    begin
      blockread(loadfile,nrofbytes,1,actualread);
      setlength(tempbefore,nrofbytes);
      blockread(loadfile,pointer(tempbefore)^,nrofbytes,actualread);

      blockread(loadfile,nrofbytes,1,actualread);
      setlength(tempactual,nrofbytes);
      blockread(loadfile,pointer(tempactual)^,nrofbytes,actualread);

      blockread(loadfile,nrofbytes,1,actualread);
      setlength(tempafter,nrofbytes);
      blockread(loadfile,pointer(tempafter)^,nrofbytes,actualread);

      blockread(loadfile,nrofbytes,1,actualread);
      getmem(x,nrofbytes+1);
      blockread(loadfile,pointer(x)^,nrofbytes,actualread);
      x[nrofbytes]:=#0;
      tempdescription:=x;
      freemem(x);

      blockread(loadfile,tempaddress,4,actualread);

      addrecord:=true;

      if merge then
      begin
        for j:=0 to advancedoptions.numberofcodes-1 do
          if (advancedoptions.code[j].Address=tempaddress) then
          begin
            addrecord:=false;
            break;
          end;

      end;

      if addrecord then
      begin
        with advancedoptions do
        begin
          inc(numberofcodes);
          setlength(code,numberofcodes);

          setlength(code[numberofcodes-1].before,length(tempbefore));
          for k:=0 to length(tempbefore)-1 do
            code[numberofcodes-1].before[k]:=tempbefore[k];

          setlength(code[numberofcodes-1].actualopcode,length(tempactual));
          for k:=0 to length(tempactual)-1 do
            code[numberofcodes-1].actualopcode[k]:=tempactual[k];

          setlength(code[numberofcodes-1].after,length(tempafter));
          for k:=0 to length(tempafter)-1 do
            code[numberofcodes-1].after[k]:=tempafter[k];

          code[numberofcodes-1].Address:=tempaddress;

          li:=codelist2.Items.Add;
          if code[numberofcodes-1].modulename<>'' then
            li.Caption:=code[numberofcodes-1].modulename+'+'+inttohex(code[numberofcodes-1].offset,1)
          else
            li.Caption:=inttohex(tempaddress,8);

          li.SubItems.Add(tempdescription);
        end;
      end;
    end;

    advancedoptions.numberofcodes:=records;

    //comments
    if merge then comments.Memo1.Lines.Add(filename);
    i:=filesize(loadfile)-filepos(Loadfile);
    getmem(x,i+1);
    blockread(loadfile,x^,i,actualread);

    x[i]:=chr(0);
    comments.Memo1.Text:=comments.Memo1.Text+x;

    freemem(x);

  end;

  closefile(loadfile);
  *)

end;

procedure LoadCT2(filename: string; merge: boolean);
var loadfile: File;
    actualread: dword;
    oldrec: MemoryrecordOld;
    i,j: integer;
    addrecord: boolean;
begin
  (*
  assignfile(LoadFile,filename);
  reset(LoadFile,1);

  i:=0;

  blockread(LoadFile,oldrec,sizeof(memoryrecordold),actualread);
  while actualread=sizeof(MemoryRecordold) do
  begin
    addrecord:=true;
    if merge then
      //find it in the current list, if it is in, dont add
      for j:=0 to mainform.NumberOfRecords-1 do
        if (mainform.memrec[j].Address=oldrec.Address) and (mainform.memrec[j].VarType=oldrec.VarType) then
        begin
          addrecord:=false;
          break;
        end;

    if addrecord then
    begin
      with mainform do
      begin
        inc(numberofrecords);
        reservemem;
        memrec[numberofrecords-1].Description:=oldrec.Description;
        memrec[numberofrecords-1].Address:=oldrec.Address;
        memrec[numberofrecords-1].VarType:=oldrec.VarType;
        memrec[numberofrecords-1].Bit:=0;
        memrec[numberofrecords-1].bitlength:=0;
        memrec[numberofrecords-1].Frozen:=false;
        memrec[numberofrecords-1].FrozenValue:=0;
      end;
    end;

    inc(i);
    blockread(LoadFile,oldrec,sizeof(memoryrecordold),actualread);
  end;

  closefile(LoadFile);     *)
end;

procedure LoadCET(filename: string; merge: boolean);
var CETFile: Textfile;
    i,j: integer;
    str: string;
    inuse: array [0..255] of boolean;
    NewRec: array [0..255] of MemoryrecordV2;
    addrecord:boolean;
begin
  (*
  //set it in memrec at once, I'll handle the counter later
  assignfile(CETfile,filename);
  reset(CETFile);

  for i:=0 to 254 do
    readln(CETFile,str);      //frozen state

  for i:=0 to 254 do
    readln(CETFile,str); //frozenvalue

  for i:=0 to 254 do //if the record is active or not
  begin
    readln(CETFile,str);
    inuse[i]:=str='1';
  end;

  for i:=0 to 254 do
  begin
    readln(CETFile,str);
    newrec[i].Address:=StrToInt(str);
  end;

  for i:=0 to 254 do
  begin
    readln(CETFile,str);
    newrec[i].VarType:=StrToInt(str);  //vartype assignments should be the same. (I hope)
  end;

  for i:=0 to 254 do
  begin
    readln(CETFile,str);
    newrec[i].Description:=str;
  end;

  //now add to the list
  for i:=0 to 254 do
  begin
    if inuse[i] then
    begin
      addrecord:=true;
      if merge then
      begin
        //find it in the current list, if it is in, dont add
        for j:=0 to mainform.NumberOfRecords-1 do
          if (mainform.memrec[j].Address=newrec[i].Address) and (mainform.memrec[j].VarType=newrec[i].VarType) then
          begin
            addrecord:=false;
            break;
          end;
      end;

      if addrecord then
      begin
        inc(mainform.numberofrecords);
        mainform.reserveMem;
        mainform.memrec[mainform.NumberOfRecords-1].Address:=newrec[i].Address;
        mainform.memrec[mainform.NumberOfRecords-1].VarType:=newrec[i].VarType;
        mainform.memrec[mainform.NumberOfRecords-1].Description:=newrec[i].Description;
      end;

    end;
  end;

  closefile(CETFile);
    *)
end;

Procedure LoadAMT(filename:string;merge:boolean);
var x,y,z: string;
    f: textfile;
    i,j: integer;

    first: boolean;
    linenr: integer;
    lines: array of string;

    records: integer;
    tempmemrec: array of MemoryrecordV3;

    addrecord: boolean;
begin
  (*
  linenr:=0;
  assignfile(f,filename);
  reset(f);

  try
    readln(f,x);

    //parse the text
    firsT:=false;
    for i:=1 to length(x) do
    begin
      if x[i]='"' then
      begin
        if not first then
        begin
          first:=true;
          y:='';
          continue;
        end
        else
        begin
          inc(linenr);
          setlength(lines,linenr);
          lines[linenr-1]:=y;
          first:=false;
        end;

      end
      else if first then y:=y+x[i];
    end;

    if lines[0]<>'ArtMoney Table' then raise exception.Create('This is not a valid ArtMoney Table');
    if lines[1]='4' then
    begin
      records:=(length(lines)-17) div 5;

      setlength(tempmemrec,records);

      for i:=0 to records-1 do
      begin
        tempmemrec[i].Description:=lines[17+5*i];

        x:=lines[(17+5*i)+1]; //address
        y:=lines[(17+5*i)+4]; //valuetype

        if length(x)=8 then
        begin
          try
            tempmemrec[i].address:=StrToInt('$'+x)
          except
            ;
          end;
        end
        else
        begin
          //example:  P6F71C7B8,1688
          z:='';
          z:=copy(x,pos(',',x)+1,length(x));

          tempmemrec[i].address:=0;
          tempmemrec[i].ispointer:=true;
          //pointer stuff
          setlength(tempmemrec[i].pointers,1);
          try
            tempmemrec[i].pointers[0].address:=0;
            tempmemrec[i].pointers[0].offset:=StrToInt(z);
          except
            setlength(tempmemrec[i].pointers,0);
            tempmemrec[i].IsPointer:=false;
          end;
        end;

        if length(y)<3 then continue;
        if y[2]+y[3]='i1' then tempmemrec[i].vartype:=0;
        if y[2]+y[3]='i2' then tempmemrec[i].vartype:=1;
        if y[2]+y[3]='i4' then tempmemrec[i].vartype:=2;
        if y[2]+y[3]='i8' then tempmemrec[i].vartype:=6;
        if y[2]+y[3]='f4' then tempmemrec[i].vartype:=3;
        if y[2]+y[3]='f8' then tempmemrec[i].vartype:=4;
        if y[1]+y[2]='nt' then tempmemrec[i].VarType:=7;
        if y[1]+y[2]='ns' then tempmemrec[i].VarType:=7;
        if y[1]+y[2]='np' then tempmemrec[i].VarType:=7;
      end;
    end
    else if lines[1]='5' then
    begin

      records:=(length(lines)-18) div 7;

      setlength(tempmemrec,records);

      for i:=0 to records-1 do
      begin
        tempmemrec[i].Description:=lines[18+7*i];

        x:=lines[(18+7*i)+1]; //address
        y:=lines[(18+7*i)+5]; //valuetype

        if length(x)=8 then
        begin
          try
            tempmemrec[i].address:=StrToInt('$'+x)
          except
            ;
          end;
        end
        else
        begin
          //example:  P6F71C7B8,1688
          z:='';
          z:=copy(x,pos(',',x)+1,length(x));

          tempmemrec[i].address:=0;
          tempmemrec[i].ispointer:=true;
          //pointer stuff
          setlength(tempmemrec[i].pointers,1);
          try
            tempmemrec[i].pointers[0].address:=0;
            tempmemrec[i].pointers[0].offset:=StrToInt(z);
          except
            setlength(tempmemrec[i].pointers,0);
            tempmemrec[i].IsPointer:=false;
          end;
        end;

        if length(y)<3 then continue;
        if y[2]+y[3]='i1' then tempmemrec[i].vartype:=0;
        if y[2]+y[3]='i2' then tempmemrec[i].vartype:=1;
        if y[2]+y[3]='i4' then tempmemrec[i].vartype:=2;
        if y[2]+y[3]='i8' then tempmemrec[i].vartype:=6;
        if y[2]+y[3]='f4' then tempmemrec[i].vartype:=3;
        if y[2]+y[3]='f8' then tempmemrec[i].vartype:=4;
        if y[1]+y[2]='nt' then tempmemrec[i].VarType:=7;
        if y[1]+y[2]='ns' then tempmemrec[i].VarType:=7;
        if y[1]+y[2]='np' then tempmemrec[i].VarType:=7;
      end;
    end else raise exception.Create('Cheat Engine can''t load this version of ArtMoney tables:'+lines[1]);

    for i:=0 to records-1 do
    begin
      addrecord:=true;
      if merge then
      begin
        //find it in the current list, if it is in, dont add
        for j:=0 to mainform.NumberOfRecords-1 do
          if (mainform.memrec[j].Address=tempmemrec[i].Address) and (mainform.memrec[j].VarType=tempmemrec[i].VarType) then
          begin
            addrecord:=false;
            break;
          end;
      end;

      if addrecord then
      begin
        inc(mainform.numberofrecords);
        mainform.reserveMem;
        mainform.memrec[mainform.NumberOfRecords-1].Address:=tempmemrec[i].Address;
        mainform.memrec[mainform.NumberOfRecords-1].VarType:=tempmemrec[i].VarType;
        mainform.memrec[mainform.NumberOfRecords-1].Description:=tempmemrec[i].Description;
        mainform.memrec[mainform.NumberOfRecords-1].bit:=5;
{$ifndef net}
        mainform.memrec[mainform.NumberOfRecords-1].IsPointer:=tempmemrec[i].IsPointer;
        if tempmemrec[i].IsPointer then
        begin
          setlength(mainform.memrec[mainform.NumberOfRecords-1].pointers,1);
          mainform.memrec[mainform.NumberOfRecords-1].pointers[0].address:=tempmemrec[i].pointers[0].Address;
          mainform.memrec[mainform.NumberOfRecords-1].pointers[0].offset:=tempmemrec[i].pointers[0].offset;
        end;
{$endif}
      end;
    end;

  finally
    closefile(f);
  end;

  //load a ArtMoneyTable
  {
0 "ArtMoney Table":Identifier
1 "5":version???
2 "7.06":Artmoney version
3 "19783D10"
4 "1C673566": crc?
5 "SE": SE version
6 "Tutorial":Application title
7 "Tutorial.exe":Application processname
8 "12/23/2003":Date
9 "2":???
10 "5":system
11 "1":system
12 "Service Pack 1":Service pack nr
13 "Eric":user
14 "":???
15 "":???
16 "":???
17 "":???

18+0 "Value 1":Description
18+1 "008B2768":address
18+2 ""
18+3 ""
18+4 ""
18+5 "ni4":integer 4 byte
18+6 ""

18+7 "Value 2":description
18+8 "008B6CF8"
18+9 ""
18+10 ""
18+11 ""
18+12 "ni4":integer 4 byte
18+13 ""

//---------------------------------------------
"ArtMoney Table":Identifier
"5":???
"7.06":version of artmoney
"1978830A":??
"382F0805":??
"SE":SE version
"Warcraft 3":process title
"War3.exe":process exe
"10/12/2002":date created
"2":??
"5":system
"1":system
"":special info about system
"ArtMoney Team":name
"Russia, Samara":location
"artmoney@mail.ru":email
"":???
"":???

"Experience":Description
"P6F71C7B8,1684":address
"90,6,0,0,88,6,,1":???
"hero 1"
""
"ni4":integer 4 bytes
"":



"Points":description
"P6F71C7B8,1688":address
"90,6,0,0,88,6,,1":??
"hero 1":
"":?
"ni4":integer 4 bytes
""

"Power"
"P6F71C7B8,1692"
"90,6,0,0,88,6,,1"
"hero 1"
""
"ni4"
""

"Dexterity"
"P6F71C7B8,1712"
"90,6,0,0,88,6,,1"
"hero 1"
""
"ni4"
""

"Health"
"P6F71C7B8,1708"
"90,6,0,0,88,6,,1"
"hero 1"
""
"nf4":float 4 bytes
""

"Manna"
"P6F71C7B8,1744"
"90,6,0,0,88,6,,1"
"hero 1"
""
"nf4":float 4 bytes
""
  }

{
0 "ArtMoney Table": ?
1 "4":?
2 "6.21":?
3 "19786E3F":?
4 "382F0805":?
5 "Warcraft 3":?
6 "War3.exe":?
7 "10/12/2002":?
8 "2":?
9 "5":?
10 "1":?
11 "":?
12 "John Sazonov":?
13 "Russia, Perm":?
14 "evg-sazonov@yandex.ru":?
15 "":?
16 "":?

17 "Experience":?
"0390242C":?
"90,6,0,0,88,6,,1":?
"":?
"ni4":?

"Points":?
"03902430":?
"90,6,0,0,88,6,,1":?
"":?
"ni4":?

"Power":?
"03902434":?
"90,6,0,0,88,6,,1":?
"":?
"ni4":?

"Dexterity":?
"03902448":?
"90,6,0,0,88,6,,1":?
"":?
"ni4":?

"Health":?
"03902444":?
"90,6,0,0,88,6,,1":?
"":?
"nf4":?

"Manna":?
"03902468":?
"90,6,0,0,88,6,,1":?
"":?
"nf4"

}
   *)
end;

Procedure LoadGH(Filename: string; merge: boolean);
(*var
  i,j: integer;
  loadfile: file;

  count: integer;
  NewRec: MemoryrecordV2;
  charstoread: byte;

  addrecord: boolean;

  x: pchar;
  ar: dword;  *)
begin
(*  assignfile(loadfile,filename);
  reset(loadfile,1);

  count:=0;
  blockread(loadfile,count,2,ar);
  if ar<>2 then raise exception.create('error while loading this table');

  //first the part that tells it's valid
  seek(loadfile,8);
  getmem(x,11);
  blockread(loadfile,x^,10);
  x[10]:=chr(0);
  if x<>'CDireccion' then
  begin
    closefile(loadfile);
    raise exception.create('This is not an valid GameHack table!');
  end;

  for i:=0 to count-1 do
  begin
    //get address
    blockread(loadfile,newrec.Address,4);

    //get VarType
    getmem(x,2);
    blockread(loadfile,x^,1);
    x[1]:=chr(0);
    if x='1' then newrec.VarType:=0 else
    if x='2' then newrec.VarType:=1 else
    if x='4' then newrec.VarType:=2 else
                  newrec.VarType:=0;

    //get Description
    blockread(loadfile,CharsToRead,1);
    getmem(x,CharsToRead+1);
    blockread(loadfile,x^,CharsToRead);
    x[CharsToRead]:=chr(0);
    newrec.Description:=x;
    freemem(x);

    //
    newrec.Frozen:=false;

    getmem(x,7);
    if i<>count-1 then blockread(loadfile,x^,6);
    freemem(x);

    addrecord:=true;
    if merge then
    begin
      //find it in the current list, if it is in, dont add
      for j:=0 to mainform.NumberOfRecords-1 do
        if (mainform.memrec[j].Address=newrec.Address) and (mainform.memrec[j].VarType=newrec.VarType) then
        begin
          addrecord:=false;
          break;
        end;
    end;

    if addrecord then
    begin
      inc(mainform.numberofrecords);
      mainform.reserveMem;
      mainform.memrec[mainform.NumberOfRecords-1].Address:=newrec.Address;
      mainform.memrec[mainform.NumberOfRecords-1].VarType:=newrec.VarType;
      mainform.memrec[mainform.NumberOfRecords-1].Description:=newrec.Description;
    end;
  end;*)
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
begin

  Extension:=uppercase(extractfileext(filename));

  (*
  If Extension='.EXE' then
  begin
    LoadExe(filename);
    exit;
  end;
  *)


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

  if Extension='.AMT' then LoadAMT(filename,merge) else
  if Extension='.GH' then LoadGH(filename,merge) else
  if Extension='.CET' then LoadCET(filename,merge) else
  if Extension='.CT2' then LoadCT2(filename,merge) else
  if Extension='.CT3' then LoadCT3(filename,merge) else
  if Extension='.CT' then LoadCT(filename,merge) else
  if Extension='.XML' then LoadXML(filename,merge) else
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
    Entries,Codes,Symbols, Structures, Comment: TDOMNode;
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
      extradata:=pointer(sl.Objects[i]);
      SymbolRecord:=symbols.AppendChild(doc.CreateElement('SymbolEntry'));
      SymbolRecord.AppendChild(doc.CreateElement('name')).TextContent:=sl[i];

      SymbolRecord.AppendChild(doc.CreateElement('Address')).TextContent:=extradata.addressstring;
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
    for i:=0 to comments.Memo1.Lines.Count-1 do
      comment.AppendChild(doc.CreateElement('Comment')).TextContent:=comments.Memo1.Lines[i];
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
      (*
      if standalone<>nil then
      begin
        StandAlone.filename:=filename;
        standAlone.showmodal;
      end;
      *)
    end;
    mainform.editedsincelastsave:=false;
  finally

  end;

end;


end.



