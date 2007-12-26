unit OpenSave;          //should be called loadsave but oh well...

interface

{$ifdef net}
uses unit2,dialogs,windows,Classes,Graphics,Controls,commentsunit,advancedoptionsunit,SysUtils,ceclient,netapis,cefuncproc;
{$else}
uses mainunit,windows,standaloneunit,SysUtils,advancedoptionsunit,commentsunit,
     cefuncproc,classes,formmemorymodifier,formMemoryTrainerUnit,shellapi,
     MemoryTrainerDesignUnit,StdCtrls,ExtraTrainerComponents,Graphics,Controls,
     ExtCtrls,Dialogs,newkernelhandler, hotkeyhandler;
{$endif}


var CurrentTableVersion: dword=7;
procedure SaveTable(Filename: string);
procedure LoadTable(Filename: string;merge: boolean);
procedure SaveCEM(Filename:string;address,size:dword);

{$ifndef net}
procedure LoadExe(filename: string);
{$endif}


{type TCEPointer=record
  Address: Dword;  //only used when last pointer in list
  offset: dword;
end;}

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

{$ifdef net}
var processhandle: thandle;
{$endif}

resourcestring strunknowncomponent='There is a unknown component in the trainer! compnr=';

implementation

uses symbolhandler;


{$ifndef net}
procedure LoadTrainer7(trainer:TFilestream);
var temp: dword;
    tempb: boolean;
    tempc: tcolor;
    tempi: integer;
    tempbc: tbevelcut;
    tempbk: tbevelkind;
    tempcursor: tcursor;
    i,j,k:integer;
    x: pchar;

    image: pointer;
//    trainerdata1: array of TTrainerData1;
begin
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
end;


procedure LoadTrainer6(trainer:TFilestream);
var temp: dword;
    tempb: boolean;
    tempc: tcolor;
    tempi: integer;
    tempbc: tbevelcut;
    tempbk: tbevelkind;
    tempcursor: tcursor;
    i,j,k:integer;
    x: pchar;

    image: pointer;
    laststate: word;
    lastshiftstate:word;

    keycombo: tkeycombo;
//    trainerdata1: array of TTrainerData1;
begin
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
end;


procedure LoadTrainer5(trainer:TFilestream);
var temp: dword;
    tempb: boolean;
    tempc: tcolor;
    tempi: integer;
    tempbc: tbevelcut;
    tempbk: tbevelkind;
    tempcursor: tcursor;
    i,j,k:integer;
    x: pchar;

    image: pointer;
    laststate: word;
    lastshiftstate: word;
//    trainerdata1: array of TTrainerData1;
begin
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
end;


procedure LoadTrainer4(trainer:TFilestream);
var temp: dword;
    tempb: boolean;
    tempc: tcolor;
    tempi: integer;
    tempbc: tbevelcut;
    tempbk: tbevelkind;
    tempcursor: tcursor;
    i,j,k:integer;
    x: pchar;

    image: pointer;
    laststate: word;
    lastshiftstate: word;     
//    trainerdata1: array of TTrainerData1;
begin
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
  if frmtrainerdesigner<>nil then frmtrainerdesigner.updatecheats
end;

procedure LoadTrainer3(trainer:tfilestream);
var temp: dword;
    tempb: boolean;
    tempc: tcolor;
    tempi: integer;
    tempbc: tbevelcut;
    tempbk: tbevelkind;
    tempcursor: tcursor;
    i,j,k:integer;
    x: pchar;

    image: pointer;
    laststate: word;
    lastshiftstate: word; 
//    trainerdata1: array of TTrainerData1;
begin
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

  //freemem(x);
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
    tempbk: tbevelkind;
    tempcursor: tcursor;
    i,j:integer;
    x: pchar;

    image: pointer;
    laststate: word;
    lastshiftstate: word;
//    trainerdata1: array of TTrainerData1;
begin
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

  //freemem(x);
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
    tempbk: tbevelkind;
    tempcursor: tcursor;
    i,j:integer;
    x: pchar;

    image: pointer;
    laststate: word;
    lastshiftstate: word;
//    trainerdata1: array of TTrainerData1;
begin
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

  //freemem(x);
end;

procedure LoadExe(filename: string);
resourcestring
  strCorruptIcon='The icon has been corrupted';
  strCantLoadFilepatcher='The file patcher can''t be loaded by Cheat Engine!';
  strNotACETrainer='This is not a trainer made by Cheat Engine (If it is a trainer at all!)';
  strUnknownTrainerVersion='This version of Cheat Engine doesn''t know how to read this trainer! Trainerversion=';
  strCantLoadProtectedfile='This trainer is protected from being opened by CE. Now go away!!!';
var trainer: tfilestream;
    temp: dword;
    hi: hicon;
begin
  if frmMemoryModifier<>nil then
    frmMemoryModifier.Close;

  frmMemoryModifier:=TFrmMemoryModifier.create(nil);
  frmMemoryModifier.show;

  //extract icon
  hi:=ExtractIcon(hinstance,pchar(filename),0);
  if (hi=0) or (hi=1) then
  begin
    frmMemorymodifier.Free;
    raise exception.Create(strCorruptIcon);
  end;

  frmMemoryModifier.Icon.Picture.Icon.Handle:=hi;
  frmMemoryTrainerPreview.Icon:=frmMemoryModifier.Icon.Picture.Icon;

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
      else raise exception.Create(strUnknownTrainerVersion+IntToStr(temp));
    end;

    if frmTrainerDesigner<>nil then
      frmTrainerDesigner.Icon:=frmMemoryModifier.Icon.Picture.Icon;

  finally
    trainer.Free;
  end;

end;
{$endif}

procedure LoadPTR(filename: string; merge: boolean);
var newrec: MemoryRecordV6;
    x: tfilestream;
    offsetlist: array of dword;
    invoffsetlist: array of dword;
    offsetsize: dword;
    i,j: integer;
begin
{$ifndef net}
  setlength(offsetlist,0);

  x:=tfilestream.Create(filename,fmopenread);
  try
    while x.Position<x.Size do
    begin
      x.ReadBuffer(offsetsize,sizeof(offsetsize));
      if length(offsetlist)<(offsetsize+1) then
      begin
        setlength(offsetlist,offsetsize*2);
        setlength(invoffsetlist,length(offsetlist));
      end;

      x.ReadBuffer(offsetlist[0],(offsetsize+1)*sizeof(offsetlist[0]));
      j:=0;
      for i:=offsetsize downto 2 do
      begin
        invoffsetlist[j]:=offsetlist[i];
        inc(j);
      end;

      mainform.addaddress('pointerscan result',offsetlist[0],invoffsetlist[0],offsetsize-1,true,2,0,0,false,false);
    end;
  finally
    x.free;
  end;
{$endif}
end;

procedure LoadV6(filename: string; ctfile: tfilestream;merge: boolean);
var newrec: MemoryRecordV6;
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
    tempdescription,tempmodulename: string;
    tempoffset: dword;

    address: dword;
    symbolname: string;
begin
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

      //interpretableaddress
      ctfile.ReadBuffer(j,sizeof(j));
      getmem(x,j+1);
      ctfile.readbuffer(x^,j);
      x[j]:=#0;
      newrec.interpretableaddress:=x;
      freemem(x);

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

          codelist.Items.Add(tempdescription);
        end;
      end;

    end;

    i:=ctfile.Position;
    ctfile.Position:=11;
    ctfile.ReadBuffer(j,4);
    ctfile.Position:=i;
    
    if j=7 then
    begin
      //version 7 also contains some stuff about symbols
      ctfile.ReadBuffer(records,sizeof(records));
      for i:=0 to records-1 do
      begin
        ctfile.ReadBuffer(address,sizeof(address));
        ctfile.ReadBuffer(j,sizeof(j));

        getmem(x,j+1);
        try
          ctfile.ReadBuffer(x^,j);
          x[j]:=#0;
          symbolname:=x;
        finally
          freemem(x);
        end;

        try
          symhandler.DeleteUserdefinedSymbol(symbolname);
          symhandler.AddUserdefinedSymbol(address,symbolname);
        except

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
begin
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

          codelist.Items.Add(tempdescription);
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
begin
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

          codelist.Items.Add(tempdescription);
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
begin
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

          codelist.Items.Add(tempdescription);
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
begin
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

          codelist.Items.Add(tempdescription);
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
begin
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

          codelist.Items.Add(tempdescription);
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
  ctfile:=Tfilestream.Create(filename,fmopenread);
  try
    getmem(x,12);
    ctfile.ReadBuffer(x^,11);
    x[11]:=#0;

    if x<>'CHEATENGINE' then
    begin
      if messagedlg('This is NOT a valid Cheat Engine table. Are you sure you want to load it?',mtWarning, [mbyes,mbno],0) = mrno then exit;
    end;

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
      6,7: LoadV6(filename,ctfile,merge);
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
begin
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

          codelist.Items.Add(tempdescription);
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
end;

procedure LoadCT2(filename: string; merge: boolean);
var loadfile: File;
    actualread: dword;
    oldrec: MemoryrecordOld;
    i,j: integer;
    addrecord: boolean;
begin
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

  closefile(LoadFile);
end;

procedure LoadCET(filename: string; merge: boolean);
var CETFile: Textfile;
    i,j: integer;
    str: string;
    inuse: array [0..255] of boolean;
    NewRec: array [0..255] of MemoryrecordV2;
    addrecord:boolean;
begin
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

end;

Procedure LoadGH(Filename: string; merge: boolean);
var
  i,j: integer;
  loadfile: file;

  count: integer;
  NewRec: MemoryrecordV2;
  charstoread: byte;

  addrecord: boolean;

  x: pchar;
  ar: dword;
begin
  assignfile(loadfile,filename);
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


begin
  Extension:=uppercase(extractfileext(filename));
  {$ifndef net}
  If Extension='.EXE' then
  begin
    LoadExe(filename);
    exit;
  end;
  {$endif}

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

      advancedoptions.codelist.Clear;
      setlength(code,0);

      codelist.clear;
      numberofcodes:=0;
    end;

    mainform.numberofrecords:=0;
    mainform.reservemem;  //erased...

    Comments.Memo1.Text:='';

  end;

  if Extension='.PTR' then LoadPTR(filename,merge) else
  if Extension='.AMT' then LoadAMT(filename,merge) else
  if Extension='.GH' then LoadGH(filename,merge) else
  if Extension='.CET' then LoadCET(filename,merge) else
  if Extension='.CT2' then LoadCT2(filename,merge) else
  if Extension='.CT3' then LoadCT3(filename,merge) else
  if Extension='.CT' then LoadCT(filename,merge) else
  raise exception.create('Unknown extention');

  with mainform do
  begin
    oldnumberofrecords:=numberofrecords;
    oldcodelistcount:=advancedoptions.codelist.Count;

    setlength(oldmemrec,numberofrecords);
    for i:=0 to numberofrecords-1 do
    begin
      oldmemrec[i].Description:=memrec[i].Description;
      oldmemrec[i].Address:=memrec[i].Address;
      oldmemrec[i].VarType:=memrec[i].VarType;
{$ifndef net}
      oldmemrec[i].IsPointer:=memrec[i].IsPointer;
      setlength(oldmemrec[i].pointers,length(memrec[i].pointers));
      for j:=0 to length(memrec[i].pointers)-1 do
      begin
        oldmemrec[i].pointers[j].Address:=memrec[i].pointers[j].Address;
        oldmemrec[i].pointers[j].offset:=memrec[i].pointers[j].offset;
      end;
{$endif}
      oldmemrec[i].Bit:=memrec[i].Bit;
      oldmemrec[i].bitlength:=memrec[i].bitlength;
      oldmemrec[i].Group:=memrec[i].Group;
    end;

    oldcomments:=comments.Memo1.Text;
  end;


  mainform.scrollbar1.position:=0;
  mainform.savedialog1.FileName:='';
  for i:=0 to mainform.numberofrecords-1 do
  begin
    if mainform.hotkeys[i]>=10 then
      unregisterhotkey(mainform.handle,mainform.hotkeys[i]);
    mainform.hotkeys[i]:=-1;
    mainform.hotkeystrings[i]:='';
  end;

  mainform.editedsincelastsave:=false;
end;


procedure SaveTable(Filename: string);
var savefile: File;
    actualwritten: Integer;
    Controle: String[11];
    records: dword;
    x: Pchar;
    i,j,k: integer;
    nrofbytes: byte;

    temp: dword;

    sl: tstringlist;
begin
//version=3;

  if Uppercase(extractfileext(filename))<>'.EXE' then
  begin
    assignfile(SaveFile,filename);
    rewrite(SaveFile,1); //sizeof(memoryrecord));

    blockwrite(savefile,'CHEATENGINE',11,actualwritten);
    blockwrite(savefile,CurrentTableVersion,4,actualwritten);

    Records:=mainform.numberofrecords;

    blockwrite(savefile,records,4,actualwritten);

    for i:=0 to mainform.NumberOfRecords-1 do
    begin
      j:=length(mainform.memrec[i].Description);
      blockwrite(savefile,j,sizeof(j),actualwritten);
      blockwrite(savefile,mainform.memrec[i].Description[1],length(mainform.memrec[i].Description),actualwritten);
      blockwrite(savefile,mainform.memrec[i].Address,sizeof(mainform.memrec[i].Address),actualwritten);

      //interpretableaddress
      j:=length(mainform.memrec[i].interpretableaddress);
      x:=pchar(mainform.memrec[i].interpretableaddress);
      blockwrite(savefile,j,sizeof(j),actualwritten);
      blockwrite(savefile,pointer(x)^,j,actualwritten);


      blockwrite(savefile,mainform.memrec[i].VarType,sizeof(mainform.memrec[i].VarType),actualwritten);
      blockwrite(savefile,mainform.memrec[i].unicode,sizeof(mainform.memrec[i].unicode),actualwritten);      
      blockwrite(savefile,mainform.memrec[i].Bit,sizeof(mainform.memrec[i].Bit),actualwritten);
      blockwrite(savefile,mainform.memrec[i].bitlength,sizeof(mainform.memrec[i].bitlength),actualwritten);
      blockwrite(savefile,mainform.memrec[i].Group,sizeof(mainform.memrec[i].Group),actualwritten);
      blockwrite(savefile,mainform.memrec[i].ShowAsHex,sizeof(mainform.memrec[i].showashex),actualwritten);
      blockwrite(savefile,mainform.memrec[i].ispointer,sizeof(mainform.memrec[i].ispointer),actualwritten);

      temp:=length(mainform.memrec[i].pointers);
      blockwrite(savefile,temp,sizeof(temp),actualwritten);
      for j:=0 to temp-1 do
      begin
        blockwrite(savefile,mainform.memrec[i].pointers[j].Address,sizeof(mainform.memrec[i].pointers[j].Address),actualwritten);
        blockwrite(savefile,mainform.memrec[i].pointers[j].offset,sizeof(mainform.memrec[i].pointers[j].offset),actualwritten);

        //interpretableaddress for pointer
        k:=length(mainform.memrec[i].pointers[j].interpretableaddress);
        x:=pchar(mainform.memrec[i].pointers[j].interpretableaddress);
        blockwrite(savefile,k,sizeof(k),actualwritten);
        blockwrite(savefile,pointer(x)^,k,actualwritten);
      end;


      //autoassemble script
      j:=length(mainform.memrec[i].autoassemblescript);
      x:=pchar(mainform.memrec[i].autoassemblescript);

      blockwrite(savefile,j,sizeof(j),actualwritten);
      blockwrite(savefile,pointer(x)^,j,actualwritten);


    end;

    records:=advancedoptions.numberofcodes;
    blockwrite(savefile,records,4,actualwritten);

    //save the code list

    for i:=0 to records-1 do
    begin
      //read the before, actual, and after codes
      //first byte is the number of bytes
      blockwrite(savefile,advancedoptions.code[i].Address,4,actualwritten);

      x:=pchar(advancedoptions.code[i].modulename);
      nrofbytes:=length(advancedoptions.code[i].modulename);
      blockwrite(savefile,nrofbytes,1,actualwritten);
      blockwrite(savefile,pointer(x)^,nrofbytes,actualwritten);

      blockwrite(savefile,advancedoptions.code[i].offset,4);
      

      nrofbytes:=length(advancedoptions.code[i].before);
      blockwrite(savefile,nrofbytes,1,actualwritten);
      blockwrite(savefile,pointer(advancedoptions.code[i].before)^,nrofbytes,actualwritten);

      nrofbytes:=length(advancedoptions.code[i].actualopcode);
      blockwrite(savefile,nrofbytes,1,actualwritten);
      blockwrite(savefile,pointer(advancedoptions.code[i].actualopcode)^,nrofbytes,actualwritten);

      nrofbytes:=length(advancedoptions.code[i].after);
      blockwrite(savefile,nrofbytes,1,actualwritten);
      blockwrite(savefile,pointer(advancedoptions.code[i].after)^,nrofbytes,actualwritten);

      x:=pchar(advancedoptions.codelist.Items[i]);
      nrofbytes:=length(x);
      blockwrite(savefile,nrofbytes,1,actualwritten);
      blockwrite(savefile,pointer(x)^,nrofbytes,actualwritten);
    end;

    //symbollist
    sl:=tstringlist.Create;
    try
      symhandler.EnumerateUserdefinedSymbols(sl);
      records:=sl.Count;
      blockwrite(savefile,records,sizeof(records),actualwritten);
      for i:=0 to records-1 do
      begin
        temp:=dword(sl.Objects[i]);
        blockwrite(savefile,temp,sizeof(temp),actualwritten);
        x:=pchar(sl[i]);
        temp:=length(x);
        blockwrite(savefile,temp,sizeof(temp),actualwritten);
        blockwrite(savefile,pointer(x)^,temp,actualwritten);
      end;
    finally
      sl.free;
    end;

    //comments
    x:=pchar(comments.memo1.text);
    blockwrite(Savefile,x^,length(comments.memo1.text),actualwritten);
    closefile(savefile);

    with mainform do
    begin
      oldnumberofrecords:=numberofrecords;
      oldcodelistcount:=advancedoptions.codelist.Count;

      setlength(oldmemrec,numberofrecords);
      for i:=0 to numberofrecords-1 do
      begin
        oldmemrec[i].Description:=memrec[i].Description;
        oldmemrec[i].Address:=memrec[i].Address;
        oldmemrec[i].VarType:=memrec[i].VarType;
        oldmemrec[i].IsPointer:=memrec[i].IsPointer;

        setlength(oldmemrec[i].pointers,length(memrec[i].pointers));
        for j:=0 to length(memrec[i].pointers)-1 do
        begin
          oldmemrec[i].pointers[j].Address:=memrec[i].pointers[j].Address;
          oldmemrec[i].pointers[j].offset:=memrec[i].pointers[j].offset;
        end;

        oldmemrec[i].Bit:=memrec[i].Bit;
        oldmemrec[i].bitlength:=memrec[i].bitlength;
        oldmemrec[i].Group:=memrec[i].Group;
      end;

      oldcomments:=comments.Memo1.Text;
    end;

  end
  else
  begin
    {$ifndef net}
    if standalone<>nil then
    begin
      StandAlone.filename:=filename;
      standAlone.showmodal;
    end;
    {$endif}
  end;
  mainform.editedsincelastsave:=false;
end;


end.

