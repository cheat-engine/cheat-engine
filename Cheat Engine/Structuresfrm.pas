unit Structuresfrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ExtCtrls, ComCtrls,cefuncproc,newkernelhandler,
  symbolhandler, XMLDoc, XMLIntf, byteinterpreter, underc;

const structureversion=1;


type TStructElement=record
                      offset: dword;
                      description:string;
                      pointerto: boolean;  //determines if it's a pointer to a structure, or the structure itself
                      pointertosize: dword;
                      structurenr: integer; //-1 and lower=base element   (they can't be both -1)
                      bytesize: dword; //size in bytes of how big this element is. (also for base elements)
                    end;


type TbaseStructure=record
  name: string;
  structelement: array of TStructElement;
  end;

  //TfrmStructures = class;
  Tstructure=class
  private
    //frmStructures: TfrmStructures; //obsolete
    treeviewused: ttreeview;
    addresses: array of dword;
    basestructure: integer;
    parentnode: ttreenode; //owner of this object
    objects: array of record //same size as the structelement of the base object
                        nodetoupdate: ttreenode; //same size as the structelement of the base object
                        child: tstructure; //if it is a pointer then this points to the structure that defines it
                        //currentvalue: string; //obsolete, just get it on request
                      end;
  public
    procedure refresh;
    procedure removeAddress(i: integer);
    procedure setaddress(i: integer; x:dword);
    constructor create(treeviewused: ttreeview;parentnode: ttreenode; addresses: array of dword; basestructure: integer);
    destructor destroy; override;
  end;

  TfrmStructures = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Structures1: TMenuItem;
    Definenewstructure1: TMenuItem;
    N1: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    Addelement1: TMenuItem;
    updatetimer: TTimer;
    Deleteelement1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ChangeElement1: TMenuItem;
    N2: TMenuItem;
    Addtoaddresslist1: TMenuItem;
    Recalculateaddress1: TMenuItem;
    N3: TMenuItem;
    Addextraaddress1: TMenuItem;
    edtAddress: TEdit;
    PopupMenu2: TPopupMenu;
    Paste1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    N4: TMenuItem;
    Remove1: TMenuItem;
    N5: TMenuItem;
    SelectAll1: TMenuItem;
    N6: TMenuItem;
    Undo1: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    Newwindow1: TMenuItem;
    Commands1: TMenuItem;
    Deletecurrentstructure1: TMenuItem;
    Renamestructure1: TMenuItem;
    ScrollBox1: TScrollBox;
    tvStructureView: TTreeView;
    HeaderControl1: THeaderControl;
    Memorybrowsepointer1: TMenuItem;
    Memorybrowsethisaddress1: TMenuItem;
    Autoguessoffsets1: TMenuItem;
    Setgroup1: TMenuItem;
    Updatethisandfollowingoffsets1: TMenuItem;
    N9: TMenuItem;
    SaveValues1: TMenuItem;
    saveValues: TSaveDialog;
    procedure Definenewstructure1Click(Sender: TObject);
    procedure Addelement1Click(Sender: TObject);
    procedure updatetimerTimer(Sender: TObject);
    procedure tvStructureViewCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure edtAddressChange(Sender: TObject);
    procedure tvStructureViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure tvStructureViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Deleteelement1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure ChangeElement1Click(Sender: TObject);
    procedure tvStructureViewDblClick(Sender: TObject);
    procedure Addtoaddresslist1Click(Sender: TObject);
    procedure Recalculateaddress1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HeaderControl1SectionResize(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure Addextraaddress1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure Remove1Click(Sender: TObject);
    procedure tvStructureViewAdvancedCustomDrawItem(
      Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
      Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure Renamestructure1Click(Sender: TObject);
    procedure Deletecurrentstructure1Click(Sender: TObject);
    procedure Newwindow1Click(Sender: TObject);
    procedure Memorybrowsepointer1Click(Sender: TObject);
    procedure Memorybrowsethisaddress1Click(Sender: TObject);
    procedure Autoguessoffsets1Click(Sender: TObject);
    procedure Setgroup1Click(Sender: TObject);
    procedure Updatethisandfollowingoffsets1Click(Sender: TObject);
    procedure SaveValues1Click(Sender: TObject);
  private
    { Private declarations }
    currentstructure: tstructure;


    addresses: array of dword;  //first address (old compat)

    groupindex: array of integer; //e.g 1,3,9,10000000
    groups: array of dword;               //the grouplist holds the groupnumbers provided by the user
    internalgrouplist: array of dword;    //where as the internelgrouplist holds the entrypoints into the groupindex for each specific address 
    edits: array of tedit;
    lastnewedit: TEdit;



    procedure refreshmenuitems;
    procedure definedstructureselect(sender:tobject);
    function convertVariableTypeTostructnr(d: TVariableType): integer;
//    function RawToType(address: dword; const buf: array of byte; size: integer):integer;
    procedure ExtraEnter(Sender: TObject);
    procedure automaticallyGuessOffsets(baseOffset: dword; structsize: integer);
    procedure UpdateGroupIndex;
  public
    { Public declarations }
    procedure setaddress(i: integer; x:dword);
    procedure update(doOthers: boolean);
  end;

var
  frmStructures: array of TfrmStructures;
  definedstructures: array of TbaseStructure;

procedure sortStructure(struct: TbaseStructure);

implementation

{$R *.dfm}

uses StructuresAddElementfrm,valuechange,mainunit, MemoryBrowserFormUnit, opensave;

destructor TStructure.destroy;
var i: integer;
begin
  for i:=0 to length(objects)-1 do
    if objects[i].child<>nil then objects[i].child.Free;

  inherited destroy;
end;

constructor TStructure.create(treeviewused: ttreeview;parentnode: ttreenode; addresses: array of dword; basestructure: integer);
var elementnr: integer;
    s: tstructure;
    i: integer;
begin

  setlength(self.addresses,length(addresses));
  for i:=0 to length(addresses)-1 do
    self.addresses[i]:=addresses[i];
    
  self.basestructure:=basestructure;
  self.treeviewused:=treeviewused;
  self.parentnode:=parentnode;
  inherited create;
end;

procedure TStructure.removeAddress(i: integer);
var j: integer;
begin
  for j:=i to length(addresses)-2 do
    addresses[j]:=addresses[j+1];

  if i<length(addresses) then //just in case it didn't get the previous add...
    setlength(addresses,length(addresses)-1);

  for j:=0 to length(objects)-1 do
    if objects[j].child<>nil then
      objects[j].child.removeAddress(i);
  refresh;
end;


procedure TStructure.setaddress(i: integer; x:dword);
var j: integer;
begin
  if i>=length(addresses) then
    setlength(addresses,i+1);

  //update children
  for j:=0 to length(objects)-1 do
  begin
    if objects[j].child<>nil then
      objects[j].child.setaddress(i,x);
  end;

  addresses[i]:=x;
  refresh;
end;

procedure TStructure.refresh;
var c,i,j,k: integer;
    newtext,typename: string;
    snr: integer;
    elementoffset: dword;
    buf: array of byte;
    x: dword;

    defaultwidth: integer;

    ws: widestring;
    pc: pchar;
    pwc: pwidechar;
    newaddress: dword;

    s: tstructure;

    elementnr: integer;
    currentvalues: array of string;

begin
  //check if all nodes are present and remove those not needed anymore
  //and adjust the text when needed
 // treeviewused.Items.BeginUpdate;
  setlength(buf,32);

  if basestructure<0 then
  begin
    //this is a base type, it has to be a child. (and pointer type)
    if length(objects)=0 then setlength(objects,1);
  end
  else
  begin
    if length(objects)<length(definedstructures[basestructure].structelement) then
      setlength(objects,length(definedstructures[basestructure].structelement));

    if length(objects)>length(definedstructures[basestructure].structelement) then
    begin
      //delete the extra ones
      for i:=length(definedstructures[basestructure].structelement) to length(objects)-1 do
      begin
        if objects[i].nodetoupdate<>nil then objects[i].nodetoupdate.Delete;
        if objects[i].child<>nil then objects[i].child.Free;
      end;

      setlength(objects,length(definedstructures[basestructure].structelement));


    end;
    treeviewused.Items.GetFirstNode.Text:=definedstructures[basestructure].name+#13;
  end;

  setlength(currentvalues,length(addresses));

  for i:=0 to length(objects)-1 do
  begin

    
    //define the text for this element
    if basestructure<0 then
    begin
      snr:=basestructure;
      elementoffset:=0;
    end
    else
    begin
      elementoffset:=definedstructures[basestructure].structelement[i].offset;
      
      if definedstructures[basestructure].structelement[i].pointerto then
        typename:='pointer to '
      else
        typename:='';

      snr:=definedstructures[basestructure].structelement[i].structurenr;
    end;

    for c:=0 to length(addresses)-1 do
    begin
      if snr<0 then
      begin
        if basestructure>=0 then
        begin
          if definedstructures[basestructure].structelement[i].pointerto then
          begin
            currentvalues[c]:='->';

            if readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@buf[0],4,x) then
              currentvalues[c]:=currentvalues[c]+inttohex(PDWORD(@buf[0])^,8)
            else
              currentvalues[c]:=currentvalues[c]+'???';

          end;
        end;

        if (basestructure<0) or (not definedstructures[basestructure].structelement[i].pointerto) then
        case snr of
          -1:
          begin
            if c=0 then typename:=typename+'Byte';

            //read the value
            if readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@buf[0],1,x) then
              currentvalues[c]:=inttostr(byte(buf[0]))
            else
              currentvalues[c]:='???';
          end;
          -2:
          begin
            if c=0 then typename:=typename+'Byte Signed';

            //read the value
            if readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@buf[0],1,x) then
              currentvalues[c]:=inttostr(Shortint(buf[0]))
            else
              currentvalues[c]:='???';
          end;
          -3:
          begin
            if c=0 then typename:=typename+'Byte Hexadecimal';
            if readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@buf[0],1,x) then
              currentvalues[c]:=inttohex(buf[0],2)
            else
              currentvalues[c]:='???';
          end;
          -4:
          begin
            if c=0 then typename:=typename+'2 Bytes';
            if readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@buf[0],2,x) then
              currentvalues[c]:=inttostr(PWORD(@buf[0])^)
            else
              currentvalues[c]:='???';
          end;
          -5:
          begin
            if c=0 then typename:=typename+'2 Bytes Signed';
            if readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@buf[0],2,x) then
              currentvalues[c]:=inttostr(PSmallint(@buf[0])^)
            else
              currentvalues[c]:='???';
          end;
          -6:
          begin
            if c=0 then typename:=typename+'2 Bytes Hexadecimal';
            if readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@buf[0],2,x) then
              currentvalues[c]:=inttohex(PWORD(@buf[0])^,4)
            else
              currentvalues[c]:='???';
          end;
          -7:
          begin
            if c=0 then typename:=typename+'4 Bytes';
            if readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@buf[0],4,x) then
              currentvalues[c]:=inttostr(PDWORD(@buf[0])^)
            else
              currentvalues[c]:='???';
          end;

          -8:
          begin
            if c=0 then typename:=typename+'4 Bytes Signed';
            if readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@buf[0],4,x) then
              currentvalues[c]:=inttostr(pinteger(@buf[0])^)
            else
              currentvalues[c]:='???';
          end;

          -9:
          begin
            if c=0 then typename:=typename+'4 Bytes Hexadecimal';
            if readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@buf[0],4,x) then
              currentvalues[c]:=inttohex(PDWORD(@buf[0])^,8)
            else
              currentvalues[c]:='???';
          end;
          -10:
          begin
            if c=0 then typename:=typename+'8 Bytes';
            if readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@buf[0],8,x) then
              currentvalues[c]:=inttostr(pint64(@buf[0])^)
            else
              currentvalues[c]:='???';
          end;
          -11:
          begin
            if c=0 then typename:=typename+'8 Bytes Hexadecimal';
            if readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@buf[0],8,x) then
              currentvalues[c]:=inttohex(pint64(@buf[0])^,16)
            else
              currentvalues[c]:='???';
          end;
          -12:
          begin
            if c=0 then typename:=typename+'Float';
            if readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@buf[0],4,x) then
              currentvalues[c]:=floattostr(psingle(@buf[0])^)
            else
              currentvalues[c]:='???';
          end;
          -13:
          begin
            if c=0 then typename:=typename+'Double';
            if readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@buf[0],8,x) then
              currentvalues[c]:=floattostr(pdouble(@buf[0])^)
            else
              currentvalues[c]:='???';
          end;
          -14:
          begin
            if c=0 then typename:=typename+'String';

            if basestructure>=0 then
              k:=definedstructures[basestructure].structelement[i].bytesize
            else
            begin
              elementnr:=parentnode.Index;
              s:=parentnode.data;
              k:=definedstructures[s.basestructure].structelement[elementnr].pointertosize;
            end;

            if length(buf)<=k then
              setlength(buf,k+1);

            if readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@buf[0],k,x) then
            begin
              buf[k]:=0;
              for j:=0 to k-1 do
                if (buf[j]>0) and (buf[j]<32) then buf[j]:=ord('?');

              pc:=@buf[0];
              currentvalues[c]:=pc;
            end
            else
              currentvalues[c]:='???';
          end;
          -15:
          begin
            if c=0 then typename:=typename+'String Unicode';
            if basestructure>=0 then
              k:=definedstructures[basestructure].structelement[i].bytesize
            else
            begin
              elementnr:=parentnode.Index;
              s:=parentnode.data;
              k:=definedstructures[s.basestructure].structelement[elementnr].pointertosize;
            end;
            if length(buf)<=k then
              setlength(buf,k+1);


            if readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@buf[0],k,x) then
            begin
              buf[k]:=0;
              buf[k-1]:=0;
              for j:=0 to k-1 do
                if (buf[j]>0) and (buf[j]<32) then buf[j]:=ord('?');
              pwc:=@buf[0];
              ws:=pwc;
              currentvalues[c]:=ws;
            end
            else
              currentvalues[c]:='???';
          end;
        end;
      end else
      begin
        //it's a defined structure (has to be a pointer)
        if c=0 then typename:=definedstructures[snr].name;

        if readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@buf[0],8,x) then
          currentvalues[c]:='->'+inttohex(pdword(@buf[0])^,8)
        else
          currentvalues[c]:='->???';
      end;

    end;

    if basestructure<0 then
    begin
      newtext:=inttohex(elementoffset,4)+' - '+'('+typename+')';
    end
    else
    begin
      newtext:=inttohex(elementoffset,4)+' - '+definedstructures[basestructure].structelement[i].description;//+'('+typename+')';
    end;
    newtext:=newtext+#13;

    for c:=0 to length(addresses)-1 do
      newtext:=newtext+{inttohex(addresses[c]+elementoffset,8)+' : '+}currentvalues[c]+#13;

    //see if a node exists or not, if not create it.
    if objects[i].nodetoupdate=nil then
    begin
      objects[i].nodetoupdate:=treeviewused.Items.AddChild(self.parentnode,newtext);
      objects[i].nodetoupdate.Data:=self;
    end
    else
    begin
      if newtext<>objects[i].nodetoupdate.Text then
        objects[i].nodetoupdate.Text:=newtext;
    end;

    if basestructure>=0 then
    begin
      if objects[i].nodetoupdate.HasChildren <> definedstructures[basestructure].structelement[i].pointerto then
        objects[i].nodetoupdate.HasChildren:=definedstructures[basestructure].structelement[i].pointerto;

      if objects[i].child<>nil then
      begin
        //do the same for the children
        //pointer

        for c:=0 to length(addresses)-1 do
        begin
          newaddress:=0;
          readprocessmemory(processhandle,pointer(addresses[c]+elementoffset),@newaddress,4,x);

          objects[i].child.addresses[c]:=newaddress;
        end;
        objects[i].child.refresh;
      end;

    end;
  end;


  if treeviewused.Items.GetFirstNode<>nil then
    treeviewused.Items.GetFirstNode.Expand(false);

  //treeviewused.Items.endupdate;
end;

procedure sortStructure(struct: TbaseStructure);
var
  i,j: integer;
  l: integer;
  temp: TStructElement;
begin
  //bubblesort
  i:=1;
  l:=length(struct.structelement);
  for i:=1 to l-1 do
  begin
    if struct.structelement[i-1].offset>struct.structelement[i].offset then
    begin
      //the previous entry is bigger than the current entry

      //find a entry that's smaller
      for j:=i to l-1 do
      begin
        if struct.structelement[j-1].offset>struct.structelement[j].offset then
        begin
          //swap
          temp:=struct.structelement[j-1];
          struct.structelement[j-1]:=struct.structelement[j];
          struct.structelement[j]:=temp;
        end;

      end;
    end;
  end;


end;

procedure TfrmStructures.update(doOthers: boolean);
{Called twice, first time true, 2nd time false. Only on false actually update}
var i: integer;
begin
  if DoOthers then
  begin
    for i:=0 to length(frmStructures)-1 do
      frmStructures[i].update(false);
  end
  else
  begin
    RefreshMenuItems;
    if currentstructure<>nil then
    begin
      if currentstructure.basestructure < length(definedstructures) then
        currentstructure.refresh
      else
        freeandnil(currentstructure);
 
    end;
  end;

  if currentstructure=nil then
    tvStructureView.Items.Clear;
end;

procedure TfrmStructures.setaddress(i: integer; x: dword);
begin
  addresses[i]:=x;
     
  if currentstructure<>nil then
  begin
    currentstructure.setAddress(i,x);
 {
    if length(currentstructure.addresses)<=i then
      setlength(currentstructure.addresses,i+1);
    currentstructure.addresses[i]:=x;
    currentstructure.parentnode.Text:=edtaddress.text+'-'+definedstructures[currentstructure.basestructure].name;
    currentstructure.refresh;  }
  end;
end;


function TfrmStructures.convertVariableTypeTostructnr(d: TVariableType): integer;
begin
  result:=-1;
  case d of
    vtByte: result:=-1;
    vtWord: result:=-4;
    vtDword: result:=-7;
    vtQword: result:=-7;
    vtSingle: result:=-12;
    vtDouble: result:=-13;
    vtString: result:=-14;
    vtUnicodeString: result:=-7; //currently not implemented
    vtByteArray: result:=-7; //FindTypeOfData should never return this
    vtBinary: result:=-7; //nor this
    vtAll: result:=-7; //also not this
    vtCustom: result:=-7; //certainly not this
    vtPointer: result:=-7; //currently not handled, but can be used to speed up things... 
  end;
end;

{
function TfrmStructures.RawToType(address: dword; const buf: array of byte; size: integer):integer;
//returns: -1,-4,-7,-12,-13,-14
var x: string;
    i: integer;
    isstring: boolean;
begin
  result:=0;

  i:=address mod 4;
  case i of
    1: //1 byte
    begin
      result:=-1;
      exit;
    end;

    2,3: //2 byte
    begin
      result:=-4;
      exit;
    end;
  end;



  if size>=8 then  //check if a double can be used
  begin
    if pdouble(@buf[0])^<>0 then
    begin
      x:=floattostr(pdouble(@buf[0])^);
      if (pos('E',x)=0) then  //no exponent
      begin
        //check if the value isn't bigger or smaller than 1000000 or smaller than -1000000
        if (pdouble(@buf[0])^<1000000) and (pdouble(@buf[0])^>-1000000) then
        begin
          result:=-13;
          exit;
        end;
      end;
    end;
  end;

  if (size>=2) and (size<4) then
  begin
    result:=-4;
    exit;
  end;

  if (size=1) then
  begin
    result:=-1;
    exit;
  end;

  //still here so either 4, or not a double
  //check if it confirms to a single float
  if psingle(@buf[0])^<>0 then
  begin
    x:=floattostr(psingle(@buf[0])^);
    if (pos('E',x)=0) then  //no exponent
    begin
      //check if the value isn't bigger or smaller than 1000000 or smaller than -1000000
      if (psingle(@buf[0])^<1000000) and (psingle(@buf[0])^>-1000000) then
      begin
        result:=-12;
        exit;
      end;
    end;
  end;

  //still here, so check if it matches a string
  isstring:=true;
  i:=0;
  while i<4 do
  begin
    //check if the first 4 characters match with a standard ascii values (32 to 127)
    if (buf[i]<32) or (buf[i]>127) then
    begin
      isstring:=false;
      break;
    end;
    inc(i);
  end;

  if isstring then
  begin
    result:=-14;
    exit;
  end;

  //none of the above, so....
  result:=-7;
end;
}

procedure TfrmStructures.Definenewstructure1Click(Sender: TObject);
var sstructsize:string;
    autofillin,structsize: integer;
    structname: string;

begin
  structname:='unnamed structure';
  if not inputquery('Structure define','Give the name for this structure',structname) then exit;
  
  autofillin:=messagedlg('Do you want Cheat Engine to try and fill in the most basic types of the struct using the current address?',mtconfirmation,[mbyes,mbno,mbcancel],0);
  if autofillin=mrcancel then exit;

  setlength(definedstructures,length(definedstructures)+1);
  definedstructures[length(definedstructures)-1].name:=structname;

  refreshmenuitems;
  structures1.Items[structures1.Count-1].Click;

  if autofillin=mryes then
  begin

    sstructsize:='4096';
    if not inputquery('Structure define','Please give a starting size of the struct (You can change this later if needed)',Sstructsize) then exit;
    structsize:=strtoint(sstructsize);

    automaticallyGuessOffsets(0, structsize);
  end;
  update(true);
end;

procedure TfrmStructures.definedstructureselect(sender:tobject);
var name: string;
begin
  caption:='Memory dissect - '+StripHotkey((sender as tmenuitem).Caption);
  if currentstructure<>nil then
    freeandnil(currentstructure);

  tvStructureView.Items.Clear;

  currentstructure:=tstructure.create(tvStructureView,tvStructureView.Items.Add(nil,definedstructures[(sender as tmenuitem).Tag].name+#13),addresses,(sender as tmenuitem).Tag);
  update(false);

  commands1.enabled:=true; 
end;

procedure TfrmStructures.refreshmenuitems;
var i: integer;
    mi: tmenuitem;
begin
  //go through the definedstructures array and see if they are in the list or not

  //delete the ones that are too many
  while (structures1.Count-2)>length(definedstructures) do
    structures1.Delete(structures1.Count-1); //delete the last one

    
  for i:=0 to length(definedstructures)-1 do
  begin
    if i<structures1.Count-2 then
    begin
      //check the name, and update if needed
      if structures1.Items[i+2].Caption<>definedstructures[i].name then
        structures1.Items[i+2].Caption:=definedstructures[i].name;
    end
    else //add it
    begin
      mi:=tmenuitem.Create(self);
      mi.Caption:=definedstructures[i].name;
      mi.OnClick:=definedstructureselect;
      mi.Tag:=i;
      structures1.Add(mi);
    end;

    if (currentstructure<>nil) and (currentstructure.basestructure=i) then
      structures1.Items[i+2].Checked:=true
    else
      structures1.Items[i+2].Checked:=false;

  end;
end;


procedure TfrmStructures.Addelement1Click(Sender: TObject);
var d,i,j,k,l:integer;
    size: dword;
    structtype: string;
    selectedstructure: tstructure;
    selectedelement: integer;
    selectednode: ttreenode;
    base: dword;
begin
  if currentstructure=nil then exit;

  selectednode:=tvStructureView.Selected;
  if selectednode=nil then
    selectedstructure:=currentstructure
  else
    selectedstructure:=tstructure(selectednode.Data);

  if selectedstructure=nil then
    selectedstructure:=currentstructure;

  if selectedstructure.basestructure<0 then
  begin
    selectedstructure:=tstructure(selectednode.parent);
    selectednode:=selectednode.Parent;
  end;

  if selectednode=nil then //lastnode
    selectedelement:=length(definedstructures[selectedstructure.basestructure].structelement)-1
  else
  begin
    selectedelement:=selectednode.index-1;
  end;


  if currentstructure=nil then raise exception.Create('First select a structure you want to modify or define one first');
  with tfrmstructuresaddelement.create(self) do
  begin
    //fill the combobox with possible types
    //the base types, and defined types
    cbtype.Items.AddObject('Byte',pointer(1));
    cbtype.Items.AddObject('Byte Signed',pointer(1));
    cbtype.Items.AddObject('Byte Hexadecimal',pointer(1));
    cbtype.Items.AddObject('2 Bytes',pointer(2));
    cbtype.Items.AddObject('2 Bytes Signed',pointer(2));
    cbtype.Items.AddObject('2 Bytes Hexadecimal',pointer(2));
    cbtype.Items.AddObject('4 Bytes',pointer(4));
    cbtype.Items.AddObject('4 Bytes Signed',pointer(4));
    cbtype.Items.AddObject('4 Bytes Hexadecimal',pointer(4));
    cbtype.Items.AddObject('8 Bytes',pointer(8));
    cbtype.Items.AddObject('8 Bytes Hexadecimal',pointer(8));
    cbtype.Items.AddObject('Float',pointer(4));
    cbtype.Items.AddObject('Double',pointer(8));
    cbtype.Items.AddObject('String',pointer(10));
    cbtype.Items.AddObject('String Unicode',pointer(10));

    cbtype.ItemIndex:=8;
    cbType.OnChange(cbType);

    cbtype.DropDownCount:=17;

    //and add the other defined structures as well
    for i:=0 to length(definedstructures)-1 do
    begin
      size:=0;
      for j:=0 to length(definedstructures[i].structelement)-1 do
        inc(size,definedstructures[i].structelement[j].bytesize);

      cbtype.Items.AddObject(definedstructures[i].name,pointer(size));
    end;


    if selectedstructure.basestructure>=0 then
    begin
      if selectednode.index>=0 then
        edtOffset.text:=inttohex(definedstructures[selectedstructure.basestructure].structelement[selectednode.index].offset-1,1)
      else
      begin
        if length(definedstructures[selectedstructure.basestructure].structelement)>0 then
          edtOffset.text:=inttohex(definedstructures[selectedstructure.basestructure].structelement[length(definedstructures[selectedstructure.basestructure].structelement)-1].offset+definedstructures[selectedstructure.basestructure].structelement[length(definedstructures[selectedstructure.basestructure].structelement)-1].bytesize,1);
      end;
    end;


    if showmodal=mrok then
    begin
      if cbtype.ItemIndex=-1 then exit;

      //allocate a spot for the new element
      i:=length(definedstructures[selectedstructure.basestructure].structelement);
      setlength(definedstructures[selectedstructure.basestructure].structelement,i+1);

      //move the elements after selectedelement
      for j:=i-1 downto selectedelement+1 do
        definedstructures[selectedstructure.basestructure].structelement[j+1]:=definedstructures[selectedstructure.basestructure].structelement[j];

      i:=selectedelement+1;

      definedstructures[selectedstructure.basestructure].structelement[i].pointerto:=cbpointerto.checked;
      definedstructures[selectedstructure.basestructure].structelement[i].description:=edtDescription.text;
      base:=strToInt('$'+edtOffset.text);

      if definedstructures[selectedstructure.basestructure].structelement[i].pointerto then
      begin
        if cbtype.itemindex<=14 then
          definedstructures[selectedstructure.basestructure].structelement[i].structurenr:=-(cbtype.ItemIndex+1)
        else
          definedstructures[selectedstructure.basestructure].structelement[i].structurenr:=cbtype.ItemIndex-15;

        definedstructures[selectedstructure.basestructure].structelement[i].bytesize:=4;
        definedstructures[selectedstructure.basestructure].structelement[i].pointertosize:=bytesize;
      end
      else
      begin
        if cbtype.ItemIndex<=14 then //basetype
        begin
          definedstructures[selectedstructure.basestructure].structelement[i].offset:=base;
          definedstructures[selectedstructure.basestructure].structelement[i].structurenr:=-(cbtype.ItemIndex+1);
          definedstructures[selectedstructure.basestructure].structelement[i].bytesize:=bytesize;
        end
        else
        begin
          //not a pointer, but also no base type, so just append the selected structure
          j:=cbtype.ItemIndex-15;  //j now contains the structure number

          d:=length(definedstructures[j].structelement);
          setlength(definedstructures[selectedstructure.basestructure].structelement,length(definedstructures[currentstructure.basestructure].structelement)+d-1);

          //move the other elements as well
          for k:=length(definedstructures[selectedstructure.basestructure].structelement)-1 downto selectedelement+d+1 do
            definedstructures[selectedstructure.basestructure].structelement[k]:=definedstructures[selectedstructure.basestructure].structelement[k-d+1];

          for k:=0 to length(definedstructures[j].structelement)-1 do
          begin
            definedstructures[selectedstructure.basestructure].structelement[i]:=definedstructures[j].structelement[k];
            definedstructures[selectedstructure.basestructure].structelement[i].description:=edtDescription.text+'_'+definedstructures[j].structelement[k].description;
            inc(definedstructures[selectedstructure.basestructure].structelement[i].offset, base);
            inc(i);
          end;
        end;
      end;

      sortStructure(definedstructures[selectedstructure.basestructure]);
      self.update(true);
      mainform.itemshavechanged:=true;

      if not tvStructureView.Items.GetFirstNode.Expanded then
        tvStructureView.Items.GetFirstNode.Expand(false);
    end;
  end;
end;



procedure TfrmStructures.updatetimerTimer(Sender: TObject);
begin
  if currentstructure<>nil then currentstructure.refresh;
end;

procedure TfrmStructures.tvStructureViewCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  allowcollapse:=not (node=tvStructureView.Items.GetFirstNode);


  currentstructure.refresh;
end;

procedure TfrmStructures.edtAddressChange(Sender: TObject);
begin
  try
    setaddress((sender as TEdit).tag, symhandler.getAddressFromName((sender as TEdit).text));
  except

  end;
end;

procedure TfrmStructures.tvStructureViewExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var s: tstructure;
    elementnr: integer;
    basestruct: integer;
    elementaddress: array of dword;
    i,j: integer;
begin
  AllowExpansion:=true;

  s:=tstructure(node.Data);
  if s=nil then exit;
  if node.getFirstChild<>nil then exit;

  elementnr:=node.Index;

  //structure and element nr are known, so lets see what it is

  basestruct:=s.basestructure;

  setlength(elementaddress,length(addresses));
  for i:=0 to length(addresses)-1 do
  begin
    elementaddress[i]:=addresses[i];
    for j:=0 to elementnr-2 do
      inc(elementaddress[i],definedstructures[basestruct].structelement[j].bytesize);
  end;

  //make sure it's a pointer
  if definedstructures[basestruct].structelement[elementnr].pointerto then
  begin
    s.objects[elementnr].child:=tstructure.create(tvStructureView,node,elementaddress,definedstructures[basestruct].structelement[elementnr].structurenr);

    setlength(s.objects[elementnr].child.addresses,length(currentstructure.addresses));
    for i:=0 to length(currentstructure.addresses)-1 do
      s.objects[elementnr].child.addresses[i]:=currentstructure.addresses[i];
  end;

  currentstructure.refresh;
end;

procedure TfrmStructures.tvStructureViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var tn: ttreenode;
begin
  if button in [mbright, mbleft] then
  begin
    tn:=tvStructureView.GetNodeAt(x,y);
    tvStructureView.Selected:=tn;

    
  end;
end;

procedure TfrmStructures.PopupMenu1Popup(Sender: TObject);
var
  i: integer;
  s: Tstructure;
  elementnr: integer;
begin
  for i:=0 to popupmenu1.Items.Count-1 do
    popupmenu1.Items[i].Visible:=currentstructure<>nil;


  n3.Visible:=(tvStructureView.selected<>nil) and (tvStructureView.selected.Level=1);


  if (tvStructureView.selected<>nil) then
  begin
    s:=tstructure(tvStructureView.Selected.Data);
    if (s<>nil) and (s.basestructure>=0) then
    begin
      Addtoaddresslist1.Visible:=true;
      Deleteelement1.Visible:=true;
      n2.Visible:=true;
      Memorybrowsethisaddress1.Visible:=true;
      elementnr:=tvStructureView.Selected.Index;

      Memorybrowsepointer1.Visible:=(elementnr>=0) and definedstructures[s.basestructure].structelement[elementnr].pointerto;
    end else
    begin
      Deleteelement1.Visible:=false;
      n2.Visible:=false;
      Memorybrowsepointer1.Visible:=false;
      Addtoaddresslist1.Visible:=false;
      Memorybrowsethisaddress1.Visible:=false;

      if (s<>nil) and (s.basestructure<0) then
      begin
        Addelement1.Visible:=false;
        ChangeElement1.Visible:=false;
      end;
        
    end;
  end;


  Recalculateaddress1.visible:=n3.Visible;
end;

procedure TfrmStructures.Deleteelement1Click(Sender: TObject);
var s: tstructure;
    elementnr: integer;
    i: integer;
begin
  if currentstructure=nil then exit;

  if tvStructureView.Selected<>nil then
  begin
    elementnr:=tvStructureView.Selected.Index;
    s:=tstructure(tvStructureView.Selected.Data);

    if s=nil then exit;
    if s.basestructure<0 then exit;

    if messagedlg('Are you sure you want to delete '+definedstructures[s.basestructure].structelement[elementnr].description+'?', mtconfirmation, [mbyes,mbno], 0) <>mryes then exit;

    if tvStructureView.Selected.HasChildren then
      tvStructureView.Selected.Collapse(true);

    for i:=elementnr to length(definedstructures[s.basestructure].structelement)-2 do
      definedstructures[s.basestructure].structelement[i]:=definedstructures[s.basestructure].structelement[i+1];

    setlength(definedstructures[s.basestructure].structelement,length(definedstructures[s.basestructure].structelement)-1);
    mainform.itemshavechanged:=true;
  end;

  update(true);
end;

procedure TfrmStructures.Save1Click(Sender: TObject);
var f: tfilestream;
    i,j: integer;
    x: dword;
    cemarker: string;

    doc: TXMLDocument;
    CheatTable: IXMLNode;
    Structures: IXMLNode;
begin
  if savedialog1.Execute then
  begin
    if uppercase(ExtractFileExt(savedialog1.FileName))='.CSX' then
    begin
      //save as xml
      doc:=TXMLDocument.Create(application);
      try
        doc.Options:=doc.Options+[doNodeAutoIndent];
        doc.Active:=true;

        CheatTable:=doc.AddChild('CheatTable');
        if length(definedstructures)>0 then
        begin
          Structures:=CheatTable.AddChild('Structures');
          for i:=0 to length(definedstructures)-1 do
            SaveStructToXMLNode(definedstructures[i],Structures);
        end;

        doc.SaveToFile(savedialog1.filename);
      finally
        doc.Free;
      end;
    end;

    if uppercase(ExtractFileExt(savedialog1.FileName))='.CES' then
    begin
      f:=tfilestream.Create(savedialog1.FileName,fmcreate);
      try
        cemarker:='CHEATENGINE';
        f.WriteBuffer(cemarker[1],length(cemarker));

        x:=structureversion;
        f.writebuffer(x,4);

        x:=length(definedstructures);
        f.WriteBuffer(x,4);
        for i:=0 to length(definedstructures)-1 do
        begin
          x:=length(definedstructures[i].name);
          f.WriteBuffer(x,4); //namelength
          if x>0 then f.WriteBuffer(definedstructures[i].name[1],x);

          x:=length(definedstructures[i].structelement);
          f.WriteBuffer(x,4);

          for j:=0 to length(definedstructures[i].structelement)-1 do
          begin
            x:=length(definedstructures[i].structelement[j].description);
            f.WriteBuffer(x,4);
            if x>0 then f.Write(definedstructures[i].structelement[j].description[1],x);


            f.WriteBuffer(definedstructures[i].structelement[j].pointerto,sizeof(definedstructures[i].structelement[j].pointerto));
            f.WriteBuffer(definedstructures[i].structelement[j].pointertoSize,sizeof(definedstructures[i].structelement[j].pointerto));
            f.WriteBuffer(definedstructures[i].structelement[j].structurenr,sizeof(definedstructures[i].structelement[j].structurenr));
            f.WriteBuffer(definedstructures[i].structelement[j].bytesize,sizeof(definedstructures[i].structelement[j].bytesize));
          end;

        end;
      finally
        f.free;
      end;
    end;    
  end;
end;

procedure TfrmStructures.Open1Click(Sender: TObject);
var f: tfilestream;
    i,j: integer;
    startindex: integer;
    x: dword;
    cemarker: string;
    c: pchar;
    s: string;
    oldsize: integer;
    structures, structure: IXMLNode;
    CheatTable: IXMLNode;
    doc: TXMLDocument;
begin
  if opendialog1.Execute then
  begin
    if (uppercase(ExtractFileExt(OpenDialog1.FileName))='.CSX') or (uppercase(ExtractFileExt(OpenDialog1.FileName))='.XML') then
    begin
      oldsize:=length(definedstructures);
      doc:=TXMLDocument.Create(application);
      try
        doc.FileName:=opendialog1.filename;
        doc.active:=true;
        CheatTable:=doc.ChildNodes.FindNode('CheatTable'); //because I made it compatible with a ct
        if cheattable<>nil then
        begin
          Structures:=cheattable.ChildNodes.FindNode('Structures');
          if Structures<>nil then
          begin

            setlength(definedstructures,length(definedstructures)+Structures.ChildNodes.Count);
            try
              for i:=0 to Structures.ChildNodes.Count-1 do
              begin
                Structure:=Structures.ChildNodes[i];
                LoadStructFromXMLNode(definedstructures[oldsize+i], structure);
              end;
            except
              setlength(definedstructures,oldsize);
              raise exception.Create('This is not a valid structure file');
            end;
          end;
        end;
        update(true);
      finally
        doc.free;
      end;
    end
    else
    if uppercase(ExtractFileExt(OpenDialog1.FileName))='.CES' then
    begin
      //old 5.4 CES
      f:=tfilestream.Create(opendialog1.FileName,fmopenread);
      try
        cemarker:='CHEATENGINE';

        getmem(c,12);
        try
          f.ReadBuffer(c^,11);
          c[11]:=#0;
          s:=c;
        finally
          freemem(c);
        end;

        if s<>cemarker then raise exception.Create('This is not a valid structure file');

        f.ReadBuffer(x,4);
        if x<>structureversion then raise exception.Create('This structure fils was generated with a newer version of Cheat Engine. (That means there''s more than likely a new version so please update....)');

        startindex:=length(definedstructures);

        f.ReadBuffer(x,4);
        setlength(definedstructures,length(definedstructures)+x);

        for i:=startindex to length(definedstructures)-1 do
        begin
          f.readbuffer(x,4);
          getmem(c,x+1);
          try
            f.ReadBuffer(c^,x);
            c[x]:=#0;
            definedstructures[i].name:=c;
          finally
            freemem(c);
          end;

          f.readbuffer(x,4);
          setlength(definedstructures[i].structelement,x);

          for j:=0 to length(definedstructures[i].structelement)-1 do
          begin
            f.readbuffer(x,4);
            getmem(c,x+1);
            try
              f.ReadBuffer(c^,x);
              c[x]:=#0;
              definedstructures[i].structelement[j].description:=c;
            finally
              freemem(c);
            end;

            f.ReadBuffer(definedstructures[i].structelement[j].pointerto,sizeof(definedstructures[i].structelement[j].pointerto));
            f.ReadBuffer(definedstructures[i].structelement[j].pointertoSize,sizeof(definedstructures[i].structelement[j].pointerto));
            f.ReadBuffer(definedstructures[i].structelement[j].structurenr,sizeof(definedstructures[i].structelement[j].structurenr));
            f.ReadBuffer(definedstructures[i].structelement[j].bytesize,sizeof(definedstructures[i].structelement[j].bytesize));
          end;

        end;
      finally
        f.free;
      end;

      TMenuItem.Create(self);


      update(true);
    end else raise exception.create('Unkown file extension');
  end;
end;

procedure TfrmStructures.New1Click(Sender: TObject);
begin
  if (length(definedstructures)>0) and (messagedlg('Are you sure you want to remove all structures?',mtconfirmation,[mbyes,mbno],0)=mryes) then
  begin
    currentstructure.Free;
    currentstructure:=nil;
    
    setlength(definedstructures,0);
    refreshmenuitems;
  end;

  Definenewstructure1.Click;
end;

procedure TfrmStructures.ChangeElement1Click(Sender: TObject);
var i,j: integer;
    size: dword;
    structtype: string;
    ts,selectedstructure: tstructure;
    selectedelement: integer;
    selectednode: ttreenode;
begin
  if currentstructure=nil then exit;
  
  if tvStructureView.Selected=tvStructureView.Items.GetFirstNode then
  begin
    renamestructure1.Click;
    exit;
  end;


  selectednode:=tvStructureView.Selected;
  if selectednode=nil then exit;

  selectedstructure:=tstructure(selectednode.Data);
  if selectedstructure=nil then exit;

  selectedelement:=selectednode.Index;

  i:=selectedstructure.basestructure;
  if i<0 then exit;
  
  size:=definedstructures[i].structelement[selectedelement].bytesize;

  with tfrmstructuresaddelement.create(self) do
  begin
    //fill the combobox with possible types
    //the base types, and defined types
    cbtype.Items.AddObject('Byte',pointer(1));
    cbtype.Items.AddObject('Byte Signed',pointer(1));
    cbtype.Items.AddObject('Byte Hexadecimal',pointer(1));
    cbtype.Items.AddObject('2 Bytes',pointer(2));
    cbtype.Items.AddObject('2 Bytes Signed',pointer(2));
    cbtype.Items.AddObject('2 Bytes Hexadecimal',pointer(2));
    cbtype.Items.AddObject('4 Bytes',pointer(4));
    cbtype.Items.AddObject('4 Bytes Signed',pointer(4));
    cbtype.Items.AddObject('4 Bytes Hexadecimal',pointer(4));
    cbtype.Items.AddObject('8 Bytes',pointer(8));
    cbtype.Items.AddObject('8 Bytes Hexadecimal',pointer(8));
    cbtype.Items.AddObject('Float',pointer(4));
    cbtype.Items.AddObject('Double',pointer(8));
    cbtype.Items.AddObject('String',pointer(size));
    cbtype.Items.AddObject('String Unicode',pointer(size));

    cbtype.DropDownCount:=17;

    //and add the other defined structures as well
    for i:=0 to length(definedstructures)-1 do
    begin
      size:=0;
      for j:=0 to length(definedstructures[i].structelement)-1 do
        inc(size,definedstructures[i].structelement[j].bytesize);

      cbtype.Items.AddObject(definedstructures[i].name,pointer(size));
    end;

    if definedstructures[selectedstructure.basestructure].structelement[selectedelement].structurenr<0 then
      cbtype.ItemIndex:=-definedstructures[selectedstructure.basestructure].structelement[selectedelement].structurenr-1
    else
      cbtype.itemindex:=definedstructures[selectedstructure.basestructure].structelement[selectedelement].structurenr+15;


    edtDescription.text:=definedstructures[selectedstructure.basestructure].structelement[selectedelement].description;
    cbpointerto.checked:=definedstructures[selectedstructure.basestructure].structelement[selectedelement].pointerto;
    if cbpointerto.Checked then
      edtByteSize.Text:=inttostr(definedstructures[selectedstructure.basestructure].structelement[selectedelement].pointertosize);
//    else
//      edtByteSize.Text

    edtOffset.Text:=inttohex(definedstructures[selectedstructure.basestructure].structelement[selectedelement].offset,1);

    cbType.OnChange(cbType);

    if showmodal=mrok then
    begin
      definedstructures[selectedstructure.basestructure].structelement[selectedelement].description:=edtDescription.text;
      definedstructures[selectedstructure.basestructure].structelement[selectedelement].pointerto:=cbpointerto.checked;
      definedstructures[selectedstructure.basestructure].structelement[selectedelement].pointertosize:=bytesize;
      definedstructures[selectedstructure.basestructure].structelement[selectedelement].offset:=strtoint('$'+edtOffset.text);

      if cbtype.itemindex<=14 then
        definedstructures[selectedstructure.basestructure].structelement[selectedelement].structurenr:=-(cbtype.ItemIndex+1)
      else
        definedstructures[selectedstructure.basestructure].structelement[selectedelement].structurenr:=cbtype.ItemIndex-15;

      if definedstructures[selectedstructure.basestructure].structelement[selectedelement].pointerto then
        definedstructures[selectedstructure.basestructure].structelement[selectedelement].bytesize:=4
      else
        definedstructures[selectedstructure.basestructure].structelement[selectedelement].bytesize:=bytesize;

      if tvStructureView.Selected.HasChildren then
      begin
        tvStructureView.Selected.DeleteChildren;
        freeandnil(selectedstructure.objects[selectedelement].child);
      end;

      tvStructureView.Selected.Collapse(true);

      currentstructure.refresh;

      sortstructure(definedstructures[selectedstructure.basestructure]);

      self.update(true);
      mainform.itemshavechanged:=true;


    end;
  end;

end;

procedure TfrmStructures.tvStructureViewDblClick(Sender: TObject);
var
  selectedstructure: tstructure;
  selectednode: ttreenode;
  selectedelement: integer;
  i: integer;
  a: dword;
  cursorpos: tpoint;
  tvrect: trect;
  selectedsection: integer;
begin
  //find out what part is doubleclicked

  //find the position that is clicked
  cursorpos:=mouse.CursorPos;
  GetWindowRect(TTreeview(sender).Handle, tvrect);

  cursorpos.X:=cursorpos.X-tvrect.Left;
  cursorpos.Y:=cursorpos.Y-tvrect.Top;
  //now find out which section this X belongs to

  selectedsection:=headercontrol1.Sections.Count-1; //default pick the last one
  for i:=0 to headercontrol1.Sections.Count-1 do
    if (cursorpos.X>headercontrol1.Sections[i].Left) and (cursorpos.X<(headercontrol1.Sections[i].Left+headercontrol1.Sections[i].width)) then
    begin
      selectedsection:=i;
      break;
    end;

  if selectedsection=0 then
  begin
    ChangeElement1.Click;
    exit;
  end;

  

  selectednode:=tvStructureView.Selected;
  if selectednode<>nil then
  begin
    selectedstructure:=tstructure(selectednode.Data);
    if (selectedstructure<>nil) and (selectedstructure.basestructure>=0) then
    begin
      selectedelement:=selectednode.Index;
      a:=selectedstructure.addresses[selectedsection-1];
      inc(a, definedstructures[selectedstructure.basestructure].structelement[selectedelement].offset);

      with Tvaluechangeform.Create(application) do
      begin
        address:=a;

        case definedstructures[selectedstructure.basestructure].structelement[selectedelement].structurenr of
          -1,-2,-3: vtype:=0;
          -4,-5,-6: vtype:=1;
          -7,-8,-9: vtype:=2;
          -10,-11: vtype:=6;
          -12: vtype:=4;
          -13: vtype:=5;
          -14:
            begin
              slength:=definedstructures[selectedstructure.basestructure].structelement[selectedelement].bytesize;
              unicode:=false;
              vtype:=7;
            end;

          -15:
            begin
              unicode:=true;
              slength:=definedstructures[selectedstructure.basestructure].structelement[selectedelement].bytesize div 2;
              vtype:=7;
            end;

          else vtype:=0;
        end;


        ShowModal;
        self.update(true);
      end;
    end;
  end;
end;

procedure TfrmStructures.Addtoaddresslist1Click(Sender: TObject);
var
  selectedstructure: tstructure;
  selectednode: ttreenode;
  selectedelement: integer;
  offsets: array of dword;

  objectname: string;

  snr: integer;
  vtype: integer;
  vlength: integer;
  unicode,ispointer,showashex: boolean;

  i: integer;

  tvrect: trect;
  clickpos: tpoint;
  section: integer;
begin
  if currentstructure=nil then exit;

  GetWindowRect(tvStructureView.Handle, tvrect);

  clickpos.X:=popupmenu1.PopupPoint.X-tvrect.Left;
  clickpos.Y:=popupmenu1.PopupPoint.Y-tvrect.Top;

  section:=headercontrol1.Sections.Count-1;
  for i:=0 to headercontrol1.Sections.Count-1 do
  begin
    if (clickpos.x>=headercontrol1.Sections[i].Left)
      and
       (clickpos.x<headercontrol1.Sections[i].Left+headercontrol1.Sections[i].width) then
    begin
      //found it
      section:=i;
      break;
    end;
  end;

  if section>0 then
    section:=section-1; //count starts from 1, so decrease

  
  showashex:=false;
  selectednode:=tvStructureView.Selected;
  if selectednode<>nil then
  begin
    selectedstructure:=tstructure(selectednode.Data);

    if selectedstructure<>nil then
    begin
      if selectedstructure.basestructure>=0 then
      case definedstructures[selectedstructure.basestructure].structelement[selectednode.Index].structurenr of
        -1,-2,-3: vtype:=0;
        -4,-5,-6: vtype:=1;
        -7,-8,-9: vtype:=2;
        -10,-11: vtype:=6;
        -12: vtype:=3;
        -13: vtype:=4;
        -14: begin
               vtype:=7;
               vlength:=definedstructures[selectedstructure.basestructure].structelement[selectednode.Index].bytesize;
               unicode:=false;
             end;

        -15: begin
               vtype:=8;
               vlength:=definedstructures[selectedstructure.basestructure].structelement[selectednode.Index].bytesize div 2;
               unicode:=true;
             end;

        0..maxint :
             begin
               vtype:=2;
               showashex:=true;
             end;
      end else
      case selectedstructure.basestructure of
        -1,-2,-3: vtype:=0;
        -4,-5,-6: vtype:=1;
        -7,-8,-9: vtype:=2;
        -10,-11: vtype:=6;
        -12: vtype:=3;
        -13: vtype:=4;
        -14: begin
               vtype:=7;
               vlength:=4;
               unicode:=false;
             end;

        -15: begin
               vtype:=8;
               vlength:=4;
               unicode:=true;
             end;

        0..maxint : exit;
      end

    end else exit;

    objectname:='';
    setlength(offsets,0);
    while (selectedstructure<>nil) do
    begin
      //get the offsets for each structure till you get to the base address
      selectedelement:=selectednode.Index;
      snr:=selectedstructure.basestructure;

      setlength(offsets,length(offsets)+1);
      offsets[length(offsets)-1]:=0;

      if snr>=0 then
      begin
        for i:=0 to selectedelement-1 do
          inc(offsets[length(offsets)-1],definedstructures[snr].structelement[i].bytesize);
      end;

      selectednode:=selectednode.Parent;
      if selectednode<>nil then
        selectedstructure:=tstructure(selectednode.data)
      else
        break;
    end;


    //now add it to the list
    mainform.addaddress('bla',addresses[section]+offsets[length(offsets)-1],offsets[0],length(offsets)-1,length(offsets)>1,vtype,vlength,0,unicode,showashex);
    mainform.itemshavechanged:=true;
  end;
end;

procedure TfrmStructures.Recalculateaddress1Click(Sender: TObject);
var a: string;
    oldaddress,newaddress: dword;

    selectedstructure: tstructure;
    selectednode: ttreenode;
    selectedelement,snr: integer;
    i: integer;
    delta: integer;
    tvrect: trect;
    clickpos: tpoint;
    section: integer;
begin
  if currentstructure=nil then exit;

  GetWindowRect(tvStructureView.Handle, tvrect);

  clickpos.X:=popupmenu1.PopupPoint.X-tvrect.Left;
  clickpos.Y:=popupmenu1.PopupPoint.Y-tvrect.Top;

  section:=headercontrol1.Sections.Count-1;
  for i:=0 to headercontrol1.Sections.Count-1 do
  begin
    if (clickpos.x>=headercontrol1.Sections[i].Left)
      and
       (clickpos.x<headercontrol1.Sections[i].Left+headercontrol1.Sections[i].width) then
    begin
      //found it
      section:=i;
      break;
    end;
  end;

  if section>0 then
    section:=section-1; //count starts from 1, so decrease

      
  selectednode:=tvStructureView.Selected;
  if selectednode<>nil then
  begin
    selectedstructure:=tstructure(selectednode.Data);
    selectedelement:=selectednode.Index;
    snr:=selectedstructure.basestructure;

    oldaddress:=addresses[section];

    inc(oldaddress,definedstructures[snr].structelement[selectedelement].offset);

    a:=inttohex(memorybrowser.memoryaddress,8);
    if inputquery('Recalculate base of structure','Give the address of this element',a) then
    begin
      try
        newaddress:=strtoint('$'+a);
      except
        raise exception.Create('I have no idea what '+a+' means');
      end;

      delta:=newaddress-oldaddress;
      addresses[section]:=addresses[section]+delta;
      edits[section].text:=inttohex(addresses[section],8);
      update(true);

      mainform.itemshavechanged:=true;
    end;
  end;
end;

procedure TfrmStructures.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TfrmStructures.HeaderControl1SectionResize(
  HeaderControl: THeaderControl; Section: THeaderSection);
var x: integer;
begin
  x:=(HeaderControl1.Sections[HeaderControl1.Sections.Count-1].Left+HeaderControl1.Sections[HeaderControl1.Sections.Count-1].Width);
  scrollbox1.HorzScrollBar.Range:=x;

  tvStructureView.refresh;
  ShowScrollBar(tvStructureView.Handle,SB_HORZ		, false);
end;

procedure TfrmStructures.FormCreate(Sender: TObject);
begin
  setlength(groups,1);
  setlength(addresses,1);
  setlength(edits,1);
  edits[0]:=edtAddress;
  groups[0]:=0;
  lastnewedit:=edtaddress;
  edtAddress.OnEnter:=extraenter;

  setlength(frmStructures,length(frmStructures)+1);
  frmStructures[length(frmStructures)-1]:=self;

  UpdateGroupIndex;
end;

procedure TfrmStructures.ExtraEnter(Sender: TObject);
begin
  popupmenu2.PopupComponent:=TComponent(sender);
end;

procedure TfrmStructures.Addextraaddress1Click(Sender: TObject);
var x: tedit;
    newsection: THeadersection;
begin
  x:=tedit.Create(self);
  with x do
  begin
    left:=lastnewedit.Left+lastnewedit.Width+16;
    top:=lastnewedit.Top;
    width:=lastnewedit.Width;
    text:=lastnewedit.Text;
    parent:=lastnewedit.Parent;

    x.PopupMenu:=PopupMenu2;
    x.OnEnter:=extraenter;
    x.OnChange:=edtAddressChange;
    x.Tag:=lastnewedit.Tag+1;

  end;

  setlength(addresses,length(addresses)+1);
  addresses[length(addresses)-1]:=addresses[x.tag-1];

  setlength(groups,length(groups)+1);
  groups[length(groups)-1]:=0;
  UpdateGroupIndex;


    
  setlength(edits,length(edits)+1);

  edits[length(edits)-1]:=x;
  lastnewedit:=x;

  newsection:=headercontrol1.Sections.Add;
  newsection.Text:='Address: Value';
  newsection.Width:=200;
  newsection.MinWidth:=20;
  edtAddressChange(x);  
end;

procedure TfrmStructures.Remove1Click(Sender: TObject);
var x: tedit;
    i: integer;
begin
  x:=TEdit(popupmenu2.PopupComponent);
  if x.tag=0 then exit; //can't remove the first one

  for i:=x.tag to length(edits)-2 do
  begin
    groups[i]:=groups[i+1];
    edits[i]:=edits[i+1];
    edits[i].Left:=edits[i].Left-edits[i].Width-16;
    edits[i].Tag:=i;
  end;
  setlength(edits,length(edits)-1);

  lastnewedit:=edits[length(edits)-1];

  if currentstructure<>nil then
    currentstructure.removeAddress(x.tag);

  UpdateGroupIndex;
  
  x.free;
end;

procedure TfrmStructures.Undo1Click(Sender: TObject);
begin
  TEdit(popupmenu2.PopupComponent).Undo;
end;

procedure TfrmStructures.Cut1Click(Sender: TObject);
begin
  TEdit(popupmenu2.PopupComponent).CutToClipboard;
end;



procedure TfrmStructures.Copy1Click(Sender: TObject);
begin
  TEdit(popupmenu2.PopupComponent).CopyToClipboard;
end;

procedure TfrmStructures.Paste1Click(Sender: TObject);
begin
  TEdit(popupmenu2.PopupComponent).PasteFromClipboard;
end;

procedure TfrmStructures.SelectAll1Click(Sender: TObject);
begin
  TEdit(popupmenu2.PopupComponent).SelectAll;
end;




procedure TfrmStructures.tvStructureViewAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
{
multigroup usage:
If all entries of the same group are the same, mark them green, otherwhise red
If the value of another group does not match the value of the first group, mark it red
}
var  
  i,j: integer;
  laststart: integer;
  textrect: trect;
  linerect: trect;
  textlinerect: trect;
  fulltextline: trect;
  totalsections: integer;
  sections: array of string;
  sections2: array of string;
  currentsection: integer;
  oldcolor: tcolor;

  clip: trect;

  currentGroup: integer;
  groupmatches: array of boolean;
  groupcolors: array of tcolor;
  groupvalues: array of string;
begin
  //looks like it's even called before create is done...
  if stage=cdPostPaint then
  begin
    textrect:=node.DisplayRect(true);
    linerect:=node.DisplayRect(false);

    fulltextline:=linerect;
    fulltextline.Left:=textrect.Left;
    oldcolor:=sender.Canvas.Brush.Color;
    sender.Canvas.Brush.color:=tvStructureView.color;
    sender.Canvas.FillRect(fulltextline); //whipe the original text

    if headercontrol1=nil then exit;
    totalsections:=headercontrol1.Sections.Count;
    setlength(sections,totalsections);
    setlength(sections2,totalsections);
    currentsection:=0;


    setlength(groupvalues,length(groupindex));
    setlength(groupcolors,length(groupindex));
    setlength(groupmatches,length(groupindex));
    for i:=0 to length(groupcolors)-1 do
    begin
      groupcolors[i]:=clGreen;
      groupmatches[i]:=true;
    end;

    laststart:=1;
    //search for seperators (#13)
    for i:=1 to length(node.Text) do
      if node.Text[i]=#13 then
      begin
        //found one
        sections[currentsection]:=copy(node.text,laststart,i-laststart);
        sections2[currentsection]:= copy(sections[currentsection],pos(':',sections[currentsection]),length(sections[currentsection]));
        laststart:=i+1;
        inc(currentsection);
        if (currentsection>=totalsections) then
          break; //enough, if there is a rest, it has to be a bug
      end;

    //go through the values
    for i:=1 to length(sections2)-1 do
    begin
      currentGroup:=internalgrouplist[i-1];

      if groupvalues[currentGroup]='' then
        groupvalues[currentGroup]:=sections2[i]
      else
      begin
        if groupvalues[currentGroup]<>sections2[i] then
        begin
          groupcolors[currentGroup]:=clred;
          groupmatches[currentGroup]:=false;
        end;
      end;
    end;

    for i:=1 to length(groupvalues)-1 do
    begin
      if (groupmatches[i]) and (groupmatches[i-1]) then //both groups match
      begin
        if groupvalues[i-1]<>groupvalues[i] then  //but the values don't match with the previous group
          groupcolors[i]:=clBlue;
      end;
    end;


    //if laststart=1 then
    //  sections[currentsection]:=node.text;

    textlinerect.left:=textrect.left;
    textlinerect.Top:=linerect.Top;
    textlinerect.Right:=headercontrol1.Sections.Items[headercontrol1.Sections.Count-1].Left+sender.Canvas.textwidth(sections[totalsections-1]);
    textlinerect.Bottom:=linerect.Bottom;
    if textlinerect.right<textlinerect.left then
      textlinerect.right:=textlinerect.left;

    sender.Canvas.Refresh;
    if not (cdsSelected in State) then
    begin
      sender.Canvas.Brush.Style:=bsSolid;
      sender.Canvas.Brush.Color:=tvStructureView.Color;
      sender.Canvas.FillRect(textlinerect);
//      if different then
//        tvStructureView.canvas.Font.Color:=clRed
//      else
//        tvStructureView.canvas.Font.Color:=clWindowText;

    end
    else
    begin
      sender.Canvas.Brush.Style:=bsSolid;
      sender.Canvas.Brush.Color:=clHighlight;
      sender.Canvas.FillRect(textlinerect);
      sender.Canvas.DrawFocusRect(textlinerect);
//      if different then
//      begin
//        tvStructureView.canvas.Font.Color:=clRed;
//        tvStructureView.canvas.Font.Style:=[fsBold];
//      end
//      else
//        tvStructureView.canvas.Font.Color:=clHighlightText;

    end;

    sender.Canvas.Refresh;

    clip:=textrect;
    clip.Right:=headercontrol1.Sections[0].Left+headercontrol1.Sections[0].Width;
    sender.Canvas.TextRect(clip,textrect.Left,textrect.Top,sections[0]);


    sender.Canvas.Refresh;
    for i:=1 to totalsections-1 do
    begin
      currentGroup:=internalgrouplist[i-1];
      tvStructureView.canvas.Font.Color:=groupcolors[currentgroup];

      if (cdsSelected in State) then
      begin
        case groupcolors[currentgroup] of
          clGreen: sender.Canvas.font.Color:=clBlack;
          clRed: sender.Canvas.font.Color:=clMaroon;
          clBlue: sender.Canvas.font.Color:=clNavy;
        end;

      end;

      clip.Left:=headercontrol1.Sections[i].Left;
      clip.Right:=headercontrol1.Sections[i].Left+headercontrol1.Sections[i].Width;
      sender.Canvas.TextRect(clip, headercontrol1.Sections[i].Left+(node.Level-1)*tvStructureView.Indent,textrect.Top,sections[i]);
      sender.Canvas.Refresh;
    end;
  end;

  DefaultDraw:=true;
  ShowScrollBar(tvStructureView.Handle,SB_HORZ		, false);
end;

procedure TfrmStructures.FormDestroy(Sender: TObject);
var i,j: integer;
begin
  //remove from the list
  for i:=0 to length(frmStructures)-1 do
    if frmStructures[i]=self then
    begin
      //found it, now move the rest and shink the array
      for j:=i to length(frmStructures)-2 do
        frmStructures[j]:=frmStructures[j+1];

      setlength(frmStructures,length(frmStructures)-1);
      exit;
    end;
end;

procedure TfrmStructures.PopupMenu2Popup(Sender: TObject);
var x: tedit;
begin
  x:=TEdit(popupmenu2.PopupComponent);
  Remove1.Visible:=(x<>nil) and (x.tag<>0);
  n6.Visible:=remove1.Visible;

  setgroup1.Caption:='Change group ('+inttostr(groups[x.tag])+')';
end;

procedure TfrmStructures.Renamestructure1Click(Sender: TObject);
begin
  if currentstructure<>nil then
  begin
    inputquery('Rename structure','Give the new name of this structure',definedstructures[currentstructure.basestructure].name);
    update(true);
  end;
end;

procedure TfrmStructures.Deletecurrentstructure1Click(Sender: TObject);
var i,j: integer;
begin
  if MessageDlg('Are you sure you want to delete '+definedstructures[currentstructure.basestructure].name+'?',mtConfirmation, [mbyes,mbno],0)=mryes then
  begin
    //remove all children that make use of this structnr
    //and move all children that point to higher numbered ones
    for i:=0 to length(definedstructures)-1 do
      for j:=0 to length(definedstructures[i].structelement)-1 do
      begin
        if definedstructures[i].structelement[j].structurenr=currentstructure.basestructure then
          definedstructures[i].structelement[j].structurenr:=-7;

        if definedstructures[i].structelement[j].structurenr>currentstructure.basestructure then
          dec(definedstructures[i].structelement[j].structurenr);

        if definedstructures[i].structelement[j].structurenr=0 then
          definedstructures[i].structelement[j].structurenr:=-7;
      end;

    for i:=currentstructure.basestructure to length(definedstructures)-2 do
      definedstructures[i]:=definedstructures[i+1];

    setlength(definedstructures,length(definedstructures)-1);
    freeandnil(currentstructure);

    update(true);
  end;
end;

procedure TfrmStructures.Newwindow1Click(Sender: TObject);
begin
  with tfrmstructures.create(application) do
  begin
    edtAddress.Text:=inttohex(memorybrowser.memoryaddress,8);
    update(false);
    show;
  end;
end;

procedure TfrmStructures.Memorybrowsepointer1Click(Sender: TObject);
var
  i: integer;
  s: Tstructure;
  elementnr: integer;
  tvrect: trect;
  clickpos: tpoint;
  section: integer;
  address: dword;
  x: dword;
begin
  if (tvStructureView.selected<>nil) then
  begin
    s:=tstructure(tvStructureView.Selected.Data);
    if s=nil then exit;
    if s.basestructure<0 then exit;

    elementnr:=tvStructureView.Selected.Index;

    if (elementnr>=0) then
    begin
      //find the position that is clicked
      GetWindowRect(tvStructureView.Handle, tvrect);

      clickpos.X:=popupmenu1.PopupPoint.X-tvrect.Left;
      clickpos.Y:=popupmenu1.PopupPoint.Y-tvrect.Top;

      section:=headercontrol1.Sections.Count-1;
      for i:=0 to headercontrol1.Sections.Count-1 do
      begin
        if (clickpos.x>=headercontrol1.Sections[i].Left)
          and
           (clickpos.x<headercontrol1.Sections[i].Left+headercontrol1.Sections[i].width) then
        begin
          //found it
          section:=i;
          break;
        end;
      end;

      if section>0 then
        section:=section-1; //count starts from 1, so decrease

      if readprocessmemory(processhandle, pointer(s.addresses[section]+definedstructures[s.basestructure].structelement[elementnr].offset), @address,4,x) then
      begin
        memorybrowser.memoryaddress:=address;
        memorybrowser.RefreshMB;
      end;

    end;
   // definedstructures[s.basestructure].structelement[elementnr].
  end;
end;

procedure TfrmStructures.Memorybrowsethisaddress1Click(Sender: TObject);
var
  i: integer;
  s: Tstructure;
  elementnr: integer;
  tvrect: trect;
  clickpos: tpoint;
  section: integer;
  address: dword;
  x: dword;
begin
  if (tvStructureView.selected<>nil) then
  begin
    s:=tstructure(tvStructureView.Selected.Data);
    elementnr:=tvStructureView.Selected.Index;
    if s=nil then exit;


    if (elementnr>=0) then
    begin
      //find the position that is clicked
      GetWindowRect(tvStructureView.Handle, tvrect);

      clickpos.X:=popupmenu1.PopupPoint.X-tvrect.Left;
      clickpos.Y:=popupmenu1.PopupPoint.Y-tvrect.Top;

      section:=headercontrol1.Sections.Count-1;
      for i:=0 to headercontrol1.Sections.Count-1 do
      begin
        if (clickpos.x>=headercontrol1.Sections[i].Left)
          and
           (clickpos.x<headercontrol1.Sections[i].Left+headercontrol1.Sections[i].width) then
        begin
          //found it
          section:=i;
          break;
        end;
      end;

      if section>0 then
        section:=section-1; //count starts from 1, so decrease

      if s.basestructure>=0 then
      begin
        memorybrowser.memoryaddress:=s.addresses[section]+definedstructures[s.basestructure].structelement[elementnr].offset;
        memorybrowser.RefreshMB;
      end;
    end;
   // definedstructures[s.basestructure].structelement[elementnr].
  end;


end;

procedure TfrmStructures.automaticallyGuessOffsets(baseOffset: dword; structsize: integer);
var
  buf: array of byte;
  buf2: array of byte;
  i,j,t: integer;
  t2: TVariableType;
  x,y: dword;
begin
    setlength(buf,structsize);
    setlength(buf2,8);
    //now read the memory
    if readprocessmemory(processhandle,pointer(addresses[0]+baseOffset),@buf[0],structsize,x) then
    begin
      x:=0;
      while x<structsize do
      begin
        i:=length(definedstructures[length(definedstructures)-1].structelement);
        setlength(definedstructures[length(definedstructures)-1].structelement,i+1);
        definedstructures[length(definedstructures)-1].structelement[i].offset:=baseoffset+x;
        definedstructures[length(definedstructures)-1].structelement[i].pointerto:=false;

        //value
        //check what type it is
        t2:=FindTypeOfData(addresses[0]+baseoffset+x,@buf[x],structsize-x);
        if t2=vtPointer then
        begin
          //pointer


          definedstructures[length(definedstructures)-1].structelement[i].pointerto:=true;
          definedstructures[length(definedstructures)-1].structelement[i].pointertoSize:=8;
          definedstructures[length(definedstructures)-1].structelement[i].bytesize:=4;
          definedstructures[length(definedstructures)-1].structelement[i].description:='pointer to ';

          if readprocessmemory(processhandle,pointer(pdword(@buf[x])^),@buf2[0],8,y) then
          begin
            t2:=FindTypeOfData(pdword(@buf[x])^,@buf2[0],8);
            t:=convertVariableTypeTostructnr(t2);
            definedstructures[length(definedstructures)-1].structelement[i].structurenr:=t;
          end
          else definedstructures[length(definedstructures)-1].structelement[i].structurenr:=-9;

          inc(x,4);
          continue;
        end;


        t:=convertVariableTypeTostructnr(t2);



        definedstructures[length(definedstructures)-1].structelement[i].structurenr:=t;



        case t of
          -1: //byte
          begin
            definedstructures[length(definedstructures)-1].structelement[i].description:='Byte';
            definedstructures[length(definedstructures)-1].structelement[i].bytesize:=1;
            inc(x,1);
          end;

          -4: //word
          begin
            definedstructures[length(definedstructures)-1].structelement[i].description:='Word';
            definedstructures[length(definedstructures)-1].structelement[i].bytesize:=2;
            inc(x,2);
          end;

          -7: //dword
          begin
            definedstructures[length(definedstructures)-1].structelement[i].description:='Dword';
            definedstructures[length(definedstructures)-1].structelement[i].bytesize:=4;
            inc(x,4);
          end;

          -12: //single
          begin
            definedstructures[length(definedstructures)-1].structelement[i].description:='Float';
            definedstructures[length(definedstructures)-1].structelement[i].bytesize:=4;
            inc(x,4);
          end;

          -13: //double
          begin
            definedstructures[length(definedstructures)-1].structelement[i].description:='Double';
            definedstructures[length(definedstructures)-1].structelement[i].bytesize:=8;
            inc(x,8);
          end;

          -14: //string
          begin
            definedstructures[length(definedstructures)-1].structelement[i].description:='String';

            //find out how long this string is:
            definedstructures[length(definedstructures)-1].structelement[i].bytesize:=0;
            while (x<structsize) and (buf[x]>=32) and (buf[x]<=127) do
            begin
              inc(x);
              inc(definedstructures[length(definedstructures)-1].structelement[i].bytesize);
            end;

            if (x<structsize-1) then
            begin
              //add the zero terminator if one exists
              if buf[x]=0 then
              begin
                inc(x);
                inc(definedstructures[length(definedstructures)-1].structelement[i].bytesize);
              end;
            end;
          end;


        end;
      end;

    end;
end;

procedure TfrmStructures.Autoguessoffsets1Click(Sender: TObject);
var
  sStartOffset: string;
  sStructSize: string;
  base: TbaseStructure;
  startOffset: integer;
  structSize: integer;
begin
  if currentstructure<>nil then
  begin
    base:=definedstructures[currentstructure.basestructure];
    if length(base.structelement)>0 then
      sStartOffset:=inttohex(base.structelement[length(base.structelement)-1].offset+base.structelement[length(base.structelement)-1].bytesize,1)
    else
      sStartOffset:='0';

    if not inputquery('Structure define','Please give a starting offset to evaluate',sStartOffset) then exit;
    startOffset:=StrToInt('$'+sStartOffset);

    sStructSize:='4096';
    if not inputquery('Structure define','Please give the size of the block to evaluate',sStructSize) then exit;
    structSize:=StrToInt(sStructSize);

    automaticallyGuessOffsets(startOffset, structsize);
  end;
end;

procedure  TfrmStructures.UpdateGroupIndex;
var i,j: integer;
    alreadyIndexed: boolean;
begin
  setlength(groupindex,0);
  setlength(internalgrouplist,length(groups));
  for i:=0 to length(groups)-1 do
  begin
    alreadyIndexed:=false;
    for j:=0 to length(groupindex)-1 do
    begin
      if groupindex[j]=groups[i] then
      begin
        alreadyIndexed:=true;
        internalgrouplist[i]:=j;
        break;
      end;
    end;

    if not alreadyIndexed then
    begin
      setlength(groupindex,length(groupindex)+1);
      groupindex[length(groupindex)-1]:=groups[i];
      internalgrouplist[i]:=length(groupindex)-1;
    end;


  end;
end;


procedure TfrmStructures.Setgroup1Click(Sender: TObject);
var
  x: tedit;
  sgroup: string;
begin
  x:=TEdit(popupmenu2.PopupComponent);

  sgroup:=inttostr(groups[x.Tag]);

  InputQuery('Structure definer', 'Which group do you want to set this address to?', sgroup);
  groups[x.Tag]:=strtoint(sgroup);

  updategroupindex;

  tvStructureView.Refresh;
end;

procedure TfrmStructures.Updatethisandfollowingoffsets1Click(
  Sender: TObject);
var
  offsetstring: string;
  offset: integer;
  s: tstructure;
  elementnr: integer;
  i: integer;
begin
  if currentstructure<>nil then
  begin
    offsetstring:='0';
    if InputQuery('Dissect Data','How many bytes do you want to shift this and following offsets?',offsetstring) then
    begin
      offset:=strtoint(offsetstring);

      s:=tstructure(tvStructureView.Selected.Data);
      if s=nil then exit;
      elementnr:=tvStructureView.Selected.Index;

      if (elementnr>=0) then
      begin

        //find the position that is clicked
        for i:=elementnr to length(definedstructures[s.basestructure].structelement)-1 do
          definedstructures[s.basestructure].structelement[i].offset:=integer(definedstructures[s.basestructure].structelement[i].offset)+offset;

        tvStructureView.Refresh;
      end;
    end;
  end;
end;

procedure TfrmStructures.SaveValues1Click(Sender: TObject);
var
  i,j: integer;
  s: string;
  laststart: integer;
  sections,sections2: array of string;
  currentsection: integer;

  f: tstringlist;
begin
  if savevalues.Execute then
  begin
    setlength(sections,headercontrol1.Sections.Count);
    setlength(sections2,headercontrol1.Sections.Count);
    f:=tstringlist.create;

    //save the values from the currently shown structure to a file
    for i:=0 to tvStructureView.Items.Count-1 do
    begin
      if tvStructureView.Items[i].Level=1 then //the selected structure
      begin
        s:=tvStructureView.items[i].Text;
        laststart:=1;
        currentsection:=0;

        //search for seperators (#13)
        for j:=1 to length(s) do
          if s[j]=#13 then
          begin
            //found one
            sections[currentsection]:=copy(s,laststart,j-laststart);
            sections2[currentsection]:= copy(sections[currentsection],pos(':',sections[currentsection]),length(sections[currentsection]));
            laststart:=j+1;
            inc(currentsection);
            if (currentsection>=length(sections2)) then
              break; //enough, if there is a rest, it has to be a bug/string
          end;

        s:='';
        for j:=0 to length(sections2)-1 do
        begin
          while length(sections2[j])<20 do
            sections2[j]:=sections2[j]+' ';


          s:=s+sections2[j];
        end;

        f.Add(s);
      end;
    end;


    f.SaveToFile(savevalues.FileName);

    f.free;
  end;  
end;

end.



