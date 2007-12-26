unit Structuresfrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ExtCtrls, ComCtrls,cefuncproc,newkernelhandler;

const structureversion=1;

type Tbasestucture=record
  name: string;
  structelement: array of record
                            description:string;
                            pointerto: boolean;  //determines if it's a pointer to a structure, or the structure itself
                            pointertosize: dword;
                            structurenr: integer; //-1 and lower=base element   (they can't be both -1)
                            bytesize: dword; //size in bytes of how big this element is. (also for base elements)
                          end;

end;

type Tstructure=class
  private
    treeviewused: ttreeview;
    address:dword;
    basestructure: integer;
    parentnode: ttreenode; //owner of this object
    objects: array of record //same size as the structelement of the base object
                        nodetoupdate: ttreenode; //same size as the structelement of the base object
                        child: tstructure; //if it is a pointer then this points to the structure that defines it
                        currentvalue: string;
                      end;
  public
    procedure refresh;
    constructor create(treeviewused: ttreeview;parentnode: ttreenode;address:dword; basestructure: integer);
    destructor destroy; override;
end;


type
  TfrmStructures = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Structures1: TMenuItem;
    Definenewstructure1: TMenuItem;
    N1: TMenuItem;
    TreeView1: TTreeView;
    Panel1: TPanel;
    edtAddress: TEdit;
    Button1: TButton;
    Button2: TButton;
    PopupMenu1: TPopupMenu;
    Addelement1: TMenuItem;
    updatetimer: TTimer;
    Deleteelement1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    New1: TMenuItem;
    ChangeElement1: TMenuItem;
    N2: TMenuItem;
    Addtoaddresslist1: TMenuItem;
    Recalculateaddress1: TMenuItem;
    N3: TMenuItem;
    procedure Definenewstructure1Click(Sender: TObject);
    procedure Addelement1Click(Sender: TObject);
    procedure updatetimerTimer(Sender: TObject);
    procedure TreeView1Collapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure edtAddressChange(Sender: TObject);
    procedure TreeView1Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Deleteelement1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure ChangeElement1Click(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure Addtoaddresslist1Click(Sender: TObject);
    procedure Recalculateaddress1Click(Sender: TObject);
  private
    { Private declarations }
    currentstructure: tstructure;
    definedstructures: array of Tbasestucture;

    faddress: dword;
    procedure setaddress(x:dword);
    procedure refreshmenuitems;
    procedure definedstructureselect(sender:tobject);
    function RawToType(address: dword; const buf: array of byte; size: integer):integer;
  public
    { Public declarations }
    property address: dword read faddress write setaddress;
  end;

var
  frmStructures: TfrmStructures;

implementation

{$R *.dfm}

uses StructuresAddElementfrm,valuechange,mainunit, MemoryBrowserFormUnit;

destructor TStructure.destroy;
var i: integer;
begin
  for i:=0 to length(objects)-1 do
    if objects[i].child<>nil then objects[i].child.Free;

  inherited destroy;
end;

constructor TStructure.create(treeviewused: ttreeview;parentnode: ttreenode;address:dword; basestructure: integer);
var elementnr: integer;
    s: tstructure;
begin
  self.address:=address;
  self.basestructure:=basestructure;
  self.treeviewused:=treeviewused;
  self.parentnode:=parentnode;
  inherited create;
end;


procedure TStructure.refresh;
var i,j,k: integer;
    newtext,typename: string;
    snr: integer;
    elementaddress: dword;
    buf: array of byte;
    x: dword;

    defaultwidth: integer;

    ws: widestring;
    pc: pchar;
    pwc: pwidechar;
    newaddress: dword;

    s: tstructure;

    elementnr: integer;


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
    if length(objects)<length(frmStructures.definedstructures[basestructure].structelement) then
      setlength(objects,length(frmStructures.definedstructures[basestructure].structelement));

    if length(objects)>length(frmStructures.definedstructures[basestructure].structelement) then
    begin
      //delete the extra ones
      for i:=length(frmStructures.definedstructures[basestructure].structelement) to length(objects)-1 do
      begin
        if objects[i].nodetoupdate<>nil then objects[i].nodetoupdate.Delete;
        if objects[i].child<>nil then objects[i].child.Free;
      end;

      setlength(objects,length(frmStructures.definedstructures[basestructure].structelement));
    end;
  end;


  elementaddress:=address;

  for i:=0 to length(objects)-1 do
  begin
    //define the text for this element

    if basestructure<0 then
    begin
      snr:=basestructure;
    end
    else
    begin
      if frmStructures.definedstructures[basestructure].structelement[i].pointerto then
        typename:='pointer to '
      else
        typename:='';

      snr:=frmStructures.definedstructures[basestructure].structelement[i].structurenr;
    end;

    if snr<0 then
    begin
      if basestructure>=0 then
      begin
        if frmStructures.definedstructures[basestructure].structelement[i].pointerto then
        begin
          if readprocessmemory(processhandle,pointer(elementaddress),@buf[0],4,x) then
            objects[i].currentvalue:=inttohex(PDWORD(@buf[0])^,8)
          else
            objects[i].currentvalue:='???';

          objects[i].currentvalue:='->'+objects[i].currentvalue;
        end;
      end;

      if (basestructure<0) or (not frmStructures.definedstructures[basestructure].structelement[i].pointerto) then
      case snr of
        -1:
        begin
          typename:=typename+'Byte';

          //read the value
          if readprocessmemory(processhandle,pointer(elementaddress),@buf[0],1,x) then
            objects[i].currentvalue:=inttostr(byte(buf[0]))
          else
            objects[i].currentvalue:='???';
        end;
        -2:
        begin
          typename:=typename+'Byte Signed';

          //read the value
          if readprocessmemory(processhandle,pointer(elementaddress),@buf[0],1,x) then
            objects[i].currentvalue:=inttostr(Shortint(buf[0]))
          else
            objects[i].currentvalue:='???';
        end;
        -3:
        begin
          typename:=typename+'Byte Hexadecimal';
          if readprocessmemory(processhandle,pointer(elementaddress),@buf[0],1,x) then
            objects[i].currentvalue:=inttohex(buf[0],2)
          else
            objects[i].currentvalue:='???';
        end;
        -4:
        begin
          typename:=typename+'2 Bytes';
          if readprocessmemory(processhandle,pointer(elementaddress),@buf[0],2,x) then
            objects[i].currentvalue:=inttostr(PWORD(@buf[0])^)
          else
            objects[i].currentvalue:='???';
        end;
        -5:
        begin
          typename:=typename+'2 Bytes Signed';
          if readprocessmemory(processhandle,pointer(elementaddress),@buf[0],2,x) then
            objects[i].currentvalue:=inttostr(PSmallint(@buf[0])^)
          else
            objects[i].currentvalue:='???';
        end;
        -6:
        begin
          typename:=typename+'2 Bytes Hexadecimal';
          if readprocessmemory(processhandle,pointer(elementaddress),@buf[0],2,x) then
            objects[i].currentvalue:=inttohex(PWORD(@buf[0])^,4)
          else
            objects[i].currentvalue:='???';
        end;
        -7:
        begin
          typename:=typename+'4 Bytes';
          if readprocessmemory(processhandle,pointer(elementaddress),@buf[0],4,x) then
            objects[i].currentvalue:=inttostr(PDWORD(@buf[0])^)
          else
            objects[i].currentvalue:='???';
        end;

        -8:
        begin
          typename:=typename+'4 Bytes Signed';
          if readprocessmemory(processhandle,pointer(elementaddress),@buf[0],4,x) then
            objects[i].currentvalue:=inttostr(pinteger(@buf[0])^)
          else
            objects[i].currentvalue:='???';
        end;

        -9:
        begin
          typename:=typename+'4 Bytes Hexadecimal';
          if readprocessmemory(processhandle,pointer(elementaddress),@buf[0],4,x) then
            objects[i].currentvalue:=inttohex(PDWORD(@buf[0])^,8)
          else
            objects[i].currentvalue:='???';
        end;
        -10:
        begin
          typename:=typename+'8 Bytes';
          if readprocessmemory(processhandle,pointer(elementaddress),@buf[0],8,x) then
            objects[i].currentvalue:=inttostr(pint64(@buf[0])^)
          else
            objects[i].currentvalue:='???';
        end;
        -11:
        begin
          typename:=typename+'8 Bytes Hexadecimal';
          if readprocessmemory(processhandle,pointer(elementaddress),@buf[0],8,x) then
            objects[i].currentvalue:=inttohex(pint64(@buf[0])^,16)
          else
            objects[i].currentvalue:='???';
        end;
        -12:
        begin
          typename:=typename+'Float';
          if readprocessmemory(processhandle,pointer(elementaddress),@buf[0],4,x) then
            objects[i].currentvalue:=floattostr(psingle(@buf[0])^)
          else
            objects[i].currentvalue:='???';
        end;
        -13:
        begin
          typename:=typename+'Double';
          if readprocessmemory(processhandle,pointer(elementaddress),@buf[0],8,x) then
            objects[i].currentvalue:=floattostr(pdouble(@buf[0])^)
          else
            objects[i].currentvalue:='???';
        end;
        -14:
        begin
          typename:=typename+'String';

          if basestructure>=0 then
            k:=frmStructures.definedstructures[basestructure].structelement[i].bytesize
          else
          begin
            elementnr:=parentnode.Index;
            s:=parentnode.data;
            k:=frmStructures.definedstructures[s.basestructure].structelement[elementnr].pointertosize;
          end;

          if length(buf)<=k then
            setlength(buf,k+1);

          if readprocessmemory(processhandle,pointer(elementaddress),@buf[0],k,x) then
          begin
            buf[k]:=0;
            for j:=0 to k-1 do
              if (buf[j]>0) and (buf[j]<32) then buf[j]:=ord('?');

            pc:=@buf[0];
            objects[i].currentvalue:=pc;
          end
          else
            objects[i].currentvalue:='???';
        end;
        -15:
        begin
          typename:=typename+'String Unicode';
          if basestructure>=0 then
            k:=frmStructures.definedstructures[basestructure].structelement[i].bytesize
          else
          begin
            elementnr:=parentnode.Index;
            s:=parentnode.data;
            k:=frmStructures.definedstructures[s.basestructure].structelement[elementnr].pointertosize;
          end;
          if length(buf)<=k then
            setlength(buf,k+1);


          if readprocessmemory(processhandle,pointer(elementaddress),@buf[0],k,x) then
          begin
            buf[k]:=0;
            buf[k-1]:=0;
            for j:=0 to k-1 do
              if (buf[j]>0) and (buf[j]<32) then buf[j]:=ord('?');
            pwc:=@buf[0];
            ws:=pwc;
            objects[i].currentvalue:=ws;
          end
          else
            objects[i].currentvalue:='???';
        end;
      end;
    end else
    begin
      //it's a defined structure (has to be a pointer)
      typename:=frmStructures.definedstructures[snr].name;

      if readprocessmemory(processhandle,pointer(elementaddress),@buf[0],8,x) then
        objects[i].currentvalue:='->'+inttohex(pdword(@buf[0])^,8)
      else
        objects[i].currentvalue:='->???';
    end;

    if basestructure<0 then
    begin
      newtext:=inttohex(elementaddress,8)+' - '+'('+typename+')';
    end
    else
    begin
      newtext:=inttohex(elementaddress,8)+' - '+frmStructures.definedstructures[basestructure].structelement[i].description;//+'('+typename+')';
    end;
    newtext:=newtext+' ';


    while length(newtext)<50 do
        newtext:=newtext+' ';

    newtext:=newtext+objects[i].currentvalue;

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
      if objects[i].nodetoupdate.HasChildren <> frmStructures.definedstructures[basestructure].structelement[i].pointerto then
        objects[i].nodetoupdate.HasChildren:=frmStructures.definedstructures[basestructure].structelement[i].pointerto;

      if objects[i].child<>nil then
      begin
        //do the same for the children
        //pointer

        newaddress:=0;
        readprocessmemory(processhandle,pointer(elementaddress),@newaddress,4,x);

        objects[i].child.address:=newaddress;
        objects[i].child.refresh;
      end;

      inc(elementaddress,frmStructures.definedstructures[basestructure].structelement[i].bytesize);
    end;
  end;

  if treeviewused.Items.GetFirstNode<>nil then
    treeviewused.Items.GetFirstNode.Expand(false);

  //treeviewused.Items.endupdate;
end;


procedure TfrmStructures.setaddress(x: dword);
begin
  faddress:=x;
  if not edtAddress.Focused then edtAddress.text:=inttohex(faddress,8);
  
  if currentstructure<>nil then
  begin
    currentstructure.address:=address;
    currentstructure.parentnode.Text:=edtaddress.text+'-'+definedstructures[currentstructure.basestructure].name;
    currentstructure.refresh;
  end;
end;

function TfrmStructures.RawToType(address: dword; const buf: array of byte; size: integer):integer;
{
returns: -1,-4,-7,-12,-13,-14
}
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

procedure TfrmStructures.Definenewstructure1Click(Sender: TObject);
var sstructsize:string;
    autofillin,structsize: integer;
    structname: string;
    buf: array of byte;
    buf2: array of byte;
    i,j,t: integer;
    x,y: dword;
begin
  if not inputquery('Structure define','Give the name for this structure',structname) then exit;
  
  autofillin:=messagedlg('Do you want Cheat Engine to try and fill in the most basic types of the struct using the current address?',mtconfirmation,[mbyes,mbno,mbcancel],0);
  if autofillin=mrcancel then exit;

  setlength(definedstructures,length(definedstructures)+1);
  definedstructures[length(definedstructures)-1].name:=structname;

  refreshmenuitems;
  structures1.Items[structures1.Count-1].Click;


  if autofillin=mryes then
  begin
    sstructsize:='512';
    if not inputquery('Structure define','Please give a starting size of the struct (You can change this later if needed)',Sstructsize) then exit;
    structsize:=strtoint(sstructsize);


    setlength(buf,structsize);
    setlength(buf2,8);
    //now read the memory
    if readprocessmemory(processhandle,pointer(address),@buf[0],structsize,x) then
    begin
      x:=0;
      while x<structsize do
      begin
        i:=length(definedstructures[length(definedstructures)-1].structelement);
        setlength(definedstructures[length(definedstructures)-1].structelement,i+1);
        definedstructures[length(definedstructures)-1].structelement[i].pointerto:=false;

        if isreadable(pdword(@buf[x])^) and (pdword(@buf[x])^ mod 4=0) then
        begin
          //pointer
          definedstructures[length(definedstructures)-1].structelement[i].pointerto:=true;
          definedstructures[length(definedstructures)-1].structelement[i].pointertoSize:=8;
          definedstructures[length(definedstructures)-1].structelement[i].bytesize:=4;
          definedstructures[length(definedstructures)-1].structelement[i].description:='pointer to ';

          if readprocessmemory(processhandle,pointer(pdword(@buf[x])^),@buf2[0],8,y) then
          begin
            t:=RawToType(pdword(@buf[x])^,buf2[0],8);
            definedstructures[length(definedstructures)-1].structelement[i].structurenr:=t;
          end
          else definedstructures[length(definedstructures)-1].structelement[i].structurenr:=-9;

          inc(x,4);
        end
        else
        begin
          //value
          //check what type it is
          t:=RawToType(address+x,buf[x],structsize-x);
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
            end;
          end;

        end;
      end;

    end;
  end;
  refreshmenuitems;
  currentstructure.refresh;   
end;

procedure TfrmStructures.definedstructureselect(sender:tobject);
begin
  caption:='Memory dissect - '+(sender as tmenuitem).Caption;
  if currentstructure<>nil then
    freeandnil(currentstructure);

  treeview1.Items.Clear;

  currentstructure:=tstructure.create(treeview1,treeview1.Items.Add(nil,edtaddress.text+'-'+definedstructures[(sender as tmenuitem).Tag].name),address,(sender as tmenuitem).Tag);
  currentstructure.refresh;
  refreshmenuitems;  
end;

procedure TfrmStructures.refreshmenuitems;
var i: integer;
    mi: tmenuitem;
begin
  //go through the definedstructures array and see if they are int the list or not

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
begin
  selectednode:=treeview1.Selected;
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
            inc(i);
          end;
        end;
      end;

      currentstructure.refresh;

      if not treeview1.Items.GetFirstNode.Expanded then
        treeview1.Items.GetFirstNode.Expand(false);
    end;
  end;
end;



procedure TfrmStructures.updatetimerTimer(Sender: TObject);
begin
  if currentstructure<>nil then currentstructure.refresh;
end;

procedure TfrmStructures.TreeView1Collapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  allowcollapse:=not (node=treeview1.Items.GetFirstNode);
end;

procedure TfrmStructures.Button1Click(Sender: TObject);
begin
  address:=address+1;
end;

procedure TfrmStructures.Button2Click(Sender: TObject);
begin
  address:=address-1;
end;

procedure TfrmStructures.edtAddressChange(Sender: TObject);
begin
  address:=strtoint('$'+edtaddress.text);
end;

procedure TfrmStructures.TreeView1Expanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var s: tstructure;
    elementnr: integer;
    basestruct: integer;
    elementaddress: dword;
    i: integer;
begin
  AllowExpansion:=true;

  s:=tstructure(node.Data);
  if s=nil then exit;
  if node.getFirstChild<>nil then exit;

  elementnr:=node.Index;

  //structure and element nr are known, so lets see what it is

  basestruct:=s.basestructure;

  elementaddress:=s.address;
  for i:=0 to elementnr-2 do
    inc(elementaddress,definedstructures[basestruct].structelement[i].bytesize);


  //make sure it's a pointer
  if definedstructures[basestruct].structelement[elementnr].pointerto then
    s.objects[elementnr].child:=tstructure.create(treeview1,node,elementaddress,definedstructures[basestruct].structelement[elementnr].structurenr);

  currentstructure.refresh;
end;

procedure TfrmStructures.TreeView1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var tn: ttreenode;
begin
  if mbright = button then
  begin
    tn:=treeview1.GetNodeAt(x,y);
    treeview1.Selected:=tn;
  end;
end;

procedure TfrmStructures.PopupMenu1Popup(Sender: TObject);
begin
  AddElement1.Visible:=currentstructure<>nil;
  Deleteelement1.Visible:=(treeview1.selected<>nil);

  n3.Visible:=(treeview1.selected<>nil) and (treeview1.selected.Level=1);
  Recalculateaddress1.visible:=n3.Visible;
end;

procedure TfrmStructures.Deleteelement1Click(Sender: TObject);
var s: tstructure;
    elementnr: integer;
    i: integer;
begin
  if treeview1.Selected<>nil then
  begin
    elementnr:=treeview1.Selected.Index;
    s:=tstructure(treeview1.Selected.Data);

    if s=nil then exit;
    if s.basestructure<0 then exit;

    for i:=elementnr to length(definedstructures[s.basestructure].structelement)-2 do
      definedstructures[s.basestructure].structelement[i]:=definedstructures[s.basestructure].structelement[i+1];

    setlength(definedstructures[s.basestructure].structelement,length(definedstructures[s.basestructure].structelement)-1);
  end;

  currentstructure.refresh;
end;

procedure TfrmStructures.Save1Click(Sender: TObject);
var f: tfilestream;
    i,j: integer;
    x: dword;
    cemarker: string;
begin
  if savedialog1.Execute then
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

procedure TfrmStructures.Open1Click(Sender: TObject);
var f: tfilestream;
    i,j: integer;
    x: dword;
    cemarker: string;
    c: pchar;
    s: string;
begin
  if opendialog1.Execute then
  begin
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

      f.ReadBuffer(x,4);
      setlength(definedstructures,x);

      for i:=0 to length(definedstructures)-1 do
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

    RefreshMenuItems;
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
end;

procedure TfrmStructures.ChangeElement1Click(Sender: TObject);
var i,j: integer;
    size: dword;
    structtype: string;
    selectedstructure: tstructure;
    selectedelement: integer;
    selectednode: ttreenode;
begin
  if treeview1.Selected=treeview1.Items.GetFirstNode then
  begin
    inputquery('Rename structure','Give the new name of this structure',definedstructures[currentstructure.basestructure].name);
    address:=address+1-1;
    currentstructure.refresh;
    exit;
  end;


  selectednode:=treeview1.Selected;
  if selectednode=nil then exit;

  selectedstructure:=tstructure(selectednode.Data);
  if selectedstructure=nil then exit;

  selectedelement:=selectednode.Index;

  i:=selectedstructure.basestructure;
  
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

    cbType.OnChange(cbType);

    if showmodal=mrok then
    begin
      definedstructures[selectedstructure.basestructure].structelement[selectedelement].description:=edtDescription.text;
      definedstructures[selectedstructure.basestructure].structelement[selectedelement].pointerto:=cbpointerto.checked;
      definedstructures[selectedstructure.basestructure].structelement[selectedelement].pointertosize:=bytesize;

      if cbtype.itemindex<=14 then
        definedstructures[selectedstructure.basestructure].structelement[selectedelement].structurenr:=-(cbtype.ItemIndex+1)
      else
        definedstructures[selectedstructure.basestructure].structelement[selectedelement].structurenr:=cbtype.ItemIndex-15;

      if definedstructures[selectedstructure.basestructure].structelement[selectedelement].pointerto then
        definedstructures[selectedstructure.basestructure].structelement[selectedelement].bytesize:=4
      else
        definedstructures[selectedstructure.basestructure].structelement[selectedelement].bytesize:=bytesize;

      currentstructure.refresh;
    end;
  end;

end;

procedure TfrmStructures.TreeView1DblClick(Sender: TObject);
var
  selectedstructure: tstructure;
  selectednode: ttreenode;
  selectedelement: integer;
  i: integer;
  a: dword;
begin
  selectednode:=treeview1.Selected;
  if selectednode<>nil then
  begin
    selectedstructure:=tstructure(selectednode.Data);
    if selectedstructure<>nil then
    begin
      selectedelement:=selectednode.Index;
      a:=selectedstructure.address;
      for i:=0 to selectedelement-1 do
        inc(a,definedstructures[selectedstructure.basestructure].structelement[i].bytesize);

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
        currentstructure.refresh;
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
begin
  showashex:=false;
  selectednode:=treeview1.Selected;
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
    mainform.addaddress('bla',address+offsets[length(offsets)-1],offsets[0],length(offsets)-1,length(offsets)>1,vtype,vlength,0,unicode,showashex);
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
begin
  selectednode:=treeview1.Selected;
  if selectednode<>nil then
  begin
    selectedstructure:=tstructure(selectednode.Data);
    selectedelement:=selectednode.Index;
    snr:=selectedstructure.basestructure;

    oldaddress:=address;

    for i:=0 to selectedelement-1 do
      inc(oldaddress,definedstructures[snr].structelement[i].bytesize);

    a:=inttohex(memorybrowser.memoryaddress,8);
    if inputquery('Recalculate base of structure','Give the address of this element',a) then
    begin
      try
        newaddress:=strtoint('$'+a);
      except
        raise exception.Create('I have no idea what '+a+' means');
      end;

      delta:=newaddress-oldaddress;
      address:=address+delta;
      currentstructure.refresh;
    end;
  end;
end;

end.


