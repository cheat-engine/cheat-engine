unit ExtraTrainerComponents;
// ok, OOP isn't very efficient, but i have to admit it's great for making programming easy

interface

uses controls,StdCtrls,classes,Graphics,ExtCtrls;

type TLabel2 = class (TLabel)
  public
    command: string;
end;

type TImage2 = class (Timage)
  public
    command: string;
end;

type TButton2 = class (TButton)
  public
    command: string;
end;

type tcheat = class (twincontrol)
//a cheat exists of 2 parts, the hotkey and the description   (what about cheats that enable you to change values them?)
  private
    hotkeylabel: tlabel;
    descriptionlabel: tlabel;
    edit:tedit;
    checkbox: tcheckbox;
    editPresent: boolean;
    ftextcolor: tcolor;
    factivated: boolean;
    fhascheckbox: boolean;
    fshowhotkey: boolean;
    procedure resetwidth;
    procedure setactivated(x:boolean);
    procedure SetHotkey(newhotkey:string);
    procedure SetDescription(newdescription: string);
    function GetHotkey:string;
    function GetDescription:string;
    function GetEditValue:string;
    procedure SetEditValue(value:string);
    procedure ChangeEdit(present: boolean);
    function  gethotkeyleft:integer;
    procedure sethotkeyleft(x: integer);
    function  getdescriptionleft:integer;
    procedure Setdescriptionleft(x: integer);
    function  geteditleft:integer;
    procedure seteditleft(x: integer);
    function  geteditwidth:integer;
    procedure seteditwidth(x: integer);
    procedure setTextColor(c:tcolor);
    procedure SetCheckbox(x: boolean);
    procedure setshowHotkey(x: boolean);

    procedure md(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    cheatnr: integer;
    activationcolor: tcolor;
    property activated: boolean read factivated write SetActivated;
    property textcolor:tcolor read ftextcolor write SetTextcolor;
    property hotkeyleft: integer read gethotkeyleft write sethotkeyleft;
    property descriptionleft:integer read getdescriptionleft write setdescriptionleft;
    property editleft:integer read geteditleft write seteditleft;
    property editwidth: integer read geteditwidth write seteditwidth;

    property Editvalue:string read GetEditValue write SetEditValue;
    property Hotkey:string read GetHotkey write SetHotkey;
    property Description:string read GetDescription write SetDescription;
    property HasEditBox:boolean read EditPresent write ChangeEdit;
    property HasCheckbox: boolean read fHasCheckbox write SetCheckbox;
    property showhotkey: boolean read fshowhotkey write SetShowHotkey;
    constructor create(AOwner:Tcomponent); override;
    destructor destroy; override;
  published
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property Color;

end;

type tcheatlist = class (twincontrol)
  private
    cheats: array of tcheat;
    cheatcount: integer;
    fautosize: boolean;
    fhotkeyleft: integer;
    fdescriptionleft: integer;
    feditleft:integer;
    feditwidth:integer;
    ftextcolor:tcolor;
    fhascheckbox: boolean;
    fshowhotkeys: boolean;
    Function GetItem(i:integer):tcheat;
    procedure SetAutosize(x:boolean);
    procedure sethotkeyleft(i:integer);
    procedure setDescriptionleft(i: integer);
    procedure setEditLeft(i: integer);
    procedure setEditWidth(i: integer);
    procedure setTextColor(c:tcolor);
    procedure setCheckbox(x: boolean);
    procedure setShowHotkeys(x: boolean);
  public
    activationcolor: tcolor;
    property HasCheckbox: boolean read fhascheckbox write setcheckbox;
    property ShowHotkeys: boolean read fshowhotkeys write setshowhotkeys;
    property TextColor: tcolor read ftextcolor write settextcolor;
    property hotkeyleft: integer read fhotkeyleft write sethotkeyleft;
    property descriptionleft: integer read fdescriptionleft write setdescriptionleft;
    property editleft: integer read feditleft write seteditleft;
    property editwidth: integer read feditwidth write seteditwidth;

    property AutoSize: boolean read fautosize write SetAutosize;
    property Items[Index: Integer]: TCheat read GetItem;  //no write
    property Count:integer read cheatcount;
    procedure addcheat(newhotkey,newdescription,defeditvalue:string;hasEdit:boolean);
    procedure clear;
    procedure deletelast;
    constructor create(AOwner:Tcomponent); override;
    destructor destroy; override;
  published
    property onmousedown;
    property OnMouseMove;
    property OnMouseUp;    
    property bevelkind;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property Color;
end;

implementation

procedure TCheat.md(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if assigned(self.OnMouseDown) then
    self.OnMouseDown(self,button,shift,x,y);
end;

procedure TCheat.SetShowHotkey(x: boolean);
begin
  fshowhotkey:=x;
  hotkeylabel.Visible:=x;
end;

procedure tcheat.SetCheckbox(x:boolean);
begin
  fhascheckbox:=x;

  if fhascheckbox then
    hotkeylabel.left:=checkbox.width+3
  else
    hotkeylabel.Left:=0;

  checkbox.Visible:=x;
end;

procedure tcheat.setactivated(x:boolean);
begin
  factivated:=x;
  if x then //set activate color
  begin
    descriptionlabel.Font.Color:=activationcolor;
    hotkeylabel.Font.Color:=activationcolor;
  end
  else
  begin
    descriptionlabel.Font.Color:=ftextcolor;
    hotkeylabel.Font.Color:=ftextcolor;
  end;

  checkbox.Checked:=x;
end;

procedure tcheat.resetwidth;
var a,b: integer;
begin
  a:=descriptionlabel.left+descriptionlabel.Width;
  b:=hotkeylabel.Left+hotkeylabel.Width;

  if b>a then a:=b;

  if editpresent then
  begin
    b:=edit.Left+edit.Width;
    if b>a then a:=b;
  end;

  width:=a;
end;


function tcheat.GetHotkeyleft: integer;
begin
  result:=hotkeylabel.Left;
end;

procedure tcheat.sethotkeyleft(x: integer);
begin
  hotkeylabel.left:=x;
  resetwidth;
end;

procedure tcheat.setTextColor(c:tcolor);
begin
  ftextcolor:=c;
  //set the color of description and hotkey
  if not activated then
  begin
    descriptionlabel.Font.Color:=c;
    hotkeylabel.Font.Color:=c;
  end;
end;

function tcheat.geteditwidth:integer;
begin
  result:=edit.width;
end;

procedure tcheat.seteditwidth(x:integer);
begin
  edit.width:=x;
  resetwidth;
end;

function tcheat.geteditleft:integer;
begin
  result:=edit.left;
end;

procedure tcheat.seteditleft(x:integer);
begin
  edit.left:=x;
  resetwidth;
end;

function tcheat.getdescriptionleft:integer;
begin
  result:=descriptionlabel.left;
end;

procedure tcheat.Setdescriptionleft(x:integer);
begin
  descriptionlabel.Left:=x;
  //change the width of the component
  resetwidth;
end;

function tcheat.GetEditValue:string;
begin
  result:=edit.text;
end;

procedure tcheat.SetEditValue(value:string);
begin
  edit.Text:=value;
end;

procedure tcheat.ChangeEdit(present:boolean);
begin
  edit.Visible:=present;
  EditPresent:=present;
  resetwidth;
end;

procedure tcheat.Sethotkey(newhotkey:string);
begin
  hotkeylabel.Caption:=newhotkey;
  resetwidth;
end;

procedure tcheat.SetDescription(newDescription:string);
begin
  descriptionlabel.Caption:=newdescription;
  resetwidth;
end;

function tcheat.GetHotkey:string;
begin
  result:=hotkeylabel.Caption;
end;

function tcheat.GetDescription:string;
begin
  result:=descriptionlabel.Caption;
end;

constructor tcheat.create(AOwner:tcomponent);
begin
  inherited create(AOwner);

  activationcolor:=clred;
  edit:=tedit.Create(self);
  edit.Left:=200;
  edit.Visible:=false;
  edit.Parent:=self;

  checkbox:=tcheckbox.Create(self);
  checkbox.caption:='';
  checkbox.Left:=0;
  checkbox.top:=1+(edit.height div 2)-(checkbox.Height div 2);
  checkbox.width:=16;
  checkbox.Visible:=false;
  checkbox.Parent:=self;

  hotkeylabel:=tlabel.Create(self);
  hotkeylabel.Caption:='undefined hotkey';
  hotkeylabel.Left:=0;

  hotkeylabel.Top:=1+(edit.height div 2)-(hotkeylabel.Height div 2);
  hotkeylabel.Parent:=self;

  descriptionlabel:=tlabel.Create(self);
  descriptionlabel.Caption:='undefined description';
  descriptionlabel.left:=100;
  descriptionlabel.Top:=1+(edit.height div 2)-(descriptionlabel.Height div 2);
  descriptionlabel.Parent:=self;

  ftextcolor:=hotkeylabel.Font.Color;
  self.Width:=descriptionlabel.left+descriptionlabel.Width;
  self.height:=edit.Height+2;

  fshowhotkey:=true;

  descriptionlabel.OnMouseDown:=md;
  hotkeylabel.OnMouseDown:=md;
  checkbox.OnMouseDown:=md;
  checkbox.enabled:=false;
end;



destructor tcheat.destroy;
begin
  hotkeylabel.Free;
  descriptionlabel.Free;
  edit.Free;
  inherited destroy;
end;

//-------------------------------------------------------
//Tcheatlist
//-------------------------------------------------------

procedure tcheatlist.setTextColor(c:tcolor);
var i:integer;
begin
  ftextcolor:=c;

  //set the color of the cheats
  for i:=0 to cheatcount-1 do
    cheats[i].textcolor:=c;
end;

procedure TCheatlist.setShowHotkeys(x: boolean);
var i: integer;
begin
  fshowhotkeys:=x;
  for i:=0 to cheatcount-1 do
    cheats[i].showhotkey:=x;          
end;

procedure TCheatlist.setcheckbox(x: boolean);
var i: integer;
begin
  fhascheckbox:=x;
  for i:=0 to cheatcount-1 do
    cheats[i].hascheckbox:=x;
end;

procedure tcheatlist.setHotkeyleft(i:integer);
var j: integer;
begin
  fhotkeyleft:=i;
  //set the descleft of aal cheats
  for j:=0 to cheatcount-1 do
    cheats[j].hotkeyleft:=i;
end;

procedure tcheatlist.setDescriptionleft(i:integer);
var j: integer;
begin
  fdescriptionleft:=i;
  //set the descleft of aal cheats
  for j:=0 to cheatcount-1 do
    cheats[j].descriptionleft:=i;
end;

procedure tcheatlist.setEditLeft(i:integer);
var j: integer;
begin
  feditleft:=i;
  //set the descleft of aal cheats
  for j:=0 to cheatcount-1 do
    cheats[j].editleft:=i;
end;

procedure tcheatlist.setEditWidth(i:integer);
var j: integer;
begin
  feditwidth:=i;
  //set the descleft of aal cheats
  for j:=0 to cheatcount-1 do
    cheats[j].editwidth:=i;
end;

procedure tcheatlist.SetAutosize(x:boolean);
begin
  fautosize:=x;
  if x and (cheatcount>0) then //autosize enabled
    height:=6+cheats[count-1].top+cheats[count-1].height;
end;

procedure tcheatlist.addcheat(newhotkey,newdescription,defeditvalue:string;hasEdit:boolean);
begin
  setlength(cheats,length(cheats)+1);
  cheats[length(cheats)-1]:=tcheat.create(self);

  with cheats[length(cheats)-1] do
  begin
    if assigned(self.onmousedown) then
    begin
      onmousedown:=self.onmousedown;
      descriptionlabel.OnMouseDown:=self.onmousedown;
      hotkeylabel.OnMouseDown:=self.onmousedown;
      edit.OnMouseDown:=self.onmousedown;
    end;

    cheatnr:=length(cheats)-1;
    left:=10;
    top:=5+cheatnr*height;
    hotkey:=newhotkey;
    description:=newdescription;
    Haseditbox:=hasedit;
    editvalue:=defeditvalue;
    textcolor:=self.ftextcolor;
    descriptionleft:=self.fdescriptionleft;
    editleft:=self.feditleft;
    editwidth:=self.feditwidth;
    activationcolor:=self.activationcolor;
    hascheckbox:=self.HasCheckbox;
    parent:=self;
  end;

  inc(cheatcount);

  if autosize then
    height:=6+cheats[count-1].top+cheats[count-1].height;

end;

constructor tcheatlist.create(AOwner:tcomponent);
begin
  inherited create(AOwner);

  activationcolor:=clred;
  width:=100;
  height:=50;
  ftextcolor:=clwindowtext;
  fdescriptionleft:=100;
  editleft:=210;
  editwidth:=50;

  fshowhotkeys:=true;

//  self.BevelEdges:=[beTop, beBottom,beleft,beright];
 // self.BevelInner:=bvNone;
//  self.BevelOuter:=bvNone;
 // self.BevelKind:=bknone;
 // self.BevelInner:=bvlowered;
 // self.BorderWidth:=2;

end;

destructor tcheatlist.destroy;
var i: integer;
begin
  for i:=0 to cheatcount-1 do cheats[i].free;
  inherited destroy;
end;

function tcheatlist.GetItem(i:integer):tcheat;
begin
  result:=cheats[i];
end;

procedure tcheatlist.deletelast;
begin
  //in case the update encounters a delete
  dec(cheatcount);
  cheats[cheatcount].Free;
  setlength(cheats,cheatcount);

  //and adjust the size to fit the new cheat
  if autosize then
  begin
    if count>0 then
      height:=6+cheats[count-1].top+cheats[count-1].height
    else
      height:=1;
  end;
end;

procedure tcheatlist.clear;
var i: integer;
begin
  for i:=0 to cheatcount-1 do cheats[i].free;

  cheatcount:=0;
  setlength(cheats,0);
end;

end.
