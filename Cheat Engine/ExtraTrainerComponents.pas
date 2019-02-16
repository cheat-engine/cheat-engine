unit ExtraTrainerComponents;

interface

{$mode DELPHI}

uses controls,StdCtrls,classes,Graphics,ExtCtrls, sysutils;

type TLabel2 = class (TLabel)
  public
    command: string;
end;

type TImage2 = class (Timage)
  public
    extension: string;
    imagedata: tmemorystream;
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
    ftextcolor: tcolor;
    factivated: boolean;

    fshowhotkey: boolean;
    factivationcolor: tcolor;
    feditleft: integer;
    feditwidth: integer;
    feditvalue: string;
    fcheckboxstate: boolean;


    deactivatetimer: TTimer;
    fCheatNr: integer;
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
    procedure seteditleft(x: integer);
    procedure seteditwidth(x: integer);
    procedure setTextColor(c:tcolor);
    procedure SetCheckbox(x: boolean);
    procedure setshowHotkey(x: boolean);

    procedure resizeControl(sender: TObject);

    procedure md(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    function getCheckboxVisible: boolean;
    function editPresent: boolean;
  public




    beeponactivate: boolean;

    constructor create(AOwner:Tcomponent); override;

    procedure setDeactivateTimer(interval: integer);
    procedure timerdeactivate(sender: tobject);


    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  published
    property CheatNr: integer read fCheatNr write fCheatNr;
    property Color;
    property Textcolor:tcolor read ftextcolor write SetTextcolor;
    property Editleft:integer read feditleft write seteditleft default 200;
    property Editwidth: integer read feditwidth write seteditwidth;
    property Editvalue:string read GetEditValue write SetEditValue;
    property Hotkey:string read GetHotkey write SetHotkey;
    property Description:string read GetDescription write SetDescription;
    property Hotkeyleft: integer read gethotkeyleft write sethotkeyleft;
    property Descriptionleft:integer read getdescriptionleft write setdescriptionleft;
    property Activated: boolean read factivated write SetActivated;
    property Activationcolor: TColor read factivationcolor write factivationcolor;

    property ShowHotkey: boolean read fshowhotkey write SetShowHotkey;
    property HasEditBox: boolean read EditPresent write ChangeEdit;
    property HasCheckbox: boolean read getCheckboxVisible write SetCheckbox;
    property Font;

end;

type TCheatList = class (twincontrol)
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
    fBeepOnActivate: boolean;
    Function GetItem(i:integer):tcheat;
    {$warn 3057 off}
    procedure SetAutosize(x:boolean);
    procedure sethotkeyleft(i:integer);
    procedure setDescriptionleft(i: integer);
    procedure setEditLeft(i: integer);
    procedure setEditWidth(i: integer);
    procedure setTextColor(c:tcolor);
    procedure setCheckbox(x: boolean);
    procedure setShowHotkeys(x: boolean);
    procedure setBeepOnActivate(x: boolean);
  public
    activationcolor: tcolor;
    property beepOnActivate: boolean read fBeepOnActivate write setBeepOnActivate;
    property HasCheckbox: boolean read fhascheckbox write setcheckbox;
    property ShowHotkeys: boolean read fshowhotkeys write setshowhotkeys;
    property TextColor: tcolor read ftextcolor write settextcolor;
    property hotkeyleft: integer read fhotkeyleft write sethotkeyleft;
    property descriptionleft: integer read fdescriptionleft write setdescriptionleft;
    property editleft: integer read feditleft write seteditleft;
    property editwidth: integer read feditwidth write seteditwidth;

    property AutoSize: boolean read fautosize write SetAutosize;

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
   // property bevelkind;
   // property BevelInner;
    //property BevelOuter;
  //  property BevelWidth;
    property Color;
    property Items[Index: Integer]: TCheat read GetItem;  //no write
end;

implementation

resourcestring
  rsUndefinedHotkey = 'undefined hotkey';
  rsUndefinedDescription = 'undefined description';



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

procedure tcheat.resizeControl(sender: TObject);
begin
  if checkbox<>nil then
    checkbox.top:=(clientheight div 2)-(checkbox.Height div 2);

end;

procedure tcheat.SetCheckbox(x:boolean);
begin
  if x then
  begin
    checkbox:=tcheckbox.Create(self);
    checkbox.caption:='';
    checkbox.Left:=0;
    checkbox.top:=(clientheight div 2)-(checkbox.Height div 2);
    checkbox.width:=16;
    checkbox.Parent:=self;
    checkbox.Visible:=true;

    checkbox.OnMouseDown:=md;
    checkbox.enabled:=false;

    hotkeyleft:=checkbox.width+3;
  end
  else
  begin
    if checkbox<>nil then
      freeandnil(checkbox);

    hotkeyleft:=0;
  end;
end;

function tcheat.getCheckboxVisible: boolean;
begin
  result:=checkbox<>nil;
end;

function tcheat.editPresent: boolean;
begin
  result:=edit<>nil;
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

  if checkbox<>nil then
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

  //width:=a;
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


procedure tcheat.seteditwidth(x:integer);
begin
  fEditWidth:=x;

  if edit<>nil then
    edit.width:=x;

  resetwidth;
end;




procedure tcheat.seteditleft(x:integer);
begin
  feditleft:=x;
  if edit<>nil then
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
  result:=feditvalue;

  if edit<>nil then
    result:=edit.Text;
end;

procedure tcheat.SetEditValue(value:string);
begin
  feditvalue:=value;
  if edit<>nil then
    edit.Text:=value;
end;

procedure tcheat.ChangeEdit(present:boolean);
begin
  if present then
  begin
    if edit=nil then
    begin
      edit:=tedit.Create(self);
      edit.Text:=feditvalue;
    end;

    edit.autosize:=false;


    edit.Left:=Self.editleft;
    edit.width:=self.Editwidth;
    edit.Parent:=self;
    edit.Visible:=true;
    edit.BorderStyle:=bsNone;


    edit.top:=hotkeylabel.top;
    edit.height:=hotkeylabel.height;
  end
  else
  begin
    if edit<>nil then
      freeandnil(edit);
  end;

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

procedure TCheat.setDeactivateTimer(interval: integer);
begin
  if deactivatetimer<>nil then
  begin
    deactivatetimer.Interval:=interval;
    deactivatetimer.enabled:=true;

  end;
end;

procedure TCheat.timerdeactivate(sender: tobject);
begin
  activated:=false;
  if deactivatetimer<>nil then
    deactivatetimer.enabled:=false;
end;

constructor tcheat.create(AOwner:tcomponent);
begin
  inherited create(AOwner);

  clientheight:=28;
  feditleft:=200;
  feditwidth:=100;


  deactivatetimer:=TTimer.create(self);
  deactivatetimer.enabled:=false;
  deactivatetimer.OnTimer:=timerdeactivate;

  activationcolor:=clred;


  hotkeylabel:=tlabel.Create(self);
  hotkeylabel.Caption:=rsUndefinedHotkey;
  hotkeylabel.Left:=0;

  hotkeylabel.Top:=1+(clientheight div 2)-(hotkeylabel.Height div 2);
  hotkeylabel.Parent:=self;

  descriptionlabel:=tlabel.Create(self);
  descriptionlabel.Caption:=rsUndefinedDescription;
  descriptionlabel.left:=100;
  descriptionlabel.Top:=1+(clientheight div 2)-(descriptionlabel.Height div 2);
  descriptionlabel.Parent:=self;

  ftextcolor:=hotkeylabel.Font.Color;
  clientWidth:=descriptionlabel.left+descriptionlabel.Width;

  fshowhotkey:=true;

  descriptionlabel.OnMouseDown:=md;
  hotkeylabel.OnMouseDown:=md;


  OnResize:=resizeControl;


  beeponactivate:=true; //default
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

procedure TCheatlist.setBeepOnActivate(x: boolean);
var i: integer;
begin
  fBeepOnActivate:=x;
  for i:=0 to cheatcount-1 do
    cheats[i].beeponactivate:=x;
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
    beeponactivate:=fbeepOnactivate;    
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

  setlength(cheats,0);

//  self.BevelEdges:=[beTop, beBottom,beleft,beright];
 // self.BevelInner:=bvNone;
//  self.BevelOuter:=bvNone;
 // self.BevelKind:=bknone;
 // self.BevelInner:=bvlowered;
 // self.BorderWidth:=2;

 fBeepOnActivate:=true;

end;

destructor tcheatlist.destroy;
var i: integer;
begin
  for i:=0 to cheatcount-1 do cheats[i].free;
  inherited destroy;
end;

function tcheatlist.GetItem(i:integer):tcheat;
begin
  result:=nil;
  if i<=length(cheats)-1 then
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

initialization
  registerclass(TCheat);
  registerclass(TCheatList);

end.
