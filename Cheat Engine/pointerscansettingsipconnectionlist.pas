unit PointerscanSettingsIPConnectionList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, Controls;

resourcestring
  rsPSSICLAdd = 'Add';
  rsPSSICLRemove = 'Remove';
  rsPSSICLHost = 'Host';
  rsPSSICLPort = 'Port';
  rsPSSICLPassword = 'Password';
  rsPSSICLStable = 'Stable';

type
  TIpinfo=class(tobject)
  private
    edtHost: TEdit;
    edtPort: Tedit;
    edtPassword: Tedit;
    cbStable: TCheckBox;

    resizing: boolean;
    function getHost: string;
    function getPort: string;
    function getPassword: string;
    function getStableState: boolean;
    procedure setHost(h: string);
    procedure setPort(p: string);
    procedure setPassword(p: string);
    procedure setStableState(s: boolean);
  public
    procedure clean;
    constructor create(TheOwner: TComponent);
    destructor destroy; override;

    property host: string read getHost write setHost;
    property port: string read getPort write setPort;
    property password: string read getPassword write setPassword;
    property stable: boolean read getStableState write setStableState;
  end;

  TIpList=class(TPanel)
  private
    btnAdd: TButton;
    btnRemove: TButton;
    ipinfo: TList;

    lblHost: TLabel;
    lblPassword: TLabel;
    lblPort: TLabel;
    lblStable: TLabel;
    resizingmyself: boolean;

    fOnWantedToDeleteLastItem: TNotifyEvent;

    pnlHPPS: tpanel;

    procedure addIpInfo(sender: tobject);
    procedure removeIpInfo(sender: tobject);
    procedure updatePositions;
    function getCount: integer;
    function getItem(index: integer): TIpInfo;
  public
    procedure clear;
    procedure add;
    destructor destroy; override;
    constructor create(TheOwner: TComponent); override;
    property OnWantedToDeleteLastItem: TNotifyEvent read fOnWantedToDeleteLastItem write fOnWantedToDeleteLastItem;

    property count: integer read getCount;
    property item[index: integer]: TIpInfo read getItem; default;
  end;

implementation

procedure TIpinfo.setHost(h: string);
begin
  edtHost.text:=h;
end;

procedure TIpinfo.setPort(p: string);
begin
  edtPort.text:=p;
end;

procedure TIpinfo.setPassword(p: string);
begin
  edtPassword.text:=p;
end;

procedure TIpinfo.setStableState(s: boolean);
begin
  cbStable.checked:=s;
end;

function TIpinfo.getHost: string;
begin
  result:=edtHost.Text;
end;

function TIpinfo.getPort: string;
begin
  result:=edtPort.text;
end;

function TIpinfo.getPassword: string;
begin
  result:=edtPassword.text;
end;

function TIpinfo.getStableState: boolean;
begin
  result:=cbStable.Checked;
end;

procedure TIpinfo.clean;
begin
  edtHost.text:='';
  edtPort.text:='52737';
  edtPassword.text:='';
  cbStable.checked:=false;
end;

constructor TIpinfo.create(TheOwner: TComponent);
begin
  inherited create;

  if TheOwner is TWinControl then
  begin
    edtHost:=Tedit.create(theowner);
    edtPort:=Tedit.create(theowner);
    edtPort.text:='52737';
    edtPassword:=Tedit.create(theowner);
    cbStable:=TCheckBox.create(theowner);
    cbStable.caption:='';
    cbStable.autoSize:=true;

    edtPassword.PasswordChar:='*';
    edtPassword.MaxLength:=255;

    edtHost.Parent:=twincontrol(theowner);
    edtPort.Parent:=twincontrol(theowner);
    edtPassword.Parent:=twincontrol(theowner);
    cbStable.Parent:=twincontrol(theowner);

  end;
end;

destructor TIpinfo.destroy;
var p: TWinControl;
begin
  p:=cbStable.Parent;
  p.BeginUpdateBounds;

  cbStable.free;
  edtPassword.free;
  edtPort.free;
  edtHost.free;

  p.EndUpdateBounds;
end;


{}

function TIpList.getCount: integer;
begin
  result:=ipinfo.count;
end;

function TIpList.getItem(index: integer): TIpInfo;
begin
  if index<count then
    result:=TIpInfo(ipinfo[index])
  else
    result:=nil;
end;



procedure TIpList.addIpInfo(sender: tobject);
var newipinfo: TIpInfo;
begin
  newipinfo:=TIpInfo.Create(pnlHPPS);
  ipinfo.Add(newipinfo);
  updatePositions;
end;

procedure TIpList.removeIpInfo(sender: tobject);
begin
  if ipinfo.count>1 then
  begin
    tipinfo(ipinfo[ipinfo.count-1]).Free;
    ipinfo.delete(ipinfo.count-1);
  end
  else
  begin
    tipinfo(ipinfo[ipinfo.count-1]).clean;
    if assigned(fOnWantedToDeleteLastItem) then
      fOnWantedToDeleteLastItem(self);
  end;

  updatePositions;

end;

procedure TIpList.updatePositions;
var
  currentipinfo: TIpInfo;
  i: integer;
  currenttop: integer;

begin


  resizingmyself:=true;
  try
    currentipinfo:=nil;

    currenttop:=lblHost.height;
    for i:=0 to ipinfo.count-1 do
    begin
      currentipinfo:=TIpinfo(ipinfo[i]);

      currentipinfo.edtHost.Constraints.MinWidth:=canvas.TextWidth(' XXX.XXX.XXX.XXX ');
      currentipinfo.edtHost.Constraints.MaxWidth:=canvas.TextWidth(' XXX.XXX.XXX.XXX ');

      currentipinfo.edtPort.Constraints.MinWidth:=canvas.TextWidth('XXXXX');
      currentipinfo.edtPort.Constraints.MaxWidth:=canvas.TextWidth(' XXXXX ');

      currentipinfo.edtPassword.Constraints.MinWidth:=canvas.TextWidth('XXXXXXXXX');
      currentipinfo.edtPassword.Constraints.MaxWidth:=canvas.TextWidth(' XXXXXXXXX ');
    end;

//    height:=btnAdd.top+btnAdd.height+4;

  finally
    resizingmyself:=false;
  end;
end;

procedure TIpList.add;
begin
  addIpInfo(self);
end;

procedure TIpList.clear;
begin
  while ipinfo.count>0 do
  begin
    TIpinfo(ipinfo[0]).free;
    ipinfo.Delete(0);
  end;
end;

destructor TIpList.destroy;
begin
  if btnAdd<>nil then
    btnAdd.free;

  if btnRemove<>nil then
    btnRemove.free;

  clear;

  if ipinfo<>nil then
    freeandnil(ipinfo);

  inherited destroy;
end;

constructor TIpList.create(TheOwner: TComponent);
begin
  inherited create(TheOwner);

  ipinfo:=TList.create;
  bevelouter:=bvNone;
  visible:=false;


  if theowner is TWinControl then
  begin
    parent:=TWinControl(TheOwner);

    pnlHPPS:=tpanel.Create(self);
    pnlHPPS.BevelOuter:=bvNone;
    pnlHPPS.parent:=self;
    pnlHPPS.ChildSizing.ControlsPerLine:=4;
    pnlHPPS.ChildSizing.EnlargeHorizontal:=crsHomogenousChildResize;
    pnlHPPS.ChildSizing.HorizontalSpacing:=3;
    pnlHPPS.ChildSizing.VerticalSpacing:=1;
    pnlHPPS.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
    pnlHPPS.autosize:=true;

    //pnlHPPS.color:=$ff0000;


    //width:=twincontrol(TheOwner).ClientWidth;

    btnAdd:=tbutton.create(self);
    btnRemove:=tbutton.create(self);

    btnAdd.caption:=rsPSSICLAdd;
    btnRemove.caption:=rsPSSICLRemove;

    btnAdd.parent:=self;
    btnRemove.parent:=self;

    btnAdd.autosize:=true;
    btnRemove.AutoSize:=true;

    btnRemove.ClientWidth:=self.Canvas.GetTextWidth(btnRemove.caption)+8;

    btnAdd.Width:=btnRemove.Width;


    btnAdd.OnClick:=@addIpInfo;
    btnRemove.OnClick:=@removeIpInfo;

    lblHost:=tlabel.create(self);
    lblHost.caption:=rsPSSICLHost;
    lblHost.parent:=pnlhpps;

    lblPort:=tlabel.create(self);
    lblPort.caption:=rsPSSICLPort;
    lblPort.parent:=pnlhpps;

    lblPassword:=tlabel.Create(self);
    lblPassword.caption:=rsPSSICLPassword;
    lblPassword.parent:=pnlhpps;

    lblStable:=tlabel.create(self);
    lblStable.caption:=rsPSSICLStable;
    lblStable.parent:=pnlhpps;

    pnlHPPS.AnchorSideTop.control:=self;
    pnlHPPS.AnchorSideTop.Side:=asrTop;
    pnlHPPS.AnchorSideLeft.control:=self;
    pnlHPPS.AnchorSideLeft.Side:=asrLeft;



    btnAdd.AnchorSideTop.control:=pnlHPPS;
    btnAdd.AnchorSideTop.Side:=asrbottom;
    btnAdd.AnchorSideLeft.control:=self;
    btnAdd.AnchorSideLeft.Side:=asrleft;
    btnAdd.BorderSpacing.Top:=3;


    btnRemove.AnchorSideTop.control:=btnAdd;
    btnRemove.AnchorSideTop.Side:=asrTop;
    btnRemove.AnchorSideLeft.control:=btnAdd;
    btnRemove.AnchorSideLeft.Side:=asrRight;
    btnRemove.BorderSpacing.Left:=4;




    autosize:=true;

    //color:=$00ff00;

    addIpInfo(self);
  end;
end;

end.

