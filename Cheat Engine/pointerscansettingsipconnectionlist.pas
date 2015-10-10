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
  TIpinfo=class(TPanel)
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
  protected
    procedure Resize; override;
  public
    procedure clean;
    constructor create(TheOwner: TComponent); override;

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

    procedure IpInfoResize(sender: TObject);
    procedure addIpInfo(sender: tobject);
    procedure removeIpInfo(sender: tobject);
    procedure updatePositions;
    function getCount: integer;
    function getItem(index: integer): TIpInfo;
  protected
    procedure Resize; override;
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

procedure TIpinfo.Resize;
begin
  inherited Resize;

  if edtHost<>nil then
    edtHost.Width:=clientwidth div 2;
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
  inherited create(TheOwner);

  visible:=false;

  if TheOwner is TWinControl then
  begin
    parent:=twincontrol(TheOwner);
    width:=twincontrol(TheOwner).ClientWidth;

    bevelouter:=bvNone;
    edtHost:=Tedit.create(self);
    edtHost.parent:=self;

    edtPort:=Tedit.create(self);
    edtPort.parent:=self;
    edtPort.text:='52737';
    edtPort.ClientWidth:=self.Canvas.GetTextWidth(edtport.text)+8;

    edtPassword:=Tedit.create(self);
    edtPassword.parent:=self;

    cbStable:=TCheckBox.create(self);
    cbStable.parent:=self;
    cbStable.caption:='';
    cbStable.autoSize:=true;

    cbStable.AnchorSideRight.Control:=self;
    cbStable.AnchorSideRight.Side:=asrRight;
    cbStable.anchors:=[akTop, akRight];
   /// cbStable.BorderSpacing.Right:=4;//self.canvas.GetTextWidth('Stable') div 2;

    edtPassword.AnchorSideRight.control:=cbStable;
    edtPassword.AnchorSideRight.Side:=asrLeft;
    edtPassword.AnchorSideLeft.control:=edtPort;
    edtPassword.AnchorSideLeft.Side:=asrRight;
    edtPassword.BorderSpacing.Right:=4; //self.canvas.GetTextWidth('Stable') div 2;
    edtPassword.BorderSpacing.Left:=4;
    edtPassword.anchors:=[akTop, akLeft, akRight];

    edtPassword.PasswordChar:='*';
    edtPassword.MaxLength:=255;


    edtPort.AnchorSideLeft.control:=edtHost;
    edtPort.AnchorSideLeft.side:=asrRight;
    edtPort.BorderSpacing.Left:=4;
    edtPort.anchors:=[akLeft, akTop];

    clientheight:=edtHost.height+1;
  end;

  visible:=true;

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

procedure TIpList.IpInfoResize(sender: TObject);
begin
  if count>0 then
  begin
    lblhost.left:=0;
    lblport.left:=item[0].edtPort.left;
    lblPassword.left:=item[0].edtPassword.left;
    lblStable.left:=clientwidth-lblStable.width-4;
  end;
end;

procedure TIpList.addIpInfo(sender: tobject);
var newipinfo: TIpInfo;
begin
  newipinfo:=TIpInfo.Create(self);
  newipinfo.parent:=self;
  newipinfo.width:=clientwidth;
  newipinfo.Anchors:=[akTop, akLeft, akRight];

  newipinfo.OnResize:=@IpInfoResize;
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

procedure TIpList.Resize;
begin
  if resizingmyself=false then
  begin
    resizingmyself:=true;
    try
      if (lblHost<>nil) then
        updatePositions;
    finally
      resizingmyself:=false;
    end;
  end;

  inherited Resize;
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
      currentipinfo.top:=currenttop;
      inc(currenttop, currentipinfo.height+1);
    end;

    btnAdd.Top:=currentTop;
    btnRemove.top:=btnAdd.top;

    btnAdd.Left:=(clientwidth div 2)-(btnAdd.width-4);
    btnRemove.Left:=(clientwidth div 2)+4;

    height:=btnAdd.top+btnAdd.height+4;

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
    //width:=twincontrol(TheOwner).ClientWidth;

    btnAdd:=tbutton.create(self);
    btnRemove:=tbutton.create(self);

    btnAdd.caption:=rsPSSICLAdd;
    btnRemove.caption:=rsPSSICLRemove;

    btnAdd.parent:=self;
    btnRemove.parent:=self;
    btnRemove.ClientWidth:=self.Canvas.GetTextWidth(btnRemove.caption)+8;

    btnAdd.Width:=btnRemove.Width;


    btnAdd.OnClick:=@addIpInfo;
    btnRemove.OnClick:=@removeIpInfo;

    lblHost:=tlabel.create(self);
    lblHost.caption:=rsPSSICLHost;
    lblHost.parent:=self;

    lblPort:=tlabel.create(self);
    lblPort.caption:=rsPSSICLPort;
    lblPort.parent:=self;

    lblPassword:=tlabel.Create(self);
    lblPassword.caption:=rsPSSICLPassword;
    lblPassword.parent:=self;

    lblStable:=tlabel.create(self);
    lblStable.caption:=rsPSSICLStable;
    lblStable.parent:=self;

    addIpInfo(self);
  end;
end;

end.

