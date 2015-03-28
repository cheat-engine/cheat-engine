unit frmRescanPointerUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, LResources, EditBtn, contnrs, cefuncproc, symbolhandler,
  multilineinputqueryunit, lua, lualib, lauxlib, registry, resolve;

type

  { TfrmRescanPointer }

  TfrmRescanPointer = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbDelay: TCheckBox;
    cbBasePointerMustBeInRange: TCheckBox;
    cbMustStartWithSpecificOffsets: TCheckBox;
    cbMustEndWithSpecificOffsets: TCheckBox;
    cbRepeat: TCheckBox;
    cbNoValueCheck: TCheckBox;
    cbLuaFilter: TCheckBox;
    cbUseSavedPointermap: TCheckBox;
    lblLuaParams: TLabel;
    edtRescanFunction: TEdit;
    edtBaseStart: TEdit;
    edtBaseEnd: TEdit;
    edtDelay: TEdit;
    edtAddress: TEdit;
    cbValueType: TComboBox;
    Label1: TLabel;
    lblAnd: TLabel;
    odLoadPointermap: TOpenDialog;
    pnlButtons: TPanel;
    Panel2: TPanel;
    rbFindAddress: TRadioButton;
    rbFindValue: TRadioButton;
    procedure btnNotifySpecificIPsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbBasePointerMustBeInRangeChange(Sender: TObject);
    procedure cbBroadcastChange(Sender: TObject);
    procedure cbDistributedRescanChange(Sender: TObject);
    procedure cbLuaFilterChange(Sender: TObject);
    procedure cbMustEndWithSpecificOffsetsChange(Sender: TObject);
    procedure cbMustStartWithSpecificOffsetsChange(Sender: TObject);
    procedure cbNoValueCheckChange(Sender: TObject);
    procedure cbUseSavedPointermapChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbFindAddressClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

    startoffsets: TComponentList;
    endoffsets: TComponentList;

    btnAddEndOffset, btnRemoveEndOffset: TButton;
    btnAddStartOffset, btnRemoveStartOffset: Tbutton;
    lblInfoFirstOffset, lblInfoLastOffset: TLabel;



    fdelay: integer;
    fBaseStart: ptruint;
    fBaseEnd: ptruint;

    iplist: TStringList;

    procedure updatepositions;
    procedure btnAddStartOffsetClick(sender: TObject);
    procedure btnRemoveStartOffsetClick(sender: TObject);
    procedure btnAddEndOffsetClick(sender: TObject);
    procedure btnRemoveEndOffsetClick(sender: TObject);
  public
    { Public declarations }
    resolvediplist: array of THostAddr;
    distributedport: integer;

    startOffsetValues, endoffsetvalues: Array of dword;
    property Delay: integer read fdelay;
    property BaseStart: ptruint read fBaseStart;
    property BaseEnd: ptruint read fBaseEnd;
  end;

implementation

uses LuaHandler;

resourcestring
  rsNotAllTheStartOffsetsHaveBeenFilledIn = 'Not all the start offsets have '
    +'been filled in';
  rsNotAllTheEndOffsetsHaveBeenFilledIn = 'Not all the end offsets have been '
    +'filled in';
  rsAdd = 'Add';
  rsRemove = 'Remove';


procedure TfrmRescanPointer.rbFindAddressClick(Sender: TObject);
begin
  if rbFindAddress.Checked then
  begin
    edtAddress.Width:=cbValueType.Left+cbValueType.Width-edtAddress.Left;
    cbValueType.Visible:=false;
  end
  else
  begin
    edtAddress.Width:=panel2.Width;
    cbValueType.Visible:=true;
  end;
end;

procedure TfrmRescanPointer.cbBasePointerMustBeInRangeChange(Sender: TObject);
begin
  edtBaseStart.enabled:=cbBasePointerMustBeInRange.checked;
  lblAnd.enabled:=cbBasePointerMustBeInRange.checked;
  edtBaseEnd.enabled:=cbBasePointerMustBeInRange.checked;
end;

procedure TfrmRescanPointer.cbBroadcastChange(Sender: TObject);
begin

end;

procedure TfrmRescanPointer.cbDistributedRescanChange(Sender: TObject);
begin


end;



procedure TfrmRescanPointer.cbLuaFilterChange(Sender: TObject);
begin
  edtRescanFunction.enabled:=cbLuaFilter.checked;
end;

procedure TfrmRescanPointer.Button1Click(Sender: TObject);
var
  i: integer;
  s: string;
  r: THostResolver;
begin
  //evaluate the given offsets and range
  fDelay:=strtoint(edtDelay.Text);


  fBaseStart:=symhandler.getAddressFromName(edtBaseStart.text);
  fBaseEnd:=symhandler.getAddressFromName(edtBaseEnd.text);

  if startoffsets<>nil then
  begin
    setlength(startOffsetValues, startoffsets.count);

    for i:=0 to startoffsets.count-1 do
    begin
      s:=tedit(startoffsets[i]).text;
      if length(s)=0 then
        raise exception.create(rsNotAllTheStartOffsetsHaveBeenFilledIn);

      if s[1]='-' then
        startoffsetvalues[i]:=StrToInt('-$'+copy(s,2,length(s)))
      else
        startoffsetvalues[i]:=StrToInt('$'+s);
    end;
  end
  else
    setlength(startoffsetvalues,0);


  if endoffsets<>nil then
  begin
    setlength(endOffsetValues, endoffsets.count);

    for i:=0 to Endoffsets.count-1 do
    begin
      s:=tedit(Endoffsets[i]).text;
      if length(s)=0 then
        raise exception.create(rsNotAllTheEndOffsetsHaveBeenFilledIn);

      if s[1]='-' then
        Endoffsetvalues[i]:=StrToInt('-$'+copy(s,2,length(s)))
      else
        Endoffsetvalues[i]:=StrToInt('$'+s);
    end;
  end
  else
    setlength(endoffsetvalues,0);

  if cbLuaFilter.checked then
  begin
    //check that the filter function is defined
    lua_getglobal(LuaVM, pchar(edtRescanFunction.Text));
    try
      if not lua_isfunction(Luavm,-1) then
        raise exception.create('The function '+edtRescanFunction.Text+'(base, offsets, target) has not yet been defined. Please define it first');
    finally
      lua_pop(Luavm,1);
    end;


  end;

  modalresult:=mrok;
end;

procedure TfrmRescanPointer.btnNotifySpecificIPsClick(Sender: TObject);
var
  reg: Tregistry;
begin
  reg:=TRegistry.create;
  try
    if MultilineInputQuery('IP List','Enter the IP addresses to notify explicitly', iplist) then  //save the new ip list
    begin
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey('\Software\Cheat Engine',true) then
        reg.WriteString('Worker IP List', iplist.text);
    end;

  finally
    reg.free;
  end;
end;

procedure TfrmRescanPointer.cbMustEndWithSpecificOffsetsChange(Sender: TObject);
var e: Tedit;
begin
  if cbMustendWithSpecificOffsets.checked then
  begin
    //create the first offset block
    endoffsets:=TComponentList.create;
    endoffsets.OwnsObjects:=true;

    e:=TEdit.Create(self);
    e.Top:=cbMustendWithSpecificOffsets.Top+cbMustendWithSpecificOffsets.Height+3;
    e.left:=cbMustendWithSpecificOffsets.left;
    e.Parent:=self;
    endoffsets.Add(e);

    if lblInfoLastOffset=nil then
    begin
      lblInfoLastOffset:=TLabel.create(self);
      lblInfoLastOffset.caption:='Last offset';
      lblInfoLastOffset.left:=e.Left+e.Width+5;
      lblInfoLastOffset.parent:=self;
      lblInfoLastOffset.visible:=false;
    end;

    if btnAddendOffset=nil then
    begin
      btnAddendOffset:=TButton.create(self);
      btnAddendOffset.caption:=rsAdd;
      btnAddendOffset.Top:=e.top;
      btnAddendOffset.left:=e.Left+e.Width+3;
      btnAddendOffset.width:=60;
      btnAddendOffset.height:=e.Height;
      btnAddendOffset.onclick:=btnAddendOffsetClick;
      btnAddendOffset.parent:=self;
    end;


    if btnRemoveendOffset=nil then
    begin
      btnRemoveendOffset:=TButton.create(self);
      btnRemoveendOffset.caption:=rsRemove;
      btnRemoveendOffset.Top:=btnAddendOffset.top;
      btnRemoveendOffset.left:=btnAddendOffset.Left+btnAddendOffset.Width+3;
      btnRemoveendOffset.width:=btnAddendOffset.width;
      btnRemoveendOffset.height:=btnAddendOffset.height;
      btnRemoveendOffset.OnClick:=btnRemoveendOffsetClick;
      btnRemoveendOffset.parent:=self;
    end;

    btnAddendOffset.visible:=true;
    btnRemoveendOffset.visible:=true;
  end
  else
  begin
    //delete all end offsets
    if btnAddendOffset<>nil then
      btnAddendOffset.visible:=false;

    if btnRemoveendOffset<>nil then
      btnRemoveendOffset.visible:=false;

    if lblInfoLastOffset<>nil then
      lblInfoLastOffset.visible:=false;

    if endoffsets<>nil then
      freeandnil(endoffsets);
  end;

  updatePositions;
end;

procedure TfrmRescanPointer.cbMustStartWithSpecificOffsetsChange(Sender: TObject);
var e: Tedit;
begin
  if cbMustStartWithSpecificOffsets.checked then
  begin
    //create the first offset block
    startoffsets:=TComponentList.create;
    startoffsets.OwnsObjects:=true;

    e:=TEdit.Create(self);
    e.left:=cbMustStartWithSpecificOffsets.left;
    e.Parent:=self;
    startoffsets.Add(e);

    if lblInfoFirstOffset=nil then
    begin
      lblInfoFirstOffset:=TLabel.create(self);
      lblInfoFirstOffset.caption:='First offset';
      lblInfoFirstOffset.left:=e.Left+e.Width+5;
      lblInfoFirstOffset.parent:=self;
      lblInfoFirstOffset.visible:=false;
    end;

    if btnAddStartOffset=nil then
    begin
      btnAddStartOffset:=TButton.create(self);
      btnAddStartOffset.caption:=rsAdd;
      btnAddStartOffset.left:=e.Left+e.Width+3;
      btnAddStartOffset.width:=60;
      btnAddStartOffset.height:=e.Height;
      btnAddStartOffset.onclick:=btnAddStartOffsetClick;
      btnAddStartOffset.parent:=self;
    end;

    if btnRemoveStartOffset=nil then
    begin
      btnRemoveStartOffset:=TButton.create(self);
      btnRemoveStartOffset.caption:=rsRemove;
      btnRemoveStartOffset.left:=btnAddStartOffset.Left+btnAddStartOffset.Width+3;
      btnRemoveStartOffset.width:=btnAddStartOffset.width;
      btnRemoveStartOffset.height:=btnAddStartOffset.height;
      btnRemoveStartOffset.OnClick:=btnRemoveStartOffsetClick;
      btnRemoveStartOffset.parent:=self;
    end;

    btnAddStartOffset.visible:=true;
    btnRemoveStartOffset.visible:=true;
  end
  else
  begin
    //delete all start offsets
    if btnAddStartOffset<>nil then
      btnAddStartOffset.visible:=false;

    if btnRemoveStartOffset<>nil then
      btnRemoveStartOffset.visible:=false;

    if lblInfoFirstOffset<>nil then
      lblInfoFirstOffset.visible:=false;

    if startoffsets<>nil then
      freeandnil(startoffsets);
  end;

  updatePositions;
end;

procedure TfrmRescanPointer.cbNoValueCheckChange(Sender: TObject);
var newstate: boolean;
begin
  newstate:=not cbNoValueCheck.checked;
  rbFindAddress.enabled:=newstate;
  rbFindValue.enabled:=newstate;
  edtAddress.enabled:=newstate;
  cbValueType.enabled:=newstate;
end;

procedure TfrmRescanPointer.cbUseSavedPointermapChange(Sender: TObject);
begin
  if cbUseSavedPointermap.checked then
  begin
    if odLoadPointermap.execute then
      cbUseSavedPointermap.caption:='Use saved pointermap: '+ExtractFileName(odLoadPointermap.FileName)
    else
      cbUseSavedPointermap.checked:=false;
  end
  else
    cbUseSavedPointermap.Caption:='Use saved pointermap';

  if cbUseSavedPointermap.checked then
  begin
    rbFindValue.checked:=false;
    rbFindValue.enabled:=false;
    rbFindAddress.Checked:=true;
    cbRepeat.Checked:=false;
    cbRepeat.Enabled:=false; //really no use rescanning an never changing static pointermap
    cbNoValueCheck.Checked:=false;
    cbNoValueCheck.Enabled:=false;
    cbDelay.checked:=false;
    cbDelay.enabled:=false;
  end
  else
  begin
    rbFindValue.enabled:=true;
    cbRepeat.Enabled:=true;
    cbNoValueCheck.Enabled:=true;
    cbDelay.enabled:=true;
  end;
end;

procedure TfrmRescanPointer.FormDestroy(Sender: TObject);
begin
  if iplist<>nil then
    freeandnil(iplist);
end;

procedure TfrmRescanPointer.FormShow(Sender: TObject);
begin
  updatePositions;
end;

procedure TfrmRescanPointer.updatePositions;
{
Updates the pnlButtons panel position and adjusts the form height
}
var e: Tedit;
  i: integer;
  nextstart: integer;
begin
  if cbMustStartWithSpecificOffsets.Checked then
  begin
    nextstart:=cbMustStartWithSpecificOffsets.top+cbMustStartWithSpecificOffsets.height+3;
    for i:=0 to startoffsets.count-1 do
    begin
      e:=tedit(startoffsets[i]);
      e.top:=nextstart;

      nextstart:=nextstart+e.height+3;
    end;

    if startoffsets.count=1 then lblInfoFirstOffset.visible:=false
    else
    begin
      lblInfoFirstOffset.visible:=true;
      lblInfoFirstOffset.top:=tedit(startoffsets[0]).top+4;
    end;

    btnAddStartOffset.Top:=e.top;
    btnRemoveStartOffset.top:=e.Top;

    //set the position of the Start buttons
    cbMustEndWithSpecificOffsets.Top:=btnAddStartOffset.top+btnAddStartOffset.height+5
  end
  else
    cbMustEndWithSpecificOffsets.Top:=cbMustStartWithSpecificOffsets.Top+cbMustStartWithSpecificOffsets.height+5;

  if cbMustEndWithSpecificOffsets.checked then
  begin
    nextstart:=cbMustEndWithSpecificOffsets.top+cbMustEndWithSpecificOffsets.Height+3;

    btnAddEndOffset.top:=nextstart;
    btnRemoveEndOffset.top:=nextstart;

    for i:=0 to endoffsets.count-1 do
    begin
      e:=tedit(endoffsets[i]);
      e.top:=nextstart;

      nextstart:=nextstart+e.height+3;
    end;

    if endoffsets.count=1 then lblInfoLastOffset.visible:=false
    else
    begin
      lblInfoLastOffset.visible:=true;
      lblInfoLastOffset.top:=tedit(endoffsets[endoffsets.count-1]).top+4;
    end;

    pnlButtons.top:=nextstart;
  end
  else
  begin
    pnlButtons.top:=cbMustEndWithSpecificOffsets.top+cbMustEndWithSpecificOffsets.height+4;
  end;

  clientheight:=pnlbuttons.top+pnlButtons.height;
end;

procedure TfrmRescanPointer.btnAddStartOffsetClick(sender: TObject);
var e: Tedit;
begin
  e:=Tedit.create(self);
  e.left:=cbMustStartWithSpecificOffsets.left;
  e.Parent:=self;
  startoffsets.Add(e);

  updatePositions;
end;

procedure TfrmRescanPointer.btnRemoveStartOffsetClick(sender: TObject);
begin
  if startoffsets.count=1 then
    cbMustStartWithSpecificOffsets.checked:=false
  else
  begin
    startoffsets.Delete(startoffsets.count-1);
    updatePositions;
  end;
end;

procedure TfrmRescanPointer.btnAddEndOffsetClick(sender: TObject);
var e: Tedit;
begin
  e:=Tedit.create(self);
  e.left:=cbMustStartWithSpecificOffsets.left;
  e.Parent:=self;
  endoffsets.Insert(0,e);
  updatePositions;
end;

procedure TfrmRescanPointer.btnRemoveEndOffsetClick(sender: TObject);
begin
  if endoffsets.count=1 then
    cbMustEndWithSpecificOffsets.checked:=false
  else
  begin
    endoffsets.Delete(0);
    updatePositions;
  end;
end;

procedure TfrmRescanPointer.FormCreate(Sender: TObject);
var reg: Tregistry;
begin
  rbFindAddressClick(rbFindAddress);

  {$ifdef cpu64}
  edtBaseStart.text:='0000000000000000';
  edtBaseEnd.text:='FFFFFFFFFFFFFFFF';
  {$else}
  edtBaseStart.text:='00000000';
  edtBaseEnd.text:='FFFFFFFF';
  {$endif}

  iplist:=TStringList.create;
  //load the ip list (if there is one)

  reg:=tregistry.create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Cheat Engine',false) then
    begin
      if reg.ValueExists('Worker IP List') then
        iplist.Text:=reg.ReadString('Worker IP List');
    end;

  finally
    reg.free;
  end;
end;

initialization
  {$i frmRescanPointerUnit.lrs}

end.
