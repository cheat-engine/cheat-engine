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
    cbBasePointerMustBeInRange: TCheckBox;
    cbDelay: TCheckBox;
    cbLuaFilter: TCheckBox;
    cbMustEndWithSpecificOffsets: TCheckBox;
    cbMustStartWithSpecificOffsets: TCheckBox;
    cbNoValueCheck: TCheckBox;
    cbFilterOutAccessible: TCheckBox;
    cbRepeat: TCheckBox;
    cbUseSavedPointermap: TCheckBox;
    cbValueType: TComboBox;
    cbChangeBasePointerOffset: TCheckBox;
    edtNewBase: TEdit;
    edtAddress: TEdit;
    edtBaseEnd: TEdit;
    edtBaseStart: TEdit;
    edtDelay: TEdit;
    edtRescanFunction: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblOriginalBase: TLabel;
    lblOffset: TLabel;
    lblAnd: TLabel;
    lblLuaParams: TLabel;
    odLoadPointermap: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlRangeOffset: TPanel;
    pnlButtons: TPanel;
    rbFindAddress: TRadioButton;
    rbFindValue: TRadioButton;
    procedure btnNotifySpecificIPsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cbBasePointerMustBeInRangeChange(Sender: TObject);
    procedure cbBroadcastChange(Sender: TObject);
    procedure cbChangeBasePointerOffsetChange(Sender: TObject);
    procedure cbDistributedRescanChange(Sender: TObject);
    procedure cbLuaFilterChange(Sender: TObject);
    procedure cbMustEndWithSpecificOffsetsChange(Sender: TObject);
    procedure cbMustStartWithSpecificOffsetsChange(Sender: TObject);
    procedure cbNoValueCheckChange(Sender: TObject);
    procedure cbFilterOutAccessibleChange(Sender: TObject);
    procedure cbUseSavedPointermapChange(Sender: TObject);
    procedure edtNewBaseChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Notebook1ChangeBounds(Sender: TObject);
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
    fOffset: ptrint;
    procedure setOffset(o: ptrint);
    function getOffset:ptrint;

    procedure updatepositions;
    procedure btnAddStartOffsetClick(sender: TObject);
    procedure btnRemoveStartOffsetClick(sender: TObject);
    procedure btnAddEndOffsetClick(sender: TObject);
    procedure btnRemoveEndOffsetClick(sender: TObject);
  public
    { Public declarations }
    resolvediplist: array of THostAddr;
    distributedport: integer;
    canceled: boolean;

    startOffsetValues, endoffsetvalues: Array of dword;
    property Delay: integer read fdelay;
    property BaseStart: ptruint read fBaseStart;
    property BaseEnd: ptruint read fBaseEnd;
    property offset: ptrint read getOffset write setOffset;
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
  rsRPTheFunction = 'The function %s(base, offsets, target) has not yet been defined. Please define it first';
  rsRPIpList = 'IP List';
  rsRPEnterTheIpAddressesToNotifyExplicitly = 'Enter the IP addresses to notify explicitly';
  rsRPLastOffset = 'Last offset';
  rsRPFirstOffset = 'First offset';
  rsRPUseSavedPointermap = 'Use saved pointermap: ';
  rsRPUseSavedPointermap2 = 'Use saved pointermap';

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

procedure TfrmRescanPointer.cbChangeBasePointerOffsetChange(Sender: TObject);
begin
  pnlRangeOffset.enabled:=cbChangeBasePointerOffset.checked;
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
  luavm: PLua_state;
begin
  //evaluate the given offsets and range
  luavm:=GetLuaState;

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
        raise exception.create(format(rsRPTheFunction,[edtRescanFunction.Text]));
    finally
      lua_pop(Luavm,1);
    end;


  end;

  edtNewBaseChange(edtNewBase);

  canceled:=false;
  modalresult:=mrok;
end;

procedure TfrmRescanPointer.Button2Click(Sender: TObject);
begin
  canceled:=true;
  modalresult:=mrcancel;
end;

procedure TfrmRescanPointer.btnNotifySpecificIPsClick(Sender: TObject);
var
  reg: Tregistry;
begin
  reg:=TRegistry.create;
  try
    if MultilineInputQuery(rsRPIpList,rsRPEnterTheIpAddressesToNotifyExplicitly, iplist) then  //save the new ip list
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
    e.AnchorSideTop.Control:=cbMustEndWithSpecificOffsets;
    e.AnchorSideTop.Side:=asrBottom;
    e.AnchorSideLeft.Control:=cbMustEndWithSpecificOffsets;
    e.AnchorSideLeft.Side:=asrLeft;
    e.Parent:=panel1;
    endoffsets.Add(e);

    if lblInfoLastOffset=nil then
    begin
      lblInfoLastOffset:=TLabel.create(self);
      lblInfoLastOffset.caption:=rsRPLastOffset;
      lblInfoLastOffset.BorderSpacing.Left:=7;
      lblInfoLastOffset.parent:=panel1;
      lblInfoLastOffset.visible:=false;
    end;

    lblInfoLastOffset.AnchorSideLeft.Control:=e;
    lblInfoLastOffset.AnchorSideLeft.Side:=asrRight;
    lblInfoLastOffset.AnchorSideTop.Control:=e;
    lblInfoLastOffset.AnchorSideTop.Side:=asrCenter;

    if btnAddendOffset=nil then
    begin
      btnAddendOffset:=TButton.create(self);
      btnAddendOffset.Constraints.MinWidth:=60;
      btnAddendOffset.autosize:=true;
      btnAddendOffset.caption:=rsAdd;
      btnAddendOffset.onclick:=btnAddendOffsetClick;
      btnAddendOffset.parent:=panel1;
    end;

    btnAddEndOffset.AnchorSideLeft.Control:=e;
    btnAddEndOffset.AnchorSideLeft.Side:=asrRight;
    btnAddEndOffset.AnchorSideTop.Control:=e;
    btnAddEndOffset.AnchorSideTop.Side:=asrCenter;
    btnAddEndOffset.BorderSpacing.Left:=7;


    if btnRemoveendOffset=nil then
    begin
      btnRemoveendOffset:=TButton.create(self);
      btnRemoveendOffset.caption:=rsRemove;
      btnRemoveendOffset.Constraints.MinWidth:=60;
      btnRemoveendOffset.AnchorSideLeft.Control:=btnAddendOffset;
      btnRemoveendOffset.AnchorSideLeft.Side:=asrRight;
      btnRemoveendOffset.AnchorSideTop.Control:=btnAddendOffset;
      btnRemoveendOffset.AnchorSideTop.Side:=asrTop;
      btnRemoveendOffset.BorderSpacing.Left:=7;
      btnRemoveendOffset.AutoSize:=true;
      btnRemoveendOffset.OnClick:=btnRemoveendOffsetClick;
      btnRemoveendOffset.parent:=panel1;
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
    e.AnchorSideTop.Control:=cbMustStartWithSpecificOffsets;
    e.AnchorSideTop.Side:=asrBottom;
    e.AnchorSideLeft.Control:=cbMustStartWithSpecificOffsets;
    e.AnchorSideLeft.Side:=asrLeft;
    e.Parent:=panel1;
    startoffsets.Add(e);

    if lblInfoFirstOffset=nil then
    begin
      lblInfoFirstOffset:=TLabel.create(self);
      lblInfoFirstOffset.caption:=rsRPFirstOffset;
      lblInfoFirstOffset.parent:=panel1;
      lblInfoFirstOffset.BorderSpacing.Left:=7;

      lblInfoFirstOffset.visible:=false;
    end;
    lblInfoFirstOffset.AnchorSideLeft.Control:=e;
    lblInfoFirstOffset.AnchorSideLeft.Side:=asrRight;
    lblInfoFirstOffset.AnchorSideTop.Control:=e;
    lblInfoFirstOffset.AnchorSideTop.Side:=asrCenter;


    if btnAddStartOffset=nil then
    begin
      btnAddStartOffset:=TButton.create(self);
      btnAddStartOffset.caption:=rsAdd;
      btnAddStartOffset.Constraints.MinWidth:=60;
      btnAddStartOffset.AutoSize:=true;

      btnAddStartOffset.onclick:=btnAddStartOffsetClick;
      btnAddStartOffset.parent:=panel1;
    end;

    btnAddStartOffset.AnchorSideLeft.Control:=e;
    btnAddStartOffset.AnchorSideLeft.Side:=asrRight;
    btnAddStartOffset.AnchorSideTop.Control:=e;
    btnAddStartOffset.AnchorSideTop.Side:=asrCenter;
    btnAddStartOffset.BorderSpacing.Left:=7;

    if btnRemoveStartOffset=nil then
    begin
      btnRemoveStartOffset:=TButton.create(self);
      btnRemoveStartOffset.caption:=rsRemove;
      btnRemoveStartOffset.Constraints.MinWidth:=60;
      btnRemoveStartOffset.AnchorSideLeft.Control:=btnAddStartOffset;
      btnRemoveStartOffset.AnchorSideLeft.Side:=asrRight;
      btnRemoveStartOffset.AnchorSideTop.Control:=btnAddStartOffset;
      btnRemoveStartOffset.AnchorSideTop.Side:=asrTop;
      btnRemoveStartOffset.BorderSpacing.Left:=7;
      btnRemoveStartOffset.AutoSize:=true;
      btnRemoveStartOffset.OnClick:=btnRemoveStartOffsetClick;
      btnRemoveStartOffset.parent:=panel1;
    end;

    btnAddStartOffset.visible:=true;
    btnRemoveStartOffset.visible:=true;

    cbMustEndWithSpecificOffsets.AnchorSideTop.Control:=btnAddStartOffset;
  end
  else
  begin
    cbMustEndWithSpecificOffsets.AnchorSideTop.Control:=cbMustStartWithSpecificOffsets;

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

  if cbNoValueCheck.checked then cbFilterOutAccessible.checked:=false;
  if newstate and cbFilterOutAccessible.checked then exit;

  rbFindAddress.enabled:=newstate;
  rbFindValue.enabled:=newstate;
  edtAddress.enabled:=newstate;
  cbValueType.enabled:=newstate;
end;

procedure TfrmRescanPointer.cbFilterOutAccessibleChange(Sender: TObject);
var newstate: boolean;
begin
  newstate:=not cbFilterOutAccessible.checked;

  if cbFilterOutAccessible.checked then cbNoValueCheck.checked:=false;
  if newstate and cbNoValueCheck.checked then exit;

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
      cbUseSavedPointermap.caption:=rsRPUseSavedPointermap+ExtractFileName(odLoadPointermap.FileName)
    else
      cbUseSavedPointermap.checked:=false;
  end
  else
    cbUseSavedPointermap.Caption:=rsRPUseSavedPointermap2;

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

procedure TfrmRescanPointer.setOffset(o: ptrint);
begin
  foffset:=o;
  if fOffset<0 then
    lblOffset.caption:='=-'+inttohex(-fOffset,1)
  else
    lblOffset.caption:='='+inttohex(fOffset,1);
end;

function TfrmRescanPointer.getOffset: ptrint;
begin
  if cbChangeBasePointerOffset.checked then
    result:=fOffset
  else
    result:=0;
end;

procedure TfrmRescanPointer.edtNewBaseChange(Sender: TObject);
var i: PtrInt;
begin
  try
    offset:=symhandler.getAddressFromName(edtNewBase.text)-strtoint('$'+lblOriginalBase.Caption);
    edtNewBase.font.color:=clDefault
  except
    edtNewBase.font.color:=clred;
  end;
end;

procedure TfrmRescanPointer.FormDestroy(Sender: TObject);
begin
  if iplist<>nil then
    freeandnil(iplist);
end;

procedure TfrmRescanPointer.FormShow(Sender: TObject);
begin
  edtRescanFunction.Constraints.MinWidth:=canvas.TextWidth('RescanFilter ');
  updatePositions;
end;

procedure TfrmRescanPointer.Notebook1ChangeBounds(Sender: TObject);
begin

end;

procedure TfrmRescanPointer.updatePositions;
{
Updates the pnlButtons panel position and adjusts the form height
}
var e: Tedit;
  i: integer;
  nextstart: integer;
begin
  //DoAutoSize;
  {
  e:=nil;
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

  clientheight:=pnlbuttons.top+pnlButtons.height;  }
end;

procedure TfrmRescanPointer.btnAddStartOffsetClick(sender: TObject);
var e: Tedit;
begin
  e:=Tedit.create(self);
  e.AnchorSideTop.Control:=tcontrol(startoffsets[startoffsets.count-1]);
  e.AnchorSideTop.Side:=asrBottom;

  e.AnchorSideLeft.Control:=tcontrol(startoffsets[0]);
  e.AnchorSideLeft.Side:=asrLeft;
  e.BorderSpacing.Top:=2;
  e.Parent:=panel1;

  btnAddStartOffset.AnchorSideTop.Control:=e;

  startoffsets.Add(e);
  lblInfoFirstOffset.Visible:=true;
  updatePositions;
end;

procedure TfrmRescanPointer.btnRemoveStartOffsetClick(sender: TObject);
begin
  if startoffsets.count=1 then
    cbMustStartWithSpecificOffsets.checked:=false
  else
  begin
    btnAddStartOffset.AnchorSideTop.Control:=tcontrol(startoffsets[startoffsets.count-2]);
    startoffsets.Delete(startoffsets.count-1);

    lblInfoFirstOffset.visible:=startoffsets.count>1;
  end;

  updatePositions;
end;

procedure TfrmRescanPointer.btnAddEndOffsetClick(sender: TObject);
var
  e: Tedit;
  prev: TEdit;
begin
  e:=Tedit.create(self);
  e.AnchorSideTop.Control:=cbMustEndWithSpecificOffsets;
  e.AnchorSideTop.Side:=asrBottom;
  e.AnchorSideLeft.Control:=cbMustEndWithSpecificOffsets;
  e.AnchorSideLeft.Side:=asrLeft;
  e.Parent:=panel1;
  btnAddEndOffset.AnchorSideTop.Control:=e;

  prev:=Tedit(endoffsets[0]);
  DisableAutoSizing;
  prev.AnchorSideTop.Control:=e;
  prev.BorderSpacing.Top:=2;
  EnableAutoSizing;

  lblInfoLastOffset.Visible:=true;
  endoffsets.Insert(0,e);
  updatePositions;
end;

procedure TfrmRescanPointer.btnRemoveEndOffsetClick(sender: TObject);
begin
  if endoffsets.count=1 then
    cbMustEndWithSpecificOffsets.checked:=false
  else
  begin
    DisableAutoSizing;
    tcontrol(endoffsets[1]).AnchorSideTop.Control:=cbMustEndWithSpecificOffsets;
    tcontrol(endoffsets[1]).BorderSpacing.Top:=0;
    EnableAutoSizing;
    btnAddEndOffset.AnchorSideTop.Control:=tcontrol(endoffsets[1]);
    endoffsets.Delete(0);

    lblInfoLastOffset.visible:=endoffsets.count>1;

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

  lblOriginalBase.BorderSpacing.Top:=(edtNewBase.height div 2)-(lblOriginalBase.Height div 2);
end;

initialization
  {$i frmRescanPointerUnit.lrs}

end.
