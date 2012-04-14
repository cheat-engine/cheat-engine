unit PointerscannerSettingsFrm;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,{tlhelp32,} ComCtrls,ExtCtrls, LResources, Contnrs
  {$ifdef injectedpscan}
  ,symbolhandlerlite;
  {$else}
  ,CEFuncProc,NewKernelHandler, symbolhandler;
  {$endif}

type tmoduledata = class
  public
    moduleaddress: dword;
    modulesize: dword;
  end;

type TOffsetEntry=class(Tedit)
  private
    function getOffset: dword;
    procedure setOffset(x: dword);
  protected
    procedure KeyPress(var Key: Char); override;
  public
    constructor create(AOwner: TComponent); override;
    property offset: dword read getOffset write setOffset;
end;

type

  { TfrmPointerScannerSettings }

  TfrmPointerScannerSettings = class(TForm)
    cbNoReadOnly: TCheckBox;
    edtAddress: TEdit;
    PSSettings: TPageControl;
    PSReverse: TTabSheet;
    CbAlligned: TCheckBox;
    edtReverseStop: TEdit;
    edtReverseStart: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    cbStaticOnly: TCheckBox;
    cbMustEndWithSpecificOffset: TCheckBox;
    Panel1: TPanel;
    Label3: TLabel;
    Label12: TLabel;
    Label9: TLabel;
    Button1: TButton;
    editStructsize: TEdit;
    editMaxLevel: TEdit;
    btnCancel: TButton;
    edtThreadcount: TEdit;
    ComboBox1: TComboBox;
    cbStackAsBase: TCheckBox;
    Edit3: TEdit;
    Label14: TLabel;
    cbOnlyStackAsBase: TCheckBox;
    cbUseHeapData: TCheckBox;
    cbHeapOnly: TCheckBox;
    cbValueType: TComboBox;
    Panel2: TPanel;
    rbFindAddress: TRadioButton;
    rbFindValue: TRadioButton;
    cbOnlyOneStatic: TCheckBox;
    cbReusePointermap: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure cbMustEndWithSpecificOffsetChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbMustEndWithSpecificOffsetClick(Sender: TObject);
    procedure cbUseHeapDataClick(Sender: TObject);
    procedure rbFindValueClick(Sender: TObject);
    procedure edtAddressChange(Sender: TObject);
    procedure cbHeapOnlyClick(Sender: TObject);
  private
    { Private declarations }
    procedure btnAddClick(sender: TObject);
    procedure btnRemoveClick(sender: TObject);
  public
    { Public declarations }
    reverse: boolean; //indicates to use the reverse method
    start:ptrUint;
    stop: ptrUint;
    unalligned: boolean;
    automaticaddress: ptrUint;
    structsize: integer;
    level0structsize: integer;
    maxlevel: integer;
    codescan: boolean;
    threadcount: integer;

    offsetlist: TComponentList;
    btnAddOffset: TButton;
    btnRemoveOffset: TButton;

    scannerpriority: TThreadPriority;
  end;

var frmpointerscannersettings: tfrmpointerscannersettings;

implementation


{$ifndef injectedpscan}
uses frmMemoryAllocHandlerUnit, MemoryBrowserFormUnit;
{$endif}



resourcestring
  rsAdd = 'Add';
  rsRemove = 'Remove';
  rsIdle = 'Idle';
  rsLowest = 'Lowest';
  rsLower = 'Lower';
  rsNormal = 'Normal';
  rsHigher = 'Higher';
  rsHighest = 'Highest';
  rsTimeCritical = 'TimeCritical';

constructor TOffsetEntry.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  text:='0';

  width:=50;
end;

procedure TOffsetEntry.KeyPress(var Key: Char);
begin
  if key in ['A'..'F', 'a'..'f','0'..'9','+','-',#0..#31] then
    inherited KeyPress(key)
  else
    key:=#0;
end;

function TOffsetEntry.getOffset: dword;
var o: integer;
begin
  if TryStrToInt('$'+text, o) then result:=o else result:=0;
end;

procedure TOffsetEntry.setOffset(x: dword);
begin
  text:=inttohex(x,1);
end;

procedure TfrmPointerScannerSettings.Button1Click(Sender: TObject);
begin
  start:=StrToQWordEx('$'+edtReverseStart.text);
  stop:=StrToQWordEx('$'+edtReverseStop.text);

  automaticaddress:=symhandler.getAddressFromName(edtAddress.text);

  unalligned:=not cballigned.checked;

  structsize:=strtoint(editstructsize.text);
  level0structsize:=4;
  maxlevel:=strtoint(editMaxLevel.text)+1;

  codescan:=false;

  threadcount:=strtoint(edtthreadcount.text);
  case combobox1.itemindex of
    0: scannerpriority:=tpIdle;
    1: scannerpriority:=tpLowest;
    2: scannerpriority:=tpLower;
    3: scannerpriority:=tpNormal;
    4: scannerpriority:=tpHigher;
    5: scannerpriority:=tpHighest;
    6: scannerpriority:=tpTimeCritical;
  end;

  modalresult:=mrok;
end;

procedure TfrmPointerScannerSettings.cbMustEndWithSpecificOffsetChange(Sender: TObject);
begin

end;

procedure TfrmPointerScannerSettings.FormShow(Sender: TObject);
var
  cpucount: integer;

begin
  cpucount:=GetCPUCount;

  //assumption: when a core with hyperthreading core is running at 100% it's hyperthreaded processor will be running at 90%
  //This means that 10 cores are needed to provide an equivalent for one extra core when hyperthreading is used
  //In short, leave the hyperhtreaded processors alone so the user can use that hardly useful processing power to surf the web or move the mouse...
  //(at most use one)
  if HasHyperthreading then
    cpucount:=1+(cpucount div 2);


  rbFindValueClick(rbFindAddress);


  edtThreadcount.text:=inttostr(cpucount);


  //check what type of process it is
  if processhandler.is64Bit then
  begin
    edtReverseStart.Width:=160;
    edtReverseStart.maxlength:=16;
    edtReverseStop.MaxLength:=edtReverseStart.MaxLength;

    //if it's not edited by the user, then fill in the default ranges for 64-bit
    if edtReverseStart.text='00000000' then
      edtReverseStart.text:='0000000000000000';

    if (edtReverseStop.text='7FFFFFFF') or (edtReverseStop.text='FFFFFFFF') then
    begin
      edtReverseStop.text:='7FFFFFFFFFFFFFFF';
    end;
  end
  else
  begin
    edtReverseStart.Width:=80;



    //if it's not edited by the user, then fill in the default ranges for 32-bit
    if edtReverseStart.text='0000000000000000' then
      edtReverseStart.text:='00000000';

    if (edtReverseStop.text='7FFFFFFFFFFFFFFF') or (edtReverseStop.text='7FFFFFFF') then
    begin
      if Is64bitOS then
        edtReverseStop.text:='FFFFFFFF'
      else
        edtReverseStop.text:='7FFFFFFF';
    end;

    edtReverseStart.maxlength:=8;
    edtReverseStop.MaxLength:=edtReverseStart.MaxLength;
  end;


  edtReverseStop.Width:=edtReverseStart.width;
  edtReverseStop.Left:=edtReverseStart.Left+edtReverseStart.Width+8;

  Label11.left:=edtReverseStop.left;
end;

procedure TfrmPointerScannerSettings.FormCreate(Sender: TObject);
begin
  ComboBox1.Items.Clear;
  with ComboBox1.items do
  begin
    add(rsIdle);
    add(rsLowest);
    add(rsLower);
    add(rsNormal);
    add(rsHigher);
    add(rsHighest);
    add(rsTimeCritical);
  end;

  ComboBox1.itemindex:=3;


  pssettings.ActivePage:=PSReverse;
  clientheight:=cbMustEndWithSpecificOffset.top+cbMustEndWithSpecificOffset.Height+2+panel1.height;
end;

procedure tfrmPointerScannerSettings.btnAddClick(sender: TObject);
var offsetentry: TOffsetEntry;
begin
  offsetentry:=TOffsetEntry.Create(self);
  offsetlist.Add(offsetentry);
    
  with offsetentry do
  begin
    top:=panel1.top;
    left:=cbMustEndWithSpecificOffset.left+15;
    self.Height:=self.Height+height+2;
    parent:=self;
  end;

  btnAddOffset.top:=offsetentry.top;
  btnRemoveOffset.top:=btnAddOffset.top;
end;

procedure tfrmPointerScannerSettings.btnRemoveClick(sender: TObject);
begin
  offsetlist.delete(offsetlist.count-1);
  if offsetlist.count>0 then
  begin
    btnAddOffset.top:=TOffsetEntry(offsetlist[offsetlist.count-1]).top;
    btnRemoveOffset.top:=btnAddOffset.top;
    self.Height:=btnAddOffset.top+btnAddOffset.Height+2+panel1.height;
  end
  else
  begin
    cbMustEndWithSpecificOffset.checked:=false;
  end;
end;


procedure TfrmPointerScannerSettings.cbMustEndWithSpecificOffsetClick(Sender: TObject);
var offsetentry: TOffsetEntry;
begin
  //create an offset entry
  if (sender as tcheckbox).Checked then //create the first one and the add button
  begin
    offsetentry:=TOffsetEntry.Create(self);

    offsetlist:=TComponentList.create;
    offsetlist.Add(offsetentry);
    
    with offsetentry do
    begin
      top:=panel1.top;
      left:=cbMustEndWithSpecificOffset.left+15;
      self.Height:=self.Height+height+2;
      parent:=self;
    end;

    if btnAddOffset=nil then
      btnAddOffset:=TButton.Create(self);

    with btnAddOffset do
    begin
      caption:=rsAdd;
      left:=offsetentry.Left+offsetentry.Width+3;
      width:=60;
      height:=offsetentry.Height;
      top:=offsetentry.top;
      parent:=self;
      onclick:=btnAddClick;
      visible:=true;
    end;

    if btnRemoveOffset=nil then
      btnRemoveOffset:=TButton.Create(self);
    
    with btnRemoveOffset do
    begin
      caption:=rsRemove;
      left:=btnAddOffset.Left+btnAddOffset.Width+3;
      width:=60;
      height:=offsetentry.Height;
      top:=offsetentry.top;
      parent:=self;
      onclick:=btnRemoveClick;
      visible:=true;
    end;

  


  end
  else
  begin
    //delete all the objects in the list
    clientheight:=cbMustEndWithSpecificOffset.top+cbMustEndWithSpecificOffset.Height+2+panel1.height;
    freeandnil(offsetlist); //deletes all the assigned objects
    btnAddOffset.Visible:=false;
    btnRemoveOffset.Visible:=false;
  end;


end;

procedure TfrmPointerScannerSettings.cbUseHeapDataClick(Sender: TObject);
begin
  cbHeapOnly.Enabled:=cbUseHeapData.Checked;
  if (frmMemoryAllocHandler<>nil) and (frmMemoryAllocHandler.hookedprocessid<>processid) then
    freeandnil(frmMemoryAllocHandler);

  frmMemoryAllocHandler:=TfrmMemoryAllocHandler.Create(memorybrowser);
  frmMemoryAllocHandler.WaitForInitializationToFinish;

  edtAddressChange(edtAddress);
end;

procedure TfrmPointerScannerSettings.rbFindValueClick(Sender: TObject);
begin
  if rbFindAddress.Checked then
  begin
    edtAddress.Width:=cbValueType.Left+cbValueType.Width-edtAddress.Left;
    cbValueType.Visible:=false;

  end
  else
  begin
    edtAddress.Width:=cbValueType.left-edtAddress.Left-3;
    cbValueType.Visible:=true;

  end;
  edtAddress.SetFocus;
end;

procedure TfrmPointerScannerSettings.edtAddressChange(Sender: TObject);
var haserror: boolean;
begin
  automaticaddress:=symhandler.getAddressFromName(edtAddress.text, false,haserror); //ignore error


  if cbHeapOnly.Checked then
  begin
   if (frmMemoryAllocHandler.FindAddress(@frmMemoryAllocHandler.HeapBaselevel, automaticaddress)<>nil) then
     edtAddress.Font.Color:=clGreen
   else
     edtAddress.Font.Color:=clRed; //BAD
  end else edtAddress.Font.Color:=clWindowText;

end;

procedure TfrmPointerScannerSettings.cbHeapOnlyClick(Sender: TObject);
begin
  edtAddressChange(edtAddress);
end;

initialization
  {$i PointerscannerSettingsFrm.lrs}

end.





