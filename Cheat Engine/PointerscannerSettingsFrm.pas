unit PointerscannerSettingsFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,tlhelp32, ComCtrls,ExtCtrls, Contnrs
  {$ifdef injectedpscan}
  ,symbolhandlerlite;
  {$else}
  ,cefuncproc,newkernelhandler, symbolhandler;
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
  TfrmPointerScannerSettings = class(TForm)
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
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
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
    start:dword;
    stop: dword;
    unalligned: boolean;
    automaticaddress: dword;
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

{$R *.dfm}

{$ifndef injectedpscan}
uses frmMemoryAllocHandlerunit;
{$endif}

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

  start:=strtoint('$'+edtReverseStart.text);
  stop:=strtoint('$'+edtReverseStop.text);

  if stop>$7fffffff then stop:=$7fffffff;

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

procedure TfrmPointerScannerSettings.FormShow(Sender: TObject);
var ths: thandle;
    me32: MODULEENTRY32;
    x: pchar;
    moduledata: tmoduledata;
    first:boolean;
    bitcount: integer;
    PA,SA: dword;
begin
  {$ifdef injectedpscan}
  //get the cpu and system affinity mask, only processmask is used
  GetProcessAffinityMask(getcurrentprocess,PA,SA);

  bitcount:=0;
  while pa>0 do
  begin
    if (pa mod 2)=1 then inc(bitcount);
    pa:=pa div 2;
  end;

  {$else}
  bitcount:=GetCPUCount;

  if HasHyperthreading then
    bitcount:=1+(bitcount div 2);
  {$endif}

  rbFindValueClick(rbFindAddress);


  edtThreadcount.text:=inttostr(bitcount);
end;

procedure TfrmPointerScannerSettings.FormCreate(Sender: TObject);
begin
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
    self.Height:=btnAddOffset.top+btnAddOffset.Height+2+panel1.height;
    
    btnAddOffset.top:=TOffsetEntry(offsetlist[offsetlist.count-1]).top;
    btnRemoveOffset.top:=btnAddOffset.top;
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
      caption:='Add';
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
      caption:='Remove';
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

  frmMemoryAllocHandler:=TfrmMemoryAllocHandler.Create(self);
  frmMemoryAllocHandler.WaitForInitializationToFinish;

  edtAddressChange(edtAddress);
end;

procedure TfrmPointerScannerSettings.rbFindValueClick(Sender: TObject);
begin
  if rbFindAddress.Checked then
  begin
    edtAddress.Width:=cbValueType.Left+cbValueType.Width-edtAddress.Left;
    cbValueType.Visible:=false;
    editMaxLevel.Text:='5';
    cbStaticOnly.Checked:=true;
  end
  else
  begin
    edtAddress.Width:=rbFindAddress.Width;
    cbValueType.Visible:=true;
    editMaxLevel.Text:='1';
    cbStaticOnly.Checked:=false;
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

end.





