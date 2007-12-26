unit PointerscannerSettingsFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,tlhelp32,cefuncproc,newkernelhandler, ComCtrls;

type tmoduledata =class
  public
    moduleaddress: dword;
    modulesize: dword;
end;
  
type
  TfrmPointerScannerSettings = class(TForm)
    Label3: TLabel;
    Label12: TLabel;
    Label7: TLabel;
    Button1: TButton;
    editStructsize: TEdit;
    edtAddress: TEdit;
    editMaxLevel: TEdit;
    btnCancel: TButton;
    rbDefault: TRadioButton;
    rbReverse: TRadioButton;
    PSSettings: TPageControl;
    PSDefault: TTabSheet;
    PSReverse: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    edtStart: TEdit;
    edtStop: TEdit;
    edtFilterStart: TEdit;
    edtFilterStop: TEdit;
    cbunaligned: TCheckBox;
    ListBox1: TListBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CbAlligned: TCheckBox;
    Label9: TLabel;
    edtThreadcount: TEdit;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    cbStaticOnly: TCheckBox;
    cbreuse: TCheckBox;
    procedure ListBox1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbDefaultClick(Sender: TObject);
    procedure rbReverseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure edtFilterStartChange(Sender: TObject);
    procedure edtFilterStopChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    reverse: boolean; //indicates to use the reverse method
    start:dword;
    stop: dword;
    filterstart: dword;
    filterstop: dword;
    unalligned: boolean;
    automaticaddress: dword;
    structsize: integer;
    level0structsize: integer;
    maxlevel: integer;
    codescan: boolean;
    writableonly: boolean;
    unallignedbase: boolean;
    threadcount: integer;
    psychotic: boolean;

    scannerpriority: TThreadPriority;
  end;

var frmpointerscannersettings: tfrmpointerscannersettings;

implementation

{$R *.dfm}

procedure TfrmPointerScannerSettings.ListBox1Click(Sender: TObject);
begin
  if listbox1.ItemIndex<>-1 then
  begin
    edtStart.Text:=inttohex(tmoduledata(listbox1.Items.Objects[listbox1.ItemIndex]).moduleaddress,8);
    edtstop.text:=inttohex(tmoduledata(listbox1.Items.Objects[listbox1.ItemIndex]).moduleaddress+tmoduledata(listbox1.Items.Objects[listbox1.ItemIndex]).modulesize,8);
  end;
end;

procedure TfrmPointerScannerSettings.FormClose(Sender: TObject;
  var Action: TCloseAction);
var i: integer;
begin
  for i:=0 to listbox1.Count-1 do
    if listbox1.Items.Objects[i]<>nil then
      tmoduledata(listbox1.Items.Objects[i]).Free;

  listbox1.Clear;
end;

procedure TfrmPointerScannerSettings.Button1Click(Sender: TObject);
begin
  start:=strtoint('$'+edtStart.text);
  stop:=strtoint('$'+edtStop.text);

  if stop>$7fffffff then stop:=$7fffffff;

  
  filterstart:=strtoint('$'+edtfilterstart.text);
  filterstop:=strtoint('$'+edtfilterstop.text);
  automaticaddress:=strtoint('$'+edtAddress.text);

  unalligned:=cbunaligned.checked;

  structsize:=strtoint(editstructsize.text);
  level0structsize:=4;
  maxlevel:=strtoint(editMaxLevel.text)+1;

  codescan:=false;
  writableonly:=checkbox2.checked;
  unallignedbase:=checkbox3.checked;

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


  psychotic:=checkbox4.checked;

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
  bitcount:=GetCPUCount+1;
  edtThreadcount.text:=inttostr(bitcount);

  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,processid);
  if ths<>0 then
  begin
    try
      first:=true;
      zeromemory(@me32,sizeof(me32));
      me32.dwSize:=sizeof(me32);
      if module32first(ths,me32) then
      repeat
        x:=@me32.szModule[0];

        moduledata:=tmoduledata.Create;
        moduledata.moduleaddress:=dword(me32.modBaseAddr);
        moduledata.modulesize:=me32.modBaseSize;

        if first then
        begin
          edtstart.text:=inttohex(moduledata.moduleaddress,8);
          edtstop.text:=inttohex(moduledata.moduleaddress+moduledata.modulesize,8);
          first:=false;
        end;

        listbox1.Items.AddObject(x,moduledata);
      until module32next(ths,me32)=false;

    finally
      closehandle(ths);
    end;
  end;
end;

procedure TfrmPointerScannerSettings.rbDefaultClick(Sender: TObject);
begin
  pssettings.ActivePage:=PSDefault;
end;

procedure TfrmPointerScannerSettings.rbReverseClick(Sender: TObject);
begin
  pssettings.ActivePage:=PSReverse;
end;

procedure TfrmPointerScannerSettings.FormCreate(Sender: TObject);
begin
  pssettings.ActivePage:=PSDefault;
end;

procedure TfrmPointerScannerSettings.Edit2Change(Sender: TObject);
begin
  edtFilterStart.Text:=edit2.Text;
end;

procedure TfrmPointerScannerSettings.Edit1Change(Sender: TObject);
begin
  edtFilterStop.text:=edit1.text;
end;

procedure TfrmPointerScannerSettings.edtFilterStartChange(Sender: TObject);
begin
  edit2.Text:=edtFilterStart.Text;
end;

procedure TfrmPointerScannerSettings.edtFilterStopChange(Sender: TObject);
begin
  edit1.text:=edtFilterStop.Text;
end;

end.




