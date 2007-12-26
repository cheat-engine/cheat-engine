unit settings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,tlhelp32;

type tmoduledata =class
  public
    moduleaddress: dword;
    modulesize: dword;
end;
  
type
  TfrmPointerScannerSettings = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label3: TLabel;
    Label12: TLabel;
    Button1: TButton;
    edtStart: TEdit;
    edtStop: TEdit;
    edtFilterStart: TEdit;
    edtFilterStop: TEdit;
    editStructsize: TEdit;
    edtAddress: TEdit;
    editMaxLevel: TEdit;
    cbunaligned: TCheckBox;
    btnCancel: TButton;
    Label7: TLabel;
    Label8: TLabel;
    ListBox1: TListBox;
    RadioButton1: TRadioButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    edtThreadcount: TEdit;
    Label9: TLabel;
    CheckBox4: TCheckBox;
    ComboBox1: TComboBox;
    procedure ListBox1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
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
    tmoduledata(listbox1.Items.Objects[i]).Free;

  listbox1.Clear;
end;

procedure TfrmPointerScannerSettings.Button1Click(Sender: TObject);
begin
  start:=strtoint('$'+edtStart.text);
  stop:=strtoint('$'+edtStop.text);

  if stop>$7fffffff then stop:=$7ffffff;

  
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
  GetProcessAffinityMask(getcurrentprocess,PA,SA);
  bitcount:=1;
  while pa>0 do
  begin
    if (pa mod 2)=1 then inc(bitcount);
    pa:=pa div 2;
  end;

  edtThreadcount.text:=inttostr(bitcount);

  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,getcurrentprocessid);
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

end.


