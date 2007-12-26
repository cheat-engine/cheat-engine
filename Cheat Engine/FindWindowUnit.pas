unit FindWindowUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,CEFuncproc,ComCtrls, ExtCtrls;

type
  TFindWindow = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    Scanvalue: TEdit;
    editStart: TEdit;
    EditStop: TEdit;
    labelType: TLabel;
    rbText: TRadioButton;
    rbArByte: TRadioButton;
    Label2: TLabel;
    Label3: TLabel;
    labelArray: TLabel;
    ProgressBar: TProgressBar;
    Timer1: TTimer;
    cbUnicode: TCheckBox;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    firstscan: boolean;
  end;

var
  FindWindow: TFindWindow;

implementation

{$R *.dfm}
uses MemorybrowserFormUnit;

procedure TFindWindow.btnOKClick(Sender: TObject);
var start,stop,temp: dword;
    cb: TCheckbox;
    valtype: integer;
    firstresult: dword;
begin
  try
    start:=StrToInt('$'+editStart.text);
    stop:=strtoint('$'+editstop.Text);
  except
    raise exception.Create('The specified range is invalid');
  end;

  if start>stop then
  begin
    temp:=start;
    start:=stop;
    stop:=temp;
  end;

  cb:=TCheckbox.create(self);
  cb.visible:=false;
  cb.checked:=true;
  cb.parent:=self;

  if rbText.checked then valtype:=7 else valtype:=8;
  if GetMemoryRangesAndScanValue2(firstresult,start,stop,true,true,Exact_value,valtype,scanvalue.text,'',rounded,true,progressbar,false,cbunicode.checked)>0 then
  begin
    //something found
    MemoryBrowser.memoryaddress:=firstresult;
    MemoryBrowser.RefreshMB;
    modalresult:=mrok;
    findwindow.close;
  end else
  begin
    showmessage('Nothing found...');
    if not firstscan then modalresult:=mrcancel;
  end;

  cb.free;
end;

procedure TFindWindow.FormShow(Sender: TObject);
begin
  progressbar.Position:=0;
  
  if firstscan then
  begin
    editstart.Text:=Inttohex(memorybrowser.memoryaddress,8);
    height:=175;
    progressbar.Top:=96;

    labelType.visible:=true;
    labelarray.visible:=true;
    rbtext.visible:=true;
    rbArByte.visible:=true;
    scanvalue.Visible:=true;
    btnOK.visible:=true;
    btnCancel.visible:=true;
    EditStop.visible:=true;
    editStart.visible:=true;
    label2.Visible:=true;
    label3.Visible:=true;

  end
  else
  begin
    clientheight:=progressbar.height+4;
    progressbar.top:=2;

    labelType.visible:=false;
    labelarray.visible:=false;
    rbtext.visible:=false;
    rbArByte.visible:=false;
    scanvalue.Visible:=false;
    btnOK.visible:=false;
    btnCancel.visible:=false;
    EditStop.visible:=false;
    editStart.visible:=false;
    label2.Visible:=false;
    label3.Visible:=false;
    timer1.enabled:=true; //bah, I wanted to do execute here but it seems thats not possible
  end;
end;

procedure TFindWindow.Timer1Timer(Sender: TObject);
begin
  timer1.enabled:=false;
  btnOK.click;
  close;
end;

end.




















