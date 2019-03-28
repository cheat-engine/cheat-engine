unit FindWindowUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,CEFuncProc,ComCtrls, ExtCtrls, LResources, memscan,
  commonTypeDefs, math, win32proc, symbolhandler;

const wm_fw_scandone=wm_user+1;
type

  { TFindWindow }

  TFindWindow = class(TForm)
    Panel1: TPanel;
    labelType: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    labelArray: TLabel;
    editStart: TEdit;
    EditStop: TEdit;
    rbText: TRadioButton;
    rbArByte: TRadioButton;
    cbUnicode: TCheckBox;
    Timer1: TTimer;
    Panel2: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    Scanvalue: TEdit;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    memscan: TMemScan;
    procedure scandone(Sender: TObject);

  public
    { Public declarations }
    firstscan: boolean;
  end;

var
  FindWindow: TFindWindow;

implementation

uses MemoryBrowserFormUnit, ProcessHandlerUnit, Parsers;

resourcestring
  rsNothingFound = 'Nothing found';
  rsTheSpecifiedRangeIsInvalid = 'The specified range is invalid';



procedure TFindWindow.scandone(sender: TObject);
var x: ptruint;
begin
  if TMemScan(sender).GetOnlyOneResult(x) then
  begin
    MemoryBrowser.memoryaddress:=x;
    modalresult:=mrok;
  end else
  begin
    MessageDlg(rsNothingFound, mtError, [mbok], 0);
  end;

  if memscan=sender then
    freeandnil(memscan);

  btnOK.enabled:=true;
end;

procedure TFindWindow.btnOKClick(Sender: TObject);
var start,stop,temp: ptruint;
    //cb: TCheckbox;
    valtype: TVariableType;
    i: integer;

begin


  if memscan<>nil then
    freeandnil(memscan);
  
  try
    start:=StrToQWordEx('$'+editstart.text);
  except
    start:=symhandler.getAddressFromName(editstart.text);
  end;

  try
    stop:=StrToQWordEx('$'+editstop.text);
  except
    stop:=symhandler.getAddressFromName(editstop.text);
  end;

  if start>stop then
  begin
    temp:=start;
    start:=stop;
    stop:=temp;
  end;


  if rbText.checked then valtype:=vtString else valtype:=vtByteArray;



  memscan:=TMemscan.create(nil);
  memscan.onlyone:=true;
  memscan.scanCopyOnWrite:=scanDontCare;
  memscan.scanExecutable:=scanDontCare;
  memscan.scanWritable:=scanDontCare;


  memscan.firstscan(soExactValue, valtype, rtRounded, scanvalue.text, '', start, stop, true, false, cbunicode.checked, false, fsmNotAligned);
  memscan.OnScanDone:=ScanDone;

  btnOK.enabled:=false;
end;

procedure TFindWindow.FormDestroy(Sender: TObject);
begin
  if memscan<>nil then
    freeandnil(memscan);
end;

procedure TFindWindow.btnCancelClick(Sender: TObject);
begin

end;

procedure TFindWindow.FormShow(Sender: TObject);
const EM_GETMARGINS=$d4;
var m: DWord;
begin
  if firstscan then
  begin

    if WindowsVersion>=wvVista then
      m:=sendmessage(editstart.Handle, EM_GETMARGINS, 0,0)
    else
      m:=0;

    if processhandler.is64bit then
    begin
      editstop.text:='7FFFFFFFFFFFFFFF';
      editstart.clientwidth:=canvas.TextWidth('DDDDDDDDDDDDDDDD')+(m shr 16)+(m and $ffff);
    end
    else
    begin
      editstart.clientwidth:=canvas.TextWidth('DDDDDDDD')+(m shr 16)+(m and $ffff);
    end;






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
    Scanvalue.SetFocus;
  end
  else
  begin
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

  btnok.AutoSize:=true;
  btnok.AutoSize:=false;

  btnCancel.autosize:=true;
  btnCancel.autosize:=false;

  btnok.width:=max(btnok.width, btncancel.width);
  btncancel.width:=max(btnok.width, btncancel.width);
end;

procedure TFindWindow.Timer1Timer(Sender: TObject);
begin
  timer1.enabled:=false;
  btnOK.click;
  close;
end;

initialization
  {$i FindWindowUnit.lrs}

end.





















