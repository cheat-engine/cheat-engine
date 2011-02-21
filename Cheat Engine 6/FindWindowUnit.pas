unit FindWindowUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,CEFuncProc,ComCtrls, ExtCtrls, LResources, memscan;

const wm_fw_scandone=wm_user+1;
type
  TFindWindow = class(TForm)
    ProgressBar: TProgressBar;
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
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    memscan: TMemScan;
    procedure scandone(var m: TMessage); message wm_fw_scandone;
  public
    { Public declarations }
    firstscan: boolean;
  end;

var
  FindWindow: TFindWindow;

implementation

uses MemoryBrowserFormUnit;

procedure TFindWindow.scandone(var m: TMessage);
var x: ptruint;
    i: integer;
begin
  //showmessage('scan finished');
  try
    if memscan.GetOnlyOneResult(x) then
    begin
      MemoryBrowser.memoryaddress:=x;
      modalresult:=mrok;
    end else raise exception.Create('Nothing found');
  finally
    for i:=0 to ControlCount-1 do
      Controls[i].Enabled:=true;
    freeandnil(memscan);
  end;
end;

procedure TFindWindow.btnOKClick(Sender: TObject);
var start,stop,temp: dword;
    cb: TCheckbox;
    valtype: TVariableType;
    i: integer;
begin
  if memscan<>nil then exit;
  
  try
    start:=StrToInt64('$'+editStart.text);
    stop:=strtoint64('$'+editstop.Text);
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

  if rbText.checked then valtype:=vtString else valtype:=vtByteArray;

//  raise exception.Create('Please tell dark byte he still has to implement a new find');


  memscan:=TMemscan.create(ProgressBar);
  memscan.setScanDoneCallback(handle, wm_fw_scandone);
  memscan.onlyone:=true;

  for i:=0 to ControlCount-1 do
    Controls[i].Enabled:=false;


  memscan.firstscan(soExactValue, valtype, rtRounded, scanvalue.text, '', start, stop, false, true, false, cbunicode.checked, false,false, fsmAligned);


    {
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
  end;    }

  cb.free;
end;

procedure TFindWindow.FormShow(Sender: TObject);
begin
  progressbar.Position:=0;
  
  if firstscan then
  begin
    editstart.Text:=Inttohex(memorybrowser.memoryaddress,8);
    height:=185;
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

initialization
  {$i FindWindowUnit.lrs}

end.





















