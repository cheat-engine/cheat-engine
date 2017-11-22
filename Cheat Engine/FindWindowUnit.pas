unit FindWindowUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,CEFuncProc,ComCtrls, ExtCtrls, LResources, memscan,
  commonTypeDefs, math, win32proc;

const wm_fw_scandone=wm_user+1;
type

  { TFindWindow }

  TFindWindow = class(TForm)
    scanvalue_line: TEdit;
    scanvalue: TMemo;
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
    statusbar: TStatusBar;
    Timer1: TTimer;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbArByteChange(Sender: TObject);
    procedure rbTextChange(Sender: TObject);
    procedure scanvalueKeyDown(Sender: TObject; var Key: Word;
        Shift: TShiftState);
    procedure scanvalue_lineKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    memscan: TMemScan;
    procedure rbChange;
  public
    { Public declarations }
    firstscan: boolean;
  end;

var
  FindWindow: TFindWindow;

implementation

uses MemoryBrowserFormUnit, ProcessHandlerUnit, Parsers, windows;

resourcestring
  rsNothingFound = 'Nothing found';
  rsTheSpecifiedRangeIsInvalid = 'The specified range is invalid';



procedure TFindWindow.btnOKClick(Sender: TObject);
var start,stop,temp: ptruint;
    valtype: TVariableType;
    i: integer;
    x: ptruint;
    searchstr,scantext,tmp:string;
    a:char;
begin


  if memscan<>nil then
    freeandnil(memscan);
  
  try
    start:=StrToQWordEx('$'+editStart.text);
    stop:=StrToQWordEx('$'+editstop.Text);
  except
    raise exception.Create(rsTheSpecifiedRangeIsInvalid);
  end;

  if start>stop then
  begin
    temp:=start;
    start:=stop;
    stop:=temp;
  end;

  if(rbText.checked)then
  begin
    valtype:=vtString;
    scantext:=scanvalue_line.Text;
  end
  else
  begin
    valtype:=vtByteArray;
    scantext:=scanvalue.Text;
  end;

  memscan:=TMemscan.create(nil);
  memscan.onlyone:=true;
  memscan.scanCopyOnWrite:=scanDontCare;
  memscan.scanExecutable:=scanDontCare;
  memscan.scanWritable:=scanDontCare;

  if(valtype=vtByteArray)then
  begin
    searchstr:='';
    tmp:=UpperCase(scantext);
    for i:=Low(tmp) to High(tmp) do
    begin
        a:=tmp[i];
        if(((a>='0') and (a<='9')) or ((a>='A') and (a<='F')))then
            searchstr:=searchstr+a;
    end;
  end
  else
    searchstr:=scantext;

  try
    statusbar.SimpleText:='Searching...';
    statusbar.Repaint;
    memscan.firstscan(soExactValue, valtype, rtRounded, searchstr, '', start, stop, true, false, cbunicode.checked, false, fsmNotAligned);
    memscan.waittilldone;

    if memscan.GetOnlyOneResult(x) then
    begin
      MemoryBrowser.memoryaddress:=x;
      modalresult:=mrok;
    end else
    begin
      //showmessage(rsNothingFound);
      //wtf...
      freeandnil(memscan);

      MessageDlg(rsNothingFound, mtError, [mbok], 0);
    end;
  finally
    statusbar.SimpleText:='';
    if memscan<>nil then
      freeandnil(memscan);
  end;

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
    rbChange;
    btnOK.visible:=true;
    btnCancel.visible:=true;
    EditStop.visible:=true;
    editStart.visible:=true;
    label2.Visible:=true;
    label3.Visible:=true;
    if(rbText.Checked)then
        scanvalue_line.SetFocus
    else
        scanvalue.SetFocus;
  end
  else
  begin
    labelType.visible:=false;
    labelarray.visible:=false;
    rbtext.visible:=false;
    rbArByte.visible:=false;
    scanvalue.Visible:=false;
    scanvalue_line.Visible:=false;
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

procedure TFindWindow.rbChange;
begin
  if(rbText.Checked)then
  begin
       scanvalue_line.Visible:=true;
       scanvalue.Visible:=false;
  end
  else
  begin
      scanvalue_line.Visible:=false;
      scanvalue.Visible:=true;
  end;
end;

procedure TFindWindow.rbArByteChange(Sender: TObject);
begin
     rbChange;
end;

procedure TFindWindow.rbTextChange(Sender: TObject);
begin
     rbChange;
end;

procedure TFindWindow.scanvalueKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    if(key=Ord('A'))then
    begin
      if(ssCtrl in Shift)then
        scanvalue.SelectAll;
    end
    else if(VK_ESCAPE=key)then
    begin
      self.Close;
    end
    else if(VK_HOME=key)then
    begin
      if(ssCtrl in Shift)then
        editStart.Text:='0';
    end
    else if(VK_RETURN=key)then
    begin
      if(Shift = [])then
      begin
        key:=0;
        btnOKClick(nil);
      end;
    end;
end;

procedure TFindWindow.scanvalue_lineKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(key=VK_HOME)then
  begin
    if(ssCtrl in Shift)then
        editStart.Text:='0';
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





















