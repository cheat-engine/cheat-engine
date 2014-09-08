unit frmFillMemoryUnit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,CEFuncProc{$ifdef net},netapis{$else},NewKernelHandler{$endif}, LResources;

type
  TfrmFillMemory = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    Edit2: TEdit;
    Edit3: TEdit;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFillMemory: TfrmFillMemory;

implementation

uses ProcessHandlerUnit;

resourcestring
  rsPleaseFillInAValidFromAddress = 'Please fill in a valid ''From'' address';
  rsPleaseFillInAValidToAddress = 'Please fill in a valid ''To'' address';
  rsPleaseFillInAValidFillValue = 'Please fill in a valid ''Fill'' value';


procedure TfrmFillMemory.Button1Click(Sender: TObject);
var start,stop: ptrUint;
    count: qword;
    count2: dword;
    temp:dword;
    fillvalue:byte;
    buf: array of byte;
    actualwritten: dword;

begin
  //fill the memory
  try
    start:=strtoint('$'+edit1.Text);
  except
    raise exception.Create(rsPleaseFillInAValidFromAddress);
  end;

  try
    stop:=strToInt('$'+edit2.Text);
  except
    raise exception.Create(rsPleaseFillInAValidToAddress);
  end;

  try
    fillvalue:=strToInt('$'+edit3.Text);
  except
    raise exception.Create(rsPleaseFillInAValidFillValue);
  end;

  count:=stop-start+1;
  if integer(count)<0 then
  begin
    temp:=stop;
    stop:=start;
    start:=temp;
  end;
  count:=stop-start+1;

  setlength(buf,count);
  fillmemory(@buf[0],count,fillvalue);

  count2:=count;
  if count<>count2 then raise exception.create('Region too large');

  RewriteCode(processhandle,start,@buf[0],count2);

  modalresult:=mrok;
end;

procedure TfrmFillMemory.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
end;

initialization
  {$i frmFillMemoryUnit.lrs}

end.
