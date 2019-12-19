unit frmFillMemoryUnit;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport, math,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, CEFuncProc, NewKernelHandler, LResources, ExtCtrls;

type

  { TfrmFillMemory }

  TfrmFillMemory = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
  rsFMRegionTooLarge = 'Region too large';

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
    start:=strtoint64('$'+edit1.Text);
  except
    raise exception.Create(rsPleaseFillInAValidFromAddress);
  end;

  try
    stop:=strToInt64('$'+edit2.Text);
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
  if count<>count2 then raise exception.create(rsFMRegionTooLarge);

  RewriteCode(processhandle,start,@buf[0],count2);

  modalresult:=mrok;
end;

procedure TfrmFillMemory.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TfrmFillMemory.FormCreate(Sender: TObject);
begin

end;

procedure TfrmFillMemory.FormShow(Sender: TObject);
begin
  constraints.MinWidth:=max(panel2.width, panel1.width)+6;
end;

initialization
  {$i frmFillMemoryUnit.lrs}

end.
