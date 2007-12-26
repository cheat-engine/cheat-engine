unit frmFillMemoryUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,cefuncproc{$ifdef net},netapis{$else},newkernelhandler{$endif};

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

{$R *.dfm}

procedure TfrmFillMemory.Button1Click(Sender: TObject);
var start,stop,count,temp:dword;
    fillvalue:byte;
    buf: array of byte;
    actualwritten: dword;

begin
  //fill the memory
  try
    start:=strtoint('$'+edit1.Text);
  except
    raise exception.Create('Please fill in a valid ''From'' address');
  end;

  try
    stop:=strToInt('$'+edit2.Text);
  except
    raise exception.Create('Please fill in a valid ''To'' address');
  end;

  try
    fillvalue:=strToInt('$'+edit3.Text);
  except
    raise exception.Create('Please fill in a valid ''Fill'' value');
  end;

  count:=stop-start+1;
  if integer(count)<0 then
  begin
    temp:=stop;
    stop:=start;
    start:=temp;
  end;

  setlength(buf,count);
  fillmemory(@buf[0],count,fillvalue);


  RewriteCode(processhandle,start,@buf[0],count);

  modalresult:=mrok;
end;

procedure TfrmFillMemory.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
end;

end.
