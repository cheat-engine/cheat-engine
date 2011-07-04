unit systemcallsignalunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type tsharedmem=record
  Infunction:boolean;
  RetrieverWindowHandle: thandle;
end;


var
  Form1: TForm1;
  sharedmem: ^tsharedmem;
  sharedmemmapping: thandle;

implementation

{$R *.dfm}

function x(handle:thandle; lparam:dworD):bool; stdcall;
begin
  result:=true;
end;

procedure TForm1.Button1Click(Sender: TObject);
var y: thandle;
begin
  y:=handle;
  sharedmem^.Infunction:=true;
  asm
    int 3
  end;
  EnumChildWindows(y,@x,0);
  sharedmem^.Infunction:=false;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SharedMemMapping:=OpenFileMapping(FILE_MAP_ALL_ACCESS,false,'SystemCallInfo');
  SharedMem:=MapViewOfFile(SharedMemMapping,FILE_MAP_ALL_ACCESS,0,0,0);

  if sharedmem=nil then
  begin
    showmessage('The systemcallnumber retriever failed to do initialize');
   // terminateprocess(getcurrentprocess,0);
  end;

  button1.Click;
  button2.Click;
end;

procedure TForm1.Button2Click(Sender: TObject);
var procid: dword;
    threadid:dword;
    h:thandle;
begin
  h:=handle;


  sharedmem^.Infunction:=true;
  asm
    int 3
  end;
  threadid:=GetWindowThreadProcessId(sharedmem^.RetrieverWindowHandle,procid);
  sharedmem^.Infunction:=false;

  if threadid=0 then showmessage('error');

end;

end.
