unit Unit1;

{$MODE Delphi}

interface

uses
  jwawindows, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, syncobjs, {psapi,} imagehlp;

type
  TChangeHealthLikeAMofo=class(tthread)
  public
    procedure execute; override;
  end;

  TChangeHealthThread=class(tthread)
  public
    healthaddress: PDWORD;
    changehealthevent: TEvent;
    procedure updatehealth;
    procedure execute; override;
    constructor create(suspended: boolean);
    destructor destroy; override;
end;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button2: TButton;
    edtTimeout: TEdit;
    Label1: TLabel;
    Button3: TButton;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Button4: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Button5: TButton;
    Button6: TButton;
    Label9: TLabel;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Timer1: TTimer;
    Timer2: TTimer;
    Timer3: TTimer;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
  private
    { Private declarations }
    originalIntegrityValue: dword;
    h: dword;
    cht: TChangeHealthThread;
  public
    { Public declarations }
    procedure ecx(Sender : TObject; E : Exception);
  end;

var
  Form1: TForm1;

  health: dword;

type
  LPMODULEINFO = ^MODULEINFO;
  {$EXTERNALSYM LPMODULEINFO}
  _MODULEINFO = record
    lpBaseOfDll: LPVOID;
    SizeOfImage: DWORD;
    EntryPoint: LPVOID;
  end;
  {$EXTERNALSYM _MODULEINFO}
  MODULEINFO = _MODULEINFO;
  {$EXTERNALSYM MODULEINFO}
  TModuleInfo = MODULEINFO;
  PModuleInfo = LPMODULEINFO;

function GetModuleInformation(hProcess: HANDLE; hModule: HMODULE; var lpmodinfo: MODULEINFO; cb: DWORD): BOOL; stdcall; external 'Psapi.dll';

implementation

{$R *.lfm}

procedure TChangeHealthLikeAMofo.execute;
var a,b: integer;
begin
  while not terminated do
  begin
    a:=health;
    b:=a+random(65536);
    health:=b;
  end;
end;

procedure exc(E: Exception);
var
  I: Integer;
  Frames: PPointerArray;
  Report: string;
begin
  Report := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := PPointerArray(ExceptFrames);
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);

  ShowMessage(Report);

end;

function generateIntegrityValue: dword;
var
  size: dword;
  mi: MODULEINFO;
  i: integer;
  p: pbytearray;

  img: PIMAGENTHEADERS;
  ish: PImageSectionHeader;
  lastvalue: byte;
begin
  result:=0;

  if GetModuleInformation(GetCurrentProcess, GetModuleHandle(0), mi, sizeof(mi)) then
  begin
    p:=mi.lpBaseOfDll;

    img:=ImageNtHeader(p);

    p:=pointer(dword(p)+img.OptionalHeader.BaseOfCode);

    lastvalue:=$ce;
    for i:=0 to img.OptionalHeader.SizeOfCode-1 do
    begin
      result:=result+(p[i] xor lastvalue);
      lastvalue:=p[i];
    end;
  end;


  if result=0 then
  begin
    showmessage('Integrity calculation failed:'+inttostr(getlasterror));
    //application.Terminate;
  end;

end;

destructor TChangeHealthThread.destroy;
begin
  terminate;
  changehealthevent.SetEvent;
  waitfor;
  inherited destroy;
end;

constructor TChangeHealthThread.create(suspended: boolean);
begin
  changehealthevent:=TEvent.Create(nil,false,false,'');
  inherited create(suspended);
end;

procedure TChangeHealthThread.execute;
begin
  while not terminated do
  begin
    if (changehealthevent.waitfor(10000)=wrSignaled) and not terminated then
    begin
      health:=random(1000);
      synchronize(updatehealth);
    end;
  end;
end;

procedure TChangeHealthThread.updatehealth;
begin
  Form1.label1.Caption:=format('%p : %d',[@health, health]);
end;

procedure x; stdcall;
{$ifdef cpu32}
var a: dword;
{$else}
var a: qword;
{$endif}
label weee;
begin
  a:=0;

  {$ifdef cpu64}
  asm
    push r14
    mov r14,$1234

    nop
    nop
    cmp r14,$1234
    jne weee
    nop
    nop
    nop
    nop
    nop
    nop
    mov r14,$9999
    nop
    nop
weee:
    nop
    nop
    mov a,r14
    nop
    nop
    nop
    pop r14
  end;

  showmessage('executed and still alive. r14='+inttohex(a,8));
  {$else}
  asm
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    push eax
    mov eax,$12345678
    mov a,eax
    pop eax
    nop
    nop
    nop
    nop
    nop
  end;

  showmessage('executed and still alive. eax='+inttohex(a,8));
  {$endif}
end;

procedure TForm1.Button1Click(Sender: TObject);
var _dr7: dword;
    _dr0: ptrUint;
    c: _CONTEXT;
    p: thandle;
    d: boolean;
    x,y,z: dword;
  label xxx;
  label something;
begin
{$ifdef cpu64}
  asm
    lea rax,something
    mov _dr0,rax
  end;

  _dr0:=_dr0+$100000000;
{$else}
  asm
    lea eax,xxx
    mov _dr0,eax
  end;
{$endif}

  _dr7:=$3;


  c.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
  d:=GetThreadContext(GetCurrentThread, c);
  if not d then showmessage('big shit:'+inttostr(getlasterror));


  c.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
  c.Dr0:=_dr0;
  c.DR7:=_dr7;




  d:=SetThreadContext(GetCurrentThread, c);
  if not d then showmessage('shit');



  try
    asm
    nop
    nop
   nop
    nop
    nop
    nop
    {
    mov ebp,7
    mov esp,8      }

    mov eax,$123
    nop
    nop
something:
    nop

XXX:
      nop
      nop
      nop


      mov z,eax
      mov x,ebp
      mov y,esp      
    end;
    ShowMessage('BAAAAAD Normal exit. Looks like the interrupt was handled(Which under normal situations should not happen). EBP='+inttohex(x,8)+' ESP='+inttohex(y,8)+' EAX='+inttohex(z,8));
    exit;
  except
    on e:exception do
    begin

      //exc(e);

      showmessage('breakpoint caused exception. As expected. Message:'+e.message+' '+inttohex(ptruint(ExceptionObject.Addr),8));

    end;
  end;
end;

procedure TForm1.Button10Click(Sender: TObject);
begin

end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  TChangeHealthLikeAMofo.create(false);
  TChangeHealthLikeAMofo.create(false);
  TChangeHealthLikeAMofo.create(false);
  timer1.enabled:=true;
end;

procedure TForm1.Button12Click(Sender: TObject);
var
  c: tchangehealththread;
  timeout: integer;
begin
  c:=tchangehealththread.create(false);
  timeout:=strtoint(edtTimeout.text);
  sleep(timeout);
  c.changehealthevent.setEvent;

  sleep(100);

  c.terminate;
  c.free;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  health:=random(1000);
  label1.caption:=format('%p : %d',[@health, health]);
end;


procedure TForm1.ecx(Sender : TObject; E : Exception);
begin
  showmessage('unexpected error');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {application.OnException:=ecx;
  label1.caption:=format('%p : %d',[@health, health]);
  label2.caption:=format('%p',[@x]);
  button4.click;

  label9.caption:=format('%p',[@h]);
  }

  cht:=tchangehealththread.create(false);
//  originalIntegrityValue:=generateIntegrityValue;
end;



procedure TForm1.Button3Click(Sender: TObject);
begin
  {asm
    nop
    push rbx
    mov rbx,$54321
    nop
    nop
  end; }
  x;

 { asm
    pop rbx
  end;  }
end;

procedure TForm1.Button4Click(Sender: TObject);
var c: TContext;
begin
  c.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
  if GetThreadContext(GetCurrentThread, c) then
  begin
    label3.caption:=inttohex(c.Dr0,8);
    label4.caption:=inttohex(c.Dr1,8);
    label5.caption:=inttohex(c.Dr2,8);
    label6.caption:=inttohex(c.Dr3,8);
    label7.caption:=inttohex(c.Dr6,8);
    label8.caption:=inttohex(c.Dr7,8);
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
var old: dword;
begin
  if VirtualProtect(@x, 1, PAGE_READWRITE, old) then
    showmessage('yes');

  x;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
asm
  int 14
end;

end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //cht.free;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin

  cht.changehealthevent.SetEvent;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  if generateIntegrityValue<>originalIntegrityValue then
    showmessage('!!!INTEGRITY FAILED!!!')
  else
    showmessage('Everything is fine');

end;

type TLongThread=class(tthread)
  private
    procedure execute; override;
end;

procedure TLongThread.execute;
begin
  FreeOnTerminate:=true;
  while not terminated do
    sleep(1000);


end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  TLongThread.create(false);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  label1.caption:=format('%p : %d',[@health, health]);
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  button10.click;
end;

var ec: integer;
procedure TForm1.Timer3Timer(Sender: TObject);
begin
  try
    asm
    int3
    sub [ec],1
    end;
  except
    inc(ec);
  end;
  label10.caption:=inttostr(ec);
end;

end.
