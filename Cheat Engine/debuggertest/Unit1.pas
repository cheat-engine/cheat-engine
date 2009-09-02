unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Button3: TButton;
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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  health: dword;

implementation

{$R *.dfm}

procedure x; stdcall;
var a: dword;
begin
  asm
    nop
    nop
    nop
    nop
    nop
  end;

  showmessage('executed and still alive. ebx='+inttohex(a,8));
end;

procedure TForm1.Button1Click(Sender: TObject);
var _dr7: dword;
    _dr0: dword;
    c: _CONTEXT;
    p: thandle;
    d: boolean;
    x,y,z: dword;
label XXX;
begin
  asm
    lea eax,[XXX]
    mov _dr0,eax
  end;

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
    mov eax,1
    mov ebx,2
    mov ecx,3
    mov edx,4
    mov edi,5
    mov esi,6 
    {
    mov ebp,7
    mov esp,8      }

    mov eax,$123
    nop
    nop
    nop

XXX:
      nop
      nop
      nop


      mov z,eax
      mov x,ebp
      mov y,esp      
    end;
    ShowMessage('Normal exit. Looks like the interrupt was handled(Which under normal situations should not happen). EBP='+inttohex(x,8)+' ESP='+inttohex(y,8)+' EAX='+inttohex(z,8));
    exit;
  except
    on e:exception do
    begin

      showmessage('breakpoint caused exception. As expected. Message:'+e.message);

    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  health:=random(1000);
  label1.caption:=format('%p : %d',[@health, health]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  label1.caption:=format('%p : %d',[@health, health]);
  label2.caption:=format('%p',[@x]);
  button4.click;
end;



procedure TForm1.Button3Click(Sender: TObject);
begin
  asm
    nop
    push ebx
    mov ebx,$54321
    nop
    nop
  end;
  x;

  asm
    pop ebx
  end;
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
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
asm
  int 14
end;

end;

end.
