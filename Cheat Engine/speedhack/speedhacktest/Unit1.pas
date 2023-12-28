unit Unit1;

{$MODE Delphi}

interface

uses
  {$ifdef windows}
  windows,
  {$endif}
  {$ifdef darwin}
  DateUtils,
  {$endif}
  LCLIntf, Classes, Messages, SysUtils, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources, dynlibs;

type
  TSpeedhackTest=class(tthread)
  private
    laste: Exception;
    procedure died;
  public
    procedure execute; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    lblbeats: TLabel;
    lblFail1: TLabel;
    lblFail2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblFail: TLabel;
    Timer1: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type TTimeGetTime=function: DWORD; stdcall;

var
  Form1: TForm1;
  timeGetTime: TTimeGetTime;

  fail, fail1, fail2: boolean;

  threadheartbeat: integer;


implementation

procedure TSpeedhackTest.died;
begin
  ShowMessage('testthread died:'+laste.message);
end;

procedure TSpeedhackTest.execute;
var
  oldtick, newtick: dword;
  oldperf, newperf: int64;

begin

  try
    oldtick:=gettickcount;
    {$ifdef windows}
    QueryPerformanceCounter(oldperf);
    {$endif}

    while {(not fail) and} (not terminated) do
    begin
      inc(threadheartbeat);
      newtick:=gettickcount;

      {$ifdef windows}
      QueryPerformanceCounter(newperf);
      if newperf<oldperf then
      begin
        fail:=true;
        fail1:=true;
      end;
      {$endif}

      if newtick<oldtick then
      begin
        fail:=true;
        fail2:=true;
      end;

      oldperf:=newperf;
      oldtick:=newtick;
    end;

  except
    on e:exception do
    begin
      laste:=e;
      synchronize(died);
    end;
  end;
end;

function getrdtsctime:qword;
var a,b: qword;
begin
  a:=0;
  b:=0;
  {$ifdef cpu64}
  asm
    push rdx
    rdtsc
    shl rdx,32
    or rax,rdx
    mov a,rax
    rdtsc
    shl rdx,32
    or rax,rdx
    mov b,rax
    pop rdx
  end;
  {$else}
  asm
    push edx
    rdtsc
    lea edi,a
    mov dword a+4,edx
    mov dword a,eax
    rdtsc
    mov dword b+4,edx
    mov dword b,eax
    pop edx
  end;
  {$endif}


  result:=b-a;
end;

function getrdtscptime:qword;
var a,b: qword;
begin
  a:=0;
  b:=0;
  {$ifdef cpu64}
  asm
    push rcx
    push rdx
    db $0f
    db $01
    db $f9
    shl rdx,32
    or rax,rdx
    mov a,rax
    db $0f
    db $01
    db $f9
    shl rdx,32
    or rax,rdx
    mov b,rax

    pop rdx
    pop rcx
  end;
  {$else}
  asm
    push ecx
    push edx
    db $0f
    db $01
    db $f9
    mov dword a+4,edx
    mov dword a,eax
    db $0f
    db $01
    db $f9
    mov dword b+4,edx
    mov dword b,eax

    pop edx
    pop ecx
  end;
  {$endif}
  result:=b-a;
end;

function getcpuidtime(v: integer):qword;
var a,b: qword;
begin
  a:=0;
  b:=0;
  {$ifdef cpu64}
  asm
    push rax
    push rbx
    push rcx
    push rdx
    rdtsc
    shl rdx,32
    or rax,rdx
    mov a,rax
    mov rax,v
    cpuid
    rdtsc
    shl rdx,32
    or rax,rdx
    mov b,rax
    pop rdx
    pop rcx
    pop rbx
    pop rax
  end;
  {$else}
  asm
    push eax
    push ebx
    push ecx
    push edx
    rdtsc
    mov dword a+4,edx
    mov dword a,eax

    mov eax,v
    cpuid

    rdtsc
    mov dword b+4,edx
    mov dword b,eax

    pop edx
    pop ecx
    pop ebx
    pop eax
  end;
  {$endif}
  result:=b-a;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var freq: int64;


  newtick: dword;
  newperf: int64;
  q: qword;
begin
  lblbeats.caption:=threadheartbeat.ToString;
  newtick:=gettickcount;
  label1.Caption:=inttostr(gettickcount div 1000);
  if assigned(timegettime) then
    label2.Caption:=inttostr(timegettime div 1000);

  {$ifdef windows}
  QueryPerformanceCounter(newperf);
  QueryPerformanceFrequency(freq);

  label6.caption:=inttohex(newperf,1);
  label5.caption:=inttostr(freq);
  label3.Caption:=inttostr(newperf)+' = '+inttostr(newperf div freq);
  {$endif}

  label4.caption:=inttostr(gettickcount64 div 1000);


  if fail then
  begin
    lblFail.visible:=true;
    if fail1 then
      lblFail1.visible:=true;

    if fail2 then
      lblfail2.visible:=true;
  end;


  label8.caption:='rdtsc diff='+inttostr(getrdtsctime);
  label7.caption:='getrdtscp diff='+inttostr(getrdtscptime);
  label9.caption:='cpuid 0 diff='+inttostr(getcpuidtime(0));
  label10.caption:='cpuid 1 diff='+inttostr(getcpuidtime(1));
  label11.caption:='cpuid 2 diff='+inttostr(getcpuidtime(2));
  label12.caption:='cpuid 3 diff='+inttostr(getcpuidtime(3));

  {$ifdef cpu64}
  asm
    push rdx
    rdtsc
    shl rdx,32
    or rax,rdx
    mov q,rax
    pop rdx
  end;
  {$else}
  asm
    push edx
    rdtsc
    mov dword q+4,edx
    mov dword q,eax
    pop edx
  end;
  {$endif}

  label13.caption:=inttostr(q);

  QueryPerformanceCounter(newperf);
  label14.caption:=inttostr(newperf);

end;

procedure TForm1.Button1Click(Sender: TObject);
var x: TLibHandle;
begin
  x:=loadlibrary('libspeedhack.dylib');
  showmessage(inttohex(x,8));
end;



procedure TForm1.FormCreate(Sender: TObject);
var xx: HModule;
  i: integer;
begin
  {$ifdef windows}
  xx:=loadlibrary('winmm.dll');
  timeGetTime:=GetProcAddress(xx,'timeGetTime');
  {$endif}

  for i:=0 to 2 do
    TSpeedhackTest.create(false);


end;

initialization
  {$i Unit1.lrs}

end.
