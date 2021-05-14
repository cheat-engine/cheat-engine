unit cheatecoins;

{
Cheat-E coin handler and simple anti debug code

WARNING: If you read this you will lose all bragging rights on how you bypassed the cheat-e-coin system
You can of course say you didn't read this, but you'll know...

Now if you didn't read this to bypass the cheat-e-coin system but just want to see how it works, have fun

(May crash on some people's systems, due to security)
}

{$mode delphi}

interface

{$IFDEF windows}
uses
  jwawindows, windows, newkernelhandler, Classes, SysUtils, dialogs, betterControls;

procedure EnableCheatEcoinSystem;

procedure decreaseCheatECoinCount; stdcall;
function getCheatECoinCount: integer;

function checkCoinStatus: integer;
{$ENDIF}

implementation

{$IFDEF windows}
uses forms, frmMicrotransactionsUnit, ceregistry, luahandler;

var
  _DecreaseCount: integer;
  something: integer;
  _GetCount: integer;
  c2: pinteger;
  c3: pinteger; //last decrease value

  mainthreadhandle: THandle;
  context: PCONTEXT;
  contextmem: pointer;

threadvar
  actualcount: pinteger;

function antidebug1: integer; stdcall; //always returns 0xcececece
begin
  result:=GetTickCount64;
end;

function antidebug2(x: dword): dword; stdcall; //jumps to antidebug3 when executed (so returns x+0x100)
begin
  if x>5 then
    raise exception.create('It''s not that easy')
  else
    result:=10;
end;

function antidebug3(x: dword): dword; stdcall;
begin
  result:=x+$100;
end;

procedure setupContext;
begin
  context^.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
  if GetThreadContext(mainthreadhandle,context^) then
  begin

    context.Dr0:=ptruint(@_DecreaseCount);
    context.dr1:=ptruint(@_GetCount);
    context.dr2:=ptruint(@antidebug1);
    context.dr3:=ptruint(@antidebug2);
    context.dr7:=$fd0055; //dr0: write - dr1: readwrite, dr2=execute dr3=execute

    SetThreadContext(mainthreadhandle,context^);
  end;
end;

procedure clearDRContext;
begin
  context^.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
  if GetThreadContext(mainthreadhandle,context^) then
  begin
    context.dr7:=0; //dr0: write - dr1: readwrite
    SetThreadContext(mainthreadhandle,context^);
  end;
end;

function CECExceptionHandler(ExceptionInfo: windows.PEXCEPTION_POINTERS): LONG; stdcall;
var
  v,v2: dword;
  c: PContext;
  s: string;
  i: integer;

  newv: pinteger;
begin
  result:=EXCEPTION_CONTINUE_SEARCH;

  //MEssageBoxA(0,'e 1','exeption',0);

  asm
    push dword $202 //just to piss of a newbie stepper (set a software bp after)
    {$ifdef cpu64}
    popfq
    {$else}
    popfd
    {$endif}
  end;
  setupContext; //clears breakpoints  (software bp's help, I won't do integrity checks yet)

  //MEssageBoxA(0,'e 2','exeption',0);

  c:=PContext(ExceptionInfo^.ContextRecord);

 // MessageBoxA(0,pchar('Exception', 'exceptionHandler',0);

  case ExceptionInfo^.ExceptionRecord.ExceptionCode of
    EXCEPTION_SINGLE_STEP, STATUS_WX86_SINGLE_STEP:
    begin
      //MessageBoxA(0,pchar(format('CECExceptionHandler 2 %.8x',[c^.dr6])),'CECExceptionHandler 2',0);
      //MEssageBoxA(0,'e 3','exeption',0);

      if (c^.Dr6 and 1=1) then  //decrease coin
      begin
        //MessageBoxA(0,'DR0','CECExceptionHandler 2',0);



        if (c3<>nil) and (c3^=actualcount^) then //frozen value or continued editing (two times 0)
        begin
          //MessageBoxA(0,'BYE','CECExceptionHandler 2',0);

          exitProcess(1);
          asm
            {$ifdef cpu64}
            mov rsp,0
            mov rbp,0
            {$else}
            mov esp,0
            mov ebp,0
            {$endif}
          end;
          exit;
        end;

        getmem(newv,4+random(64));
        if c3<>nil then
          freemem(c3);

        c3:=newv;
        c3^:=actualcount^;

        v:=(dword(actualcount^) xor dword(antidebug1)) shr dword(13);


        asm
          push dword $202 //just to piss of a newbie stepper (set a software bp after)
          {$ifdef cpu64}
          popfq
          {$else}
          popfd
          {$endif}
        end;
        v2:=dword(c2^ xor (antidebug2($deadf023)));




        asm
          push dword $202 //just to piss of a newbie stepper (set a software bp after)
          {$ifdef cpu64}
          popfq
          {$else}
          popfd
          {$endif}
        end;

        if v2<>v then   //someone tried to tamper
        begin
          //MessageBoxA(0,pchar(format('Coin tamper detected %d <> %d (actualcount=%x)',[v2, v, actualcount])),'CECExceptionHandler 2',0);

          v:=0 //goodbye coins
        end
        else
          dec(v);

        freemem(c2);
        getmem(c2,4+random(64));

        c2^:=integer(dword(v) xor antidebug2($deadf023)); //just to keep people of track

        freemem(actualcount);
        getmem(actualcount,4+random(64));

        actualcount^:=integer(dword((dword(v) shl 13) xor antidebug1));

        //      c^.dr6:=0;  MEssageBoxA(0,'test done','exeption',0); exit(EXCEPTION_CONTINUE_EXECUTION);


        v2:=dword(v) xor dword($deadf123);
        if dword(v2)<>dword(c2^) then
        begin
         // MessageBoxA(0,pchar(format('antidebug tamper detected %x<>%x',[v2,c2])),'CECExceptionHandler 2',0);

          ExitProcess(2);
          asm
            {$ifdef cpu64}
            mov rsp,0
            mov rbp,0
            {$else}
            mov esp,0
            mov ebp,0
            {$endif}
          end;
          exit;
        end;



        c^.dr6:=0;


       // MEssageBoxA(0,'e 4','exeption',0);

        exit(EXCEPTION_CONTINUE_EXECUTION);
      end;

      if (ExceptionInfo^.ContextRecord^.Dr6 and 2=2) then
      begin
        //MessageBoxA(0,'DR1','CECExceptionHandler 2',0);

        c^.{$ifdef cpu64}Rax{$else}eax{$endif}:=qword(dword(c2^) xor antidebug2($deadf023));
        c^.dr6:=0;

        //MessageBoxA(0,'CECExceptionHandler 2.2','CECExceptionHandler 2.2',0);

        exit(EXCEPTION_CONTINUE_EXECUTION);
      end;

      if (ExceptionInfo^.ContextRecord^.Dr6 and 4=4) then
      begin
        //MessageBoxa(0,'dr2','cec',0);


        {$ifdef cpu64}
        c^.rax:=$cececece;
        c^.rip:=pqword(c^.Rsp)^; //change rip back to the caller
        c^.rsp:=c^.rsp+8; //pop the return of the stack
        {$else}
        c^.eax:=$cececece;
        c^.eip:=pdword(c^.esp)^; //change rip back to the caller
        c^.esp:=c^.esp+4; //pop the return address of the stack

        {$endif}
        c^.dr6:=0;
        exit(EXCEPTION_CONTINUE_EXECUTION);
      end;

      if (ExceptionInfo^.ContextRecord^.Dr6 and 8=8) then
      begin
        //MessageBoxa(0,'dr3','cec',0);
        c^.{$ifdef cpu64}rip{$else}eip{$endif}:=ptruint(@antidebug3);
        c^.dr6:=0;
        exit(EXCEPTION_CONTINUE_EXECUTION);
      end;

    end;

    EXCEPTION_ACCESS_VIOLATION:
    begin
      //s:=format('Address = %.8x rip=%.8x rax=%.8x r8=%.8x', [ptruint(ExceptionInfo^.ExceptionRecord.ExceptionAddress), c^.rip, c^.rax, c^.r8]);


      for i:=0 to ExceptionInfo^.ExceptionRecord.NumberParameters do
      begin
        s:=s+' '+inttostr(i)+':'+inttohex(ExceptionInfo^.ExceptionRecord^.ExceptionInformation[i],1);
      end;

      //MessageBoxA(0,pchar(s),'CECExceptionHandler 3',0);

      if ExceptionInfo^.ExceptionRecord.ExceptionInformation[1]=$101 then
      begin
        //MessageBoxA(0,'bla2','bla2',0);
        {$ifdef cpu64}
        c^.rip:=c^.r8;
        {$else}
        c^.eip:=c^.ecx;
        {$endif}
        exit(EXCEPTION_CONTINUE_EXECUTION);
      end;
    end;
  end;

end;

function Control: integer; assembler;
label oka;
asm
  {$ifdef cpu64}

  mov rax,$101
  push r8
  lea r8,oka
  mov dword [rax],12
oka:
  pop r8
  nop
  {$else}
  mov eax,$101
  push ecx
  lea ecx,oka
  mov dword [eax],12
oka:
  pop ecx
  nop
  {$endif}
end;


procedure decreaseCheatECoinCount; stdcall;
begin
  asm
    {$ifdef cpu64}
    lea r8,[rip+_DecreaseCount]
    xor dword [r8],$deadbeef
    {$else}
    add [_DecreaseCount],2
    {$endif}
  end;

  control;
end;

function getCheatECoinCount: integer; assembler; nostackframe;
asm
  {$ifdef cpu64}
  lea r8,[rip+_GetCount]
  mov eax,[r8]
  {$else}
  lea eax,[_GetCount]
  mov eax,[eax]
  {$endif}
  ret
end;

function checkCoinStatus: integer;
var
  c: integer;
  d: dword;
begin
  c:=getCheatECoinCount;

  try
    control;
  except
    //MessageBoxA(0,'BYE2', 'ccs',0);
    ExitProcess(1);
    asm
      {$ifdef cpu64}
      mov rsp,0
      mov rbp,0
      {$else}
      mov esp,0
      mov ebp,0
      {$endif}
    end;
  end;

  d:=integer((antidebug1 xor dword($cececece))+antidebug2($ce));

  if (c>20) or (d<>$1ce) then
  begin
    //MessageBoxA(0,pchar(format('Bye3 c=%d d=%d',[c,d])),'ccs',0);
    ExitProcess(1);
    asm
      {$ifdef cpu64}
      mov rsp,0
      mov rbp,0
      {$else}
      mov esp,0
      mov ebp,0
      {$endif}
    end;
  end;
  result:=c;
end;

procedure saveCoins;
begin
  if mainthreadhandle<>0 then //most stupid way to save the coins:
    cereg.writeInteger('Used Coins',20-getCheatECoinCount);
end;

procedure EnableCheatECoinSystem;   //simple solution to all this shittery, remove this function
var
  AddVectoredExceptionHandler: function (FirstHandler: Cardinal; VectoredHandler: PVECTORED_EXCEPTION_HANDLER): pointer; stdcall;
  k: HModule;

  th: THandle;

  v: integer;

  used: integer;

begin
  //MEssageBoxA(0,'0','0',0);

  ShowMessage('New!!! Cheat-e-coins! Now you can buy Cheat-e-coins to be able to use Cheat Engine. It''s just like a game!!! Yay!');

  k:=GetModuleHandle('kernel32.dll');
  AddVectoredExceptionHandler:=GetProcAddress(k,'AddVectoredExceptionHandler');

  AddVectoredExceptionHandler(1, PVECTORED_EXCEPTION_HANDLER(CECExceptionHandler));

  mainthreadhandle:=OpenThread(THREAD_SET_CONTEXT or THREAD_GET_CONTEXT,false, GetCurrentThreadId);

  getmem(contextmem,sizeof(TCONTEXT)+4096);
  zeromemory(contextmem,sizeof(TCONTEXT)+4096);

  context:=Align(contextmem,16);

  getmem(actualcount, 4+random(64));
  actualcount^:=integer(dword((20 shl 13) xor $cececece));

  getmem(c2,4+random(64));
  c2^:=integer(dword(20 xor $deadf123));

  used:=cereg.readInteger('Used Coins',0);

  //MEssageBoxA(0,'1','1',0);


  setupcontext;

  while used>0 do
  begin
    decreaseCheatECoinCount;
    dec(used);
  end;

 // MEssageBoxA(0,'2','2',0);


  frmMicroTransactions:=TfrmMicroTransactions.Create(application);
  frmMicroTransactions.show;
end;

finalization
  saveCoins;
{$ENDIF}


end.

