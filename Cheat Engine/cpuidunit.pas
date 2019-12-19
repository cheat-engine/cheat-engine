unit cpuidUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCPUIDResult=record
    eax: dword;
    ebx: dword;
    ecx: dword;
    edx: dword;
  end;

function CPUID(index: dword; index2: dword=0): TCPUIDResult;

implementation




{$ifdef cpu32}
procedure _cpuid(p: pointer); assembler;
asm
  push edi
  mov edi,p

  push eax
  push ebx
  push ecx
  push edx

  mov eax,[edi]
  mov ebx,[edi+4]
  mov ecx,[edi+8]
  mov edx,[edi+12]

  cpuid
  mov [edi],eax
  mov [edi+4],ebx
  mov [edi+8],ecx
  mov [edi+12],edx

  pop edx
  pop ecx
  pop ebx
  pop eax

  pop edi
end;
{$endif}


{$ifdef cpu64}
procedure _cpuid(p: pointer); stdcall; assembler;
asm
  mov r8,p
  mov r9,rbx //save rbx
  mov eax,[r8]
  mov ebx,[r8+4]
  mov ecx,[r8+8]
  mov edx,[r8+12]
  cpuid

  mov [r8],eax
  mov [r8+4],ebx
  mov [r8+8],ecx
  mov [r8+12],edx

  mov rbx,r9 //restore rbx
end;
{$endif}

function CPUID(index: dword; index2: dword=0): TCPUIDResult;
var
  r: TCPUIDResult;
begin
  r.eax:=index;
  r.ebx:=0;
  r.ecx:=index2;
  r.edx:=0;

  _cpuid(@r);

  result:=r;
end;

end.

