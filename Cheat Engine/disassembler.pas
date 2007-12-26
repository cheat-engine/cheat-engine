unit disassembler;
//eric, voeg int3 afhandeling toe
interface

uses imagehlp,sysutils,windows,symbolhandler,cefuncproc{$ifdef net}{$ifndef netserver},NetAPIs{$endif}{$endif}{$ifndef netclient},NewKernelHandler{$endif};

type Tprefix = set of byte;
type TMemory = array [0..23] of byte;

function rd(bt: byte): string;
function rd8(bt:byte): string;
function rd16(bt:byte): string;

function r8(bt:byte): string;
function r16(bt:byte): string;
function r32(bt:byte): string;
function mm(bt:byte): string;
function xmm(bt:byte): string;
function sreg(bt:byte): string;
function CR(bt:byte):string;
function DR(bt:byte):string;



function GetBitOf(Bt: dword; bit: integer): byte;
function getsegmentoverride(prefix: TPrefix): string;
function getmod(bt: byte): byte;
function getRM(bt: byte): byte;
function getREG(bt: byte): byte;

function SIB(memory:TMemory; sibbyte: integer; var last: dword): string;
function MODRM(memory:TMemory; prefix: TPrefix; modrmbyte: integer; inst: integer; var last: dword): string;

function disassemble(var offset: dword): string; overload;
function disassemble(var offset: dword; var description: string): string; overload;

function previousopcode(address: dword):dword;
//function translatestring(disassembled: string; numberofbytes: integer; showvalues: boolean):string;
function translatestring(disassembled: string; numberofbytes: integer; showvalues: boolean; var address: string; var bytes: string; var opcode: string; var special:string):string;

function inttohexs(address:dword;chars: integer):string;

var mode16: boolean;

implementation

//dont use it by otherunits
{$ifndef net}
{$ifndef standalonetrainer}
uses assemblerunit,debugger, StrUtils;
{$endif}
{$endif}


function rd(bt:byte):string;
begin
  case bt of
  0: result:='eax';
  1: result:='ecx';
  2: result:='edx';
  3: result:='ebx';
  4: result:='esp';
  5: result:='ebp';
  6: result:='esi';
  7: result:='edi';
  end;
end;


function rd8(bt:byte): string;
begin
  case bt of
  0: result:='al';
  1: result:='cl';
  2: result:='dl';
  3: result:='bl';
  4: result:='ah';
  5: result:='ch';
  6: result:='dh';
  7: result:='bh';
  end;
end;


function rd16(bt:byte):string;
begin
  case bt of
  0: result:='ax';
  1: result:='cx';
  2: result:='dx';
  3: result:='bx';
  4: result:='sp';
  5: result:='bp';
  6: result:='si';
  7: result:='di';
  end;
end;


function r8(bt:byte): string;
begin
  case getreg(bt) of
  0: result:='al';
  1: result:='cl';
  2: result:='dl';
  3: result:='bl';
  4: result:='ah';
  5: result:='ch';
  6: result:='dh';
  7: result:='bh';
  end;
end;

function r16(bt:byte): string;
begin
  case getreg(bt) of
  0: result:='ax';
  1: result:='cx';
  2: result:='dx';
  3: result:='bx';
  4: result:='sp';
  5: result:='bp';
  6: result:='si';
  7: result:='di';
  end;
end;

function r32(bt:byte): string;
begin
  case getreg(bt) of
    0: result:='eax';
    1: result:='ecx';
    2: result:='edx';
    3: result:='ebx';
    4: result:='esp';
    5: result:='ebp';
    6: result:='esi';
    7: result:='edi';
  end;

end;

function xmm(bt:byte): string;
begin
  case getreg(bt) of
  0: result:='XMM0';
  1: result:='XMM1';
  2: result:='XMM2';
  3: result:='XMM3';
  4: result:='XMM4';
  5: result:='XMM5';
  6: result:='XMM6';
  7: result:='XMM7';
  end;
end;

function mm(bt:byte): string;
begin
  case getreg(bt) of
  0: result:='MM0';
  1: result:='MM1';
  2: result:='MM2';
  3: result:='MM3';
  4: result:='MM4';
  5: result:='MM5';
  6: result:='MM6';
  7: result:='MM7';
  end;
end;

function sreg(bt:byte): string;
begin
  case getreg(bt) of
  0: result:='ES';
  1: result:='CS';
  2: result:='SS';
  3: result:='DS';
  4: result:='FS';
  5: result:='GS';
  6: result:='HS';  //as if...
  7: result:='IS';
  end;
end;

function CR(bt:byte):string;
begin
  case getreg(bt) of
  0: result:='CR0';
  1: result:='CR1';
  2: result:='CR2';
  3: result:='CR3';
  4: result:='CR4';
  5: result:='CR5';
  6: result:='CR6';
  7: result:='CR7';
  end;
end;

function DR(bt:byte):string;
begin
  case getreg(bt) of
  0: result:='DR0';
  1: result:='DR1';
  2: result:='DR2';
  3: result:='DR3';
  4: result:='DR4';
  5: result:='DR5';
  6: result:='DR6';
  7: result:='DR7';
  end;
end;



function GetBitOf(Bt: dword; bit: integer): byte;
begin
  bt:=bt shl (31-bit);
  result:=bt shr 31;
//  result:=(bt shl (7-bit)) shr 7;  //can someone explain why this isn't working ?
end;

function getsegmentoverride(prefix: TPrefix): string;
begin
  if $2e in prefix then result:='CS:' else
  if $26 in prefix then result:='ES:' else
  if $36 in prefix then result:='SS:' else
  if $3e in prefix then result:='' else
  if $64 in prefix then result:='FS:' else
  if $65 in prefix then result:='GS:';
end;

function getmod(bt: byte): byte;
begin
  result:=(bt shr 6) and 3;
end;

function getRM(bt: byte): byte;
begin
  result:=bt and 7;
end;

function getREG(bt: byte): byte;
begin
  result:=(bt shr 3) and 7;
end;


function MODRM2(memory:TMemory; prefix: TPrefix; modrmbyte: integer; inst: integer; var last: dword): string;
var dwordptr: ^dword;
begin

  dwordptr:=@memory[modrmbyte+1];
  last:=modrmbyte+1;


  if $67 in prefix then
  begin
    // put some 16-bit stuff in here
    // but since this is a 32-bit debugger only ,forget it...

  end
  else
  begin
    case getmod(memory[modrmbyte]) of
      0:  case getrm(memory[modrmbyte]) of
            0:  result:=getsegmentoverride(prefix)+'[EAX],';
            1:  result:=getsegmentoverride(prefix)+'[ECX],';
            2:  result:=getsegmentoverride(prefix)+'[EDX],';
            3:  result:=getsegmentoverride(prefix)+'[EBX],';
            4:  result:=getsegmentoverride(prefix)+'['+sib(memory,modrmbyte+1,last)+'],';
            5:  begin
                  result:=getsegmentoverride(prefix)+'['+inttohexs(dwordptr^,8)+'],';
                  last:=last+4;
                end;
            6:  result:=getsegmentoverride(prefix)+'[ESI],';
            7:  result:=getsegmentoverride(prefix)+'[EDI],';
          end;

      1:  begin
            case getrm(memory[modrmbyte]) of
              0:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'[EAX+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'[EAX-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              1:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'[ECX+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'[ECX-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              2:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'[EDX+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'[EDX-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              3:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'[EBX+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'[EBX-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              4:  begin
                    result:=getsegmentoverride(prefix)+'['+sib(memory,modrmbyte+1,last);
                    if memory[last]<=$7F then
                      result:=result+'+'+inttohexs(memory[last],2)+'],'
                    else
                      result:=result+'-'+inttohexs($100-memory[last],2)+'],';
                  end;
              5:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'[EBP+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'[EBP-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              6:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'[ESI+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'[ESI-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              7:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'[EDI+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'[EDI-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
            end;
            inc(last);
          end;

      2:  begin
            case getrm(memory[modrmbyte]) of
              0:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'[EAX+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'[EAX-'+inttohexs($100000000-dwordptr^,8)+'],';
              1:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'[ECX+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'[ECX-'+inttohexs($100000000-dwordptr^,8)+'],';
              2:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'[EDX+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'[EDX-'+inttohexs($100000000-dwordptr^,8)+'],';
              3:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'[EBX+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'[EBX-'+inttohexs($100000000-dwordptr^,8)+'],';
              4:  begin
                    result:=getsegmentoverride(prefix)+'['+sib(memory,modrmbyte+1,last);
                    dwordptr:=@memory[last];
                    if dwordptr^ <=$7FFFFFFF then
                      result:=result+'+'+inttohexs(dwordptr^,8)+'],' else
                      result:=result+'+'+inttohexs($100000000-dwordptr^,8)+'],';

                  end;
              5:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'[EBP+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'[EBP-'+inttohexs($100000000-dwordptr^,8)+'],';
              6:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'[ESI+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'[ESI-'+inttohexs($100000000-dwordptr^,8)+'],';
              7:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'[EDI+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'[EDI-'+inttohexs($100000000-dwordptr^,8)+'],';
            end;
            inc(last,4);
          end;

      3:  begin
            case getrm(memory[modrmbyte]) of
              0:  case inst of
                    0: result:='EAX,';
                    1: result:='AX,';
                    2: result:='AL,';
                    3: result:='MM0,';
                    4: result:='XMM0,';
                  end;

              1:  case inst of
                    0: result:='ECX,';
                    1: result:='CX,';
                    2: result:='CL,';
                    3: result:='MM1,';
                    4: result:='XMM1,';
                  end;

              2:  case inst of
                    0: result:='EDX,';
                    1: result:='DX,';
                    2: result:='DL,';
                    3: result:='MM2,';
                    4: result:='XMM2,';
                  end;

              3:  case inst of
                    0: result:='EBX,';
                    1: result:='BX,';
                    2: result:='BL,';
                    3: result:='MM3,';
                    4: result:='XMM3,';
                  end;

              4:  case inst of
                    0: result:='ESP,';
                    1: result:='SP,';
                    2: result:='AH,';
                    3: result:='MM4,';
                    4: result:='XMM4,';
                  end;

              5:  case inst of
                    0: result:='EBP,';
                    1: result:='BP,';
                    2: result:='CH,';
                    3: result:='MM5,';
                    4: result:='XMM5,';
                  end;

              6:  case inst of
                    0: result:='ESI,';
                    1: result:='SI,';
                    2: result:='DH,';
                    3: result:='MM6,';
                    4: result:='XMM6,';
                  end;

              7:  case inst of
                    0: result:='EDI,';
                    1: result:='DI,';
                    2: result:='BH,';
                    3: result:='MM7,';
                    4: result:='XMM7,';
                  end;
            end;
          end;
    end;

  end;

end;


function MODRM(memory:TMemory; prefix: TPrefix; modrmbyte: integer; inst: integer; var last: dword): string; overload;
begin
  result:=modrm2(memory,prefix,modrmbyte,inst,last);
end;

function MODRM(memory:TMemory; prefix: TPrefix; modrmbyte: integer; inst: integer; var last: dword;opperandsize:integer): string; overload;
begin
  result:=modrm2(memory,prefix,modrmbyte,inst,last);
  if (length(result)>0) and (result[1]='[') then
  begin
    case opperandsize of
     8 : result:='byte ptr '+result;
     16: result:='word ptr '+result;
     32: result:='dword ptr '+result;
     64: result:='qword ptr '+result;
     80: result:='tword ptr '+result;
     128: result:='dqword ptr '+result;
    end;
  end;
end;

function SIB(memory:TMemory; sibbyte: integer; var last: dword): string;
var dwordptr: ^dword;
begin
  case memory[sibbyte] of
   $00 : begin
           result:='EAX+EAX';
           last:=sibbyte+1;
         end;

   $01 : begin
           result:='ECX+EAX';
           last:=sibbyte+1;
         end;

   $02 : begin
           result:='EDX+EAX';
           last:=sibbyte+1;
         end;

   $03 : begin
           result:='EBX+EAX';
           last:=sibbyte+1;
         end;

   $04 : begin
           result:='ESP+EAX';
           last:=sibbyte+1;
         end;

   $05 : begin
            dwordptr:=@memory[sibbyte+1];
            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EAX+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EAX';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EAX';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $06 : begin
           result:='ESI+EAX';
           last:=sibbyte+1;
         end;

   $07 : begin
           result:='EDI+EAX';
           last:=sibbyte+1;
         end;
      //--------------
   $08 : begin
           result:='EAX+ECX';
           last:=sibbyte+1;
         end;

   $09 : begin
           result:='ECX+ECX';
           last:=sibbyte+1;
         end;

   $0a : begin
           result:='EDX+ECX';
           last:=sibbyte+1;
         end;

   $0b : begin
           result:='EBX+ECX';
           last:=sibbyte+1;
         end;

   $0c : begin
           result:='ESP+ECX';
           last:=sibbyte+1;
         end;

   $0d : begin
            dwordptr:=@memory[sibbyte+1];
            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='ECX+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+ECX';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+ECX';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $0e : begin
           result:='ESI+ECX';
           last:=sibbyte+1;
         end;

   $0f : begin
           result:='EDI+ECX';
           last:=sibbyte+1;
         end;

//10-17
   $10 : begin
           result:='EAX+EDX';
           last:=sibbyte+1;
         end;

   $11 : begin
           result:='ECX+EDX';
           last:=sibbyte+1;
         end;

   $12 : begin
           result:='EDX+EDX';
           last:=sibbyte+1;
         end;

   $13 : begin
           result:='EBX+EDX';
           last:=sibbyte+1;
         end;

   $14 : begin
           result:='ESP+EDX';
           last:=sibbyte+1;
         end;

   $15 : begin
            dwordptr:=@memory[sibbyte+1];
            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EDX+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EDX';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EDX';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $16 : begin
           result:='ESI+EDX';
           last:=sibbyte+1;
         end;

   $17 : begin
           result:='EDI+EDX';
           last:=sibbyte+1;
         end;
//18-1F
   $18 : begin
           result:='EAX+EBX';
           last:=sibbyte+1;
         end;

   $19 : begin
           result:='ECX+EBX';
           last:=sibbyte+1;
         end;

   $1a : begin
           result:='EDX+EBX';
           last:=sibbyte+1;
         end;

   $1b : begin
           result:='EBX+EBX';
           last:=sibbyte+1;
         end;

   $1c : begin
           result:='ESP+EBX';
           last:=sibbyte+1;
         end;

   $1d : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EBX+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EBX';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EBX';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $1e : begin
           result:='ESI+EBX';
           last:=sibbyte+1;
         end;

   $1f : begin
           result:='EDI+EBX';
           last:=sibbyte+1;
         end;

//20-27  []
   $20,$60,$a0,$e0 : begin
           result:='EAX';
           last:=sibbyte+1;
         end;

   $21,$61,$a1,$e1 : begin
           result:='ECX';
           last:=sibbyte+1;
         end;

   $22,$62,$a2,$e2 : begin
           result:='EDX';
           last:=sibbyte+1;
         end;

   $23,$63,$a3,$e3 : begin
           result:='EBX';
           last:=sibbyte+1;
         end;

   $24,$64,$A4,$e4 : begin
           result:='ESP';
           last:=sibbyte+1;
         end;

   $25,$65,$a5,$e5 : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+4;
                    result:=inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+5;
                    result:='EBP+'+inttohexs(dwordptr^,8);
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $26,$66,$a6,$e6 : begin
           result:='ESI';
           last:=sibbyte+1;
         end;

   $27,$67,$a7,$e7 : begin
           result:='EDI';
           last:=sibbyte+1;
         end;
//28-2F
   $28 : begin
           result:='EAX+EBP';
           last:=sibbyte+1;
         end;

   $29 : begin
           result:='ECX+EBP';
           last:=sibbyte+1;
         end;

   $2a : begin
           result:='EDX+EBP';
           last:=sibbyte+1;
         end;

   $2b : begin
           result:='EBX+EBP';
           last:=sibbyte+1;
         end;

   $2c : begin
           result:='ESP+EBP';
           last:=sibbyte+1;
         end;

   $2d : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EBP+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EBP';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EBP';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $2e : begin
           result:='ESI+EBP';
           last:=sibbyte+1;
         end;

   $2f : begin
           result:='EDI+EBP';
           last:=sibbyte+1;
         end;

   $30 : begin
           result:='EAX+ESI';
           last:=sibbyte+1;
         end;

   $31 : begin
           result:='ECX+ESI';
           last:=sibbyte+1;
         end;

   $32 : begin
           result:='EDX+ESI';
           last:=sibbyte+1;
         end;

   $33 : begin
           result:='EBX+ESI';
           last:=sibbyte+1;
         end;

   $34 : begin
           result:='ESP+ESI';
           last:=sibbyte+1;
         end;

   $35 : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='ESI+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+ESI';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+ESI';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $36 : begin
           result:='ESI+ESI';
           last:=sibbyte+1;
         end;

   $37 : begin
           result:='EDI+ESI';
           last:=sibbyte+1;
         end;
//38-3F
   $38 : begin
           result:='EAX+EDI';
           last:=sibbyte+1;
         end;

   $39 : begin
           result:='ECX+EDI';
           last:=sibbyte+1;
         end;

   $3a : begin
           result:='EDX+EDI';
           last:=sibbyte+1;
         end;

   $3b : begin
           result:='EBX+EDI';
           last:=sibbyte+1;
         end;

   $3c : begin
           result:='ESP+EDI';
           last:=sibbyte+1;
         end;

   $3d : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EDI+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EDI';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EDI';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $3e : begin
           result:='ESI+EDI';
           last:=sibbyte+1;
         end;

   $3f : begin
           result:='EDI+EDI';
           last:=sibbyte+1;
         end;

//*2
//40-47
   $40 : begin
           result:='EAX+EAX*2';
           last:=sibbyte+1;
         end;

   $41 : begin
           result:='ECX+EAX*2';
           last:=sibbyte+1;
         end;

   $42 : begin
           result:='EDX+EAX*2';
           last:=sibbyte+1;
         end;

   $43 : begin
           result:='EBX+EAX*2';
           last:=sibbyte+1;
         end;

   $44 : begin
           result:='ESP+EAX*2';
           last:=sibbyte+1;
         end;

   $45 : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EAX*2+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EAX*2';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EAX*2';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $46 : begin
           result:='ESI+EAX*2';
           last:=sibbyte+1;
         end;

   $47 : begin
           result:='EDI+EAX*2';
           last:=sibbyte+1;
         end;
//48-4f
   $48 : begin
           result:='EAX+ECX*2';
           last:=sibbyte+1;
         end;

   $49 : begin
           result:='ECX+ECX*2';
           last:=sibbyte+1;
         end;

   $4a : begin
           result:='EDX+ECX*2';
           last:=sibbyte+1;
         end;

   $4b : begin
           result:='EBX+ECX*2';
           last:=sibbyte+1;
         end;

   $4c : begin
           result:='ESP+ECX*2';
           last:=sibbyte+1;
         end;

   $4d : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='ECX*2+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+ECX*2';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+ECX*2';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $4e : begin
           result:='ESI+ECX*2';
           last:=sibbyte+1;
         end;

   $4f : begin
           result:='EDI+ECX*2';
           last:=sibbyte+1;
         end;

//50-57
   $50 : begin
           result:='EAX+EDX*2';
           last:=sibbyte+1;
         end;

   $51 : begin
           result:='ECX+EDX*2';
           last:=sibbyte+1;
         end;

   $52 : begin
           result:='EDX+EDX*2';
           last:=sibbyte+1;
         end;

   $53 : begin
           result:='EBX+EDX*2';
           last:=sibbyte+1;
         end;

   $54 : begin
           result:='ESP+EDX*2';
           last:=sibbyte+1;
         end;

   $55 : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EDX*2+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EDX*2';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EDX*2';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $56 : begin
           result:='ESI+EDX*2';
           last:=sibbyte+1;
         end;

   $57 : begin
           result:='EDI+EDX*2';
           last:=sibbyte+1;
         end;
//58-5f
   $58 : begin
           result:='EAX+EBX*2';
           last:=sibbyte+1;
         end;

   $59 : begin
           result:='ECX+EBX*2';
           last:=sibbyte+1;
         end;

   $5a : begin
           result:='EDX+EBX*2';
           last:=sibbyte+1;
         end;

   $5b : begin
           result:='EBX+EBX*2';
           last:=sibbyte+1;
         end;

   $5c : begin
           result:='ESP+EBX*2';
           last:=sibbyte+1;
         end;

   $5d : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EBX*2+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EBX*2';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EBX*2';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $5e : begin
           result:='ESI+EBX*2';
           last:=sibbyte+1;
         end;

   $5f : begin
           result:='EDI+EBX*2';
           last:=sibbyte+1;
         end;
//60-67 see 20-27
//68-6f
   $68 : begin
           result:='EAX+EBP*2';
           last:=sibbyte+1;
         end;

   $69 : begin
           result:='ECX+EBP*2';
           last:=sibbyte+1;
         end;

   $6a : begin
           result:='EDX+EBP*2';
           last:=sibbyte+1;
         end;

   $6b : begin
           result:='EBX+EBP*2';
           last:=sibbyte+1;
         end;

   $6c : begin
           result:='ESP+EBP*2';
           last:=sibbyte+1;
         end;

   $6d : begin
            dwordptr:=@memory[sibbyte+1];


            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EBP*2+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+2;
                    result:='EBP+EBP*2';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EBP*2';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $6e : begin
           result:='ESI+EBP*2';
           last:=sibbyte+1;
         end;

   $6f : begin
           result:='EDI+EBP*2';
           last:=sibbyte+1;
         end;

//70-77
   $70 : begin
           result:='EAX+ESI*2';
           last:=sibbyte+1;
         end;

   $71 : begin
           result:='ECX+ESI*2';
           last:=sibbyte+1;
         end;

   $72 : begin
           result:='EDX+ESI*2';
           last:=sibbyte+1;
         end;

   $73 : begin
           result:='EBX+ESI*2';
           last:=sibbyte+1;
         end;

   $74 : begin
           result:='ESP+ESI*2';
           last:=sibbyte+1;
         end;

   $75 : begin
            dwordptr:=@memory[sibbyte+1];


            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='ESI*2+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+ESI*2';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+ESI*2';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $76 : begin
           result:='ESI+ESI*2';
           last:=sibbyte+1;
         end;

   $77 : begin
           result:='EDI+ESI*2';
           last:=sibbyte+1;
         end;
//78-7f
   $78 : begin
           result:='EAX+EDI*2';
           last:=sibbyte+1;
         end;

   $79 : begin
           result:='ECX+EDI*2';
           last:=sibbyte+1;
         end;

   $7a : begin
           result:='EDX+EDI*2';
           last:=sibbyte+1;
         end;

   $7b : begin
           result:='EBX+EDI*2';
           last:=sibbyte+1;
         end;

   $7c : begin
           result:='ESP+EDI*2';
           last:=sibbyte+1;
         end;

   $7d : begin
            dwordptr:=@memory[sibbyte+1];


            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EDI*2+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EDI*2';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EDI*2';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $7e : begin
           result:='ESI+EDI*2';
           last:=sibbyte+1;
         end;

   $7f : begin
           result:='EDI+EDI*2';
           last:=sibbyte+1;
         end;
//-----------------
//------*4---------
//-----------------
//80-BF  (COPY PASTE FROM 40-7F) but now replace *2 wih *4  (hope it doesn't cause bugs)

   $80 : begin
           result:='EAX+EAX*4';
           last:=sibbyte+1;
         end;

   $81 : begin
           result:='ECX+EAX*4';
           last:=sibbyte+1;
         end;

   $82 : begin
           result:='EDX+EAX*4';
           last:=sibbyte+1;
         end;

   $83 : begin
           result:='EBX+EAX*4';
           last:=sibbyte+1;
         end;

   $84 : begin
           result:='ESP+EAX*4';
           last:=sibbyte+1;
         end;

   $85 : begin
            dwordptr:=@memory[sibbyte+1];


            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EAX*4+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EAX*4';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EAX*4';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $86 : begin
           result:='ESI+EAX*4';
           last:=sibbyte+1;
         end;

   $87 : begin
           result:='EDI+EAX*4';
           last:=sibbyte+1;
         end;
//88-8f
   $88 : begin
           result:='EAX+ECX*4';
           last:=sibbyte+1;
         end;

   $89 : begin
           result:='ECX+ECX*4';
           last:=sibbyte+1;
         end;

   $8a : begin
           result:='EDX+ECX*4';
           last:=sibbyte+1;
         end;

   $8b : begin
           result:='EBX+ECX*4';
           last:=sibbyte+1;
         end;

   $8c : begin
           result:='ESP+ECX*4';
           last:=sibbyte+1;
         end;

   $8d : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='ECX*4+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+ECX*4';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+ECX*4';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $8e : begin
           result:='ESI+ECX*4';
           last:=sibbyte+1;
         end;

   $8f : begin
           result:='EDI+ECX*4';
           last:=sibbyte+1;
         end;

//90-97
   $90 : begin
           result:='EAX+EDX*4';
           last:=sibbyte+1;
         end;

   $91 : begin
           result:='ECX+EDX*4';
           last:=sibbyte+1;
         end;

   $92 : begin
           result:='EDX+EDX*4';
           last:=sibbyte+1;
         end;

   $93 : begin
           result:='EBX+EDX*4';
           last:=sibbyte+1;
         end;

   $94 : begin
           result:='ESP+EDX*4';
           last:=sibbyte+1;
         end;

   $95 : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EDX*4+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EDX*4';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EDX*4';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $96 : begin
           result:='ESI+EDX*4';
           last:=sibbyte+1;
         end;

   $97 : begin
           result:='EDI+EDX*4';
           last:=sibbyte+1;
         end;
//98-9f
   $98 : begin
           result:='EAX+EBX*4';
           last:=sibbyte+1;
         end;

   $99 : begin
           result:='ECX+EBX*4';
           last:=sibbyte+1;
         end;

   $9a : begin
           result:='EDX+EBX*4';
           last:=sibbyte+1;
         end;

   $9b : begin
           result:='EBX+EBX*4';
           last:=sibbyte+1;
         end;

   $9c : begin
           result:='ESP+EBX*4';
           last:=sibbyte+1;
         end;

   $9d : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EBX*4+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EBX*4';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EBX*4';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $9e : begin
           result:='ESI+EBX*4';
           last:=sibbyte+1;
         end;

   $9f : begin
           result:='EDI+EBX*4';
           last:=sibbyte+1;
         end;
//a0-a7 see 20-27
//a8-af
   $a8 : begin
           result:='EAX+EBP*4';
           last:=sibbyte+1;
         end;

   $a9 : begin
           result:='ECX+EBP*4';
           last:=sibbyte+1;
         end;

   $aa : begin
           result:='EDX+EBP*4';
           last:=sibbyte+1;
         end;

   $ab : begin
           result:='EBX+EBP*4';
           last:=sibbyte+1;
         end;

   $ac : begin
           result:='ESP+EBP*4';
           last:=sibbyte+1;
         end;

   $ad : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EBP*4+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EBP*4';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EBP*4';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $ae : begin
           result:='ESI+EBP*4';
           last:=sibbyte+1;
         end;

   $af : begin
           result:='EDI+EBP*4';
           last:=sibbyte+1;
         end;

//b0-b7
   $b0 : begin
           result:='EAX+ESI*4';
           last:=sibbyte+1;
         end;

   $b1 : begin
           result:='ECX+ESI*4';
           last:=sibbyte+1;
         end;

   $b2 : begin
           result:='EDX+ESI*4';
           last:=sibbyte+1;
         end;

   $b3 : begin
           result:='EBX+ESI*4';
           last:=sibbyte+1;
         end;

   $b4 : begin
           result:='ESP+ESI*4';
           last:=sibbyte+1;
         end;

   $b5 : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='ESI*4+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+ESI*4';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+ESI*4';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $b6 : begin
           result:='ESI+ESI*4';
           last:=sibbyte+1;
         end;

   $b7 : begin
           result:='EDI+ESI*4';
           last:=sibbyte+1;
         end;
//b8-bf
   $b8 : begin
           result:='EAX+EDI*4';
           last:=sibbyte+1;
         end;

   $b9 : begin
           result:='ECX+EDI*4';
           last:=sibbyte+1;
         end;

   $ba : begin
           result:='EDX+EDI*4';
           last:=sibbyte+1;
         end;

   $bb : begin
           result:='EBX+EDI*4';
           last:=sibbyte+1;
         end;

   $bc : begin
           result:='ESP+EDI*4';
           last:=sibbyte+1;
         end;

   $bd : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EDI*4+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EDI*4';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EDI*4';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $be : begin
           result:='ESI+EDI*4';
           last:=sibbyte+1;
         end;

   $bf : begin
           result:='EDI+EDI*4';
           last:=sibbyte+1;
         end;

//c0-ff same as 80-bf but with 8* instead of 4*
   $c0 : begin
           result:='EAX+EAX*8';
           last:=sibbyte+1;
         end;

   $c1 : begin
           result:='ECX+EAX*8';
           last:=sibbyte+1;
         end;

   $c2 : begin
           result:='EDX+EAX*8';
           last:=sibbyte+1;
         end;

   $c3 : begin
           result:='EBX+EAX*8';
           last:=sibbyte+1;
         end;

   $c4 : begin
           result:='ESP+EAX*8';
           last:=sibbyte+1;
         end;

   $c5 : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EAX*8+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EAX*8';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EAX*8';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $c6 : begin
           result:='ESI+EAX*8';
           last:=sibbyte+1;
         end;

   $c7 : begin
           result:='EDI+EAX*8';
           last:=sibbyte+1;
         end;
//88-8f
   $c8 : begin
           result:='EAX+ECX*8';
           last:=sibbyte+1;
         end;

   $c9 : begin
           result:='ECX+ECX*8';
           last:=sibbyte+1;
         end;

   $ca : begin
           result:='EDX+ECX*8';
           last:=sibbyte+1;
         end;

   $cb : begin
           result:='EBX+ECX*8';
           last:=sibbyte+1;
         end;

   $cc : begin
           result:='ESP+ECX*8';
           last:=sibbyte+1;
         end;

   $cd : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='ECX*8+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+ECX*8';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+ECX*8';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $ce : begin
           result:='ESI+ECX*8';
           last:=sibbyte+1;
         end;

   $cf : begin
           result:='EDI+ECX*8';
           last:=sibbyte+1;
         end;

//90-97
   $d0 : begin
           result:='EAX+EDX*8';
           last:=sibbyte+1;
         end;

   $d1 : begin
           result:='ECX+EDX*8';
           last:=sibbyte+1;
         end;

   $d2 : begin
           result:='EDX+EDX*8';
           last:=sibbyte+1;
         end;

   $d3 : begin
           result:='EBX+EDX*8';
           last:=sibbyte+1;
         end;

   $d4 : begin
           result:='ESP+EDX*8';
           last:=sibbyte+1;
         end;

   $d5 : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EDX*8+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EDX*8';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EDX*8';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $d6 : begin
           result:='ESI+EDX*8';
           last:=sibbyte+1;
         end;

   $d7 : begin
           result:='EDI+EDX*8';
           last:=sibbyte+1;
         end;
//98-9f
   $d8 : begin
           result:='EAX+EBX*8';
           last:=sibbyte+1;
         end;

   $d9 : begin
           result:='ECX+EBX*8';
           last:=sibbyte+1;
         end;

   $da : begin
           result:='EDX+EBX*8';
           last:=sibbyte+1;
         end;

   $db : begin
           result:='EBX+EBX*8';
           last:=sibbyte+1;
         end;

   $dc : begin
           result:='ESP+EBX*8';
           last:=sibbyte+1;
         end;

   $dd : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EBX*8+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EBX*8';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EBX*8';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $de : begin
           result:='ESI+EBX*8';
           last:=sibbyte+1;
         end;

   $df : begin
           result:='EDI+EBX*8';
           last:=sibbyte+1;
         end;
//a0-a7 see 20-27
//a8-af
   $e8 : begin
           result:='EAX+EBP*8';
           last:=sibbyte+1;
         end;

   $e9 : begin
           result:='ECX+EBP*8';
           last:=sibbyte+1;
         end;

   $ea : begin
           result:='EDX+EBP*8';
           last:=sibbyte+1;
         end;

   $eb : begin
           result:='EBX+EBP*8';
           last:=sibbyte+1;
         end;

   $ec : begin
           result:='ESP+EBP*8';
           last:=sibbyte+1;
         end;

   $ed : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EBP*8+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EBP*8';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EBP*8';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $ee : begin
           result:='ESI+EBP*8';
           last:=sibbyte+1;
         end;

   $ef : begin
           result:='EDI+EBP*8';
           last:=sibbyte+1;
         end;

//b0-b7
   $f0 : begin
           result:='EAX+ESI*8';
           last:=sibbyte+1;
         end;

   $f1 : begin
           result:='ECX+ESI*8';
           last:=sibbyte+1;
         end;

   $f2 : begin
           result:='EDX+ESI*8';
           last:=sibbyte+1;
         end;

   $f3 : begin
           result:='EBX+ESI*8';
           last:=sibbyte+1;
         end;

   $f4 : begin
           result:='ESP+ESI*8';
           last:=sibbyte+1;
         end;

   $f5 : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='ESI*8+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+ESI*8';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+ESI*8';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $f6 : begin
           result:='ESI+ESI*8';
           last:=sibbyte+1;
         end;

   $f7 : begin
           result:='EDI+ESI*8';
           last:=sibbyte+1;
         end;
//b8-bf
   $f8 : begin
           result:='EAX+EDI*8';
           last:=sibbyte+1;
         end;

   $f9 : begin
           result:='ECX+EDI*8';
           last:=sibbyte+1;
         end;

   $fa : begin
           result:='EDX+EDI*8';
           last:=sibbyte+1;
         end;

   $fb : begin
           result:='EBX+EDI*8';
           last:=sibbyte+1;
         end;

   $fc : begin
           result:='ESP+EDI*8';
           last:=sibbyte+1;
         end;

   $fd : begin
            dwordptr:=@memory[sibbyte+1];

            case getmod(memory[sibbyte-1]) of
              0 : begin
                    last:=sibbyte+5;
                    result:='EDI*8+'+inttohexs(dwordptr^,8);
                  end;

              1 : begin
                    last:=sibbyte+1;
                    result:='EBP+EDI*8';
                  end;

              2 : begin
                    last:=sibbyte+1;
                    result:='EBP+EDI*8';
                  end;

              3 : begin
                    result:='error';
                  end;
            end;
         end;

   $fe : begin
           result:='ESI+EDI*8';
           last:=sibbyte+1;
         end;

   $ff : begin
           result:='EDI+EDI*8';
           last:=sibbyte+1;
         end;

   end;

end;

function disassemble(var offset: dword; var description: string): string; overload;
var memory: TMemory;
    actualread: dword;
    startoffset: dword;
    tempresult: string;
    tempst: string;
    wordptr: ^word;
    dwordptr: ^dword;
    dwordptr2: ^dword;
    singleptr: ^single;
    doubleptr: ^double;
    extenedptr: ^extended;
    int64ptr: ^int64;
    i,j: integer;

    prefix: TPrefix;
    prefix2: TPrefix;
    isprefix: boolean;

    last: dword;
    foundit: boolean;
begin
  result:=inttohex(offset,8)+' - ';

  isprefix:=true;
  prefix:=[$f0,$f2,$f3,$2e,$36,$3e,$26,$64,$65,$66,$67];
  prefix2:=[];

  //forced 16-bit
  if mode16 then
    prefix2:=prefix2+[$66];



  startoffset:=offset;
  readprocessmemory(processhandle,pointer(offset),addr(memory),24,actualread);


  if actualread>0 then
  begin
    //I HATE THESE...   (I propably will not add them all, but I'll see how far I get)
    {$ifndef net}

    for i:=0 to actualread-1 do
    if (memory[i]=$CC) then
    begin
      //try to find it in the breakpointlist (not for net)
      try
        WaitForSingleObject(semaphore,infinite); //make sure it doesnt get deleted while I'm reading it
        if debuggerthread<>nil then
        begin
          for j:=0 to length(debuggerthread.int3userbreakpoints)-1 do
            if debuggerthread.int3userbreakpoints[j].address=offset+i then
            begin
              //it's in the list
              memory[i]:=debuggerthread.int3userbreakpoints[j].originalbyte;
              break;

            end;

          if debuggerthread.int3CEBreakpoint.address=offset+i then
            memory[i]:=debuggerthread.int3CEBreakpoint.originalbyte;

  //    memory[0]:=original byte
        end;

      finally
        releasesemaphore(semaphore,1,nil);
      end;
    end;
    {$endif}

    while isprefix do
    begin
      inc(offset);
      if memory[0] in prefix then
      begin
        result:=result+inttohexs(memory[0],2)+' ';
        isprefix:=true;
        inc(startoffset);
        prefix2:=prefix2+[memory[0]];
        readprocessmemory(processhandle,pointer(offset),addr(memory),24,actualread);
      end else isprefix:=false;
    end;

    if $F0 in prefix2 then tempresult:='lock ';
    if $F2 in prefix2 then tempresult:=tempresult+'repne ';
    if $f3 in prefix2 then tempresult:=tempresult+'repe ';


    case memory[0] of  //opcode
      $00 : begin
              description:='Add';
              tempresult:=tempresult+'add '+MODRM(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $01 : begin
              description:='Add';
              if $66 in prefix2 then  tempresult:=tempresult+'ADD '+MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                                      tempresult:=tempresult+'ADD '+MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $02 : begin
              description:='Add';
              tempresult:=tempresult+'ADD '+r8(memory[1])+','+MODRM(memory,prefix2,1,2,last);
              tempresult:=copy(tempresult,0,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $03 : begin
              description:='Add';
              if $66 in prefix2 then tempresult:=tempresult+'ADD '+r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                                     tempresult:=tempresult+'ADD '+r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);
              tempresult:=copy(tempresult,0,length(tempresult)-1);
              inc(offset,last-1);
            end;



      $04 : begin
              description:='Add x to y';
              tempresult:=tempresult+'ADD AL,'+inttohexs(memory[1],2);
              inc(offset);
            end;

      $05 : begin
              description:='Add x to y';
              wordptr:=@memory[1];
              dwordptr:=@memory[1];
              if $66 in prefix2 then
              begin
                tempresult:=tempresult+'ADD AX,'+inttohexs(wordptr^,4);
                inc(offset,2);
              end else
              begin
                tempresult:=tempresult+'ADD EAX,'+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;

      $06 : begin
              description:='Place ES on the stack';
              tempresult:=tempresult+'PUSH ES';
            end;

      $07 : begin
              description:='Remove ES from the stack';
              tempresult:=tempresult+'POP ES';
            end;

      $08 : begin
              description:='Logical Inclusive OR';
              tempresult:=tempresult+'OR '+MODRM(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $09 : begin
              description:='Logical Inclusive OR';
              if $66 in prefix2 then  tempresult:=tempresult+'OR '+MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                                      tempresult:=tempresult+'OR '+MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $0a : begin
              description:='Logical Inclusive OR';
              tempresult:=tempresult+'OR '+r8(memory[1])+','+MODRM(memory,prefix2,1,2,last);
              tempresult:=copy(tempresult,0,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $0b : begin
              description:='Logical Inclusive OR';
              if $66 in prefix2 then tempresult:=tempresult+'OR '+r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                                     tempresult:=tempresult+'OR '+r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);
              tempresult:=copy(tempresult,0,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $0c : begin
              description:='Logical Inclusive OR';
              tempresult:=tempresult+'OR AL,'+inttohexs(memory[1],2);
              inc(offset);
            end;

      $0d : begin
              description:='Logical Inclusive OR';
              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                tempresult:=tempresult+'OR AX,'+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                tempresult:=tempresult+'OR EAX,'+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;

      $0e : begin
              description:='Place CS on the stack';
              tempresult:=tempresult+'PUSH CS';
            end;

      $0f : begin  //SIMD extensions
              case memory[1] of
                $00 : begin
                        case getreg(memory[2]) of
                         0:  begin
                               description:='Store Local Descriptor Table Register';
                               if $66 in prefix2 then tempresult:=tempresult+'SLDT '+modrm(memory,prefix2,2,1,last,16) else
                                                      tempresult:=tempresult+'SLDT '+modrm(memory,prefix2,2,0,last);
                               tempresult:=copy(tempresult,1,length(tempresult)-1);
                               inc(offset,last-1);
                             end;

                         1:  begin
                               description:='Store Task Register';
                               tempresult:=tempresult+'STR '+modrm(memory,prefix2,2,1,last,16);
                               tempresult:=copy(tempresult,1,length(tempresult)-1);
                               inc(offset,last-1);
                             end;

                         2:  begin
                               description:='Load Local Descriptor Table Register';
                               tempresult:=tempresult+'LLDT '+modrm(memory,prefix2,2,1,last,16);
                               tempresult:=copy(tempresult,1,length(tempresult)-1);
                               inc(offset,last-1);
                             end;

                         3:  begin
                               description:='Load Task Register';
                               tempresult:=tempresult+'LTR '+modrm(memory,prefix2,2,1,last,16);
                               tempresult:=copy(tempresult,1,length(tempresult)-1);
                               inc(offset,last-1);
                             end;

                         4:  begin
                               description:='Verify a Segment for Reading';
                               tempresult:=tempresult+'VERR '+modrm(memory,prefix2,2,1,last,16);
                               tempresult:=copy(tempresult,1,length(tempresult)-1);
                               inc(offset,last-1);
                             end;

                         5:  begin
                               description:='Verify a Segment for Writing';
                               tempresult:=tempresult+'VERW '+modrm(memory,prefix2,2,1,last,16);
                               tempresult:=copy(tempresult,1,length(tempresult)-1);
                               inc(offset,last-1);
                             end;

//the following 2 were made up by me.
                         else
                         begin
                           description:='Not specified by the intel documentation';
                           tempresult:=tempresult+'DB 0F';
                         end;

                        end;

                      end;

                $01 : begin
                        case getreg(memory[2]) of
                         0:  begin
                                case memory[2] of
                                  $c1:  begin
                                          description:='Call to VM monitor by causing VM exit';
                                          tempresult:=tempresult+'VMCALL';
                                          inc(offset,2);
                                        end;

                                  $c2:  begin
                                          description:='Launch virtual machine managed by current VMCS';
                                          tempresult:=tempresult+'VMLAUNCH';
                                          inc(offset,2);
                                        end;

                                  $c3:  begin
                                          description:='Resume virtual machine managed by current VMCS';
                                          tempresult:=tempresult+'VMRESUME';
                                          inc(offset,2);
                                        end;

                                  $c4:  begin
                                          description:='Leaves VMX operation';
                                          tempresult:=tempresult+'VMXOFF';
                                          inc(offset,2);
                                        end;

                                  else  begin
                                          description:='Store Global Descriptor Table Register';
                                          tempresult:=tempresult+'SGDT '+modrm(memory,prefix2,2,0,last);
                                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                                          inc(offset,last-1);
                                        end;

                                end;


                              end;

                         1:  begin
                                description:='Store Interrupt Descriptor Table Register';
                                tempresult:=tempresult+'SIDT '+modrm(memory,prefix2,2,0,last);
                                tempresult:=copy(tempresult,1,length(tempresult)-1);
                                inc(offset,last-1);
                              end;

                         2:  begin
                                description:='Load Global Descriptor Table Register';
                                tempresult:=tempresult+'LGDT '+modrm(memory,prefix2,2,0,last);
                                tempresult:=copy(tempresult,1,length(tempresult)-1);
                                inc(offset,last-1);
                              end;

                         3:  begin
                                description:='Load Interupt Descriptor Table Register';
                                tempresult:=tempresult+'LIDT '+modrm(memory,prefix2,2,0,last);
                                tempresult:=copy(tempresult,1,length(tempresult)-1);
                                inc(offset,last-1);
                              end;

                         4:  begin
                                description:='Store Machine Status Word';
                                if $66 in prefix2 then tempresult:=tempresult+'SMSW '+modrm(memory,prefix2,2,0,last)
                                                  else tempresult:=tempresult+'SMSW '+modrm(memory,prefix2,2,0,last);
                                tempresult:=copy(tempresult,1,length(tempresult)-1);
                                inc(offset,last-1);
                              end;

                         6:  begin
                                description:='Load Machine Status Word';
                                tempresult:=tempresult+'LMSW '+modrm(memory,prefix2,2,1,last);
                                tempresult:=copy(tempresult,1,length(tempresult)-1);
                                inc(offset,last-1);
                              end;

                          7:  begin
                                description:='Invalidate TLB Entry';
                                tempresult:=tempresult+'INVPLG '+modrm(memory,prefix2,2,0,last);
                                tempresult:=copy(tempresult,1,length(tempresult)-1);
                                inc(offset,last-1);
                              end;
                        end;
                      end;

                $02 : begin
                        description:='Load Access Rights Byte';
                        if $66 in prefix2 then tempresult:=tempresult+'LAR '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'LAR '+r32(memory[2])+','+modrm(memory,prefix2,2,2,last);

                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $03 : begin
                        description:='Load Segment Limit';
                        if $66 in prefix2 then tempresult:=tempresult+'LSL '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'LSL '+r32(memory[2])+','+modrm(memory,prefix2,2,2,last);

                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $06 : begin
                        description:='Clear Task-Switched Flag in CR0';
                        tempresult:=tempresult+'CLTS';
                        inc(offset);
                      end;

                $08 : begin
                        description:='Invalidate Internal Caches';
                        tempresult:=tempresult+'INCD';
                        inc(offset);
                      end;

                $09 : begin
                        description:='Write Back and Invalidate Cache';
                        tempresult:=tempresult+'WBINVD';
                        inc(offset);
                      end;

                $0b : begin
                        description:='Undefined Instruction(Yes, this one really excists..)';
                        tempresult:=tempresult+'UD2';
                        inc(offset);
                      end;


                $10 : begin
                        if $f2 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          description:='Move Scalar Double-FP';
                          tempresult:='MOVSD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-5);
                          description:='Move Scalar Single-FP';
                          tempresult:='MOVSS '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='Move Unaligned Packed Double-FP';
                          tempresult:='MOVUPD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move Unaligned Four Packed Single-FP';
                          tempresult:='MOVUPS '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $11 : begin
                        if $f2 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          description:='Move Scalar Double-FP';
                          tempresult:='MOVSD '+modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-5);
                          description:='Move Scalar Single-FP';
                          tempresult:='MOVSS '+modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='Move Unaligned Packed Double-FP';
                          tempresult:='MOVUPD '+modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move Unaligned Four Packed Single-FP';
                          tempresult:='MOVUPS '+modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end;

                      end;

                $12 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Move low packed Double-Precision Floating-Point Value';
                          tempresult:=tempresult+'MOVLPD '+modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='High to Low Packed Single-FP';
                          tempresult:=tempresult+'MOVLPS '+modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end;
                      end;

                $13 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Move Low Packed Double-FP';
                          tempresult:=tempresult+'MOVLPD '+modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move Low Packed Single-FP';
                          tempresult:=tempresult+'MOVLPS '+modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end;
                      end;

                $14 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack Low Packed Single-FP';
                          tempresult:=tempresult+'UNPCKLPD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unpack Low Packed Single-FP';
                          tempresult:=tempresult+'UNPCKLPS '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $15 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack and Interleave High Packed Double-FP';
                          tempresult:=tempresult+'UNPCKHPD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unpack High packed Single-FP';
                          tempresult:=tempresult+'UNPCKHPS '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $16 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Move High Packed Double-Precision Floating-Point Value';
                          tempresult:=tempresult+'MOVHPD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='High to Low Packed Single-FP';
                          tempresult:=tempresult+'MOVHPS '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $17 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Move High Packed Double-Precision Floating-Point Value';
                          tempresult:=tempresult+'MOVHPD '+modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='High to Low Packed Single-FP';
                          tempresult:=tempresult+'MOVHPS '+modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end;
                      end;

                $18 : begin
                        case getreg(memory[2]) of
                          0:  begin
                                description:='Prefetch';
                                tempresult:=tempresult+'PREFETCHNTA '+modrm(memory,prefix2,2,2,last);
                                tempresult:=copy(tempresult,1,length(tempresult)-1);
                                inc(offset,last-1);
                              end;

                          1:  begin
                                description:='Prefetch';
                                tempresult:=tempresult+'PREFETCHT0 '+modrm(memory,prefix2,2,2,last);
                                tempresult:=copy(tempresult,1,length(tempresult)-1);
                                inc(offset,last-1);
                              end;

                          2:  begin
                                description:='Prefetch';
                                tempresult:=tempresult+'PREFETCHT1 '+modrm(memory,prefix2,2,2,last);
                                tempresult:=copy(tempresult,1,length(tempresult)-1);
                                inc(offset,last-1);
                              end;

                          3:  begin
                                description:='Prefetch';
                                tempresult:=tempresult+'PREFETCHT2 '+modrm(memory,prefix2,2,2,last);
                                tempresult:=copy(tempresult,1,length(tempresult)-1);
                                inc(offset,last-1);
                              end;

                        end;
                      end;


                $20 : begin
                        description:='Move from Control Register';
                        tempresult:=tempresult+'MOV '+MODRM(memory,prefix2,2,0,last)+cr(memory[2]);
                        inc(offset,last-1);
                      end;

                $21 : begin
                        description:='Move from Debug Register';
                        tempresult:=tempresult+'MOV '+MODRM(memory,prefix2,2,0,last)+dr(memory[2]);
                        inc(offset,last-1);
                      end;

                $22 : begin
                        description:='Move to Control Register';
                        tempresult:=tempresult+'MOV '+cr(memory[2])+','+MODRM(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $23 : begin
                        description:='Move to Debug Register';
                        tempresult:=tempresult+'MOV '+dr(memory[2])+','+MODRM(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $28 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Move Aligned Packed Fouble-FP Values';
                          tempresult:=tempresult+'MOVAPD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move Aligned Four Packed Single-FP';
                          tempresult:=tempresult+'MOVAPS '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $29 : begin
                        description:='Move Aligned Four Packed Single-FP';
                        tempresult:=tempresult+'MOVAPS '+modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                        inc(offset,last-1);
                      end;


                $2a : begin
                        if $f2 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          description:='Convert Doubleword integer to Scalar DoublePrecision Floating-point value';
                          tempresult:=tempresult+'CVTSI2SD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-5);
                          description:='Scalar Signed INT32 to Single-FP Conversion';
                          tempresult:=tempresult+'CVTSI2SS '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='Convert Packed DWORD''s to Packed DP-FP''s';
                            tempresult:=tempresult+'CVTPI2PD '+xmm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                            tempresult:=copy(tempresult,1,length(tempresult)-1);
                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='Packed Signed INT32 to Packed Single-FP Conversion';
                            tempresult:=tempresult+'CVTPI2PS '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                            tempresult:=copy(tempresult,1,length(tempresult)-1);
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $2B : begin
                        if $66 in prefix2 then
                        begin
                          tempresult:=tempresult+'MOVNTPD '+MODRM(memory,prefix2,2,4,last)+xmm(memory[2]);
                          description:='Move Packed double-precision floating-point using Non-Temporal hint';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          tempresult:=tempresult+'MOVNTPS '+MODRM(memory,prefix2,2,4,last)+xmm(memory[2]);
                          description:='Move Aligned Four Packed Single-FP Non Temporal';
                          inc(offset,last-1);
                        end;
                      end;

                $2c : begin
                        if $f2 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          description:='Convert with truncation scalar Double-precision floating point value to Signed doubleword integer';
                          tempresult:=tempresult+'CVTTSD2SI '+r32(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-5);
                          description:='Scalar Single-FP to Signed INT32 Conversion (Truncate)';
                          tempresult:=tempresult+'CVTTSS2SI '+r32(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='Packed DoublePrecision-FP to Packed DWORD Conversion (Truncate)';
                            tempresult:=tempresult+'CVTTPD2PI '+mm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                            tempresult:=copy(tempresult,1,length(tempresult)-1);
                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='Packed Single-FP to Packed INT32 Conversion (Truncate)';
                            tempresult:=tempresult+'CVTTPS2PI '+mm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                            tempresult:=copy(tempresult,1,length(tempresult)-1);
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $2d : begin
                        if $f2 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          description:='Convert Scalar Double-Precision Floating-Point Value to Doubleword Integer';
                          tempresult:=tempresult+'CVTSD2SI '+r32(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-5);
                          description:='Scalar Single-FP to Signed INT32 Conversion';
                          tempresult:=tempresult+'CVTSS2SI '+r32(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='Convert 2 packed DP-FP''s from param 2 to packed signed dword in param1';
                            tempresult:=tempresult+'CVTPI2PS '+mm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                            tempresult:=copy(tempresult,1,length(tempresult)-1);
                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='Packed Single-FP to Packed INT32 Conversion';
                            tempresult:=tempresult+'CVTPS2PI '+mm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                            tempresult:=copy(tempresult,1,length(tempresult)-1);
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $2e : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unordered Scalar Double-FP Compare and Set EFLAGS';
                          tempresult:=tempresult+'UCOMISD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unordered Scalar Single-FP Compare and Set EFLAGS';
                          tempresult:=tempresult+'UCOMISS '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;


                $2f : begin
                        if $66 in prefix2 then
                        begin
                          description:='Compare scalar ordered double-precision Floating Point Values and set EFLAGS';
                          tempresult:=tempresult+'COMISD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Scalar Ordered Single-FP Compare and Set EFLAGS';
                          tempresult:=tempresult+'COMISS '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $30 : begin
                        description:='Write to Model Specific Register';
                        tempresult:=tempresult+'WRMSR';
                        inc(offset);
                      end;

                $31 : begin
                        description:='Read Time-Stamp Counter';
                        tempresult:=tempresult+'RDTSC';
                        inc(offset);
                      end;

                $32 : begin
                        description:='Read from Model Specific Register';
                        tempresult:=tempresult+'RDMSR';
                        inc(offset);
                      end;

                $33: begin
                        description:='Read Performance-Monitoring counters';
                        tempresult:=tempresult+'RDPMC';
                        inc(offset);
                      end;

                $34: begin
                        description:='Fast Transistion to System Call Entry Point';
                        tempresult:=tempresult+'SYSENTER';
                        inc(offset);
                      end;

                $35: begin
                        description:='Fast Transistion from System Call Entry Point';
                        tempresult:=tempresult+'SYSEXIT';
                        inc(offset);
                      end;

                $40 : begin
                        description:='Move if overflow';
                        if $66 in prefix2 then tempresult:=tempresult+'CMOVO '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'CMOVO '+r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        inc(offset,last-1);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;

                $41 : begin
                        description:='Move if not overflow';
                        if $66 in prefix2 then tempresult:=tempresult+'CMOVNO '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'CMOVNO '+r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $42 : begin
                        description:='Move if below/ Move if Carry';
                        if $66 in prefix2 then tempresult:=tempresult+'CMOVB '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'CMOVB '+r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $43 : begin
                        description:='Move if above or equal/ Move if not carry';
                        if $66 in prefix2 then tempresult:=tempresult+'CMOVAE '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'CMOVAE '+r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $44 : begin
                        description:='Move if equal/Move if Zero';
                        if $66 in prefix2 then tempresult:=tempresult+'CMOVE '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'CMOVE '+r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $45 : begin
                        description:='Move if not equal/Move if not zero';
                        if $66 in prefix2 then tempresult:=tempresult+'CMOVNE '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'CMOVNE '+r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $46 : begin
                        description:='Move if below or equal';
                        if $66 in prefix2 then tempresult:=tempresult+'CMOVBE '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'CMOVBE '+r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;


                $47 : begin
                        description:='Move if Above';
                        if $66 in prefix2 then tempresult:=tempresult+'CMOVA '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'CMOVA '+r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $48 : begin
                        description:='Move if Sign';
                        if $66 in prefix2 then tempresult:=tempresult+'CMOVS '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'CMOVS '+r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $49 : begin
                        description:='Move if not sign';
                        if $66 in prefix2 then tempresult:=tempresult+'CMOVNS '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'CMOVNS '+r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $4A : begin
                        description:='Move if parity Even';
                        if $66 in prefix2 then tempresult:=tempresult+'CMOVPE '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'CMOVPE '+r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $4B : begin
                        description:='Move if not parity/Move if parity odd';
                        if $66 in prefix2 then tempresult:=tempresult+'CMOVNP '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'CMOVNP '+r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $4C : begin
                        description:='Move if less';
                        if $66 in prefix2 then tempresult:=tempresult+'CMOVL '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'CMOVL '+r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $4D : begin
                        description:='Move if greater or equal';
                        if $66 in prefix2 then tempresult:=tempresult+'CMOVGE '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'CMOVGE '+r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $4E : begin
                        description:='Move if less or equal';
                        if $66 in prefix2 then tempresult:=tempresult+'CMOVLE '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'CMOVLE '+r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $4F : begin
                        description:='Move if greater';
                        if $66 in prefix2 then tempresult:=tempresult+'CMOVG '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'CMOVG '+r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $50 : begin
                        if $66 in prefix2 then
                        begin
                          tempresult:=tempresult+'MOVMSKPD '+MODRM(memory,prefix2,2,0,last)+xmm(memory[2]);
                          description:='Extract Packed Double-Precision Floating-Point sign Mask';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          tempresult:=tempresult+'MOVMSKPS '+MODRM(memory,prefix2,2,0,last)+xmm(memory[2]);
                          description:='Move Mask To Integer';
                          inc(offset,last-1);
                        end;
                      end;

                $51 : begin
                        if $f2 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          tempresult:=tempresult+'SQRTSD '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          description:='Scalar Double-FP Square Root';
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-5);
                          tempresult:=tempresult+'SQRTSS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          description:='Scalar Single-FP Square Root';
                          tempresult:=copy(tempresult,1,length(tempresult)-1);

                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          tempresult:=tempresult+'SQRTPD '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          description:='Packed Double-FP Square Root';
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          tempresult:=tempresult+'SQRTPS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          description:='Packed Single-FP Square Root';
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $52 : begin
                        if $f3 in prefix2 then
                        begin
                          tempresult:=tempresult+'RSQRSS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Packed Single-FP Square Root Reciprocal';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          tempresult:=tempresult+'RSQRTPS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Scalar Single-FP Square Root Reciprocal';
                          inc(offset,last-1);
                        end;
                      end;

                $53 : begin
                        if $f3 in prefix2 then
                        begin
                          tempresult:=tempresult+'RCPSS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Scalar Single-FP Reciprocal';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          tempresult:=tempresult+'RCPPS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Packed Single-FP Reciprocal';
                          inc(offset,last-1);
                        end;
                      end;

                $54 : begin
                        if $66 in prefix2 then
                        begin
                          tempresult:=tempresult+'ANDPD '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Bit-wise Logical AND of xmm2/m128 and xmm1';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          tempresult:=tempresult+'ANDPS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Bit-wise Logical And For Single FP';
                          inc(offset,last-1);
                        end;
                      end;

                $55 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Bit-wise Logical AND NOT of Packed Double-precision FP Values';
                          tempresult:=tempresult+'ANDNPD '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Bit-wise Logical And Not For Single-FP';
                          tempresult:=tempresult+'ANDNPS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);

                          inc(offset,last-1);
                        end;
                      end;

                $56 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Bit-wise Logical OR of Double-FP';
                          tempresult:=tempresult+'ORPD '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Bit-wise Logical OR For Single-FP';
                          tempresult:=tempresult+'ORPS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);

                          inc(offset,last-1);
                        end;
                      end;

                $57 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Bit-wise Logical XOR For Double-FP Data';
                          tempresult:=tempresult+'XORPD '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Bit-wise Logical XOR For Single-FP Data';
                          tempresult:=tempresult+'XORPS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);

                          inc(offset,last-1);
                        end;
                      end;

                $58 : begin
                        if $f2 in prefix2 then
                        begin
                          //delete the repne from the tempresult
                          tempresult:=copy(tempresult,1,length(tempresult)-6);

                          tempresult:='ADDSD '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Add the lower SP FP number from XMM2/Mem to XMM1.';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          //delete the repe from the tempresult
                          tempresult:=copy(tempresult,1,length(tempresult)-5);

                          tempresult:='ADDSS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Add the lower SP FP number from XMM2/Mem to XMM1.';
                          inc(offset,last-1);
                        end else
                        begin
                          if $66 in prefix2 then
                          begin
                            tempresult:=tempresult+'ADDPD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                            tempresult:=copy(tempresult,1,length(tempresult)-1);
                            description:='Add packed double-precision floating-point values from XMM2/Mem to xmm1';
                            inc(offset,last-1);
                          end
                          else
                          begin
                            tempresult:=tempresult+'ADDPS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                            tempresult:=copy(tempresult,1,length(tempresult)-1);
                            description:='Add packed SP FP numbers from XMM2/Mem to XMM1';
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $59 : begin
                        if $f2 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          tempresult:='MULSD '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Scalar Double-FP Multiply';
                          inc(offset,last-1);
                        end else
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-5);
                          tempresult:='MULSS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Scalar Single-FP Multiply';
                          inc(offset,last-1);
                        end else
                        if $66 in prefix2 then
                        begin
                          tempresult:=tempresult+'MULPD '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Packed Double-FP Multiply';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          tempresult:=tempresult+'MULPS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Packed Single-FP Multiply';
                          inc(offset,last-1);
                        end;
                      end;

                $5A : begin
                        if $f2 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          tempresult:=tempresult+'CVTSD2SS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Convert Scalar Double-Precision Floating-Point Value to Scalar Single-Precision Floating-Point Value';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-5);
                          tempresult:=tempresult+'CVTSS2SD '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Convert Scalar Single-Precision Floating-Point Value to Scalar Double-Precision Floating-Point Value';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            tempresult:=tempresult+'CVTPD2PS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                            tempresult:=copy(tempresult,1,length(tempresult)-1);
                            description:='Convert Packed Double Precision FP Values to Packed Single Precision FP Values';
                            inc(offset,last-1);
                          end
                          else
                          begin
                            tempresult:=tempresult+'CVTPS2PD '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                            tempresult:=copy(tempresult,1,length(tempresult)-1);
                            description:='Convert Packed Single Precision FP Values to Packed Double Precision FP Values';
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $5B : begin
                        if $66 in prefix2 then
                        begin
                          tempresult:=tempresult+'CVTPS2DQ '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Convert PS-Precision FPoint Values to Packed DWORD''s ';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          tempresult:=tempresult+'CVTDQ2PS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Convert Packed DWORD''s to PS-Precision FPoint Values';
                          inc(offset,last-1);
                        end;
                      end;

                $5c : begin
                        if $f2 in prefix2 then
                        begin
                          tempresult:=tempresult+'SUBSD '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Scalar Double-FP Subtract';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          tempresult:=tempresult+'SUBSS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Scalar Single-FP Subtract';
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          tempresult:=tempresult+'SUBPD '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Packed Double-FP Subtract';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          tempresult:=tempresult+'SUBPS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Packed Single-FP Subtract';
                          inc(offset,last-1);
                        end;
                      end;


                $5d : begin
                        if $f2 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          tempresult:=tempresult+'MINSD '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Scalar Single-FP Minimum';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-5);
                          tempresult:=tempresult+'MINSS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Scalar Single-FP Minimum';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            tempresult:=tempresult+'MINPD '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                            tempresult:=copy(tempresult,1,length(tempresult)-1);
                            description:='Packed Double-FP Minimum';
                            inc(offset,last-1);
                          end
                          else
                          begin
                            tempresult:=tempresult+'MINPS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                            tempresult:=copy(tempresult,1,length(tempresult)-1);
                            description:='Packed Single-FP Minimum';
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $5e : begin
                        if $f2 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          tempresult:=tempresult+'DIVSD '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Scalar Double-Precision-FP Divide';
                          inc(offset,last-1);
                        end else
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-5);
                          tempresult:=tempresult+'DIVSS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          description:='Scalar Single-FP Divide';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            tempresult:=tempresult+'DIVPD '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                            tempresult:=copy(tempresult,1,length(tempresult)-1);
                            description:='Packed Double-Precision FP Divide';
                            inc(offset,last-1);
                          end
                          else
                          begin
                            tempresult:=tempresult+'DIVPS '+xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                            tempresult:=copy(tempresult,1,length(tempresult)-1);
                            description:='Packed Single-FP Divide';
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $5f : begin
                        if $f2 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          description:='Scalar Double-FP Maximum';
                          tempresult:=tempresult+'MAXSD '+xmm(memory[1])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end else
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          description:='Scalar Single-FP Maximum';
                          tempresult:=tempresult+'MAXSS '+xmm(memory[1])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='Packed Double-FP Maximum';
                            tempresult:=tempresult+'MAXPD '+xmm(memory[1])+','+modrm(memory,prefix2,2,4,last);
                            tempresult:=copy(tempresult,1,length(tempresult)-1);
                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='Packed Single-FP Maximum';
                            tempresult:=tempresult+'MAXPS '+xmm(memory[1])+','+modrm(memory,prefix2,2,4,last);
                            tempresult:=copy(tempresult,1,length(tempresult)-1);
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $60 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack Low Packed Data';
                          tempresult:=tempresult+'PUNPCKLBW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unpack Low Packed Data';
                          tempresult:=tempresult+'PUNPCKLBW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $61 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack Low Packed Data';
                          tempresult:=tempresult+'PUNPCKLWD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unpack Low Packed Data';
                          tempresult:=tempresult+'PUNPCKLWD '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $62 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack Low Packed Data';
                          tempresult:=tempresult+'PUNPCKLDQ '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unpack Low Packed Data';
                          tempresult:=tempresult+'PUNPCKLDQ '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $63 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Pack with signed Saturation';
                          tempresult:=tempresult+'PACKSSWB '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Pack with signed Saturation';
                          tempresult:=tempresult+'PACKSSWB '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $64 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Compare for Greater Than';
                          tempresult:=tempresult+'PCMPGTB '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Compare for Greater Than';
                          tempresult:=tempresult+'PCMPGTB '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $65 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Compare for Greater Than';
                          tempresult:=tempresult+'PCMPGTW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Compare for Greater Than';
                          tempresult:=tempresult+'PCMPGTW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $66 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Compare for Greater Than';
                          tempresult:=tempresult+'PCMPGTD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Compare for Greater Than';
                          tempresult:=tempresult+'PCMPGTD '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;


                $67 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Pack with Unsigned Saturation';
                          tempresult:=tempresult+'PACKUSWB '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Pack with Unsigned Saturation';
                          tempresult:=tempresult+'PACKUSWB '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $68 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack High Packed Data';
                          tempresult:=tempresult+'PUNPCKHBW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unpack High Packed Data';
                          tempresult:=tempresult+'PUNPCKHBW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $69 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack High Packed Data';
                          tempresult:=tempresult+'PUNPCKHWD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unpack High Packed Data';
                          tempresult:=tempresult+'PUNPCKHWD '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $6a : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack High Packed Data';
                          tempresult:=tempresult+'PUNPCKHDQ '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unpack High Packed Data';
                          tempresult:=tempresult+'PUNPCKHDQ '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $6B : begin
                        if $66 in prefix2 then
                        begin
                          description:='Pack with signed Saturation';
                          tempresult:=tempresult+'PACKSSDW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Pack with signed Saturation';
                          tempresult:=tempresult+'PACKSSDW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $6c : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack Low Packed Data';
                          tempresult:=tempresult+'PUNPCKLQDQ '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                      end;

                $6d : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack High Packed Data';
                          tempresult:=tempresult+'PUNPCKHQDQ '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                      end;


                $6e : begin
                        if $66 in prefix2 then
                        begin
                          description:='Move Doubleword';
                          tempresult:=tempresult+'MOVD '+xmm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move 32 Bits';
                          tempresult:=tempresult+'MOVD '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $6f : begin
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-5);
                          description:='Move UnAligned Double Quadword';
                          tempresult:=tempresult+'MOVDQU '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='Move Aligned Double Quadword';
                          tempresult:=tempresult+'MOVDQA '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move 64 Bits';
                          tempresult:=tempresult+'MOVDQA '+mm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $70 : begin
                        if $f2 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          description:='Shuffle Packed Low Words';
                          tempresult:=tempresult+'PSHUFLW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last)+''+inttohexs(memory[last],2);
                          inc(offset,last);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-5);
                          description:='Shuffle Packed High Words';
                          tempresult:=tempresult+'PSHUFHW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last)+''+inttohexs(memory[last],2);
                          inc(offset,last);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shuffle DoubleWord';
                          tempresult:=tempresult+'PSHUFD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last)+''+inttohexs(memory[last],2);
                          inc(offset,last);
                        end
                        else
                        begin
                          description:='Packed Shuffle Word';
                          tempresult:=tempresult+'PSHUFW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last)+''+inttohexs(memory[last],2);
                          inc(offset,last);
                        end;
                      end;

                $71 : begin
                        case getreg(memory[2]) of
                          2 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed Shift Right Logical';
                                  tempresult:=tempresult+'PSRLW '+xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='Packed Shift Right Logical';
                                  tempresult:=tempresult+'PSRLW '+mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          4 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed Shift Right Arithmetic';
                                  tempresult:=tempresult+'PSRAW '+xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='Packed Shift Right Arithmetic';
                                  tempresult:=tempresult+'PSRAW '+mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          6 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed Shift Left Logical';
                                  tempresult:=tempresult+'PSLLW '+xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='Packed Shift Left Logical';
                                  tempresult:=tempresult+'PSLLW '+mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;
                        end;
                      end;

                $72 : begin
                        case getreg(memory[2]) of
                          2 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed Shift Right Logical';
                                  tempresult:=tempresult+'PSRLD '+xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='Packed Shift Right Logical';
                                  tempresult:=tempresult+'PSRLD '+mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          4 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed Shift Right Arithmetic';
                                  tempresult:=tempresult+'PSRAD '+xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='Packed Shift Right Arithmetic';
                                  tempresult:=tempresult+'PSRAD '+mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          6 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed Shift Left Logical';
                                  tempresult:=tempresult+'PSLLD '+xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='Packed Shift Left Logical';
                                  tempresult:=tempresult+'PSLLD '+mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;
                        end;
                      end;

                $73 : begin
                        case getreg(memory[2]) of
                          2 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed Shift Right Logical';
                                  tempresult:=tempresult+'PSRLQ '+xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='Packed Shift Right Logical';
                                  tempresult:=tempresult+'PSRLQ '+mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          3 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Shift double Quadword right Lopgical';
                                  tempresult:=tempresult+'PSRLDQ '+xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          6 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed Shift Left Logical';
                                  tempresult:=tempresult+'PSLLQ '+xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='Packed Shift Left Logical';
                                  tempresult:=tempresult+'PSLLQ '+mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          7 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Shift Double Quadword Left Logical';
                                  tempresult:=tempresult+'PSLLDQ '+xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;
                        end;
                      end;



                $74 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Compare for Equal';
                          tempresult:=tempresult+'PCMPEQB '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Compare for Equal';
                          tempresult:=tempresult+'PCMPEQB '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $75 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Compare for Equal';
                          tempresult:=tempresult+'PCMPEQW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Compare for Equal';
                          tempresult:=tempresult+'PCMPEQW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $76 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Compare for Equal';
                          tempresult:=tempresult+'PCMPEQD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Compare for Equal';
                          tempresult:=tempresult+'PCMPEQD '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;


                $77 : begin
                        description:='Empty MMX™ State';
                        tempresult:=tempresult+'EMMS';
                        inc(offset);
                      end;

                $78 : begin
                        description:='Reads a specified VMCS field (32 Bits)';
                        tempresult:=tempresult+'VMREAD '+modrm(memory,prefix2,2,2,last)+r32(memory[2]);
                        inc(offset,last-1);
                      end;

                $79 : begin
                        description:='Writes a specified VMCS field (32 Bits)';
                        tempresult:=tempresult+'VMWRITE '+r32(memory[2])+','+modrm(memory,prefix2,2,2,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $7e : begin
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-5);
                          description:='Move quadword';
                          tempresult:=tempresult+'MOVQ '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='Move 32 Bits';
                          tempresult:=tempresult+'MOVD '+modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move 32 Bits';
                          tempresult:=tempresult+'MOVD '+modrm(memory,prefix2,2,3,last)+mm(memory[2]);
                          inc(offset,last-1);
                        end;
                      end;

                $7f : begin
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-5);
                          description:='Move Unaligned double Quadword';
                          tempresult:=tempresult+'MOVDQU '+modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='Move aligned double Quadword';
                          tempresult:=tempresult+'MOVDQA '+modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move 64 Bits';
                          tempresult:=tempresult+'MOVQ '+modrm(memory,prefix2,2,3,last)+mm(memory[2]);
                          inc(offset,last-1);
                        end;
                      end;

                $80 : begin
                        description:='Jump near if overflow';
                        tempresult:=tempresult+'JO ';

                        inc(offset,1+4);
                        tempresult:=tempresult+inttohexs(dword(offset+pint(@memory[2])^),8);
                      end;

                $81 : begin
                        description:='Jump near if not overflow';
                        tempresult:=tempresult+'JNO ';
                        inc(offset,1+4);
                        tempresult:=tempresult+inttohexs(offset+pint(@memory[2])^,8);

                      end;

                $82 : begin
                        description:='Jump near if below/carry';
                        dwordptr:=@memory[2];
                        tempresult:=tempresult+'JB ';
                        inc(offset,1+4);
                        tempresult:=tempresult+inttohexs(dword(offset+pint(@memory[2])^),8);

                      end;

                $83 : begin
                        description:='Jump near if above or equal';
                        tempresult:=tempresult+'JAE ';
                        inc(offset,1+4);
                        tempresult:=tempresult+inttohexs(dword(offset+pint(@memory[2])^),8);
                      end;

                $84 : begin
                        description:='Jump near if equal';
                        tempresult:=tempresult+'JE ';

                        inc(offset,1+4);
                        tempresult:=tempresult+inttohexs(dword(offset+pint(@memory[2])^),8);
                      end;

                $85 : begin
                        description:='Jump near if not equal';
                        tempresult:=tempresult+'JNE ';

                        inc(offset,1+4);
                        tempresult:=tempresult+inttohexs(dword(offset+pint(@memory[2])^),8);

                      end;

                $86 : begin
                        description:='Jump near if below or equal';
                        tempresult:=tempresult+'JBE ';

                        inc(offset,1+4);
                        tempresult:=tempresult+inttohexs(dword(offset+pint(@memory[2])^),8);
                      end;

                $87 : begin
                        description:='Jump near if above';
                        tempresult:=tempresult+'JA ';

                        inc(offset,1+4);
                        tempresult:=tempresult+inttohexs(dword(offset+pint(@memory[2])^),8);
                      end;

                $88 : begin
                        description:='Jump near if sign';
                        tempresult:=tempresult+'JS ';

                        inc(offset,1+4);
                        tempresult:=tempresult+inttohexs(dword(offset+pint(@memory[2])^),8);
                      end;

                $89 : begin
                        description:='Jump near if less';
                        tempresult:=tempresult+'JL ';

                        inc(offset,1+4);
                        tempresult:=tempresult+inttohexs(dword(offset+pint(@memory[2])^),8);
                      end;

                $8a : begin
                        description:='Jump near if parity';
                        tempresult:=tempresult+'JP ';

                        inc(offset,1+4);
                        tempresult:=tempresult+inttohexs(dword(offset+pint(@memory[2])^),8);
                      end;

                $8b : begin
                        description:='Jump near if not parity';
                        tempresult:=tempresult+'JNP ';

                        inc(offset,1+4);
                        tempresult:=tempresult+inttohexs(dword(offset+pint(@memory[2])^),8);
                      end;

                $8c : begin
                        description:='Jump near if less';
                        tempresult:=tempresult+'JL ';

                        inc(offset,1+4);
                        tempresult:=tempresult+inttohexs(dword(offset+pint(@memory[2])^),8);
                      end;

                $8d : begin
                        description:='Jump near if not less';
                        tempresult:=tempresult+'JNL ';

                        inc(offset,1+4);
                        tempresult:=tempresult+inttohexs(dword(offset+pint(@memory[2])^),8);
                      end;

                $8e : begin
                        description:='Jump near if not greater';
                        tempresult:=tempresult+'JNG ';

                        inc(offset,1+4);
                        tempresult:=tempresult+inttohexs(dword(offset+pint(@memory[2])^),8);
                      end;

                $8f : begin
                        description:='Jump near if greater';
                        tempresult:=tempresult+'JG ';
                        tempresult:=tempresult+inttohexs(dword(offset+pint(@memory[2])^),8);
                        inc(offset,1+4);
                      end;

                $90 : begin
                        description:='Set byte if overflow';
                        tempresult:=tempresult+'SETO '+modrm(memory,prefix2,2,2,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $91 : begin
                        description:='Set byte if not overfloww';
                        tempresult:=tempresult+'SETNO '+modrm(memory,prefix2,2,2,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $92 : begin
                        description:='Set byte if below/carry';
                        tempresult:=tempresult+'SETB '+modrm(memory,prefix2,2,2,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $93 : begin
                        description:='Set byte if above or equal';
                        tempresult:=tempresult+'SETAE '+modrm(memory,prefix2,2,2,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $94 : begin
                        description:='Set byte if equal';
                        tempresult:=tempresult+'SETE '+modrm(memory,prefix2,2,2,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $95 : begin
                        description:='Set byte if not carry(not equal)';
                        tempresult:=tempresult+'SETNC '+modrm(memory,prefix2,2,2,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $96 : begin
                        description:='Set byte if below or equal';
                        tempresult:=tempresult+'SETBE '+modrm(memory,prefix2,2,2,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $97 : begin
                        description:='Set byte if above';
                        tempresult:=tempresult+'SETA '+modrm(memory,prefix2,2,2,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $98 : begin
                        description:='Set byte if sign';
                        tempresult:=tempresult+'SETS '+modrm(memory,prefix2,2,2,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $99 : begin
                        description:='Set byte if not sign';
                        tempresult:=tempresult+'SETNS '+modrm(memory,prefix2,2,2,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $9a : begin
                        description:='Set byte if parity';
                        tempresult:=tempresult+'SETP '+modrm(memory,prefix2,2,2,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $9b : begin
                        description:='Set byte if not parity';
                        tempresult:=tempresult+'SETNP '+modrm(memory,prefix2,2,2,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $9c : begin
                        description:='Set byte if less';
                        tempresult:=tempresult+'SETL '+modrm(memory,prefix2,2,2,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $9d : begin
                        description:='Set byte if greater or equal';
                        tempresult:=tempresult+'SETGE '+modrm(memory,prefix2,2,2,last);
                        inc(offset,last-1);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;

                $9e : begin
                        description:='Set byte if less or equal';
                        tempresult:=tempresult+'SETLE '+modrm(memory,prefix2,2,2,last);
                        inc(offset,last-1);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;

                $9f : begin
                        description:='Set byte if greater';
                        tempresult:=tempresult+'SETG '+modrm(memory,prefix2,2,2,last);
                        inc(offset,last-1);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);

                      end;

                $a0 : begin
                        description:='Push Word or Doubleword Onto the Stack';
                        tempresult:=tempresult+'PUSH FS';
                        inc(offset);
                      end;

                $a1 : begin
                        description:='Pop a Value from the Stack';
                        tempresult:=tempresult+'POP FS';
                        inc(offset);
                      end;


                $a2 : begin
                        description:='CPU Identification';
                        tempresult:=tempresult+'CPUID';
                        inc(offset);
                      end;

                $a3 : begin
                        description:='Bit Test';
                        if $66 in prefix2 then tempresult:=tempresult+'BT '+MODRM(memory,prefix2,2,1,last)+r16(memory[2]) else
                                               tempresult:=tempresult+'BT '+MODRM(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);

                      end;

                $a4 : begin
                        description:='Double Precision Shift Left';
                        if $66 in prefix2 then tempresult:=tempresult+'SHLD '+MODRM(memory,prefix2,2,1,last)+r16(memory[2]) else
                                               tempresult:=tempresult+'SHLD '+MODRM(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);

                      end;

                $a5 : begin
                        description:='Double Precision Shift Left';
                        if $66 in prefix2 then tempresult:=tempresult+'SHLD '+MODRM(memory,prefix2,2,1,last)+'CL' else
                                               tempresult:=tempresult+'SHLD '+MODRM(memory,prefix2,2,0,last)+'CL';
                        inc(offset,last-1);

                      end;

                $a8 : begin
                        description:='Push Word or Doubleword Onto the Stack';
                        tempresult:=tempresult+'PUSH GS';
                        inc(offset);
                      end;

                $a9 : begin
                        description:='Pop a Value from the Stack';
                        tempresult:=tempresult+'POP GS';
                        inc(offset);
                      end;

                $aa : begin
                        description:='Resume from System Management Mode';
                        tempresult:=tempresult+'RSM';
                        inc(offset);
                      end;

                $ab : begin
                        description:='Bit Test and Set';
                        if $66 in prefix2 then tempresult:=tempresult+'BTS '+MODRM(memory,prefix2,2,1,last)+r16(memory[2]) else
                                               tempresult:=tempresult+'BTS '+MODRM(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);

                      end;

                $ac : begin
                        description:='Double Precision Shift Right';
                        if $66 in prefix2 then tempresult:=tempresult+'SHRD '+MODRM(memory,prefix2,2,1,last)+r16(memory[2]) else
                                               tempresult:=tempresult+'SHRD '+MODRM(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);

                      end;

                $ad : begin
                        description:='Double Precision Shift Right';
                        if $66 in prefix2 then tempresult:=tempresult+'SHRD '+MODRM(memory,prefix2,2,1,last)+'CL' else
                                               tempresult:=tempresult+'SHRD '+MODRM(memory,prefix2,2,0,last)+'CL';
                        inc(offset,last-1);

                      end;

                $ae : begin
                        case getreg(memory[2]) of
                          0:  begin
                                description:='Store FP and MMX State and Streaming SIMD Extension State';
                                tempresult:=tempresult+'FXSAVE '+MODRM(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          1:  begin
                                description:='Restore FP and MMX State and Streaming SIMD Extension State';
                                tempresult:=tempresult+'FXRSTOR '+MODRM(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          2:  begin
                                description:='Load Streaming SIMD Extension Control/Status';
                                tempresult:='LDMXCSR '+MODRM(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          3:  begin
                                description:='Store Streaming SIMD Extension Control/Status';
                                tempresult:='STMXCSR '+MODRM(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          7:  begin
                                description:='Store Fence';
                                tempresult:='SFENCE '+MODRM(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                        END;

                      end;

                $af : begin
                        description:='Signed Multiply';
                        if $66 in prefix2 then tempresult:=tempresult+'IMUL '+r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'IMUL '+r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $b0 : begin
                        description:='Compare and Exchange';
                        tempresult:=tempresult+'CMPXCHG '+modrm(memory,prefix2,2,2,last)+r8(memory[2]);
                        inc(offset,last-1);
                      end;

                $b1 : begin
                        description:='Compare and Exchange';
                        if $66 in prefix2 then tempresult:=tempresult+'CMPXCHG '+MODRM(memory,prefix2,2,1,last)+r16(memory[2]) else
                                               tempresult:=tempresult+'CMPXCHG '+MODRM(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);
                      end;

                $b2 : begin
                        description:='Load Far Pointer';
                        if $66 in prefix2 then tempresult:=tempresult+'LSS '+r16(memory[2])+','+MODRM(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'LSS '+r32(memory[2])+','+MODRM(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $b3 : begin
                        description:='Bit Test and Reset';
                        if $66 in prefix2 then tempresult:=tempresult+'BTR '+MODRM(memory,prefix2,2,1,last)+r16(memory[2]) else
                                               tempresult:=tempresult+'BTR '+MODRM(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);

                      end;

                $b4 : begin
                        description:='Load Far Pointer';
                        if $66 in prefix2 then tempresult:=tempresult+'LFS '+r16(memory[2])+','+MODRM(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'LFS '+r32(memory[2])+','+MODRM(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $b5 : begin
                        description:='Load Far Pointer';
                        if $66 in prefix2 then tempresult:=tempresult+'LGS '+r16(memory[2])+','+MODRM(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'LGS '+r32(memory[2])+','+MODRM(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $b6 : begin
                        description:='Load Far Pointer';
                        if $66 in prefix2 then tempresult:=tempresult+'MOVZX '+r16(memory[2])+','+MODRM(memory,prefix2,2,2,last,8) else
                                               tempresult:=tempresult+'MOVZX '+r32(memory[2])+','+MODRM(memory,prefix2,2,2,last,8);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);

                        inc(offset,last-1);
                      end;

                $b7 : begin
                        description:='Load Far Pointer';
                        tempresult:=tempresult+'MOVZX '+r32(memory[2])+','+MODRM(memory,prefix2,2,1,last,16);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);

                        inc(offset,last-1);
                      end;


                $ba : begin
                        case getreg(memory[2]) of
                          4:  begin
                                //BT
                                description:='Bit Test';
                                if $66 in prefix2 then tempresult:=tempresult+'BT '+MODRM(memory,prefix2,2,1,last)+''+inttohexs(memory[3],2)
                                                  else tempresult:=tempresult+'BT '+MODRM(memory,prefix2,2,0,last)+''+inttohexs(memory[3],2);     //notice the difference in the modrm 4th parameter

                                inc(offset,last-1+1);
                              end;

                          5:  begin
                                //BTS
                                description:='Bit Test and Set';
                                if $66 in prefix2 then tempresult:=tempresult+'BTS '+MODRM(memory,prefix2,2,1,last)+''+inttohexs(memory[3],2)
                                                  else tempresult:=tempresult+'BTS '+MODRM(memory,prefix2,2,0,last)+''+inttohexs(memory[3],2);     //notice the difference in the modrm 4th parameter
                                inc(offset,last-1+1);
                              end;

                          6:  begin
                                //BTR
                                description:='Bit Test and Reset';
                                if $66 in prefix2 then tempresult:=tempresult+'BTR '+MODRM(memory,prefix2,2,1,last)+''+inttohexs(memory[3],2)
                                                  else tempresult:=tempresult+'BTR '+MODRM(memory,prefix2,2,0,last)+''+inttohexs(memory[3],2);     //notice the difference in the modrm 4th parameter
                                inc(offset,last-1+1);
                              end;

                          7:  begin
                                //BTC
                                description:='Bit Test and Complement';
                                if $66 in prefix2 then tempresult:=tempresult+'BTC '+MODRM(memory,prefix2,2,1,last)+''+inttohexs(memory[3],2)
                                                  else tempresult:=tempresult+'BTC '+MODRM(memory,prefix2,2,0,last)+''+inttohexs(memory[3],2);     //notice the difference in the modrm 4th parameter
                                inc(offset,last-1+1);
                              end;

                        end;

                      end;

                $bb : begin
                        description:='Bit Test and Complement';
                        if $66 in prefix2 then tempresult:=tempresult+'BTC '+MODRM(memory,prefix2,2,1,last)+r16(memory[2]) else
                                               tempresult:=tempresult+'BTC '+MODRM(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);

                      end;


                $bc : begin
                        //bsf
                        description:='Bit Scan Forward';
                        if $66 in prefix2 then tempresult:=tempresult+'BSF '+r16(memory[2])+','+MODRM(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'BSF '+r32(memory[2])+','+MODRM(memory,prefix2,2,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);

                        inc(offset,last-1);
                      end;

                $bd : begin
                        //bsf
                        description:='Bit Scan Reverse';
                        if $66 in prefix2 then tempresult:=tempresult+'BSR '+r16(memory[2])+','+MODRM(memory,prefix2,2,1,last) else
                                               tempresult:=tempresult+'BSR '+r32(memory[2])+','+MODRM(memory,prefix2,2,1,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);

                        inc(offset,last-1);
                      end;

                $be : begin
                        description:='Move with Sign-Extension';
                        if $66 in prefix2 then tempresult:=tempresult+'MOVSX '+r16(memory[2])+','+MODRM(memory,prefix2,2,2,last,8) else
                                               tempresult:=tempresult+'MOVSX '+r32(memory[2])+','+MODRM(memory,prefix2,2,2,last,8);

                        tempresult:=copy(tempresult,1,length(tempresult)-1);

                        inc(offset,last-1);
                      end;

                $bf : begin
                        description:='Move with Sign-Extension';
                        tempresult:=tempresult+'MOVSX '+r32(memory[2])+','+MODRM(memory,prefix2,2,1,last,16);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                $c0 : begin
                        description:='Exchange and Add';
                        tempresult:=tempresult+'XADD '+MODRM(memory,prefix2,2,2,last)+r8(memory[2]);
                        inc(offset,last-1);
                      end;

                $c1 : begin
                        description:='Exchange and Add';
                        if $66 in prefix2 then tempresult:=tempresult+'XADD '+MODRM(memory,prefix2,2,1,last)+r16(memory[2])
                                          else tempresult:=tempresult+'XADD '+MODRM(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);
                      end;

                $c2 : begin
                        if $f2 in prefix2 then
                        begin
                          description:='Compare Scalar Dpuble-Precision Floating-Point Values';
                          tempresult:=tempresult+'CMPSD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last,128)+''+inttohexs(memory[last],2);
                          inc(offset,last);
                        end
                        else
                        if $F3 in prefix2 then
                        begin
                          description:='Packed Single-FP Compare';
                          tempresult:=tempresult+'CMPSS '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last,128)+''+inttohexs(memory[last],2);
                          inc(offset,last);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='Compare packed double-Precision Floating-Point Values';
                            tempresult:=tempresult+'CMPPD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last,128)+''+inttohexs(memory[last],2);
                            inc(offset,last);
                          end
                          else
                          begin
                            description:='Packed Single-FP Compare';
                            tempresult:=tempresult+'CMPPS '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last,128)+''+inttohexs(memory[last],2);
                            inc(offset,last);
                          end;
                        end;
                      end;

                $c3 : begin
                        description:='Store doubleword using Non-temporal Hint';
                        tempresult:=tempresult+'MOVNTI '+modrm(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last);
                      end;

                $c4 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Insert Word';
                          tempresult:=tempresult+'PINSRW '+xmm(memory[2])+','+modrm(memory,prefix2,2,0,last)+inttohexs(memory[last],2);
                          inc(offset,last);
                        end
                        else
                        begin
                          description:='Insert Word';
                          tempresult:=tempresult+'PINSRW '+mm(memory[2])+','+modrm(memory,prefix2,2,0,last)+inttohexs(memory[last],2);
                          inc(offset,last);
                        end;
                      end;

                $c5 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Extract Word';
                          tempresult:=tempresult+'PEXTRW '+r32(memory[2])+','+modrm(memory,prefix2,2,4,last)+','+inttohexs(memory[last],2);
                          inc(offset,3);
                        end
                        else
                        begin
                          description:='Extract Word';
                          tempresult:=tempresult+'PEXTRW '+r32(memory[2])+','+modrm(memory,prefix2,2,3,last)+','+inttohexs(memory[last],2);
                          inc(offset,3);
                        end;
                      end;

                $c6 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Shuffle Double-FP';
                          tempresult:=tempresult+'SHUFPD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last)+inttohexs(memory[last],2);
                          inc(offset,last);
                        end
                        else
                        begin
                          description:='Shuffle Single-FP';
                          tempresult:=tempresult+'SHUFPS '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last)+inttohexs(memory[last],2);
                          inc(offset,last);
                        end;
                      end;

                $c7 : begin
                        case getreg(memory[2]) of
                          1:  begin
                                description:='Compare and Exchange 8 Bytes';
                                tempresult:=tempresult+'CMPXCHG8B '+modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          6:  begin
                                if $66 in prefix2 then
                                begin
                                  description:='Copy VMCS data to VMCS region in memory';
                                  tempresult:='VMCLEAR '+modrm(memory,prefix2,2,0,last);
                                  tempresult:=copy(tempresult,1,length(tempresult)-1);
                                  inc(offset,last-1);
                                end
                                else
                                if $f3 in prefix2 then
                                begin
                                  description:='Enter VMX root operation';
                                  tempresult:='VMXON '+modrm(memory,prefix2,2,0,last);
                                  tempresult:=copy(tempresult,1,length(tempresult)-1);
                                  inc(offset,last-1);
                                end
                                else
                                begin
                                  description:='Loads ther current VMCS pointer from memory';
                                  tempresult:='VMPTRLD '+modrm(memory,prefix2,2,0,last);
                                  tempresult:=copy(tempresult,1,length(tempresult)-1);
                                  inc(offset,last-1);
                                end;
                              end;

                          7:  begin
                                description:='Stores the current VMCS pointer into memory';
                                tempresult:=tempresult+'VMPTRST '+modrm(memory,prefix2,2,0,last);
                                tempresult:=copy(tempresult,1,length(tempresult)-1);
                                inc(offset,last-1);
                              end;
                        end;

                      end;

                $c8..$cf : begin
                        //BSWAP
                        description:='Byte Swap';
                        tempresult:=tempresult+'BSWAP '+rd(memory[1]-$c8);
                        inc(offset);
                      end;

                $d1 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shift Right Logical';
                          tempresult:=tempresult+'PSRLW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Shift Right Logical';
                          tempresult:=tempresult+'PSRLW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $d2 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shift Right Logical';
                          tempresult:=tempresult+'PSRLD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Shift Right Logical';
                          tempresult:=tempresult+'PSRLD '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $d3 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shift Right Logical';
                          tempresult:=tempresult+'PSRLQ '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Shift Right Logical';
                          tempresult:=tempresult+'PSRLQ '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $d4 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Add Packed Quadwprd Integers';
                          tempresult:=tempresult+'PADDQ '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Add Packed Quadwprd Integers';
                          tempresult:=tempresult+'PADDQ '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;


                $d5 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Multiply Low';
                          tempresult:=tempresult+'PMULLW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Multiply Low';
                          tempresult:=tempresult+'PMULLW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $d6 : begin
                        if $f2 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          description:='Move low quadword from xmm to MMX technology register';
                          tempresult:=tempresult+'MOVDQ2Q '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          description:='Move low quadword from xmm to MMX technology register';
                          tempresult:=tempresult+'MOVQ2DQ '+xmm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='Move low quadword from xmm to MMX technology register';
                          tempresult:=tempresult+'MOVQ '+modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move quadword from MMX technology to xmm register';
                          tempresult:=tempresult+'MOVQ2Dq '+modrm(memory,prefix2,2,4,last)+mm(memory[2]);
                          inc(offset,last-1);
                        end;

                      end;


                $d7 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Move Byte Mask To Integer';
                          tempresult:=tempresult+'PMOVMSKB '+r32(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move Byte Mask To Integer';
                          tempresult:=tempresult+'PMOVMSKB '+r32(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $d8 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Subtract Unsigned with Saturation';
                          tempresult:=tempresult+'PSUBUSB '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Subtract Unsigned with Saturation';
                          tempresult:=tempresult+'PSUBUSB '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $d9 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Subtract Unsigned with Saturation';
                          tempresult:=tempresult+'PSUBUSW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Subtract Unsigned with Saturation';
                          tempresult:=tempresult+'PSUBUSW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $da : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Unsigned Integer Byte Minimum';
                          tempresult:=tempresult+'PMINUB '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Unsigned Integer Byte Minimum';
                          tempresult:=tempresult+'PMINUB '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $db : begin
                        if $66 in prefix2 then
                        begin
                          description:='Logical AND';
                          tempresult:=tempresult+'PAND '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Logical AND';
                          tempresult:=tempresult+'PAND '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $dc : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Add Unsigned with Saturation';
                          tempresult:=tempresult+'PADDUSB '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Add Unsigned with Saturation';
                          tempresult:=tempresult+'PADDUSB '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $dd : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Add Unsigned with Saturation';
                          tempresult:=tempresult+'PADDUSW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Add Unsigned with Saturation';
                          tempresult:=tempresult+'PADDUSW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $de : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Unsigned Integer Byte Maximum';
                          tempresult:=tempresult+'PMAXUB '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Unsigned Integer Byte Maximum';
                          tempresult:=tempresult+'PMAXUB '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $df : begin
                        if $66 in prefix2 then
                        begin
                          description:='Logical AND NOT';
                          tempresult:=tempresult+'PANDN '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Logical AND NOT';
                          tempresult:=tempresult+'PANDN '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $e0 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Average';
                          tempresult:=tempresult+'PAVGB '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Average';
                          tempresult:=tempresult+'PAVGB '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $e1 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shift Right Arithmetic';
                          tempresult:=tempresult+'PSRAW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Shift Right Arithmetic';
                          tempresult:=tempresult+'PSRAW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $e2 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shift Left Logical';
                          tempresult:=tempresult+'PSRAD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Shift Left Logical';
                          tempresult:=tempresult+'PSRAD '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $e3 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Average';
                          tempresult:=tempresult+'PAVGW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Average';
                          tempresult:=tempresult+'PAVGW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $e4 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Multiply High Unsigned';
                          tempresult:=tempresult+'PMULHUW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Multiply High Unsigned';
                          tempresult:=tempresult+'PMULHUW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $e5 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Multiply High';
                          tempresult:=tempresult+'PMULHW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Multiply High';
                          tempresult:=tempresult+'PMULHW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $e6 : begin
                        if $f2 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          description:='Convert two packed signed dwords from param2 to two packed DP-Floating point values in param1';
                          tempresult:=tempresult+'CVTPD2DQ '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-5);
                          description:='Convert two packed signed dwords from param2 to two packed DP-Floating point values in param1';
                          tempresult:=tempresult+'CVTDQ2PD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='Convert with truncation Packed Double-precision Floating-Point Values to Packed Doubleword Integers';
                            tempresult:=tempresult+'CVTTPD2DQ '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                            tempresult:=copy(tempresult,1,length(tempresult)-1);
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $e7 : begin
                        if $66 in prefix2 then
                        begin
                          tempresult:=tempresult+'MOVNTDQ '+MODRM(memory,prefix2,2,4,last)+xmm(memory[2]);
                          description:='Move Double quadword Using Non-Temporal Hint';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          tempresult:=tempresult+'MOVNTQ '+MODRM(memory,prefix2,2,3,last)+mm(memory[2]);
                          description:='Move 64 Bits Non Temporal';
                          inc(offset,last-1);
                        end;
                      end;

                $e8 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Subtract with Saturation';
                          tempresult:=tempresult+'PSUBSB '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Subtract with Saturation';
                          tempresult:=tempresult+'PSUBSB '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $e9 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Subtract with Saturation';
                          tempresult:=tempresult+'PSUBSW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Subtract with Saturation';
                          tempresult:=tempresult+'PSUBSW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $ea : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Signed Integer Word Minimum';
                          tempresult:=tempresult+'PMINSW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Signed Integer Word Minimum';
                          tempresult:=tempresult+'PMINSW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $eb : begin
                        if $66 in prefix2 then
                        begin
                          description:='Bitwise Logical OR';
                          tempresult:=tempresult+'POR '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Bitwise Logical OR';
                          tempresult:=tempresult+'POR '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $ec : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Add with Saturation';
                          tempresult:=tempresult+'PADDSB '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Add with Saturation';
                          tempresult:=tempresult+'PADDSB '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $ed : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Add with Saturation';
                          tempresult:=tempresult+'PADDSW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Add with Saturation';
                          tempresult:=tempresult+'PADDSW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $ee : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Signed Integer Word Maximum';
                          tempresult:=tempresult+'PMAXSW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Signed Integer Word Maximum';
                          tempresult:=tempresult+'PMAXSW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $ef : begin
                        if $66 in prefix2 then
                        begin
                          description:='Logical Exclusive OR';
                          tempresult:=tempresult+'PXOR '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Logical Exclusive OR';
                          tempresult:=tempresult+'PXOR '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $F1 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shift Left Logical';
                          tempresult:=tempresult+'PSLLW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Shift Left Logical';
                          tempresult:=tempresult+'PSLLW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $F2 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shift Left Logical';
                          tempresult:=tempresult+'PSLLD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Shift Left Logical';
                          tempresult:=tempresult+'PSLLD '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $F3 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shift Left Logical';
                          tempresult:=tempresult+'PSLLQ '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Shift Left Logical';
                          tempresult:=tempresult+'PSLLQ '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $F4 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Multiply Packed Unsigned Doubleword Integers';
                          tempresult:=tempresult+'PMULUDQ '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Multiply Packed Unsigned Doubleword Integers';
                          tempresult:=tempresult+'PMULUDQ '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;


                $F5 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Multiply and Add';
                          tempresult:=tempresult+'PMADDWD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Multiply and Add';
                          tempresult:=tempresult+'PMADDWD '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $F6 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Sum of Absolute Differences';
                          tempresult:=tempresult+'PSADBW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Sum of Absolute Differences';
                          tempresult:=tempresult+'PSADBW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $F7 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Store Selected Bytes of Double Quadword';
                          tempresult:=tempresult+'MASKMOVDQU '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Byte Mask Write';
                          tempresult:=tempresult+'MASKMOVQ '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $F8 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Subtract';
                          tempresult:=tempresult+'PSUBB '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Subtract';
                          tempresult:=tempresult+'PSUBB '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $F9 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Subtract';
                          tempresult:=tempresult+'PSUBW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Subtract';
                          tempresult:=tempresult+'PSUBW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $Fa : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Subtract';
                          tempresult:=tempresult+'PSUBD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Subtract';
                          tempresult:=tempresult+'PSUBD '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $Fb : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Subtract';
                          tempresult:=tempresult+'PSUBQ '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Subtract';
                          tempresult:=tempresult+'PSUBQ '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $fc : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Add';
                          tempresult:=tempresult+'PADDB '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Add';
                          tempresult:=tempresult+'PADDB '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $fd : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Add';
                          tempresult:=tempresult+'PADDW '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Add';
                          tempresult:=tempresult+'PADDW '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;

                $fe : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Add';
                          tempresult:=tempresult+'PADDD '+xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Add';
                          tempresult:=tempresult+'PADDD '+mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          tempresult:=copy(tempresult,1,length(tempresult)-1);
                          inc(offset,last-1);
                        end;
                      end;


                end;



            end;

//

//

      $10 : begin
              description:='Add with carry';
              tempresult:=tempresult+'ADC '+MODRM(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $11 : begin
              description:='Add with carry';
              if $66 in prefix2 then  tempresult:=tempresult+'ADC '+MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                                      tempresult:=tempresult+'ADC '+MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $12 : begin
              description:='Add with carry';
              tempresult:=tempresult+'ADC '+r8(memory[1])+','+MODRM(memory,prefix2,1,2,last,8);
              tempresult:=copy(tempresult,1,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $13 : begin
              description:='Add with carry';
              if $66 in prefix2 then tempresult:=tempresult+'ADC '+r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                                     tempresult:=tempresult+'ADC '+r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);
              tempresult:=copy(tempresult,1,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $14 : begin
              description:='Add with carry';
              tempresult:=tempresult+'ADC AL,'+inttohexs(memory[1],2);
              inc(offset);
            end;

      $15 : begin
              description:='Add with carry';
              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                tempresult:=tempresult+'ADC AX,'+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                tempresult:=tempresult+'ADC EAX,'+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;

      $16 : begin
              description:='Place SS on the stack';
              tempresult:=tempresult+'PUSH SS';
            end;

      $17 : begin
              description:='Remove SS from the stack';
              tempresult:=tempresult+'POP SS';
            end;

      $18 : begin
              description:='Integer Subtraction with Borrow';
              tempresult:=tempresult+'SBB '+MODRM(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $19 : begin
              description:='Integer Subtraction with Borrow';
              if $66 in prefix2 then  tempresult:=tempresult+'SBB '+MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                                      tempresult:=tempresult+'SBB '+MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $1a : begin
              description:='Integer Subtraction with Borrow';
              tempresult:=tempresult+'SBB '+r8(memory[1])+','+MODRM(memory,prefix2,1,2,last,8);
              tempresult:=copy(tempresult,0,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $1b : begin
              description:='Integer subtraction with Borrow';
              if $66 in prefix2 then tempresult:=tempresult+'SBB '+r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                                     tempresult:=tempresult+'SBB '+r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);
              tempresult:=copy(tempresult,0,length(tempresult)-1);

              inc(offset,last-1);
            end;

      $1c : begin
              description:='Integer Subtraction with Borrow';
              tempresult:=tempresult+'SBB AL,'+inttohexs(memory[1],2);
              inc(offset);
            end;

      $1d : begin
              description:='Integer Subtraction with Borrow';
              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                tempresult:=tempresult+'SBB AX,'+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                tempresult:=tempresult+'SBB EAX,'+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;

      $1e : begin
              description:='Place DS on the stack';
              tempresult:=tempresult+'PUSH DS';
            end;

      $1f : begin
              description:='Remove DS from the stack';
              tempresult:=tempresult+'POP DS';
            end;

      $20 : begin
              description:='Logical AND';
              tempresult:=tempresult+'AND '+MODRM(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $21 : begin
              description:='Logical AND';
              if $66 in prefix2 then tempresult:=tempresult+'AND '+MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                                     tempresult:=tempresult+'AND '+MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $22 : begin
              description:='Logical AND';
              tempresult:=tempresult+'AND '+r8(memory[1])+','+MODRM(memory,prefix2,1,2,last);
              tempresult:=copy(tempresult,0,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $23 : begin
              description:='Logical AND';
              if $66 in prefix2 then tempresult:=tempresult+'AND '+r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                                     tempresult:=tempresult+'AND '+r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);
              tempresult:=copy(tempresult,0,length(tempresult)-1);
              inc(offset,last-1);
            end;


      $24 : begin
              description:='Logical AND';
              tempresult:=tempresult+'AND AL,'+inttohexs(memory[1],2);
              inc(offset);
            end;

      $25 : begin
              description:='Logical AND';
              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                tempresult:=tempresult+'AND AX,'+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                tempresult:=tempresult+'AND EAX,'+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;

      $27 : begin
              description:='Decimal Adjust AL after Addition';
              tempresult:=tempresult+'DAA';
            end;

      $28 : begin
              description:='Subtract';
              tempresult:=tempresult+'SUB '+MODRM(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $29 : begin
              description:='Subtract';
              if $66 in prefix2 then  tempresult:=tempresult+'SUB '+MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                                      tempresult:=tempresult+'SUB '+MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $2a : begin
              description:='Subtract';
              tempresult:=tempresult+'SUB '+r8(memory[1])+','+MODRM(memory,prefix2,1,2,last);
              tempresult:=copy(tempresult,0,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $2b : begin
              description:='Subtract';
              if $66 in prefix2 then tempresult:=tempresult+'SUB '+r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                                     tempresult:=tempresult+'SUB '+r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);
              tempresult:=copy(tempresult,0,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $2c : begin
              description:='Subtract';
              tempresult:=tempresult+'SUB AL,'+inttohexs(memory[1],2);
              inc(offset);
            end;

      $2d : begin
              description:='Subtract';
              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                tempresult:=tempresult+'SUB AX,'+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                tempresult:=tempresult+'SUB EAX,'+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;


      $2f : begin
              description:='Decimal Adjust AL after Subtraction';
              tempresult:=tempresult+'DAS';
            end;

      $30 : begin
              description:='Logical Exclusive OR';
              tempresult:=tempresult+'XOR '+MODRM(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $31 : begin
              description:='Logical Exclusive OR';
              if $66 in prefix2 then  tempresult:=tempresult+'XOR '+MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                                      tempresult:=tempresult+'XOR '+MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $32 : begin
              description:='Logical Exclusive OR';
              tempresult:=tempresult+'XOR '+r8(memory[1])+','+MODRM(memory,prefix2,1,2,last);
              tempresult:=copy(tempresult,0,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $33 : begin
              description:='Logical Exclusive OR';
              if $66 in prefix2 then tempresult:=tempresult+'XOR '+r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                                     tempresult:=tempresult+'XOR '+r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);
              tempresult:=copy(tempresult,1,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $34 : begin
              description:='Logical Exclusive OR';
              tempresult:=tempresult+'XOR AL,'+inttohexs(memory[1],2);
              inc(offset);
            end;

      $35 : begin
              description:='Logical Exclusive OR';
              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                tempresult:=tempresult+'XOR AX,'+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                tempresult:=tempresult+'XOR EAX,'+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;


      $37 : begin  //AAA
              tempresult:=tempresult+'AAA';
              description:='ASCII adjust AL after addition'
            end;

//---------
      $38 : begin//CMP
              description:='Compare Two Operands';
              tempresult:=tempresult+'CMP '+MODRM(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $39 : begin
              description:='Compare Two Operands';
              if $66 in prefix2 then  tempresult:=tempresult+'CMP '+MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                                      tempresult:=tempresult+'CMP '+MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $3a : begin
              description:='Compare Two Operands';
              tempresult:=tempresult+'CMP '+r8(memory[1])+','+MODRM(memory,prefix2,1,2,last);
              tempresult:=copy(tempresult,0,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $3b : begin
              description:='Compare Two Operands';
              if $66 in prefix2 then tempresult:=tempresult+'CMP '+r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                                     tempresult:=tempresult+'CMP '+r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);
              tempresult:=copy(tempresult,0,length(tempresult)-1);
              inc(offset,last-1);
            end;

//---------

      $3c : begin
              description:='Compare Two Operands';
              tempresult:=tempresult+'CMP AL,'+inttohexs(memory[1],2);
              inc(offset);
            end;

      $3d : begin
              description:='Compare Two Operands';
              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                tempresult:=tempresult+'CMP AX,'+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                tempresult:=tempresult+'CMP EAX,'+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;

      $3F : begin  //AAS
              tempresult:=tempresult+'AAS';
              description:='ASCII Adjust AL After Subtraction';
            end;

      $40..$47 :
            begin
              description:='Increment by 1';
              if $66 in prefix2 then tempresult:=tempresult+'INC '+rd16(memory[0]-$40) else
                                     tempresult:=tempresult+'INC '+rd(memory[0]-$40);
            end;

      $48..$4f :
            begin
              description:='Decrement by 1';
              if $66 in prefix2 then tempresult:=tempresult+'DEC '+rd16(memory[0]-$48) else
                                     tempresult:=tempresult+'DEC '+rd(memory[0]-$48);
            end;

      $50..$57 :
            begin
              description:='Push Word or Doubleword Onto the Stack';
              if $66 in prefix2 then tempresult:=tempresult+'PUSH '+rd16(memory[0]-$50) else
                                     tempresult:=tempresult+'PUSH '+rd(memory[0]-$50);
            end;

      $58..$5f :
            begin
              description:='Pop a Value from the Stack';
              if $66 in prefix2 then tempresult:=tempresult+'POP '+rd16(memory[0]-$58) else
                                     tempresult:=tempresult+'POP '+rd(memory[0]-$58);
            end;

      $60 : begin
              description:='Push All General-Purpose Registers';
              if $66 in prefix2 then tempresult:=tempresult+'PUSHA' else
                                    tempresult:=tempresult+'PUSHAD';
            end;

      $61 : begin
              description:='Pop All General-Purpose Registers';
              if $66 in prefix2 then tempresult:=tempresult+'POPA' else
                                    tempresult:=tempresult+'POPAD';
            end;

      $62 : begin
              //BOUND
              description:='Check Array Index Against Bounds';
              if $66 in prefix2 then tempresult:=tempresult+'BOUND '+r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                                     tempresult:=tempresult+'BOUND '+r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);

              tempresult:=copy(tempresult,0,length(tempresult)-1);

              inc(offset,last-1);

            end;

      $63 : begin
              //ARPL
              tempresult:=tempresult+'ARPL '+MODRM(memory,prefix2,1,1,last)+r16(memory[1]);
              inc(offset,last-1);
              description:='Adjust RPL Field of Segment Selector';
            end;

      $68 : begin
              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                tempresult:=tempresult+'PUSH '+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                tempresult:=tempresult+'PUSH '+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
              description:='Push Word or Doubleword Onto the Stack';
            end;

      $69 : begin
              description:='Signed Multiply';
              if $66 in prefix2 then
              begin
                tempresult:=tempresult+'IMUL '+r16(memory[1])+','+MODRM(memory,prefix2,1,1,last);
                wordptr:=@memory[last];
                tempresult:=tempresult+inttohexs(wordptr^,4);
                inc(offset,last-1+2);
              end
              else
              begin
                tempresult:=tempresult+'IMUL '+r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);
                dwordptr:=@memory[last];
                tempresult:=tempresult+inttohexs(dwordptr^,8);
                inc(offset,last-1+4);
              end;
            end;

      $6a : begin
              tempresult:=tempresult+'PUSH '+inttohexs(memory[1],2);
              inc(offset);
              description:='Push Byte Onto the Stack';
            end;

      $6b : begin

              description:='Signed Multiply';
              if $66 in prefix2 then tempresult:=tempresult+'IMUL '+r16(memory[1])+','+MODRM(memory,prefix2,1,1,last)+inttohexs(memory[last],2) else
                                     tempresult:=tempresult+'IMUL '+r32(memory[1])+','+MODRM(memory,prefix2,1,0,last)+inttohexs(memory[last],2);
              inc(offset,last-1+1);
            end;

      $6c : begin
              //m8, DX
              description:='Input from Port to String';
              tempresult:=tempresult+'INSB';
            end;

      $6D : begin
              //m8, DX
              description:='Input from Port to String';
              if $66 in prefix2 then tempresult:=tempresult+'INSW' else
                                     tempresult:=tempresult+'INSD';
            end;

      $6e : begin
              //m8, DX
              description:='Output String to Port';
              tempresult:=tempresult+'OUTSB';
            end;

      $6f : begin
              //m8, DX
              description:='Output String to Port';
              if $66 in prefix2 then tempresult:=tempresult+'OUTSW' else
                                     tempresult:=tempresult+'OUTSD';
            end;


      $70 : begin
              description:='Jump short if overflow';
              tempresult:=tempresult+'JO ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+shortint(memory[1])),8);
            end;

      $71 : begin
              description:='Jump short if not overflow';
              tempresult:=tempresult+'JNO ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+shortint(memory[1])),8);
            end;

      $72 : begin
              description:='Jump short if below/carry';
              tempresult:=tempresult+'JB ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+shortint(memory[1])),8);
            end;

      $73 : begin
              description:='Jump short if above or equal';
              tempresult:=tempresult+'JAE ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+shortint(memory[1])),8);
            end;

      $74 : begin
              description:='Jump short if equal';
              tempresult:=tempresult+'JE ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+shortint(memory[1])),8);
            end;

      $75 : begin
              description:='Jump short if not equal';
              tempresult:=tempresult+'JNE ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+shortint(memory[1])),8);
            end;

      $76 : begin
              description:='Jump short if not Above';
              tempresult:=tempresult+'JNA ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+shortint(memory[1])),8);
            end;

      $77 : begin
              description:='Jump short if above';
              tempresult:=tempresult+'JA ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+shortint(memory[1])),8);
            end;

      $78 : begin
              description:='Jump short if sign';
              tempresult:=tempresult+'JS ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+shortint(memory[1])),8);
            end;

      $79 : begin
              description:='Jump short if not sign';
              tempresult:=tempresult+'JNS ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+shortint(memory[1])),8);
            end;

      $7a : begin
              description:='Jump short if parity';
              tempresult:=tempresult+'JP ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+shortint(memory[1])),8);
            end;

      $7b : begin
              description:='Jump short if not parity';
              tempresult:=tempresult+'JNP ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+shortint(memory[1])),8);
            end;

      $7c : begin
              description:='Jump short if not greater or equal';
              tempresult:=tempresult+'JNGE ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+shortint(memory[1])),8);
            end;

      $7d : begin
              description:='Jump short if not less (greater or equal)';
              tempresult:=tempresult+'JNL ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+shortint(memory[1])),8);
            end;

      $7e : begin
              description:='Jump short if less or equal';
              tempresult:=tempresult+'JLE ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+shortint(memory[1])),8);
            end;

      $7f : begin
              description:='Jump short if greater';
              tempresult:=tempresult+'JG ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+shortint(memory[1])),8);
            end;

      $80 : begin
              case getreg(memory[1]) of
                0:  begin
                      //ADD
                      tempresult:=tempresult+'ADD '+modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='Add x to y';
                    end;

                1:  begin
                      //ADC
                      tempresult:=tempresult+'OR '+modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='Logical Inclusive Or';
                    end;


                2:  begin
                      //ADC
                      tempresult:=tempresult+'ADC '+modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='Add with Carry';
                    end;

                3:  begin
                      //sbb
                      tempresult:=tempresult+'SBB '+modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='Integer Subtraction with Borrow';
                    end;

                4:  begin
                      //AND
                      tempresult:=tempresult+'AND '+modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='Logical AND';
                    end;

                5:  begin
                      tempresult:=tempresult+'SUB '+modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='Subtract';
                    end;

                6:  begin
                      tempresult:=tempresult+'XOR '+modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='Logical Exclusive OR';
                    end;

                7:  begin
                      //AND
                      tempresult:=tempresult+'CMP '+modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='Compare Two Operands';
                    end;

              end;
            end;

      $81 : begin
              case getreg(memory[1]) of
                0:  begin
                      //ADD
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'ADD '+modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        tempresult:=tempresult+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        tempresult:=tempresult+'ADD '+modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        tempresult:=tempresult+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;

//                      offset:=offset+last;
                      description:='Add x to y';
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'OR '+modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        tempresult:=tempresult+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        tempresult:=tempresult+'OR '+modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        tempresult:=tempresult+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='Logical Inclusive OR';
                    end;

                2:  begin
                      //ADC
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'ADC '+modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        tempresult:=tempresult+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        tempresult:=tempresult+'ADC '+modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        tempresult:=tempresult+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='Add with Carry';
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'SBB '+modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        tempresult:=tempresult+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        tempresult:=tempresult+'SBB '+modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        tempresult:=tempresult+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='Integer Subtraction with Borrow';
                    end;


                4:  begin
                      //AND
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'AND '+modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        tempresult:=tempresult+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        tempresult:=tempresult+'AND '+modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        tempresult:=tempresult+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='Logical AND';
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'SUB '+modrm(memory,prefix2,1,1,last);
                        wordptr:=@memory[last];
                        tempresult:=tempresult+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        tempresult:=tempresult+'SUB '+modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        tempresult:=tempresult+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='Subtract';
                    end;

                6:  begin
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'XOR '+modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        tempresult:=tempresult+inttohexs(wordptr^,4);
                        inc(offset,last-1);
                      end else
                      begin
                        tempresult:=tempresult+'XOR '+modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        tempresult:=tempresult+inttohexs(dwordptr^,8);
                        inc(offset,last-1+2);
                      end;

                      offset:=offset+last;
                      description:='Logical Exclusive OR';
                    end;

                7:  begin
                      //CMP
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'CMP '+modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        tempresult:=tempresult+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        tempresult:=tempresult+'CMP '+modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        tempresult:=tempresult+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;

                      description:='Compare Two Operands';
                    end;


              end;
            end;

      $83 : begin
              case getreg(memory[1]) of
                0:  begin
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'ADD '+modrm(memory,prefix2,1,1,last,16)+inttohexs(memory[last],2);
                      end else
                      begin
                        tempresult:=tempresult+'ADD '+modrm(memory,prefix2,1,0,last,32)+inttohexs(memory[last],2);
                      end;

                      inc(offset,last);
                      description:='Add (Sign Extended)';
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'OR '+modrm(memory,prefix2,1,1,last,16)+inttohexs(memory[last],2);
                      end else
                      begin
                        tempresult:=tempresult+'OR '+modrm(memory,prefix2,1,0,last,32)+inttohexs(memory[last],2);
                      end;

                      inc(offset,last);
                      description:='Add (Sign Extended)';
                    end;


                2:  begin
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'ADC '+modrm(memory,prefix2,1,1,last,16)+inttohexs(memory[last],2);
                      end else
                      begin
                        tempresult:=tempresult+'ADC '+modrm(memory,prefix2,1,0,last,32)+inttohexs(memory[last],2);

                      end;

                      inc(offset,last);
                      description:='Add with Carry (Sign Extended)';
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'SBB '+modrm(memory,prefix2,1,1,last,16)+inttohexs(memory[last],2);
                      end else
                      begin
                        tempresult:=tempresult+'SBB '+modrm(memory,prefix2,1,0,last,32)+inttohexs(memory[last],2);

                      end;

                      inc(offset,last);
                      description:='Integer Subtraction with Borrow (Sign Extended)';
                    end;

                4:  begin
                      //AND
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'AND '+modrm(memory,prefix2,1,1,last,16)+inttohexs(memory[last],2);
                      end else
                      begin
                        tempresult:=tempresult+'AND '+modrm(memory,prefix2,1,0,last,32)+inttohexs(memory[last],2);

                      end;

                      offset:=offset+last;
                      description:='Logical AND (Sign Extended)';
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'SUB '+modrm(memory,prefix2,1,1,last,16)+inttohexs(memory[last],2);
                      end else
                      begin
                        tempresult:=tempresult+'SUB '+modrm(memory,prefix2,1,0,last,32)+inttohexs(memory[last],2);

                      end;

                      offset:=offset+last;
                      description:='Subtract';
                    end;

                6:  begin
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'XOR '+modrm(memory,prefix2,1,1,last,16)+inttohexs(memory[last],2);
                      end else
                      begin
                        tempresult:=tempresult+'XOR '+modrm(memory,prefix2,1,0,last,32)+inttohexs(memory[last],2);

                      end;

                      offset:=offset+last;
                      description:='Logical Exclusive OR';
                    end;

                7:  begin
                      //CMP
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'CMP '+modrm(memory,prefix2,1,1,last,16)+inttohexs(memory[last],2);
                      end else
                      begin
                        tempresult:=tempresult+'CMP '+modrm(memory,prefix2,1,0,last,32)+inttohexs(memory[last],2);

                      end;

                      offset:=offset+last;
                      description:='Compare Two Operands';
                    end;


              end;
            end;

      $84 : begin
              description:='Logical Compare';
              tempresult:=tempresult+'TEST '+r8(memory[1])+','+MODRM(memory,prefix2,1,2,last);
              tempresult:=copy(tempresult,0,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $85 : begin
              description:='Logical Compare';
              if $66 in prefix2 then tempresult:=tempresult+'TEST '+r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                                     tempresult:=tempresult+'TEST '+r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);
              tempresult:=copy(tempresult,0,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $86 : begin
              description:='Exchage Memory with Register';
              tempresult:=tempresult+'XCHG '+modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $87 : begin
              description:='Exchage Memory with Register';
              if $66 in prefix2 then tempresult:=tempresult+'XCHG '+MODRM(memory,prefix2,1,1,last)+r16(memory[1])
                                else tempresult:=tempresult+'XCHG '+MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $88 : begin
              description:='Copy memory';
              tempresult:=tempresult+'MOV '+modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $89 : begin
              description:='Copy memory';
              if $66 in prefix2 then tempresult:=tempresult+'MOV '+MODRM(memory,prefix2,1,1,last)+r32(memory[1])
                                else tempresult:=tempresult+'MOV '+MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $8A : begin
              description:='Copy memory';
              tempresult:=tempresult+'MOV '+r8(memory[1])+','+modrm(memory,prefix2,1,2,last);
              tempresult:=copy(tempresult,1,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $8B : begin
              description:='Copy memory';
              if $66 in prefix2 then tempresult:=tempresult+'MOV '+r16(memory[1])+','+MODRM(memory,prefix2,1,1,last)
                                else tempresult:=tempresult+'MOV '+r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);
              tempresult:=copy(tempresult,1,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $8c : begin
              description:='Copy memory';
              tempresult:=tempresult+'MOV '+modrm(memory,prefix2,1,1,last)+sreg(memory[1]);
              inc(offset,last-1);
            end;

      $8D : begin
              description:='Load Effective Address';
              if $66 in prefix2 then tempresult:=tempresult+'LEA '+r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                                     tempresult:=tempresult+'LEA '+r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);

              tempresult:=copy(tempresult,1,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $8e : begin
              description:='Copy memory';
              tempresult:=tempresult+'MOV '+sreg(memory[1])+','+modrm(memory,prefix2,1,1,last);
              tempresult:=copy(tempresult,1,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $8f : begin
              case getreg(memory[1]) of
               0:  begin
                     description:='Pop a Value from the Stack';
                     if $66 in prefix2 then tempresult:='POP '+modrm(memory,prefix2,1,1,last,16) else
                                            tempresult:='POP '+modrm(memory,prefix2,1,0,last);
                     tempresult:=copy(tempresult,1,length(tempresult)-1);
                     inc(offset,last-1);
                   end;

               else
                   begin
                     description:='Undefined by the intel specification';
                     tempresult:='DB 8F';
                   end;
              end;
            end;


      $90 : begin
              description:='No Operation';
              tempresult:='NOP';
            end;

      $91..$97:
            begin
              description:='Exchagne Register with Register';
              if $66 in prefix2 then
                tempresult:=tempresult+'XCHG AX,'+rd16(memory[0]-$90)
                else
                tempresult:=tempresult+'XCHG EAX,'+rd(memory[0]-$90);
            end;


      $98 : begin
              //CBW/CWDE
              if $66 in prefix2 then
              begin
                tempresult:=tempresult+'CBW';
                description:='Convert Byte to Word';
              end else
              begin
                tempresult:=tempresult+'CWDE';
                description:='Convert Word to Doubleword';
              end;
            end;

      $99 : begin
              if $66 in prefix2 then
              begin
                description:='Convert Word to Doubleword';
                tempresult:=tempresult+'CWD';
              end
              else
              begin
                description:='Convert Doubleword to Quadword';
                tempresult:=tempresult+'CDQ';
              end;
            end;

      $9A : begin
              description:='Call Procedure';
              wordptr:=@memory[5];
              tempresult:=tempresult+'CALL '+inttohexs(wordptr^,4)+':';
              dwordptr:=@memory[1];
              tempresult:=tempresult+inttohexs(dwordptr^,8);
              inc(offset,6);
            end;

      $9b : begin    //ehrm, wait???
              case memory[1] of

               $d9 : begin
                       case getreg(memory[2]) of
                         6:  begin
                                 description:='Store FPU Environment';
                                 tempresult:=tempresult+'WAIT:FSTENV '+modrm(memory,prefix2,2,0,last);
                                 inc(offset,last-1);
                             end;


                         7:  begin
                                 description:='Store Control Word';
                                 tempresult:=tempresult+'WAIT:FSTCW '+modrm(memory,prefix2,2,0,last);
                                 inc(offset,last-1);
                             end;
                       end;
                     end;

               $db : begin
                       case memory[2] of
                         $e2 : begin
                                 description:='Clear Exceptions';
                                 tempresult:=tempresult+'WAIT:FCLEX';
                                 inc(offset,2);
                               end;

                         $e3 : begin
                                 description:='Initialize Floaring-Point Unit';
                                 tempresult:=tempresult+'WAIT:FINIT';
                                 inc(offset,2);
                               end;
                       end;
                     end;

               $dd : begin
                       case getreg(memory[2]) of
                         6:  begin
                                 description:='Store FPU State';
                                 tempresult:=tempresult+'WAIT:FSAVE '+modrm(memory,prefix2,2,0,last);
                                 inc(offset,last-1);
                             end;

                         7:  begin
                                 description:='Store Status Word';
                                 tempresult:=tempresult+'WAIT:FSTSW '+modrm(memory,prefix2,2,0,last);
                                 inc(offset,last-1);
                             end;
                       end;
                     end;

               $df : begin
                       case memory[2] of
                         $e0 : begin
                                 description:='Store Status Word';
                                 tempresult:=Tempresult+'WAIT:FSTSW AX';
                                 inc(offset,2);
                               end;
                       end;
                     end;

               else  begin
                       description:='Wait';
                       tempresult:=Tempresult+'WAIT';
                     end;

              end;

            end;

      $9c : begin
              description:='Push EFLAGS Register onto the Stack';
              if $66 in prefix2 then tempresult:=tempresult+'PUSHF' else
                                     tempresult:=tempresult+'PUSHFD';
            end;

      $9d : begin
              description:='Pop Stack into EFLAGS Register';
              if $66 in prefix2 then tempresult:=tempresult+'POPF' else
                                     tempresult:=tempresult+'POPFD';
            end;

      $9e : begin
              description:='Store AH into Flags';
              tempresult:=tempresult+'SAHF';
            end;

      $9f : begin
              description:='Load Status Flag into AH Register';
              tempresult:=tempresult+'LAHF';
            end;

      $a0 : begin
              description:='Copy memory';
              dwordptr:=@memory[1];
              tempresult:=tempresult+'MOV AX,'+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+']';
              inc(offset,4);
            end;

      $a1 : begin
              description:='Copy memory';
              dwordptr:=@memory[1];
              if $66 in prefix2 then tempresult:=tempresult+'MOV AX,'+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+']' else
                                     tempresult:=tempresult+'MOV EAX,'+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+']';
              inc(offset,4);
            end;

      $a2 : begin
              description:='Copy memory';
              dwordptr:=@memory[1];
              tempresult:=tempresult+'MOV byte ptr '+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+'],AL';
              inc(offset,4);
            end;

      $a3 : begin
              description:='Copy memory';
              dwordptr:=@memory[1];
              tempresult:=tempresult+'MOV '+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+'],';
              if $66 in prefix2 then tempresult:=tempresult+'AX' else
                                     tempresult:=tempresult+'EAX';
              inc(offset,4);
            end;

      $a4 : begin
              description:='Move Data from String to String';
              tempresult:=tempresult+'MOVSB';
            end;

      $a5 : begin
              description:='Move Data from String to String';
              if $66 in prefix2 then tempresult:=tempresult+'MOVSW' else
                                     tempresult:=tempresult+'MOVSD';
            end;

      $a6 : begin
              description:='Compare String Operands';
              tempresult:=tempresult+'CMPSB';
            end;

      $a7 : begin
              description:='Compare String Operands';
              if $66 in prefix2 then tempresult:=tempresult+'CMPSW' else
                                     tempresult:=tempresult+'CMPSD';
            end;

      $a8 : begin
              description:='Logical Compare';
              tempresult:=tempresult+'TEST AL,'+inttohexs(memory[1],2);
              inc(offset);
            end;

      $a9 : begin
              description:='Logical Compare';
              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                tempresult:=tempresult+'TEST AX,'+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                tempresult:=tempresult+'TEST EAX,'+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;

      $aa : begin
              description:='Store String';
              tempresult:=tempresult+'STOSB';
            end;

      $ab : begin
              description:='Store String';
              if $66 in prefix2 then tempresult:=tempresult+'STOSW' else
                                     tempresult:=tempresult+'STOSD';
            end;

      $ac : begin
              description:='Compare String Operands';
              tempresult:=tempresult+'LODSB';
            end;

      $ad : begin
              description:='Compare String Operands';
              if $66 in prefix2 then tempresult:=tempresult+'LODSW' else
                                     tempresult:=tempresult+'LODSD';
            end;

      $ae : begin
              description:='Compare AL with byte at ES:EDI and set status flag';
              tempresult:=tempresult+'SCASB';
            end;

      $af : begin
              description:='Scan String';
              if $66 in prefix2 then tempresult:=tempresult+'SCASW' else
                                     tempresult:=tempresult+'SCASD';
            end;

      $b0..$b7:
            begin
              description:='Copy Memory';
              tempresult:=tempresult+'MOV '+rd8(memory[0]-$b0)+','+inttohexs(memory[1],2);
              inc(offset);
            end;

      $b8..$bf:
            begin
              description:='Copy Memory';
              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                tempresult:=tempresult+'MOV '+rd16(memory[0]-$b8)+','+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                tempresult:=tempresult+'MOV '+rd(memory[0]-$b8)+','+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;

      $c0 : begin
              case getreg(memory[1]) of
                0:  begin
                      tempresult:=tempresult+'ROL '+modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      description:='Rotate eight bits left '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                1:  begin
                      tempresult:=tempresult+'ROR '+modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      description:='Rotate eight bits right '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                2:  begin
                      tempresult:=tempresult+'RCL '+modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      description:='Rotate nine bits left '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                3:  begin
                      tempresult:=tempresult+'RCR '+modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      description:='Rotate nine bits right '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                4:  begin
                      tempresult:=tempresult+'SHL '+modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      description:='Multiply by 2, '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                5:  begin
                      tempresult:=tempresult+'SHR '+modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      description:='Unsigned divide by 2, '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

{Not in intel spec}
                6:  begin
                      tempresult:=tempresult+'ROL '+modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      description:='Rotate eight bits left '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;
{^^^^^^^^^^^^^^^^^^}

                7:  begin
                      tempresult:=tempresult+'SAR '+modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      description:='Signed divide by 2, '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

              end;
            end;

      $c1 : begin
              case getreg(memory[1]) of
                0:  begin
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'ROL '+modrm(memory,prefix2,1,1,last,16)+inttohexs(memory[last],2);
                        description:='Rotate 16 bits left '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        tempresult:=tempresult+'ROL '+modrm(memory,prefix2,1,0,last)+inttohexs(memory[last],2);
                        description:='Rotate 32 bits left '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'ROR '+modrm(memory,prefix2,1,1,last,16)+inttohexs(memory[last],2);
                        description:='Rotate 16 bits right '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        tempresult:=tempresult+'ROR '+modrm(memory,prefix2,1,0,last)+inttohexs(memory[last],2);
                        description:='Rotate 32 bits right '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                2:  begin
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'RCL '+modrm(memory,prefix2,1,1,last,16)+inttohexs(memory[last],2);
                        description:='Rotate 17 bits left '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        tempresult:=tempresult+'RCL '+modrm(memory,prefix2,1,0,last)+inttohexs(memory[last],2);
                        description:='Rotate 33 bits left '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'RCR '+modrm(memory,prefix2,1,1,last,16)+inttohexs(memory[last],2);
                        description:='Rotate 17 bits right '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        tempresult:=tempresult+'RCR '+modrm(memory,prefix2,1,0,last)+inttohexs(memory[last],2);
                        description:='Rotate 33 bits right '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                4:  begin
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'SHL '+modrm(memory,prefix2,1,1,last,16)+inttohexs(memory[last],2);
                        description:='Multiply by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        tempresult:=tempresult+'SHL '+modrm(memory,prefix2,1,0,last)+inttohexs(memory[last],2);
                        description:='Multiply by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'SHR '+modrm(memory,prefix2,1,1,last,16)+inttohexs(memory[last],2);
                        description:='Unsigned divide by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        tempresult:=tempresult+'SHR '+modrm(memory,prefix2,1,0,last)+inttohexs(memory[last],2);
                        description:='Unsigned divide by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                7:  begin
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'SAR '+modrm(memory,prefix2,1,2,last,16)+inttohexs(memory[last],2);
                        description:='Signed divide by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        tempresult:=tempresult+'SAR '+modrm(memory,prefix2,1,2,last)+inttohexs(memory[last],2);
                        description:='Signed divide by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

              end;
            end;

      $c2 : begin
              description:='Near return to calling procedure and pop 2 bytes from stack';
              wordptr:=@memory[1];
              tempresult:=tempresult+'RET '+inttohexs(wordptr^,4);
              inc(offset,2);
            end;

      $c3 : begin
              description:='Near return to calling procedure';
              tempresult:=tempresult+'RET';
            end;

      $c4 : begin
              description:='Load Far Pointer';
              if $66 in prefix2 then tempresult:=tempresult+'LES '+r16(memory[1])+','+modrm(memory,prefix2,1,1,last) else
                                     tempresult:=tempresult+'LES '+r32(memory[1])+','+modrm(memory,prefix2,1,0,last);

              tempresult:=copy(tempresult,1,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $c5 : begin
              description:='Load Far Pointer';
              if $66 in prefix2 then tempresult:=tempresult+'LDS '+r16(memory[1])+','+modrm(memory,prefix2,1,1,last) else
                                     tempresult:=tempresult+'LDS '+r32(memory[1])+','+modrm(memory,prefix2,1,0,last);

              tempresult:=copy(tempresult,1,length(tempresult)-1);
              inc(offset,last-1);
            end;

      $c6 : begin
              case getreg(memory[1]) of
              0 : begin
                    description:='Copy Memory';
                    tempresult:=tempresult+'MOV '+modrm(memory,prefix2,1,2,last,8)+''+inttohexs(memory[last],2);
                    inc(offset,last);
                  end;

              else begin
                     description:='Not defined by the intel documentation';
                     tempresult:=tempresult+'DB C6';
                   end;
              end;
            end;

      $c7 : begin
              case getreg(memory[1]) of
              0 : begin
                    description:='Copy Memory';
                    if $66 in prefix2 then
                    begin
                      wordptr:=@memory[1];
                      tempresult:=tempresult+'MOV '+modrm(memory,prefix2,1,1,last,16);

                      wordptr:=@memory[last];
                      tempresult:=tempresult+inttohexs(wordptr^,4);
                      inc(offset,last+1);
                    end
                    else
                    begin
                      dwordptr:=@memory[1];
                      tempresult:=tempresult+'MOV '+modrm(memory,prefix2,1,0,last);

                      dwordptr:=@memory[last];
                      tempresult:=tempresult+inttohexs(dwordptr^,8);
                      inc(offset,last+3);
                    end;
                  end;

             else begin
                    description:='Not defined by the intel documentation';
                    tempresult:=tempresult+'DB C7';
                  end;

              end;
            end;

      $c8 : begin
              description:='Make Stack Frame for Procedure Parameters';
              wordptr:=@memory[1];
              tempresult:=tempresult+'ENTER '+inttohexs(wordptr^,4)+','+inttohexs(memory[3],2);
              inc(offset,3);
            end;

      $c9 : begin
              description:='High Level Procedure Exit';
              tempresult:=tempresult+'LEAVE';
            end;

      $ca : begin
              description:='Far return to calling procedure and pop 2 bytes from stack';
              wordptr:=@memory[1];
              tempresult:=tempresult+'RET '+inttohexs(wordptr^,4);
              inc(offset,2);
            end;

      $cb : begin
              description:='Far return to calling procedure';
              tempresult:=tempresult+'RET';
            end;

      $cc : begin
              //should not be shown if its being debugged using int 3'
              description:='Call to Interrupt Procedure-3:Trap to debugger';
              tempresult:=tempresult+'INT 3';
            end;

      $cd : begin
              description:='Call to Interrupt Procedure';
              tempresult:=tempresult+'INT '+inttohexs(memory[1],2);
              inc(offset);
            end;

      $ce : begin
              description:='Call to Interrupt Procedure-4:If overflow flag=1';
              tempresult:=tempresult+'INTO';
            end;

      $cf : begin
              description:='Interrupt Return';
              if $66 in prefix2 then tempresult:=tempresult+'IRET' else
                                     tempresult:=tempresult+'IRETD';
            end;

      $d0 : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='Rotate eight bits left once';
                      tempresult:=tempresult+'ROL '+modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='Rotate eight bits right once';
                      tempresult:=tempresult+'ROR '+modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;


                2:  begin
                      description:='Rotate nine bits left once';
                      tempresult:=tempresult+'RCL '+modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='Rotate nine bits right once';
                      tempresult:=tempresult+'RCR '+modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='Multiply by 2, once';
                      tempresult:=tempresult+'SHL '+modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='Unsigned devide by 2, once';
                      tempresult:=tempresult+'SHR '+modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='Not defined by the intel documentation';
                      tempresult:='DB D0'+inttohexs(memory[1],2);
                    end;

                7:  begin
                      description:='Signed devide by 2, once';
                      tempresult:=tempresult+'SAR '+modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

              end;
            end;

      $d1 : begin
              case getreg(memory[1]) of
                0:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Rotate 16 bits left once';
                        tempresult:=tempresult+'ROL '+modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Rotate 32 bits left once';
                        tempresult:=tempresult+'ROL '+modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Rotate 16 bits right once';
                        tempresult:=tempresult+'ROR '+modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Rotate 32 bits right once';
                        tempresult:=tempresult+'ROR '+modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                2:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Rotate 17 bits left once';
                        tempresult:=tempresult+'RCL '+modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Rotate 33 bits left once';
                        tempresult:=tempresult+'RCL '+modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Rotate 17 bits right once';
                        tempresult:=tempresult+'RCR '+modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Rotate 33 bits right once';
                        tempresult:=tempresult+'RCR '+modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                4:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Multiply by 2, Once';
                        tempresult:=tempresult+'SHL '+modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Multiply by 2, once';
                        tempresult:=tempresult+'SHL '+modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Unsigned divide by 2, Once';
                        tempresult:=tempresult+'SHR '+modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Unsigned divide by 2, once';
                        tempresult:=tempresult+'SHR '+modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                6:  begin
                      description:='Undefined by the intel documentation';
                      tempresult:='DB D1';
                    end;

                7:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Signed divide by 2, Once';
                        tempresult:=tempresult+'SAR '+modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Signed divide by 2, once';
                        tempresult:=tempresult+'SAR '+modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

              end;
            end;


      $d2 : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='Rotate eight bits left CL times';
                      tempresult:=tempresult+'ROL '+modrm(memory,prefix2,1,2,last,8)+'CL';
                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='Rotate eight bits right CL times';
                      tempresult:=tempresult+'ROR '+modrm(memory,prefix2,1,2,last,8)+'CL';
                      inc(offset,last-1);
                    end;

                2:  begin
                      description:='Rotate nine bits left CL times';
                      tempresult:=tempresult+'RCL '+modrm(memory,prefix2,1,2,last,8)+'CL';
                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='Rotate nine bits right CL times';
                      tempresult:=tempresult+'RCR '+modrm(memory,prefix2,1,2,last,8)+'CL';
                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='Multiply by 2, CL times';
                      tempresult:=tempresult+'SHL '+modrm(memory,prefix2,1,2,last,8)+'CL';
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='Unsigned devide by 2, CL times';
                      tempresult:=tempresult+'SHR '+modrm(memory,prefix2,1,2,last,8)+'CL';
                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='Multiply by 2, CL times';
                      tempresult:=tempresult+'SHL '+modrm(memory,prefix2,1,2,last,8)+'CL';
                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='Signed devide by 2, CL times';
                      tempresult:=tempresult+'SAR '+modrm(memory,prefix2,1,2,last,8)+'CL';
                      inc(offset,last-1);
                    end;


              end;
            end;

      $d3 : begin
              case getreg(memory[1]) of
                0:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Rotate 16 bits left CL times';
                        tempresult:=tempresult+'ROL '+modrm(memory,prefix2,1,1,last,16)+'CL';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Rotate 32 bits left CL times';
                        tempresult:=tempresult+'ROL '+modrm(memory,prefix2,1,0,last)+'CL';
                        inc(offset,last-1);
                      end;
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Rotate 16 bits right CL times';
                        tempresult:=tempresult+'ROR '+modrm(memory,prefix2,1,1,last,16)+'CL';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Rotate 32 bits right CL times';
                        tempresult:=tempresult+'ROR '+modrm(memory,prefix2,1,0,last)+'CL';
                        inc(offset,last-1);
                      end;
                    end;

                2:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Rotate 17 bits left CL times';
                        tempresult:=tempresult+'RCL '+modrm(memory,prefix2,1,1,last,16)+'CL';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Rotate 33 bits left CL times';
                        tempresult:=tempresult+'RCL '+modrm(memory,prefix2,1,0,last)+'CL';
                        inc(offset,last-1);
                      end;
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Rotate 17 bits right CL times';
                        tempresult:=tempresult+'RCR '+modrm(memory,prefix2,1,1,last,16)+'CL';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Rotate 33 bits right CL times';
                        tempresult:=tempresult+'RCR '+modrm(memory,prefix2,1,0,last)+'CL';
                        inc(offset,last-1);
                      end;
                    end;

                4:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Multiply by 2, CL times';
                        tempresult:=tempresult+'SHL '+modrm(memory,prefix2,1,1,last,16)+'CL';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Multiply by 2, CL times';
                        tempresult:=tempresult+'SHL '+modrm(memory,prefix2,1,0,last)+'CL';
                        inc(offset,last-1);
                      end;
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Unsigned divide by 2, CL times';
                        tempresult:=tempresult+'SHR '+modrm(memory,prefix2,1,1,last,16)+'CL';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Unsigned divide by 2, CL times';
                        tempresult:=tempresult+'SHR '+modrm(memory,prefix2,1,0,last)+'CL';
                        inc(offset,last-1);
                      end;
                    end;

                7:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Signed divide by 2, CL times';
                        tempresult:=tempresult+'SAR '+modrm(memory,prefix2,1,1,last,16)+'CL';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Signed divide by 2, CL times';
                        tempresult:=tempresult+'SAR '+modrm(memory,prefix2,1,0,last)+'CL';
                        inc(offset,last-1);
                      end;
                    end;

              end;
            end;


      $D4 : begin  // AAM
              inc(offset);
              tempresult:=tempresult+'AAM';
              description:='ASCII Adjust AX After Multiply';
              if memory[1]<>$0A then tempresult:=tempresult+' '+inttohexs(memory[1],2);
            end;

      $D5 : begin  // AAD
              inc(offset);
              tempresult:=tempresult+'AAD';
              description:='ASCII adjust AX before division';
              if memory[1]<>$0A then tempresult:=tempresult+' '+inttohexs(memory[1],2);
            end;

      $D7 : begin
              description:='Table Look-up Translation';
              tempresult:=tempresult+'XLATB';
            end;

      $d8 : begin
              case getreg(memory[1]) of
                0:  begin
                      //fadd
                      description:='Add';
                      last:=2;
                      if memory[1]>=$c0 then tempresult:=tempresult+'FADD ST('+IntToStr(memory[1]-$c0)+')' else
                      begin
                        tempresult:=tempresult+'FADD '+modrm(memory,prefix2,1,0,last,32);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;
                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='Multiply';
                      last:=2;
                      if memory[1]>=$c8 then tempresult:=tempresult+'FMUL ST('+IntToStr(memory[1]-$c8)+')' else
                      begin
                        tempresult:=tempresult+'FMUL '+modrm(memory,prefix2,1,0,last,32);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;
                      inc(offset,last-1);
                    end;


                2:  begin
                      description:='Compare Real';
                      last:=2;
                      if memory[1]>=$d0 then tempresult:=tempresult+'FCOM ST('+IntToStr(memory[1]-$d0)+')' else
                      begin
                        tempresult:=tempresult+'FCOM '+modrm(memory,prefix2,1,0,last,32);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;
                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='Compare Real and pop register stack';
                      last:=2;
                      if memory[1]>=$d8 then tempresult:=tempresult+'FCOMP ST('+IntToStr(memory[1]-$d8)+')' else
                      begin
                        tempresult:=tempresult+'FCOMP '+modrm(memory,prefix2,1,0,last,32);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;
                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='Substract';
                      last:=2;
                      if memory[1]>=$e0 then tempresult:=tempresult+'FSUB ST('+IntToStr(memory[1]-$e0)+')' else
                      begin
                        tempresult:=tempresult+'FSUB '+modrm(memory,prefix2,1,0,last,32);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='Reverse Substract';
                      last:=2;
                      if memory[1]>=$e0 then tempresult:=tempresult+'FSUBR ST('+IntToStr(memory[1]-$e0)+')' else
                      begin
                        tempresult:=tempresult+'FSUBR '+modrm(memory,prefix2,1,0,last,32);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;
                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='Divide';
                      last:=2;
                      if memory[1]>=$f0 then tempresult:=tempresult+'FDIV ST('+IntToStr(memory[1]-$d8)+')' else
                      begin
                        tempresult:=tempresult+'FDIV '+modrm(memory,prefix2,1,0,last,32);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;
                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='Reverse Divide';
                      last:=2;
                      if memory[1]>=$f8 then tempresult:=tempresult+'FDIVR ST('+IntToStr(memory[1]-$d8)+')' else
                      begin
                        tempresult:=tempresult+'FDIVR '+modrm(memory,prefix2,1,0,last,32);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;
                      inc(offset,last-1);
                    end;
              end;

            end;

      $d9 : begin
              case memory[1] of
              $00..$bf : begin
                           case getreg(memory[1]) of
                           0:  begin
                                 description:='Load Floating Point Value';
                                 tempresult:=tempresult+'FLD '+modrm(memory,prefix2,1,0,last,32);
                                 tempresult:=copy(tempresult,1,length(tempresult)-1);
                                 inc(offset,last-1);
                               end;

                           2:  begin
                                 description:='Store Real';
                                 tempresult:=tempresult+'FST '+modrm(memory,prefix2,1,0,last,32);
                                 tempresult:=copy(tempresult,1,length(tempresult)-1);
                                 inc(offset,last-1);
                               end;

                           3:  begin
                                 description:='Store Real';
                                 tempresult:=tempresult+'FSTP '+modrm(memory,prefix2,1,0,last,32);
                                 tempresult:=copy(tempresult,1,length(tempresult)-1);
                                 inc(offset,last-1);
                               end;

                           4:  begin
                                 description:='Load FPU Environment';
                                 tempresult:=tempresult+'FLDENV '+modrm(memory,prefix2,1,0,last);
                                 tempresult:=copy(tempresult,1,length(tempresult)-1);
                                 inc(offset,last-1);
                               end;

                           5:  begin
                                 description:='Load Control Word';
                                 tempresult:=tempresult+'FLDCW '+modrm(memory,prefix2,1,0,last);
                                 tempresult:=copy(tempresult,1,length(tempresult)-1);
                                 inc(offset,last-1);
                               end;

                           6:  begin
                                 description:='Store FPU Environment';
                                 tempresult:=tempresult+'FNSTENV '+modrm(memory,prefix2,1,0,last);
                                 tempresult:=copy(tempresult,1,length(tempresult)-1);
                                 inc(offset,last-1);
                               end;

                           7:  begin
                                 description:='Store Control Word';
                                 tempresult:=tempresult+'FNSTCW '+modrm(memory,prefix2,1,0,last);
                                 tempresult:=copy(tempresult,1,length(tempresult)-1);
                                 inc(offset,last-1);
                               end;


                           end;
                         end;

              $c0..$c7 : begin
                           description:='Push ST(i) onto the FPU register stack';
                           tempresult:=tempresult+'FLD ST('+IntToStr(memory[1]-$c0)+')';
                           inc(offset);
                         end;

              $c8..$cf : begin
                           description:='Exchange Register Contents';
                           tempresult:=tempresult+'FLD ST('+IntToStr(memory[1]-$c8)+')';
                           inc(offset);
                         end;


              $d9..$df : begin
                           description:='Exchagne Register contents';
                           tempresult:=tempresult+'FXCH ST('+IntToStr(memory[1]-$d9)+')';
                           inc(offset);
                         end;


              $d0 : begin
                      description:='No Operation';
                      tempresult:=tempresult+'FNOP';
                      inc(offset);
                    end;

              $e0 : begin
                      description:='Change Sign';
                      tempresult:=tempresult+'FCHS';
                      inc(offset);
                    end;

              $e1 : begin
                      description:='Absolute Value';
                      tempresult:=tempresult+'FABS';
                      inc(offset);
                    end;

              $e4 : begin
                      description:='TEST';
                      tempresult:=tempresult+'FTST';
                      inc(offset);
                    end;

              $e5 : begin
                      description:='Examine';
                      tempresult:=tempresult+'FXAM';
                      inc(offset);
                    end;



              $e8 : begin
                      description:='Load constant';
                      tempresult:=tempresult+'FLD1';
                      inc(offset);
                    end;

              $e9 : begin
                      description:='Load constant';
                      tempresult:=tempresult+'FLDL2T';
                      inc(offset);
                    end;

              $ea : begin
                      description:='Load constant';
                      tempresult:=tempresult+'FLD2E';
                      inc(offset);
                    end;

              $eb : begin
                      description:='Load constant';
                      tempresult:=tempresult+'FLDPI';
                      inc(offset);
                    end;

              $ec : begin
                      description:='Load constant';
                      tempresult:=tempresult+'FLDLG2';
                      inc(offset);
                    end;

              $ed : begin
                      description:='Load constant';
                      tempresult:=tempresult+'FLDLN2';
                      inc(offset);
                    end;

              $ee : begin
                      description:='Load constant';
                      tempresult:=tempresult+'FLDZ';
                      inc(offset);
                    end;


              $f0 : begin
                      description:='Compute 2^x–1';
                      tempresult:=tempresult+'F2XM1';
                      inc(offset);
                    end;

              $f1 : begin
                      description:='Compute y*log(2)x';
                      tempresult:=tempresult+'FYL2X';
                      inc(offset);
                    end;

              $f2 : begin
                      description:='Partial Tangent';
                      tempresult:=tempresult+'FPTAN';
                      inc(offset);
                    end;

              $f3 : begin
                      description:='Partial Arctangent';
                      tempresult:=tempresult+'FPATAN';
                      inc(offset);
                    end;

              $f4 : begin
                      description:='Extract Exponent and Significand';
                      tempresult:=tempresult+'FXTRACT';
                      inc(offset);
                    end;

              $f5 : begin
                      description:='Partial Remainder';
                      tempresult:=tempresult+'FPREM1';
                      inc(offset);
                    end;

              $f6 : begin
                      description:='Decrement Stack-Top Pointer';
                      tempresult:='FDECSTP';
                      inc(offset);
                    end;

              $f7 : begin
                      description:='Increment Stack-Top Pointer';
                      tempresult:='FINCSTP';
                      inc(offset);
                    end;

              $f8 : begin
                      description:='Partial Remainder';
                      tempresult:=tempresult+'FPREM';
                      inc(offset);
                    end;

              $f9 : begin
                      description:='Compute y*log(2)(x+1)';
                      tempresult:=tempresult+'FYL2XP1';
                      inc(offset);
                    end;

              $fa : begin
                      description:='Square Root';
                      tempresult:=tempresult+'FSQRT';
                      inc(offset);
                    end;

              $fb : begin
                      description:='Sine and Cosine';
                      tempresult:=tempresult+'FSINCOS';
                      inc(offset);
                    end;


              $fc : begin
                      description:='Round to Integer';
                      tempresult:=tempresult+'FRNDINT';
                      inc(offset);
                    end;

              $fd : begin
                      description:='Scale';
                      tempresult:=tempresult+'FSCALE';
                      inc(offset);
                    end;

              $fe : begin
                      description:='Sine';
                      tempresult:=tempresult+'FSIN';
                      inc(offset);
                    end;

              $ff : begin
                      description:='Cosine';
                      tempresult:=tempresult+'FCOS';
                      inc(offset);
                    end;
              end;
            end;

      $da : begin
              if memory[1]<$BF then
              begin
                case getreg(memory[1]) of
                  0:  begin
                        description:='Add';
                        tempresult:=tempresult+'FIADD '+modrm(memory,prefix2,1,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                  1:  begin
                        description:='Multiply';
                        tempresult:=tempresult+'FIMUL '+modrm(memory,prefix2,1,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                  2:  begin
                        description:='Compare Integer';
                        tempresult:=tempresult+'FICOM '+modrm(memory,prefix2,1,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                  3:  begin
                        description:='Compare Integer';
                        tempresult:=tempresult+'FICOMP '+modrm(memory,prefix2,1,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                  4:  begin
                        description:='Subtract';
                        tempresult:=tempresult+'FISUB '+modrm(memory,prefix2,1,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                  5:  begin
                        description:='Reverse Subtract';
                        tempresult:=tempresult+'FISUBR '+modrm(memory,prefix2,1,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;


                  6:  begin
                        description:='Devide';
                        tempresult:=tempresult+'FIDIV '+modrm(memory,prefix2,1,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;

                  7:  begin
                        description:='Reverse Devide';
                        tempresult:=tempresult+'FIDIVR '+modrm(memory,prefix2,1,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;
                end;
              end
              else
              begin
                case getreg(memory[1]) of
                  0:  begin
                        description:='Floating-Point: Move if below';
                        tempresult:='FCMOVB ST('+IntToStr(memory[1]-$c0)+')';
                        inc(offset);
                      end;

                  1:  begin
                        description:='Floating-Point: Move if equal';
                        tempresult:='FCMOVE ST('+IntToStr(memory[1]-$c8)+')';
                        inc(offset);
                      end;

                  2:  begin
                        description:='Floating-Point: Move if below or equal';
                        tempresult:='FCMOVBE ST('+IntToStr(memory[1]-$d0)+')';
                        inc(offset);
                      end;

                  3:  begin
                        description:='Floating-Point: Move if unordered';
                        tempresult:='FCMOVU ST('+IntToStr(memory[1]-$d8)+')';
                        inc(offset);
                      end;

                  5:  begin
                        case memory[1] of
                        $e9 : begin
                                description:='Unordered Compare Real';
                                tempresult:=tempresult+'FUCOMPP';
                                inc(offset);
                              end;
                        end;
                      end;
                end;
              end;
            end;

      $db : begin
              case memory[1] of
                $0..$bf : begin
                            case getreg(memory[1]) of
                              0:  begin
                                    description:='Load Integer';
                                    tempresult:=tempresult+'FILD '+modrm(memory,prefix2,1,0,last,32);
                                    tempresult:=copy(tempresult,1,length(tempresult)-1);
                                    inc(offset,last-1);
                                  end;

                              2:  begin
                                    description:='Store Integer';
                                    tempresult:=tempresult+'FIST '+modrm(memory,prefix2,1,0,last,32);
                                    tempresult:=copy(tempresult,1,length(tempresult)-1);
                                    inc(offset,last-1);
                                  end;

                              3:  begin
                                    description:='Store Integer';
                                    tempresult:=tempresult+'FISTP '+modrm(memory,prefix2,1,0,last,32);
                                    tempresult:=copy(tempresult,1,length(tempresult)-1);
                                    inc(offset,last-1);
                                  end;

                              5:  begin
                                    description:='Load Floating Point Value';
                                    tempresult:=tempresult+'FLD '+modrm(memory,prefix2,1,0,last,64);
                                    tempresult:=copy(tempresult,1,length(tempresult)-1);
                                    inc(offset,last-1);
                                  end;

                              7:  begin
                                    description:='Store Real';
                                    tempresult:=tempresult+'FSTP '+modrm(memory,prefix2,1,0,last,80);
                                    tempresult:=copy(tempresult,1,length(tempresult)-1);
                                    inc(offset,last-1);
                                  end;

                            end;
                          end;

                $c0..$c7 : begin
                             description:='Floating-Point: Move if not below';
                             tempresult:='FCMOVNB ST('+IntToStr(memory[1]-$c0)+')';
                             inc(offset);
                           end;

                $c8..$cf : begin
                             description:='Floating-Point: Move if not equal';
                             tempresult:='FCMOVNE ST('+IntToStr(memory[1]-$c8)+')';
                             inc(offset);
                           end;

                $d0..$d7 : begin
                             description:='Floating-Point: Move if not below or equal';
                             tempresult:='FCMOVNBE ST('+IntToStr(memory[1]-$d0)+')';
                             inc(offset);
                           end;

                $d8..$df : begin
                             description:='Floating-Point: Move if not unordered';
                             tempresult:='FCMOVNU ST('+IntToStr(memory[1]-$d8)+')';
                             inc(offset);
                           end;

                $e2 : begin
                        description:='Clear Exceptions';
                        tempresult:=tempresult+'FNCLEX';
                        inc(offset);
                      end;

                $e3 : begin
                        description:='Initialize floating-Point Unit';
                        tempresult:=tempresult+'FNINIT';
                        inc(offset);
                      end;

                $e8..$ef : begin
                             description:='Floating-Point: Compare Real and Set EFLAGS';
                             tempresult:='FUCOMI ST('+IntToStr(memory[1]-$e8)+')';
                             inc(offset);
                           end;

                $f0..$f7 : begin
                             description:='Floating-Point: Compare Real and Set EFLAGS';
                             tempresult:='FCOMI ST('+IntToStr(memory[1]-$f0)+')';
                             inc(offset);
                           end;
              end;


            end;

      $dc : begin
              case getreg(memory[1]) of
                0:  begin
                      //fadd
                      description:='Add';
                      last:=2;
                      if memory[1]>=$c0 then tempresult:=tempresult+'FADD ST('+IntToStr(memory[1]-$c0)+')' else
                      begin
                        tempresult:=tempresult+'FADD '+modrm(memory,prefix2,1,0,last,64);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;
                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='Multiply';
                      last:=2;
                      if memory[1]>=$c8 then tempresult:=tempresult+'FMUL ST('+IntToStr(memory[1]-$c8)+')' else
                      begin
                        tempresult:=tempresult+'FMUL '+modrm(memory,prefix2,1,0,last,64);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;
                      inc(offset,last-1);
                    end;

                2:  begin
                      description:='Compare Real';
                      last:=2;
                      tempresult:=tempresult+'FCOM '+modrm(memory,prefix2,1,0,last,64);
                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='Compare Real';
                      last:=2;
                      tempresult:=tempresult+'FCOMP '+modrm(memory,prefix2,1,0,last,64);
                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='Subtract';
                      last:=2;
                      if memory[1]>=$e0 then tempresult:=tempresult+'FSUB ST('+IntToStr(memory[1]-$e0)+')' else
                      begin
                        tempresult:=tempresult+'FSUB '+modrm(memory,prefix2,1,0,last,64);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='Reverse Subtract';
                      last:=2;
                      if memory[1]>=$e8 then tempresult:=tempresult+'FSUBR ST('+IntToStr(memory[1]-$e8)+')' else
                      begin
                        tempresult:=tempresult+'FSUBR '+modrm(memory,prefix2,1,0,last,64);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;


                      inc(offset,last-1);
                    end;


                6:  begin
                      description:='Divide';
                      last:=2;
                      if memory[1]>=$f0 then tempresult:=tempresult+'FDIV ST('+IntToStr(memory[1]-$f0)+'),ST(0)' else
                      begin
                        tempresult:=tempresult+'FDIV '+modrm(memory,prefix2,1,0,last,64);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;
                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='Reverse Divide';
                      last:=2;
                      if memory[1]>=$f0 then tempresult:=tempresult+'FDIVR ST('+IntToStr(memory[1]-$f8)+'),ST(0)' else
                      begin
                        tempresult:=tempresult+'FDIVR '+modrm(memory,prefix2,1,0,last,64);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;
                      inc(offset,last-1);
                    end;
              end;
            end;

      $dd : begin
              case memory[1] of
              $0..$bf :  begin
                           case getreg(memory[1]) of
                             0:  begin
                                   description:='Load floating point value';
                                   tempresult:=tempresult+'FLD '+modrm(memory,prefix2,1,0,last,64);
                                   tempresult:=copy(tempresult,1,length(tempresult)-1);
                                   inc(offset,last-1);
                                 end;

                             2:  begin
                                   description:='Store Real';
                                   tempresult:=tempresult+'FST '+modrm(memory,prefix2,1,0,last,64);
                                   tempresult:=copy(tempresult,1,length(tempresult)-1);
                                   inc(offset,last-1);
                                 end;

                             3:  begin
                                   description:='Store Real';
                                   tempresult:=tempresult+'FSTP '+modrm(memory,prefix2,1,0,last,64);
                                   tempresult:=copy(tempresult,1,length(tempresult)-1);
                                   inc(offset,last-1);
                                 end;

                             4:  begin
                                   description:='Restore FPU State';
                                   tempresult:=tempresult+'FRSTOR '+modrm(memory,prefix2,1,0,last);
                                   tempresult:=copy(tempresult,1,length(tempresult)-1);
                                   inc(offset,last-1);
                                 end;

                             6:  begin
                                   description:='Store FPU State';
                                   tempresult:=tempresult+'FSAVE '+modrm(memory,prefix2,1,0,last);
                                   tempresult:=copy(tempresult,1,length(tempresult)-1);
                                   inc(offset,last-1);
                                 end;

                             7:  begin
                                   description:='Store Status Word';
                                   tempresult:=tempresult+'FNSTSW '+modrm(memory,prefix2,1,0,last);
                                   tempresult:=copy(tempresult,1,length(tempresult)-1);
                                   inc(offset,last-1);
                                 end;

                           end;

                         end;

              $c0..$c7 : begin
                           description:='Free Floating-Point Register';
                           tempresult:=tempresult+'FFREE ST('+IntToStr(memory[1]-$c0)+')';
                           inc(offset);
                         end;

              $d0..$d7 : begin
                           description:='Store Real';
                           tempresult:=tempresult+'FST ST('+IntToStr(memory[1]-$d0)+')';
                           inc(offset);
                         end;

              $d8..$df : begin
                           description:='Store Real';
                           tempresult:=tempresult+'FSTP ST('+IntToStr(memory[1]-$d8)+')';
                           inc(offset);
                         end;

              $e0..$e7 : begin
                           description:='Unordered Compare Real';
                           tempresult:=tempresult+'FUCOM ST('+IntToStr(memory[1]-$e0)+')';
                           inc(offset);
                         end;

              $e8..$ef : begin
                           description:='Unordered Compare Real';
                           tempresult:=tempresult+'FUCOMP ST('+IntToStr(memory[1]-$e0)+')';
                           inc(offset);
                         end;
              else tempresult:=tempresult+'DB '+inttohexs(memory[0],2);

              end;
            end;

      $de : begin
              case getreg(memory[1]) of
                0:  begin
                      //faddp
                      description:='Add and pop';
                      last:=2;
                      if (memory[1]=$c1) then tempresult:=tempresult+'FADDP'
                      else
                      if memory[1]>$c0 then tempresult:=tempresult+'FADDP ST('+IntToStr(memory[1]-$c0)+')' else
                      begin
                        description:='Add';
                        tempresult:=tempresult+'FIADD '+modrm(memory,prefix2,1,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;
                      inc(offset,last-1);
                    end;

                1: begin
                      description:='Multiply';
                      last:=2;
                      if memory[1]>=$c8 then tempresult:=tempresult+'FMULP ST('+IntToStr(memory[1]-$c0)+'),ST(0)' else
                      begin
                        tempresult:=tempresult+'FIMUL '+modrm(memory,prefix2,1,0,last);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;

                      inc(offset,last-1);
                   end;

                2: begin
                     description:='Compare Integer';
                     last:=2;
                     tempresult:=tempresult+'FICOM'+modrm(memory,prefix2,1,0,last);
                     inc(offset,last-1);
                   end;


                3: begin
                     if memory[1]<$c0 then
                     begin
                       description:='Compare Integer';
                       tempresult:=tempresult+'FICOMP '+modrm(memory,prefix2,1,0,last);
                       tempresult:=copy(tempresult,1,length(tempresult)-1);
                       inc(offset,last-1);
                     end;

                     if memory[1]=$D9 then
                     begin
                       description:='Compare Real and pop register stack twice';
                       tempresult:=tempresult+'FCOMPP';
                       inc(offset);
                     end;
                   end;

                4: begin
                     description:='Subtract';
                     last:=2;
                     if memory[1]>=$e0 then tempresult:=tempresult+'FSUBRP ST('+IntToStr(memory[1]-$c0)+'),ST(0)' else
                     begin
                       tempresult:=tempresult+'FISUB '+modrm(memory,prefix2,1,0,last);
                       tempresult:=copy(tempresult,1,length(tempresult)-1);
                     end;
                     inc(offset,last-1);
                   end;


                5: begin
                     description:='Reverse Devide';
                     last:=2;
                     if memory[1]>=$e8 then tempresult:=tempresult+'FSUB ST('+IntToStr(memory[1]-$e8)+')';
                     begin
                       tempresult:=tempresult+'FISUBR '+modrm(memory,prefix2,1,0,last);
                       tempresult:=copy(tempresult,1,length(tempresult)-1);
                     end;

                     inc(offset,last-1);
                   end;


                6: begin
                     description:='Reverse Devide';
                     last:=2;
                     if memory[1]>=$f0 then
                     begin
                       tempresult:=tempresult+'FDIVRP ST('+IntToStr(memory[1]-$f0)+')';
                       inc(offset,last-1);
                     end
                     else tempresult:='DB DE'
                   end;

                7: begin
                     description:='Devide';
                     last:=2;
                     if memory[1]>=$f8 then tempresult:=tempresult+'FDIVP ST('+IntToStr(memory[1]-$f8)+')' else
                     begin
                       tempresult:=tempresult+'FDIVR '+modrm(memory,prefix2,1,0,last);
                       tempresult:=copy(tempresult,1,length(tempresult)-1);
                     end;
                     inc(offset,last-1);
                   end;

              end;
            end;

      $df : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='Load Integer';
                      tempresult:='FILD '+modrm(memory,prefix2,1,0,last,16);
                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                2:  begin
                      description:='Store Integer';
                      tempresult:='FIST '+modrm(memory,prefix2,1,0,last,16);
                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='Store Integer';
                      tempresult:='FISTP '+modrm(memory,prefix2,1,0,last,16);
                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='Load Binary Coded Decimal';
                      last:=2;
                      if memory[1]>=$e0 then tempresult:=tempresult+'FNSTSW AX' else
                      begin
                        tempresult:='FBLD '+modrm(memory,prefix2,1,0,last,80);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                      end;
                      inc(offset,last-1);
                    end;

               5:  begin
                     if memory[1]<$c0 then
                     begin
                       description:='Load Integer';
                       tempresult:='FILD '+modrm(memory,prefix2,1,0,last,128);
                       tempresult:=copy(tempresult,1,length(tempresult)-1);
                       inc(offset,last-1);
                     end;

                     if memory[1]>=$e8 then
                     begin
                       description:='Compare Real and Set EFLAGS';
                       tempresult:='FUCOMIP ST,('+IntToStr(memory[1]-$e8)+')';
                       inc(offset);
                     end;
                   end;

               6:  begin
                      if memory[1]>=$f0 then
                      begin
                        description:='Compare Real and Set EFLAGS';
                        tempresult:='FCOMI ST,('+IntToStr(memory[1]-$f0)+')';
                      end
                      else
                      begin
                        description:='Store BCD Integer and Pop';
                        tempresult:='FBSTP '+modrm(memory,prefix2,1,0,last,80);
                        tempresult:=copy(tempresult,1,length(tempresult)-1);
                        inc(offset,last-1);
                      end;
                    end;

                7:  begin
                      description:='Store Integer';
                      tempresult:='FISTP '+modrm(memory,prefix2,1,0,last,64);
                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;
                else tempresult:=tempresult+'DB '+inttohexs(memory[0],2);
              end;

            end;

      $e0 : begin
              description:='Loop According to ECX counter';
              if $66 in prefix2 then
              begin
                tempresult:=tempresult+'LOOPNE ';
                inc(offset);
                tempresult:=tempresult+inttohexs(dword(offset+pshortint(@memory[1])^),8);
              end
              else
              begin
                tempresult:=tempresult+'LOOPNZ ';
                inc(offset);
                tempresult:=tempresult+inttohexs(dword(offset+pshortint(@memory[1])^),8);
              end;
            end;

      $e1 : begin
              description:='Loop According to ECX counter';
              if $66 in prefix2 then
              begin
                tempresult:=tempresult+'LOOPE ';
              end
              else
              begin
                tempresult:=tempresult+'LOOPZ ';
              end;
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+pshortint(@memory[1])^),0);
            end;

      $e2 : begin
              description:='Loop According to ECX counting';
              tempresult:=tempresult+'LOOP ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+pshortint(@memory[1])^),0);
            end;

      $e3 : begin
              description:='Jump short if CX=0';
              if $66 in prefix2 then
                tempresult:=tempresult+'JCXZ '
              else
                tempresult:=tempresult+'JECXZ ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+pshortint(@memory[1])^),8);
            end;

      $e4 : begin
              description:='Input from Port';
              tempresult:='IN AL,'+inttohexs(memory[1],2);
              inc(offset);
            end;

      $e5 : begin
              description:='Input from Port';
              if $66 in prefix2 then tempresult:=tempresult+'IN AX,'+inttohexs(memory[1],2)
                                else tempresult:=tempresult+'IN EAX,'+inttohexs(memory[1],2);
              inc(offset);

            end;

      $e6 : begin
              description:='Output to Port';
              tempresult:='OUT '+inttohexs(memory[1],2)+',AL';
              inc(offset);
            end;

      $e7 : begin
              description:='Output toPort';
              if $66 in prefix2 then tempresult:=tempresult+'OUT '+inttohexs(memory[1],2)+',AX'
                                else tempresult:=tempresult+'OUT '+inttohexs(memory[1],2)+',EAX';
              inc(offset);

            end;

      $e8 : begin
              //call
              //this time no $66 prefix because it will only run in win32
              description:='Call Procedure';
              tempresult:=tempresult+'CALL ';
              inc(offset,4);

              tempresult:=tempresult+inttohexs(offset+pint(@memory[1])^,8);
            end;

      $e9 : begin
              description:='Jump near';
              if $66 in prefix2 then
              begin
                tempresult:=tempresult+'JMP ';
                inc(offset,2);
                tempresult:=tempresult+inttohexs(dword(offset+psmallint(@memory[1])^),8);
              end
              else
              begin
                tempresult:=tempresult+'JMP ';
                inc(offset,4);
                tempresult:=tempresult+inttohexs(dword(offset+pInteger(@memory[1])^),8);
              end;

            end;

      $ea : begin
              description:='Jump far';
              wordptr:=@memory[5];
              tempresult:=tempresult+'JMP '+inttohexs(wordptr^,4)+':';
              dwordptr:=@memory[1];
              tempresult:=tempresult+inttohexs(dwordptr^,8);
              inc(offset,6);
            end;

      $eb : begin
              description:='Jump short';
              tempresult:=tempresult+'JMP ';
              inc(offset);
              tempresult:=tempresult+inttohexs(dword(offset+pshortint(@memory[1])^),8);
            end;

      $ec : begin
              description:='Input from Port';
              tempresult:=tempresult+'IN AL,DX';
            end;

      $ed : begin
              description:='Input from Port';
              if $66 in prefix2 then tempresult:=tempresult+'IN AX,DX' else
                                     tempresult:=tempresult+'IN EAX,DX';
            end;

      $ee : begin
              description:='Input from Port';
              tempresult:=tempresult+'OUT DX,AL';
            end;

      $ef : begin
              description:='Input from Port';
              if $66 in prefix2 then tempresult:=tempresult+'OUT DX,AX' else
                                     tempresult:=tempresult+'OUT DX,EAX';
            end;

      $f3 : begin

            end;

      $f4 : begin
              description:='Halt';
              tempresult:=tempresult+'HLT';
            end;

      $f5 : begin
              description:='Complement Carry Flag';
              tempresult:=tempresult+'CMC';
            end;

      $f6 : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='Logical Compare';
                      tempresult:=tempresult+'TEST '+modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      inc(offset,last);
                    end;

                2:  begin
                      description:='One''s Complement Negation';
                      tempresult:=tempresult+'NOT '+modrm(memory,prefix2,1,2,last,8);
                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='Two''s Complement Negation';
                      tempresult:=tempresult+'NEG '+modrm(memory,prefix2,1,2,last,8);
                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='Unsigned Multiply';
                      tempresult:=tempresult+'MUL '+modrm(memory,prefix2,1,2,last,8);
                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='Signed Multiply';
                      tempresult:=tempresult+'IMUL '+modrm(memory,prefix2,1,2,last,8);
                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='Unsigned Divide';
                      tempresult:=tempresult+'DIV '+modrm(memory,prefix2,1,2,last,8);
                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='Signed Divide';
                      tempresult:=tempresult+'IDIV '+modrm(memory,prefix2,1,2,last,8);
                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;
                else tempresult:=tempresult+'DB '+inttohexs(memory[0],2);

              end;
            end;

      $f7 : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='Logical Compare';
                      if $66 in prefix2 then
                      begin
                        tempresult:=tempresult+'TEST '+modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        tempresult:=tempresult+''+inttohexs(wordptr^,4);
                        inc(offset,last+1);
                      end
                      else
                      begin
                        tempresult:=tempresult+'TEST '+modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        tempresult:=tempresult+''+inttohexs(dwordptr^,4);
                        inc(offset,last+3);
                      end;
                    end;

                2:  begin
                      description:='One''s Complement Negation';
                      if $66 in prefix2 then
                        tempresult:=tempresult+'NOT '+modrm(memory,prefix2,1,1,last,16)
                      else
                        tempresult:=tempresult+'NOT '+modrm(memory,prefix2,1,0,last);

                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='Two''s Complement Negation';
                      if $66 in prefix2 then
                        tempresult:=tempresult+'NEG '+modrm(memory,prefix2,1,1,last,16)
                      else
                        tempresult:=tempresult+'NEG '+modrm(memory,prefix2,1,0,last);

                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='Unsigned Multiply';
                      if $66 in prefix2 then
                        tempresult:=tempresult+'MUL '+modrm(memory,prefix2,1,1,last,16)
                      else
                        tempresult:=tempresult+'MUL '+modrm(memory,prefix2,1,0,last);

                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='Signed Multiply';
                      if $66 in prefix2 then
                        tempresult:=tempresult+'IMUL '+modrm(memory,prefix2,1,1,last,16)
                      else
                        tempresult:=tempresult+'IMUL '+modrm(memory,prefix2,1,0,last);

                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='Unsigned Divide';
                      if $66 in prefix2 then
                        tempresult:=tempresult+'DIV '+modrm(memory,prefix2,1,1,last,16)
                      else
                        tempresult:=tempresult+'DIV '+modrm(memory,prefix2,1,0,last);

                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='Signed Divide';
                      if $66 in prefix2 then
                        tempresult:=tempresult+'IDIV '+modrm(memory,prefix2,1,1,last,16)
                      else
                        tempresult:=tempresult+'IDIV '+modrm(memory,prefix2,1,0,last);

                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                  else tempresult:=tempresult+'DB '+inttohexs(memory[0],2);
                end;
            end;

      $f8 : begin
              description:='Clear Carry Flag';
              tempresult:=tempresult+'CLC';
            end;

      $f9 : begin
              description:='Set Carry Flag';
              tempresult:=tempresult+'STC';
            end;

      $fa : begin
              description:='Clear Interrupt Flag';
              tempresult:=tempresult+'CLI';
            end;

      $fb : begin
              description:='Set Interrupt Flag';
              tempresult:=tempresult+'STI';
            end;

      $fc : begin
              description:='Clear Direction Flag';
              tempresult:=tempresult+'CLD';
            end;

      $fd : begin
              description:='Set Direction Flag';
              tempresult:=tempresult+'STD';
            end;

      $fe : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='Increment by 1';
                      tempresult:=tempresult+'INC '+modrm(memory,prefix2,1,2,last,8);
                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='Decrement by 1';
                      tempresult:=tempresult+'DEC '+modrm(memory,prefix2,1,2,last,7);
                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                else tempresult:=tempresult+'DB '+inttohexs(memory[0],2);
              end;
            end;

      $ff : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='Increment by 1';
                      if $66 in prefix2 then
                        tempresult:=tempresult+'INC '+modrm(memory,prefix2,1,1,last,16)
                      else
                        tempresult:=tempresult+'INC '+modrm(memory,prefix2,1,0,last);

                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='Decrement by 1';
                      if $66 in prefix2 then
                        tempresult:=tempresult+'DEC '+modrm(memory,prefix2,1,1,last,16)
                      else
                        tempresult:=tempresult+'DEC '+modrm(memory,prefix2,1,0,last);

                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                2:  begin
                      //call
                      description:='Call Procedure';
                      if memory[1]>=$c0 then tempresult:='CALL '+modrm(memory,prefix2,1,0,last) else
                                             tempresult:='CALL '+modrm(memory,prefix2,1,0,last,32);
                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                3:  begin
                      //call
                      description:='Call Procedure';
                      tempresult:='CALL '+modrm(memory,prefix2,1,0,last);
                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                4:  begin
                      //call
                      description:='Jump near';
                      if memory[1]>=$c0 then tempresult:='JMP '+modrm(memory,prefix2,1,0,last) else
                                             tempresult:='JMP '+modrm(memory,prefix2,1,0,last,32);
                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                5:  begin
                      //call
                      description:='Jump far';
                      tempresult:='JMP '+modrm(memory,prefix2,1,0,last);
                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='Push Word or Doubleword Onto the Stack';
                      if $66 in prefix2 then
                        tempresult:=tempresult+'PUSH '+modrm(memory,prefix2,1,1,last)
                      else
                        tempresult:=tempresult+'PUSH '+modrm(memory,prefix2,1,0,last);

                      tempresult:=copy(tempresult,1,length(tempresult)-1);
                      inc(offset,last-1);
                    end;
                else tempresult:=tempresult+'DB '+inttohexs(memory[0],2);

              end;

            end;

      else  begin
              tempresult:='DB '+inttohex(memory[0],2);
            end;
    end;

    for i:=0 to (offset-startoffset)-1 do
      result:=result+inttohex(memory[i],2)+' ';
    result:=result+'- '+tempresult;
  end
  else
  begin
    result:=result+'??';
    inc(offset);
  end;

  result:=lowercase(result);
end;

function disassemble(var offset: dword): string; overload;
var ignore: string;
begin
  result:=disassemble(offset,ignore);
end;



function previousopcode(address: dword):dword;
var x,y: dword;
    s: string;
    found: boolean;
    i: integer;
begin
  y:=address-40;

  while y<address do
  begin
    x:=y;
    disassemble(y,s);
  end;

  i:=address-20;
  while (i<address) and (y<>address) do
  begin
    y:=i;
    while y<address do
    begin
      x:=y;
      disassemble(y,s);
    end;
    inc(i);
  end;

  if i=address then result:=address-1 else result:=x;

 // if x<>address then result:=address-1 else result:=y;
end;

function translatestring(disassembled: string; numberofbytes: integer; showvalues: boolean; var address: string; var bytes: string; var opcode: string; var special:string):string;
var offset,value:dword;
    e: integer;
    i,j,j2,k,l: integer;
    ts,ts2,ts3: string;
    actualread: dword;
    valuetype: integer;

    tokens: ttokens;
    fvalue: single;
    fvalue2: double;
begin
  result:=disassembled;


{
  ts:=disassembled;
  val('$'+disassembled,offset,e);

  result:=inttohex(offset,8)+' - ';

  i:=pos('-',disassembled);
  if i=0 then exit;

  ts[i]:=' ';
  inc(i,2);
  j:=pos('-',ts);
  if j=0 then
  begin
    result:=result+'??';
    exit;
  end;

  dec(j,2);


 { if (j-i)>2*numberofbytes then
  begin
    result:=result+copy(disassembled,i,(2*(numberofbytes)-1));
    result:=result+'...';
  end
  else
  begin
    ts:=copy(disassembled,i,j-i);

    k:=2*(numberofbytes);

    while length(ts)<k+2 do ts:=ts+' ';

    result:=result+ts;
  end;   }


  if showvalues then
  begin
    i:=pos('[',disassembled);
    if i>0 then
    begin
      ts:=copy(disassembled,i+1,pos(']',disassembled)-i-1);
      try
        offset:=symhandler.getAddressFromName(ts,false);
        if offset<$80000000 then
        begin
          //decide what kind of value it is

          valuetype:=2; //4 bytehex by default
          setlength(tokens,0); //init

          ts2:=copy(disassembled,pos('-',disassembled)+2,length(disassembled));
          ts2:=copy(ts2,pos('-',ts2)+2,length(ts2));


          if pos('dword ptr',ts2)>0 then valuetype:=2 else
          if pos('byte ptr',ts2)>0 then valuetype:=0 else
          if pos('word ptr',ts2)>0 then valuetype:=1 else
          begin
            //check the register used
            j2:=pos(',[',ts2);
            k:=pos('],',ts2);
            if j2>0 then //register in front
            begin
              l:=pos(' ',ts2);
              ts3:=copy(ts2,l+1,j2-l-1);
              valuetype:=TokenToRegisterbit(uppercase(ts3));
              case valuetype of
                1: valuetype:=0;
                2: valuetype:=1;
                3: valuetype:=2;
                else valuetype:=2;
              end;
            end
            else
            if k>0 then  //register after ],
            begin
              l:=pos('],',ts2);
              ts3:=copy(ts2,l+2,length(ts2)-l-1);
              valuetype:=TokenToRegisterbit(uppercase(ts3));
              case valuetype of
                1: valuetype:=0;
                2: valuetype:=1;
                3: valuetype:=2;
                else valuetype:=2;
              end;
            end; //else no ideam check instruction

            if valuetype=2 then
            begin
              //default or not decided yet, check the instruction for the float
             // if pos('
            end;

          end;

          setlength(tokens,0);

          ts:='';
          value:=0;
          fvalue:=0;
          fvalue2:=0;
          if valuetype=0 then //1 byte
          begin
            if readprocessmemory(processhandle,pointer(offset),@value,1,actualread) then
              ts:=' : '+inttohex(value,2)
          end
          else
          if valuetype=1 then //2 byte
          begin
            if readprocessmemory(processhandle,pointer(offset),@value,2,actualread) then
              ts:=' : '+inttohex(value,4)
          end
          else
          if valuetype=2 then //4 byte, could be pointer,so look up name if possible
          begin
            if readprocessmemory(processhandle,pointer(offset),@value,4,actualread) then
              ts:=' : '+symhandler.getNameFromAddress(value)
          end
          else
          if valuetype=3 then //single
          begin
            if readprocessmemory(processhandle,pointer(offset),@fvalue,4,actualread) then
              ts:=' : '+format('%.4f',[fvalue]);
          end else
          if valuetype=4 then //double
          begin
            if readprocessmemory(processhandle,pointer(offset),@fvalue2,8,actualread) then
              ts:=' : '+format('%.4f',[fvalue2]);
          end;

        end
        else ts:='';
      except
        ts:='';
      end;

    end else ts:='';

  end else ts:='';

  result:=result{+copy(disassembled,j+1,length(disassembled)-j)}+ts;

  i:=pos(' - ',result);
  address:=uppercase(copy(result,1,i-1));

  inc(i,3);
  j:=PosEx(' - ',result,i);
  if j=0 then j:=length(result)+1;
  bytes:=copy(result,i,(j-i));

  inc(j,3);
  k:=PosEx(' : ',result,j);
  l:=k;
  if k=0 then
    k:=length(result)+1;

  opcode:=copy(result,j,(k-j));

  if l>0 then
  begin
    special:=copy(result,l+3,length(result));
  end else special:='';


end;


function inttohexs(address:dword;chars: integer):string;
var symbol: PImagehlpSymbol;
    disp: dword;
begin
  if symhandler.showsymbols and (chars=8) then
    result:=symhandler.getNameFromAddress(address)
  else
    result:=sysutils.IntToHex(address,chars);
end;

end.



