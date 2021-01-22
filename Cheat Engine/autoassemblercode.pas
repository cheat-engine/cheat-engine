// Copyright Cheat Engine. All Rights Reserved.

(*
replaces {$luacode}/{$ccode} with a call to a safecall routine.
If lua:
This routine then calls a lua function in CE using the ceserver pipe with the saved state.
the lua function wraps the userprovided function with code that sets up the parameters to what the user wishes :"myvarname=RAX somethingelse=RDI"
on return of that lua function the given parameters get written back to the original state, which gets restored on function exit (This is slightly different from {$ccode} which allows specifying reference of val )

If c:
This routine then calls a c-compiled function.  One problem is that the address needs to be known before compilation, so has to be done on the 2nd pass again
*)

unit autoassemblercode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TAutoAssemblerCodePass2Data=record
    //luadata not needed
    cdata: array of record
      name: string;
      linenr: integer;
      cscript: string;
      address: ptruint;
      bytesallocated: integer; //if for some weird unknown reason bytesallocated is smaller than the final script, allocate more somewhere else, recompile (again) and write a jmp <newaddress> at the old location
      references: array of record //cscript code can reference local AA symbols
        name: string;
        address: ptruint; //filled in before pass2
      end;
      targetself: boolean;
    end;
  end;




procedure AutoAssemblerCodePass1(script: TStrings; out dataForPass2: TAutoAssemblerCodePass2Data; syntaxcheckonly: boolean; targetself: boolean);
procedure AutoAssemblerCodePass2(var dataForPass2: TAutoAssemblerCodePass2Data);


implementation

uses windows,ProcessHandlerUnit, symbolhandler, luahandler, lua, lauxlib, lualib, StrUtils,
  Clipbrd, dialogs, lua_server, tcclib, SymbolListHandler, Assemblerunit, NewKernelHandler;


type
  TAACodeType=(ctLua, ctC);
  TLuaCodeParameter=record
    varname: string;
    contextitem: integer;

    {
    ContextItem:
    0: RAX  / EAX
    1: RBX  / EBX
    2: RCX
    3: RDX
    4: RSI
    5: RDI
    6: RSP
    7: RBP
    8: R8
    9: R9
    10: R10
    11: R11
    12: R12
    13: R13
    14: R14
    15: R15
    16..31: ^ as float  (bit 4 is 1)  noted as RAXF, RBXF, RCXF, etc...
    32..47: XMM0..XMM15 (bytetables/bytetablestruct)
    48: XMM0.0 or XMM0.0F (float)
    49: XMM0.1
    50: XMM0.2
    51: XMM0.3
    52..55: XMM1.*
    56..59: XMM2.*
    60..53: XMM3.*
    64..67: XMM4.*
    68..71: XMM5.*
    72..75: XMM6.*
    76..79: XMM7.*
    80..83: XMM8.*
    84..87: XMM9.*
    88..91: XMM10.*
    92..95: XMM11.*
    96..99: XMM12.*
   100..103: XMM13.*
   104..107: XMM14.*
   108..111: XMM15.*
   112: XMM0.0D (double)
   113: XMM0.1D (double)
   114..115: XMM1.*D
   116..117: XMM2.*D
   118..119: XMM3.*D
   120..121: XMM4.*D
   122..123: XMM5.*D
   124..125: XMM6.*D
   126..127: XMM7.*D
   128..129: XMM8.*D
   130..131: XMM9.*D
   132..133: XMM10.*D
   134..135: XMM11.*D
   136..137: XMM12.*D
   138..139: XMM13.*D
   140..141: XMM14.*D
   142..143: XMM15.*D



    }
  end;

  TLuaCodeParams=array of TLuaCodeParameter;

procedure parseLuaCodeParameters(s: string; var output: TLuaCodeParams);
var
  i: integer;
  r,r2,r3: TStringArray;


  varname, regname: string;
  so: TStringSplitOptions;
  o: TLuaCodeParameter;
  xmmnr: integer;
  subnr: integer;
  st: string;
begin
  setlength(output,0);

  r:=s.Split(' ');
  for i:=0 to length(r)-1 do
  begin
    FillByte(o, sizeof(o),0);

    r2:=r[i].Split('=');

    if length(r2)<>2 then
      raise exception.create('Invalid parameter : '+r[i]);

    varname:=r2[0];
    regname:=uppercase(r2[1]);


    o.varname:=varname;


    case regname of
      'EAX','RAX': o.contextitem:=0;
      'EBX','RBX': o.contextitem:=1;
      'ECX','RCX': o.contextitem:=2;
      'EDX','RDX': o.contextitem:=3;
      'ESI','RSI': o.contextitem:=4;
      'EDI','RDI': o.contextitem:=5;
      'ESP','RSP': o.contextitem:=6;
      'EBP','RBP': o.contextitem:=7;
      'R8': o.contextItem:=8;
      'R9': o.contextItem:=9;
      'R10': o.contextItem:=10;
      'R11': o.contextItem:=11;
      'R12': o.contextItem:=12;
      'R13': o.contextItem:=13;
      'R14': o.contextItem:=14;
      'R15': o.contextItem:=15;

      'EAXF','RAXF': o.contextitem:=16;
      'EBXF','RBXF': o.contextitem:=17;
      'ECXF','RCXF': o.contextitem:=18;
      'EDXF','RDXF': o.contextitem:=19;
      'ESIF','RSIF': o.contextitem:=20;
      'EDIF','RDIF': o.contextitem:=21;
      'EBPF','RBPF': o.contextitem:=22;
      'ESPF','RSPF': o.contextitem:=23;
      'R8F': o.contextItem:=24;
      'R9F': o.contextItem:=25;
      'R10F': o.contextItem:=26;
      'R11F': o.contextItem:=27;
      'R12F': o.contextItem:=28;
      'R13F': o.contextItem:=29;
      'R14F': o.contextItem:=30;
      'R15F': o.contextItem:=31;
      else
      begin
        if regname.StartsWith('XMM') then
        begin
          if regname.Contains('.')=false then
          begin
            //xmm bytetable
            xmmnr:=strtoint(regname.Substring(4)); //except on invalid data. That's ok
            o.contextItem:=32+xmmnr;
          end
          else
          begin
            r3:=regname.Split('.');
            if length(r3)<>2 then raise exception.create('Invalid xmm register format (Invalid dot usage)');
            xmmnr:=strtoint(r3[0].Substring(4));

            if (length(r3[1])>2) or (length(r3[1])=0) then raise exception.create('Invalid xmm register format');

            if (length(r3[1])=2) and (not (uppercase(r3[1])[2] in ['D','F'])) then exception.create('Invalid xmm register format (Not F or D)');

            subnr:=strtoint(r3[1][1]);

            if (length(r3[1])=2) and (uppercase(r3[1][2])='D') then
              o.contextItem:=112+xmmnr*2+subnr //XMM*.*D (double)
            else
              o.contextItem:=48+xmmnr*4+subnr; //XMM*.*F or //XMM*.*
          end;
        end;
      end;

    end;

    setlength(output,length(output)+1);
    output[length(output)-1]:=o;
  end;
end;

function AddSafeCallStub(script: TStrings; functionname: string; targetself: boolean):string; //a function that can be called from any stack alignment
begin
  result:='ceinternal_autofree_safecallstub_for_'+functionname;

  script.add('');
  script.insert(0,'alloc(ceinternal_autofree_safecallstub_for_'+functionname+',512)'); //Let's place bets how many people are going to remark that this is what breaks their code and not because they didn't allocate enough memory properly...


  script.add('ceinternal_autofree_safecallstub_for_'+functionname+':');
  if processhandler.is64Bit{$ifdef cpu64} or targetself{$endif} then
  begin
    script.add('pushfq //save flags');
    script.add('push rax');
    script.add('mov rax,rsp');
    script.add('and rsp,fffffffffffffff0   //align stack');

    script.add('sub rsp,2a0 //allocate local space for scratchspace, the registers, and sse registers. And keep alignment');

    script.add('//store state');
    script.add('fxsave qword [rsp+20]');
    script.add('mov [rsp+220],rbx');
    script.add('mov [rsp+228],rcx');
    script.add('mov [rsp+230],rdx');
    script.add('mov [rsp+238],rsi');
    script.add('mov [rsp+240],rdi');
    script.add('mov [rsp+248],rax //rsp');
    script.add('mov [rsp+250],rbp');
    script.add('mov [rsp+258],r8');
    script.add('mov [rsp+260],r9');
    script.add('mov [rsp+268],r10');
    script.add('mov [rsp+270],r11');
    script.add('mov [rsp+278],r12');
    script.add('mov [rsp+280],r13');
    script.add('mov [rsp+288],r14');
    script.add('mov [rsp+290],r15');

    script.add('//[rsp+248]+0=original rax');
    script.add('//[rsp+248]+8=original flags');

    script.add('//call lua function');
    script.add('lea rcx,[rsp+20]  //pointer to the saved state  ([rcx+248-20]+0=rax   [rcx+248-20]+0=flags)');
    script.add('call '+functionname);

    script.add('//restore registers (could have been changed by the function on purpose)');
    script.add('mov r15,[rsp+290]');
    script.add('mov r14,[rsp+288]');
    script.add('mov r13,[rsp+280]');
    script.add('mov r12,[rsp+278]');
    script.add('mov r11,[rsp+270]');
    script.add('mov r10,[rsp+268]');
    script.add('mov r9,[rsp+260]');
    script.add('mov r8,[rsp+258]');
    script.add('mov rbp,[rsp+250]');
    script.add('mov rdi,[rsp+240]');
    script.add('mov rsi,[rsp+238]');
    script.add('mov rdx,[rsp+230]');
    script.add('mov rcx,[rsp+228]');
    script.add('mov rbx,[rsp+220]');

    script.add('fxrstor qword [rsp+20]');

    script.add('mov rsp,[rsp+248] //restore rsp');
    script.add('pop rax');
    script.add('popfq');
    script.add('ret');

  end
  else
  begin
    script.add('pushfd //save flags');
    script.add('push eax');
    script.add('mov eax,esp');
    script.add('and esp,fffffff0   //align stack');
    script.add('sub esp,220 //allocate local space for scratchspace, the registers, and sse registers. And keep alignment');

    script.add('//store state');
    script.add('fxsave [esp]');
    script.add('mov [esp+200],ebx');
    script.add('mov [esp+204],ecx');
    script.add('mov [esp+208],edx');
    script.add('mov [esp+20c],esi');
    script.add('mov [esp+210],edi');
    script.add('mov [esp+214],eax //rsp');
    script.add('mov [esp+218],ebp');

    script.add('//[esp+214]+0=original eax');
    script.add('//[esp+214]+4=original eflags');

    script.add('//call lua function');
    script.add('mov eax,esp'); //just to be safe
    script.add('push eax');
    script.add('call '+functionname);
    script.add('add esp,4');

    script.add('//restore registers (could have been changed by the function on purpose)');
    script.add('mov ebp,[esp+218]');
    script.add('mov edi,[rsp+210]');
    script.add('mov esi,[rsp+20c]');
    script.add('mov edx,[rsp+208]');
    script.add('mov ecx,[rsp+204]');
    script.add('mov ebx,[rsp+200]');

    script.add('fxrstor [rsp]');

    script.add('mov esp,[rsp+214] //restore rsp');
    script.add('pop eax');
    script.add('popfd');
    script.add('ret');
  end;
end;



procedure AutoAssemblerCCodePass2(dataForPass2: TAutoAssemblerCodePass2Data);
//right after the allocs have been done
var
  secondarylist,errorlog: tstringlist;
  i,j: integer;
  bytes: tmemorystream;

  jmpbytes, nopfiller: array of byte;

  sl : TSymbolListHandler;
  newAddress: ptruint;
  syminfo: PCESymbolInfo;
  bw: size_t;

  phandle: THandle;

  _tcc: TTCC;
begin




  secondarylist:=TStringList.create;
  bytes:=tmemorystream.create;
  sl:=TSymbolListHandler.create;
  errorlog:=tstringlist.create;

  try

    for i:=0 to length(dataForPass2.cdata)-1 do
    begin
      if dataForPass2.cdata[i].targetself then
        _tcc:=tccself
      else
        _tcc:=tcc;

      sl.clear;
      bytes.clear;
      secondarylist.Clear;
      errorlog.clear;

      if dataForPass2.cdata[i].targetself then
        phandle:=GetCurrentProcess
      else
        phandle:=processhandle;


      for j:=0 to length(dataForPass2.cdata[i].references)-1 do
        secondarylist.AddObject(dataForPass2.cdata[i].references[j].name, tobject(dataForPass2.cdata[i].references[j].address));

      if _tcc.compileScript(dataForPass2.cdata[i].cscript, dataForPass2.cdata[i].address, bytes, sl, errorlog, secondarylist, dataForPass2.cdata[i].targetself ) then
      begin
        if bytes.Size>dataForPass2.cdata[i].bytesallocated then
        begin
          //this will be a slight memoryleak but whatever
          //allocate 4x the amount of memory needed



          newAddress:=ptruint(VirtualAllocEx(phandle,nil,4*bytes.size,mem_reserve or mem_commit, PAGE_EXECUTE_READWRITE));
          if newAddress<>0 then
          begin
            Assemble('jmp '+inttohex(newaddress,8),dataForPass2.cdata[i].address, jmpbytes); //oldaddress: jmp newAddress
            WriteProcessMemory(phandle, pointer(dataForPass2.cdata[i].address), @jmpbytes[0], length(jmpbytes),bw);
            dataForPass2.cdata[i].address:=newAddress;
            dataForPass2.cdata[i].bytesallocated:=4*bytes.size;

            //try again
            sl.clear;
            bytes.clear;
            secondarylist.Clear;
            errorlog.clear;
            if _tcc.compileScript(dataForPass2.cdata[i].cscript, dataForPass2.cdata[i].address, bytes, sl, errorlog, secondarylist, dataForPass2.cdata[i].targetself )=false then
            begin
              //wtf? something really screwed up here
              VirtualFreeEx(phandle, pointer(newAddress), 0,MEM_FREE);
              raise exception.create('Failure in {$CCode} block at line '+inttostr(dataForPass2.cdata[i].linenr)+' (3th compile failure)');
            end;


            if bytes.Size>dataForPass2.cdata[i].bytesallocated then
            begin
              VirtualFreeEx(phandle, pointer(newAddress), 0,MEM_FREE);
              raise exception.create('Failure in {$CCode} block at line '+inttostr(dataForPass2.cdata[i].linenr)+' (Unexplained and unmitigated code growth)');
            end;
          end
          else
            raise exception.create('Failure allocating memory for the {$CCode} block at line '+inttostr(dataForPass2.cdata[i].linenr));
        end;

        //still here so compilation is within the parameters

        syminfo:=sl.FindSymbol(dataForPass2.cdata[i].name);
        if syminfo<>nil then
        begin

          newaddress:=syminfo^.address;

          if newaddress>dataForPass2.cdata[i].address then
          begin
            //should never happen, but fill with nops in case it does
            setlength(nopfiller,dataForPass2.cdata[i].address-newaddress);
            for j:=0 to length(nopfiller)-1 do
              nopfiller[j]:=$90;

            writeprocessmemory(phandle,pointer(dataForPass2.cdata[i].address),@nopfiller[0],length(nopfiller),bw);
            dataForPass2.cdata[i].address:=newaddress;

          end;

          writeprocessmemory(phandle, pointer(dataForPass2.cdata[i].address), bytes.memory,bytes.Size,bw);
        end
        else
          raise exception.create('Failure finding the proper function for the {$CCode} script at line '+inttostr(dataForPass2.cdata[i].linenr));
      end
      else
        raise exception.create('Failure compiling the {$CCode} script at line '+inttostr(dataForPass2.cdata[i].linenr)+': '+errorlog.Text);
    end;

  finally
    freeandnil(errorlog);
    freeandnil(sl);
    freeandnil(bytes);
    freeandnil(secondarylist);
  end;
end;

procedure AutoAssemblerCCodePass1(script: TStrings; parameters: TLuaCodeParams; var i: integer; out dataForPass2: TAutoAssemblerCodePass2Data; syntaxcheckonly: boolean; targetself: boolean);
var
  j,k: integer;
  s: string;
  endpos, scriptstart, scriptend: integer;
  scriptstartlinenr: integer;

  stubcounter: integer=0;

  cscript, imports, errorlog: tstringlist;
  functionname: string;

  refnr: integer;


  usesXMMType: boolean=false;

  ms: TMemorystream;
  bytesizeneeded: integer;

  _tcc: TTCC;
begin
  if targetself then
    _tcc:=tccself
  else
    _tcc:=tcc;

  setlength(dataforpass2.cdata,0);
  scriptstartlinenr:=ptruint(script.Objects[i]);

  scriptstart:=i;
  j:=i+1;
  while j<script.Count-1 do
  begin
    if uppercase(script[j])='{$ASM}' then
      break;

    inc(j);
  end;
  scriptend:=j;



  cscript:=tstringlist.create;
  functionname:='ceinternal_autofree_cfunction_at_line'+inttostr(ptruint(script.objects[scriptstart]));
  cscript.add('void '+functionname+'(void *parameters)');
  cscript.add('{');
  //load the values from parameters pointer

  if processhandler.is64Bit {$ifdef cpu64}or targetself{$endif} then
  begin
    for j:=0 to length(parameters)-1 do
    begin
      case parameters[j].contextitem of
        0: s:='unsigned long long *'+parameters[j].varname+'=(unsigned long long *)*(unsigned long long *)((unsigned long long)parameters+0x228);'; //RAX
        1..15: s:='unsigned long long *'+parameters[j].varname+'=(unsigned long long*)((unsigned long long)parameters+0x'+inttohex($200+(parameters[j].contextitem-1)*8,1)+');'; //RBX..R15
        16: s:='float *'+parameters[j].varname+'=(float *)((unsigned long long)parameters+0x228);';
        17..31: s:='float *'+parameters[j].varname+'=(float *)((unsigned long long)parameters+0x'+inttohex($200+(parameters[j].contextitem-1)*8,1)+');'; //RBX..R15
        32..47:
        begin
          usesXMMType:=true;
          s:='pxmmreg '+parameters[j].varname+'=(pxmmreg)((unsigned long long)parameters+0x'+inttohex($a0+(parameters[j].contextitem-32)*16,1)+');';
        end;
        48..111: s:='float *'+parameters[j].varname+'=(float *)((unsigned long long)parameters+0x'+inttohex($a0+(parameters[j].contextitem-48)*4,1)+');'; //RBX..R15
        112..143: s:='double *'+parameters[j].varname+'=(double *)((unsigned long long)parameters+0x'+inttohex($a0+(parameters[j].contextitem-112)*8,1)+');';
      end;
      cscript.insert(j+2,s);
    end;
  end
  else
  begin
    for j:=0 to length(parameters)-1 do
    begin
      case parameters[j].contextitem of
        0: s:='unsigned long *'+parameters[j].varname+'=(unsigned long *)*(unsigned long *)((unsigned long)parameters+0x214);'; //EAX
        1..7: s:='unsigned long *'+parameters[j].varname+'=(unsigned long*)((unsigned long)parameters+0x'+inttohex($200+(parameters[j].contextitem-1)*4,1)+');'; //RBX..R15
        16: s:='float *'+parameters[j].varname+'=(float *)((unsigned long)parameters+0x214);';
        17..23: s:='float *'+parameters[j].varname+'=(float *)((unsigned long)parameters+0x'+inttohex($200+(parameters[j].contextitem-1)*4,1)+');'; //EBX..EBP
        32..39:
        begin
          usesXMMType:=true;
          s:='pxmmreg '+parameters[j].varname+'=(pxmmreg)((unsigned long)parameters+0x'+inttohex($a0+(parameters[j].contextitem-32)*16,1)+');';
        end;
        48..79: s:='float *'+parameters[j].varname+'=(float *)((unsigned long)parameters+0x'+inttohex($a0+(parameters[j].contextitem-48)*4,1)+');'; //EBX..EBP
        112..127: s:='double *'+parameters[j].varname+'=(double *)((unsigned long)parameters+0x'+inttohex($a0+(parameters[j].contextitem-112)*8,1)+');';
      end;
      cscript.insert(j+2,s);
    end;
  end;

  for j:=scriptstart+1 to scriptend-1 do
    cscript.add(script[j]);

  cscript.add('}');

  if usesxmmtype then
  begin
    //insert the xmm type
    cscript.insert( 0,'typedef struct {');
    cscript.insert( 1,'  union{');
    cscript.insert( 2,'    struct{');
    cscript.insert( 3,'        float f0;');
    cscript.insert( 4,'        float f1;');
    cscript.insert( 5,'        float f2;');
    cscript.insert( 6,'        float f3;');
    cscript.insert( 7,'    };');
    cscript.insert( 8,'    struct{');
    cscript.insert( 9,'        double d0;');
    cscript.insert(10,'        double d1;');
    cscript.insert(11,'    };');
    cscript.insert(12,'    float fa[4];');
    cscript.insert(13,'    double da[2];');
    cscript.insert(14,'  };');
    cscript.insert(15,'} xmmreg, *pxmmreg;');
  end;

  //debug:
 // clipboard.AsText:=cscript.text;
  //  showmessage(luascript.text);
  //^^^^^^^^^

  //compile the function and get the needed size and fill in the data for pass 2
  ms:=TMemoryStream.Create;
  errorlog:=tstringlist.create;
  imports:=tstringlist.create;

  try
    bytesizeneeded:=0;
    if _tcc.testcompileScript(cscript.text, bytesizeneeded,imports,errorlog,targetself)=false then
      raise exception.create('Error at {$CCode} block starting at '+inttostr(scriptstartlinenr)+' error:'+errorlog.text );

    j:=length(dataforpass2.cdata);
    setlength(dataforpass2.cdata,j+1);
    dataforpass2.cdata[j].name:=functionname;
    dataforpass2.cdata[j].linenr:=scriptstartlinenr;
    dataforpass2.cdata[j].cscript:=cscript.text;
    dataforpass2.cdata[j].bytesallocated:=max(32,align(ptruint(bytesizeneeded*2),16));
    dataforpass2.cdata[j].targetself:=targetself;
    setlength(dataforpass2.cdata[j].references, imports.Count);

    //fill the functions this script referenced by name
    for k:=0 to imports.count-1 do
      dataforpass2.cdata[j].references[k].name:=imports[k];

  finally
    ms.free;
    imports.free;
    errorlog.free;
  end;

  script.insert(0, 'alloc('+functionname+','+inttostr(dataforpass2.cdata[j].bytesallocated)+')');
  inc(scriptstart,1);
  inc(scriptend,1);

  //create a safecall stub to call this routine with a simple call (note, could be a 16 byte call so beware of that)
  s:=AddSafeCallStub(script, functionname, targetself);
  inc(scriptstart,1);
  inc(scriptend,1);


  //and finally replace the c block with a call to the safecallstub
  for j:=scriptend downto scriptstart+1 do
    script.Delete(j);

  script[scriptstart]:='call '+s;

 // clipboard.AsText:=script.text;
end;

procedure AutoAssemblerLuaCodePass(script: TStrings; parameters: TLuaCodeParams; var i: integer; syntaxcheckonly: boolean);
var
  j,k: integer;
  s: string;
  endpos, scriptstart, scriptend: integer;
  luascript: tstringlist;

  linenr: integer;
  refnr: integer;

  hasAddedLuaServerCode: boolean=false;
  hasAddedAsmStatement: boolean=false;

begin
  //search for {$LUACODE/
  linenr:=ptruint(script.Objects[i]);
  scriptstart:=i;

  j:=i+1;
  while j<script.Count do
  begin
    if uppercase(script[j])='{$ASM}' then
      break;

    inc(j);
  end;

  scriptend:=j;


  luascript:=tstringlist.Create;
  for j:=i+1 to scriptend-1 do
    luascript.add(script[j]);

  if luaL_loadstring(luavm, pchar(luascript.text))=0 then
    lua_pop(LuaVM,1)
  else
  begin
    s:=lua_tostring(LuaVM,-1);
    lua_pop(LuaVM,1);

   // clipboard.AsText:=luascript.text;

    raise exception.create('Invalid lua code in {$LUACODE} block at line '+inttostr(linenr)+' :'+s);
  end;

  //parse s for the parameters that it wants



  luascript.insert(0,'return createRef(function(parameters)');
  //load the values from parameters pointer

  if processhandler.is64bit then
  begin
    for j:=0 to length(parameters)-1 do
    begin
      s:='local '+parameters[j].varname+'=';

      case parameters[j].contextitem of
        0: s:=s+'readPointer(readPointer(parameters+0x228))'; //RAX
        1..15: s:=s+'readPointer(parameters+0x'+inttohex($200+(parameters[j].contextitem-1)*8,1)+')'; //RBX..R15
        16: s:=s+'readFloat(readPointer(parameters+0x228))'; //RAX as float
        17..31: s:=s+'readFloat(parameters+0x'+inttohex($200+(parameters[j].contextitem-17)*8,1)+')'; //RBX..R15 as float
        32..47: s:=s+'readBytes(parameters+0x'+inttohex($a0+(parameters[j].contextitem-32)*16,1)+',16,true)';
        48..111: s:=s+'readFloat(parameters+0x'+inttohex($a0+(parameters[j].contextitem-48)*4,1)+')';
        112..143: s:=s+'readDouble(parameters+0x'+inttohex($a0+(parameters[j].contextitem-112)*8,1)+')';
      end;

      luascript.insert(j+1,s);
    end;

  end
  else
  begin
    for j:=0 to length(parameters)-1 do
    begin
      s:='local '+parameters[j].varname+'=';
      case parameters[j].contextitem of
        0: s:=s+'readPointer(readPointer(parameters+0x214))'; //RAX
        1..7: s:=s+'readPointer(parameters+0x'+inttohex($200+(parameters[j].contextitem-1)*4,1)+')'; //RBX..R15
        16: s:=s+'readFloat(readPointer(parameters+0x214))'; //RAX as float
        17..23: s:=s+'readFloat(parameters+0x'+inttohex($200+(parameters[j].contextitem-17)*4,1)+')'; //RBX..R15 as float
        32..39: s:=s+'readBytes(parameters+0x'+inttohex($a0+(parameters[j].contextitem-32)*16,1)+',16,true)';
        48..79: s:=s+'readFloat(parameters+0x'+inttohex($a0+(parameters[j].contextitem-48)*4,1)+')';
        112..127: s:=s+'readDouble(parameters+0x'+inttohex($a0+(parameters[j].contextitem-112)*8,1)+')';
      end;
      luascript.insert(j+1,s);
    end;

  end;


  //end of the script: write the values back
  if processhandler.is64bit then
  begin
    for j:=0 to length(parameters)-1 do
    begin
      case parameters[j].contextitem of
        0: s:='writePointer(readPointer(parameters+0x228),'+parameters[j].varname+')';
        1..15: s:='writePointer(parameters+0x'+inttohex($200+(parameters[j].contextitem-1)*8,1)+','+parameters[j].varname+')'; //RBX..R15
        16: s:='writeFloat(readPointer(parameters+0x228),'+parameters[j].varname+')'; //RAX as float
        17..31: s:='writeFloat(parameters+0x'+inttohex($200+(parameters[j].contextitem-17)*8,1)+','+parameters[j].varname+')'; //RBX..R15 as float
        32..47: s:='writeBytes(parameters+0x'+inttohex($a0+(parameters[j].contextitem-32)*16,1)+','+parameters[j].varname+')';
        48..111: s:='writeFloat(parameters+0x'+inttohex($a0+(parameters[j].contextitem-48)*4,1)+','+parameters[j].varname+')';
        112..143: s:='writeDouble(parameters+0x'+inttohex($a0+(parameters[j].contextitem-112)*8,1)+','+parameters[j].varname+')';
      end;

      luascript.add(s);

    end;

  end
  else
  begin
    for j:=0 to length(parameters)-1 do
    begin
      case parameters[j].contextitem of
        0: s:='writePointer(readPointer(parameters+0x214),'+parameters[j].varname+')';
        1..7: s:='writePointer(parameters+0x'+inttohex($200+(parameters[j].contextitem-1)*4,1)+','+parameters[j].varname+')'; //EBX..EBP
        16: s:='writeFloat(readPointer(parameters+0x214),'+parameters[j].varname+')'; //EAX as float
        17..23: s:='writeFloat(parameters+0x'+inttohex($200+(parameters[j].contextitem-17)*4,1)+','+parameters[j].varname+')'; //EBX..EBP as float
        32..39: s:='writeBytes(parameters+0x'+inttohex($a0+(parameters[j].contextitem-32)*16,1)+','+parameters[j].varname+')';
        48..79: s:='writeFloat(parameters+0x'+inttohex($a0+(parameters[j].contextitem-48)*4,1)+','+parameters[j].varname+')';
        112..127: s:='writeDouble(parameters+0x'+inttohex($a0+(parameters[j].contextitem-112)*8,1)+','+parameters[j].varname+')';
      end;

      luascript.add(s);

    end;
  end;

  luascript.add('return end )');

  if lua_dostring(luavm, pchar(luascript.Text))=0 then
  begin
    refnr:=lua_tointeger(LuaVM,-1);
    lua_pop(LuaVM,1);

    if syntaxcheckonly then  //remove this reference
      lua_unref(LuaVM, refnr);
  end
  else
  begin
    s:=lua_tostring(LuaVM,-1);
    lua_pop(LuaVM,1);
    raise exception.create('Invalid lua code in internal {$LUACODE} block at line '+inttostr(linenr)+' :'+s);
  end;




  //add the assemble function used to call the lua code, no need to save registers, it's called by a safecall stub, just pass param1 to lua
  script.insert(0,'alloc(ceinternal_autofree_luacallstub_at'+inttostr(linenr)+',64)');

  inc(scriptstart,1);
  inc(scriptend,1);

  script.add('ceinternal_autofree_luacallstub_at'+inttostr(linenr)+':');

  if processhandler.is64Bit then
  begin
    script.add('sub rsp,28'); //scratchspace, and parameter save. Also aligns the stack
    script.add('mov [rsp+20],rcx');  //save the parameter pointer

    script.add('mov ecx,'+inttohex(refnr,1));
    script.add('mov edx,1'); //1 parameteer
    script.add('lea r8,[rsp+20]'); //the address where the parameter pointer is stored
    script.add('mov r9,1');
    script.add('call CELUA_ExecuteFunctionByReference');
    script.add('add rsp,28');
    script.add('ret');
  end
  else
  begin
    script.add('lea eax,[esp+4]'); //esp=return address, esp+4=param1
    //[ebp]=old ebp
    //[ebp+4]=return address
    //[ebp+8]=param1 (parameters)
    script.add('push 1');
    script.add('push eax'); //push pointer to param1
    script.add('push 1'); //1 parameter
    script.add('push '+inttohex(refnr,1));
    script.add('call CELUA_ExecuteFunctionByReference');
    script.add('ret');
  end;



  //create a safecall stub to call this routine with a simple call (note, could be a 16 byte call so beware of that)
  s:=AddSafeCallStub(script, 'ceinternal_autofree_luacallstub_at'+inttostr(linenr),false);
  inc(scriptstart,1); //also inserts an alloc at top
  inc(scriptend,1);

  //and finally replace the luacode block with a call to the safecallstub
  for j:=scriptend downto scriptstart+1 do
  begin
    script.Delete(j);
  end;

  script[scriptstart]:='call '+s;

  //clipboard.AsText:=script.text;
  //debug^^^

end;

procedure AutoAssemblerCodePass2(var dataForPass2: TAutoAssemblerCodePass2Data);
begin
  AutoAssemblerCCodePass2(dataForPass2);
end;

procedure AutoAssemblerCodePass1(script: TStrings; out dataForPass2: TAutoAssemblerCodePass2Data; syntaxcheckonly: boolean; targetself: boolean);
//this way the script only needs to be parsed once for quite similar code
var
  i: integer;
  endpos: integer;
  uppercaseline: string;
  s: string;
  parameterstring: string;
  parameters:  TLuaCodeParams;
  hasAddedLuaServerCode: boolean=false;
begin
  i:=0;
  while i<script.count do
  begin
    s:=script[i];
    if (length(s)>=7) and (s[1]='{') and (s[2]='$') and (s[3] in ['l','L','c','C']) and (s[4] in ['u','U','c','C']) then  //{$CC or {$LU
    begin
      endpos:=pos('}',s);
      if endpos=-1 then
        raise exception.create('Invalid command line at '+inttostr(ptrUint(script.Objects[i]))+' ('+script[i]+')');

      uppercaseline:=uppercase(script[i]);




      if copy(uppercaseline,1,9)='{$LUACODE' then
      begin
        if targetself or (processid=GetCurrentProcessId) then
          raise exception.create('{$LUACODE} blocks can not be used inside CE');

        parameterstring:=copy(s,11,endpos-11);
        setlength(parameters,0);
        parseLuaCodeParameters(parameterstring, parameters);

        if (syntaxcheckonly=false) and (hasAddedLuaServerCode=false) then
        begin
          //add the code that runs and configures the luaserver
          if luaserverExists('CELUASERVER'+inttostr(getcurrentprocessid))=false then
            tluaserver.create('CELUASERVER'+inttostr(getcurrentprocessid));


          if processhandler.is64Bit then
            script.insert(0,'loadlibrary(luaclient-x86_64.dll)')
          else
            script.insert(0,'loadlibrary(luaclient-i386.dll)');

          script.insert(1,'CELUA_ServerName:');
          script.insert(2,'db ''CELUASERVER'+inttostr(getcurrentprocessid)+''',0');
          inc(i,3);

          hasAddedLuaServerCode:=true;
        end;

        AutoAssemblerLuaCodePass(script, parameters, i, syntaxcheckonly)
      end
      else
      if copy(uppercaseline,1,7)='{$CCODE' then
      begin
        parameterstring:=copy(s,9,endpos-9);
        setlength(parameters,0);
        parseLuaCodeParameters(parameterstring, parameters);
        AutoAssemblerCCodePass1(script, parameters, i, dataForPass2, syntaxcheckonly, targetself);
      end;

    end;

    inc(i);
  end;
end;

end.

