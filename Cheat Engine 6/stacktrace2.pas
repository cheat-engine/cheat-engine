unit stacktrace2;

{$MODE Delphi}

interface

uses LCLIntf, sysutils, classes, symbolhandler, CEFuncProc, NewKernelHandler, byteinterpreter;

procedure seperatestacktraceline(s: string; var address: string; var bytes: string; var details: string);
procedure ce_stacktrace(esp: ptrUint; ebp: ptrUint; eip: ptrUint; stack: Pbytearray; sizeinbytes: integer; trace: tstrings; force4byteblocks: boolean=true; showmodulesonly: boolean=false; nosystemmodules:boolean=false; maxdepth:integer=0);

implementation

uses StrUtils;

procedure seperatestacktraceline(s: string; var address: string; var bytes: string; var details: string);
var i,j: integer;
begin
  //has 2 ' - ' occurances
  i:=pos(' - ',s);
  address:=copy(s,1,i-1);

  j:=posex(' - ',s,i+3);
  bytes:=copy(s,i+3,j-(i+3));

  details:=copy(s,j+3,length(s)); //leftover
end;

procedure ce_stacktrace(esp: ptrUint; ebp: ptrUint; eip: ptrUint; stack: pbytearray; sizeinbytes: integer; trace: tstrings; force4byteblocks: boolean=true; showmodulesonly: boolean=false; nosystemmodules:boolean=false; maxdepth:integer=0);
{
ce_stacktrace will walk the provided stack trying to figure out functionnames ,passed strings and optional other data
esp must be aligned on a 4 byte boundary the first entry alignment but other entries will try to be forced to 4 byte alignment unless otherwise needed (double, string,...)

if eip is in a known function location this can help with the parameter naming (for the location of ebp)
esp is the first 4 byte of stack

nosystemmodules is only in effect when show moduleso only is enabled
}
var
  alignedstack: pbytearray;
  alignedstack32: PDwordArray absolute alignedstack;
  alignedstack64: Puint64Array absolute alignedstack;
  i: integer;
  Entries: integer;

  currentStackpos: ptruint;
  currentStackValue: ptruint;


  address: string;
  value: string;
  secondary: string;
  v: TVariableType;
  oldi: integer;

begin
  i:=esp mod processhandler.pointersize;
  if i>0 then
  begin
    //unalligned
    alignedstack:=stack+(processhandler.pointersize-i);
    dec(sizeinbytes,processhandler.pointersize-i);
    inc(esp,i);
  end else alignedstack:=stack;

  dec(sizeinbytes,sizeinbytes mod processhandler.pointersize); //so it's a full boundary

  Entries:=sizeinbytes div processhandler.pointersize;

  i:=0;
  while i<entries do
  begin
    oldi:=i;
    inc(i);

    address:=inttohex(esp,8);


    if processhandler.is64bit then
    begin
      currentStackpos:=ptruint(@alignedstack64[i]);
      value:=inttohex(alignedstack64[i],16);
      currentStackValue:=ptruint(alignedstack64[i]);
    end
    else
    begin
      currentStackpos:=ptruint(@alignedstack32[i]);
      value:=inttohex(alignedstack32[i],8);
      currentStackValue:=ptruint(alignedstack32[i]);
    end;



    v:=vtdword;

    //figure out what alignedstack[i] is.

    if showmodulesonly then
    begin
      if symhandler.inModule(currentStackValue) and ((not nosystemmodules) or (not symhandler.inSystemModule(currentStackValue))) then
        v:=vtPointer
      else
      begin
        inc(i);
        inc(esp, (i-oldi)*processhandler.pointersize);
        continue; //skip
      end;
    end
    else
      v:=FindTypeOfData(esp,pointer(currentstackpos),sizeinbytes-(i*processhandler.pointersize));

    if v in [vtbyte..vtQword] then
    begin
      //override into pointersize type
      if processhandler.is64bit then
        v:=vtQword
      else
        v:=vtDword;
    end;

    secondary:=DataToString(@currentStackValue,processhandler.pointersize, v);
    //optional: find the lengts of a string and increase i with the number of stack elements used in the string
    inc(i);


    trace.Add(address+' - '+value+' - '+secondary);
    if (maxdepth>0) and (trace.Count>=maxdepth) then exit;

    inc(esp, (i-oldi)*processhandler.pointersize);

  end;
end;


end.
