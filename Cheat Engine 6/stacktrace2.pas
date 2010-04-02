unit stacktrace2;

{$MODE Delphi}

interface

uses LCLIntf, sysutils, classes, symbolhandler, CEFuncProc, NewKernelHandler, byteinterpreter;

procedure seperatestacktraceline(s: string; var address: string; var bytes: string; var details: string);
procedure ce_stacktrace(esp: ptrUint; ebp: ptrUint; eip: ptrUint; stack: PptrUintArray; sizeinbytes: integer; trace: tstrings; force4byteblocks: boolean=true; showmodulesonly: boolean=false; nosystemmodules:boolean=false; maxdepth:integer=0);

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

procedure ce_stacktrace(esp: ptrUint; ebp: ptrUint; eip: ptrUint; stack: PptrUintArray; sizeinbytes: integer; trace: tstrings; force4byteblocks: boolean=true; showmodulesonly: boolean=false; nosystemmodules:boolean=false; maxdepth:integer=0);
{
ce_stacktrace will walk the provided stack trying to figure out functionnames ,passed strings and optional other data
esp must be aligned on a 4 byte boundary the first entry alignment but other entries will try to be forced to 4 byte alignment unless otherwise needed (double, string,...)

if eip is in a known function location this can help with the parameter naming (for the location of ebp)
esp is the first 4 byte of stack

nosystemmodules is only in effect when show moduleso only is enabled
}
var
  alignedstack: pptrUintArray;
  i: integer;
  Entries: integer;


  address: string[8];
  value: string[8];
  secondary: string;
  secondaryws: widestring;
  v: TVariableType;
  t: string;
  oldi: integer;

begin
  i:=esp mod processhandler.pointersize;
  if i>0 then
  begin
    alignedstack:=pptrUintarray(ptrUint(stack)+(processhandler.pointersize-i)); //unalligned
    dec(sizeinbytes,processhandler.pointersize-i);
    inc(esp,i);
  end else alignedstack:=stack;

  dec(sizeinbytes,sizeinbytes mod processhandler.pointersize); //so it's a full boundary

  Entries:=sizeinbytes div processhandler.pointersize;

  i:=0;
  while i<entries do
  begin
    oldi:=i;
    address:=inttohex(esp,8);
    value:=inttohex(alignedstack[i],8);
    v:=vtdword;

    //figure out what alignedstack[i] is.

    if showmodulesonly then
    begin
      if symhandler.inModule(alignedstack[i]) and ((not nosystemmodules) or (not symhandler.inSystemModule(alignedstack[i]))) then
        v:=vtPointer
      else
      begin
        inc(i);
        inc(esp, (i-oldi)*processhandler.pointersize);
        continue; //skip
      end;
    end
    else
      v:=FindTypeOfData(esp,@alignedstack[i],sizeinbytes-(i*processhandler.pointersize));

    case v of
      vtbyte..vtdword:
      begin
        secondary:=inttostr(integer(alignedstack[i]));
        inc(i);
      end;

      vtSingle:
      begin
        secondary:=format('%.2f',[psingle(@alignedstack[i])^]);
        inc(i);
      end;

      vtDouble:
      begin
        secondary:=format('%.2f',[pdouble(@alignedstack[i])^]);
        if force4byteblocks then
          inc(i)
        else
          inc(i,2);
      end;

      vtString:
      begin
        pchar(@pbytearray(alignedstack)[sizeinbytes-1])[0]:=#0; //make sure it has an end
        secondary:=pchar(@alignedstack[i]);

        if force4byteblocks then
        begin
          secondary:=copy(secondary,1,processhandler.pointersize);
          inc(i);
        end
        else
        begin
          inc(i,length(secondary) div processhandler.pointersize);
          if (length(secondary) mod processhandler.pointersize)>0 then
            inc(i);
        end;

        secondary:='"'+secondary+'"';
      end;

      vtUnicodeString:
      begin
        pchar(@pbytearray(alignedstack)[sizeinbytes-1])[0]:=#0; //make sure it has an end
        pchar(@pbytearray(alignedstack)[sizeinbytes-2])[0]:=#0;
        secondaryws:=pwidechar(@alignedstack[i]);

        if force4byteblocks then
        begin
          secondaryws:=copy(secondaryws,1,processhandler.pointersize);
          inc(i);
        end
        else
        begin
          inc(i,(length(secondaryws)*2) div processhandler.pointersize);
          if ((length(secondaryws)*2) mod processhandler.pointersize)>0 then
            inc(i);
        end;

        secondary:='"'+secondaryws+'"';
      end;

      vtPointer:
      begin
        secondary:= symhandler.getNameFromAddress(alignedstack[i],true,true);
        inc(i);
      end;

      else secondary:='';
    end;

    trace.Add(address+' - '+value+' - '+secondary);
    if (maxdepth>0) and (trace.Count>=maxdepth) then exit;

    inc(esp, (i-oldi)*processhandler.pointersize);

  end;
end;


end.
