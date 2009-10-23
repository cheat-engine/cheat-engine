unit stacktrace2;

interface

uses windows, sysutils, classes, symbolhandler, cefuncproc, newkernelhandler, byteinterpreter;

procedure seperatestacktraceline(s: string; var address: string; var bytes: string; var details: string);
procedure ce_stacktrace(esp: dword; ebp: dword; eip: dword; stack: PDwordArray; sizeinbytes: integer; trace: tstrings; force4byteblocks: boolean=true; showmodulesonly: boolean=false; nosystemmodules:boolean=false; maxdepth:integer=0);

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

procedure ce_stacktrace(esp: dword; ebp: dword; eip: dword; stack: PDwordArray; sizeinbytes: integer; trace: tstrings; force4byteblocks: boolean=true; showmodulesonly: boolean=false; nosystemmodules:boolean=false; maxdepth: integer=0);
{
ce_stacktrace will walk the provided stack trying to figure out functionnames ,passed strings and optional other data
esp must be aligned on a 4 byte boundary the first entry alignment but other entries will try to be forced to 4 byte alignment unless otherwise needed (double, string,...)

if eip is in a known function location this can help with the parameter naming (for the location of ebp)
esp is the first 4 byte of stack

nosystemmodules is only in effect when show moduleso only is enabled
}
var
  alignedstack: pdwordarray;
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
  i:=esp mod 4;
  if i>0 then
  begin
    alignedstack:=pdwordarray(dword(stack)+(4-i)); //unalligned
    dec(sizeinbytes,4-i);
    inc(esp,i);
  end else alignedstack:=stack;

  dec(sizeinbytes,sizeinbytes mod 4); //so it's a full boundary

  Entries:=sizeinbytes div 4;

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
        inc(esp, (i-oldi)*4); 
        continue; //skip
      end;
    end
    else
      v:=FindTypeOfData(esp,@alignedstack[i],sizeinbytes-(i*4));

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
        secondary:='"'+pchar(@alignedstack[i])+'"';

        if force4byteblocks then
        begin
          secondary:=copy(secondary,1,4);
          inc(i);
        end
        else
        begin
          inc(i,length(secondary) div 4);
          if (length(secondary) mod 4)>0 then
            inc(i);
        end;
      end;

      vtUnicodeString:
      begin
        pchar(@pbytearray(alignedstack)[sizeinbytes-1])[0]:=#0; //make sure it has an end
        pchar(@pbytearray(alignedstack)[sizeinbytes-2])[0]:=#0;
        secondaryws:=pwidechar(@alignedstack[i]);

        if force4byteblocks then
        begin
          secondaryws:=copy(secondaryws,1,4);
          inc(i);
        end
        else
        begin
          inc(i,(length(secondaryws)*2) div 4);
          if ((length(secondaryws)*2) mod 4)>0 then
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

    inc(esp, (i-oldi)*4);   

  end;
end;


end.
