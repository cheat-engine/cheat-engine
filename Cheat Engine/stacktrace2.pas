unit stacktrace2;

interface

uses windows, classes, symbolhandler, cefuncproc, newkernelhandler;

procedure ce_stacktrace(esp: dword; ebp: dword; eip: dword; stack: PDwordArray; sizeinbytes: integer; trace: tstrings; force4byteblocks: boolean=true);

implementation

procedure ce_stacktrace(esp: dword; ebp: dword; eip: dword; stack: PDwordArray; sizeinbytes: integer; trace: tstrings; force4byteblocks: boolean=true);
{
ce_stacktrace will walk the provided stack trying to figure out functionnames ,passed strings and optional other data
esp must be aligned on a 4 byte boundary the first entry alignment but other entries will try to be forced to 4 byte alignment unless otherwise needed (double, string,...)

if eip is in a known function location this can help with the parameter naming (for the location of ebp)
esp is the first 4 byte of stack
}
var
  alignedstack: pdwordarray;
  i: integer;
  Entries: integer;
  s: string;
begin
  i:=esp mod 4;
  if i>0 then
  begin
    alignedstack:=pdwordarray(dword(stack)+(4-i)); //unalligned
    dec(sizeinbytes,4-i);
  end else alignedstack:=stack;

  Entries:=sizeinbytes div 4;

  i:=0;
  while i<entries do
  begin
    s:=inttohex(alignedstack[i],8);
    inc(i);
  end;
end;


end.
