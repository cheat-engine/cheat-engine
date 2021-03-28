unit rttihelper;

{helper for rtti structures}

{$mode delphi}

interface

uses
  {$ifdef darwin}macport,{$endif}
  Classes, SysUtils;

function getRTTIClassName(StructureBaseAddress: ptruint; var classname: string): boolean;

implementation

uses newkernelhandler, ProcessHandlerUnit, symbolhandler, symbolhandlerstructs;

function isvalidstring(s: string): boolean;
var i: integer;
begin
  result:=length(s)>0;
  for i:=1 to length(s) do
    if (ord(s[i])<32) or (ord(s[i])>126) then exit(false);
end;

function getRTTIClassNamePascal(StructureBaseAddress: ptruint; var classname: string): boolean;
{

vInstanceSize: SizeInt;
vInstanceSize2: SizeInt;
vParentRef: pointer
vClassName: PShortString;
}
var
  vtable: ptruint;
  classnameaddress: ptruint;
  cname: array [0..255] of byte;
  x: ptruint;
  i: integer;
  count: integer;
begin
  result:=false;
  vtable:=0;

  if readprocessmemory(processhandle, pointer(StructureBaseAddress), @vtable,processhandler.pointersize,x) then
  begin
    classnameaddress:=0;
    if readprocessmemory(processhandle, pointer(vtable+processhandler.pointersize*3), @classnameaddress,processhandler.pointersize,x) then
    begin
      //make sure it is a proper shortstring.  (byte, chars[byte],0)
      if readprocessmemory(processhandle, pointer(classnameaddress), @cname[0],256,x) then
      begin
        count:=cname[0];
        if count>0 then
        begin
          for i:=1 to count do
          begin
            if not (char(cname[i]) in ['0'..'9','a'..'z','A'..'Z']) then
              break;
          end;

          //still here
          if (count>0) and (count<255) and (cname[count+1]=0) then
          begin
            classname:=pchar(@cname[1]);
            if length(classname)=count then
              exit(isvalidstring(classname));
          end;
        end;
      end;
    end;
  end;
end;

//based on visual studio 2017 rtti stuff
function getRTTIClassNameVS2017(StructureBaseAddress: ptruint; var classname: string): boolean;
const UNDNAME_NAME_ONLY=$1000;
var
  vtable: ptruint;
  rttiinfo: ptruint;
  x: ptruint;
  mi: tmoduleinfo;

  something: packed record
    somethingtype: dword; //1 for 64-bit it seems
    dword1: dword;
    dword2: dword;
    dwTypeInfo: dword;
  end;

  TypeInfoAddress: ptruint;

  TypeInfo32: packed record
    pointertosomethingelse: dword;
    undecoratednamepointer: dword;
    decoratedname: array [0..255] of char;
  end;

  TypeInfo64: packed record
    pointertosomethingelse: qword;
    undecoratednamepointer: qword;
    decoratedname: array [0..255] of char;
  end;

  decoratedname: string;

  undecoratedstring: array [0..255] of byte;
  i: integer;

  s: string;

  cp: pchar;

  slen: integer;
begin
  result:=false;
  vtable:=0;
  if readprocessmemory(processhandle, pointer(StructureBaseAddress), @vtable,processhandler.pointersize,x) then
  begin
    rttiinfo:=0;
    if readprocessmemory(processhandle, pointer(vtable-processhandler.pointersize), @rttiinfo,processhandler.pointersize, x) then
    begin
      if symhandler.getmodulebyaddress(rttiinfo,mi) then
      begin
        //inside a module                     |||||||||||
        //01 00 00 00 00 00 00 00 00 00 00 00 D8 50 00 00 50 37 00 00 80 37 00 00   = 64-bit
        //00 00 00 00 00 00 00 00 00 00 00 00 90 30 EC 00 88 24 EC 00 00 00 00 00   = 32-bit
        if readprocessmemory(processhandle, pointer(rttiinfo), @something,sizeof(something), x) then
        begin
          if something.somethingtype=1 then //dwTypeInfo is relative to the modulebase
            TypeInfoAddress:=mi.baseaddress+something.dwTypeInfo
          else //dwTypeInfo is the exact address
            TypeInfoAddress:=something.dwTypeInfo;

          //                                    ^^^^^^^^^^^
          //                            undecorated name          decoratedname[1]
          //->48 32 6D 5F F6 7F 00 00   00 00 00 00 00 00 00 00   2E 3F 41 56 74 65 73 74
          //->48 32 6D 22 F7 7F 00 00   00 AE 7A F5 BE 01 00 00   2E 3F 41 56 74 65 73 74
          //->14 21 9E 00               00 00 00 00               2E 3F 41 56 74 65 73 74 32 40 40 00 14 21 9E 00  = 32bit

          cp:=nil;
          if processhandler.is64Bit then
          begin
            if readprocessmemory(processhandle, pointer(TypeInfoAddress), @TypeInfo64,sizeof(TypeInfo64), x) then
            begin
              TypeInfo64.decoratedname[255]:=#0;

              if copy(pchar(@TypeInfo64.decoratedname[0]),1,4)='.?AV' then
                cp:=@TypeInfo64.decoratedname[4]
            end;
          end
          else
          begin
            if readprocessmemory(processhandle, pointer(TypeInfoAddress), @TypeInfo32,sizeof(TypeInfo32), x) then
            begin
              TypeInfo32.decoratedname[255]:=#0;

              if copy(pchar(@TypeInfo32.decoratedname[0]),1,4)='.?AV' then
                cp:=@TypeInfo32.decoratedname[4]
            end;
          end;

          if cp<>nil then //found the string
          begin
            s:='?'+cp;

            {$ifdef windows}

            i:=UnDecorateSymbolName(pchar(s), @undecoratedstring[0],255,UNDNAME_NAME_ONLY);
            undecoratedstring[i]:=0;

            classname:=pchar(@undecoratedstring[0]);
            {$else}
            classname:=cp;
            {$endif}


            if isvalidstring(classname) then exit(true);
            classname:=cp;

            if isvalidstring(classname) then exit(true);

            //everything matches (including the .?AV line), except the name is bad, still useful as an identifier
            classname:='unknown classid ';
            for i:=0 to 15 do
            begin
              if ord(cp[i])=0 then
                break;

              classname:=classname+inttohex(ord(cp[i]),2);
            end;

            exit(true);
          end;
        end;
      end;

    end;
  end;
end;

function getRTTIClassName(StructureBaseAddress: ptruint; var classname: string): boolean;
begin
  result:=false;
  if getRTTIClassNameVS2017(StructureBaseAddress, classname) then exit(True);
  if getRTTIClassNamePascal(StructureBaseAddress, classname) then exit(True);

  //todo: add other compilers
end;

end.

