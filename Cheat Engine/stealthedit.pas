unit stealthedit;

interface

uses windows, sysutils, strutils, assemblerunit, disassembler, symbolhandler, cefuncproc, newkernelhandler;

type TStealthEdit=class
  private
    relocations: array of record
      baseaddress: dword;
      size: integer;
    end;
  public
    constructor create;
    function ManualStartEdit(address: dword; relocationpagebase: dword; size: integer): dword;
    function StartEdit(address: dword; size: integer): dword;
    function RestoreEdit(address: dword): boolean;
    function isRelocated(address: dword): boolean;     
  end;

var stealtheditor: TStealthEdit;
  
implementation



constructor TStealthEdit.create;
begin
  stealthedit_InitializeHooks;
end;

function TStealthEdit.isRelocated(address: dword): boolean;
var i: integer;
begin
  result:=false;
  for i:=0 to length(relocations)-1 do
  begin
    if (address>relocations[i].baseaddress) and (address<(relocations[i].baseaddress+relocations[i].size)) then
    begin
      result:=true;
      exit;
    end;
  end;
end;

function TStealthEdit.ManualStartEdit(address: dword; relocationpagebase: dword; size: integer): dword;
begin
  //sooo, the big bad user knows how to do it better than my automated method?
  //well then, let's see if he's right
  if stealthedit_AddCloakedSection(processid, address, relocationpagebase, size) then
  begin
    result:=relocationpagebase;
    setlength(relocations,length(relocations)+1);
    relocations[length(relocations)-1].baseaddress:=address;
    relocations[length(relocations)-1].size:=size;    
  end
  else
    result:=0;


end;

function TStealthEdit.StartEdit(address: dword; size: integer): dword;
var relocationpagebase: dword;
    cloaksize: dword;

    tempbuf: pbytearray;
    baseaddress: dword;
    actualread,actualwritten: dword;

    prologue: dword;
    epilogue: dword;
    tempaddress, prev: dword;
    s: string;
    i: integer;
    a,b: string;
    x: dword;
    oldshowmodules: boolean;
    oldshowsymbols: boolean;
    bt: TassemblerBytes;

begin
  baseaddress:=address and $fffff000; //should already be aligned, but let's check anyhow
  cloaksize:=(1+((size -1) div 4096)) * 4096;

  //allocate new mem location
  relocationpagebase:=dword(VirtualAllocEx(processhandle, nil, cloaksize+8192, mem_commit, page_execute_readwrite));
  

  tempbuf:=VirtualAlloc(nil, cloaksize+8192, mem_commit, PAGE_READWRITE);
  FillMemory(tempbuf, cloaksize+8192, $cc);

  //and now the internal code
  ReadProcessMemory(processhandle, pointer(baseaddress), @tempbuf[4096], cloaksize, actualread);

  //now add in the first and last incomplete instruction
  prologue:=previousopcode(baseaddress);
  ReadProcessMemory(processhandle, pointer(prologue), @tempbuf[4096-baseaddress-prologue], baseaddress-prologue, actualread);

  epilogue:=baseaddress+cloaksize-20;
  while epilogue<(baseaddress+cloaksize+5) do
    disassemble(epilogue);

  ReadProcessMemory(processhandle, pointer(baseaddress+cloaksize), @tempbuf[4096+baseaddress+cloaksize], epilogue-(baseaddress+cloaksize), actualread);

  //adjust the long jumps
  oldshowmodules:=symhandler.showmodules;
  oldshowsymbols:=symhandler.showsymbols;
  symhandler.showmodules:=false;
  symhandler.showsymbols:=false;

  tempaddress:=prologue;
  while (tempaddress<epilogue) do
  begin
    prev:=tempaddress;
    s:=disassemble(tempaddress);
    i:=posex('-',s);
    i:=posex('-',s,i+1);
    s:=copy(s,i+2,length(s));

    i:=pos(' ',s);
    a:=copy(s,1,i-1);
    b:=copy(s,i+1,length(s));

    if length(a)>1 then
    begin
      if (lowercase(a)='loop') or (lowercase(a[1])='j') or (lowercase(a)='call') then
      begin
        try
          x:=strtoint('$'+b); //raises exception if not a normal hex address notation  (e.g register and address specifier)
          
          if not ((x>=(baseaddress-4095)) and (x<(baseaddress+cloaksize+4095))) then //jump that's not within the region
          begin
            //has to be a long jump,
            setlength(bt,0);
            Assemble(a+' '+b, relocationpagebase+4096+(prev-baseaddress), bt);
            if length(bt)<(tempaddress-prev) then
            begin
              //should never happen...
              i:=length(bt);
              setlength(bt,(tempaddress-prev));
              while i<length(bt) do
              begin
                bt[i]:=$90; //fill with nops
                inc(i);
              end;

            end;

            if length(bt)=(tempaddress-prev) then
            begin
              for i:=0 to length(bt)-1 do
              begin
                tempbuf[prev-baseaddress+4096+i]:=bt[i];
              end;
            end else
            begin
              Raise Exception.Create('Error at address '+inttohex(prev,8)+' the relocator can''t make a fitting replacement for this. You could try it manually');
            end;
          end;
        except
          //nolabel (e.g address specifier)
        end;
      end;
    end;

  end;

  symhandler.showmodules:=oldshowmodules;
  symhandler.showsymbols:=oldshowsymbols;

  //and write to the new memory location
  WriteProcessMemory(processhandle, pointer(relocationpagebase), tempbuf, cloaksize+8192, actualwritten);

  if stealthedit_AddCloakedSection(processid, address, relocationpagebase, cloaksize) then
  begin
    result:=relocationpagebase;

    setlength(relocations,length(relocations)+1);
    relocations[length(relocations)-1].baseaddress:=address;
    relocations[length(relocations)-1].size:=cloaksize;

  end
  else
  begin
    //failure ?
    //free copy, it's useless now
    VirtualFreeEx(processhandle, pointer(relocationpagebase), cloaksize+8192, MEM_RELEASE);
    result:=0;
  end;
end;

function TStealthEdit.RestoreEdit(address: dword): boolean;
var i,j: integer;
begin
  result:=false;
  for i:=0 to length(relocations)-1 do
  begin
    if relocations[i].baseaddress=address then
    begin
      result:=stealthedit_RemoveCloakedSection(processid, address);
      if result then
      begin
        //shift items
        for j:=i to length(relocations)-2 do
          relocations[j]:=relocations[j+1];

        setlength(relocations,length(relocations)-1);
      end;
    end;
  end;
end;

end.
