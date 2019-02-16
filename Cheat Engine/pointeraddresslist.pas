unit pointeraddresslist;

{$mode delphi}

{
the pointeraddresslist will hold a map of all addresses that contain an pointer
and the value they hold.
It's similar to the reversepointerlist, with the exception that I expect this to
eat a TON more RAM since there are more addresses with a pointer then there are
unique pointer values (multiple addresses point to the same address)

Also, this first implementation will make use of the default available maps object
}

interface

uses
  Classes, SysUtils, maps, ComCtrls, bigmemallochandler, CEFuncProc;

resourcestring
  rsPALInvalidScandataFile = 'Invalid scandata file';
  rsPALInvalidScandataVersion = 'Invalid scandata version';

type
  TPointerListHandler=class
  private
    bma: TBigMemoryAllocHandler;
    pmap: TMap;
    modulelist: tstringlist;
    modulebases: array of ptruint;
    is32bit: boolean;
  public

    procedure reorderModuleIdList(ml: tstrings);  //changes the order of modules so they match the provided list (addresses stay unchanged of course)
    function getAddressFromModuleIndexPlusOffset(moduleindex: integer; offset: integer): ptruint;
    function getPointer(address: ptruint): ptruint; //return 0 if not found
    constructor createFromStream(s: TStream; progressbar: tprogressbar=nil);
    destructor destroy; override;
  end;

implementation

uses pointervaluelist;

function TPointerListHandler.getAddressFromModuleIndexPlusOffset(moduleindex: integer; offset: integer): ptruint;
begin
  if moduleindex>=0 then
    result:=modulebases[moduleindex]+offset
  else
    result:=offset; //if moduleindex=-1 it's an non module base
end;

function TPointerListHandler.getPointer(address: ptruint): ptruint;
var
  base: ptruint;
  pbase: PByteArray;
begin
  base:=address and qword(not qword($ffff));
  if pmap.GetData(base, pbase) then
  begin
    if is32bit then
      result:=pdword(@pbase[address-base])^
    else
      result:=pqword(@pbase[address-base])^
  end
  else
    result:=0;
end;

procedure TPointerListHandler.reorderModuleIdList(ml: tstrings);
//sorts the modulelist based on the given modulelist
var
  oldindex: integer;
  oldmodulename: string;
  oldaddress :ptruint;
  i: integer;
begin

  //example:
  //in ml: game.exe is loaded at     20000000 (module 1)
  //in modulelist: game is loaded at 30000000 (module 2)

  //during the scan the moduleid's of ml are requested but they need to return the address as it was
  //so if the pscan requests moduleid1+10 it must return 30000010


  //make room for the list if it's smaller
  while modulelist.Count<ml.count do
  begin
    i:=modulelist.add('missing');
    setlength(modulebases, length(modulebases)+1);
    modulebases[length(modulebases)-1]:=0;
  end;

  for i:=0 to ml.count-1 do
  begin
    //find this module in the current module list
    oldindex:=modulelist.IndexOf(ml[i]);

    if oldindex=-1 then //it wasn'tin the list, add it with address 0
    begin
      oldindex:=modulelist.Add(ml[i]);
      setlength(modulebases, length(modulebases)+1);
      modulebases[length(modulebases)-1]:=0;
    end;

    if oldindex<>i then
    begin
      //swap
      oldaddress:=modulebases[oldindex];
      oldmodulename:=modulelist[oldindex];

      modulebases[oldindex]:=modulebases[i];
      modulelist[oldindex]:=modulelist[i];

      modulebases[i]:=oldaddress;
      modulelist[i]:=oldmodulename;
    end;

  end;

end;

constructor TPointerListHandler.createFromStream(s: TStream; progressbar: tprogressbar=nil);
var
  i,x: integer;
  mlistlength: integer;
  mname: pchar;
  count: qword;
  totalcount: qword;

  ml: dword;
  value: ptruint;
  nrofpointers: integer;

  lastupdate: qword;
  address: qword;
  base: qword;
  offset: dword;
  pbase: pbytearray;
  limit: integer;
begin
  //create and fill in the pointerlist based on a reversepointerlist
  if s.ReadByte<>$ce then
    raise exception.create(rsPALInvalidScandataFile);

  if s.ReadByte<>ScanDataVersion then raise exception.create(rsPALInvalidScandataVersion);



  bma:=TBigMemoryAllocHandler.create;
  pmap:=TMap.Create(ituPtrSize, sizeof(ptruint));


  modulelist:=TStringList.create;
  modulelist.CaseSensitive:=false;
  mlistlength:=s.ReadDWord;
  setlength(modulebases, mlistlength);

  for i:=0 to mlistlength-1 do
  begin
    x:=s.ReadDWord;
    getmem(mname, x+1);
    s.ReadBuffer(mname^, x);
    mname[x]:=#0;

    modulebases[i]:=s.ReadQWord; //seperate array for quicker lookup
    modulelist.AddObject(mname, tobject(modulebases[i]));
    FreeMemAndNil(mname);
  end;

  if s.ReadByte=1 then  //specific base as static only
  begin
    s.ReadQWord; //basestart
    s.ReadQWord; //basestop
  end;


  ml:=s.ReadDWord; //maxlevel (for determining if 32 or 64-bit)

  if ml=7 then
  begin
    is32bit:=true;
    limit:=65536-sizeof(dword);
  end
  else
  begin
    is32bit:=false;
    limit:=65536-sizeof(qword);
  end;

  totalcount:=s.ReadQWord;
  count:=0;


  if progressbar<>nil then
  begin
    progressbar.position:=0;
    progressbar.Max:=100;
  end;

  lastupdate:=GetTickCount64;

  while count<totalcount do
  begin
    value:=ptruint(s.ReadQWord);



    nrofpointers:=s.ReadDWord;
    for i:=0 to nrofpointers-1 do
    begin
      address:=s.ReadQWord;
      base:=address and qword(not qword($ffff));

      if not pmap.GetData(base, pbase) then
      begin
        pbase:=bma.alloc(65536);
        pmap.Add(base, pbase);
      end;

      if (address-base)<limit then
      begin
        if is32bit then
          PDWord(@pbase[address-base])^:=value
        else
          PQWord(@pbase[address-base])^:=value
      end;
      //todo: else add some overlap support. But for now, only allow alligned pointers


      if s.ReadByte=1 then //has static info
        s.ReadQWord; //discard data
    end;

    inc(count, nrofpointers);

    if (progressbar<>nil) and (gettickcount64>lastupdate+1000) then
    begin
      progressbar.position:=trunc(count/totalcount*100);
      lastupdate:=GetTickCount64;
    end;
  end;

end;

destructor TPointerListHandler.destroy;
begin
  if pmap<>nil then
    pmap.free;

  bma.free;

  inherited destroy;
end;

end.

