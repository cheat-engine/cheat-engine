unit PageMap;
{
A container specifically for storing and looking up pages
}

{$mode delphi}

interface

uses
  {$ifdef windows}
  windows, Classes, SysUtils;
  {$endif}
  {$ifdef darwin}
  macport, Classes, SysUtils;
  {$endif}

type
  TPageInfo=record
    data: PByteArray;
  end;
  PPageInfo=^TPageInfo;

  PPageEntryTable=^TPageEntryTable;
  PPageEntryArray=^TPageEntryArray;
  TPageEntryTable=record
    case integer of
      1: (pageinfo: PPageInfo); //if this is the last level (maxlevel) this is an PPointerList
      2: (PageEntryArray: PPageEntryArray);   //else it's a PReversePointerListArray
  end;
  TPageEntryArray=array [0..15] of TPageEntryTable;


  TPageMap=class
  private
    level0list: TPageEntryArray;
    maxlevel: integer;
    procedure DeletePath(list: PPageEntryArray; level: integer);
  public
    function Add(pageindex: integer; pagedata: pointer): PPageInfo;
    function GetPageInfo(pageindex: integer): PPageInfo;
    constructor create;
    destructor destroy; override;
  end;

implementation

function TPagemap.GetPageInfo(pageindex: integer): PPageInfo;
var
  level: integer;
  maxlevel: integer;
  currentarray: PPageEntryArray;
  entrynr: integer;
begin
  result:=nil;
  maxlevel:=self.maxlevel;
  currentarray:=@level0list;

  level:=0;

  while level<maxlevel do
  begin

    entrynr:=pageindex shr ((maxlevel-level)*4) and $f;
    if currentarray[entrynr].PageEntryArray=nil then exit; //not found

    currentarray:=currentarray[entrynr].PageEntryArray;
    inc(level);
  end;

  //got till level (maxlevel)
  entrynr:=pageindex shr ((maxlevel-level)*4) and $f;
  result:=currentarray[entrynr].pageinfo;   //can be nil
end;

function TPagemap.Add(pageindex: integer; pagedata: pointer): PPageInfo;
{
add a page to the map
precondition: only one thread can call this at a time. Not thread safe
}
var
  level: integer;
  maxlevel: integer;
  temp, currentarray: PPageEntryArray;
  entrynr: integer;

  pi: PPageinfo;
begin
  //pageindex has already stripped of the 12 useless bits, so in theory, the maxlevel could be maxlevel-3

  maxlevel:=self.maxlevel;
  currentarray:=@level0list;

  level:=0;

  while level<maxlevel do
  begin
    //add the path if needed
    entrynr:=pageindex shr ((maxlevel-level)*4) and $f;
    if currentarray[entrynr].PageEntryArray=nil then //allocate
    begin
      getmem(temp, sizeof(TPageEntryArray));
      ZeroMemory(temp, sizeof(TPageEntryArray));
      currentarray[entrynr].PageEntryArray:=temp;
    end;

    currentarray:=currentarray[entrynr].PageEntryArray;
    inc(level);
  end;

  //got till level (maxlevel)
  entrynr:=pageindex shr ((maxlevel-level)*4) and $f;
  pi:=currentarray[entrynr].pageinfo;
  if pi=nil then //add it
  begin
    getmem(pi, sizeof(TPageInfo));
    pi.data:=pagedata;
    currentarray[entrynr].pageinfo:=pi;
  end;

  result:=pi;
end;

constructor TPageMap.create;
begin
  maxlevel:=15-3;
end;

procedure TPageMap.DeletePath(list: PPageEntryArray; level: integer);
var i: integer;
begin
  if level=maxlevel then
  begin
    for i:=0 to $F do
    begin
      if list[i].pageinfo<>nil then
      begin
        if list^[i].pageinfo.data<>nil then
          freemem(list^[i].pageinfo.data);

        FreeMemAndNil(list^[i].pageinfo);
      end;

    end;
  end
  else
  begin
    for i:=0 to $F do
    begin
      if list^[i].PageEntryArray<>nil then
      begin
        deletepath(list^[i].PageEntryArray,level+1);
        FreeMemAndNil(list^[i].PageEntryArray);

      end;
    end;
  end;
end;

destructor TPageMap.Destroy;
begin
  DeletePath(@level0list,0);
  inherited destroy;
end;

end.

