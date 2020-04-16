unit changelist;

{
changelist is a helper class for the hexview
it keeps a list of entries containing lastvalue and timestamp of the last time it changed
}

{$mode delphi}

interface

uses
{$IFNDEF STANDALONEHV}
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
{$ENDIF}
  Classes, SysUtils;

type TChangeList=class
  private
    lastaddress: ptrUint;
    list: array of record
      value: string;
      LastChange: dword; //tickcount of the last time it was changed
    end;
    procedure SetValue(i: integer; value: string);
    function GetValue(i: integer): string;
    function getLastChange(i: integer): dword;
  public
    procedure ClearLastChange;
    procedure Clear;
    procedure Initialize(address: ptrUint; size: integer);
    property Values[index: integer]: string read getValue write setValue;
    property LastChange[index: integer]: dword read getLastChange;
end;

implementation

function TChangeList.getLastChange(i: integer): dword;
begin
  result:=list[i].LastChange;
end;

procedure TChangeList.SetValue(i: integer; value: string);
begin
  if (list[i].value<>'') and (list[i].value<>value) then
    list[i].LastChange:=Gettickcount;

  list[i].value:=value;
end;

function TChangeList.GetValue(i: integer): string;
begin
  result:=list[i].value;
end;

procedure TChangeList.ClearLastChange;
var i: integer;
begin
  for i:=0 to length(list)-1 do
    list[i].Lastchange:=0;
end;

procedure TChangeList.clear;
var i: integer;
begin
  for i:=0 to length(list)-1 do
  begin
    list[i].value:='';
    list[i].Lastchange:=0;
  end;
end;

procedure TChangeList.Initialize(address: ptrUint; size: integer);
{
sets the size of the list, returns true if a Clear might be needed
}
var i: integer;
begin
  if size>0 then
  begin
    if (address<>lastaddress) or (length(list)<>size) then
    begin
      lastaddress:=address;
      setlength(list,size);
      clear;
    end;
  end;


end;

end.

