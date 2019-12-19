unit addresslisthandlerunit;
{
obsolete

the addresslisthandler is a class which will deal with the general address list
functions, like allocating memory, adding/removing from the list, finding the
correct address if it's a pointer, etc...
}

{obsolete, handling is now done in addresslist}

{$mode DELPHI}

interface

uses
  Classes, SysUtils, controls, stdctrls, comctrls, MemoryRecordUnit, symbolhandler, cefuncproc,newkernelhandler, addresslist;

resourcestring
  rsALHAGroupWithTheName = 'A group with the name ';
  rsALHAlreadyExists = ' already exists';

    {

type TAddresslistHandler=class
  private
    FAddressList: TAddressList;

    //procedure freezeInterval
    //procedure UpdateValues
    function GetCount: integer;
    function GetRecord(Index: Integer): TMemoryRecord;
    procedure PutRecord(Index: Integer; Item: TMemoryRecord);
  public
    procedure delete(t: TTreeview);
   // procedure addaddress(description: string; address: PtrUInt; const offsets: array of dword; offsetcount: integer; vartype: TVariableType; length: integer; startbit: integer; unicode: boolean); overload;

    //procedure addaddress(description: string; address: uint_ptr; const offsets: array of dword; offsetcount: integer; ispointer: boolean; vartype: integer; length: integer; startbit: integer; unicode, showashex: boolean); overload;

    procedure CreateGroup(groupname: string);
    procedure DeleteGroup(groupname:string);

    constructor create(addresslist: TAddressList);
    destructor destroy; override;
    property Records[Index: Integer]: TMemoryRecord read GetRecord write PutRecord; default;
    property Count: Integer read GetCount;
  end;
    }

implementation

   {
function TAddresslistHandler.GetRecord(Index: Integer): TMemoryRecord;
begin

end;

procedure TAddresslistHandler.PutRecord(Index: Integer; Item: TMemoryRecord);
begin

end;

function TAddresslistHandler.GetCount: integer;
begin

end;

procedure TAddresslistHandler.delete(t: TTreeview);
begin


end;

procedure TAddresslistHandler.CreateGroup(groupname: string);
var memrec: TMemoryRecord;
    i: integer;
begin
  //first check if a group with this name already exists, if so, error
  for i:=0 to FAddressList.Items.Count-1 do
  begin
    memrec:=TMemoryRecord(FAddressList.Items[i].data);
    if memrec.isGroupHeader and (memrec.Description=groupname) then
      raise exception.create(rsALHAGroupWithTheName+groupname+rsALHAlreadyExists);
  end;

  memrec:=TMemoryRecord.create(self);
  memrec.Description:=groupname;
  memrec.isGroupHeader:=true;

 // memrec.treenode:=TTreenode.Create;



//  memrec.treenode.Parent:=

end;

procedure TAddresslistHandler.DeleteGroup(groupname:string);
var memrec: TMemoryRecord;
    i,j: integer;
begin
  for i:=0 to FAddressList.Items.Count-1 do
  begin
    memrec:=TMemoryRecord(FAddressList.Items[i].data);
    if memrec.isGroupHeader and (memrec.Description=groupname) then
    begin


      memrec.free;
    end;
  end;


end;


constructor TAddresslistHandler.create(addresslist: TAddressList);
begin
  FAddressList:=addresslist;
end;

destructor TAddresslistHandler.destroy;
begin
  while FAddressList.Items.Count>0 do
    TMemoryRecord(FAddressList.Items[0].data).Free;
end; }

end.

