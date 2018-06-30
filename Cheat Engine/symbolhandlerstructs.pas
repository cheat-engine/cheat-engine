unit symbolhandlerstructs;

{$mode delphi}

interface

uses
  Classes, SysUtils, cvconst, commonTypeDefs;


type TUDSEnum=record
  address: ptrUint;
  allocsize: dword;
  addressstring: pchar; //points to the string
  doNotSave: boolean;
end;

type symexception=class(Exception);

type
  TDBStructInfo=class(TObject)
    moduleid: integer;
    typeid: integer;
    length: integer;
    callbackid: integer;
  end;

  TDBElementInfo=class (TObject)
    offset: dword;
    basetype: integer;
    typeid: integer;
    tag: TSymTagEnum;
    vartype: TVariableType;
  end;


type TUserdefinedsymbol=record
  symbolname: string;
  address: ptrUint;
  addressstring: string;

  allocsize: dword; //if it is a global alloc, allocsize>0
  processid: dword; //the processid this memory was allocated to (in case of processswitches)
  doNotSave: boolean; //if true this will cause this entry to not be saved when the user saves the table
end;

type TModuleInfo=record
  modulename: string;
  modulepath: string;
  isSystemModule: boolean;
  baseaddress: ptrUint;
  basesize: dword;
  is64bitmodule: boolean;
  symbolsLoaded: boolean; //true if the api symbols have been handled
  hasStructInfo: boolean;
  databaseModuleID: dword;
end;


implementation

end.

