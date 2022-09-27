unit commonTypeDefs;

{$mode delphi}

interface

uses
  {$ifdef windows}windows,{$endif}{$ifdef unix}unixporthelper,{$endif} Classes, SysUtils;


//memscan
type TScanOption=(soUnknownValue=0,soExactValue=1,soValueBetween=2,soBiggerThan=3,soSmallerThan=4, soIncreasedValue=5, soIncreasedValueBy=6, soDecreasedValue=7, soDecreasedValueBy=8, soChanged=9, soUnchanged=10, soForgot=11, soCustom);
type TScanType=(stNewScan=0, stFirstScan=1, stNextScan=2);
type TRoundingType=(rtRounded=0,rtExtremerounded=1,rtTruncated=2);
type TVariableType=(vtByte=0, vtWord=1, vtDword=2, vtQword=3, vtSingle=4, vtDouble=5, vtString=6, vtUnicodeString=7, vtByteArray=8, vtBinary=9, vtAll=10, vtAutoAssembler=11, vtPointer=12, vtCustom=13, vtGrouped=14, vtByteArrays=15, vtCodePageString=16); //all ,grouped and MultiByteArray are special types
type TPointerType=(ptStatic, ptDynamic, ptExecutable); // used for pointer variables in group scans. currently not using this as a bitfield but values selected to make it easy if any future features want to.
type TPointerTypes=set of TPointerType;

type TCustomScanType=(cstNone, cstAutoAssembler, cstCPP, cstDLLFunction);
type TFastScanMethod=(fsmNotAligned=0, fsmAligned=1, fsmLastDigits=2);

type TaccessRight=(arExecute, arRead, arWrite);
type TAccessRights=set of TAccessRight;
type TVariableTypes=set of TVariableType;

type TAddressArray=array of ptruint;

Type TBytes = array of integer; //An array that represents a row of byte. Ints are used to be able to represent wildcards (-1)
     TWindowPosArray=TBytes;

type tfloatscan=(rounded,extremerounded,truncated);
type TMemoryRegionSmall = record
  Address: ptruint;
  size: qword;
end;

Type TMemoryRegion = record
  BaseAddress: ptrUint;
  MemorySize: qword;
  IsChild: boolean;  //means there is a region before it
  startaddress: pointer; //pointer to a spot in the whole memory copy, it means the start of this region
  end;
type TMemoryRegions = array of TMemoryRegion;
type PMemoryRegions = ^TMemoryRegions;


type
  TGroupAddress=record
    address: ptruint;
    offsets: array [0..999999] of dword;
  end;
  PGroupAddress=^TGroupAddress;
  PPGroupAddress=^PGroupAddress;
  TGroupAddressArray=array [0..0] of TGroupAddress;
  PGroupAddressArray=^TGroupAddressArray;



type TBitAddress = record
  address: ptruint;
  bit: ptruint; //in 64-bit when it was dword it would get aligned to 64-bit anyhow
end;

type TBitAddressArray=array [0..999999] of TBitAddress;
type PBitAddressArray=^TBitAddressArray;

type
  ToffsetList=array of integer;
  TDynDwordArray=array of dword;


{$IFDEF WINDOWS}
type TProcessListInfo=record
  processID: dword;
  winhandle: HWND;
  processIcon: HICON;
  //issystemprocess: boolean;
end;
PProcessListInfo=^TProcessListInfo;
{$ENDIF}


type tmoduledata =class
  public
    moduleaddress: ptrUint;
    modulesize: dword;
end;

type TPtrUintArray=array[0..100] of ptruint;
type PPtrUintArray=^TPtrUintArray;

type TDynPtrUintArray=array of ptruint;


type TDwordArray=array[0..100] of dword;
type PDwordArray=^TDwordArray;

type TSingleArray=array[0..100] of single;
type PSingleArray=^TSingleArray;

type TdoubleArray=array[0..100] of double;
type PdoubleArray=^TdoubleArray;

type Tint64Array=array[0..100] of int64;
type Pint64Array=^Tint64Array;

type Tuint64Array=array[0..100] of uint64;
type Puint64Array=^Tuint64Array;

type PQWordArray=Puint64Array;

type TExtendedArray=array[0..100] of extended;
type PExtendedArray=^TExtendedArray;

type TCEPointer=record
  Address: ptrUint;  //only used when last pointer in list
  Interpretableaddress: string; //same as address
  offset: integer;
end;

type TCEAlloc=record
  address: ptrUint;
  varname: string;
  size: dword;
  prefered: ptrUint;
  protection: dword;

end;
type PCEAlloc=^TCEAlloc;
type TCEAllocArray=array of TCEAlloc;

type TCEExceptionListArray=TDynPtrUintArray;



type TKeyCombo=array [0..4] of word;


implementation

end.

