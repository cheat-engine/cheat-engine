{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclCLR.pas.                                                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is Flier Lu (<flier_lu att yahoo dott com dott cn>).  }
{ Portions created by Flier Lu are Copyright (C) Flier Lu. All Rights Reserved.                    }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Flier Lu (flier)                                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Olivier Sannier (obones)                                                                       }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Microsoft .Net framework Clr information support routines and classes.                           }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-02-09 21:11:06 +0100 (sam., 09 févr. 2008)                        $ }
{ Revision:      $Rev:: 2351                                                                     $ }
{ Author:        $Author:: marcovtje                                                             $ }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date: 2008-02-09 21:11:06 +0100 (sam., 09 févr. 2008) $

unit JclCLR;

interface

{$I jcl.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils,
  {$IFDEF HAS_UNIT_CONTNRS}
  Contnrs,
  {$ENDIF HAS_UNIT_CONTNRS}
  JclBase, JclFileUtils, JclPeImage, JclSysUtils;

type
  _IMAGE_COR_VTABLEFIXUP = packed record
    RVA: DWORD;     // Offset of v-table array in image.
    Count: Word;    // How many entries at location.
    Kind: Word;     // COR_VTABLE_xxx type of entries.
  end;
  IMAGE_COR_VTABLEFIXUP = _IMAGE_COR_VTABLEFIXUP;
  TImageCorVTableFixup = _IMAGE_COR_VTABLEFIXUP;
  PImageCorVTableFixup = ^TImageCorVTableFixup;
  TImageCorVTableFixupArray = array [0..MaxWord-1] of TImageCorVTableFixup;
  PImageCorVTableFixupArray = ^TImageCorVTableFixupArray;

type
  PClrStreamHeader = ^TClrStreamHeader;
  TClrStreamHeader = packed record
    Offset: DWORD; // Memory offset to start of this stream from start of the metadata root
    Size: DWORD;   // Size of this stream in bytes, shall be a multiple of 4.
    // Name of the stream as null terminated variable length
    // array of ASCII characters, padded with \0 characters
    Name: array [0..MaxWord] of Char;
  end;

  PClrTableStreamHeader = ^TClrTableStreamHeader;
  TClrTableStreamHeader = packed record
    Reserved: DWORD;    // Reserved, always 0
    MajorVersion: Byte; // Major version of table schemata, always 1
    MinorVersion: Byte; // Minor version of table schemata, always 0
    HeapSizes: Byte;    // Bit vector for heap sizes.
    Reserved2: Byte;    // Reserved, always 1
    Valid: Int64;       // Bit vector of present tables, let n be the number of bits that are 1.
    Sorted: Int64;      // Bit vector of sorted tables.
    // Array of n four byte unsigned integers indicating the number of rows
    // for each present table.
    Rows: array [0..MaxWord] of DWORD;
    //Rows: array [0..n-1] of DWORD;
    //Tables: array
  end;

  PClrMetadataHeader = ^TClrMetadataHeader;
  TClrMetadataHeader = packed record
    Signature: DWORD;   // Magic signature for physical metadata : $424A5342.
    MajorVersion: Word; // Major version, 1
    MinorVersion: Word; // Minor version, 0
    Reserved: DWORD;    // Reserved, always 0
    Length: DWORD;      // Length of version string in bytes, say m.
    Version: array [0..0] of Char;
    // UTF8-encoded version string of length m
    // Padding to next 4 byte boundary, say x.
    {
    Version: array [0..((m+3) and (not $3))-1] of Char;
    Flags,              // Reserved, always 0
    Streams: Word;      // Number of streams, say n.
    // Array of n StreamHdr structures.
    StreamHeaders: array [0..n-1] of TClrStreamHeader;
    }
  end;

type
  TJclClrTableKind = (
    ttModule,               //  $00
    ttTypeRef,              //  $01
    ttTypeDef,              //  $02
    ttFieldPtr,             //  $03
    ttFieldDef,             //  $04
    ttMethodPtr,            //  $05
    ttMethodDef,            //  $06
    ttParamPtr,             //  $07
    ttParamDef,             //  $08
    ttInterfaceImpl,        //  $09
    ttMemberRef,            //  $0a
    ttConstant,             //  $0b
    ttCustomAttribute,      //  $0c
    ttFieldMarshal,         //  $0d
    ttDeclSecurity,         //  $0e
    ttClassLayout,          //  $0f
    ttFieldLayout,          //  $10
    ttSignature,            //  $11
    ttEventMap,             //  $12
    ttEventPtr,             //  $13
    ttEventDef,             //  $14
    ttPropertyMap,          //  $15
    ttPropertyPtr,          //  $16
    ttPropertyDef,          //  $17
    ttMethodSemantics,      //  $18
    ttMethodImpl,           //  $19
    ttModuleRef,            //  $1a
    ttTypeSpec,             //  $1b
    ttImplMap,              //  $1c
    ttFieldRVA,             //  $1d
    ttENCLog,               //  $1e
    ttENCMap,               //  $1f
    ttAssembly,             //  $20
    ttAssemblyProcessor,    //  $21
    ttAssemblyOS,           //  $22
    ttAssemblyRef,          //  $23
    ttAssemblyRefProcessor, //  $24
    ttAssemblyRefOS,        //  $25
    ttFile,                 //  $26
    ttExportedType,         //  $27
    ttManifestResource,     //  $28
    ttNestedClass,          //  $29
    ttTypeTyPar,            //  $2a
    ttMethodTyPar);         //  $2b

  TJclClrToken = DWORD;
  PJclClrToken = ^TJclClrToken;

type
  TJclClrHeaderEx = class;
  TJclPeMetadata = class;

  TJclClrStream = class(TObject)
  private
    FMetadata: TJclPeMetadata;
    FHeader: PClrStreamHeader;
    function GetName: string;
    function GetOffset: DWORD;
    function GetSize: DWORD;
    function GetData: Pointer;
  protected
    constructor Create(const AMetadata: TJclPeMetadata;
      AHeader: PClrStreamHeader); virtual;
  public
    property Metadata: TJclPeMetadata read FMetadata;
    property Header: PClrStreamHeader read FHeader;
    property Name: string read GetName;
    property Offset: DWORD read GetOffset;
    property Size: DWORD read GetSize;
    property Data: Pointer read GetData;
  end;

  TJclClrStreamClass = class of TJclClrStream;

  TJclClrStringsStream = class(TJclClrStream)
  private
    FStrings: TStringList;
    function GetString(const Idx: Integer): WideString;
    function GetOffset(const Idx: Integer): DWORD;
    function GetStringCount: Integer;
  protected
    constructor Create(const AMetadata: TJclPeMetadata;
      AHeader: PClrStreamHeader); override;
  public
    destructor Destroy; override;
    function At(const Offset: DWORD): WideString;
    property Strings[const Idx: Integer]: WideString read GetString; default;
    property Offsets[const Idx: Integer]: DWord read GetOffset;
    property StringCount: Integer read GetStringCount;
  end;

  TJclClrGuidStream = class(TJclClrStream)
  private
    FGuids: array of TGUID;
    function GetGuid(const Idx: Integer): TGUID;
    function GetGuidCount: Integer;
  protected
    constructor Create(const AMetadata: TJclPeMetadata;
      AHeader: PClrStreamHeader); override;
  public
    property Guids[const Idx: Integer]: TGUID read GetGuid; default;
    property GuidCount: Integer read GetGuidCount;
  end;

  TJclClrBlobRecord = class(TJclReferenceMemoryStream)
  private
    FPtr: PJclByteArray;
    FOffset: DWORD;
    function GetData: PJclByteArray;
  protected
    constructor Create(const AStream: TJclClrStream; APtr: PJclByteArray);
  public
    function Dump(Indent: string): string;
    property Ptr: PJclByteArray read FPtr;
    property Offset: DWORD read FOffset;
    property Data: PJclByteArray read GetData;
  end;

  TJclClrBlobStream = class(TJclClrStream)
  private
    FBlobs: TObjectList;
    function GetBlob(const Idx: Integer): TJclClrBlobRecord;
    function GetBlobCount: Integer;
  protected
    constructor Create(const AMetadata: TJclPeMetadata;
      AHeader: PClrStreamHeader); override;
  public
    destructor Destroy; override;
    function At(const Offset: DWORD): TJclClrBlobRecord;
    property Blobs[const Idx: Integer]: TJclClrBlobRecord read GetBlob; default;
    property BlobCount: Integer read GetBlobCount;
  end;

  TJclClrUserStringStream = class(TJclClrBlobStream)
  private
    function BlobToString(const ABlob: TJclClrBlobRecord): WideString;
    function GetString(const Idx: Integer): WideString;
    function GetOffset(const Idx: Integer): DWORD;
    function GetStringCount: Integer;
  public
    function At(const Offset: DWORD): WideString;
    property Strings[const Idx: Integer]: WideString read GetString; default;
    property Offsets[const Idx: Integer]: DWord read GetOffset;
    property StringCount: Integer read GetStringCount;
  end;

  TJclClrTableStream = class;

  TJclClrHeapKind = (hkString, hkGuid, hkBlob);
  TJclClrComboIndex = (ciResolutionScope);

  ITableCanDumpIL = interface(IUnknown)
    ['{C7AC787B-5DCD-411A-8674-D424A61B76D1}']
  end;

  TJclClrTable = class;

  TJclClrTableRow = class(TObject)
  private
    FTable: TJclClrTable;
    FIndex: Integer;
    function GetToken: TJclClrToken;
  protected
    constructor Create(const ATable: TJclClrTable); virtual;
    procedure Update; virtual;
    function DecodeTypeDefOrRef(const Encoded: DWORD): TJclClrTableRow;
    function DecodeResolutionScope(const Encoded: DWORD): TJclClrTableRow;
  public
    function DumpIL: string; virtual;
    property Table: TJclClrTable read FTable;
    property Index: Integer read FIndex;
    property Token: TJclClrToken read GetToken;
  end;

  TJclClrTableRowClass = class of TJclClrTableRow;

  TJclClrTable = class(TInterfacedObject)
  private
    FStream: TJclClrTableStream;
    FData: PChar;
    FPtr: PChar;
    FRows: TObjectList;
    FRowCount: Integer;
    FSize: DWORD;
    function GetOffset: DWORD;
  protected
    constructor Create(const AStream: TJclClrTableStream;
      const Ptr: Pointer; const ARowCount: Integer); virtual;
    procedure Load; virtual;
    procedure SetSize(const Value: Integer);
    procedure Update; virtual;
    function DumpIL: string; virtual;
    function GetRow(const Idx: Integer): TJclClrTableRow;
    function GetRowCount: Integer;
    function AddRow(const ARow: TJclClrTableRow): Integer;
    function RealRowCount: Integer;
    procedure Reset;
    class function TableRowClass: TJclClrTableRowClass; virtual;
  public
    destructor Destroy; override;
    function ReadCompressedValue: DWORD;
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadDWord: DWORD;
    function ReadIndex(const HeapKind: TJclClrHeapKind): DWORD; overload;
    function ReadIndex(const TableKinds: array of TJclClrTableKind): DWORD; overload;
    function IsWideIndex(const HeapKind: TJclClrHeapKind): Boolean; overload;
    function IsWideIndex(const TableKinds: array of TJclClrTableKind): Boolean; overload;
    function GetCodedIndexTag(const CodedIndex, TagWidth: DWORD;
      const WideIndex: Boolean): DWORD;
    function GetCodedIndexValue(const CodedIndex, TagWidth: DWORD;
      const WideIndex: Boolean): DWORD;
    property Stream: TJclClrTableStream read FStream;
    property Data: PChar read FData;
    property Size: DWORD read FSize;
    property Offset: DWORD read GetOffset;
    property Rows[const Idx: Integer]: TJclClrTableRow read GetRow; default;
    property RowCount: Integer read GetRowCount;
  end;

  TJclClrTableClass = class of TJclClrTable;

  TJclClrTableStream = class(TJclClrStream)
  private
    FHeader: PClrTableStreamHeader;
    FTables: array [TJclClrTableKind] of TJclClrTable;
    FTableCount: Integer;
    function GetVersionString: string;
    function GetTable(const AKind: TJclClrTableKind): TJclClrTable;
    function GetBigHeap(const AHeapKind: TJclClrHeapKind): Boolean;
  protected
    constructor Create(const AMetadata: TJclPeMetadata;
      AHeader: PClrStreamHeader); override;
  public
    destructor Destroy; override;
    procedure Update; virtual;
    function DumpIL: string;
    function FindTable(const AKind: TJclClrTableKind;
      var ATable: TJclClrTable): Boolean;
    property Header: PClrTableStreamHeader read FHeader;
    property VersionString: string read GetVersionString;
    property BigHeap[const AHeapKind: TJclClrHeapKind]: Boolean read GetBigHeap;
    property Tables[const AKind: TJclClrTableKind]: TJclClrTable read GetTable;
    property TableCount: Integer read FTableCount;
  end;

  TJclPeMetadata = class(TObject)
  private
    FImage: TJclPeImage;
    FHeader: PClrMetadataHeader;
    FStreams: TObjectList;
    FStringStream: TJclClrStringsStream;
    FGuidStream: TJclClrGuidStream;
    FBlobStream: TJclClrBlobStream;
    FUserStringStream: TJclClrUserStringStream;
    FTableStream: TJclClrTableStream;
    function GetStream(const Idx: Integer): TJclClrStream;
    function GetStreamCount: Integer;
    function GetString(const Idx: Integer): WideString;
    function GetStringCount: Integer;
    function GetGuid(const Idx: Integer): TGUID;
    function GetGuidCount: Integer;
    function GetBlob(const Idx: Integer): TJclClrBlobRecord;
    function GetBlobCount: Integer;
    function GetTable(const AKind: TJclClrTableKind): TJclClrTable;
    function GetTableCount: Integer;
    function GetToken(const AToken: TJclClrToken): TJclClrTableRow;
    function GetVersion: string;
    function GetVersionString: WideString;
    function GetFlags: Word;
    function UserGetString(const Idx: Integer): WideString;
    function UserGetStringCount: Integer;
  protected
    constructor Create(const AImage: TJclPeImage);
  public
    destructor Destroy; override;
    function DumpIL: string;
    function FindStream(const AName: string; var Stream: TJclClrStream): Boolean; overload;
    function FindStream(const AClass: TJclClrStreamClass; var Stream: TJclClrStream): Boolean; overload;
    function StringAt(const Offset: DWORD): WideString;
    function UserStringAt(const Offset: DWORD): WideString;
    function BlobAt(const Offset: DWORD): TJclClrBlobRecord;
    function TokenExists(const Token: TJclClrToken): Boolean;
    class function TokenTable(const Token: TJclClrToken): TJclClrTableKind;
    class function TokenIndex(const Token: TJclClrToken): Integer;
    class function TokenCode(const Token: TJclClrToken): Integer;
    class function MakeToken(const Table: TJclClrTableKind; const Idx: Integer): TJclClrToken;
    property Image: TJclPeImage read FImage;
    property Header: PClrMetadataHeader read FHeader;
    property Version: string read GetVersion;
    property VersionString: WideString read GetVersionString;
    property Flags: Word read GetFlags;
    property Streams[const Idx: Integer]: TJclClrStream read GetStream; default;
    property StreamCount: Integer read GetStreamCount;
    property Strings[const Idx: Integer]: WideString read GetString;
    property StringCount: Integer read GetStringCount;
    property UserStrings[const Idx: Integer]: WideString read UserGetString;
    property UserStringCount: Integer read UserGetStringCount;
    property Guids[const Idx: Integer]: TGUID read GetGuid;
    property GuidCount: Integer read GetGuidCount;
    property Blobs[const Idx: Integer]: TJclClrBlobRecord read GetBlob;
    property BlobCount: Integer read GetBlobCount;
    property Tables[const AKind: TJclClrTableKind]: TJclClrTable read GetTable;
    property TableCount: Integer read GetTableCount;
    property Tokens[const AToken: TJclClrToken]: TJclClrTableRow read GetToken;
  end;

  TJclClrResourceRecord = class(TJClreferenceMemoryStream)
  private
    FData: Pointer;
    FOffset: DWORD;
    FRVA: DWORD;
  protected
    constructor Create(const AData: PChar; const AOffset: DWORD; const ARVA: DWORD);
  public
    property Data: Pointer read FData;
    property Offset: DWORD read FOffset;
    property RVA: DWORD read FRVA;
  end;

  TJclClrVTableKind = (vtk32Bit, vtk64Bit, vtkFromUnmanaged, vtkCallMostDerived);
  TJclClrVTableKinds = set of TJclClrVTableKind;

  TJclClrVTableFixupRecord = class(TObject)
  private
    FData: PImageCorVTableFixup;
    function GetCount: DWORD;
    function GetKinds: TJclClrVTableKinds;
    function GetRVA: DWORD;
  protected
    constructor Create(AData: PImageCorVTableFixup);
    class function VTableKinds(const Kinds: TJclClrVTableKinds): DWORD; overload;
    class function VTableKinds(const Kinds: DWORD): TJclClrVTableKinds; overload;
  public
    property Data: PImageCorVTableFixup read FData;
    property RVA: DWORD read GetRVA;                  // RVA of Vtable
    property Count: DWORD read GetCount;              // Number of entries in Vtable
    property Kinds: TJclClrVTableKinds read GetKinds; // Type of the entries
  end;

  TJclClrImageFlag = (cifILOnly, cif32BitRequired, cifStrongNameSinged, cifTrackDebugData);
  TJclClrImageFlags = set of TJclClrImageFlag;

  TJclClrHeaderEx = class(TJclPeClrHeader)
  private
    FMetadata: TJclPeMetadata;
    FFlags: TJclClrImageFlags;
    FStrongNameSignature: TCustomMemoryStream;
    FResources: TObjectList;
    FVTableFixups: TObjectList;
    function GetMetadata: TJclPeMetadata;
    function GetStrongNameSignature: TCustomMemoryStream;
    function GetEntryPointToken: TJclClrTableRow;
    function GetVTableFixup(const Idx: Integer): TJclClrVTableFixupRecord;
    function GetVTableFixupCount: Integer;
    procedure UpdateResources;
    function GetResource(const Idx: Integer): TJclClrResourceRecord;
    function GetResourceCount: Integer;
  public
    constructor Create(const AImage: TJclPeImage);
    destructor Destroy; override;
    function DumpIL: string;
    function HasResources: Boolean;
    function HasStrongNameSignature: Boolean;
    function HasVTableFixup: Boolean;
    function ResourceAt(const Offset: DWORD): TJclClrResourceRecord;
    class function ClrImageFlag(const Flags: DWORD): TJclClrImageFlags; overload;
    class function ClrImageFlag(const Flags: TJclClrImageFlags): DWORD; overload;
    property Metadata: TJclPeMetadata read GetMetadata;
    property Flags: TJclClrImageFlags read FFlags;
    property EntryPointToken: TJclClrTableRow read GetEntryPointToken;
    property StrongNameSignature: TCustomMemoryStream read GetStrongNameSignature;
    property Resources[const Idx: Integer]: TJclClrResourceRecord read GetResource;
    property ResourceCount: Integer read GetResourceCount;
    property VTableFixups[const Idx: Integer]: TJclClrVTableFixupRecord read GetVTableFixup;
    property VTableFixupCount: Integer read GetVTableFixupCount;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclCLR.pas $';
    Revision: '$Revision: 2351 $';
    Date: '$Date: 2008-02-09 21:11:06 +0100 (sam., 09 févr. 2008) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  Math, TypInfo,
  JclMetadata, JclResources, JclStrings, JclUnicode;

const
  MetadataHeaderSignature = $424A5342; // 'BSJB'

  GUID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}';

  ValidTableMapping: array [TJclClrTableKind] of TJclClrTableClass = (
    TJclClrTableModule,               //  $00 ttModule
    TJclClrTableTypeRef,              //  $01 ttTypeRef
    TJclClrTableTypeDef,              //  $02 ttTypeDef
    TJclClrTableFieldPtr,             //  $03 ttFieldPtr
    TJclClrTableFieldDef,             //  $04 ttFieldDef
    TJclClrTableMethodPtr,            //  $05 ttMethodPtr
    TJclClrTableMethodDef,            //  $06 ttMethodDef
    TJclClrTableParamPtr,             //  $07 ttParamPtr
    TJclClrTableParamDef,             //  $08 ttParamDef
    TJclClrTableInterfaceImpl,        //  $09 ttInterfaceImpl
    TJclClrTableMemberRef,            //  $0a ttMemberRef
    TJclClrTableConstant,             //  $0b ttConstant
    TJclClrTableCustomAttribute,      //  $0c ttCustomAttribute
    TJclClrTableFieldMarshal,         //  $0d ttFieldMarshal
    TJclClrTableDeclSecurity,         //  $0e ttDeclSecurity
    TJclClrTableClassLayout,          //  $0f ttClassLayout
    TJclClrTableFieldLayout,          //  $10 ttFieldLayout
    TJclClrTableStandAloneSig,        //  $11 ttSignature
    TJclClrTableEventMap,             //  $12 ttEventMap
    TJclClrTableEventPtr,             //  $13 ttEventPtr
    TJclClrTableEventDef,             //  $14 ttEventDef
    TJclClrTablePropertyMap,          //  $15 ttPropertyMap
    TJclClrTablePropertyPtr,          //  $16 ttPropertyPtr
    TJclClrTablePropertyDef,          //  $17 ttPropertyDef
    TJclClrTableMethodSemantics,      //  $18 ttMethodSemantics
    TJclClrTableMethodImpl,           //  $19 ttMethodImpl
    TJclClrTableModuleRef,            //  $1a ttModuleRef
    TJclClrTableTypeSpec,             //  $1b ttTypeSpec
    TJclClrTableImplMap,              //  $1c ttImplMap
    TJclClrTableFieldRVA,             //  $1d ttFieldRVA
    TJclClrTableENCLog,               //  $1e ttENCLog
    TJclClrTableENCMap,               //  $1f ttENCMap
    TJclClrTableAssembly,             //  $20 ttAssembly
    TJclClrTableAssemblyProcessor,    //  $21 ttAssemblyProcessor
    TJclClrTableAssemblyOS,           //  $22 ttAssemblyOS
    TJclClrTableAssemblyRef,          //  $23 ttAssemblyRef
    TJclClrTableAssemblyRefProcessor, //  $24 ttAssemblyRefProcessor
    TJclClrTableAssemblyRefOS,        //  $25 ttAssemblyRefOS
    TJclClrTableFile,                 //  $26 ttFile
    TJclClrTableExportedType,         //  $27 ttExportedType
    TJclClrTableManifestResource,     //  $28 ttManifestResource
    TJclClrTableNestedClass,          //  $29 ttNestedClass
    TJclClrTable,                     //  $2A ttGenericPar
    TJclClrTableMethodSpec);          //  $2B ttMethodSpec

// CLR Header entry point flags.
const
  COMIMAGE_FLAGS_ILONLY           = $00000001;  // Always 1 (see Section 23.1).
  COMIMAGE_FLAGS_32BITREQUIRED    = $00000002;
    // Image may only be loaded into a 32-bit process,
    // for instance if there are 32-bit vtablefixups,
    // or casts from native integers to int32.
    // CLI implementations that have 64 bit native integers shall refuse
    // loading binaries with this flag set.
  COMIMAGE_FLAGS_STRONGNAMESIGNED = $00000008;  // Image has a strong name signature.
  COMIMAGE_FLAGS_TRACKDEBUGDATA   = $00010000;  // Always 0 (see Section 23.1).
  ClrImageFlagMapping: array [TJclClrImageFlag] of DWORD =
    (COMIMAGE_FLAGS_ILONLY, COMIMAGE_FLAGS_32BITREQUIRED,
     COMIMAGE_FLAGS_STRONGNAMESIGNED, COMIMAGE_FLAGS_TRACKDEBUGDATA);

// V-table constants
const
  COR_VTABLE_32BIT             = $01;          // V-table slots are 32-bits in size.
  COR_VTABLE_64BIT             = $02;          // V-table slots are 64-bits in size.
  COR_VTABLE_FROM_UNMANAGED    = $04;          // If set, transition from unmanaged.
  COR_VTABLE_CALL_MOST_DERIVED = $10;          // Call most derived method described by

  ClrVTableKindMapping: array [TJclClrVTableKind] of DWORD =
    (COR_VTABLE_32BIT, COR_VTABLE_64BIT,
     COR_VTABLE_FROM_UNMANAGED, COR_VTABLE_CALL_MOST_DERIVED);

//=== { TJclClrStream } ======================================================

constructor TJclClrStream.Create(const AMetadata: TJclPeMetadata;
  AHeader: PClrStreamHeader);
begin
  inherited Create;
  FMetadata := AMetadata;
  FHeader := AHeader;
end;

function TJclClrStream.GetName: string;
begin
  Result := FHeader.Name;
end;

function TJclClrStream.GetOffset: DWORD;
begin
  Result := Data - Metadata.Image.LoadedImage.MappedAddress;
end;

function TJclClrStream.GetSize: DWORD;
begin
  Result := FHeader.Size;
end;

function TJclClrStream.GetData: Pointer;
begin
  Result := Pointer(DWORD(FMetadata.Header) + FHeader.Offset);
end;

//=== { TJclClrStringsStream } ===============================================

constructor TJclClrStringsStream.Create(const AMetadata: TJclPeMetadata;
  AHeader: PClrStreamHeader);
var
  pch: PChar;
  off: DWORD;
begin
  inherited Create(AMetadata, AHeader);
  FStrings := TStringList.Create;
  pch := Data;
  off := 0;
  while off < Size do
  begin
    if pch^ <> #0 then
      FStrings.AddObject(pch, TObject(off));
    pch := pch + StrLen(pch) + 1;
    off := DWORD(pch - Data);
  end;
end;

destructor TJclClrStringsStream.Destroy;
begin
  FreeAndNil(FStrings);
  inherited Destroy;
end;

function TJclClrStringsStream.GetString(const Idx: Integer): WideString;
begin
  Result := UTF8ToWideString(FStrings.Strings[Idx]);
end;

function TJclClrStringsStream.GetOffset(const Idx: Integer): DWORD;
begin
  Result := DWord(FStrings.Objects[Idx]);
end;

function TJclClrStringsStream.GetStringCount: Integer;
begin
  Result := FStrings.Count;
end;

function TJclClrStringsStream.At(const Offset: DWORD): WideString;
var
  Idx: Integer;
begin
  Idx := FStrings.IndexOfObject(TObject(Offset));
  if Idx <> -1 then
    Result := GetString(Idx)
  else
    Result := '';
end;

//=== { TJclClrGuidStream } ==================================================

constructor TJclClrGuidStream.Create(const AMetadata: TJclPeMetadata;
  AHeader: PClrStreamHeader);
var
  I: Integer;
  pg: PGUID;
begin
  inherited Create(AMetadata, AHeader);
  SetLength(FGuids, Size div SizeOf(TGuid));
  pg := Data;
  for I := 0 to GetGuidCount-1 do
  begin
    FGuids[I] := pg^;
    Inc(pg);
  end;
end;

function TJclClrGuidStream.GetGuid(const Idx: Integer): TGUID;
begin
  Assert((0 <= Idx) and (Idx < GetGuidCount));
  Result := FGuids[Idx];
end;

function TJclClrGuidStream.GetGuidCount: Integer;
begin
  Result := Length(FGuids);
end;

//=== { TJclClrBlobRecord } ==================================================

constructor TJclClrBlobRecord.Create(const AStream: TJclClrStream; APtr: PJclByteArray);
var
  b: Byte;
  AData: Pointer;
  ASize: DWORD;
begin
  FPtr := APtr;
  FOffset := DWORD(FPtr) - DWORD(AStream.Data);

  b := FPtr[0];
  if b = 0 then
  begin
    AData := @FPtr[1];
    ASize := 0;
  end
  else
  if ((b and $C0) = $C0) and ((b and $20) = 0) then    // 110bs
  begin
    AData := @FPtr[4];
    ASize := ((b and $1F) shl 24) + (FPtr[1] shl 16) + (FPtr[2] shl 8) + FPtr[3];
  end
  else
  if ((b and $80) = $80) and ((b and $40) = 0) then    // 10bs
  begin
    AData := @FPtr[2];
    ASize := ((b and $3F) shl 8) + FPtr[1];
  end
  else
  begin
    AData := @FPtr[1];
    ASize := b and $7F;
  end;
  Assert(not IsBadReadPtr(AData, ASize));
  inherited Create(AData, ASize);
end;

function TJclClrBlobRecord.Dump(Indent: string): string;
const
  BufSize = 16;
var
  I, Len: Integer;

  function DumpBuf(Buf: PChar; Size: Integer; IsHead, IsTail: Boolean): string;
  var
    I: Integer;
    HexStr, AsciiStr: string;
  begin
    for I := 0 to Size-1 do
    begin
      HexStr := HexStr + IntToHex(Integer(Buf[I]), 2) + ' ';
      if CharIsPrintable(Buf[I]) and ((Byte(Buf[I]) and $80) <> $80) then
        AsciiStr := AsciiStr + Buf[I]
      else
        AsciiStr := AsciiStr + '.';
    end;

    if IsTail then
      Result := HexStr + ')' + StrRepeat(' ', (BufSize-Size)*3) + ' // ' + AsciiStr
    else
      Result := HexStr + ' ' + StrRepeat(' ', (BufSize-Size)*3) + ' // ' + AsciiStr;
    if IsHead then
      Result := Indent + '( ' + Result
    else
      Result := StrRepeat(' ', Length(Indent)+2) + Result;
  end;

begin
  with TStringList.Create do
  try
    Len := (Size + BufSize - 1) div BufSize;
    for I := 0 to Len-1 do
      if I = Len - 1 then
        Add(DumpBuf(PChar(Memory) + I * BufSize, Size - I * BufSize, I=0, I=Len-1))
      else
        Add(DumpBuf(PChar(Memory) + I * BufSize, BufSize, I=0, I=Len-1));
    Result := Text;
  finally
    Free;
  end;
end;

function TJclClrBlobRecord.GetData: PJclByteArray;
begin
  Result := PJclByteArray(LongInt(Memory) + Position);
end;

//=== { TJclClrBlobStream } ==================================================

constructor TJclClrBlobStream.Create(const AMetadata: TJclPeMetadata;
  AHeader: PClrStreamHeader);
var
  ABlob: TJclClrBlobRecord;
begin
  inherited Create(AMetadata, AHeader);
  FBlobs := TObjectList.Create;
  ABlob := TJclClrBlobRecord.Create(Self, Data);
  while Assigned(ABlob) do
  begin
    if ABlob.Size > 0 then
      FBlobs.Add(ABlob);
    if (Integer(ABlob.Memory) + ABlob.Size) < (Integer(Self.Data) + Integer(Self.Size)) then
      ABlob := TJclClrBlobRecord.Create(Self, Pointer(Integer(ABlob.Memory) + ABlob.Size))
    else
      ABlob := nil;
  end;
end;

destructor TJclClrBlobStream.Destroy;
begin
  FreeAndNil(FBlobs);
  inherited Destroy;
end;

function TJclClrBlobStream.At(const Offset: DWORD): TJclClrBlobRecord;
var
  I: Integer;
begin
  for I := 0 to FBlobs.Count-1 do
  begin
    Result := TJclClrBlobRecord(FBlobs.Items[I]);
    if Result.Offset = Offset then
      Exit;
  end;
  Result := nil;
end;

function TJclClrBlobStream.GetBlob(const Idx: Integer): TJclClrBlobRecord;
begin
  Result := TJclClrBlobRecord(FBlobs.Items[Idx])
end;

function TJclClrBlobStream.GetBlobCount: Integer;
begin
  Result := FBlobs.Count;
end;

//=== { TJclClrUserStringStream } ============================================

function TJclClrUserStringStream.BlobToString(const ABlob: TJclClrBlobRecord): WideString;
begin
  if Assigned(ABlob) then
  begin
    SetLength(Result, ABlob.Size div 2);
    Move(PWideChar(ABlob.Memory)^, PWideChar(Result)^, ABlob.Size and not 1);
  end
  else
    Result := '';
end;

function TJclClrUserStringStream.GetString(const Idx: Integer): WideString;
begin
  Result := BlobToString(Blobs[Idx]);
end;

function TJclClrUserStringStream.GetOffset(const Idx: Integer): DWORD;
begin
  Result := Blobs[Idx].Offset;
end;

function TJclClrUserStringStream.GetStringCount: Integer;
begin
  Result := BlobCount;
end;

function TJclClrUserStringStream.At(const Offset: DWORD): WideString;
begin
  Result := BlobToString(inherited At(Offset));
end;

//=== { TJclClrTableRow } ====================================================

constructor TJclClrTableRow.Create(const ATable: TJclClrTable);
begin
  inherited Create;
  FTable := ATable;
  FIndex := Table.RealRowCount;
end;

function TJclClrTableRow.DecodeResolutionScope(const Encoded: DWORD): TJclClrTableRow;
const
  ResolutionScopeEncoded: array [0..3] of TJclClrTableKind =
    (ttModule, ttModuleRef, ttAssemblyRef, ttTypeRef);
begin
  Result := Table.Stream.Tables[ResolutionScopeEncoded[Encoded and 3]].Rows[Encoded shr 2 - 1];
end;

function TJclClrTableRow.DecodeTypeDefOrRef(const Encoded: DWORD): TJclClrTableRow;
const
  TypeDefOrRefEncoded: array [0..2] of TJclClrTableKind =
    (ttTypeDef, ttTypeRef, ttTypeSpec);
begin
  Result := Table.Stream.Tables[TypeDefOrRefEncoded[Encoded and 3]].Rows[Encoded shr 2 - 1];
end;

function TJclClrTableRow.DumpIL: string;
begin
  // (rom) needs comment why empty
end;

function TJclClrTableRow.GetToken: TJclClrToken;

  function GetTableId: TJclClrTableKind;
  begin
    for Result := Low(TJclClrTableKind) to High(TJclClrTableKind) do
      if ValidTableMapping[Result] = Table.ClassType then
        Exit;
    raise EJclError.CreateResFmt(@RsUnknownTableFmt, [LoadResString(@RsUnknownTable), ClassName]);
  end;

begin
  Result := Byte(GetTableId) shl 24 + Index + 1;
end;

procedure TJclClrTableRow.Update;
begin
  // do nothing, just for override
end;

//=== {  TJclClrTable } ======================================================

constructor TJclClrTable.Create(const AStream: TJclClrTableStream;
  const Ptr: Pointer; const ARowCount: Integer);
begin
  inherited Create;
  FStream := AStream;
  FData := Ptr;
  FRows  := nil; // Create on demand
  FRowCount := ARowCount;
  Reset;
  Load;
  SetSize(FPtr - FData);
end;

destructor TJclClrTable.Destroy;
begin
  FreeAndNil(FRows);
  inherited Destroy;
end;

procedure TJclClrTable.Reset;
begin
  FPtr := FData;
end;

procedure TJclClrTable.Load;
var
  I: Integer;
begin
  Assert(RowCount > 0);

  if TableRowClass <> TJclClrTableRow then
    for I := 0 to RowCount-1 do
      AddRow(TableRowClass.Create(Self));
end;

procedure TJclClrTable.SetSize(const Value: Integer);
begin
  FSize := Value;
  Assert(not IsBadReadPtr(FData, FSize));
end;

function TJclClrTable.GetOffset: DWORD;
begin
  Result := DWORD(Data) - DWORD(Stream.Metadata.Image.LoadedImage.MappedAddress);
end;

function TJclClrTable.GetRow(const Idx: Integer): TJclClrTableRow;
begin
  Result := TJclClrTableRow(FRows.Items[Idx]);
end;

function TJclClrTable.GetRowCount: Integer;
begin
  Result := FRowCount;
end;

function TJclClrTable.AddRow(const ARow: TJclClrTableRow): Integer;
begin
  if not Assigned(FRows) then
    FRows := TObjectList.Create;
  Result := FRows.Add(ARow);
end;

function TJclClrTable.RealRowCount: Integer;
begin
  if Assigned(FRows) then
    Result := FRows.Count
  else
    Result := 0;
end;

function TJclClrTable.ReadIndex(const HeapKind: TJclClrHeapKind): DWORD;
begin
  if IsWideIndex(HeapKind) then
    Result := ReadDWord
  else
    Result := ReadWord;
end;

function TJclClrTable.ReadIndex(const TableKinds: array of TJclClrTableKind): DWORD;
begin
  if IsWideIndex(TableKinds) then
    Result := ReadDWord
  else
    Result := ReadWord;
end;

function TJclClrTable.IsWideIndex(const HeapKind: TJclClrHeapKind): Boolean;
begin
  Result := Stream.BigHeap[HeapKind];
end;

function TJclClrTable.IsWideIndex(const TableKinds: array of TJclClrTableKind): Boolean;
var
  I: Integer;
  ATable: TJclClrTable;
begin
  Result := False;
  for I := Low(TableKinds) to High(TableKinds) do
    if Stream.FindTable(TableKinds[I], ATable) then
      Result := Result or (ATable.RowCount > MAXWORD);
end;

function TJclClrTable.ReadByte: Byte;
begin
  Result := PByte(FPtr)^;
  Inc(FPtr, SizeOf(Byte));
end;

function TJclClrTable.ReadWord: Word;
begin
  Result := PWord(FPtr)^;
  Inc(FPtr, SizeOf(Word));
end;

function TJclClrTable.ReadDWord: DWORD;
begin
  Result := PDWORD(FPtr)^;
  Inc(FPtr, SizeOf(DWORD));
end;

function TJclClrTable.ReadCompressedValue: DWORD;
var
  I: Integer;
begin
  Result := ReadByte;
  if Result = 0 then
  begin
    Exit;
  end
  else
  if ((Result and $C0) = $C0) and ((Result and $20) = 0) then    // 110bs
  begin
    Result := Result and $1F;
    for I := 0 to 2 do
      Result := Result shl 8 + ReadByte;
  end
  else
  if ((Result and $80) = $80) and ((Result and $40) = 0) then    // 10bs
  begin
    Result := ((Result and $3F) shl 8) + ReadByte;
  end
  else
  begin
    Result := Result and $7F;
  end;
end;

class function TJclClrTable.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableRow;
end;

procedure TJclClrTable.Update;
var
  I: Integer;
begin
  if Assigned(FRows) then
  for I := 0 to RowCount-1 do
    Rows[I].Update;
end;

function TJclClrTable.GetCodedIndexTag(const CodedIndex, TagWidth: DWORD;
  const WideIndex: Boolean): DWORD;
var
  I, TagMask: DWORD;
begin
  TagMask := 0;
  for I := 0 to TagWidth-1 do
    TagMask := TagMask or (1 shl I);
  Result := CodedIndex and TagMask;
end;

function TJclClrTable.GetCodedIndexValue(const CodedIndex, TagWidth: DWORD;
  const WideIndex: Boolean): DWORD;
const
  IndexBits: array [Boolean] of DWORD = (SizeOf(WORD) * 8, SizeOf(DWORD) * 8);
var
  I, ValueMask: DWORD;
begin
  ValueMask := 0;
  for I := TagWidth to IndexBits[WideIndex]-1 do
    ValueMask := ValueMask or (1 shl I);
  Result := (CodedIndex and ValueMask) shr TagWidth;
end;

function TJclClrTable.DumpIL: string;
var
  I: Integer;
begin
  Result := '// Dump ' + ClassName + AnsiLineBreak;
  {$IFDEF RTL140_UP}
  if Supports(ClassType, ITableCanDumpIL) then
  {$ELSE RTL140_UP}
  if ClassType.GetInterfaceEntry(ITableCanDumpIL) <> nil then
  {$ENDIF RTL140_UP}
    for I := 0 to FRows.Count - 1 do
      Result := Result + TJclClrTableRow(FRows[I]).DumpIL;
end;

//=== { TJclClrTableStream } =================================================

constructor TJclClrTableStream.Create(const AMetadata: TJclPeMetadata;
  AHeader: PClrStreamHeader);

  function BitCount(const Value: Int64): Integer;
  var
    AKind: TJclClrTableKind;
  begin
    Result := 0;
    for AKind := Low(TJclClrTableKind) to High(TJclClrTableKind) do
      if (Value and (Int64(1) shl Integer(AKind))) <> 0 then
        Inc(Result);
  end;

  procedure EnumTables;
  var
    AKind: TJclClrTableKind;
    pTable: Pointer;
  begin
    pTable := @Header.Rows[BitCount(Header.Valid)];
    FTableCount := 0;
    for AKind := Low(TJclClrTableKind) to High(TJclClrTableKind) do
    begin
      if (Header.Valid and (Int64(1) shl Integer(AKind))) <> 0 then
      begin
        FTables[AKind] := ValidTableMapping[AKind].Create(Self, pTable, Header.Rows[FTableCount]);
        pTable := Pointer(DWORD(pTable) + FTables[AKind].Size);
        Inc(FTableCount);
      end
      else
        FTables[AKind] := nil;
    end;
  end;

begin
  inherited Create(AMetadata, AHeader);
  FHeader := Data;
  EnumTables;
end;

destructor TJclClrTableStream.Destroy;
begin
  FreeAndNil(FTables);
  inherited Destroy;
end;

function TJclClrTableStream.GetVersionString: string;
begin
  Result := FormatVersionString(Header.MajorVersion, Header.MinorVersion);
end;

function TJclClrTableStream.GetTable(const AKind: TJclClrTableKind): TJclClrTable;
begin
  Result := TJclClrTable(FTables[AKind]);
end;

function TJclClrTableStream.GetBigHeap(const AHeapKind: TJclClrHeapKind): Boolean;
const
  HeapSizesMapping: array [TJclClrHeapKind] of DWORD = (1, 2, 4);
begin
  Result := (Header.HeapSizes and HeapSizesMapping[AHeapKind]) <> 0;
end;

function TJclClrTableStream.FindTable(const AKind: TJclClrTableKind;
  var ATable: TJclClrTable): Boolean;
begin
  ATable := FTables[AKind];
  Result := Assigned(ATable);
end;

procedure TJclClrTableStream.Update;
var
  AKind: TJclClrTableKind;
begin
  for AKind := Low(TJclClrTableKind) to High(TJclClrTableKind) do
    if Assigned(FTables[AKind]) then
      FTables[AKind].Update;
end;

function TJclClrTableStream.DumpIL: string;
var
  AKind: TJclClrTableKind;
begin
  for AKind := Low(TJclClrTableKind) to High(TJclClrTableKind) do
    if Assigned(FTables[AKind]) then
      Result := Result + FTables[AKind].DumpIL;
end;

//=== { TJclPeMetadata } =====================================================

constructor TJclPeMetadata.Create(const AImage: TJclPeImage);

  function GetStreamClass(const Name: string): TJclClrStreamClass;
  begin
    if CompareText(Name, '#Strings') = 0 then
      Result := TJclClrStringsStream
    else
    if CompareText(Name, '#GUID') = 0 then
      Result := TJclClrGuidStream
    else
    if CompareText(Name, '#Blob') = 0 then
      Result := TJclClrBlobStream
    else
    if CompareText(Name, '#US') = 0 then
      Result := TJclClrUserStringStream
    else
    if CompareText(Name, '#~') = 0 then
      Result := TJclClrTableStream
    else
      Result := TJclClrStream;
  end;

  procedure UpdateStreams;
  type
    PStreamPartitionHeader = ^TStreamPartitionHeader;
    TStreamPartitionHeader = packed record
      Flags,
      StreamCount: Word;
      StreamHeaders: array [0..0] of TClrStreamHeader;
    end;
  var
    pStreamPart: PStreamPartitionHeader;
    pStream: PClrStreamHeader;
    I: Integer;
    TableStream: TJclClrTableStream;
  begin
    pStreamPart := PStreamPartitionHeader(DWORD(@Header.Version[0]) + Header.Length);
    pStream := @pStreamPart.StreamHeaders[0];
    for I := 0 to pStreamPart.StreamCount-1 do
    begin
      FStreams.Add(GetStreamClass(pStream.Name).Create(Self, pStream));

      pStream := PClrStreamHeader(DWORD(@pStream.Name[0]) +
        DWORD((((StrLen(@pStream.Name[0])+1)+3) and (not $3))));
    end;
    if FindStream(TJclClrTableStream, TJclClrStream(TableStream)) then
      TableStream.Update;
  end;

begin
  Assert(AImage.IsClr and AImage.ClrHeader.HasMetadata);
  inherited Create;
  FImage := AImage;
  with Image.ClrHeader.Header.MetaData do
  begin
    Assert(Size > SizeOf(FHeader^));
    FHeader := Image.RvaToVa(VirtualAddress);
    Assert(not IsBadReadPtr(FHeader, Size));
  end;

  FStreams := TObjectList.Create;
  UpdateStreams;

  FindStream(TJclClrStringsStream, TJclClrStream(FStringStream));
  FindStream(TJclClrGuidStream, TJclClrStream(FGuidStream));
  FindStream(TJclClrBlobStream, TJclClrStream(FBlobStream));
  FindStream(TJclClrUserStringStream, TJclClrStream(FUserStringStream));
  FindStream(TJclClrTableStream, TJclClrStream(FTableStream));
end;

destructor TJclPeMetadata.Destroy;
begin
  FreeAndNil(FStreams);
  inherited Destroy;
end;

function TJclPeMetadata.GetVersionString: WideString;
var
  VerStr: string;
begin
  SetLength(VerStr, Header.Length+1);
  StrlCopy(PChar(VerStr), @Header.Version[0], Header.Length);
  SetLength(VerStr, StrLen(PChar(VerStr)));
  Result := UTF8ToWideString(VerStr)
end;

function TJclPeMetadata.GetVersion: string;
begin
  Result := FormatVersionString(Header.MajorVersion, Header.MinorVersion);
end;

function TJclPeMetadata.GetFlags: Word;
begin
  Result := PWord(PChar(@Header.Version[0]) + (Header.Length + 3) and (not 3))^;
end;

function TJclPeMetadata.GetStream(const Idx: Integer): TJclClrStream;
begin
  Result := TJclClrStream(FStreams.Items[Idx]);
end;

function TJclPeMetadata.GetStreamCount: Integer;
begin
  Result := FStreams.Count;
end;

function TJclPeMetadata.FindStream(const AName: string;
  var Stream: TJclClrStream): Boolean;
var
  I: Integer;
begin
  for I := 0 to GetStreamCount-1 do
  begin
    Stream := Streams[I];
    if CompareText(Stream.Name, AName) = 0 then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
  Stream := nil;
end;

function TJclPeMetadata.FindStream(const AClass: TJclClrStreamClass;
  var Stream: TJclClrStream): Boolean;
var
  I: Integer;
begin
  for I := 0 to GetStreamCount-1 do
  begin
    Stream := Streams[I];
    if Stream.ClassType = AClass then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
  Stream := nil;
end;

function TJclPeMetadata.GetToken(const AToken: TJclClrToken): TJclClrTableRow;
begin
  if AToken = 0 then
    Result := nil
  else
  try
    Result := Tables[TokenTable(AToken)].Rows[TokenIndex(AToken)-1];
  except
    Result := nil;
  end;
end;

function TJclPeMetadata.GetString(const Idx: Integer): WideString;
begin
  if Assigned(FStringStream) or
     FindStream(TJclClrStringsStream, TJclClrStream(FStringStream)) then
    Result := FStringStream.Strings[Idx]
  else
    Result := '';
end;

function TJclPeMetadata.GetStringCount: Integer;
begin
  if Assigned(FStringStream) or
     FindStream(TJclClrStringsStream, TJclClrStream(FStringStream)) then
    Result := FStringStream.StringCount
  else
    Result := 0;
end;

function TJclPeMetadata.UserGetString(const Idx: Integer): WideString;
begin
  if Assigned(FUserStringStream) or
     FindStream(TJclClrUserStringStream, TJclClrStream(FUserStringStream)) then
    Result := FUserStringStream.Strings[Idx-1]
  else
    Result := '';
end;

function TJclPeMetadata.UserGetStringCount: Integer;
begin
  if Assigned(FUserStringStream) or
     FindStream(TJclClrUserStringStream, TJclClrStream(FUserStringStream)) then
    Result := FUserStringStream.StringCount
  else
    Result := 0;
end;

function TJclPeMetadata.StringAt(const Offset: DWORD): WideString;
begin
  if Assigned(FStringStream) or
     FindStream(TJclClrStringsStream, TJclClrStream(FStringStream)) then
    Result := FStringStream.At(Offset)
  else
    Result := '';
end;

function TJclPeMetadata.UserStringAt(const Offset: DWORD): WideString;
begin
  if Assigned(FUserStringStream) or
     FindStream(TJclClrUserStringStream, TJclClrStream(FUserStringStream)) then
    Result := TJclClrUserStringStream(FUserStringStream).At(Offset)
  else
    Result := '';
end;

function TJclPeMetadata.BlobAt(const Offset: DWORD): TJclClrBlobRecord;
begin
  if Assigned(FBlobStream) or
     FindStream(TJclClrBlobStream, TJclClrStream(FBlobStream)) then
    Result := TJclClrBlobStream(FBlobStream).At(Offset)
  else
    Result := nil;
end;

function TJclPeMetadata.GetGuid(const Idx: Integer): TGUID;
begin
  if Assigned(FGuidStream) or
     FindStream(TJclClrGuidStream, TJclClrStream(FGuidStream)) then
    Result := FGuidStream.Guids[Idx]
  else
    Result := GUID_NULL;
end;

function TJclPeMetadata.GetGuidCount: Integer;
begin
  if Assigned(FGuidStream) or
     FindStream(TJclClrGuidStream, TJclClrStream(FGuidStream)) then
    Result := FGuidStream.GuidCount
  else
    Result := 0;
end;

function TJclPeMetadata.GetBlob(const Idx: Integer): TJclClrBlobRecord;
begin
  if Assigned(FBlobStream) or
     FindStream(TJclClrBlobStream, TJclClrStream(FBlobStream)) then
    Result := FBlobStream.Blobs[Idx]
  else
    Result := nil;
end;

function TJclPeMetadata.GetBlobCount: Integer;
begin
  if Assigned(FBlobStream) or
     FindStream(TJclClrBlobStream, TJclClrStream(FBlobStream)) then
    Result := FBlobStream.BlobCount
  else
    Result := 0;
end;

function TJclPeMetadata.GetTable(const AKind: TJclClrTableKind): TJclClrTable;
begin
  if Assigned(FTableStream) or
     FindStream(TJclClrTableStream, TJclClrStream(FTableStream)) then
    Result := FTableStream.Tables[AKind]
  else
    Result := nil;
end;

function TJclPeMetadata.GetTableCount: Integer;
begin
  if Assigned(FTableStream) or
     FindStream(TJclClrTableStream, TJclClrStream(FTableStream)) then
    Result := FTableStream.TableCount
  else
    Result := 0;
end;

function TJclPeMetadata.TokenExists(const Token: TJclClrToken): Boolean;
begin
  Result := TokenIndex(Token) in [1..Tables[TokenTable(Token)].RowCount];
end;

class function TJclPeMetadata.TokenTable(const Token: TJclClrToken): TJclClrTableKind;
begin
  Result := TJclClrTableKind(Token shr 24);
end;

class function TJclPeMetadata.TokenIndex(const Token: TJclClrToken): Integer;
begin
  Result := Token and DWORD($FFFFFF);
end;

class function TJclPeMetadata.TokenCode(const Token: TJclClrToken): Integer;
begin
  Result := Token and $FF000000;
end;

class function TJclPeMetadata.MakeToken(const Table: TJclClrTableKind;
  const Idx: Integer): TJclClrToken;
begin
  Result := (DWORD(Table) shl 24) and TokenIndex(Idx);
end;

function TJclPeMetadata.DumpIL: string;
begin
  with TStringList.Create do
  try
    case Image.Target of
      taWin32:
        begin
          Add(Format('.imagebase 0x%.8x', [Image.OptionalHeader32.ImageBase]));
          Add(Format('.subsystem 0x%.8x', [Image.OptionalHeader32.SubSystem]));
          Add(Format('.file alignment %d', [Image.OptionalHeader32.FileAlignment]));
        end;
      taWin64:
        begin
          Add(Format('.imagebase 0x%.16x', [Image.OptionalHeader64.ImageBase]));
          Add(Format('.subsystem 0x%.8x', [Image.OptionalHeader64.SubSystem]));
          Add(Format('.file alignment %d', [Image.OptionalHeader64.FileAlignment]));
        end;
    //taUnknown: ;
    end;

    if Assigned(FTableStream) then
    begin
      FTableStream.Update;
      Result := Text + AnsiLineBreak + FTableStream.DumpIL;
    end;
  finally
    Free;
  end;
end;

//=== { TJclClrResourceRecord } ==============================================

constructor TJclClrResourceRecord.Create(const AData: PChar;
  const AOffset: DWORD; const ARVA: DWORD);
begin
  FData := AData;
  FOffset := AOffset;
  FRVA := ARVA;
  inherited Create(Pointer(DWORD(Data)+SizeOf(DWORD)), PDWORD(Data)^);
end;

//=== { TJclClrVTableFixupRecord } ===========================================

constructor TJclClrVTableFixupRecord.Create(AData: PImageCorVTableFixup);
begin
  inherited Create;
  FData := AData;
end;

function TJclClrVTableFixupRecord.GetCount: DWORD;
begin
  Result := Data.Count;
end;

function TJclClrVTableFixupRecord.GetKinds: TJclClrVTableKinds;
begin
  Result := VTableKinds(Data.Kind);
end;

function TJclClrVTableFixupRecord.GetRVA: DWORD;
begin
  Result := Data.RVA;
end;

class function TJclClrVTableFixupRecord.VTableKinds(const Kinds: TJclClrVTableKinds): DWORD;
var
  AKind: TJclClrVTableKind;
begin
  Result := 0;
  for AKind := Low(TJclClrVTableKind) to High(TJclClrVTableKind) do
    if AKind in Kinds then
      Result := Result or ClrVTableKindMapping[AKind];
end;

class function TJclClrVTableFixupRecord.VTableKinds(const Kinds: DWORD): TJclClrVTableKinds;
var
  AKind: TJclClrVTableKind;
begin
  Result := [];
  for AKind := Low(TJclClrVTableKind) to High(TJclClrVTableKind) do
    if (ClrVTableKindMapping[AKind] and Kinds) = ClrVTableKindMapping[AKind] then
      Include(Result, AKind);
end;

//=== { TJclClrInformation } =================================================

constructor TJclClrHeaderEx.Create(const AImage: TJclPeImage);

  procedure UpdateVTableFixups;
  begin
    // (rom) What is this?
    if Header.VTableFixups.VirtualAddress = 0 then
  end;

begin
  inherited Create(AImage);
  FFlags := ClrImageFlag(Header.Flags);
  FMetadata := nil;
  FResources := nil;
  FStrongNameSignature := nil;
  FVTableFixups := nil;
end;

destructor TJclClrHeaderEx.Destroy;
begin
  FreeAndNil(FVTableFixups);
  FreeAndNil(FStrongNameSignature);
  FreeAndNil(FResources);
  FreeAndNil(FMetadata);
  inherited Destroy;
end;

class function TJclClrHeaderEx.ClrImageFlag(const Flags: DWORD): TJclClrImageFlags;
var
  AFlag: TJclClrImageFlag;
begin
  Result := [];
  for AFlag := Low(TJclClrImageFlag) to High(TJclClrImageFlag) do
    if (ClrImageFlagMapping[AFlag] and Flags) = ClrImageFlagMapping[AFlag] then
      Include(Result, AFlag);
end;

class function TJclClrHeaderEx.ClrImageFlag(const Flags: TJclClrImageFlags): DWORD;
var
  AFlag: TJclClrImageFlag;
begin
  Result := 0;
  for AFlag := Low(TJclClrImageFlag) to High(TJclClrImageFlag) do
    if AFlag in Flags then
      Result := Result or ClrImageFlagMapping[AFlag];
end;

function TJclClrHeaderEx.GetMetadata: TJclPeMetadata;
begin
  if not Assigned(FMetadata) and HasMetadata then
    FMetadata := TJclPeMetadata.Create(Image);
  Result := FMetadata;
end;

function TJclClrHeaderEx.HasStrongNameSignature: Boolean;
begin
  with Header.StrongNameSignature do
  Result := Assigned(FStrongNameSignature) or
    ((Size > 0) and not IsBadReadPtr(Image.RvaToVa(VirtualAddress), Size));
end;

function TJclClrHeaderEx.HasVTableFixup: Boolean;
begin
  with Header.VTableFixups do
  Result := Assigned(FVTableFixups) or
    ((Size > 0) and not IsBadReadPtr(Image.RvaToVa(VirtualAddress), Size));
end;

function TJclClrHeaderEx.GetStrongNameSignature: TCustomMemoryStream;
begin
  if not Assigned(FStrongNameSignature) and HasStrongNameSignature then
  with Header.StrongNameSignature do
    FStrongNameSignature := TJClreferenceMemoryStream.Create(Image.RvaToVa(VirtualAddress), Size);
  Result := FStrongNameSignature;
end;

function TJclClrHeaderEx.HasResources: Boolean;
begin
  with Header.Resources do
  Result := Assigned(FResources) or
    ((Size > 0) and not IsBadReadPtr(Image.RvaToVa(VirtualAddress), Size));
end;

procedure TJclClrHeaderEx.UpdateResources;
var
  Base, Ptr: PChar;
  ARes: TJclClrResourceRecord;
begin
  FResources := TObjectList.Create;
  with Header.Resources do
  begin
    Base := Image.RvaToVa(VirtualAddress);
    Ptr := Base;
    while DWORD(Ptr-Base) < Size do
    begin
      ARes := TJclClrResourceRecord.Create(Ptr, Ptr-Base, Ptr-Image.LoadedImage.MappedAddress);
      FResources.Add(ARes);
      Ptr := PChar(ARes.Memory) + ARes.Size;
    end;
  end;
end;

function TJclClrHeaderEx.GetResource(
  const Idx: Integer): TJclClrResourceRecord;
begin
  if not Assigned(FResources) and HasResources then
    UpdateResources;
  Result := TJclClrResourceRecord(FResources.Items[Idx]);
end;

function TJclClrHeaderEx.GetResourceCount: Integer;
begin
  if not Assigned(FResources) and HasResources then
    UpdateResources;
  if Assigned(FResources) then
    Result := FResources.Count
  else
    Result := 0;
end;

function TJclClrHeaderEx.GetEntryPointToken: TJclClrTableRow;
begin
  Result := Metadata.Tokens[Header.EntryPointToken]
end;

function TJclClrHeaderEx.GetVTableFixup(
  const Idx: Integer): TJclClrVTableFixupRecord;
var
  I: Integer;
  pData: PImageCorVTableFixup;
begin
  if not Assigned(FVTableFixups) and HasVTableFixup then
  begin
    FVTableFixups := TObjectList.Create;
    with Header.VTableFixups do
    begin
      pData := PImageCorVTableFixup(Image.RvaToVa(VirtualAddress));
      for I := 0 to GetVTableFixupCount-1 do
      begin
        FVTableFixups.Add(TJclClrVTableFixupRecord.Create(pData));
        Inc(pData);
      end;
    end;
  end;
  Result := TJclClrVTableFixupRecord(FVTableFixups.Items[Idx]);
end;

function TJclClrHeaderEx.GetVTableFixupCount: Integer;
begin
  Result := Header.VTableFixups.Size div SizeOf(TImageCorVTableFixup);
end;

function TJclClrHeaderEx.ResourceAt(const Offset: DWORD): TJclClrResourceRecord;
var
  I: Integer;
begin
  if HasResources then
    for I := 0 to ResourceCount-1 do
    begin
      Result := Resources[I];
      if Result.Offset = Offset then
        Exit;
    end;
  Result := nil;
end;

function TJclClrHeaderEx.DumpIL: string;
begin
  with TStringList.Create do
  try
    Add(RsClrCopyright);
    Add(Format('.corflags 0x%.8x', [Header.Flags]));
    Result := Text + AnsiLineBreak + Metadata.DumpIL;
  finally
    Free;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
