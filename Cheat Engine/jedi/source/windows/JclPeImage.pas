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
{ The Original Code is JclPeImage.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) Petr Vones. All Rights Reserved.                                                   }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Robert Marquardt (marquardt)                                                                   }
{   Uwe Schuster (uschuster)                                                                       }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   Hallvard Vassbotn                                                                              }
{   Jean-Fabien Connault (cycocrew)                                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains various classes and support routines to read the contents of portable         }
{ executable (PE) files. You can use these classes to, for example examine the contents of the     }
{ imports section of an executable. In addition the unit contains support for Borland specific     }
{ structures and name unmangling.                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-05-21 22:19:06 +0200 (mer., 21 mai 2008)                           $ }
{ Revision:      $Rev:: 2372                                                                     $ }
{ Author:        $Author:: ahuser                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclPeImage;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes, SysUtils, TypInfo, Contnrs,
  JclBase, JclDateTime, JclFileUtils, JclStrings, JclSysInfo, JclWin32;
  
type
  // Smart name compare function
  TJclSmartCompOption = (scSimpleCompare, scIgnoreCase);
  TJclSmartCompOptions = set of TJclSmartCompOption;

function PeStripFunctionAW(const FunctionName: string): string;

function PeSmartFunctionNameSame(const ComparedName, FunctionName: string;
  Options: TJclSmartCompOptions = []): Boolean;

type
  // Base list
  EJclPeImageError = class(EJclError);

  TJclPeImage = class;
  TJclPeBorImage = class;

  TJclPeImageClass = class of TJclPeImage;

  TJclPeImageBaseList = class(TObjectList)
  private
    FImage: TJclPeImage;
  public
    constructor Create(AImage: TJclPeImage);
    property Image: TJclPeImage read FImage;
  end;

  // Images cache
  TJclPeImagesCache = class(TObject)
  private
    FList: TStringList;
    function GetCount: Integer;
    function GetImages(const FileName: TFileName): TJclPeImage;
  protected
    function GetPeImageClass: TJclPeImageClass; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Images[const FileName: TFileName]: TJclPeImage read GetImages; default;
    property Count: Integer read GetCount;
  end;

  TJclPeBorImagesCache = class(TJclPeImagesCache)
  private
    function GetImages(const FileName: TFileName): TJclPeBorImage;
  protected
    function GetPeImageClass: TJclPeImageClass; override;
  public
    property Images[const FileName: TFileName]: TJclPeBorImage read GetImages; default;
  end;

  // Import section related classes
  TJclPeImportSort = (isName, isOrdinal, isHint, isLibImport);
  TJclPeImportLibSort = (ilName, ilIndex);
  TJclPeImportKind = (ikImport, ikDelayImport, ikBoundImport);
  TJclPeResolveCheck = (icNotChecked, icResolved, icUnresolved);
  TJclPeLinkerProducer = (lrBorland, lrMicrosoft);
  // lrBorland   -> Delphi PE files
  // lrMicrosoft -> MSVC and BCB PE files

  TJclPeImportLibItem = class;

  // Created from a IMAGE_THUNK_DATA64 or IMAGE_THUNK_DATA32 record
  TJclPeImportFuncItem = class(TObject)
  private
    FOrdinal: Word;  // word in 32/64
    FHint: Word;
    FImportLib: TJclPeImportLibItem;
    FIndirectImportName: Boolean;
    FName: string;
    FResolveCheck: TJclPeResolveCheck;
    function GetIsByOrdinal: Boolean;
  protected
    procedure SetName(const Value: string);
    procedure SetIndirectImportName(const Value: string);
    procedure SetResolveCheck(Value: TJclPeResolveCheck);
  public
    constructor Create(AImportLib: TJclPeImportLibItem; AOrdinal: Word;
      AHint: Word; const AName: string);
    property Ordinal: Word read FOrdinal;
    property Hint: Word read FHint;
    property ImportLib: TJclPeImportLibItem read FImportLib;
    property IndirectImportName: Boolean read FIndirectImportName;
    property IsByOrdinal: Boolean read GetIsByOrdinal;
    property Name: string read FName;
    property ResolveCheck: TJclPeResolveCheck read FResolveCheck;
  end;

  // Created from a IMAGE_IMPORT_DESCRIPTOR
  TJclPeImportLibItem = class(TJclPeImageBaseList)
  private
    FImportDescriptor: Pointer;
    FImportDirectoryIndex: Integer;
    FImportKind: TJclPeImportKind;
    FLastSortType: TJclPeImportSort;
    FLastSortDescending: Boolean;
    FName: string;
    FSorted: Boolean;
    FTotalResolveCheck: TJclPeResolveCheck;
    FThunk: Pointer;
    FThunkData: Pointer;
    function GetCount: Integer;
    function GetFileName: TFileName;
    function GetItems(Index: Integer): TJclPeImportFuncItem;
    function GetName: string;
    {$IFDEF KEEP_DEPRECATED}
    function GetThunkData: PImageThunkData;
    {$ENDIF KEEP_DEPRECATED}
    function GetThunkData32: PImageThunkData32;
    function GetThunkData64: PImageThunkData64;
  protected
    procedure CheckImports(ExportImage: TJclPeImage);
    procedure CreateList;
    procedure SetImportDirectoryIndex(Value: Integer);
    procedure SetImportKind(Value: TJclPeImportKind);
    procedure SetSorted(Value: Boolean);
    procedure SetThunk(Value: Pointer);
  public
    constructor Create(AImage: TJclPeImage; AImportDescriptor: Pointer;
      AImportKind: TJclPeImportKind; const AName: AnsiString; AThunk: Pointer);
    procedure SortList(SortType: TJclPeImportSort; Descending: Boolean = False);
    property Count: Integer read GetCount;
    property FileName: TFileName read GetFileName;
    property ImportDescriptor: Pointer read FImportDescriptor;
    property ImportDirectoryIndex: Integer read FImportDirectoryIndex;
    property ImportKind: TJclPeImportKind read FImportKind;
    property Items[Index: Integer]: TJclPeImportFuncItem read GetItems; default;
    property Name: string read GetName;
    property OriginalName: string read FName;
    {$IFDEF KEEP_DEPRECATED}
    property ThunkData: PImageThunkData read GetThunkData;
    {$ENDIF KEEP_DEPRECATED}
    property ThunkData32: PImageThunkData32 read GetThunkData32;
    property ThunkData64: PImageThunkData64 read GetThunkData64;
    property TotalResolveCheck: TJclPeResolveCheck read FTotalResolveCheck;
  end;

  TJclPeImportList = class(TJclPeImageBaseList)
  private
    FAllItemsList: TList;
    FFilterModuleName: string;
    FLastAllSortType: TJclPeImportSort;
    FLastAllSortDescending: Boolean;
    FLinkerProducer: TJclPeLinkerProducer;
    FParallelImportTable: array of Pointer;
    FUniqueNamesList: TStringList;
    function GetAllItemCount: Integer;
    function GetAllItems(Index: Integer): TJclPeImportFuncItem;
    function GetItems(Index: Integer): TJclPeImportLibItem;
    function GetUniqueLibItemCount: Integer;
    function GetUniqueLibItems(Index: Integer): TJclPeImportLibItem;
    function GetUniqueLibNames(Index: Integer): string;
    function GetUniqueLibItemFromName(const Name: string): TJclPeImportLibItem;
    procedure SetFilterModuleName(const Value: string);
  protected
    procedure CreateList;
    procedure RefreshAllItems;
  public
    constructor Create(AImage: TJclPeImage);
    destructor Destroy; override;
    procedure CheckImports(PeImageCache: TJclPeImagesCache = nil);
    function MakeBorlandImportTableForMappedImage: Boolean;
    function SmartFindName(const CompareName, LibName: string; Options: TJclSmartCompOptions = []): TJclPeImportFuncItem;
    procedure SortAllItemsList(SortType: TJclPeImportSort; Descending: Boolean = False);
    procedure SortList(SortType: TJclPeImportLibSort);
    procedure TryGetNamesForOrdinalImports;
    property AllItems[Index: Integer]: TJclPeImportFuncItem read GetAllItems;
    property AllItemCount: Integer read GetAllItemCount;
    property FilterModuleName: string read FFilterModuleName write SetFilterModuleName;
    property Items[Index: Integer]: TJclPeImportLibItem read GetItems; default;
    property LinkerProducer: TJclPeLinkerProducer read FLinkerProducer;
    property UniqueLibItemCount: Integer read GetUniqueLibItemCount;
    property UniqueLibItemFromName[const Name: string]: TJclPeImportLibItem read GetUniqueLibItemFromName;
    property UniqueLibItems[Index: Integer]: TJclPeImportLibItem read GetUniqueLibItems;
    property UniqueLibNames[Index: Integer]: string read GetUniqueLibNames;
  end;

  // Export section related classes
  TJclPeExportSort = (esName, esOrdinal, esHint, esAddress, esForwarded,  esAddrOrFwd, esSection);

  TJclPeExportFuncList = class;

  // Created from a IMAGE_EXPORT_DIRECTORY
  TJclPeExportFuncItem = class(TObject)
  private
    FAddress: DWORD;
    FExportList: TJclPeExportFuncList;
    FForwardedName: string;
    FForwardedDotPos: string;
    FHint: Word;
    FName: string;
    FOrdinal: Word;
    FResolveCheck: TJclPeResolveCheck;
    function GetAddressOrForwardStr: string;
    function GetForwardedFuncName: string;
    function GetForwardedLibName: string;
    function GetForwardedFuncOrdinal: DWORD;
    function GetIsExportedVariable: Boolean;
    function GetIsForwarded: Boolean;
    function GetSectionName: string;
    function GetMappedAddress: Pointer;
  protected
    procedure SetResolveCheck(Value: TJclPeResolveCheck);
  public
    constructor Create(AExportList: TJclPeExportFuncList; const AName, AForwardedName: string;
      AAddress: DWORD; AHint: Word; AOrdinal: Word; AResolveCheck: TJclPeResolveCheck);
    property Address: DWORD read FAddress;
    property AddressOrForwardStr: string read GetAddressOrForwardStr;
    property IsExportedVariable: Boolean read GetIsExportedVariable;
    property IsForwarded: Boolean read GetIsForwarded;
    property ForwardedName: string read FForwardedName;
    property ForwardedLibName: string read GetForwardedLibName;
    property ForwardedFuncOrdinal: DWORD read GetForwardedFuncOrdinal;
    property ForwardedFuncName: string read GetForwardedFuncName;
    property Hint: Word read FHint;
    property MappedAddress: Pointer read GetMappedAddress;
    property Name: string read FName;
    property Ordinal: Word read FOrdinal;
    property ResolveCheck: TJclPeResolveCheck read FResolveCheck;
    property SectionName: string read GetSectionName;
  end;

  TJclPeExportFuncList = class(TJclPeImageBaseList)
  private
    FAnyForwards: Boolean;
    FBase: DWORD;
    FExportDir: PImageExportDirectory;
    FForwardedLibsList: TStringList;
    FFunctionCount: DWORD;
    FLastSortType: TJclPeExportSort;
    FLastSortDescending: Boolean;
    FSorted: Boolean;
    FTotalResolveCheck: TJclPeResolveCheck;
    function GetForwardedLibsList: TStrings;
    function GetItems(Index: Integer): TJclPeExportFuncItem;
    function GetItemFromAddress(Address: DWORD): TJclPeExportFuncItem;
    function GetItemFromOrdinal(Ordinal: DWORD): TJclPeExportFuncItem;
    function GetItemFromName(const Name: string): TJclPeExportFuncItem;
    function GetName: string;
  protected
    function CanPerformFastNameSearch: Boolean;
    procedure CreateList;
    property LastSortType: TJclPeExportSort read FLastSortType;
    property LastSortDescending: Boolean read FLastSortDescending;
    property Sorted: Boolean read FSorted;
  public
    constructor Create(AImage: TJclPeImage);
    destructor Destroy; override;
    procedure CheckForwards(PeImageCache: TJclPeImagesCache = nil);
    class function ItemName(Item: TJclPeExportFuncItem): string;
    function OrdinalValid(Ordinal: DWORD): Boolean;
    procedure PrepareForFastNameSearch;
    function SmartFindName(const CompareName: string; Options: TJclSmartCompOptions = []): TJclPeExportFuncItem;
    procedure SortList(SortType: TJclPeExportSort; Descending: Boolean = False);
    property AnyForwards: Boolean read FAnyForwards;
    property Base: DWORD read FBase;
    property ExportDir: PImageExportDirectory read FExportDir;
    property ForwardedLibsList: TStrings read GetForwardedLibsList;
    property FunctionCount: DWORD read FFunctionCount;
    property Items[Index: Integer]: TJclPeExportFuncItem read GetItems; default;
    property ItemFromAddress[Address: DWORD]: TJclPeExportFuncItem read GetItemFromAddress;
    property ItemFromName[const Name: string]: TJclPeExportFuncItem read GetItemFromName;
    property ItemFromOrdinal[Ordinal: DWORD]: TJclPeExportFuncItem read GetItemFromOrdinal;
    property Name: string read GetName;
    property TotalResolveCheck: TJclPeResolveCheck read FTotalResolveCheck;
  end;

  // Resource section related classes
  TJclPeResourceKind = (
    rtUnknown0,
    rtCursorEntry,
    rtBitmap,
    rtIconEntry,
    rtMenu,
    rtDialog,
    rtString,
    rtFontDir,
    rtFont,
    rtAccelerators,
    rtRCData,
    rtMessageTable,
    rtCursor,
    rtUnknown13,
    rtIcon,
    rtUnknown15,
    rtVersion,
    rtDlgInclude,
    rtUnknown18,
    rtPlugPlay,
    rtVxd,
    rtAniCursor,
    rtAniIcon,
    rtHmtl,
    rtManifest,
    rtUserDefined);

  TJclPeResourceList = class;
  TJclPeResourceItem = class;

  TJclPeResourceRawStream = class(TCustomMemoryStream)
  public
    constructor Create(AResourceItem: TJclPeResourceItem);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TJclPeResourceItem = class(TObject)
  private
    FEntry: PImageResourceDirectoryEntry;
    FImage: TJclPeImage;
    FList: TJclPeResourceList;
    FLevel: Byte;
    FParentItem: TJclPeResourceItem;
    FNameCache: string;
    function GetDataEntry: PImageResourceDataEntry;
    function GetIsDirectory: Boolean;
    function GetIsName: Boolean;
    function GetLangID: LANGID;
    function GetList: TJclPeResourceList;
    function GetName: string;
    function GetParameterName: string;
    function GetRawEntryData: Pointer;
    function GetRawEntryDataSize: Integer;
    function GetResourceType: TJclPeResourceKind;
    function GetResourceTypeStr: string;
  protected
    function OffsetToRawData(Ofs: DWORD): DWORD;
    function Level1Item: TJclPeResourceItem;
    function SubDirData: PImageResourceDirectory;
  public
    constructor Create(AImage: TJclPeImage; AParentItem: TJclPeResourceItem;
      AEntry: PImageResourceDirectoryEntry);
    destructor Destroy; override;
    function CompareName(AName: PChar): Boolean;
    property DataEntry: PImageResourceDataEntry read GetDataEntry;
    property Entry: PImageResourceDirectoryEntry read FEntry;
    property Image: TJclPeImage read FImage;
    property IsDirectory: Boolean read GetIsDirectory;
    property IsName: Boolean read GetIsName;
    property LangID: LANGID read GetLangID;
    property List: TJclPeResourceList read GetList;
    property Level: Byte read FLevel;
    property Name: string read GetName;
    property ParameterName: string read GetParameterName;
    property ParentItem: TJclPeResourceItem read FParentItem;
    property RawEntryData: Pointer read GetRawEntryData;
    property RawEntryDataSize: Integer read GetRawEntryDataSize;
    property ResourceType: TJclPeResourceKind read GetResourceType;
    property ResourceTypeStr: string read GetResourceTypeStr;
  end;

  TJclPeResourceList = class(TJclPeImageBaseList)
  private
    FDirectory: PImageResourceDirectory;
    FParentItem: TJclPeResourceItem;
    function GetItems(Index: Integer): TJclPeResourceItem;
  protected
    procedure CreateList(AParentItem: TJclPeResourceItem);
  public
    constructor Create(AImage: TJclPeImage; AParentItem: TJclPeResourceItem;
      ADirectory: PImageResourceDirectory);
    function FindName(const Name: string): TJclPeResourceItem;
    property Directory: PImageResourceDirectory read FDirectory;
    property Items[Index: Integer]: TJclPeResourceItem read GetItems; default;
    property ParentItem: TJclPeResourceItem read FParentItem;
  end;

  TJclPeRootResourceList = class(TJclPeResourceList)
  private
    FManifestContent: TStringList;
    function GetManifestContent: TStrings;
  public
    destructor Destroy; override;
    function FindResource(ResourceType: TJclPeResourceKind;
      const ResourceName: string = ''): TJclPeResourceItem; overload;
    function FindResource(const ResourceType: PChar;
      const ResourceName: PChar = nil): TJclPeResourceItem; overload;
    function ListResourceNames(ResourceType: TJclPeResourceKind; const Strings: TStrings): Boolean;
    property ManifestContent: TStrings read GetManifestContent;
  end;

  // Relocation section related classes
  TJclPeRelocation = record
    Address: Word;
    RelocType: Byte;
    VirtualAddress: DWORD;
  end;

  TJclPeRelocEntry = class(TObject)
  private
    FChunk: PImageBaseRelocation;
    FCount: Integer;
    function GetRelocations(Index: Integer): TJclPeRelocation;
    function GetSize: DWORD;
    function GetVirtualAddress: DWORD;
  public
    constructor Create(AChunk: PImageBaseRelocation; ACount: Integer);
    property Count: Integer read FCount;
    property Relocations[Index: Integer]: TJclPeRelocation read GetRelocations; default;
    property Size: DWORD read GetSize;
    property VirtualAddress: DWORD read GetVirtualAddress;
  end;

  TJclPeRelocList = class(TJclPeImageBaseList)
  private
    FAllItemCount: Integer;
    function GetItems(Index: Integer): TJclPeRelocEntry;
    function GetAllItems(Index: Integer): TJclPeRelocation;
  protected
    procedure CreateList;
  public
    constructor Create(AImage: TJclPeImage);
    property AllItems[Index: Integer]: TJclPeRelocation read GetAllItems;
    property AllItemCount: Integer read FAllItemCount;
    property Items[Index: Integer]: TJclPeRelocEntry read GetItems; default;
  end;

  // Debug section related classes
  TJclPeDebugList = class(TJclPeImageBaseList)
  private
    function GetItems(Index: Integer): TImageDebugDirectory;
  protected
    procedure CreateList;
  public
    constructor Create(AImage: TJclPeImage);
    property Items[Index: Integer]: TImageDebugDirectory read GetItems; default;
  end;

  // Certificates section related classes
  TJclPeCertificate = class(TObject)
  private
    FData: Pointer;
    FHeader: TWinCertificate;
  public
    constructor Create(AHeader: TWinCertificate; AData: Pointer);
    property Data: Pointer read FData;
    property Header: TWinCertificate read FHeader;
  end;

  TJclPeCertificateList = class(TJclPeImageBaseList)
  private
    function GetItems(Index: Integer): TJclPeCertificate;
  protected
    procedure CreateList;
  public
    constructor Create(AImage: TJclPeImage);
    property Items[Index: Integer]: TJclPeCertificate read GetItems; default;
  end;

  // Common Language Runtime section related classes
  TJclPeCLRHeader = class(TObject)
  private
    FHeader: TImageCor20Header;
    FImage: TJclPeImage;
    function GetVersionString: string;
    function GetHasMetadata: Boolean;
  protected
    procedure ReadHeader;
  public
    constructor Create(AImage: TJclPeImage);
    property HasMetadata: Boolean read GetHasMetadata; 
    property Header: TImageCor20Header read FHeader;
    property VersionString: string read GetVersionString;
    property Image: TJclPeImage read FImage;
  end;

  // PE Image
  TJclPeHeader = (
    JclPeHeader_Signature,
    JclPeHeader_Machine,
    JclPeHeader_NumberOfSections,
    JclPeHeader_TimeDateStamp,
    JclPeHeader_PointerToSymbolTable,
    JclPeHeader_NumberOfSymbols,
    JclPeHeader_SizeOfOptionalHeader,
    JclPeHeader_Characteristics,
    JclPeHeader_Magic,
    JclPeHeader_LinkerVersion,
    JclPeHeader_SizeOfCode,
    JclPeHeader_SizeOfInitializedData,
    JclPeHeader_SizeOfUninitializedData,
    JclPeHeader_AddressOfEntryPoint,
    JclPeHeader_BaseOfCode,
    JclPeHeader_BaseOfData,
    JclPeHeader_ImageBase,
    JclPeHeader_SectionAlignment,
    JclPeHeader_FileAlignment,
    JclPeHeader_OperatingSystemVersion,
    JclPeHeader_ImageVersion,
    JclPeHeader_SubsystemVersion,
    JclPeHeader_Win32VersionValue,
    JclPeHeader_SizeOfImage,
    JclPeHeader_SizeOfHeaders,
    JclPeHeader_CheckSum,
    JclPeHeader_Subsystem,
    JclPeHeader_DllCharacteristics,
    JclPeHeader_SizeOfStackReserve,
    JclPeHeader_SizeOfStackCommit,
    JclPeHeader_SizeOfHeapReserve,
    JclPeHeader_SizeOfHeapCommit,
    JclPeHeader_LoaderFlags,
    JclPeHeader_NumberOfRvaAndSizes);

  TJclLoadConfig = (
    JclLoadConfig_Characteristics,   { TODO : rename to Size? }
    JclLoadConfig_TimeDateStamp,
    JclLoadConfig_Version,
    JclLoadConfig_GlobalFlagsClear,
    JclLoadConfig_GlobalFlagsSet,
    JclLoadConfig_CriticalSectionDefaultTimeout,
    JclLoadConfig_DeCommitFreeBlockThreshold,
    JclLoadConfig_DeCommitTotalFreeThreshold,
    JclLoadConfig_LockPrefixTable,
    JclLoadConfig_MaximumAllocationSize,
    JclLoadConfig_VirtualMemoryThreshold,
    JclLoadConfig_ProcessHeapFlags,
    JclLoadConfig_ProcessAffinityMask,
    JclLoadConfig_CSDVersion,
    JclLoadConfig_Reserved1,
    JclLoadConfig_EditList,
    JclLoadConfig_Reserved           { TODO : extend to the new fields? }
  );

  TJclPeFileProperties = record
    Size: DWORD;
    CreationTime: TDateTime;
    LastAccessTime: TDateTime;
    LastWriteTime: TDateTime;
    Attributes: Integer;
  end;

  TJclPeImageStatus = (stNotLoaded, stOk, stNotPE, stNotSupported, stNotFound, stError);
  TJclPeTarget = (taUnknown, taWin32, taWin64);

  TJclPeImage = class(TObject)
  private
    FAttachedImage: Boolean;
    FCertificateList: TJclPeCertificateList;
    FCLRHeader: TJclPeCLRHeader;
    FDebugList: TJclPeDebugList;
    FFileName: TFileName;
    FImageSections: TStringList;
    FLoadedImage: TLoadedImage;
    FExportList: TJclPeExportFuncList;
    FImportList: TJclPeImportList;
    FNoExceptions: Boolean;
    FReadOnlyAccess: Boolean;
    FRelocationList: TJclPeRelocList;
    FResourceList: TJclPeRootResourceList;
    FResourceVA: TJclAddr;
    FStatus: TJclPeImageStatus;
    FTarget: TJclPeTarget;
    FVersionInfo: TJclFileVersionInfo;
    function GetCertificateList: TJclPeCertificateList;
    function GetCLRHeader: TJclPeCLRHeader;
    function GetDebugList: TJclPeDebugList;
    function GetDescription: string;
    function GetDirectories(Directory: Word): TImageDataDirectory;
    function GetDirectoryExists(Directory: Word): Boolean;
    function GetExportList: TJclPeExportFuncList;
    function GetFileProperties: TJclPeFileProperties;
    function GetImageSectionCount: Integer;
    function GetImageSectionHeaders(Index: Integer): TImageSectionHeader;
    function GetImageSectionNames(Index: Integer): string;
    function GetImageSectionNameFromRva(const Rva: DWORD): string;
    function GetImportList: TJclPeImportList;
    function GetHeaderValues(Index: TJclPeHeader): string;
    function GetLoadConfigValues(Index: TJclLoadConfig): string;
    function GetMappedAddress: TJclAddr;
    {$IFDEF KEEP_DEPRECATED}
    function GetOptionalHeader: TImageOptionalHeader;
    {$ENDIF KEEP_DEPRECATED}
    function GetOptionalHeader32: TImageOptionalHeader32;
    function GetOptionalHeader64: TImageOptionalHeader64;
    function GetRelocationList: TJclPeRelocList;
    function GetResourceList: TJclPeRootResourceList;
    function GetUnusedHeaderBytes: TImageDataDirectory;
    function GetVersionInfo: TJclFileVersionInfo;
    function GetVersionInfoAvailable: Boolean;
    procedure ReadImageSections;
    procedure SetFileName(const Value: TFileName);
  protected
    procedure AfterOpen; dynamic;
    procedure CheckNotAttached;
    procedure Clear; dynamic;
    function ExpandModuleName(const ModuleName: string): TFileName;
    procedure RaiseStatusException;
    function ResourceItemCreate(AEntry: PImageResourceDirectoryEntry;
      AParentItem: TJclPeResourceItem): TJclPeResourceItem; virtual;
    function ResourceListCreate(ADirectory: PImageResourceDirectory;
      AParentItem: TJclPeResourceItem): TJclPeResourceList; virtual;
    property NoExceptions: Boolean read FNoExceptions;
  public
    constructor Create(ANoExceptions: Boolean = False); virtual;
    destructor Destroy; override;
    procedure AttachLoadedModule(const Handle: HMODULE);
    function CalculateCheckSum: DWORD;
    function DirectoryEntryToData(Directory: Word): Pointer;
    function GetSectionHeader(const SectionName: string; var Header: PImageSectionHeader): Boolean;
    function GetSectionName(Header: PImageSectionHeader): string;
    function IsBrokenFormat: Boolean;
    function IsCLR: Boolean;
    function IsSystemImage: Boolean;
    // RVA are always DWORD
    function RawToVa(Raw: DWORD): Pointer; overload;
    function RvaToSection(Rva: DWORD): PImageSectionHeader; overload;
    function RvaToVa(Rva: DWORD): Pointer; overload;
    function RvaToVaEx(Rva: DWORD): Pointer; overload;
    function StatusOK: Boolean;
    procedure TryGetNamesForOrdinalImports;
    function VerifyCheckSum: Boolean;
    class function DebugTypeNames(DebugType: DWORD): string;
    class function DirectoryNames(Directory: Word): string;
    class function ExpandBySearchPath(const ModuleName, BasePath: string): TFileName;
    class function HeaderNames(Index: TJclPeHeader): string;
    class function LoadConfigNames(Index: TJclLoadConfig): string;
    class function ShortSectionInfo(Characteristics: DWORD): string;
    class function DateTimeToStamp(const DateTime: TDateTime): DWORD;
    class function StampToDateTime(TimeDateStamp: DWORD): TDateTime;
    property AttachedImage: Boolean read FAttachedImage;
    property CertificateList: TJclPeCertificateList read GetCertificateList;
    property CLRHeader: TJclPeCLRHeader read GetCLRHeader;
    property DebugList: TJclPeDebugList read GetDebugList;
    property Description: string read GetDescription;
    property Directories[Directory: Word]: TImageDataDirectory read GetDirectories;
    property DirectoryExists[Directory: Word]: Boolean read GetDirectoryExists;
    property ExportList: TJclPeExportFuncList read GetExportList;
    property FileName: TFileName read FFileName write SetFileName;
    property FileProperties: TJclPeFileProperties read GetFileProperties;
    property HeaderValues[Index: TJclPeHeader]: string read GetHeaderValues;
    property ImageSectionCount: Integer read GetImageSectionCount;
    property ImageSectionHeaders[Index: Integer]: TImageSectionHeader read GetImageSectionHeaders;
    property ImageSectionNames[Index: Integer]: string read GetImageSectionNames;
    property ImageSectionNameFromRva[const Rva: DWORD]: string read GetImageSectionNameFromRva;
    property ImportList: TJclPeImportList read GetImportList;
    property LoadConfigValues[Index: TJclLoadConfig]: string read GetLoadConfigValues;
    property LoadedImage: TLoadedImage read FLoadedImage;
    property MappedAddress: TJclAddr read GetMappedAddress;
    {$IFDEF KEEP_DEPRECATED}
    property OptionalHeader: TImageOptionalHeader read GetOptionalHeader;
    {$ENDIF KEEP_DEPRECATED}
    property OptionalHeader32: TImageOptionalHeader32 read GetOptionalHeader32;
    property OptionalHeader64: TImageOptionalHeader64 read GetOptionalHeader64;
    property ReadOnlyAccess: Boolean read FReadOnlyAccess write FReadOnlyAccess;
    property RelocationList: TJclPeRelocList read GetRelocationList;
    property ResourceVA: DWORD read FResourceVA;
    property ResourceList: TJclPeRootResourceList read GetResourceList;
    property Status: TJclPeImageStatus read FStatus;
    property Target: TJclPeTarget read FTarget;
    property UnusedHeaderBytes: TImageDataDirectory read GetUnusedHeaderBytes;
    property VersionInfo: TJclFileVersionInfo read GetVersionInfo;
    property VersionInfoAvailable: Boolean read GetVersionInfoAvailable;
  end;

  // Borland Delphi PE Image specific information
  TJclPePackageInfo = class(TObject)
  private
    FAvailable: Boolean;
    FContains: TStringList;
    FDcpName: string;
    FRequires: TStringList;
    FFlags: Integer;
    FDescription: string;
    FEnsureExtension: Boolean;
    FSorted: Boolean;
    function GetContains: TStrings;
    function GetContainsCount: Integer;
    function GetContainsFlags(Index: Integer): Byte;
    function GetContainsNames(Index: Integer): string;
    function GetRequires: TStrings;
    function GetRequiresCount: Integer;
    function GetRequiresNames(Index: Integer): string;
  protected
    procedure ReadPackageInfo(ALibHandle: THandle);
    procedure SetDcpName(const Value: string);
  public
    constructor Create(ALibHandle: THandle);
    destructor Destroy; override;
    class function PackageModuleTypeToString(Flags: Integer): string;
    class function PackageOptionsToString(Flags: Integer): string;
    class function ProducerToString(Flags: Integer): string;
    class function UnitInfoFlagsToString(UnitFlags: Byte): string;
    property Available: Boolean read FAvailable;
    property Contains: TStrings read GetContains;
    property ContainsCount: Integer read GetContainsCount;
    property ContainsNames[Index: Integer]: string read GetContainsNames;
    property ContainsFlags[Index: Integer]: Byte read GetContainsFlags;
    property Description: string read FDescription;
    property DcpName: string read FDcpName;
    property EnsureExtension: Boolean read FEnsureExtension write FEnsureExtension;
    property Flags: Integer read FFlags;
    property Requires: TStrings read GetRequires;
    property RequiresCount: Integer read GetRequiresCount;
    property RequiresNames[Index: Integer]: string read GetRequiresNames;
    property Sorted: Boolean read FSorted write FSorted;
  end;

  TJclPeBorForm = class(TObject)
  private
    FFormFlags: TFilerFlags;
    FFormClassName: string;
    FFormObjectName: string;
    FFormPosition: Integer;
    FResItem: TJclPeResourceItem;
    function GetDisplayName: string;
  public
    constructor Create(AResItem: TJclPeResourceItem; AFormFlags: TFilerFlags;
      AFormPosition: Integer; const AFormClassName, AFormObjectName: string);
    procedure ConvertFormToText(const Stream: TStream); overload;
    procedure ConvertFormToText(const Strings: TStrings); overload;
    property FormClassName: string read FFormClassName;
    property FormFlags: TFilerFlags read FFormFlags;
    property FormObjectName: string read FFormObjectName;
    property FormPosition: Integer read FFormPosition;
    property DisplayName: string read GetDisplayName;
    property ResItem: TJclPeResourceItem read FResItem;
  end;

  TJclPeBorImage = class(TJclPeImage)
  private
    FForms: TObjectList;
    FIsPackage: Boolean;
    FIsBorlandImage: Boolean;
    FLibHandle: THandle;
    FPackageInfo: TJclPePackageInfo;
    FPackageInfoSorted: Boolean;
    FPackageCompilerVersion: Integer;
    function GetFormCount: Integer;
    function GetForms(Index: Integer): TJclPeBorForm;
    function GetFormFromName(const FormClassName: string): TJclPeBorForm;
    function GetLibHandle: THandle;
    function GetPackageCompilerVersion: Integer;
    function GetPackageInfo: TJclPePackageInfo;
  protected
    procedure AfterOpen; override;
    procedure Clear; override;
    procedure CreateFormsList;
  public
    constructor Create(ANoExceptions: Boolean = False); override;
    destructor Destroy; override;
    function DependedPackages(List: TStrings; FullPathName, Descriptions: Boolean): Boolean;
    function FreeLibHandle: Boolean;
    property Forms[Index: Integer]: TJclPeBorForm read GetForms;
    property FormCount: Integer read GetFormCount;
    property FormFromName[const FormClassName: string]: TJclPeBorForm read GetFormFromName;
    property IsBorlandImage: Boolean read FIsBorlandImage;
    property IsPackage: Boolean read FIsPackage;
    property LibHandle: THandle read GetLibHandle;
    property PackageCompilerVersion: Integer read GetPackageCompilerVersion;
    property PackageInfo: TJclPePackageInfo read GetPackageInfo;
    property PackageInfoSorted: Boolean read FPackageInfoSorted write FPackageInfoSorted;
  end;

  // Threaded function search
  TJclPeNameSearchOption = (seImports, seDelayImports, seBoundImports, seExports);
  TJclPeNameSearchOptions = set of TJclPeNameSearchOption;

  TJclPeNameSearchNotifyEvent = procedure (Sender: TObject; PeImage: TJclPeImage;
    var Process: Boolean) of object;
  TJclPeNameSearchFoundEvent = procedure (Sender: TObject; const FileName: TFileName;
    const FunctionName: string; Option: TJclPeNameSearchOption) of object;

  TJclPeNameSearch = class(TThread)
  private
    F_FileName: TFileName;
    F_FunctionName: string;
    F_Option: TJclPeNameSearchOption;
    F_Process: Boolean;
    FFunctionName: string;
    FOptions: TJclPeNameSearchOptions;
    FPath: string;
    FPeImage: TJclPeImage;
    FOnFound: TJclPeNameSearchFoundEvent;
    FOnProcessFile: TJclPeNameSearchNotifyEvent;
  protected
    function CompareName(const FunctionName, ComparedName: string): Boolean; virtual;
    procedure DoFound;
    procedure DoProcessFile;
    procedure Execute; override;
  public
    constructor Create(const FunctionName, Path: string; Options: TJclPeNameSearchOptions = [seImports, seExports]);
    procedure Start;
    property OnFound: TJclPeNameSearchFoundEvent read FOnFound write FOnFound;
    property OnProcessFile: TJclPeNameSearchNotifyEvent read FOnProcessFile write FOnProcessFile;
  end;

// PE Image miscellaneous functions
type
  TJclRebaseImageInfo32 = record
    OldImageSize: DWORD;
    OldImageBase: TJclAddr32;
    NewImageSize: DWORD;
    NewImageBase: TJclAddr32;
  end;
  TJclRebaseImageInfo64 = record
    OldImageSize: DWORD;
    OldImageBase: TJclAddr64;
    NewImageSize: DWORD;
    NewImageBase: TJclAddr64;
  end;
{$IFDEF KEEP_DEPRECATED}
type
  TJclRebaseImageInfo = TJclRebaseImageInfo32;
{$ENDIF KEEP_DEPRECATED}

{ Image validity }

function IsValidPeFile(const FileName: TFileName): Boolean;

{$IFDEF KEEP_DEPRECATED}
function PeGetNtHeaders(const FileName: TFileName; var NtHeaders: TImageNtHeaders): Boolean;
{$ENDIF KEEP_DEPRECATED}
function PeGetNtHeaders32(const FileName: TFileName; var NtHeaders: TImageNtHeaders32): Boolean;
function PeGetNtHeaders64(const FileName: TFileName; var NtHeaders: TImageNtHeaders64): Boolean;

{ Image modifications }

function PeCreateNameHintTable(const FileName: TFileName): Boolean;

{$IFDEF KEEP_DEPRECATED}
function PeRebaseImage(const ImageName: TFileName; NewBase: DWORD = 0; TimeStamp: DWORD = 0;
  MaxNewSize: DWORD = 0): TJclRebaseImageInfo;
{$ENDIF KEEP_DEPRECATED}
function PeRebaseImage32(const ImageName: TFileName; NewBase: TJclAddr32 = 0; TimeStamp: DWORD = 0;
  MaxNewSize: DWORD = 0): TJclRebaseImageInfo32;
function PeRebaseImage64(const ImageName: TFileName; NewBase: TJclAddr64 = 0; TimeStamp: DWORD = 0;
  MaxNewSize: DWORD = 0): TJclRebaseImageInfo64;

function PeUpdateLinkerTimeStamp(const FileName: string; const Time: TDateTime): Boolean;
function PeReadLinkerTimeStamp(const FileName: string): TDateTime;

function PeInsertSection(const FileName: TFileName; SectionStream: TStream; SectionName: string): Boolean;

{ Image Checksum }

function PeVerifyCheckSum(const FileName: TFileName): Boolean;
function PeClearCheckSum(const FileName: TFileName): Boolean;
function PeUpdateCheckSum(const FileName: TFileName): Boolean;

// Various simple PE Image searching and listing routines
{ Exports searching }

function PeDoesExportFunction(const FileName: TFileName; const FunctionName: string;
  Options: TJclSmartCompOptions = []): Boolean;

function PeIsExportFunctionForwardedEx(const FileName: TFileName; const FunctionName: string;
  var ForwardedName: string; Options: TJclSmartCompOptions = []): Boolean;
function PeIsExportFunctionForwarded(const FileName: TFileName; const FunctionName: string;
  Options: TJclSmartCompOptions = []): Boolean;

{ Imports searching }

function PeDoesImportFunction(const FileName: TFileName; const FunctionName: string;
  const LibraryName: string = ''; Options: TJclSmartCompOptions = []): Boolean;

function PeDoesImportLibrary(const FileName: TFileName; const LibraryName: string;
  Recursive: Boolean = False): Boolean;

{ Imports listing }

function PeImportedLibraries(const FileName: TFileName; const LibrariesList: TStrings;
  Recursive: Boolean = False; FullPathName: Boolean = False): Boolean;

function PeImportedFunctions(const FileName: TFileName; const FunctionsList: TStrings;
  const LibraryName: string = ''; IncludeLibNames: Boolean = False): Boolean;

{ Exports listing }

function PeExportedFunctions(const FileName: TFileName; const FunctionsList: TStrings): Boolean;
function PeExportedNames(const FileName: TFileName; const FunctionsList: TStrings): Boolean;
function PeExportedVariables(const FileName: TFileName; const FunctionsList: TStrings): Boolean;

{ Resources listing }

function PeResourceKindNames(const FileName: TFileName; ResourceType: TJclPeResourceKind;
  const NamesList: TStrings): Boolean;

{ Borland packages specific }

function PeBorFormNames(const FileName: TFileName; const NamesList: TStrings): Boolean;

function PeBorDependedPackages(const FileName: TFileName; PackagesList: TStrings;
  FullPathName, Descriptions: Boolean): Boolean;

// Missing imports checking routines
function PeFindMissingImports(const FileName: TFileName; MissingImportsList: TStrings): Boolean; overload;
function PeFindMissingImports(RequiredImportsList, MissingImportsList: TStrings): Boolean; overload;

function PeCreateRequiredImportList(const FileName: TFileName; RequiredImportsList: TStrings): Boolean;

// Mapped or loaded image related routines
{$IFDEF KEEP_DEPRECATED}
function PeMapImgNtHeaders(const BaseAddress: Pointer): PImageNtHeaders;
{$ENDIF KEEP_DEPRECATED}
function PeMapImgNtHeaders32(const BaseAddress: Pointer): PImageNtHeaders32;
function PeMapImgNtHeaders64(const BaseAddress: Pointer): PImageNtHeaders64;

function PeMapImgLibraryName(const BaseAddress: Pointer): string;
function PeMapImgSize(const BaseAddress: Pointer): DWORD;
function PeMapImgTarget(const BaseAddress: Pointer): TJclPeTarget;

{$IFDEF KEEP_DEPRECATED}
function PeMapImgSections(NtHeaders: PImageNtHeaders): PImageSectionHeader;
{$ENDIF KEEP_DEPRECATED}
function PeMapImgSections32(NtHeaders: PImageNtHeaders32): PImageSectionHeader;
function PeMapImgSections64(NtHeaders: PImageNtHeaders64): PImageSectionHeader;

{$IFDEF KEEP_DEPRECATED}
function PeMapImgFindSection(NtHeaders: PImageNtHeaders;
  const SectionName: string): PImageSectionHeader;
{$ENDIF KEEP_DEPRECATED}
function PeMapImgFindSection32(NtHeaders: PImageNtHeaders32;
  const SectionName: string): PImageSectionHeader;
function PeMapImgFindSection64(NtHeaders: PImageNtHeaders64;
  const SectionName: string): PImageSectionHeader;

function PeMapImgFindSectionFromModule(const BaseAddress: Pointer;
  const SectionName: string): PImageSectionHeader;

function PeMapImgExportedVariables(const Module: HMODULE; const VariablesList: TStrings): Boolean;

function PeMapImgResolvePackageThunk(Address: Pointer): Pointer;

function PeMapFindResource(const Module: HMODULE; const ResourceType: PChar;
  const ResourceName: string): Pointer;

type
  TJclPeSectionStream = class(TCustomMemoryStream)
  private
    FInstance: HMODULE;
    FSectionHeader: TImageSectionHeader;
    procedure Initialize(Instance: HMODULE; const ASectionName: string);
  public
    constructor Create(Instance: HMODULE; const ASectionName: string);
    function Write(const Buffer; Count: Longint): Longint; override;
    property Instance: HMODULE read FInstance;
    property SectionHeader: TImageSectionHeader read FSectionHeader;
  end;

// API hooking classes
type
  TJclPeMapImgHookItem = class(TObject)
  private
    FBaseAddress: Pointer;
    FFunctionName: string;
    FModuleName: string;
    FNewAddress: Pointer;
    FOriginalAddress: Pointer;
    FList: TObjectList;
  protected
    function InternalUnhook: Boolean;
  public
    constructor Create(AList: TObjectList; const AFunctionName, AModuleName: string;
      ABaseAddress, ANewAddress, AOriginalAddress: Pointer);
    destructor Destroy; override;
    function Unhook: Boolean;
    property BaseAddress: Pointer read FBaseAddress;
    property FunctionName: string read FFunctionName;
    property ModuleName: string read FModuleName;
    property NewAddress: Pointer read FNewAddress;
    property OriginalAddress: Pointer read FOriginalAddress;
  end;

  TJclPeMapImgHooks = class(TObjectList)
  private
    function GetItems(Index: Integer): TJclPeMapImgHookItem;
    function GetItemFromOriginalAddress(OriginalAddress: Pointer): TJclPeMapImgHookItem;
    function GetItemFromNewAddress(NewAddress: Pointer): TJclPeMapImgHookItem;
  public
    function HookImport(Base: Pointer; const ModuleName, FunctionName: string;
      NewAddress: Pointer; var OriginalAddress: Pointer): Boolean;
    class function IsWin9xDebugThunk(P: Pointer): Boolean;
    class function ReplaceImport(Base: Pointer; const ModuleName: string; FromProc, ToProc: Pointer): Boolean;
    class function SystemBase: Pointer;
    procedure UnhookAll;
    function UnhookByNewAddress(NewAddress: Pointer): Boolean;
    procedure UnhookByBaseAddress(BaseAddress: Pointer);
    property Items[Index: Integer]: TJclPeMapImgHookItem read GetItems; default;
    property ItemFromOriginalAddress[OriginalAddress: Pointer]: TJclPeMapImgHookItem read GetItemFromOriginalAddress;
    property ItemFromNewAddress[NewAddress: Pointer]: TJclPeMapImgHookItem read GetItemFromNewAddress;
  end;

// Image access under a debbuger
{$IFDEF KEEP_DEPRECATED}
function PeDbgImgNtHeaders(ProcessHandle: THandle; BaseAddress: Pointer;
  var NtHeaders: TImageNtHeaders32): Boolean;
{$ENDIF KEEP_DEPRECATED}
function PeDbgImgNtHeaders32(ProcessHandle: THandle; BaseAddress: TJclAddr32;
  var NtHeaders: TImageNtHeaders32): Boolean;
// TODO 64 bit version
//function PeDbgImgNtHeaders64(ProcessHandle: THandle; BaseAddress: TJclAddr64;
//  var NtHeaders: TImageNtHeaders64): Boolean;

{$IFDEF KEEP_DEPRECATED}
function PeDbgImgLibraryName(ProcessHandle: THandle; BaseAddress: Pointer;
  var Name: string): Boolean;
{$ENDIF KEEP_DEPRECATED}
function PeDbgImgLibraryName32(ProcessHandle: THandle; BaseAddress: TJclAddr32;
  var Name: string): Boolean;
//function PeDbgImgLibraryName64(ProcessHandle: THandle; BaseAddress: TJclAddr64;
//  var Name: string): Boolean;

// Borland BPL packages name unmangling
type
  TJclBorUmSymbolKind = (skData, skFunction, skConstructor, skDestructor, skRTTI, skVTable);
  TJclBorUmSymbolModifier = (smQualified, smLinkProc);
  TJclBorUmSymbolModifiers = set of TJclBorUmSymbolModifier;
  TJclBorUmDescription = record
    Kind: TJclBorUmSymbolKind;
    Modifiers: TJclBorUmSymbolModifiers;
  end;
  TJclBorUmResult = (urOk, urNotMangled, urMicrosoft, urError);
  TJclPeUmResult = (umNotMangled, umBorland, umMicrosoft);

function PeBorUnmangleName(const Name: string; var Unmangled: string;
  var Description: TJclBorUmDescription; var BasePos: Integer): TJclBorUmResult; overload;
function PeBorUnmangleName(const Name: string; var Unmangled: string;
  var Description: TJclBorUmDescription): TJclBorUmResult; overload;
function PeBorUnmangleName(const Name: string; var Unmangled: string): TJclBorUmResult; overload;
function PeBorUnmangleName(const Name: string): string; overload;

function PeIsNameMangled(const Name: string): TJclPeUmResult;

function PeUnmangleName(const Name: string; var Unmangled: string): TJclPeUmResult;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclPeImage.pas $';
    Revision: '$Revision: 2372 $';
    Date: '$Date: 2008-05-21 22:19:06 +0200 (mer., 21 mai 2008) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclLogic, JclResources, JclSysUtils;

const
  MANIFESTExtension = '.manifest';

  PackageInfoResName    = 'PACKAGEINFO';
  DescriptionResName    = 'DESCRIPTION';
  PackageOptionsResName = 'PACKAGEOPTIONS';
  DVclAlResName         = 'DVCLAL';

  DebugSectionName    = '.debug';
  ReadOnlySectionName = '.rdata';

  BinaryExtensionPackage = '.bpl';
  BinaryExtensionLibrary = '.dll';
  CompilerExtensionDCP   = '.dcp';

// Helper routines
function AddFlagTextRes(var Text: string; const FlagText: PResStringRec; const Value, Mask: Integer): Boolean;
begin
  Result := (Value and Mask <> 0);
  if Result then
  begin
    if Length(Text) > 0 then
      Text := Text + ', ';
    Text := Text + LoadResString(FlagText);
  end;
end;

function CompareResourceName(T1, T2: PChar): Boolean;
begin
  if (LongRec(T1).Hi = 0) or (LongRec(T2).Hi = 0) then
    Result := Word(T1) = Word(T2)
  else
    Result := (StrIComp(T1, T2) = 0);
end;

function CreatePeImage(const FileName: TFileName): TJclPeImage;
begin
  Result := TJclPeImage.Create(True);
  Result.FileName := FileName;
end;

function InternalImportedLibraries(const FileName: TFileName;
  Recursive, FullPathName: Boolean; ExternalCache: TJclPeImagesCache): TStringList;
var
  Cache: TJclPeImagesCache;

  procedure ProcessLibraries(const AFileName: TFileName);
  var
    I: Integer;
    S: string;
    ImportLib: TJclPeImportLibItem;
  begin
    with Cache[AFileName].ImportList do
      for I := 0 to Count - 1 do
      begin
        ImportLib := Items[I];
        if FullPathName then
          S := ImportLib.FileName
        else
          S := ImportLib.Name;
        if Result.IndexOf(S) = -1 then
        begin
          Result.Add(S);
          if Recursive then
            ProcessLibraries(ImportLib.FileName);
        end;
      end;
  end;

begin
  if ExternalCache = nil then
    Cache := TJclPeImagesCache.Create
  else
    Cache := ExternalCache;
  try
    Result := TStringList.Create;
    try
      Result.Sorted := True;
      Result.Duplicates := dupIgnore;
      ProcessLibraries(FileName);
    except
      FreeAndNil(Result);
      raise;
    end;
  finally
    if ExternalCache = nil then
      Cache.Free;
  end;
end;

// Smart name compare function
function PeStripFunctionAW(const FunctionName: string): string;
var
  L: Integer;
begin
  Result := FunctionName;
  L := Length(Result);
  // (rom) possible bug. 'A'..'Z' missing from set (better use AnsiValidIdentifierLetters).
  if (L > 1) and (Result[L] in ['A', 'W']) and
    (Result[L - 1] in ['a'..'z', '_', '0'..'9']) then
    Delete(Result, L, 1);
end;

function PeSmartFunctionNameSame(const ComparedName, FunctionName: string;
  Options: TJclSmartCompOptions): Boolean;
var
  S: string;
begin
  if scIgnoreCase in Options then
    Result := StrSame(FunctionName, ComparedName)
  else
    Result := (FunctionName = ComparedName);
  if (not Result) and not (scSimpleCompare in Options) then
  begin
    if Length(FunctionName) > 0 then
    begin
      S := PeStripFunctionAW(FunctionName);
      if scIgnoreCase in Options then
        Result := StrSame(S, ComparedName)
      else
        Result := (S = ComparedName);
    end
    else
      Result := False;
  end;
end;

//=== { TJclPeImagesCache } ==================================================

constructor TJclPeImagesCache.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  FList.Sorted := True;
  FList.Duplicates := dupIgnore;
end;

destructor TJclPeImagesCache.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TJclPeImagesCache.Clear;
var
  I: Integer;
begin
  with FList do
    for I := 0 to Count - 1 do
      Objects[I].Free;
  FList.Clear;
end;

function TJclPeImagesCache.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJclPeImagesCache.GetImages(const FileName: TFileName): TJclPeImage;
var
  I: Integer;
begin
  I := FList.IndexOf(FileName);
  if I = -1 then
  begin
    Result := GetPeImageClass.Create(True);
    Result.FileName := FileName;
    FList.AddObject(FileName, Result);
  end
  else
    Result := TJclPeImage(FList.Objects[I]);
end;

function TJclPeImagesCache.GetPeImageClass: TJclPeImageClass;
begin
  Result := TJclPeImage;
end;

//=== { TJclPeBorImagesCache } ===============================================

function TJclPeBorImagesCache.GetImages(const FileName: TFileName): TJclPeBorImage;
begin
  Result := TJclPeBorImage(inherited Images[FileName]);
end;

function TJclPeBorImagesCache.GetPeImageClass: TJclPeImageClass;
begin
  Result := TJclPeBorImage;
end;

//=== { TJclPeImageBaseList } ================================================

constructor TJclPeImageBaseList.Create(AImage: TJclPeImage);
begin
  inherited Create(True);
  FImage := AImage;
end;

// Import sort functions

function ImportSortByName(Item1, Item2: Pointer): Integer;
begin
  Result := CompareStr(TJclPeImportFuncItem(Item1).Name, TJclPeImportFuncItem(Item2).Name);
  if Result = 0 then
    Result := CompareStr(TJclPeImportFuncItem(Item1).ImportLib.Name, TJclPeImportFuncItem(Item2).ImportLib.Name);
  if Result = 0 then
    Result := TJclPeImportFuncItem(Item1).Ordinal - TJclPeImportFuncItem(Item2).Ordinal;
end;

function ImportSortByNameDESC(Item1, Item2: Pointer): Integer;
begin
  Result := ImportSortByName(Item2, Item1);
end;

function ImportSortByHint(Item1, Item2: Pointer): Integer;
begin
  Result := TJclPeImportFuncItem(Item1).Hint - TJclPeImportFuncItem(Item2).Hint;
end;

function ImportSortByHintDESC(Item1, Item2: Pointer): Integer;
begin
  Result := ImportSortByHint(Item2, Item1);
end;

function ImportSortByDll(Item1, Item2: Pointer): Integer;
begin
  Result := CompareStr(TJclPeImportFuncItem(Item1).ImportLib.Name,
    TJclPeImportFuncItem(Item2).ImportLib.Name);
  if Result = 0 then
    Result := ImportSortByName(Item1, Item2);
end;

function ImportSortByDllDESC(Item1, Item2: Pointer): Integer;
begin
  Result := ImportSortByDll(Item2, Item1);
end;

function ImportSortByOrdinal(Item1, Item2: Pointer): Integer;
begin
  Result := CompareStr(TJclPeImportFuncItem(Item1).ImportLib.Name,
    TJclPeImportFuncItem(Item2).ImportLib.Name);
  if Result = 0 then
    Result := TJclPeImportFuncItem(Item1).Ordinal -  TJclPeImportFuncItem(Item2).Ordinal;
end;

function ImportSortByOrdinalDESC(Item1, Item2: Pointer): Integer;
begin
  Result := ImportSortByOrdinal(Item2, Item1);
end;

function GetImportSortFunction(SortType: TJclPeImportSort; Descending: Boolean): TListSortCompare;
const
  SortFunctions: array [TJclPeImportSort, Boolean] of TListSortCompare =
    ((ImportSortByName, ImportSortByNameDESC),
     (ImportSortByOrdinal, ImportSortByOrdinalDESC),
     (ImportSortByHint, ImportSortByHintDESC),
     (ImportSortByDll, ImportSortByDllDESC)
    );
begin
  Result := SortFunctions[SortType, Descending];
end;

function ImportLibSortByIndex(Item1, Item2: Pointer): Integer;
begin
  Result := TJclPeImportLibItem(Item1).ImportDirectoryIndex -
    TJclPeImportLibItem(Item2).ImportDirectoryIndex;
end;

function ImportLibSortByName(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareStr(TJclPeImportLibItem(Item1).Name, TJclPeImportLibItem(Item2).Name);
  if Result = 0 then
    Result := ImportLibSortByIndex(Item1, Item2);
end;

function GetImportLibSortFunction(SortType: TJclPeImportLibSort): TListSortCompare;
const
  SortFunctions: array [TJclPeImportLibSort] of TListSortCompare =
    (ImportLibSortByName, ImportLibSortByIndex);
begin
  Result := SortFunctions[SortType];
end;

//=== { TJclPeImportFuncItem } ===============================================

constructor TJclPeImportFuncItem.Create(AImportLib: TJclPeImportLibItem;
  AOrdinal: Word; AHint: Word; const AName: string);
begin
  inherited Create;
  FImportLib := AImportLib;
  FOrdinal := AOrdinal;
  FHint := AHint;
  FName := AName;
  FResolveCheck := icNotChecked;
  FIndirectImportName := False;
end;

function TJclPeImportFuncItem.GetIsByOrdinal: Boolean;
begin
  Result := FOrdinal <> 0;
end;

procedure TJclPeImportFuncItem.SetIndirectImportName(const Value: string);
begin
  FName := Value;
  FIndirectImportName := True;
end;

procedure TJclPeImportFuncItem.SetName(const Value: string);
begin
  FName := Value;
  FIndirectImportName := False;
end;

procedure TJclPeImportFuncItem.SetResolveCheck(Value: TJclPeResolveCheck);
begin
  FResolveCheck := Value;
end;

//=== { TJclPeImportLibItem } ================================================

constructor TJclPeImportLibItem.Create(AImage: TJclPeImage;
  AImportDescriptor: Pointer; AImportKind: TJclPeImportKind; const AName: AnsiString;
  AThunk: Pointer);
begin
  inherited Create(AImage);
  FTotalResolveCheck := icNotChecked;
  FImportDescriptor := AImportDescriptor;
  FImportKind := AImportKind;
  FName := AName;
  FThunk := AThunk;
  FThunkData := AThunk;
end;

procedure TJclPeImportLibItem.CheckImports(ExportImage: TJclPeImage);
var
  I: Integer;
  ExportList: TJclPeExportFuncList;
begin
  if ExportImage.StatusOK then
  begin
    FTotalResolveCheck := icResolved;
    ExportList := ExportImage.ExportList;
    for I := 0 to Count - 1 do
    begin
      with Items[I] do
        if IsByOrdinal then
        begin
          if ExportList.OrdinalValid(Ordinal) then
            SetResolveCheck(icResolved)
          else
          begin
            SetResolveCheck(icUnresolved);
            Self.FTotalResolveCheck := icUnresolved;
          end;
        end
        else
        begin
          if ExportList.ItemFromName[Items[I].Name] <> nil then
            SetResolveCheck(icResolved)
          else
          begin
            SetResolveCheck(icUnresolved);
            Self.FTotalResolveCheck := icUnresolved;
          end;
        end;
    end;
  end
  else
  begin
    FTotalResolveCheck := icUnresolved;
    for I := 0 to Count - 1 do
      Items[I].SetResolveCheck(icUnresolved);
  end;
end;

procedure TJclPeImportLibItem.CreateList;
  procedure CreateList32;
  var
    Thunk32: PImageThunkData32;
    OrdinalName: PImageImportByName;
    Ordinal, Hint: Word;
    Name: PChar;
  begin
    Thunk32 := PImageThunkData32(FThunk);
    while Thunk32^.Function_ <> 0 do
    begin
      Ordinal := 0;
      Hint := 0;
      Name := nil;
      if Thunk32^.Ordinal and IMAGE_ORDINAL_FLAG32 = 0 then
      begin
        case ImportKind of
          ikImport, ikBoundImport:
            begin
              OrdinalName := PImageImportByName(Image.RvaToVa(Thunk32^.AddressOfData));
              Hint := OrdinalName.Hint;
              Name := OrdinalName.Name;
            end;
          ikDelayImport:
            begin
              OrdinalName := PImageImportByName(Image.RvaToVaEx(Thunk32^.AddressOfData));
              Hint := OrdinalName.Hint;
              Name := OrdinalName.Name;
            end;
        end;
      end
      else
        Ordinal := IMAGE_ORDINAL32(Thunk32^.Ordinal);

      Add(TJclPeImportFuncItem.Create(Self, Ordinal, Hint, Name));
      Inc(Thunk32);
    end;
  end;

  procedure CreateList64;
  var
    Thunk64: PImageThunkData64;
    OrdinalName: PImageImportByName;
    Ordinal, Hint: Word;
    Name: PChar;
  begin
    Thunk64 := PImageThunkData64(FThunk);
    while Thunk64^.Function_ <> 0 do
    begin
      Ordinal := 0;
      Hint := 0;
      Name := nil;
      if Thunk64^.Ordinal and IMAGE_ORDINAL_FLAG64 = 0 then
      begin
        case ImportKind of
          ikImport, ikBoundImport:
            begin
              OrdinalName := PImageImportByName(Image.RvaToVa(Thunk64^.AddressOfData));
              Hint := OrdinalName.Hint;
              Name := OrdinalName.Name;
            end;
          ikDelayImport:
            begin
              OrdinalName := PImageImportByName(Image.RvaToVaEx(Thunk64^.AddressOfData));
              Hint := OrdinalName.Hint;
              Name := OrdinalName.Name;
            end;
        end;
      end
      else
        Ordinal := IMAGE_ORDINAL64(Thunk64^.Ordinal);

      Add(TJclPeImportFuncItem.Create(Self, Ordinal, Hint, Name));
      Inc(Thunk64);
    end;
  end;
begin
  if FThunk = nil then
    Exit;

  case Image.Target of
    taWin32:
      CreateList32;
    taWin64:
      CreateList64;
  end;

  FThunk := nil;
end;

function TJclPeImportLibItem.GetCount: Integer;
begin
  if FThunk <> nil then
    CreateList;
  Result := inherited Count;
end;

function TJclPeImportLibItem.GetFileName: TFileName;
begin
  Result := Image.ExpandModuleName(Name);
end;

function TJclPeImportLibItem.GetItems(Index: Integer): TJclPeImportFuncItem;
begin
  Result := TJclPeImportFuncItem(Get(Index));
end;

function TJclPeImportLibItem.GetName: string;
begin
  Result := AnsiLowerCase(OriginalName);
end;

{$IFDEF KEEP_DEPRECATED}
function TJclPeImportLibItem.GetThunkData: PImageThunkData;
begin
  Result := FThunkData;
end;
{$ENDIF KEEP_DEPRECATED}

function TJclPeImportLibItem.GetThunkData32: PImageThunkData32;
begin
  if Image.Target = taWin32 then
    Result := FThunkData
  else
    Result := nil;
end;

function TJclPeImportLibItem.GetThunkData64: PImageThunkData64;
begin
  if Image.Target = taWin64 then
    Result := FThunkData
  else
    Result := nil;
end;

procedure TJclPeImportLibItem.SetImportDirectoryIndex(Value: Integer);
begin
  FImportDirectoryIndex := Value;
end;

procedure TJclPeImportLibItem.SetImportKind(Value: TJclPeImportKind);
begin
  FImportKind := Value;
end;

procedure TJclPeImportLibItem.SetSorted(Value: Boolean);
begin
  FSorted := Value;
end;

procedure TJclPeImportLibItem.SetThunk(Value: Pointer);
begin
  FThunk := Value;
  FThunkData := Value;
end;

procedure TJclPeImportLibItem.SortList(SortType: TJclPeImportSort; Descending: Boolean);
begin
  if not FSorted or (SortType <> FLastSortType) or (Descending <> FLastSortDescending) then
  begin
    GetCount; // create list if it wasn't created
    Sort(GetImportSortFunction(SortType, Descending));
    FLastSortType := SortType;
    FLastSortDescending := Descending;
    FSorted := True;
  end;
end;

//=== { TJclPeImportList } ===================================================

constructor TJclPeImportList.Create(AImage: TJclPeImage);
begin
  inherited Create(AImage);
  FAllItemsList := TList.Create;
  FAllItemsList.Capacity := 256;
  FUniqueNamesList := TStringList.Create;
  FUniqueNamesList.Sorted := True;
  FUniqueNamesList.Duplicates := dupIgnore;
  FLastAllSortType := isName;
  FLastAllSortDescending := False;
  CreateList;
end;

destructor TJclPeImportList.Destroy;
var
  I: Integer;
begin
  FreeAndNil(FAllItemsList);
  FreeAndNil(FUniqueNamesList);
  for I := 0 to Length(FparallelImportTable) - 1 do
    FreeMem(FparallelImportTable[I]);
  inherited Destroy;
end;

procedure TJclPeImportList.CheckImports(PeImageCache: TJclPeImagesCache);
var
  I: Integer;
  ExportPeImage: TJclPeImage;
begin
  Image.CheckNotAttached;
  if PeImageCache <> nil then
    ExportPeImage := nil // to make the compiler happy
  else
    ExportPeImage := TJclPeImage.Create(True);
  try
    for I := 0 to Count - 1 do
      if Items[I].TotalResolveCheck = icNotChecked then
      begin
        if PeImageCache <> nil then
          ExportPeImage := PeImageCache[Items[I].FileName]
        else
          ExportPeImage.FileName := Items[I].FileName;
        ExportPeImage.ExportList.PrepareForFastNameSearch;
        Items[I].CheckImports(ExportPeImage);
      end;
  finally
    if PeImageCache = nil then
      ExportPeImage.Free;
  end;
end;

procedure TJclPeImportList.CreateList;
  procedure CreateDelayImportList32(DelayImportDesc: PImgDelayDescrV1);
  var
    LibItem: TJclPeImportLibItem;
  begin
    while DelayImportDesc^.szName <> nil do
    begin
      LibItem := TJclPeImportLibItem.Create(Image, DelayImportDesc, ikDelayImport,
        PAnsiChar(Image.RvaToVaEx(DWORD(DelayImportDesc^.szName))), Image.RvaToVaEx(DWORD(DelayImportDesc^.pINT)));
      Add(LibItem);
      FUniqueNamesList.AddObject(AnsiLowerCase(LibItem.Name), LibItem);
      Inc(DelayImportDesc);
    end;
  end;

  procedure CreateDelayImportList64(DelayImportDesc: PImgDelayDescrV2);
  var
    LibItem: TJclPeImportLibItem;
  begin
    while DelayImportDesc^.rvaDLLName <> 0 do
    begin
      LibItem := TJclPeImportLibItem.Create(Image, DelayImportDesc, ikDelayImport,
        PAnsiChar(Image.RvaToVa(DelayImportDesc^.rvaDLLName)), Image.RvaToVa(DelayImportDesc^.rvaINT));
      Add(LibItem);
      FUniqueNamesList.AddObject(AnsiLowerCase(LibItem.Name), LibItem);
      Inc(DelayImportDesc);
    end;
  end;
var
  ImportDesc: PImageImportDescriptor;
  LibItem: TJclPeImportLibItem;
  DelayImportDesc: Pointer;
  BoundImports, BoundImport: PImageBoundImportDescriptor;
  S: string;
  I: Integer;
  Thunk: Pointer;
begin
  SetCapacity(100);
  with Image do
  begin
    if not StatusOK then
      Exit;
    ImportDesc := DirectoryEntryToData(IMAGE_DIRECTORY_ENTRY_IMPORT);
    if ImportDesc <> nil then
      while ImportDesc^.Name <> 0 do
      begin
        if ImportDesc^.Union.Characteristics = 0 then
        begin
          if AttachedImage then  // Borland images doesn't have two parallel arrays
            Thunk := nil // see MakeBorlandImportTableForMappedImage method
          else
            Thunk := RvaToVa(ImportDesc^.FirstThunk);
          FLinkerProducer := lrBorland;
        end
        else
        begin
          Thunk := RvaToVa(ImportDesc^.Union.Characteristics);
          FLinkerProducer := lrMicrosoft;
        end;
        LibItem := TJclPeImportLibItem.Create(Image, ImportDesc, ikImport, PAnsiChar(RvaToVa(ImportDesc^.Name)), Thunk);
        Add(LibItem);
        FUniqueNamesList.AddObject(AnsiLowerCase(LibItem.Name), LibItem);
        Inc(ImportDesc);
      end;
    DelayImportDesc := DirectoryEntryToData(IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT);
    if DelayImportDesc <> nil then
    begin
      case Target of
        taWin32:
          CreateDelayImportList32(DelayImportDesc);
        taWin64:
          CreateDelayImportList64(DelayImportDesc);
      end;
    end;
    BoundImports := DirectoryEntryToData(IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT);
    if BoundImports <> nil then
    begin
      BoundImport := BoundImports;
      while BoundImport^.OffsetModuleName <> 0 do
      begin
        S := AnsiLowerCase(PChar(TJclAddr(BoundImports) + BoundImport^.OffsetModuleName));
        I := FUniqueNamesList.IndexOf(S);
        if I >= 0 then
          TJclPeImportLibItem(FUniqueNamesList.Objects[I]).SetImportKind(ikBoundImport);
        for I := 1 to BoundImport^.NumberOfModuleForwarderRefs do
          Inc(PImageBoundForwarderRef(BoundImport)); // skip forward information
        Inc(BoundImport);
      end;
    end;
  end;
  for I := 0 to Count - 1 do
    Items[I].SetImportDirectoryIndex(I);
end;

function TJclPeImportList.GetAllItemCount: Integer;
begin
  Result := FAllItemsList.Count;
  if Result = 0 then // we haven't created the list yet -> create unsorted list
  begin
    RefreshAllItems;
    Result := FAllItemsList.Count;
  end;
end;

function TJclPeImportList.GetAllItems(Index: Integer): TJclPeImportFuncItem;
begin
  Result := TJclPeImportFuncItem(FAllItemsList[Index]);
end;

function TJclPeImportList.GetItems(Index: Integer): TJclPeImportLibItem;
begin
  Result := TJclPeImportLibItem(Get(Index));
end;

function TJclPeImportList.GetUniqueLibItemCount: Integer;
begin
  Result := FUniqueNamesList.Count;
end;

function TJclPeImportList.GetUniqueLibItemFromName(const Name: string): TJclPeImportLibItem;
var
  I: Integer;
begin
  I := FUniqueNamesList.IndexOf(Name);
  if I = -1 then
    Result := nil
  else
    Result := TJclPeImportLibItem(FUniqueNamesList.Objects[I]);
end;

function TJclPeImportList.GetUniqueLibItems(Index: Integer): TJclPeImportLibItem;
begin
  Result := TJclPeImportLibItem(FUniqueNamesList.Objects[Index]);
end;

function TJclPeImportList.GetUniqueLibNames(Index: Integer): string;
begin
  Result := FUniqueNamesList[Index];
end;

function TJclPeImportList.MakeBorlandImportTableForMappedImage: Boolean;
var
  FileImage: TJclPeImage;
  I, TableSize: Integer;
begin
  if Image.AttachedImage and (LinkerProducer = lrBorland) and
    (Length(FParallelImportTable) = 0) then
  begin
    FileImage := TJclPeImage.Create(True);
    try
      FileImage.FileName := Image.FileName;
      Result := FileImage.StatusOK;
      if Result then
      begin
        SetLength(FParallelImportTable, FileImage.ImportList.Count);
        for I := 0 to FileImage.ImportList.Count - 1 do
        begin
          Assert(Items[I].ImportKind = ikImport); // Borland doesn't have Delay load or Bound imports
          TableSize := (FileImage.ImportList[I].Count + 1);
          case Image.Target of
            taWin32:
              begin
                TableSize := TableSize * SizeOf(TImageThunkData32);
                GetMem(FParallelImportTable[I], TableSize);
                System.Move(FileImage.ImportList[I].ThunkData32^, FParallelImportTable[I]^, TableSize);
                Items[I].SetThunk(FParallelImportTable[I]);
              end;
            taWin64:
              begin
                TableSize := TableSize * SizeOf(TImageThunkData64);
                GetMem(FParallelImportTable[I], TableSize);
                System.Move(FileImage.ImportList[I].ThunkData64^, FParallelImportTable[I]^, TableSize);
                Items[I].SetThunk(FParallelImportTable[I]);
              end;
          end;
        end;
      end;
    finally
      FileImage.Free;
    end;
  end
  else
    Result := True;
end;

procedure TJclPeImportList.RefreshAllItems;
var
  L, I: Integer;
  LibItem: TJclPeImportLibItem;
begin
  FAllItemsList.Clear;
  for L := 0 to Count - 1 do
  begin
    LibItem := Items[L];
    if (Length(FFilterModuleName) = 0) or (AnsiCompareText(LibItem.Name, FFilterModuleName) = 0) then
      for I := 0 to LibItem.Count - 1 do
        FAllItemsList.Add(LibItem[I]);
  end;
end;

procedure TJclPeImportList.SetFilterModuleName(const Value: string);
begin
  if (FFilterModuleName <> Value) or (FAllItemsList.Count = 0) then
  begin
    FFilterModuleName := Value;
    RefreshAllItems;
    FAllItemsList.Sort(GetImportSortFunction(FLastAllSortType, FLastAllSortDescending));
  end;
end;

function TJclPeImportList.SmartFindName(const CompareName, LibName: string;
  Options: TJclSmartCompOptions): TJclPeImportFuncItem;
var
  L, I: Integer;
  LibItem: TJclPeImportLibItem;
begin
  Result := nil;
  for L := 0 to Count - 1 do
  begin
    LibItem := Items[L];
    if (Length(LibName) = 0) or (AnsiCompareText(LibItem.Name, LibName) = 0) then
      for I := 0 to LibItem.Count - 1 do
        if PeSmartFunctionNameSame(CompareName, LibItem[I].Name, Options) then
        begin
          Result := LibItem[I];
          Break;
        end;
  end;
end;

procedure TJclPeImportList.SortAllItemsList(SortType: TJclPeImportSort; Descending: Boolean);
begin
  GetAllItemCount; // create list if it wasn't created
  FAllItemsList.Sort(GetImportSortFunction(SortType, Descending));
  FLastAllSortType := SortType;
  FLastAllSortDescending := Descending;
end;

procedure TJclPeImportList.SortList(SortType: TJclPeImportLibSort);
begin
  Sort(GetImportLibSortFunction(SortType));
end;

procedure TJclPeImportList.TryGetNamesForOrdinalImports;
var
  LibNamesList: TStringList;
  L, I: Integer;
  LibPeDump: TJclPeImage;

  procedure TryGetNames(const ModuleName: string);
  var
    Item: TJclPeImportFuncItem;
    I, L: Integer;
    ImportLibItem: TJclPeImportLibItem;
    ExportItem: TJclPeExportFuncItem;
    ExportList: TJclPeExportFuncList;
  begin
    if Image.AttachedImage then
      LibPeDump.AttachLoadedModule(GetModuleHandle(PChar(ModuleName)))
    else
      LibPeDump.FileName := Image.ExpandModuleName(ModuleName);
    if not LibPeDump.StatusOK then
      Exit;
    ExportList := LibPeDump.ExportList;
    for L := 0 to Count - 1 do
    begin
      ImportLibItem := Items[L];
      if AnsiCompareText(ImportLibItem.Name, ModuleName) = 0 then
      begin
        for I := 0 to ImportLibItem.Count - 1 do
        begin
          Item := ImportLibItem[I];
          if Item.IsByOrdinal then
          begin
            ExportItem := ExportList.ItemFromOrdinal[Item.Ordinal];
            if (ExportItem <> nil) and (ExportItem.Name <> '') then
              Item.SetIndirectImportName(ExportItem.Name);
          end;
        end;
        ImportLibItem.SetSorted(False);
      end;
    end;
  end;

begin
  LibNamesList := TStringList.Create;
  try
    LibNamesList.Sorted := True;
    LibNamesList.Duplicates := dupIgnore;
    for L := 0 to Count - 1 do
      with Items[L] do
        for I := 0 to Count - 1 do
          if Items[I].IsByOrdinal then
            LibNamesList.Add(AnsiUpperCase(Name));
    LibPeDump := TJclPeImage.Create(True);
    try
      for I := 0 to LibNamesList.Count - 1 do
        TryGetNames(LibNamesList[I]);
    finally
      LibPeDump.Free;
    end;
    SortAllItemsList(FLastAllSortType, FLastAllSortDescending);
  finally
    LibNamesList.Free;
  end;
end;

//=== { TJclPeExportFuncItem } ===============================================

constructor TJclPeExportFuncItem.Create(AExportList: TJclPeExportFuncList;
  const AName, AForwardedName: string; AAddress: DWORD; AHint: Word;
  AOrdinal: Word; AResolveCheck: TJclPeResolveCheck);
var
  DotPos: Integer;
begin
  inherited Create;
  FExportList := AExportList;
  FName := AName;
  FForwardedName := AForwardedName;
  FAddress := AAddress;
  FHint := AHint;
  FOrdinal := AOrdinal;
  FResolveCheck := AResolveCheck;

  DotPos := AnsiPos('.', ForwardedName);
  if DotPos > 0 then
    FForwardedDotPos := Copy(ForwardedName, DotPos + 1, Length(ForwardedName) - DotPos)
  else
    FForwardedDotPos := '';
end;

function TJclPeExportFuncItem.GetAddressOrForwardStr: string;
begin
  if IsForwarded then
    Result := ForwardedName
  else
    FmtStr(Result, '%.8x', [Address]);
end;

function TJclPeExportFuncItem.GetForwardedFuncName: string;
begin
  if (Length(FForwardedDotPos) > 0) and (FForwardedDotPos[1] <> '#') then
    Result := FForwardedDotPos
  else
    Result := '';
end;

function TJclPeExportFuncItem.GetForwardedFuncOrdinal: DWORD;
begin
  if (Length(FForwardedDotPos) > 0) and (FForwardedDotPos[1] = '#') then
    Result := StrToIntDef(FForwardedDotPos, 0)
  else
    Result := 0;
end;

function TJclPeExportFuncItem.GetForwardedLibName: string;
begin
  if Length(FForwardedDotPos) = 0 then
    Result := ''
  else
    Result := AnsiLowerCase(Copy(FForwardedName, 1, Length(FForwardedName) - Length(FForwardedDotPos) - 1)) + BinaryExtensionLibrary;
end;

function TJclPeExportFuncItem.GetIsExportedVariable: Boolean;
begin
  case FExportList.Image.Target of
    taWin32:
      Result := (Address >= FExportList.Image.OptionalHeader32.BaseOfData);
    taWin64:
      Result := False;
      // TODO equivalent for 64-bit modules
      //Result := (Address >= FExportList.Image.OptionalHeader64.BaseOfData);
  else
    Result := False;
  end;
end;

function TJclPeExportFuncItem.GetIsForwarded: Boolean;
begin
  Result := Length(FForwardedName) <> 0;
end;

function TJclPeExportFuncItem.GetMappedAddress: Pointer;
begin
  Result := FExportList.Image.RvaToVa(FAddress);
end;

function TJclPeExportFuncItem.GetSectionName: string;
begin
  if IsForwarded then
    Result := ''
  else
    with FExportList.Image do
      Result := ImageSectionNameFromRva[Address];
end;

procedure TJclPeExportFuncItem.SetResolveCheck(Value: TJclPeResolveCheck);
begin
  FResolveCheck := Value;
end;

// Export sort functions
function ExportSortByName(Item1, Item2: Pointer): Integer;
begin
  Result := CompareStr(TJclPeExportFuncItem(Item1).Name, TJclPeExportFuncItem(Item2).Name);
end;

function ExportSortByNameDESC(Item1, Item2: Pointer): Integer;
begin
  Result := ExportSortByName(Item2, Item1);
end;

function ExportSortByOrdinal(Item1, Item2: Pointer): Integer;
begin
  Result := TJclPeExportFuncItem(Item1).Ordinal - TJclPeExportFuncItem(Item2).Ordinal;
end;

function ExportSortByOrdinalDESC(Item1, Item2: Pointer): Integer;
begin
  Result := ExportSortByOrdinal(Item2, Item1);
end;

function ExportSortByHint(Item1, Item2: Pointer): Integer;
begin
  Result := TJclPeExportFuncItem(Item1).Hint - TJclPeExportFuncItem(Item2).Hint;
end;

function ExportSortByHintDESC(Item1, Item2: Pointer): Integer;
begin
  Result := ExportSortByHint(Item2, Item1);
end;

function ExportSortByAddress(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(TJclPeExportFuncItem(Item1).Address) - Integer(TJclPeExportFuncItem(Item2).Address);
  if Result = 0 then
    Result := ExportSortByName(Item1, Item2);
end;

function ExportSortByAddressDESC(Item1, Item2: Pointer): Integer;
begin
  Result := ExportSortByAddress(Item2, Item1);
end;

function ExportSortByForwarded(Item1, Item2: Pointer): Integer;
begin
  Result := CompareStr(TJclPeExportFuncItem(Item1).ForwardedName, TJclPeExportFuncItem(Item2).ForwardedName);
  if Result = 0 then
    Result := ExportSortByName(Item1, Item2);
end;

function ExportSortByForwardedDESC(Item1, Item2: Pointer): Integer;
begin
  Result := ExportSortByForwarded(Item2, Item1);
end;

function ExportSortByAddrOrFwd(Item1, Item2: Pointer): Integer;
begin
  Result := CompareStr(TJclPeExportFuncItem(Item1).AddressOrForwardStr, TJclPeExportFuncItem(Item2).AddressOrForwardStr);
end;

function ExportSortByAddrOrFwdDESC(Item1, Item2: Pointer): Integer;
begin
  Result := ExportSortByAddrOrFwd(Item2, Item1);
end;

function ExportSortBySection(Item1, Item2: Pointer): Integer;
begin
  Result := CompareStr(TJclPeExportFuncItem(Item1).SectionName, TJclPeExportFuncItem(Item2).SectionName);
  if Result = 0 then
    Result := ExportSortByName(Item1, Item2);
end;

function ExportSortBySectionDESC(Item1, Item2: Pointer): Integer;
begin
  Result := ExportSortBySection(Item2, Item1);
end;

//=== { TJclPeExportFuncList } ===============================================

constructor TJclPeExportFuncList.Create(AImage: TJclPeImage);
begin
  inherited Create(AImage);
  FTotalResolveCheck := icNotChecked;
  CreateList;
end;

destructor TJclPeExportFuncList.Destroy;
begin
  FreeAndNil(FForwardedLibsList);
  inherited Destroy;
end;

function TJclPeExportFuncList.CanPerformFastNameSearch: Boolean;
begin
  Result := FSorted and (FLastSortType = esName) and not FLastSortDescending;
end;

procedure TJclPeExportFuncList.CheckForwards(PeImageCache: TJclPeImagesCache);
var
  I: Integer;
  FullFileName: TFileName;
  ForwardPeImage: TJclPeImage;
  ModuleResolveCheck: TJclPeResolveCheck;

  procedure PerformCheck(const ModuleName: string);
  var
    I: Integer;
    Item: TJclPeExportFuncItem;
    EL: TJclPeExportFuncList;
  begin
    EL := ForwardPeImage.ExportList;
    EL.PrepareForFastNameSearch;
    ModuleResolveCheck := icResolved;
    for I := 0 to Count - 1 do
    begin
      Item := Items[I];
      if (not Item.IsForwarded) or (Item.ResolveCheck <> icNotChecked) or
        (Item.ForwardedLibName <> ModuleName) then
        Continue;
      if EL.ItemFromName[Item.ForwardedFuncName] = nil then
      begin
        Item.SetResolveCheck(icUnresolved);
        ModuleResolveCheck := icUnresolved;
      end
      else
        Item.SetResolveCheck(icResolved);
    end;
  end;

begin
  if not AnyForwards then
    Exit;
  FTotalResolveCheck := icResolved;
  if PeImageCache <> nil then
    ForwardPeImage := nil // to make the compiler happy
  else
    ForwardPeImage := TJclPeImage.Create(True);
  try
    for I := 0 to ForwardedLibsList.Count - 1 do
    begin
      FullFileName := Image.ExpandModuleName(ForwardedLibsList[I]);
      if PeImageCache <> nil then
        ForwardPeImage := PeImageCache[FullFileName]
      else
        ForwardPeImage.FileName := FullFileName;
      if ForwardPeImage.StatusOK then
        PerformCheck(ForwardedLibsList[I])
      else
        ModuleResolveCheck := icUnresolved;
      FForwardedLibsList.Objects[I] := Pointer(ModuleResolveCheck);
      if ModuleResolveCheck = icUnresolved then
        FTotalResolveCheck := icUnresolved;
    end;
  finally
    if PeImageCache = nil then
      ForwardPeImage.Free;
  end;
end;

procedure TJclPeExportFuncList.CreateList;
var
  Functions: Pointer;
  Address: DWORD;
  NameOrdinals: PWORD;
  Names: PDWORD;
  I: Integer;
  ExportItem: TJclPeExportFuncItem;
  ExportVABegin, ExportVAEnd: DWORD;
  ForwardedName: string;
begin
  with Image do
  begin
    if not StatusOK then
      Exit;
    with Directories[IMAGE_DIRECTORY_ENTRY_EXPORT] do
    begin
      ExportVABegin := VirtualAddress;
      ExportVAEnd := VirtualAddress + Size;
    end;
    FExportDir := DirectoryEntryToData(IMAGE_DIRECTORY_ENTRY_EXPORT);
    if FExportDir <> nil then
    begin
      FBase := FExportDir^.Base;
      FFunctionCount := FExportDir^.NumberOfFunctions;
      Functions := RvaToVa(FExportDir^.AddressOfFunctions);
      NameOrdinals := RvaToVa(FExportDir^.AddressOfNameOrdinals);
      Names := RvaToVa(FExportDir^.AddressOfNames);
      Count := FExportDir^.NumberOfNames;
      for I := 0 to FExportDir^.NumberOfNames - 1 do
      begin
        Address := PDWORD(TJclAddr(Functions) + NameOrdinals^ * SizeOf(DWORD))^;
        if (Address >= ExportVABegin) and (Address <= ExportVAEnd) then
        begin
          FAnyForwards := True;
          ForwardedName := PChar(RvaToVa(Address));
        end
        else
          ForwardedName := '';

        ExportItem := TJclPeExportFuncItem.Create(Self, PChar(RvaToVa(Names^)),
          ForwardedName, Address, I, NameOrdinals^ + FBase, icNotChecked);

        List^[I] := ExportItem;
        Inc(NameOrdinals);
        Inc(Names);
      end;
    end;
  end;
end;

function TJclPeExportFuncList.GetForwardedLibsList: TStrings;
var
  I: Integer;
begin
  if FForwardedLibsList = nil then
  begin
    FForwardedLibsList := TStringList.Create;
    FForwardedLibsList.Sorted := True;
    FForwardedLibsList.Duplicates := dupIgnore;
    if FAnyForwards then
      for I := 0 to Count - 1 do
        with Items[I] do
          if IsForwarded then
            FForwardedLibsList.AddObject(ForwardedLibName, Pointer(icNotChecked));
  end;
  Result := FForwardedLibsList;
end;

function TJclPeExportFuncList.GetItemFromAddress(Address: DWORD): TJclPeExportFuncItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Address = Address then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TJclPeExportFuncList.GetItemFromName(const Name: string): TJclPeExportFuncItem;
var
  L, H, I, C: Integer;
  B: Boolean;
begin
  Result := nil;
  if CanPerformFastNameSearch then
  begin
    L := 0;
    H := Count - 1;
    B := False;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := CompareStr(Items[I].Name, Name);
      if C < 0 then
        L := I + 1
      else
      begin
        H := I - 1;
        if C = 0 then
        begin
          B := True;
          L := I;
        end;
      end;
    end;
    if B then
      Result := Items[L];
  end
  else
    for I := 0 to Count - 1 do
      if Items[I].Name = Name then
      begin
        Result := Items[I];
        Break;
      end;
end;

function TJclPeExportFuncList.GetItemFromOrdinal(Ordinal: DWORD): TJclPeExportFuncItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].Ordinal = Ordinal then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TJclPeExportFuncList.GetItems(Index: Integer): TJclPeExportFuncItem;
begin
  Result := TJclPeExportFuncItem(Get(Index));
end;

function TJclPeExportFuncList.GetName: string;
begin
  if (FExportDir = nil) or (FExportDir^.Name = 0) then
    Result := ''
  else
    Result := PChar(Image.RvaToVa(FExportDir^.Name));
end;

class function TJclPeExportFuncList.ItemName(Item: TJclPeExportFuncItem): string;
begin
  if Item = nil then
    Result := ''
  else
    Result := Item.Name;
end;

function TJclPeExportFuncList.OrdinalValid(Ordinal: DWORD): Boolean;
begin
  Result := (FExportDir <> nil) and (Ordinal >= Base) and
    (Ordinal < FunctionCount + Base);
end;

procedure TJclPeExportFuncList.PrepareForFastNameSearch;
begin
  if not CanPerformFastNameSearch then
    SortList(esName, False);
end;

function TJclPeExportFuncList.SmartFindName(const CompareName: string;
  Options: TJclSmartCompOptions): TJclPeExportFuncItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if PeSmartFunctionNameSame(CompareName, Items[I].Name, Options) then
    begin
      Result := Items[I];
      Break;
    end;
  end;
end;

procedure TJclPeExportFuncList.SortList(SortType: TJclPeExportSort; Descending: Boolean);
const
  SortFunctions: array [TJclPeExportSort, Boolean] of TListSortCompare =
    ((ExportSortByName, ExportSortByNameDESC),
     (ExportSortByOrdinal, ExportSortByOrdinalDESC),
     (ExportSortByHint, ExportSortByHintDESC),
     (ExportSortByAddress, ExportSortByAddressDESC),
     (ExportSortByForwarded, ExportSortByForwardedDESC),
     (ExportSortByAddrOrFwd, ExportSortByAddrOrFwdDESC),
     (ExportSortBySection, ExportSortBySectionDESC)
    );
begin
  if not FSorted or (SortType <> FLastSortType) or (Descending <> FLastSortDescending) then
  begin
    Sort(SortFunctions[SortType, Descending]);
    FLastSortType := SortType;
    FLastSortDescending := Descending;
    FSorted := True;
  end;
end;

//=== { TJclPeResourceRawStream } ============================================

constructor TJclPeResourceRawStream.Create(AResourceItem: TJclPeResourceItem);
begin
  Assert(not AResourceItem.IsDirectory);
  inherited Create;
  SetPointer(AResourceItem.RawEntryData, AResourceItem.RawEntryDataSize);
end;

function TJclPeResourceRawStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise EJclPeImageError.CreateRes(@RsPeReadOnlyStream);
end;

//=== { TJclPeResourceItem } =================================================

constructor TJclPeResourceItem.Create(AImage: TJclPeImage;
  AParentItem: TJclPeResourceItem; AEntry: PImageResourceDirectoryEntry);
begin
  inherited Create;
  FImage := AImage;
  FEntry := AEntry;
  FParentItem := AParentItem;
  if AParentItem = nil then
    FLevel := 1
  else
    FLevel := AParentItem.Level + 1;
end;

destructor TJclPeResourceItem.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TJclPeResourceItem.CompareName(AName: PChar): Boolean;
var
  P: PChar;
begin
  if IsName then
    P := PChar(Name)
  else
    P := PChar(FEntry^.Name and $FFFF);
  Result := CompareResourceName(AName, P);
end;

function TJclPeResourceItem.GetDataEntry: PImageResourceDataEntry;
begin
  if GetIsDirectory then
    Result := nil
  else
    Result := PImageResourceDataEntry(OffsetToRawData(FEntry^.OffsetToData));
end;

function TJclPeResourceItem.GetIsDirectory: Boolean;
begin
  Result := FEntry^.OffsetToData and IMAGE_RESOURCE_DATA_IS_DIRECTORY <> 0;
end;

function TJclPeResourceItem.GetIsName: Boolean;
begin
  Result := FEntry^.Name and IMAGE_RESOURCE_NAME_IS_STRING <> 0;
end;

function TJclPeResourceItem.GetLangID: LANGID;
begin
  if IsDirectory then
  begin
    GetList;
    if FList.Count = 1 then
      Result := StrToIntDef(FList[0].Name, 0)
    else
      Result := 0;
  end
  else
    Result := StrToIntDef(Name, 0);
end;

function TJclPeResourceItem.GetList: TJclPeResourceList;
begin
  if not IsDirectory then
  begin
    if Image.NoExceptions then
    begin
      Result := nil;
      Exit;
    end
    else
      raise EJclPeImageError.CreateRes(@RsPeNotResDir);
  end;
  if FList = nil then
    FList := FImage.ResourceListCreate(SubDirData, Self);
  Result := FList;
end;

function TJclPeResourceItem.GetName: string;
begin
  if IsName then
  begin
    if FNameCache = '' then
    begin
      with PImageResourceDirStringU(OffsetToRawData(FEntry^.Name))^ do
        FNameCache := WideCharLenToString(NameString, Length);
      StrResetLength(FNameCache);
    end;
    Result := FNameCache;
  end
  else
    Result := IntToStr(FEntry^.Name and $FFFF);
end;

function TJclPeResourceItem.GetParameterName: string;
begin
  if IsName then
    Result := Name
  else
    Result := Format('#%d', [FEntry^.Name and $FFFF]);
end;

function TJclPeResourceItem.GetRawEntryData: Pointer;
begin
  if GetIsDirectory then
    Result := nil
  else
    Result := FImage.RvaToVa(GetDataEntry^.OffsetToData);
end;

function TJclPeResourceItem.GetRawEntryDataSize: Integer;
begin
  if GetIsDirectory then
    Result := -1
  else
    Result := PImageResourceDataEntry(OffsetToRawData(FEntry^.OffsetToData))^.Size;
end;

function TJclPeResourceItem.GetResourceType: TJclPeResourceKind;
begin
  with Level1Item do
  begin
    if FEntry^.Name < Cardinal(High(TJclPeResourceKind)) then
      Result := TJclPeResourceKind(FEntry^.Name)
    else
      Result := rtUserDefined
  end;
end;

function TJclPeResourceItem.GetResourceTypeStr: string;
begin
  with Level1Item do
  begin
    if FEntry^.Name < Cardinal(High(TJclPeResourceKind)) then
      Result := Copy(GetEnumName(TypeInfo(TJclPeResourceKind), Ord(FEntry^.Name)), 3, 30)
    else
      Result := Name;
  end;
end;

function TJclPeResourceItem.Level1Item: TJclPeResourceItem;
begin
  Result := Self;
  while Result.FParentItem <> nil do
    Result := Result.FParentItem;
end;

function TJclPeResourceItem.OffsetToRawData(Ofs: DWORD): DWORD;
begin
  Result := (Ofs and $7FFFFFFF) + Image.ResourceVA;
end;

function TJclPeResourceItem.SubDirData: PImageResourceDirectory;
begin
  Result := Pointer(OffsetToRawData(FEntry^.OffsetToData));
end;

//=== { TJclPeResourceList } =================================================

constructor TJclPeResourceList.Create(AImage: TJclPeImage;
  AParentItem: TJclPeResourceItem; ADirectory: PImageResourceDirectory);
begin
  inherited Create(AImage);
  FDirectory := ADirectory;
  FParentItem := AParentItem;
  CreateList(AParentItem);
end;

procedure TJclPeResourceList.CreateList(AParentItem: TJclPeResourceItem);
var
  Entry: PImageResourceDirectoryEntry;
  DirItem: TJclPeResourceItem;
  I: Integer;
begin
  if FDirectory = nil then
    Exit;
  Entry := Pointer(TJclAddr(FDirectory) + SizeOf(TImageResourceDirectory));
  for I := 1 to FDirectory^.NumberOfNamedEntries + FDirectory^.NumberOfIdEntries do
  begin
    DirItem := Image.ResourceItemCreate(Entry, AParentItem);
    Add(DirItem);
    Inc(Entry);
  end;
end;

function TJclPeResourceList.FindName(const Name: string): TJclPeResourceItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if StrSame(Items[I].Name, Name) then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TJclPeResourceList.GetItems(Index: Integer): TJclPeResourceItem;
begin
  Result := TJclPeResourceItem(Get(Index));
end;

//=== { TJclPeRootResourceList } =============================================

destructor TJclPeRootResourceList.Destroy;
begin
  FreeAndNil(FManifestContent);
  inherited Destroy;
end;

function TJclPeRootResourceList.FindResource(ResourceType: TJclPeResourceKind;
  const ResourceName: string): TJclPeResourceItem;
var
  I: Integer;
  TypeItem: TJclPeResourceItem;
begin
  Result := nil;
  TypeItem := nil;
  for I := 0 to Count - 1 do
  begin
    if Items[I].ResourceType = ResourceType then
    begin
      TypeItem := Items[I];
      Break;
    end;
  end;
  if TypeItem <> nil then
    if ResourceName = '' then
      Result := TypeItem
    else
      with TypeItem.List do
        for I := 0 to Count - 1 do
          if Items[I].Name = ResourceName then
          begin
            Result := Items[I];
            Break;
          end;
end;

function TJclPeRootResourceList.FindResource(const ResourceType: PChar;
  const ResourceName: PChar): TJclPeResourceItem;
var
  I: Integer;
  TypeItem: TJclPeResourceItem;
begin
  Result := nil;
  TypeItem := nil;
  for I := 0 to Count - 1 do
    if Items[I].CompareName(ResourceType) then
    begin
      TypeItem := Items[I];
      Break;
    end;
  if TypeItem <> nil then
    if ResourceName = nil then
      Result := TypeItem
    else
      with TypeItem.List do
        for I := 0 to Count - 1 do
          if Items[I].CompareName(ResourceName) then
          begin
            Result := Items[I];
            Break;
          end;
end;

function TJclPeRootResourceList.GetManifestContent: TStrings;
var
  ManifestFileName: string;
  ResItem: TJclPeResourceItem;
  ResStream: TJclPeResourceRawStream;
begin
  if FManifestContent = nil then
  begin
    FManifestContent := TStringList.Create;
    ResItem := FindResource(RT_MANIFEST, CREATEPROCESS_MANIFEST_RESOURCE_ID);
    if ResItem = nil then
    begin
      ManifestFileName := Image.FileName + MANIFESTExtension;
      if FileExists(ManifestFileName) then
        FManifestContent.LoadFromFile(ManifestFileName);
    end
    else
    begin
      ResStream := TJclPeResourceRawStream.Create(ResItem.List[0]);
      try
        FManifestContent.LoadFromStream(ResStream);
      finally
        ResStream.Free;
      end;    
    end;
  end;
  Result := FManifestContent;
end;

function TJclPeRootResourceList.ListResourceNames(ResourceType: TJclPeResourceKind;
  const Strings: TStrings): Boolean;
var
  ResTypeItem, TempItem: TJclPeResourceItem;
  I: Integer;
begin
  ResTypeItem := FindResource(ResourceType, '');
  Result := (ResTypeItem <> nil);
  if Result then
  begin
    Strings.BeginUpdate;
    try
      with ResTypeItem.List do
        for I := 0 to Count - 1 do
        begin
          TempItem := Items[I];
          Strings.AddObject(TempItem.Name, Pointer(TempItem.IsName));
        end;
    finally
      Strings.EndUpdate;
    end;
  end;
end;

//=== { TJclPeRelocEntry } ===================================================

constructor TJclPeRelocEntry.Create(AChunk: PImageBaseRelocation; ACount: Integer);
begin
  inherited Create;
  FChunk := AChunk;
  FCount := ACount;
end;

function TJclPeRelocEntry.GetRelocations(Index: Integer): TJclPeRelocation;
var
  Temp: Word;
begin
  Temp := PWord(TJclAddr(FChunk) + SizeOf(TImageBaseRelocation) + DWORD(Index) * SizeOf(Word))^;
  Result.Address := Temp and $0FFF;
  Result.RelocType := (Temp and $F000) shr 12;
  Result.VirtualAddress := Result.Address + VirtualAddress;
end;

function TJclPeRelocEntry.GetSize: DWORD;
begin
  Result := FChunk^.SizeOfBlock;
end;

function TJclPeRelocEntry.GetVirtualAddress: DWORD;
begin
  Result := FChunk^.VirtualAddress;
end;

//=== { TJclPeRelocList } ====================================================

constructor TJclPeRelocList.Create(AImage: TJclPeImage);
begin
  inherited Create(AImage);
  CreateList;
end;

procedure TJclPeRelocList.CreateList;
var
  Chunk: PImageBaseRelocation;
  Item: TJclPeRelocEntry;
  RelocCount: Integer;
begin
  with Image do
  begin
    if not StatusOK then
      Exit;
    Chunk := DirectoryEntryToData(IMAGE_DIRECTORY_ENTRY_BASERELOC);
    if Chunk = nil then
      Exit;
    FAllItemCount := 0;
    while Chunk^.SizeOfBlock <> 0 do
    begin
      RelocCount := (Chunk^.SizeOfBlock - SizeOf(TImageBaseRelocation)) div SizeOf(Word);
      Item := TJclPeRelocEntry.Create(Chunk, RelocCount);
      Inc(FAllItemCount, RelocCount);
      Add(Item);
      Chunk := Pointer(TJclAddr(Chunk) + Chunk^.SizeOfBlock);
    end;
  end;
end;

function TJclPeRelocList.GetAllItems(Index: Integer): TJclPeRelocation;
var
  I, N, C: Integer;
begin
  N := Index;
  for I := 0 to Count - 1 do
  begin
    C := Items[I].Count;
    Dec(N, C);
    if N < 0 then
    begin
      Result := Items[I][N + C];
      Break;
    end;
  end;
end;

function TJclPeRelocList.GetItems(Index: Integer): TJclPeRelocEntry;
begin
  Result := TJclPeRelocEntry(Get(Index));
end;

//=== { TJclPeDebugList } ====================================================

constructor TJclPeDebugList.Create(AImage: TJclPeImage);
begin
  inherited Create(AImage);
  OwnsObjects := False;
  CreateList;
end;

procedure TJclPeDebugList.CreateList;
var
  DebugImageDir: TImageDataDirectory;
  DebugDir: PImageDebugDirectory;
  Header: PImageSectionHeader;
  FormatCount, I: Integer;
begin
  with Image do
  begin
    if not StatusOK then
      Exit;
    DebugImageDir := Directories[IMAGE_DIRECTORY_ENTRY_DEBUG];
    if DebugImageDir.VirtualAddress = 0 then
      Exit;
    if GetSectionHeader(DebugSectionName, Header) and
      (Header^.VirtualAddress = DebugImageDir.VirtualAddress) then
    begin
      FormatCount := DebugImageDir.Size;
      DebugDir := RvaToVa(Header^.VirtualAddress);
    end
    else
    begin
      if not GetSectionHeader(ReadOnlySectionName, Header) then
        Exit;
      FormatCount := DebugImageDir.Size div SizeOf(TImageDebugDirectory);
      DebugDir := Pointer(MappedAddress + DebugImageDir.VirtualAddress -
        Header^.VirtualAddress + Header^.PointerToRawData);
    end;
    for I := 1 to FormatCount do
    begin
      Add(TObject(DebugDir));
      Inc(DebugDir);
    end;
  end;
end;

function TJclPeDebugList.GetItems(Index: Integer): TImageDebugDirectory;
begin
  Result := PImageDebugDirectory(Get(Index))^;
end;

//=== { TJclPeCertificate } ==================================================

constructor TJclPeCertificate.Create(AHeader: TWinCertificate; AData: Pointer);
begin
  inherited Create;
  FHeader := AHeader;
  FData := AData;
end;

//=== { TJclPeCertificateList } ==============================================

constructor TJclPeCertificateList.Create(AImage: TJclPeImage);
begin
  inherited Create(AImage);
  CreateList;
end;

procedure TJclPeCertificateList.CreateList;
var
  Directory: TImageDataDirectory;
  CertPtr: PChar;
  TotalSize: Integer;
  Item: TJclPeCertificate;
begin
  Directory := Image.Directories[IMAGE_DIRECTORY_ENTRY_SECURITY];
  if Directory.VirtualAddress = 0 then
    Exit;
  CertPtr := Image.RawToVa(Directory.VirtualAddress); // Security directory is a raw offset
  TotalSize := Directory.Size;
  while TotalSize >= SizeOf(TWinCertificate) do
  begin
    Item := TJclPeCertificate.Create(PWinCertificate(CertPtr)^, CertPtr + SizeOf(TWinCertificate));
    Dec(TotalSize, Item.Header.dwLength);
    Add(Item);
  end;
end;

function TJclPeCertificateList.GetItems(Index: Integer): TJclPeCertificate;
begin
  Result := TJclPeCertificate(Get(Index));
end;

//=== { TJclPeCLRHeader } ====================================================

constructor TJclPeCLRHeader.Create(AImage: TJclPeImage);
begin
  FImage := AImage;
  ReadHeader;
end;

function TJclPeCLRHeader.GetHasMetadata: Boolean;
const
  METADATA_SIGNATURE = $424A5342; // Reference: Partition II Metadata.doc - 23.2.1 Metadata root
begin
  with Header.MetaData do
    Result := (VirtualAddress <> 0) and (PDWORD(FImage.RvaToVa(VirtualAddress))^ = METADATA_SIGNATURE);
end;
{ TODO -cDOC : "Flier Lu" <flier_lu att yahoo dott com dott cn> }

function TJclPeCLRHeader.GetVersionString: string;
begin
  Result := FormatVersionString(Header.MajorRuntimeVersion, Header.MinorRuntimeVersion);
end;

procedure TJclPeCLRHeader.ReadHeader;
var
  HeaderPtr: PImageCor20Header;
begin
  HeaderPtr := Image.DirectoryEntryToData(IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR);
  if (HeaderPtr <> nil) and (HeaderPtr^.cb >= SizeOf(TImageCor20Header)) then
    FHeader := HeaderPtr^;
end;

//=== { TJclPeImage } ========================================================

constructor TJclPeImage.Create(ANoExceptions: Boolean);
begin
  FNoExceptions := ANoExceptions;
  FReadOnlyAccess := True;
  FImageSections := TStringList.Create;
end;

destructor TJclPeImage.Destroy;
begin
  Clear;
  FreeAndNil(FImageSections);
  inherited Destroy;
end;

procedure TJclPeImage.AfterOpen;
begin
end;

procedure TJclPeImage.AttachLoadedModule(const Handle: HMODULE);
  procedure AttachLoadedModule32;
  var
    NtHeaders: PImageNtHeaders32;
  begin
    NtHeaders := PeMapImgNtHeaders32(Pointer(Handle));
    if NtHeaders = nil then
      FStatus := stNotPE
    else
    begin
      FStatus := stOk;
      FAttachedImage := True;
      FFileName := GetModulePath(Handle);
      FLoadedImage.ModuleName := PChar(FFileName);
      FLoadedImage.hFile := INVALID_HANDLE_VALUE;
      FLoadedImage.MappedAddress := Pointer(Handle);
      FLoadedImage.FileHeader := PImageNtHeaders(NtHeaders);
      FLoadedImage.NumberOfSections := NtHeaders^.FileHeader.NumberOfSections;
      FLoadedImage.Sections := PeMapImgSections32(NtHeaders);
      FLoadedImage.LastRvaSection := FLoadedImage.Sections;
      FLoadedImage.Characteristics := NtHeaders^.FileHeader.Characteristics;
      FLoadedImage.fSystemImage := (FLoadedImage.Characteristics and IMAGE_FILE_SYSTEM <> 0);
      FLoadedImage.fDOSImage := False;
      FLoadedImage.SizeOfImage := NtHeaders^.OptionalHeader.SizeOfImage;
      ReadImageSections;
      AfterOpen;
    end;
    RaiseStatusException;
  end;

  procedure AttachLoadedModule64;
   var
    NtHeaders: PImageNtHeaders64;
  begin
    NtHeaders := PeMapImgNtHeaders64(Pointer(Handle));
    if NtHeaders = nil then
      FStatus := stNotPE
    else
    begin
      FStatus := stOk;
      FAttachedImage := True;
      FFileName := GetModulePath(Handle);
      FLoadedImage.ModuleName := PChar(FFileName);
      FLoadedImage.hFile := INVALID_HANDLE_VALUE;
      FLoadedImage.MappedAddress := Pointer(Handle);
      FLoadedImage.FileHeader := PImageNtHeaders(NtHeaders);
      FLoadedImage.NumberOfSections := NtHeaders^.FileHeader.NumberOfSections;
      FLoadedImage.Sections := PeMapImgSections64(NtHeaders);
      FLoadedImage.LastRvaSection := FLoadedImage.Sections;
      FLoadedImage.Characteristics := NtHeaders^.FileHeader.Characteristics;
      FLoadedImage.fSystemImage := (FLoadedImage.Characteristics and IMAGE_FILE_SYSTEM <> 0);
      FLoadedImage.fDOSImage := False;
      FLoadedImage.SizeOfImage := NtHeaders^.OptionalHeader.SizeOfImage;
      ReadImageSections;
      AfterOpen;
    end;
    RaiseStatusException;
  end;
begin
  Clear;
  if Handle = 0 then
    Exit;
  FTarget := PeMapImgTarget(Pointer(Handle));
  case Target of
    taWin32:
      AttachLoadedModule32;
    taWin64:
      AttachLoadedModule64;
    taUnknown:
      FStatus := stNotSupported;
  end;
end;

function TJclPeImage.CalculateCheckSum: DWORD;
var
  C: DWORD;
begin
  if StatusOK then
  begin
    CheckNotAttached;
    if CheckSumMappedFile(FLoadedImage.MappedAddress, FLoadedImage.SizeOfImage,
      C, Result) = nil then
        RaiseLastOSError;
  end
  else
    Result := 0;
end;

procedure TJclPeImage.CheckNotAttached;
begin
  if FAttachedImage then
    raise EJclPeImageError.CreateRes(@RsPeNotAvailableForAttached);
end;

procedure TJclPeImage.Clear;
begin
  FImageSections.Clear;
  FreeAndNil(FCertificateList);
  FreeAndNil(FCLRHeader);
  FreeAndNil(FDebugList);
  FreeAndNil(FImportList);
  FreeAndNil(FExportList);
  FreeAndNil(FRelocationList);
  FreeAndNil(FResourceList);
  FreeAndNil(FVersionInfo);
  if not FAttachedImage and StatusOK then
    UnMapAndLoad(FLoadedImage);
  FillChar(FLoadedImage, SizeOf(FLoadedImage), #0);
  FStatus := stNotLoaded;
  FAttachedImage := False;
end;

class function TJclPeImage.DateTimeToStamp(const DateTime: TDateTime): DWORD;
begin
  Result := Round((DateTime - UnixTimeStart) * SecsPerDay);
end;

class function TJclPeImage.DebugTypeNames(DebugType: DWORD): string;
begin
  case DebugType of
    IMAGE_DEBUG_TYPE_UNKNOWN:
      Result := RsPeDEBUG_UNKNOWN;
    IMAGE_DEBUG_TYPE_COFF:
      Result := RsPeDEBUG_COFF;
    IMAGE_DEBUG_TYPE_CODEVIEW:
      Result := RsPeDEBUG_CODEVIEW;
    IMAGE_DEBUG_TYPE_FPO:
      Result := RsPeDEBUG_FPO;
    IMAGE_DEBUG_TYPE_MISC:
      Result := RsPeDEBUG_MISC;
    IMAGE_DEBUG_TYPE_EXCEPTION:
      Result := RsPeDEBUG_EXCEPTION;
    IMAGE_DEBUG_TYPE_FIXUP:
      Result := RsPeDEBUG_FIXUP;
    IMAGE_DEBUG_TYPE_OMAP_TO_SRC:
      Result := RsPeDEBUG_OMAP_TO_SRC;
    IMAGE_DEBUG_TYPE_OMAP_FROM_SRC:
      Result := RsPeDEBUG_OMAP_FROM_SRC;
  else
    Result := '???';
  end;
end;

function TJclPeImage.DirectoryEntryToData(Directory: Word): Pointer;
var
  Size: DWORD;
begin
  Result := ImageDirectoryEntryToData(FLoadedImage.MappedAddress, FAttachedImage, Directory, Size);
end;

class function TJclPeImage.DirectoryNames(Directory: Word): string;
begin
  case Directory of
    IMAGE_DIRECTORY_ENTRY_EXPORT:
      Result := RsPeImg_00;
    IMAGE_DIRECTORY_ENTRY_IMPORT:
      Result := RsPeImg_01;
    IMAGE_DIRECTORY_ENTRY_RESOURCE:
      Result := RsPeImg_02;
    IMAGE_DIRECTORY_ENTRY_EXCEPTION:
      Result := RsPeImg_03;
    IMAGE_DIRECTORY_ENTRY_SECURITY:
      Result := RsPeImg_04;
    IMAGE_DIRECTORY_ENTRY_BASERELOC:
      Result := RsPeImg_05;
    IMAGE_DIRECTORY_ENTRY_DEBUG:
      Result := RsPeImg_06;
    IMAGE_DIRECTORY_ENTRY_COPYRIGHT:
      Result := RsPeImg_07;
    IMAGE_DIRECTORY_ENTRY_GLOBALPTR:
      Result := RsPeImg_08;
    IMAGE_DIRECTORY_ENTRY_TLS:
      Result := RsPeImg_09;
    IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG:
      Result := RsPeImg_10;
    IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT:
      Result := RsPeImg_11;
    IMAGE_DIRECTORY_ENTRY_IAT:
      Result := RsPeImg_12;
    IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT:
      Result := RsPeImg_13;
    IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR:
      Result := RsPeImg_14;
  else
    Result := Format('reserved [%.2d]', [Directory]);
  end;
end;

class function TJclPeImage.ExpandBySearchPath(const ModuleName, BasePath: string): TFileName;
var
  FullName: array [0..MAX_PATH] of Char;
  FilePart: PChar;
begin
  Result := PathAddSeparator(ExtractFilePath(BasePath)) + ModuleName;
  if FileExists(Result) then
    Exit;
  if SearchPath(nil, PChar(ModuleName), nil, SizeOf(FullName), FullName, FilePart) = 0 then
    Result := ModuleName
  else
    Result := FullName;
end;

function TJclPeImage.ExpandModuleName(const ModuleName: string): TFileName;
begin
  Result := ExpandBySearchPath(ModuleName, ExtractFilePath(FFileName));
end;

function TJclPeImage.GetCertificateList: TJclPeCertificateList;
begin
  if FCertificateList = nil then
    FCertificateList := TJclPeCertificateList.Create(Self);
  Result := FCertificateList;   
end;

function TJclPeImage.GetCLRHeader: TJclPeCLRHeader;
begin
  if FCLRHeader = nil then
    FCLRHeader := TJclPeCLRHeader.Create(Self);
  Result := FCLRHeader;  
end;

function TJclPeImage.GetDebugList: TJclPeDebugList;
begin
  if FDebugList = nil then
    FDebugList := TJclPeDebugList.Create(Self);
  Result := FDebugList;
end;

function TJclPeImage.GetDescription: string;
begin
  if DirectoryExists[IMAGE_DIRECTORY_ENTRY_COPYRIGHT] then
    Result := PChar(DirectoryEntryToData(IMAGE_DIRECTORY_ENTRY_COPYRIGHT))
  else
    Result := '';
end;

function TJclPeImage.GetDirectories(Directory: Word): TImageDataDirectory;
begin
  if StatusOK then
  begin
    case Target of
      taWin32:
        Result := PImageNtHeaders32(FLoadedImage.FileHeader)^.OptionalHeader.DataDirectory[Directory];
      taWin64:
        Result := PImageNtHeaders64(FLoadedImage.FileHeader)^.OptionalHeader.DataDirectory[Directory];
    else
      Result.VirtualAddress := 0;
      Result.Size := 0;
    end
  end
  else
  begin
    Result.VirtualAddress := 0;
    Result.Size := 0;
  end;
end;

function TJclPeImage.GetDirectoryExists(Directory: Word): Boolean;
begin
  Result := (Directories[Directory].VirtualAddress <> 0);
end;

function TJclPeImage.GetExportList: TJclPeExportFuncList;
begin
  if FExportList = nil then
    FExportList := TJclPeExportFuncList.Create(Self);
  Result := FExportList;
end;

function TJclPeImage.GetFileProperties: TJclPeFileProperties;
const
  faFile = faReadOnly or faHidden or faSysFile or faArchive;
var
  Se: TSearchRec;
  Res: Integer;
begin
  FillChar(Result, SizeOf(Result), #0);
  Res := FindFirst(FileName, faFile, Se);
  if Res = 0 then
  begin
    Result.Size := Se.Size;
    Result.CreationTime := FileTimeToLocalDateTime(Se.FindData.ftCreationTime);
    Result.LastAccessTime := FileTimeToLocalDateTime(Se.FindData.ftLastAccessTime);
    Result.LastWriteTime := FileTimeToLocalDateTime(Se.FindData.ftLastWriteTime);
    Result.Attributes := Se.Attr;
  end;
  FindClose(Se);
end;

function TJclPeImage.GetHeaderValues(Index: TJclPeHeader): string;

  function GetMachineString(Value: DWORD): string;
  begin
    case Value of
      IMAGE_FILE_MACHINE_UNKNOWN:
        Result := RsPeMACHINE_UNKNOWN;
      IMAGE_FILE_MACHINE_I386:
        Result := RsPeMACHINE_I386;
      IMAGE_FILE_MACHINE_R3000:
        Result := RsPeMACHINE_R3000;
      IMAGE_FILE_MACHINE_R4000:
        Result := RsPeMACHINE_R4000;
      IMAGE_FILE_MACHINE_R10000:
        Result := RsPeMACHINE_R10000;
      IMAGE_FILE_MACHINE_WCEMIPSV2:
        Result := RsPeMACHINE_WCEMIPSV2;
      IMAGE_FILE_MACHINE_ALPHA:
        Result := RsPeMACHINE_ALPHA;
      IMAGE_FILE_MACHINE_SH3:
        Result := RsPeMACHINE_SH3;        // SH3 little-endian
      IMAGE_FILE_MACHINE_SH3DSP:
        Result := RsPeMACHINE_SH3DSP;
      IMAGE_FILE_MACHINE_SH3E:
        Result := RsPeMACHINE_SH3E;       // SH3E little-endian
      IMAGE_FILE_MACHINE_SH4:
        Result := RsPeMACHINE_SH4;        // SH4 little-endian
      IMAGE_FILE_MACHINE_SH5:
        Result := RsPeMACHINE_SH5;        // SH5
      IMAGE_FILE_MACHINE_ARM:
        Result := RsPeMACHINE_ARM;        // ARM Little-Endian
      IMAGE_FILE_MACHINE_THUMB:
        Result := RsPeMACHINE_THUMB;
      IMAGE_FILE_MACHINE_AM33:
        Result := RsPeMACHINE_AM33;
      IMAGE_FILE_MACHINE_POWERPC:
        Result := RsPeMACHINE_POWERPC;
      IMAGE_FILE_MACHINE_POWERPCFP:
        Result := RsPeMACHINE_POWERPCFP;
      IMAGE_FILE_MACHINE_IA64:
        Result := RsPeMACHINE_IA64;       // Intel 64
      IMAGE_FILE_MACHINE_MIPS16:
        Result := RsPeMACHINE_MIPS16;     // MIPS
      IMAGE_FILE_MACHINE_ALPHA64:
        Result := RsPeMACHINE_AMPHA64;    // ALPHA64
      //IMAGE_FILE_MACHINE_AXP64
      IMAGE_FILE_MACHINE_MIPSFPU:
        Result := RsPeMACHINE_MIPSFPU;    // MIPS
      IMAGE_FILE_MACHINE_MIPSFPU16:
        Result := RsPeMACHINE_MIPSFPU16;  // MIPS
      IMAGE_FILE_MACHINE_TRICORE:
        Result := RsPeMACHINE_TRICORE;    // Infineon
      IMAGE_FILE_MACHINE_CEF:
        Result := RsPeMACHINE_CEF;
      IMAGE_FILE_MACHINE_EBC:
        Result := RsPeMACHINE_EBC;        // EFI Byte Code
      IMAGE_FILE_MACHINE_AMD64:
        Result := RsPeMACHINE_AMD64;      // AMD64 (K8)
      IMAGE_FILE_MACHINE_M32R:
        Result := RsPeMACHINE_M32R;       // M32R little-endian
      IMAGE_FILE_MACHINE_CEE:
        Result := RsPeMACHINE_CEE;       
    else
      Result := Format('[%.8x]', [Value]);
    end;
  end;

  function GetSubsystemString(Value: DWORD): string;
  begin
    case Value of
      IMAGE_SUBSYSTEM_UNKNOWN:
        Result := RsPeSUBSYSTEM_UNKNOWN;
      IMAGE_SUBSYSTEM_NATIVE:
        Result := RsPeSUBSYSTEM_NATIVE;
      IMAGE_SUBSYSTEM_WINDOWS_GUI:
        Result := RsPeSUBSYSTEM_WINDOWS_GUI;
      IMAGE_SUBSYSTEM_WINDOWS_CUI:
        Result := RsPeSUBSYSTEM_WINDOWS_CUI;
      IMAGE_SUBSYSTEM_OS2_CUI:
        Result := RsPeSUBSYSTEM_OS2_CUI;
      IMAGE_SUBSYSTEM_POSIX_CUI:
        Result := RsPeSUBSYSTEM_POSIX_CUI;
      IMAGE_SUBSYSTEM_RESERVED8:
        Result := RsPeSUBSYSTEM_RESERVED8;
    else
      Result := Format('[%.8x]', [Value]);
    end;
  end;

  function GetHeaderValues32(Index: TJclPeHeader): string;
  var
    OptionalHeader: TImageOptionalHeader32;
  begin
    OptionalHeader := OptionalHeader32;
    case Index of
      JclPeHeader_Magic:
        Result := IntToHex(OptionalHeader.Magic, 4);
      JclPeHeader_LinkerVersion:
        Result := FormatVersionString(OptionalHeader.MajorLinkerVersion, OptionalHeader.MinorLinkerVersion);
      JclPeHeader_SizeOfCode:
        Result := IntToHex(OptionalHeader.SizeOfCode, 8);
      JclPeHeader_SizeOfInitializedData:
        Result := IntToHex(OptionalHeader.SizeOfInitializedData, 8);
      JclPeHeader_SizeOfUninitializedData:
        Result := IntToHex(OptionalHeader.SizeOfUninitializedData, 8);
      JclPeHeader_AddressOfEntryPoint:
        Result := IntToHex(OptionalHeader.AddressOfEntryPoint, 8);
      JclPeHeader_BaseOfCode:
        Result := IntToHex(OptionalHeader.BaseOfCode, 8);
      JclPeHeader_BaseOfData:
        Result := IntToHex(OptionalHeader.BaseOfData, 8);
      JclPeHeader_ImageBase:
        Result := IntToHex(OptionalHeader.ImageBase, 8);
      JclPeHeader_SectionAlignment:
        Result := IntToHex(OptionalHeader.SectionAlignment, 8);
      JclPeHeader_FileAlignment:
        Result := IntToHex(OptionalHeader.FileAlignment, 8);
      JclPeHeader_OperatingSystemVersion:
        Result := FormatVersionString(OptionalHeader.MajorOperatingSystemVersion, OptionalHeader.MinorOperatingSystemVersion);
      JclPeHeader_ImageVersion:
        Result := FormatVersionString(OptionalHeader.MajorImageVersion, OptionalHeader.MinorImageVersion);
      JclPeHeader_SubsystemVersion:
        Result := FormatVersionString(OptionalHeader.MajorSubsystemVersion, OptionalHeader.MinorSubsystemVersion);
      JclPeHeader_Win32VersionValue:
        Result := IntToHex(OptionalHeader.Win32VersionValue, 8);
      JclPeHeader_SizeOfImage:
        Result := IntToHex(OptionalHeader.SizeOfImage, 8);
      JclPeHeader_SizeOfHeaders:
        Result := IntToHex(OptionalHeader.SizeOfHeaders, 8);
      JclPeHeader_CheckSum:
        Result := IntToHex(OptionalHeader.CheckSum, 8);
      JclPeHeader_Subsystem:
        Result := GetSubsystemString(OptionalHeader.Subsystem);
      JclPeHeader_DllCharacteristics:
        Result := IntToHex(OptionalHeader.DllCharacteristics, 4);
      JclPeHeader_SizeOfStackReserve:
        Result := IntToHex(OptionalHeader.SizeOfStackReserve, 8);
      JclPeHeader_SizeOfStackCommit:
        Result := IntToHex(OptionalHeader.SizeOfStackCommit, 8);
      JclPeHeader_SizeOfHeapReserve:
        Result := IntToHex(OptionalHeader.SizeOfHeapReserve, 8);
      JclPeHeader_SizeOfHeapCommit:
        Result := IntToHex(OptionalHeader.SizeOfHeapCommit, 8);
      JclPeHeader_LoaderFlags:
        Result := IntToHex(OptionalHeader.LoaderFlags, 8);
      JclPeHeader_NumberOfRvaAndSizes:
        Result := IntToHex(OptionalHeader.NumberOfRvaAndSizes, 8);
    end;
  end;

  function GetHeaderValues64(Index: TJclPeHeader): string;
  var
    OptionalHeader: TImageOptionalHeader64;
  begin
    OptionalHeader := OptionalHeader64;
    case Index of
      JclPeHeader_Magic:
        Result := IntToHex(OptionalHeader.Magic, 4);
      JclPeHeader_LinkerVersion:
        Result := FormatVersionString(OptionalHeader.MajorLinkerVersion, OptionalHeader.MinorLinkerVersion);
      JclPeHeader_SizeOfCode:
        Result := IntToHex(OptionalHeader.SizeOfCode, 8);
      JclPeHeader_SizeOfInitializedData:
        Result := IntToHex(OptionalHeader.SizeOfInitializedData, 8);
      JclPeHeader_SizeOfUninitializedData:
        Result := IntToHex(OptionalHeader.SizeOfUninitializedData, 8);
      JclPeHeader_AddressOfEntryPoint:
        Result := IntToHex(OptionalHeader.AddressOfEntryPoint, 8);
      JclPeHeader_BaseOfCode:
        Result := IntToHex(OptionalHeader.BaseOfCode, 8);
      JclPeHeader_BaseOfData:
        Result := ''; // IntToHex(OptionalHeader.BaseOfData, 8);
      JclPeHeader_ImageBase:
        Result := IntToHex(OptionalHeader.ImageBase, 16);
      JclPeHeader_SectionAlignment:
        Result := IntToHex(OptionalHeader.SectionAlignment, 8);
      JclPeHeader_FileAlignment:
        Result := IntToHex(OptionalHeader.FileAlignment, 8);
      JclPeHeader_OperatingSystemVersion:
        Result := FormatVersionString(OptionalHeader.MajorOperatingSystemVersion, OptionalHeader.MinorOperatingSystemVersion);
      JclPeHeader_ImageVersion:
        Result := FormatVersionString(OptionalHeader.MajorImageVersion, OptionalHeader.MinorImageVersion);
      JclPeHeader_SubsystemVersion:
        Result := FormatVersionString(OptionalHeader.MajorSubsystemVersion, OptionalHeader.MinorSubsystemVersion);
      JclPeHeader_Win32VersionValue:
        Result := IntToHex(OptionalHeader.Win32VersionValue, 8);
      JclPeHeader_SizeOfImage:
        Result := IntToHex(OptionalHeader.SizeOfImage, 8);
      JclPeHeader_SizeOfHeaders:
        Result := IntToHex(OptionalHeader.SizeOfHeaders, 8);
      JclPeHeader_CheckSum:
        Result := IntToHex(OptionalHeader.CheckSum, 8);
      JclPeHeader_Subsystem:
        Result := GetSubsystemString(OptionalHeader.Subsystem);
      JclPeHeader_DllCharacteristics:
        Result := IntToHex(OptionalHeader.DllCharacteristics, 4);
      JclPeHeader_SizeOfStackReserve:
        Result := IntToHex(OptionalHeader.SizeOfStackReserve, 16);
      JclPeHeader_SizeOfStackCommit:
        Result := IntToHex(OptionalHeader.SizeOfStackCommit, 16);
      JclPeHeader_SizeOfHeapReserve:
        Result := IntToHex(OptionalHeader.SizeOfHeapReserve, 16);
      JclPeHeader_SizeOfHeapCommit:
        Result := IntToHex(OptionalHeader.SizeOfHeapCommit, 16);
      JclPeHeader_LoaderFlags:
        Result := IntToHex(OptionalHeader.LoaderFlags, 8);
      JclPeHeader_NumberOfRvaAndSizes:
        Result := IntToHex(OptionalHeader.NumberOfRvaAndSizes, 8);
    end;
  end;

begin
  if StatusOK then
    with FLoadedImage.FileHeader^ do
      case Index of
        JclPeHeader_Signature:
          Result := IntToHex(Signature, 8);
        JclPeHeader_Machine:
          Result := GetMachineString(FileHeader.Machine);
        JclPeHeader_NumberOfSections:
          Result := IntToHex(FileHeader.NumberOfSections, 4);
        JclPeHeader_TimeDateStamp:
          Result := IntToHex(FileHeader.TimeDateStamp, 8);
        JclPeHeader_PointerToSymbolTable:
          Result := IntToHex(FileHeader.PointerToSymbolTable, 8);
        JclPeHeader_NumberOfSymbols:
          Result := IntToHex(FileHeader.NumberOfSymbols, 8);
        JclPeHeader_SizeOfOptionalHeader:
          Result := IntToHex(FileHeader.SizeOfOptionalHeader, 4);
        JclPeHeader_Characteristics:
          Result := IntToHex(FileHeader.Characteristics, 4);
        JclPeHeader_Magic..JclPeHeader_NumberOfRvaAndSizes:
          case Target of
            taWin32:
              Result := GetHeaderValues32(Index);
            taWin64:
              Result := GetHeaderValues64(Index);
            //taUnknown:
          else
            Result := '';
          end;
      else
        Result := '';
      end
  else
    Result := '';
end;

function TJclPeImage.GetImageSectionCount: Integer;
begin
  Result := FImageSections.Count;
end;

function TJclPeImage.GetImageSectionHeaders(Index: Integer): TImageSectionHeader;
begin
  Result := PImageSectionHeader(FImageSections.Objects[Index])^;
end;

function TJclPeImage.GetImageSectionNameFromRva(const Rva: DWORD): string;
begin
  Result := GetSectionName(RvaToSection(Rva));
end;

function TJclPeImage.GetImageSectionNames(Index: Integer): string;
begin
  Result := FImageSections[Index];
end;

function TJclPeImage.GetImportList: TJclPeImportList;
begin
  if FImportList = nil then
    FImportList := TJclPeImportList.Create(Self);
  Result := FImportList;
end;

function TJclPeImage.GetLoadConfigValues(Index: TJclLoadConfig): string;
  function GetLoadConfigValues32(Index: TJclLoadConfig): string;
  var
    LoadConfig: PImageLoadConfigDirectory32;
  begin
    LoadConfig := DirectoryEntryToData(IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG);
    if LoadConfig <> nil then
      with LoadConfig^ do
        case Index of
          JclLoadConfig_Characteristics:
            Result := IntToHex(Size, 8);
          JclLoadConfig_TimeDateStamp:
            Result := IntToHex(TimeDateStamp, 8);
          JclLoadConfig_Version:
            Result := FormatVersionString(MajorVersion, MinorVersion);
          JclLoadConfig_GlobalFlagsClear:
            Result := IntToHex(GlobalFlagsClear, 8);
          JclLoadConfig_GlobalFlagsSet:
            Result := IntToHex(GlobalFlagsSet, 8);
          JclLoadConfig_CriticalSectionDefaultTimeout:
            Result := IntToHex(CriticalSectionDefaultTimeout, 8);
          JclLoadConfig_DeCommitFreeBlockThreshold:
            Result := IntToHex(DeCommitFreeBlockThreshold, 8);
          JclLoadConfig_DeCommitTotalFreeThreshold:
            Result := IntToHex(DeCommitTotalFreeThreshold, 8);
          JclLoadConfig_LockPrefixTable:
            Result := IntToHex(LockPrefixTable, 8);
          JclLoadConfig_MaximumAllocationSize:
            Result := IntToHex(MaximumAllocationSize, 8);
          JclLoadConfig_VirtualMemoryThreshold:
            Result := IntToHex(VirtualMemoryThreshold, 8);
          JclLoadConfig_ProcessHeapFlags:
            Result := IntToHex(ProcessHeapFlags, 8);
          JclLoadConfig_ProcessAffinityMask:
            Result := IntToHex(ProcessAffinityMask, 8);
          JclLoadConfig_CSDVersion:
            Result := IntToHex(CSDVersion, 4);
          JclLoadConfig_Reserved1:
            Result := IntToHex(Reserved1, 4);
          JclLoadConfig_EditList:
            Result := IntToHex(EditList, 8);
          JclLoadConfig_Reserved:
            Result := RsPeReserved;
        end;
  end;
  function GetLoadConfigValues64(Index: TJclLoadConfig): string;
  var
    LoadConfig: PImageLoadConfigDirectory64;
  begin
    LoadConfig := DirectoryEntryToData(IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG);
    if LoadConfig <> nil then
      with LoadConfig^ do
        case Index of
          JclLoadConfig_Characteristics:
            Result := IntToHex(Size, 8);
          JclLoadConfig_TimeDateStamp:
            Result := IntToHex(TimeDateStamp, 8);
          JclLoadConfig_Version:
            Result := FormatVersionString(MajorVersion, MinorVersion);
          JclLoadConfig_GlobalFlagsClear:
            Result := IntToHex(GlobalFlagsClear, 8);
          JclLoadConfig_GlobalFlagsSet:
            Result := IntToHex(GlobalFlagsSet, 8);
          JclLoadConfig_CriticalSectionDefaultTimeout:
            Result := IntToHex(CriticalSectionDefaultTimeout, 8);
          JclLoadConfig_DeCommitFreeBlockThreshold:
            Result := IntToHex(DeCommitFreeBlockThreshold, 16);
          JclLoadConfig_DeCommitTotalFreeThreshold:
            Result := IntToHex(DeCommitTotalFreeThreshold, 16);
          JclLoadConfig_LockPrefixTable:
            Result := IntToHex(LockPrefixTable, 16);
          JclLoadConfig_MaximumAllocationSize:
            Result := IntToHex(MaximumAllocationSize, 16);
          JclLoadConfig_VirtualMemoryThreshold:
            Result := IntToHex(VirtualMemoryThreshold, 16);
          JclLoadConfig_ProcessHeapFlags:
            Result := IntToHex(ProcessHeapFlags, 8);
          JclLoadConfig_ProcessAffinityMask:
            Result := IntToHex(ProcessAffinityMask, 16);
          JclLoadConfig_CSDVersion:
            Result := IntToHex(CSDVersion, 4);
          JclLoadConfig_Reserved1:
            Result := IntToHex(Reserved1, 4);
          JclLoadConfig_EditList:
            Result := IntToHex(EditList, 16);
          JclLoadConfig_Reserved:
            Result := RsPeReserved;
        end;
  end;
begin
  Result := '';
  case Target of
    taWin32:
      Result := GetLoadConfigValues32(Index);
    taWin64:
      Result := GetLoadConfigValues64(Index);
  end;
end;

function TJclPeImage.GetMappedAddress: TJclAddr;
begin
  if StatusOK then
    Result := TJclAddr(LoadedImage.MappedAddress)
  else
    Result := 0;
end;

{$IFDEF KEEP_DEPRECATED}
function TJclPeImage.GetOptionalHeader: TImageOptionalHeader;
begin
  if Target = taWin32 then
    Result := PImageNtHeaders(FLoadedImage.FileHeader)^.OptionalHeader
  else
    ZeroMemory(@Result, SizeOf(Result));
end;
{$ENDIF KEEP_DEPRECATED}

function TJclPeImage.GetOptionalHeader32: TImageOptionalHeader32;
begin
  if Target = taWin32 then
    Result := PImageNtHeaders32(FLoadedImage.FileHeader)^.OptionalHeader
  else
    ZeroMemory(@Result, SizeOf(Result));
end;

function TJclPeImage.GetOptionalHeader64: TImageOptionalHeader64;
begin
  if Target = taWin64 then
    Result := PImageNtHeaders64(FLoadedImage.FileHeader)^.OptionalHeader
  else
    ZeroMemory(@Result, SizeOf(Result));
end;

function TJclPeImage.GetRelocationList: TJclPeRelocList;
begin
  if FRelocationList = nil then
    FRelocationList := TJclPeRelocList.Create(Self);
  Result := FRelocationList;
end;

function TJclPeImage.GetResourceList: TJclPeRootResourceList;
begin
  if FResourceList = nil then
  begin
    FResourceVA := Directories[IMAGE_DIRECTORY_ENTRY_RESOURCE].VirtualAddress;
    if FResourceVA <> 0 then
      FResourceVA := TJclAddr(RvaToVa(FResourceVA));
    FResourceList := TJclPeRootResourceList.Create(Self, nil, PImageResourceDirectory(FResourceVA));
  end;
  Result := FResourceList;
end;

function TJclPeImage.GetSectionHeader(const SectionName: string;
  var Header: PImageSectionHeader): Boolean;
var
  I: Integer;
begin
  I := FImageSections.IndexOf(SectionName);
  if I = -1 then
  begin
    Header := nil;
    Result := False;
  end
  else
  begin
    Header := PImageSectionHeader(FImageSections.Objects[I]);
    Result := True;
  end;
end;

function TJclPeImage.GetSectionName(Header: PImageSectionHeader): string;
var
  I: Integer;
begin
  I := FImageSections.IndexOfObject(TObject(Header));
  if I = -1 then
    Result := ''
  else
    Result := FImageSections[I];
end;

function TJclPeImage.GetUnusedHeaderBytes: TImageDataDirectory;
begin
  CheckNotAttached;
  Result.VirtualAddress := GetImageUnusedHeaderBytes(FLoadedImage, Result.Size);
  if Result.VirtualAddress = 0 then
    RaiseLastOSError;
end;

function TJclPeImage.GetVersionInfo: TJclFileVersionInfo;
var
  VersionInfoResource: TJclPeResourceItem;
begin
  if (FVersionInfo = nil) and VersionInfoAvailable then
  begin
    VersionInfoResource := ResourceList.FindResource(rtVersion, '1').List[0];
    with VersionInfoResource do
      try
        FVersionInfo := TJclFileVersionInfo.Attach(RawEntryData, RawEntryDataSize);
      except
        FreeAndNil(FVersionInfo);
      end;
  end;
  Result := FVersionInfo;
end;

function TJclPeImage.GetVersionInfoAvailable: Boolean;
begin
  Result := StatusOK and (ResourceList.FindResource(rtVersion, '1') <> nil);
end;

class function TJclPeImage.HeaderNames(Index: TJclPeHeader): string;
begin
  case Index of
    JclPeHeader_Signature:
      Result := RsPeSignature;
    JclPeHeader_Machine:
      Result := RsPeMachine;
    JclPeHeader_NumberOfSections:
      Result := RsPeNumberOfSections;
    JclPeHeader_TimeDateStamp:
      Result := RsPeTimeDateStamp;
    JclPeHeader_PointerToSymbolTable:
      Result := RsPePointerToSymbolTable;
    JclPeHeader_NumberOfSymbols:
      Result := RsPeNumberOfSymbols;
    JclPeHeader_SizeOfOptionalHeader:
      Result := RsPeSizeOfOptionalHeader;
    JclPeHeader_Characteristics:
      Result := RsPeCharacteristics;
    JclPeHeader_Magic:
      Result := RsPeMagic;
    JclPeHeader_LinkerVersion:
      Result := RsPeLinkerVersion;
    JclPeHeader_SizeOfCode:
      Result := RsPeSizeOfCode;
    JclPeHeader_SizeOfInitializedData:
      Result := RsPeSizeOfInitializedData;
    JclPeHeader_SizeOfUninitializedData:
      Result := RsPeSizeOfUninitializedData;
    JclPeHeader_AddressOfEntryPoint:
      Result := RsPeAddressOfEntryPoint;
    JclPeHeader_BaseOfCode:
      Result := RsPeBaseOfCode;
    JclPeHeader_BaseOfData:
      Result := RsPeBaseOfData;
    JclPeHeader_ImageBase:
      Result := RsPeImageBase;
    JclPeHeader_SectionAlignment:
      Result := RsPeSectionAlignment;
    JclPeHeader_FileAlignment:
      Result := RsPeFileAlignment;
    JclPeHeader_OperatingSystemVersion:
      Result := RsPeOperatingSystemVersion;
    JclPeHeader_ImageVersion:
      Result := RsPeImageVersion;
    JclPeHeader_SubsystemVersion:
      Result := RsPeSubsystemVersion;
    JclPeHeader_Win32VersionValue:
      Result := RsPeWin32VersionValue;
    JclPeHeader_SizeOfImage:
      Result := RsPeSizeOfImage;
    JclPeHeader_SizeOfHeaders:
      Result := RsPeSizeOfHeaders;
    JclPeHeader_CheckSum:
      Result := RsPeCheckSum;
    JclPeHeader_Subsystem:
      Result := RsPeSubsystem;
    JclPeHeader_DllCharacteristics:
      Result := RsPeDllCharacteristics;
    JclPeHeader_SizeOfStackReserve:
      Result := RsPeSizeOfStackReserve;
    JclPeHeader_SizeOfStackCommit:
      Result := RsPeSizeOfStackCommit;
    JclPeHeader_SizeOfHeapReserve:
      Result := RsPeSizeOfHeapReserve;
    JclPeHeader_SizeOfHeapCommit:
      Result := RsPeSizeOfHeapCommit;
    JclPeHeader_LoaderFlags:
      Result := RsPeLoaderFlags;
    JclPeHeader_NumberOfRvaAndSizes:
      Result := RsPeNumberOfRvaAndSizes;
  else
    Result := '';
  end;
end;

function TJclPeImage.IsBrokenFormat: Boolean;
  function IsBrokenFormat32: Boolean;
  var
    OptionalHeader: TImageOptionalHeader32;
  begin
    OptionalHeader := OptionalHeader32;
    Result := not ((OptionalHeader.AddressOfEntryPoint = 0) or IsCLR);
    if Result then
    begin
      Result := (ImageSectionCount = 0);
      if not Result then
        with ImageSectionHeaders[0] do
          Result := (VirtualAddress <> OptionalHeader.BaseOfCode) or (SizeOfRawData = 0) or
            (OptionalHeader.AddressOfEntryPoint > VirtualAddress + Misc.VirtualSize) or
            (Characteristics and (IMAGE_SCN_CNT_CODE or IMAGE_SCN_MEM_WRITE) <> IMAGE_SCN_CNT_CODE);
    end;
  end;
  function IsBrokenFormat64: Boolean;
  var
    OptionalHeader: TImageOptionalHeader64;
  begin
    OptionalHeader := OptionalHeader64;
    Result := not ((OptionalHeader.AddressOfEntryPoint = 0) or IsCLR);
    if Result then
    begin
      Result := (ImageSectionCount = 0);
      if not Result then
        with ImageSectionHeaders[0] do
          Result := (VirtualAddress <> OptionalHeader.BaseOfCode) or (SizeOfRawData = 0) or
            (OptionalHeader.AddressOfEntryPoint > VirtualAddress + Misc.VirtualSize) or
            (Characteristics and (IMAGE_SCN_CNT_CODE or IMAGE_SCN_MEM_WRITE) <> IMAGE_SCN_CNT_CODE);
    end;
  end;
begin
  case Target of
    taWin32:
      Result := IsBrokenFormat32;
    taWin64:
      Result := IsBrokenFormat64;
    //taUnknown:
  else
    Result := False; // don't know how to check it
  end;
end;

function TJclPeImage.IsCLR: Boolean;
begin
  Result := DirectoryExists[IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR] and CLRHeader.HasMetadata;
end;

function TJclPeImage.IsSystemImage: Boolean;
begin       
  Result := StatusOK and FLoadedImage.fSystemImage;
end;

class function TJclPeImage.LoadConfigNames(Index: TJclLoadConfig): string;
begin
  case Index of
    JclLoadConfig_Characteristics:
      Result := RsPeCharacteristics;
    JclLoadConfig_TimeDateStamp:
      Result := RsPeTimeDateStamp;
    JclLoadConfig_Version:
      Result := RsPeVersion;
    JclLoadConfig_GlobalFlagsClear:
      Result := RsPeGlobalFlagsClear;
    JclLoadConfig_GlobalFlagsSet:
      Result := RsPeGlobalFlagsSet;
    JclLoadConfig_CriticalSectionDefaultTimeout:
      Result := RsPeCriticalSectionDefaultTimeout;
    JclLoadConfig_DeCommitFreeBlockThreshold:
      Result := RsPeDeCommitFreeBlockThreshold;
    JclLoadConfig_DeCommitTotalFreeThreshold:
      Result := RsPeDeCommitTotalFreeThreshold;
    JclLoadConfig_LockPrefixTable:
      Result := RsPeLockPrefixTable;
    JclLoadConfig_MaximumAllocationSize:
      Result := RsPeMaximumAllocationSize;
    JclLoadConfig_VirtualMemoryThreshold:
      Result := RsPeVirtualMemoryThreshold;
    JclLoadConfig_ProcessHeapFlags:
      Result := RsPeProcessHeapFlags;
    JclLoadConfig_ProcessAffinityMask:
      Result := RsPeProcessAffinityMask;
    JclLoadConfig_CSDVersion:
      Result := RsPeCSDVersion;
    JclLoadConfig_Reserved1:
      Result := RsPeReserved;
    JclLoadConfig_EditList:
      Result := RsPeEditList;
    JclLoadConfig_Reserved:
      Result := RsPeReserved;
  else
    Result := '';
  end;
end;

procedure TJclPeImage.RaiseStatusException;
begin
  if not FNoExceptions then
    case FStatus of
      stNotPE:
        raise EJclPeImageError.CreateRes(@RsPeNotPE);
      stNotFound:
        raise EJclPeImageError.CreateResFmt(@RsPeCantOpen, [FFileName]);
      stNotSupported:
        raise EJclPeImageError.CreateRes(@RsPeUnknownTarget);
      stError:
        RaiseLastOSError;
    end;
end;

function TJclPeImage.RawToVa(Raw: DWORD): Pointer;
begin
  Result := Pointer(TJclAddr(FLoadedImage.MappedAddress) + Raw);
end;

procedure TJclPeImage.ReadImageSections;
var
  I: Integer;
  Header: PImageSectionHeader;
begin
  if not StatusOK then
    Exit;
  Header := FLoadedImage.Sections;
  for I := 0 to FLoadedImage.NumberOfSections - 1 do
  begin
    FImageSections.AddObject(Copy(PChar(@Header.Name), 1, IMAGE_SIZEOF_SHORT_NAME), Pointer(Header));
    Inc(Header);
  end;
end;

function TJclPeImage.ResourceItemCreate(AEntry: PImageResourceDirectoryEntry;
  AParentItem: TJclPeResourceItem): TJclPeResourceItem;
begin
  Result := TJclPeResourceItem.Create(Self, AParentItem, AEntry);
end;

function TJclPeImage.ResourceListCreate(ADirectory: PImageResourceDirectory;
  AParentItem: TJclPeResourceItem): TJclPeResourceList;
begin
  Result := TJclPeResourceList.Create(Self, AParentItem, ADirectory);
end;

function TJclPeImage.RvaToSection(Rva: DWORD): PImageSectionHeader;
var
  I: Integer;
  SectionHeader: PImageSectionHeader;
  EndRVA: DWORD;
begin
  Result := ImageRvaToSection(FLoadedImage.FileHeader, FLoadedImage.MappedAddress, Rva);
  if Result = nil then
    for I := 0 to FImageSections.Count - 1 do
    begin
      SectionHeader := PImageSectionHeader(FImageSections.Objects[I]);
      if SectionHeader^.SizeOfRawData = 0 then
        EndRVA := SectionHeader^.Misc.VirtualSize
      else
        EndRVA := SectionHeader^.SizeOfRawData;
      Inc(EndRVA, SectionHeader^.VirtualAddress);
      if (SectionHeader^.VirtualAddress <= Rva) and (EndRVA >= Rva) then
      begin
        Result := SectionHeader;
        Break;
      end;
    end;
end;

function TJclPeImage.RvaToVa(Rva: DWORD): Pointer;
begin
  if FAttachedImage then
    Result := FLoadedImage.MappedAddress + Rva
  else
    Result := ImageRvaToVa(FLoadedImage.FileHeader, FLoadedImage.MappedAddress, Rva, nil);
end;

function TJclPeImage.RvaToVaEx(Rva: DWORD): Pointer;
  function RvaToVaEx32(Rva: DWORD): Pointer;
  var
    OptionalHeader: TImageOptionalHeader32;
  begin
    OptionalHeader := OptionalHeader32;
    if (Rva >= OptionalHeader.ImageBase) and (Rva < (OptionalHeader.ImageBase + FLoadedImage.SizeOfImage)) then
      Dec(Rva, OptionalHeader.ImageBase);
    Result := RvaToVa(Rva);
  end;
  function RvaToVaEx64(Rva: DWORD): Pointer;
  var
    OptionalHeader: TImageOptionalHeader64;
  begin
    OptionalHeader := OptionalHeader64;
    if (Rva >= OptionalHeader.ImageBase) and (Rva < (OptionalHeader.ImageBase + FLoadedImage.SizeOfImage)) then
      Dec(Rva, OptionalHeader.ImageBase);
    Result := RvaToVa(Rva);
  end;
begin
  case Target of
    taWin32:
      Result := RvaToVaEx32(Rva);
    taWin64:
      Result := RvaToVaEx64(Rva);
    //taUnknown:
  else
    Result := nil;
  end;
end;

procedure TJclPeImage.SetFileName(const Value: TFileName);
begin
  if FFileName <> Value then
  begin
    Clear;
    FFileName := Value;
    if FFileName = '' then
      Exit;
    if MapAndLoad(PChar(FFileName), nil, FLoadedImage, True, FReadOnlyAccess) then
    begin
      FTarget := PeMapImgTarget(FLoadedImage.MappedAddress);
      if FTarget <> taUnknown then
      begin
        FStatus := stOk;
        ReadImageSections;
        AfterOpen;
      end
      else
        FStatus := stNotSupported;
    end
    else
      case GetLastError of
        ERROR_SUCCESS:
          FStatus := stNotPE;
        ERROR_FILE_NOT_FOUND:
          FStatus := stNotFound;
      else
        FStatus := stError;
      end;
    RaiseStatusException;
  end;
end;

class function TJclPeImage.ShortSectionInfo(Characteristics: DWORD): string;
type
  TSectionCharacteristics = packed record
    Mask: DWORD;
    InfoChar: Char;
  end;
const
  Info: array [1..8] of TSectionCharacteristics = (
    (Mask: IMAGE_SCN_CNT_CODE; InfoChar: 'C'),
    (Mask: IMAGE_SCN_MEM_EXECUTE; InfoChar: 'E'),
    (Mask: IMAGE_SCN_MEM_READ; InfoChar: 'R'),
    (Mask: IMAGE_SCN_MEM_WRITE; InfoChar: 'W'),
    (Mask: IMAGE_SCN_CNT_INITIALIZED_DATA; InfoChar: 'I'),
    (Mask: IMAGE_SCN_CNT_UNINITIALIZED_DATA; InfoChar: 'U'),
    (Mask: IMAGE_SCN_MEM_SHARED; InfoChar: 'S'),
    (Mask: IMAGE_SCN_MEM_DISCARDABLE; InfoChar: 'D')
  );
var
  I: Integer;
begin
  SetLength(Result, High(Info));
  Result := '';
  for I := Low(Info) to High(Info) do
    with Info[I] do
      if (Characteristics and Mask) = Mask then
        Result := Result + InfoChar;
end;

function TJclPeImage.StatusOK: Boolean;
begin
  Result := (FStatus = stOk);
end;

class function TJclPeImage.StampToDateTime(TimeDateStamp: DWORD): TDateTime;
begin
  Result := TimeDateStamp / SecsPerDay + UnixTimeStart
end;

procedure TJclPeImage.TryGetNamesForOrdinalImports;
begin
  if StatusOK then
  begin
    GetImportList;
    FImportList.TryGetNamesForOrdinalImports;
  end;
end;

function TJclPeImage.VerifyCheckSum: Boolean;
  function VerifyCheckSum32: Boolean;
  var
    OptionalHeader: TImageOptionalHeader32;
  begin
    OptionalHeader := OptionalHeader32;
    Result := StatusOK and ((OptionalHeader.CheckSum = 0) or (CalculateCheckSum = OptionalHeader.CheckSum));
  end;
  function VerifyCheckSum64: Boolean;
  var
    OptionalHeader: TImageOptionalHeader64;
  begin
    OptionalHeader := OptionalHeader64;
    Result := StatusOK and ((OptionalHeader.CheckSum = 0) or (CalculateCheckSum = OptionalHeader.CheckSum));
  end;
begin
  CheckNotAttached;
  case Target of
    taWin32:
      Result := VerifyCheckSum32;
    taWin64:
      Result := VerifyCheckSum64;
    //taUnknown: ;
  else
    Result := True;
  end;
end;

//=== { TJclPePackageInfo } ==================================================

constructor TJclPePackageInfo.Create(ALibHandle: THandle);
begin
  FContains := TStringList.Create;
  FRequires := TStringList.Create;
  FEnsureExtension := True;
  FSorted := True;
  ReadPackageInfo(ALibHandle);
end;

destructor TJclPePackageInfo.Destroy;
begin
  FreeAndNil(FContains);
  FreeAndNil(FRequires);
  inherited Destroy;
end;

function TJclPePackageInfo.GetContains: TStrings;
begin
  Result := FContains;
end;

function TJclPePackageInfo.GetContainsCount: Integer;
begin
  Result := Contains.Count;
end;

function TJclPePackageInfo.GetContainsFlags(Index: Integer): Byte;
begin
  Result := Byte(Contains.Objects[Index]);
end;

function TJclPePackageInfo.GetContainsNames(Index: Integer): string;
begin
  Result := Contains[Index];
end;

function TJclPePackageInfo.GetRequires: TStrings;
begin
  Result := FRequires;
end;

function TJclPePackageInfo.GetRequiresCount: Integer;
begin
  Result := Requires.Count;
end;

function TJclPePackageInfo.GetRequiresNames(Index: Integer): string;
begin
  Result := Requires[Index];
  if FEnsureExtension then
    StrEnsureSuffix(BinaryExtensionPackage, Result);
end;

class function TJclPePackageInfo.PackageModuleTypeToString(Flags: Integer): string;
begin
  case Flags and pfModuleTypeMask of
    pfExeModule, pfModuleTypeMask:
      Result := RsPePkgExecutable;
    pfPackageModule:
      Result := RsPePkgPackage;
    pfLibraryModule:
      Result := PsPePkgLibrary;
  else
    Result := '';
  end;
end;

class function TJclPePackageInfo.PackageOptionsToString(Flags: Integer): string;
begin
  Result := '';
  AddFlagTextRes(Result, @RsPePkgNeverBuild, Flags, pfNeverBuild);
  AddFlagTextRes(Result, @RsPePkgDesignOnly, Flags, pfDesignOnly);
  AddFlagTextRes(Result, @RsPePkgRunOnly, Flags, pfRunOnly);
  AddFlagTextRes(Result, @RsPePkgIgnoreDupUnits, Flags, pfIgnoreDupUnits);
end;

class function TJclPePackageInfo.ProducerToString(Flags: Integer): string;
begin
  case Flags and pfProducerMask of
    pfV3Produced:
      Result := RsPePkgV3Produced;
    pfProducerUndefined:
      Result := RsPePkgProducerUndefined;
    pfBCB4Produced:
      Result := RsPePkgBCB4Produced;
    pfDelphi4Produced:
      Result := RsPePkgDelphi4Produced;
  else
    Result := '';
  end;
end;

procedure PackageInfoProc(const Name: string; NameType: TNameType; AFlags: Byte; Param: Pointer);
begin
  with TJclPePackageInfo(Param) do
    case NameType of
      ntContainsUnit:
        Contains.AddObject(Name, Pointer(AFlags));
      ntRequiresPackage:
        Requires.Add(Name);
      {$IFDEF COMPILER6_UP}
      ntDcpBpiName:
        SetDcpName(Name);
      {$ENDIF COMPILER6_UP}
    end;
end;

procedure TJclPePackageInfo.ReadPackageInfo(ALibHandle: THandle);
var
  DescrResInfo: HRSRC;
  DescrResData: HGLOBAL;
begin
  FAvailable := FindResource(ALibHandle, PackageInfoResName, RT_RCDATA) <> 0;
  if FAvailable then
  begin
    GetPackageInfo(ALibHandle, Self, FFlags, PackageInfoProc);
    if FDcpName = '' then
      FDcpName := PathExtractFileNameNoExt(GetModulePath(ALibHandle)) + CompilerExtensionDCP;
    if FSorted then
    begin
      FContains.Sort;
      FRequires.Sort;
    end;
  end;
  DescrResInfo := FindResource(ALibHandle, DescriptionResName, RT_RCDATA);
  if DescrResInfo <> 0 then
  begin
    DescrResData := LoadResource(ALibHandle, DescrResInfo);
    if DescrResData <> 0 then
    begin
      FDescription := WideCharLenToString(LockResource(DescrResData),
        SizeofResource(ALibHandle, DescrResInfo));
      StrResetLength(FDescription);
    end;
  end;
end;

procedure TJclPePackageInfo.SetDcpName(const Value: string);
begin
  FDcpName := Value;
end;

class function TJclPePackageInfo.UnitInfoFlagsToString(UnitFlags: Byte): string;
begin
  Result := '';
  AddFlagTextRes(Result, @RsPePkgMain, UnitFlags, ufMainUnit);
  AddFlagTextRes(Result, @RsPePkgPackage, UnitFlags, ufPackageUnit);
  AddFlagTextRes(Result, @RsPePkgWeak, UnitFlags, ufWeakUnit);
  AddFlagTextRes(Result, @RsPePkgOrgWeak, UnitFlags, ufOrgWeakUnit);
  AddFlagTextRes(Result, @RsPePkgImplicit, UnitFlags, ufImplicitUnit);
end;

//=== { TJclPeBorForm } ======================================================

constructor TJclPeBorForm.Create(AResItem: TJclPeResourceItem;
  AFormFlags: TFilerFlags; AFormPosition: Integer;
  const AFormClassName, AFormObjectName: string);
begin
  inherited Create;
  FResItem := AResItem;
  FFormFlags := AFormFlags;
  FFormPosition := AFormPosition;
  FFormClassName := AFormClassName;
  FFormObjectName := AFormObjectName;
end;

procedure TJclPeBorForm.ConvertFormToText(const Stream: TStream);
var
  SourceStream: TJclPeResourceRawStream;
begin
  SourceStream := TJclPeResourceRawStream.Create(ResItem);
  try
    ObjectBinaryToText(SourceStream, Stream);
  finally
    SourceStream.Free;
  end;
end;

procedure TJclPeBorForm.ConvertFormToText(const Strings: TStrings);
var
  TempStream: TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  try
    ConvertFormToText(TempStream);
    TempStream.Seek(0, soFromBeginning);
    Strings.LoadFromStream(TempStream);
  finally
    TempStream.Free;
  end;    
end;

function TJclPeBorForm.GetDisplayName: string;
begin
  if FFormObjectName <> '' then
    Result := FFormObjectName + ': '
  else
    Result := '';
  Result := Result + FFormClassName;
end;

//=== { TJclPeBorImage } =====================================================

constructor TJclPeBorImage.Create(ANoExceptions: Boolean);
begin
  FForms := TObjectList.Create(True);
  FPackageInfoSorted := True;
  inherited Create(ANoExceptions);
end;

destructor TJclPeBorImage.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FForms);
end;

procedure TJclPeBorImage.AfterOpen;
var
  HasDVCLAL, HasPACKAGEINFO, HasPACKAGEOPTIONS: Boolean;
begin
  inherited AfterOpen;
  if StatusOK then
    with ResourceList do
    begin
      HasDVCLAL := (FindResource(rtRCData, DVclAlResName) <> nil);
      HasPACKAGEINFO := (FindResource(rtRCData, PackageInfoResName) <> nil);
      HasPACKAGEOPTIONS := (FindResource(rtRCData, PackageOptionsResName) <> nil);
      FIsPackage := HasPACKAGEINFO and HasPACKAGEOPTIONS;
      FIsBorlandImage := HasDVCLAL or FIsPackage;
    end;
end;

procedure TJclPeBorImage.Clear;
begin
  FForms.Clear;
  FreeAndNil(FPackageInfo);
  FreeLibHandle;
  inherited Clear;
  FIsBorlandImage := False;
  FIsPackage := False;
  FPackageCompilerVersion := 0;
end;

procedure TJclPeBorImage.CreateFormsList;
var
  ResTypeItem: TJclPeResourceItem;
  I: Integer;

  procedure ProcessListItem(DfmResItem: TJclPeResourceItem);
  const
    FilerSignature: array [1..4] of Char = 'TPF0';
  var
    SourceStream: TJclPeResourceRawStream;
    Reader: TReader;
    FormFlags: TFilerFlags;
    FormPosition: Integer;
    ClassName, FormName: string;
  begin
    SourceStream := TJclPeResourceRawStream.Create(DfmResItem);
    try
      if (SourceStream.Size > SizeOf(FilerSignature)) and
        (PInteger(SourceStream.Memory)^ = Integer(FilerSignature)) then
      begin
        Reader := TReader.Create(SourceStream, 4096);
        try
          Reader.ReadSignature;
          Reader.ReadPrefix(FormFlags, FormPosition);
          ClassName := Reader.ReadStr;
          FormName := Reader.ReadStr;
          FForms.Add(TJclPeBorForm.Create(DfmResItem, FormFlags, FormPosition,
            ClassName, FormName));
        finally
          Reader.Free;
        end;
      end;
    finally
      SourceStream.Free;
    end;
  end;

begin
  if StatusOK then
    with ResourceList do
    begin
      ResTypeItem := FindResource(rtRCData, '');
      if ResTypeItem <> nil then
        with ResTypeItem.List do
          for I := 0 to Count - 1 do
            ProcessListItem(Items[I].List[0]);
    end;
end;

function TJclPeBorImage.DependedPackages(List: TStrings; FullPathName, Descriptions: Boolean): Boolean;
var
  ImportList: TStringList;
  I: Integer;
  Name: string;
begin
  Result := IsBorlandImage;
  if not Result then
    Exit;
  ImportList := InternalImportedLibraries(FileName, True, FullPathName, nil);
  List.BeginUpdate;
  try
    for I := 0 to ImportList.Count - 1 do
    begin
      Name := ImportList[I];
      if StrSame(ExtractFileExt(Name), BinaryExtensionPackage) then
      begin
        if Descriptions then
          List.Add(Name + '=' + GetPackageDescription(PChar(Name)))
        else
          List.Add(Name);
      end;
    end;
  finally
    ImportList.Free;
    List.EndUpdate;
  end;
end;

function TJclPeBorImage.FreeLibHandle: Boolean;
begin
  if FLibHandle <> 0 then
  begin
    Result := FreeLibrary(FLibHandle);
    FLibHandle := 0;
  end
  else
    Result := True;
end;

function TJclPeBorImage.GetFormCount: Integer;
begin
  if FForms.Count = 0 then
    CreateFormsList;
  Result := FForms.Count;
end;

function TJclPeBorImage.GetFormFromName(const FormClassName: string): TJclPeBorForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FormCount - 1 do
    if StrSame(FormClassName, Forms[I].FormClassName) then
    begin
      Result := Forms[I];
      Break;
    end;
end;

function TJclPeBorImage.GetForms(Index: Integer): TJclPeBorForm;
begin
  Result := TJclPeBorForm(FForms[Index]);
end;

function TJclPeBorImage.GetLibHandle: THandle;
begin
  if StatusOK and (FLibHandle = 0) then
  begin
    FLibHandle := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE);
    if FLibHandle = 0 then
      RaiseLastOSError;
  end;
  Result := FLibHandle;
end;

function TJclPeBorImage.GetPackageCompilerVersion: Integer;
var
  I: Integer;
  ImportName: string;

  function CheckName: Boolean;
  begin
    Result := False;
    ImportName := AnsiUpperCase(ImportName);
    if StrSame(ExtractFileExt(ImportName), BinaryExtensionPackage) then
    begin
      ImportName := PathExtractFileNameNoExt(ImportName);
      if (Length(ImportName) = 5) and
        CharIsDigit(ImportName[4]) and CharIsDigit(ImportName[5]) and
        ((Pos('RTL', ImportName) = 1) or (Pos('VCL', ImportName) = 1)) then
      begin
        FPackageCompilerVersion := StrToIntDef(Copy(ImportName, 4, 2), 0);
        Result := True;
      end;
    end;
  end;

begin
  if (FPackageCompilerVersion = 0) and IsPackage then
  begin
    with ImportList do
      for I := 0 to UniqueLibItemCount - 1 do
      begin
        ImportName := UniqueLibNames[I];
        if CheckName then
          Break;
      end;
    if FPackageCompilerVersion = 0 then
    begin
      ImportName := ExtractFileName(FileName);
      CheckName;
    end;  
  end;
  Result := FPackageCompilerVersion;
end;

function TJclPeBorImage.GetPackageInfo: TJclPePackageInfo;
begin
  if StatusOK and (FPackageInfo = nil) then
  begin
    GetLibHandle;
    FPackageInfo := TJclPePackageInfo.Create(FLibHandle);
    FPackageInfo.Sorted := FPackageInfoSorted;
    FreeLibHandle;
  end;
  Result := FPackageInfo;
end;

//=== { TJclPeNameSearch } ===================================================

constructor TJclPeNameSearch.Create(const FunctionName, Path: string; Options: TJclPeNameSearchOptions);
begin
  inherited Create(True);
  FFunctionName := FunctionName;
  FOptions := Options;
  FPath := Path;
  FreeOnTerminate := True;
end;

function TJclPeNameSearch.CompareName(const FunctionName, ComparedName: string): Boolean;
begin
  Result := PeSmartFunctionNameSame(ComparedName, FunctionName, [scIgnoreCase]);
end;

procedure TJclPeNameSearch.DoFound;
begin
  if Assigned(FOnFound) then
    FOnFound(Self, F_FileName, F_FunctionName, F_Option);
end;

procedure TJclPeNameSearch.DoProcessFile;
begin
  if Assigned(FOnProcessFile) then
    FOnProcessFile(Self, FPeImage, F_Process);
end;

procedure TJclPeNameSearch.Execute;
var
  PathList: TStringList;
  I: Integer;

  function CompareNameAndNotify(const S: string): Boolean;
  begin
    Result := CompareName(S, FFunctionName);
    if Result and not Terminated then
    begin
      F_FunctionName := S;
      Synchronize(DoFound);
    end;
  end;

  procedure ProcessDirectorySearch(const DirName: string);
  var
    Se: TSearchRec;
    SearchResult: Integer;
    ImportList: TJclPeImportList;
    ExportList: TJclPeExportFuncList;
    I: Integer;
  begin
    SearchResult := FindFirst(DirName, faArchive + faReadOnly, Se);
    try
      while not Terminated and (SearchResult = 0) do
      begin
        F_FileName := PathAddSeparator(ExtractFilePath(DirName)) + Se.Name;
        F_Process := True;
        FPeImage.FileName := F_FileName;
        if Assigned(FOnProcessFile) then
          Synchronize(DoProcessFile);
        if F_Process and FPeImage.StatusOK then
        begin
          if seExports in FOptions then
          begin
            ExportList := FPeImage.ExportList;
            F_Option := seExports;
            for I := 0 to ExportList.Count - 1 do
            begin
              if Terminated then
                Break;
              CompareNameAndNotify(ExportList[I].Name);
            end;
          end;
          if FOptions * [seImports, seDelayImports, seBoundImports] <> [] then
          begin
            ImportList := FPeImage.ImportList;
            FPeImage.TryGetNamesForOrdinalImports;
            for I := 0 to ImportList.AllItemCount - 1 do
              with ImportList.AllItems[I] do
              begin
                if Terminated then
                  Break;
                case ImportLib.ImportKind of
                  ikImport:
                    if seImports in FOptions then
                    begin
                      F_Option := seImports;
                      CompareNameAndNotify(Name);
                    end;
                  ikDelayImport:
                    if seDelayImports in FOptions then
                    begin
                      F_Option := seDelayImports;
                      CompareNameAndNotify(Name);
                    end;
                  ikBoundImport:
                    if seDelayImports in FOptions then
                    begin
                      F_Option := seBoundImports;
                      CompareNameAndNotify(Name);
                    end;
                end;
              end;
          end;
        end;
        SearchResult := FindNext(Se);
      end;
    finally
      FindClose(Se);
    end;
  end;

begin
  FPeImage := TJclPeImage.Create(True);
  PathList := TStringList.Create;
  try
    PathList.Sorted := True;
    PathList.Duplicates := dupIgnore;
    StrToStrings(FPath, ';', PathList);
    for I := 0 to PathList.Count - 1 do
      ProcessDirectorySearch(PathAddSeparator(Trim(PathList[I])) + '*.*');
  finally
    PathList.Free;
    FPeImage.Free;
  end;
end;

procedure TJclPeNameSearch.Start;
begin
  Resume;
end;

//=== PE Image miscellaneous functions =======================================

function IsValidPeFile(const FileName: TFileName): Boolean;
var
  NtHeaders: TImageNtHeaders32;
begin
  Result := PeGetNtHeaders32(FileName, NtHeaders);
end;

function InternalGetNtHeaders32(const FileName: TFileName; var NtHeaders): Boolean;
var
  FileHandle: THandle;
  Mapping: TJclFileMapping;
  View: TJclFileMappingView;
  HeadersPtr: PImageNtHeaders32;
begin
  Result := False;
  FillChar(NtHeaders, SizeOf(NtHeaders), #0);
  FileHandle := FileOpen(FileName, fmOpenRead or fmShareDenyWrite);
  if FileHandle = INVALID_HANDLE_VALUE then
    Exit;
  try
    if GetSizeOfFile(FileHandle) >= SizeOf(TImageDosHeader) then
    begin
      Mapping := TJclFileMapping.Create(FileHandle, '', PAGE_READONLY, 0, nil);
      try
        View := TJclFileMappingView.Create(Mapping, FILE_MAP_READ, 0, 0);
        HeadersPtr := PeMapImgNtHeaders32(View.Memory);
        if HeadersPtr <> nil then
        begin
          Result := True;
          TImageNtHeaders32(NtHeaders) := HeadersPtr^;
        end;
      finally
        Mapping.Free;
      end;
    end;
  finally
    FileClose(FileHandle);
  end;
end;

{$IFDEF KEEP_DEPRECATED}
function PeGetNtHeaders(const FileName: TFileName; var NtHeaders: TImageNtHeaders): Boolean;
begin
  Result := InternalGetNtHeaders32(FileName, NtHeaders);
end;
{$ENDIF KEEP_DEPRECATED}

function PeGetNtHeaders32(const FileName: TFileName; var NtHeaders: TImageNtHeaders32): Boolean;
begin
  Result := InternalGetNtHeaders32(FileName, NtHeaders);
end;

function PeGetNtHeaders64(const FileName: TFileName; var NtHeaders: TImageNtHeaders64): Boolean;
var
  FileHandle: THandle;
  Mapping: TJclFileMapping;
  View: TJclFileMappingView;
  HeadersPtr: PImageNtHeaders64;
begin
  Result := False;
  FillChar(NtHeaders, SizeOf(NtHeaders), #0);
  FileHandle := FileOpen(FileName, fmOpenRead or fmShareDenyWrite);
  if FileHandle = INVALID_HANDLE_VALUE then
    Exit;
  try
    if GetSizeOfFile(FileHandle) >= SizeOf(TImageDosHeader) then
    begin
      Mapping := TJclFileMapping.Create(FileHandle, '', PAGE_READONLY, 0, nil);
      try
        View := TJclFileMappingView.Create(Mapping, FILE_MAP_READ, 0, 0);
        HeadersPtr := PeMapImgNtHeaders64(View.Memory);
        if HeadersPtr <> nil then
        begin
          Result := True;
          NtHeaders := HeadersPtr^;
        end;
      finally
        Mapping.Free;
      end;
    end;
  finally
    FileClose(FileHandle);
  end;
end;

function PeCreateNameHintTable(const FileName: TFileName): Boolean;
var
  PeImage, ExportsImage: TJclPeImage;
  I: Integer;
  ImportItem: TJclPeImportLibItem;
  Thunk32: PImageThunkData32;
  Thunk64: PImageThunkData64;
  OrdinalName: PImageImportByName;
  ExportItem: TJclPeExportFuncItem;
  Cache: TJclPeImagesCache;
  ImageBase32: TJclAddr32;
  ImageBase64: TJclAddr64;
begin
  Cache := TJclPeImagesCache.Create;
  try
    PeImage := TJclPeImage.Create(False);
    try
      PeImage.ReadOnlyAccess := False;
      PeImage.FileName := FileName;
      Result := PeImage.ImportList.Count > 0;
      for I := 0 to PeImage.ImportList.Count - 1 do
      begin
        ImportItem := PeImage.ImportList[I];
        if ImportItem.ImportKind = ikBoundImport then
          Continue;
        ExportsImage := Cache[ImportItem.FileName];
        ExportsImage.ExportList.PrepareForFastNameSearch;
        case PEImage.Target of
          taWin32:
            begin
              Thunk32 := ImportItem.ThunkData32;
              ImageBase32 := PeImage.OptionalHeader32.ImageBase;
              while Thunk32^.Function_ <> 0 do
              begin
                if Thunk32^.Ordinal and IMAGE_ORDINAL_FLAG32 = 0 then
                begin
                  case ImportItem.ImportKind of
                    ikImport:
                      OrdinalName := PImageImportByName(PeImage.RvaToVa(Thunk32^.AddressOfData));
                    ikDelayImport:
                      OrdinalName := PImageImportByName(PeImage.RvaToVa(Thunk32^.AddressOfData - ImageBase32));
                  else
                    OrdinalName := nil;
                  end;
                  ExportItem := ExportsImage.ExportList.ItemFromName[PChar(@OrdinalName.Name)];
                  if ExportItem <> nil then
                    OrdinalName.Hint := ExportItem.Hint
                  else
                    OrdinalName.Hint := 0;
                end;
                Inc(Thunk32);
              end;
            end;
          taWin64:
            begin
              Thunk64 := ImportItem.ThunkData64;
              ImageBase64 := PeImage.OptionalHeader64.ImageBase;
              while Thunk64^.Function_ <> 0 do
              begin
                if Thunk64^.Ordinal and IMAGE_ORDINAL_FLAG64 = 0 then
                begin
                  case ImportItem.ImportKind of
                    ikImport:
                      OrdinalName := PImageImportByName(PeImage.RvaToVa(Thunk64^.AddressOfData));
                    ikDelayImport:
                      OrdinalName := PImageImportByName(PeImage.RvaToVa(Thunk64^.AddressOfData - ImageBase64));
                  else
                    OrdinalName := nil;
                  end;
                  ExportItem := ExportsImage.ExportList.ItemFromName[PChar(@OrdinalName.Name)];
                  if ExportItem <> nil then
                    OrdinalName.Hint := ExportItem.Hint
                  else
                    OrdinalName.Hint := 0;
                end;
                Inc(Thunk64);
              end;
            end;
        end;
      end;
    finally
      PeImage.Free;
    end;
  finally
    Cache.Free;
  end;
end;

{$IFDEF KEEP_DEPRECATED}
function PeRebaseImage(const ImageName: TFileName; NewBase, TimeStamp, MaxNewSize: DWORD): TJclRebaseImageInfo;
begin
  Result := PeRebaseImage32(ImageName, NewBase, TimeStamp, MaxNewSize);
end;
{$ENDIF KEEP_DEPRECATED}

function PeRebaseImage32(const ImageName: TFileName; NewBase: TJclAddr32;
  TimeStamp, MaxNewSize: DWORD): TJclRebaseImageInfo32;
  function CalculateBaseAddress: TJclAddr32;
  var
    FirstChar: Char;
    ModuleName: string;
  begin
    ModuleName := ExtractFileName(ImageName);
    if Length(ModuleName) > 0 then
      FirstChar := UpCase(ModuleName[1])
    else
      FirstChar := AnsiNull;
    if not (FirstChar in AnsiUppercaseLetters) then
      FirstChar := 'A';
    Result := $60000000 + (((Ord(FirstChar) - Ord('A')) div 3) * $1000000);
  end;

begin
  if NewBase = 0 then
    NewBase := CalculateBaseAddress;
  with Result do
  begin
    NewImageBase := NewBase;
    Win32Check(ReBaseImage(PChar(ImageName), nil, True, False, False, MaxNewSize,
      OldImageSize, OldImageBase, NewImageSize, NewImageBase, TimeStamp));
  end;
end;

function PeRebaseImage64(const ImageName: TFileName; NewBase: TJclAddr64;
  TimeStamp, MaxNewSize: DWORD): TJclRebaseImageInfo64;
  function CalculateBaseAddress: TJclAddr64;
  var
    FirstChar: Char;
    ModuleName: string;
  begin
    ModuleName := ExtractFileName(ImageName);
    if Length(ModuleName) > 0 then
      FirstChar := UpCase(ModuleName[1])
    else
      FirstChar := AnsiNull;
    if not (FirstChar in AnsiUppercaseLetters) then
      FirstChar := 'A';
    Result := $60000000 + (((Ord(FirstChar) - Ord('A')) div 3) * $1000000);
    Result := Result shl 32;
  end;

begin
  if NewBase = 0 then
    NewBase := CalculateBaseAddress;
  with Result do
  begin
    NewImageBase := NewBase;
    Win32Check(ReBaseImage64(PChar(ImageName), nil, True, False, False, MaxNewSize,
      OldImageSize, OldImageBase, NewImageSize, NewImageBase, TimeStamp));
  end;
end;

function PeUpdateLinkerTimeStamp(const FileName: string; const Time: TDateTime): Boolean;
var
  Mapping: TJclFileMapping;
  View: TJclFileMappingView;
  Headers: PImageNtHeaders32; // works with 64-bit binaries too
                              // only the optional field differs
begin
  Mapping := TJclFileMapping.Create(FileName, fmOpenReadWrite, '', PAGE_READWRITE, 0, nil);
  try
    View := TJclFileMappingView.Create(Mapping, FILE_MAP_WRITE, 0, 0);
    Headers := PeMapImgNtHeaders32(View.Memory);
    Result := (Headers <> nil);
    if Result then
      Headers^.FileHeader.TimeDateStamp := TJclPeImage.DateTimeToStamp(Time);
  finally
    Mapping.Free;
  end;
end;

function PeReadLinkerTimeStamp(const FileName: string): TDateTime;
var
  Mapping: TJclFileMappingStream;
  Headers: PImageNtHeaders32; // works with 64-bit binaries too
                              // only the optional field differs
begin
  Mapping := TJclFileMappingStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Headers := PeMapImgNtHeaders32(Mapping.Memory);
    if Headers <> nil then
      Result := TJclPeImage.StampToDateTime(Headers^.FileHeader.TimeDateStamp)
    else
      Result := -1;
  finally
    Mapping.Free;
  end;
end;

{ TODO -cHelp : Author: Uwe Schuster(just a generic version of JclDebug.InsertDebugDataIntoExecutableFile) }
function PeInsertSection(const FileName: TFileName; SectionStream: TStream; SectionName: string): Boolean;
  procedure RoundUpToAlignment(var Value: DWORD; Alignment: DWORD);
  begin
    if (Value mod Alignment) <> 0 then
      Value := ((Value div Alignment) + 1) * Alignment;
  end;
  function PeInsertSection32(ImageStream: TMemoryStream): Boolean;
  var
    NtHeaders: PImageNtHeaders32;
    Sections, LastSection, NewSection: PImageSectionHeader;
    VirtualAlignedSize: DWORD;
    I, X, NeedFill: Integer;
    SectionDataSize: Integer;
  begin
    Result := True;
    try
      SectionDataSize := SectionStream.Size;
      NtHeaders := PeMapImgNtHeaders32(ImageStream.Memory);
      Assert(NtHeaders <> nil);
      Sections := PeMapImgSections32(NtHeaders);
      Assert(Sections <> nil);
      // Check whether there is not a section with the name already. If so, return True (#0000069)
      if PeMapImgFindSection32(NtHeaders, SectionName) <> nil then
      begin
        Result := True;
        Exit;
      end;

      LastSection := Sections;
      Inc(LastSection, NtHeaders^.FileHeader.NumberOfSections - 1);
      NewSection := LastSection;
      Inc(NewSection);

      // Increase the number of sections
      Inc(NtHeaders^.FileHeader.NumberOfSections);
      FillChar(NewSection^, SizeOf(TImageSectionHeader), #0);
      // JCLDEBUG Virtual Address
      NewSection^.VirtualAddress := LastSection^.VirtualAddress + LastSection^.Misc.VirtualSize;
      RoundUpToAlignment(NewSection^.VirtualAddress, NtHeaders^.OptionalHeader.SectionAlignment);
      // JCLDEBUG Physical Offset
      NewSection^.PointerToRawData := LastSection^.PointerToRawData + LastSection^.SizeOfRawData;
      RoundUpToAlignment(NewSection^.PointerToRawData, NtHeaders^.OptionalHeader.FileAlignment);
      // JCLDEBUG Section name
      StrPLCopy(PChar(@NewSection^.Name), SectionName, IMAGE_SIZEOF_SHORT_NAME);
      // JCLDEBUG Characteristics flags
      NewSection^.Characteristics := IMAGE_SCN_MEM_READ or IMAGE_SCN_CNT_INITIALIZED_DATA;

      // Size of virtual data area
      NewSection^.Misc.VirtualSize := SectionDataSize;
      VirtualAlignedSize := SectionDataSize;
      RoundUpToAlignment(VirtualAlignedSize, NtHeaders^.OptionalHeader.SectionAlignment);
      // Update Size of Image
      Inc(NtHeaders^.OptionalHeader.SizeOfImage, VirtualAlignedSize);
      // Raw data size
      NewSection^.SizeOfRawData := SectionDataSize;
      RoundUpToAlignment(NewSection^.SizeOfRawData, NtHeaders^.OptionalHeader.FileAlignment);
      // Update Initialized data size
      Inc(NtHeaders^.OptionalHeader.SizeOfInitializedData, NewSection^.SizeOfRawData);

      // Fill data to alignment
      NeedFill := Integer(NewSection^.SizeOfRawData) - SectionDataSize;

      // Note: Delphi linker seems to generate incorrect (unaligned) size of
      // the executable when adding TD32 debug data so the position could be
      // behind the size of the file then.
      ImageStream.Seek(NewSection^.PointerToRawData, soFromBeginning);
      ImageStream.CopyFrom(SectionStream, 0);
      X := 0;
      for I := 1 to NeedFill do
        ImageStream.WriteBuffer(X, 1);
    except
      Result := False;
    end;
  end;
  function PeInsertSection64(ImageStream: TMemoryStream): Boolean;
  var
    NtHeaders: PImageNtHeaders64;
    Sections, LastSection, NewSection: PImageSectionHeader;
    VirtualAlignedSize: DWORD;
    I, X, NeedFill: Integer;
    SectionDataSize: Integer;
  begin
    Result := True;
    try
      SectionDataSize := SectionStream.Size;
      NtHeaders := PeMapImgNtHeaders64(ImageStream.Memory);
      Assert(NtHeaders <> nil);
      Sections := PeMapImgSections64(NtHeaders);
      Assert(Sections <> nil);
      // Check whether there is not a section with the name already. If so, return True (#0000069)
      if PeMapImgFindSection64(NtHeaders, SectionName) <> nil then
      begin
        Result := True;
        Exit;
      end;

      LastSection := Sections;
      Inc(LastSection, NtHeaders^.FileHeader.NumberOfSections - 1);
      NewSection := LastSection;
      Inc(NewSection);

      // Increase the number of sections
      Inc(NtHeaders^.FileHeader.NumberOfSections);
      FillChar(NewSection^, SizeOf(TImageSectionHeader), #0);
      // JCLDEBUG Virtual Address
      NewSection^.VirtualAddress := LastSection^.VirtualAddress + LastSection^.Misc.VirtualSize;
      RoundUpToAlignment(NewSection^.VirtualAddress, NtHeaders^.OptionalHeader.SectionAlignment);
      // JCLDEBUG Physical Offset
      NewSection^.PointerToRawData := LastSection^.PointerToRawData + LastSection^.SizeOfRawData;
      RoundUpToAlignment(NewSection^.PointerToRawData, NtHeaders^.OptionalHeader.FileAlignment);
      // JCLDEBUG Section name
      StrPLCopy(PChar(@NewSection^.Name), SectionName, IMAGE_SIZEOF_SHORT_NAME);
      // JCLDEBUG Characteristics flags
      NewSection^.Characteristics := IMAGE_SCN_MEM_READ or IMAGE_SCN_CNT_INITIALIZED_DATA;

      // Size of virtual data area
      NewSection^.Misc.VirtualSize := SectionDataSize;
      VirtualAlignedSize := SectionDataSize;
      RoundUpToAlignment(VirtualAlignedSize, NtHeaders^.OptionalHeader.SectionAlignment);
      // Update Size of Image
      Inc(NtHeaders^.OptionalHeader.SizeOfImage, VirtualAlignedSize);
      // Raw data size
      NewSection^.SizeOfRawData := SectionDataSize;
      RoundUpToAlignment(NewSection^.SizeOfRawData, NtHeaders^.OptionalHeader.FileAlignment);
      // Update Initialized data size
      Inc(NtHeaders^.OptionalHeader.SizeOfInitializedData, NewSection^.SizeOfRawData);

      // Fill data to alignment
      NeedFill := Integer(NewSection^.SizeOfRawData) - SectionDataSize;

      // Note: Delphi linker seems to generate incorrect (unaligned) size of
      // the executable when adding TD32 debug data so the position could be
      // behind the size of the file then.
      ImageStream.Seek(NewSection^.PointerToRawData, soFromBeginning);
      ImageStream.CopyFrom(SectionStream, 0);
      X := 0;
      for I := 1 to NeedFill do
        ImageStream.WriteBuffer(X, 1);
    except
      Result := False;
    end;
  end;

var
  ImageStream: TMemoryStream;
begin
  Result := Assigned(SectionStream) and (SectionName <> '');
  if not Result then
    Exit;
  ImageStream := TMemoryStream.Create;
  try
    ImageStream.LoadFromFile(FileName);
    case PeMapImgTarget(ImageStream.Memory) of
      taWin32:
        Result := PeInsertSection32(ImageStream);
      taWin64:
        Result := PeInsertSection64(ImageStream);
      //taUnknown:
    else
      Result := False;
    end;

    if Result then
      ImageStream.SaveToFile(FileName);
  finally
    ImageStream.Free;
  end;
end;

function PeVerifyCheckSum(const FileName: TFileName): Boolean;
begin
  with CreatePeImage(FileName) do
  try
    Result := VerifyCheckSum;
  finally
    Free;
  end;
end;

function PeClearCheckSum(const FileName: TFileName): Boolean;
  function PeClearCheckSum32(ModuleAddress: Pointer): Boolean;
  var
    Headers: PImageNtHeaders32;
  begin
    Headers := PeMapImgNtHeaders32(ModuleAddress);
    Result := (Headers <> nil);
    if Result then
      Headers^.OptionalHeader.CheckSum := 0;
  end;
  function PeClearCheckSum64(ModuleAddress: Pointer): Boolean;
  var
    Headers: PImageNtHeaders64;
  begin
    Headers := PeMapImgNtHeaders64(ModuleAddress);
    Result := (Headers <> nil);
    if Result then
      Headers^.OptionalHeader.CheckSum := 0;
  end;
var
  Mapping: TJclFileMapping;
  View: TJclFileMappingView;
begin
  Mapping := TJclFileMapping.Create(FileName, fmOpenReadWrite, '', PAGE_READWRITE, 0, nil);
  try
    View := TJclFileMappingView.Create(Mapping, FILE_MAP_WRITE, 0, 0);
    case PeMapImgTarget(View.Memory) of
      taWin32:
        Result := PeClearCheckSum32(View.Memory);
      taWin64:
        Result := PeClearCheckSum64(View.Memory);
      //taUnknown:
    else
      Result := False;
    end;
  finally
    Mapping.Free;
  end;
end;

function PeUpdateCheckSum(const FileName: TFileName): Boolean;
var
  LI: TLoadedImage;
begin
  Result := MapAndLoad(PChar(FileName), nil, LI, True, False);
  if Result then
    Result := UnMapAndLoad(LI);
end;

// Various simple PE Image searching and listing routines

function PeDoesExportFunction(const FileName: TFileName; const FunctionName: string;
  Options: TJclSmartCompOptions): Boolean;
begin
  with CreatePeImage(FileName) do
  try
    Result := StatusOK and Assigned(ExportList.SmartFindName(FunctionName, Options));
  finally
    Free;
  end;
end;

function PeIsExportFunctionForwardedEx(const FileName: TFileName; const FunctionName: string;
  var ForwardedName: string; Options: TJclSmartCompOptions): Boolean;
var
  ExportItem: TJclPeExportFuncItem;
begin
  with CreatePeImage(FileName) do
  try
    Result := StatusOK;
    if Result then
    begin
      ExportItem := ExportList.SmartFindName(FunctionName, Options);
      if ExportItem <> nil then
      begin
        Result := ExportItem.IsForwarded;
        ForwardedName := ExportItem.ForwardedName;
      end
      else
      begin
        Result := False;
        ForwardedName := '';
      end;
    end;
  finally
    Free;
  end;
end;

function PeIsExportFunctionForwarded(const FileName: TFileName; const FunctionName: string;
  Options: TJclSmartCompOptions): Boolean;
var
  Dummy: string;
begin
  Result := PeIsExportFunctionForwardedEx(FileName, FunctionName, Dummy, Options);
end;

function PeDoesImportFunction(const FileName: TFileName; const FunctionName: string;
  const LibraryName: string; Options: TJclSmartCompOptions): Boolean;
begin
  with CreatePeImage(FileName) do
  try
    Result := StatusOK;
    if Result then
      with ImportList do
      begin
        TryGetNamesForOrdinalImports;
        Result := SmartFindName(FunctionName, LibraryName, Options) <> nil;
      end;
  finally
    Free;
  end;
end;

function PeDoesImportLibrary(const FileName: TFileName; const LibraryName: string;
  Recursive: Boolean): Boolean;
var
  SL: TStringList;
begin
  with CreatePeImage(FileName) do
  try
    Result := StatusOK;
    if Result then
    begin
      SL := InternalImportedLibraries(FileName, Recursive, False, nil);
      try
        Result := SL.IndexOf(LibraryName) > -1;
      finally
        SL.Free;
      end;
    end;
  finally
    Free;
  end;
end;

function PeImportedLibraries(const FileName: TFileName; const LibrariesList: TStrings;
  Recursive, FullPathName: Boolean): Boolean;
var
  SL: TStringList;
begin
  with CreatePeImage(FileName) do
  try
    Result := StatusOK;
    if Result then
    begin
      SL := InternalImportedLibraries(FileName, Recursive, FullPathName, nil);
      try
        LibrariesList.Assign(SL);
      finally
        SL.Free;
      end;
    end;
  finally
    Free;
  end;
end;

function PeImportedFunctions(const FileName: TFileName; const FunctionsList: TStrings;
  const LibraryName: string; IncludeLibNames: Boolean): Boolean;
var
  I: Integer;
begin
  with CreatePeImage(FileName) do
    try
      Result := StatusOK;
      if Result then
        with ImportList do
        begin
          TryGetNamesForOrdinalImports;
          FunctionsList.BeginUpdate;
          try
            for I := 0 to AllItemCount - 1 do
              with AllItems[I] do
                if ((Length(LibraryName) = 0) or StrSame(ImportLib.Name, LibraryName)) and
                  (Name <> '') then
                begin
                  if IncludeLibNames then
                    FunctionsList.Add(ImportLib.Name + '=' + Name)
                  else
                    FunctionsList.Add(Name);
                end;
          finally
            FunctionsList.EndUpdate;
          end;
        end;
    finally
      Free;
    end;
end;

function PeExportedFunctions(const FileName: TFileName; const FunctionsList: TStrings): Boolean;
var
  I: Integer;
begin
  with CreatePeImage(FileName) do
    try
      Result := StatusOK;
      if Result then
      begin
        FunctionsList.BeginUpdate;
        try
          with ExportList do
            for I := 0 to Count - 1 do
              with Items[I] do
                if not IsExportedVariable then
                  FunctionsList.Add(Name);
        finally
          FunctionsList.EndUpdate;
        end;
      end;
    finally
      Free;
    end;
end;

function PeExportedNames(const FileName: TFileName; const FunctionsList: TStrings): Boolean;
var
  I: Integer;
begin
  with CreatePeImage(FileName) do
  try
    Result := StatusOK;
    if Result then
    begin
      FunctionsList.BeginUpdate;
      try
        with ExportList do
          for I := 0 to Count - 1 do
            FunctionsList.Add(Items[I].Name);
      finally
        FunctionsList.EndUpdate;
      end;
    end;
  finally
    Free;
  end;
end;

function PeExportedVariables(const FileName: TFileName; const FunctionsList: TStrings): Boolean;
var
  I: Integer;
begin
  with CreatePeImage(FileName) do
  try
    Result := StatusOK;
    if Result then
    begin
      FunctionsList.BeginUpdate;
      try
        with ExportList do
          for I := 0 to Count - 1 do
            with Items[I] do
              if IsExportedVariable then
                FunctionsList.AddObject(Name, Pointer(Address));
      finally
        FunctionsList.EndUpdate;
      end;
    end;
  finally
    Free;
  end;
end;

function PeResourceKindNames(const FileName: TFileName; ResourceType: TJclPeResourceKind;
  const NamesList: TStrings): Boolean;
begin
  with CreatePeImage(FileName) do
  try
    Result := StatusOK and ResourceList.ListResourceNames(ResourceType, NamesList);
  finally
    Free;
  end;
end;

function PeBorFormNames(const FileName: TFileName; const NamesList: TStrings): Boolean;
var
  I: Integer;
  BorImage: TJclPeBorImage;
  BorForm: TJclPeBorForm;
begin
  BorImage := TJclPeBorImage.Create(True);
  try
    BorImage.FileName := FileName;
    Result := BorImage.IsBorlandImage;
    if Result then
    begin
      NamesList.BeginUpdate;
      try
        for I := 0 to BorImage.FormCount - 1 do
        begin
          BorForm := BorImage.Forms[I];
          NamesList.AddObject(BorForm.DisplayName, Pointer(BorForm.ResItem.RawEntryDataSize));
        end;
      finally
        NamesList.EndUpdate;
      end;
    end;
  finally
    BorImage.Free;
  end;
end;

function PeBorDependedPackages(const FileName: TFileName; PackagesList: TStrings;
  FullPathName, Descriptions: Boolean): Boolean;
var
  BorImage: TJclPeBorImage;
begin
  BorImage := TJclPeBorImage.Create(True);
  try
    BorImage.FileName := FileName;
    Result := BorImage.DependedPackages(PackagesList, FullPathName, Descriptions);
  finally
    BorImage.Free;
  end;
end;

// Missing imports checking routines

function PeFindMissingImports(const FileName: TFileName; MissingImportsList: TStrings): Boolean;
var
  Cache: TJclPeImagesCache;
  FileImage, LibImage: TJclPeImage;
  L, I: Integer;
  LibItem: TJclPeImportLibItem;
  List: TStringList;
begin
  Result := False;
  List := nil;
  Cache := TJclPeImagesCache.Create;
  try
    List := TStringList.Create;
    List.Duplicates := dupIgnore;
    List.Sorted := True;
    FileImage := Cache[FileName];
    if FileImage.StatusOK then
    begin
      for L := 0 to FileImage.ImportList.Count - 1 do
      begin
        LibItem := FileImage.ImportList[L];
        LibImage := Cache[LibItem.FileName];
        if LibImage.StatusOK then
        begin
          LibImage.ExportList.PrepareForFastNameSearch;
          for I := 0 to LibItem.Count - 1 do
            if LibImage.ExportList.ItemFromName[LibItem[I].Name] = nil then
              List.Add(LibItem.Name + '=' + LibItem[I].Name);
        end
        else
          List.Add(LibItem.Name + '=');
      end;
      MissingImportsList.Assign(List);
      Result := List.Count > 0;
    end;
  finally
    List.Free;
    Cache.Free;
  end;
end;

function PeFindMissingImports(RequiredImportsList, MissingImportsList: TStrings): Boolean;
var
  Cache: TJclPeImagesCache;
  LibImage: TJclPeImage;
  I, SepPos: Integer;
  List: TStringList;
  S, LibName, ImportName: string;
begin
  List := nil;
  Cache := TJclPeImagesCache.Create;
  try
    List := TStringList.Create;
    List.Duplicates := dupIgnore;
    List.Sorted := True;
    for I := 0 to RequiredImportsList.Count - 1 do
    begin
      S := RequiredImportsList[I];
      SepPos := Pos('=', S);
      if SepPos = 0 then
        Continue;
      LibName := StrLeft(S, SepPos - 1);
      LibImage := Cache[LibName];
      if LibImage.StatusOK then
      begin
        LibImage.ExportList.PrepareForFastNameSearch;
        ImportName := StrRestOf(S, SepPos + 1);
        if LibImage.ExportList.ItemFromName[ImportName] = nil then
          List.Add(LibName + '=' + ImportName);
      end
      else
        List.Add(LibName + '=');
    end;
    MissingImportsList.Assign(List);
    Result := List.Count > 0;
  finally
    List.Free;
    Cache.Free;
  end;
end;

function PeCreateRequiredImportList(const FileName: TFileName; RequiredImportsList: TStrings): Boolean;
begin
  Result := PeImportedFunctions(FileName, RequiredImportsList, '', True);
end;

// Mapped or loaded image related functions

{$IFDEF KEEP_DEPRECATED}
function PeMapImgNtHeaders(const BaseAddress: Pointer): PImageNtHeaders;
begin
  Result := PImageNtHeaders(PeMapImgNtHeaders32(BaseAddress));
end;
{$ENDIF KEEP_DEPRECATED}

function PeMapImgNtHeaders32(const BaseAddress: Pointer): PImageNtHeaders32;
begin
  Result := nil;
  if IsBadReadPtr(BaseAddress, SizeOf(TImageDosHeader)) then
    Exit;
  if (PImageDosHeader(BaseAddress)^.e_magic <> IMAGE_DOS_SIGNATURE) or
    (PImageDosHeader(BaseAddress)^._lfanew = 0) then
    Exit;
  Result := PImageNtHeaders32(TJclAddr(BaseAddress) + DWORD(PImageDosHeader(BaseAddress)^._lfanew));
  if IsBadReadPtr(Result, SizeOf(TImageNtHeaders32)) or
    (Result^.Signature <> IMAGE_NT_SIGNATURE) then
      Result := nil
end;

function PeMapImgNtHeaders64(const BaseAddress: Pointer): PImageNtHeaders64;
begin
  Result := nil;
  if IsBadReadPtr(BaseAddress, SizeOf(TImageDosHeader)) then
    Exit;
  if (PImageDosHeader(BaseAddress)^.e_magic <> IMAGE_DOS_SIGNATURE) or
    (PImageDosHeader(BaseAddress)^._lfanew = 0) then
    Exit;
  Result := PImageNtHeaders64(TJclAddr(BaseAddress) + DWORD(PImageDosHeader(BaseAddress)^._lfanew));
  if IsBadReadPtr(Result, SizeOf(TImageNtHeaders64)) or
    (Result^.Signature <> IMAGE_NT_SIGNATURE) then
      Result := nil
end;

function PeMapImgSize(const BaseAddress: Pointer): DWORD;
  function PeMapImgSize32(const BaseAddress: Pointer): DWORD;
  var
    NtHeaders32: PImageNtHeaders32;
  begin
    Result := 0;
    NtHeaders32 := PeMapImgNtHeaders32(BaseAddress);
    if Assigned(NtHeaders32) then
      Result := NtHeaders32^.OptionalHeader.SizeOfImage;
  end;
  function PeMapImgSize64(const BaseAddress: Pointer): DWORD;
  var
    NtHeaders64: PImageNtHeaders64;
  begin
    Result := 0;
    NtHeaders64 := PeMapImgNtHeaders64(BaseAddress);
    if Assigned(NtHeaders64) then
      Result := NtHeaders64^.OptionalHeader.SizeOfImage;
  end;
begin
  case PeMapImgTarget(BaseAddress) of
    taWin32:
      Result := PeMapImgSize32(BaseAddress);
    taWin64:
      Result := PeMapImgSize64(BaseAddress);
    //taUnknown:
  else
    Result := 0;
  end;
end;

function PeMapImgLibraryName(const BaseAddress: Pointer): string;
  function PeMapImgLibraryName32(const BaseAddress: Pointer): string;
  var
    NtHeaders: PImageNtHeaders32;
    DataDir: TImageDataDirectory;
    ExportDir: PImageExportDirectory;
  begin
    Result := '';
    NtHeaders := PeMapImgNtHeaders32(BaseAddress);
    if NtHeaders = nil then
      Exit;
    DataDir := NtHeaders^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT];
    if DataDir.Size = 0 then
      Exit;
    ExportDir := PImageExportDirectory(TJclAddr(BaseAddress) + DataDir.VirtualAddress);
    if IsBadReadPtr(ExportDir, SizeOf(TImageExportDirectory)) or (ExportDir^.Name = 0) then
      Exit;
    Result := PChar(TJclAddr(BaseAddress) + ExportDir^.Name);
  end;
  function PeMapImgLibraryName64(const BaseAddress: Pointer): string;
  var
    NtHeaders: PImageNtHeaders64;
    DataDir: TImageDataDirectory;
    ExportDir: PImageExportDirectory;
  begin
    Result := '';
    NtHeaders := PeMapImgNtHeaders64(BaseAddress);
    if NtHeaders = nil then
      Exit;
    DataDir := NtHeaders^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT];
    if DataDir.Size = 0 then
      Exit;
    ExportDir := PImageExportDirectory(TJclAddr(BaseAddress) + DataDir.VirtualAddress);
    if IsBadReadPtr(ExportDir, SizeOf(TImageExportDirectory)) or (ExportDir^.Name = 0) then
      Exit;
    Result := PChar(TJclAddr(BaseAddress) + ExportDir^.Name);
  end;
begin
  case PeMapImgTarget(BaseAddress) of
    taWin32:
      Result := PeMapImgLibraryName32(BaseAddress);
    taWin64:
      Result := PeMapImgLibraryName64(BaseAddress);
    //taUnknown:
  else
    Result := '';
  end;
end;

function PeMapImgTarget(const BaseAddress: Pointer): TJclPeTarget;
var
  ImageNtHeaders: PImageNtHeaders32;
begin
  Result := taUnknown;

  ImageNtHeaders := PeMapImgNtHeaders32(BaseAddress);
  if Assigned(ImageNtHeaders) then
    case ImageNtHeaders.FileHeader.Machine of
      IMAGE_FILE_MACHINE_I386:
        Result := taWin32;
      IMAGE_FILE_MACHINE_AMD64:
        Result := taWin64;
    end;
end;

{$IFDEF KEEP_DEPRECATED}
function PeMapImgSections(NtHeaders: PImageNtHeaders): PImageSectionHeader;
begin
  Result := PeMapImgSections32(PImageNtHeaders32(NtHeaders));
end;
{$ENDIF KEEP_DEPRECATED}

function PeMapImgSections32(NtHeaders: PImageNtHeaders32): PImageSectionHeader;
begin
  if NtHeaders = nil then
    Result := nil
  else
    Result := PImageSectionHeader(TJclAddr(@NtHeaders^.OptionalHeader) +
      NtHeaders^.FileHeader.SizeOfOptionalHeader);
end;

function PeMapImgSections64(NtHeaders: PImageNtHeaders64): PImageSectionHeader;
begin
  if NtHeaders = nil then
    Result := nil
  else
    Result := PImageSectionHeader(TJclAddr(@NtHeaders^.OptionalHeader) +
      NtHeaders^.FileHeader.SizeOfOptionalHeader);
end;

{$IFDEF KEEP_DEPRECATED}
function PeMapImgFindSection(NtHeaders: PImageNtHeaders;
  const SectionName: string): PImageSectionHeader;
begin
  Result := PeMapImgFindSection32(PImageNtHeaders32(NtHeaders), SectionName);
end;
{$ENDIF KEEP_DEPRECATED}

function PeMapImgFindSection32(NtHeaders: PImageNtHeaders32;
  const SectionName: string): PImageSectionHeader;
var
  Header: PImageSectionHeader;
  I: Integer;
  P: PChar;
begin
  Result := nil;
  if NtHeaders <> nil then
  begin
    P := PChar(SectionName);
    Header := PeMapImgSections32(NtHeaders);
    with NtHeaders^ do
      for I := 1 to FileHeader.NumberOfSections do
        if StrLComp(PChar(@Header^.Name), P, IMAGE_SIZEOF_SHORT_NAME) = 0 then
        begin
          Result := Header;
          Break;
        end
        else
          Inc(Header);
  end;
end;

function PeMapImgFindSection64(NtHeaders: PImageNtHeaders64;
  const SectionName: string): PImageSectionHeader;
var
  Header: PImageSectionHeader;
  I: Integer;
  P: PChar;
begin
  Result := nil;
  if NtHeaders <> nil then
  begin
    P := PChar(SectionName);
    Header := PeMapImgSections64(NtHeaders);
    with NtHeaders^ do
      for I := 1 to FileHeader.NumberOfSections do
        if StrLComp(PChar(@Header^.Name), P, IMAGE_SIZEOF_SHORT_NAME) = 0 then
        begin
          Result := Header;
          Break;
        end
        else
          Inc(Header);
  end;
end;

function PeMapImgFindSectionFromModule(const BaseAddress: Pointer;
  const SectionName: string): PImageSectionHeader;
  function PeMapImgFindSectionFromModule32(const BaseAddress: Pointer;
    const SectionName: string): PImageSectionHeader;
  var
    NtHeaders32: PImageNtHeaders32;
  begin
    Result := nil;
    NtHeaders32 := PeMapImgNtHeaders32(BaseAddress);
    if Assigned(NtHeaders32) then
      Result := PeMapImgFindSection32(NtHeaders32, SectionName);
  end;
  function PeMapImgFindSectionFromModule64(const BaseAddress: Pointer;
    const SectionName: string): PImageSectionHeader;
  var
    NtHeaders64: PImageNtHeaders64;
  begin
    Result := nil;
    NtHeaders64 := PeMapImgNtHeaders64(BaseAddress);
    if Assigned(NtHeaders64) then
      Result := PeMapImgFindSection64(NtHeaders64, SectionName);
  end;
begin
  case PeMapImgTarget(BaseAddress) of
    taWin32:
      Result := PeMapImgFindSectionFromModule32(BaseAddress, SectionName);
    taWin64:
      Result := PeMapImgFindSectionFromModule64(BaseAddress, SectionName);
    //taUnknown:
  else
    Result := nil;
  end;
end;

function PeMapImgExportedVariables(const Module: HMODULE; const VariablesList: TStrings): Boolean;
var
  I: Integer;
begin
  with TJclPeImage.Create(True) do
  try
    AttachLoadedModule(Module);
    Result := StatusOK;
    if Result then
    begin
      VariablesList.BeginUpdate;
      try
        with ExportList do
          for I := 0 to Count - 1 do
            with Items[I] do
              if IsExportedVariable then
                VariablesList.AddObject(Name, MappedAddress);
      finally
        VariablesList.EndUpdate;
      end;
    end;
  finally
    Free;
  end;
end;

function PeMapImgResolvePackageThunk(Address: Pointer): Pointer;
const
  JmpInstructionCode = $25FF;
type
  PPackageThunk = ^TPackageThunk;
  TPackageThunk = packed record
    JmpInstruction: Word;
    JmpAddress: PPointer;
  end;
begin
  if not IsCompiledWithPackages then
    Result := Address
  else
  if not IsBadReadPtr(Address, SizeOf(TPackageThunk)) and
    (PPackageThunk(Address)^.JmpInstruction = JmpInstructionCode) then
    Result := PPackageThunk(Address)^.JmpAddress^
  else
    Result := nil;
end;

function PeMapFindResource(const Module: HMODULE; const ResourceType: PChar;
  const ResourceName: string): Pointer;
var
  ResItem: TJclPeResourceItem;
begin
  Result := nil;
  with TJclPeImage.Create(True) do
  try
    AttachLoadedModule(Module);
    if StatusOK then
    begin
      ResItem := ResourceList.FindResource(ResourceType, PChar(ResourceName));
      if (ResItem <> nil) and ResItem.IsDirectory then
        Result := ResItem.List[0].RawEntryData;
    end;  
  finally
    Free;
  end;
end;

//=== { TJclPeSectionStream } ================================================

constructor TJclPeSectionStream.Create(Instance: HMODULE; const ASectionName: string);
begin
  inherited Create;
  Initialize(Instance, ASectionName);
end;

procedure TJclPeSectionStream.Initialize(Instance: HMODULE; const ASectionName: string);
var
  Header: PImageSectionHeader;
  NtHeaders32: PImageNtHeaders32;
  NtHeaders64: PImageNtHeaders64;
  DataSize: Integer;
begin
  FInstance := Instance;
  case PeMapImgTarget(Pointer(Instance)) of
    taWin32:
      begin
        NtHeaders32 := PeMapImgNtHeaders32(Pointer(Instance));
        if NtHeaders32 = nil then
          raise EJclPeImageError.CreateRes(@RsPeNotPE);
        Header := PeMapImgFindSection32(NtHeaders32, ASectionName);
      end;
    taWin64:
      begin
        NtHeaders64 := PeMapImgNtHeaders64(Pointer(Instance));
        if NtHeaders64 = nil then
          raise EJclPeImageError.CreateRes(@RsPeNotPE);
        Header := PeMapImgFindSection64(NtHeaders64, ASectionName);
      end;
    //toUnknown:
  else
    raise EJclPeImageError.CreateRes(@RsPeUnknownTarget);
  end;
  if Header = nil then
    raise EJclPeImageError.CreateResFmt(@RsPeSectionNotFound, [ASectionName]);
  // Borland and Microsoft seems to have swapped the meaning of this items.
  DataSize := Min(Header^.SizeOfRawData, Header^.Misc.VirtualSize);
  SetPointer(Pointer(FInstance + Header^.VirtualAddress), DataSize);
  FSectionHeader := Header^;
end;

function TJclPeSectionStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise EJclPeImageError.CreateRes(@RsPeReadOnlyStream);
end;

//=== { TJclPeMapImgHookItem } ===============================================

constructor TJclPeMapImgHookItem.Create(AList: TObjectList;
  const AFunctionName, AModuleName: string;
  ABaseAddress, ANewAddress, AOriginalAddress: Pointer);
begin
  inherited Create;
  FList := AList;
  FFunctionName := AFunctionName;
  FModuleName := AModuleName;
  FBaseAddress := ABaseAddress;
  FNewAddress := ANewAddress;
  FOriginalAddress := AOriginalAddress;
end;

destructor TJclPeMapImgHookItem.Destroy;
begin
  if FBaseAddress <> nil then
    InternalUnhook;
  inherited Destroy;
end;

function TJclPeMapImgHookItem.InternalUnhook: Boolean;
var
  Buf: TMemoryBasicInformation;
begin
  if (VirtualQuery(FBaseAddress, Buf, SizeOf(Buf)) = SizeOf(Buf)) and (Buf.State and MEM_FREE = 0) then
    Result := TJclPeMapImgHooks.ReplaceImport(FBaseAddress, ModuleName, NewAddress, OriginalAddress)
  else
    Result := True; // PE image is not available anymore (DLL got unloaded)
  if Result then
    FBaseAddress := nil;
end;

function TJclPeMapImgHookItem.Unhook: Boolean;
begin
  Result := InternalUnhook;
  if Result then
    FList.Remove(Self);
end;

//=== { TJclPeMapImgHooks } ==================================================

type
  PWin9xDebugThunk32 = ^TWin9xDebugThunk32;
  TWin9xDebugThunk32 = packed record
    PUSH: Byte;    // PUSH instruction opcode ($68)
    Addr: DWORD; // The actual address of the DLL routine
    JMP: Byte;     // JMP instruction opcode ($E9)
    Rel: DWORD;  // Relative displacement (a Kernel32 address)
  end;

function TJclPeMapImgHooks.GetItemFromNewAddress(NewAddress: Pointer): TJclPeMapImgHookItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].NewAddress = NewAddress then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TJclPeMapImgHooks.GetItemFromOriginalAddress(OriginalAddress: Pointer): TJclPeMapImgHookItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].OriginalAddress = OriginalAddress then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TJclPeMapImgHooks.GetItems(Index: Integer): TJclPeMapImgHookItem;
begin
  Result := TJclPeMapImgHookItem(Get(Index));
end;

function TJclPeMapImgHooks.HookImport(Base: Pointer; const ModuleName, FunctionName: string;
  NewAddress: Pointer; var OriginalAddress: Pointer): Boolean;
var
  ModuleHandle: THandle;
begin
  ModuleHandle := GetModuleHandle(PChar(ModuleName));
  Result := (ModuleHandle <> 0);
  if not Result then
  begin
    SetLastError(ERROR_MOD_NOT_FOUND);
    Exit;
  end;
  OriginalAddress := GetProcAddress(ModuleHandle, PChar(FunctionName));
  Result := (OriginalAddress <> nil);
  if not Result then
  begin
    SetLastError(ERROR_PROC_NOT_FOUND);
    Exit;
  end;
  Result := (ItemFromOriginalAddress[OriginalAddress] = nil) and (NewAddress <> nil) and
    (OriginalAddress <> NewAddress);
  if not Result then
  begin
    SetLastError(ERROR_ALREADY_EXISTS);
    Exit;
  end;
  if Result then
    Result := ReplaceImport(Base, ModuleName, OriginalAddress, NewAddress);
  if Result then
  begin
    Add(TJclPeMapImgHookItem.Create(Self, FunctionName, ModuleName, Base,
      NewAddress, OriginalAddress));
  end
  else
    SetLastError(ERROR_INVALID_PARAMETER);
end;

class function TJclPeMapImgHooks.IsWin9xDebugThunk(P: Pointer): Boolean;
begin
  with PWin9xDebugThunk32(P)^ do
    Result := (PUSH = $68) and (JMP = $E9);
end;

class function TJclPeMapImgHooks.ReplaceImport(Base: Pointer; const ModuleName: string;
  FromProc, ToProc: Pointer): Boolean;
// TODO: 64 bit version
var
  FromProcDebugThunk32, ImportThunk32: PWin9xDebugThunk32;
  IsThunked: Boolean;
  NtHeader32: PImageNtHeaders32;
  ImportDir: TImageDataDirectory;
  ImportDesc: PImageImportDescriptor;
  CurrName: PChar;
  ImportEntry32: PImageThunkData32;
  FoundProc: Boolean;
  WrittenBytes: Cardinal;
begin
  Result := False;
  FromProcDebugThunk32 := PWin9xDebugThunk32(FromProc);
  IsThunked := not IsWinNT and IsWin9xDebugThunk(FromProcDebugThunk32);
  NtHeader32 := PeMapImgNtHeaders32(Base);
  if NtHeader32 = nil then
    Exit;
  ImportDir := NtHeader32.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
  if ImportDir.VirtualAddress = 0 then
    Exit;
  ImportDesc := PImageImportDescriptor(TJclAddr(Base) + ImportDir.VirtualAddress);
  while ImportDesc^.Name <> 0 do
  begin
    CurrName := PChar(Base) + ImportDesc^.Name;
    if StrIComp(CurrName, PChar(ModuleName)) = 0 then
    begin
      ImportEntry32 := PImageThunkData32(TJclAddr(Base) + ImportDesc^.FirstThunk);
      while ImportEntry32^.Function_ <> 0 do
      begin
        if IsThunked then
        begin
          ImportThunk32 := PWin9xDebugThunk32(ImportEntry32^.Function_);
          FoundProc := IsWin9xDebugThunk(ImportThunk32) and (ImportThunk32^.Addr = FromProcDebugThunk32^.Addr);
        end
        else
          FoundProc := Pointer(ImportEntry32^.Function_) = FromProc;
        if FoundProc then
          Result := WriteProtectedMemory(@ImportEntry32^.Function_, @ToProc, SizeOf(ToProc), WrittenBytes);
        Inc(ImportEntry32);
      end;
    end;
    Inc(ImportDesc);
  end;
end;

class function TJclPeMapImgHooks.SystemBase: Pointer;
begin
  Result := Pointer(SystemTObjectInstance);
end;

procedure TJclPeMapImgHooks.UnhookAll;
var
  I: Integer;
begin
  I := 0;
  while I < Count do
    if not Items[I].Unhook then
      Inc(I);
end;

function TJclPeMapImgHooks.UnhookByNewAddress(NewAddress: Pointer): Boolean;
var
  Item: TJclPeMapImgHookItem;
begin
  Item := ItemFromNewAddress[NewAddress];
  Result := (Item <> nil) and Item.Unhook;
end;

procedure TJclPeMapImgHooks.UnhookByBaseAddress(BaseAddress: Pointer);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Items[I].BaseAddress = BaseAddress then
      Items[I].Unhook;
end;

// Image access under a debbuger

function InternalReadProcMem(ProcessHandle: THandle; Address: DWORD;
  Buffer: Pointer; Size: Integer): Boolean;
// TODO 64 bit version
var
  BR: DWORD;
begin
  Result := ReadProcessMemory(ProcessHandle, Pointer(Address), Buffer, Size, BR);
end;

{$IFDEF KEEP_DEPRECATED}
function PeDbgImgNtHeaders(ProcessHandle: THandle; BaseAddress: Pointer;
  var NtHeaders: TImageNtHeaders32): Boolean;
begin
  Result := PeDbgImgNtHeaders32(ProcessHandle, TJclAddr32(BaseAddress), NtHeaders);
end;
{$ENDIF KEEP_DEPRECATED}

// TODO: 64 bit version
function PeDbgImgNtHeaders32(ProcessHandle: THandle; BaseAddress: TJclAddr32;
  var NtHeaders: TImageNtHeaders32): Boolean;
var
  DosHeader: TImageDosHeader;
begin
  Result := False;
  FillChar(NtHeaders, SizeOf(NtHeaders), 0);
  FillChar(DosHeader, SizeOf(DosHeader), 0);
  if not InternalReadProcMem(ProcessHandle, TJclAddr32(BaseAddress), @DosHeader, SizeOf(DosHeader)) then
    Exit;
  if DosHeader.e_magic <> IMAGE_DOS_SIGNATURE then
    Exit;
  Result := InternalReadProcMem(ProcessHandle, TJclAddr32(BaseAddress) + TJclAddr32(DosHeader._lfanew),
    @NtHeaders, SizeOf(TImageNtHeaders32));
end;

{$IFDEF KEEP_DEPRECATED}
function PeDbgImgLibraryName(ProcessHandle: THandle; BaseAddress: Pointer;
  var Name: string): Boolean;
begin
  Result := PeDbgImgLibraryName32(ProcessHandle, TJclAddr32(BaseAddress), Name);
end;
{$ENDIF KEEP_DEPRECATED}

// TODO: 64 bit version
function PeDbgImgLibraryName32(ProcessHandle: THandle; BaseAddress: TJclAddr32;
  var Name: string): Boolean;
var
  NtHeaders32: TImageNtHeaders32;
  DataDir: TImageDataDirectory;
  ExportDir: TImageExportDirectory;
begin
  Name := '';

  Result := PeDbgImgNtHeaders32(ProcessHandle, BaseAddress, NtHeaders32);
  if not Result then
    Exit;
  DataDir := NtHeaders32.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT];
  if DataDir.Size = 0 then
    Exit;
  if not InternalReadProcMem(ProcessHandle, TJclAddr(BaseAddress) + DataDir.VirtualAddress,
    @ExportDir, SizeOf(ExportDir)) then
    Exit;
  if ExportDir.Name = 0 then
    Exit;
  SetLength(Name, MAX_PATH);
  if InternalReadProcMem(ProcessHandle, TJclAddr(BaseAddress) + ExportDir.Name, PChar(Name), MAX_PATH) then
    StrResetLength(Name)
  else
    Name := '';
end;

// Borland BPL packages name unmangling

function PeBorUnmangleName(const Name: string; var Unmangled: string;
  var Description: TJclBorUmDescription; var BasePos: Integer): TJclBorUmResult;
var
  NameP, NameU, NameUFirst: PChar;
  QualifierFound, LinkProcFound: Boolean;

  procedure MarkQualifier;
  begin
    if not QualifierFound then
    begin
      QualifierFound := True;
      BasePos := NameU - NameUFirst + 2;
    end;
  end;

  procedure ReadSpecialSymbol;
  var
    SymbolLength: Integer;
  begin
    SymbolLength := 0;
    while NameP^ in AnsiDecDigits do
    begin
      SymbolLength := SymbolLength * 10 + Ord(NameP^) - 48;
      Inc(NameP);
    end;
    while (SymbolLength > 0) and (NameP^ <> #0) do
    begin
      if NameP^ = '@' then
      begin
        MarkQualifier;
        NameU^ := '.';
      end
      else
        NameU^ := NameP^;
      Inc(NameP);
      Inc(NameU);
      Dec(SymbolLength);
    end;
  end;

  procedure ReadRTTI;
  begin
    if StrLComp(NameP, '$xp$', 4) = 0 then
    begin
      Inc(NameP, 4);
      Description.Kind := skRTTI;
      QualifierFound := False;
      ReadSpecialSymbol;
      if QualifierFound then
        Include(Description.Modifiers, smQualified);
    end
    else
      Result := urError;
  end;

  procedure ReadNameSymbol;
  begin
    if NameP^ = '@' then
    begin
      LinkProcFound := True;
      Inc(NameP);
    end;
    while NameP^ in AnsiValidIdentifierLetters do
    begin
      NameU^ := NameP^;
      Inc(NameP);
      Inc(NameU);
    end;
  end;

  procedure ReadName;
  begin
    Description.Kind := skData;
    QualifierFound := False;
    LinkProcFound := False;
    repeat
      ReadNameSymbol;
      if LinkProcFound and not QualifierFound then
        LinkProcFound := False;
      case NameP^ of
        '@':
          case (NameP + 1)^ of
            #0:
              begin
                Description.Kind := skVTable;
                Break;
              end;
            '$':
              begin
                if (NameP + 2)^ = 'b' then
                begin
                  case (NameP + 3)^ of
                    'c':
                      Description.Kind := skConstructor;
                    'd':
                      Description.Kind := skDestructor;
                  end;
                  Inc(NameP, 6);
                end
                else
                  Description.Kind := skFunction;
                Break; // no parameters unmangling yet
              end;
          else
            MarkQualifier;
            NameU^ := '.';
            Inc(NameU);
            Inc(NameP);
          end;
        '$':
          begin
            Description.Kind := skFunction;
            Break; // no parameters unmangling yet
          end;
      else
        Break;
      end;
    until False;
    if QualifierFound then
      Include(Description.Modifiers, smQualified);
    if LinkProcFound then
      Include(Description.Modifiers, smLinkProc);
  end;

begin
  NameP := PChar(Name);
  Result := urError;
  case NameP^ of
    '@':
      Result := urOk;
    '?':
      Result := urMicrosoft;
    '_', 'A'..'Z', 'a'..'z':
      Result := urNotMangled;
  end;
  if Result <> urOk then
    Exit;
  Inc(NameP);
  SetLength(UnMangled, 1024);
  NameU := Pointer(UnMangled);
  NameUFirst := NameU;
  Description.Modifiers := [];
  BasePos := 1;
  case NameP^ of
    '$':
      ReadRTTI;
    '_', 'A'..'Z', 'a'..'z':
      ReadName;
  else
    Result := urError;
  end;
  NameU^ := #0;
  StrResetLength(Unmangled);
end;

function PeBorUnmangleName(const Name: string; var Unmangled: string;
  var Description: TJclBorUmDescription): TJclBorUmResult;
var
  BasePos: Integer;
begin
  Result := PeBorUnmangleName(Name, Unmangled, Description, BasePos);
end;

function PeBorUnmangleName(const Name: string; var Unmangled: string): TJclBorUmResult;
var
  Description: TJclBorUmDescription;
  BasePos: Integer;
begin
  Result := PeBorUnmangleName(Name, Unmangled, Description, BasePos);
end;

function PeBorUnmangleName(const Name: string): string;
var
  Unmangled: string;
  Description: TJclBorUmDescription;
  BasePos: Integer;
begin
  if PeBorUnmangleName(Name, Unmangled, Description, BasePos) = urOk then
    Result := Unmangled
  else
    Result := '';
end;

function PeIsNameMangled(const Name: string): TJclPeUmResult;
begin
  Result := umNotMangled;
  if Length(Name) > 0 then
    case Name[1] of
      '@':
        Result := umBorland;
      '?':
        Result := umMicrosoft;
    end;
end;

function PeUnmangleName(const Name: string; var Unmangled: string): TJclPeUmResult;
var
  Res: DWORD;
begin
  Result := umNotMangled;
  case PeBorUnmangleName(Name, Unmangled) of
    urOk:
      Result := umBorland;
    urMicrosoft:
      begin
        SetLength(Unmangled, 2048);
        Res := UnDecorateSymbolName(PChar(Name), PChar(Unmangled), 2048, UNDNAME_NAME_ONLY);
        if Res > 0 then
        begin
          StrResetLength(Unmangled);
          Result := umMicrosoft;
        end
        else
          Unmangled := '';
      end;
  end;
  if Result = umNotMangled then
    Unmangled := Name;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
