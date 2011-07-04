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
{ The Original Code is JclMetadata.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Flier Lu (<flier_lu att yahoo dott com dott cn>).  }
{ Portions created by Flier Lu are Copyright (C) Flier Lu. All Rights Reserved.                    }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Flier Lu (flier)                                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Olivier Sannier (obones)                                                                       }
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

unit JclMetadata;

{$I jcl.inc}

interface

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
  JclBase, JclClr, JclFileUtils, JclPeImage, JclSysUtils;

type
  TJclClrElementType = (etEnd, etVoid, etBoolean, etChar,
    etI1, etU1, etI2, etU2, etI4, etU4, etI8, etU8, etR4, etR8, etString,
    etPtr, etByRef, etValueType, etClass, etArray, etTypedByRef,
    etI, etU, etFnPtr, etObject, etSzArray, etCModReqd, etCModOpt,
    etInternal, etMax, etModifier, etSentinel, etPinned);

  TJclClrTableModuleRow = class(TJclClrTableRow)
  private
    FGeneration: Word;
    FNameOffset: DWORD;
    FMvidIdx: DWORD;
    FEncIdIdx: DWORD;
    FEncBaseIdIdx: DWORD;
    function GetMvid: TGUID;
    function GetName: WideString;
    function GetEncBaseId: TGUID;
    function GetEncId: TGUID;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    function DumpIL: string; override;

    function HasEncId: Boolean;
    function HasEncBaseId: Boolean;

    property Generation: Word read FGeneration;
    property NameOffset: DWORD read FNameOffset;
    property MvidIdx: DWORD read FMvidIdx;
    property EncIdIdx: DWORD read FEncIdIdx;
    property EncBaseIdIdx: DWORD read FEncBaseIdIdx;

    property Name: WideString read GetName;
    property Mvid: TGUID read GetMvid;
    property EncId: TGUID read GetEncId;
    property EncBaseId: TGUID read GetEncBaseId;
  end;

  TJclClrTableModule = class(TJclClrTable, ITableCanDumpIL)
  private
    function GetRow(const Idx: Integer): TJclClrTableModuleRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableModuleRow read GetRow; default;
  end;

  TJclClrTableModuleRefRow = class(TJclClrTableRow)
  private
    FNameOffset: DWORD;
    function GetName: WideString;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    function DumpIL: string; override;
    property NameOffset: DWORD read FNameOffset;
    property Name: WideString read GetName;
  end;

  TJclClrTableModuleRef = class(TJclClrTable, ITableCanDumpIL)
  private
    function GetRow(const Idx: Integer): TJclClrTableModuleRefRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableModuleRefRow read GetRow; default;
  end;

  TJclClrAssemblyFlag =
    (cafPublicKey, cafCompatibilityMask, cafSideBySideCompatible,
     cafNonSideBySideAppDomain, cafNonSideBySideProcess,
     cafNonSideBySideMachine, cafEnableJITcompileTracking,
     cafDisableJITcompileOptimizer);
  TJclClrAssemblyFlags = set of TJclClrAssemblyFlag;

  TJclClrTableAssemblyRow = class(TJclClrTableRow)
  private
    FCultureOffset: DWORD;
    FPublicKeyOffset: DWORD;
    FHashAlgId: DWORD;
    FNameOffset: DWORD;
    FMajorVersion: Word;
    FBuildNumber: Word;
    FRevisionNumber: Word;
    FMinorVersion: Word;
    FFlagMask: DWORD;
    function GetCulture: WideString;
    function GetName: WideString;
    function GetPublicKey: TJclClrBlobRecord;
    function GetVersion: string;
    function GetFlags: TJclClrAssemblyFlags;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    function DumpIL: string; override;

    class function AssemblyFlags(const Flags: TJclClrAssemblyFlags): DWORD; overload;
    class function AssemblyFlags(const Flags: DWORD): TJclClrAssemblyFlags; overload;

    property HashAlgId: DWORD read FHashAlgId;
    property MajorVersion: Word read FMajorVersion;
    property MinorVersion: Word read FMinorVersion;
    property BuildNumber: Word read FBuildNumber;
    property RevisionNumber: Word read FRevisionNumber;
    property FlagMask: DWORD read FFlagMask;
    property PublicKeyOffset: DWORD read FPublicKeyOffset;
    property NameOffset: DWORD read FNameOffset;
    property CultureOffset: DWORD read FCultureOffset;

    property PublicKey: TJclClrBlobRecord read GetPublicKey;
    property Name: WideString read GetName;
    property Culture: WideString read GetCulture;
    property Version: string read GetVersion;
    property Flags: TJclClrAssemblyFlags read GetFlags;
  end;

  TJclClrTableAssembly = class(TJclClrTable, ITableCanDumpIL)
  private
    function GetRow(const Idx: Integer): TJclClrTableAssemblyRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableAssemblyRow read GetRow; default;
  end;

  TJclClrTableAssemblyOSRow = class(TJclClrTableRow)
  private
    FPlatformID: DWORD;
    FMajorVersion: DWORD;
    FMinorVersion: DWORD;
    function GetVersion: string;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property PlatformID: DWORD read FPlatformID;
    property MajorVersion: DWORD read FMajorVersion;
    property MinorVersion: DWORD read FMinorVersion;
    property Version: string read GetVersion;
  end;

  TJclClrTableAssemblyOS = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableAssemblyOSRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableAssemblyOSRow read GetRow; default;
  end;

  TJclClrTableAssemblyProcessorRow = class(TJclClrTableRow)
  private
    FProcessor: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property Processor: DWORD read FProcessor;
  end;

  TJclClrTableAssemblyProcessor = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableAssemblyProcessorRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableAssemblyProcessorRow read GetRow; default;
  end;

  TJclClrTableAssemblyRefRow = class(TJclClrTableRow)
  private
    FCultureOffset: DWORD;
    FNameOffset: DWORD;
    FPublicKeyOrTokenOffset: DWORD;
    FHashValueOffset: DWORD;
    FMajorVersion: Word;
    FRevisionNumber: Word;
    FBuildNumber: Word;
    FMinorVersion: Word;
    FFlagMask: DWORD;
    function GetCulture: WideString;
    function GetHashValue: TJclClrBlobRecord;
    function GetName: WideString;
    function GetPublicKeyOrToken: TJclClrBlobRecord;
    function GetVersion: string;
    function GetFlags: TJclClrAssemblyFlags;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    function DumpIL: string; override;

    property MajorVersion: Word read FMajorVersion;
    property MinorVersion: Word read FMinorVersion;
    property BuildNumber: Word read FBuildNumber;
    property RevisionNumber: Word read FRevisionNumber;
    property FlagMask: DWORD read FFlagMask;
    property PublicKeyOrTokenOffset: DWORD read FPublicKeyOrTokenOffset;
    property NameOffset: DWORD read FNameOffset;
    property CultureOffset: DWORD read FCultureOffset;
    property HashValueOffset: DWORD read FHashValueOffset;

    property PublicKeyOrToken: TJclClrBlobRecord read GetPublicKeyOrToken;
    property Name: WideString read GetName;
    property Culture: WideString read GetCulture;
    property Version: string read GetVersion;
    property HashValue: TJclClrBlobRecord read GetHashValue;
    property Flags: TJclClrAssemblyFlags read GetFlags;
  end;

  TJclClrTableAssemblyRef = class(TJclClrTable, ITableCanDumpIL)
  private
    function GetRow(const Idx: Integer): TJclClrTableAssemblyRefRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableAssemblyRefRow read GetRow; default;
  end;

  TJclClrTableAssemblyRefOSRow = class(TJclClrTableAssemblyOSRow)
  private
    FAssemblyRefIdx: DWORD;
    function GetAssemblyRef: TJclClrTableAssemblyRefRow;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property AssemblyRefIdx: DWORD read FAssemblyRefIdx;
    property AssemblyRef: TJclClrTableAssemblyRefRow read GetAssemblyRef;
  end;

  TJclClrTableAssemblyRefOS = class(TJclClrTableAssemblyOS)
  private
    function GetRow(const Idx: Integer): TJclClrTableAssemblyRefOSRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableAssemblyRefOSRow read GetRow; default;
  end;

  TJclClrTableAssemblyRefProcessorRow = class(TJclClrTableAssemblyProcessorRow)
  private
    FAssemblyRefIdx: DWORD;
    function GetAssemblyRef: TJclClrTableAssemblyRefRow;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property AssemblyRefIdx: DWORD read FAssemblyRefIdx;
    property AssemblyRef: TJclClrTableAssemblyRefRow read GetAssemblyRef;
  end;

  TJclClrTableAssemblyRefProcessor = class(TJclClrTableAssemblyProcessor)
  private
    function GetRow(const Idx: Integer): TJclClrTableAssemblyRefProcessorRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableAssemblyRefProcessorRow read GetRow; default;
  end;

  TJclClrTableClassLayoutRow = class(TJclClrTableRow)
  private
    FClassSize: DWORD;
    FParentIdx: DWORD;
    FPackingSize: Word;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property PackingSize: Word read FPackingSize;
    property ClassSize: DWORD read FClassSize;
    property ParentIdx: DWORD read FParentIdx;
  end;

  TJclClrTableClassLayout = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableClassLayoutRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableClassLayoutRow read GetRow; default;
  end;

  TJclClrTableConstantRow = class(TJclClrTableRow)
  private
    FKind: Byte;
    FParentIdx: DWORD;
    FValueOffset: DWORD;
    function GetElementType: TJclClrElementType;
    function GetParent: TJclClrTableRow;
    function GetValue: TJclClrBlobRecord;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    function DumpIL: string; override;

    property Kind: Byte read FKind;
    property ParentIdx: DWORD read FParentIdx;
    property ValueOffset: DWORD read FValueOffset;

    property ElementType: TJclClrElementType read GetElementType;
    property Parent: TJclClrTableRow read GetParent;
    property Value: TJclClrBlobRecord read GetValue;
  end;

  TJclClrTableConstant = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableConstantRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableConstantRow read GetRow; default;
  end;

  TJclClrTableCustomAttributeRow = class(TJclClrTableRow)
  private
    FParentIdx: DWORD;
    FTypeIdx: DWORD;
    FValueOffset: DWORD;
    function GetValue: TJclClrBlobRecord;
    function GetParent: TJclClrTableRow;
    function GetMethod: TJclClrTableRow;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    function DumpIL: string; override;

    property ParentIdx: DWORD read FParentIdx;
    property TypeIdx: DWORD read FTypeIdx;
    property ValueOffset: DWORD read FValueOffset;

    property Parent: TJclClrTableRow read GetParent;
    property Method: TJclClrTableRow read GetMethod;
    property Value: TJclClrBlobRecord read GetValue;
  end;

  TJclClrTableCustomAttribute = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableCustomAttributeRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableCustomAttributeRow read GetRow; default;
  end;

  TJclClrTableDeclSecurityRow = class(TJclClrTableRow)
  private
    FPermissionSetOffset: DWORD;
    FParentIdx: DWORD;
    FAction: Word;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property Action: Word read FAction;
    property ParentIdx: DWORD read FParentIdx;
    property PermissionSetOffset: DWORD read FPermissionSetOffset;  
  end;

  TJclClrTableDeclSecurity = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableDeclSecurityRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableDeclSecurityRow read GetRow; default;
  end;

  TJclClrTableEventMapRow = class(TJclClrTableRow)
  private
    FEventListIdx: DWORD;
    FParentIdx: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property ParentIdx: DWORD read FParentIdx;
    property EventListIdx: DWORD read FEventListIdx;
  end;

  TJclClrTableEventMap = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableEventMapRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableEventMapRow read GetRow; default;
  end;

  TJclClrTableEventFlag = (efSpecialName, efRTSpecialName);
  TJclClrTableEventFlags = set of TJclClrTableEventFlag;

  TJclClrTableEventDefRow = class(TJclClrTableRow)
  private
    FNameOffset: DWORD;
    FEventTypeIdx: DWORD;
    FEventFlags: Word;
    function GetName: WideString;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property EventFlags: Word read FEventFlags;
    property NameOffset: DWORD read FNameOffset;
    property EventTypeIdx: DWORD read FEventTypeIdx;
    property Name: WideString read GetName;
  end;

  TJclClrTableEventDef = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableEventDefRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableEventDefRow read GetRow; default;
  end;

  TJclClrTableExportedTypeRow = class(TJclClrTableRow)
  private
    FTypeDefIdx: DWORD;
    FFlags: DWORD;
    FImplementationIdx: DWORD;
    FTypeNamespaceOffset: DWORD;
    FTypeNameOffset: DWORD;
    function GetTypeName: WideString;
    function GetTypeNamespace: WideString;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property Flags: DWORD read FFlags;
    property TypeDefIdx: DWORD read FTypeDefIdx;
    property TypeNameOffset: DWORD read FTypeNameOffset;
    property TypeNamespaceOffset: DWORD read FTypeNamespaceOffset;
    property ImplementationIdx: DWORD read FImplementationIdx;
    property TypeName: WideString read GetTypeName;
    property TypeNamespace: WideString read GetTypeNamespace;
  end;

  TJclClrTableEventPtrRow = class(TJclClrTableRow)
  private
    FEventIdx: DWORD;
    function GetEvent: TJclClrTableEventDefRow;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property EventIdx: DWORD read FEventIdx;
    property Event: TJclClrTableEventDefRow read GetEvent;
  end;

  TJclClrTableEventPtr = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableEventPtrRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableEventPtrRow read GetRow; default;
  end;

  TJclClrTableExportedType = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableExportedTypeRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableExportedTypeRow read GetRow; default;
  end;

  TJclClrTableTypeDefRow = class;

  TJclClrTableFieldDefVisibility =
   (fvPrivateScope, fvPrivate, fvFamANDAssem,
    fvAssembly, fvFamily, fvFamORAssem, fvPublic);

  TJclClrTableFieldDefFlag =
   (ffStatic, ffInitOnly, ffLiteral, ffNotSerialized,
    ffSpecialName, ffPinvokeImpl, ffRTSpecialName,
    ffHasFieldMarshal, ffHasDefault, ffHasFieldRVA);
  TJclClrTableFieldDefFlags = set of TJclClrTableFieldDefFlag;

  TJclClrTableFieldDefRow = class(TJclClrTableRow)
  private
    FFlags: Word;
    FNameOffset: DWORD;
    FSignatureOffset: DWORD;
    FParentToken: TJclClrTableTypeDefRow;
    function GetName: WideString;
    function GetSignature: TJclClrBlobRecord;
    function GetFlag: TJclClrTableFieldDefFlags;
    function GetVisibility: TJclClrTableFieldDefVisibility;
  protected
    constructor Create(const ATable: TJclClrTable); override;
    procedure SetParentToken(const ARow: TJclClrTableTypeDefRow);
  public
    function DumpIL: string; override;

    property RawFlags: Word read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property SignatureOffset: DWORD read FSignatureOffset;

    property Name: WideString read GetName;
    property Signature: TJclClrBlobRecord read GetSignature;

    property ParentToken: TJclClrTableTypeDefRow read FParentToken;
    property Visibility: TJclClrTableFieldDefVisibility read GetVisibility;
    property Flags: TJclClrTableFieldDefFlags read GetFlag;
  end;

  TJclClrTableFieldDef = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableFieldDefRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableFieldDefRow read GetRow; default;
  end;

  TJclClrTableFieldPtrRow = class(TJclClrTableRow)
  private
    FFieldIdx: DWORD;
    function GetField: TJclClrTableFieldDefRow;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property FieldIdx: DWORD read FFieldIdx;
    property Field: TJclClrTableFieldDefRow read GetField;
  end;

  TJclClrTableFieldPtr = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableFieldPtrRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableFieldPtrRow read GetRow; default;
  end;

  TJclClrTableFieldLayoutRow = class(TJclClrTableRow)
  private
    FOffset: DWORD;
    FFieldIdx: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property Offset: DWORD read FOffset;
    property FieldIdx: DWORD read FFieldIdx;
  end;

  TJclClrTableFieldLayout = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableFieldLayoutRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableFieldLayoutRow read GetRow; default;
  end;

  TJclClrTableFieldMarshalRow = class(TJclClrTableRow)
  private
    FParentIdx: DWORD;
    FNativeTypeOffset: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property ParentIdx: DWORD read FParentIdx;
    property NativeTypeOffset: DWORD read FNativeTypeOffset;
  end;

  TJclClrTableFieldMarshal = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableFieldMarshalRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableFieldMarshalRow read GetRow; default;
  end;

  TJclClrTableFieldRVARow = class(TJclClrTableRow)
  private
    FRVA: DWORD;
    FFieldIdx: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property RVA: DWORD read FRVA;
    property FieldIdx: DWORD read FFieldIdx;
  end;

  TJclClrTableFieldRVA = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableFieldRVARow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableFieldRVARow read GetRow; default;
  end;

  TJclClrTableFileRow = class(TJclClrTableRow)
  private
    FHashValueOffset: DWORD;
    FNameOffset: DWORD;
    FFlags: DWORD;
    function GetName: WideString;
    function GetHashValue: TJclClrBlobRecord;
    function GetContainsMetadata: Boolean;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    function DumpIL: string; override;

    property Flags: DWORD read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property HashValueOffset: DWORD read FHashValueOffset;

    property Name: WideString read GetName;
    property HashValue: TJclClrBlobRecord read GetHashValue;
    property ContainsMetadata: Boolean read GetContainsMetadata;
  end;

  TJclClrTableFile = class(TJclClrTable, ITableCanDumpIL)
  private
    function GetRow(const Idx: Integer): TJclClrTableFileRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableFileRow read GetRow; default;
  end;

  TJclClrTableImplMapRow = class(TJclClrTableRow)
  private
    FImportNameOffset: DWORD;
    FMemberForwardedIdx: DWORD;
    FImportScopeIdx: DWORD;
    FMappingFlags: Word;
    function GetImportName: WideString;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property MappingFlags: Word read FMappingFlags;
    property MemberForwardedIdx: DWORD read FMemberForwardedIdx;
    property ImportNameOffset: DWORD read FImportNameOffset;
    property ImportScopeIdx: DWORD read FImportScopeIdx;
    property ImportName: WideString read GetImportName;
  end;

  TJclClrTableImplMap = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableImplMapRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableImplMapRow read GetRow; default;
  end;

  TJclClrTableInterfaceImplRow = class(TJclClrTableRow)
  private
    FInterfaceIdx: DWORD;
    FClassIdx: DWORD;
    function GetImplClass: TJclClrTableRow;
    function GetImplInterface: TJclClrTableRow;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    function DumpIL: string; override;

    property ClassIdx: DWORD read FClassIdx;
    property InterfaceIdx: DWORD read FInterfaceIdx;

    property ImplClass: TJclClrTableRow read GetImplClass;
    property ImplInterface: TJclClrTableRow read GetImplInterface;
  end;

  TJclClrTableInterfaceImpl = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableInterfaceImplRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableInterfaceImplRow read GetRow; default;
  end;

  TJclClrTableManifestResourceVisibility = (rvPublic, rvPrivate);

  TJclClrTableManifestResourceRow = class(TJclClrTableRow)
  private
    FOffset: DWORD;
    FFlags: DWORD;
    FImplementationIdx: DWORD;
    FNameOffset: DWORD;
    function GetName: WideString;
    function GetVisibility: TJclClrTableManifestResourceVisibility;
    function GetImplementationRow: TJclClrTableRow;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    function DumpIL: string; override;

    property Offset: DWORD read FOffset;
    property Flags: DWORD read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property ImplementationIdx: DWORD read FImplementationIdx;

    property Name: WideString read GetName;
    property Visibility: TJclClrTableManifestResourceVisibility read GetVisibility;
    property ImplementationRow: TJclClrTableRow read GetImplementationRow;
  end;

  TJclClrTableManifestResource = class(TJclClrTable, ITableCanDumpIL)
  private
    function GetRow(const Idx: Integer): TJclClrTableManifestResourceRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableManifestResourceRow read GetRow; default;
  end;

  TJclClrTableMemberRefRow = class(TJclClrTableRow)
  private
    FClassIdx: DWORD;
    FNameOffset: DWORD;
    FSignatureOffset: DWORD;
    function GetName: WideString;
    function GetSignature: TJclClrBlobRecord;
    function GetParentClass: TJclClrTableRow;
    function GetFullName: WideString;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property ClassIdx: DWORD read FClassIdx;
    property NameOffset: DWORD read FNameOffset;
    property SignatureOffset: DWORD read FSignatureOffset;

    property Name: WideString read GetName;
    property FullName: WideString read GetFullName;
    property Signature: TJclClrBlobRecord read GetSignature;
    property ParentClass: TJclClrTableRow read GetParentClass;
  end;

  TJclClrTableMemberRef = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableMemberRefRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableMemberRefRow read GetRow; default;
  end;

  TJclClrTableMethodDefRow = class;

  TJclClrParamKind = (pkIn, pkOut, pkOptional, pkHasDefault, pkHasFieldMarshal);
  TJclClrParamKinds = set of TJclClrParamKind;

  TJclClrTableParamDefRow = class(TJclClrTableRow)
  private
    FFlagMask: Word;
    FSequence: Word;
    FNameOffset: DWORD;
    FMethod: TJclClrTableMethodDefRow;
    FFlags: TJclClrParamKinds;
    function GetName: WideString;
  protected
    constructor Create(const ATable: TJclClrTable); override;
    procedure SetMethod(const AMethod: TJclClrTableMethodDefRow);
  public
    function DumpIL: string; override;

    class function ParamFlags(const AFlags: TJclClrParamKinds): Word; overload;
    class function ParamFlags(const AFlags: Word): TJclClrParamKinds; overload;

    property FlagMask: Word read FFlagMask;
    property Sequence: Word read FSequence;
    property NameOffset: DWORD read FNameOffset;

    property Name: WideString read GetName;
    property Method: TJclClrTableMethodDefRow read FMethod;
    property Flags: TJclClrParamKinds read FFlags;
  end;

  TJclClrTableParamDef = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableParamDefRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableParamDefRow read GetRow; default;
  end;

  TJclClrTableParamPtrRow = class(TJclClrTableRow)
  private
    FParamIdx: DWORD;
    function GetParam: TJclClrTableParamDefRow;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property ParamIdx: DWORD read FParamIdx;
    property Param: TJclClrTableParamDefRow read GetParam;
  end;

  TJclClrTableParamPtr = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableParamPtrRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableParamPtrRow read GetRow; default;
  end;

  IMAGE_COR_ILMETHOD_TINY = packed record
    Flags_CodeSize: Byte;
  end;
  TImageCorILMethodTiny = IMAGE_COR_ILMETHOD_TINY;
  PImageCorILMethodTiny = ^TImageCorILMethodTiny;

  IMAGE_COR_ILMETHOD_FAT = packed record
    Flags_Size,
    MaxStack: Word;
    CodeSize: DWORD;
    LocalVarSigTok: TJclClrToken;
  end;
  TImageCorILMethodFat = IMAGE_COR_ILMETHOD_FAT;
  PImageCorILMethodFat = ^TImageCorILMethodFat;

  PImageCorILMethodHeader = ^TImageCorILMethodHeader;
  TImageCorILMethodHeader = packed record
  case Boolean of
    True:
      (Tiny: TImageCorILMethodTiny);
    False:
      (Fat: TImageCorILMethodFat);
  end;

  IMAGE_COR_ILMETHOD_SECT_SMALL = packed record
    Kind: Byte;
    Datasize: Byte;
    Padding: Word;
  end;
  TImageCorILMethodSectSmall = IMAGE_COR_ILMETHOD_SECT_SMALL;
  PImageCorILMethodSectSmall = ^TImageCorILMethodSectSmall;

  IMAGE_COR_ILMETHOD_SECT_FAT = packed record
    Kind_DataSize: DWORD;
  end;
  TImageCorILMethodSectFat = IMAGE_COR_ILMETHOD_SECT_FAT;
  PImageCorILMethodSectFat = ^TImageCorILMethodSectFat;

  PImageCorILMethodSectHeader = ^TImageCorILMethodSectHeader;
  TImageCorILMethodSectHeader = packed record
  case Boolean of
    True:
      (Small: TImageCorILMethodSectSmall);
    False:
      (Fat: TImageCorILMethodSectFat);
  end;

  IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT = packed record
    Flags: DWORD;
    TryOffset: DWORD;
    TryLength: DWORD;      // relative to start of try block
    HandlerOffset: DWORD;
    HandlerLength: DWORD;  // relative to start of handler
    case Boolean of
      True:
        (ClassToken: DWORD);   // use for type-based exception handlers
      False:
        (FilterOffset: DWORD); // use for filter-based exception handlers (COR_ILEXCEPTION_FILTER is set)
  end;
  TImageCorILMethodSectEHClauseFat = IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT;
  PImageCorILMethodSectEHClauseFat = ^TImageCorILMethodSectEHClauseFat;

  IMAGE_COR_ILMETHOD_SECT_EH_FAT = packed record
    SectFat: IMAGE_COR_ILMETHOD_SECT_FAT;
    Clauses: array [0..MaxWord-1] of IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_FAT; // actually variable size
  end;
  TImageCorILMethodSectEHFat = IMAGE_COR_ILMETHOD_SECT_EH_FAT;
  PImageCorILMethodSectEHFat = ^TImageCorILMethodSectEHFat;

  IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_SMALL = packed record
    Flags,
    TryOffset: Word;
    TryLength: Byte;     // relative to start of try block
    HandlerOffset: Word;
    HandlerLength: Byte; // relative to start of handler
    case Boolean of
      True:
        (ClassToken: DWORD);   // use for type-based exception handlers
      False:
        (FilterOffset: DWORD); // use for filter-based exception handlers (COR_ILEXCEPTION_FILTER is set)
  end;
  TImageCorILMethodSectEHClauseSmall = IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_SMALL;
  PImageCorILMethodSectEHClauseSmall = ^TImageCorILMethodSectEHClauseSmall;

  IMAGE_COR_ILMETHOD_SECT_EH_SMALL = packed record
    SectSmall: IMAGE_COR_ILMETHOD_SECT_SMALL;
    Clauses: array [0..MaxWord-1] of IMAGE_COR_ILMETHOD_SECT_EH_CLAUSE_SMALL; // actually variable size
  end;
  TImageCorILMethodSectEHSmall = IMAGE_COR_ILMETHOD_SECT_EH_SMALL;
  PImageCorILMethodSectEHSmall = ^TImageCorILMethodSectEHSmall;

  IMAGE_COR_ILMETHOD_SECT_EH = packed record
  case Boolean of
    True:
      (Small: IMAGE_COR_ILMETHOD_SECT_EH_SMALL);
    False:
      (Fat: IMAGE_COR_ILMETHOD_SECT_EH_FAT);
  end;
  TImageCorILMethodSectEH = IMAGE_COR_ILMETHOD_SECT_EH;
  PImageCorILMethodSectEH = ^TImageCorILMethodSectEH;

  TJclClrCodeBlock = record
    Offset: DWORD;
    Length: DWORD;
  end;

  TJclClrExceptionClauseFlag = (cfException, cfFilter, cfFinally, cfFault);
  TJclClrExceptionClauseFlags = set of TJclClrExceptionClauseFlag;

  TJclClrExceptionHandler = class(TObject)
  private
    FFlags: DWORD;
    FFilterOffset: DWORD;
    FTryBlock: TJclClrCodeBlock;
    FHandlerBlock: TJclClrCodeBlock;
    FClassToken: TJclClrToken;
    function GetFlags: TJclClrExceptionClauseFlags;
  public
    constructor Create(const EHClause: TImageCorILMethodSectEHClauseSmall); overload;
    constructor Create(const EHClause: TImageCorILMethodSectEHClauseFat); overload;

    property EHFlags: DWORD read FFlags;
    property Flags: TJclClrExceptionClauseFlags read GetFlags;

    property TryBlock: TJclClrCodeBlock read FTryBlock;
    property HandlerBlock: TJclClrCodeBlock read FHandlerBlock;

    property ClassToken: TJclClrToken read FClassToken;
    property FilterOffset: DWORD read FFilterOffset;
  end;

  TJclClrSignature = class(TObject)
  private
    FBlob: TJclClrBlobRecord;
  protected
    function IsModifierType(const AElementType: TJclClrElementType): Boolean;
    function IsPrimitiveType(const AElementType: TJclClrElementType): Boolean;

    function Inc(var DataPtr: PJclByteArray; Step: Integer = 1): PByte;

    function UncompressedDataSize(DataPtr: PJclByteArray): Integer;
    function UncompressData(DataPtr: PJclByteArray; var Value: DWord): Integer;
    function UncompressToken(DataPtr: PJclByteArray; var Token: TJclClrToken): Integer;
    function UncompressCallingConv(DataPtr: PJclByteArray): Byte;
    function UncompressSignedInt(DataPtr: PJclByteArray; var Value: Integer): Integer;
    function UncompressElementType(DataPtr: PJclByteArray): TJclClrElementType;
    function UncompressTypeSignature(DataPtr: PJclByteArray): string;
  public
    constructor Create(const ABlob: TJclClrBlobRecord);

    function UncompressFieldSignature: string;

    function ReadValue: DWORD;
    function ReadByte: Byte;
    function ReadInteger: Integer;
    function ReadToken: TJclClrToken;
    function ReadElementType: TJclClrElementType;

    property Blob: TJclClrBlobRecord read FBlob;
  end;

  TJclClrArrayData = (adSize, adLowBound);

  TJclClrArraySignBound = array [TJclClrArrayData] of Integer;
  TJclClrArraySignBounds = array of TJclClrArraySignBound;

  TJclClrArraySign = class(TJclClrSignature)
  private
    FBounds: TJclClrArraySignBounds;
  public
    constructor Create(const ABlob: TJclClrBlobRecord);
  end;

  TJclClrLocalVarFlag = (lvfPinned, lvfByRef);
  TJclClrLocalVarFlags = set of TJclClrLocalVarFlag;

  TJclClrLocalVar = class(TObject)
  private
    FElementType: TJclClrElementType;
    FFlags: TJclClrLocalVarFlags;
    FToken: TJclClrToken;
    function GetName: WideString;
  public
    property ElementType: TJclClrElementType read FElementType write FElementType;

    property Name: WideString read GetName;
    property Flags: TJclClrLocalVarFlags read FFlags write FFlags;
    property Token: TJclClrToken read FToken write FToken;
  end;

  TJclClrLocalVarSign = class(TJclClrSignature)
  private
    FLocalVars: TObjectList;
    function GetLocalVar(const Idx: Integer): TJclClrLocalVar;
    function GetLocalVarCount: Integer;
  public
    constructor Create(const ABlob: TJclClrBlobRecord);
    destructor Destroy; override;

    property LocalVars[const Idx: Integer]: TJclClrLocalVar read GetLocalVar;
    property LocalVarCount: Integer read GetLocalVarCount;
  end;

  TJclClrMethodBody = class(TObject)
  private
    FMethod: TJclClrTableMethodDefRow;
    FSize: DWORD;
    FCode: Pointer;
    FMaxStack: DWORD;
    FLocalVarSignToken: TJclClrToken;
    FLocalVarSign: TJclClrLocalVarSign;
    FEHTable: TObjectList;
    procedure AddEHTable(EHTable: PImageCorILMethodSectEH);
    procedure AddOptILTable(OptILTable: Pointer; Size: Integer);

    procedure ParseMoreSections(SectHeader: PImageCorILMethodSectHeader);

    function GetExceptionHandler(const Idx: Integer): TJclClrExceptionHandler;
    function GetExceptionHandlerCount: Integer;
    function GetLocalVarSign: TJclClrLocalVarSign;
    function GetLocalVarSignData: TJclClrBlobRecord;
  public
    constructor Create(const AMethod: TJclClrTableMethodDefRow);
    destructor Destroy; override;

    property Method: TJclClrTableMethodDefRow read FMethod;

    property Size: DWORD read FSize;
    property Code: Pointer read FCode;

    property MaxStack: DWORD read FMaxStack;
    property LocalVarSignToken: TJclClrToken read FLocalVarSignToken;
    property LocalVarSignData: TJclClrBlobRecord read GetLocalVarSignData;
    property LocalVarSign: TJclClrLocalVarSign read GetLocalVarSign;
    property ExceptionHandlers[const Idx: Integer]: TJclClrExceptionHandler read GetExceptionHandler;
    property ExceptionHandlerCount: Integer read GetExceptionHandlerCount;
  end;

  TJclClrCustomModifierSign = class(TJclClrSignature)
  private
    FRequired: Boolean;
    FToken: TJclClrToken;
  public
    constructor Create(const ABlob: TJclClrBlobRecord);
    property Required: Boolean read FRequired;
    property Token: TJclClrToken read FToken;
  end;

  TJclClrMethodSign = class;

  TJclClrMethodParam = class(TJclClrSignature)
  private
    FCustomMods: TObjectList;
    FByRef: Boolean;
    FElementType: TJclClrElementType;
    FToken: TJclClrToken;
    FMethodSign: TJclClrMethodSign;
    FArraySign: TJclClrArraySign;
    function GetCustomModifier(const Idx: Integer): TJclClrCustomModifierSign;
    function GetCustomModifierCount: Integer;
  public
    constructor Create(const ABlob: TJclClrBlobRecord);
    destructor Destroy; override;

    property CustomModifiers[const Idx: Integer]: TJclClrCustomModifierSign read GetCustomModifier;
    property CustomModifierCount: Integer read GetCustomModifierCount;

    property ElementType: TJclClrElementType read FElementType;
    property ByRef: Boolean read FByRef;
    property Token: TJclClrToken read FToken;
    property MethodSign: TJclClrMethodSign read FMethodSign;
    property ArraySign: TJclClrArraySign read FArraySign;
  end;

  TJclClrMethodRetType = class(TJclClrMethodParam)
  end;

  TJclClrMethodSignFlag = (mfHasThis, mfExplicitThis, mfDefault, mfVarArg);
  TJclClrMethodSignFlags = set of TJclClrMethodSignFlag;

  TJclClrMethodSign = class(TJclClrSignature)
  private
    FFlags: TJclClrMethodSignFlags;
    FParams: TObjectList;
    FRetType: TJclClrMethodRetType;
    function GetParam(const Idx: Integer): TJclClrMethodParam;
    function GetParamCount: Integer;
  public
    constructor Create(const ABlob: TJclClrBlobRecord);
    destructor Destroy; override;

    property Flags: TJclClrMethodSignFlags read FFlags;
    property Params[const Idx: Integer]: TJclClrMethodParam read GetParam;
    property ParamCount: Integer read GetParamCount;
    property RetType: TJclClrMethodRetType read FRetType;
  end;

  TJclClrMemberAccess =
   (maCompilercontrolled, maPrivate, maFamilyAndAssembly,
    maAssembly, maFamily, maFamilyOrAssembly, maPublic);

  TJclClrMethodFlag =
   (mfStatic, mfFinal, mfVirtual, mfHideBySig,
    mfCheckAccessOnOverride, mfAbstract, mfSpecialName,
    mfPInvokeImpl, mfUnmanagedExport,
    mfRTSpcialName, mfHasSecurity, mfRequireSecObject);
  TJclClrMethodFlags = set of TJclClrMethodFlag;

  TJclClrMethodCodeType = (ctIL, ctNative, ctOptIL, ctRuntime);

  TJclClrMethodImplFlag =
   (mifForwardRef, mifPreserveSig, mifInternalCall,
    mifSynchronized, mifNoInlining);
  TJclClrMethodImplFlags = set of TJclClrMethodImplFlag;

  TJclClrTableMethodDefRow = class(TJclClrTableRow)
  private
    FRVA: DWORD;
    FImplFlags: Word;
    FFlags: Word;
    FNameOffset: DWORD;
    FSignatureOffset: DWORD;
    FParamListIdx: DWORD;
    FParentToken: TJclClrTableTypeDefRow;
    FParams: TList;
    FMethodBody: TJclClrMethodBody;
    FSignature: TJclClrMethodSign;
    function GetName: WideString;
    function GetSignatureData: TJclClrBlobRecord;
    function GetParam(const Idx: Integer): TJclClrTableParamDefRow;
    function GetParamCount: Integer;
    function GetHasParam: Boolean;
    procedure UpdateParams;
    function GetFullName: WideString;
    function GetSignature: TJclClrMethodSign;
    function GetMemberAccess: TJclClrMemberAccess;
    function GetMethodFlags: TJclClrMethodFlags;
    function GetNewSlot: Boolean;
    function GetCodeType: TJclClrMethodCodeType;
    function GetManaged: Boolean;
    function GetMethodImplFlags: TJclClrMethodImplFlags;
  protected
    constructor Create(const ATable: TJclClrTable); override;
    procedure Update; override;
    procedure SetParentToken(const ARow: TJclClrTableTypeDefRow);
  public
    function DumpIL: string; override;

    destructor Destroy; override;

    property RVA: DWORD read FRVA;
    property ImplFlags: Word read FImplFlags;
    property Flags: Word read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property SignatureOffset: DWORD read FSignatureOffset;
    property ParamListIdx: DWORD read FParamListIdx;

    property Name: WideString read GetName;
    property FullName: WideString read GetFullName;

    property MethodFlags: TJclClrMethodFlags read GetMethodFlags;
    property MethodImplFlags: TJclClrMethodImplFlags read GetMethodImplFlags;

    property MemberAccess: TJclClrMemberAccess read GetMemberAccess;
    property NewSlot: Boolean read GetNewSlot;
    property CodeType: TJclClrMethodCodeType read GetCodeType;
    property Managed: Boolean read GetManaged;

    property Signature: TJclClrMethodSign read GetSignature;
    property SignatureData: TJclClrBlobRecord read GetSignatureData;
    property ParentToken: TJclClrTableTypeDefRow read FParentToken;
    property HasParam: Boolean read GetHasParam;
    property Params[const Idx: Integer]: TJclClrTableParamDefRow read GetParam;
    property ParamCount: Integer read GetParamCount;

    property MethodBody: TJclClrMethodBody read FMethodBody;
  end;

  TJclClrTableMethodDef = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableMethodDefRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableMethodDefRow read GetRow; default;
  end;

  TJclClrTableMethodPtrRow = class(TJclClrTableRow)
  private
    FMethodIdx: DWORD;
    function GetMethod: TJclClrTableMethodDefRow;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property MethodIdx: DWORD read FMethodIdx;
    property Method: TJclClrTableMethodDefRow read GetMethod;
  end;

  TJclClrTableMethodPtr = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableMethodPtrRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableMethodPtrRow read GetRow; default;
  end;

  TJclClrTableMethodImplRow = class(TJclClrTableRow)
  private
    FClassIdx: DWORD;
    FMethodBodyIdx: DWORD;
    FMethodDeclarationIdx: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property ClassIdx: DWORD read FClassIdx;
    property MethodBodyIdx: DWORD read FMethodBodyIdx;
    property MethodDeclarationIdx: DWORD read FMethodDeclarationIdx;    
  end;

  TJclClrTableMethodImpl = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableMethodImplRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableMethodImplRow read GetRow; default;
  end;

  TJclClrTableMethodSemanticsRow = class(TJclClrTableRow)
  private
    FSemantics: Word;
    FMethodIdx: DWORD;
    FAssociationIdx: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property Semantics: Word read FSemantics;
    property MethodIdx: DWORD read FMethodIdx;
    property AssociationIdx: DWORD read FAssociationIdx;
  end;

  TJclClrTableMethodSemantics = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableMethodSemanticsRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableMethodSemanticsRow read GetRow; default;
  end;

  TJclClrTableMethodSpecRow = class(TJclClrTableRow)
  private
    FMethodIdx: DWORD;
    FInstantiationOffset: DWORD;
    function GetInstantiation: TJclClrBlobRecord;
    function GetMethod: TJclClrTableRow;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property MethodIdx: DWORD read FMethodIdx;
    property InstantiationOffset: DWORD read FInstantiationOffset;
    property Method: TJclClrTableRow read GetMethod;
    property Instantiation: TJclClrBlobRecord read GetInstantiation;
  end;

  TJclClrTableMethodSpec = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableMethodSpecRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableMethodSpecRow read GetRow; default;
  end;

  TJclClrTableNestedClassRow = class(TJclClrTableRow)
  private
    FEnclosingClassIdx: DWORD;
    FNestedClassIdx: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property NestedClassIdx: DWORD read FNestedClassIdx;
    property EnclosingClassIdx: DWORD read FEnclosingClassIdx;
  end;

  TJclClrTableNestedClass = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableNestedClassRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableNestedClassRow read GetRow; default;
  end;

  TJclClrTablePropertyFlag = (pfSpecialName, pfRTSpecialName, pfHasDefault);
  TJclClrTablePropertyFlags = set of TJclClrTablePropertyFlag;

  TJclClrTablePropertyDefRow = class(TJclClrTableRow)
  private
    FKindIdx: DWORD;
    FNameOffset: DWORD;
    FFlags: Word;
    function GetName: WideString;
    function GetFlags: TJclClrTablePropertyFlags;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    function DumpIL: string; override;

    property RawFlags: Word read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property KindIdx: DWORD read FKindIdx;

    property Name: WideString read GetName;
    property Flags: TJclClrTablePropertyFlags read GetFlags;
  end;

  TJclClrTablePropertyDef = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTablePropertyDefRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTablePropertyDefRow read GetRow; default;
  end;

  TJclClrTablePropertyPtrRow = class(TJclClrTableRow)
  private
    FPropertyIdx: DWORD;
    function GetProperty: TJclClrTablePropertyDefRow;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property PropertyIdx: DWORD read FPropertyIdx;
    property _Property: TJclClrTablePropertyDefRow read GetProperty;
  end;

  TJclClrTablePropertyPtr = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTablePropertyPtrRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTablePropertyPtrRow read GetRow; default;
  end;

  TJclClrTablePropertyMapRow = class(TJclClrTableRow)
  private
    FParentIdx: DWORD;
    FPropertyListIdx: DWORD;
    FProperties: TList;
    function GetParent: TJclClrTableTypeDefRow;
    function GetProperty(const Idx: Integer): TJclClrTablePropertyDefRow;
    function GetPropertyCount: Integer;
  protected
    constructor Create(const ATable: TJclClrTable); override;

    function Add(const ARow: TJclClrTablePropertyDefRow): Integer;
  public
    destructor Destroy; override;

    property ParentIdx: DWORD read FParentIdx;
    property PropertyListIdx: DWORD read FPropertyListIdx;

    property Parent: TJclClrTableTypeDefRow read GetParent;

    property Properties[const Idx: Integer]: TJclClrTablePropertyDefRow read GetProperty;
    property PropertyCount: Integer read GetPropertyCount;
  end;

  TJclClrTablePropertyMap = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTablePropertyMapRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
    procedure Update; override;
  public
    property Rows[const Idx: Integer]: TJclClrTablePropertyMapRow read GetRow; default;
  end;

  TJclClrTableStandAloneSigRow = class(TJclClrTableRow)
  private
    FSignatureOffset: DWORD;
    function GetSignature: TJclClrBlobRecord;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property SignatureOffset: DWORD read FSignatureOffset;
    property Signature: TJclClrBlobRecord read GetSignature;
  end;

  TJclClrTableStandAloneSig = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableStandAloneSigRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableStandAloneSigRow read GetRow; default;
  end;

  TJclClrTypeVisibility =
   (tvNotPublic, tvPublic, tvNestedPublic,
    tvNestedPrivate, tvNestedFamily, tvNestedAssembly,
    tvNestedFamANDAssem, tvNestedFamORAssem);
  TJclClrClassLayout = (clAuto, clSequential, clExplicit);
  TJclClrClassSemantics = (csClass, csInterface);
  TJclClrStringFormatting = (sfAnsi, sfUnicode, sfAutoChar);

  TJclClrTypeAttribute =
   (taAbstract, taSealed, taSpecialName, taImport,
    taSerializable, taBeforeFieldInit, taRTSpecialName, taHasSecurity);
  TJclClrTypeAttributes = set of TJclClrTypeAttribute;

  TJclClrTableTypeDefRow = class(TJclClrTableRow)
  private
    FNamespaceOffset: DWORD;
    FNameOffset: DWORD;
    FFlags: DWORD;
    FExtendsIdx: DWORD;
    FFieldListIdx: DWORD;
    FMethodListIdx: DWORD;
    FFields: TList;
    FMethods: TList;
    function GetName: WideString;
    function GetNamespace: WideString;
    function GetField(const Idx: Integer): TJclClrTableFieldDefRow;
    function GetFieldCount: Integer;
    function GetMethod(const Idx: Integer): TJclClrTableMethodDefRow;
    function GetMethodCount: Integer;
    procedure UpdateFields;
    procedure UpdateMethods;
    function GetFullName: WideString;
    function GetAttributes: TJclClrTypeAttributes;
    function GetClassLayout: TJclClrClassLayout;
    function GetClassSemantics: TJclClrClassSemantics;
    function GetStringFormatting: TJclClrStringFormatting;
    function GetVisibility: TJclClrTypeVisibility;
    function GetExtends: TJclClrTableRow;
  protected
    constructor Create(const ATable: TJclClrTable); override;
    procedure Update; override;
  public
    destructor Destroy; override;

    function DumpIL: string; override;

    function HasField: Boolean;
    function HasMethod: Boolean;

    property Flags: DWORD read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property NamespaceOffset: DWORD read FNamespaceOffset;
    property ExtendsIdx: DWORD read FExtendsIdx;
    property FieldListIdx: DWORD read FFieldListIdx;
    property MethodListIdx: DWORD read FMethodListIdx;

    property Name: WideString read GetName;
    property Namespace: WideString read GetNamespace;
    property FullName: WideString read GetFullName;
    property Extends: TJclClrTableRow read GetExtends;

    property Attributes: TJclClrTypeAttributes read GetAttributes;

    property Visibility: TJclClrTypeVisibility read GetVisibility;
    property ClassLayout: TJclClrClassLayout read GetClassLayout;
    property ClassSemantics: TJclClrClassSemantics read GetClassSemantics;
    property StringFormatting: TJclClrStringFormatting read GetStringFormatting;

    property Fields[const Idx: Integer]: TJclClrTableFieldDefRow read GetField;
    property FieldCount: Integer read GetFieldCount;
    property Methods[const Idx: Integer]: TJclClrTableMethodDefRow read GetMethod;
    property MethodCount: Integer read GetMethodCount;
  end;

  TJclClrTableTypeDef = class(TJclClrTable, ITableCanDumpIL)
  private
    function GetRow(const Idx: Integer): TJclClrTableTypeDefRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableTypeDefRow read GetRow; default;
  end;

  TJclClrTableTypeRefRow = class(TJclClrTableRow)
  private
    FResolutionScopeIdx: DWORD;
    FNamespaceOffset: DWORD;
    FNameOffset: DWORD;
    function GetName: WideString;
    function GetNamespace: WideString;
    function GetResolutionScope: TJclClrTableRow;
    function GetResolutionScopeName: string;
    function GetFullName: WideString;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    function DumpIL: string; override;

    property ResolutionScopeIdx: DWORD read FResolutionScopeIdx;
    property NameOffset: DWORD read FNameOffset;
    property NamespaceOffset: DWORD read FNamespaceOffset;

    property ResolutionScope: TJclClrTableRow read GetResolutionScope;
    property ResolutionScopeName: string read GetResolutionScopeName;
    property Name: WideString read GetName;
    property Namespace: WideString read GetNamespace;
    property FullName: WideString read GetFullName;
  end;

  TJclClrTableTypeRef = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableTypeRefRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableTypeRefRow read GetRow; default;
  end;

  TJclClrTableTypeSpecRow = class(TJclClrTableRow)
  private
    FSignatureOffset: DWORD;
    function GetSignature: TJclClrBlobRecord;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property SignatureOffset: DWORD read FSignatureOffset;
    property Signature: TJclClrBlobRecord read GetSignature;
  end;

  TJclClrTableTypeSpec = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableTypeSpecRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableTypeSpecRow read GetRow; default;
  end;

  TJclClrTableENCMapRow = class(TJclClrTableRow)
  private
    FToken: DWORD;
    FFuncCode: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
    property FuncCode: DWORD read FFuncCode;
  end;

  TJclClrTableENCMap = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableENCMapRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableENCMapRow read GetRow; default;
  end;

  TJclClrTableENCLogRow = class(TJclClrTableENCMapRow)
  private
    FFuncCode: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
    property FuncCode: DWORD read FFuncCode;
  end;

  TJclClrTableENCLog = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableENCLogRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableENCLogRow read GetRow; default;
  end;

  EJclMetadataError = class(EJclError);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclMetadata.pas $';
    Revision: '$Revision: 2351 $';
    Date: '$Date: 2008-02-09 21:11:06 +0100 (sam., 09 févr. 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  Math,
  JclCIL, JclResources, JclStrings;

const
  MAX_CLASS_NAME = 1024;
  MAX_PATH_NAME  = 260;

  // Assembly attr bits, used by DefineAssembly.
  afPublicKey                  = $0001; // The assembly ref holds the full (unhashed) public key.
  afCompatibilityMask          = $0070;
  afSideBySideCompatible       = $0000; // The assembly is side by side compatible.
  afNonSideBySideAppDomain     = $0010; // The assembly cannot execute with other versions if
                                        // they are executing in the same application domain.
  afNonSideBySideProcess       = $0020; // The assembly cannot execute with other versions if
                                        // they are executing in the same process.
  afNonSideBySideMachine       = $0030; // The assembly cannot execute with other versions if
                                        // they are executing on the same machine.
  afEnableJITcompileTracking   = $8000; // From "DebuggableAttribute".
  afDisableJITcompileOptimizer = $4000; // From "DebuggableAttribute".

  ClrAssemblyFlagMapping: array [TJclClrAssemblyFlag] of DWORD =
    (afPublicKey, afCompatibilityMask, afSideBySideCompatible,
     afNonSideBySideAppDomain, afNonSideBySideProcess,
     afNonSideBySideMachine, afEnableJITcompileTracking,
     afDisableJITcompileOptimizer);

  mrVisibilityMask = $0007;
  mrPublic         = $0001;     // The Resource is exported from the Assembly.
  mrPrivate        = $0002;     // The Resource is private to the Assembly.

  ManifestResourceVisibilityMapping: array [TJclClrTableManifestResourceVisibility] of DWORD =
    (mrPublic, mrPrivate);

  // MethodDef attr bits, Used by DefineMethod.
  // member access mask - Use this mask to retrieve accessibility information.
  mdMemberAccessMask      = $0007;
  mdPrivateScope          = $0000;     // Member not referenceable.
  mdPrivate               = $0001;     // Accessible only by the parent type.
  mdFamANDAssem           = $0002;     // Accessible by sub-types only in this Assembly.
  mdAssem                 = $0003;     // Accessibly by anyone in the Assembly.
  mdFamily                = $0004;     // Accessible only by type and sub-types.
  mdFamORAssem            = $0005;     // Accessibly by sub-types anywhere, plus anyone in assembly.
  mdPublic                = $0006;     // Accessibly by anyone who has visibility to this scope.
  // end member access mask

  // method contract attributes.
  mdStatic                = $0010;     // Defined on type, else per instance.
  mdFinal                 = $0020;     // Method may not be overridden.
  mdVirtual               = $0040;     // Method virtual.
  mdHideBySig             = $0080;     // Method hides by name+sig, else just by name.

  // vtable layout mask - Use this mask to retrieve vtable attributes.
  mdVtableLayoutMask      = $0100;
  mdReuseSlot             = $0000;     // The default.
  mdNewSlot               = $0100;     // Method always gets a new slot in the vtable.
  // end vtable layout mask

  // method implementation attributes.
  mdCheckAccessOnOverride = $0200;     // Overridability is the same as the visibility.
  mdAbstract              = $0400;     // Method does not provide an implementation.
  mdSpecialName           = $0800;     // Method is special.  Name describes how.

  // interop attributes
  mdPinvokeImpl           = $2000;     // Implementation is forwarded through pinvoke.
  mdUnmanagedExport       = $0008;     // Managed method exported via thunk to unmanaged code.

  // Reserved flags for runtime use only.
  mdReservedMask          = $d000;
  mdRTSpecialName         = $1000;     // Runtime should check name encoding.
  mdHasSecurity           = $4000;     // Method has security associate with it.
  mdRequireSecObject      = $8000;     // Method calls another method containing security code.

  // MethodImpl attr bits, used by DefineMethodImpl.
  // code impl mask
  miCodeTypeMask     = $0003;   // Flags about code type.
  miIL               = $0000;   // Method impl is IL.
  miNative           = $0001;   // Method impl is native.
  miOPTIL            = $0002;   // Method impl is OPTIL
  miRuntime          = $0003;   // Method impl is provided by the runtime.
  // end code impl mask

  // managed mask
  miManagedMask      = $0004;   // Flags specifying whether the code is managed or unmanaged.
  miUnmanaged        = $0004;   // Method impl is unmanaged, otherwise managed.
  miManaged          = $0000;   // Method impl is managed.
  // end managed mask

  // implementation info and interop
  miForwardRef       = $0010;   // Indicates method is defined; used primarily in merge scenarios.
  miPreserveSig      = $0080;   // Indicates method sig is not to be mangled to do HRESULT conversion.

  miInternalCall     = $1000;   // Reserved for internal use.

  miSynchronized     = $0020;   // Method is single threaded through the body.
  miNoInlining       = $0008;   // Method may not be inlined.
  miMaxMethodImplVal = $ffff;   // Range check value

  // Calling convention flags.
  IMAGE_CEE_CS_CALLCONV_DEFAULT      = $0;
  IMAGE_CEE_CS_CALLCONV_VARARG       = $5;
  IMAGE_CEE_CS_CALLCONV_FIELD        = $6;
  IMAGE_CEE_CS_CALLCONV_LOCAL_SIG    = $7;
  IMAGE_CEE_CS_CALLCONV_PROPERTY     = $8;
  IMAGE_CEE_CS_CALLCONV_UNMGD        = $9;
  IMAGE_CEE_CS_CALLCONV_MAX          = $10;  // first invalid calling convention
  // The high bits of the calling convention convey additional info
  IMAGE_CEE_CS_CALLCONV_MASK         = $0f;  // Calling convention is bottom 4 bits
  IMAGE_CEE_CS_CALLCONV_HASTHIS      = $20;  // Top bit indicates a 'this' parameter
  IMAGE_CEE_CS_CALLCONV_EXPLICITTHIS = $40;  // This parameter is explicitly in the signature

  // TypeDef/ExportedType attr bits, used by DefineTypeDef.
  // Use this mask to retrieve the type visibility information.
  tdVisibilityMask     = $00000007;
  tdNotPublic          = $00000000;     // Class is not public scope.
  tdPublic             = $00000001;     // Class is public scope.
  tdNestedPublic       = $00000002;     // Class is nested with public visibility.
  tdNestedPrivate      = $00000003;     // Class is nested with private visibility.
  tdNestedFamily       = $00000004;     // Class is nested with family visibility.
  tdNestedAssembly     = $00000005;     // Class is nested with assembly visibility.
  tdNestedFamANDAssem  = $00000006;     // Class is nested with family and assembly visibility.
  tdNestedFamORAssem   = $00000007;     // Class is nested with family or assembly visibility.

  // Use this mask to retrieve class layout information
  tdLayoutMask         = $00000018;
  tdAutoLayout         = $00000000;     // Class fields are auto-laid out
  tdSequentialLayout   = $00000008;     // Class fields are laid out sequentially
  tdExplicitLayout     = $00000010;     // Layout is supplied explicitly
  // end layout mask

  // Use this mask to retrieve class semantics information.
  tdClassSemanticsMask = $00000020;
  tdClass              = $00000000;     // Type is a class.
  tdInterface          = $00000020;     // Type is an interface.
  // end semantics mask

  // Special semantics in addition to class semantics.
  tdAbstract           = $00000080;     // Class is abstract
  tdSealed             = $00000100;     // Class is concrete and may not be extended
  tdSpecialName        = $00000400;     // Class name is special.  Name describes how.

  // Implementation attributes.
  tdImport             = $00001000;     // Class / interface is imported
  tdSerializable       = $00002000;     // The class is Serializable.

  // Use tdStringFormatMask to retrieve string information for native interop
  tdStringFormatMask   = $00030000;
  tdAnsiClass          = $00000000;     // LPTSTR is interpreted as ANSI in this class
  tdUnicodeClass       = $00010000;     // LPTSTR is interpreted as UNICODE
  tdAutoClass          = $00020000;     // LPTSTR is interpreted automatically
  // end string format mask

  tdBeforeFieldInit    = $00100000;     // Initialize the class any time before first static field access.

  // Flags reserved for runtime use.
  tdReservedMask       = $00040800;
  tdRTSpecialName      = $00000800;     // Runtime should check name encoding.
  tdHasSecurity        = $00040000;     // Class has security associate with it.

  // FieldDef attr bits, used by DefineField.
  // member access mask - Use this mask to retrieve accessibility information.
  fdFieldAccessMask = $0007;
  fdPrivateScope    = $0000;     // Member not referenceable.
  fdPrivate         = $0001;     // Accessible only by the parent type.
  fdFamANDAssem     = $0002;     // Accessible by sub-types only in this Assembly.
  fdAssembly        = $0003;     // Accessibly by anyone in the Assembly.
  fdFamily          = $0004;     // Accessible only by type and sub-types.
  fdFamORAssem      = $0005;     // Accessibly by sub-types anywhere, plus anyone in assembly.
  fdPublic          = $0006;     // Accessibly by anyone who has visibility to this scope.
  // end member access mask

  // field contract attributes.
  fdStatic          = $0010;     // Defined on type, else per instance.
  fdInitOnly        = $0020;     // Field may only be initialized, not written to after init.
  fdLiteral         = $0040;     // Value is compile time constant.
  fdNotSerialized   = $0080;     // Field does not have to be serialized when type is remoted.

  fdSpecialName     = $0200;     // field is special.  Name describes how.

  // interop attributes
  fdPinvokeImpl     = $2000;     // Implementation is forwarded through pinvoke.

  // Reserved flags for runtime use only.
  fdReservedMask    = $9500;
  fdRTSpecialName   = $0400;     // Runtime(metadata internal APIs) should check name encoding.
  fdHasFieldMarshal = $1000;     // Field has marshalling information.
  fdHasDefault      = $8000;     // Field has default.
  fdHasFieldRVA     = $0100;     // Field has RVA.

  // Flags for Params
  pdIn              = $0001;     // Param is [In]
  pdOut             = $0002;     // Param is [out]
  pdOptional        = $0010;     // Param is optional

  // Reserved flags for Runtime use only.
  pdReservedMask    = $f000;
  pdHasDefault      = $1000;     // Param has default value.
  pdHasFieldMarshal = $2000;     // Param has FieldMarshal.

  pdUnused          = $cfe0;

  ClrParamKindMapping: array [TJclClrParamKind] of DWORD =
    (pdIn, pdOut, pdOptional, pdHasDefault, pdHasFieldMarshal);

  // Element type for Cor signature
  ELEMENT_TYPE_END        = $0;
  ELEMENT_TYPE_VOID       = $1;
  ELEMENT_TYPE_BOOLEAN    = $2;
  ELEMENT_TYPE_CHAR       = $3;
  ELEMENT_TYPE_I1         = $4;
  ELEMENT_TYPE_U1         = $5;
  ELEMENT_TYPE_I2         = $6;
  ELEMENT_TYPE_U2         = $7;
  ELEMENT_TYPE_I4         = $8;
  ELEMENT_TYPE_U4         = $9;
  ELEMENT_TYPE_I8         = $a;
  ELEMENT_TYPE_U8         = $b;
  ELEMENT_TYPE_R4         = $c;
  ELEMENT_TYPE_R8         = $d;
  ELEMENT_TYPE_STRING     = $e;

  // every type above PTR will be simple type
  ELEMENT_TYPE_PTR        = $f;      // PTR <type>
  ELEMENT_TYPE_BYREF      = $10;     // BYREF <type>

  // Please use ELEMENT_TYPE_VALUETYPE. ELEMENT_TYPE_VALUECLASS is deprecated.
  ELEMENT_TYPE_VALUETYPE  = $11;     // VALUETYPE <class Token>
  ELEMENT_TYPE_CLASS      = $12;     // CLASS <class Token>

  ELEMENT_TYPE_ARRAY      = $14;     // MDARRAY <type> <rank> <bcount> <bound1> ... <lbcount> <lb1> ...

  ELEMENT_TYPE_TYPEDBYREF = $16;     // This is a simple type.

  ELEMENT_TYPE_I          = $18;     // native integer size
  ELEMENT_TYPE_U          = $19;     // native unsigned integer size
  ELEMENT_TYPE_FNPTR      = $1B;     // FNPTR <complete sig for the function including calling convention>
  ELEMENT_TYPE_OBJECT     = $1C;     // Shortcut for System.Object
  ELEMENT_TYPE_SZARRAY    = $1D;     // Shortcut for single dimension zero lower bound array
                                          // SZARRAY <type>

  // This is only for binding
  ELEMENT_TYPE_CMOD_REQD  = $1F;     // required C modifier : E_T_CMOD_REQD <mdTypeRef/mdTypeDef>
  ELEMENT_TYPE_CMOD_OPT   = $20;     // optional C modifier : E_T_CMOD_OPT <mdTypeRef/mdTypeDef>

  // This is for signatures generated internally (which will not be persisted in any way).
  ELEMENT_TYPE_INTERNAL   = $21;     // INTERNAL <typehandle>

  // Note that this is the max of base type excluding modifiers
  ELEMENT_TYPE_MAX        = $22;     // first invalid element type


  ELEMENT_TYPE_MODIFIER   = $40;
  ELEMENT_TYPE_SENTINEL   = $01 or ELEMENT_TYPE_MODIFIER; // sentinel for varargs
  ELEMENT_TYPE_PINNED     = $05 or ELEMENT_TYPE_MODIFIER;

  ClrElementTypeMapping: array [TJclClrElementType] of Byte =
   (ELEMENT_TYPE_END, ELEMENT_TYPE_VOID, ELEMENT_TYPE_BOOLEAN,
    ELEMENT_TYPE_CHAR, ELEMENT_TYPE_I1, ELEMENT_TYPE_U1,
    ELEMENT_TYPE_I2, ELEMENT_TYPE_U2, ELEMENT_TYPE_I4, ELEMENT_TYPE_U4,
    ELEMENT_TYPE_I8, ELEMENT_TYPE_U8, ELEMENT_TYPE_R4, ELEMENT_TYPE_R8,
    ELEMENT_TYPE_STRING, ELEMENT_TYPE_PTR, ELEMENT_TYPE_BYREF,
    ELEMENT_TYPE_VALUETYPE, ELEMENT_TYPE_CLASS, ELEMENT_TYPE_ARRAY,
    ELEMENT_TYPE_TYPEDBYREF, ELEMENT_TYPE_I, ELEMENT_TYPE_U,
    ELEMENT_TYPE_FNPTR, ELEMENT_TYPE_OBJECT, ELEMENT_TYPE_SZARRAY,
    ELEMENT_TYPE_CMOD_REQD, ELEMENT_TYPE_CMOD_OPT, ELEMENT_TYPE_INTERNAL,
    ELEMENT_TYPE_MAX, ELEMENT_TYPE_MODIFIER, ELEMENT_TYPE_SENTINEL,
    ELEMENT_TYPE_PINNED);

  ClrMethodFlagMapping: array [TJclClrMethodFlag] of Word =
   (mdStatic, mdFinal, mdVirtual, mdHideBySig, mdCheckAccessOnOverride,
    mdAbstract, mdSpecialName, mdPinvokeImpl, mdUnmanagedExport,
    mdRTSpecialName, mdHasSecurity, mdRequireSecObject);

  ClrMethodImplFlagMapping: array [TJclClrMethodImplFlag] of Word =
   (miForwardRef, miPreserveSig, miInternalCall, miSynchronized, miNoInlining);

  // Property attr bits, used by DefineProperty.
  prSpecialName   = $0200;     // property is special.  Name describes how.

  // Reserved flags for Runtime use only.
  prReservedMask  = $f400;
  prRTSpecialName = $0400;     // Runtime(metadata internal APIs) should check name encoding.
  prHasDefault    = $1000;     // Property has default

  prUnused        = $e9ff;

  ClrTablePropertyFlagMapping: array [TJclClrTablePropertyFlag] of Word =
   (prSpecialName, prRTSpecialName, prHasDefault);

  // Event attr bits, used by DefineEvent.
  evSpecialName   = $0200;     // event is special.  Name describes how.

  // Reserved flags for Runtime use only.
  evReservedMask  = $0400;
  evRTSpecialName = $0400;     // Runtime(metadata internal APIs) should check name encoding.

  ClrTableEventFlagMapping: array [TJclClrTableEventFlag] of Word =
   (evSpecialName, evRTSpecialName);

  // DeclSecurity attr bits, used by DefinePermissionSet
  dclActionMask        = $000f;     // Mask allows growth of enum.
  dclActionNil         = $0000;
  dclRequest           = $0001;
  dclDemand            = $0002;
  dclAssert            = $0003;
  dclDeny              = $0004;
  dclPermitOnly        = $0005;
  dclLinktimeCheck     = $0006;
  dclInheritanceCheck  = $0007;
  dclRequestMinimum    = $0008;
  dclRequestOptional   = $0009;
  dclRequestRefuse     = $000a;
  dclPrejitGrant       = $000b;     // Persisted grant set at prejit time
  dclPrejitDenied      = $000c;     // Persisted denied set at prejit time
  dclNonCasDemand      = $000d;     //
  dclNonCasLinkDemand  = $000e;
  dclNonCasInheritance = $000f;
  dclMaximumValue      = $000f;     // Maximum legal value

  // PinvokeMap attr bits, used by DefinePinvokeMap
  pmNoMangle        = $0001;   // Pinvoke is to use the member name as specified.

  // Use this mask to retrieve the CharSet information.
  pmCharSetMask     = $0006;
  pmCharSetNotSpec  = $0000;
  pmCharSetAnsi     = $0002;
  pmCharSetUnicode  = $0004;
  pmCharSetAuto     = $0006;


  pmBestFitUseAssem = $0000;
  pmBestFitEnabled  = $0010;
  pmBestFitDisabled = $0020;
  pmBestFitMask     = $0030;

  pmThrowOnUnmappableCharUseAssem = $0000;
  pmThrowOnUnmappableCharEnabled  = $1000;
  pmThrowOnUnmappableCharDisabled = $2000;
  pmThrowOnUnmappableCharMask     = $3000;

  pmSupportsLastError = $0040;   // Information about target function. Not relevant for fields.

  // None of the calling convention flags is relevant for fields.
  pmCallConvMask     = $0700;
  pmCallConvWinapi   = $0100;   // Pinvoke will use native callconv appropriate to target windows platform.
  pmCallConvCdecl    = $0200;
  pmCallConvStdcall  = $0300;
  pmCallConvThiscall = $0400;   // In M9, pinvoke will raise exception.
  pmCallConvFastcall = $0500;

function IsBitSet(const Value, Flag: DWORD): Boolean;
begin
  Result := (Value and Flag) = Flag;
end;

//=== { TJclClrSignature } ===================================================

constructor TJclClrSignature.Create(const ABlob: TJclClrBlobRecord);
begin
  inherited Create;
  FBlob := ABlob;
end;

function TJclClrSignature.IsModifierType(const AElementType: TJclClrElementType): Boolean;
begin
  Result := AElementType in [etPtr, etByRef, etModifier, etSentinel, etPinned];
end;

function TJclClrSignature.IsPrimitiveType(const AElementType: TJclClrElementType): Boolean;
begin
  Result := AElementType < etPtr;
end;

function TJclClrSignature.UncompressedDataSize(DataPtr: PJclByteArray): Integer;
begin
  if (DataPtr[0] and $80) = 0 then
    Result := 1
  else
  if (DataPtr[0] and $C0) = $80 then
    Result := 2
  else
    Result := 4;
end;

function TJclClrSignature.UncompressData(DataPtr: PJclByteArray; var Value: DWord): Integer;
begin
  if (DataPtr[0] and $80) = 0 then // 0??? ????
  begin
    Value  := DataPtr[0];
    Result := 1;
  end
  else
  if (DataPtr[0] and $C0) = $80 then // 10?? ????
  begin
    Value  := (DataPtr[0] and $3F) shl 8 + DataPtr[1];
    Result := 2;
  end
  else
  if (DataPtr[0] and $E0) = $C0 then // 110? ????
  begin
    Value  := (DataPtr[0] and $1F) shl 24 + DataPtr[1] shl 16 + DataPtr[2] shl 8 + DataPtr[3];
    Result := 4;
  end
  else
    raise EJclMetadataError.CreateResFmt(@RsInvalidSignatureData,
      [DataPtr[0], DataPtr[1], DataPtr[2], DataPtr[3]]);
end;

function TJclClrSignature.UncompressToken(DataPtr: PJclByteArray; var Token: TJclClrToken): Integer;
const
  TableMapping: array [0..3] of TJclClrTableKind = (ttTypeDef, ttTypeRef, ttTypeSpec, TJclClrTableKind(0));
begin
  Result := UncompressData(DataPtr, Token);
  Token  := Byte(TableMapping[Token and 3]) shl 24 + Token shr 2;
end;

function TJclClrSignature.UncompressCallingConv(DataPtr: PJclByteArray): Byte;
begin
  Result := DataPtr[0];
end;

function TJclClrSignature.UncompressSignedInt(DataPtr: PJclByteArray; var Value: Integer): Integer;
var
  Data: DWord;
begin
  Result := UncompressData(DataPtr, Data);

  if (Data and 1) <> 0 then
  begin
    case Result of
      1:
        Value := Integer(DWord(Data shr 1) or $ffffffc0);
      2:
        Value := Integer(DWord(Data shr 1) or $ffffe000);
    else
      Value := Integer(DWord(Data shr 1) or $f0000000);
    end;
  end;
end;

function TJclClrSignature.UncompressElementType(DataPtr: PJclByteArray): TJclClrElementType;
begin
  for Result := Low(TJclClrElementType) to High(TJclClrElementType) do
    if ClrElementTypeMapping[Result] = (DataPtr[0] and $7F) then
      Break;
end;

function TJclClrSignature.UncompressFieldSignature: string;
var
  DataPtr: PJclByteArray;
begin
  DataPtr := Blob.Memory;

  Assert(DataPtr[0] = IMAGE_CEE_CS_CALLCONV_FIELD);
  Inc(DataPtr);
  Result := UncompressTypeSignature(DataPtr);
end;

function TJclClrSignature.UncompressTypeSignature(DataPtr: PJclByteArray): string;
const
  SimpleTypeName: array [etVoid..etString] of PChar =
   ('void', 'bool', 'char',
    'int8', 'unsigned int8', 'int16', 'unsigned int16',
    'int32', 'unsigned int32', 'int64', 'unsigned int64',
    'float32', 'float64', 'string');
  TypedTypeName: array [etPtr..etClass] of PChar =
    ('ptr', 'byref', 'valuetype', 'class');
var
  ElementType: TJclClrElementType;
  Token: TJclClrToken;
begin
  ElementType := UncompressElementType(DataPtr);

  case ElementType of
    etVoid, etBoolean, etChar, etI1, etU1, etI2, etU2, etI4, etU4, etI8, etU8, etR4, etR8, etString:
      Result := SimpleTypeName[ElementType];
    etI:
      Result := 'System.IntPtr';
    etU:
      Result := 'System.UIntPtr';
    etObject:
      Result := 'System.object';
    etTypedByRef:
      Result := 'Typed By Ref';
    etPtr, etByRef, etValueType, etClass:
      begin
        UncompressToken(DataPtr, Token);
        Result := Format('%s /*%.8x*/', [TypedTypeName[ElementType], Token]);
      end;
    etSzArray:
      begin
      end;
    etFnPtr:
      begin
      end;
    etArray:
      begin
      end;
    else
      Result := 'Unknown Type';
  end;
end;

function TJclClrSignature.Inc(var DataPtr: PJclByteArray; Step: Integer): PByte;
begin
  Result := PByte(Integer(DataPtr) + Step);
  DataPtr := PJclByteArray(Result);
end;

function TJclClrSignature.ReadValue: DWORD;
begin
  FBlob.Seek(UncompressData(Blob.Data, Result), soFromCurrent);
end;

function TJclClrSignature.ReadInteger: Integer;
begin
  FBlob.Seek(UncompressSignedInt(Blob.Data, Result), soFromCurrent);
end;

function TJclClrSignature.ReadToken: TJclClrToken;
begin
  FBlob.Seek(UncompressToken(Blob.Data, Result), soFromCurrent);
end;

function TJclClrSignature.ReadElementType: TJclClrElementType;
begin
  Result := UncompressElementType(Blob.Data);
  FBlob.Seek(1, soFromCurrent);
end;

function TJclClrSignature.ReadByte: Byte;
begin
  Result := Blob.Data[0];
  FBlob.Seek(1, soFromCurrent);
end;

//=== { TJclClrArraySign } ===================================================

constructor TJclClrArraySign.Create(const ABlob: TJclClrBlobRecord);
var
  I: Integer;
begin
  inherited Create(ABlob);

  SetLength(FBounds, ReadInteger);

  for I := 0 to Length(FBounds)-1 do
  begin
    FBounds[I][adSize] := 0;
    FBounds[I][adLowBound] := 0;
  end;
  for I := 0 to ReadInteger-1 do
    FBounds[I][adSize] := ReadInteger;
  for I := 0 to ReadInteger-1 do
    FBounds[I][adLowBound] := ReadInteger;
end;

//=== { TJclClrTableModuleRow } ==============================================

constructor TJclClrTableModuleRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FGeneration   := Table.ReadWord;            // Generation (reserved, shall be zero)
  FNameOffset   := Table.ReadIndex(hkString); // Name (index into String heap)
  FMvidIdx      := Table.ReadIndex(hkGuid);   // Mvid (index into Guid heap)
  FEncIdIdx     := Table.ReadIndex(hkGuid);   // Mvid (index into Guid heap)
  FEncBaseIdIdx := Table.ReadIndex(hkGuid);   // Mvid (index into Guid heap)
end;

function TJclClrTableModuleRow.HasEncId: Boolean;
begin
  Result := FEncIdIdx > 0;
end;

function TJclClrTableModuleRow.HasEncBaseId: Boolean;
begin
  Result := FEncBaseIdIdx > 0;
end;

function TJclClrTableModuleRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
  Assert(Result <> ''); // Name shall index a non-null string.
  Assert(Length(Result) < MAX_PATH_NAME);
end;

function TJclClrTableModuleRow.GetMvid: TGUID;
begin
  // Mvid shall index a non-null GUID in the Guid heap
  Assert(FMvidIdx <= DWORD(Table.Stream.Metadata.GuidCount));
  Result := Table.Stream.Metadata.Guids[FMvidIdx-1];
end;

function TJclClrTableModuleRow.GetEncId: TGUID;
begin
  Result := Table.Stream.Metadata.Guids[FEncIdIdx-1];
end;

function TJclClrTableModuleRow.GetEncBaseId: TGUID;
begin
  Result := Table.Stream.Metadata.Guids[FEncBaseIdIdx-1];
end;

function TJclClrTableModuleRow.DumpIL: string;
begin
  Result := '.module ' + Name + ' // MVID:' + JclGUIDToString(Mvid) + AnsiLineBreak;
end;

function TJclClrTableModule.GetRow(const Idx: Integer): TJclClrTableModuleRow;
begin
  Result := TJclClrTableModuleRow(inherited GetRow(Idx));
end;

class function TJclClrTableModule.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableModuleRow;
end;

//=== { TJclClrTableModuleRefRow } ===========================================

constructor TJclClrTableModuleRefRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FNameOffset := Table.ReadIndex(hkString);
end;

function TJclClrTableModuleRefRow.DumpIL: string;
begin
  Result := '.module extern ' + Name + AnsiLineBreak;
end;

function TJclClrTableModuleRefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableModuleRef.GetRow(const Idx: Integer): TJclClrTableModuleRefRow;
begin
  Result := TJclClrTableModuleRefRow(inherited GetRow(Idx));
end;

class function TJclClrTableModuleRef.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableModuleRefRow;
end;

//=== { TJclClrTableAssemblyRow } ============================================

constructor TJclClrTableAssemblyRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);

  FHashAlgId       := Table.ReadDWord;

  FMajorVersion    := Table.ReadWord;
  FMinorVersion    := Table.ReadWord;
  FBuildNumber     := Table.ReadWord;
  FRevisionNumber  := Table.ReadWord;

  FFlagMask        := Table.ReadDWord;

  FPublicKeyOffset := Table.ReadIndex(hkBlob);
  FNameOffset      := Table.ReadIndex(hkString);
  FCultureOffset   := Table.ReadIndex(hkString);
end;

function TJclClrTableAssemblyRow.GetCulture: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FCultureOffset);
end;

function TJclClrTableAssemblyRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableAssemblyRow.GetPublicKey: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FPublicKeyOffset);
end;

function TJclClrTableAssemblyRow.GetVersion: string;
begin
  Result := FormatVersionString(FMajorVersion, FMinorVersion, FBuildNumber, FRevisionNumber);
end;

function TJclClrTableAssemblyRow.GetFlags: TJclClrAssemblyFlags;
begin
  Result := AssemblyFlags(FFlagMask);
end;

class function TJclClrTableAssemblyRow.AssemblyFlags(const Flags: DWORD): TJclClrAssemblyFlags;
var
  AFlag: TJclClrAssemblyFlag;
begin
  Result := [];
  for AFlag := Low(TJclClrAssemblyFlag) to High(TJclClrAssemblyFlag) do
    if (Flags and ClrAssemblyFlagMapping[AFlag]) = ClrAssemblyFlagMapping[AFlag] then
      Include(Result, AFlag);
end;

class function TJclClrTableAssemblyRow.AssemblyFlags(const Flags: TJclClrAssemblyFlags): DWORD;
var
  AFlag: TJclClrAssemblyFlag;
begin
  Result := 0;
  for AFlag := Low(TJclClrAssemblyFlag) to High(TJclClrAssemblyFlag) do
    if AFlag in Flags then
      Result := Result or ClrAssemblyFlagMapping[AFlag];
end;

function TJclClrTableAssemblyRow.DumpIL: string;
var
  I: Integer;
  TblCustomAttribute: TJclClrTableCustomAttribute;
begin
  with TStringList.Create do
    try
      Add(Format('.assembly /*%.8x*/ %s', [Token, Name]));
      Add('{');

      if Table.Stream.FindTable(ttCustomAttribute, TJclClrTable(TblCustomAttribute)) then
        for I := 0 to TblCustomAttribute.RowCount-1 do
          if TblCustomAttribute.Rows[I].Parent = Self then
            Add('  ' + TblCustomAttribute.Rows[I].DumpIL);

      if FPublicKeyOffset <> 0 then
        Add(PublicKey.Dump('  .publickey = '));
        
      Add('  .hash algorithm 0x' + IntToHex(HashAlgId, 8));

      if FCultureOffset <> 0 then
        Add('  .culture "' + Culture + '"');

      Add('  .ver ' + Version);
      Add('}');
      Result := Text;
    finally
      Free;
    end;
end;

function TJclClrTableAssembly.GetRow(const Idx: Integer): TJclClrTableAssemblyRow;
begin
  Result := TJclClrTableAssemblyRow(inherited GetRow(Idx));
end;

class function TJclClrTableAssembly.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableAssemblyRow;
end;

//=== { TJclClrTableAssemblyOSRow } ==========================================

constructor TJclClrTableAssemblyOSRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);

  FPlatformID   := Table.ReadDWord;
  FMajorVersion := Table.ReadDWord;
  FMinorVersion := Table.ReadDWord;
end;

function TJclClrTableAssemblyOSRow.GetVersion: string;
begin
  Result := FormatVersionString(FMajorVersion, FMinorVersion);
end;

function TJclClrTableAssemblyOS.GetRow(const Idx: Integer): TJclClrTableAssemblyOSRow;
begin
  Result := TJclClrTableAssemblyOSRow(inherited GetRow(Idx));
end;

class function TJclClrTableAssemblyOS.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableAssemblyOSRow;
end;

//=== { TJclClrTableAssemblyProcessorRow } ===================================

constructor TJclClrTableAssemblyProcessorRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FProcessor := Table.ReadDWord;
end;

function TJclClrTableAssemblyProcessor.GetRow(const Idx: Integer): TJclClrTableAssemblyProcessorRow;
begin
  Result := TJclClrTableAssemblyProcessorRow(inherited GetRow(Idx));
end;

class function TJclClrTableAssemblyProcessor.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableAssemblyProcessorRow;
end;

//=== { TJclClrTableAssemblyRefRow } =========================================

constructor TJclClrTableAssemblyRefRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);

  FMajorVersion           := Table.ReadWord;
  FMinorVersion           := Table.ReadWord;
  FBuildNumber            := Table.ReadWord;
  FRevisionNumber         := Table.ReadWord;

  FFlagMask               := Table.ReadDWord;

  FPublicKeyOrTokenOffset := Table.ReadIndex(hkBlob);
  FNameOffset             := Table.ReadIndex(hkString);
  FCultureOffset          := Table.ReadIndex(hkString);
  FHashValueOffset        := Table.ReadIndex(hkBlob);
end;

function TJclClrTableAssemblyRefRow.DumpIL: string;
var
  I: Integer;
  TblCustomAttribute: TJclClrTableCustomAttribute;

  function DumpPublicKey: string;
  var
    I: Integer;
    Pch: PChar;
    HexStr, AsciiStr: string;
  begin
    Pch := PChar(PublicKeyOrToken.Memory);
    for I := 0 to PublicKeyOrToken.Size do
    begin
      HexStr := HexStr + IntToHex(Integer(Pch[I]), 2) + ' ';
      if CharIsAlphaNum(Pch[I]) then
        AsciiStr := AsciiStr + Pch[I]
      else
        AsciiStr := AsciiStr + '.';
    end;
    Result := '(' + HexStr + ')                    // ' + AsciiStr;
  end;

begin
  with TStringList.Create do
    try
      Add(Format('.assembly extern /*%.8x*/ %s', [Token, Name]));
      Add('{');

      if Table.Stream.FindTable(ttCustomAttribute, TJclClrTable(TblCustomAttribute)) then
        for I := 0 to TblCustomAttribute.RowCount-1 do
          if TblCustomAttribute.Rows[I].Parent = Self then
            Add('  ' + TblCustomAttribute.Rows[I].DumpIL);

      if Assigned(HashValue) then
        Add(PublicKeyOrToken.Dump('  .hash = '));

      if Assigned(PublicKeyOrToken) then
        Add(PublicKeyOrToken.Dump('  .publickeytoken = '));

      if FCultureOffset <> 0 then
        Add('  .culture "' + Culture + '"');

      Add('  .ver ' + Version);
      Add('}');
      Result := Text;
    finally
      Free;
    end;
end;

function TJclClrTableAssemblyRefRow.GetCulture: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FCultureOffset);
end;

function TJclClrTableAssemblyRefRow.GetFlags: TJclClrAssemblyFlags;
begin
  Result := TJclClrTableAssemblyRow.AssemblyFlags(FFlagMask);
end;

function TJclClrTableAssemblyRefRow.GetHashValue: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FHashValueOffset);
end;

function TJclClrTableAssemblyRefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableAssemblyRefRow.GetPublicKeyOrToken: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FPublicKeyOrTokenOffset);
end;

function TJclClrTableAssemblyRefRow.GetVersion: string;
begin
  Result := FormatVersionString(FMajorVersion, FMinorVersion, FBuildNumber, FRevisionNumber);
end;

function TJclClrTableAssemblyRef.GetRow(const Idx: Integer): TJclClrTableAssemblyRefRow;
begin
  Result := TJclClrTableAssemblyRefRow(inherited GetRow(Idx));
end;

class function TJclClrTableAssemblyRef.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableAssemblyRefRow;
end;

//=== { TJclClrTableAssemblyRefOSRow } =======================================

constructor TJclClrTableAssemblyRefOSRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FAssemblyRefIdx := Table.ReadIndex([ttAssemblyRef]);
end;

function TJclClrTableAssemblyRefOSRow.GetAssemblyRef: TJclClrTableAssemblyRefRow;
var
  AssemblyRefTable: TJclClrTableAssemblyRef;
begin
  if Table.Stream.FindTable(ttAssemblyRef, TJclClrTable(AssemblyRefTable)) then
    Result := AssemblyRefTable[FAssemblyRefIdx-1]
  else
    Result := nil;
end;

function TJclClrTableAssemblyRefOS.GetRow(const Idx: Integer): TJclClrTableAssemblyRefOSRow;
begin
  Result := TJclClrTableAssemblyRefOSRow(inherited GetRow(Idx));
end;

class function TJclClrTableAssemblyRefOS.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableAssemblyRefOSRow;
end;

//=== { TJclClrTableAssemblyRefProcessorRow } ================================

constructor TJclClrTableAssemblyRefProcessorRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FAssemblyRefIdx := Table.ReadIndex([ttAssemblyRef]);
end;

function TJclClrTableAssemblyRefProcessorRow.GetAssemblyRef: TJclClrTableAssemblyRefRow;
var
  AssemblyRefTable: TJclClrTableAssemblyRef;
begin
  if Table.Stream.FindTable(ttAssemblyRef, TJclClrTable(AssemblyRefTable)) then
    Result := AssemblyRefTable[FAssemblyRefIdx-1]
  else
    Result := nil;
end;

function TJclClrTableAssemblyRefProcessor.GetRow(
  const Idx: Integer): TJclClrTableAssemblyRefProcessorRow;
begin
  Result := TJclClrTableAssemblyRefProcessorRow(inherited GetRow(Idx));
end;

class function TJclClrTableAssemblyRefProcessor.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableAssemblyRefProcessorRow;
end;

//=== { TJclClrTableClassLayoutRow } =========================================

constructor TJclClrTableClassLayoutRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FPackingSize := Table.ReadWord;
  FClassSize   := Table.ReadDWord;
  FParentIdx   := Table.ReadIndex([ttTypeDef]);
end;

function TJclClrTableClassLayout.GetRow(const Idx: Integer): TJclClrTableClassLayoutRow;
begin
  Result := TJclClrTableClassLayoutRow(inherited GetRow(Idx));
end;

class function TJclClrTableClassLayout.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableClassLayoutRow;
end;

//=== { TJclClrTableConstantRow } ============================================

constructor TJclClrTableConstantRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FKind        := Table.ReadByte;
  Table.ReadByte; // padding zero
  FParentIdx   := Table.ReadIndex([ttParamDef, ttFieldDef, ttPropertyDef]);
  FValueOffset := Table.ReadIndex(hkBlob);
end;

function TJclClrTableConstantRow.DumpIL: string;
begin
  case ElementType of
    etBoolean:
      Result := BooleanToStr(PBoolean(Value.Memory)^);
    etChar:
      Result := PWideChar(Value.Memory)^;
    etI1:
      Result := IntToStr(PShortInt(Value.Memory)^);
    etU1:
      Result := IntToStr(PByte(Value.Memory)^);
    etI2:
      Result := IntToStr(PSmallint(Value.Memory)^);
    etU2:
      Result := IntToStr(PWord(Value.Memory)^);
    etI4:
      Result := IntToStr(PInteger(Value.Memory)^);
    etU4:
      Result := IntToStr(PDWord(Value.Memory)^);
    etI8:
      Result := IntToStr(PInt64(Value.Memory)^);
    etU8:
      Result := IntToStr(PInt64(Value.Memory)^);
    etR4:
      Result := FloatToStr(PSingle(Value.Memory)^);
    etR8:
      Result := FloatToStr(PDouble(Value.Memory)^);
    etString:
      Result := '"' + WideCharLenToString(PWideChar(Value.Memory), Value.Size div 2) + '"';
    etClass:
      begin
        if FValueOffset = 0 then
        begin
          Result := ' nullref';
          Exit;
        end;

        Result := Table.Stream.Metadata.Tokens[PJclClrToken(Value.Memory)^].DumpIL;
      end;
  end;
  Result := ' = ' + Result;
end;

function TJclClrTableConstantRow.GetElementType: TJclClrElementType;
begin
  for Result := Low(TJclClrElementType) to High(TJclClrElementType) do
    if ClrElementTypeMapping[Result] = FKind then
      Exit;
  Result := etEnd;
end;

function TJclClrTableConstantRow.GetParent: TJclClrTableRow;
const
  HasConstantMapping: array [0..2] of TJclClrTableKind =
    (ttFieldDef, ttParamDef, ttPropertyDef);
begin
  Assert(FParentIdx and 3 <> 3);
  Result := Table.Stream.Tables[HasConstantMapping[FParentIdx and 3]].Rows[FParentIdx shr 2 - 1];
end;

function TJclClrTableConstantRow.GetValue: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FValueOffset);
end;

function TJclClrTableConstant.GetRow(const Idx: Integer): TJclClrTableConstantRow;
begin
  Result := TJclClrTableConstantRow(inherited GetRow(Idx));
end;

class function TJclClrTableConstant.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableConstantRow;
end;

//=== { TJclClrTableCustomAttributeRow } =====================================

constructor TJclClrTableCustomAttributeRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FParentIdx := Table.ReadIndex([ttModule, ttTypeRef, ttTypeDef, ttFieldDef,
    ttMethodDef, ttParamDef, ttInterfaceImpl, ttMemberRef, ttConstant,
    ttFieldMarshal, ttDeclSecurity, ttClassLayout, ttFieldLayout, ttSignature,
    ttEventMap, ttEventDef, ttPropertyMap, ttPropertyDef, ttMethodSemantics,
    ttMethodImpl, ttModuleRef, ttTypeSpec, ttImplMap, ttFieldRVA, ttAssembly,
    ttAssemblyProcessor, ttAssemblyOS, ttAssemblyRef, ttAssemblyRefProcessor,
    ttAssemblyRefOS, ttFile, ttExportedType, ttManifestResource, ttNestedClass]);
  FTypeIdx := Table.ReadIndex([ttMethodDef, ttMemberRef]);
  FValueOffset := Table.ReadIndex(hkBlob);
end;

function TJclClrTableCustomAttributeRow.GetParent: TJclClrTableRow;
const
  MapTagToTable: array [0..18] of TJclClrTableKind =
   (ttMethodDef, ttFieldDef, ttTypeRef, ttTypeDef, ttParamDef, ttInterfaceImpl,
    ttMemberRef, ttModule, ttDeclSecurity, ttPropertyDef, ttEventDef, ttSignature,
    ttModuleRef, ttTypeSpec, ttAssembly, ttAssemblyRef, ttFile, ttExportedType,
    ttManifestResource);
var
  WideIndex: Boolean;
begin
  WideIndex := Table.IsWideIndex([ttModule, ttTypeRef, ttTypeDef, ttFieldDef,
    ttMethodDef, ttParamDef, ttInterfaceImpl, ttMemberRef, ttConstant,
    ttFieldMarshal, ttDeclSecurity, ttClassLayout, ttFieldLayout, ttSignature,
    ttEventMap, ttEventDef, ttPropertyMap, ttPropertyDef, ttMethodSemantics,
    ttMethodImpl, ttModuleRef, ttTypeSpec, ttImplMap, ttFieldRVA, ttAssembly,
    ttAssemblyProcessor, ttAssemblyOS, ttAssemblyRef, ttAssemblyRefProcessor,
    ttAssemblyRefOS, ttFile, ttExportedType, ttManifestResource, ttNestedClass]);

  Assert(Table.GetCodedIndexTag(FParentIdx, 5, WideIndex) <= 18);
  Result := Table.Stream.Tables[
    MapTagToTable[Table.GetCodedIndexTag(FParentIdx, 5, WideIndex)]].
    Rows[Table.GetCodedIndexValue(FParentIdx, 5, WideIndex)-1];
end;

function TJclClrTableCustomAttributeRow.GetMethod: TJclClrTableRow;
const
  MapTagToTable: array [2..3] of TJclClrTableKind = (ttMethodDef, ttMemberRef);
var
  WideIndex: Boolean;
begin
  WideIndex := Table.IsWideIndex([ttMethodDef, ttMemberRef]);
  Assert(Table.GetCodedIndexTag(FTypeIdx, 3, WideIndex) in [2, 3]);
  Result := Table.Stream.Tables[
    MapTagToTable[Table.GetCodedIndexTag(FTypeIdx, 3, WideIndex)]].
    Rows[Table.GetCodedIndexValue(FTypeIdx, 3, WideIndex)-1];
end;

function TJclClrTableCustomAttributeRow.GetValue: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FValueOffset);
end;

function TJclClrTableCustomAttributeRow.DumpIL: string;
begin
  // .custom /*0C000001:0A00000C*/ intance void [mscorlib/* 23000001 */]System.Reflection.AssemblyInformationalVersionAttribute/* 0100001C */::.ctor(string) /* 0A00000C */ = ( 01 00 0A 31 2E 30 2E 33 37 30 35 2E 30 00 00 )    // ...1.0.3705.0..
  Result := Value.Dump(Format('.custom /*%.8x:%.8x*/ %s = ', [Token, Method.Token, Method.DumpIL]));
end;

function TJclClrTableCustomAttribute.GetRow(const Idx: Integer): TJclClrTableCustomAttributeRow;
begin
  Result := TJclClrTableCustomAttributeRow(inherited GetRow(Idx));
end;

class function TJclClrTableCustomAttribute.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableCustomAttributeRow;
end;

//=== { TJclClrTableDeclSecurityRow } ========================================

constructor TJclClrTableDeclSecurityRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FAction              := Table.ReadWord;
  FParentIdx           := Table.ReadIndex([ttTypeDef, ttMethodDef, ttAssembly]);
  FPermissionSetOffset := Table.ReadIndex(hkBlob);
end;

function TJclClrTableDeclSecurity.GetRow(const Idx: Integer): TJclClrTableDeclSecurityRow;
begin
  Result := TJclClrTableDeclSecurityRow(inherited GetRow(Idx));
end;

class function TJclClrTableDeclSecurity.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableDeclSecurityRow;
end;

//=== { TJclClrTableEventMapRow } ============================================

constructor TJclClrTableEventMapRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FParentIdx := Table.ReadIndex([ttTypeDef]);
  FEventListIdx := Table.ReadIndex([ttEventDef]);
end;

function TJclClrTableEventMap.GetRow(const Idx: Integer): TJclClrTableEventMapRow;
begin
  Result := TJclClrTableEventMapRow(inherited GetRow(Idx));
end;

class function TJclClrTableEventMap.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableEventMapRow;
end;

//=== { TJclClrTableEventDefRow } ============================================

constructor TJclClrTableEventDefRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FEventFlags   := Table.ReadWord;
  FNameOffset   := Table.ReadIndex(hkString);
  FEventTypeIdx := Table.ReadIndex([ttTypeDef, ttTypeRef, ttTypeSpec]);
end;

function TJclClrTableEventDefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableEventDef.GetRow(const Idx: Integer): TJclClrTableEventDefRow;
begin
  Result := TJclClrTableEventDefRow(inherited GetRow(Idx));
end;

class function TJclClrTableEventDef.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableEventDefRow;
end;

//=== { TJclClrTableEventPtrRow } ============================================

constructor TJclClrTableEventPtrRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FEventIdx := Table.ReadIndex([ttEventDef]);
end;

function TJclClrTableEventPtrRow.GetEvent: TJclClrTableEventDefRow;
begin
  Result := TJclClrTableEventDef(Table.Stream.Tables[ttEventDef]).Rows[FEventIdx-1];
end;

function TJclClrTableEventPtr.GetRow(const Idx: Integer): TJclClrTableEventPtrRow;
begin
  Result := TJclClrTableEventPtrRow(inherited GetRow(Idx));
end;

class function TJclClrTableEventPtr.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableEventPtrRow;
end;

//=== { TJclClrTableExportedTypeRow } ========================================

constructor TJclClrTableExportedTypeRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FFlags               := Table.ReadDWord;
  FTypeDefIdx          := Table.ReadDWord;
  FTypeNameOffset      := Table.ReadIndex(hkString);
  FTypeNamespaceOffset := Table.ReadIndex(hkString);
  FImplementationIdx   := Table.ReadIndex([ttFile, ttExportedType]);
end;

function TJclClrTableExportedTypeRow.GetTypeName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FTypeNameOffset);
end;

function TJclClrTableExportedTypeRow.GetTypeNamespace: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FTypeNamespaceOffset);
end;

function TJclClrTableExportedType.GetRow(const Idx: Integer): TJclClrTableExportedTypeRow;
begin
  Result := TJclClrTableExportedTypeRow(inherited GetRow(Idx));
end;

class function TJclClrTableExportedType.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableExportedTypeRow;
end;

//=== { TJclClrTableFieldDefRow } ============================================

constructor TJclClrTableFieldDefRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FFlags           := Table.ReadWord;
  FNameOffset      := Table.ReadIndex(hkString);
  FSignatureOffset := Table.ReadIndex(hkBlob);
  FParentToken     := nil;
end;

function TJclClrTableFieldDefRow.DumpIL: string;
const
  StaticName: array [Boolean] of PChar =
    ('', 'static ');
  VisibilityName: array [TJclClrTableFieldDefVisibility] of PChar =
    ('', 'private', 'famandassem', 'assembly', 'family', 'famandassem', 'public');
var
  I: Integer;

  function DumpFlags: string;
  const
    FlagName: array [ffInitOnly..ffRTSpecialName] of PChar =
      ('initonly', 'literal', 'notserialized', 'specialname', '', 'rtspecialname');
  var
    AFlag: TJclClrTableFieldDefFlag;
  begin
    for AFlag := Low(FlagName) to High(FlagName) do
      if AFlag in Flags then
        Result := Result + FlagName[AFlag] + ' ';
  end;

  function DumpSignature: string;
  begin
    with TJclClrSignature.Create(Signature) do
      try
        Result := UncompressFieldSignature;
      finally
        Free;
      end;
  end;

begin
  Result := Format('.field /*%.8x*/ %s%s %s%s %s', [Token,
    StaticName[ffStatic in Flags], VisibilityName[Visibility],
    DumpFlags, DumpSignature, Name]);

  if ffHasDefault in Flags then
  begin
    with TJclClrTableConstant(Table.Stream.Tables[ttConstant]) do
      for I := 0 to RowCount-1 do
        if Rows[I].Parent = Self then
        begin
          Result := Result + Rows[I].DumpIL;
          Break;
        end;
  end
  else
  if ffHasFieldRVA in Flags then
  begin
    { TODO : What to do? }
  end;
end;

function TJclClrTableFieldDefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableFieldDefRow.GetSignature: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

function TJclClrTableFieldDefRow.GetVisibility: TJclClrTableFieldDefVisibility;
const
  FieldVisibilityMapping: array [fdPrivateScope..fdPublic] of TJclClrTableFieldDefVisibility =
   (fvPrivateScope, fvPrivate, fvFamANDAssem, fvAssembly, fvFamily, fvFamORAssem, fvPublic);
begin
  Result := FieldVisibilityMapping[FFlags and fdFieldAccessMask];
end;

function TJclClrTableFieldDefRow.GetFlag: TJclClrTableFieldDefFlags;
const
  FieldFlagMapping: array [TJclClrTableFieldDefFlag] of Word =
    (fdStatic, fdInitOnly, fdLiteral, fdNotSerialized, fdSpecialName,
     fdPinvokeImpl, fdRTSpecialName, fdHasFieldMarshal, fdHasDefault, fdHasFieldRVA);
var
  AFlag: TJclClrTableFieldDefFlag;
begin
  for AFlag := Low(TJclClrTableFieldDefFlag) to High(TJclClrTableFieldDefFlag) do
    if FFlags and FieldFlagMapping[AFlag] <> 0 then
      Include(Result, AFlag);
end;

procedure TJclClrTableFieldDefRow.SetParentToken(const ARow: TJclClrTableTypeDefRow);
begin
  FParentToken := ARow;
end;

function TJclClrTableFieldDef.GetRow(const Idx: Integer): TJclClrTableFieldDefRow;
begin
  Result := TJclClrTableFieldDefRow(inherited GetRow(Idx));
end;

class function TJclClrTableFieldDef.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableFieldDefRow;
end;

//=== { TJclClrTableFieldPtrRow } ============================================

constructor TJclClrTableFieldPtrRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FFieldIdx := Table.ReadIndex([ttFieldDef]);
end;

function TJclClrTableFieldPtrRow.GetField: TJclClrTableFieldDefRow;
begin
  Result := TJclClrTableFieldDef(Table.Stream.Tables[ttFieldDef]).Rows[FFieldIdx-1];
end;

function TJclClrTableFieldPtr.GetRow(const Idx: Integer): TJclClrTableFieldPtrRow;
begin
  Result := TJclClrTableFieldPtrRow(inherited GetRow(Idx));
end;

class function TJclClrTableFieldPtr.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableFieldPtrRow;
end;

//=== { TJclClrTableFieldLayoutRow } =========================================

constructor TJclClrTableFieldLayoutRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FOffset   := Table.ReadDWord;
  FFieldIdx := Table.ReadIndex([ttFieldDef]);
end;

function TJclClrTableFieldLayout.GetRow(
  const Idx: Integer): TJclClrTableFieldLayoutRow;
begin
  Result := TJclClrTableFieldLayoutRow(inherited GetRow(Idx));
end;

class function TJclClrTableFieldLayout.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableFieldLayoutRow;
end;

//=== { TJclClrTableFieldMarshalRow } ========================================

constructor TJclClrTableFieldMarshalRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FParentIdx        := Table.ReadIndex([ttFieldDef, ttParamDef]);
  FNativeTypeOffset := Table.ReadIndex(hkBlob);
end;

function TJclClrTableFieldMarshal.GetRow(
  const Idx: Integer): TJclClrTableFieldMarshalRow;
begin
  Result := TJclClrTableFieldMarshalRow(inherited GetRow(Idx));
end;

class function TJclClrTableFieldMarshal.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableFieldMarshalRow;
end;

//=== { TJclClrTableFieldRVARow } ============================================

constructor TJclClrTableFieldRVARow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FRVA      := Table.ReadDWord;
  FFieldIdx := Table.ReadIndex([ttFieldDef]);
end;

function TJclClrTableFieldRVA.GetRow(const Idx: Integer): TJclClrTableFieldRVARow;
begin
  Result := TJclClrTableFieldRVARow(inherited GetRow(Idx));
end;

class function TJclClrTableFieldRVA.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableFieldRVARow;
end;

//=== { TJclClrTableFileRow } ================================================

constructor TJclClrTableFileRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FFlags           := Table.ReadDWord;
  FNameOffset      := Table.ReadIndex(hkString);
  FHashValueOffset := Table.ReadIndex(hkBlob);
end;

function TJclClrTableFileRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableFileRow.GetHashValue: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FHashValueOffset);
end;

function TJclClrTableFileRow.GetContainsMetadata: Boolean;
const
  ffContainsNoMetaData = $0001;
begin
  Result := (FFlags and ffContainsNoMetaData) = ffContainsNoMetaData;
end;

function TJclClrTableFileRow.DumpIL: string;

  function GetMetadataName: string;
  begin
    if not ContainsMetadata then
      Result := 'nometadata '
  end;

begin
  Result := HashValue.Dump('.file ' + GetMetadataName + Name + ' .hash = ');
end;

function TJclClrTableFile.GetRow(const Idx: Integer): TJclClrTableFileRow;
begin
  Result := TJclClrTableFileRow(inherited GetRow(Idx));
end;

class function TJclClrTableFile.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableFileRow;
end;

//=== { TJclClrTableImplMapRow } =============================================

constructor TJclClrTableImplMapRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FMappingFlags       := Table.ReadWord;
  FMemberForwardedIdx := Table.ReadIndex([ttFieldDef, ttMethodDef]);
  FImportNameOffset   := Table.ReadIndex(hkString);
  FImportScopeIdx     := Table.ReadIndex([ttModuleRef]);
end;

function TJclClrTableImplMapRow.GetImportName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FImportNameOffset);
end;

function TJclClrTableImplMap.GetRow(const Idx: Integer): TJclClrTableImplMapRow;
begin
  Result := TJclClrTableImplMapRow(inherited GetRow(Idx));
end;

class function TJclClrTableImplMap.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableImplMapRow;
end;

//=== { TJclClrTableInterfaceImplRow } =======================================

constructor TJclClrTableInterfaceImplRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FClassIdx     := Table.ReadIndex([ttTypeDef]);
  FInterfaceIdx := Table.ReadIndex([ttTypeDef, ttTypeRef, ttTypeSpec]);
end;

function TJclClrTableInterfaceImplRow.DumpIL: string;
begin
  if ImplInterface is TJclClrTableTypeRefRow then
    Result := TJclClrTableTypeRefRow(ImplInterface).DumpIL
  else
  if ImplInterface is TJclClrTableTypeDefRow then
    with TJclClrTableTypeDefRow(ImplInterface) do
      Result := Format('%s.%s/*%.8x*/', [Namespace, Name, Token])
  else
    Result := 'Unknown';
end;

function TJclClrTableInterfaceImplRow.GetImplClass: TJclClrTableRow;
begin
  Result := Table.Stream.Metadata.Tokens[FClassIdx];
end;

function TJclClrTableInterfaceImplRow.GetImplInterface: TJclClrTableRow;
begin
  Result := DecodeTypeDefOrRef(FInterfaceIdx);
end;

function TJclClrTableInterfaceImpl.GetRow(
  const Idx: Integer): TJclClrTableInterfaceImplRow;
begin
  Result := TJclClrTableInterfaceImplRow(inherited GetRow(Idx));
end;

class function TJclClrTableInterfaceImpl.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableInterfaceImplRow;
end;

//=== { TJclClrTableManifestResourceRow } ====================================

constructor TJclClrTableManifestResourceRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FOffset            := Table.ReadDWord;
  FFlags             := Table.ReadDWord;
  FNameOffset        := Table.ReadIndex(hkString);
  FImplementationIdx := Table.ReadIndex([ttFile, ttAssemblyRef]);
end;

function TJclClrTableManifestResourceRow.DumpIL: string;
const
  VisibilityName: array [TJclClrTableManifestResourceVisibility] of PChar =
    ('public', 'private');
var
  I: Integer;
  TblCustomAttribute: TJclClrTableCustomAttribute;
begin
  with TStringList.Create do
    try
      Add(Format('.mresource /*%.8x*/ %s %s', [Token, VisibilityName[Visibility], Name]));
      Add('(');

      if Table.Stream.FindTable(ttCustomAttribute, TJclClrTable(TblCustomAttribute)) then
        for I := 0 to TblCustomAttribute.RowCount-1 do
          if TblCustomAttribute.Rows[I].Parent = Self then
            Add('  ' + TblCustomAttribute.Rows[I].DumpIL);

      if FImplementationIdx <> 0 then
        if ImplementationRow is TJclClrTableAssemblyRefRow then
          Add('  .assembly extern ' +
            TJclClrTableAssemblyRefRow(ImplementationRow).Name)
        else
          Add(Format('  .file %s at %d',
            [TJclClrTableFileRow(ImplementationRow).Name, Offset]));
      Add(')');
      Result := Text;
    finally
      Free;
    end;
end;

function TJclClrTableManifestResourceRow.GetImplementationRow: TJclClrTableRow;
begin
  Result := Table.Stream.Metadata.Tokens[FImplementationIdx];
end;

function TJclClrTableManifestResourceRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableManifestResourceRow.GetVisibility: TJclClrTableManifestResourceVisibility;
begin
  for Result := Low(TJclClrTableManifestResourceVisibility) to High(TJclClrTableManifestResourceVisibility) do
    if (FFlags and mrVisibilityMask) = ManifestResourceVisibilityMapping[Result] then
      Exit;
  raise EJclMetadataError.CreateResFmt(@RsUnknownManifestResource, [FFlags and mrVisibilityMask]);
end;

function TJclClrTableManifestResource.GetRow(
  const Idx: Integer): TJclClrTableManifestResourceRow;
begin
  Result := TJclClrTableManifestResourceRow(inherited GetRow(Idx));
end;

class function TJclClrTableManifestResource.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableManifestResourceRow;
end;

//=== { TJclClrTableMemberRefRow } ===========================================

constructor TJclClrTableMemberRefRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FClassIdx     := Table.ReadIndex([ttTypeRef, ttModuleRef, ttMethodDef, ttTypeSpec, ttTypeDef]);
  FNameOffset   := Table.ReadIndex(hkString);
  FSignatureOffset := Table.ReadIndex(hkBlob);
end;

function TJclClrTableMemberRefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableMemberRefRow.GetSignature: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

function TJclClrTableMemberRefRow.GetParentClass: TJclClrTableRow;
const
  MapTagToTable: array [1..5] of TJclClrTableKind =
    (ttTypeRef, ttModuleRef, ttMethodDef, ttTypeSpec, ttTypeDef);
var
  WideIndex: Boolean;
begin
  WideIndex := Table.IsWideIndex([ttTypeRef, ttModuleRef, ttMethodDef, ttTypeSpec, ttTypeDef]);
  Assert(Table.GetCodedIndexTag(FClassIdx, 3, WideIndex) in [1..5]);
  Result := Table.Stream.Tables[
    MapTagToTable[Table.GetCodedIndexTag(FClassIdx, 3, WideIndex)]].
    Rows[Table.GetCodedIndexValue(FClassIdx, 3, WideIndex)-1];
end;

function TJclClrTableMemberRefRow.GetFullName: WideString;
var
  Row: TJclClrTableRow;
begin
  Row := GetParentClass;

  if Row is TJclClrTableTypeRefRow then
    Result := TJclClrTableTypeRefRow(Row).FullName
  else
  if Row is TJclClrTableModuleRow then
    Result := TJclClrTableModuleRow(Row).Name
  else
  if Row is TJclClrTableMethodDefRow then
    Result := TJclClrTableMethodDefRow(Row).FullName
  else
  if Row is TJclClrTableTypeSpecRow then
    Result := ''
  else
  if Row is TJclClrTableTypeDefRow then
    Result := TJclClrTableTypeDefRow(Row).FullName;

  Result := Result + '.' + Name;
end;

function TJclClrTableMemberRef.GetRow(const Idx: Integer): TJclClrTableMemberRefRow;
begin
  Result := TJclClrTableMemberRefRow(inherited GetRow(Idx));
end;

class function TJclClrTableMemberRef.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableMemberRefRow;
end;

//=== { TJclClrTableParamDefRow } ============================================

constructor TJclClrTableParamDefRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FFlagMask   := Table.ReadWord;
  FSequence   := Table.ReadWord;
  FNameOffset := Table.ReadIndex(hkString);

  FMethod     := nil;
  FFlags      := ParamFlags(FFlagMask);
end;

function TJclClrTableParamDefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

procedure TJclClrTableParamDefRow.SetMethod(const AMethod: TJclClrTableMethodDefRow);
begin
  FMethod := AMethod;
end;

class function TJclClrTableParamDefRow.ParamFlags(const AFlags: TJclClrParamKinds): Word;
var
  AFlag: TJclClrParamKind;
begin
  Result := 0;
  for AFlag := Low(TJclClrParamKind) to High(TJclClrParamKind) do
    if AFlag in AFlags then
      Result := Result or ClrParamKindMapping[AFlag];
end;

class function TJclClrTableParamDefRow.ParamFlags(const AFlags: Word): TJclClrParamKinds;
var
  AFlag: TJclClrParamKind;
begin
  Result := [];
  for AFlag := Low(TJclClrParamKind) to High(TJclClrParamKind) do
    if (AFlags and ClrParamKindMapping[AFlag]) = ClrParamKindMapping[AFlag] then
      Include(Result, AFlag);
end;

function TJclClrTableParamDefRow.DumpIL: string;
begin
  { TODO : What to do? }
end;

function TJclClrTableParamDef.GetRow(const Idx: Integer): TJclClrTableParamDefRow;
begin
  Result := TJclClrTableParamDefRow(inherited GetRow(Idx));
end;

class function TJclClrTableParamDef.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableParamDefRow;
end;

//=== { TJclClrTableParamPtrRow } ============================================

constructor TJclClrTableParamPtrRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FParamIdx := Table.ReadIndex([ttParamDef]);
end;

function TJclClrTableParamPtrRow.GetParam: TJclClrTableParamDefRow;
begin
  Result := TJclClrTableParamDef(Table.Stream.Tables[ttParamDef]).Rows[FParamIdx-1];
end;

function TJclClrTableParamPtr.GetRow(const Idx: Integer): TJclClrTableParamPtrRow;
begin
  Result := TJclClrTableParamPtrRow(inherited GetRow(Idx));
end;

class function TJclClrTableParamPtr.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableParamPtrRow;
end;

//=== { TJclClrExceptionHandler } ============================================

const
  // Indicates the format for the COR_ILMETHOD header
  CorILMethod_FormatShift     = 2;
  CorILMethod_FormatMask      = ((1 shl CorILMethod_FormatShift) - 1);

  CorILMethod_TinyFormat      = $0002;
  CorILMethod_FatFormat       = $0003;

  CorILMethod_TinyFormatEven  = $0002;
  CorILMethod_TinyFormatOdd   = $0006;

  CorILMethod_InitLocals      = $0010;
  CorILMethod_MoreSects       = $0008;

  CorILMethod_Sect_Reserved   = 0;
  CorILMethod_Sect_EHTable    = 1;
  CorILMethod_Sect_OptILTable = 2;

  CorILMethod_Sect_KindMask   = $3F; // The mask for decoding the type code
  CorILMethod_Sect_FatFormat  = $40; // fat format
  CorILMethod_Sect_MoreSects  = $80; // there is another attribute after this one

  COR_ILEXCEPTION_CLAUSE_NONE       = $0000; // This is a typed handler
  COR_ILEXCEPTION_CLAUSE_OFFSETLEN  = $0000; // Deprecated
  COR_ILEXCEPTION_CLAUSE_DEPRECATED = $0000; // Deprecated
  COR_ILEXCEPTION_CLAUSE_FILTER     = $0001; // If this bit is on, then this EH entry is for a filter
  COR_ILEXCEPTION_CLAUSE_FINALLY    = $0002; // This clause is a finally clause
  COR_ILEXCEPTION_CLAUSE_FAULT      = $0004; // Fault clause (finally that is called on exception only)

  ExceptionClauseFlags: array [TJclClrExceptionClauseFlag] of DWORD =
   (COR_ILEXCEPTION_CLAUSE_NONE, COR_ILEXCEPTION_CLAUSE_FILTER,
    COR_ILEXCEPTION_CLAUSE_FINALLY, COR_ILEXCEPTION_CLAUSE_FAULT);

constructor TJclClrExceptionHandler.Create(const EHClause: TImageCorILMethodSectEHClauseSmall);
begin
  FFlags               := EHClause.Flags;
  FTryBlock.Offset     := EHClause.TryOffset;
  FTryBlock.Length     := EHClause.TryLength;
  FHandlerBlock.Offset := EHClause.HandlerOffset;
  FHandlerBlock.Length := EHClause.HandlerLength;
  if (FFlags and COR_ILEXCEPTION_CLAUSE_FILTER) = COR_ILEXCEPTION_CLAUSE_FILTER then
  begin
    FClassToken   := 0;
    FFilterOffset := EHClause.FilterOffset;
  end
  else
  begin
    FClassToken   := EHClause.ClassToken;
    FFilterOffset := 0;
  end;
end;

constructor TJclClrExceptionHandler.Create(const EHClause: TImageCorILMethodSectEHClauseFat);
begin
  FFlags               := EHClause.Flags;
  FTryBlock.Offset     := EHClause.TryOffset;
  FTryBlock.Length     := EHClause.TryLength;
  FHandlerBlock.Offset := EHClause.HandlerOffset;
  FHandlerBlock.Length := EHClause.HandlerLength;
  if (FFlags and COR_ILEXCEPTION_CLAUSE_FILTER) = COR_ILEXCEPTION_CLAUSE_FILTER then
  begin
    FClassToken   := 0;
    FFilterOffset := EHClause.FilterOffset;
  end
  else
  begin
    FClassToken   := EHClause.ClassToken;
    FFilterOffset := 0;
  end;
end;

function TJclClrExceptionHandler.GetFlags: TJclClrExceptionClauseFlags;
var
  AFlag: TJclClrExceptionClauseFlag;
begin
  Result := [];
  for AFlag := Low(TJclClrExceptionClauseFlag) to High(TJclClrExceptionClauseFlag) do
    if (FFlags and ExceptionClauseFlags[AFlag]) = ExceptionClauseFlags[AFlag] then
      Include(Result, AFlag);
end;

//=== { TJclClrMethodBody } ==================================================

constructor TJclClrMethodBody.Create(const AMethod: TJclClrTableMethodDefRow);
var
  ILMethod: PImageCorILMethodHeader;
begin
  FMethod  := AMethod;
  FEHTable := TObjectList.Create;

  FLocalVarSign := nil;

  ILMethod := FMethod.Table.Stream.Metadata.Image.RvaToVa(FMethod.RVA);
  if (ILMethod.Tiny.Flags_CodeSize and CorILMethod_FormatMask) = CorILMethod_TinyFormat then
  begin
    FSize              := (ILMethod.Tiny.Flags_CodeSize shr CorILMethod_FormatShift) and ((1 shl 6) - 1);
    FCode              := Pointer(DWORD(ILMethod) + 1);
    FMaxStack          := 0;
    FLocalVarSignToken := 0;
  end
  else
  begin
    FSize              := ILMethod.Fat.CodeSize;
    FCode              := Pointer(DWORD(ILMethod) + (ILMethod.Fat.Flags_Size shr 12) * SizeOf(DWORD));
    FMaxStack          := ILMethod.Fat.MaxStack;
    FLocalVarSignToken := ILMethod.Fat.LocalVarSigTok;

    if IsBitSet(ILMethod.Fat.Flags_Size, CorILMethod_MoreSects) then
      ParseMoreSections(Pointer((DWORD(FCode) + FSize + 1) and not 1));
  end;
end;

destructor TJclClrMethodBody.Destroy;
begin
  FreeAndNil(FLocalVarSign);
  FreeAndNil(FEHTable);
  inherited Destroy;
end;

procedure TJclClrMethodBody.AddEHTable(EHTable: PImageCorILMethodSectEH);
var
  I, Count: Integer;
  FatFormat: Boolean;
begin
  FatFormat := IsBitSet( EHTable.Small.SectSmall.Kind, CorILMethod_Sect_FatFormat);
  if FatFormat then
    Count := ((EHTable.Fat.SectFat.Kind_DataSize shr 8) - SizeOf(DWORD)) div SizeOf(TImageCorILMethodSectEHClauseFat)
  else
    Count := (EHTable.Small.SectSmall.Datasize - SizeOf(DWORD)) div SizeOf(TImageCorILMethodSectEHClauseSmall);

  for I := 0 to Count-1 do
  begin
    if FatFormat then
      FEHTable.Add(TJclClrExceptionHandler.Create(EHTable.Fat.Clauses[I]))
    else
      FEHTable.Add(TJclClrExceptionHandler.Create(EHTable.Small.Clauses[I]));
  end;
end;

procedure TJclClrMethodBody.AddOptILTable(OptILTable: Pointer; Size: Integer);
begin
  { TODO : What to do? }
end;

procedure TJclClrMethodBody.ParseMoreSections(SectHeader: PImageCorILMethodSectHeader);
var
  SectSize: DWORD;
begin
  if IsBitSet(SectHeader.Small.Kind, CorILMethod_Sect_FatFormat) then
    SectSize := SectHeader.Fat.Kind_DataSize shr 8
  else
    SectSize := SectHeader.Small.Datasize;

  if IsBitSet(SectHeader.Small.Kind, CorILMethod_Sect_EHTable) then
    AddEHTable(PImageCorILMethodSectEH(SectHeader))
  else
  if IsBitSet(SectHeader.Small.Kind, CorILMethod_Sect_OptILTable) then
    AddOptILTable(Pointer(DWORD(FCode) + FSize), SectSize);

  if IsBitSet(SectHeader.Small.Kind, CorILMethod_Sect_MoreSects) then
    ParseMoreSections(Pointer(DWORD(SectHeader) + SectSize));
end;

function TJclClrMethodBody.GetExceptionHandler(const Idx: Integer): TJclClrExceptionHandler;
begin
  Result := TJclClrExceptionHandler(FEHTable.Items[Idx]);
end;

function TJclClrMethodBody.GetExceptionHandlerCount: Integer;
begin
  Result := FEHTable.Count;
end;

function TJclClrMethodBody.GetLocalVarSign: TJclClrLocalVarSign;
begin
  if not Assigned(FLocalVarSign) and (FLocalVarSignToken <> 0) then
    FLocalVarSign := TJclClrLocalVarSign.Create(LocalVarSignData);

  Result := FLocalVarSign;
end;

function TJclClrMethodBody.GetLocalVarSignData: TJclClrBlobRecord;
begin
  Result := TJclClrTableStandAloneSigRow(FMethod.Table.Stream.Metadata.Tokens[FLocalVarSignToken]).Signature;
end;

//=== { TJclClrTableMethodDefRow } ===========================================

constructor TJclClrTableMethodDefRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);

  FRVA          := Table.ReadDWord;
  FImplFlags    := Table.ReadWord;
  FFlags        := Table.ReadWord;
  FNameOffset   := Table.ReadIndex(hkString);
  FSignatureOffset := Table.ReadIndex(hkBlob);
  FParamListIdx := Table.ReadIndex([ttParamDef]);
  
  FParentToken  := nil;
  FParams       := nil;
  FSignature    := nil;

  if FRVA <> 0 then
    FMethodBody := TJclClrMethodBody.Create(Self)
  else
    FMethodBody := nil;
end;

destructor TJclClrTableMethodDefRow.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FSignature);
  inherited Destroy;
end;

function TJclClrTableMethodDefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableMethodDefRow.GetSignatureData: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

procedure TJclClrTableMethodDefRow.SetParentToken(const ARow: TJclClrTableTypeDefRow);
begin
  FParentToken := ARow;
end;

procedure TJclClrTableMethodDefRow.UpdateParams;
var
  ParamTable: TJclClrTableParamDef;
  Idx, MaxParamListIdx: DWORD;
begin
  with Table as TJclClrTableMethodDef do
    if not Assigned(FParams) and (ParamListIdx <> 0) and
      Stream.FindTable(ttParamDef, TJclClrTable(ParamTable)) then
    begin
      if RowCount > (Index+1) then
        MaxParamListIdx := Rows[Index+1].ParamListIdx-1
      else
        MaxParamListIdx := ParamTable.RowCount;
      if (ParamListIdx-1) < MaxParamListIdx then
      begin
        FParams := TList.Create;
        for Idx := ParamListIdx-1 to MaxParamListIdx-1 do
        begin
          FParams.Add(ParamTable.Rows[Idx]);
          ParamTable.Rows[Idx].SetMethod(Self);
        end;
      end;
    end;
end;

procedure TJclClrTableMethodDefRow.Update;
begin
  UpdateParams;
end;
              
function TJclClrTableMethodDefRow.GetHasParam: Boolean;
begin
  Result := Assigned(FParams);
end;

function TJclClrTableMethodDefRow.GetParam(const Idx: Integer): TJclClrTableParamDefRow;
begin
  Result := TJclClrTableParamDefRow(FParams.Items[Idx]);
end;

function TJclClrTableMethodDefRow.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

function TJclClrTableMethodDefRow.DumpIL: string;
const
  MemberAccessNames: array [TJclClrMemberAccess] of PChar =
    ('compilercontrolled', 'private', 'famandassem',
     'assembly', 'family', 'famorassem', 'public');
  CodeTypeNames: array [TJclClrMethodCodeType] of PChar =
    ('cil', 'native', 'optil', 'runtime');
  ManagedNames: array [Boolean] of PChar =
    ('unmanaged', 'managed');
var
  I: Integer;

  function LocalVarToString(LocalVar: TJclClrLocalVar): string;
  var
    Row: TJclClrTableRow;
  begin
    case LocalVar.ElementType of
      etClass:
        if LocalVar.Token <> 0 then
        begin
          Row := Table.Stream.Metadata.Tokens[LocalVar.Token];
          if Row is TJclClrTableTypeDefRow then
            Result := TJclClrTableTypeDefRow(Row).FullName
          else
          if Row is TJclClrTableTypeRefRow then
            Result := TJclClrTableTypeRefRow(Row).FullName
          else
          if Row is TJclClrTableTypeSpecRow then
            Result := TJclClrTableTypeSpecRow(Row).Signature.Dump('')
          else
            Result := '/*' + IntToHex(Row.Token, 8) + '*/';
        end;
    else
      Result := LocalVar.Name;
    end;
  end;

  function GetMethodFlagDescription: string;
  const
    MethodFlagName: array [TJclClrMethodFlag] of PChar =
      ('static', 'final', 'virtual', 'hidebysig', '', 'abstract',
       'specialname', 'pinvokeimpl', 'unmanagedexp', 'rtspecialname', '', '');
  var
    AFlag: TJclClrMethodFlag;
  begin
    for AFlag := Low(TJclClrMethodFlag) to High(TJclClrMethodFlag) do
      if AFlag in MethodFlags then
        Result := Result + MethodFlagName[AFlag] + ' ';
  end;

  function GetMethodImplFlagDescription: string;
  const
    MethodImplFlagName: array [TJclClrMethodImplFlag] of PChar =
      ('forwardref', '', 'internalcall', 'synchronized', 'noinlining');
  var
    AFlag: TJclClrMethodImplFlag;
  begin
    for AFlag := Low(TJclClrMethodImplFlag) to High(TJclClrMethodImplFlag) do
      if AFlag in MethodImplFlags then
        Result := Result + ' ' + MethodImplFlagName[AFlag];
  end;

  function GetParamTypeName(Param: TJclClrMethodParam): string;
  const
    BuildInTypeNames: array [etVoid..etString] of PChar =
      ('void', 'bool', 'char', 'sbyte', 'byte', 'short', 'ushort',
       'int', 'uint', 'long', 'ulong', 'float', 'double', 'string');
  var
    Row: TJclClrTableRow;
  begin
    case Param.ElementType of
      etVoid, etBoolean, etChar,
      etI1, etU1, etI2, etU2, etI4, etU4,
      etI8, etU8, etR4, etR8, etString:
        Result := BuildInTypeNames[Param.ElementType];
      etI:
        Result := 'System.IntPtr';
      etU:
        Result := 'System.UIntPtr';
      etObject:
        Result := 'object';
      etClass:
        begin
          Row := Table.Stream.Metadata.Tokens[Param.Token];
          if Row is TJclClrTableTypeDefRow then
            Result := TJclClrTableTypeDefRow(Row).FullName
          else
          if Row is TJclClrTableTypeRefRow then
            Result := TJclClrTableTypeRefRow(Row).FullName;

          Result := Result + ' /* ' + IntToHex(Param.Token, 8) + ' */';
        end;
      etSzArray:
        Result := 'char *';
    end;
    if Param.ByRef then
      Result := 'ref ' + Result;
  end;

begin
  Result := Format('.method /*%.8x*/ %s %s%s %s(', [Token,
    MemberAccessNames[MemberAccess], GetMethodFlagDescription,
    GetParamTypeName(Signature.RetType), Name]);
  if HasParam then
    for I := 0 to Min(ParamCount, Signature.ParamCount)-1 do
    begin
      Result := Result + GetParamTypeName(Signature.Params[I]) + ' ' + Params[I].Name;
      if I <> ParamCount-1 then
        Result := Result + ', ';
    end;
  Result := Result + ') ' + CodeTypeNames[CodeType] + ' ' + ManagedNames[Managed] + GetMethodImplFlagDescription;

  if Assigned(MethodBody) then
  begin
    Result := Result + AnsiLineBreak + '{' + AnsiLineBreak +
      '.maxstack ' + IntToStr(MethodBody.MaxStack) + AnsiLineBreak;

    if MethodBody.LocalVarSignToken <> 0 then
    begin
      Result := Result + '.locals /* ' + IntToHex(MethodBody.LocalVarSignToken, 8) + ' */ init(' + AnsiLineBreak;
      for I := 0 to MethodBody.LocalVarSign.LocalVarCount-1 do
      begin
        Result := Format(Result+'  %s V_%d', [LocalVarToString(MethodBody.LocalVarSign.LocalVars[I]), I]);
        if I = MethodBody.LocalVarSign.LocalVarCount-1 then
          Result := Result + ')' + AnsiLineBreak
        else
          Result := Result + ',' + AnsiLineBreak;
      end;
    end;

    with TJclClrILGenerator.Create(MethodBody) do
      try
        Result := Result + AnsiLineBreak + DumpIL(InstructionDumpILAllOption);
      finally
        Free;
      end;
    Result := Result + '}';
  end;
end;

function TJclClrTableMethodDefRow.GetFullName: WideString;
begin
  Result := ParentToken.FullName + '.' + Name;
end;

function TJclClrTableMethodDefRow.GetSignature: TJclClrMethodSign;
begin
  if not Assigned(FSignature) then
    FSignature := TJclClrMethodSign.Create(SignatureData);
  Result := FSignature;
end;

function TJclClrTableMethodDefRow.GetMemberAccess: TJclClrMemberAccess;
begin
  Result := TJclClrMemberAccess(FFlags and mdMemberAccessMask)
end;

function TJclClrTableMethodDefRow.GetMethodFlags: TJclClrMethodFlags;
var
  AFlag: TJclClrMethodFlag;
begin
  for AFlag := Low(TJclClrMethodFlag) to High(TJclClrMethodFlag) do
    if (FFlags and ClrMethodFlagMapping[AFlag]) = ClrMethodFlagMapping[AFlag] then
      Include(Result, AFlag);
end;

function TJclClrTableMethodDefRow.GetNewSlot: Boolean;
begin
  Result := (FFlags and mdVtableLayoutMask) = mdNewSlot;
end;

function TJclClrTableMethodDefRow.GetCodeType: TJclClrMethodCodeType;
begin
  Result := TJclClrMethodCodeType(FImplFlags and miCodeTypeMask);
end;

function TJclClrTableMethodDefRow.GetManaged: Boolean;
begin
  Result := (FImplFlags and miManagedMask) = miManaged;
end;

function TJclClrTableMethodDefRow.GetMethodImplFlags: TJclClrMethodImplFlags;
var
  AFlag: TJclClrMethodImplFlag;
begin
  for AFlag := Low(TJclClrMethodImplFlag) to High(TJclClrMethodImplFlag) do
    if (FFlags and ClrMethodImplFlagMapping[AFlag]) = ClrMethodImplFlagMapping[AFlag] then
      Include(Result, AFlag);
end;

function TJclClrTableMethodDef.GetRow(const Idx: Integer): TJclClrTableMethodDefRow;
begin
  Result := TJclClrTableMethodDefRow(inherited GetRow(Idx));
end;

class function TJclClrTableMethodDef.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableMethodDefRow;
end;

//=== { TJclClrTableMethodPtrRow } ===========================================

constructor TJclClrTableMethodPtrRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FMethodIdx := Table.ReadIndex([ttMethodDef]);
end;

function TJclClrTableMethodPtrRow.GetMethod: TJclClrTableMethodDefRow;
begin
  Result := TJclClrTableMethodDef(Table.Stream.Tables[ttMethodDef]).Rows[FMethodIdx-1];
end;

function TJclClrTableMethodPtr.GetRow(const Idx: Integer): TJclClrTableMethodPtrRow;
begin
  Result := TJclClrTableMethodPtrRow(inherited GetRow(Idx));
end;

class function TJclClrTableMethodPtr.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableMethodPtrRow;
end;

//=== { TJclClrTableMethodImplRow } ==========================================

constructor TJclClrTableMethodImplRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FClassIdx             := Table.ReadIndex([ttTypeDef]);
  FMethodBodyIdx        := Table.ReadIndex([ttMethodDef, ttMemberRef]);
  FMethodDeclarationIdx := Table.ReadIndex([ttMethodDef, ttMemberRef]);
end;

function TJclClrTableMethodImpl.GetRow(
  const Idx: Integer): TJclClrTableMethodImplRow;
begin
  Result := TJclClrTableMethodImplRow(inherited GetRow(Idx));
end;

class function TJclClrTableMethodImpl.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableMethodImplRow;
end;

//=== { TJclClrTableMethodSemanticsRow } =====================================

constructor TJclClrTableMethodSemanticsRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FSemantics      := Table.ReadWord;
  FMethodIdx      := Table.ReadIndex([ttMethodDef]);
  FAssociationIdx := Table.ReadIndex([ttEventDef, ttPropertyDef]);
end;

function TJclClrTableMethodSemantics.GetRow(const Idx: Integer): TJclClrTableMethodSemanticsRow;
begin
  Result := TJclClrTableMethodSemanticsRow(inherited GetRow(Idx));
end;

class function TJclClrTableMethodSemantics.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableMethodSemanticsRow;
end;

//=== { TJclClrTableMethodSpecRow } ==========================================

constructor TJclClrTableMethodSpecRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FMethodIdx           := Table.ReadIndex([ttMethodDef, ttMemberRef]);
  FInstantiationOffset := Table.ReadIndex(hkBlob);
end;

function TJclClrTableMethodSpecRow.GetMethod: TJclClrTableRow;
const
  MethodDefOrRefEncodedTag: array [0..1] of TJclClrTableKind =
    (ttMethodDef, ttMemberRef);
begin
  Result := Table.Stream.Metadata.Tables[MethodDefOrRefEncodedTag[FMethodIdx and 1]].Rows[FMethodIdx shr 1];
end;

function TJclClrTableMethodSpecRow.GetInstantiation: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FInstantiationOffset);
end;

function TJclClrTableMethodSpec.GetRow(const Idx: Integer): TJclClrTableMethodSpecRow;
begin
  Result := TJclClrTableMethodSpecRow(inherited GetRow(Idx));
end;

class function TJclClrTableMethodSpec.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableMethodSpecRow;
end;

//=== { TJclClrTableNestedClassRow } =========================================

constructor TJclClrTableNestedClassRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FNestedClassIdx    := Table.ReadIndex([ttTypeDef]);
  FEnclosingClassIdx := Table.ReadIndex([ttTypeDef]);
end;

function TJclClrTableNestedClass.GetRow(const Idx: Integer): TJclClrTableNestedClassRow;
begin
  Result := TJclClrTableNestedClassRow(inherited GetRow(Idx));
end;

class function TJclClrTableNestedClass.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableNestedClassRow;
end;

//=== { TJclClrTablePropertyDefRow } =========================================

constructor TJclClrTablePropertyDefRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FFlags      := Table.ReadWord;
  FNameOffset := Table.ReadIndex(hkString);
  FKindIdx    := Table.ReadIndex(hkBlob);
end;

function TJclClrTablePropertyDefRow.DumpIL: string;

  function DumpFlags: string;
  const
    SpecialName: array [Boolean] of string = ('', 'specialname ');
    RTSpecialName: array [Boolean] of string = ('', 'rtspecialname ');
  begin
    Result := SpecialName[pfSpecialName in Flags] +
      RTSpecialName[pfRTSpecialName in Flags];
  end;

begin
  Result := Format('.property /*%.8x*/ %s%s ()', [Token, DumpFlags, Name]);
end;

function TJclClrTablePropertyDefRow.GetFlags: TJclClrTablePropertyFlags;
var
  AFlag: TJclClrTablePropertyFlag;
begin
  for AFlag := Low(TJclClrTablePropertyFlag) to High(TJclClrTablePropertyFlag) do
    if ClrTablePropertyFlagMapping[AFlag] and FFlags <> 0 then
      Include(Result, AFlag);
end;

function TJclClrTablePropertyDefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTablePropertyDef.GetRow(const Idx: Integer): TJclClrTablePropertyDefRow;
begin
  Result := TJclClrTablePropertyDefRow(inherited GetRow(Idx));
end;

class function TJclClrTablePropertyDef.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTablePropertyDefRow;
end;

//=== { TJclClrTablePropertyPtrRow } =========================================

constructor TJclClrTablePropertyPtrRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FPropertyIdx := Table.ReadIndex([ttPropertyDef]);
end;

function TJclClrTablePropertyPtrRow.GetProperty: TJclClrTablePropertyDefRow;
begin
  Result := TJclClrTablePropertyDef(Table.Stream.Tables[ttPropertyDef]).Rows[FPropertyIdx-1];
end;

function TJclClrTablePropertyPtr.GetRow(const Idx: Integer): TJclClrTablePropertyPtrRow;
begin
  Result := TJclClrTablePropertyPtrRow(inherited GetRow(Idx));
end;

class function TJclClrTablePropertyPtr.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTablePropertyPtrRow;
end;

//=== { TJclClrTablePropertyMapRow } =========================================

constructor TJclClrTablePropertyMapRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FParentIdx       := Table.ReadIndex([ttTypeDef]);
  FPropertyListIdx := Table.ReadIndex([ttPropertyDef]);
  FProperties      := TList.Create;
end;

destructor TJclClrTablePropertyMapRow.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

function TJclClrTablePropertyMapRow.GetParent: TJclClrTableTypeDefRow;
begin
  Result := TJclClrTableTypeDef(Table.Stream.Tables[ttTypeDef]).Rows[FParentIdx-1];
end;

function TJclClrTablePropertyMapRow.Add(const ARow: TJclClrTablePropertyDefRow): Integer;
begin
  Result := FProperties.Add(ARow);
end;

function TJclClrTablePropertyMapRow.GetProperty(const Idx: Integer): TJclClrTablePropertyDefRow;
begin
  Result := TJclClrTablePropertyDefRow(FProperties.Items[Idx]);
end;

function TJclClrTablePropertyMapRow.GetPropertyCount: Integer;
begin
  Result := FProperties.Count;
end;

function TJclClrTablePropertyMap.GetRow(const Idx: Integer): TJclClrTablePropertyMapRow;
begin
  Result := TJclClrTablePropertyMapRow(inherited GetRow(Idx));
end;

class function TJclClrTablePropertyMap.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTablePropertyMapRow;
end;

procedure TJclClrTablePropertyMap.Update;
var
  I, J: Integer;
begin
  J := 0;
  with TJclClrTablePropertyDef(Stream.Tables[ttPropertyDef]) do
    for I := 0 to RowCount-1 do
    begin
      if I >= Integer(Self.Rows[J].PropertyListIdx) then
        Inc(J);
      if J >= Self.RowCount then
        Break;
      Self.Rows[J].Add(Rows[I]);
    end;
end;

//=== { TJclClrTableStandAloneSigRow } =======================================

constructor TJclClrTableStandAloneSigRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FSignatureOffset := Table.ReadIndex(hkBlob);
end;

function TJclClrTableStandAloneSigRow.GetSignature: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

function TJclClrTableStandAloneSig.GetRow(
  const Idx: Integer): TJclClrTableStandAloneSigRow;
begin
  Result := TJclClrTableStandAloneSigRow(inherited GetRow(Idx));
end;

class function TJclClrTableStandAloneSig.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableStandAloneSigRow;
end;

//=== { TJclClrTableTypeDefRow } =============================================

constructor TJclClrTableTypeDefRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FFlags           := Table.ReadDWord;
  FNameOffset      := Table.ReadIndex(hkString);
  FNamespaceOffset := Table.ReadIndex(hkString);
  FExtendsIdx      := Table.ReadIndex([ttTypeDef, ttTypeRef, ttTypeSpec]);
  FFieldListIdx    := Table.ReadIndex([ttFieldDef]);
  FMethodListIdx   := Table.ReadIndex([ttMethodDef]);

  FFields := nil;
  FMethods := nil;
end;

destructor TJclClrTableTypeDefRow.Destroy;
begin
  FreeAndNil(FFields);
  FreeAndNil(FMethods);
  inherited Destroy;
end;

function TJclClrTableTypeDefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableTypeDefRow.GetNamespace: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNamespaceOffset);
end;

function TJclClrTableTypeDefRow.GetField(const Idx: Integer): TJclClrTableFieldDefRow;
begin
  Result := TJclClrTableFieldDefRow(FFields.Items[Idx])
end;

function TJclClrTableTypeDefRow.GetFieldCount: Integer;
begin
  Result := FFields.Count
end;

function TJclClrTableTypeDefRow.HasField: Boolean;
begin
  Result := Assigned(FFields);
end;

function TJclClrTableTypeDefRow.GetMethod(const Idx: Integer): TJclClrTableMethodDefRow;
begin
  Result := TJclClrTableMethodDefRow(FMethods.Items[Idx])
end;

function TJclClrTableTypeDefRow.GetMethodCount: Integer;
begin
  Result := FMethods.Count
end;

function TJclClrTableTypeDefRow.HasMethod: Boolean;
begin
  Result := Assigned(FMethods);
end;

procedure TJclClrTableTypeDefRow.UpdateFields;
var
  FieldTable: TJclClrTableFieldDef;
  Idx, MaxFieldListIdx: DWORD;
begin
  with Table as TJclClrTableTypeDef do
    if not Assigned(FFields) and (FieldListIdx <> 0) and
      Stream.FindTable(ttFieldDef, TJclClrTable(FieldTable)) then
    begin
      if RowCount > (Index+1) then
        MaxFieldListIdx := Rows[Index+1].FieldListIdx-1
      else
        MaxFieldListIdx := FieldTable.RowCount;
      if (FieldListIdx-1) < MaxFieldListIdx then
      begin
        FFields := TList.Create;
        for Idx := FieldListIdx-1 to MaxFieldListIdx-1 do
        begin
          FFields.Add(FieldTable.Rows[Idx]);
          FieldTable.Rows[Idx].SetParentToken(Self);
        end;
      end;
    end;
end;

procedure TJclClrTableTypeDefRow.UpdateMethods;
var
  MethodTable: TJclClrTableMethodDef;
  Idx, MaxMethodListIdx: DWORD;
begin
  with Table as TJclClrTableTypeDef do
    if not Assigned(FMethods) and (MethodListIdx <> 0) and
      Stream.FindTable(ttMethodDef, TJclClrTable(MethodTable)) then
    begin
      if RowCount > (Index+1) then
        MaxMethodListIdx := Rows[Index+1].MethodListIdx-1
      else
        MaxMethodListIdx := MethodTable.RowCount;
      if (MethodListIdx-1) < MaxMethodListIdx then
      begin
        FMethods := TList.Create;
        for Idx := MethodListIdx-1 to MaxMethodListIdx-1 do
        begin
          FMethods.Add(MethodTable.Rows[Idx]);
          MethodTable.Rows[Idx].SetParentToken(Self);
        end;
      end;
    end;
end;

procedure TJclClrTableTypeDefRow.Update;
begin
  inherited Update;
  UpdateFields;
  UpdateMethods;
end;

function TJclClrTableTypeDefRow.GetFullName: WideString;
begin
  if FNamespaceOffset <> 0 then
    Result := Namespace + '.' + Name
  else
    Result := Name;
end;

function TJclClrTableTypeDefRow.GetAttributes: TJclClrTypeAttributes;
const
  TypeAttributesMapping: array [TJclClrTypeAttribute] of DWORD =
    (tdAbstract, tdSealed, tdSpecialName, tdImport,
     tdSerializable, tdBeforeFieldInit, tdRTSpecialName, tdHasSecurity);
var
  Attr: TJclClrTypeAttribute;
begin
  Result := [];
  for Attr := Low(TJclClrTypeAttribute) to High(TJclClrTypeAttribute) do
    if (FFlags and TypeAttributesMapping[Attr]) = TypeAttributesMapping[Attr] then
      Include(Result, Attr);
end;

function TJclClrTableTypeDefRow.GetClassLayout: TJclClrClassLayout;
begin
  case FFlags and tdLayoutMask of
    tdAutoLayout:
      Result := clAuto;
    tdSequentialLayout:
      Result := clSequential;
    tdExplicitLayout:
      Result := clExplicit;
  else
    raise EJclMetadataError.CreateResFmt(@RsUnknownClassLayout, [FFlags and tdLayoutMask]);
  end;
end;

function TJclClrTableTypeDefRow.GetClassSemantics: TJclClrClassSemantics;
const
  ClassSemanticsMapping: array [Boolean] of TJclClrClassSemantics =
    (csClass, csInterface);
begin
  Result := ClassSemanticsMapping[(FFlags and tdClassSemanticsMask) = tdInterface];
end;

function TJclClrTableTypeDefRow.GetStringFormatting: TJclClrStringFormatting;
begin
  case FFlags and tdStringFormatMask of
    tdAnsiClass:
      Result := sfAnsi;
    tdUnicodeClass:
      Result := sfUnicode;
    tdAutoClass:
      Result := sfAutoChar;
  else
    raise EJclMetadataError.CreateResFmt(@RsUnknownStringFormatting, [FFlags and tdStringFormatMask]);
  end;
end;

function TJclClrTableTypeDefRow.GetVisibility: TJclClrTypeVisibility;
begin
  Result := TJclClrTypeVisibility(FFlags and tdVisibilityMask);
end;

function TJclClrTableTypeDefRow.GetExtends: TJclClrTableRow;
begin
  Result := DecodeTypeDefOrRef(FExtendsIdx);
end;

function TJclClrTableTypeDefRow.DumpIL: string;
const
  ClassSemanticName: array [TJclClrClassSemantics] of PChar =
    ('class', 'interface');
  VisibilityName: array [TJclClrTypeVisibility] of PChar =
    ('private', 'public', 'nested public', 'nested private',
     'nested family', 'nested assembly', 'nested famandassem', 'nested famorassem');
  ClassLayoutName: array [TJclClrClassLayout] of PChar =
    ('auto', 'explicit', 'sequential');
  StringFormattingName: array [TJclClrStringFormatting] of PChar =
    ('ansi', 'unicode', 'autoChar');
  TypeAttributeName: array [TJclClrTypeAttribute] of PChar =
    ('abstract', 'sealed', 'specialname', '' {'import'},
     'serializable', 'beforefieldinit', 'rtspecialname', '' {'hassecurity'});
  Indent = '  ';
  IntfPrefix: array [Boolean] of PChar = ('           ', 'implements ');
var
  I, J: Integer;
  ListIntfs: TList;

  function GetTypeAttributesName: string;
  var
    Attr: TJclClrTypeAttribute;
  begin
    for Attr := Low(TJclClrTypeAttribute) to High(TJclClrTypeAttribute) do
      if Attr in Attributes then
        Result := Result + TypeAttributeName[Attr] + ' ';
  end;

  function GetExtends(const Row: TJclClrTableTypeDefRow): string; overload;
  begin
    Result := Format('%s.%s/* %.8x */', [Row.Namespace, Row.Name, Row.Token]);
  end;

  function GetExtends(const Row: TJclClrTableRow): string; overload;
  begin
    if Row is TJclClrTableTypeDefRow then
      Result := GetExtends(TJclClrTableTypeDefRow(Row))
    else
    if Row is TJclClrTableTypeRefRow then
      Result := TJclClrTableTypeRefRow(Row).DumpIL
    else
    if Row is TJclClrTableTypeSpecRow then
      Result := TJclClrTableTypeSpecRow(Row).DumpIL
    else
      Result := 'Unknown Extends ' + Row.ClassName;
  end;

begin
  with TStringList.Create do
    try
      Add(Format('.%s /*%.8x*/ %s %s %s %s%s.%s',
        [ClassSemanticName[ClassSemantics], Token,
         VisibilityName[Visibility], ClassLayoutName[ClassLayout],
         StringFormattingName[StringFormatting], GetTypeAttributesName,
         Namespace, Name]));

      if ExtendsIdx <> 0 then
        Add(Indent + 'extends ' + GetExtends(Extends));

      ListIntfs := TList.Create;
      try
        if Assigned(Table.Stream.Tables[ttInterfaceImpl]) then
          with TJclClrTableInterfaceImpl(Table.Stream.Tables[ttInterfaceImpl]) do
            for I := 0 to RowCount-1 do
              if Rows[I].ClassIdx = DWORD(Index + 1) then
                ListIntfs.Add(Rows[I]);

        if ListIntfs.Count > 0 then
          for I := 0 to ListIntfs.Count-1 do
            Add(Indent + IntfPrefix[I = 0] + TJclClrTableInterfaceImplRow(ListIntfs[I]).DumpIL);
      finally
        ListIntfs.Free;
      end;

      Add('(');

      if HasField then
      for I := 0 to FieldCount-1 do
        Add(Indent + Fields[I].DumpIL);

      if HasMethod then
      for I := 0 to MethodCount-1 do
        Add(Indent + Methods[I].DumpIL);

      if Assigned(Table.Stream.Tables[ttPropertyMap]) then
        with TJclClrTablePropertyMap(Table.Stream.Tables[ttPropertyMap]) do
          for I := 0 to RowCount-1 do
            if Rows[I].Parent = Self then
              for J := 0 to Rows[I].PropertyCount-1 do
                Add(Indent + Rows[I].Properties[J].DumpIL);

      Add(') // end of class ' + Name);
      Result := Text;
    finally
      Free;
    end;
end;

class function TJclClrTableTypeDef.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableTypeDefRow;
end;

function TJclClrTableTypeDef.GetRow(const Idx: Integer): TJclClrTableTypeDefRow;
begin
  Result := TJclClrTableTypeDefRow(inherited GetRow(Idx));
end;

//=== { TJclClrTableTypeRefRow } =============================================

constructor TJclClrTableTypeRefRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FResolutionScopeIdx := Table.ReadIndex([ttModule, ttModuleRef, ttAssemblyRef, ttTypeRef]);
  FNameOffset         := Table.ReadIndex(hkString);
  FNamespaceOffset    := Table.ReadIndex(hkString);
end;

function TJclClrTableTypeRefRow.DumpIL: string;
begin
  Result := Format('[%s/* %.8x */]%s.%s/* %.8x */',
    [ResolutionScopeName, ResolutionScope.Token, Namespace, Name, Token]);
end;

function TJclClrTableTypeRefRow.GetFullName: WideString;
begin
  Result := Namespace + '.' + Name;
end;

function TJclClrTableTypeRefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableTypeRefRow.GetNamespace: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNamespaceOffset);
end;

function TJclClrTableTypeRefRow.GetResolutionScope: TJclClrTableRow;
begin
  Result := DecodeResolutionScope(FResolutionScopeIdx);
end;

function TJclClrTableTypeRefRow.GetResolutionScopeName: string;
begin
  if ResolutionScope is TJclClrTableModuleRow then
    Result := TJclClrTableModuleRow(ResolutionScope).Name
  else
  if ResolutionScope is TJclClrTableModuleRefRow then
    Result := TJclClrTableModuleRefRow(ResolutionScope).Name
  else
  if ResolutionScope is TJclClrTableAssemblyRefRow then
    Result := TJclClrTableAssemblyRefRow(ResolutionScope).Name
  else
  if ResolutionScope is TJclClrTableTypeRefRow then
    Result := TJclClrTableTypeRefRow(ResolutionScope).Namespace + '.' +
      TJclClrTableTypeRefRow(ResolutionScope).Name
  else
    Result := 'Unknown';
end;

class function TJclClrTableTypeRef.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableTypeRefRow;
end;

function TJclClrTableTypeRef.GetRow(const Idx: Integer): TJclClrTableTypeRefRow;
begin
  Result := TJclClrTableTypeRefRow(inherited GetRow(Idx));
end;

//=== { TJclClrTableTypeSpecRow } ============================================

constructor TJclClrTableTypeSpecRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FSignatureOffset := Table.ReadIndex(hkBlob);
end;

function TJclClrTableTypeSpecRow.GetSignature: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

function TJclClrTableTypeSpec.GetRow(const Idx: Integer): TJclClrTableTypeSpecRow;
begin
  Result := TJclClrTableTypeSpecRow(inherited GetRow(Idx));
end;

class function TJclClrTableTypeSpec.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableTypeSpecRow;
end;

//=== { TJclClrTableENCMapRow } ==============================================

constructor TJclClrTableENCMapRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FToken := Table.ReadDWord;
end;

function TJclClrTableENCMap.GetRow(const Idx: Integer): TJclClrTableENCMapRow;
begin
  Result := TJclClrTableENCMapRow(inherited GetRow(Idx));
end;

class function TJclClrTableENCMap.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableENCMapRow;
end;

//=== { TJclClrTableENCLogRow } ==============================================

constructor TJclClrTableENCLogRow.Create(const ATable: TJclClrTable);
begin
  inherited Create(ATable);
  FFuncCode := Table.ReadDWord;
end;

function TJclClrTableENCLog.GetRow(const Idx: Integer): TJclClrTableENCLogRow;
begin
  Result := TJclClrTableENCLogRow(inherited GetRow(Idx));
end;

class function TJclClrTableENCLog.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableENCLogRow;
end;

function TJclClrLocalVar.GetName: WideString;
const
  ClrElementTypeNameMapping: array [etVoid..etString] of PChar =
    ('void', 'bool',
     'char', 'sbyte', 'byte',
     'short', 'ushort', 'int', 'unit',
     'long', 'ulong', 'float', 'Double',
     'string');
begin
  case ElementType of
    etVoid, etBoolean, etChar,
    etI1, etU1, etI2, etU2, etI4, etU4,
    etI8, etU8, etR4, etR8, etString:
      Result := ClrElementTypeNameMapping[ElementType];
    etPtr, etByRef, etValueType, etClass:
      Result := IntToHex(Token, 8);
    etArray:
      Result := 'Array';
    etTypedByRef:
      Result := 'TypedByRef';
    etI:
      Result := 'IntPtr';
    etU:
      Result := 'UIntPtr';
    etFnPtr:
      Result := 'Function';
    etObject:
      Result := 'System.Object';
    etSzArray:
      // (rom) possible BUG! Result not assigned
  else
    Result := 'Unknown';
  end;
end;

//=== { TJclClrLocalVarSign } ================================================

constructor TJclClrLocalVarSign.Create(const ABlob: TJclClrBlobRecord);
var
  Sign, ElemType: Byte;
  T: TJclClrElementType;
  I, VarCount: DWORD;
  LocalVar: TJclClrLocalVar;
begin
  inherited Create(ABlob);

  Blob.Seek(0, soFromBeginning);

  Sign := ReadByte;

  if (Sign and IMAGE_CEE_CS_CALLCONV_MASK) <> IMAGE_CEE_CS_CALLCONV_LOCAL_SIG then
    raise EJclMetadataError.CreateResFmt(@RsNoLocalVarSig, [IntToHex(Sign, 2)]);

  VarCount := ReadValue;
  if (VarCount < 1) or ($FFFE < VarCount) then
    raise EJclMetadataError.CreateResFmt(@RsLocalVarSigOutOfRange, [VarCount]);

  FLocalVars := TObjectList.Create;

  LocalVar := TJclClrLocalVar.Create;

  for I := 0 to VarCount-1 do
  begin
    ElemType := ReadByte;

    case ElemType of
      ELEMENT_TYPE_PINNED:
        LocalVar.Flags := LocalVar.Flags + [lvfPinned];
      ELEMENT_TYPE_BYREF:
        LocalVar.Flags := LocalVar.Flags + [lvfByRef];
      ELEMENT_TYPE_END:
        Break;
    else
      for T := Low(TJclClrElementType) to High(TJclClrElementType) do
        if ClrElementTypeMapping[T] = ElemType then
        begin
          LocalVar.ElementType := T;
          Break;
        end;
      if LocalVar.ElementType in [etPtr, etByRef, etValueType, etClass] then
        LocalVar.Token := ReadToken
      else
        LocalVar.Token := 0;

      FLocalVars.Add(LocalVar);
      LocalVar := TJclClrLocalVar.Create;
    end;
  end;
  FreeAndNil(LocalVar);
end;

destructor TJclClrLocalVarSign.Destroy;
begin
  FreeAndNil(FLocalVars);
  inherited Destroy;
end;

function TJclClrLocalVarSign.GetLocalVar(const Idx: Integer): TJclClrLocalVar;
begin
  Result := TJclClrLocalVar(FLocalVars[Idx]);
end;

function TJclClrLocalVarSign.GetLocalVarCount: Integer;
begin
  Result := FLocalVars.Count;
end;

//=== { TJclClrMethodSign } ==================================================

constructor TJclClrMethodSign.Create(const ABlob: TJclClrBlobRecord);
var
  Sign: Byte;
  I, ParamCount: Integer;
begin
  inherited Create(ABlob);

  FParams := TObjectList.Create;

  Sign := ReadByte;

  if IsBitSet(Sign, IMAGE_CEE_CS_CALLCONV_HASTHIS) then
    Include(FFlags, mfHasThis);

  if IsBitSet(Sign, IMAGE_CEE_CS_CALLCONV_EXPLICITTHIS) then
    Include(FFlags, mfExplicitThis);

  case Sign and IMAGE_CEE_CS_CALLCONV_MASK of
    IMAGE_CEE_CS_CALLCONV_DEFAULT:
      Include(FFlags, mfDefault);
    IMAGE_CEE_CS_CALLCONV_VARARG:
      Include(FFlags, mfVarArg);
  end;

  ParamCount := ReadValue;

  FRetType := TJclClrMethodRetType.Create(Blob);

  for I := 0 to ParamCount-1 do
    FParams.Add(TJclClrMethodParam.Create(Blob));
end;

destructor TJclClrMethodSign.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

function TJclClrMethodSign.GetParam(const Idx: Integer): TJclClrMethodParam;
begin
  Result := TJclClrMethodParam(FParams.Items[Idx]);
end;

function TJclClrMethodSign.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

//=== { TJclClrCustomModifierSign } ==========================================

constructor TJclClrCustomModifierSign.Create(const ABlob: TJclClrBlobRecord);
begin
  inherited Create(ABlob);
  FRequired := ReadByte = ELEMENT_TYPE_CMOD_REQD;
  FToken    := ReadToken;
end;

//=== { TJclClrMethodParam } =================================================

constructor TJclClrMethodParam.Create(const ABlob: TJclClrBlobRecord);
var
  By: Byte;
  Finished: Boolean;
begin
  inherited Create(ABlob);

  FCustomMods  := TObjectList.Create;
  FByRef       := False;
  FElementType := etEnd;
  FToken       := 0;
  FMethodSign  := nil;

  Finished := False;
  while not Finished and (Blob.Position < Blob.Size) do
  begin
    By := ReadByte;
    case By of
      ELEMENT_TYPE_CMOD_REQD, ELEMENT_TYPE_CMOD_OPT:
        begin
          Blob.Seek(-SizeOf(Byte), soFromCurrent);
          FCustomMods.Add(TJclClrCustomModifierSign.Create(Blob));
        end;
      ELEMENT_TYPE_BYREF:
        FByRef := True;
    else
      FElementType := TJclClrElementType(By);
      case FElementType of
        etPtr, etTypedByRef, etValueType, etClass:
          FToken := ReadToken;
        etFnPtr:
          FMethodSign := TJclClrMethodSign.Create(Blob);
        etArray:
          FArraySign := TJclClrArraySign.Create(Blob);
      end;
      Finished := True;
    end;
  end;
end;

destructor TJclClrMethodParam.Destroy;
begin
  FreeAndNil(FCustomMods);
  FreeAndNil(FMethodSign);
  inherited Destroy;
end;

function TJclClrMethodParam.GetCustomModifier(const Idx: Integer): TJclClrCustomModifierSign;
begin
  Result := TJclClrCustomModifierSign(FCustomMods.Items[Idx]);
end;

function TJclClrMethodParam.GetCustomModifierCount: Integer;
begin
  Result := FCustomMods.Count;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
