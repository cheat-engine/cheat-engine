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
{ The Original Code is JvSimpleXML.PAS, released on 2002-06-03.                                    }
{                                                                                                  }
{ The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com].    }
{ Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Christophe Paris,                                                                              }
{   Florent Ouchet (move from the JVCL to the JCL)                                                 }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains Xml parser and writter classes                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-07-06 23:06:16 +0200 (dim., 06 juil. 2008)                         $ }
{ Revision:      $Rev:: 2390                                                                     $ }
{ Author:        $Author:: uschuster                                                             $ }
{                                                                                                  }
{**************************************************************************************************}

// Known Issues: This component does not parse the !DOCTYPE tags but preserves them

unit JclSimpleXml;

interface

{$I jcl.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows, // Delphi 2005 inline
  {$ENDIF MSWINDOWS}
  {$IFDEF CLR}
  System.Text,
  System.IO,
  {$ENDIF CLR}
  SysUtils, Classes,
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  IniFiles,
  JclBase;

type
  {$IFDEF COMPILER5}
  THashedStringList = class(TStringList);
  THandle = Longword;
  {$ENDIF COMPILER5}
  TJclSimpleXML = class;
  EJclSimpleXMLError = class(EJclError);
  {$M+} // generate RTTI for published properties
  TJclSimpleXMLElem = class;
  {$M-}
  TJclSimpleXMLElems = class;
  TJclSimpleXMLProps = class;
  TJclSimpleXMLElemComment = class;
  TJclSimpleXMLElemClassic = class;
  TJclSimpleXMLElemCData = class;
  TJclSimpleXMLElemDocType = class;
  TJclSimpleXMLElemText = class;
  TJclSimpleXMLElemHeader = class;
  TJclSimpleXMLElemSheet = class;
  TJclOnSimpleXMLParsed = procedure(Sender: TObject; Name: string) of object;
  TJclOnValueParsed = procedure(Sender: TObject; Name, Value: string) of object;
  TJclOnSimpleProgress = procedure(Sender: TObject; const Position, Total: Integer) of object;

  //Those hash stuffs are for future use only
  //Plans are to replace current hash by this mechanism
  TJclHashKind = (hkList, hkDirect);
  {$IFDEF CLR}
  TJclHashElem = class(TObject)
    Next: TJclHashElem;
    Obj: TObject;
  end;
  PJclHashElem = TJclHashElem;
  TJclHashRecord = class;
  TJclHashList = array [0..25] of TJclHashRecord;
  PJclHashList = TJclHashList;
  TJclHashRecord = class(TObject)
  public
    Count: Byte;
    Kind: TJclHashKind;
    List: PJclHashList;
    FirstElem: PJclHashElem;
  end;
  PJclHashRecord = TJclHashRecord;
  {$ELSE}
  PJclHashElem = ^TJclHashElem;
  TJclHashElem = packed record
    Next: PJclHashElem;
    Obj: TObject;
  end;
  PJclHashRecord = ^TJclHashRecord;
  TJclHashList = array [0..25] of PJclHashRecord;
  PJclHashList = ^TJclHashList;
  TJclHashRecord = packed record
    Count: Byte;
    case Kind: TJclHashKind of
      hkList: (List: PJclHashList);
      hkDirect: (FirstElem: PJclHashElem);
  end;
  {$ENDIF CLR}

  TJclSimpleHashTable = class(TObject)
  private
    FList: PJclHashRecord;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddObject(const AName: string; AObject: TObject);
    procedure Clear;
  end;

  TJclSimpleXMLProp = class(TObject)
  private
    FName: string;
    FValue: string;
    FParent: TJclSimpleXMLProps;
    FNameSpace: string;
    FData: {$IFDEF CLR} TObject {$ELSE} Pointer {$ENDIF};
    function GetBoolValue: Boolean;
    procedure SetBoolValue(const Value: Boolean);
    procedure SetName(const Value: string);
    function GetFloatValue: Extended;
    procedure SetFloatValue(const Value: Extended);
  protected
    function GetIntValue: Int64;
    procedure SetIntValue(const Value: Int64);
  public
    function GetSimpleXML: TJclSimpleXML;
    function SaveToString: string;
    function FullName:string;
    property Parent: TJclSimpleXMLProps read FParent write FParent;
    property Name: string read FName write SetName;
    property Value: string read FValue write FValue;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property FloatValue: Extended read GetFloatValue write SetFloatValue;
    property NameSpace: string read FNameSpace write FNameSpace;

    property Data: {$IFDEF CLR} TObject {$ELSE} Pointer {$ENDIF} read FData write FData;
  end;

  TJclSimpleXMLProps = class(TObject)
  private
    FProperties: THashedStringList;
    FParent: TJclSimpleXMLElem;
    function GetCount: Integer;
    function GetItemNamedDefault(const Name, Default: string): TJclSimpleXMLProp;
    function GetItemNamed(const Name: string): TJclSimpleXMLProp;
  protected
    function GetSimpleXML: TJclSimpleXML;
    function GetItem(const Index: Integer): TJclSimpleXMLProp;
    procedure DoItemRename(Value: TJclSimpleXMLProp; const Name: string);
    procedure Error(const S: string);
    procedure FmtError(const S: string; const Args: array of const);
  public
    constructor Create(Parent: TJclSimpleXMLElem);
    destructor Destroy; override;
    function Add(const Name, Value: string): TJclSimpleXMLProp; overload;
    function Add(const Name: string; const Value: Int64): TJclSimpleXMLProp; overload;
    function Add(const Name: string; const Value: Boolean): TJclSimpleXMLProp; overload;
    function Insert(const Index: Integer; const Name, Value: string): TJclSimpleXMLProp; overload;
    function Insert(const Index: Integer; const Name: string; const Value: Int64): TJclSimpleXMLProp; overload;
    function Insert(const Index: Integer; const Name: string; const Value: Boolean): TJclSimpleXMLProp; overload;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string); overload;
    function Value(const Name: string; Default: string = ''): string;
    function IntValue(const Name: string; Default: Int64 = -1): Int64;
    function BoolValue(const Name: string; Default: Boolean = True): Boolean;
    procedure LoadFromStream(const Stream: TStream);
    procedure SaveToStream(const Stream: TStream);
    property Item[const Index: Integer]: TJclSimpleXMLProp read GetItem; default;
    property ItemNamed[const Name: string]: TJclSimpleXMLProp read GetItemNamed;
    property Count: Integer read GetCount;
  end;

  TJclSimpleXMLElemsProlog = class(TObject)
  private
    FElems: THashedStringList;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TJclSimpleXMLElem;
    function GetEncoding: string;
    function GetStandAlone: Boolean;
    function GetVersion: string;
    procedure SetEncoding(const Value: string);
    procedure SetStandAlone(const Value: Boolean);
    procedure SetVersion(const Value: string);
  protected
    function FindHeader: TJclSimpleXMLElem;
    procedure Error(const S: string);
    procedure FmtError(const S: string; const Args: array of const);
  public
    constructor Create;
    destructor Destroy; override;
    function AddComment(const AValue: string): TJclSimpleXMLElemComment;
    function AddDocType(const AValue: string): TJclSimpleXMLElemDocType;
    procedure Clear;
    function AddStyleSheet(AType, AHRef: string): TJclSimpleXMLElemSheet;
    function LoadFromStream(const Stream: TStream; AParent: TJclSimpleXML = nil): string;
    procedure SaveToStream(const Stream: TStream; AParent: TJclSimpleXML = nil);
    property Item[const Index: Integer]: TJclSimpleXMLElem read GetItem; default;
    property Count: Integer read GetCount;
    property Encoding: string read GetEncoding write SetEncoding;
    property StandAlone: Boolean read GetStandAlone write SetStandAlone;
    property Version: string read GetVersion write SetVersion;
  end;

  TJclSimpleXMLNamedElems = class(TObject)
  private
    FElems: TJclSimpleXMLElems;
    FName: string;
    function GetCount: Integer;
  protected
    FItems: TList;
    function GetItem(const Index: Integer): TJclSimpleXMLElem;
  public
    constructor Create(const AOwner: TJClSimpleXMLElems; const AName: string);
    destructor Destroy; override;

    function Add: TJclSimpleXmlElemClassic; overload;
    function Add(const Value: string): TJclSimpleXmlElemClassic; overload;
    function Add(const Value: Int64): TJclSimpleXmlElemClassic; overload;
    function Add(const Value: Boolean): TJclSimpleXmlElemClassic; overload;
    function Add(const Value: TStream): TJclSimpleXmlElemClassic; overload;
    function AddFirst: TJclSimpleXmlElemClassic;
    function AddComment(const Value: string): TJclSimpleXMLElemComment;
    function AddCData(const Value: string): TJclSimpleXMLElemCData;
    function AddText(const Value: string): TJclSimpleXMLElemText;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer);
    procedure Move(const CurIndex, NewIndex: Integer);
    function IndexOf(const Value: TJclSimpleXMLElem): Integer; overload;
    function IndexOf(const Value: string): Integer; overload;

    property Elems: TJclSimpleXMLElems read FElems;
    property Item[const Index: Integer]: TJclSimpleXMLElem read GetItem; default;
    property Count: Integer read GetCount;
    property Name: string read FName;
  end;

  TJclSimpleXMLElemCompare = function(Elems: TJclSimpleXMLElems; Index1, Index2: Integer): Integer of object;
  TJclSimpleXMLElems = class(TObject)
  private
    FParent: TJclSimpleXMLElem;
    function GetCount: Integer;
    function GetItemNamedDefault(const Name, Default: string): TJclSimpleXMLElem;
    function GetItemNamed(const Name: string): TJclSimpleXMLElem;
    function GetNamedElems(const Name: string): TJclSimpleXMLNamedElems;
  protected
    FElems: THashedStringList;
    FCompare: TJclSimpleXMLElemCompare;
    FNamedElems: THashedStringList;
    function GetItem(const Index: Integer): TJclSimpleXMLElem;
    procedure AddChild(const Value: TJclSimpleXMLElem);
    procedure AddChildFirst(const Value: TJclSimpleXMLElem);
    procedure InsertChild(const Value: TJclSimpleXMLElem; Index: Integer);
    procedure DoItemRename(Value: TJclSimpleXMLElem; const Name: string);
    procedure CreateElems;
  public
    constructor Create(const AOwner: TJclSimpleXMLElem);
    destructor Destroy; override;

    // Use notify to indicate to a list that the given element is removed
    // from the list so that it doesn't delete it as well as the one
    // that insert it in itself. This method is automatically called
    // by AddChild and AddChildFirst if the Container property of the
    // given element is set.
    procedure Notify(Value: TJclSimpleXMLElem; Operation: TOperation);

    function Add(const Name: string): TJclSimpleXMLElemClassic; overload;
    function Add(const Name, Value: string): TJclSimpleXMLElemClassic; overload;
    function Add(const Name: string; const Value: Int64): TJclSimpleXMLElemClassic; overload;
    function Add(const Name: string; const Value: Boolean): TJclSimpleXMLElemClassic; overload;
    function Add(const Name: string; const Value: TStream): TJclSimpleXMLElemClassic; overload;
    function Add(Value: TJclSimpleXMLElem): TJclSimpleXMLElem; overload;
    function AddFirst(Value: TJclSimpleXMLElem): TJclSimpleXMLElem; overload;
    function AddFirst(const Name: string): TJclSimpleXMLElemClassic; overload;
    function AddComment(const Name: string; const Value: string): TJclSimpleXMLElemComment;
    function AddCData(const Name: string; const Value: string): TJclSimpleXMLElemCData;
    function AddText(const Name: string; const Value: string): TJclSimpleXMLElemText;
    function Insert(Value: TJclSimpleXMLElem; Index: Integer): TJclSimpleXMLElem; overload;
    function Insert(const Name: string; Index: Integer): TJclSimpleXMLElemClassic; overload;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string); overload;
    function Remove(Value: TJclSimpleXMLElem): Integer;
    procedure Move(const CurIndex, NewIndex: Integer);
    function IndexOf(const Value: TJclSimpleXMLElem): Integer; overload;
    function IndexOf(const Name: string): Integer; overload;
    function Value(const Name: string; Default: string = ''): string;
    function IntValue(const Name: string; Default: Int64 = -1): Int64;
    function BoolValue(const Name: string; Default: Boolean = True): Boolean;
    procedure BinaryValue(const Name: string; const Stream: TStream);
    function LoadFromStream(const Stream: TStream; AParent: TJclSimpleXML = nil): string;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TJclSimpleXML = nil);
    procedure Sort;
    procedure CustomSort(AFunction: TJclSimpleXMLElemCompare);
    property Parent: TJclSimpleXMLElem read FParent write FParent;
    property Item[const Index: Integer]: TJclSimpleXMLElem read GetItem; default;
    property ItemNamed[const Name: string]: TJclSimpleXMLElem read GetItemNamed;
    property Count: Integer read GetCount;
    property NamedElems[const Name: string]: TJclSimpleXMLNamedElems read GetNamedElems;
  end;

  {$M+}
  TJclSimpleXMLElem = class(TObject)
  private
    FName: string;
    FParent: TJclSimpleXMLElem;
    FItems: TJclSimpleXMLElems;
    FProps: TJclSimpleXMLProps;
    FValue: string;
    FNameSpace: string;
    FData: {$IFDEF CLR} TObject {$ELSE} Pointer {$ENDIF};
    FSimpleXML: TJclSimpleXML;
    FContainer: TJclSimpleXMLElems;
    function GetFloatValue: Extended;
    procedure SetFloatValue(const Value: Extended);
  protected
    function GetSimpleXML: TJclSimpleXML;
    function GetIntValue: Int64;
    function GetBoolValue: Boolean;
    function GetChildsCount: Integer;
    function GetProps: TJclSimpleXMLProps;
    procedure SetBoolValue(const Value: Boolean);
    procedure SetName(const Value: string);
    procedure SetIntValue(const Value: Int64);
    function GetItems: TJclSimpleXMLElems;
    procedure Error(const S: string);
    procedure FmtError(const S: string; const Args: array of const);
  public
    constructor Create(const AOwner: TJclSimpleXMLElem); virtual;
    destructor Destroy; override;
    procedure Assign(Value: TJclSimpleXMLElem); virtual;
    procedure Clear; virtual;
    function SaveToString(AParent: TJclSimpleXML = nil): string;
    procedure LoadFromString(const Value: string; AParent: TJclSimpleXML = nil);
    procedure LoadFromStream(const Stream: TStream; AParent: TJclSimpleXML = nil); virtual; abstract;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TJclSimpleXML = nil); virtual;
      abstract;
    procedure GetBinaryValue(const Stream: TStream);
    property Data: {$IFDEF CLR} TObject {$ELSE} Pointer {$ENDIF} read FData write FData;
    function GetChildIndex(const AChild: TJclSimpleXMLElem): Integer;
    function GetNamedIndex(const AChild: TJclSimpleXMLElem): Integer;

    property SimpleXML: TJclSimpleXML read GetSimpleXML;
    property Container: TJclSimpleXMLElems read FContainer write FContainer;
  published
    function FullName: string;virtual;
    property Name: string read FName write SetName;
    property Parent: TJclSimpleXMLElem read FParent write FParent;
    property NameSpace: string read FNameSpace write FNameSpace;
    property ChildsCount: Integer read GetChildsCount;
    property Items: TJclSimpleXMLElems read GetItems;
    property Properties: TJclSimpleXMLProps read GetProps;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property FloatValue: Extended read GetFloatValue write SetFloatValue;
    property Value: string read FValue write FValue;
  end;
  {$M-}
  TJclSimpleXMLElemClass = class of TJclSimpleXMLElem;

  TJclSimpleXMLElemComment = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; AParent: TJclSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TJclSimpleXML = nil); override;
  end;

  TJclSimpleXMLElemClassic = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; AParent: TJclSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TJclSimpleXML = nil); override;
  end;

  TJclSimpleXMLElemCData = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; AParent: TJclSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TJclSimpleXML = nil); override;
  end;

  TJclSimpleXMLElemText = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; AParent: TJclSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TJclSimpleXML = nil); override;
  end;

  TJclSimpleXMLElemHeader = class(TJclSimpleXMLElem)
  private
    FStandalone: Boolean;
    FEncoding: string;
    FVersion: string;
  public
    procedure Assign(Value: TJclSimpleXMLElem); override;
    
    procedure LoadFromStream(const Stream: TStream; AParent: TJclSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TJclSimpleXML = nil); override;
    property Version: string read FVersion write FVersion;
    property StandAlone: Boolean read FStandalone write FStandalone;
    property Encoding: string read FEncoding write FEncoding;
    constructor Create(const AOwner: TJclSimpleXMLElem); override;
  end;

  TJclSimpleXMLElemDocType = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; AParent: TJclSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TJclSimpleXML = nil); override;
  end;

  TJclSimpleXMLElemSheet = class(TJclSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; AParent: TJclSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TJclSimpleXML = nil); override;
  end;

  TJclSimpleXMLOptions = set of (sxoAutoCreate, sxoAutoIndent, sxoAutoEncodeValue,
    sxoAutoEncodeEntity, sxoDoNotSaveProlog, sxoTrimPrecedingTextWhitespace);
  TJclSimpleXMLEncodeEvent = procedure(Sender: TObject; var Value: string) of object;
  TJclSimpleXMLEncodeStreamEvent = procedure(Sender: TObject; InStream, OutStream: TStream) of object;

  TJclSimpleXML = class(TObject)
  protected
    FFileName: TFileName;
    FOptions: TJclSimpleXMLOptions;
    FRoot: TJclSimpleXMLElemClassic;
    FOnTagParsed: TJclOnSimpleXMLParsed;
    FOnValue: TJclOnValueParsed;
    FOnLoadProg: TJclOnSimpleProgress;
    FOnSaveProg: TJclOnSimpleProgress;
    FProlog: TJclSimpleXMLElemsProlog;
    FSaveCount: Integer;
    FSaveCurrent: Integer;
    FIndentString: string;
    FOnEncodeValue: TJclSimpleXMLEncodeEvent;
    FOnDecodeValue: TJclSimpleXMLEncodeEvent;
    FOnDecodeStream: TJclSimpleXMLEncodeStreamEvent;
    FOnEncodeStream: TJclSimpleXMLEncodeStreamEvent;
    procedure SetIndentString(const Value: string);
    procedure SetRoot(const Value: TJclSimpleXMLElemClassic);
    procedure SetFileName(Value: TFileName);
    procedure DoLoadProgress(const APosition, ATotal: Integer);
    procedure DoSaveProgress;
    procedure DoTagParsed(const AName: string);
    procedure DoValueParsed(const AName, AValue: string);
    procedure DoEncodeValue(var Value: string); virtual;
    procedure DoDecodeValue(var Value: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromString(const Value: string);
    procedure LoadFromFile(const FileName: TFileName);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromResourceName(Instance: THandle; const ResName: string);
    procedure SaveToFile(FileName: TFileName);
    procedure SaveToStream(Stream: TStream);
    function SaveToString: string;
    property Prolog: TJclSimpleXMLElemsProlog read FProlog write FProlog;
    property Root: TJclSimpleXMLElemClassic read FRoot write SetRoot;
    property XMLData: string read SaveToString write LoadFromString;
    property FileName: TFileName read FFileName write SetFileName;
    property IndentString: string read FIndentString write SetIndentString;
    property Options: TJclSimpleXMLOptions read FOptions write FOptions;
    property OnSaveProgress: TJclOnSimpleProgress read FOnSaveProg write FOnSaveProg;
    property OnLoadProgress: TJclOnSimpleProgress read FOnLoadProg write FOnLoadProg;
    property OnTagParsed: TJclOnSimpleXMLParsed read FOnTagParsed write FOnTagParsed;
    property OnValueParsed: TJclOnValueParsed read FOnValue write FOnValue;
    property OnEncodeValue: TJclSimpleXMLEncodeEvent read FOnEncodeValue write FOnEncodeValue;
    property OnDecodeValue: TJclSimpleXMLEncodeEvent read FOnDecodeValue write FOnDecodeValue;
    property OnEncodeStream: TJclSimpleXMLEncodeStreamEvent read FOnEncodeStream write FOnEncodeStream;
    property OnDecodeStream: TJclSimpleXMLEncodeStreamEvent read FOnDecodeStream write FOnDecodeStream;
  end;

{$IFNDEF CLR}
{$IFDEF COMPILER6_UP}

  TXMLVariant = class(TInvokeableVariantType)
  public
    procedure Clear(var V: TVarData); override;
    function IsClear(const V: TVarData): Boolean; override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;

    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(const V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
  end;

  TXMLVarData = packed record
    vType: TVarType;
    Reserved1: Word;
    Reserved2: Word;
    Reserved3: Word;
    XML: TJclSimpleXMLElem;
    Reserved4: Longint;
  end;

procedure XMLCreateInto(var ADest: Variant; const AXML: TJclSimpleXMLElem);
function XMLCreate(const AXML: TJclSimpleXMLElem): Variant; overload;
function XMLCreate: Variant; overload;
function VarXML: TVarType;

{$ENDIF COMPILER6_UP}
{$ENDIF !CLR}

// Encodes a string into an internal format:
// any character <= #127 is preserved
// all other characters are converted to hex notation except
// for some special characters that are converted to XML entities
function SimpleXMLEncode(const S: string): string;
// Decodes a string encoded with SimpleXMLEncode:
// any character <= #127 is preserved
// all other characters and substrings are converted from
// the special XML entities to characters or from hex to characters
// NB! Setting TrimBlanks to true will slow down the process considerably
procedure SimpleXMLDecode(var S: string; TrimBlanks: Boolean);

function XMLEncode(const S: string): string;
function XMLDecode(const S: string): string;

// Encodes special characters (', ", <, > and &) into XML entities (@apos;, &quot;, &lt;, &gt; and &amp;)
function EntityEncode(const S: string): string;
// Decodes XML entities (@apos;, &quot;, &lt;, &gt; and &amp;) into special characters (', ", <, > and &)
function EntityDecode(const S: string): string;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclSimpleXml.pas $';
    Revision: '$Revision: 2390 $';
    Date: '$Date: 2008-07-06 23:06:16 +0200 (dim., 06 juil. 2008) $';
    LogPath: 'JCL\source\common'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclStrings,
  JclResources;

const
  cBufferSize = 8192;
  DefaultTrueBoolStr = 'True'; // DO NOT LOCALIZE
  DefaultFalseBoolStr = 'False'; // DO NOT LOCALIZE

var
  GlobalSorts: TList = nil;

  {$IFNDEF CLR}
  {$IFDEF COMPILER6_UP}
  GlobalXMLVariant: TXMLVariant = nil;
  {$ENDIF COMPILER6_UP}
  {$ENDIF !CLR}

  {$IFDEF COMPILER5}
  TrueBoolStrs: array of string;
  FalseBoolStrs: array of string;
  {$ENDIF COMPILER5}

  PreparedNibbleCharMapping: Boolean = False;
  NibbleCharMapping: array [Low(Char)..High(Char)] of Byte;

function GSorts: TList;
begin
  if not Assigned(GlobalSorts) then
    GlobalSorts := TList.Create;
  Result := GlobalSorts;
end;

{$IFNDEF CLR}
{$IFDEF COMPILER6_UP}

function XMLVariant: TXMLVariant;
begin
  if not Assigned(GlobalXMLVariant) then
    GlobalXMLVariant := TXMLVariant.Create;
  Result := GlobalXMLVariant;
end;
{$ENDIF COMPILER6_UP}
{$ENDIF !CLR}

function EntityEncode(const S: string): string;
var
  I, J, K, L: Integer;
  tmp: string;
begin
  SetLength(Result, Length(S) * 6); // worst case
  J := 1;
  I := 1;
  L := Length(S);
  while I <= L do
  begin
    case S[I] of
      '"':
        tmp := '&quot;';
      '&':
        tmp := '&amp;';
      #39:
        tmp := '&apos;';
      '<':
        tmp := '&lt;';
      '>':
        tmp := '&gt;';
    else
      tmp := S[I];
    end;
    for K := 1 to Length(tmp) do
    begin
      Result[J] := tmp[K];
      Inc(J);
    end;
    Inc(I);
  end;
  if J > 1 then
    SetLength(Result, J - 1)
  else
    SetLength(Result, 0);
end;

function EntityDecode(const S: string): string;
var
  I, J, L: Integer;
begin
  Result := S;
  I := 1;
  J := 1;
  L := Length(Result);

  while I <= L do
  begin
    if Result[I] = '&' then
    begin
      if AnsiSameText(AnsiString(Copy(Result, I, 5)), AnsiString('&amp;')) then
      begin
        Result[J] := '&';
        Inc(J);
        Inc(I, 4);
      end
      else
      if AnsiSameText(AnsiString(Copy(Result, I, 4)), AnsiString('&lt;')) then
      begin
        Result[J] := '<';
        Inc(J);
        Inc(I, 3);
      end
      else
      if AnsiSameText(AnsiString(Copy(Result, I, 4)), AnsiString('&gt;')) then
      begin
        Result[J] := '>';
        Inc(J);
        Inc(I, 3);
      end
      else
      if AnsiSameText(AnsiString(Copy(Result, I, 6)), AnsiString('&apos;')) then
      begin
        Result[J] := #39;
        Inc(J);
        Inc(I, 5);
      end
      else
      if AnsiSameText(AnsiString(Copy(Result, I, 6)), AnsiString('&quot;')) then
      begin
        Result[J] := '"';
        Inc(J);
        Inc(I, 5);
      end
      else
      begin
        Result[J] := Result[I];
        Inc(J);
      end;
    end
    else
    begin
      Result[J] := Result[I];
      Inc(J);
    end;
    Inc(I);
  end;
  if J > 1 then
    SetLength(Result, J - 1)
  else
    SetLength(Result, 0);
end;

function ReadCharsFromStream(Stream: TStream; var Buf: array of Char; BufSize: Integer): Integer;
{$IFDEF CLR}
var
  Bytes: TBytes;
{$ENDIF CLR}
begin
  {$IFDEF CLR}
  SetLength(Bytes, BufSize);
  Result := Stream.Read(Bytes, 0, BufSize);
  System.Array.Copy(AnsiEncoding.GetChars(Bytes), 0, Buf, 0, BufSize);
  {$ELSE}
  Result := Stream.Read(Buf, BufSize);
  {$ENDIF CLR}
end;

function WriteStringToStream(Stream: TStream; const Buf: string; BufSize: Integer): Integer;
begin
  {$IFDEF CLR}
  Result := Stream.Write(BytesOf(Buf), BufSize);
  {$ELSE}
  Result := Stream.Write(Buf[1], BufSize);
  {$ENDIF CLR}
end;

{$IFDEF COMPILER5}

procedure VerifyBoolStrArray;
begin
  if Length(TrueBoolStrs) = 0 then
  begin
    SetLength(TrueBoolStrs, 1);
    TrueBoolStrs[0] := DefaultTrueBoolStr;
  end;
  if Length(FalseBoolStrs) = 0 then
  begin
    SetLength(FalseBoolStrs, 1);
    FalseBoolStrs[0] := DefaultFalseBoolStr;
  end;
end;

function TryStrToBool(const S: string; out Value: Boolean): Boolean;
var
  lResult: Extended;

  function CompareWith(const AStrings: array of string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Low(AStrings) to High(AStrings) do
      if AnsiSameText(S, AStrings[I]) then
      begin
        Result := True;
        Break;
      end;
  end;

begin
  Result := TryStrToFloat(S, lResult);
  if Result then
    Value := lResult <> 0
  else
  begin
    VerifyBoolStrArray;
    Result := CompareWith(TrueBoolStrs);
    if Result then
      Value := True
    else
    begin
      Result := CompareWith(FalseBoolStrs);
      if Result then
        Value := False;
    end;
  end;
end;

function StrToBoolDef(const S: string; const Default: Boolean): Boolean;
begin
  if not TryStrToBool(S, Result) then
    Result := Default;
end;

(*  make Delphi 5 compiler happy // andreas
function StrToBool(const S: string): Boolean;
begin
  if not TryStrToBool(S, Result) then
    ConvertErrorFmt(@SInvalidBoolean, [S]);
end;
*)

function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
const
  cSimpleBoolStrs: array [Boolean] of string = ('0', '-1');
begin
  if UseBoolStrs then
  begin
    VerifyBoolStrArray;
    if B then
      Result := TrueBoolStrs[0]
    else
      Result := FalseBoolStrs[0];
  end
  else
    Result := cSimpleBoolStrs[B];
end;

{$ENDIF COMPILER5}

{$IFDEF CLR}
function TryStrToFloat(const S: string; out Value: Extended): Boolean;
var
  Temp: Double;
begin
  Result := SysUtils.TryStrToFloat(S, Temp);
  if Result then
    Value := Temp
  else
    Value := 0;
end;
{$ENDIF CLR}

function SimpleXMLEncode(const S: string): string;
const
  NoConversion = [#0..#127] - ['"', '&', #39, '<', '>'];
var
  I, J, K: Integer;
  tmp: string;
begin
  SetLength(Result, Length(S) * 6); // worst case
  J := 1;
  for I := 1 to Length(S) do
  begin
    if AnsiChar(S[I]) in NoConversion then
      Result[J] := S[I]
    else
    begin
      case S[I] of
        '"':
          tmp := '&quot;';
        '&':
          tmp := '&amp;';
        #39:
          tmp := '&apos;';
        '<':
          tmp := '&lt;';
        '>':
          tmp := '&gt;';
      else
        tmp := Format('&#x%.2x;', [Ord(S[I])]);
      end;
      for K := 1 to Length(tmp) do
      begin
        Result[J] := tmp[K];
        Inc(J);
      end;
      Dec(J);
    end;
    Inc(J);
  end;
  if J > 0 then
    SetLength(Result, J - 1)
  else
    SetLength(Result, 0);
end;

procedure SimpleXMLDecode(var S: string; TrimBlanks: Boolean);
var
  StringLength, ReadIndex, WriteIndex: Cardinal;

  procedure DecodeEntity(var S: string; StringLength: Cardinal;
    var ReadIndex, WriteIndex: Cardinal);
  const
    cHexPrefix: array [Boolean] of string[1] = ('', '$');
  var
    I: Cardinal;
    Value: Integer;
    IsHex: Boolean;
  begin
    Inc(ReadIndex, 2);
    IsHex := (ReadIndex <= StringLength) and (AnsiChar(S[ReadIndex]) in ['x', 'X']);
    Inc(ReadIndex, Ord(IsHex));
    I := ReadIndex;
    while ReadIndex <= StringLength do
    begin
      if S[ReadIndex] = ';' then
      begin
        Value := StrToIntDef(cHexPrefix[IsHex] + Copy(S, I, ReadIndex - I), -1); // no characters are less than 0
        if Value > 0 then
          S[WriteIndex] := Chr(Value)
        else
          ReadIndex := I - (2 + Cardinal(IsHex)); // reset to start
        Exit;
      end;
      Inc(ReadIndex);
    end;
    ReadIndex := I - (2 + Cardinal(IsHex)); // reset to start
  end;

  procedure SkipBlanks(var S: string; StringLength: Cardinal; var ReadIndex: Cardinal);
  begin
    while ReadIndex < StringLength do
    begin
      if S[ReadIndex] = AnsiCarriageReturn then
        S[ReadIndex] := AnsiLineFeed
      else
      if S[ReadIndex + 1] = AnsiCarriageReturn then
        S[ReadIndex + 1] := AnsiLineFeed;
      if (S[ReadIndex] < #33) and (S[ReadIndex] = S[ReadIndex + 1]) then
        Inc(ReadIndex)
      else
        Exit;
    end;
  end;

begin
  // NB! This procedure replaces the text inplace to speed up the conversion. This
  // works because when decoding, the string can only become shorter. This is
  // accomplished by keeping track of the current read and write points.
  // In addition, the original string length is read only once and passed to the
  // inner procedures to speed up conversion as much as possible
  ReadIndex := 1;
  WriteIndex := 1;
  StringLength := Length(S);
  while ReadIndex <= StringLength do
  begin
    // this call lowers conversion speed by ~30%, ie 21MB/sec -> 15MB/sec (repeated tests, various inputs)
    if TrimBlanks then
      SkipBlanks(S, StringLength, ReadIndex);
    if S[ReadIndex] = '&' then
    begin
      if S[ReadIndex + 1] = '#' then
      begin
        DecodeEntity(S, StringLength, ReadIndex, WriteIndex);
        Inc(WriteIndex);
      end
      else
      if AnsiSameText(AnsiString(Copy(S, ReadIndex, 5)), AnsiString('&amp;')) then
      begin
        S[WriteIndex] := '&';
        Inc(WriteIndex);
        Inc(ReadIndex, 4);
      end
      else
      if AnsiSameText(AnsiString(Copy(S, ReadIndex, 4)), AnsiString('&lt;')) then
      begin
        S[WriteIndex] := '<';
        Inc(WriteIndex);
        Inc(ReadIndex, 3);
      end
      else
      if AnsiSameText(AnsiString(Copy(S, ReadIndex, 4)), AnsiString('&gt;')) then
      begin
        S[WriteIndex] := '>';
        Inc(WriteIndex);
        Inc(ReadIndex, 3);
      end
      else
      if AnsiSameText(AnsiString(Copy(S, ReadIndex, 6)), AnsiString('&apos;')) then
      begin
        S[WriteIndex] := #39;
        Inc(WriteIndex);
        Inc(ReadIndex, 5);
      end
      else
      if AnsiSameText(AnsiString(Copy(S, ReadIndex, 6)), AnsiString('&quot;')) then
      begin
        S[WriteIndex] := '"';
        Inc(WriteIndex);
        Inc(ReadIndex, 5);
      end
      else
      begin
        S[WriteIndex] := S[ReadIndex];
        Inc(WriteIndex);
      end;
    end
    else
    begin
      S[WriteIndex] := S[ReadIndex];
      Inc(WriteIndex);
    end;
    Inc(ReadIndex);
  end;
  if WriteIndex > 0 then
    SetLength(S, WriteIndex - 1)
  else
    SetLength(S, 0);
    // this call lowers conversion speed by ~65%, ie 21MB/sec -> 7MB/sec (repeated tests, various inputs)
//  if TrimBlanks then
//    S := AdjustLineBreaks(S);
end;

function XMLEncode(const S: string): string;
begin
  Result := SimpleXMLEncode(S);
end;

function XMLDecode(const S: string): string;
begin
  Result := S;
  SimpleXMLDecode(Result, False);
end;

//=== { TJclSimpleXML } ======================================================

constructor TJclSimpleXML.Create;
begin
  inherited Create;
  FRoot := TJclSimpleXMLElemClassic.Create(nil);
  FRoot.FSimpleXML := Self;
  FProlog := TJclSimpleXMLElemsProlog.Create;
  FOptions := [sxoAutoIndent, sxoAutoEncodeValue, sxoAutoEncodeEntity];
  FIndentString := '  ';
end;

destructor TJclSimpleXML.Destroy;
begin
  FreeAndNil(FRoot);
  FreeAndNil(FProlog);
  inherited Destroy;
end;

procedure TJclSimpleXML.DoDecodeValue(var Value: string);
begin
  if sxoAutoEncodeValue in Options then
    SimpleXMLDecode(Value, False)
  else
  if sxoAutoEncodeEntity in Options then
    Value := EntityDecode(Value);
  if Assigned(FOnDecodeValue) then
    FOnDecodeValue(Self, Value);
end;

procedure TJclSimpleXML.DoEncodeValue(var Value: string);
begin
  if Assigned(FOnEncodeValue) then
    FOnEncodeValue(Self, Value);
  if sxoAutoEncodeValue in Options then
    Value := SimpleXMLEncode(Value)
  else
  if sxoAutoEncodeEntity in Options then
    Value := EntityEncode(Value);
end;

procedure TJclSimpleXML.DoLoadProgress(const APosition, ATotal: Integer);
begin
  if Assigned(FOnLoadProg) then
    FOnLoadProg(Self, APosition, ATotal);
end;

procedure TJclSimpleXML.DoSaveProgress;
begin
  if Assigned(FOnSaveProg) then
  begin
    Inc(FSaveCount);
    FOnSaveProg(Self, FSaveCurrent, FSaveCount);
  end;
end;

procedure TJclSimpleXML.DoTagParsed(const AName: string);
begin
  if Assigned(FOnTagParsed) then
    FOnTagParsed(Self, AName);
end;

procedure TJclSimpleXML.DoValueParsed(const AName, AValue: string);
begin
  if Assigned(FOnValue) then
    FOnValue(Self, AName, AValue);
end;

procedure TJclSimpleXML.LoadFromFile(const FileName: TFileName);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(FileName);
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXML.LoadFromResourceName(Instance: THandle; const ResName: string);
{$IFNDEF MSWINDOWS}
const
  RT_RCDATA = PChar(10);
{$ENDIF !MSWINDOWS}
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXML.LoadFromStream(Stream: TStream);
var
  AOutStream: TStream;
  DoFree: Boolean;
begin
  FRoot.Clear;
  FProlog.Clear;
  AOutStream := nil;
  DoFree := False;
  try
    if Assigned(FOnDecodeStream) then
    begin
      AOutStream := TMemoryStream.Create;
      DoFree := True;
      FOnDecodeStream(Self, Stream, AOutStream);
      AOutStream.Seek(0, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
    end
    else
      AOutStream := Stream;
    if Assigned(FOnLoadProg) then
    begin
      FOnLoadProg(Self, AOutStream.Position, AOutStream.Size);
    // Read doctype and so on
      FProlog.LoadFromStream(AOutStream, Self);
    // Read elements
      FRoot.LoadFromStream(AOutStream, Self);
      FOnLoadProg(Self, AOutStream.Position, AOutStream.Size);
    end
    else
    begin
      FProlog.LoadFromStream(AOutStream, Self);
      FRoot.LoadFromStream(AOutStream, Self);
    end;
  finally
    if DoFree then
      AOutStream.Free;
  end;
end;

procedure TJclSimpleXML.LoadFromString(const Value: string);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(Value);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXML.SaveToFile(FileName: TFileName);
var
  Stream: TFileStream;
begin
  if SysUtils.FileExists(FileName) then
  begin
    Stream := TFileStream.Create(FileName, fmOpenWrite);
    Stream.Size := 0;
  end
  else
    Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXML.SaveToStream(Stream: TStream);
var
  lCount: Integer;
  AOutStream: TStream;
  DoFree: Boolean;
begin
  if Assigned(FOnEncodeStream) then
  begin
    AOutStream := TMemoryStream.Create;
    DoFree := True;
  end
  else
  begin
    AOutStream := Stream;
    DoFree := False;
  end;
  try
    if Assigned(FOnSaveProg) then
    begin
      lCount := Root.ChildsCount + Prolog.Count;
      FSaveCount := lCount;
      FSaveCurrent := 0;
      FOnSaveProg(Self, 0, lCount);
      if not (sxoDoNotSaveProlog in FOptions) then
        Prolog.SaveToStream(AOutStream, Self);
      Root.SaveToStream(AOutStream, '', Self);
      FOnSaveProg(Self, lCount, lCount);
    end
    else
    begin
      if not (sxoDoNotSaveProlog in FOptions) then
        Prolog.SaveToStream(AOutStream, Self);
      Root.SaveToStream(AOutStream, '', Self);
    end;
    if Assigned(FOnEncodeStream) then
    begin
      AOutStream.Seek(0, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
      FOnEncodeStream(Self, AOutStream, Stream);
    end;
  finally
    if DoFree then
      AOutStream.Free;
  end;
end;

function TJclSimpleXML.SaveToString: string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    SaveToStream(Stream);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXML.SetFileName(Value: TFileName);
begin
  FFileName := Value;
  LoadFromFile(Value);
end;

//=== { TJclSimpleXMLElem } ==================================================

procedure TJclSimpleXMLElem.Assign(Value: TJclSimpleXMLElem);
var
  Elems: TJclSimpleXMLElem;
  Elem: TJclSimpleXMLElem;
  I: Integer;
begin
  Clear;
  if Value = nil then
    Exit;
  Elems := TJclSimpleXMLElem(Value);
  Name := Elems.Name;
  Self.Value := Elems.Value;
  for I := 0 to Elems.Properties.Count - 1 do
    Properties.Add(Elems.Properties[I].Name, Elems.Properties[I].Value);

  for I := 0 to Elems.Items.Count - 1 do
  begin
    // Create from the class type, so that the virtual constructor is called
    // creating an element of the correct class type.
    Elem := TJclSimpleXMLElemClass(Elems.Items[I].ClassType).Create(Elems.Items[I].Parent);
    Elem.Assign(Elems.Items[I]);
    Items.Add(Elem);
  end;
end;

procedure TJclSimpleXMLElem.Clear;
begin
  if FItems <> nil then
    FItems.Clear;
  if FProps <> nil then
    FProps.Clear;
end;

constructor TJclSimpleXMLElem.Create(const AOwner: TJclSimpleXMLElem);
begin
  inherited Create;
  FName := '';
  FParent := TJclSimpleXMLElem(AOwner);
  if Assigned(FParent) then
    FSimpleXML := FParent.FSimpleXML;
  FContainer := nil;
end;

destructor TJclSimpleXMLElem.Destroy;
begin
  FParent := nil;
  Clear;
  FreeAndNil(FItems);
  FreeAndNil(FProps);
  inherited Destroy;
end;

procedure TJclSimpleXMLElem.Error(const S: string);
begin
  raise EJclSimpleXMLError.Create(S);
end;

procedure TJclSimpleXMLElem.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S, Args));
end;

function TJclSimpleXMLElem.FullName: string;
begin
  if FNameSpace <> '' then
    Result := FNameSpace + ':' + Name
  else
    Result := Name;
end;

procedure TJclSimpleXMLElem.GetBinaryValue(const Stream: TStream);
var
  I, J, ValueLength, RequiredStreamSize: Integer;
  Buf: array [0..cBufferSize - 1] of Byte;
  N1, N2: Byte;

  function NibbleCharToNibble(const AChar: Char): Byte;
  begin
    case AChar of
      '0': Result := 0;
      '1': Result := 1;
      '2': Result := 2;
      '3': Result := 3;
      '4': Result := 4;
      '5': Result := 5;
      '6': Result := 6;
      '7': Result := 7;
      '8': Result := 8;
      '9': Result := 9;
      'a', 'A': Result := 10;
      'b', 'B': Result := 11;
      'c', 'C': Result := 12;
      'd', 'D': Result := 13;
      'e', 'E': Result := 14;
      'f', 'F': Result := 15;
      else
        Result := 16;
    end;
  end;

  procedure PrepareNibbleCharMapping;
  var
    C: Char;
  begin
    if not PreparedNibbleCharMapping then
    begin
      for C := Low(Char) to High(Char) do
        NibbleCharMapping[C] := NibbleCharToNibble(C);
      PreparedNibbleCharMapping := True;
    end;
  end;

var
  CurrentStreamPosition: Integer;
begin
  PrepareNibbleCharMapping;
  I := 1;
  J := 0;
  ValueLength := Length(Value);
  RequiredStreamSize := Stream.Position + ValueLength div 2;
  if Stream.Size < RequiredStreamSize then
  begin
    CurrentStreamPosition := Stream.Position;
    Stream.Size := RequiredStreamSize;
    Stream.Seek(CurrentStreamPosition, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
  end;
  while I < ValueLength do
  begin
    if J = cBufferSize - 1 then //Buffered write to speed up the process a little
    begin
      Stream.Write(Buf, J);
      J := 0;
    end;
    //faster replacement for St := '$' + Value[I] + Value[I + 1]; Buf[J] := StrToIntDef(St, 0);
    N1 := NibbleCharMapping[Value[I]];
    N2 := NibbleCharMapping[Value[I + 1]];
    if (N1 > 15) or (N2 > 15) then
      Buf[J] := 0
    else
      Buf[J] := N1 shl 4 + N2;
    Inc(J);
    Inc(I, 2);
  end;
  Stream.Write(Buf, J);
end;

function TJclSimpleXMLElem.GetBoolValue: Boolean;
begin
  Result := StrToBoolDef(Value, False);
end;

function TJclSimpleXMLElem.GetChildIndex(
  const AChild: TJclSimpleXMLElem): Integer;
begin
  if FItems = nil then
    Result := -1
  else
    Result := FItems.FElems.IndexOfObject(AChild);
end;

function TJclSimpleXMLElem.GetChildsCount: Integer;
var
  I: Integer;
begin
  Result := 1;
  if FItems <> nil then
    for I := 0 to FItems.Count - 1 do
      Result := Result + FItems[I].ChildsCount;
end;

function TJclSimpleXMLElem.GetFloatValue: Extended;
begin
  if not TryStrToFloat(Value, Result) then
    Result := 0.0;
end;

function TJclSimpleXMLElem.GetIntValue: Int64;
begin
  Result := StrToInt64Def(Value, -1);
end;

function TJclSimpleXMLElem.GetItems: TJclSimpleXMLElems;
begin
  if FItems = nil then
    FItems := TJclSimpleXMLElems.Create(Self);
  Result := FItems;
end;

function TJclSimpleXMLElem.GetNamedIndex(const AChild: TJclSimpleXMLElem): Integer;
begin
  Result := Items.NamedElems[AChild.Name].IndexOf(AChild);
end;

function TJclSimpleXMLElem.GetProps: TJclSimpleXMLProps;
begin
  if FProps = nil then
    FProps := TJclSimpleXMLProps.Create(Self);
  Result := FProps;
end;

function TJclSimpleXMLElem.GetSimpleXML: TJclSimpleXML;
begin
  if FParent <> nil then
    Result := FParent.GetSimpleXML
  else
    Result := FSimpleXML;
end;

procedure TJclSimpleXMLElem.LoadFromString(const Value: string; AParent : TJclSimpleXML);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(Value);
  try
    LoadFromStream(Stream, AParent);
  finally
    Stream.Free;
  end;
end;

function TJclSimpleXMLElem.SaveToString(AParent : TJclSimpleXML): string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    SaveToStream(Stream, '', AParent);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXMLElem.SetBoolValue(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;

procedure TJclSimpleXMLElem.SetFloatValue(const Value: Extended);
begin
  FValue := FloatToStr(Value);
end;

procedure TJclSimpleXMLElem.SetIntValue(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;

procedure TJclSimpleXMLElem.SetName(const Value: string);
begin
  if (Value <> FName) and (Value <> '') then
  begin
    if (Parent <> nil) and (FName <> '') then
      Parent.Items.DoItemRename(Self, Value);
    FName := Value;
  end;
end;

//=== { TJclSimpleXMLNamedElems } ============================================

constructor TJclSimpleXMLNamedElems.Create(const AOwner: TJClSimpleXMLElems; const AName: string);
begin
  inherited Create;
  FElems := AOwner;
  FName := AName;
  FItems := TList.Create;
end;

destructor TJclSimpleXMLNamedElems.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJclSimpleXMLNamedElems.Add(const Value: Int64): TJclSimpleXmlElemClassic;
begin
  Result := Elems.Add(Name, Value);
end;

function TJclSimpleXMLNamedElems.Add(const Value: TStream): TJclSimpleXmlElemClassic;
begin
  Result := Elems.Add(Name, Value);
end;

function TJclSimpleXMLNamedElems.Add(const Value: Boolean): TJclSimpleXmlElemClassic;
begin
  Result := Elems.Add(Name, Value);
end;

function TJclSimpleXMLNamedElems.Add: TJclSimpleXmlElemClassic;
begin
  Result := Elems.Add(Name);
end;

function TJclSimpleXMLNamedElems.Add(const Value: string): TJclSimpleXmlElemClassic;
begin
  Result := Elems.Add(Name, Value);
end;

function TJclSimpleXMLNamedElems.AddCData(const Value: string): TJclSimpleXMLElemCData;
begin
  Result := Elems.AddCData(Name, Value);
end;

function TJclSimpleXMLNamedElems.AddComment(const Value: string): TJclSimpleXMLElemComment;
begin
  Result := Elems.AddComment(Name, Value);
end;

function TJclSimpleXMLNamedElems.AddFirst: TJclSimpleXmlElemClassic;
begin
  Result := Elems.AddFirst(Name);
end;

function TJclSimpleXMLNamedElems.AddText(const Value: string): TJclSimpleXMLElemText;
begin
  Result := Elems.AddText(Name, Value);
end;

procedure TJclSimpleXMLNamedElems.Clear;
var
  Index: Integer;
begin
  for Index := FItems.Count - 1 downto 0 do
    Elems.Remove(TJclSimpleXMLElem(FItems.Items[Index]));
end;

procedure TJclSimpleXMLNamedElems.Delete(const Index: Integer);
begin
  if (Index >= 0) and (Index < FItems.Count) then
    Elems.Remove(TJclSimpleXMLElem(FItems.Items[Index]));
end;

function TJclSimpleXMLNamedElems.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJclSimpleXMLNamedElems.GetItem(const Index: Integer): TJclSimpleXMLElem;
begin
  if (Index >= 0) then
  begin
    While (Index >= Count) do
      if Assigned(Elems.Parent) and Assigned(Elems.Parent.SimpleXML) and
         (sxoAutoCreate in Elems.Parent.SimpleXML.Options) then
        Add
      else
        break;
    if Index < Count then
      Result := TJclSimpleXMLElem(FItems.Items[Index])
    else
      Result := nil;
  end
  else
    Result := nil;
end;

function TJclSimpleXMLNamedElems.IndexOf(const Value: TJclSimpleXMLElem): Integer;
begin
  Result := FItems.IndexOf(Value);
end;

function TJclSimpleXMLNamedElems.IndexOf(const Value: string): Integer;
var
  Index: Integer;
  NewItem: TJclSimpleXMLElem;
begin
  Result := -1;
  for Index := 0 to FItems.Count - 1 do
    if TJclSimpleXMLElem(FItems.Items[Index]).Value = Value then
  begin
    Result := Index;
    Break;
  end;
  if (Result = -1) and (sxoAutoCreate in Elems.Parent.SimpleXML.Options) then
  begin
    NewItem := Elems.Add(Name, Value);
    Result := FItems.IndexOf(NewItem);
  end;
end;

procedure TJclSimpleXMLNamedElems.Move(const CurIndex, NewIndex: Integer);
var
  ElemsCurIndex, ElemsNewIndex: Integer;
begin
  ElemsCurIndex := Elems.IndexOf(TJclSimpleXMLElem(FItems.Items[CurIndex]));
  ElemsNewIndex := Elems.IndexOf(TJclSimpleXMLElem(FItems.Items[NewIndex]));
  Elems.Move(ElemsCurIndex, ElemsNewIndex);
  FItems.Move(CurIndex, NewIndex);
end;

//=== { TJclSimpleXMLElems } =================================================

function TJclSimpleXMLElems.Add(const Name: string): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Parent);
  Result.FName := Name; //Directly set parent to avoid notification
  AddChild(Result);
end;

function TJclSimpleXMLElems.Add(const Name, Value: string): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TJclSimpleXMLElems.Add(const Name: string; const Value: Int64): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Parent);
  Result.FName := Name;
  Result.Value := IntToStr(Value);
  AddChild(Result);
end;

function TJclSimpleXMLElems.Add(Value: TJclSimpleXMLElem): TJclSimpleXMLElem;
begin
  if Value <> nil then
    AddChild(Value);
  Result := Value;
end;

function TJclSimpleXMLElems.Add(const Name: string;
  const Value: Boolean): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Parent);
  Result.FName := Name;
  Result.Value := BoolToStr(Value);
  AddChild(Result);
end;

function TJclSimpleXMLElems.Add(const Name: string;
  const Value: TStream): TJclSimpleXMLElemClassic;
var
  Stream: TStringStream;
  Buf: array [0..cBufferSize - 1] of Byte;
  St: string;
  I, Count: Integer;
begin
  Stream := TStringStream.Create('');
  try
    repeat
      Count := Value.Read(Buf, Length(Buf));
      St := '';
      for I := 0 to Count - 1 do
        St := St + IntToHex(Buf[I], 2);
      Stream.WriteString(St);
    until Count = 0;
    Result := TJclSimpleXMLElemClassic.Create(Parent);
    Result.FName := Name;
    Result.Value := Stream.DataString;
    AddChild(Result);
  finally
    Stream.Free;
  end;
end;

procedure TJclSimpleXMLElems.AddChild(const Value: TJclSimpleXMLElem);
var
  NamedIndex: Integer;
begin
  CreateElems;

  // If there already is a container, notify it to remove the element
  if Assigned(Value.Container) then
  begin
    Value.Container.Notify(Value, opRemove);
    Value.Parent := Parent;
  end;

  FElems.AddObject(Value.Name, Value);

  if FNamedElems <> nil then
  begin
    NamedIndex := FNamedElems.IndexOf(Value.Name);
    if NamedIndex >= 0 then
      TJclSimpleXMLNamedElems(FNamedElems.Objects[NamedIndex]).FItems.Add(Value);
  end;

  Notify(Value, opInsert);
end;

procedure TJclSimpleXMLElems.AddChildFirst(const Value: TJclSimpleXMLElem);
var
  NamedIndex: Integer;
begin
  CreateElems;

  // If there already is a container, notify it to remove the element
  if Assigned(Value.Container) then
  begin
    Value.Container.Notify(Value, opRemove);
    Value.Parent := Parent;
  end;

  FElems.InsertObject(0, Value.Name, Value);

  if FNamedElems <> nil then
  begin
    NamedIndex := FNamedElems.IndexOf(Value.Name);
    if NamedIndex >= 0 then
      TJclSimpleXMLNamedElems(FNamedElems.Objects[NamedIndex]).FItems.Insert(0, Value);
  end;

  Notify(Value, opInsert);
end;

function TJclSimpleXMLElems.AddFirst(const Name: string): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Parent);
  Result.FName := Name; //Directly set parent to avoid notification
  AddChildFirst(Result);
end;

function TJclSimpleXMLElems.AddFirst(Value: TJclSimpleXMLElem): TJclSimpleXMLElem;
begin
  if Value <> nil then
    AddChildFirst(Value);
  Result := Value;
end;

function TJclSimpleXMLElems.AddComment(const Name,
  Value: string): TJclSimpleXMLElemComment;
begin
  Result := TJclSimpleXMLElemComment.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TJclSimpleXMLElems.AddCData(const Name, Value: string): TJclSimpleXMLElemCData;
begin
  Result := TJclSimpleXMLElemCData.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TJclSimpleXMLElems.AddText(const Name, Value: string): TJclSimpleXMLElemText;
begin
  Result := TJclSimpleXMLElemText.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

procedure TJclSimpleXMLElems.BinaryValue(const Name: string;
  const Stream: TStream);
var
  Elem: TJclSimpleXMLElem;
begin
  Elem := GetItemNamed(Name);
  if Elem <> nil then
    Elem.GetBinaryValue(Stream);
end;

function TJclSimpleXMLElems.BoolValue(const Name: string; Default: Boolean): Boolean;
var
  Elem: TJclSimpleXMLElem;
begin
  try
    Elem := GetItemNamedDefault(Name, BoolToStr(Default));
    if (Elem = nil) or (Elem.Value = '') then
      Result := Default
    else
      Result := Elem.BoolValue;
  except
    Result := Default;
  end;
end;

procedure TJclSimpleXMLElems.Clear;
var
  I: Integer;
begin
  if FElems <> nil then
  begin
    for I := 0 to FElems.Count - 1 do
    begin
      // TJclSimpleXMLElem(FElems.Objects[I]).Clear; // (p3) not needed -called in Destroy
      FElems.Objects[I].Free;
      FElems.Objects[I] := nil;
    end;
    FElems.Clear;
  end;
  if FNamedElems <> nil then
  begin
    for I := 0 to FNamedElems.Count - 1 do
    begin
      FNamedElems.Objects[I].Free;
      FNamedElems.Objects[I] := nil;
    end;
    FNamedElems.Clear;
  end;
end;

constructor TJclSimpleXMLElems.Create(const AOwner: TJclSimpleXMLElem);
begin
  inherited Create;
  FParent := AOwner;
end;

procedure TJclSimpleXMLElems.CreateElems;
begin
  if FElems = nil then
    FElems := THashedStringList.Create;
end;

procedure TJclSimpleXMLElems.Delete(const Index: Integer);
var
  Elem: TJclSimpleXMLElem;
  NamedIndex: Integer;
begin
  if (FElems <> nil) and (Index >= 0) and (Index < FElems.Count) then
  begin
    Elem := TJclSimpleXMLElem(FElems.Objects[Index]);
    if FNamedElems <> nil then
    begin
      NamedIndex := FNamedElems.IndexOf(Elem.Name);
      if NamedIndex >= 0 then
        TJclSimpleXMLNamedElems(FNamedElems.Objects[NamedIndex]).FItems.Remove(Elem);
    end;
    FElems.Delete(Index);
  end;
end;

procedure TJclSimpleXMLElems.Delete(const Name: string);
begin
  if FElems <> nil then
    Delete(FElems.IndexOf(Name));
end;

destructor TJclSimpleXMLElems.Destroy;
begin
  FParent := nil;
  Clear;
  FreeAndNil(FElems);
  FreeAndNil(FNamedElems);
  inherited Destroy;
end;

procedure TJclSimpleXMLElems.DoItemRename(Value: TJclSimpleXMLElem; const Name: string);
var
  I: Integer;
  NamedIndex: Integer;
begin
  if FNamedElems <> nil then
  begin
    NamedIndex := FNamedElems.IndexOf(Value.Name);
    if NamedIndex >= 0 then
      TJclSimpleXMLNamedElems(FNamedElems.Objects[NamedIndex]).FItems.Remove(Value);
  end;

  I := FElems.IndexOfObject(Value);
  if I <> -1 then
    FElems.Strings[I] := Name;

  if FNamedElems <> nil then
  begin
    NamedIndex := FNamedElems.IndexOf(Name);
    if NamedIndex >= 0 then
      TJclSimpleXMLNamedElems(FNamedElems.Objects[NamedIndex]).FItems.Add(Value);
  end;
end;

function TJclSimpleXMLElems.GetCount: Integer;
begin
  if FElems = nil then
    Result := 0
  else
    Result := FElems.Count;
end;

function TJclSimpleXMLElems.GetItem(const Index: Integer): TJclSimpleXMLElem;
begin
  if (FElems = nil) or (Index > FElems.Count) then
    Result := nil
  else
    Result := TJclSimpleXMLElem(FElems.Objects[Index]);
end;

function TJclSimpleXMLElems.GetItemNamedDefault(const Name, Default: string): TJclSimpleXMLElem;
var
  I: Integer;
begin
  Result := nil;
  if FElems <> nil then
  begin
    I := FElems.IndexOf(Name);
    if I <> -1 then
      Result := TJclSimpleXMLElem(FElems.Objects[I])
    else
    if Assigned(Parent) and Assigned(Parent.SimpleXML) and (sxoAutoCreate in Parent.SimpleXML.Options) then
      Result := Add(Name, Default);
  end
  else
  if Assigned(Parent) and Assigned(Parent.SimpleXML) and (sxoAutoCreate in Parent.SimpleXML.Options) then
    Result := Add(Name, Default);
end;

function TJclSimpleXMLElems.GetNamedElems(const Name: string): TJclSimpleXMLNamedElems;
var
  NamedIndex: Integer;
begin
  if FNamedElems = nil then
    FNamedElems := THashedStringList.Create;
  NamedIndex := FNamedElems.IndexOf(Name);
  if NamedIndex = -1 then
  begin
    Result := TJclSimpleXMLNamedElems.Create(Self, Name);
    FNamedElems.AddObject(Name, Result);
    if FElems <> nil then
      for NamedIndex := 0 to FElems.Count - 1 do                 
        if FElems.Strings[NamedIndex] = Name then
          Result.FItems.Add(FElems.Objects[NamedIndex]);
  end
  else
    Result := TJclSimpleXMLNamedElems(FNamedElems.Objects[NamedIndex]);
end;

function TJclSimpleXMLElems.GetItemNamed(const Name: string): TJclSimpleXMLElem;
begin
  Result := GetItemNamedDefault(Name, '');
end;

function TJclSimpleXMLElems.IntValue(const Name: string; Default: Int64): Int64;
var
  Elem: TJclSimpleXMLElem;
begin
  Elem := GetItemNamedDefault(Name, IntToStr(Default));
  if Elem = nil then
    Result := Default
  else
    Result := Elem.IntValue;
end;

function TJclSimpleXMLElems.LoadFromStream(const Stream: TStream; AParent: TJclSimpleXML): string;
type
  TReadStatus = (rsWaitingTag, rsReadingTagKind, rsProcessingEndTag); 
var
  I, lStreamPos, Count: Integer;
  lPos: TReadStatus;
  lBuf: array [0..cBufferSize - 1] of Char;
  St: string;
  Po: string;
  lElem: TJclSimpleXMLElem;
  Ch: Char;
  lTrimWhiteSpace, lContainsWhiteSpace: Boolean;
  lStartOfContentPos, lTempStreamPos: Integer;
begin
  lStreamPos := Stream.Position;
  Result := '';
  Po := '';
  St := '';
  lPos := rsWaitingTag;

  // Preserve old preceeding whitespace trimming behaviour
  lTrimWhiteSpace := Assigned(AParent) and (sxoTrimPrecedingTextWhitespace in AParent.Options);

  // We read from a stream, thus replacing the existing items
  Clear;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    lContainsWhiteSpace := False;
    lStartOfContentPos := lStreamPos;
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);
      Ch := lBuf[I];

      case lPos of
        rsWaitingTag: //We are waiting for a tag and thus avoiding spaces
          begin
            case Ch of
              ' ', AnsiTab, AnsiCarriageReturn, AnsiLineFeed:
                begin
                  lContainsWhiteSpace := True;
                end;
              '<':
                begin
                  lPos := rsReadingTagKind;
                  St := Ch;
                end;
            else
              begin
                  //This is a text
                lElem := TJclSimpleXMLElemText.Create(Parent);
                if lTrimWhiteSpace then
                  Stream.Seek(lStreamPos - 1, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP})
                else
                  Stream.Seek(lStartOfContentPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
                lElem.LoadFromStream(Stream, AParent);
                lStreamPos := Stream.Position;
                CreateElems;
                FElems.AddObject(lElem.Name, lElem);
                Break;
              end;
            end;
          end;

        rsReadingTagKind: //We are trying to determine the kind of the tag
          begin
            lElem := nil;
            case Ch of
              '/':
                if St = '<' then
                begin
                  lPos := rsProcessingEndTag;
                  St := '';
                end
                else
                begin
                  lElem := TJclSimpleXMLElemClassic.Create(Parent);
                  St := St + Ch;
                end;

              ' ', '>', ':': //This should be a classic tag
                begin
                  lElem := TJclSimpleXMLElemClassic.Create(Parent);
                  St := St + Ch;
                end;
            else
              begin
                if (St <> '<![CDATA') or not (AnsiChar(Ch) in [' ', AnsiTab, AnsiCarriageReturn, AnsiLineFeed]) then
                  St := St + Ch;
                if St = '<![CDATA[' then
                  lElem := TJclSimpleXMLElemCData.Create(Parent)
                else
                if St = '<!--' then
                  lElem := TJclSimpleXMLElemComment.Create(Parent);
                  //<?
              end;
            end;

            if lElem <> nil then
            begin
              CreateElems;
              Stream.Seek(lStreamPos - (Length(St)), {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
              lElem.LoadFromStream(Stream, AParent);
              lStreamPos := Stream.Position;
              FElems.AddObject(lElem.Name, lElem);
              Notify(lElem, opInsert);
              St := '';
              lPos := rsWaitingTag;
              Break;
            end;
          end;

        rsProcessingEndTag: //This is an end tag
          case Ch of
            '>':
              begin
                if Po <> '' then
                  Result := Po + ':' + St
                else
                  Result := St;
                Count := 0;

                // We have reached an end tag. If whitespace was found while
                // waiting for the end tag, and the user told us to keep it
                // then we have to create a text element.
                // But it must only be created if there are not other elements
                // in the list. If we did not check this, we would create a
                // text element for whitespace found between two adjacent end
                // tags.  
                if lContainsWhiteSpace and not lTrimWhiteSpace  and
                   (not Assigned(FElems) or (FElems.Count=0))then
                begin
                  lTempStreamPos := Stream.Position;
                  lElem := TJclSimpleXMLElemText.Create(Parent);
                  Stream.Seek(lStartOfContentPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
                  lElem.LoadFromStream(Stream, AParent);
                  CreateElems;
                  FElems.AddObject(lElem.Name, lElem);
                  Stream.Seek(lTempStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
                end;

                Break;
              end;
            ':':
              begin
                Po := St;
                St := '';
              end;
          else
            St := St + Ch;
          end;
      end;
    end;
  until Count = 0;

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TJclSimpleXMLElems.Notify(Value: TJclSimpleXMLElem;
  Operation: TOperation);
var
  NamedIndex: Integer;
begin
  case Operation of
    opRemove:
      if Value.Container = Self then  // Only remove if we have it
      begin
        if FNamedElems <> nil then
        begin
          NamedIndex := FNamedElems.IndexOf(Value.Name);
          if NamedIndex >= 0 then
            TJclSimpleXMLNamedElems(FNamedElems.Objects[NamedIndex]).FItems.Remove(Value);
        end;
        FElems.Delete(FElems.IndexOfObject(Value));
      end;
    opInsert:
      Value.Container := Self;
  end;
end;

function TJclSimpleXMLElems.Remove(Value: TJclSimpleXMLElem): Integer;
begin
  Result := FElems.IndexOfObject(Value);
  Notify(Value, opRemove);
end;

procedure TJclSimpleXMLElems.SaveToStream(const Stream: TStream;
  const Level: string; AParent: TJclSimpleXML);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Item[I].SaveToStream(Stream, Level, AParent);
end;

function TJclSimpleXMLElems.Value(const Name: string; Default: string): string;
var
  Elem: TJclSimpleXMLElem;
begin
  Result := '';
  Elem := GetItemNamedDefault(Name, Default);
  if Elem = nil then
    Result := Default
  else
    Result := Elem.Value;
end;

procedure TJclSimpleXMLElems.Move(const CurIndex, NewIndex: Integer);
begin
  if FElems <> nil then
    FElems.Move(CurIndex, NewIndex);
end;

function TJclSimpleXMLElems.IndexOf(const Value: TJclSimpleXMLElem): Integer;
begin
  if FElems = nil then
    Result := -1
  else
    Result := FElems.IndexOfObject(Value);
end;

function TJclSimpleXMLElems.IndexOf(const Name: string): Integer;
begin
  if FElems = nil then
    Result := -1
  else
    Result := FElems.IndexOf(Name);
end;

procedure TJclSimpleXMLElems.InsertChild(const Value: TJclSimpleXMLElem; Index: Integer);
var
  NamedIndex: Integer;
begin
  CreateElems;

  // If there already is a container, notify it to remove the element
  if Assigned(Value.Container) then
  begin
    Value.Container.Notify(Value, opRemove);
    Value.Parent := Parent;
  end;

  FElems.InsertObject(Index, Value.Name, Value);

  if FNamedElems <> nil then
  begin
    NamedIndex := FNamedElems.IndexOf(Value.Name);
    if NamedIndex >= 0 then
      TJclSimpleXMLNamedElems(FNamedElems.Objects[NamedIndex]).FItems.Add(Value);
  end;

  Notify(Value, opInsert);
end;

function TJclSimpleXMLElems.Insert(Value: TJclSimpleXMLElem;
  Index: Integer): TJclSimpleXMLElem;
begin
  if Value <> nil then
    InsertChild(Value, Index);
  Result := Value;
end;

function TJclSimpleXMLElems.Insert(const Name: string;
  Index: Integer): TJclSimpleXMLElemClassic;
begin
  Result := TJclSimpleXMLElemClassic.Create(Parent);
  Result.FName := Name; //Directly set parent to avoid notification
  InsertChild(Result, Index);
end;

function SortItems(List: TStringList; Index1, Index2: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to GSorts.Count - 1 do
    if TJclSimpleXMLElems(GSorts[I]).FElems = List then
    begin
      Result := TJclSimpleXMLElems(GSorts[I]).FCompare(TJclSimpleXMLElems(GSorts[I]), Index1, Index2);
      Break;
    end;
end;

procedure TJclSimpleXMLElems.CustomSort(AFunction: TJclSimpleXMLElemCompare);
begin
  if FElems <> nil then
  begin
    GSorts.Add(Self);
    FCompare := AFunction;
    FElems.CustomSort(SortItems);
    GSorts.Remove(Self);
  end;
end;

procedure TJclSimpleXMLElems.Sort;
begin
  if FElems <> nil then
    FElems.Sort;
end;

//=== { TJclSimpleXMLProps } =================================================

function TJclSimpleXMLProps.Add(const Name, Value: string): TJclSimpleXMLProp;
var
  Elem: TJclSimpleXMLProp;
begin
  if FProperties = nil then
    FProperties := THashedStringList.Create;
  Elem := TJclSimpleXMLProp.Create();
  FProperties.AddObject(Name, Elem);
  Elem.FName := Name; //Avoid notification
  Elem.Value := Value;
  Elem.Parent := Self;
  Result := Elem;
end;

function TJclSimpleXMLProps.Add(const Name: string; const Value: Int64): TJclSimpleXMLProp;
begin
  Result := Add(Name, IntToStr(Value));
end;

function TJclSimpleXMLProps.Add(const Name: string; const Value: Boolean): TJclSimpleXMLProp;
begin
  Result := Add(Name, BoolToStr(Value));
end;

function TJclSimpleXMLProps.Insert(const Index: Integer; const Name, Value: string): TJclSimpleXMLProp;
var
  Elem: TJclSimpleXMLProp;
begin
  if FProperties = nil then
    FProperties := THashedStringList.Create;
  Elem := TJclSimpleXMLProp.Create();
  FProperties.InsertObject(Index, Name, Elem);
  Elem.FName := Name; //Avoid notification
  Elem.Value := Value;
  Elem.Parent := Self;
  Result := Elem;
end;

function TJclSimpleXMLProps.Insert(const Index: Integer; const Name: string; const Value: Int64): TJclSimpleXMLProp;
begin
  Result := Insert(Index, Name, IntToStr(Value));
end;

function TJclSimpleXMLProps.Insert(const Index: Integer; const Name: string; const Value: Boolean): TJclSimpleXMLProp;
begin
  Result := Insert(Index, Name, BoolToStr(Value));
end;

function TJclSimpleXMLProps.BoolValue(const Name: string; Default: Boolean): Boolean;
var
  Prop: TJclSimpleXMLProp;
begin
  try
    Prop := GetItemNamedDefault(Name, BoolToStr(Default));
    if (Prop = nil) or (Prop.Value = '') then
      Result := Default
    else
      Result := Prop.BoolValue;
  except
    Result := Default;
  end;
end;

procedure TJclSimpleXMLProps.Clear;
var
  I: Integer;
begin
  if FProperties <> nil then
  begin
    for I := 0 to FProperties.Count - 1 do
    begin
      TJclSimpleXMLProp(FProperties.Objects[I]).Free;
      FProperties.Objects[I] := nil;
    end;
    FProperties.Clear;
  end;
end;

procedure TJclSimpleXMLProps.Delete(const Index: Integer);
begin
  if (FProperties <> nil) and (Index >= 0) and (Index < FProperties.Count) then
  begin
    TObject(FProperties.Objects[Index]).Free;
    FProperties.Delete(Index);
  end;
end;

constructor TJclSimpleXMLProps.Create(Parent: TJclSimpleXMLElem);
begin
  inherited Create;
  FParent := Parent;
end;

procedure TJclSimpleXMLProps.Delete(const Name: string);
begin
  if FProperties <> nil then
    Delete(FProperties.IndexOf(Name));
end;

destructor TJclSimpleXMLProps.Destroy;
begin
  FParent := nil;
  Clear;
  FreeAndNil(FProperties);
  inherited Destroy;
end;

procedure TJclSimpleXMLProps.DoItemRename(Value: TJclSimpleXMLProp; const Name: string);
var
  I: Integer;
begin
  if FProperties = nil then
    Exit;
  I := FProperties.IndexOfObject(Value);
  if I <> -1 then
    FProperties[I] := Name;
end;

procedure TJclSimpleXMLProps.Error(const S: string);
begin
  raise EJclSimpleXMLError.Create(S);
end;

procedure TJclSimpleXMLProps.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S, Args));
end;

function TJclSimpleXMLProps.GetCount: Integer;
begin
  if FProperties = nil then
    Result := 0
  else
    Result := FProperties.Count;
end;

function TJclSimpleXMLProps.GetItem(const Index: Integer): TJclSimpleXMLProp;
begin
  if FProperties <> nil then
    Result := TJclSimpleXMLProp(FProperties.Objects[Index])
  else
    Result := nil;
end;

function TJclSimpleXMLProps.GetItemNamedDefault(const Name, Default: string): TJclSimpleXMLProp;
var
  I: Integer;
begin
  Result := nil;
  if FProperties <> nil then
  begin
    I := FProperties.IndexOf(Name);
    if I <> -1 then
      Result := TJclSimpleXMLProp(FProperties.Objects[I])
    else
    if Assigned(FParent) and Assigned(FParent.SimpleXML) and (sxoAutoCreate in FParent.SimpleXML.Options) then
      Result := Add(Name, Default);
  end
  else
  if Assigned(FParent) and Assigned(FParent.SimpleXML) and (sxoAutoCreate in FParent.SimpleXML.Options) then
  begin
    Result := Add(Name, Default);
  end;
end;

function TJclSimpleXMLProps.GetItemNamed(const Name: string): TJclSimpleXMLProp;
begin
  Result := GetItemNamedDefault(Name, '');
end;

function TJclSimpleXMLProps.GetSimpleXML: TJclSimpleXML;
begin
  if FParent <> nil then
    Result := FParent.GetSimpleXML
  else
    Result := nil;
end;

function TJclSimpleXMLProps.IntValue(const Name: string; Default: Int64): Int64;
var
  Prop: TJclSimpleXMLProp;
begin
  Prop := GetItemNamedDefault(Name, IntToStr(Default));
  if Prop = nil then
    Result := Default
  else
    Result := Prop.IntValue;
end;

procedure TJclSimpleXMLProps.LoadFromStream(const Stream: TStream);
//<element Prop="foo" Prop='bar' foo:bar="beuh"/>
//Stop on / or ? or >
type
  TPosType = (
    ptWaiting,
    ptReadingName,
    ptStartingContent,
    ptReadingValue,
    ptSpaceBeforeEqual
    );
var
  lPos: TPosType;
  I, lStreamPos, Count: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  lName, lValue, lNameSpace: string;
  lPropStart: Char;
  Ch: Char;
begin
  lStreamPos := Stream.Position;
  lValue := '';
  lNameSpace := '';
  lName := '';
  lPropStart := ' ';
  lPos := ptWaiting;

  // We read from a stream, thus replacing the existing properties
  Clear;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);
      Ch := lBuf[I];

      case lPos of
        ptWaiting: //We are waiting for a property
          begin
            case Ch of
              ' ', AnsiTab, AnsiCarriageReturn, AnsiLineFeed:
                begin
                end;
              'a'..'z', 'A'..'Z', '0'..'9', '-', '_':
                begin
                  lName := Ch;
                  lNameSpace := '';
                  lPos := ptReadingName;
                end;
              '/', '>', '?':
                begin
                  Dec(lStreamPos);
                  Count := 0;
                  Break;
                end;
            else
              FmtError(RsEInvalidXMLElementUnexpectedCharacte, [Ch]);
            end;
          end;

        ptReadingName: //We are reading a property name
          case Ch of
            'a'..'z', 'A'..'Z', '0'..'9', '-', '_':
              lName := lName + Ch;
            ':':
              begin
                lNameSpace := lName;
                lName := '';
              end;
            '=':
              lPos := ptStartingContent;
            ' ', AnsiTab, AnsiCarriageReturn, AnsiLineFeed:
              lPos := ptSpaceBeforeEqual;
          else
            FmtError(RsEInvalidXMLElementUnexpectedCharacte, [Ch]);
          end;

        ptStartingContent: //We are going to start a property content
          case Ch of
            ' ', AnsiTab, AnsiCarriageReturn, AnsiLineFeed:
              ; // ignore white space
            '''', '"':
              begin
                lPropStart := Ch;
                lValue := '';
                lPos := ptReadingValue;
              end;
          else
            FmtError(RsEInvalidXMLElementUnexpectedCharacte_, [Ch]);
          end;
        ptReadingValue: //We are reading a property
          if Ch = lPropStart then
          begin
            if GetSimpleXML <> nil then
              GetSimpleXML.DoDecodeValue(lValue);
            with Add(lName, lValue) do
              NameSpace := lNameSpace;
            lPos := ptWaiting;
          end
          else
            lValue := lValue + Ch;
        ptSpaceBeforeEqual: // We are reading the white space between a property name and the = sign
          case Ch of
            ' ', AnsiTab, AnsiCarriageReturn, AnsiLineFeed:
              ; // more white space, stay in this state and ignore
            '=':
              lPos := ptStartingContent;
          else
            FmtError(RsEInvalidXMLElementUnexpectedCharacte, [Ch]);
          end;
      else
        Assert(False, RsEUnexpectedValueForLPos);
      end;
    end;
  until Count = 0;

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TJclSimpleXMLProps.SaveToStream(const Stream: TStream);
var
  St: string;
  I: Integer;
begin
  St := '';
  for I := 0 to Count - 1 do
    St := St + Item[I].SaveToString;
  if St <> '' then
    WriteStringToStream(Stream, St, Length(St));
end;

function TJclSimpleXMLProps.Value(const Name: string; Default: string): string;
var
  Prop: TJclSimpleXMLProp;
begin
  Result := '';
  Prop := GetItemNamedDefault(Name, Default);
  if Prop = nil then
    Result := Default
  else
    Result := Prop.Value;
end;

//=== { TJclSimpleXMLProp } ==================================================

function TJclSimpleXMLProp.GetBoolValue: Boolean;
begin
  Result := StrToBoolDef(Value, False);
end;

function TJclSimpleXMLProp.GetFloatValue: Extended;
begin
  if not TryStrToFloat(Value, Result) then
    Result := 0.0;
end;

function TJclSimpleXMLProp.FullName: string;
begin
  if FNameSpace <> '' then
    Result := FNameSpace + ':' + Name
  else
    Result := Name;
end;

function TJclSimpleXMLProp.GetIntValue: Int64;
begin
  Result := StrToInt64Def(Value, -1);
end;

function TJclSimpleXMLProp.GetSimpleXML: TJclSimpleXML;
begin
  if (FParent <> nil) and (FParent.FParent <> nil) then
    Result := FParent.FParent.GetSimpleXML
  else
    Result := nil;
end;

function TJclSimpleXMLProp.SaveToString: string;
var
  AEncoder: TJclSimpleXML;
  tmp:string;
begin
  AEncoder := GetSimpleXML;
  tmp := FValue;
  if NameSpace <> '' then
  begin
    if AEncoder <> nil then
      AEncoder.DoEncodeValue(tmp);
    Result := Format(' %s:%s="%s"', [NameSpace, Name, tmp]);
  end
  else
  begin
    if AEncoder <> nil then
      AEncoder.DoEncodeValue(tmp);
    Result := Format(' %s="%s"', [Name, tmp]);
  end;
end;

procedure TJclSimpleXMLProp.SetBoolValue(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;

procedure TJclSimpleXMLProp.SetFloatValue(const Value: Extended);
begin
  FValue := FloatToStr(Value);
end;

procedure TJclSimpleXMLProp.SetIntValue(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;

procedure TJclSimpleXMLProp.SetName(const Value: string);
begin
  if (Value <> FName) and (Value <> '') then
  begin
    if (Parent <> nil) and (FName <> '') then
      Parent.DoItemRename(Self, Value);
    FName := Value;
  end;
end;

//=== { TJclSimpleXMLElemClassic } ===========================================

procedure TJclSimpleXMLElemClassic.LoadFromStream(const Stream: TStream; AParent: TJclSimpleXML);
//<element Prop="foo" Prop='bar'/>
//<element Prop="foo" Prop='bar'>foor<b>beuh</b>bar</element>
//<xml:element Prop="foo" Prop='bar'>foor<b>beuh</b>bar</element>
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  St, lName, lValue, lNameSpace: string;
  Ch: Char;
begin
  lStreamPos := Stream.Position;
  St := '';
  lValue := '';
  lNameSpace := '';
  lPos := 1;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);
      Ch := lBuf[I];

      case lPos of
        1:
          if Ch = '<' then
            lPos := 2
          else
            FmtError(RsEInvalidXMLElementExpectedBeginningO, [Ch]);
        -1:
          if Ch = '>' then
          begin
            Count := 0;
            Break;
          end
          else
            FmtError(RsEInvalidXMLElementExpectedEndOfTagBu, [Ch]);
      else
        begin
          if AnsiChar(Ch) in [AnsiTab, AnsiLineFeed, AnsiCarriageReturn, ' ' {, '.'}] then
          begin
            if lPos = 2 then
              Error(RsEInvalidXMLElementMalformedTagFoundn);
            Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
            Properties.LoadFromStream(Stream);
            lStreamPos := Stream.Position;
            Break; //Re read buffer
          end
          else
          begin
            case Ch of
              '>':
                begin
                  lName := St;
                  //Load elements
                  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
                  St := Items.LoadFromStream(Stream, AParent);
                  if lNameSpace <> '' then
                  begin
                    if not AnsiSameText(AnsiString(lNameSpace + ':' + lName), AnsiString(St)) then
                      FmtError(RsEInvalidXMLElementErroneousEndOfTagE, [lName, St]);
                  end
                  else
                    if not AnsiSameText(AnsiString(lName), AnsiString(St)) then
                      FmtError(RsEInvalidXMLElementErroneousEndOfTagE, [lName, St]);
                  lStreamPos := Stream.Position;

                  //Set value if only one sub element
                  //This might reduce speed, but this is for compatibility issues
                  if (Items.Count = 1) and (Items[0] is TJclSimpleXMLElemText) then
                  begin
                    lValue := Items[0].Value;
                    Items.Clear;
                  end;

                  Count := 0;
                  Break;
                end;
              '/':
                begin
                  lName := St;
                  lPos := -1;
                end;
              ':':
                begin
                  lNameSpace := St;
                  St := '';
                end;
            else
              begin
                St := St + Ch;
                Inc(lPos);
              end;
            end;
          end;
        end;
      end;
    end;
  until Count = 0;

  Name := lName;
  if GetSimpleXML <> nil then
    GetSimpleXML.DoDecodeValue(lValue);
  Value := lValue;
  NameSpace := lNameSpace;

  if AParent <> nil then
  begin
    AParent.DoTagParsed(lName);
    AParent.DoValueParsed(lName, lValue);
  end;

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TJclSimpleXMLElemClassic.SaveToStream(const Stream: TStream; const Level: string; AParent: TJclSimpleXML);
var
  St, AName, tmp: string;
  LevelAdd: string;
begin
  if(NameSpace <> '') then
  begin
    AName := NameSpace + ':' + Name;
  end
  else
  begin
    AName := Name;
  end;

  if Name <> '' then
  begin
    if GetSimpleXML <> nil then
       GetSimpleXML.DoEncodeValue(AName);
    St := Level + '<' + AName;

    WriteStringToStream(Stream, St, Length(St));
    Properties.SaveToStream(Stream);
  end;

  if (Items.Count = 0) then
  begin
    tmp := FValue;
    if (Name <> '') then
    begin
      if Value = '' then
        St := '/>' + sLineBreak
      else
      begin
        if GetSimpleXML <> nil then
          GetSimpleXML.DoEncodeValue(tmp);
        St := '>' + tmp + '</' + AName + '>' + sLineBreak;
      end;
      WriteStringToStream(Stream, St, Length(St));
    end;
  end
  else
  begin
    if (Name <> '') then
    begin
      St := '>' + sLineBreak;
      WriteStringToStream(Stream, St, Length(St));
    end;
    if Assigned(SimpleXML) and
      (sxoAutoIndent in SimpleXML.Options) then
    begin
      LevelAdd := SimpleXML.IndentString;
    end;
    Items.SaveToStream(Stream, Level + LevelAdd, AParent);
    if Name <> '' then
    begin
      St := Level + '</' + AName + '>' + sLineBreak;
      WriteStringToStream(Stream, St, Length(St));
    end;
  end;
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemComment } ===========================================

procedure TJclSimpleXMLElemComment.LoadFromStream(const Stream: TStream; AParent: TJclSimpleXML);
//<!-- declarations for <head> & <body> -->
const
  CS_START_COMMENT = '<!--';
  CS_STOP_COMMENT = '    -->';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  St: string;
  lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  St := '';
  lPos := 1;
  lOk := False;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..4: //<!--
          if lBuf[I] = CS_START_COMMENT[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidCommentExpectedsButFounds, [CS_START_COMMENT[lPos], lBuf[I]]);
        5:
          if lBuf[I] = CS_STOP_COMMENT[lPos] then
            Inc(lPos)
          else
            St := St + lBuf[I];
        6: //-
          if lBuf[I] = CS_STOP_COMMENT[lPos] then
            Inc(lPos)
          else
          begin
            St := St + '-' + lBuf[I];
            Dec(lPos);
          end;
        7: //>
          if lBuf[I] = CS_STOP_COMMENT[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
          begin
            if lBuf[I + 1] <> '>' then
              Error(RsEInvalidCommentNotAllowedInsideComme);
            St := St + '--' + lBuf[I];
            Dec(lPos, 2);
          end;
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(RsEInvalidCommentUnexpectedEndOfData);

  Value := St;
  Name := '';

  if AParent <> nil then
    AParent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TJclSimpleXMLElemComment.SaveToStream(const Stream: TStream; const Level: string; AParent: TJclSimpleXML);
var
  St: string;
begin
  St := Level + '<!--';
  WriteStringToStream(Stream, St, Length(St));
  if Value <> '' then
    WriteStringToStream(Stream, Value, Length(Value));
  St := '-->' + sLineBreak;
  WriteStringToStream(Stream, St, Length(St));
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemCData } =============================================

procedure TJclSimpleXMLElemCData.LoadFromStream(const Stream: TStream; AParent: TJclSimpleXML);
//<![CDATA[<greeting>Hello, world!</greeting>]]>
const
  CS_START_CDATA = '<![CDATA[';
  CS_STOP_CDATA =  '         ]]>';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  St: string;
  lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  St := '';
  lPos := 1;
  lOk := False;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..9: //<![CDATA[
          if lBuf[I] = CS_START_CDATA[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidCDATAExpectedsButFounds, [CS_START_CDATA[lPos], lBuf[I]]);
        10:
          if lBuf[I] = CS_STOP_CDATA[lPos] then
            Inc(lPos)
          else
            St := St + lBuf[I];
        11: //-
          if lBuf[I] = CS_STOP_CDATA[lPos] then
            Inc(lPos)
          else
          begin
            St := St + ']' + lBuf[I];
            Dec(lPos);
          end;
        12: //>
          if lBuf[I] = CS_STOP_CDATA[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
          begin
            St := St + ']]' + lBuf[I];
            Dec(lPos, 2);
          end;
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(RsEInvalidCDATAUnexpectedEndOfData);

  Value := St;
  Name := '';

  if AParent <> nil then
    AParent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TJclSimpleXMLElemCData.SaveToStream(const Stream: TStream; const Level: string; AParent: TJclSimpleXML);
var
  St: string;
begin
  St := Level + '<![CDATA[';
  WriteStringToStream(Stream, St, Length(St));
  if Value <> '' then
    WriteStringToStream(Stream, Value, Length(Value));
  St := ']]>' + sLineBreak;
  WriteStringToStream(Stream, St, Length(St));
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemText } ==============================================

procedure TJclSimpleXMLElemText.LoadFromStream(const Stream: TStream; AParent: TJclSimpleXML);
var
  I, lStreamPos, Count: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  St: string;
  StLength: Integer;
begin
  lStreamPos := Stream.Position;
  St := '';
  StLength := 0;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    SetLength(St, StLength + Count);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lBuf[I] of
        '<':
          begin
            //Quit text
            Dec(lStreamPos);
            Count := 0;
            Break;
          end;
      else
        begin
          Inc(StLength);
          St[StLength] := lBuf[I];
        end;
      end;
    end;
  until Count = 0;
  SetLength(St, StLength);
  if GetSimpleXML <> nil then
    GetSimpleXML.DoDecodeValue(St);
  Value := St;
  Name := '';

  if AParent <> nil then
    AParent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TJclSimpleXMLElemText.SaveToStream(const Stream: TStream; const Level: string; AParent: TJclSimpleXML);
var
  St, tmp: string;
begin
  if Value <> '' then
  begin
    tmp := Value;
    if GetSimpleXML <> nil then
      GetSimpleXML.DoEncodeValue(tmp);
    St := Level + tmp + sLineBreak;
    WriteStringToStream(Stream, St, Length(St));
  end;
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemHeader } ============================================

procedure TJclSimpleXMLElemHeader.Assign(Value: TJclSimpleXMLElem);
begin
  inherited Assign(Value);
  if Value is TJclSimpleXMLElemHeader then
  begin
    FStandalone := TJclSimpleXMLElemHeader(Value).FStandalone;
    FEncoding := TJclSimpleXMLElemHeader(Value).FEncoding;
    FVersion := TJclSimpleXMLElemHeader(Value).FVersion;
  end;
end;

constructor TJclSimpleXMLElemHeader.Create(const AOwner: TJclSimpleXMLElem);
begin
  inherited Create(AOwner);
  FVersion := '1.0';
  FEncoding := 'iso-8859-1';
  FStandalone := False;
end;

procedure TJclSimpleXMLElemHeader.LoadFromStream(const Stream: TStream; AParent: TJclSimpleXML);
//<?xml version="1.0" encoding="iso-xyzxx" standalone="yes"?>
const
  CS_START_HEADER = '<?xml';
  CS_STOP_HEADER = '     ?>';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  lPos := 1;
  lOk := False;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..4: //<?xml
          if lBuf[I] = CS_START_HEADER[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidHeaderExpectedsButFounds, [CS_START_HEADER[lPos], lBuf[I]]);
        5: //L
          if lBuf[I] = CS_START_HEADER[lPos] then
          begin
            Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
            Properties.LoadFromStream(Stream);
            lStreamPos := Stream.Position;
            Inc(lPos);

            FVersion := Properties.Value('version');
            FEncoding := Properties.Value('encoding');
            FStandalone := Properties.Value('standalone') = 'yes';

            Properties.Clear;

            Break; //Re read buffer
          end
          else
            FmtError(RsEInvalidHeaderExpectedsButFounds, [CS_START_HEADER[lPos], lBuf[I]]);
        6: //?
          if lBuf[I] = CS_STOP_HEADER[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidHeaderExpectedsButFounds, [CS_STOP_HEADER[lPos], lBuf[I]]);
        7: //>
          if lBuf[I] = CS_STOP_HEADER[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
            FmtError(RsEInvalidHeaderExpectedsButFounds, [CS_STOP_HEADER[lPos], lBuf[I]]);
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(RsEInvalidCommentUnexpectedEndOfData);

  Name := '';

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TJclSimpleXMLElemHeader.SaveToStream(const Stream: TStream;
  const Level: string; AParent: TJclSimpleXML);
var
  St: string;
begin
  St := Level + '<?xml version="' + FVersion + '"';
  if Encoding <> '' then
    St := St + ' encoding="' + Encoding + '"';
  if StandAlone then
    St := St + ' standalone="yes"';
  St := St + '?>' + sLineBreak;
  WriteStringToStream(Stream, St, Length(St));
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemDocType } ===========================================

procedure TJclSimpleXMLElemDocType.LoadFromStream(const Stream: TStream; AParent: TJclSimpleXML);
{
<!DOCTYPE test [
<!ELEMENT test (#PCDATA) >
<!ENTITY % xx '&#37;zz;'>
<!ENTITY % zz '&#60;!ENTITY tricky "error-prone" >' >
%xx;
]>

<!DOCTYPE greeting SYSTEM "hello.dtd">
}
const
  CS_START_DOCTYPE = '<!DOCTYPE';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  lOk: Boolean;
  lChar: Char;
  St: string;
begin
  lStreamPos := Stream.Position;
  lPos := 1;
  lOk := False;
  lChar := '>';
  St := '';

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..9: //<!DOCTYPE
          if lBuf[I] = CS_START_DOCTYPE[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidHeaderExpectedsButFounds, [CS_START_DOCTYPE[lPos], lBuf[I]]);
        10: //]> or >
          if lChar = lBuf[I] then
          begin
            if lChar = '>' then
            begin
              lOk := True;
              Count := 0;
              Break; //This is the end
            end
            else
            begin
              St := St + lBuf[I];
              lChar := '>';
            end;
          end
          else
          begin
            St := St + lBuf[I];
            if lBuf[I] = '[' then
              lChar := ']';
          end;
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(RsEInvalidCommentUnexpectedEndOfData);

  Name := '';
  Value := Trim(St);

  if AParent <> nil then
    AParent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TJclSimpleXMLElemDocType.SaveToStream(const Stream: TStream;
  const Level: string; AParent: TJclSimpleXML);
var
  St: string;
begin
  St := '<!DOCTYPE ' + Value + '>' + sLineBreak;
  WriteStringToStream(Stream, St, Length(St));
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemSheet } =============================================

procedure TJclSimpleXMLElemSheet.LoadFromStream(const Stream: TStream;
  AParent: TJclSimpleXML);
//<?xml-stylesheet alternate="yes" type="text/xsl" href="sheet.xsl"?>
const
  CS_START_PI = '<?xml-stylesheet';
  CS_STOP_PI = '                ?>';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  lPos := 1;
  lOk := False;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..15: //<?xml-stylesheet
          if lBuf[I] = CS_START_PI[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidStylesheetExpectedsButFounds, [CS_START_PI[lPos], lBuf[I]]);
        16: //L
          if lBuf[I] = CS_START_PI[lPos] then
          begin
            Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
            Properties.LoadFromStream(Stream);
            lStreamPos := Stream.Position;
            Inc(lPos);
            Break; //Re read buffer
          end
          else
            FmtError(RsEInvalidStylesheetExpectedsButFounds, [CS_START_PI[lPos], lBuf[I]]);
        17: //?
          if lBuf[I] = CS_STOP_PI[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidStylesheetExpectedsButFounds, [CS_STOP_PI[lPos], lBuf[I]]);
        18: //>
          if lBuf[I] = CS_STOP_PI[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
            FmtError(RsEInvalidStylesheetExpectedsButFounds, [CS_STOP_PI[lPos], lBuf[I]]);
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(RsEInvalidStylesheetUnexpectedEndOfDat);

  Name := '';

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TJclSimpleXMLElemSheet.SaveToStream(const Stream: TStream;
  const Level: string; AParent: TJclSimpleXML);
var
  I: Integer;
  St: string;
begin
  St := Level + '<?xml-stylesheet';
  for I := 0 to Properties.GetCount - 1 do
    St := St + Properties.Item[I].SaveToString;
  St := St + '?>' + sLineBreak;
  WriteStringToStream(Stream, St, Length(St));
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TJclSimpleXMLElemsProlog } ===========================================

constructor TJclSimpleXMLElemsProlog.Create;
begin
  inherited Create;
  FElems := THashedStringList.Create;
end;

destructor TJclSimpleXMLElemsProlog.Destroy;
begin
  Clear;
  FreeAndNil(FElems);
  inherited Destroy;
end;

procedure TJclSimpleXMLElemsProlog.Clear;
var
  I: Integer;
begin
  for I := 0 to FElems.Count - 1 do
  begin
    FElems.Objects[I].Free;
    FElems.Objects[I] := nil;
  end;
  FElems.Clear;
end;

function TJclSimpleXMLElemsProlog.GetCount: Integer;
begin
  Result := FElems.Count;
end;

function TJclSimpleXMLElemsProlog.GetItem(const Index: Integer): TJclSimpleXMLElem;
begin
  Result := TJclSimpleXMLElem(FElems.Objects[Index]);
end;

function TJclSimpleXMLElemsProlog.LoadFromStream(
  const Stream: TStream; AParent: TJclSimpleXML): string;
{<?xml version="1.0" encoding="UTF-8" ?>
<!-- Test -->
<!DOCTYPE greeting [
  <!ELEMENT greeting (#PCDATA)>
]>
<greeting>Hello, world!</greeting>

<?xml version="1.0"?> <!DOCTYPE greeting SYSTEM "hello.dtd"> <greeting>Hello, world!</greeting>
}
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  St: string;
  lEnd: Boolean;
  lElem: TJclSimpleXMLElem;
begin
  lStreamPos := Stream.Position;
  Result := '';
  St := '';
  lPos := 0;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        0: //We are waiting for a tag and thus avoiding spaces and any BOM
          begin
            case lBuf[I] of
              ' ', AnsiTab, AnsiCarriageReturn, AnsiLineFeed, #$00, #$FE, #$FF, #$EF, #$BB, #$BF:
                begin
                end;
              '<':
                begin
                  lPos := 1;
                  St := lBuf[I];
                end;
            else
              Error(RsEInvalidDocumentUnexpectedTextInFile);
            end;
          end;
        1: //We are trying to determine the kind of the tag
          begin
            lElem := nil;
            lEnd := False;

            if (St <> '<![CDATA') or not (AnsiChar(lBuf[I]) in [' ', AnsiTab, AnsiCarriageReturn, AnsiLineFeed]) then
              St := St + lBuf[I];
            if St = '<![CDATA[' then
              lEnd := True
            else
            if St = '<!--' then
              lElem := TJclSimpleXMLElemComment.Create(nil)
            else
            if St = '<?xml-stylesheet' then
              lElem := TJclSimpleXMLElemSheet.Create(nil)
            else
            if St = '<?xml ' then
              lElem := TJclSimpleXMLElemHeader.Create(nil)
            else
            if St = '<!DOCTYPE' then
              lElem := TJclSimpleXMLElemDocType.Create(nil)
            else
            if (Length(St) > 1) and not (AnsiChar(St[2]) in ['!', '?']) then
              lEnd := True;

            if lEnd then
            begin
              lStreamPos := lStreamPos - Length(St);
              Count := 0;
              Break;
            end
            else
            if lElem <> nil then
            begin
              Stream.Seek(lStreamPos - (Length(St)), {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
              lElem.LoadFromStream(Stream, AParent);
              lStreamPos := Stream.Position;
              FElems.AddObject(lElem.Name, lElem);
              St := '';
              lPos := 0;
              Break;
            end;
          end;
      end;
    end;
  until Count = 0;

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TJclSimpleXMLElemsProlog.SaveToStream(const Stream: TStream; AParent: TJclSimpleXML);
var
  I: Integer;
begin
  FindHeader;
  for I := 0 to Count - 1 do
    Item[I].SaveToStream(Stream, '', AParent);
end;

//=== { TJclSimpleHashTable } ================================================

constructor TJclSimpleHashTable.Create;
begin
  inherited Create;
  //XXX
  {$IFDEF CLR}
  FList := TJclHashRecord.Create;
  {$ELSE}
  New(FList);
  {$ENDIF CLR}
  FList.Count := 0;
  FList.Kind := hkDirect;
  FList.FirstElem := nil;
end;

destructor TJclSimpleHashTable.Destroy;
begin
  Clear;
  {$IFNDEF CLR}
  Dispose(FList);
  {$ENDIF !CLR}
  inherited Destroy;
end;

procedure TJclSimpleHashTable.AddObject(const AName: string;
  AObject: TObject);
begin
  //XXX
  {$IFDEF CLR}
  FList.FirstElem := TJclHashElem.Create;
  {$ELSE}
  New(FList.FirstElem);
  {$ENDIF CLR}
  //FList.FirstElem.Value := AName;
  //FList.FirstElem.Obj := nil;
end;

procedure TJclSimpleHashTable.Clear;
begin
  //XXX
end;

{$IFNDEF CLR}
{$IFDEF COMPILER6_UP}

function VarXML: TVarType;
begin
  Result := XMLVariant.VarType;
end;

procedure XMLCreateInto(var ADest: Variant; const AXML: TJclSimpleXMLElem);
begin
  TXMLVarData(ADest).vType := VarXML;
  TXMLVarData(ADest).XML := AXML;
end;

function XMLCreate(const AXML: TJclSimpleXMLElem): Variant;
begin
  XMLCreateInto(Result, AXML);
end;

function XMLCreate: Variant;
begin
  XMLCreateInto(Result, TJclSimpleXMLElemClassic.Create(nil));
end;

//=== { TXMLVariant } ========================================================

procedure TXMLVariant.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
begin
  if Source.vType = VarType then
  begin
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, TXMLVarData(Source).XML.SaveToString);
      varString:
        VarDataFromStr(Dest, TXMLVarData(Source).XML.SaveToString);
    else
      RaiseCastError;
    end;
  end
  else
    inherited CastTo(Dest, Source, AVarType);
end;

procedure TXMLVariant.Clear(var V: TVarData);
begin
  V.vType := varEmpty;
  TXMLVarData(V).XML := nil;
end;

procedure TXMLVariant.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
    with TXMLVarData(Dest) do
    begin
      vType := VarType;
      XML := TXMLVarData(Source).XML;
    end;
end;

function TXMLVariant.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
var
  LXML: TJclSimpleXMLElem;
  I, J, K: Integer;
begin
  Result := False;
  if (Length(Arguments) = 1) and (Arguments[0].vType in [vtInteger, vtExtended]) then
    with TXMLVarData(V) do
    begin
      K := Arguments[0].vInteger;
      J := 0;

      if K > 0 then
        for I := 0 to XML.Items.Count - 1 do
          if UpperCase(XML.Items[I].Name) = Name then
          begin
            Inc(J);
            if J = K then
              Break;
          end;

      if (J = K) and (J < XML.Items.Count) then
      begin
        LXML := XML.Items[J];
        if LXML <> nil then
        begin
          Dest.vType := VarXML;
          TXMLVarData(Dest).XML := LXML;
          Result := True;
        end
      end;
    end;
end;

function TXMLVariant.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
var
  LXML: TJclSimpleXMLElem;
  lProp: TJclSimpleXMLProp;
begin
  Result := False;
  with TXMLVarData(V) do
  begin
    LXML := XML.Items.ItemNamed[Name];
    if LXML <> nil then
    begin
      Dest.vType := VarXML;
      TXMLVarData(Dest).XML := LXML;
      Result := True;
    end
    else
    begin
      lProp := XML.Properties.ItemNamed[Name];
      if lProp <> nil then
      begin
        VarDataFromOleStr(Dest, lProp.Value);
        Result := True;
      end;
    end;
  end;
end;

function TXMLVariant.IsClear(const V: TVarData): Boolean;
begin
  Result := (TXMLVarData(V).XML = nil) or (TXMLVarData(V).XML.Items.Count = 0);
end;

function TXMLVariant.SetProperty(const V: TVarData; const Name: string;
  const Value: TVarData): Boolean;

  function GetStrValue: string;
  begin
    try
      Result := Value.VOleStr;
    except
      Result := '';
    end;
  end;

var
  LXML: TJclSimpleXMLElem;
  lProp: TJclSimpleXMLProp;
begin
  Result := False;
  with TXMLVarData(V) do
  begin
    LXML := XML.Items.ItemNamed[Name];
    if LXML = nil then
    begin
      lProp := XML.Properties.ItemNamed[Name];
      if lProp <> nil then
      begin
        lProp.Value := GetStrValue;
        Result := True;
      end;
    end
    else
    begin
      LXML.Value := GetStrValue;
      Result := True;
    end;
  end;
end;

{$ENDIF COMPILER6_UP}
{$ENDIF !CLR}

procedure TJclSimpleXMLElemsProlog.Error(const S: string);
begin
  raise EJclSimpleXMLError.Create(S);
end;

procedure TJclSimpleXMLElemsProlog.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S, Args));
end;

procedure TJclSimpleXML.SetIndentString(const Value: string);
var
  I: Integer;
begin
  // test if the new value is only made of spaces or tabs
  for I := 1 to Length(Value) do
    if not (AnsiChar(Value[I]) in [AnsiTab, ' ']) then
      Exit;
  FIndentString := Value;
end;

procedure TJclSimpleXML.SetRoot(const Value: TJclSimpleXMLElemClassic);
begin
  if Value <> FRoot then
  begin
//    FRoot.FSimpleXML := nil;
    FRoot := Value;
//    FRoot.FSimpleXML := Self;
  end;
end;

function TJclSimpleXMLElemsProlog.GetEncoding: string;
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Result := Elem.Encoding
  else
    Result := 'UTF-8';
end;

function TJclSimpleXMLElemsProlog.GetStandAlone: Boolean;
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Result := Elem.StandAlone
  else
    Result := False;
end;

function TJclSimpleXMLElemsProlog.GetVersion: string;
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Result := Elem.Version
  else
    Result := '1.0';
end;

procedure TJclSimpleXMLElemsProlog.SetEncoding(const Value: string);
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Elem.Encoding := Value;
end;

procedure TJclSimpleXMLElemsProlog.SetStandAlone(const Value: Boolean);
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Elem.StandAlone := Value;
end;

procedure TJclSimpleXMLElemsProlog.SetVersion(const Value: string);
var
  Elem: TJclSimpleXMLElemHeader;
begin
  Elem := TJclSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Elem.Version := Value;
end;

function TJclSimpleXMLElemsProlog.FindHeader: TJclSimpleXMLElem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Item[I] is TJclSimpleXMLElemHeader then
    begin
      Result := Item[I];
      Exit;
    end;
  // (p3) if we get here, an xml header was not found
  Result := TJclSimpleXMLElemHeader.Create(nil);
  Result.Name := 'xml';
  FElems.AddObject('', Result);
end;

function TJclSimpleXMLElemsProlog.AddStyleSheet(AType, AHRef: string): TJclSimpleXMLElemSheet;
begin
  // make sure there is an xml header
  FindHeader;
  Result := TJclSimpleXMLElemSheet.Create(nil);
  Result.Name := 'xml-stylesheet';
  Result.Properties.Add('type',AType);
  Result.Properties.Add('href',AHRef);
  FElems.AddObject('xml-stylesheet', Result);
end;

function TJclSimpleXMLElemsProlog.AddComment(const AValue: string): TJclSimpleXMLElemComment;
begin
  // make sure there is an xml header
  FindHeader;
  Result := TJclSimpleXMLElemComment.Create(nil);
  Result.Value := AValue;
  FElems.AddObject('', Result);
end;

function TJclSimpleXMLElemsProlog.AddDocType(const AValue: string): TJclSimpleXMLElemDocType;
begin
  // make sure there is an xml header
  FindHeader;
  Result := TJclSimpleXMLElemDocType.Create(nil);
  Result.Value := AValue;
  FElems.AddObject('', Result);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFNDEF CLR}
  {$IFDEF COMPILER6_UP}
  FreeAndNil(GlobalXMLVariant);
  {$ENDIF COMPILER6_UP}
  {$ENDIF !CLR}
  FreeAndNil(GlobalSorts);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
