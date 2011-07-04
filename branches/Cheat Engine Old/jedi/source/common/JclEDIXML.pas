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
{ The Original Code is JclEDIXML.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Raymond Alexander.                                 }
{ Portions created by Raymond Alexander are Copyright (C) Raymond Alexander. All rights reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Raymond Alexander (rayspostbox3), Robert Marquardt, Robert Rossmair, Petr Vones                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ A complementary unit to JclEDI.pas.                                                              }
{                                                                                                  }
{ Unit owner: Raymond Alexander                                                                    }
{ Date created: March 6, 2003                                                                      }
{ Additional Info:                                                                                 }
{   E-Mail at RaysDelphiBox3 att hotmail dott com                                                  }
{   For latest EDI specific demos see http://sourceforge.net/projects/edisdk                       }
{   See home page for latest news & events and online help.                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ 04/21/2003 (R.A.)                                                                                }
{                                                                                                  }
{   The current status of this unit is experimental.                                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-12-01 12:59:43 +0100 (sam., 01 déc. 2007)                         $ }
{ Revision:      $Rev:: 2255                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclEDIXML;

{$I jcl.inc}

{$IFDEF EDI_WEAK_PACKAGE_UNITS}
  {$IFDEF SUPPORTS_WEAKPACKAGEUNIT}
    {$WEAKPACKAGEUNIT ON}
  {$ENDIF SUPPORTS_WEAKPACKAGEUNIT}
{$ENDIF EDI_WEAK_PACKAGE_UNITS}

interface

uses
  SysUtils, Classes,
  {$IFNDEF EDI_WEAK_PACKAGE_UNITS}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$ENDIF ~EDI_WEAK_PACKAGE_UNITS}
  JclBase, JclEDI, JclEDI_ANSIX12;

const
  XMLTag_Element = 'Element';
  XMLTag_Segment = 'Segment';
  XMLTag_TransactionSetLoop = 'Loop';
  XMLTag_TransactionSet = 'TransactionSet';
  XMLTag_FunctionalGroup = 'FunctionalGroup';
  XMLTag_InterchangeControl = 'InterchangeControl';
  XMLTag_EDIFile = 'EDIFile';
  XMLTag_ICHSegmentId = ICHSegmentId; // Interchange Control Header Segment Id
  XMLTag_ICTSegmentId = ICTSegmentId; // Interchange Control Trailer Segment Id
  XMLTag_FGHSegmentId = FGHSegmentId; // Functional Group Header Segment Id
  XMLTag_FGTSegmentId = FGTSegmentId; // Functional Group Trailer Segment Id
  XMLTag_TSHSegmentId = TSHSegmentId; // Transaction Set Header Segment Id
  XMLTag_TSTSegmentId = TSTSegmentId; // Transaction Set Trailer Segment Id
  XMLAttribute_Id = 'Id';
  XMLAttribute_Position = 'Position';
  XMLAttribute_Description = 'Description';
  XMLAttribute_RequirementDesignator = 'RequirementDesignator';
  XMLAttribute_Type = 'Type';
  XMLAttribute_MinimumLength = 'MinimumLength';
  XMLAttribute_MaximumLength = 'MaximumLength';
  XMLAttribute_Section = 'Section';
  XMLAttribute_MaximumUsage = 'MaximumUsage';
  XMLAttribute_OwnerLoopId = 'OwnerLoopId';
  XMLAttribute_ParentLoopId = 'ParentLoopId';

type
  //  EDI Forward Class Declarations
  TEDIXMLObject = class(TEDIObject);
  TEDIXMLDataObject = class;
  TEDIXMLElement = class;
  TEDIXMLSegment = class;
  TEDIXMLTransactionSet = class;
  TEDIXMLFunctionalGroup = class;
  TEDIXMLInterchangeControl = class;
  TEDIXMLFile = class;

  //  EDI Delimiters Object
  TEDIXMLDelimiters = class(TEDIXMLObject)
  private
    FBeginTagDelimiter: string;
    FEndTagDelimiter: string;
    FBeginTagLength: Integer;
    FEndTagLength: Integer;
    FBeginCDataDelimiter: string;
    FEndCDataDelimiter: string;
    FBeginCDataLength: Integer;
    FEndCDataLength: Integer;
    FBeginOfEndTagDelimiter: string;
    FBeginOfEndTagLength: Integer;
    //Special Delimiters for Attributes
    FSpaceDelimiter: string;
    FAssignmentDelimiter: string;
    FSingleQuote: string;
    FDoubleQuote: string;
    procedure SetBeginTagDelimiter(const Value: string);
    procedure SetEndTagDelimiter(const Value: string);
    procedure SetBeginCDataDelimiter(const Value: string);
    procedure SetEndCDataDelimiter(const Value: string);
    procedure SetBeginOfEndTagDelimiter(const Value: string);
  public
    constructor Create;
  published
    property BTD: string read FBeginTagDelimiter write SetBeginTagDelimiter;
    property ETD: string read FEndTagDelimiter write SetEndTagDelimiter;
    property BTDLength: Integer read FBeginTagLength;
    property ETDLength: Integer read FEndTagLength;
    property BOfETD: string read FBeginOfEndTagDelimiter write SetBeginOfEndTagDelimiter;
    property BOfETDLength: Integer read FBeginOfEndTagLength;
    property BCDataD: string read FBeginCDataDelimiter write SetBeginCDataDelimiter;
    property ECDataD: string read FEndCDataDelimiter write SetEndCDataDelimiter;
    property BCDataLength: Integer read FBeginCDataLength;
    property ECDataLength: Integer read FEndCDataLength;
    //Special Delimiters for Attributes
    property SpaceDelimiter: string read FSpaceDelimiter write FSpaceDelimiter;
    property AssignmentDelimiter: string read FAssignmentDelimiter write FAssignmentDelimiter;
    property SingleQuote: string read FSingleQuote write FSingleQuote;
    property DoubleQuote: string read FDoubleQuote write FDoubleQuote;
  end;

  //  EDI XML Attributes
  TEDIXMLAttributes = class(TEDIXMLObject)
  private
    FAttributes: TStringList;
    FDelimiters: TEDIXMLDelimiters;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseAttributes(XMLStartTag: string);
    function CombineAttributes: string;
    procedure SetAttribute(Name, Value: string);
    function CheckAttribute(Name, Value: string): Integer;
    function GetAttributeValue(Name: string): string;
    function GetAttributeString(Name: string): string;
  end;

  //  EDI Data Object
  TEDIXMLObjectArray = array of TEDIXMLObject;

  TEDIXMLDataObject = class(TEDIXMLObject)
  private
    procedure SetDelimiters(const Delimiters: TEDIXMLDelimiters);
  protected
    FEDIDOT: TEDIDataObjectType;
    FState: TEDIDataObjectDataState;
    FData: string;
    FLength: Integer;
    FParent: TEDIXMLDataObject;
    FDelimiters: TEDIXMLDelimiters;
    FAttributes: TEDIXMLAttributes;
    FErrorLog: TStrings;
    FSpecPointer: TEDIObject;
    FCustomData1: TCustomData;
    FCustomData2: TCustomData;
    function GetData: string;
    procedure SetData(const Data: string);
    function Assemble: string; virtual; abstract;
    procedure Disassemble; virtual; abstract;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce;
    destructor Destroy; override;
    property SpecPointer: TEDIObject read FSpecPointer write FSpecPointer;
    property CustomData1: TCustomData read FCustomData1 write FCustomData1;
    property CustomData2: TCustomData read FCustomData2 write FCustomData2;
  published
    property State: TEDIDataObjectDataState read FState;
    property Data: string read GetData write SetData;
    property DataLength: Integer read FLength;
    property Parent: TEDIXMLDataObject read FParent write FParent;
    property Delimiters: TEDIXMLDelimiters read FDelimiters write SetDelimiters;
    property Attributes: TEDIXMLAttributes read FAttributes write FAttributes;
  end;

  TEDIXMLDataObjectArray = array of TEDIXMLDataObject;

  //  EDI Element
  TEDIXMLElement = class(TEDIXMLDataObject)
  private
    FCData: Boolean;
  protected
    function InternalAssignDelimiters: TEDIXMLDelimiters; virtual;
    function Assemble: string; override;
    procedure Disassemble; override;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce;
    function GetIndexPositionFromParent: Integer;
  published
    property CData: Boolean read FCData write FCData;
  end;

  TEDIXMLElementArray = array of TEDIXMLElement;

  //  EDI Data Object Group
  TEDIXMLDataObjectGroup = class(TEDIXMLDataObject)
  protected
    FEDIDataObjects: TEDIXMLDataObjectArray;
    function GetEDIDataObject(Index: Integer): TEDIXMLDataObject;
    procedure SetEDIDataObject(Index: Integer; EDIDataObject: TEDIXMLDataObject);
    function InternalAssignDelimiters: TEDIXMLDelimiters; virtual; abstract;
    function InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup; virtual; abstract;
    function SearchForSegmentInDataString(Id: string; StartPos: Integer): Integer;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce;
    destructor Destroy; override;
    //
    //  ToDo:  More procedures and functions to manage internal structures
    //
    function AppendEDIDataObject(EDIDataObject: TEDIXMLDataObject): Integer;
    function InsertEDIDataObject(InsertIndex: Integer; EDIDataObject: TEDIXMLDataObject): Integer;
    procedure DeleteEDIDataObject(Index: Integer); overload;
    procedure DeleteEDIDataObject(EDIDataObject: TEDIXMLDataObject); overload;
    //
    function AddSegment: Integer;
    function InsertSegment(InsertIndex: Integer): Integer;
    //
    function AddGroup: Integer; virtual;
    function InsertGroup(InsertIndex: Integer): Integer; virtual;
    //
    procedure DeleteEDIDataObjects;
    property EDIDataObject[Index: Integer]: TEDIXMLDataObject read GetEDIDataObject
    write SetEDIDataObject; default;
    property EDIDataObjects: TEDIXMLDataObjectArray read FEDIDataObjects write FEDIDataObjects;
  end;

  //  EDI Segment Classes
  TEDIXMLSegment = class(TEDIXMLDataObject)
  private
    FSegmentID: string;
    FElements: TEDIXMLElementArray;
    function GetElement(Index: Integer): TEDIXMLElement;
    procedure SetElement(Index: Integer; Element: TEDIXMLElement);
  protected
    function InternalAssignDelimiters: TEDIXMLDelimiters; virtual;
    function InternalCreateElement: TEDIXMLElement; virtual;
    //
    function Assemble: string; override;
    procedure Disassemble; override;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIXMLDataObject; ElementCount: Integer); reintroduce; overload;
    destructor Destroy; override;
    //
    function AddElement: Integer;
    function AppendElement(Element: TEDIXMLElement): Integer;
    function InsertElement(InsertIndex: Integer): Integer; overload;
    function InsertElement(InsertIndex: Integer; Element: TEDIXMLElement): Integer; overload;
    procedure DeleteElement(Index: Integer); overload;
    procedure DeleteElement(Element: TEDIXMLElement); overload;
    //
    function AddElements(Count: Integer): Integer;
    function AppendElements(ElementArray: TEDIXMLElementArray): Integer;
    function InsertElements(InsertIndex, Count: Integer): Integer; overload;
    function InsertElements(InsertIndex: Integer;
      ElementArray: TEDIXMLElementArray): Integer; overload;
    procedure DeleteElements; overload;
    procedure DeleteElements(Index, Count: Integer); overload;
    //
    function GetIndexPositionFromParent: Integer;
    property Element[Index: Integer]: TEDIXMLElement read GetElement write SetElement; default;
    property Elements: TEDIXMLElementArray read FElements write FElements;
  published
    property SegmentID: string read FSegmentID write FSegmentID;
  end;

  TEDIXMLSegmentArray = array of TEDIXMLSegment;  

  TEDIXMLTransactionSetSegment = class(TEDIXMLSegment)
  protected
    function InternalAssignDelimiters: TEDIXMLDelimiters; override;  
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIXMLDataObject; ElementCount: Integer); reintroduce; overload;
  end;

  TEDIXMLFunctionalGroupSegment = class(TEDIXMLSegment)
  protected
    function InternalAssignDelimiters: TEDIXMLDelimiters; override;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIXMLDataObject; ElementCount: Integer); reintroduce; overload;
  end;

  TEDIXMLInterchangeControlSegment = class(TEDIXMLSegment)
  protected
    function InternalAssignDelimiters: TEDIXMLDelimiters; override;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce; overload;
    constructor Create(Parent: TEDIXMLDataObject; ElementCount: Integer); reintroduce; overload;
  end;

  //  EDI Transaction Set Loop
  TEDIXMLTransactionSetLoop = class(TEDIXMLDataObjectGroup)
  private
    FParentTransactionSet: TEDIXMLTransactionSet;
  protected
    function InternalAssignDelimiters: TEDIXMLDelimiters; override;
    function InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup; override;
    function Assemble: string; override;
    procedure Disassemble; override;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce;
    destructor Destroy; override;
  published
    property ParentTransactionSet: TEDIXMLTransactionSet read FParentTransactionSet
      write FParentTransactionSet;
  end;

  //  EDI Transaction Set
  TEDIXMLTransactionSet = class(TEDIXMLTransactionSetLoop)
  private
    FSTSegment: TEDIXMLSegment;
    FSESegment: TEDIXMLSegment;
  protected
    function InternalAssignDelimiters: TEDIXMLDelimiters; override;
    function InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup; override;
    function Assemble: string; override;
    procedure Disassemble; override;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce;
    destructor Destroy; override;
  published
    property SegmentST: TEDIXMLSegment read FSTSegment write FSTSegment;
    property SegmentSE: TEDIXMLSegment read FSESegment write FSESegment;
  end;

  //  EDI Functional Group
  TEDIXMLFunctionalGroup = class(TEDIXMLDataObjectGroup)
  private
    FGSSegment: TEDIXMLSegment;
    FGESegment: TEDIXMLSegment;
  protected
    function InternalAssignDelimiters: TEDIXMLDelimiters; override;
    function InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup; override;
    function Assemble: string; override;
    procedure Disassemble; override;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce;
    destructor Destroy; override;
  published
    property SegmentGS: TEDIXMLSegment read FGSSegment write FGSSegment;
    property SegmentGE: TEDIXMLSegment read FGESegment write FGESegment;
  end;

  //  EDI Interchange Control
  TEDIXMLInterchangeControl = class(TEDIXMLDataObjectGroup)
  private
    FISASegment: TEDIXMLSegment;
    FIEASegment: TEDIXMLSegment;
  protected
    function InternalAssignDelimiters: TEDIXMLDelimiters; override;
    function InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup; override;
    function Assemble: string; override;
    procedure Disassemble; override;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce;
    destructor Destroy; override;
  published
    property SegmentISA: TEDIXMLSegment read FISASegment write FISASegment;
    property SegmentIEA: TEDIXMLSegment read FIEASegment write FIEASegment;
  end;

  //  EDI XML File Header
  TEDIXMLNameSpaceOption = (nsNone, nsDefault, nsQualified);

  TEDIXMLFileHeader = class(TEDIXMLObject)
  private
    FDelimiters: TEDIXMLDelimiters;
    FAttributes: TEDIXMLAttributes;
    FXMLNameSpaceOption: TEDIXMLNameSpaceOption;
  protected
    function OutputAdditionalXMLHeaderAttributes: string; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseXMLHeader(XMLHeader: string);
    function OutputXMLHeader: string;
  published
    property Delimiters: TEDIXMLDelimiters read FDelimiters;
    property Attributes: TEDIXMLAttributes read FAttributes;
    property XMLNameSpaceOption: TEDIXMLNameSpaceOption read FXMLNameSpaceOption
      write FXMLNameSpaceOption;
  end;

  //  EDI XML File
  TEDIXMLFile = class(TEDIXMLDataObjectGroup)
  private
    FFileID: Integer;
    FFileName: string;
    FEDIXMLFileHeader: TEDIXMLFileHeader;
    procedure InternalLoadFromFile;
  protected
    function InternalAssignDelimiters: TEDIXMLDelimiters; override;
    function InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup; override;
    function Assemble: string; override;
    procedure Disassemble; override;
  public
    constructor Create(Parent: TEDIXMLDataObject); reintroduce;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: string);
    procedure ReLoadFromFile;
    procedure SaveToFile;
    procedure SaveAsToFile(const FileName: string);
  published
    property FileID: Integer read FFileID write FFileID;
    property FileName: string read FFileName write FFileName;
    property XMLFileHeader: TEDIXMLFileHeader read FEDIXMLFileHeader;
  end;

  //  EDI XML Format Translator
  TEDIXMLANSIX12FormatTranslator = class(TEDIObject)
  private
    procedure ConvertTransactionSetLoopToXML(EDILoop: TEDITransactionSetLoop;
      XMLLoop: TEDIXMLTransactionSetLoop);
    procedure ConvertTransactionSetLoopToEDI(EDITransactionSet: TEDITransactionSet;
      XMLLoop: TEDIXMLTransactionSetLoop);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    //
    function ConvertToXMLSegment(EDISegment: TEDISegment): TEDIXMLSegment;
    function ConvertToXMLTransaction(
      EDITransactionSet: TEDITransactionSet): TEDIXMLTransactionSet; overload;
    function ConvertToXMLTransaction(EDITransactionSet: TEDITransactionSet;
      EDITransactionSetSpec: TEDITransactionSetSpec): TEDIXMLTransactionSet; overload;
    function ConvertToEDISegment(XMLSegment: TEDIXMLSegment): TEDISegment;
    function ConvertToEDITransaction(
      XMLTransactionSet: TEDIXMLTransactionSet): TEDITransactionSet;
  end;

{$IFNDEF EDI_WEAK_PACKAGE_UNITS}
{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclEDIXML.pas $';
    Revision: '$Revision: 2255 $';
    Date: '$Date: 2007-12-01 12:59:43 +0100 (sam., 01 déc. 2007) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}
{$ENDIF ~EDI_WEAK_PACKAGE_UNITS}

implementation

uses
  JclResources, JclStrings;

const
  EDIXML_Ampersand = '&';
  EDIXML_LessThanSign = '<';
  EDIXML_GreaterThanSign = '>';
  EDIXML_QuotationMark = '"';
  EDIXML_Apostrophe = '''';

  EDIXML_HTMLAmpersand = '&amp;';
  EDIXML_HTMLLessThanSign = '&lt;';
  EDIXML_HTMLGreaterThanSign = '&gt;';
  EDIXML_HTMLQuotationMark = '&quot;';
  EDIXML_HTMLApostrophe = '&apos;';

  EDIXMLDelimiter_ForwardSlash = '/';
  EDIXMLDelimiter_EqualToSign = '=';
  EDIXMLDelimiter_CDATABegin = '<![CDATA[';
  EDIXMLDelimiter_CDATAEnd = ']]>';
  EDIXMLDelimiter_FileHeaderBegin = '<?';
  EDIXMLDelimiter_FileHeaderEnd = '?>';

  EDIXMLAttributeStr_version = 'version';
  EDIXMLAttributeStr_encoding = 'encoding';
  EDIXMLAttributeStr_xmlns = 'xmlns';
  EDIXMLAttributeStr_xmlnsEDI = 'xmlns:EDI';

  Value_xml = 'xml';
  Value_Version10 = '1.0';
  Value_Windows1252 = 'windows-1252';
  Value_EDITRANSDOC = 'EDITRANSDOC';  

//=== { TEDIXMLDelimiters } ==================================================

constructor TEDIXMLDelimiters.Create;
begin
  inherited Create;
  SetBeginTagDelimiter(EDIXML_LessThanSign);
  SetBeginOfEndTagDelimiter(FBeginTagDelimiter + EDIXMLDelimiter_ForwardSlash);
  SetEndTagDelimiter(EDIXML_GreaterThanSign);
  FSpaceDelimiter := AnsiSpace;
  FAssignmentDelimiter := EDIXMLDelimiter_EqualToSign;
  FSingleQuote := EDIXML_Apostrophe;
  FDoubleQuote := EDIXML_QuotationMark;
  SetBeginCDataDelimiter(EDIXMLDelimiter_CDATABegin);
  SetEndCDataDelimiter(EDIXMLDelimiter_CDATAEnd);
end;

procedure TEDIXMLDelimiters.SetBeginCDataDelimiter(const Value: string);
begin
  FBeginCDataDelimiter := Value;
  FBeginCDataLength := Length(FBeginCDataDelimiter);
end;

procedure TEDIXMLDelimiters.SetBeginOfEndTagDelimiter(const Value: string);
begin
  FBeginOfEndTagDelimiter := Value;
  FBeginOfEndTagLength := Length(FBeginOfEndTagDelimiter);
end;

procedure TEDIXMLDelimiters.SetBeginTagDelimiter(const Value: string);
begin
  FBeginTagDelimiter := Value;
  FBeginTagLength := Length(FBeginTagDelimiter);
end;

procedure TEDIXMLDelimiters.SetEndCDataDelimiter(const Value: string);
begin
  FEndCDataDelimiter := Value;
  FEndCDataLength := Length(FEndCDataDelimiter);
end;

procedure TEDIXMLDelimiters.SetEndTagDelimiter(const Value: string);
begin
  FEndTagDelimiter := Value;
  FEndTagLength := Length(FEndTagDelimiter);
end;

//=== { TEDIXMLAttributes } ==================================================

constructor TEDIXMLAttributes.Create;
begin
  inherited Create;
  FAttributes := TStringList.Create;
  FDelimiters := TEDIXMLDelimiters.Create;
end;

destructor TEDIXMLAttributes.Destroy;
begin
  FDelimiters.Free;
  FAttributes.Free;
  inherited Destroy;
end;

function TEDIXMLAttributes.CheckAttribute(Name, Value: string): Integer;
begin
  Result := -1;
  if FAttributes.Values[Name] = Value then
    Result := FAttributes.IndexOfName(Name);
end;

function TEDIXMLAttributes.CombineAttributes: string;
var
  I, J, K: Integer;
  QuoteDelimiter: string;
begin
  Result := '';
  for I := 0 to FAttributes.Count - 1 do
  begin
    {$IFDEF COMPILER7_UP}
    J := StrSearch(FDelimiters.SingleQuote, FAttributes.ValueFromIndex[I]);
    K := StrSearch(FDelimiters.DoubleQuote, FAttributes.ValueFromIndex[I]);
    {$ELSE}
    J := StrSearch(FDelimiters.SingleQuote, FAttributes.Values[FAttributes.Names[I]]);
    K := StrSearch(FDelimiters.DoubleQuote, FAttributes.Values[FAttributes.Names[I]]);
    {$ENDIF COMPILER7_UP}
    if J > K then
      QuoteDelimiter := FDelimiters.SingleQuote
    else
      QuoteDelimiter := FDelimiters.DoubleQuote;
    if Result <> '' then
      Result := Result + FDelimiters.SpaceDelimiter;
    {$IFDEF COMPILER7_UP}
    Result := Result + FAttributes.Names[I] + FDelimiters.AssignmentDelimiter +
      QuoteDelimiter + FAttributes.ValueFromIndex[I] + QuoteDelimiter;
    {$ELSE}
    Result := Result + FAttributes.Names[I] + FDelimiters.AssignmentDelimiter +
      QuoteDelimiter + FAttributes.Values[FAttributes.Names[I]] + QuoteDelimiter;
    {$ENDIF COMPILER7_UP}
  end;
end;

function TEDIXMLAttributes.GetAttributeString(Name: string): string;
var
  J, K: Integer;
  QuoteDelimiter: string;
begin
  Result := '';
  J := StrSearch(FDelimiters.SingleQuote, FAttributes.Values[Name]);
  K := StrSearch(FDelimiters.DoubleQuote, FAttributes.Values[Name]);
  if J > K then
    QuoteDelimiter := FDelimiters.SingleQuote
  else
    QuoteDelimiter := FDelimiters.DoubleQuote;
  Result := Name + FDelimiters.AssignmentDelimiter +
    QuoteDelimiter + FAttributes.Values[Name] + QuoteDelimiter;
end;

function TEDIXMLAttributes.GetAttributeValue(Name: string): string;
begin
  Result := FAttributes.Values[Name];
end;

procedure TEDIXMLAttributes.ParseAttributes(XMLStartTag: string);
var
  SearchResult: Integer;
  EndDataChar: string;
  Attribute, Value: string;
  AttributeStart, AttributeLen: Integer;
  ValueStart, ValueLen: Integer;
begin
  FAttributes.Clear;
  // Search for begin of attribute
  SearchResult := StrSearch(FDelimiters.SpaceDelimiter, XMLStartTag, 1);
  AttributeStart := SearchResult + Length(FDelimiters.SpaceDelimiter);
  while SearchResult > 0 do
  begin
    // Get the end data delimiter
    SearchResult := StrSearch(FDelimiters.AssignmentDelimiter, XMLStartTag, AttributeStart);
    if SearchResult > 0 then
    begin
      AttributeLen := SearchResult - AttributeStart;
      ValueStart := SearchResult + Length(FDelimiters.AssignmentDelimiter);
      EndDataChar := Copy(XMLStartTag, ValueStart, 1);
      // Search for end of data
      ValueStart := ValueStart + Length(FDelimiters.AssignmentDelimiter);
      SearchResult := StrSearch(EndDataChar, XMLStartTag, ValueStart);
      if SearchResult > 0 then
      begin
        ValueLen := SearchResult - ValueStart;
        Attribute := Copy(XMLStartTag, AttributeStart, AttributeLen);
        Value := Copy(XMLStartTag, ValueStart, ValueLen);
        FAttributes.Values[Attribute] := Value;
      end;
      // Search for begin of attribute
      SearchResult := StrSearch(FDelimiters.SpaceDelimiter, XMLStartTag, SearchResult);
      AttributeStart := SearchResult + Length(FDelimiters.SpaceDelimiter);
    end;
  end;
end;

procedure TEDIXMLAttributes.SetAttribute(Name, Value: string);
begin
  FAttributes.Values[Name] := Value;
end;

//=== { TEDIXMLDataObject } ==================================================

constructor TEDIXMLDataObject.Create(Parent: TEDIXMLDataObject);
begin
  inherited Create;
  FState := ediCreated;
  FEDIDOT := ediUnknown;
  FData := '';
  FLength := 0;
  FParent := Parent;
  FDelimiters := nil;
  FAttributes := TEDIXMLAttributes.Create;
end;

destructor TEDIXMLDataObject.Destroy;
begin
  FAttributes.Free;
  if not Assigned(FParent) then
    FDelimiters.Free;
  FDelimiters := nil;
  inherited Destroy;
end;

function TEDIXMLDataObject.GetData: string;
begin
  Result := FData;
end;

procedure TEDIXMLDataObject.SetData(const Data: string);
begin
  FData := Data;
  FLength := Length(FData);
end;

procedure TEDIXMLDataObject.SetDelimiters(const Delimiters: TEDIXMLDelimiters);
begin
  if not Assigned(FParent) then
    FreeAndNil(FDelimiters);
  FDelimiters := Delimiters;
end;

//=== { TEDIXMLElement } =====================================================

constructor TEDIXMLElement.Create(Parent: TEDIXMLDataObject);
begin
  if Assigned(Parent) and (Parent is TEDIXMLSegment) then
    inherited Create(Parent)
  else
    inherited Create(nil);
  FEDIDOT := ediElement;
  FCData := False;
end;

function TEDIXMLElement.Assemble: string;
var
  AttributeString: string;
  OriginalData: string;
begin
  // Check delimiter assignment
  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(47);
  end;

  OriginalData := FData;
  // Handle Entity Reference Characters
  StrReplace(OriginalData, EDIXML_Ampersand, EDIXML_HTMLAmpersand, [rfReplaceAll]);
  StrReplace(OriginalData, EDIXML_LessThanSign, EDIXML_HTMLLessThanSign, [rfReplaceAll]);
  StrReplace(OriginalData, EDIXML_GreaterThanSign, EDIXML_HTMLGreaterThanSign, [rfReplaceAll]);
  StrReplace(OriginalData, EDIXML_QuotationMark, EDIXML_HTMLQuotationMark, [rfReplaceAll]);
  StrReplace(OriginalData, EDIXML_Apostrophe, EDIXML_HTMLApostrophe, [rfReplaceAll]);
  //
  AttributeString := FAttributes.CombineAttributes;
  if AttributeString <> '' then
    FData := FDelimiters.BTD + XMLTag_Element + FDelimiters.SpaceDelimiter +
      AttributeString + FDelimiters.ETD
  else
    FData := FDelimiters.BTD + XMLTag_Element + FDelimiters.ETD;

  if FCData then
    FData := FData + FDelimiters.BCDataD + OriginalData + FDelimiters.ECDataD
  else
    FData := FData + OriginalData;

  FData := FData + FDelimiters.BOfETD + XMLTag_Element + FDelimiters.ETD;

  Result := FData;
  FState := ediAssembled;
end;

procedure TEDIXMLElement.Disassemble;
var
  StartPos, EndPos, SearchResult: Integer;
  XMLStartTag: string;
begin
  // Check delimiter assignment
  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(46);
  end;
  // Set next start positon
  StartPos := 1;
  // Move past begin element tag
  SearchResult := StrSearch(FDelimiters.BTD + XMLTag_Element, FData, StartPos);
  if SearchResult > 0 then
  begin
    SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
    XMLStartTag := Copy(FData, StartPos, (SearchResult + FDelimiters.ETDLength) - StartPos);
    FAttributes.ParseAttributes(XMLStartTag);
  end
  else
    raise EJclEDIError.CreateID(48);
  // Set data start positon
  StartPos := SearchResult + FDelimiters.ETDLength;
  // Check for CData tag
  FCData := False;
  SearchResult := StrSearch(FDelimiters.BCDataD, FData, StartPos);
  if SearchResult > 0 then
  begin
    StartPos := SearchResult + FDelimiters.BCDataLength;
    FCData := True;
  end;
  //
  SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_Element, FData, StartPos);
  if SearchResult > 0 then
  begin
    EndPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
    if SearchResult > 0 then
    begin
      if FCData then
        EndPos := EndPos - FDelimiters.ECDataLength;
      FData := Copy(FData, StartPos, (EndPos - StartPos));
    end
    else
      raise EJclEDIError.CreateID(50);
  end
  else
    raise EJclEDIError.CreateID(49);
  // Handle Entity Reference Characters
  StrReplace(FData, EDIXML_HTMLLessThanSign, EDIXML_LessThanSign, [rfReplaceAll]);
  StrReplace(FData, EDIXML_HTMLGreaterThanSign, EDIXML_GreaterThanSign, [rfReplaceAll]);
  StrReplace(FData, EDIXML_HTMLQuotationMark, EDIXML_QuotationMark, [rfReplaceAll]);
  StrReplace(FData, EDIXML_HTMLApostrophe, EDIXML_Apostrophe, [rfReplaceAll]);
  StrReplace(FData, EDIXML_HTMLAmpersand, EDIXML_Ampersand, [rfReplaceAll]);
  //
  FState := ediDisassembled;
end;

function TEDIXMLElement.GetIndexPositionFromParent: Integer;
var
  I: Integer;
begin
  Result := -1;
  if Assigned(Parent) and (Parent is TEDIXMLSegment) then
    for I := Low(TEDIXMLSegment(Parent).Elements) to High(TEDIXMLSegment(Parent).Elements) do
      if TEDIXMLSegment(Parent).Element[I] = Self then
        Result := I;
end;

function TEDIXMLElement.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := nil;
  // Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
    // Get the delimiters from the parent segment
    if Assigned(Parent) and (Parent is TEDIXMLSegment) then
      Result := Parent.Delimiters;
end;

//=== { TEDIXMLSegment } =====================================================

constructor TEDIXMLSegment.Create(Parent: TEDIXMLDataObject; ElementCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDIXMLTransactionSet) then
    inherited Create(Parent)
  else
    inherited Create(nil);
  FEDIDOT := ediSegment;
  SetLength(FElements, 0);
  AddElements(ElementCount);
end;

constructor TEDIXMLSegment.Create(Parent: TEDIXMLDataObject);
begin
  if Assigned(Parent) and (Parent is TEDIXMLDataObjectGroup) then
    inherited Create(Parent)
  else
    inherited Create(nil);
  FEDIDOT := ediSegment;
  SetLength(FElements, 0);
end;

destructor TEDIXMLSegment.Destroy;
begin
  DeleteElements;
  inherited Destroy;
end;

function TEDIXMLSegment.AddElement: Integer;
begin
  SetLength(FElements, Length(FElements) + 1);
  FElements[High(FElements)] := InternalCreateElement;
  Result := High(FElements); // Return position of element
end;

function TEDIXMLSegment.AddElements(Count: Integer): Integer;
var
  I, J: Integer;
begin
  I := Length(FElements);
  Result := I; // Return position of 1st element
  // Resize
  SetLength(FElements, Length(FElements) + Count);
  // Add
  for J := I to High(FElements) do
    FElements[J] := InternalCreateElement;
end;

function TEDIXMLSegment.AppendElement(Element: TEDIXMLElement): Integer;
begin
  SetLength(FElements, Length(FElements) + 1);
  FElements[High(FElements)] := Element;
  Element.Parent := Self;
  Result := High(FElements); // Return position of element
end;

function TEDIXMLSegment.AppendElements(ElementArray: TEDIXMLElementArray): Integer;
var
  I, J, K: Integer;
begin
  I := 0;
  J := Length(FElements);
  Result := J; // Return position of 1st element
  // Resize
  SetLength(FElements, Length(FElements) + Length(ElementArray));
  //Append
  for K := J to High(ElementArray) do
  begin
    FElements[K] := ElementArray[I];
    FElements[K].Parent := Self;
    Inc(I);
  end;
end;

function TEDIXMLSegment.Assemble: string;
var
  I: Integer;
  AttributeString: string;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(42);
  end;

  AttributeString := FAttributes.CombineAttributes;
  if AttributeString <> '' then
    FData := FDelimiters.BTD + XMLTag_Segment + FDelimiters.SpaceDelimiter +
      AttributeString + FDelimiters.ETD
  else
    FData := FDelimiters.BTD + XMLTag_Segment + FDelimiters.ETD;

  if Length(FElements) > 0 then
    for I := Low(FElements) to High(FElements) do
      if Assigned(FElements[I]) then
        FData := FData + FElements[I].Assemble
      else
        FData := FData + FDelimiters.BTD + XMLTag_Element + FDelimiters.ETD +
          FDelimiters.BOfETD + XMLTag_Element + FDelimiters.ETD;
  FData := FData + FDelimiters.BOfETD + XMLTag_Segment + FDelimiters.ETD;
  FLength := Length(FData);
  Result := FData; // Return assembled string

  DeleteElements;

  FState := ediAssembled;
end;

procedure TEDIXMLSegment.DeleteElement(Element: TEDIXMLElement);
var
  I: Integer;
begin
  for I := Low(FElements) to High(FElements) do
    if FElements[I] = Element then
      DeleteElement(I);
end;

procedure TEDIXMLSegment.DeleteElement(Index: Integer);
var
  I: Integer;
begin
  if (Length(FElements) > 0) and (Index >= Low(FElements)) and (Index <= High(FElements)) then
  begin
    // Delete
    FreeAndNil(FElements[Index]);
    // Shift
    for I := Index + 1 to High(FElements) do
      FElements[I - 1] := FElements[I];
    // Resize
    SetLength(FElements, High(FElements));
  end
  else
    raise EJclEDIError.CreateIDFmt(58, [IntToStr(Index)]);
end;

procedure TEDIXMLSegment.DeleteElements;
var
  I: Integer;
begin
  for I := Low(FElements) to High(FElements) do
    // Delete
    FreeAndNil(FElements[I]);
  // Resize
  SetLength(FElements, 0);
end;

procedure TEDIXMLSegment.DeleteElements(Index, Count: Integer);
var
  I: Integer;
begin
  if (Length(FElements) > 0) and (Index >= Low(FElements)) and (Index <= High(FElements)) then
  begin
    // Delete
    for I := Index to (Index + Count) - 1 do
      FreeAndNil(FElements[I]);
    // Shift
    for I := (Index + Count) to High(FElements) do
    begin
      FElements[I - Count] := FElements[I];
      FElements[I] := nil;
    end;
    // Resize
    SetLength(FElements, Length(FElements) - Count);
  end
  else
    raise EJclEDIError.CreateIDFmt(58, [IntToStr(Index)]);
end;

procedure TEDIXMLSegment.Disassemble;
var
  I, StartPos, SearchResult: Integer;
  XMLStartTag: string;
begin
  DeleteElements;
  // Check delimiter assignment
  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(41);
  end;
  // Set next start positon
  StartPos := 1;
  // Move past begin segment tag
  SearchResult := StrSearch(FDelimiters.BTD + XMLTag_Segment, FData, StartPos);
  if SearchResult > 0 then
  begin
    StartPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
    XMLStartTag := Copy(FData, StartPos, (SearchResult + FDelimiters.ETDLength) - StartPos);
    FAttributes.ParseAttributes(XMLStartTag);
  end
  else
    raise EJclEDIError.CreateID(43);
  // Set next start positon
  StartPos := SearchResult + FDelimiters.ETDLength;
  // Search for element
  SearchResult := StrSearch(FDelimiters.BTD + XMLTag_Element, FData, StartPos);
  // Search for Segments
  while SearchResult > 0 do
  begin
    SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_Element, FData, SearchResult);
    if SearchResult > 0 then
    begin
      SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        I := AddElement; // Add Element
        FElements[I].Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
        FElements[I].Disassemble;
      end
      else
        raise EJclEDIError.CreateID(50);
    end
    else
      raise EJclEDIError.CreateID(49);
    // Set next start positon
    StartPos := SearchResult + FDelimiters.ETDLength;
    // Search for element
    SearchResult := StrSearch(FDelimiters.BTD + XMLTag_Element, FData, StartPos);
  end;
  FData := '';
  //
  FState := ediDisassembled;
end;

function TEDIXMLSegment.GetElement(Index: Integer): TEDIXMLElement;
begin
  if Length(FElements) > 0 then
    if Index >= Low(FElements) then
      if Index <= High(FElements) then
      begin
        if not Assigned(FElements[Index]) then
          raise EJclEDIError.CreateIDFmt(57, [IntToStr(Index)]);
        Result := FElements[Index];
      end
      else
        raise EJclEDIError.CreateIDFmt(56, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateIDFmt(55, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateIDFmt(54, [IntToStr(Index)]);
end;

function TEDIXMLSegment.GetIndexPositionFromParent: Integer;
var
  I: Integer;
begin
  Result := -1;
  if Assigned(Parent) and (Parent is TEDIXMLTransactionSet) then
    for I := Low(TEDIXMLTransactionSet(Parent).EDIDataObjects) to
      High(TEDIXMLTransactionSet(Parent).EDIDataObjects) do
      if TEDIXMLTransactionSet(Parent).EDIDataObject[I] = Self then
      begin
        Result := I;
        Break;
      end;
end;

function TEDIXMLSegment.InsertElement(InsertIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FElements) > 0) and (InsertIndex >= Low(FElements)) and
    (InsertIndex <= High(FElements)) then
  begin
    // Resize
    SetLength(FElements, Length(FElements) + 1);
    // Shift
    for I := High(FElements) downto InsertIndex + 1 do
      FElements[I] := FElements[I - 1];
    // Insert
    FElements[InsertIndex] := InternalCreateElement;
  end
  else
    Result := AddElement;
end;

function TEDIXMLSegment.InsertElement(InsertIndex: Integer; Element: TEDIXMLElement): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FElements) > 0) and (InsertIndex >= Low(FElements)) and
    (InsertIndex <= High(FElements)) then
  begin
    // Resize
    SetLength(FElements, Length(FElements) + 1);
    // Shift
    for I := High(FElements) downto InsertIndex + 1 do
      FElements[I] := FElements[I - 1];
    // Insert
    FElements[InsertIndex] := Element;
    FElements[InsertIndex].Parent := Self;
  end
  else
    Result := AppendElement(Element);
end;

function TEDIXMLSegment.InsertElements(InsertIndex: Integer;
  ElementArray: TEDIXMLElementArray): Integer;
var
  I, J, K: Integer;
begin
  Result := InsertIndex;
  I := Length(ElementArray);
  if (Length(FElements) > 0) and (InsertIndex >= Low(FElements)) and
    (InsertIndex <= High(FElements)) then
  begin
    // Resize
    SetLength(FElements, Length(FElements) + I);
    // Shift
    for J := High(FElements) downto InsertIndex + I do
    begin
      FElements[J] := FElements[J - I];
      FElements[J - I] := nil;
    end;
    // Insert
    K := 0;
    for J := InsertIndex to (InsertIndex + I) - 1 do
    begin
      FElements[J] := ElementArray[K];
      FElements[J].Parent := Self;
      Inc(K);
    end;
  end
  else
    Result := AppendElements(ElementArray);
end;

function TEDIXMLSegment.InsertElements(InsertIndex, Count: Integer): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FElements) > 0) and (InsertIndex >= Low(FElements)) and
    (InsertIndex <= High(FElements)) then
  begin
    // Resize
    SetLength(FElements, Length(FElements) + Count);
    // Shift
    for I := High(FElements) downto InsertIndex + Count do
    begin
      FElements[I] := FElements[I - Count];
      FElements[I - Count] := nil;
    end;
    // Insert
    for I := InsertIndex to (InsertIndex + Count) - 1 do
      FElements[I] := InternalCreateElement;
  end
  else
    Result := AddElements(Count);
end;

function TEDIXMLSegment.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := nil;
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    // Get the delimiters from the transaction set loop
    if Assigned(Parent) and (Parent is TEDIXMLTransactionSetLoop) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := TEDIXMLTransactionSetLoop(Parent).ParentTransactionSet.Delimiters;
        Exit;
      end;
    end;
    // Get the delimiters from the transaction set
    if Assigned(Parent) and (Parent is TEDIXMLTransactionSet) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
        Exit;
      end;
      // Get the delimiters from the functional group
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIXMLFunctionalGroup) then
      begin
        if Assigned(Parent.Parent.Delimiters) then
        begin
          Result := Parent.Parent.Delimiters;
          Exit;
        end;
        // Get the delimiters from the interchange control header
        if Assigned(Parent.Parent.Parent) and
          (Parent.Parent.Parent is TEDIXMLInterchangeControl) then
          if Assigned(Parent.Parent.Parent.Delimiters) then
            Result := Parent.Parent.Parent.Delimiters;
      end;
    end;
  end;
end;

function TEDIXMLSegment.InternalCreateElement: TEDIXMLElement;
begin
  Result := TEDIXMLElement.Create(Self);
end;

procedure TEDIXMLSegment.SetElement(Index: Integer; Element: TEDIXMLElement);
begin
  if Length(FElements) > 0 then
    if Index >= Low(FElements) then
      if Index <= High(FElements) then
      begin
        FreeAndNil(FElements[Index]);
        FElements[Index] := Element;
      end
      else
        raise EJclEDIError.CreateIDFmt(53, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateIDFmt(52, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateIDFmt(51, [IntToStr(Index)]);
end;

//=== { TEDIXMLTransactionSetSegment } =======================================

constructor TEDIXMLTransactionSetSegment.Create(Parent: TEDIXMLDataObject);
begin
  inherited Create(Parent);
  if Assigned(Parent) and (Parent is TEDIXMLTransactionSet) then
    FParent := Parent;
end;

constructor TEDIXMLTransactionSetSegment.Create(Parent: TEDIXMLDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  if Assigned(Parent) and (Parent is TEDIXMLTransactionSet) then
    FParent := Parent;
end;

function TEDIXMLTransactionSetSegment.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := inherited InternalAssignDelimiters;
end;

//=== { TEDIXMLFunctionalGroupSegment } ======================================

constructor TEDIXMLFunctionalGroupSegment.Create(Parent: TEDIXMLDataObject);
begin
  inherited Create(Parent);
  if Assigned(Parent) and (Parent is TEDIXMLFunctionalGroup) then
    FParent := Parent;
end;

constructor TEDIXMLFunctionalGroupSegment.Create(Parent: TEDIXMLDataObject;
  ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  if Assigned(Parent) and (Parent is TEDIXMLFunctionalGroup) then
    FParent := Parent;
end;

function TEDIXMLFunctionalGroupSegment.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := nil;
  // Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
    // Get the delimiters from the functional group
    if Assigned(Parent) and (Parent is TEDIXMLFunctionalGroup) then
      if Assigned(Parent.Delimiters) then
        Result := Parent.Delimiters
      else
      // Get the delimiters from the interchange control
        if Assigned(Parent.Parent) and (Parent.Parent is TEDIXMLInterchangeControl) then
        Result := Parent.Parent.Delimiters;
end;

//=== { TEDIXMLInterchangeControlSegment } ===================================

constructor TEDIXMLInterchangeControlSegment.Create(Parent: TEDIXMLDataObject);
begin
  inherited Create(Parent);
  if Assigned(Parent) and (Parent is TEDIXMLInterchangeControl) then
    FParent := Parent;
end;

constructor TEDIXMLInterchangeControlSegment.Create(Parent: TEDIXMLDataObject;
  ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  if Assigned(Parent) and (Parent is TEDIXMLInterchangeControl) then
    FParent := Parent;
end;

function TEDIXMLInterchangeControlSegment.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := nil;
  // Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
    // Get the delimiters from the interchange control
    if Assigned(Parent) and (Parent is TEDIXMLInterchangeControl) then
      Result := Parent.Delimiters;
end;

//=== { TEDIXMLDataObjectGroup } =============================================

constructor TEDIXMLDataObjectGroup.Create(Parent: TEDIXMLDataObject);
begin
  inherited Create(Parent);
end;

destructor TEDIXMLDataObjectGroup.Destroy;
begin
  DeleteEDIDataObjects;
  inherited Destroy;
end;

function TEDIXMLDataObjectGroup.AddGroup: Integer;
var
  EDIGroup: TEDIXMLDataObjectGroup;
begin
  EDIGroup := InternalCreateDataObjectGroup;
  Result := AppendEDIDataObject(EDIGroup);
end;

function TEDIXMLDataObjectGroup.AddSegment: Integer;
var
  EDISegment: TEDIXMLSegment;
begin
  EDISegment := TEDIXMLSegment.Create(Self);
  Result := AppendEDIDataObject(EDISegment);
end;

function TEDIXMLDataObjectGroup.AppendEDIDataObject(EDIDataObject: TEDIXMLDataObject): Integer;
begin
  SetLength(FEDIDataObjects, Length(FEDIDataObjects) + 1);
  FEDIDataObjects[High(FEDIDataObjects)] := EDIDataObject;
  EDIDataObject.Parent := Self;
  Result := High(FEDIDataObjects);
end;

procedure TEDIXMLDataObjectGroup.DeleteEDIDataObject(EDIDataObject: TEDIXMLDataObject);
var
  I: Integer;
begin
  for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
    if FEDIDataObjects[I] = EDIDataObject then
      DeleteEDIDataObject(I);
end;

procedure TEDIXMLDataObjectGroup.DeleteEDIDataObject(Index: Integer);
var
  I: Integer;
begin
  if (Length(FEDIDataObjects) > 0) and (Index >= Low(FEDIDataObjects)) and
    (Index <= High(FEDIDataObjects)) then
  begin
    // Delete
    FreeAndNil(FEDIDataObjects[Index]);
    // Shift
    for I := Index + 1 to High(FEDIDataObjects) do
      FEDIDataObjects[I - 1] := FEDIDataObjects[I];
    // Resize
    SetLength(FEDIDataObjects, High(FEDIDataObjects));
  end
  else
    raise EJclEDIError.CreateID(40);
end;

procedure TEDIXMLDataObjectGroup.DeleteEDIDataObjects;
var
  I: Integer;
begin
  for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
    FreeAndNil(FEDIDataObjects[I]);
  // Resize
  SetLength(FEDIDataObjects, 0);
end;

function TEDIXMLDataObjectGroup.GetEDIDataObject(Index: Integer): TEDIXMLDataObject;
begin
  if Length(FEDIDataObjects) > 0 then
    if Index >= Low(FEDIDataObjects) then
      if Index <= High(FEDIDataObjects) then
      begin
        if not Assigned(FEDIDataObjects[Index]) then
          raise EJclEDIError.CreateIDFmt(39, [IntToStr(Index)]);
        Result := FEDIDataObjects[Index];
      end
      else
        raise EJclEDIError.CreateIDFmt(38, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateIDFmt(37, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateIDFmt(36, [IntToStr(Index)]);
end;

function TEDIXMLDataObjectGroup.InsertEDIDataObject(InsertIndex: Integer;
  EDIDataObject: TEDIXMLDataObject): Integer;
var
  I: Integer;
begin
  Result := InsertIndex;
  if (Length(FEDIDataObjects) > 0) and (InsertIndex >= Low(FEDIDataObjects)) and
    (InsertIndex <= High(FEDIDataObjects)) then
  begin
    // Resize
    SetLength(FEDIDataObjects, Length(FEDIDataObjects) + 1);
    // Shift
    for I := High(FEDIDataObjects) downto InsertIndex + 1 do
      FEDIDataObjects[I] := FEDIDataObjects[I - 1];
    // Insert
    FEDIDataObjects[InsertIndex] := EDIDataObject;
    FEDIDataObjects[InsertIndex].Parent := Self;
  end
  else
    Result := AppendEDIDataObject(EDIDataObject);
end;

function TEDIXMLDataObjectGroup.InsertGroup(InsertIndex: Integer): Integer;
var
  EDIGroup: TEDIXMLDataObjectGroup;
begin
  EDIGroup := InternalCreateDataObjectGroup;
  Result := InsertEDIDataObject(InsertIndex, EDIGroup);
end;

function TEDIXMLDataObjectGroup.InsertSegment(InsertIndex: Integer): Integer;
var
  EDISegment: TEDIXMLSegment;
begin
  EDISegment := TEDIXMLSegment.Create(Self);
  Result := InsertEDIDataObject(InsertIndex, EDISegment);
end;

function TEDIXMLDataObjectGroup.SearchForSegmentInDataString(Id: string;
  StartPos: Integer): Integer;
var
  SegmentTag: string;
  SearchResult, SegmentTagStartPos: Integer;
  EDIXMLAttributes: TEDIXMLAttributes;
begin
  Result := 0;
  EDIXMLAttributes := TEDIXMLAttributes.Create;
  SearchResult := StrSearch(FDelimiters.BTD + XMLTag_Segment, FData, StartPos);
  SegmentTagStartPos := SearchResult;
  while SearchResult > 0 do
  begin
    SearchResult := StrSearch(FDelimiters.ETD, FData, SegmentTagStartPos);
    if SearchResult > 0 then
    begin
      SegmentTag := Copy(FData, SegmentTagStartPos, ((SearchResult - SegmentTagStartPos) +
        FDelimiters.ETDLength));
      EDIXMLAttributes.ParseAttributes(SegmentTag);
      Result := EDIXMLAttributes.CheckAttribute(XMLAttribute_Id, Id);
      if Result >= 0 then
      begin
        Result := SegmentTagStartPos;
        Break;
      end;
    end;
    SegmentTagStartPos := SearchResult + FDelimiters.ETDLength;
    SearchResult := StrSearch(FDelimiters.BTD + XMLTag_Segment, FData, SegmentTagStartPos);
  end;
  EDIXMLAttributes.Free;
end;

procedure TEDIXMLDataObjectGroup.SetEDIDataObject(Index: Integer; EDIDataObject: TEDIXMLDataObject);
begin
  if Length(FEDIDataObjects) > 0 then
    if Index >= Low(FEDIDataObjects) then
      if Index <= High(FEDIDataObjects) then
      begin
        FreeAndNil(FEDIDataObjects[Index]);
        FEDIDataObjects[Index] := EDIDataObject;
      end
      else
        raise EJclEDIError.CreateIDFmt(35, [IntToStr(Index)])
    else
      raise EJclEDIError.CreateIDFmt(34, [IntToStr(Index)])
  else
    raise EJclEDIError.CreateIDFmt(33, [IntToStr(Index)]);
end;

//=== { TEDIXMLTransactionSetLoop } ==========================================

constructor TEDIXMLTransactionSetLoop.Create(Parent: TEDIXMLDataObject);
begin
  inherited Create(Parent);
  if Assigned(Parent) and (Parent is TEDIXMLTransactionSet) then
    FParentTransactionSet := TEDIXMLTransactionSet(Parent)
  else
  if Assigned(Parent) and (Parent is TEDIXMLTransactionSetLoop) then
    FParentTransactionSet := TEDIXMLTransactionSetLoop(Parent).ParentTransactionSet
  else
    FParentTransactionSet := nil;
  FEDIDOT := ediLoop;
end;

destructor TEDIXMLTransactionSetLoop.Destroy;
begin
  inherited Destroy;
end;

function TEDIXMLTransactionSetLoop.Assemble: string;
var
  I: Integer;
  AttributeString: string;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(30);
  end;

  AttributeString := FAttributes.CombineAttributes;
  if AttributeString <> '' then
    FData := FDelimiters.BTD + XMLTag_TransactionSetLoop + FDelimiters.SpaceDelimiter +
      AttributeString + FDelimiters.ETD
  else
    FData := FDelimiters.BTD + XMLTag_TransactionSetLoop + FDelimiters.ETD;

  if Length(FEDIDataObjects) > 0 then
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;
  FData := FData + FDelimiters.BOfETD + XMLTag_TransactionSetLoop + FDelimiters.ETD;
  FLength := Length(FData);
  Result := FData; // Return assembled string

  DeleteEDIDataObjects;

  FState := ediAssembled;
end;

procedure TEDIXMLTransactionSetLoop.Disassemble;
var
  I, J, StartPos, SearchResult: Integer;
  XMLStartTag, SearchTag: string;
  NestedLoopCount: Integer;
begin
  DeleteEDIDataObjects;
  // Check delimiter assignment
  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(29);
  end;
  // Set next start positon
  StartPos := 1;
  // Move past begin loop tag
  SearchResult := StrSearch(FDelimiters.BTD + XMLTag_TransactionSetLoop, FData, StartPos);
  if SearchResult > 0 then
  begin
    StartPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
    XMLStartTag := Copy(FData, StartPos, (SearchResult + FDelimiters.ETDLength) - StartPos);
    FAttributes.ParseAttributes(XMLStartTag);
  end
  else
    raise EJclEDIError.CreateID(31);
  // Set next start positon
  StartPos := SearchResult + FDelimiters.ETDLength;
  // Determine the nearest tag to search for
  I := StrSearch(FDelimiters.BTD + XMLTag_Segment, FData, StartPos);
  J := StrSearch(FDelimiters.BTD + XMLTag_TransactionSetLoop, FData, StartPos);
  if (I < J) or (J <= 0) then
  begin
    SearchTag := XMLTag_Segment;
    SearchResult := I;
  end
  else
  begin
    SearchTag := XMLTag_TransactionSetLoop;
    SearchResult := J;
  end;
  // Search for Segments or Loops
  while SearchResult > 0 do
  begin
    if SearchTag = XMLTag_Segment then
    begin
      SearchResult := StrSearch(FDelimiters.BOfETD + SearchTag, FData, SearchResult);
      if SearchResult > 0 then
      begin
        SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
        if SearchResult > 0 then
        begin
          I := AddSegment; // Add Segment
          EDIDataObjects[I].Data :=
            Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
          EDIDataObjects[I].Disassemble;
        end
        else
          raise EJclEDIError.CreateID(45);
      end
      else
        raise EJclEDIError.CreateID(44);
    end
    else
    begin
      NestedLoopCount := 0;
      SearchResult := StartPos;
      // Search for the proper end loop tag
      repeat
        I := StrSearch(FDelimiters.BOfETD + SearchTag, FData, SearchResult); //Find loop end
        J := StrSearch(FDelimiters.BTD + SearchTag, FData, SearchResult); //Find loop begin
        if (I < J) or (J <= 0) then
        begin
          Dec(NestedLoopCount);
          SearchResult := I + FDelimiters.ETDLength;
        end
        else
        if (I > J) and (J > 0) then
        begin
          Inc(NestedLoopCount);
          SearchResult := J + FDelimiters.ETDLength;
        end;
      until (NestedLoopCount <= 0) or (I <= 0);
      SearchResult := I;
      //
      if SearchResult > 0 then
      begin
        SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
        if SearchResult > 0 then
        begin
          I := AddGroup; // Add Transaction Set Loop
          EDIDataObjects[I].Data :=
            Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
          EDIDataObjects[I].Disassemble;
        end
        else
          raise EJclEDIError.CreateID(32);
      end
      else
        raise EJclEDIError.CreateID(31);
    end;
    // Set next start positon
    StartPos := SearchResult + FDelimiters.ETDLength;
    // Determine the nearest tag to search for
    I := StrSearch(FDelimiters.BTD + XMLTag_Segment, FData, StartPos);
    J := StrSearch(FDelimiters.BTD + XMLTag_TransactionSetLoop, FData, StartPos);
    if (I < J) or (J <= 0) then
      SearchTag := XMLTag_Segment
    else
      SearchTag := XMLTag_TransactionSetLoop;
    SearchResult := StrSearch(FDelimiters.BTD + SearchTag, FData, StartPos);
    StartPos := SearchResult;
  end;
  FData := '';
  //
  FState := ediDisassembled;
end;

function TEDIXMLTransactionSetLoop.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := nil;
  if Assigned(FParentTransactionSet) then
    Result := Parent.Delimiters;
end;

function TEDIXMLTransactionSetLoop.InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup;
begin
  Result := TEDIXMLTransactionSetLoop.Create(Self);
end;

//=== { TEDIXMLTransactionSet } ==============================================

constructor TEDIXMLTransactionSet.Create(Parent: TEDIXMLDataObject);
begin
  inherited Create(Parent);
  FParentTransactionSet := Self;
  FEDIDOT := ediTransactionSet;
end;

destructor TEDIXMLTransactionSet.Destroy;
begin
  inherited Destroy;
end;

function TEDIXMLTransactionSet.Assemble: string;
var
  I: Integer;
  AttributeString: string;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(26);
  end;

  AttributeString := FAttributes.CombineAttributes;
  if AttributeString <> '' then
    FData := FDelimiters.BTD + XMLTag_TransactionSet + FDelimiters.SpaceDelimiter +
      AttributeString + FDelimiters.ETD
  else
    FData := FDelimiters.BTD + XMLTag_TransactionSet + FDelimiters.ETD;

  if Length(FEDIDataObjects) > 0 then
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;
  FData := FData + FDelimiters.BOfETD + XMLTag_TransactionSet + FDelimiters.ETD;
  FLength := Length(FData);
  Result := FData; // Return assembled string

  DeleteEDIDataObjects;

  FState := ediAssembled;
end;

procedure TEDIXMLTransactionSet.Disassemble;
var
  I, J, StartPos, SearchResult: Integer;
  SearchTag, TempData: string;
  NestedLoopCount: Integer;
begin
  DeleteEDIDataObjects;
  // Check delimiter assignment
  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(25);
  end;
  // Set next start positon
  StartPos := 1;
  // Determine the nearest tag to search for
  I := StrSearch(FDelimiters.BTD + XMLTag_Segment, FData, StartPos);
  J := StrSearch(FDelimiters.BTD + XMLTag_TransactionSetLoop, FData, StartPos);
  if (I < J) or (J <= 0) then
    SearchTag := XMLTag_Segment
  else
    SearchTag := XMLTag_TransactionSetLoop;
  // Search for Segments or Loops
  SearchResult := StrSearch(FDelimiters.BTD + SearchTag, FData, StartPos);
  StartPos := SearchResult;
  while SearchResult > 0 do
  begin
    if SearchTag = XMLTag_Segment then
    begin
      SearchResult := StrSearch(FDelimiters.BOfETD + SearchTag, FData, SearchResult);
      if SearchResult > 0 then
      begin
        SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
        if SearchResult > 0 then
        begin
          I := AddSegment; //A dd Segment
          EDIDataObjects[I].Data :=
            Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
          EDIDataObjects[I].Disassemble;
        end
        else
          raise EJclEDIError.CreateID(45);
      end
      else
        raise EJclEDIError.CreateID(44);
    end
    else
    begin
      NestedLoopCount := 0;
      SearchResult := StartPos;
      // Search for the proper end loop tag
      repeat
        I := StrSearch(FDelimiters.BOfETD + SearchTag, FData, SearchResult); //Find loop end
        J := StrSearch(FDelimiters.BTD + SearchTag, FData, SearchResult); //Find loop begin
        if (I < J) or (J <= 0) then
        begin
          Dec(NestedLoopCount);
          SearchResult := I + FDelimiters.ETDLength;
        end
        else
        if (I > J) and (J > 0) then
        begin
          Inc(NestedLoopCount);
          SearchResult := J + FDelimiters.ETDLength;
        end;
      until (NestedLoopCount <= 0) or (I <= 0);
      SearchResult := I;
      //
      if SearchResult > 0 then
      begin
        SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
        if SearchResult > 0 then
        begin
          I := AddGroup; // Add Transaction Set Loop
          EDIDataObjects[I].Data :=
            Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
          EDIDataObjects[I].Disassemble;
        end
        else
          raise EJclEDIError.CreateID(32);
      end
      else
        raise EJclEDIError.CreateID(31);
    end;
    // Set next start positon
    StartPos := SearchResult + FDelimiters.ETDLength;
    // Determine the nearest tag to search for
    I := StrSearch(FDelimiters.BTD + XMLTag_Segment, FData, StartPos);
    J := StrSearch(FDelimiters.BTD + XMLTag_TransactionSetLoop, FData, StartPos);
    if (I < J) or (J <= 0) then
      SearchTag := XMLTag_Segment
    else
      SearchTag := XMLTag_TransactionSetLoop;
    SearchResult := StrSearch(FDelimiters.BTD + SearchTag, FData, StartPos);
  end;

  if Length(FEDIDataObjects) > 0 then
  begin
    // Search for Transaction Set Header and Trailer
    FSTSegment := TEDIXMLSegment(FEDIDataObjects[0]);
    FSESegment := TEDIXMLSegment(FEDIDataObjects[High(FEDIDataObjects)]);

    if FSTSegment.Attributes.GetAttributeValue(XMLAttribute_Id) = XMLTag_TSHSegmentId then
    begin
      TempData := FEDIDataObjects[0].Assemble;
      FreeAndNil(FEDIDataObjects[0]);
      //
      FSTSegment := TEDIXMLTransactionSetSegment.Create(Self);
      FSTSegment.Data := TempData;
      FSTSegment.Disassemble;
      //
      FEDIDataObjects[0] := FSTSegment;
    end
    else
    begin
      FSTSegment := nil;
      raise EJclEDIError.CreateID(59);
    end;

    if FSESegment.Attributes.GetAttributeValue(XMLAttribute_Id) = XMLTag_TSTSegmentId then
    begin
      TempData := FEDIDataObjects[High(FEDIDataObjects)].Assemble;
      FreeAndNil(FEDIDataObjects[High(FEDIDataObjects)]);
      //
      FSESegment := TEDIXMLTransactionSetSegment.Create(Self);
      FSESegment.Data := TempData;
      FSESegment.Disassemble;
      //
      FEDIDataObjects[High(FEDIDataObjects)] := FSESegment;
    end
    else
    begin
      FSESegment := nil;
      raise EJclEDIError.CreateID(60);
    end;
  end
  else
  begin
    FSTSegment := nil;
    FSESegment := nil;
    raise EJclEDIError.CreateID(61);
  end;
  FData := '';
  //
  FState := ediDisassembled;
end;

function TEDIXMLTransactionSet.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := nil;
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
    if Assigned(Parent) and (Parent is TEDIXMLFunctionalGroup) then
      if Assigned(Parent.Delimiters) then
        Result := Parent.Delimiters
      else
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIXMLInterchangeControl) then
        Result := Parent.Parent.Delimiters;
end;

function TEDIXMLTransactionSet.InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup;
begin
  Result := TEDIXMLTransactionSetLoop.Create(Self);
end;

//=== { TEDIXMLFunctionalGroup } =============================================

constructor TEDIXMLFunctionalGroup.Create(Parent: TEDIXMLDataObject);
begin
  inherited Create(Parent);
  FEDIDOT := ediFunctionalGroup;
end;

destructor TEDIXMLFunctionalGroup.Destroy;
begin
  inherited Destroy;
end;

function TEDIXMLFunctionalGroup.Assemble: string;
var
  I: Integer;
  AttributeString: string;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(16);
  end;

  AttributeString := FAttributes.CombineAttributes;
  if AttributeString <> '' then
    FData := FDelimiters.BTD + XMLTag_FunctionalGroup + FDelimiters.SpaceDelimiter +
      AttributeString + FDelimiters.ETD
  else
    FData := FDelimiters.BTD + XMLTag_FunctionalGroup + FDelimiters.ETD;

  if Length(FEDIDataObjects) > 0 then
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;
  FData := FData + FDelimiters.BOfETD + XMLTag_FunctionalGroup + FDelimiters.ETD;
  FLength := Length(FData);
  Result := FData; // Return assembled string

  DeleteEDIDataObjects;

  FState := ediAssembled;
end;

procedure TEDIXMLFunctionalGroup.Disassemble;
var
  I, StartPos, SearchResult: Integer;
begin
  DeleteEDIDataObjects;
  // Check delimiter assignment
  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(15);
  end;
  // Search for Functional Group Header
  StartPos := 1;
  SearchResult := SearchForSegmentInDataString(XMLTag_FGHSegmentId, StartPos);
  if SearchResult > 0 then
  begin
    StartPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_Segment, FData, SearchResult);
    if SearchResult > 0 then
    begin
      SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        FGSSegment := TEDIXMLFunctionalGroupSegment.Create(nil);
        AppendEDIDataObject(FGSSegment);
        FGSSegment.Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
        FGSSegment.Disassemble;
      end
      else
        raise EJclEDIError.CreateID(21);
    end
    else
      raise EJclEDIError.CreateID(20);
  end
  else
    raise EJclEDIError.CreateID(19);
  // Set next start positon
  StartPos := SearchResult + FDelimiters.ETDLength;
  // Search for Transaction Set
  SearchResult := StrSearch(FDelimiters.BTD + XMLTag_TransactionSet, FData, StartPos);
  while SearchResult > 0 do
  begin
    StartPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_TransactionSet, FData, SearchResult);
    if SearchResult > 0 then
    begin
      SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        I := AddGroup; // Add Transaction Set
        EDIDataObjects[I].Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
        EDIDataObjects[I].Disassemble;
      end
      else
        raise EJclEDIError.CreateID(28);
    end
    else
      raise EJclEDIError.CreateID(27);
    // Set next start positon
    StartPos := SearchResult + FDelimiters.ETDLength;
    // Search for Transaction Set
    SearchResult := StrSearch(FDelimiters.BTD + XMLTag_TransactionSet, FData, StartPos);
  end;
  // Search for Functional Group Trailer
  SearchResult := SearchForSegmentInDataString(XMLTag_FGTSegmentId, StartPos);
  if SearchResult > 0 then
  begin
    StartPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_Segment, FData, SearchResult);
    if SearchResult > 0 then
    begin
      SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        FGESegment := TEDIXMLFunctionalGroupSegment.Create(nil);
        AppendEDIDataObject(FGESegment);
        FGESegment.Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
        FGESegment.Disassemble;
      end
      else
        raise EJclEDIError.CreateID(24);
    end
    else
      raise EJclEDIError.CreateID(23);
  end
  else
    raise EJclEDIError.CreateID(22);
  FData := '';
  //
  FState := ediDisassembled;
end;

function TEDIXMLFunctionalGroup.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := nil;
  // Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
    if Assigned(Parent) and (Parent is TEDIXMLInterchangeControl) then
      Result := Parent.Delimiters;
end;

function TEDIXMLFunctionalGroup.InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup;
begin
  Result := TEDIXMLTransactionSet.Create(Self);
end;

//=== { TEDIXMLInterchangeControl } ==========================================

constructor TEDIXMLInterchangeControl.Create(Parent: TEDIXMLDataObject);
begin
  inherited Create(Parent);
  FEDIDOT := ediInterchangeControl;
end;

destructor TEDIXMLInterchangeControl.Destroy;
begin
  FreeAndNil(FDelimiters);
  inherited Destroy;
end;

function TEDIXMLInterchangeControl.Assemble: string;
var
  I: Integer;
  AttributeString: string;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(5);
  end;

  AttributeString := FAttributes.CombineAttributes;
  if AttributeString <> '' then
    FData := FDelimiters.BTD + XMLTag_InterchangeControl + FDelimiters.SpaceDelimiter +
      AttributeString + FDelimiters.ETD
  else
    FData := FDelimiters.BTD + XMLTag_InterchangeControl + FDelimiters.ETD;

  if Length(FEDIDataObjects) > 0 then
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;
  FData := FData + FDelimiters.BOfETD + XMLTag_InterchangeControl + FDelimiters.ETD;
  FLength := Length(FData);
  Result := FData; // Return assembled string

  DeleteEDIDataObjects;

  FState := ediAssembled;
end;

procedure TEDIXMLInterchangeControl.Disassemble;
var
  I, StartPos, SearchResult: Integer;
begin
  DeleteEDIDataObjects;
  // Check if delimiters are assigned
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(6);
  end;
  // Search for Interchange Control Header
  StartPos := 1;
  SearchResult := SearchForSegmentInDataString(XMLTag_ICHSegmentId, StartPos);
  if SearchResult > 0 then
  begin
    StartPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_Segment, FData, SearchResult);
    if SearchResult > 0 then
    begin
      SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        FISASegment := TEDIXMLInterchangeControlSegment.Create(nil);
        AppendEDIDataObject(FISASegment);
        FISASegment.Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
        FISASegment.Disassemble;
      end
      else
        raise EJclEDIError.CreateID(11);
    end
    else
      raise EJclEDIError.CreateID(10);
  end
  else
    raise EJclEDIError.CreateID(9);
  // Set next start position. Move past the delimiter
  StartPos := SearchResult + FDelimiters.ETDLength;
  // Search for Functional Group
  SearchResult := StrSearch(FDelimiters.BTD + XMLTag_FunctionalGroup, FData, StartPos);
  while SearchResult > 0 do
  begin
    StartPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_FunctionalGroup, FData, SearchResult);
    if SearchResult > 0 then
    begin
      SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        I := AddGroup; // Add Functional Group
        EDIDataObjects[I].Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
        EDIDataObjects[I].Disassemble;
      end
      else
        raise EJclEDIError.CreateID(18);
    end
    else
      raise EJclEDIError.CreateID(17);
    // Set next start positon
    StartPos := SearchResult + FDelimiters.ETDLength;
    // Search for Functional Group
    SearchResult := StrSearch(FDelimiters.BTD + XMLTag_FunctionalGroup, FData, StartPos);
  end;
  // Search for Interchange Control Trailer
  SearchResult := SearchForSegmentInDataString(XMLTag_ICTSegmentId, StartPos);
  if SearchResult > 0 then
  begin
    StartPos := SearchResult;
    SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_Segment, FData, SearchResult);
    if SearchResult > 0 then
    begin
      SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        FIEASegment := TEDIXMLInterchangeControlSegment.Create(nil);
        AppendEDIDataObject(FIEASegment);
        FIEASegment.Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
        FIEASegment.Disassemble;
      end
      else
        raise EJclEDIError.CreateID(14);
    end
    else
      raise EJclEDIError.CreateID(13);
  end
  else
    raise EJclEDIError.CreateID(12);
  FData := '';
  //
  FState := ediDisassembled;
end;

function TEDIXMLInterchangeControl.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := TEDIXMLDelimiters.Create;
end;

function TEDIXMLInterchangeControl.InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup;
begin
  Result := TEDIXMLFunctionalGroup.Create(Self);
end;

//=== { TEDIXMLFile } ========================================================

constructor TEDIXMLFile.Create(Parent: TEDIXMLDataObject);
begin
  inherited Create(Parent);
  FEDIXMLFileHeader := TEDIXMLFileHeader.Create;
  FEDIDOT := ediFile;
end;

destructor TEDIXMLFile.Destroy;
begin
  FEDIXMLFileHeader.Free;
  inherited Destroy;
end;

function TEDIXMLFile.Assemble: string;
var
  I: Integer;
  AttributeString: string;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(4);
  end;

  FData := FEDIXMLFileHeader.OutputXMLHeader;

  AttributeString := FAttributes.CombineAttributes;
  if AttributeString <> '' then
    FData := FData + FDelimiters.BTD + XMLTag_EDIFile + FDelimiters.SpaceDelimiter +
      AttributeString + FDelimiters.ETD
  else
    FData := FData + FDelimiters.BTD + XMLTag_EDIFile + FDelimiters.ETD;

  if Length(FEDIDataObjects) > 0 then
    for I := Low(FEDIDataObjects) to High(FEDIDataObjects) do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;
  FData := FData + FDelimiters.BOfETD + XMLTag_EDIFile + FDelimiters.ETD;
  FLength := Length(FData);
  Result := FData; // Return assembled string

  DeleteEDIDataObjects;

  FState := ediAssembled;
end;

procedure TEDIXMLFile.Disassemble;
var
  I, StartPos, SearchResult: Integer;
  XMLHeader: string;
begin
  DeleteEDIDataObjects;
  //
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(3);
  end;
  // Search for XML file heaer
  StartPos := 1;
  SearchResult := StrSearch(EDIXMLDelimiter_FileHeaderBegin, FData, StartPos);
  StartPos := SearchResult;
  if SearchResult > 0 then
  begin
    SearchResult := StrSearch(EDIXMLDelimiter_FileHeaderEnd, FData, StartPos);
    if SearchResult > 0 then
    begin
      XMLHeader :=
        Copy(FData, StartPos, ((SearchResult - StartPos) + Length(EDIXMLDelimiter_FileHeaderEnd)));
      FEDIXMLFileHeader.ParseXMLHeader(XMLHeader);
    end
    else
    begin
      // Hey the header was not found
    end;
  end
  else
  begin
    // Hey the header was not found
  end;
  // Search for Interchange
  StartPos := 1;
  SearchResult := StrSearch(FDelimiters.BTD + XMLTag_InterchangeControl, FData, StartPos);
  StartPos := SearchResult;
  while SearchResult > 0 do
  begin
    // Search for Interchange end tag
    SearchResult := StrSearch(FDelimiters.BOfETD + XMLTag_InterchangeControl, FData, SearchResult);
    if SearchResult > 0 then
    begin
      // Search for Interchange end tag delimiter
      SearchResult := StrSearch(FDelimiters.ETD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        I := AddGroup; // Add Interchange
        FEDIDataObjects[I].Delimiters := TEDIXMLDelimiters.Create;
        FEDIDataObjects[I].Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.ETDLength));
        FEDIDataObjects[I].Disassemble;
      end
      else
        raise EJclEDIError.CreateID(8);
    end
    else
      raise EJclEDIError.CreateID(7);
    // Set next start position. Move past the delimiter
    StartPos := SearchResult + FDelimiters.ETDLength;
    // Search for Interchange
    SearchResult := StrSearch(FDelimiters.BTD + XMLTag_InterchangeControl, FData, StartPos);
  end;
  FData := '';

  FState := ediDisassembled;
end;

function TEDIXMLFile.InternalAssignDelimiters: TEDIXMLDelimiters;
begin
  Result := TEDIXMLDelimiters.Create;
end;

function TEDIXMLFile.InternalCreateDataObjectGroup: TEDIXMLDataObjectGroup;
begin
  Result := TEDIXMLInterchangeControl.Create(Self);
end;

procedure TEDIXMLFile.InternalLoadFromFile;
var
  EDIFileStream: TFileStream;
begin
  FData := '';
  if FFileName <> '' then
  begin
    EDIFileStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
    try
      {$IFDEF CLR}
      EDIFileStream.ReadStringAnsiBuffer(FData, EDIFileStream.Size);
      {$ELSE}
      SetLength(FData, EDIFileStream.Size);
      EDIFileStream.Read(Pointer(FData)^, EDIFileStream.Size);
      {$ENDIF CLR}
    finally
      EDIFileStream.Free;
    end;
    FData := StringReplace(FData, AnsiCrLf, '', [rfReplaceAll, rfIgnoreCase]);
  end
  else
    raise EJclEDIError.CreateID(1);
end;

procedure TEDIXMLFile.LoadFromFile(const FileName: string);
begin
  FFileName := FileName;
  InternalLoadFromFile;
end;

procedure TEDIXMLFile.ReLoadFromFile;
begin
  InternalLoadFromFile;
end;

procedure TEDIXMLFile.SaveAsToFile(const FileName: string);
var
  EDIFileStream: TFileStream;
begin
  FFileName := FileName;
  if FFileName <> '' then
  begin
    EDIFileStream := TFileStream.Create(FFileName, fmCreate or fmShareDenyNone);
    try
      {$IFDEF CLR}
      EDIFileStream.WriteStringAnsiBuffer(FData);
      {$ELSE}
      EDIFileStream.Write(Pointer(FData)^, Length(FData));
      {$ENDIF CLR}
    finally
      EDIFileStream.Free;
    end;
  end
  else
    raise EJclEDIError.CreateID(2);
end;

procedure TEDIXMLFile.SaveToFile;
var
  EDIFileStream: TFileStream;
begin
  if FFileName <> '' then
  begin
    EDIFileStream := TFileStream.Create(FFileName, fmCreate or fmShareDenyNone);
    try
      {$IFDEF CLR}
      EDIFileStream.WriteStringAnsiBuffer(FData);
      {$ELSE}
      EDIFileStream.Write(Pointer(FData)^, Length(FData));
      {$ENDIF CLR}
    finally
      EDIFileStream.Free;
    end;
  end
  else
    raise EJclEDIError.CreateID(2);
end;

//=== { TEDIXMLFileHeader } ==================================================

constructor TEDIXMLFileHeader.Create;
begin
  inherited Create;
  FAttributes := TEDIXMLAttributes.Create;
  FDelimiters := TEDIXMLDelimiters.Create;
  FAttributes.SetAttribute(EDIXMLAttributeStr_version, Value_Version10);
  FAttributes.SetAttribute(EDIXMLAttributeStr_encoding, Value_Windows1252); // ISO-8859-1
  FXMLNameSpaceOption := nsNone;
  FAttributes.SetAttribute(EDIXMLAttributeStr_xmlns, Value_EDITRANSDOC);
  FAttributes.SetAttribute(EDIXMLAttributeStr_xmlnsEDI, Value_EDITRANSDOC);
end;

destructor TEDIXMLFileHeader.Destroy;
begin
  FDelimiters.Free;
  FAttributes.Free;
  inherited Destroy;
end;

function TEDIXMLFileHeader.OutputAdditionalXMLHeaderAttributes: string;
begin
  Result := '';
end;

function TEDIXMLFileHeader.OutputXMLHeader: string;
var
  AdditionalAttributes: string;
begin
  Result := EDIXMLDelimiter_FileHeaderBegin + Value_xml + Delimiters.SpaceDelimiter +
    FAttributes.GetAttributeString(EDIXMLAttributeStr_version);
  case FXMLNameSpaceOption of
    nsNone:
      Result := Result + Delimiters.SpaceDelimiter +
        FAttributes.GetAttributeString(EDIXMLAttributeStr_encoding);
    nsDefault:
      Result := Result +
        Delimiters.SpaceDelimiter + FAttributes.GetAttributeString(EDIXMLAttributeStr_encoding) +
        Delimiters.SpaceDelimiter + FAttributes.GetAttributeString(EDIXMLAttributeStr_xmlns);
    nsQualified:
      Result := Result +
        Delimiters.SpaceDelimiter + FAttributes.GetAttributeString(EDIXMLAttributeStr_encoding) +
        Delimiters.SpaceDelimiter + FAttributes.GetAttributeString(EDIXMLAttributeStr_xmlnsEDI);
  end;
  AdditionalAttributes := OutputAdditionalXMLHeaderAttributes;
  if AdditionalAttributes <> '' then
    Result := Result + Delimiters.SpaceDelimiter + AdditionalAttributes;
  Result := Result + EDIXMLDelimiter_FileHeaderEnd;
end;

procedure TEDIXMLFileHeader.ParseXMLHeader(XMLHeader: string);
begin
  FAttributes.ParseAttributes(XMLHeader);
end;

//=== { TEDIXMLANSIX12FormatTranslator } =====================================

constructor TEDIXMLANSIX12FormatTranslator.Create;
begin
  inherited Create;
end;

destructor TEDIXMLANSIX12FormatTranslator.Destroy;
begin
  inherited Destroy;
end;

function TEDIXMLANSIX12FormatTranslator.ConvertToEDISegment(
  XMLSegment: TEDIXMLSegment): TEDISegment;
var
  ediE, xmlE: Integer;
begin
  if XMLSegment is TEDIXMLInterchangeControlSegment then
    Result := TEDIInterchangeControlSegment.Create(nil)
  else
  if XMLSegment is TEDIXMLFunctionalGroupSegment then
    Result := TEDIFunctionalGroupSegment.Create(nil)
  else
  if XMLSegment is TEDIXMLTransactionSetSegment then
    Result := TEDITransactionSetSegment.Create(nil)
  else
    Result := TEDISegment.Create(nil);
  Result.SegmentID := XMLSegment.Attributes.GetAttributeValue(XMLAttribute_Id);
  for ediE := Low(XMLSegment.Elements) to High(XMLSegment.Elements) do
  begin
    xmlE := Result.AddElement;
    Result[xmlE].Data := XMLSegment[ediE].Data;
  end;
end;

function TEDIXMLANSIX12FormatTranslator.ConvertToEDITransaction(
  XMLTransactionSet: TEDIXMLTransactionSet): TEDITransactionSet;
var
  I: Integer;
  EDISegment: TEDISegment;
  XMLSegment: TEDIXMLSegment;
  XMLLoop: TEDIXMLTransactionSetLoop;
begin
  Result := TEDITransactionSet.Create(nil);
  for I := Low(XMLTransactionSet.EDIDataObjects) to High(XMLTransactionSet.EDIDataObjects) do
  begin
    if XMLTransactionSet[I] is TEDIXMLSegment then
    begin
      XMLSegment := TEDIXMLSegment(XMLTransactionSet[I]);
      if XMLSegment.Attributes.GetAttributeValue(XMLAttribute_Id) = XMLTag_TSHSegmentId then
      begin
        EDISegment := ConvertToEDISegment(XMLSegment);
        Result.SegmentST := TEDITransactionSetSegment(EDISegment);
      end
      else
      if XMLSegment.Attributes.GetAttributeValue(XMLAttribute_Id) = XMLTag_TSTSegmentId then
      begin
        EDISegment := ConvertToEDISegment(XMLSegment);
        Result.SegmentSE := TEDITransactionSetSegment(EDISegment);
      end
      else
      begin
        EDISegment := ConvertToEDISegment(XMLSegment);
        Result.AppendSegment(EDISegment);
      end;
    end
    else
    if XMLTransactionSet[I] is TEDIXMLTransactionSetLoop then
    begin
      XMLLoop := TEDIXMLTransactionSetLoop(XMLTransactionSet[I]);
      ConvertTransactionSetLoopToEDI(Result, XMLLoop);
    end
    else
      raise EJclEDIError.CreateIDFmt(62, [XMLTransactionSet[I].ClassName]);
  end;
end;

function TEDIXMLANSIX12FormatTranslator.ConvertToXMLSegment(
  EDISegment: TEDISegment): TEDIXMLSegment;
var
  ediE, xmlE: Integer;
begin
  if EDISegment is TEDIInterchangeControlSegment then
    Result := TEDIXMLInterchangeControlSegment.Create(nil)
  else
  if EDISegment is TEDIFunctionalGroupSegment then
    Result := TEDIXMLFunctionalGroupSegment.Create(nil)
  else
  if EDISegment is TEDITransactionSetSegment then
    Result := TEDIXMLTransactionSetSegment.Create(nil)
  else
    Result := TEDIXMLSegment.Create(nil);
  Result.Attributes.SetAttribute(XMLAttribute_Id, EDISegment.SegmentID);
  for ediE := 0 to EDISegment.ElementCount - 1 do
  begin
    xmlE := Result.AddElement;
    Result[xmlE].Data := EDISegment[ediE].Data;
  end;
end;

function TEDIXMLANSIX12FormatTranslator.ConvertToXMLTransaction(
  EDITransactionSet: TEDITransactionSet;
  EDITransactionSetSpec: TEDITransactionSetSpec): TEDIXMLTransactionSet;
var
  EDIDoc: TEDITransactionSetDocument;
  XMLSegment: TEDIXMLSegment;
begin
  Result := TEDIXMLTransactionSet.Create(nil);
  EDIDoc := TEDITransactionSetDocument.Create(EDITransactionSet,
    EDITransactionSet, EDITransactionSetSpec);
  try
    EDIDoc.FormatDocument;

    XMLSegment := ConvertToXMLSegment(EDITransactionSet.SegmentST);
    Result.AppendEDIDataObject(XMLSegment);

    ConvertTransactionSetLoopToXML(EDIDoc, Result);

    XMLSegment := ConvertToXMLSegment(EDITransactionSet.SegmentSE);
    Result.AppendEDIDataObject(XMLSegment);
  finally
    EDIDoc.Free;
  end;
end;

function TEDIXMLANSIX12FormatTranslator.ConvertToXMLTransaction(
  EDITransactionSet: TEDITransactionSet): TEDIXMLTransactionSet;
var
  I: Integer;
  XMLSegment: TEDIXMLSegment;
begin
  Result := TEDIXMLTransactionSet.Create(nil);

  XMLSegment := ConvertToXMLSegment(EDITransactionSet.SegmentST);
  Result.AppendEDIDataObject(XMLSegment);

  for I := 0 to EDITransactionSet.SegmentCount - 1 do
  begin
    XMLSegment := ConvertToXMLSegment(EDITransactionSet.Segment[I]);
    Result.AppendEDIDataObject(XMLSegment);
  end;

  XMLSegment := ConvertToXMLSegment(EDITransactionSet.SegmentSE);
  Result.AppendEDIDataObject(XMLSegment);
end;

procedure TEDIXMLANSIX12FormatTranslator.ConvertTransactionSetLoopToEDI(
  EDITransactionSet: TEDITransactionSet;
  XMLLoop: TEDIXMLTransactionSetLoop);
var
  I: Integer;
  EDISegment: TEDISegment;
  XMLSegment: TEDIXMLSegment;
  nXMLLoop: TEDIXMLTransactionSetLoop;
begin
  for I := Low(XMLLoop.EDIDataObjects) to High(XMLLoop.EDIDataObjects) do
  begin
    if XMLLoop[I] is TEDIXMLSegment then
    begin
      XMLSegment := TEDIXMLSegment(XMLLoop[I]);
      EDISegment := ConvertToEDISegment(XMLSegment);
      EDITransactionSet.AppendSegment(EDISegment);
    end
    else
    if XMLLoop[I] is TEDIXMLTransactionSetLoop then
    begin
      nXMLLoop := TEDIXMLTransactionSetLoop(XMLLoop[I]);
      ConvertTransactionSetLoopToEDI(EDITransactionSet, nXMLLoop);
    end
    else
      raise EJclEDIError.CreateIDFmt(62, [XMLLoop[I].ClassName]);
  end;
end;

procedure TEDIXMLANSIX12FormatTranslator.ConvertTransactionSetLoopToXML(
  EDILoop: TEDITransactionSetLoop; XMLLoop: TEDIXMLTransactionSetLoop);
var
  I, xmlL: Integer;
  EDISegment: TEDISegment;
  XMLSegment: TEDIXMLSegment;
  nEDILoop: TEDITransactionSetLoop;
  nXMLLoop: TEDIXMLTransactionSetLoop;
begin
  for I := 0 to EDILoop.EDIDataObjectCount - 1 do
  begin
    if EDILoop[I] is TEDISegment then
    begin
      EDISegment := TEDISegment(EDILoop[I]);
      XMLSegment := ConvertToXMLSegment(EDISegment);
      XMLLoop.AppendEDIDataObject(XMLSegment);
    end
    else
    if EDILoop[I] is TEDITransactionSetLoop then
    begin
      nEDILoop := TEDITransactionSetLoop(EDILoop[I]);
      xmlL := XMLLoop.AddGroup;
      nXMLLoop := TEDIXMLTransactionSetLoop(XMLLoop[xmlL]);
      nXMLLoop.Attributes.SetAttribute(XMLAttribute_Id, nEDILoop.OwnerLoopId);
      ConvertTransactionSetLoopToXML(nEDILoop, nXMLLoop);
    end
    else
      raise EJclEDIError.CreateIDFmt(62, [EDILoop[I].ClassName]);
  end;
end;

{$IFNDEF EDI_WEAK_PACKAGE_UNITS}
{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}
{$ENDIF ~EDI_WEAK_PACKAGE_UNITS}

end.

