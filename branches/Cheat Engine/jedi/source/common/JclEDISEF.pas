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
{ The Original Code is JclEDISEF.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Raymond Alexander.                                 }
{ Portions created by Raymond Alexander are Copyright (C) Raymond Alexander. All rights reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Raymond Alexander (rayspostbox3), Robert Marquardt, Robert Rossmair, Petr Vones                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ EDI Standard Exchange Format (*.sef) File Parser Unit                                            }
{                                                                                                  }
{ This unit is still in development                                                                }
{                                                                                                  }
{ Unit owner: Raymond Alexander                                                                    }
{ Date created: July, 20, 2003                                                                     }
{ Additional Info:                                                                                 }
{   E-Mail at RaysDelphiBox3 att hotmail dott com                                                  }
{   For latest EDI specific demos see http://sourceforge.net/projects/edisdk                       }
{   See home page for latest news & events and online help.                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-12-01 12:59:43 +0100 (sam., 01 déc. 2007)                         $ }
{ Revision:      $Rev:: 2255                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclEDISEF;

{$I jcl.inc}

{$IFDEF EDI_WEAK_PACKAGE_UNITS}
  {$IFDEF SUPPORTS_WEAKPACKAGEUNIT}
    {$WEAKPACKAGEUNIT ON}
  {$ENDIF SUPPORTS_WEAKPACKAGEUNIT}
{$ENDIF EDI_WEAK_PACKAGE_UNITS}

interface

uses
  SysUtils, Classes, Contnrs,
  {$IFNDEF EDI_WEAK_PACKAGE_UNITS}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$ENDIF ~EDI_WEAK_PACKAGE_UNITS}
  JclBase, JclEDI;

const
  SectionTag_VER = '.VER';
  SectionTag_INI = '.INI';
  SectionTag_PRIVATE = '';
  SectionTag_PUBLIC = '';
  SectionTag_STD = '.STD';
  SectionTag_SETS = '.SETS';
  SectionTag_SEGS = '.SEGS';
  SectionTag_COMS = '.COMS';
  SectionTag_ELMS = '.ELMS';
  SectionTag_CODES = '.CODES';
  SectionTag_VALLISTS = '';
  SectionTag_OBJVARS = '';
  SectionTag_SEMREFS = '';
  SectionTag_TEXT = '';
  SectionTag_TEXTSETS = '.TEXT,SETS';
  SectionTag_ = '';
  // EDI SDK Specific Extensions
  SectionTag_JCL_SETSEXT = '.SETSEXT';
  SectionTag_JCL_SEGSEXT = '.SEGSEXT';
  SectionTag_JCL_COMSEXT = '.COMSEXT';
  SectionTag_JCL_ELMSEXT = '.ELMSEXT';

  Value_UndefinedMaximum = MaxInt;

  EDISEFUserAttributePeriod = '.';
  EDISEFUserAttributeExclamationPoint = '!';
  EDISEFUserAttributeDollarSign = '$';
  EDISEFUserAttributeHyphen = '-';
  EDISEFUserAttributeAmpersand = '&';

  EDISEFUserAttributePeriodDesc = 'Not Used';
  EDISEFUserAttributeExclamationPointDesc = 'Mandatory';
  EDISEFUserAttributeDollarSignDesc = 'Recommended';
  EDISEFUserAttributeHyphenDesc = 'Not Recommended';
  EDISEFUserAttributeAmpersandDesc = 'Dependent';

  EDISEFUserAttributeSet =
    [EDISEFUserAttributePeriod, EDISEFUserAttributeExclamationPoint,
     EDISEFUserAttributeDollarSign, EDISEFUserAttributeHyphen,
     EDISEFUserAttributeAmpersand];

const
  // EDI SEF Text,Sets Constants
  SEFTextCR = '\r'; // carriage return
  SEFTextLF = '\n'; // line feed
  SEFTextCRLF = SEFTextCR + SEFTextLF;
  // Example: Transaction Set:850
  SEFTextSetsCode_Set0 = '0'; // Transaction Set or message title.
  SEFTextSetsCode_Set1 = '1'; // Transaction Set functional group (X12).
  SEFTextSetsCode_Set2 = '2'; // Transaction Set or message purpose.
  SEFTextSetsCode_Set3 = '3'; // Level 1 note on transaction set or message.
  SEFTextSetsCode_Set4 = '4'; // Level 2 note on transaction set or message.
  SEFTextSetsCode_Set5 = '5'; // Level 3 note on transaction set or message. See * below for other levels of notes.
  // Example: Transaction Set~segment ordinal number: 850~1
  SEFTextSetsCode_Seg0 = '0'; // Segment reference notes that are part of the transaction set in X12.
  SEFTextSetsCode_Seg1 = '1'; // Segment reference notes documented with the segment (like in VICS/UCS).
  SEFTextSetsCode_Seg2 = '2'; // Segment reference comment documented with the transaction set.
  SEFTextSetsCode_Seg3 = '3'; // Segment name.
  SEFTextSetsCode_Seg4 = '4'; // Level 1 note on segment.
  SEFTextSetsCode_Seg5 = '5'; // Level 2 note on segment.
  SEFTextSetsCode_Seg6 = '6'; // Segment purpose.
  SEFTextSetsCode_Seg7 = '7'; // Level 3 note on segment. See * below for other levels of notes.
  // Example: Transaction Set~segment ordinal number~element or composite ordinal number: 850~1~4
  SEFTextSetsCode_Elm0 = '0'; // Level 1 note on element or composite.
  SEFTextSetsCode_Elm1 = '1'; // Level 2 note on element or composite.
  SEFTextSetsCode_Elm2 = '2'; // Name of element or composite.
  SEFTextSetsCode_Elm4 = '4'; // Level 3 note on element or composite. See * below for other levels of notes.

type
  TEDISEFComsUserAttributes =
    (caPeriod, caExclamationPoint, caDollarSign, caHyphen, caAmpersand);

  TEDISEFObject = class(TEDIObject);
  TEDISEFDataObject = class;
  TEDISEFDataObjectGroup = class;
  TEDISEFSubElement = class;
  TEDISEFElement = class;
  TEDISEFCompositeElement = class;
  TEDISEFSegment = class;
  TEDISEFLoop = class;
  TEDISEFTable = class;
  TEDISEFSet = class;
  TEDISEFFile = class;

  TEDISEFDataObjectListItem = class;
  TEDISEFDataObjectList = class;

  TEDISEFObjectParentType =
    (sefNil, sefList, sefElement, sefCompositeElement, sefSegment);

  //  EDI SEF Data Object
  TEDISEFDataObject = class(TEDISEFObject)
  private
    procedure SetId(const Value: string);
  protected
    FState: TEDIDataObjectDataState;
    FId: string;
    FData: string;
    FLength: Integer;
    FParent: TEDISEFDataObject;
    FSEFFile: TEDISEFFile;
    FErrorLog: TStrings;
    FOwnerItemRef: TEDISEFDataObjectListItem;
    function GetData: string;
    procedure SetData(const Data: string);
    procedure SetParent(const Value: TEDISEFDataObject); virtual;
    property OwnerItemRef: TEDISEFDataObjectListItem read FOwnerItemRef write FOwnerItemRef;
    function CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject; virtual; abstract;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; virtual; abstract;
    procedure Disassemble; virtual; abstract;
    procedure UpdateOwnerItemName;
    function Clone(NewParent: TEDISEFDataObject): TEDISEFDataObject;
  published
    property State: TEDIDataObjectDataState read FState;
    property Id: string read FId write SetId;
    property Data: string read GetData write SetData;
    property DataLength: Integer read FLength;
    property Parent: TEDISEFDataObject read FParent write SetParent;
    property SEFFile: TEDISEFFile read FSEFFile write FSEFFile;
  end;

  TEDISEFDataObjectClass = class of TEDISEFDataObject;

  //  EDI SEF Data Object List Item
  TEDISEFDataObjectListItem = class(TEDIObjectListItem)
  private
    function GetEDISEFDataObject: TEDISEFDataObject;
    procedure SetEDISEFDataObject(const Value: TEDISEFDataObject);
  public
    procedure LinkToObject;
    procedure UpdateName;
    function NextItem: TEDISEFDataObjectListItem;
    function PriorItem: TEDISEFDataObjectListItem;
  published
    property EDISEFDataObject: TEDISEFDataObject read GetEDISEFDataObject write SetEDISEFDataObject;
  end;

  //  EDI SEF Data Object List
  TEDISEFDataObjectList = class(TEDIObjectList)
  private
    function GetEDISEFDataObject(Index: Integer): TEDISEFDataObject;
    procedure SetEDISEFDataObject(Index: Integer; const Value: TEDISEFDataObject);
  public
    function CreateListItem(PriorItem: TEDIObjectListItem;
      EDIObject: TEDIObject = nil): TEDIObjectListItem; override;

    function First(Index: Integer = 0): TEDISEFDataObjectListItem; reintroduce;
    function Next: TEDISEFDataObjectListItem; reintroduce;
    function Prior: TEDISEFDataObjectListItem; reintroduce;
    function Last: TEDISEFDataObjectListItem; reintroduce;

    function Add(EDISEFDataObject: TEDISEFDataObject;
      Name: string = ''): TEDISEFDataObjectListItem; overload;

    function Insert(EDISEFDataObject,
      BeforeEDISEFDataObject: TEDISEFDataObject): TEDISEFDataObjectListItem; overload;

    function FindItemByName(Name: string;
      StartItem: TEDIObjectListItem = nil): TEDISEFDataObjectListItem; reintroduce;
    function GetObjectByItemByName(Name: string): TEDISEFDataObject;
    //
    property EDISEFDataObject[Index: Integer]: TEDISEFDataObject read GetEDISEFDataObject
      write SetEDISEFDataObject; default;
  end;

  //  EDI SEF Data Object Group
  TEDISEFDataObjectGroup = class(TEDISEFDataObject)
  private
    function GetEDISEFDataObject(Index: Integer): TEDISEFDataObject;
    function GetCount: Integer;
  protected
    FEDISEFDataObjects: TEDISEFDataObjectList;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;

    property EDISEFDataObject[Index: Integer]: TEDISEFDataObject read GetEDISEFDataObject; default;
  published
    property EDISEFDataObjects: TEDISEFDataObjectList read FEDISEFDataObjects;
    property EDISEFDataObjectCount: Integer read GetCount;
  end;

  //  EDI SEF Repeating Pattern
  TEDISEFRepeatingPattern = class(TEDISEFDataObjectGroup)
  private
    FBaseParent: TEDISEFDataObject;
    FRepeatCount: Integer;
  protected
    procedure SetParent(const Value: TEDISEFDataObject); override;
    function CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject; override;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    function Clone(NewParent: TEDISEFDataObject): TEDISEFRepeatingPattern; reintroduce;

    function AddRepeatingPattern: TEDISEFRepeatingPattern;
    function AppendRepeatingPattern(
      RepeatingPattern: TEDISEFRepeatingPattern): TEDISEFRepeatingPattern;
    function ExtractRepeatingPattern(
      RepeatingPattern: TEDISEFRepeatingPattern): TEDISEFRepeatingPattern;
    procedure DeleteRepeatingPattern(
      RepeatingPattern: TEDISEFRepeatingPattern);
    function InsertRepeatingPattern(
      BeforeObject: TEDISEFDataObject): TEDISEFRepeatingPattern; overload;
    function InsertRepeatingPattern(RepeatingPattern: TEDISEFRepeatingPattern;
      BeforeObject: TEDISEFDataObject): TEDISEFRepeatingPattern; overload;

  published
    property BaseParent: TEDISEFDataObject read FBaseParent;
    property RepeatCount: Integer read FRepeatCount write FRepeatCount;
  end;

  //  EDI SEF Text Objects
  TEDISEFWhereType = (twUnknown, twSet, twSegment, twElementOrCompositeElement, twSubElement);

  TEDISEFText = class(TEDIObject)
  private
    FWhereLocation: TStringList;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetDescription: string;
    function GetWhereLocation: TStrings;
  protected
    FData: string;
    FEDISEFWhereType: TEDISEFWhereType;
    FWhere: string;
    FWhat: string;
    FText: string;
    function GetData: string;
    procedure SetData(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    function Assemble: string; virtual;
    procedure Disassemble; virtual;
  published
    property Data: string read GetData write SetData;
    property WhereLocation: TStrings read GetWhereLocation;
    property Where: string read FWhere;
    property What: string read FWhat;
    property Text: string read GetText write SetText;
    property Description: string read GetDescription;
  end;

  TEDISEFTextSet = class(TEDISEFText)
  private
    FWhereSet: string;
    FWhereSegment: Integer; // Ordinal
    FWhereElement: Integer; // Ordinal
    FWhereSubElement: Integer; // Ordinal
  public
    constructor Create;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
  end;

  TEDISEFTextSets = class(TEDIObjectList)
  public
    function GetText(Code: string): string;
    procedure SetText(EDISEFFile: TEDISEFFile; Location, Code, Text: string);
  end;

  //  EDI SEF Element
  TEDISEFElement = class(TEDISEFDataObject)
  protected
    FUserAttribute: string;
    FOrdinal: Integer;
    FOutOfSequenceOrdinal: Boolean;
    FElementType: string;
    FMinimumLength: Integer;
    FMaximumLength: Integer;
    FRequirementDesignator: string;
    FRepeatCount: Integer;
    FEDISEFTextSets: TEDISEFTextSets;
    function CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject; override;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    procedure Assign(EDISEFElement: TEDISEFElement);
    function Clone(NewParent: TEDISEFDataObject): TEDISEFElement; reintroduce;
    function CloneAsSubElement(NewParent: TEDISEFDataObject): TEDISEFSubElement;
    function GetTextSetsLocation: string;
    procedure BindTextSets(TEXTSETS: TEDISEFTextSets);
  published
    property UserAttribute: string read FUserAttribute write FUserAttribute;
    property Ordinal: Integer read FOrdinal write FOrdinal;
    property OutOfSequenceOrdinal: Boolean read FOutOfSequenceOrdinal write FOutOfSequenceOrdinal;
    property ElementId: string read FId write FId;
    property ElementType: string read FElementType write FElementType;
    property MinimumLength: Integer read FMinimumLength write FMinimumLength;
    property MaximumLength: Integer read FMaximumLength write FMaximumLength;
    property RequirementDesignator: string read FRequirementDesignator write FRequirementDesignator;
    property RepeatCount: Integer read FRepeatCount write FRepeatCount;
    property TextSetsLocation: string read GetTextSetsLocation;
    //
    property TEXTSETS: TEDISEFTextSets read FEDISEFTextSets;
  end;

  TEDISEFSubElement = class(TEDISEFElement)
  protected
    function CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject; override;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    function Clone(NewParent: TEDISEFDataObject): TEDISEFSubElement; reintroduce;
  end;

  TEDISEFCompositeElement = class(TEDISEFDataObjectGroup)
  private
    FUserAttribute: string;
    FOrdinal: Integer;
    FOutOfSequenceOrdinal: Boolean;
    FRequirementDesignator: string;
    FRepeatCount: Integer;
    FExtendedData: string;
    FEDISEFTextSets: TEDISEFTextSets;
  protected
    function CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject; override;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    procedure Assign(CompositeElement: TEDISEFCompositeElement);
    function Clone(NewParent: TEDISEFDataObject): TEDISEFCompositeElement; reintroduce;
    function GetElementObjectList: TObjectList;
    procedure AssignElementOrdinals;
    function GetTextSetsLocation: string;
    procedure BindTextSets(TEXTSETS: TEDISEFTextSets);

    function AddSubElement: TEDISEFSubElement;
    function AppendSubElement(SubElement: TEDISEFSubElement): TEDISEFSubElement;
    function ExtractSubElement(SubElement: TEDISEFSubElement): TEDISEFSubElement;
    procedure DeleteSubElement(SubElement: TEDISEFSubElement);
    function InsertSubElement(BeforeObject: TEDISEFDataObject): TEDISEFSubElement; overload;
    function InsertSubElement(SubElement: TEDISEFSubElement;
      BeforeObject: TEDISEFDataObject): TEDISEFSubElement; overload;

    function AddRepeatingPattern: TEDISEFRepeatingPattern;
    function AppendRepeatingPattern(
      RepeatingPattern: TEDISEFRepeatingPattern): TEDISEFRepeatingPattern;
    function ExtractRepeatingPattern(
      RepeatingPattern: TEDISEFRepeatingPattern): TEDISEFRepeatingPattern;
    procedure DeleteRepeatingPattern(
      RepeatingPattern: TEDISEFRepeatingPattern);
    function InsertRepeatingPattern(
      BeforeObject: TEDISEFDataObject): TEDISEFRepeatingPattern; overload;
    function InsertRepeatingPattern(RepeatingPattern: TEDISEFRepeatingPattern;
      BeforeObject: TEDISEFDataObject): TEDISEFRepeatingPattern; overload;

  published
    property UserAttribute: string read FUserAttribute write FUserAttribute;
    property Ordinal: Integer read FOrdinal write FOrdinal;
    property OutOfSequenceOrdinal: Boolean read FOutOfSequenceOrdinal write FOutOfSequenceOrdinal;
    property CompositeElementId: string read FId write FId;
    property RequirementDesignator: string read FRequirementDesignator write FRequirementDesignator;
    property RepeatCount: Integer read FRepeatCount write FRepeatCount;
    property Elements: TEDISEFDataObjectList read FEDISEFDataObjects;
    property TextSetsLocation: string read GetTextSetsLocation;
    //
    property TEXTSETS: TEDISEFTextSets read FEDISEFTextSets;
  end;

  //  EDI SEF Segment
  TEDISEFSegment = class(TEDISEFDataObjectGroup)
  private
    FUserAttribute: string;
    FPosition: Integer;
    FPositionIncrement: Integer;
    FResetPositionInc: Boolean;
    FOrdinal: Integer;
    FOutOfSequenceOrdinal: Boolean;
    FRequirementDesignator: string;
    FMaximumUse: Integer;
    FOwnerLoopId: string;
    FParentLoopId: string;
    FParentSet: TEDISEFSet;
    FParentTable: TEDISEFTable;
    FEDISEFTextSets: TEDISEFTextSets;
    FMaskNumber: Integer;
    FMaskNumberSpecified: Boolean;
    FExtendedData: string;
    function GetOwnerLoopId: string;
    function GetParentLoopId: string;
  protected
    function CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject; override;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    procedure Assign(Segment: TEDISEFSegment);
    function Clone(NewParent: TEDISEFDataObject): TEDISEFSegment; reintroduce;
    function GetElementObjectList: TObjectList;
    procedure AssignElementOrdinals;
    procedure BindElementTextSets;
    function GetTextSetsLocation: string;
    procedure BindTextSets(TEXTSETS: TEDISEFTextSets);

    function AddElement: TEDISEFElement;
    function AppendElement(Element: TEDISEFElement): TEDISEFElement;
    function ExtractElement(Element: TEDISEFElement): TEDISEFElement;
    procedure DeleteElement(Element: TEDISEFElement);
    function InsertElement(BeforeObject: TEDISEFDataObject): TEDISEFElement; overload;
    function InsertElement(Element: TEDISEFElement;
      BeforeObject: TEDISEFDataObject): TEDISEFElement; overload;

    function AddCompositeElement: TEDISEFCompositeElement;
    function AppendCompositeElement(
      CompositeElement: TEDISEFCompositeElement): TEDISEFCompositeElement;
    function ExtractCompositeElement(
      CompositeElement: TEDISEFCompositeElement): TEDISEFCompositeElement;
    procedure DeleteCompositeElement(
      CompositeElement: TEDISEFCompositeElement);
    function InsertCompositeElement(
      BeforeObject: TEDISEFDataObject): TEDISEFCompositeElement; overload;
    function InsertCompositeElement(CompositeElement: TEDISEFCompositeElement;
      BeforeObject: TEDISEFDataObject): TEDISEFCompositeElement; overload;

    function AddRepeatingPattern: TEDISEFRepeatingPattern;
    function AppendRepeatingPattern(
      RepeatingPattern: TEDISEFRepeatingPattern): TEDISEFRepeatingPattern;
    function ExtractRepeatingPattern(
      RepeatingPattern: TEDISEFRepeatingPattern): TEDISEFRepeatingPattern;
    procedure DeleteRepeatingPattern(
      RepeatingPattern: TEDISEFRepeatingPattern);
    function InsertRepeatingPattern(
      BeforeObject: TEDISEFDataObject): TEDISEFRepeatingPattern; overload;
    function InsertRepeatingPattern(RepeatingPattern: TEDISEFRepeatingPattern;
      BeforeObject: TEDISEFDataObject): TEDISEFRepeatingPattern; overload;

  published
    property UserAttribute: string read FUserAttribute write FUserAttribute;
    property Position: Integer read FPosition write FPosition;    
    property PositionIncrement: Integer read FPositionIncrement write FPositionIncrement;
    property ResetPositionInc: Boolean read FResetPositionInc write FResetPositionInc;
    property Ordinal: Integer read FOrdinal write FOrdinal;
    property OutOfSequenceOrdinal: Boolean read FOutOfSequenceOrdinal write FOutOfSequenceOrdinal;
    property SegmentId: string read FId write FId;
    property RequirementDesignator: string read FRequirementDesignator write FRequirementDesignator;
    property MaximumUse: Integer read FMaximumUse write FMaximumUse;
    property Elements: TEDISEFDataObjectList read FEDISEFDataObjects;
    property OwnerLoopId: string read GetOwnerLoopId;
    property ParentLoopId: string read GetParentLoopId;
    property TextSetsLocation: string read GetTextSetsLocation;
    property ParentSet: TEDISEFSet read FParentSet;
    property ParentTable: TEDISEFTable read FParentTable;
    //
    property TEXTSETS: TEDISEFTextSets read FEDISEFTextSets;
  end;

  //  EDI SEF Loop
  TEDISEFLoop = class(TEDISEFDataObjectGroup)
  private
    FMaximumRepeat: Integer;
    function GetParentLoopId: string;
    function GetParentSet: TEDISEFSet;
    function GetParentTable: TEDISEFTable;
  protected
    function CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject; override;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    function Clone(NewParent: TEDISEFDataObject): TEDISEFLoop; reintroduce;

    function AddSegment: TEDISEFSegment;
    function AppendSegment(Segment: TEDISEFSegment): TEDISEFSegment;
    function ExtractSegment(Segment: TEDISEFSegment): TEDISEFSegment;
    procedure DeleteSegment(Segment: TEDISEFSegment);
    function InsertSegment(BeforeObject: TEDISEFDataObject): TEDISEFSegment; overload;
    function InsertSegment(Segment: TEDISEFSegment;
      BeforeObject: TEDISEFDataObject): TEDISEFSegment; overload;

    function AddLoop: TEDISEFLoop;
    function AppendLoop(Loop: TEDISEFLoop): TEDISEFLoop;
    function ExtractLoop(Loop: TEDISEFLoop): TEDISEFLoop;
    procedure DeleteLoop(Loop: TEDISEFLoop);
    function InsertLoop(BeforeObject: TEDISEFDataObject): TEDISEFLoop; overload;
    function InsertLoop(Loop: TEDISEFLoop;
      BeforeObject: TEDISEFDataObject): TEDISEFLoop; overload;

  published
    property LoopId: string read FId write FId;
    property MaximumRepeat: Integer read FMaximumRepeat write FMaximumRepeat;
    property ParentLoopId: string read GetParentLoopId;
    property ParentSet: TEDISEFSet read GetParentSet;
    property ParentTable: TEDISEFTable read GetParentTable;
  end;

  //  EDI SEF Table
  TEDISEFTable = class(TEDISEFDataObjectGroup)
  private
    function GetSEFSet: TEDISEFSet;
  protected
    function CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject; override;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    function Clone(NewParent: TEDISEFDataObject): TEDISEFTable; reintroduce;

    function AddSegment: TEDISEFSegment;
    function AppendSegment(Segment: TEDISEFSegment): TEDISEFSegment;
    function ExtractSegment(Segment: TEDISEFSegment): TEDISEFSegment;
    procedure DeleteSegment(Segment: TEDISEFSegment);
    function InsertSegment(BeforeObject: TEDISEFDataObject): TEDISEFSegment; overload;
    function InsertSegment(Segment: TEDISEFSegment;
      BeforeObject: TEDISEFDataObject): TEDISEFSegment; overload;

    function AddLoop: TEDISEFLoop;
    function AppendLoop(Loop: TEDISEFLoop): TEDISEFLoop;
    function ExtractLoop(Loop: TEDISEFLoop): TEDISEFLoop;
    procedure DeleteLoop(Loop: TEDISEFLoop);
    function InsertLoop(BeforeObject: TEDISEFDataObject): TEDISEFLoop; overload;
    function InsertLoop(Loop: TEDISEFLoop;
      BeforeObject: TEDISEFDataObject): TEDISEFLoop; overload;

  published
    property SEFSet: TEDISEFSet read GetSEFSet;
  end;

  //  EDI SEF Set
  TEDISEFSet = class(TEDISEFDataObjectGroup)
  private
    FEDISEFTextSets: TEDISEFTextSets;
    function GetEDISEFTable(Index: Integer): TEDISEFTable;
    procedure BuildSegmentObjectListFromLoop(ObjectList: TObjectList; Loop: TEDISEFLoop);
  protected
    function CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject; override;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    function Clone(NewParent: TEDISEFDataObject): TEDISEFSet; reintroduce;
    function GetSegmentObjectList: TObjectList;
    procedure AssignSegmentOrdinals;
    procedure AssignSegmentPositions;
    procedure BindSegmentTextSets;
    function GetTextSetsLocation: string;
    procedure BindTextSets(TEXTSETS: TEDISEFTextSets);

    function AddTable: TEDISEFTable;
    function AppendTable(Table: TEDISEFTable): TEDISEFTable;
    function ExtractTable(Table: TEDISEFTable): TEDISEFTable;
    procedure DeleteTable(Table: TEDISEFTable);
    function InsertTable(BeforeTable: TEDISEFTable): TEDISEFTable; overload;
    function InsertTable(Table, BeforeTable: TEDISEFTable): TEDISEFTable; overload;

    property Table[Index: Integer]: TEDISEFTable read GetEDISEFTable;
  published
    property Tables: TEDISEFDataObjectList read FEDISEFDataObjects;
    property TextSetsLocation: string read GetTextSetsLocation;
    //
    property TEXTSETS: TEDISEFTextSets read FEDISEFTextSets;
  end;

  //  EDI SEF File
  TEDISEFFile = class(TEDISEFDataObject)
  private
    FFileName: string;
    FEDISEFTextSets: TEDISEFTextSets;
    FEDISEFCodesList: TStringList;
    FEDISEFElms: TEDISEFDataObjectList;
    FEDISEFComs: TEDISEFDataObjectList;
    FEDISEFSegs: TEDISEFDataObjectList;
    FEDISEFSets: TEDISEFDataObjectList;
    FEDISEFStd: TStringList;
    FEDISEFIni: TStringList;
    FEDISEFVer: string;
    procedure ParseTextSets;
    procedure ParseCodes;
    procedure ParseELMS;
    procedure ParseCOMS;
    procedure ParseSEGS;
    procedure ParseSETS;
    procedure ParseSTD;
    procedure ParseINI;
    procedure ParseVER;
    // EDI JCL SEF Extensions
    //procedure ParseELMSExt;
    //procedure ParseCOMSExt;
    //procedure ParseSEGSExt;
    //procedure ParseSETSExt;
    function GetEDISEFCodesList: TStrings;
    function GetEDISEFStd: TStrings;
    function GetEDISEFIni: TStrings;
  protected
    function CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject; override;
  public
    constructor Create(Parent: TEDISEFDataObject); reintroduce;
    destructor Destroy; override;

    procedure LoadFromFile; overload;
    procedure LoadFromFile(const FileName: string); overload;
    procedure SaveToFile; overload;
    procedure SaveToFile(const FileName: string); overload;
    procedure Unload;

    function Assemble: string; override;
    procedure Disassemble; override;

    function Clone(NewParent: TEDISEFDataObject): TEDISEFFile; reintroduce;
  published
    property FileName: string read FFileName write FFileName;
    property Codes: TStrings read GetEDISEFCodesList;
    property ELMS: TEDISEFDataObjectList read FEDISEFElms;
    property COMS: TEDISEFDataObjectList read FEDISEFComs;
    property SEGS: TEDISEFDataObjectList read FEDISEFSegs;
    property SETS: TEDISEFDataObjectList read FEDISEFSets;
    property STD: TStrings read GetEDISEFStd;
    property INI: TStrings read GetEDISEFIni;
    property VER: string read FEDISEFVer write FEDISEFVer;
    //
    property TEXTSETS: TEDISEFTextSets read FEDISEFTextSets;
  end;

//  Procedures
function GetEDISEFUserAttributeDescription(
  Attribute: TEDISEFComsUserAttributes): string; overload;
function GetEDISEFUserAttributeDescription(Attribute: string): string; overload;

procedure ParseELMSDataOfELMSDefinition(Data: string; Element: TEDISEFElement);
function CombineELMSDataOfELMSDefinition(Element: TEDISEFElement): string;
procedure ParseELMSDataOfCOMSDefinition(Data: string; Element: TEDISEFElement;
  ELMSList: TEDISEFDataObjectList);
procedure ParseELMSDataOfSEGSDefinition(Data: string; Element: TEDISEFElement;
  ELMSList: TEDISEFDataObjectList);
function CombineELMSDataOfCOMSorSEGSDefinition(Element: TEDISEFElement;
  ELMSList: TEDISEFDataObjectList): string;

procedure ParseCOMSDataOfCOMSDefinition(Data: string; CompositeElement: TEDISEFCompositeElement;
  ELMSList: TEDISEFDataObjectList);
function CombineCOMSDataOfCOMSDefinition(CompositeElement: TEDISEFCompositeElement): string;
procedure ParseCOMSDataOfSEGSDefinition(Data: string; CompositeElement: TEDISEFCompositeElement;
  COMSList: TEDISEFDataObjectList);
function CombineCOMSDataOfSEGSDefinition(CompositeElement: TEDISEFCompositeElement): string;

procedure ParseSEGSDataOfSEGSDefinition(Data: string; Segment: TEDISEFSegment;
  SEFFile: TEDISEFFile);
function CombineSEGSDataOfSEGSDefinition(Segment: TEDISEFSegment): string;
procedure ParseSEGSDataOfSETSDefinition(Data: string; Segment: TEDISEFSegment;
  SEFFile: TEDISEFFile);
function CombineSEGSDataOfSETSDefinition(Segment: TEDISEFSegment): string;

procedure ParseLoopDataOfSETSDefinition(Data: string; Loop: TEDISEFLoop;
  SEFFile: TEDISEFFile);
procedure ParseTableDataOfSETSDefinition(Data: string; Table: TEDISEFTable;
  SEFFile: TEDISEFFile);
procedure ParseSetsDataOfSETSDefinition(Data: string; ASet: TEDISEFSet; SEFFile: TEDISEFFile);

procedure ExtractFromDataObjectGroup(DataObjectClass: TEDISEFDataObjectClass;
  DataObjectGroup: TEDISEFDataObjectGroup; ObjectList: TObjectList); overload;

procedure ExtractFromDataObjectGroup(DataObjectClasses: array of TEDISEFDataObjectClass;
  DataObjectGroup: TEDISEFDataObjectGroup; ObjectList: TObjectList); overload;

function AddSubElementTo(DataObjectGroup: TEDISEFDataObjectGroup): TEDISEFSubElement;
function AppendSubElementTo(DataObjectGroup: TEDISEFDataObjectGroup;
  SubElement: TEDISEFSubElement): TEDISEFSubElement;
function ExtractSubElementFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  SubElement: TEDISEFSubElement): TEDISEFSubElement;
procedure DeleteSubElementFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  SubElement: TEDISEFSubElement);
function InsertSubElementInto(DataObjectGroup: TEDISEFDataObjectGroup;
  BeforeObject: TEDISEFDataObject): TEDISEFSubElement; overload;
function InsertSubElementInto(DataObjectGroup: TEDISEFDataObjectGroup;
  SubElement: TEDISEFSubElement; BeforeObject: TEDISEFDataObject): TEDISEFSubElement; overload;

function AddElementTo(DataObjectGroup: TEDISEFDataObjectGroup): TEDISEFElement;
function AppendElementTo(DataObjectGroup: TEDISEFDataObjectGroup;
  Element: TEDISEFElement): TEDISEFElement;
function ExtractElementFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  Element: TEDISEFElement): TEDISEFElement;
procedure DeleteElementFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  Element: TEDISEFElement);
function InsertElementInto(DataObjectGroup: TEDISEFDataObjectGroup;
  BeforeObject: TEDISEFDataObject): TEDISEFElement; overload;
function InsertElementInto(DataObjectGroup: TEDISEFDataObjectGroup;
  Element: TEDISEFElement; BeforeObject: TEDISEFDataObject): TEDISEFElement; overload;

function AddRepeatingPatternTo(DataObjectGroup: TEDISEFDataObjectGroup): TEDISEFRepeatingPattern;
function AppendRepeatingPatternTo(DataObjectGroup: TEDISEFDataObjectGroup;
  RepeatingPattern: TEDISEFRepeatingPattern): TEDISEFRepeatingPattern;
function ExtractRepeatingPatternFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  RepeatingPattern: TEDISEFRepeatingPattern): TEDISEFRepeatingPattern;
procedure DeleteRepeatingPatternFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  RepeatingPattern: TEDISEFRepeatingPattern);
function InsertRepeatingPatternInto(DataObjectGroup: TEDISEFDataObjectGroup;
  BeforeObject: TEDISEFDataObject): TEDISEFRepeatingPattern; overload;
function InsertRepeatingPatternInto(DataObjectGroup: TEDISEFDataObjectGroup;
  RepeatingPattern: TEDISEFRepeatingPattern;
  BeforeObject: TEDISEFDataObject): TEDISEFRepeatingPattern; overload;

function AddCompositeElementTo(DataObjectGroup: TEDISEFDataObjectGroup): TEDISEFCompositeElement;
function AppendCompositeElementTo(DataObjectGroup: TEDISEFDataObjectGroup;
  CompositeElement: TEDISEFCompositeElement): TEDISEFCompositeElement;
function ExtractCompositeElementFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  CompositeElement: TEDISEFCompositeElement): TEDISEFCompositeElement;
procedure DeleteCompositeElementFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  CompositeElement: TEDISEFCompositeElement);
function InsertCompositeElementInto(DataObjectGroup: TEDISEFDataObjectGroup;
  BeforeObject: TEDISEFDataObject): TEDISEFCompositeElement; overload;
function InsertCompositeElementInto(DataObjectGroup: TEDISEFDataObjectGroup;
  CompositeElement: TEDISEFCompositeElement;
  BeforeObject: TEDISEFDataObject): TEDISEFCompositeElement; overload;

function AddSegmentTo(DataObjectGroup: TEDISEFDataObjectGroup): TEDISEFSegment;
function AppendSegmentTo(DataObjectGroup: TEDISEFDataObjectGroup;
  Segment: TEDISEFSegment): TEDISEFSegment;
function ExtractSegmentFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  Segment: TEDISEFSegment): TEDISEFSegment;
procedure DeleteSegmentFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  Segment: TEDISEFSegment);
function InsertSegmentInto(DataObjectGroup: TEDISEFDataObjectGroup;
  BeforeObject: TEDISEFDataObject): TEDISEFSegment; overload;
function InsertSegmentInto(DataObjectGroup: TEDISEFDataObjectGroup;
  Segment: TEDISEFSegment; BeforeObject: TEDISEFDataObject): TEDISEFSegment; overload;

function AddLoopTo(DataObjectGroup: TEDISEFDataObjectGroup): TEDISEFLoop;
function AppendLoopTo(DataObjectGroup: TEDISEFDataObjectGroup;
  Loop: TEDISEFLoop): TEDISEFLoop;
function ExtractLoopFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  Loop: TEDISEFLoop): TEDISEFLoop;
procedure DeleteLoopFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  Loop: TEDISEFLoop);
function InsertLoopInto(DataObjectGroup: TEDISEFDataObjectGroup;
  BeforeObject: TEDISEFDataObject): TEDISEFLoop; overload;
function InsertLoopInto(DataObjectGroup: TEDISEFDataObjectGroup;
  Loop: TEDISEFLoop; BeforeObject: TEDISEFDataObject): TEDISEFLoop; overload;

function AddTableTo(DataObjectGroup: TEDISEFDataObjectGroup): TEDISEFTable;
function AppendTableTo(DataObjectGroup: TEDISEFDataObjectGroup;
  Table: TEDISEFTable): TEDISEFTable;
function ExtractTableFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  Table: TEDISEFTable): TEDISEFTable;
procedure DeleteTableFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  Table: TEDISEFTable);
function InsertTableInto(DataObjectGroup: TEDISEFDataObjectGroup;
  BeforeObject: TEDISEFDataObject): TEDISEFTable; overload;
function InsertTableInto(DataObjectGroup: TEDISEFDataObjectGroup;
  Table: TEDISEFTable; BeforeObject: TEDISEFDataObject): TEDISEFTable; overload;

{$IFNDEF EDI_WEAK_PACKAGE_UNITS}
{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclEDISEF.pas $';
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
  Value_Optional = 'O';
  Value_Conditional = 'C';
  Value_One = '1';
  Value_GreaterThanOne = '>1';
  Value_Version10 = '1.0';
  Value_QuestionMark = '?';

  SEFDelimiter_EqualSign = '=';
  SEFDelimiter_OpeningBrace = '{';
  SEFDelimiter_ClosingBrace = '}';
  SEFDelimiter_OpeningBracket = '[';
  SEFDelimiter_ClosingBracket = ']';
  SEFDelimiter_AtSign = '@';
  SEFDelimiter_SemiColon = ';';
  SEFDelimiter_Colon = ':';
  SEFDelimiter_Comma = ',';
  SEFDelimiter_Period = '.';
  SEFDelimiter_Caret = '^';
  SEFDelimiter_PlusSign = '+';
  SEFDelimiter_MinusSign = '-';
  SEFDelimiter_Asterisk = '*';

//  Procedures
function GetEDISEFUserAttributeDescription(Attribute: TEDISEFComsUserAttributes): string;
begin
  case Attribute of
    caPeriod:
      Result := EDISEFUserAttributePeriodDesc;
    caExclamationPoint:
      Result := EDISEFUserAttributeExclamationPointDesc;
    caDollarSign:
      Result := EDISEFUserAttributeDollarSignDesc;
    caHyphen:
      Result := EDISEFUserAttributeHyphenDesc;
    caAmpersand:
      Result := EDISEFUserAttributeAmpersandDesc;
  else
    Result := RsUnknownAttribute;
  end;
end;

function GetEDISEFUserAttributeDescription(Attribute: string): string;
begin
  if Attribute = '' then
    Attribute := Value_QuestionMark;
  case Attribute[1] of
    EDISEFUserAttributePeriod:
      Result := EDISEFUserAttributePeriodDesc;
    EDISEFUserAttributeExclamationPoint:
      Result := EDISEFUserAttributeExclamationPointDesc;
    EDISEFUserAttributeDollarSign:
      Result := EDISEFUserAttributeDollarSignDesc;
    EDISEFUserAttributeHyphen:
      Result := EDISEFUserAttributeHyphenDesc;
    EDISEFUserAttributeAmpersand:
      Result := EDISEFUserAttributeAmpersandDesc;
  else
    Result := RsUnknownAttribute;
  end;
end;

procedure ParseELMSDataOfELMSDefinition(Data: string; Element: TEDISEFElement);
var
  Temp: TStringList;
begin
  // Clear any old values
  Element.UserAttribute := '';
  Element.Ordinal := -1;
  Element.ElementType := '';
  Element.MinimumLength := 0;
  Element.MaximumLength := 0;
  Element.RequirementDesignator := Value_Optional;
  Element.RepeatCount := 1;
  //
  Temp := TStringList.Create;
  try
    Temp.Text := Data;
    Element.Id := Temp.Names[0];
    {$IFDEF COMPILER7_UP}
    Temp.CommaText := Temp.ValueFromIndex[0];
    {$ELSE}
    Temp.CommaText := Temp.Values[Element.Id];
    {$ENDIF COMPILER7_UP}
    if Temp.Count >= 1 then
      Element.ElementType := Temp[0];
    if Temp.Count >= 2 then
      Element.MinimumLength := StrToInt(Temp[1]);
    if Temp.Count >= 3 then
      Element.MaximumLength := StrToInt(Temp[2]);
  finally
    Temp.Free;
  end;
end;

function CombineELMSDataOfELMSDefinition(Element: TEDISEFElement): string;
begin
  Result := Element.Id + SEFDelimiter_EqualSign + Element.ElementType + SEFDelimiter_Comma +
    IntToStr(Element.MinimumLength) + SEFDelimiter_Comma + IntToStr(Element.MaximumLength);
end;

procedure ParseELMSDataOfCOMSDefinition(Data: string; Element: TEDISEFElement;
  ELMSList: TEDISEFDataObjectList);
var
  I, J, K, L, M, N, O, P: Integer;
  ListItem: TEDISEFDataObjectListItem;
begin
  // Clear any old values
  Element.UserAttribute := '';
  Element.Ordinal := -1;
  Element.ElementType := '';
  Element.MinimumLength := 0;
  Element.MaximumLength := 0;
  Element.RequirementDesignator := Value_Optional;
  Element.RepeatCount := 1;
  // Parse User Attribute
  if Data[1] in EDISEFUserAttributeSet then
  begin
    Element.UserAttribute := Data[1];
    I := 2;
  end
  else
    I := 1;
  // Get delimiter locations
  J := StrSearch(SEFDelimiter_AtSign, Data, 1);
  K := StrSearch(SEFDelimiter_SemiColon, Data, 1);
  L := StrSearch(SEFDelimiter_Colon, Data, 1);
  M := StrSearch(SEFDelimiter_Comma, Data, 1);
  N := StrSearch(SEFDelimiter_Comma, Data, M + 1); 
  P := Length(Data) + 1;
  // Parse Id using the closest delimiter
  O := P;
  if J <> 0 then
    if O > J then
      O := J;
  if K <> 0 then
    if O > K then
      O := K;
  if L <> 0 then
    if O > L then
      O := L;
  if M <> 0 then
    if O > M then
      O := M;
  Element.Id := Copy(Data, I, O - I);
  // Get Default Values
  if ELMSList <> nil then
  begin
    ListItem := ELMSList.FindItemByName(Element.Id);
    if ListItem <> nil then
      Element.Assign(TEDISEFElement(ListItem.EDISEFDataObject));
  end;
  // Parse other attributes
  if J <> 0 then
  begin
    Inc(J);
    O := P;
    if K <> 0 then
      if O > K then
        O := K;
    if L <> 0 then
      if O > L then
        O := L;
    if M <> 0 then
      if O > M then
        O := M;
    if (O - J) > 0 then
      Element.Ordinal := StrToInt(Copy(Data, J, O - J));
  end;
  if K <> 0 then
  begin
    Inc(K);
    O := P;
    if L <> 0 then
      if O > L then
        O := L;
    if M <> 0 then
      if O > M then
        O := M;
    if (O - K) > 0 then
      Element.MinimumLength := StrToInt(Copy(Data, K, O - K));
  end;
  if L <> 0 then
  begin
    Inc(L);
    O := P;
    if M <> 0 then
      if O > M then
        O := M;
    if (O - L) > 0 then
      Element.MaximumLength := StrToInt(Copy(Data, L, O - L));
  end;
  if M <> 0 then
  begin
    Inc(M);
    O := P;
    if N <> 0 then
      if O > N then
        O := N;
    if (O - M) > 0 then
      Element.RequirementDesignator := Copy(Data, M, O - M);
  end;
  if N <> 0 then
  begin
    Inc(N);
    if (P - N) > 0 then
      Element.RepeatCount := StrToInt(Copy(Data, N, 1));
  end;
end;

function CombineELMSDataOfCOMSorSEGSDefinition(Element: TEDISEFElement;
  ELMSList: TEDISEFDataObjectList): string;
var
  CompareElement: TEDISEFElement;
  ListItem: TEDISEFDataObjectListItem;
begin
  if Element.UserAttribute <> '' then
    Result := Result + Element.UserAttribute;
  Result := Result + Element.Id;
  if Element.OutOfSequenceOrdinal then
    Result := Result + SEFDelimiter_AtSign + IntToStr(Element.Ordinal);
  // Get Default Values
  CompareElement := nil;
  if ELMSList <> nil then
  begin
    ListItem := ELMSList.FindItemByName(Element.Id);
    if ListItem <> nil then
      CompareElement := TEDISEFElement(ListItem.EDISEFDataObject);
  end;
  // Test for changes in default values
  if CompareElement <> nil then
  begin
    if (CompareElement.MinimumLength <> Element.MinimumLength) or
      (CompareElement.MaximumLength <> Element.MaximumLength) then
    begin
      Result := Result + SEFDelimiter_SemiColon;
      if CompareElement.MinimumLength <> Element.MinimumLength then
        Result := Result + IntToStr(Element.MinimumLength);
      Result := Result + SEFDelimiter_Colon;
      if CompareElement.MaximumLength <> Element.MaximumLength then
        Result := Result + IntToStr(Element.MaximumLength);
    end;
  end
  else
  begin
    Result := Result + SEFDelimiter_SemiColon;
    Result := Result + IntToStr(Element.MinimumLength);
    Result := Result + SEFDelimiter_Colon;
    Result := Result + IntToStr(Element.MaximumLength);
  end;
  if (Element.RequirementDesignator <> '') and
    (Element.RequirementDesignator <> Value_Optional) then
    Result := Result + SEFDelimiter_Comma + Element.RequirementDesignator;
  if Element.RepeatCount > 1 then
  begin
    if (Element.RequirementDesignator = '') or
      (Element.RequirementDesignator = Value_Optional) then
      Result := Result + SEFDelimiter_Comma;
    Result := Result + SEFDelimiter_Comma + IntToStr(Element.RepeatCount);
  end;
end;

procedure ParseELMSDataOfSEGSDefinition(Data: string; Element: TEDISEFElement;
  ELMSList: TEDISEFDataObjectList);
var
  I, J, K, L, M, N, O, P: Integer;
  ListItem: TEDISEFDataObjectListItem;
begin
  // Clear any old values
  Element.UserAttribute := '';
  Element.Ordinal := -1;
  Element.ElementType := '';
  Element.MinimumLength := 0;
  Element.MaximumLength := 0;
  Element.RequirementDesignator := Value_Optional;
  Element.RepeatCount := 1;
  // Parse User Attribute
  if Data[1] in EDISEFUserAttributeSet then
  begin
    Element.UserAttribute := Data[1];
    I := 2;
  end
  else
    I := 1;
  // Get delimiter locations
  J := StrSearch(SEFDelimiter_AtSign, Data, 1);
  K := StrSearch(SEFDelimiter_SemiColon, Data, 1);
  L := StrSearch(SEFDelimiter_Colon, Data, 1);
  M := StrSearch(SEFDelimiter_Comma, Data, 1);
  N := StrSearch(SEFDelimiter_Comma, Data, M + 1);
  P := Length(Data) + 1;
  // Parse Id
  O := P;
  if J <> 0 then
    if O > J then
      O := J;
  if K <> 0 then
    if O > K then
      O := K;
  if L <> 0 then
    if O > L then
      O := L;
  if M <> 0 then
    if O > M then
      O := M;
  Element.Id := Copy(Data, I, O - I);
  // Get Default Values
  if ELMSList <> nil then
  begin
    ListItem := ELMSList.FindItemByName(Element.Id);
    if ListItem <> nil then
      Element.Assign(TEDISEFElement(ListItem.EDISEFDataObject));
  end;
  // Parse other attributes
  if J <> 0 then
  begin
    Inc(J);                                       
    O := P; 
    if K <> 0 then
      if O > K then
        O := K;
    if L <> 0 then
      if O > L then
        O := L;
    if M <> 0 then
      if O > M then
        O := M;
    if (O - J) > 0 then
      Element.Ordinal := StrToInt(Copy(Data, J, O - J));
  end;
  if K <> 0 then
  begin
    Inc(K);
    O := P;
    if L <> 0 then
      if O > L then
        O := L;
    if M <> 0 then
      if O > M then
        O := M;
    if (O - K) > 0 then
      Element.MinimumLength := StrToInt(Copy(Data, K, O - K));
  end;
  if L <> 0 then
  begin
    Inc(L);
    O := P;
    if M <> 0 then
      if O > M then
        O := M;
    if (O - L) > 0 then
      Element.MaximumLength := StrToInt(Copy(Data, L, O - L));
  end;
  if M <> 0 then
  begin
    Inc(M);
    O := P;
    if N <> 0 then
      if O > N then
        O := N;
    if (O - M) > 0 then
      Element.RequirementDesignator := Copy(Data, M, O - M);
  end;
  if N <> 0 then
  begin
    Inc(N);
    if (P - N) > 0 then
      Element.RepeatCount := StrToInt(Copy(Data, N, 1));
  end;
end;

// Parse TEDISEFCompositeElement or Repeating Pattern in TEDISEFCompositeElement

procedure InternalParseCOMSDataOfCOMSDefinition(Data: string; Element: TEDISEFDataObjectGroup;
  ELMSList: TEDISEFDataObjectList);
var
  I, J, K, L, M, N: Integer;
  RepeatCount: Integer;
  RepeatData: string;
  SubElement: TEDISEFSubElement;
  RepeatingPattern: TEDISEFRepeatingPattern;
begin
  I := StrSearch(SEFDelimiter_EqualSign, Data, 1);
  if (I > 0) and (Element is TEDISEFCompositeElement) then
  begin
    Element.EDISEFDataObjects.Clear;
    Element.Id := Copy(Data, 1, I - 1);
  end;
  Inc(I);
  M := I;
  while I > 0 do
  begin
    // Start search
    I := StrSearch(SEFDelimiter_OpeningBracket, Data, M);
    if I = 0 then
      Break;
    J := StrSearch(SEFDelimiter_ClosingBracket, Data, M);
    K := StrSearch(SEFDelimiter_OpeningBrace, Data, M);
    L := StrSearch(SEFDelimiter_ClosingBrace, Data, M);
    // Determine if data block to process is a repetition
    if (I < K) or (K = 0) then // Normal data block
    begin
      SubElement := AddSubElementTo(Element);
      SubElement.Data := Copy(Data, I + 1, (J - I) - 1);
      SubElement.Disassemble;
      M := J + 1;
    end
    else // Repeating data block (K = 1 on the first pass)
    begin
      // Get repeat count
      N := StrSearch(SEFDelimiter_OpeningBracket, Data, K);
      RepeatCount := StrToInt(Copy(Data, K + 1, (N - K) - 1));
      // Correct start position
      K := StrSearch(SEFDelimiter_OpeningBracket, Data, K);
      // Validate end position
      N := StrSearch(SEFDelimiter_OpeningBrace, Data, K + 1);
      while (N <> 0) and (N < L) do // Detect nested repetition
      begin
        N := StrSearch(SEFDelimiter_OpeningBrace, Data, N + 1); // Search for nested repetition
        L := StrSearch(SEFDelimiter_ClosingBrace, Data, L + 1); // Correct end position
      end;
      // Copy data for repetition
      RepeatData := Copy(Data, K, L - K);
      M := L + 1;
      // Handle Repeating Data Block
      RepeatingPattern := AddRepeatingPatternTo(Element);
      RepeatingPattern.RepeatCount := RepeatCount;
      RepeatingPattern.Data := RepeatData;
      RepeatingPattern.Disassemble;
    end;
  end;
  // Store Currently Unused Data
  if (M <= Length(Data)) and (Element is TEDISEFCompositeElement) then
    TEDISEFCompositeElement(Element).FExtendedData := Copy(Data, M, (Length(Data) - M) + 1);
end;

procedure ParseCOMSDataOfCOMSDefinition(Data: string; CompositeElement: TEDISEFCompositeElement;
  ELMSList: TEDISEFDataObjectList);
begin
  InternalParseCOMSDataOfCOMSDefinition(Data, CompositeElement, ELMSList);
end;

function CombineCOMSDataOfCOMSDefinition(CompositeElement: TEDISEFCompositeElement): string;
var
  ListItem: TEDISEFDataObjectListItem;
begin
  Result := CompositeElement.Id + SEFDelimiter_EqualSign;
  ListItem := CompositeElement.EDISEFDataObjects.First;
  while ListItem <> nil do
  begin
    if not (ListItem.EDIObject is TEDISEFRepeatingPattern) then
      Result := Result + SEFDelimiter_OpeningBracket + ListItem.EDISEFDataObject.Assemble +
        SEFDelimiter_ClosingBracket
    else
      Result := Result + SEFDelimiter_OpeningBrace + ListItem.EDISEFDataObject.Assemble +
        SEFDelimiter_ClosingBrace;
    ListItem := ListItem.NextItem;
  end;
  Result := Result + CompositeElement.FExtendedData;
end;

procedure ParseCOMSDataOfSEGSDefinition(Data: string; CompositeElement: TEDISEFCompositeElement;
  COMSList: TEDISEFDataObjectList);
var
  Temp: TStringList;
  ListItem: TEDISEFDataObjectListItem;
  DefaultCompositeElement: TEDISEFCompositeElement;
begin
  CompositeElement.EDISEFDataObjects.Clear;
  Temp := TStringList.Create;
  try
    Temp.CommaText := Data;
    if Temp.Count >= 1 then
      CompositeElement.Id := Temp[0];
    ListItem := COMSList.FindItemByName(CompositeElement.Id);
    if (ListItem <> nil) and (ListItem.EDISEFDataObject <> nil) then
    begin
      DefaultCompositeElement := TEDISEFCompositeElement(ListItem.EDISEFDataObject);
      CompositeElement.Assign(DefaultCompositeElement);
    end;
    if Temp.Count >= 2 then
      CompositeElement.RequirementDesignator := Temp[1];
  finally
    Temp.Free;
  end;
end;

function CombineCOMSDataOfSEGSDefinition(CompositeElement: TEDISEFCompositeElement): string;
begin
  if CompositeElement.UserAttribute <> '' then
    Result := Result + CompositeElement.UserAttribute;
  Result := Result + CompositeElement.Id;
  if CompositeElement.OutOfSequenceOrdinal then
    Result := Result + SEFDelimiter_AtSign + IntToStr(CompositeElement.Ordinal);
  if (CompositeElement.RequirementDesignator <> '') and
    (CompositeElement.RequirementDesignator <> Value_Optional) then
  begin
    Result := Result + SEFDelimiter_Comma + CompositeElement.RequirementDesignator;
  end;
end;

// Parse TEDISEFSegment or Repeating Pattern in TEDISEFSegment

procedure InternalParseSEGSDataOfSEGSDefinition(Data: string; Segment: TEDISEFDataObjectGroup;
  SEFFile: TEDISEFFile);
var
  I, J, K, L, M, N: Integer;
  ElementData: string;
  RepeatCount: Integer;
  RepeatData: string;
  Element: TEDISEFElement;
  CompositeElement: TEDISEFCompositeElement;
  RepeatingPattern: TEDISEFRepeatingPattern;
begin
  I := StrSearch(SEFDelimiter_EqualSign, Data, 1);
  if (I > 0) and (Segment is TEDISEFSegment) then
  begin
    Segment.EDISEFDataObjects.Clear;
    Segment.Id := Copy(Data, 1, I - 1);
    TEDISEFSegment(Segment).RequirementDesignator := Value_Optional;
    TEDISEFSegment(Segment).MaximumUse := 1;
  end;
  Inc(I);
  M := I;
  while I > 0 do
  begin
    // Start search
    I := StrSearch(SEFDelimiter_OpeningBracket, Data, M);
    if I = 0 then
      Break;
    J := StrSearch(SEFDelimiter_ClosingBracket, Data, M);
    K := StrSearch(SEFDelimiter_OpeningBrace, Data, M);
    L := StrSearch(SEFDelimiter_ClosingBrace, Data, M);
    // Determine if data block to process is a repetition
    if (I < K) or (K = 0) then // Normal data block
    begin
      ElementData := Copy(Data, I + 1, (J - I) - 1);
      if ElementData[1] = Value_Conditional then
      begin
        CompositeElement := AddCompositeElementTo(Segment);
        CompositeElement.SEFFile := SEFFile;
        CompositeElement.Data := ElementData;
        CompositeElement.Disassemble;
      end
      else
      begin
        Element := AddElementTo(Segment);
        Element.SEFFile := SEFFile;
        Element.Data := ElementData;
        Element.Disassemble;
      end;
      M := J + 1;
    end
    else // Repeating data block (K = 1 on the first pass)
    begin
      // Get repeat count
      N := StrSearch(SEFDelimiter_OpeningBracket, Data, K);
      RepeatCount := StrToInt(Copy(Data, K + 1, (N - K) - 1));
      // Correct start position
      K := StrSearch(SEFDelimiter_OpeningBracket, Data, K);
      // Validate end position
      N := StrSearch(SEFDelimiter_OpeningBrace, Data, K + 1);
      while (N <> 0) and (N < L) do // Detect nested repetition
      begin
        N := StrSearch(SEFDelimiter_OpeningBrace, Data, N + 1); // Search for nested repetition
        L := StrSearch(SEFDelimiter_ClosingBrace, Data, L + 1); // Correct end position
      end;
      // Copy data for repetition
      RepeatData := Copy(Data, K, L - K);
      M := L + 1;
      // Handle Repeating Data Block
      RepeatingPattern := AddRepeatingPatternTo(Segment);
      RepeatingPattern.RepeatCount := RepeatCount;
      RepeatingPattern.Data := RepeatData;
      RepeatingPattern.Disassemble;
    end;
  end;
  // Store Currently Unused Data
  if (M <= Length(Data)) and (Segment is TEDISEFSegment) then
    TEDISEFSegment(Segment).FExtendedData := Copy(Data, M, (Length(Data) - M) + 1);
end;

procedure ParseSEGSDataOfSEGSDefinition(Data: string; Segment: TEDISEFSegment;
  SEFFile: TEDISEFFile);
begin
  InternalParseSEGSDataOfSEGSDefinition(Data, Segment, SEFFile);
end;

function CombineSEGSDataOfSEGSDefinition(Segment: TEDISEFSegment): string;
var
  ListItem: TEDISEFDataObjectListItem;
begin
  Result := Segment.Id + SEFDelimiter_EqualSign;
  ListItem := Segment.EDISEFDataObjects.First;
  while ListItem <> nil do
  begin
    if not (ListItem.EDISEFDataObject is TEDISEFRepeatingPattern) then
      Result := Result + SEFDelimiter_OpeningBracket + ListItem.EDISEFDataObject.Assemble +
        SEFDelimiter_ClosingBracket
    else
      Result := Result + SEFDelimiter_OpeningBrace + ListItem.EDISEFDataObject.Assemble +
        SEFDelimiter_ClosingBrace;
    ListItem := ListItem.NextItem;
  end;
  Result := Result + Segment.FExtendedData;
end;

procedure ParseSEGSDataOfSETSDefinition(Data: string; Segment: TEDISEFSegment;
  SEFFile: TEDISEFFile);

  {$IFDEF CLR}
  function ToPChar(const S: string): string;
  var
    I: Integer;
  begin
    for I := 1 to Length(S) do
      if S[I] = #0 then
      begin
        Result := Copy(S, 1, I - 1);
        Exit;
      end;
    Result := S;
  end;
  {$ELSE}
  function ToPChar(const S: string): PChar;
  begin
    Result := PChar(S);
  end;
  {$ENDIF CLR}

var
  Temp: TStringList;
  ListItem: TEDISEFDataObjectListItem;
  SegmentDef: TEDISEFSegment;
  I, J, K: Integer;
begin
  Segment.UserAttribute := '';
  Segment.Ordinal := -1;
  Segment.RequirementDesignator := Value_Optional;
  Segment.MaximumUse := 1;
  Segment.EDISEFDataObjects.Clear;
  Temp := TStringList.Create;
  try
    Temp.CommaText := Data;
    if Temp.Count >= 1 then
    begin
      I := 1;
      // Parse User Attribute
      if Temp[0][1] in EDISEFUserAttributeSet then
      begin
        Segment.UserAttribute := Temp[0][1];
        I := 2;
      end;
      J := StrSearch(SEFDelimiter_Asterisk, Temp[0], 1);
      K := StrSearch(SEFDelimiter_AtSign, Temp[0], 1);
      // Parse Mask Number
      if J <> 0 then
      begin
        Segment.FMaskNumberSpecified := True;
        if K = 0 then
          Segment.FMaskNumber := StrToInt(Copy(ToPChar(Temp[0]), J + 1, Length(ToPChar(Temp[0])) - J))
        else
          Segment.FMaskNumber := StrToInt(Copy(ToPChar(Temp[0]), J + 1, (K - J) - 1));
      end;
      // Parse Explicitly Assigned Ordinal
      if K <> 0 then
        Segment.Ordinal := StrToInt(Copy(ToPChar(Temp[0]), K + 1, Length(ToPChar(Temp[0])) - K));
      // Parse Segment Id
      if (J = 0) and (K = 0) then
      begin
        if I = 1 then
          Segment.Id := Temp[0]
        else // Had to cast Temp[0] as PChar here because of a bug during runtime
          Segment.Id := Copy(ToPChar(Temp[0]), I, Length(ToPChar(Temp[0])) - 1);
      end
      else
      begin
        K := Length(ToPChar(Temp[0])) - 1;
        if (J < K) and (J <> 0) then
          K := J;
        Segment.Id := Copy(ToPChar(Temp[0]), I, K - I);
      end;
    end;
    ListItem := SEFFile.SEGS.FindItemByName(Segment.Id);
    if (ListItem <> nil) and (ListItem.EDISEFDataObject <> nil) then
    begin
      SegmentDef := TEDISEFSegment(ListItem.EDISEFDataObject);
      Segment.Assign(SegmentDef);
    end;
    if Temp.Count >= 2 then
    begin
      if Temp[1] = '' then
        Temp[1] := Value_Optional;
      Segment.RequirementDesignator := Temp[1];
    end;
    if Temp.Count >= 3 then
    begin
      if Temp[2] = Value_GreaterThanOne then
        Temp[2] := IntToStr(Value_UndefinedMaximum);
      Segment.MaximumUse := StrToInt(Temp[2]);
    end;
  finally
    Temp.Free;
  end;
end;

function CombineSEGSDataOfSETSDefinition(Segment: TEDISEFSegment): string;
begin
  if Segment.UserAttribute <> '' then
    Result := Result + Segment.UserAttribute;
  Result := Result + Segment.Id;
  if Segment.FMaskNumberSpecified then
    Result := Result + SEFDelimiter_Asterisk + IntToStr(Segment.FMaskNumber);
  if Segment.OutOfSequenceOrdinal then
    Result := Result + SEFDelimiter_AtSign + IntToStr(Segment.Ordinal);
  if (Segment.RequirementDesignator <> '') and
    (Segment.RequirementDesignator <> Value_Optional) then
    Result := Result + SEFDelimiter_Comma + Segment.RequirementDesignator;
  if Segment.MaximumUse > 1 then
  begin
    if (Segment.RequirementDesignator = '') or
      (Segment.RequirementDesignator = Value_Optional) then
      Result := Result + SEFDelimiter_Comma;
    if Segment.MaximumUse >= Value_UndefinedMaximum then    
      Result := Result + SEFDelimiter_Comma + Value_GreaterThanOne
    else
      Result := Result + SEFDelimiter_Comma + IntToStr(Segment.MaximumUse);
  end;
end;

procedure ParseLoopDataOfSETSDefinition(Data: string; Loop: TEDISEFLoop;
  SEFFile: TEDISEFFile);
var
  I, J, K, L, M, N: Integer;
  SegmentData: string;
  PositionIncrement: string;  
  RepeatCount: Integer;
  LoopId, RepeatData: string;
  Segment: TEDISEFSegment;
  NestedLoop: TEDISEFLoop;
  ListItem: TEDISEFDataObjectListItem;
begin
  Loop.EDISEFDataObjects.Clear;
  I := 1;
  M := I;
  // Search for Loops and Segments
  while I > 0 do
  begin
    // Start search
    I := StrSearch(SEFDelimiter_OpeningBracket, Data, M);
    if I = 0 then
      Break;
    J := StrSearch(SEFDelimiter_ClosingBracket, Data, M);
    K := StrSearch(SEFDelimiter_OpeningBrace, Data, M);
    L := StrSearch(SEFDelimiter_ClosingBrace, Data, M);
    // Determine if data block to process is a repetition
    if (I < K) or (K = 0) then // Normal data block
    begin
      L := M;
      M := StrSearch(SEFDelimiter_PlusSign, Data, L);
      N := StrSearch(SEFDelimiter_MinusSign, Data, L);
      L := 0;
      if (M < I) and (M <> 0) then
        L := M;
      if (N < I) and (N <> 0) then
        L := N;
      if L <> 0 then
        PositionIncrement := Copy(Data, L, (I - L));      

      SegmentData := Copy(Data, I + 1, (J - I) - 1);
      //
      Segment := Loop.AddSegment;
      Segment.SEFFile := SEFFile;
      if L <> 0 then
      begin
        Segment.ResetPositionInc := True;
        Segment.PositionIncrement := StrToInt(PositionIncrement);
      end;
      Segment.Data := SegmentData;
      Segment.Disassemble;
      if Loop.Id = '' then
        Loop.Id := Segment.Id;
      M := J + 1;
    end
    else // Repeating data block (K = 1 on the first pass)
    begin
      N := StrSearch(SEFDelimiter_OpeningBracket, Data, K);
      J := StrSearch(SEFDelimiter_PlusSign, Data, K);
      M := StrSearch(SEFDelimiter_MinusSign, Data, K);
      // Adjustments
      if (J < N) and (J <> 0) then
        N := J;
      if (M < N) and (M <> 0) then
        N := M;
      // Get Loop Id: <Id>:<Repeat>+<Number>[<Id>]..."
      RepeatData := Copy(Data, K + 1, (N - K) - 1);
      J := StrSearch(SEFDelimiter_Colon, RepeatData, 1);
      if J = 0 then
      begin
        LoopId := RepeatData;
        RepeatData := '';
      end
      else
      begin
        LoopId := Copy(RepeatData, 1, J - 1);
        RepeatData := Copy(RepeatData, J + 1, Length(RepeatData) - J);
      end;
      // Get Repeat Count
      if RepeatData = Value_GreaterThanOne then
        RepeatData := IntToStr(Value_UndefinedMaximum);
      if RepeatData = '' then
        RepeatData := Value_One;
      RepeatCount := StrToInt(RepeatData);
      // Correct start position
      K := N;      
      // Validate end position
      N := StrSearch(SEFDelimiter_OpeningBrace, Data, K + 1);
      while (N <> 0) and (N < L) do // Detect nested repetition
      begin
        N := StrSearch(SEFDelimiter_OpeningBrace, Data, N + 1); // Search for nested repetition
        L := StrSearch(SEFDelimiter_ClosingBrace, Data, L + 1); // Correct end position
      end;
      // Copy data for repetition
      RepeatData := Copy(Data, K, L - K);
      //
      M := L + 1;
      // Create Loop Object
      NestedLoop := Loop.AddLoop;
      NestedLoop.SEFFile := SEFFile;
      NestedLoop.LoopId := LoopId;
      NestedLoop.MaximumRepeat := RepeatCount;                
      NestedLoop.Data := RepeatData;
      NestedLoop.Disassemble;
      if NestedLoop.LoopId = '' then
      begin
        ListItem := NestedLoop.EDISEFDataObjects.First;
        NestedLoop.LoopId := ListItem.EDISEFDataObject.Id;
      end;
    end;
  end;
end;

procedure ParseTableDataOfSETSDefinition(Data: string; Table: TEDISEFTable;
  SEFFile: TEDISEFFile);
var
  I, J, K, L, M, N: Integer;
  SegmentData: string;
  PositionIncrement: string;
  RepeatCount: Integer;
  LoopId, RepeatData: string;
  Segment: TEDISEFSegment;
  Loop: TEDISEFLoop;
  ListItem: TEDISEFDataObjectListItem;
begin
  Table.EDISEFDataObjects.Clear;
  I := 1;
  M := I;
  // Search for Loops and Segments
  while I > 0 do
  begin
    // Start search
    I := StrSearch(SEFDelimiter_OpeningBracket, Data, M);
    if I = 0 then
      Break;
    J := StrSearch(SEFDelimiter_ClosingBracket, Data, M);
    K := StrSearch(SEFDelimiter_OpeningBrace, Data, M);
    L := StrSearch(SEFDelimiter_ClosingBrace, Data, M);
    // Determine if data block to process is a repetition
    if (I < K) or (K = 0) then // Normal data block
    begin
      L := M;
      M := StrSearch(SEFDelimiter_PlusSign, Data, L);
      N := StrSearch(SEFDelimiter_MinusSign, Data, L);
      L := 0;
      if (M < I) and (M <> 0) then
        L := M;
      if (N < I) and (N <> 0) then
        L := N;
      if L <> 0 then
        PositionIncrement := Copy(Data, L, (I - L));          

      SegmentData := Copy(Data, I + 1, (J - I) - 1);
      //
      Segment := Table.AddSegment;
      Segment.SEFFile := SEFFile;
      if L <> 0 then
      begin
        Segment.ResetPositionInc := True;
        Segment.PositionIncrement := StrToInt(PositionIncrement);
      end;
      Segment.Data := SegmentData;
      Segment.Disassemble;
      M := J + 1;
    end
    else // Repeating data block (K = 1 on the first pass)
    begin
      N := StrSearch(SEFDelimiter_OpeningBracket, Data, K);
      J := StrSearch(SEFDelimiter_PlusSign, Data, K);
      M := StrSearch(SEFDelimiter_MinusSign, Data, K);
      // Adjustments - N becomes the start position of RepeatData
      if (J < N) and (J <> 0) then
        N := J;
      if (M < N) and (M <> 0) then
        N := M;
      // Get Loop Id: {<Loop Id>:<Repeat><+or-><Position Increment>[<Segment Id>]..."
      RepeatData := Copy(Data, K + 1, (N - K) - 1);
      J := StrSearch(SEFDelimiter_Colon, RepeatData, 1);
      if J = 0 then
      begin
        LoopId := RepeatData;
        RepeatData := '';
      end
      else
      begin
        LoopId := Copy(RepeatData, 1, J - 1);
        RepeatData := Copy(RepeatData, J + 1, Length(RepeatData) - J);
      end;
      // Get Repeat Count
      if RepeatData = Value_GreaterThanOne then
        RepeatData := IntToStr(Value_UndefinedMaximum);
      if RepeatData = '' then
        RepeatData := Value_One;
      RepeatCount := StrToInt(RepeatData);
      // Correct start position (Move to first "<+or-><position increment>[<Segment Id>]")
      K := N;
      // Validate end position
      N := StrSearch(SEFDelimiter_OpeningBrace, Data, K + 1);
      while (N <> 0) and (N < L) do // Detect nested repetition
      begin
        N := StrSearch(SEFDelimiter_OpeningBrace, Data, N + 1); // Search for nested repetition
        L := StrSearch(SEFDelimiter_ClosingBrace, Data, L + 1); // Correct end position
      end;
      // Copy data for repetition
      RepeatData := Copy(Data, K, L - K);
      //
      M := L + 1;
      // Create Loop Object
      Loop := Table.AddLoop;
      Loop.SEFFile := SEFFile;
      Loop.LoopId := LoopId;
      Loop.MaximumRepeat := RepeatCount;
      Loop.Data := RepeatData;
      Loop.Disassemble;
      if Loop.LoopId = '' then
      begin
        ListItem := Loop.EDISEFDataObjects.First;
        Loop.LoopId := ListItem.EDISEFDataObject.Id;
      end;
    end;
  end;
end;

procedure ParseSetsDataOfSETSDefinition(Data: string; ASet: TEDISEFSet; SEFFile: TEDISEFFile);
var
  I, J: Integer;
  Table: TEDISEFTable;
  TableData: string;
begin
  ASet.EDISEFDataObjects.Clear;
  I := StrSearch(SEFDelimiter_EqualSign, Data, 1);
  ASet.Id := Copy(Data, 1, I - 1);
  while I > 0 do
  begin
    // Start search
    I := StrSearch(SEFDelimiter_Caret, Data, I);
    J := StrSearch(SEFDelimiter_Caret, Data, I + 1);
    if I = 0 then
    begin
      Table := ASet.AddTable;
      Table.Data := Data;
      Table.Disassemble;
    end
    else
    begin
      if J = 0 then
      begin
        TableData := Copy(Data, I + 1, Length(Data) - I);
        I := 0;
      end
      else
      begin
        TableData := Copy(Data, I + 1, J - (I + 1));
        I := J;
      end;
      Table := ASet.AddTable;
      Table.Data := TableData;
      Table.Disassemble;
    end;
  end;
end;

procedure ExtractFromDataObjectGroup(DataObjectClass: TEDISEFDataObjectClass;
  DataObjectGroup: TEDISEFDataObjectGroup; ObjectList: TObjectList);
var
  ListItem: TEDISEFDataObjectListItem;
  RepeatingPattern: TEDISEFRepeatingPattern;
begin
  ListItem := DataObjectGroup.EDISEFDataObjects.First;
  while ListItem <> nil do
  begin
    if ListItem.EDISEFDataObject is DataObjectClass then
      ObjectList.Add(ListItem.EDISEFDataObject)
    else
    if ListItem.EDISEFDataObject is TEDISEFRepeatingPattern then
    begin
      RepeatingPattern := TEDISEFRepeatingPattern(ListItem.EDISEFDataObject);
      ExtractFromDataObjectGroup(DataObjectClass, RepeatingPattern, ObjectList);
    end;
    ListItem := ListItem.NextItem;
  end; 
end;

procedure ExtractFromDataObjectGroup(DataObjectClasses: array of TEDISEFDataObjectClass;
  DataObjectGroup: TEDISEFDataObjectGroup; ObjectList: TObjectList); overload;
var
  ClassCount: Integer;
  ListItem: TEDISEFDataObjectListItem;
  J: Integer;
  ClassMatch: Boolean;
  RepeatingPattern: TEDISEFRepeatingPattern;
begin
  ClassCount := Length(DataObjectClasses);
  ListItem := DataObjectGroup.EDISEFDataObjects.First;
  while ListItem <> nil do
  begin
    ClassMatch := False;
    for J := 0 to ClassCount - 1 do
    begin
      if ListItem.EDISEFDataObject is DataObjectClasses[J] then
      begin
        ClassMatch := True;
        Break;
      end;
    end;
    if ClassMatch then
      ObjectList.Add(ListItem.EDISEFDataObject)
    else
    if ListItem.EDISEFDataObject is TEDISEFRepeatingPattern then
    begin
      RepeatingPattern := TEDISEFRepeatingPattern(ListItem.EDISEFDataObject);
      ExtractFromDataObjectGroup(DataObjectClasses, RepeatingPattern, ObjectList);
    end;
    ListItem := ListItem.NextItem;
  end;
end;

function AddSubElementTo(DataObjectGroup: TEDISEFDataObjectGroup): TEDISEFSubElement;
begin
  Result := TEDISEFSubElement.Create(DataObjectGroup);
  DataObjectGroup.EDISEFDataObjects.Add(Result);
end;

function AppendSubElementTo(DataObjectGroup: TEDISEFDataObjectGroup;
  SubElement: TEDISEFSubElement): TEDISEFSubElement;
begin
  if SubElement <> nil then
  begin
    // Be sure the object will be managed by the current object
    if (SubElement.Parent is TEDISEFDataObjectGroup) and
      (SubElement.Parent <> DataObjectGroup) then
      with TEDISEFDataObjectGroup(SubElement.Parent).EDISEFDataObjects do
      begin
        Extract(SubElement);
        UpdateIndexes(nil); // Force update all index positions
      end;
    // Assign the new parent
    SubElement.Parent := DataObjectGroup;
    // Add to parent
    DataObjectGroup.EDISEFDataObjects.Add(SubElement, SubElement.Id);
    // Return appended object
    Result := SubElement;
  end
  else // Do not allow nil item objects
    Result := AddSubElementTo(DataObjectGroup);
end;

function ExtractSubElementFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  SubElement: TEDISEFSubElement): TEDISEFSubElement;
begin
  Result := TEDISEFSubElement(DataObjectGroup.EDISEFDataObjects.Extract(SubElement));
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

procedure DeleteSubElementFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  SubElement: TEDISEFSubElement);
begin
  DataObjectGroup.EDISEFDataObjects.Remove(SubElement);
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

function InsertSubElementInto(DataObjectGroup: TEDISEFDataObjectGroup;
  BeforeObject: TEDISEFDataObject): TEDISEFSubElement; overload;
begin
  Result := TEDISEFSubElement.Create(DataObjectGroup);
  DataObjectGroup.EDISEFDataObjects.Insert(Result, BeforeObject);
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

function InsertSubElementInto(DataObjectGroup: TEDISEFDataObjectGroup;
  SubElement: TEDISEFSubElement; BeforeObject: TEDISEFDataObject): TEDISEFSubElement; overload;
begin
  if SubElement <> nil then
  begin
    // Be sure the object will be managed by the current object
    if (SubElement.Parent is TEDISEFDataObjectGroup) and
      (SubElement.Parent <> DataObjectGroup) then
      with TEDISEFDataObjectGroup(SubElement.Parent).EDISEFDataObjects do
      begin
        Extract(SubElement);
        UpdateIndexes(nil); // Force update all index positions
      end;
    // Assign the new parent
    SubElement.Parent := DataObjectGroup;
    // Add to parent
    DataObjectGroup.EDISEFDataObjects.Insert(SubElement, BeforeObject);
    DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
    // Return appended object
    Result := SubElement;
  end
  else // Do not allow nil item objects
    Result := InsertSubElementInto(DataObjectGroup, BeforeObject);
end;

function AddElementTo(DataObjectGroup: TEDISEFDataObjectGroup): TEDISEFElement;
begin
  Result := TEDISEFElement.Create(DataObjectGroup);
  DataObjectGroup.EDISEFDataObjects.Add(Result);
end;

function AppendElementTo(DataObjectGroup: TEDISEFDataObjectGroup;
  Element: TEDISEFElement): TEDISEFElement;
begin
  if Element <> nil then
  begin
    // Be sure the object will be managed by the current object
    if (Element.Parent is TEDISEFDataObjectGroup) and
      (Element.Parent <> DataObjectGroup) then
      with TEDISEFDataObjectGroup(Element.Parent).EDISEFDataObjects do
      begin
        Extract(Element);
        UpdateIndexes(nil); // Force update all index positions
      end;
    // Assign the new parent
    Element.Parent := DataObjectGroup;
    // Add to parent
    DataObjectGroup.EDISEFDataObjects.Add(Element, Element.Id);
    // Return appended object
    Result := Element;
  end
  else // Do not allow nil item objects
    Result := AddElementTo(DataObjectGroup);
end;

function ExtractElementFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  Element: TEDISEFElement): TEDISEFElement;
begin
  Result := TEDISEFElement(DataObjectGroup.EDISEFDataObjects.Extract(Element));
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

procedure DeleteElementFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  Element: TEDISEFElement);
begin
  DataObjectGroup.EDISEFDataObjects.Remove(Element);
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

function InsertElementInto(DataObjectGroup: TEDISEFDataObjectGroup;
  BeforeObject: TEDISEFDataObject): TEDISEFElement; overload;
begin
  Result := TEDISEFElement.Create(DataObjectGroup);
  DataObjectGroup.EDISEFDataObjects.Insert(Result, BeforeObject);
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

function InsertElementInto(DataObjectGroup: TEDISEFDataObjectGroup;
  Element: TEDISEFElement; BeforeObject: TEDISEFDataObject): TEDISEFElement; overload;
begin
  if Element <> nil then
  begin
    // Be sure the object will be managed by the current object
    if (Element.Parent is TEDISEFDataObjectGroup) and
      (Element.Parent <> DataObjectGroup) then
      with TEDISEFDataObjectGroup(Element.Parent).EDISEFDataObjects do
      begin
        Extract(Element);
        UpdateIndexes(nil); // Force update all index positions
      end;
    // Assign the new parent
    Element.Parent := DataObjectGroup;
    // Add to parent
    DataObjectGroup.EDISEFDataObjects.Insert(Element, BeforeObject);
    DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
    // Return appended object
    Result := Element;
  end
  else // Do not allow nil item objects
    Result := InsertElementInto(DataObjectGroup, BeforeObject);
end;

function AddRepeatingPatternTo(DataObjectGroup: TEDISEFDataObjectGroup): TEDISEFRepeatingPattern;
begin
  Result := TEDISEFRepeatingPattern.Create(DataObjectGroup);
  DataObjectGroup.EDISEFDataObjects.Add(Result);
end;

function AppendRepeatingPatternTo(DataObjectGroup: TEDISEFDataObjectGroup;
  RepeatingPattern: TEDISEFRepeatingPattern): TEDISEFRepeatingPattern;
begin
  if RepeatingPattern <> nil then
  begin
    // Be sure the object will be managed by the current object
    if (RepeatingPattern.Parent is TEDISEFDataObjectGroup) and
      (RepeatingPattern.Parent <> DataObjectGroup) then
      with TEDISEFDataObjectGroup(RepeatingPattern.Parent).EDISEFDataObjects do
      begin
        Extract(RepeatingPattern);
        UpdateIndexes(nil); // Force update all index positions
      end;
    // Assign the new parent
    RepeatingPattern.Parent := DataObjectGroup;
    // Add to parent
    DataObjectGroup.EDISEFDataObjects.Add(RepeatingPattern, RepeatingPattern.Id);
    // Return appended object
    Result := RepeatingPattern;
  end
  else // Do not allow nil item objects
    Result := AddRepeatingPatternTo(DataObjectGroup);
end;

function ExtractRepeatingPatternFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  RepeatingPattern: TEDISEFRepeatingPattern): TEDISEFRepeatingPattern;
begin
  Result := TEDISEFRepeatingPattern(DataObjectGroup.EDISEFDataObjects.Extract(RepeatingPattern));
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

procedure DeleteRepeatingPatternFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  RepeatingPattern: TEDISEFRepeatingPattern);
begin
  DataObjectGroup.EDISEFDataObjects.Remove(RepeatingPattern);
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

function InsertRepeatingPatternInto(DataObjectGroup: TEDISEFDataObjectGroup;
  BeforeObject: TEDISEFDataObject): TEDISEFRepeatingPattern; overload;
begin
  Result := TEDISEFRepeatingPattern.Create(DataObjectGroup);
  DataObjectGroup.EDISEFDataObjects.Insert(Result, BeforeObject);
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

function InsertRepeatingPatternInto(DataObjectGroup: TEDISEFDataObjectGroup;
  RepeatingPattern: TEDISEFRepeatingPattern;
  BeforeObject: TEDISEFDataObject): TEDISEFRepeatingPattern; overload;
begin
  if RepeatingPattern <> nil then
  begin
    // Be sure the object will be managed by the current object
    if (RepeatingPattern.Parent is TEDISEFDataObjectGroup) and
      (RepeatingPattern.Parent <> DataObjectGroup) then
      with TEDISEFDataObjectGroup(RepeatingPattern.Parent).EDISEFDataObjects do
      begin
        Extract(RepeatingPattern);
        UpdateIndexes(nil); // Force update all index positions
      end;
    // Assign the new parent
    RepeatingPattern.Parent := DataObjectGroup;
    // Add to parent
    DataObjectGroup.EDISEFDataObjects.Insert(RepeatingPattern, BeforeObject);
    DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
    // Return appended object
    Result := RepeatingPattern;
  end
  else // Do not allow nil item objects
    Result := InsertRepeatingPatternInto(DataObjectGroup, BeforeObject);
end;

function AddCompositeElementTo(DataObjectGroup: TEDISEFDataObjectGroup): TEDISEFCompositeElement;
begin
  Result := TEDISEFCompositeElement.Create(DataObjectGroup);
  DataObjectGroup.EDISEFDataObjects.Add(Result);
end;

function AppendCompositeElementTo(DataObjectGroup: TEDISEFDataObjectGroup;
  CompositeElement: TEDISEFCompositeElement): TEDISEFCompositeElement;
begin
  if CompositeElement <> nil then
  begin
    // Be sure the object will be managed by the current object
    if (CompositeElement.Parent is TEDISEFDataObjectGroup) and
      (CompositeElement.Parent <> DataObjectGroup) then
      with TEDISEFDataObjectGroup(CompositeElement.Parent).EDISEFDataObjects do
      begin
        Extract(CompositeElement);
        UpdateIndexes(nil); // Force update all index positions
      end;
    // Assign the new parent
    CompositeElement.Parent := DataObjectGroup;
    // Add to parent
    DataObjectGroup.EDISEFDataObjects.Add(CompositeElement, CompositeElement.Id);
    // Return appended object
    Result := CompositeElement;
  end
  else // Do not allow nil item objects
    Result := AddCompositeElementTo(DataObjectGroup);
end;

function ExtractCompositeElementFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  CompositeElement: TEDISEFCompositeElement): TEDISEFCompositeElement;
begin
  Result := TEDISEFCompositeElement(DataObjectGroup.EDISEFDataObjects.Extract(CompositeElement));
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

procedure DeleteCompositeElementFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  CompositeElement: TEDISEFCompositeElement);
begin
  DataObjectGroup.EDISEFDataObjects.Remove(CompositeElement);
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

function InsertCompositeElementInto(DataObjectGroup: TEDISEFDataObjectGroup;
  BeforeObject: TEDISEFDataObject): TEDISEFCompositeElement; overload;
begin
  Result := TEDISEFCompositeElement.Create(DataObjectGroup);
  DataObjectGroup.EDISEFDataObjects.Insert(Result, BeforeObject);
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

function InsertCompositeElementInto(DataObjectGroup: TEDISEFDataObjectGroup;
  CompositeElement: TEDISEFCompositeElement;
  BeforeObject: TEDISEFDataObject): TEDISEFCompositeElement; overload;
begin
  if CompositeElement <> nil then
  begin
    // Be sure the object will be managed by the current object
    if (CompositeElement.Parent is TEDISEFDataObjectGroup) and
      (CompositeElement.Parent <> DataObjectGroup) then
      with TEDISEFDataObjectGroup(CompositeElement.Parent).EDISEFDataObjects do
      begin
        Extract(CompositeElement);
        UpdateIndexes(nil); // Force update all index positions
      end;
    // Assign the new parent
    CompositeElement.Parent := DataObjectGroup;
    // Add to parent
    DataObjectGroup.EDISEFDataObjects.Insert(CompositeElement, BeforeObject);
    DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
    // Return appended object
    Result := CompositeElement;
  end
  else // Do not allow nil item objects
    Result := InsertCompositeElementInto(DataObjectGroup, BeforeObject);
end;

function AddSegmentTo(DataObjectGroup: TEDISEFDataObjectGroup): TEDISEFSegment;
begin
  Result := TEDISEFSegment.Create(DataObjectGroup);
  DataObjectGroup.EDISEFDataObjects.Add(Result);
end;

function AppendSegmentTo(DataObjectGroup: TEDISEFDataObjectGroup;
  Segment: TEDISEFSegment): TEDISEFSegment;
begin
  if Segment <> nil then
  begin
    // Be sure the object will be managed by the current object
    if (Segment.Parent is TEDISEFDataObjectGroup) and
      (Segment.Parent <> DataObjectGroup) then
      with TEDISEFDataObjectGroup(Segment.Parent).EDISEFDataObjects do
      begin
        Extract(Segment);
        UpdateIndexes(nil); // Force update all index positions
      end;
    // Assign the new parent
    Segment.Parent := DataObjectGroup;
    // Add to parent
    DataObjectGroup.EDISEFDataObjects.Add(Segment, Segment.Id);
    // Return appended object
    Result := Segment;
  end
  else // Do not allow nil item objects
    Result := AddSegmentTo(DataObjectGroup);
end;

function ExtractSegmentFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  Segment: TEDISEFSegment): TEDISEFSegment;
begin
  Result := TEDISEFSegment(DataObjectGroup.EDISEFDataObjects.Extract(Segment));
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

procedure DeleteSegmentFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  Segment: TEDISEFSegment);
begin
  DataObjectGroup.EDISEFDataObjects.Remove(Segment);
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

function InsertSegmentInto(DataObjectGroup: TEDISEFDataObjectGroup;
  BeforeObject: TEDISEFDataObject): TEDISEFSegment; overload;
begin
  Result := TEDISEFSegment.Create(DataObjectGroup);
  DataObjectGroup.EDISEFDataObjects.Insert(Result, BeforeObject);
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

function InsertSegmentInto(DataObjectGroup: TEDISEFDataObjectGroup;
  Segment: TEDISEFSegment; BeforeObject: TEDISEFDataObject): TEDISEFSegment; overload;
begin
  if Segment <> nil then
  begin
    // Be sure the object will be managed by the current object
    if (Segment.Parent is TEDISEFDataObjectGroup) and
      (Segment.Parent <> DataObjectGroup) then
      with TEDISEFDataObjectGroup(Segment.Parent).EDISEFDataObjects do
      begin
        Extract(Segment);
        UpdateIndexes(nil); // Force update all index positions
      end;
    // Assign the new parent
    Segment.Parent := DataObjectGroup;
    // Add to parent
    DataObjectGroup.EDISEFDataObjects.Insert(Segment, BeforeObject);
    DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
    // Return appended object
    Result := Segment;
  end
  else // Do not allow nil item objects
    Result := InsertSegmentInto(DataObjectGroup, BeforeObject);
end;

function AddLoopTo(DataObjectGroup: TEDISEFDataObjectGroup): TEDISEFLoop;
begin
  Result := TEDISEFLoop.Create(DataObjectGroup);
  DataObjectGroup.EDISEFDataObjects.Add(Result);
end;

function AppendLoopTo(DataObjectGroup: TEDISEFDataObjectGroup;
  Loop: TEDISEFLoop): TEDISEFLoop;
begin
  if Loop <> nil then
  begin
    // Be sure the object will be managed by the current object
    if (Loop.Parent is TEDISEFDataObjectGroup) and
      (Loop.Parent <> DataObjectGroup) then
      with TEDISEFDataObjectGroup(Loop.Parent).EDISEFDataObjects do
      begin
        Extract(Loop);
        UpdateIndexes(nil); // Force update all index positions
      end;
    // Assign the new parent
    Loop.Parent := DataObjectGroup;
    // Add to parent
    DataObjectGroup.EDISEFDataObjects.Add(Loop, Loop.Id);
    // Return appended object
    Result := Loop;
  end
  else // Do not allow nil item objects
    Result := AddLoopTo(DataObjectGroup);
end;

function ExtractLoopFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  Loop: TEDISEFLoop): TEDISEFLoop;
begin
  Result := TEDISEFLoop(DataObjectGroup.EDISEFDataObjects.Extract(Loop));
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

procedure DeleteLoopFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  Loop: TEDISEFLoop);
begin
  DataObjectGroup.EDISEFDataObjects.Remove(Loop);
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

function InsertLoopInto(DataObjectGroup: TEDISEFDataObjectGroup;
  BeforeObject: TEDISEFDataObject): TEDISEFLoop; overload;
begin
  Result := TEDISEFLoop.Create(DataObjectGroup);
  DataObjectGroup.EDISEFDataObjects.Insert(Result, BeforeObject);
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

function InsertLoopInto(DataObjectGroup: TEDISEFDataObjectGroup;
  Loop: TEDISEFLoop; BeforeObject: TEDISEFDataObject): TEDISEFLoop; overload;
begin
  if Loop <> nil then
  begin
    // Be sure the object will be managed by the current object
    if (Loop.Parent is TEDISEFDataObjectGroup) and
      (Loop.Parent <> DataObjectGroup) then
      with TEDISEFDataObjectGroup(Loop.Parent).EDISEFDataObjects do
      begin
        Extract(Loop);
        UpdateIndexes(nil); // Force update all index positions
      end;
    // Assign the new parent
    Loop.Parent := DataObjectGroup;
    // Add to parent
    DataObjectGroup.EDISEFDataObjects.Insert(Loop, BeforeObject);
    DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
    // Return appended object
    Result := Loop;
  end
  else // Do not allow nil item objects
    Result := InsertLoopInto(DataObjectGroup, BeforeObject);
end;

function AddTableTo(DataObjectGroup: TEDISEFDataObjectGroup): TEDISEFTable;
begin
  Result := TEDISEFTable.Create(DataObjectGroup);
  DataObjectGroup.EDISEFDataObjects.Add(Result);
end;

function AppendTableTo(DataObjectGroup: TEDISEFDataObjectGroup;
  Table: TEDISEFTable): TEDISEFTable;
begin
  if Table <> nil then
  begin
    // Be sure the object will be managed by the current object
    if (Table.Parent is TEDISEFDataObjectGroup) and
      (Table.Parent <> DataObjectGroup) then
      with TEDISEFDataObjectGroup(Table.Parent).EDISEFDataObjects do
      begin
        Extract(Table);
        UpdateIndexes(nil); // Force update all index positions
      end;
    // Assign the new parent
    Table.Parent := DataObjectGroup;
    // Add to parent
    DataObjectGroup.EDISEFDataObjects.Add(Table, Table.Id);
    // Return appended object
    Result := Table;
  end
  else // Do not allow nil item objects
    Result := AddTableTo(DataObjectGroup);
end;

function ExtractTableFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  Table: TEDISEFTable): TEDISEFTable;
begin
  Result := TEDISEFTable(DataObjectGroup.EDISEFDataObjects.Extract(Table));
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

procedure DeleteTableFrom(DataObjectGroup: TEDISEFDataObjectGroup;
  Table: TEDISEFTable);
begin
  DataObjectGroup.EDISEFDataObjects.Remove(Table);
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

function InsertTableInto(DataObjectGroup: TEDISEFDataObjectGroup;
  BeforeObject: TEDISEFDataObject): TEDISEFTable; overload;
begin
  Result := TEDISEFTable.Create(DataObjectGroup);
  DataObjectGroup.EDISEFDataObjects.Insert(Result, BeforeObject);
  DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
end;

function InsertTableInto(DataObjectGroup: TEDISEFDataObjectGroup;
  Table: TEDISEFTable; BeforeObject: TEDISEFDataObject): TEDISEFTable; overload;
begin
  if Table <> nil then
  begin
    // Be sure the object will be managed by the current object
    if (Table.Parent is TEDISEFDataObjectGroup) and
      (Table.Parent <> DataObjectGroup) then
      with TEDISEFDataObjectGroup(Table.Parent).EDISEFDataObjects do
      begin
        Extract(Table);
        UpdateIndexes(nil); // Force update all index positions
      end;
    // Assign the new parent
    Table.Parent := DataObjectGroup;
    // Add to parent
    DataObjectGroup.EDISEFDataObjects.Insert(Table, BeforeObject);
    DataObjectGroup.EDISEFDataObjects.UpdateIndexes(nil); // Force update all index positions
    // Return appended object
    Result := Table;
  end
  else // Do not allow nil item objects
    Result := InsertTableInto(DataObjectGroup, BeforeObject);
end;

//=== { TEDISEFDataObject } ==================================================

constructor TEDISEFDataObject.Create(Parent: TEDISEFDataObject);
begin
  inherited Create;
  FId := '';
  FData := '';
  FLength := 0;
  FParent := nil;
  FSEFFile := nil;
  FOwnerItemRef := nil;
  if Assigned(Parent) then
  begin
    FParent := Parent;
    if Parent is TEDISEFFile then
      FSEFFile := TEDISEFFile(Parent)
    else
      FSEFFile := Parent.SEFFile;
  end;
end;

destructor TEDISEFDataObject.Destroy;
begin
  FOwnerItemRef := nil;
  inherited Destroy;
end;

function TEDISEFDataObject.Clone(NewParent: TEDISEFDataObject): TEDISEFDataObject;
begin
  Result := CloneDataObject(NewParent);
end;

function TEDISEFDataObject.GetData: string;
begin
  Result := FData;
end;

procedure TEDISEFDataObject.SetData(const Data: string);
begin
  FData := Data;
  FLength := Length(FData);
end;

procedure TEDISEFDataObject.SetParent(const Value: TEDISEFDataObject);
begin
  FParent := Value;
  if FParent is TEDISEFFile then
    FSEFFile := TEDISEFFile(FParent)
  else
    FSEFFile := FParent.SEFFile;
end;

procedure TEDISEFDataObject.SetId(const Value: string);
begin
  FId := Value;
  UpdateOwnerItemName;
end;

procedure TEDISEFDataObject.UpdateOwnerItemName;
begin
  if FOwnerItemRef <> nil then
    FOwnerItemRef.UpdateName;
end;

//=== { TEDISEFDataObjectListItem } ==========================================

function TEDISEFDataObjectListItem.GetEDISEFDataObject: TEDISEFDataObject;
begin
  Result := TEDISEFDataObject(FEDIObject);
end;

procedure TEDISEFDataObjectListItem.LinkToObject;
begin
  if FEDIObject <> nil then
    TEDISEFDataObject(FEDIObject).OwnerItemRef := Self;
end;

function TEDISEFDataObjectListItem.NextItem: TEDISEFDataObjectListItem;
begin
  Result := TEDISEFDataObjectListItem(FNextItem);
end;

function TEDISEFDataObjectListItem.PriorItem: TEDISEFDataObjectListItem;
begin
  Result := TEDISEFDataObjectListItem(FPriorItem);
end;

procedure TEDISEFDataObjectListItem.SetEDISEFDataObject(const Value: TEDISEFDataObject);
begin
  FEDIObject := Value;
end;

procedure TEDISEFDataObjectListItem.UpdateName;
begin
  if FEDIObject <> nil then
    FName := TEDISEFDataObject(FEDIObject).Id;
end;

//=== { TEDISEFDataObjectList } ==============================================

function TEDISEFDataObjectList.Add(EDISEFDataObject: TEDISEFDataObject;
  Name: string): TEDISEFDataObjectListItem;
begin
  if Name = '' then
    Name := EDISEFDataObject.Id;
  Result := TEDISEFDataObjectListItem(inherited Add(EDISEFDataObject, Name));
  Result.LinkToObject;
end;

function TEDISEFDataObjectList.CreateListItem(PriorItem: TEDIObjectListItem;
  EDIObject: TEDIObject): TEDIObjectListItem;
begin
  Result := TEDISEFDataObjectListItem.Create(Self, PriorItem, EDIObject);
end;

function TEDISEFDataObjectList.FindItemByName(Name: string;
  StartItem: TEDIObjectListItem = nil): TEDISEFDataObjectListItem;
var
  ListItem: TEDIObjectListItem;
begin
  ListItem := inherited FindItemByName(Name, StartItem);
  Result := TEDISEFDataObjectListItem(ListItem);
end;

function TEDISEFDataObjectList.First(Index: Integer): TEDISEFDataObjectListItem;
begin
  Result := TEDISEFDataObjectListItem(inherited First(Index));
end;

function TEDISEFDataObjectList.GetEDISEFDataObject(Index: Integer): TEDISEFDataObject;
begin
  Result := TEDISEFDataObject(GetEDIObject(Index));
end;

function TEDISEFDataObjectList.GetObjectByItemByName(Name: string): TEDISEFDataObject;
var
  ListItem: TEDISEFDataObjectListItem;
begin
  Result := nil;
  ListItem := FindItemByName(Name);
  if ListItem <> nil then
    Result := FindItemByName(Name).EDISEFDataObject;
end;

function TEDISEFDataObjectList.Insert(
  EDISEFDataObject, BeforeEDISEFDataObject: TEDISEFDataObject): TEDISEFDataObjectListItem;
begin
  Result := TEDISEFDataObjectListItem(inherited Insert(EDISEFDataObject, BeforeEDISEFDataObject));
  Result.LinkToObject;
end;

function TEDISEFDataObjectList.Last: TEDISEFDataObjectListItem;
begin
  Result := TEDISEFDataObjectListItem(inherited Last);
end;

function TEDISEFDataObjectList.Next: TEDISEFDataObjectListItem;
begin
  Result := TEDISEFDataObjectListItem(inherited Next);
end;

function TEDISEFDataObjectList.Prior: TEDISEFDataObjectListItem;
begin
  Result := TEDISEFDataObjectListItem(inherited Prior);
end;

procedure TEDISEFDataObjectList.SetEDISEFDataObject(Index: Integer; const Value: TEDISEFDataObject);
begin
  SetEDIObject(Index, Value);
end;

//=== { TEDISEFDataObjectGroup } =============================================

constructor TEDISEFDataObjectGroup.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  FEDISEFDataObjects := TEDISEFDataObjectList.Create;
end;

destructor TEDISEFDataObjectGroup.Destroy;
begin
  FEDISEFDataObjects.Free;
  inherited Destroy;
end;

function TEDISEFDataObjectGroup.GetCount: Integer;
begin
  Result := FEDISEFDataObjects.Count;
end;

function TEDISEFDataObjectGroup.GetEDISEFDataObject(Index: Integer): TEDISEFDataObject;
begin
  Result := FEDISEFDataObjects[Index];
end;

//=== { TEDISEFElement } =====================================================

constructor TEDISEFElement.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  FUserAttribute := '';
  FOrdinal := -1;
  FOutOfSequenceOrdinal := False;
  FElementType := '';
  FMinimumLength := 0;
  FMaximumLength := 0;
  FRequirementDesignator := '';
  FRepeatCount := -1;
  FEDISEFTextSets := TEDISEFTextSets.Create(False);
end;

destructor TEDISEFElement.Destroy;
begin
  FEDISEFTextSets.Free;
  inherited Destroy;
end;

function TEDISEFElement.Assemble: string;
begin
  Result := '';
  if FParent is TEDISEFFile then
    Result := CombineELMSDataOfELMSDefinition(Self)
  else
  if (FParent is TEDISEFCompositeElement) or (FParent is TEDISEFSegment) then
    Result := CombineELMSDataOfCOMSorSEGSDefinition(Self, FSEFFile.ELMS)
  else
  if FParent is TEDISEFRepeatingPattern then
  begin
    if (TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFCompositeElement) or
      (TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFSegment) then
    begin
      Result := CombineELMSDataOfCOMSorSEGSDefinition(Self, FSEFFile.ELMS);
    end;
  end;
end;

procedure TEDISEFElement.Assign(EDISEFElement: TEDISEFElement);
begin
  // FUserAttribute := EDISEFElement.UserAttribute;
  // FOrdinal := EDISEFElement.Ordinal;
  FId := EDISEFElement.ElementId;
  FElementType := EDISEFElement.ElementType;
  FMinimumLength := EDISEFElement.MinimumLength;
  FMaximumLength := EDISEFElement.MaximumLength;
  FRequirementDesignator := EDISEFElement.RequirementDesignator;
  FRepeatCount := EDISEFElement.RepeatCount;
end;

function TEDISEFElement.CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject;
begin
  Result := Clone(NewParent);
end;

function TEDISEFElement.Clone(NewParent: TEDISEFDataObject): TEDISEFElement;
begin
  Result := TEDISEFElement.Create(NewParent);
  Result.Data := FData;
  Result.UserAttribute := FUserAttribute;
  Result.ElementId := FId;
  Result.Ordinal := FOrdinal;
  Result.ElementType := FElementType;
  Result.MinimumLength := FMinimumLength;
  Result.MaximumLength := FMaximumLength;
  Result.RequirementDesignator := FRequirementDesignator;
end;

procedure TEDISEFElement.Disassemble;
begin
  if FParent is TEDISEFFile then
    ParseELMSDataOfELMSDefinition(FData, Self)
  else
  if FParent is TEDISEFCompositeElement then
    ParseELMSDataOfCOMSDefinition(FData, Self, FSEFFile.ELMS)
  else
  if FParent is TEDISEFSegment then
    ParseELMSDataOfSEGSDefinition(FData, Self, FSEFFile.ELMS)
  else
  if FParent is TEDISEFRepeatingPattern then
  begin
    if TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFCompositeElement then
      ParseELMSDataOfCOMSDefinition(FData, Self, FSEFFile.ELMS)
    else
    if TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFSegment then
      ParseELMSDataOfSEGSDefinition(FData, Self, FSEFFile.ELMS);
  end;
  UpdateOwnerItemName;
end;

function TEDISEFElement.GetTextSetsLocation: string;
var
  DataObject: TEDISEFDataObject;
begin
  Result := '';
  if FParent is TEDISEFCompositeElement then
    Result := TEDISEFCompositeElement(FParent).GetTextSetsLocation
  else
  if FParent is TEDISEFSegment then
    Result := TEDISEFSegment(FParent).GetTextSetsLocation
  else
  if FParent is TEDISEFRepeatingPattern then
  begin
    DataObject := TEDISEFRepeatingPattern(FParent).BaseParent;
    if DataObject is TEDISEFCompositeElement then
      Result := TEDISEFCompositeElement(FParent).GetTextSetsLocation
    else
    if DataObject is TEDISEFSegment then
      Result := TEDISEFSegment(DataObject).GetTextSetsLocation;
  end; 
  if Result <> '' then
    Result := Result + '~' + IntToStr(FOrdinal);
end;

procedure TEDISEFElement.BindTextSets(TEXTSETS: TEDISEFTextSets);
begin
  FEDISEFTextSets.Free;
  FEDISEFTextSets := TEDISEFTextSets(TextSets.ReturnListItemsByName(GetTextSetsLocation));
end;

function TEDISEFElement.CloneAsSubElement(NewParent: TEDISEFDataObject): TEDISEFSubElement;
begin
  Result := TEDISEFSubElement.Create(NewParent);
  Result.Data := FData;
  Result.UserAttribute := FUserAttribute;
  Result.ElementId := FId;
  Result.Ordinal := FOrdinal;
  Result.ElementType := FElementType;
  Result.MinimumLength := FMinimumLength;
  Result.MaximumLength := FMaximumLength;
  Result.RequirementDesignator := FRequirementDesignator;
end;

//=== { TEDISEFSubElement } ==================================================

constructor TEDISEFSubElement.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
end;

destructor TEDISEFSubElement.Destroy;
begin
  inherited Destroy;
end;

function TEDISEFSubElement.Assemble: string;
begin
  Result := inherited Assemble;
end;

function TEDISEFSubElement.CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject;
begin
  Result := Clone(NewParent);
end;

function TEDISEFSubElement.Clone(NewParent: TEDISEFDataObject): TEDISEFSubElement;
begin
  Result := TEDISEFSubElement.Create(NewParent);
  Result.Data := FData;
  Result.UserAttribute := FUserAttribute;
  Result.ElementId := FId;
  Result.Ordinal := FOrdinal;
  Result.ElementType := FElementType;
  Result.MinimumLength := FMinimumLength;
  Result.MaximumLength := FMaximumLength;
  Result.RequirementDesignator := FRequirementDesignator;
end;

procedure TEDISEFSubElement.Disassemble;
begin
  inherited Disassemble;
end;

//=== { TEDISEFCompositeElement } ============================================

constructor TEDISEFCompositeElement.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  FUserAttribute := '';
  FOrdinal := -1;
  FOutOfSequenceOrdinal := False;
  FRequirementDesignator := '';
  FExtendedData := '';
  FEDISEFTextSets := TEDISEFTextSets.Create(False);
end;

destructor TEDISEFCompositeElement.Destroy;
begin
  FEDISEFTextSets.Free;
  inherited Destroy;
end;

function TEDISEFCompositeElement.Assemble: string;
begin
  Result := '';
  if FParent is TEDISEFFile then
    Result := CombineCOMSDataOfCOMSDefinition(Self)
  else
  if FParent is TEDISEFSegment then
    Result := CombineCOMSDataOfSEGSDefinition(Self)
  else
  if FParent is TEDISEFRepeatingPattern then
    if TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFSegment then
      Result := CombineCOMSDataOfSEGSDefinition(Self);
end;

procedure TEDISEFCompositeElement.Assign(CompositeElement: TEDISEFCompositeElement);
var
  ListItem: TEDISEFDataObjectListItem;
  SubElement: TEDISEFSubElement;
  RepeatingPattern: TEDISEFRepeatingPattern;
begin
  FEDISEFDataObjects.Clear;
  FUserAttribute := CompositeElement.UserAttribute;
  FId := CompositeElement.CompositeElementId;
  FOrdinal := CompositeElement.Ordinal;
  ListItem := CompositeElement.Elements.First;
  while ListItem <> nil do
  begin
    if ListItem.EDISEFDataObject <> nil then
    begin
      if ListItem.EDISEFDataObject is TEDISEFSubElement then
      begin
        SubElement := TEDISEFSubElement(ListItem.EDISEFDataObject);
        SubElement := SubElement.Clone(Self);
        AppendSubElement(SubElement);
      end
      else
      if ListItem.EDISEFDataObject is TEDISEFRepeatingPattern then
      begin
        RepeatingPattern := TEDISEFRepeatingPattern(ListItem.EDISEFDataObject);
        RepeatingPattern := RepeatingPattern.Clone(Self);
        AppendRepeatingPattern(RepeatingPattern);
      end;
    end
    else
      AddSubElement;
    ListItem := ListItem.NextItem;
  end;
end;

function TEDISEFCompositeElement.CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject;
begin
  Result := Clone(NewParent);
end;

function TEDISEFCompositeElement.Clone(NewParent: TEDISEFDataObject): TEDISEFCompositeElement;
var
  ListItem: TEDISEFDataObjectListItem;
  SubElement: TEDISEFSubElement;
  RepeatingPattern: TEDISEFRepeatingPattern;
begin
  Result := TEDISEFCompositeElement.Create(NewParent);
  Result.UserAttribute := FUserAttribute;
  Result.CompositeElementId := FId;
  Result.RequirementDesignator := FRequirementDesignator;
  Result.Ordinal := FOrdinal;
  ListItem := FEDISEFDataObjects.First;
  while ListItem <> nil do
  begin
    if ListItem.EDISEFDataObject <> nil then
    begin
      if ListItem.EDISEFDataObject is TEDISEFSubElement then
      begin
        SubElement := TEDISEFSubElement(ListItem.EDISEFDataObject);
        SubElement := SubElement.Clone(Result);
        Result.Elements.Add(SubElement, SubElement.Id);
      end
      else
      if ListItem.EDISEFDataObject is TEDISEFRepeatingPattern then
      begin
        RepeatingPattern := TEDISEFRepeatingPattern(ListItem.EDISEFDataObject);
        RepeatingPattern := RepeatingPattern.Clone(Result);
        Result.Elements.Add(RepeatingPattern, RepeatingPattern.Id);
      end;
    end
    else
    begin
      SubElement := TEDISEFSubElement.Create(Self);
      Result.Elements.Add(SubElement);
    end;
    ListItem := ListItem.NextItem;
  end;
end;

procedure TEDISEFCompositeElement.Disassemble;
begin
  if FParent is TEDISEFFile then
    ParseCOMSDataOfCOMSDefinition(FData, Self, FSEFFile.ELMS)
  else
  if FParent is TEDISEFSegment then
    ParseCOMSDataOfSEGSDefinition(FData, Self, FSEFFile.COMS)
  else
  if FParent is TEDISEFRepeatingPattern then
  begin
    if TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFSegment then
      ParseCOMSDataOfSEGSDefinition(FData, Self, FSEFFile.COMS);
  end;
  UpdateOwnerItemName;  
  AssignElementOrdinals;
end;

function TEDISEFCompositeElement.GetTextSetsLocation: string;
begin
  Result := '';
  if FParent is TEDISEFSegment then
    Result := TEDISEFSegment(FParent).GetTextSetsLocation
  else
  if FParent is TEDISEFRepeatingPattern then
    if TEDISEFRepeatingPattern(FParent).BaseParent is TEDISEFSegment then
      Result := TEDISEFSegment(TEDISEFRepeatingPattern(FParent).BaseParent).GetTextSetsLocation;
  if Result <> '' then
    Result := Result + '~' + IntToStr(FOrdinal);
end;

procedure TEDISEFCompositeElement.AssignElementOrdinals;
var
  I: Integer;
  Element: TEDISEFElement;
  ElementList: TObjectList;
  AssignOrdinal: Integer;
begin
  ElementList := GetElementObjectList;
  try
    AssignOrdinal := 0;
    for I := 0 to ElementList.Count - 1 do
    begin
      if ElementList[I] is TEDISEFElement then
      begin
        Element := TEDISEFElement(ElementList[I]);
        if Element.Ordinal = -1 then
        begin
          Inc(AssignOrdinal);
          Element.Ordinal := AssignOrdinal;
        end
        else
        begin
          AssignOrdinal := Element.Ordinal;
          Element.OutOfSequenceOrdinal := True;
        end;
      end;
    end; 
  finally
    ElementList.Free;
  end;
end;

function TEDISEFCompositeElement.GetElementObjectList: TObjectList;
begin
  Result := TObjectList.Create(False);
  ExtractFromDataObjectGroup(TEDISEFElement, Self, Result);
end;

procedure TEDISEFCompositeElement.BindTextSets(TEXTSETS: TEDISEFTextSets);
begin
  FEDISEFTextSets.Free;
  FEDISEFTextSets := TEDISEFTextSets(TextSets.ReturnListItemsByName(GetTextSetsLocation));
end;

function TEDISEFCompositeElement.AddSubElement: TEDISEFSubElement;
begin
  Result := AddSubElementTo(Self);
end;

function TEDISEFCompositeElement.AppendSubElement(SubElement: TEDISEFSubElement): TEDISEFSubElement;
begin
  Result := AppendSubElementTo(Self, SubElement);
end;

procedure TEDISEFCompositeElement.DeleteSubElement(SubElement: TEDISEFSubElement);
begin
  DeleteSubElementFrom(Self, SubElement);
end;

function TEDISEFCompositeElement.ExtractSubElement(
  SubElement: TEDISEFSubElement): TEDISEFSubElement;
begin
  Result := ExtractSubElementFrom(Self, SubElement);
end;

function TEDISEFCompositeElement.InsertSubElement(
  BeforeObject: TEDISEFDataObject): TEDISEFSubElement;
begin
  Result := InsertSubElementInto(Self, BeforeObject);
end;

function TEDISEFCompositeElement.InsertSubElement(SubElement: TEDISEFSubElement;
  BeforeObject: TEDISEFDataObject): TEDISEFSubElement;
begin
  Result := InsertSubElementInto(Self, SubElement, BeforeObject);
end;

function TEDISEFCompositeElement.AddRepeatingPattern: TEDISEFRepeatingPattern;
begin
  Result := AddRepeatingPatternTo(Self);
end;

function TEDISEFCompositeElement.AppendRepeatingPattern(
  RepeatingPattern: TEDISEFRepeatingPattern): TEDISEFRepeatingPattern;
begin
  Result := AppendRepeatingPatternTo(Self, RepeatingPattern);
end;

procedure TEDISEFCompositeElement.DeleteRepeatingPattern(RepeatingPattern: TEDISEFRepeatingPattern);
begin
  DeleteRepeatingPatternFrom(Self, RepeatingPattern);
end;

function TEDISEFCompositeElement.ExtractRepeatingPattern(
  RepeatingPattern: TEDISEFRepeatingPattern): TEDISEFRepeatingPattern;
begin
  Result := ExtractRepeatingPatternFrom(Self, RepeatingPattern);
end;

function TEDISEFCompositeElement.InsertRepeatingPattern(RepeatingPattern: TEDISEFRepeatingPattern;
  BeforeObject: TEDISEFDataObject): TEDISEFRepeatingPattern;
begin
  Result := InsertRepeatingPatternInto(Self, RepeatingPattern, BeforeObject);
end;

function TEDISEFCompositeElement.InsertRepeatingPattern(
  BeforeObject: TEDISEFDataObject): TEDISEFRepeatingPattern;
begin
  Result := InsertRepeatingPatternInto(Self, BeforeObject);
end;

//=== { TEDISEFSegment } =====================================================

constructor TEDISEFSegment.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  if FParent is TEDISEFTable then
  begin
    FParentSet := TEDISEFSet(FParent.Parent);
    FParentTable := TEDISEFTable(FParent);
  end
  else
  if FParent is TEDISEFLoop then
  begin
    FParentSet := TEDISEFLoop(FParent).ParentSet;
    FParentTable := TEDISEFLoop(FParent).ParentTable;
  end
  else
  begin
    FParentSet := nil;
    FParentTable := nil;
  end;
  FOrdinal := -1;
  FPosition := -1;
  FPositionIncrement := 0;
  FResetPositionInc := False;
  FOutOfSequenceOrdinal := False;
  FRequirementDesignator := '';
  FMaximumUse := 0;
  FOwnerLoopId := NA_LoopId;
  FParentLoopId := NA_LoopId;
  FMaskNumber := -1;
  FMaskNumberSpecified := False;
  FExtendedData := '';
  FEDISEFTextSets := TEDISEFTextSets.Create(False);
end;

destructor TEDISEFSegment.Destroy;
begin
  inherited Destroy;
end;

function TEDISEFSegment.Assemble: string;
begin
  Result := '';
  if FParent is TEDISEFFile then
    Result := CombineSEGSDataOfSEGSDefinition(Self)
  else
  if (FParent is TEDISEFTable) or (FParent is TEDISEFLoop) then
    Result := CombineSEGSDataOfSETSDefinition(Self);
end;

procedure TEDISEFSegment.Assign(Segment: TEDISEFSegment);
var
  ListItem: TEDISEFDataObjectListItem;
  EDISEFDataObject: TEDISEFDataObject;
begin
  FEDISEFDataObjects.Clear;
  FRequirementDesignator := Segment.RequirementDesignator;
  FMaximumUse := Segment.MaximumUse;
  ListItem := Segment.Elements.First;
  while ListItem <> nil do
  begin
    EDISEFDataObject := nil;
    if ListItem.EDISEFDataObject <> nil then
    begin
      if ListItem.EDISEFDataObject is TEDISEFElement then
        EDISEFDataObject := TEDISEFElement(ListItem.EDISEFDataObject).Clone(Self)
      else
      if ListItem.EDISEFDataObject is TEDISEFCompositeElement then
        EDISEFDataObject := TEDISEFCompositeElement(ListItem.EDISEFDataObject).Clone(Self)
      else
      if ListItem.EDISEFDataObject is TEDISEFRepeatingPattern then
        EDISEFDataObject := TEDISEFRepeatingPattern(ListItem.EDISEFDataObject).Clone(Self);
    end
    else
      EDISEFDataObject := TEDISEFElement.Create(Self);
    FEDISEFDataObjects.Add(EDISEFDataObject, EDISEFDataObject.Id);
    ListItem := ListItem.NextItem;
  end;
end;

function TEDISEFSegment.CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject;
begin
  Result := Clone(NewParent);
end;

function TEDISEFSegment.Clone(NewParent: TEDISEFDataObject): TEDISEFSegment;
var
  ListItem: TEDISEFDataObjectListItem;
  EDISEFDataObject: TEDISEFDataObject;
begin
  Result := TEDISEFSegment.Create(NewParent);
  Result.SegmentId := FId;
  Result.RequirementDesignator := FRequirementDesignator;
  Result.MaximumUse := FMaximumUse;
  ListItem := FEDISEFDataObjects.First;
  while ListItem <> nil do
  begin
    if ListItem.EDISEFDataObject <> nil then
    begin
      if ListItem.EDISEFDataObject is TEDISEFElement then
        EDISEFDataObject := TEDISEFElement(ListItem.EDISEFDataObject).Clone(Result)
      else
      if ListItem.EDISEFDataObject is TEDISEFCompositeElement then
        EDISEFDataObject := TEDISEFCompositeElement(ListItem.EDISEFDataObject).Clone(Result)
      else
      if ListItem.EDISEFDataObject is TEDISEFRepeatingPattern then
        EDISEFDataObject := TEDISEFRepeatingPattern(ListItem.EDISEFDataObject).Clone(Result)
      else
        EDISEFDataObject := TEDISEFElement.Create(Result);
    end
    else
      EDISEFDataObject := TEDISEFElement.Create(Result);
    Result.EDISEFDataObjects.Add(EDISEFDataObject, EDISEFDataObject.Id);
    ListItem := ListItem.NextItem;
  end;
end;

procedure TEDISEFSegment.Disassemble;
begin
  if FParent is TEDISEFFile then
    ParseSEGSDataOfSEGSDefinition(FData, Self, FSEFFile)
  else
  if (FParent is TEDISEFTable) or (FParent is TEDISEFLoop) then
    ParseSEGSDataOfSETSDefinition(FData, Self, FSEFFile);
  UpdateOwnerItemName;    
  AssignElementOrdinals;
end;

function TEDISEFSegment.GetOwnerLoopId: string;
begin
  Result := NA_LoopId;
  if FParent is TEDISEFLoop then
    Result := FParent.Id;
end;

function TEDISEFSegment.GetParentLoopId: string;
begin
  Result := NA_LoopId;
  if FParent is TEDISEFLoop then
    Result := TEDISEFLoop(FParent).ParentLoopId;
end;

function TEDISEFSegment.GetTextSetsLocation: string;
begin
  Result := '';
  if FParentSet <> nil then
    Result := FParentSet.GetTextSetsLocation + '~' + IntToStr(FOrdinal);
end;

function TEDISEFSegment.GetElementObjectList: TObjectList;
begin
  Result := TObjectList.Create(False);
  ExtractFromDataObjectGroup([TEDISEFElement, TEDISEFCompositeElement], Self, Result);
end;

procedure TEDISEFSegment.AssignElementOrdinals;
var
  I: Integer;
  Element: TEDISEFElement;
  CompositeElement: TEDISEFCompositeElement;
  ElementList: TObjectList;
  AssignOrdinal: Integer;
begin
  ElementList := GetElementObjectList;
  try
    AssignOrdinal := 0;
    for I := 0 to ElementList.Count - 1 do
    begin
      if ElementList[I] is TEDISEFElement then
      begin
        Element := TEDISEFElement(ElementList[I]);
        if Element.Ordinal = -1 then
        begin
          Inc(AssignOrdinal);
          Element.Ordinal := AssignOrdinal;
        end
        else
        begin
          AssignOrdinal := Element.Ordinal;
          Element.OutOfSequenceOrdinal := True;
        end;
      end
      else
      if ElementList[I] is TEDISEFCompositeElement then
      begin
        CompositeElement := TEDISEFCompositeElement(ElementList[I]);
        if CompositeElement.Ordinal = -1 then
        begin
          Inc(AssignOrdinal);
          CompositeElement.Ordinal := AssignOrdinal;
        end
        else
        begin
          AssignOrdinal := CompositeElement.Ordinal;
          CompositeElement.OutOfSequenceOrdinal := True;
        end;
      end;
    end;
  finally
    ElementList.Free;
  end;
end;

procedure TEDISEFSegment.BindTextSets(TEXTSETS: TEDISEFTextSets);
begin
  FEDISEFTextSets.Free;
  FEDISEFTextSets := TEDISEFTextSets(TextSets.ReturnListItemsByName(GetTextSetsLocation));
end;

procedure TEDISEFSegment.BindElementTextSets;
var
  I: Integer;
  Element: TEDISEFElement;
  CompositeElement: TEDISEFCompositeElement;
  ElementList: TObjectList;
begin
  ElementList := GetElementObjectList;
  try
    for I := 0 to ElementList.Count - 1 do
    begin
      if ElementList[I] is TEDISEFElement then
      begin
        Element := TEDISEFElement(ElementList[I]);
        Element.BindTextSets(FSEFFile.TEXTSETS);
      end
      else
      if ElementList[I] is TEDISEFCompositeElement then
      begin
        CompositeElement := TEDISEFCompositeElement(ElementList[I]);
        CompositeElement.BindTextSets(FSEFFile.TEXTSETS);
      end;
    end; 
  finally
    ElementList.Free;
  end;
end;

function TEDISEFSegment.AddElement: TEDISEFElement;
begin
  Result := AddElementTo(Self);
end;

function TEDISEFSegment.AppendElement(Element: TEDISEFElement): TEDISEFElement;
begin
  Result := AppendElementTo(Self, Element);
end;

procedure TEDISEFSegment.DeleteElement(Element: TEDISEFElement);
begin
  DeleteElementFrom(Self, Element);
end;

function TEDISEFSegment.ExtractElement(Element: TEDISEFElement): TEDISEFElement;
begin
  Result := ExtractElementFrom(Self, Element);
end;

function TEDISEFSegment.InsertElement(BeforeObject: TEDISEFDataObject): TEDISEFElement;
begin
  Result := InsertElementInto(Self, BeforeObject);
end;

function TEDISEFSegment.InsertElement(Element: TEDISEFElement;
  BeforeObject: TEDISEFDataObject): TEDISEFElement;
begin
  Result := InsertElementInto(Self, Element, BeforeObject);
end;

function TEDISEFSegment.AddCompositeElement: TEDISEFCompositeElement;
begin
  Result := AddCompositeElementTo(Self);
end;

function TEDISEFSegment.AppendCompositeElement(
  CompositeElement: TEDISEFCompositeElement): TEDISEFCompositeElement;
begin
  Result := AppendCompositeElementTo(Self, CompositeElement);
end;

procedure TEDISEFSegment.DeleteCompositeElement(CompositeElement: TEDISEFCompositeElement);
begin
  DeleteCompositeElementFrom(Self, CompositeElement);
end;

function TEDISEFSegment.ExtractCompositeElement(
  CompositeElement: TEDISEFCompositeElement): TEDISEFCompositeElement;
begin
  Result := ExtractCompositeElementFrom(Self, CompositeElement);
end;

function TEDISEFSegment.InsertCompositeElement(
  BeforeObject: TEDISEFDataObject): TEDISEFCompositeElement;
begin
  Result := InsertCompositeElementInto(Self, BeforeObject);
end;

function TEDISEFSegment.InsertCompositeElement(CompositeElement: TEDISEFCompositeElement;
  BeforeObject: TEDISEFDataObject): TEDISEFCompositeElement;
begin
  Result := InsertCompositeElementInto(Self, CompositeElement, BeforeObject);
end;

function TEDISEFSegment.AddRepeatingPattern: TEDISEFRepeatingPattern;
begin
  Result := AddRepeatingPatternTo(Self);
end;

function TEDISEFSegment.AppendRepeatingPattern(
  RepeatingPattern: TEDISEFRepeatingPattern): TEDISEFRepeatingPattern;
begin
  Result := AppendRepeatingPatternTo(Self, RepeatingPattern);
end;

procedure TEDISEFSegment.DeleteRepeatingPattern(RepeatingPattern: TEDISEFRepeatingPattern);
begin
  DeleteRepeatingPatternFrom(Self, RepeatingPattern);
end;

function TEDISEFSegment.ExtractRepeatingPattern(
  RepeatingPattern: TEDISEFRepeatingPattern): TEDISEFRepeatingPattern;
begin
  Result := ExtractRepeatingPatternFrom(Self, RepeatingPattern);
end;

function TEDISEFSegment.InsertRepeatingPattern(RepeatingPattern: TEDISEFRepeatingPattern;
  BeforeObject: TEDISEFDataObject): TEDISEFRepeatingPattern;
begin
  Result := InsertRepeatingPatternInto(Self, RepeatingPattern, BeforeObject);
end;

function TEDISEFSegment.InsertRepeatingPattern(
  BeforeObject: TEDISEFDataObject): TEDISEFRepeatingPattern;
begin
  Result := InsertRepeatingPatternInto(Self, BeforeObject);
end;

//=== { TEDISEFLoop } ========================================================

constructor TEDISEFLoop.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  FMaximumRepeat := Value_UndefinedMaximum; 
end;

destructor TEDISEFLoop.Destroy;
begin
  inherited Destroy;
end;

function TEDISEFLoop.Assemble: string;
var
  ListItem: TEDISEFDataObjectListItem;
  Segment: TEDISEFSegment;
begin
  Result := '';
  ListItem := FEDISEFDataObjects.First;
  while ListItem <> nil do
  begin
    if ListItem.EDISEFDataObject is TEDISEFSegment then
    begin
      Segment := TEDISEFSegment(ListItem.EDISEFDataObject);
      if Segment.ResetPositionInc then
      begin
        if Segment.PositionIncrement > 0 then
          Result := Result + SEFDelimiter_PlusSign + IntToStr(Segment.PositionIncrement) +
            SEFDelimiter_OpeningBracket + Segment.Assemble + SEFDelimiter_ClosingBracket
        else
          Result := Result + IntToStr(Segment.PositionIncrement) + SEFDelimiter_OpeningBracket +
            Segment.Assemble + SEFDelimiter_ClosingBracket;
      end
      else
      begin
        Result := Result + SEFDelimiter_OpeningBracket + Segment.Assemble +
          SEFDelimiter_ClosingBracket;
      end;
    end
    else // is TEDISEFLoop
      Result := Result + SEFDelimiter_OpeningBrace + ListItem.EDISEFDataObject.Assemble +
        SEFDelimiter_ClosingBrace;
    ListItem := ListItem.NextItem;
  end;
  if FEDISEFDataObjects.Count > 0 then
  begin
    if FEDISEFDataObjects[0].Id <> FId then
    begin
      if FMaximumRepeat >= Value_UndefinedMaximum then
        Result := FId + SEFDelimiter_Colon + Value_GreaterThanOne + Result
      else
      if FMaximumRepeat > 1 then
        Result := FId + SEFDelimiter_Colon + IntToStr(FMaximumRepeat) + Result
      else
        Result := FId + Result;
    end
    else
    begin
      if FMaximumRepeat >= Value_UndefinedMaximum then
        Result := SEFDelimiter_Colon + Value_GreaterThanOne + Result
      else
      if FMaximumRepeat > 1 then
        Result := SEFDelimiter_Colon + IntToStr(FMaximumRepeat) + Result
      else
        Result := FId + Result;
    end;
  end
  else
    Result := FId + SEFDelimiter_Colon + IntToStr(FMaximumRepeat) + Result;
end;

function TEDISEFLoop.CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject;
begin
  Result := Clone(NewParent);
end;

function TEDISEFLoop.Clone(NewParent: TEDISEFDataObject): TEDISEFLoop;
begin
  Result := nil;
end;

procedure TEDISEFLoop.Disassemble;
begin
  // FParent is TEDISEFTable
  ParseLoopDataOfSETSDefinition(FData, Self, FSEFFile);
  UpdateOwnerItemName;  
end;

function TEDISEFLoop.GetParentLoopId: string;
begin
  Result := NA_LoopId;
  if FParent is TEDISEFLoop then
    Result := FParent.Id;
end;

function TEDISEFLoop.GetParentSet: TEDISEFSet;
var
  DataObject: TEDISEFDataObject;
begin
  Result := nil;
  DataObject := FParent;
  while DataObject <> nil do
  begin
    if DataObject is TEDISEFTable then
    begin
      Result := TEDISEFSet(DataObject.Parent);
      Break;
    end
    else
    if DataObject is TEDISEFLoop then
      DataObject := DataObject.Parent;
  end;
end;

function TEDISEFLoop.AddLoop: TEDISEFLoop;
begin
  Result := AddLoopTo(Self);
end;

function TEDISEFLoop.AddSegment: TEDISEFSegment;
begin
  Result := AddSegmentTo(Self);
end;

function TEDISEFLoop.AppendLoop(Loop: TEDISEFLoop): TEDISEFLoop;
begin
  Result := AppendLoopTo(Self, Loop);
end;

function TEDISEFLoop.AppendSegment(Segment: TEDISEFSegment): TEDISEFSegment;
begin
  Result := AppendSegmentTo(Self, Segment);
end;

procedure TEDISEFLoop.DeleteLoop(Loop: TEDISEFLoop);
begin
  DeleteLoopFrom(Self, Loop);
end;

procedure TEDISEFLoop.DeleteSegment(Segment: TEDISEFSegment);
begin
  DeleteSegmentFrom(Self, Segment);
end;

function TEDISEFLoop.ExtractLoop(Loop: TEDISEFLoop): TEDISEFLoop;
begin
  Result := ExtractLoopFrom(Self, Loop);
end;

function TEDISEFLoop.ExtractSegment(Segment: TEDISEFSegment): TEDISEFSegment;
begin
  Result := ExtractSegmentFrom(Self, Segment);
end;

function TEDISEFLoop.InsertLoop(Loop: TEDISEFLoop; BeforeObject: TEDISEFDataObject): TEDISEFLoop;
begin
  Result := InsertLoopInto(Self, Loop, BeforeObject);
end;

function TEDISEFLoop.InsertLoop(BeforeObject: TEDISEFDataObject): TEDISEFLoop;
begin
  Result := InsertLoopInto(Self, BeforeObject);
end;

function TEDISEFLoop.InsertSegment(Segment: TEDISEFSegment;
  BeforeObject: TEDISEFDataObject): TEDISEFSegment;
begin
  Result := InsertSegmentInto(Self, Segment, BeforeObject);
end;

function TEDISEFLoop.InsertSegment(BeforeObject: TEDISEFDataObject): TEDISEFSegment;
begin
  Result := InsertSegmentInto(Self, BeforeObject);
end;

function TEDISEFLoop.GetParentTable: TEDISEFTable;
var
  DataObject: TEDISEFDataObject;
begin
  Result := nil;
  DataObject := FParent;
  while DataObject <> nil do
  begin
    if DataObject is TEDISEFTable then
    begin
      Result := TEDISEFTable(DataObject);
      Break;
    end
    else
    if DataObject is TEDISEFLoop then
      DataObject := DataObject.Parent;
  end;
end;

//=== { TEDISEFTable } =======================================================

constructor TEDISEFTable.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
end;

destructor TEDISEFTable.Destroy;
begin
  inherited Destroy;
end;

function TEDISEFTable.Assemble: string;
var
  ListItem: TEDISEFDataObjectListItem;
  Segment: TEDISEFSegment;
begin
  Result := '';
  if FEDISEFDataObjects.Count > 0 then
    Result := SEFDelimiter_Caret;
  ListItem := FEDISEFDataObjects.First;
  while ListItem <> nil do
  begin
    if ListItem.EDISEFDataObject is TEDISEFSegment then
    begin
      Segment := TEDISEFSegment(ListItem.EDISEFDataObject);
      if Segment.ResetPositionInc then
      begin
        if Segment.PositionIncrement > 0 then
          Result := Result + SEFDelimiter_PlusSign + IntToStr(Segment.PositionIncrement) +
            SEFDelimiter_OpeningBracket + Segment.Assemble + SEFDelimiter_ClosingBracket
        else
          Result := Result + IntToStr(Segment.PositionIncrement) + SEFDelimiter_OpeningBracket +
            Segment.Assemble + SEFDelimiter_ClosingBracket;
      end
      else
      begin
        Result := Result + SEFDelimiter_OpeningBracket + Segment.Assemble +
          SEFDelimiter_ClosingBracket;
      end;
    end
    else // is TEDISEFLoop
      Result := Result + SEFDelimiter_OpeningBrace + ListItem.EDISEFDataObject.Assemble +
        SEFDelimiter_ClosingBrace;
    ListItem := ListItem.NextItem;
  end;
end;

function TEDISEFTable.CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject;
begin
  Result := Clone(NewParent);
end;

function TEDISEFTable.Clone(NewParent: TEDISEFDataObject): TEDISEFTable;
begin
  Result := nil;
end;

function TEDISEFTable.GetSEFSet: TEDISEFSet;
begin
  Result := nil;
  if FParent is TEDISEFSet then
    Result := TEDISEFSet(FParent);
end;

function TEDISEFTable.AddLoop: TEDISEFLoop;
begin
  Result := AddLoopTo(Self);
end;

function TEDISEFTable.AddSegment: TEDISEFSegment;
begin
  Result := AddSegmentTo(Self);
end;

function TEDISEFTable.AppendLoop(Loop: TEDISEFLoop): TEDISEFLoop;
begin
  Result := AppendLoopTo(Self, Loop);
end;

function TEDISEFTable.AppendSegment(Segment: TEDISEFSegment): TEDISEFSegment;
begin
  Result := AppendSegmentTo(Self, Segment);
end;

procedure TEDISEFTable.DeleteLoop(Loop: TEDISEFLoop);
begin
  DeleteLoopFrom(Self, Loop);
end;

procedure TEDISEFTable.DeleteSegment(Segment: TEDISEFSegment);
begin
  DeleteSegmentFrom(Self, Segment);
end;

function TEDISEFTable.ExtractLoop(Loop: TEDISEFLoop): TEDISEFLoop;
begin
  Result := ExtractLoopFrom(Self, Loop);
end;

function TEDISEFTable.ExtractSegment(Segment: TEDISEFSegment): TEDISEFSegment;
begin
  Result := ExtractSegmentFrom(Self, Segment);
end;

function TEDISEFTable.InsertLoop(Loop: TEDISEFLoop; BeforeObject: TEDISEFDataObject): TEDISEFLoop;
begin
  Result := InsertLoopInto(Self, Loop, BeforeObject);
end;

function TEDISEFTable.InsertLoop(BeforeObject: TEDISEFDataObject): TEDISEFLoop;
begin
  Result := InsertLoopInto(Self, BeforeObject);
end;

function TEDISEFTable.InsertSegment(Segment: TEDISEFSegment;
  BeforeObject: TEDISEFDataObject): TEDISEFSegment;
begin
  Result := InsertSegmentInto(Self, Segment, BeforeObject);
end;

function TEDISEFTable.InsertSegment(BeforeObject: TEDISEFDataObject): TEDISEFSegment;
begin
  Result := InsertSegmentInto(Self, BeforeObject);
end;

//=== { TEDISEFSet } =========================================================

constructor TEDISEFSet.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  FEDISEFTextSets := TEDISEFTextSets.Create(False);
end;

destructor TEDISEFSet.Destroy;
begin
  FEDISEFTextSets.Free;
  inherited Destroy;
end;

function TEDISEFSet.Assemble: string;
var
  ListItem: TEDISEFDataObjectListItem;
begin
  Result := FId + SEFDelimiter_EqualSign;
  ListItem := FEDISEFDataObjects.First;
  while ListItem <> nil do
  begin
    Result := Result + ListItem.EDISEFDataObject.Assemble;
    ListItem := ListItem.NextItem;
  end;
end;

function TEDISEFSet.CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject;
begin
  Result := Clone(NewParent);
end;

function TEDISEFSet.Clone(NewParent: TEDISEFDataObject): TEDISEFSet;
begin
  Result := nil;
end;

procedure TEDISEFSet.Disassemble;
begin
  // FParent is TEDISEFFile
  ParseSetsDataOfSETSDefinition(FData, Self, FSEFFile);
  UpdateOwnerItemName;  
  // Assign segment ordinals that were not explicitly defined
  AssignSegmentOrdinals;
  // Assign segment positions
  AssignSegmentPositions;
  // Bind TEXT,SETS to segments
  BindSegmentTextSets;
  // Misc
  if Tables.Count = 3 then
  begin
    // (rom) make resourcestrings?
    Table[0].Id := 'Heading';             
    Table[1].Id := 'Detail';
    Table[2].Id := 'Summary';
  end;
end;

function TEDISEFSet.GetEDISEFTable(Index: Integer): TEDISEFTable;
begin
  Result := TEDISEFTable(FEDISEFDataObjects[Index])
end;

procedure TEDISEFSet.BuildSegmentObjectListFromLoop(ObjectList: TObjectList; Loop: TEDISEFLoop);
var
  ListItem: TEDISEFDataObjectListItem;
  NestedLoop: TEDISEFLoop;
  Segment: TEDISEFSegment;
begin
  ListItem := Loop.EDISEFDataObjects.First;
  while ListItem <> nil do
  begin
    if ListItem.EDISEFDataObject is TEDISEFSegment then
    begin
      Segment := TEDISEFSegment(ListItem.EDISEFDataObject);
      ObjectList.Add(Segment)
    end
    else
    if ListItem.EDISEFDataObject is TEDISEFLoop then
    begin
      NestedLoop := TEDISEFLoop(ListItem.EDISEFDataObject);
      BuildSegmentObjectListFromLoop(ObjectList, NestedLoop);
    end;
    ListItem := ListItem.NextItem;
  end;
end;

function TEDISEFSet.GetSegmentObjectList: TObjectList;
var
  ListItem, ListItem2: TEDISEFDataObjectListItem;
  Table: TEDISEFTable;
  Loop: TEDISEFLoop;
begin
  Result := TObjectList.Create(False);
  ListItem := FEDISEFDataObjects.First;
  while ListItem <> nil do
  begin
    Table := TEDISEFTable(ListItem.EDISEFDataObject);
    ListItem2 := Table.EDISEFDataObjects.First;
    while ListItem2 <> nil do
    begin
      if ListItem2.EDISEFDataObject is TEDISEFSegment then
        Result.Add(ListItem2.EDISEFDataObject)
      else
      if ListItem2.EDISEFDataObject is TEDISEFLoop then
      begin
        Loop := TEDISEFLoop(ListItem2.EDISEFDataObject);
        BuildSegmentObjectListFromLoop(Result, Loop);
      end;
      ListItem2 := ListItem2.NextItem;
    end;
    ListItem := ListItem.NextItem;
  end;
end;

procedure TEDISEFSet.AssignSegmentOrdinals;
var
  I: Integer;
  Segment: TEDISEFSegment;
  SegmentList: TObjectList;
  AssignOrdinal: Integer;
begin
  SegmentList := GetSegmentObjectList;
  try
    AssignOrdinal := 0;
    for I := 0 to SegmentList.Count - 1 do
    begin
      Segment := TEDISEFSegment(SegmentList[I]);
      if Segment.Ordinal = -1 then
      begin
        Inc(AssignOrdinal);
        Segment.Ordinal := AssignOrdinal;
      end
      else
      begin
        AssignOrdinal := Segment.Ordinal;
        Segment.OutOfSequenceOrdinal := True;
      end;
    end;
  finally
    SegmentList.Free;
  end;
end;

function TEDISEFSet.GetTextSetsLocation: string;
begin
  Result := FId;
end;

procedure TEDISEFSet.BindTextSets(TEXTSETS: TEDISEFTextSets);
begin
  FEDISEFTextSets.Free;
  FEDISEFTextSets := TEDISEFTextSets(TextSets.ReturnListItemsByName(GetTextSetsLocation));
end;

procedure TEDISEFSet.BindSegmentTextSets;
var
  I: Integer;
  Segment: TEDISEFSegment;
  SegmentList: TObjectList;
begin
  SegmentList := GetSegmentObjectList;
  try
    for I := 0 to SegmentList.Count - 1 do
    begin
      Segment := TEDISEFSegment(SegmentList[I]);
      Segment.BindTextSets(FSEFFile.TEXTSETS);
      Segment.BindElementTextSets;
    end; 
  finally
    SegmentList.Free;
  end;
end;

function TEDISEFSet.AddTable: TEDISEFTable;
begin
  Result := AddTableTo(Self);
end;

function TEDISEFSet.InsertTable(Table, BeforeTable: TEDISEFTable): TEDISEFTable;
begin
  Result := InsertTableInto(Self, Table, BeforeTable);
end;

function TEDISEFSet.InsertTable(BeforeTable: TEDISEFTable): TEDISEFTable;
begin
  Result := InsertTableInto(Self, BeforeTable);
end;

function TEDISEFSet.ExtractTable(Table: TEDISEFTable): TEDISEFTable;
begin
  Result := ExtractTableFrom(Self, Table);
end;

function TEDISEFSet.AppendTable(Table: TEDISEFTable): TEDISEFTable;
begin
  Result := AppendTableTo(Self, Table);
end;

procedure TEDISEFSet.DeleteTable(Table: TEDISEFTable);
begin
  DeleteTableFrom(Self, Table);
end;

procedure TEDISEFSet.AssignSegmentPositions;
var
  SegmentList: TObjectList;
  I: Integer;
  Segment: TEDISEFSegment;
  Table: TEDISEFTable;
  AssignPosition: Integer;
  PositionIncrement: Integer;
begin
  SegmentList := GetSegmentObjectList;
  try
    Table := nil;
    AssignPosition := 0;
    PositionIncrement := 10;
    for I := 0 to SegmentList.Count - 1 do
    begin
      Segment := TEDISEFSegment(SegmentList[I]);
      if Table <> Segment.ParentTable then
      begin
        Table := Segment.ParentTable;
        AssignPosition := 0;
      end;
      if Segment.ResetPositionInc then
        PositionIncrement := Segment.PositionIncrement;
      AssignPosition := AssignPosition + PositionIncrement;
      Segment.Position := AssignPosition;
      Segment.PositionIncrement := PositionIncrement;
    end;
  finally
    SegmentList.Free;
  end;
end;

//=== { TEDISEFFile } ========================================================

constructor TEDISEFFile.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(nil);
  FEDISEFCodesList := TStringList.Create;
  FEDISEFElms := TEDISEFDataObjectList.Create;
  FEDISEFComs := TEDISEFDataObjectList.Create;
  FEDISEFSegs := TEDISEFDataObjectList.Create;
  FEDISEFSets := TEDISEFDataObjectList.Create;
  FEDISEFStd := TStringList.Create;
  FEDISEFIni := TStringList.Create;
  //
  FEDISEFTextSets := TEDISEFTextSets.Create;
end;

destructor TEDISEFFile.Destroy;
begin
  FEDISEFIni.Free;
  FEDISEFStd.Free;
  FEDISEFSets.Free;
  FEDISEFSegs.Free;
  FEDISEFComs.Free;
  FEDISEFElms.Free;
  FEDISEFCodesList.Free;
  FEDISEFTextSets.Free;
  inherited Destroy;
end;

function TEDISEFFile.Assemble: string;
var
  I: Integer;
begin
  Result := '';
  Result := Result + SectionTag_VER + AnsiSpace + FEDISEFVer + AnsiCrLf;
  Result := Result + SectionTag_INI + AnsiCrLf;
  Result := Result + INI.Text + AnsiCrLf;
  if STD.Text <> '' then
    Result := Result + SectionTag_STD + AnsiCrLf;
  Result := Result + STD.Text + AnsiCrLf;
  if FEDISEFSets.Count > 0 then
  begin
    Result := Result + SectionTag_SETS + AnsiCrLf;
    for I := 0 to FEDISEFSets.Count - 1 do
      Result := Result + FEDISEFSets[I].Assemble + AnsiCrLf;
  end;
  if FEDISEFSegs.Count > 0 then
  begin
    Result := Result + SectionTag_SEGS + AnsiCrLf;
    for I := 0 to FEDISEFSegs.Count - 1 do
      Result := Result + FEDISEFSegs[I].Assemble + AnsiCrLf;
  end;
  if FEDISEFComs.Count > 0 then
  begin
    Result := Result + SectionTag_COMS + AnsiCrLf;
    for I := 0 to FEDISEFComs.Count - 1 do
      Result := Result + FEDISEFComs[I].Assemble + AnsiCrLf;
  end;
  if FEDISEFElms.Count > 0 then
  begin
    Result := Result + SectionTag_ELMS + AnsiCrLf;
    for I := 0 to FEDISEFElms.Count - 1 do
      Result := Result + FEDISEFElms[I].Assemble + AnsiCrLf;
  end;
  if Codes.Text <> '' then
  begin
    Result := Result + SectionTag_CODES + AnsiCrLf;
    Result := Result + Codes.Text + AnsiCrLf;
  end;
  if FEDISEFTextSets.Count > 0 then
  begin
    Result := Result + SectionTag_TEXTSETS + AnsiCrLf;
    for I := 0 to FEDISEFTextSets.Count - 1 do
      if TEDISEFText(FEDISEFTextSets[I]).Text <> '' then
        Result := Result + TEDISEFText(FEDISEFTextSets[I]).Assemble + AnsiCrLf;
  end;
  FData := Result;
end;

function TEDISEFFile.CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject;
begin
  Result := Clone(NewParent);
end;

function TEDISEFFile.Clone(NewParent: TEDISEFDataObject): TEDISEFFile;
begin
  Result := nil;
end;

procedure TEDISEFFile.Disassemble;
begin
  // Must parse file in reverse order to build specification from the dictionary values
  // .TEXT,SETS
  ParseTextSets;   
  // .CODES
  ParseCodes;
  // .ELMS
  ParseELMS;
  // .COMS
  ParseCOMS;
  // .SEGS
  ParseSEGS;
  // .SETS
  ParseSETS;
  // .STD
  ParseSTD;
  // .INI
  ParseINI;
  // .VER
  ParseVER;
end;

procedure TEDISEFFile.LoadFromFile(const FileName: string);
begin
  if FileName <> '' then
    FFileName := FileName;
  LoadFromFile;
end;

procedure TEDISEFFile.LoadFromFile;
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
  end
  else
    raise EJclEDIError.CreateID(1);
end;

procedure TEDISEFFile.ParseTextSets;
var
  TempList: TStringList;
  SearchResult, SearchResult2, I: Integer;
  TextSet: TEDISEFTextSet;
begin
  TempList := TStringList.Create;
  try
    FEDISEFTextSets.Clear;
    SearchResult := StrSearch(SectionTag_TEXTSETS, FData, 1);
    if SearchResult > 0 then
    begin
      SearchResult := SearchResult + Length(SectionTag_TEXTSETS + AnsiCrLf);
      SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
      if SearchResult2 <> 0 then
        TempList.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
      else
        TempList.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      for I := 0 to TempList.Count - 1 do
      begin
        TextSet := TEDISEFTextSet.Create;
        TextSet.Data := TempList[I];
        if TextSet.Data <> '' then
          TextSet.Disassemble;
        FEDISEFTextSets.Add(TextSet, TextSet.Where);
      end;
    end;
  finally
    TempList.Free;
  end;
end;

procedure TEDISEFFile.ParseCodes;
var
  SearchResult, SearchResult2: Integer;
begin
  Codes.Clear;
  SearchResult := StrSearch(SectionTag_CODES, FData, 1);
  if SearchResult > 0 then
  begin
    SearchResult := SearchResult + Length(SectionTag_CODES + AnsiCrLf);
    SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
    if SearchResult2 <> 0 then
      Codes.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
    else
      Codes.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
  end;
end;

procedure TEDISEFFile.ParseCOMS;
var
  TempList: TStringList;
  SearchResult, SearchResult2, I: Integer;
  CompositeElement: TEDISEFCompositeElement;
begin
  TempList := TStringList.Create;
  try
    FEDISEFComs.Clear;
    SearchResult := StrSearch(SectionTag_COMS, FData, 1);
    if SearchResult > 0 then
    begin
      SearchResult := SearchResult + Length(SectionTag_COMS + AnsiCrLf);
      SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
      if SearchResult2 <> 0 then
        TempList.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
      else
        TempList.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      for I := 0 to TempList.Count - 1 do
      begin
        CompositeElement := TEDISEFCompositeElement.Create(Self);
        FEDISEFComs.Add(CompositeElement);        
        CompositeElement.Data := TempList[I];
        CompositeElement.SEFFile := Self;
        if CompositeElement.Data <> '' then
          CompositeElement.Disassemble;
      end;
    end;
  finally
    TempList.Free;
  end;
end;

procedure TEDISEFFile.ParseELMS;
var
  TempList: TStringList;
  SearchResult, SearchResult2, I: Integer;
  Element: TEDISEFElement;
begin
  TempList := TStringList.Create;
  try
    FEDISEFElms.Clear;
    SearchResult := StrSearch(SectionTag_ELMS, FData, 1);
    if SearchResult > 0 then
    begin
      SearchResult := SearchResult + Length(SectionTag_ELMS + AnsiCrLf);
      SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
      if SearchResult2 <> 0 then
        TempList.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
      else
        TempList.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      for I := 0 to TempList.Count - 1 do
      begin
        Element := TEDISEFElement.Create(Self);
        FEDISEFElms.Add(Element);        
        Element.Data := TempList[I];
        Element.SEFFile := Self;
        if Element.Data <> '' then
          Element.Disassemble;
      end;
    end;
  finally
    TempList.Free;
  end;
end;

procedure TEDISEFFile.ParseINI;
var
  SearchResult, SearchResult2: Integer;
begin
  INI.Clear;
  {$IFDEF COMPILER6_UP}
  INI.Delimiter := SEFDelimiter_Comma;
  {$ELSE}
  // TODO : (rom) ?
  {$ENDIF COMPILER6_UP}
  SearchResult := StrSearch(SectionTag_INI, FData, 1);
  if SearchResult > 0 then
  begin
    SearchResult := SearchResult + Length(SectionTag_INI + AnsiCrLf);
    SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
    if SearchResult2 <> 0 then
      INI.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
    else
      INI.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
  end;
  FId := INI.Text;
end;

procedure TEDISEFFile.ParseSEGS;
var
  TempList: TStringList;
  SearchResult, SearchResult2, I: Integer;
  Segment: TEDISEFSegment;
begin
  TempList := TStringList.Create;
  try
    FEDISEFSegs.Clear;
    SearchResult := StrSearch(SectionTag_SEGS, FData, 1);
    if SearchResult > 0 then
    begin
      SearchResult := SearchResult + Length(SectionTag_SEGS + AnsiCrLf);
      SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
      if SearchResult2 <> 0 then
        TempList.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
      else
        TempList.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      for I := 0 to TempList.Count - 1 do
      begin
        Segment := TEDISEFSegment.Create(Self);
        FEDISEFSegs.Add(Segment);        
        Segment.Data := TempList[I];
        Segment.SEFFile := Self;
        if Segment.Data <> '' then
          Segment.Disassemble;
      end;
    end;
  finally
    TempList.Free;
  end;
end;

procedure TEDISEFFile.ParseSETS;
var
  TempList: TStringList;
  SearchResult, SearchResult2, I: Integer;
  TransactionSet: TEDISEFSet;
begin
  TempList := TStringList.Create;
  try
    FEDISEFSets.Clear;
    SearchResult := StrSearch(SectionTag_SETS, FData, 1);
    if SearchResult > 0 then
    begin
      SearchResult := SearchResult + Length(SectionTag_SETS + AnsiCrLf);
      SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
      if SearchResult2 <> 0 then
        TempList.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult)
      else
        TempList.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      for I := 0 to TempList.Count - 1 do
      begin
        TransactionSet := TEDISEFSet.Create(Self);
        FEDISEFSets.Add(TransactionSet);        
        TransactionSet.Data := TempList[I];
        TransactionSet.SEFFile := Self;
        if TransactionSet.Data <> '' then
          TransactionSet.Disassemble;
        TransactionSet.BindTextSets(FEDISEFTextSets);
      end;
    end;
  finally
    TempList.Free;
  end;
end;

procedure TEDISEFFile.ParseSTD;
var
  SearchResult, SearchResult2: Integer;
begin
  STD.Clear;
  {$IFDEF COMPILER6_UP}
  STD.Delimiter := SEFDelimiter_Comma;
  {$ELSE}
  // TODO : (rom) ?
  {$ENDIF COMPILER6_UP}
  SearchResult := StrSearch(SectionTag_STD, FData, 1);
  if SearchResult > 0 then
  begin
    SearchResult := SearchResult + Length(SectionTag_STD + AnsiCrLf);
    SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
    if SearchResult2 <> 0 then
    begin
      {$IFDEF COMPILER6_UP}
      STD.DelimitedText := Copy(FData, SearchResult, SearchResult2 - SearchResult);
      {$ELSE}
      STD.Text := Copy(FData, SearchResult, SearchResult2 - SearchResult);
      {$ENDIF COMPILER6_UP}
    end
    else
    begin
      {$IFDEF COMPILER6_UP}
      STD.DelimitedText := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      {$ELSE}
      STD.Text := Copy(FData, SearchResult, (Length(FData) - SearchResult) + 1);
      {$ENDIF COMPILER6_UP}
    end;
  end;
end;

procedure TEDISEFFile.ParseVER;
var
  SearchResult, SearchResult2: Integer;
begin
  FEDISEFVer := '';
  SearchResult := StrSearch(SectionTag_VER, FData, 1);
  if SearchResult > 0 then
  begin
    SearchResult := SearchResult + Length(SectionTag_VER);
    SearchResult2 := StrSearch(AnsiCrLf + SEFDelimiter_Period, FData, SearchResult + 1);
    if SearchResult2 <> 0 then
      FEDISEFVer := Copy(FData, SearchResult + 1, (SearchResult2 - SearchResult) - 1)       
    else
      FEDISEFVer := Copy(FData, SearchResult + 1, (Length(FData) - SearchResult) - 2);
    if FEDISEFVer = '' then
      FEDISEFVer := Value_Version10;
  end;
end;

procedure TEDISEFFile.SaveToFile(const FileName: string);
begin
  FFileName := FileName;
  SaveToFile;
end;

procedure TEDISEFFile.SaveToFile;
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

procedure TEDISEFTable.Disassemble;
begin
  // FParent is TEDISEFSet
  ParseTableDataOfSETSDefinition(FData, Self, FSEFFile);
  UpdateOwnerItemName;  
end;

function TEDISEFFile.GetEDISEFCodesList: TStrings;
begin
  Result := FEDISEFCodesList;
end;

function TEDISEFFile.GetEDISEFStd: TStrings;
begin
  Result := FEDISEFStd;
end;

function TEDISEFFile.GetEDISEFIni: TStrings;
begin
  Result := FEDISEFIni;
end;

procedure TEDISEFFile.Unload;
begin
  Codes.Clear;
  FEDISEFElms.Clear;
  FEDISEFComs.Clear;
  FEDISEFSegs.Clear;
  FEDISEFSets.Clear;
  STD.Clear;
  INI.Clear;
  FEDISEFVer := '';
end;

//=== { TEDISEFRepeatingPattern } ============================================

constructor TEDISEFRepeatingPattern.Create(Parent: TEDISEFDataObject);
begin
  inherited Create(Parent);
  if Parent is TEDISEFRepeatingPattern then
    FBaseParent := TEDISEFRepeatingPattern(Parent).BaseParent
  else
    FBaseParent := Parent;
end;

destructor TEDISEFRepeatingPattern.Destroy;
begin
  inherited Destroy;
end;

function TEDISEFRepeatingPattern.Assemble: string;
var
  ListItem: TEDISEFDataObjectListItem;
begin
  Result := IntToStr(FRepeatCount);
  ListItem := FEDISEFDataObjects.First;
  while ListItem <> nil do
  begin
    if not (ListItem.EDISEFDataObject is TEDISEFRepeatingPattern) then
      Result := Result + SEFDelimiter_OpeningBracket + ListItem.EDISEFDataObject.Assemble +
        SEFDelimiter_ClosingBracket
    else
      Result := Result + SEFDelimiter_OpeningBrace + ListItem.EDISEFDataObject.Assemble +
        SEFDelimiter_ClosingBrace;
    ListItem := ListItem.NextItem;
  end;
end;

function TEDISEFRepeatingPattern.CloneDataObject(NewParent: TEDISEFDataObject): TEDISEFDataObject;
begin
  Result := Clone(NewParent);
end;

function TEDISEFRepeatingPattern.Clone(NewParent: TEDISEFDataObject): TEDISEFRepeatingPattern;
var
  ListItem: TEDISEFDataObjectListItem;
  SEFDataObject: TEDISEFDataObject;
begin
  Result := TEDISEFRepeatingPattern.Create(NewParent);
  Result.Id := FId;
  Result.RepeatCount := FRepeatCount;
  ListItem := FEDISEFDataObjects.First;
  while ListItem <> nil do
  begin
    if ListItem.EDISEFDataObject <> nil then
    begin
      SEFDataObject := ListItem.EDISEFDataObject;
      if ListItem.EDISEFDataObject is TEDISEFElement then
        SEFDataObject := TEDISEFElement(SEFDataObject).Clone(Result)
      else
      if ListItem.EDISEFDataObject is TEDISEFCompositeElement then
        SEFDataObject := TEDISEFCompositeElement(SEFDataObject).Clone(Result)
      else
      if ListItem.EDISEFDataObject is TEDISEFSegment then
        SEFDataObject := TEDISEFSegment(SEFDataObject).Clone(Result)
      else
      if ListItem.EDISEFDataObject is TEDISEFRepeatingPattern then
        SEFDataObject := TEDISEFRepeatingPattern(SEFDataObject).Clone(Result);
      Result.EDISEFDataObjects.Add(SEFDataObject, SEFDataObject.Id);
    end;
    ListItem := ListItem.NextItem;
  end;
end;

procedure TEDISEFRepeatingPattern.Disassemble;
begin
  FEDISEFDataObjects.Clear;
  FId := FData;
  if FParent is TEDISEFCompositeElement then
    InternalParseCOMSDataOfCOMSDefinition(FData, Self, FSEFFile.ELMS)
  else
  if FParent is TEDISEFSegment then
    InternalParseSEGSDataOfSEGSDefinition(FData, Self, FSEFFile)
  else
  if FParent is TEDISEFRepeatingPattern then
  begin
    if FBaseParent is TEDISEFCompositeElement then
      InternalParseCOMSDataOfCOMSDefinition(FData, Self, FSEFFile.ELMS)
    else
    if FBaseParent is TEDISEFSegment then
      InternalParseSEGSDataOfSEGSDefinition(FData, Self, FSEFFile);
  end;
  UpdateOwnerItemName;  
end;

procedure TEDISEFRepeatingPattern.SetParent(const Value: TEDISEFDataObject);
begin
  inherited SetParent(Value);
  if Value is TEDISEFRepeatingPattern then
    FBaseParent := TEDISEFRepeatingPattern(Value).BaseParent
  else
    FBaseParent := Value;
end;

function TEDISEFRepeatingPattern.AddRepeatingPattern: TEDISEFRepeatingPattern;
begin
  Result := AddRepeatingPatternTo(Self);
end;

function TEDISEFRepeatingPattern.AppendRepeatingPattern(
  RepeatingPattern: TEDISEFRepeatingPattern): TEDISEFRepeatingPattern;
begin
  Result := AppendRepeatingPatternTo(Self, RepeatingPattern);
end;

procedure TEDISEFRepeatingPattern.DeleteRepeatingPattern(RepeatingPattern: TEDISEFRepeatingPattern);
begin
  DeleteRepeatingPatternFrom(Self, RepeatingPattern);
end;

function TEDISEFRepeatingPattern.ExtractRepeatingPattern(
  RepeatingPattern: TEDISEFRepeatingPattern): TEDISEFRepeatingPattern;
begin
  Result := ExtractRepeatingPatternFrom(Self, RepeatingPattern);
end;

function TEDISEFRepeatingPattern.InsertRepeatingPattern(RepeatingPattern: TEDISEFRepeatingPattern;
  BeforeObject: TEDISEFDataObject): TEDISEFRepeatingPattern;
begin
  Result := InsertRepeatingPatternInto(Self, RepeatingPattern, BeforeObject);
end;

function TEDISEFRepeatingPattern.InsertRepeatingPattern(
  BeforeObject: TEDISEFDataObject): TEDISEFRepeatingPattern;
begin
  Result := InsertRepeatingPatternInto(Self, BeforeObject);
end;

//=== { TEDISEFText } ========================================================

constructor TEDISEFText.Create;
begin
  inherited Create;
  FEDISEFWhereType := twUnknown;
  FData := '';
  FWhere := '';
  FWhat := '';
  FText := '';
  FWhereLocation := TStringList.Create;
end;

destructor TEDISEFText.Destroy;
begin
  FWhereLocation.Free;
  inherited Destroy;
end;

function TEDISEFText.Assemble: string;
var
  I: Integer;
begin
  FWhere := '';
  for I := 0 to WhereLocation.Count - 1 do
  begin
    if (FWhere <> '') and (WhereLocation[I] <> '') then
      FWhere := FWhere + '~';
    FWhere := FWhere + WhereLocation[I];
  end;
  Result := FWhere + ',' + FWhat + ',' + FText;
end;

procedure TEDISEFText.Disassemble;
var
  SearchResult, SearchResult2: Integer;
begin
  FEDISEFWhereType := twUnknown;
  SearchResult := StrSearch(',', FData, 1);
  FWhere := Copy(FData, 1, SearchResult - 1);
  WhereLocation.Text := Copy(FData, 1, SearchResult - 1);
  WhereLocation.CommaText := JclEDI.StringReplace(WhereLocation.Text, '~', ',', [rfReplaceAll]);
  SearchResult2 := StrSearch(',', FData, SearchResult + 1);
  FWhat := Copy(FData, SearchResult + 1, (SearchResult2 - SearchResult) - 1);
  if SearchResult2 > 0 then
    FText := Copy(FData, SearchResult2 + 1, Length(FData) - SearchResult2);
end;

function TEDISEFText.GetData: string;
begin
  Result := FData;
end;

function TEDISEFText.GetWhereLocation: TStrings;
begin
  Result := FWhereLocation;
end;

function TEDISEFText.GetText: string;
begin
  Result := FText;
  Result := JclEDI.StringReplace(Result, SEFTextCRLF, AnsiCrLf, [rfReplaceAll]);
  Result := JclEDI.StringReplace(Result, SEFTextCR, AnsiCarriageReturn, [rfReplaceAll]);
  Result := JclEDI.StringReplace(Result, SEFTextLF, AnsiLineFeed, [rfReplaceAll]);
end;

function TEDISEFText.GetDescription: string;
begin
  Result := '';
  case FEDISEFWhereType of
    twSet:
      case FWhat[1] of
        SEFTextSetsCode_Set0: Result := SEFTextSetsCode_Set0_Desc;
        SEFTextSetsCode_Set1: Result := SEFTextSetsCode_Set1_Desc;
        SEFTextSetsCode_Set2: Result := SEFTextSetsCode_Set2_Desc;
        SEFTextSetsCode_Set3: Result := SEFTextSetsCode_Set3_Desc;
        SEFTextSetsCode_Set4: Result := SEFTextSetsCode_Set4_Desc;
        SEFTextSetsCode_Set5: Result := SEFTextSetsCode_Set5_Desc;
      end;
    twSegment:
      case FWhat[1] of
        SEFTextSetsCode_Seg0: Result := SEFTextSetsCode_Seg0_Desc;
        SEFTextSetsCode_Seg1: Result := SEFTextSetsCode_Seg1_Desc;
        SEFTextSetsCode_Seg2: Result := SEFTextSetsCode_Seg2_Desc;
        SEFTextSetsCode_Seg3: Result := SEFTextSetsCode_Seg3_Desc;
        SEFTextSetsCode_Seg4: Result := SEFTextSetsCode_Seg4_Desc;
        SEFTextSetsCode_Seg5: Result := SEFTextSetsCode_Seg5_Desc;
        SEFTextSetsCode_Seg6: Result := SEFTextSetsCode_Seg6_Desc;
        SEFTextSetsCode_Seg7: Result := SEFTextSetsCode_Seg7_Desc;
      end;
    twElementOrCompositeElement, twSubElement:
      case FWhat[1] of
        SEFTextSetsCode_Elm0: Result := SEFTextSetsCode_Elm0_Desc;
        SEFTextSetsCode_Elm1: Result := SEFTextSetsCode_Elm1_Desc;
        SEFTextSetsCode_Elm2: Result := SEFTextSetsCode_Elm2_Desc;
        SEFTextSetsCode_Elm4: Result := SEFTextSetsCode_Elm4_Desc;
      end;
  end;
end;

procedure TEDISEFText.SetData(const Value: string);
begin
  FData := Value;
end;

procedure TEDISEFText.SetText(const Value: string);
var
  Temp: string;
begin
  Temp := Value;
  Temp := JclEDI.StringReplace(Temp, AnsiCrLf, SEFTextCRLF, [rfReplaceAll]);
  Temp := JclEDI.StringReplace(Temp, AnsiCarriageReturn, SEFTextCR, [rfReplaceAll]);
  Temp := JclEDI.StringReplace(Temp, AnsiLineFeed, SEFTextLF, [rfReplaceAll]);
  FText := Temp;
end;

//=== { TEDISEFTextSet } =====================================================

constructor TEDISEFTextSet.Create;
begin
  inherited Create;
  FWhereSet := '';
  FWhereSegment := -1;
  FWhereElement := -1;
  FWhereSubElement := -1;
end;

destructor TEDISEFTextSet.Destroy;
begin
  inherited Destroy;
end;

function TEDISEFTextSet.Assemble: string;
begin
  Result := inherited Assemble;
end;

procedure TEDISEFTextSet.Disassemble;
begin
  FWhereSet := '';
  FWhereSegment := -1;
  FWhereElement := -1;
  FWhereSubElement := -1;
  inherited Disassemble;
  if WhereLocation.Count >= 1 then
  begin
    FEDISEFWhereType := twSet;
    FWhereSet := WhereLocation[0];
  end;
  if WhereLocation.Count >= 2 then
  begin
    FEDISEFWhereType := twSegment;
    FWhereSegment := StrToInt(WhereLocation[1]);
  end;
  if WhereLocation.Count >= 3 then
  begin
    FEDISEFWhereType := twElementOrCompositeElement;
    try
      if WhereLocation[2][1] in AnsiDecDigits then
        FWhereElement := StrToInt(WhereLocation[2]);
    except
      // Eat this error if it occurs for now
    end;
  end;
  if WhereLocation.Count >= 4 then
  begin
    FEDISEFWhereType := twSubElement;
    try
      if WhereLocation[3][1] in AnsiDecDigits then
        FWhereSubElement := StrToInt(WhereLocation[3]);
    except
      // Eat this error if it occurs for now
    end;
  end;
end;

//=== { TEDISEFTextSets } ====================================================

function TEDISEFTextSets.GetText(Code: string): string;
var
  ListItem: TEDIObjectListItem;
  TextSet: TEDISEFTextSet;
begin
  Result := '';
  ListItem := FFirstItem;
  while ListItem <> nil do
  begin
    TextSet := TEDISEFTextSet(ListItem.EDIObject);
    if TextSet.What = Code then
    begin
      Result := TextSet.Text;
      Break;
    end;
    ListItem := ListItem.NextItem;
  end;
end;

procedure TEDISEFTextSets.SetText(EDISEFFile: TEDISEFFile; Location, Code, Text: string);
var
  ListItem: TEDIObjectListItem;
  TextSet: TEDISEFTextSet;
  Found: Boolean;
begin
  Found := False;
  ListItem := FFirstItem;
  while ListItem <> nil do
  begin
    TextSet := TEDISEFTextSet(ListItem.EDIObject);
    if TextSet.What = Code then
    begin
      TextSet.Text := Text;
      Break;
    end;
    ListItem := ListItem.NextItem;
  end;
  // If the item is not found then it will be created
  if (not Found) and (Text <> '') then
  begin
    TextSet := TEDISEFTextSet.Create;
    TextSet.Data := Location + ',' + Code + ',' + Text;
    TextSet.Disassemble;
    Add(TextSet, TextSet.Where);
    EDISEFFile.TEXTSETS.Add(TextSet, TextSet.Where);
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
