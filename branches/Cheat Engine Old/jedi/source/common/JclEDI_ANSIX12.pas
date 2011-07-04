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
{ The Original Code is JclEDI_ANSIX12.pas.                                                         }
{                                                                                                  }
{ The Initial Developer of the Original Code is Raymond Alexander.                                 }
{ Portions created by Raymond Alexander are Copyright Raymond Alexander. All rights reserved.      }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Raymond Alexander (rayspostbox3), Robert Marquardt, Robert Rossmair, Petr Vones                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains classes to eaisly parse EDI documents and data. Variable delimiter detection allows     }
{ parsing of the file without knowledge of the standards at an Interchange level.  This enables    }
{ parsing and construction of EDI documents with different delimiters.                             }
{                                                                                                  }
{ Unit owner: Raymond Alexander                                                                    }
{ Date created: May 22, 2003                                                                       }
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

unit JclEDI_ANSIX12;

{$I jcl.inc}

{$IFDEF EDI_WEAK_PACKAGE_UNITS}
  {$IFDEF SUPPORTS_WEAKPACKAGEUNIT}
    {$WEAKPACKAGEUNIT ON}
  {$ENDIF SUPPORTS_WEAKPACKAGEUNIT}
{$ENDIF EDI_WEAK_PACKAGE_UNITS}

// (Default) Enable the following directive to use the optimized JclEDI.StringReplace function.
{$DEFINE OPTIMIZED_STRINGREPLACE}

interface

uses
  SysUtils, Classes,
  {$IFNDEF EDI_WEAK_PACKAGE_UNITS}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$ENDIF ~EDI_WEAK_PACKAGE_UNITS}
  JclEDI;

const
  //  ANSI X12 Segment Id's
  ICHSegmentId = 'ISA'; // Interchange Control Header Segment Id
  ICTSegmentId = 'IEA'; // Interchange Control Trailer Segment Id
  FGHSegmentId = 'GS';  // Functional Group Header Segment Id
  FGTSegmentId = 'GE';  // Functional Group Trailer Segment Id
  TSHSegmentId = 'ST';  // Transaction Set Header Segment Id
  TSTSegmentId = 'SE';  // Transaction Set Trailer Segment Id
  TA1SegmentId = 'TA1'; // Interchange Acknowledgment Segment

  //  Reserved Data Field Names (TStringList Values)
  RDFN_Id = 'Id';
  RDFN_Position = 'Position';
  RDFN_Description = 'Description';
  RDFN_Notes = 'Notes';
  RDFN_Section = 'Section';                     // For Segment Only
  RDFN_RequirementDesignator = 'RequirementDesignator';
  RDFN_MaximumUsage = 'MaximumUsage';           // For Segment Only
  RDFN_OwnerLoopId = 'OwnerLoopId';             // ...
  RDFN_ParentLoopId = 'ParentLoopId';           // ...
  RDFN_MaximumLoopRepeat = 'MaximumLoopRepeat'; // For Loop however saved in segment that begins loop
  RDFN_Type = 'Type';                           // For Element Only
  RDFN_MinimumLength = 'MinimumLength';         // ...
  RDFN_MaximumLength = 'MaximumLength';         // ...

  RDFN_TransSetId = 'TransSetId';               // For Segment ST Only
  RDFN_TransSetDesc = 'TransSetDesc';           // ...

  RDFN_FunctionalGroupId = 'FunctionalGroupId'; // For Segment GS Only
  RDFN_FGDescription = 'FGDescription';         // ...
  RDFN_AgencyCodeId = 'AgencyCodeId';           // ...
  RDFN_VersionReleaseId = 'VersionReleaseId';   // ...

  RDFN_StandardId = 'StandardId';               // For Segment ISA Only
  RDFN_VersionId = 'VersionId';                 // ...
  RDFN_ICDescription = 'ICDescription';         // ...

type
  //  EDI Forward Class Declarations
  TEDIElement = class;
  TEDISegment = class;
  TEDITransactionSet = class;
  TEDIFunctionalGroup = class;
  TEDIInterchangeControl = class;
  TEDIFile = class;

  //  EDI Element
  TEDIElement = class(TEDIDataObject)
  public
    constructor Create(Parent: TEDIDataObject); reintroduce;
    function Assemble: string; override;
    procedure Disassemble; override;
    function GetIndexPositionFromParent: Integer;
  end;

  TEDIElementArray = array of TEDIElement;

  //  EDI Element Specification
  TEDIElementSpec = class(TEDIElement)
  private
    FReservedData: TStringList;
    FElementId: string;
    FPosition: Integer;
    FDescription: string;
    FNotes: string;
    FRequirementDesignator: string;
    FType: string;
    FMinimumLength: Integer;
    FMaximumLength: Integer;
    function GetReservedData: TStrings;
  public
    constructor Create(Parent: TEDIDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
  published
    property ReservedData: TStrings read GetReservedData;
    property Id: string read FElementId write FElementId;
    property ElementId: string read FElementId write FElementId;
    property Position: Integer read FPosition write FPosition;
    property Description: string read FDescription write FDescription;
    property Notes: string read FNotes write FNotes;
    property RequirementDesignator: string read FRequirementDesignator write FRequirementDesignator;
    property ElementType: string read FType write FType;
    property MinimumLength: Integer read FMinimumLength write FMinimumLength;
    property MaximumLength: Integer read FMaximumLength write FMaximumLength;
  end;

  //  EDI Segment Classes
  TEDISegment = class(TEDIDataObjectGroup)
  private
    function GetElement(Index: Integer): TEDIElement;
    procedure SetElement(Index: Integer; Element: TEDIElement);
  protected
    FSegmentId: string;
    function InternalCreateElement: TEDIElement; virtual;
    function InternalAssignDelimiters: TEDIDelimiters; override;
    function InternalCreateEDIDataObject: TEDIDataObject; override;
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
    destructor Destroy; override;
    //
    function AddElement: Integer;
    function AppendElement(Element: TEDIElement): Integer;
    function InsertElement(InsertIndex: Integer): Integer; overload;
    function InsertElement(InsertIndex: Integer; Element: TEDIElement): Integer; overload;
    procedure DeleteElement(Index: Integer); overload;
    procedure DeleteElement(Element: TEDIElement); overload;
    //
    function AddElements(Count: Integer): Integer;
    function AppendElements(ElementArray: TEDIElementArray): Integer;
    function InsertElements(InsertIndex, Count: Integer): Integer; overload;
    function InsertElements(InsertIndex: Integer;
      ElementArray: TEDIElementArray): Integer; overload;
    procedure DeleteElements; overload;
    procedure DeleteElements(Index, Count: Integer); overload;
    //
    function Assemble: string; override;
    procedure Disassemble; override;
    //
    property Element[Index: Integer]: TEDIElement read GetElement write SetElement; default;
    property Elements: TEDIDataObjectList read FEDIDataObjects;
  published
    property SegmentId: string read FSegmentId write FSegmentId;
    property ElementCount: Integer read GetCount;
  end;

  TEDISegmentArray = array of TEDISegment;

  TEDITransactionSetSegment = class(TEDISegment)
  protected
    function InternalAssignDelimiters: TEDIDelimiters; override;
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
  end;

  TEDIFunctionalGroupSegment = class(TEDISegment)
  protected
    function InternalAssignDelimiters: TEDIDelimiters; override;
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
  end;

  TEDIInterchangeControlSegment = class(TEDISegment)
  protected
    function InternalAssignDelimiters: TEDIDelimiters; override;
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
  end;

  //  EDI Segment Specification Classes
  TEDISegmentSpec = class(TEDISegment)
  private
    FReservedData: TStringList;
    FPosition: Integer;
    FDescription: string;
    FNotes: string;
    FSection: string;
    FRequirementDesignator: string;
    FMaximumUsage: Integer;
    FOwnerLoopId: string;
    FParentLoopId: string;
    function GetReservedData: TStrings;
  protected
    function InternalCreateElement: TEDIElement; override;
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
    destructor Destroy; override;
    procedure AssembleReservedData(ReservedData: TStrings); virtual;
    procedure DisassembleReservedData(ReservedData: TStrings); virtual;
    function Assemble: string; override;
    procedure Disassemble; override;
    procedure ValidateElementIndexPositions;
  published
    property ReservedData: TStrings read GetReservedData;
    property Id: string read FSegmentId write FSegmentId;
    property Position: Integer read FPosition write FPosition;
    property Description: string read FDescription write FDescription;
    property Notes: string read FNotes write FNotes;
    property Section: string read FSection write FSection;
    property RequirementDesignator: string read FRequirementDesignator write FRequirementDesignator;
    property MaximumUsage: Integer read FMaximumUsage write FMaximumUsage;
    property OwnerLoopId: string read FOwnerLoopId write FOwnerLoopId;
    property ParentLoopId: string read FParentLoopId write FParentLoopId;
  end;

  TEDITransactionSetSegmentSpec = class(TEDISegmentSpec)
  protected
    function InternalAssignDelimiters: TEDIDelimiters; override;
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
  end;

  TEDITransactionSetSegmentSTSpec = class(TEDITransactionSetSegmentSpec)
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
    procedure AssembleReservedData(ReservedData: TStrings); override;
    procedure DisassembleReservedData(ReservedData: TStrings); override;
  end;

  TEDIFunctionalGroupSegmentSpec = class(TEDISegmentSpec)
  protected
    function InternalAssignDelimiters: TEDIDelimiters; override;
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
  end;

  TEDIFunctionalGroupSegmentGSSpec = class(TEDIFunctionalGroupSegmentSpec)
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
    procedure AssembleReservedData(ReservedData: TStrings); override;
    procedure DisassembleReservedData(ReservedData: TStrings); override;
  end;

  TEDIInterchangeControlSegmentSpec = class(TEDISegmentSpec)
  protected
    function InternalAssignDelimiters: TEDIDelimiters; override;
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
  end;

  TEDIInterchangeControlSegmentISASpec = class(TEDIInterchangeControlSegmentSpec)
  public
    constructor Create(Parent: TEDIDataObject; ElementCount: Integer = 0); reintroduce;
    function Assemble: string; override;
    procedure Disassemble; override;
    procedure AssembleReservedData(ReservedData: TStrings); override;
    procedure DisassembleReservedData(ReservedData: TStrings); override;
  end;

  //  EDI Transaction Set
  TEDITransactionSet = class(TEDIDataObjectGroup)
  private
    FSTSegment: TEDISegment;
    FSESegment: TEDISegment;
    function GetSegment(Index: Integer): TEDISegment;
    procedure SetSegment(Index: Integer; Segment: TEDISegment);
    procedure SetSTSegment({$IFNDEF BCB6} const {$ENDIF} STSegment: TEDISegment);
    procedure SetSESegment({$IFNDEF BCB6} const {$ENDIF} SESegment: TEDISegment);
  protected
    procedure InternalCreateHeaderTrailerSegments; virtual;
    function InternalCreateSegment: TEDISegment; virtual;
    function InternalAssignDelimiters: TEDIDelimiters; override;
    function InternalCreateEDIDataObject: TEDIDataObject; override;
  public
    constructor Create(Parent: TEDIDataObject; SegmentCount: Integer = 0); reintroduce;
    destructor Destroy; override;

    function AddSegment: Integer;
    function AppendSegment(Segment: TEDISegment): Integer;
    function InsertSegment(InsertIndex: Integer): Integer; overload;
    function InsertSegment(InsertIndex: Integer; Segment: TEDISegment): Integer; overload;
    procedure DeleteSegment(Index: Integer); overload;
    procedure DeleteSegment(Segment: TEDISegment); overload;

    function AddSegments(Count: Integer): Integer;
    function AppendSegments(SegmentArray: TEDISegmentArray): Integer;
    function InsertSegments(InsertIndex, Count: Integer): Integer; overload;
    function InsertSegments(InsertIndex: Integer;
      SegmentArray: TEDISegmentArray): Integer; overload;
    procedure DeleteSegments; overload;
    procedure DeleteSegments(Index, Count: Integer); overload;

    function Assemble: string; override;
    procedure Disassemble; override;

    property Segment[Index: Integer]: TEDISegment read GetSegment write SetSegment; default;
    property Segments: TEDIDataObjectList read FEDIDataObjects;
  published
    property SegmentST: TEDISegment read FSTSegment write SetSTSegment;
    property SegmentSE: TEDISegment read FSESegment write SetSESegment;
    property SegmentCount: Integer read GetCount;
  end;

  TEDITransactionSetArray = array of TEDITransactionSet;

  //  EDI Transaction Set Specification
  TEDITransactionSetSpec = class(TEDITransactionSet)
  private
    FTransactionSetId: string;
    FTSDescription: string;
  protected
    procedure InternalCreateHeaderTrailerSegments; override;
    function InternalCreateSegment: TEDISegment; override;
  public
    procedure ValidateSegmentIndexPositions;
  published
    property Id: string read FTransactionSetId write FTransactionSetId;
    property TransactionSetId: string read FTransactionSetId write FTransactionSetId;
    property TSDescription: string read FTSDescription write FTSDescription;
  end;

  //  EDI Transaction Set Loop
  TEDITransactionSetLoop = class(TEDIDataObjectGroup)
  protected
    FOwnerLoopId: string;
    FParentLoopId: string;
    FParentTransactionSet: TEDITransactionSet;
    function InternalAssignDelimiters: TEDIDelimiters; override;
    function InternalCreateEDIDataObject: TEDIDataObject; override;
  public
    constructor Create(Parent: TEDIDataObject); reintroduce;
    destructor Destroy; override;
    function Assemble: string; override;
    procedure Disassemble; override;
    //
    //  ToDo:  More procedures and functions to manage internal structures
    //
    function FindLoop(LoopId: string; var StartIndex: Integer): TEDITransactionSetLoop;
    function FindSegment(SegmentId: string; var StartIndex: Integer): TEDISegment; overload;
    function FindSegment(SegmentId: string; var StartIndex: Integer;
      ElementConditions: TStrings): TEDISegment; overload;
    //
    function AddLoop(OwnerLoopId, ParentLoopId: string): Integer;
    procedure AppendSegment(Segment: TEDISegment);
    procedure DeleteEDIDataObjects;
  published
    property OwnerLoopId: string read FOwnerLoopId write FOwnerLoopId;
    property ParentLoopId: string read FParentLoopId write FParentLoopId;
    property ParentTransactionSet: TEDITransactionSet read FParentTransactionSet
      write FParentTransactionSet;
  end;

  //  EDI Transaction Set Document and related types and classes
  TEDITransactionSetDocumentOptions = set of (doLinkSpecToDataObject);

  TEDITransactionSetDocument = class(TEDITransactionSetLoop)
  private
  protected
    FErrorOccured: Boolean;
    FEDITSDOptions: TEDITransactionSetDocumentOptions;
    FEDILoopStack: TEDILoopStack;
    // References
    FEDITransactionSet: TEDITransactionSet;
    FEDITransactionSetSpec: TEDITransactionSetSpec;
    function ValidateSegSpecIndex(DataSegmentId: string; SpecStartIndex: Integer): Integer;
    function AdvanceSegSpecIndex(DataIndex, SpecStartIndex, SpecEndIndex: Integer): Integer;
    procedure AddLoopToDoc(StackRecord: TEDILoopStackRecord;
      SegmentId, OwnerLoopId, ParentLoopId: string; var EDIObject: TEDIObject);
    procedure SetSpecificationPointers(DataSegment, SpecSegment: TEDISegment);
  protected
    procedure ValidateData(TSDocument: TEDITransactionSetDocument;
      LoopStack: TEDILoopStack;
      DataSegment, SpecSegment: TEDISegment;
      var DataIndex, SpecIndex: Integer;
      var ErrorOccured: Boolean); virtual;
  public
    constructor Create(Parent: TEDIDataObject; EDITransactionSet: TEDITransactionSet;
      EDITransactionSetSpec: TEDITransactionSetSpec); reintroduce;
    destructor Destroy; override;
    //
    //  ToDo:  More procedures and functions to manage internal structures
    //
    procedure FormatDocument; virtual;
  published
    property EDITSDOptions: TEDITransactionSetDocumentOptions read FEDITSDOptions
      write FEDITSDOptions;
    property ErrorOccured: Boolean read FErrorOccured;
  end;

  TEDITransactionSetDocumentArray = array of TEDITransactionSetDocument;

  //  EDI Functional Group
  TEDIFunctionalGroup = class(TEDIDataObjectGroup)
  private
    FGSSegment: TEDISegment;
    FGESegment: TEDISegment;
    function GetTransactionSet(Index: Integer): TEDITransactionSet;
    procedure SetTransactionSet(Index: Integer; TransactionSet: TEDITransactionSet);
    procedure SetGSSegment(const GSSegment: TEDISegment);
    procedure SetGESegment(const GESegment: TEDISegment);
  protected
    procedure InternalCreateHeaderTrailerSegments; virtual;
    function InternalCreateTransactionSet: TEDITransactionSet; virtual;
    function InternalAssignDelimiters: TEDIDelimiters; override;
    function InternalCreateEDIDataObject: TEDIDataObject; override;
  public
    constructor Create(Parent: TEDIDataObject; TransactionSetCount: Integer = 0); reintroduce;
    destructor Destroy; override;

    function AddTransactionSet: Integer;
    function AppendTransactionSet(TransactionSet: TEDITransactionSet): Integer;
    function InsertTransactionSet(InsertIndex: Integer): Integer; overload;
    function InsertTransactionSet(InsertIndex: Integer;
      TransactionSet: TEDITransactionSet): Integer; overload;
    procedure DeleteTransactionSet(Index: Integer); overload;
    procedure DeleteTransactionSet(TransactionSet: TEDITransactionSet); overload;

    function AddTransactionSets(Count: Integer): Integer;
    function AppendTransactionSets(TransactionSetArray: TEDITransactionSetArray): Integer;
    function InsertTransactionSets(InsertIndex, Count: Integer): Integer; overload;
    function InsertTransactionSets(InsertIndex: Integer;
      TransactionSetArray: TEDITransactionSetArray): Integer; overload;
    procedure DeleteTransactionSets; overload;
    procedure DeleteTransactionSets(Index, Count: Integer); overload;

    function Assemble: string; override;
    procedure Disassemble; override;

    property TransactionSet[Index: Integer]: TEDITransactionSet read GetTransactionSet
      write SetTransactionSet; default;
    property TransactionSets: TEDIDataObjectList read FEDIDataObjects;
  published
    property SegmentGS: TEDISegment read FGSSegment write SetGSSegment;
    property SegmentGE: TEDISegment read FGESegment write SetGESegment;
    property TransactionSetCount: Integer read GetCount;
  end;

  TEDIFunctionalGroupArray = array of TEDIFunctionalGroup;

  //  EDI Functional Specification
  TEDIFunctionalGroupSpec = class(TEDIFunctionalGroup)
  private
    FFunctionalGroupId: string;
    FFGDescription: string;
    FAgencyCodeId: string;
    FVersionReleaseId: string;
  protected
    procedure InternalCreateHeaderTrailerSegments; override;
    function InternalCreateTransactionSet: TEDITransactionSet; override;
  public
    function FindTransactionSetSpec(TransactionSetId: string): TEDITransactionSetSpec;
  published
    property Id: string read FFunctionalGroupId write FFunctionalGroupId;
    property FunctionalGroupId: string read FFunctionalGroupId write FFunctionalGroupId;
    property FGDescription: string read FFGDescription write FFGDescription;
    property AgencyCodeId: string read FAgencyCodeId write FAgencyCodeId;
    property VersionReleaseId: string read FVersionReleaseId write FVersionReleaseId;
  end;

  //  EDI Interchange Control
  TEDIInterchangeControl = class(TEDIDataObjectGroup)
  private
    FISASegment: TEDISegment;
    FIEASegment: TEDISegment;
    FTA1Segments: TEDIObjectList;
    function GetFunctionalGroup(Index: Integer): TEDIFunctionalGroup;
    procedure SetFunctionalGroup(Index: Integer; FunctionalGroup: TEDIFunctionalGroup);
    procedure SetISASegment(const ISASegment: TEDISegment);
    procedure SetIEASegment(const IEASegment: TEDISegment);
  protected
    procedure InternalCreateHeaderTrailerSegments; virtual;
    function InternalCreateFunctionalGroup: TEDIFunctionalGroup; virtual;
    function InternalAssignDelimiters: TEDIDelimiters; override;
    function InternalCreateEDIDataObject: TEDIDataObject; override;
  public
    constructor Create(Parent: TEDIDataObject; FunctionalGroupCount: Integer = 0); reintroduce;
    destructor Destroy; override;

    function AddFunctionalGroup: Integer;
    function AppendFunctionalGroup(FunctionalGroup: TEDIFunctionalGroup): Integer;
    function InsertFunctionalGroup(InsertIndex: Integer): Integer; overload;
    function InsertFunctionalGroup(InsertIndex: Integer;
      FunctionalGroup: TEDIFunctionalGroup): Integer; overload;
    procedure DeleteFunctionalGroup(Index: Integer); overload;
    procedure DeleteFunctionalGroup(FunctionalGroup: TEDIFunctionalGroup); overload;

    function AddFunctionalGroups(Count: Integer): Integer;
    function AppendFunctionalGroups(FunctionalGroupArray: TEDIFunctionalGroupArray): Integer;
    function InsertFunctionalGroups(InsertIndex, Count: Integer): Integer; overload;
    function InsertFunctionalGroups(InsertIndex: Integer;
      FunctionalGroupArray: TEDIFunctionalGroupArray): Integer; overload;
    procedure DeleteFunctionalGroups; overload;
    procedure DeleteFunctionalGroups(Index, Count: Integer); overload;

    function Assemble: string; override;
    procedure Disassemble; override;

    property FunctionalGroup[Index: Integer]: TEDIFunctionalGroup read GetFunctionalGroup
      write SetFunctionalGroup; default;
    property FunctionalGroups: TEDIDataObjectList read FEDIDataObjects;
  published
    property SegmentISA: TEDISegment read FISASegment write SetISASegment;
    property SegmentIEA: TEDISegment read FIEASegment write SetIEASegment;
    property TA1Segments: TEDIObjectList read FTA1Segments;
    property FunctionalGroupCount: Integer read GetCount;
  end;

  TEDIInterchangeControlArray = array of TEDIInterchangeControl;

  //  EDI Interchange Specification
  TEDIInterchangeControlSpec = class(TEDIInterchangeControl)
  private
    FStandardId: string;
    FVersionId: string;
    FICDescription: string;
  protected
    procedure InternalCreateHeaderTrailerSegments; override;
    function InternalCreateFunctionalGroup: TEDIFunctionalGroup; override;
  public
    function FindFunctionalGroupSpec(FunctionalGroupId, AgencyCodeId,
      VersionReleaseId: string): TEDIFunctionalGroupSpec;
    function FindTransactionSetSpec(FunctionalGroupId, AgencyCodeId, VersionReleaseId,
      TransactionSetId: string): TEDITransactionSetSpec;
  published
    property StandardId: string read FStandardId write FStandardId;
    property VersionId: string read FVersionId write FVersionId;
    property ICDescription: string read FICDescription write FICDescription;
  end;

  //  EDI File
  TEDIFileOptions = set of (foVariableDelimiterDetection, foUseAltDelimiterDetection, foRemoveCrLf,
    foRemoveCr, foRemoveLf, foIgnoreGarbageAtEndOfFile);

  TEDIFile = class(TEDIDataObjectGroup)
  private
    FFileID: Integer;
    FFileName: string;
    FEDIFileOptions: TEDIFileOptions;
    function GetInterchangeControl(Index: Integer): TEDIInterchangeControl;
    procedure SetInterchangeControl(Index: Integer; Interchange: TEDIInterchangeControl);
    procedure InternalLoadFromFile;
  protected
    procedure InternalDelimitersDetection(StartPos: Integer); virtual;
    procedure InternalAlternateDelimitersDetection(StartPos: Integer);
    function InternalCreateInterchangeControl: TEDIInterchangeControl; virtual;
    function InternalAssignDelimiters: TEDIDelimiters; override;
    function InternalCreateEDIDataObject: TEDIDataObject; override;
  public
    constructor Create(Parent: TEDIDataObject; InterchangeCount: Integer = 0); reintroduce;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: string);
    procedure ReLoadFromFile;
    procedure SaveToFile;
    procedure SaveAsToFile(const FileName: string);

    function AddInterchange: Integer;
    function AppendInterchange(Interchange: TEDIInterchangeControl): Integer;
    function InsertInterchange(InsertIndex: Integer): Integer; overload;
    function InsertInterchange(InsertIndex: Integer;
      Interchange: TEDIInterchangeControl): Integer; overload;
    procedure DeleteInterchange(Index: Integer); overload;
    procedure DeleteInterchange(Interchange: TEDIInterchangeControl); overload;

    function AddInterchanges(Count: Integer): Integer;
    function AppendInterchanges(
      InterchangeControlArray: TEDIInterchangeControlArray): Integer;
    function InsertInterchanges(InsertIndex, Count: Integer): Integer; overload;
    function InsertInterchanges(InsertIndex: Integer;
      InterchangeControlArray: TEDIInterchangeControlArray): Integer; overload;
    procedure DeleteInterchanges; overload;
    procedure DeleteInterchanges(Index, Count: Integer); overload;

    function Assemble: string; override;
    procedure Disassemble; override;

    property Interchange[Index: Integer]: TEDIInterchangeControl read GetInterchangeControl
      write SetInterchangeControl; default;
    property Interchanges: TEDIDataObjectList read FEDIDataObjects;
  published
    property FileID: Integer read FFileID write FFileID;
    property FileName: string read FFileName write FFileName;
    property Options: TEDIFileOptions read FEDIFileOptions write FEDIFileOptions;
    property InterchangeControlCount: Integer read GetCount;
  end;

  TEDIFileArray = array of TEDIFile;

  //  EDI File Specification
  TEDIFileSpec = class(TEDIFile)
  protected
    procedure InternalDelimitersDetection(StartPos: Integer); override;
    function InternalCreateInterchangeControl: TEDIInterchangeControl; override;
  public
    constructor Create(Parent: TEDIDataObject; InterchangeCount: Integer = 0); reintroduce;
    function FindTransactionSetSpec(StandardId, VersionId, FunctionalGroupId, AgencyCodeId,
      VersionReleaseId, TransactionSetId: string): TEDITransactionSetSpec;
    function FindFunctionalGroupSpec(StandardId, VersionId, FunctionalGroupId, AgencyCodeId,
      VersionReleaseId: string): TEDIFunctionalGroupSpec;
    function FindInterchangeControlSpec(StandardId, VersionId: string): TEDIInterchangeControlSpec;
  end;

{$IFNDEF EDI_WEAK_PACKAGE_UNITS}
{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclEDI_ANSIX12.pas $';
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
  { Reserved Data Field Values }
  Value_Unknown = 'Unknown';
  Value_NotAssigned = 'Not Assigned';
  Value_None = 'None';
  Value_Optional = 'O';
  Value_Mandatory = 'M';
  Value_AlphaNumeric = 'AN';

//=== { TEDIElement } ========================================================

constructor TEDIElement.Create(Parent: TEDIDataObject);
begin
  if Assigned(Parent) and (Parent is TEDISegment) then
    inherited Create(Parent)
  else
    inherited Create(nil);
  FEDIDOT := ediElement;
end;

function TEDIElement.Assemble: string;
begin
  Result := FData;
  FState := ediAssembled;
end;

procedure TEDIElement.Disassemble;
begin
  FState := ediDisassembled;
end;

function TEDIElement.GetIndexPositionFromParent: Integer;
var
  I: Integer;
begin
  Result := -1;
  if Assigned(Parent) and (Parent is TEDISegment) then
    for I := 0 to TEDISegment(Parent).ElementCount - 1 do
      if TEDISegment(Parent).Element[I] = Self then
      begin
        Result := I;
        Break;
      end;
end;

//=== { TEDISegment } ========================================================

constructor TEDISegment.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDITransactionSet) then
    inherited Create(Parent, ElementCount)
  else
    inherited Create(nil, ElementCount);
  FSegmentId := '';
  FEDIDOT := ediSegment;
end;

destructor TEDISegment.Destroy;
begin
  inherited Destroy;
end;

function TEDISegment.AddElements(Count: Integer): Integer;
begin
  Result := AddEDIDataObjects(Count);
end;

function TEDISegment.AddElement: Integer;
begin
  Result := AddEDIDataObject;
end;

function TEDISegment.AppendElement(Element: TEDIElement): Integer;
begin
  Result := AppendEDIDataObject(Element);
end;

function TEDISegment.AppendElements(ElementArray: TEDIElementArray): Integer;
{$IFDEF CLR}
var
  HelpArray: TEDIDataObjectArray;
  I: Integer;
{$ENDIF CLR}
begin
  {$IFDEF CLR}
  SetLength(HelpArray, Length(ElementArray));
  for I := 0 to High(ElementArray) do
    HelpArray[I] := TEDIDataObject(ElementArray[I]);
  Result := AppendEDIDataObjects(HelpArray);
  {$ELSE}
  Result := AppendEDIDataObjects(TEDIDataObjectArray(ElementArray));
  {$ENDIF CLR}
end;

function TEDISegment.Assemble: string;
var
  I: Integer;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(36);
  end;

  FData := FSegmentId;
  if GetCount > 0 then
    for I := 0 to GetCount - 1 do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FDelimiters.ED + FEDIDataObjects[I].Assemble
      else
        FData := FData + FDelimiters.ED;
  FData := FData + FDelimiters.SD;
  FLength := Length(FData);
  Result := FData; // Return assembled string

  DeleteElements;

  FState := ediAssembled;
end;

procedure TEDISegment.DeleteElement(Index: Integer);
begin
  DeleteEDIDataObject(Index);
end;

procedure TEDISegment.DeleteElement(Element: TEDIElement);
begin
  DeleteEDIDataObject(Element);
end;

procedure TEDISegment.DeleteElements(Index, Count: Integer);
begin
  DeleteEDIDataObjects(Index, Count);
end;

procedure TEDISegment.DeleteElements;
begin
  DeleteEDIDataObjects;
end;

procedure TEDISegment.Disassemble;
var
  I, StartPos, SearchResult: Integer;
begin
  // Data Input Scenarios
  // 4.)  SegID*---*---~
  // Composite Element Data Input Secnarios
  // 9.)  SegID*---*--->---~
  FSegmentId := '';
  DeleteElements;
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(35);
  end;
  // Continue
  StartPos := 1;
  SearchResult := StrSearch(FDelimiters.ED, FData, StartPos);
  FSegmentId := Copy(FData, 1, SearchResult - 1);
  StartPos := SearchResult + 1;
  SearchResult := StrSearch(FDelimiters.ED, FData, StartPos);
  while SearchResult <> 0 do
  begin
    I := AddElement;
    if (SearchResult - StartPos) > 0 then // data exists
    begin
      FEDIDataObjects[I].Data := Copy(FData, ((StartPos + FDelimiters.EDLen) - 1),
        (SearchResult - StartPos));
      FEDIDataObjects[I].Disassemble;
    end;
    StartPos := SearchResult + 1;
    SearchResult := StrSearch(FDelimiters.ED, FData, StartPos);
  end;
  // Get last element before next segment
  SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
  if SearchResult <> 0 then
  begin
    I := AddElement;
    if (SearchResult - StartPos) > 0 then // data exists
    begin
      FEDIDataObjects[I].Data := Copy(FData, ((StartPos + FDelimiters.EDLen) - 1),
        (SearchResult - StartPos));
      FEDIDataObjects[I].Disassemble;
    end;
  end;
  FData := '';
  FState := ediDisassembled;
end;

function TEDISegment.GetElement(Index: Integer): TEDIElement;
begin
  Result := TEDIElement(GetEDIDataObject(Index));
end;

function TEDISegment.InsertElement(InsertIndex: Integer): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex);
end;

function TEDISegment.InsertElement(InsertIndex: Integer; Element: TEDIElement): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, Element);
end;

function TEDISegment.InsertElements(InsertIndex: Integer; ElementArray: TEDIElementArray): Integer;
{$IFDEF CLR}
var
  HelpArray: TEDIDataObjectArray;
  I: Integer;
{$ENDIF CLR}
begin
  {$IFDEF CLR}
  SetLength(HelpArray, Length(ElementArray));
  for I := 0 to High(ElementArray) do
    HelpArray[I] := TEDIDataObject(ElementArray[I]);
  Result := InsertEDIDataObjects(InsertIndex, HelpArray);
  {$ELSE}
  Result := InsertEDIDataObjects(InsertIndex, TEDIDataObjectArray(ElementArray));
  {$ENDIF CLR}
end;

function TEDISegment.InsertElements(InsertIndex, Count: Integer): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

function TEDISegment.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
    // Get the delimiters from the transaction set
    if Assigned(Parent) and (Parent is TEDITransactionSet) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
        Exit;
      end;
      // Get the delimiters from the functional group
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIFunctionalGroup) then
      begin
        if Assigned(Parent.Parent.Delimiters) then
        begin
          Result := Parent.Parent.Delimiters;
          Exit;
        end;
        // Get the delimiters from the interchange control header
        if Assigned(Parent.Parent.Parent) and (Parent.Parent.Parent is TEDIInterchangeControl) then
          Result := Parent.Parent.Parent.Delimiters;
      end;
    end;
end;

function TEDISegment.InternalCreateElement: TEDIElement;
begin
  Result := TEDIElement.Create(Self);
end;

procedure TEDISegment.SetElement(Index: Integer; Element: TEDIElement);
begin
  SetEDIDataObject(Index, Element);
end;

function TEDISegment.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateElement;
end;

//=== { TEDITransactionSetSegment } ==========================================

constructor TEDITransactionSetSegment.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  if Assigned(Parent) and (Parent is TEDITransactionSet) then
    FParent := Parent;
end;

function TEDITransactionSetSegment.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := inherited InternalAssignDelimiters;
end;

//=== { TEDIFunctionalGroupSegment } =========================================

constructor TEDIFunctionalGroupSegment.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
    FParent := Parent;
end;

function TEDIFunctionalGroupSegment.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  // Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
    // Get the delimiters from the functional group
    if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
        Exit;
      end;
      // Get the delimiters from the interchange control
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIInterchangeControl) then
        Result := Parent.Parent.Delimiters;
    end;
end;

//=== {  TEDIInterchangeControlSegment } =====================================

constructor TEDIInterchangeControlSegment.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
    FParent := Parent;
end;

function TEDIInterchangeControlSegment.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  // Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
    // Get the delimiters from the interchange control
    if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
      Result := Parent.Delimiters;
end;

//=== { TEDITransactionSet } =================================================

constructor TEDITransactionSet.Create(Parent: TEDIDataObject; SegmentCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
    inherited Create(Parent, SegmentCount)
  else
    inherited Create(nil, SegmentCount);
  FEDIDOT := ediTransactionSet;
  InternalCreateHeaderTrailerSegments;
end;

destructor TEDITransactionSet.Destroy;
begin
  FSESegment.Free;
  FSTSegment.Free;
  inherited Destroy;
end;

function TEDITransactionSet.AddSegment: Integer;
begin
  Result := AddEDIDataObject;
end;

function TEDITransactionSet.AddSegments(Count: Integer): Integer;
begin
  Result := AddEDIDataObjects(Count);
end;

function TEDITransactionSet.AppendSegment(Segment: TEDISegment): Integer;
begin
  Result := AppendEDIDataObject(Segment);
end;

function TEDITransactionSet.AppendSegments(SegmentArray: TEDISegmentArray): Integer;
{$IFDEF CLR}
var
  HelpArray: TEDIDataObjectArray;
  I: Integer;
{$ENDIF CLR}
begin
  {$IFDEF CLR}
  SetLength(HelpArray, Length(SegmentArray));
  for I := 0 to High(SegmentArray) do
    HelpArray[I] := TEDIDataObject(SegmentArray[I]);
  Result := AppendEDIDataObjects(HelpArray);
  {$ELSE}
  Result := AppendEDIDataObjects(TEDIDataObjectArray(SegmentArray));
  {$ENDIF CLR}
end;

function TEDITransactionSet.Assemble: string;
var
  I: Integer;
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

  FData := FSTSegment.Assemble;
  FSTSegment.Data := '';

  if GetCount > 0 then
    for I := 0 to GetCount - 1 do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;

  DeleteSegments;

  FData := FData + FSESegment.Assemble;
  FSESegment.Data := '';

  FLength := Length(FData);
  Result := FData;

  FState := ediAssembled;
end;

procedure TEDITransactionSet.DeleteSegment(Index: Integer);
begin
  DeleteEDIDataObject(Index);
end;

procedure TEDITransactionSet.DeleteSegment(Segment: TEDISegment);
begin
  DeleteEDIDataObject(Segment);
end;

procedure TEDITransactionSet.DeleteSegments;
begin
  DeleteEDIDataObjects;
end;

procedure TEDITransactionSet.DeleteSegments(Index, Count: Integer);
begin
  DeleteEDIDataObjects(Index, Count);
end;

procedure TEDITransactionSet.Disassemble;
var
  I, StartPos, SearchResult: Integer;
  S, S2: string;
begin
  FSTSegment.Data := '';
  FSTSegment.DeleteElements;
  FSESegment.Data := '';
  FSESegment.DeleteElements;
  DeleteSegments;
  // Check delimiter assignment
  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(25);
  end;
  // Find the first segment
  StartPos := 1;
  SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
  while SearchResult <> 0 do
  begin
    S := Copy(FData, StartPos, Length(TSHSegmentId));
    S2 := Copy(FData, StartPos, Length(TSTSegmentId));
    if (S <> TSHSegmentId) and (S2 <> TSTSegmentId) then
    begin
      I := AddSegment;
      if (SearchResult - StartPos) > 0 then // data exists
      begin
        FEDIDataObjects[I].Data := Copy(FData, StartPos,
          ((SearchResult - StartPos) + FDelimiters.SDLen));
        FEDIDataObjects[I].Disassemble;
      end;
    end
    else
    if S = TSHSegmentId then
    begin
      if (SearchResult - StartPos) > 0 then // data exists
      begin
        FSTSegment.Data := Copy(FData, StartPos,
          ((SearchResult - StartPos) + FDelimiters.SDLen));
        FSTSegment.Disassemble;
      end;
    end
    else
    if S2 = TSTSegmentId then
    begin
      if (SearchResult - StartPos) > 0 then // data exists
      begin
        FSESegment.Data := Copy(FData, StartPos,
          ((SearchResult - StartPos) + FDelimiters.SDLen));
        FSESegment.Disassemble;
      end;
    end;
    StartPos := SearchResult + FDelimiters.SDLen;
    SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
  end;
  FData := '';
  FState := ediDisassembled;
end;

function TEDITransactionSet.GetSegment(Index: Integer): TEDISegment;
begin
  Result := TEDISegment(GetEDIDataObject(Index));
end;

function TEDITransactionSet.InsertSegment(InsertIndex: Integer): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex);
end;

function TEDITransactionSet.InsertSegment(InsertIndex: Integer; Segment: TEDISegment): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, Segment);
end;

function TEDITransactionSet.InsertSegments(InsertIndex, Count: Integer): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

function TEDITransactionSet.InsertSegments(InsertIndex: Integer;
  SegmentArray: TEDISegmentArray): Integer;
{$IFDEF CLR}
var
  HelpArray: TEDIDataObjectArray;
  I: Integer;
{$ENDIF CLR}
begin
  {$IFDEF CLR}
  SetLength(HelpArray, Length(SegmentArray));
  for I := 0 to High(SegmentArray) do
    HelpArray[I] := TEDIDataObject(SegmentArray[I]);
  Result := InsertEDIDataObjects(InsertIndex, HelpArray);
  {$ELSE}
  Result := InsertEDIDataObjects(InsertIndex, TEDIDataObjectArray(SegmentArray));
  {$ENDIF CLR}
end;

function TEDITransactionSet.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  if FDelimiters = nil then // Attempt to assign the delimiters
    if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
        Exit;
      end;
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIInterchangeControl) then
        Result := Parent.Parent.Delimiters;
    end;
end;

function TEDITransactionSet.InternalCreateSegment: TEDISegment;
begin
  Result := TEDISegment.Create(Self);
end;

procedure TEDITransactionSet.InternalCreateHeaderTrailerSegments;
begin
  FSTSegment := TEDITransactionSetSegment.Create(Self);
  FSESegment := TEDITransactionSetSegment.Create(Self);
end;

procedure TEDITransactionSet.SetSegment(Index: Integer; Segment: TEDISegment);
begin
  SetEDIDataObject(Index, Segment);
end;

procedure TEDITransactionSet.SetSESegment({$IFNDEF BCB6} const {$ENDIF} SESegment: TEDISegment);
begin
  FreeAndNil(FSESegment);
  FSESegment := SESegment;
  if Assigned(FSESegment) then
    FSESegment.Parent := Self;
end;

procedure TEDITransactionSet.SetSTSegment({$IFNDEF BCB6} const {$ENDIF} STSegment: TEDISegment);
begin
  FreeAndNil(FSTSegment);
  FSTSegment := STSegment;
  if Assigned(FSTSegment) then
    FSTSegment.Parent := Self;
end;

function TEDITransactionSet.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateSegment;
end;

//=== { TEDIFunctionalGroup } ================================================

constructor TEDIFunctionalGroup.Create(Parent: TEDIDataObject; TransactionSetCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
    inherited Create(Parent, TransactionSetCount)
  else
    inherited Create(nil, TransactionSetCount);
  FEDIDOT := ediFunctionalGroup;
  InternalCreateHeaderTrailerSegments;
end;

destructor TEDIFunctionalGroup.Destroy;
begin
  FGSSegment.Free;
  FGESegment.Free;
  inherited Destroy;
end;

function TEDIFunctionalGroup.AddTransactionSet: Integer;
begin
  Result := AddEDIDataObject;
end;

function TEDIFunctionalGroup.AddTransactionSets(Count: Integer): Integer;
begin
  Result := AddEDIDataObjects(Count);
end;

function TEDIFunctionalGroup.AppendTransactionSet(TransactionSet: TEDITransactionSet): Integer;
begin
  Result := AppendEDIDataObject(TransactionSet);
end;

function TEDIFunctionalGroup.AppendTransactionSets(
  TransactionSetArray: TEDITransactionSetArray): Integer;
{$IFDEF CLR}
var
  HelpArray: TEDIDataObjectArray;
  I: Integer;
{$ENDIF CLR}
begin
  {$IFDEF CLR}
  SetLength(HelpArray, Length(TransactionSetArray));
  for I := 0 to High(TransactionSetArray) do
    HelpArray[I] := TEDIDataObject(TransactionSetArray[I]);
  Result := AppendEDIDataObjects(HelpArray);
  {$ELSE}
  Result := AppendEDIDataObjects(TEDIDataObjectArray(TransactionSetArray));
  {$ENDIF CLR}
end;

function TEDIFunctionalGroup.Assemble: string;
var
  I: Integer;
begin
  FData := '';
  FLength := 0;
  Result := '';
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(20);
  end;
  FData := FGSSegment.Assemble;
  FGSSegment.Data := '';

  if GetCount > 0 then
    for I := 0 to GetCount - 1 do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;

  DeleteTransactionSets;

  FData := FData + FGESegment.Assemble;
  FGESegment.Data := '';

  FLength := Length(FData);
  Result := FData;

  FState := ediAssembled;
end;

procedure TEDIFunctionalGroup.DeleteTransactionSet(Index: Integer);
begin
  DeleteEDIDataObject(Index);
end;

procedure TEDIFunctionalGroup.DeleteTransactionSet(TransactionSet: TEDITransactionSet);
begin
  DeleteEDIDataObject(TransactionSet);
end;

procedure TEDIFunctionalGroup.DeleteTransactionSets;
begin
  DeleteEDIDataObjects;
end;

procedure TEDIFunctionalGroup.DeleteTransactionSets(Index, Count: Integer);
begin
  DeleteEDIDataObjects(Index, Count);
end;

procedure TEDIFunctionalGroup.Disassemble;
var
  I, StartPos, SearchResult: Integer;
begin
  FGSSegment.Data := '';
  FGSSegment.DeleteElements;
  FGESegment.Data := '';
  FGESegment.DeleteElements;
  DeleteTransactionSets;
  // Check delimiter assignment
  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(19);
  end;
  // Find Functional Group Header Segment
  StartPos := 1;
  // Search for Functional Group Header
  if FGHSegmentId + FDelimiters.ED = Copy(FData, 1, Length(FGHSegmentId + FDelimiters.ED)) then
  begin
    // Search for Functional Group Header Segment Terminator
    SearchResult := StrSearch(FDelimiters.SD, FData, 1);
    if (SearchResult - StartPos) > 0 then // data exists
    begin
      FGSSegment.Data := Copy(FData, 1, (SearchResult + FDelimiters.SDLen) - 1);
      FGSSegment.Disassemble;
    end
    else
      raise EJclEDIError.CreateID(21);
  end
  else
    raise EJclEDIError.CreateID(22);
  // Search for Transaction Set Header
  SearchResult := StrSearch(FDelimiters.SD + TSHSegmentId + FDelimiters.ED, FData, StartPos);
  if SearchResult <= 0 then
    raise EJclEDIError.CreateID(27);
  // Set next start position
  StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
  // Continue
  while SearchResult <> 0 do
  begin
    // Search for Transaction Set Trailer
    SearchResult := StrSearch(FDelimiters.SD + TSTSegmentId + FDelimiters.ED, FData, StartPos);
    if SearchResult <> 0 then
    begin
      // Set the next start position
      SearchResult := SearchResult + FDelimiters.SDLen; // Move past the delimiter
      // Search for the end of Transaction Set Trailer
      SearchResult := StrSearch(FDelimiters.SD, FData, SearchResult);
      if SearchResult <> 0 then
      begin
        I := AddTransactionSet;
        FEDIDataObjects[I].Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
        FEDIDataObjects[I].Disassemble;
      end
      else
        raise EJclEDIError.CreateID(28);
    end
    else
      raise EJclEDIError.CreateID(29);
    // Set the next start position
    StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
    //
    // Verify the next record is a Transaction Set Header
    if (TSHSegmentId + FDelimiters.ED) <>
       Copy(FData, StartPos, (Length(TSHSegmentId) + FDelimiters.EDLen)) then
      Break;
  end;
  // Set the next start position
  StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
  // Find Functional Group Trailer Segment
  if (FGTSegmentId + FDelimiters.ED) =
    Copy(FData, StartPos, Length(FGTSegmentId + FDelimiters.ED)) then
  begin
    // Find Functional Group Trailer Segment Terminator
    SearchResult := StrSearch(FDelimiters.SD, FData, StartPos + FDelimiters.SDLen);
    if (SearchResult - StartPos) > 0 then // data exists
    begin
      FGESegment.Data := Copy(FData, StartPos, (SearchResult + FDelimiters.SDLen));
      FGESegment.Disassemble;
    end
    else
      raise EJclEDIError.CreateID(23);
  end
  else
    raise EJclEDIError.CreateID(24);
  FData := '';
  FState := ediDisassembled;
end;

function TEDIFunctionalGroup.GetTransactionSet(Index: Integer): TEDITransactionSet;
begin
  Result := TEDITransactionSet(GetEDIDataObject(Index));
end;

function TEDIFunctionalGroup.InsertTransactionSet(InsertIndex: Integer): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex);
end;

function TEDIFunctionalGroup.InsertTransactionSet(InsertIndex: Integer;
  TransactionSet: TEDITransactionSet): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, TransactionSet);
end;

function TEDIFunctionalGroup.InsertTransactionSets(InsertIndex: Integer;
  TransactionSetArray: TEDITransactionSetArray): Integer;
{$IFDEF CLR}
var
  HelpArray: TEDIDataObjectArray;
  I: Integer;
{$ENDIF CLR}
begin
  {$IFDEF CLR}
  SetLength(HelpArray, Length(TransactionSetArray));
  for I := 0 to High(TransactionSetArray) do
    HelpArray[I] := TEDIDataObject(TransactionSetArray[I]);
  Result := InsertEDIDataObjects(InsertIndex, HelpArray);
  {$ELSE}
  Result := InsertEDIDataObjects(InsertIndex, TEDIDataObjectArray(TransactionSetArray));
  {$ENDIF CLR}
end;

function TEDIFunctionalGroup.InsertTransactionSets(InsertIndex, Count: Integer): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

function TEDIFunctionalGroup.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  // Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
    if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
      Result := Parent.Delimiters;
end;

function TEDIFunctionalGroup.InternalCreateTransactionSet: TEDITransactionSet;
begin
  Result := TEDITransactionSet.Create(Self);
end;

procedure TEDIFunctionalGroup.InternalCreateHeaderTrailerSegments;
begin
  FGSSegment := TEDIFunctionalGroupSegment.Create(Self);
  FGESegment := TEDIFunctionalGroupSegment.Create(Self);
end;

procedure TEDIFunctionalGroup.SetTransactionSet(Index: Integer; TransactionSet: TEDITransactionSet);
begin
  SetEDIDataObject(Index, TransactionSet);
end;

procedure TEDIFunctionalGroup.SetGESegment(const GESegment: TEDISegment);
begin
  FreeAndNil(FGESegment);
  FGESegment := GESegment;
  if Assigned(FGESegment) then
    FGESegment.Parent := Self;
end;

procedure TEDIFunctionalGroup.SetGSSegment(const GSSegment: TEDISegment);
begin
  FreeAndNil(FGSSegment);
  FGSSegment := GSSegment;
  if Assigned(FGSSegment) then
    FGSSegment.Parent := Self;
end;

function TEDIFunctionalGroup.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateTransactionSet;
end;

//== { TEDIInterchangeControl } ==============================================

constructor TEDIInterchangeControl.Create(Parent: TEDIDataObject; FunctionalGroupCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDIFile) then
    inherited Create(Parent, FunctionalGroupCount)
  else
    inherited Create(nil, FunctionalGroupCount);
  FEDIDOT := ediInterchangeControl;
  InternalCreateHeaderTrailerSegments;
  FTA1Segments := TEDIObjectList.Create;
end;

destructor TEDIInterchangeControl.Destroy;
begin
  FTA1Segments.Clear;
  FTA1Segments.Free;
  FISASegment.Free;
  FIEASegment.Free;
  FreeAndNil(FDelimiters);
  inherited Destroy;
end;

function TEDIInterchangeControl.AddFunctionalGroup: Integer;
begin
  Result := AddEDIDataObject;
end;

function TEDIInterchangeControl.AddFunctionalGroups(Count: Integer): Integer;
begin
  Result := AddEDIDataObjects(Count);
end;

function TEDIInterchangeControl.AppendFunctionalGroup(
  FunctionalGroup: TEDIFunctionalGroup): Integer;
begin
  Result := AppendEDIDataObject(FunctionalGroup);
end;

function TEDIInterchangeControl.AppendFunctionalGroups(
  FunctionalGroupArray: TEDIFunctionalGroupArray): Integer;
{$IFDEF CLR}
var
  HelpArray: TEDIDataObjectArray;
  I: Integer;
{$ENDIF CLR}
begin
  {$IFDEF CLR}
  SetLength(HelpArray, Length(FunctionalGroupArray));
  for I := 0 to High(FunctionalGroupArray) do
    HelpArray[I] := TEDIDataObject(FunctionalGroupArray[I]);
  Result := AppendEDIDataObjects(HelpArray);
  {$ELSE}
  Result := AppendEDIDataObjects(TEDIDataObjectArray(FunctionalGroupArray));
  {$ENDIF CLR}
end;

function TEDIInterchangeControl.Assemble: string;
var
  I: Integer;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if not Assigned(FDelimiters) then
    raise EJclEDIError.CreateID(13);

  FData := FISASegment.Assemble;
  FISASegment.Data := '';

  if GetCount > 0 then
    for I := 0 to GetCount - 1 do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;

  DeleteFunctionalGroups;

  FData := FData + FIEASegment.Assemble;
  FIEASegment.Data := '';

  FLength := Length(FData);
  Result := FData;

  FState := ediAssembled;
end;

procedure TEDIInterchangeControl.DeleteFunctionalGroup(Index: Integer);
begin
  DeleteEDIDataObject(Index);
end;

procedure TEDIInterchangeControl.DeleteFunctionalGroups;
begin
  DeleteEDIDataObjects;
end;

procedure TEDIInterchangeControl.DeleteFunctionalGroups(Index, Count: Integer);
begin
  DeleteEDIDataObjects(Index, Count);
end;

procedure TEDIInterchangeControl.Disassemble;
var
  I, StartPos, SearchResult: Integer;
  ProcessTA1: Boolean;
  TA1Segment: TEDIInterchangeControlSegment;
begin
  ProcessTA1 := False;
  FTA1Segments.Clear;
  FISASegment.Data := '';
  FISASegment.DeleteElements;
  FIEASegment.Data := '';
  FIEASegment.DeleteElements;
  DeleteFunctionalGroups;

  if not Assigned(FDelimiters) then
    raise EJclEDIError.CreateID(12);

  StartPos := 1;
  // Search for Interchange Control Header
  if ICHSegmentId + FDelimiters.ED = Copy(FData, 1, Length(ICHSegmentId + FDelimiters.ED)) then
  begin
    SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
    if (SearchResult - StartPos) > 0 then // data exists
    begin
      FISASegment.Data := Copy(FData, 1, (SearchResult + FDelimiters.SDLen) - 1);
      FISASegment.Disassemble;
    end
    else
      raise EJclEDIError.CreateID(14);
  end
  else
    raise EJclEDIError.CreateID(15);
  // Search for Functional Group Header
  SearchResult := StrSearch(FDelimiters.SD + FGHSegmentId + FDelimiters.ED, FData, StartPos);
  // Check for TA1 Segment
  I := StrSearch(FDelimiters.SD + TA1SegmentId + FDelimiters.ED, FData, StartPos);
  if ((I < SearchResult) or ((I > SearchResult) and (SearchResult = 0))) and (I <> 0) then
  begin
    ProcessTA1 := True;
    SearchResult := I;
  end;
  if (SearchResult <= 0) and (not ProcessTA1) then
    raise EJclEDIError.CreateID(22);
  // Set next start positon
  StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
  // Continue
  while ((StartPos + Length(FGHSegmentId)) < Length(FData)) and (SearchResult > 0) do
  begin
    if not ProcessTA1 then
    begin
      // Search for Functional Group Trailer
      SearchResult := StrSearch(FDelimiters.SD + FGTSegmentId + FDelimiters.ED, FData, StartPos);
      if SearchResult > 0 then
      begin
        // Set next start positon
        SearchResult := SearchResult + FDelimiters.SDLen; // Move past the delimiter
        // Search for end of Functional Group Trailer Segment Terminator
        SearchResult := StrSearch(FDelimiters.SD, FData, SearchResult);
        if SearchResult > 0 then
        begin
          I := AddFunctionalGroup;
          FEDIDataObjects[I].Data :=
            Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
          FEDIDataObjects[I].Disassemble;
        end
        else
          raise EJclEDIError.CreateID(23);
      end
      else
        raise EJclEDIError.CreateID(24);
      // Set next start positon
      StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
      // Verify the next record is a Functional Group Header
      if (TA1SegmentId + FDelimiters.ED) =
        Copy(FData, StartPos, (Length(TA1SegmentId) + FDelimiters.EDLen)) then
        ProcessTA1 := True
      else
      if (FGHSegmentId + FDelimiters.ED) <>
        Copy(FData, StartPos, (Length(FGHSegmentId) + FDelimiters.EDLen)) then
        Break;
    end
    else //Process TA1 Segment
    begin
      ProcessTA1 := False;
      // Check next segment
      SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
      // Debug
      //ShowMessage('"' + Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen)) + '"');
      TA1Segment := TEDIInterchangeControlSegment.Create(Self);
      FTA1Segments.Add(TA1Segment);
      TA1Segment.Data := Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
      TA1Segment.Disassemble;
      // Set next start positon
      StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
      // Check next segment
      if (TA1SegmentId + FDelimiters.ED) =
        Copy(FData, StartPos, (Length(TA1SegmentId) + FDelimiters.EDLen)) then
        ProcessTA1 := True
      else
      if (FGHSegmentId + FDelimiters.ED) <>
        Copy(FData, StartPos, (Length(FGHSegmentId) + FDelimiters.EDLen)) then
        Break;
    end;
  end;
  // Verify the next record is a Interchange Control Trailer
  if (ICTSegmentId + FDelimiters.ED) =
    Copy(FData, StartPos, Length(ICTSegmentId + FDelimiters.ED)) then
  begin
    // Search for the end of Interchange Control Trailer Segment Terminator
    SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
    if (SearchResult - StartPos) > 0 then // data exists
    begin
      FIEASegment.Data := Copy(FData, StartPos, (SearchResult + FDelimiters.SDLen));
      FIEASegment.Disassemble;
    end
    else
      raise EJclEDIError.CreateID(16);
  end
  else
    raise EJclEDIError.CreateID(17);
  FData := '';
  FState := ediDisassembled;
end;

function TEDIInterchangeControl.GetFunctionalGroup(Index: Integer): TEDIFunctionalGroup;
begin
  Result := TEDIFunctionalGroup(GetEDIDataObject(Index));
end;

function TEDIInterchangeControl.InsertFunctionalGroup(InsertIndex: Integer;
  FunctionalGroup: TEDIFunctionalGroup): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, FunctionalGroup);
end;

function TEDIInterchangeControl.InsertFunctionalGroup(InsertIndex: Integer): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex);
end;

function TEDIInterchangeControl.InsertFunctionalGroups(InsertIndex, Count: Integer): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

function TEDIInterchangeControl.InsertFunctionalGroups(InsertIndex: Integer;
  FunctionalGroupArray: TEDIFunctionalGroupArray): Integer;
{$IFDEF CLR}
var
  HelpArray: TEDIDataObjectArray;
  I: Integer;
{$ENDIF CLR}
begin
  {$IFDEF CLR}
  SetLength(HelpArray, Length(FunctionalGroupArray));
  for I := 0 to High(FunctionalGroupArray) do
    HelpArray[I] := TEDIDataObject(FunctionalGroupArray[I]);
  Result := InsertEDIDataObjects(InsertIndex, HelpArray);
  {$ELSE}
  Result := InsertEDIDataObjects(InsertIndex, TEDIDataObjectArray(FunctionalGroupArray));
  {$ENDIF CLR}
end;

procedure TEDIInterchangeControl.SetFunctionalGroup(Index: Integer;
  FunctionalGroup: TEDIFunctionalGroup);
begin
  SetEDIDataObject(Index, FunctionalGroup);
end;

function TEDIInterchangeControl.InternalCreateFunctionalGroup: TEDIFunctionalGroup;
begin
  Result := TEDIFunctionalGroup.Create(Self);
end;

procedure TEDIInterchangeControl.InternalCreateHeaderTrailerSegments;
begin
  FISASegment := TEDIInterchangeControlSegment.Create(Self);
  FIEASegment := TEDIInterchangeControlSegment.Create(Self);
end;

procedure TEDIInterchangeControl.SetIEASegment(const IEASegment: TEDISegment);
begin
  FreeAndNil(FIEASegment);
  FIEASegment := IEASegment;
  if Assigned(FIEASegment) then
    FIEASegment.Parent := Self;
end;

procedure TEDIInterchangeControl.SetISASegment(const ISASegment: TEDISegment);
begin
  FreeAndNil(FISASegment);
  FISASegment := ISASegment;
  if Assigned(FISASegment) then
    FISASegment.Parent := Self;
end;

procedure TEDIInterchangeControl.DeleteFunctionalGroup(FunctionalGroup: TEDIFunctionalGroup);
begin
  DeleteEDIDataObject(FunctionalGroup);
end;

function TEDIInterchangeControl.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateFunctionalGroup;
end;

function TEDIInterchangeControl.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
end;

//=== { TEDIFile } ===========================================================

constructor TEDIFile.Create(Parent: TEDIDataObject; InterchangeCount: Integer);
begin
  inherited Create(nil, InterchangeCount);
  FEDIFileOptions := [foVariableDelimiterDetection, foRemoveCrLf, foRemoveCr, foRemoveLf];
  FEDIDOT := ediFile;
end;

destructor TEDIFile.Destroy;
begin
  inherited Destroy;
end;

function TEDIFile.AddInterchange: Integer;
begin
  Result := AddEDIDataObject;
end;

function TEDIFile.AddInterchanges(Count: Integer): Integer;
begin
  Result := AddEDIDataObjects(Count);
end;

function TEDIFile.AppendInterchange(Interchange: TEDIInterchangeControl): Integer;
begin
  Result := AppendEDIDataObject(Interchange);
end;

function TEDIFile.AppendInterchanges(InterchangeControlArray: TEDIInterchangeControlArray): Integer;
{$IFDEF CLR}
var
  HelpArray: TEDIDataObjectArray;
  I: Integer;
{$ENDIF CLR}
begin
  {$IFDEF CLR}
  SetLength(HelpArray, Length(InterchangeControlArray));
  for I := 0 to High(InterchangeControlArray) do
    HelpArray[I] := TEDIDataObject(InterchangeControlArray[I]);
  Result := AppendEDIDataObjects(HelpArray);
  {$ELSE}
  Result := AppendEDIDataObjects(TEDIDataObjectArray(InterchangeControlArray));
  {$ENDIF CLR}
end;

function TEDIFile.Assemble: string;
var
  I: Integer;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if GetCount > 0 then
    for I := 0 to GetCount - 1 do
    begin
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;
      FEDIDataObjects[I].Data := '';
    end;

  FLength := Length(FData);
  Result := FData;

  DeleteInterchanges;

  FState := ediAssembled;
end;

procedure TEDIFile.DeleteInterchange(Index: Integer);
begin
  DeleteEDIDataObject(Index);
end;

procedure TEDIFile.DeleteInterchanges(Index, Count: Integer);
begin
  DeleteEDIDataObjects(Index, Count);
end;

procedure TEDIFile.DeleteInterchanges;
begin
  DeleteEDIDataObjects;
end;

procedure TEDIFile.Disassemble;
var
  I, StartPos, SearchResult: Integer;
begin
  DeleteInterchanges;

  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    FEDIFileOptions := FEDIFileOptions + [foVariableDelimiterDetection];
  end;

  if foRemoveCrLf in FEDIFileOptions then
    {$IFDEF OPTIMIZED_STRINGREPLACE}
    FData := JclEDI.StringReplace(FData, AnsiCrLf, '', [rfReplaceAll]);
    {$ELSE}
    FData := SysUtils.StringReplace(FData, AnsiCrLf, '', [rfReplaceAll]);
    {$ENDIF OPTIMIZED_STRINGREPLACE}
  if foRemoveCr in FEDIFileOptions then
    {$IFDEF OPTIMIZED_STRINGREPLACE}
    FData := JclEDI.StringReplace(FData, AnsiCarriageReturn, '', [rfReplaceAll]);
    {$ELSE}
    FData := SysUtils.StringReplace(FData, AnsiCarriageReturn, '', [rfReplaceAll]);
    {$ENDIF OPTIMIZED_STRINGREPLACE}
  if foRemoveLf in FEDIFileOptions then
    {$IFDEF OPTIMIZED_STRINGREPLACE}
    FData := JclEDI.StringReplace(FData, AnsiLineFeed, '', [rfReplaceAll]);
    {$ELSE}
    FData := SysUtils.StringReplace(FData, AnsiLineFeed, '', [rfReplaceAll]);
    {$ENDIF OPTIMIZED_STRINGREPLACE}

  StartPos := 1;
  // Search for Interchange Control Header
  if ICHSegmentId = Copy(FData, StartPos, Length(ICHSegmentId)) then
  begin
    if foVariableDelimiterDetection in FEDIFileOptions then          
      if foUseAltDelimiterDetection in FEDIFileOptions then
        InternalAlternateDelimitersDetection(StartPos)
      else
        InternalDelimitersDetection(StartPos);
  end
  else
    raise EJclEDIError.CreateID(15);
  // Continue
  while (StartPos + Length(ICHSegmentId)) < Length(FData) do
  begin
    // Search for Interchange Control Trailer
    SearchResult := StrSearch(FDelimiters.SD + ICTSegmentId + FDelimiters.ED, FData, StartPos);
    if SearchResult > 0 then
    begin
      SearchResult := SearchResult + FDelimiters.SDLen; // Move past the delimiter
      // Search for the end of Interchange Control Trailer
      SearchResult := StrSearch(FDelimiters.SD, FData, SearchResult);
      if SearchResult > 0 then
      begin
        I := AddInterchange;
        FEDIDataObjects[I].Delimiters :=
          TEDIDelimiters.Create(FDelimiters.SD, FDelimiters.ED, FDelimiters.SS);
        FEDIDataObjects[I].Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
        FEDIDataObjects[I].Disassemble;
      end
      else
        raise EJclEDIError.CreateID(16);
    end
    else
      raise EJclEDIError.CreateID(17);
    // Set next start position, Move past the delimiter
    StartPos := SearchResult + FDelimiters.SDLen;
    // Verify the next record is an Interchange Control Header
    if ICHSegmentId = Copy(FData, StartPos, Length(ICHSegmentId)) then
    begin
      if (foVariableDelimiterDetection in FEDIFileOptions) then
        if foUseAltDelimiterDetection in FEDIFileOptions then
          InternalAlternateDelimitersDetection(StartPos)
        else
          InternalDelimitersDetection(StartPos);
    end
    else
    if (StartPos + Length(ICHSegmentId)) < Length(FData) then
    begin
      if foIgnoreGarbageAtEndOfFile in FEDIFileOptions then
        Break
      else
        raise EJclEDIError.CreateID(18);
    end;
  end;
  FData := '';
  FState := ediDisassembled;
end;

function TEDIFile.GetInterchangeControl(Index: Integer): TEDIInterchangeControl;
begin
  Result := TEDIInterchangeControl(GetEDIDataObject(Index));
end;

function TEDIFile.InsertInterchange(InsertIndex: Integer;
  Interchange: TEDIInterchangeControl): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, Interchange);
end;

function TEDIFile.InsertInterchange(InsertIndex: Integer): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex);
end;

function TEDIFile.InsertInterchanges(InsertIndex, Count: Integer): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

function TEDIFile.InsertInterchanges(InsertIndex: Integer;
  InterchangeControlArray: TEDIInterchangeControlArray): Integer;
{$IFDEF CLR}
var
  HelpArray: TEDIDataObjectArray;
  I: Integer;
{$ENDIF CLR}
begin
  {$IFDEF CLR}
  SetLength(HelpArray, Length(InterchangeControlArray));
  for I := 0 to High(InterchangeControlArray) do
    HelpArray[I] := TEDIDataObject(InterchangeControlArray[I]);
  Result := InsertEDIDataObjects(InsertIndex, HelpArray);
  {$ELSE}
  Result := InsertEDIDataObjects(InsertIndex, TEDIDataObjectArray(InterchangeControlArray));
  {$ENDIF CLR}
end;

procedure TEDIFile.InternalLoadFromFile;
var
  EDIFileStream: TFileStream;
  {$IFDEF CLR}
  Buf: TBytes;
  {$ENDIF CLR}
begin
  FData := '';
  if FFileName <> '' then
  begin
    EDIFileStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
    try
      {$IFDEF CLR}
      SetLength(Buf, EDIFileStream.Size);
      EDIFileStream.Read(Buf, EDIFileStream.Size);
      FData := StringOf(Buf);
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

procedure TEDIFile.LoadFromFile(const FileName: string);
begin
  if FileName <> '' then
    FFileName := FileName;
  InternalLoadFromFile;
end;

procedure TEDIFile.ReLoadFromFile;
begin
  InternalLoadFromFile;
end;

procedure TEDIFile.SaveAsToFile(const FileName: string);
var
  EDIFileStream: TFileStream;
begin
  FFileName := FileName;
  if FFileName <> '' then
  begin
    EDIFileStream := TFileStream.Create(FFileName, fmCreate or fmShareDenyNone);
    try
      {$IFDEF CLR}
      EDIFileStream.Write(BytesOf(FData), Length(FData));
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

procedure TEDIFile.SaveToFile;
var
  EDIFileStream: TFileStream;
begin
  if FFileName <> '' then
  begin
    EDIFileStream := TFileStream.Create(FFileName, fmCreate or fmShareDenyNone);
    try
      {$IFDEF CLR}
      EDIFileStream.Write(BytesOf(FData), Length(FData));
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

procedure TEDIFile.SetInterchangeControl(Index: Integer; Interchange: TEDIInterchangeControl);
begin
  SetEDIDataObject(Index, Interchange);
end;

procedure TEDIFile.InternalDelimitersDetection(StartPos: Integer);
var
  I, SearchResult: Integer;
begin
  SearchResult := 1;
  FDelimiters.ED := Copy(FData, StartPos + Length(ICHSegmentId), 1);
  for I := 0 to 15 do
  begin
    SearchResult := StrSearch(FDelimiters.ED, FData, SearchResult);
    SearchResult := SearchResult + 1;
  end;                                                            
  FDelimiters.SS := Copy(FData, SearchResult, 1);
  if Copy(FData, SearchResult + 1, 2) = AnsiCrLf then
    FDelimiters.SD := Copy(FData, SearchResult + 1, 2)
  else
    FDelimiters.SD := Copy(FData, SearchResult + 1, 1);
end;

procedure TEDIFile.InternalAlternateDelimitersDetection(StartPos: Integer);
var
  SearchResult: Integer;
begin
  SearchResult := 1;
  FDelimiters.ED := Copy(FData, StartPos + Length(ICHSegmentId), 1);
  SearchResult := StrSearch(FGHSegmentId + FDelimiters.ED, FData, SearchResult);
  if SearchResult = 0 then
    SearchResult := StrSearch(TA1SegmentId + FDelimiters.ED, FData, 1); 
  if Copy(FData, SearchResult - 2, 2) = AnsiCrLf then
  begin
    FDelimiters.SS := Copy(FData, SearchResult - 3, 1);
    FDelimiters.SD := Copy(FData, SearchResult - 2, 2);
  end
  else
  begin
    FDelimiters.SS := Copy(FData, SearchResult - 2, 1);
    FDelimiters.SD := Copy(FData, SearchResult - 1, 1);
  end;
end;

function TEDIFile.InternalCreateInterchangeControl: TEDIInterchangeControl;
begin
  Result := TEDIInterchangeControl.Create(Self);
end;

procedure TEDIFile.DeleteInterchange(Interchange: TEDIInterchangeControl);
begin
  DeleteEDIDataObject(Interchange);
end;

function TEDIFile.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := TEDIDelimiters.Create;
end;

function TEDIFile.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateInterchangeControl;
end;

//=== { TEDIElementSpec } ====================================================

constructor TEDIElementSpec.Create(Parent: TEDIDataObject);
begin
  inherited Create(Parent);
  FReservedData := TStringList.Create;
  FElementId := '';
  FPosition := 0;
  FDescription := '';
  FRequirementDesignator := '';
  FType := '';
  FMinimumLength := 1;
  FMaximumLength := 1;
end;

destructor TEDIElementSpec.Destroy;
begin
  FReservedData.Free;
  inherited Destroy;
end;

function TEDIElementSpec.Assemble: string;
begin
  if FElementId <> ElementSpecId_Reserved then
  begin
    if FElementId = '' then
      FElementId := Value_NotAssigned;
    ReservedData.Values[RDFN_Id] := FElementId;
    ReservedData.Values[RDFN_Position] := IntToStr(FPosition);
    if FDescription = '' then
      FDescription := Value_None;
    ReservedData.Values[RDFN_Description] := FDescription;
    if FNotes = '' then
      FNotes := Value_None;
    ReservedData.Values[RDFN_Notes] := FNotes;
    if FRequirementDesignator = '' then
      FRequirementDesignator := Value_Optional;
    ReservedData.Values[RDFN_RequirementDesignator] := FRequirementDesignator;
    if FType = '' then
      FType := Value_AlphaNumeric;
    ReservedData.Values[RDFN_Type] := FType;
    ReservedData.Values[RDFN_MinimumLength] := IntToStr(FMinimumLength);
    ReservedData.Values[RDFN_MaximumLength] := IntToStr(FMaximumLength);
    FData := ReservedData.CommaText;
  end;
  ReservedData.Clear;
  Result := FData;
  FState := ediAssembled;
end;

procedure TEDIElementSpec.Disassemble;
begin
  ReservedData.Clear;
  ReservedData.CommaText := FData;
  if ReservedData.Values[RDFN_Id] <> ElementSpecId_Reserved then
  begin
    FElementId := ReservedData.Values[RDFN_Id];
    if FElementId = '' then
      FElementId := Value_NotAssigned;
    FPosition := StrToInt(ReservedData.Values[RDFN_Position]);
    FDescription := ReservedData.Values[RDFN_Description];
    if FDescription = '' then
      FDescription := Value_None;
    FNotes := ReservedData.Values[RDFN_Notes];
    if FNotes = '' then
      FNotes := Value_None;
    FRequirementDesignator := ReservedData.Values[RDFN_RequirementDesignator];
    if FRequirementDesignator = '' then
      FRequirementDesignator := Value_Optional;
    FType := ReservedData.Values[RDFN_Type];
    if FType = '' then
      FType := Value_AlphaNumeric;
    FMinimumLength := StrToInt(ReservedData.Values[RDFN_MinimumLength]);
    FMaximumLength := StrToInt(ReservedData.Values[RDFN_MaximumLength]);
  end;
  FState := ediDisassembled;
end;

function TEDIElementSpec.GetReservedData: TStrings;
begin
  Result := FReservedData;
end;

//=== { TEDISegmentSpec } ====================================================

constructor TEDISegmentSpec.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  FReservedData := TStringList.Create;
  FSegmentId := Value_NotAssigned;
  FPosition := 0;
  FDescription := Value_None;
  FRequirementDesignator := Value_Optional;
  FSection := '?';
  FMaximumUsage := 999;
  FOwnerLoopId := NA_LoopId;
  FParentLoopId := NA_LoopId;
end;

destructor TEDISegmentSpec.Destroy;
begin
  FReservedData.Free;
  inherited Destroy;
end;

function TEDISegmentSpec.Assemble: string;
begin
  // Insert Segment Spec as Element[0]
  InsertElement(0);
  TEDIElementSpec(FEDIDataObjects[0]).ElementId := ElementSpecId_Reserved;
  AssembleReservedData(ReservedData);
  FEDIDataObjects[0].Data := ReservedData.CommaText;
  ReservedData.Clear;
  //
  Result := inherited Assemble;
end;

procedure TEDISegmentSpec.AssembleReservedData(ReservedData: TStrings);
begin
  with ReservedData do
  begin
    BeginUpdate;
    try
      Values[RDFN_Id] := ElementSpecId_Reserved;
      Values[RDFN_Position] := IntToStr(FPosition);
      Values[RDFN_Description] := FDescription;
      Values[RDFN_Notes] := FNotes;
      Values[RDFN_Section] := FSection;
      Values[RDFN_RequirementDesignator] := FRequirementDesignator;
      Values[RDFN_MaximumUsage] := IntToStr(FMaximumUsage);
      if FOwnerLoopId = '' then
        FOwnerLoopId := NA_LoopId;
      Values[RDFN_OwnerLoopId] := FOwnerLoopId;
      if FParentLoopId = '' then
        FParentLoopId := NA_LoopId;
      Values[RDFN_ParentLoopId] := FParentLoopId;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TEDISegmentSpec.Disassemble;
begin
  inherited Disassemble;
  // Element[0] is always the Segment Spec
  ReservedData.Clear;
  ReservedData.CommaText := FEDIDataObjects[0].Data;
  DisassembleReservedData(ReservedData);
  DeleteElement(0);
end;

function TEDISegmentSpec.GetReservedData: TStrings;
begin
  Result := FReservedData;
end;

procedure TEDISegmentSpec.ValidateElementIndexPositions;
var
  I: Integer;
begin
  for I := 0 to GetCount - 1 do
    TEDIElementSpec(FEDIDataObjects[I]).Position := I + 1;
end;

procedure TEDISegmentSpec.DisassembleReservedData(ReservedData: TStrings);
begin
  with ReservedData do
  begin
    // FSegmentId already set by the inherited Disassemble
    FPosition := StrToInt(Values[RDFN_Position]);
    FDescription := Values[RDFN_Description];
    FNotes := Values[RDFN_Notes];
    FSection := Values[RDFN_Section];
    FRequirementDesignator := Values[RDFN_RequirementDesignator];
    FMaximumUsage := StrToInt(Values[RDFN_MaximumUsage]);
    FOwnerLoopId := Values[RDFN_OwnerLoopId];
    if FOwnerLoopId = '' then
      FOwnerLoopId := NA_LoopId;
    FParentLoopId := Values[RDFN_ParentLoopId];
    if FParentLoopId = '' then
      FParentLoopId := NA_LoopId;
  end;
end;

function TEDISegmentSpec.InternalCreateElement: TEDIElement;
begin
  Result := TEDIElementSpec.Create(Self);
end;

//=== { TEDITransactionSetSegmentSpec } ======================================

constructor TEDITransactionSetSegmentSpec.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  if Assigned(Parent) and (Parent is TEDITransactionSet) then
    FParent := Parent;
  FRequirementDesignator := Value_Mandatory;
  FMaximumUsage := 1;
end;

function TEDITransactionSetSegmentSpec.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := inherited InternalAssignDelimiters;
end;

//=== { TEDITransactionSetSegmentSTSpec } ====================================

constructor TEDITransactionSetSegmentSTSpec.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  FSegmentId := TSHSegmentId;
  FPosition := 0;
end;

procedure TEDITransactionSetSegmentSTSpec.AssembleReservedData(ReservedData: TStrings);
var
  Spec: TEDITransactionSetSpec;
begin
  if Parent is TEDITransactionSetSpec then
  begin
    Spec := TEDITransactionSetSpec(Parent);
    if Spec.TransactionSetId = '' then
      Spec.TransactionSetId := Value_Unknown;
    ReservedData.BeginUpdate;
    try
      ReservedData.Values[RDFN_TransSetId] := Spec.TransactionSetId;
      if Spec.TSDescription = '' then
        Spec.TSDescription := Value_None;
      ReservedData.Values[RDFN_TransSetDesc] := Spec.TSDescription;
    finally
      ReservedData.EndUpdate;
    end;
  end;
  inherited AssembleReservedData(ReservedData);
end;

procedure TEDITransactionSetSegmentSTSpec.DisassembleReservedData(ReservedData: TStrings);
var
  Spec: TEDITransactionSetSpec;
begin
  inherited DisassembleReservedData(ReservedData);
  if Parent is TEDITransactionSetSpec then
  begin
    Spec := TEDITransactionSetSpec(Parent);
    Spec.TransactionSetId := ReservedData.Values[RDFN_TransSetId];
    if Spec.TransactionSetId = '' then
      Spec.TransactionSetId := Value_Unknown;
    Spec.TSDescription := ReservedData.Values[RDFN_TransSetDesc];
    if Spec.TSDescription = '' then
      Spec.TSDescription := Value_None;
  end;
end;

//=== { TEDIFunctionalGroupSegmentSpec } =====================================

constructor TEDIFunctionalGroupSegmentSpec.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
    FParent := Parent;
  FRequirementDesignator := Value_Mandatory;
  FMaximumUsage := 1;
end;

function TEDIFunctionalGroupSegmentSpec.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  // Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
    // Get the delimiters from the functional group
    if Assigned(Parent) and (Parent is TEDIFunctionalGroup) then
    begin
      if Assigned(Parent.Delimiters) then
      begin
        Result := Parent.Delimiters;
        Exit;
      end;
      // Get the delimiters from the interchange control
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIInterchangeControl) then
        Result := Parent.Parent.Delimiters;
    end;
end;

//=== { TEDIFunctionalGroupSegmentGSSpec } ===================================

constructor TEDIFunctionalGroupSegmentGSSpec.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  FSegmentId := FGHSegmentId;
  FPosition := -1;
end;

procedure TEDIFunctionalGroupSegmentGSSpec.AssembleReservedData(ReservedData: TStrings);
var
  Spec: TEDIFunctionalGroupSpec;
begin
  if Parent is TEDIFunctionalGroupSpec then
  begin
    Spec := TEDIFunctionalGroupSpec(Parent);
    if Spec.FunctionalGroupId = '' then
      Spec.FunctionalGroupId := Value_Unknown;
    ReservedData.BeginUpdate;
    try
      ReservedData.Values[RDFN_FunctionalGroupId] := Spec.FunctionalGroupId;
      if Spec.FGDescription = '' then
        Spec.FGDescription := Value_None;
      ReservedData.Values[RDFN_FGDescription] := Spec.FGDescription;
      if Spec.AgencyCodeId = '' then
        Spec.AgencyCodeId := Value_Unknown;
      ReservedData.Values[RDFN_AgencyCodeId] := Spec.AgencyCodeId;
      if Spec.VersionReleaseId = '' then
        Spec.VersionReleaseId := Value_Unknown;
      ReservedData.Values[RDFN_VersionReleaseId] := Spec.VersionReleaseId;
    finally
      ReservedData.EndUpdate;
    end;
  end;
  inherited AssembleReservedData(ReservedData);
end;

procedure TEDIFunctionalGroupSegmentGSSpec.DisassembleReservedData(ReservedData: TStrings);
var
  Spec: TEDIFunctionalGroupSpec;
begin
  inherited DisassembleReservedData(ReservedData);

  if Parent is TEDIFunctionalGroupSpec then
  begin
    Spec := TEDIFunctionalGroupSpec(Parent);
    Spec.FunctionalGroupId := ReservedData.Values[RDFN_FunctionalGroupId];
    if Spec.FunctionalGroupId = '' then
      Spec.FunctionalGroupId := Value_Unknown;
    Spec.FGDescription := ReservedData.Values[RDFN_FGDescription];
    if Spec.FGDescription = '' then
      Spec.FGDescription := Value_None;
    Spec.AgencyCodeId := ReservedData.Values[RDFN_AgencyCodeId];
    if Spec.AgencyCodeId = '' then
      Spec.AgencyCodeId := Value_Unknown;
    Spec.VersionReleaseId := ReservedData.Values[RDFN_VersionReleaseId];
    if Spec.VersionReleaseId = '' then
      Spec.VersionReleaseId := Value_Unknown;
  end;
end;

//=== { TEDIInterchangeControlSegmentSpec } ==================================

constructor TEDIInterchangeControlSegmentSpec.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
    FParent := Parent;
  FRequirementDesignator := Value_Mandatory;
  FMaximumUsage := 1;
end;

function TEDIInterchangeControlSegmentSpec.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  // Attempt to assign the delimiters
  if not Assigned(FDelimiters) then
    // Get the delimiters from the interchange control
    if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
      Result := Parent.Delimiters;
end;

//=== { TEDIInterchangeControlSegmentISASpec } ===============================

constructor TEDIInterchangeControlSegmentISASpec.Create(Parent: TEDIDataObject;
  ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  FSegmentId := ICHSegmentId;
  FPosition := -2;
end;

function TEDIInterchangeControlSegmentISASpec.Assemble: string;
begin
  // Because the last element carries specification data and not the subelement separator
  // the subelement separator must be added as an additional element.
  Result := inherited Assemble;
  Result := Copy(Result, 1, Length(Result)-1) + FDelimiters.ED + FDelimiters.SS + FDelimiters.SD;
end;

procedure TEDIInterchangeControlSegmentISASpec.AssembleReservedData(ReservedData: TStrings);
var
  Spec: TEDIInterchangeControlSpec;
begin
  if Parent is TEDIInterchangeControlSpec then
  begin
    Spec := TEDIInterchangeControlSpec(Parent);
    if Spec.StandardId = '' then
      Spec.StandardId := Value_Unknown;
    ReservedData.BeginUpdate;
    try
      ReservedData.Values[RDFN_StandardId] := Spec.StandardId;
      if Spec.VersionId = '' then
        Spec.VersionId := Value_Unknown;
      ReservedData.Values[RDFN_VersionId] := Spec.VersionId;
      if Spec.ICDescription = '' then
        Spec.ICDescription := Value_None;
      ReservedData.Values[RDFN_ICDescription] := Spec.ICDescription;
    finally
      ReservedData.EndUpdate;
    end;
  end;
  inherited AssembleReservedData(ReservedData);
end;

procedure TEDIInterchangeControlSegmentISASpec.Disassemble;
var
  SearchResult: Integer;
begin
  // Because the subelement separator was added as an additional element it must now be removed.
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(35);
  end;
  SearchResult := StrSearch(FDelimiters.ED + FDelimiters.SS, FData, 1);
  if SearchResult <> 0 then
    FData := StringReplace(FData, FDelimiters.ED + FDelimiters.SS, '', [rfReplaceAll]);
  inherited Disassemble;
end;

procedure TEDIInterchangeControlSegmentISASpec.DisassembleReservedData(ReservedData: TStrings);
var
  Spec: TEDIInterchangeControlSpec;
begin
  inherited DisassembleReservedData(ReservedData);
  if Parent is TEDIInterchangeControlSpec then
  begin
    Spec := TEDIInterchangeControlSpec(Parent);
    Spec.StandardId := ReservedData.Values[RDFN_StandardId];
    if Spec.StandardId = '' then
      Spec.StandardId := Value_Unknown;
    Spec.VersionId := ReservedData.Values[RDFN_VersionId];
    if Spec.VersionId = '' then
      Spec.VersionId := Value_Unknown;
    Spec.ICDescription := ReservedData.Values[RDFN_ICDescription];
    if Spec.ICDescription = '' then
      Spec.ICDescription := Value_None;
  end;
end;

//=== { TEDITransactionSetSpec } =============================================

procedure TEDITransactionSetSpec.InternalCreateHeaderTrailerSegments;
begin
  FSTSegment := TEDITransactionSetSegmentSTSpec.Create(Self);
  FSESegment := TEDITransactionSetSegmentSpec.Create(Self);
end;

function TEDITransactionSetSpec.InternalCreateSegment: TEDISegment;
begin
  Result := TEDISegmentSpec.Create(Self);
end;

procedure TEDITransactionSetSpec.ValidateSegmentIndexPositions;
var
  I: Integer;
begin
  for I := 0 to GetCount - 1 do
  begin
    TEDISegmentSpec(FEDIDataObjects[I]).Position := I + 1;
    TEDISegmentSpec(FEDIDataObjects[I]).ValidateElementIndexPositions;
  end;
end;

//=== { TEDIFunctionalGroupSpec } ============================================

procedure TEDIFunctionalGroupSpec.InternalCreateHeaderTrailerSegments;
begin
  FGSSegment := TEDIFunctionalGroupSegmentGSSpec.Create(Self);
  FGESegment := TEDIFunctionalGroupSegmentSpec.Create(Self);
end;

function TEDIFunctionalGroupSpec.InternalCreateTransactionSet: TEDITransactionSet;
begin
  Result := TEDITransactionSetSpec.Create(Self);
end;

function TEDIFunctionalGroupSpec.FindTransactionSetSpec(
  TransactionSetId: string): TEDITransactionSetSpec;
var
  I: Integer;
  EDITransactionSetSpec: TEDITransactionSetSpec;
begin
  Result := nil;
  for I := 0 to GetCount - 1 do
  begin
    EDITransactionSetSpec := TEDITransactionSetSpec(FEDIDataObjects[I]);
    if TransactionSetId = EDITransactionSetSpec.TransactionSetId then
    begin
      Result := EDITransactionSetSpec;
      Break;
    end;
  end;
end;

//=== { TEDIInterchangeControlSpec } =========================================

procedure TEDIInterchangeControlSpec.InternalCreateHeaderTrailerSegments;
begin
  FISASegment := TEDIInterchangeControlSegmentISASpec.Create(Self);
  FIEASegment := TEDIInterchangeControlSegmentSpec.Create(Self);
end;

function TEDIInterchangeControlSpec.InternalCreateFunctionalGroup: TEDIFunctionalGroup;
begin
  Result := TEDIFunctionalGroupSpec.Create(Self);
end;

function TEDIInterchangeControlSpec.FindTransactionSetSpec(FunctionalGroupId, AgencyCodeId,
  VersionReleaseId, TransactionSetId: string): TEDITransactionSetSpec;
var
  EDIFunctionalGroupSpec: TEDIFunctionalGroupSpec;
begin
  Result := nil;
  EDIFunctionalGroupSpec := FindFunctionalGroupSpec(FunctionalGroupId, AgencyCodeId,
    VersionReleaseId);
  if EDIFunctionalGroupSpec <> nil then
    Result := EDIFunctionalGroupSpec.FindTransactionSetSpec(TransactionSetId);
end;

function TEDIInterchangeControlSpec.FindFunctionalGroupSpec(FunctionalGroupId, AgencyCodeId,
  VersionReleaseId: string): TEDIFunctionalGroupSpec;
var
  F: Integer;
  EDIFunctionalGroupSpec: TEDIFunctionalGroupSpec;
begin
  Result := nil;
  for F := 0 to GetCount - 1 do
  begin
    EDIFunctionalGroupSpec := TEDIFunctionalGroupSpec(FEDIDataObjects[F]);
    if (FunctionalGroupId = EDIFunctionalGroupSpec.FunctionalGroupId) and
      (AgencyCodeId = EDIFunctionalGroupSpec.AgencyCodeId) and
      (VersionReleaseId = EDIFunctionalGroupSpec.VersionReleaseId) then
    begin
      Result := EDIFunctionalGroupSpec;
      Exit;
    end;
  end;
end;

//=== { TEDIFileSpec } =======================================================

constructor TEDIFileSpec.Create(Parent: TEDIDataObject; InterchangeCount: Integer);
begin
  inherited Create(Parent, InterchangeCount);
  FEDIFileOptions := [foVariableDelimiterDetection, foUseAltDelimiterDetection];
end;

procedure TEDIFileSpec.InternalDelimitersDetection(StartPos: Integer);
begin
  InternalAlternateDelimitersDetection(StartPos);
end;

function TEDIFileSpec.InternalCreateInterchangeControl: TEDIInterchangeControl;
begin
  Result := TEDIInterchangeControlSpec.Create(Self);
end;

function TEDIFileSpec.FindTransactionSetSpec(StandardId, VersionId, FunctionalGroupId, AgencyCodeId,
  VersionReleaseId, TransactionSetId: string): TEDITransactionSetSpec;
var
  EDIFunctionalGroupSpec: TEDIFunctionalGroupSpec;
begin
  Result := nil;
  EDIFunctionalGroupSpec := FindFunctionalGroupSpec(StandardId, VersionId, FunctionalGroupId,
    AgencyCodeId, VersionReleaseId);
  if EDIFunctionalGroupSpec <> nil then
    Result := EDIFunctionalGroupSpec.FindTransactionSetSpec(TransactionSetId);
end;

function TEDIFileSpec.FindFunctionalGroupSpec(StandardId, VersionId, FunctionalGroupId,
  AgencyCodeId, VersionReleaseId: string): TEDIFunctionalGroupSpec;
var
  EDIInterchangeControlSpec: TEDIInterchangeControlSpec;
begin
  Result := nil;
  EDIInterchangeControlSpec := FindInterchangeControlSpec(StandardId, VersionId);
  if EDIInterchangeControlSpec <> nil then
    Result := EDIInterchangeControlSpec.FindFunctionalGroupSpec(FunctionalGroupId,
      AgencyCodeId, VersionReleaseId);
end;

function TEDIFileSpec.FindInterchangeControlSpec(StandardId,
  VersionId: string): TEDIInterchangeControlSpec;
var
  I: Integer;
  EDIInterchangeControlSpec: TEDIInterchangeControlSpec;
begin
  Result := nil;
  for I := 0 to GetCount - 1 do
  begin
    EDIInterchangeControlSpec := TEDIInterchangeControlSpec(FEDIDataObjects[I]);
    if (EDIInterchangeControlSpec.StandardId = StandardId) and
       (EDIInterchangeControlSpec.VersionId = VersionId) then
      Result := EDIInterchangeControlSpec;
  end;
end;

//=== { TEDITransactionSetLoop } =============================================

constructor TEDITransactionSetLoop.Create(Parent: TEDIDataObject);
begin
  inherited Create(Parent);
  FCreateObjectType := ediLoop;  
  FGroupIsParent := False;
  if Assigned(Parent) and (Parent is TEDITransactionSet) then
    FParentTransactionSet := TEDITransactionSet(Parent)
  else
  if Assigned(Parent) and (Parent is TEDITransactionSetLoop) then
    FParentTransactionSet := TEDITransactionSetLoop(Parent).ParentTransactionSet
  else
    FParentTransactionSet := nil;
  FEDIDOT := ediLoop;
  FEDIDataObjects.OwnsObjects := False;
end;

destructor TEDITransactionSetLoop.Destroy;
begin
  DeleteEDIDataObjects;
  inherited Destroy;
end;

function TEDITransactionSetLoop.AddLoop(OwnerLoopId, ParentLoopId: string): Integer;
var
  Loop: TEDITransactionSetLoop;
begin
  FCreateObjectType := ediLoop;
  Loop := TEDITransactionSetLoop(InternalCreateEDIDataObject);
  Loop.OwnerLoopId := OwnerLoopId;
  Loop.ParentLoopId := ParentLoopId;
  Loop.Parent := Self;
  Result := AppendEDIDataObject(Loop);
end;

procedure TEDITransactionSetLoop.AppendSegment(Segment: TEDISegment);
begin
  AppendEDIDataObject(Segment);
end;

function TEDITransactionSetLoop.Assemble: string;
begin
  Result := '';
end;

procedure TEDITransactionSetLoop.DeleteEDIDataObjects;
var
  I: Integer;
begin
  for I := 0 to FEDIDataObjects.Count - 1 do
    if Assigned(FEDIDataObjects[I]) then
      try
        // Delete
        if FEDIDataObjects[I] is TEDITransactionSetLoop then
          FEDIDataObjects.Item[I].FreeAndNilEDIDataObject
        else
          // Do not free segments because they are not owned by
          FEDIDataObjects[I] := nil;
      except
        // This exception block was put here to capture the case where FEDIDataObjects[I] was
        // actually destroyed prior to destroying this object.
        FEDIDataObjects[I] := nil;
      end;
  // Resize
  FEDIDataObjects.Clear;
end;

procedure TEDITransactionSetLoop.Disassemble;
begin
  // Do Nothing
end;

function TEDITransactionSetLoop.FindLoop(LoopId: string;
  var StartIndex: Integer): TEDITransactionSetLoop;
var
  I, J: Integer;
begin
  Result := nil;
  J := StartIndex;
  for I := StartIndex to GetCount {FEDIDataObjects.Count} - 1 do
  begin
    StartIndex := I;
    if FEDIDataObjects[I] is TEDITransactionSetLoop then
    begin
      Result := TEDITransactionSetLoop(GetEDIDataObject(I));
      if Result.OwnerLoopId = LoopId then
      begin
        Inc(StartIndex);
        Break;
      end;
      Result := nil;
    end;
  end;
  if Result = nil then
    StartIndex := J;
end;

function TEDITransactionSetLoop.FindSegment(SegmentId: string; var StartIndex: Integer): TEDISegment;
var
  I, J: Integer;
begin
  Result := nil;
  J := StartIndex;
  for I := StartIndex to GetCount {FEDIDataObjects.Count} - 1 do
  begin
    StartIndex := I;
    if FEDIDataObjects[I] is TEDISegment then
    begin
      Result := TEDISegment(GetEDIDataObject(I));
      if Result.SegmentId = SegmentId then
      begin
        Inc(StartIndex);
        Break;
      end;
      Result := nil;
    end;
  end;
  if Result = nil then
    StartIndex := J;
end;

function TEDITransactionSetLoop.FindSegment(SegmentId: string; var StartIndex: Integer;
  ElementConditions: TStrings): TEDISegment;
var
  I, TrueCount, ElementIndex: Integer;
  Name: string;
begin
  Result := FindSegment(SegmentId, StartIndex);
  while Result <> nil do
  begin
    TrueCount := 0;
    for I := 0 to ElementConditions.Count - 1 do
    begin
      Name := ElementConditions.Names[I];
      ElementIndex := StrToInt(Name);
      if Result[ElementIndex].Data = ElementConditions.Values[Name] then
        Inc(TrueCount);
    end;
    if TrueCount = ElementConditions.Count then
      Break;
    Result := FindSegment(SegmentId, StartIndex);
  end;
end;

function TEDITransactionSetLoop.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  if FDelimiters = nil then // Attempt to assign the delimiters
    if Assigned(FParentTransactionSet) then
      Result := FParentTransactionSet.Delimiters;
end;

function TEDITransactionSetLoop.InternalCreateEDIDataObject: TEDIDataObject;
begin
  case FCreateObjectType of
    ediLoop:
    begin
      Result := TEDITransactionSetLoop.Create(Self);
      TEDITransactionSetLoop(Result).OwnerLoopId := OwnerLoopId;
      TEDITransactionSetLoop(Result).ParentLoopId := ParentLoopId;
      TEDITransactionSetLoop(Result).Parent := Self;
    end;
  else
    Result := nil;
  end;
end;

//=== { TEDITransactionSetDocument } =========================================

constructor TEDITransactionSetDocument.Create(Parent: TEDIDataObject;
  EDITransactionSet: TEDITransactionSet;
  EDITransactionSetSpec: TEDITransactionSetSpec);
begin
  inherited Create(Parent);
  FEDILoopStack := TEDILoopStack.Create;
  FEDILoopStack.OnAddLoop := AddLoopToDoc;
  FEDITransactionSet := EDITransactionSet;
  FEDITransactionSetSpec := EDITransactionSetSpec;
  FEDITSDOptions := [];
end;

destructor TEDITransactionSetDocument.Destroy;
begin
  FreeAndNil(FEDILoopStack);
  FEDITransactionSet := nil;
  FEDITransactionSetSpec := nil;
  inherited Destroy;
end;

procedure TEDITransactionSetDocument.FormatDocument;
var
  I, J: Integer;
  LSR: TEDILoopStackRecord;
  DataSegment: TEDISegment;
  SpecSegment: TEDISegmentSpec;
  EDIFunctionalGroup: TEDIFunctionalGroup;
  EDIFunctionalGroupSpec: TEDIFunctionalGroupSpec;
  EDIInterchangeControl: TEDIInterchangeControl;
  EDIInterchangeControlSpec: TEDIInterchangeControlSpec;
begin
  I := 0;
  J := 0;
  if doLinkSpecToDataObject in FEDITSDOptions then
  begin
    FEDITransactionSet.SpecPointer := FEDITransactionSetSpec;
    FEDITransactionSet.SegmentST.SpecPointer := FEDITransactionSetSpec.SegmentST;
    SetSpecificationPointers(FEDITransactionSet.SegmentST, FEDITransactionSetSpec.SegmentST);
    FEDITransactionSet.SegmentSE.SpecPointer := FEDITransactionSetSpec.SegmentSE;
    SetSpecificationPointers(FEDITransactionSet.SegmentSE, FEDITransactionSetSpec.SegmentSE);
    if FEDITransactionSet.Parent <> nil then
    begin
      EDIFunctionalGroup := TEDIFunctionalGroup(FEDITransactionSet.Parent);
      EDIFunctionalGroupSpec := TEDIFunctionalGroupSpec(FEDITransactionSetSpec.Parent);
      EDIFunctionalGroup.SpecPointer := EDIFunctionalGroupSpec;
      EDIFunctionalGroup.SegmentGS.SpecPointer := EDIFunctionalGroupSpec.SegmentGS;
      SetSpecificationPointers(EDIFunctionalGroup.SegmentGS, EDIFunctionalGroupSpec.SegmentGS);
      EDIFunctionalGroup.SegmentGE.SpecPointer := EDIFunctionalGroupSpec.SegmentGE;
      SetSpecificationPointers(EDIFunctionalGroup.SegmentGE, EDIFunctionalGroupSpec.SegmentGE);
      if EDIFunctionalGroup.Parent <> nil then
      begin
        EDIInterchangeControl := TEDIInterchangeControl(EDIFunctionalGroup.Parent);
        EDIInterchangeControlSpec := TEDIInterchangeControlSpec(EDIFunctionalGroupSpec.Parent);
        EDIInterchangeControl.SpecPointer := EDIInterchangeControlSpec;
        EDIInterchangeControl.SegmentISA.SpecPointer := EDIInterchangeControlSpec.SegmentISA;
        SetSpecificationPointers(EDIInterchangeControl.SegmentISA, EDIInterchangeControlSpec.SegmentISA);
        EDIInterchangeControl.SegmentIEA.SpecPointer := EDIInterchangeControlSpec.SegmentIEA;
        SetSpecificationPointers(EDIInterchangeControl.SegmentIEA, EDIInterchangeControlSpec.SegmentIEA);
      end;
    end;
  end;
  // Initialize the stack
  FEDILoopStack.Flags := FEDILoopStack.Flags - [ediLoopRepeated];
  LSR := FEDILoopStack.ValidateLoopStack(FEDITransactionSet.Segment[I].SegmentID,
    NA_LoopId, NA_LoopId, 0, Self);
  //
  while (I <= FEDITransactionSet.SegmentCount - 1) and
    (J <= FEDITransactionSetSpec.SegmentCount - 1) do
  begin
    FEDILoopStack.Flags := FEDILoopStack.Flags - [ediLoopRepeated];
    DataSegment := FEDITransactionSet.Segment[I];
    // If loop has repeated then move the spec index back
    J := ValidateSegSpecIndex(DataSegment.SegmentID, J);
    // Check current segment against segment spec
    SpecSegment := TEDISegmentSpec(FEDITransactionSetSpec.Segment[J]);
    if DataSegment.SegmentID = SpecSegment.SegmentID then
    begin
      // Retrieve the correct record to use from the stack
      LSR := FEDILoopStack.ValidateLoopStack(SpecSegment.SegmentID, SpecSegment.OwnerLoopId,
        SpecSegment.ParentLoopId, J, LSR.EDIObject);
      //
      // Debug - Keep the following here in case someone wants to debug what happens to the stack.
      // ShowMessage('Current Data Segment: [' + IntToStr(I) + '] ' + DataSegment.SegmentID + #13#10 +
      //             'Current Spec Segment: [' + IntToStr(J) + '] ' + SpecSegment.SegmentID + #13#10 +
      //             FEDILoopStack.Debug);
      //
      // Do error checking and data validation in decendent class
      ValidateData(Self, FEDILoopStack, DataSegment, SpecSegment, I, J, FErrorOccured);
      if FErrorOccured then
        Exit;
      // Process Segment Id
      TEDITransactionSetLoop(LSR.EDIObject).AppendSegment(DataSegment);
      //
      if doLinkSpecToDataObject in FEDITSDOptions then
        SetSpecificationPointers(DataSegment, SpecSegment);
      // Move to the next data segment
      Inc(I);
    end
    else
    begin
      // Do error checking and data validation in decendent class
      ValidateData(Self, FEDILoopStack, DataSegment, SpecSegment, I, J, FErrorOccured);
      if FErrorOccured then
        Exit;
      //
      // Debug - Keep the following here in case someone wants to debug what happens to the stack.
      // ShowMessage('Current Data Segment: [' + IntToStr(I) + '] ' + DataSegment.SegmentID + #13#10 +
      //             'Current Spec Segment: [' + IntToStr(J) + '] ' + SpecSegment.SegmentID + #13#10 +
      //             FEDILoopStack.Debug);
      //
      // Move to the next specification segment
      J := AdvanceSegSpecIndex(I, J, FEDITransactionSetSpec.SegmentCount - 1); //Inc(J);
    end;
  end;
end;

procedure TEDITransactionSetDocument.ValidateData(
  TSDocument: TEDITransactionSetDocument; LoopStack: TEDILoopStack;
  DataSegment, SpecSegment: TEDISegment; var DataIndex, SpecIndex: Integer;
  var ErrorOccured: Boolean);
begin
  ErrorOccured := False;
end;

function TEDITransactionSetDocument.AdvanceSegSpecIndex(DataIndex, SpecStartIndex,
  SpecEndIndex: Integer): Integer;
var
  DataSegment: TEDISegment;
  TestSegment: TEDISegmentSpec;
  I: Integer;
begin
  Result := SpecEndIndex + 1;
  DataSegment := FEDITransactionSet.Segment[DataIndex];
  for I := SpecStartIndex + 1 to SpecEndIndex do
  begin
    TestSegment := TEDISegmentSpec(FEDITransactionSetSpec.Segment[I]);
    // Find matching segment
    if ((DataSegment.SegmentID) = (TestSegment.SegmentID)) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TEDITransactionSetDocument.SetSpecificationPointers(DataSegment, SpecSegment: TEDISegment);
var
  I, J: Integer;
begin
  DataSegment.SpecPointer := SpecSegment;
  J := SpecSegment.ElementCount - 1;
  for I := 0 to DataSegment.ElementCount - 1 do
  begin
    if I > J then
      raise EJclEDIError.CreateIDFmt(58, [IntToStr(I), DataSegment.SegmentID,
         IntToStr(DataSegment.GetIndexPositionFromParent)]);
    DataSegment.Element[I].SpecPointer := SpecSegment.Element[I];
  end;
end;

function TEDITransactionSetDocument.ValidateSegSpecIndex(DataSegmentId: string;
  SpecStartIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := SpecStartIndex;
  // Find the segment in the stack to determine if a loop has repeated
  for I := High(FEDILoopStack.Stack) downto Low(FEDILoopStack.Stack) do
  begin
    if (DataSegmentId = FEDILoopStack.Stack[I].SegmentId) and
      (FEDILoopStack.Stack[I].OwnerLoopId <> NA_LoopId) then
    begin
      FEDILoopStack.Flags := FEDILoopStack.Flags + [ediLoopRepeated];
      Result := FEDILoopStack.Stack[I].SpecStartIndex;
      Break;
    end;
  end;
end;

procedure TEDITransactionSetDocument.AddLoopToDoc(StackRecord: TEDILoopStackRecord;
  SegmentId, OwnerLoopId, ParentLoopId: string; var EDIObject: TEDIObject);
var
  I: Integer;
  Loop: TEDITransactionSetLoop;
begin
  Loop := TEDITransactionSetLoop(StackRecord.EDIObject);
  I := Loop.AddLoop(OwnerLoopId, ParentLoopId);
  EDIObject := TEDITransactionSetLoop(Loop[I]);
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
