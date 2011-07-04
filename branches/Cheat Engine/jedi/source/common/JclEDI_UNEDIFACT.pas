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
{ The Original Code is JclEDI_UNEDIFACT.pas.                                                       }
{                                                                                                  }
{ The Initial Developer of the Original Code is Raymond Alexander.                                 }
{ Portions created by Raymond Alexander are Copyright (C) Raymond Alexander. All rights reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Raymond Alexander (rayspostbox3), Robert Marquardt, Robert Rossmair, Petr Vones                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains classes to easily parse EDI documents and data. Variable delimiter detection allows     }
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

unit JclEDI_UNEDIFACT;

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
  JclBase, JclEDI;

const
  //  UN/EDIFACT Segment Id's
  UNASegmentId = 'UNA';  // Service String Advice Segment Id
  UNBSegmentId = 'UNB';  // Interchange Control Header Segment Id
  UNZSegmentId = 'UNZ';  // Interchange Control Trailer Segment Id
  UNGSegmentId = 'UNG';  // Functional Group Header Segment Id
  UNESegmentId = 'UNE';  // Functional Group Trailer Segment Id
  UNHSegmentId = 'UNH';  // Message (Transaction Set) Header Segment Id
  UNTSegmentId = 'UNT';  // Message (Transaction Set) Trailer Segment Id

type
  //  EDI Forward Class Declarations
  TEDIElement = class;
  TEDICompositeElement = class;
  TEDISegment = class;
  TEDIMessage = class; // (Transaction Set)
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

  //  EDI Composite Element Classes
  TEDICompositeElement = class(TEDIDataObjectGroup)
  private
    function GetElement(Index: Integer): TEDIElement;
    procedure SetElement(Index: Integer; Element: TEDIElement);
  protected
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
  end;

  TEDICompositeElementArray = array of TEDICompositeElement;

  //  EDI Segment Classes
  TEDISegment = class(TEDIDataObjectGroup)
  private
    FSegmentID: string;
    //FSegmentIdData: T??? // ToDo: ex: AAA:1:1:2+data1+data2'
  protected
    function InternalCreateElement: TEDIElement; virtual;
    function InternalCreateCompositeElement: TEDICompositeElement; virtual;
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
    function AddCompositeElement: Integer;
    function AppendCompositeElement(CompositeElement: TEDICompositeElement): Integer;
    function InsertCompositeElement(InsertIndex: Integer): Integer; overload;
    function InsertCompositeElement(InsertIndex: Integer;
      CompositeElement: TEDICompositeElement): Integer; overload;
    //
    function AddCompositeElements(Count: Integer): Integer;
    function AppendCompositeElements(CompositeElementArray: TEDICompositeElementArray): Integer;
    function InsertCompositeElements(InsertIndex, Count: Integer): Integer; overload;
    function InsertCompositeElements(InsertIndex: Integer;
      CompositeElementArray: TEDICompositeElementArray): Integer; overload;
    //
    function Assemble: string; override;
    procedure Disassemble; override;
  published
    property SegmentID: string read FSegmentID write FSegmentID;
    property ElementCount: Integer read GetCount;
  end;

  TEDISegmentArray = array of TEDISegment;

  TEDIMessageSegment = class(TEDISegment)
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

  //  EDI Transaction Set Loop
  TEDIMessageLoop = class(TEDIDataObjectGroup)
  protected
    FOwnerLoopId: string;
    FParentLoopId: string;
    FParentMessage: TEDIMessage;
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
    function FindLoop(LoopId: string; var StartIndex: Integer): TEDIMessageLoop;
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
    property ParentMessage: TEDIMessage read FParentMessage write FParentMessage;
  end;

  //  EDI Message (Transaction Set)
  TEDIMessage = class(TEDIDataObjectGroup)
  private
    FUNHSegment: TEDIMessageSegment;
    FUNTSegment: TEDIMessageSegment;
    function GetSegment(Index: Integer): TEDISegment;
    procedure SetSegment(Index: Integer; Segment: TEDISegment);
    procedure SetUNHSegment(const UNHSegment: TEDIMessageSegment);
    procedure SetUNTSegment(const UNTSegment: TEDIMessageSegment);
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
    property SegmentUNH: TEDIMessageSegment read FUNHSegment write SetUNHSegment;
    property SegmentUNT: TEDIMessageSegment read FUNTSegment write SetUNTSegment;
    property SegmentCount: Integer read GetCount;
  end;

  TEDIMessageArray = array of TEDIMessage;

  //  EDI Functional Group
  TEDIFunctionalGroup = class(TEDIDataObjectGroup)
  private
    FUNGSegment: TEDIFunctionalGroupSegment;
    FUNESegment: TEDIFunctionalGroupSegment;
    function GetMessage(Index: Integer): TEDIMessage;
    procedure SetMessage(Index: Integer; Message: TEDIMessage);
    procedure SetUNGSegment(const UNGSegment: TEDIFunctionalGroupSegment);
    procedure SetUNESegment(const UNESegment: TEDIFunctionalGroupSegment);
  protected
    procedure InternalCreateHeaderTrailerSegments; virtual;
    function InternalCreateMessage: TEDIMessage; virtual;
    function InternalAssignDelimiters: TEDIDelimiters; override;
    function InternalCreateEDIDataObject: TEDIDataObject; override;
  public
    constructor Create(Parent: TEDIDataObject; MessageCount: Integer = 0); reintroduce;
    destructor Destroy; override;

    function AddMessage: Integer;
    function AppendMessage(Message: TEDIMessage): Integer;
    function InsertMessage(InsertIndex: Integer): Integer; overload;
    function InsertMessage(InsertIndex: Integer;
      Message: TEDIMessage): Integer; overload;
    procedure DeleteMessage(Index: Integer); overload;
    procedure DeleteMessage(Message: TEDIMessage); overload;

    function AddMessages(Count: Integer): Integer;
    function AppendMessages(MessageArray: TEDIMessageArray): Integer;
    function InsertMessages(InsertIndex, Count: Integer): Integer; overload;
    function InsertMessages(InsertIndex: Integer;
      MessageArray: TEDIMessageArray): Integer; overload;
    procedure DeleteMessages; overload;
    procedure DeleteMessages(Index, Count: Integer); overload;

    function Assemble: string; override;
    procedure Disassemble; override;

    property Message[Index: Integer]: TEDIMessage read GetMessage
      write SetMessage; default;
    property Messages: TEDIDataObjectList read FEDIDataObjects;
  published
    property SegmentUNG: TEDIFunctionalGroupSegment read FUNGSegment write SetUNGSegment;
    property SegmentUNE: TEDIFunctionalGroupSegment read FUNESegment write SetUNESegment;
    property MessageCount: Integer read GetCount;
  end;

  TEDIFunctionalGroupArray = array of TEDIFunctionalGroup;

  //  EDI Interchange Control
  TEDIInterchangeControl = class(TEDIDataObjectGroup)
  private
    FUNASegment: TEDIInterchangeControlSegment;
    FUNBSegment: TEDIInterchangeControlSegment;
    FUNZSegment: TEDIInterchangeControlSegment;
    procedure SetUNBSegment(const UNBSegment: TEDIInterchangeControlSegment);
    procedure SetUNZSegment(const UNZSegment: TEDIInterchangeControlSegment);
  protected
    FCreateObjectType: TEDIDataObjectType;
    procedure InternalCreateHeaderTrailerSegments; virtual;
    function InternalCreateFunctionalGroup: TEDIFunctionalGroup; virtual;
    function InternalCreateMessage: TEDIMessage; virtual;
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

    function AddFunctionalGroups(Count: Integer): Integer;
    function AppendFunctionalGroups(FunctionalGroupArray: TEDIFunctionalGroupArray): Integer;
    function InsertFunctionalGroups(InsertIndex, Count: Integer): Integer; overload;
    function InsertFunctionalGroups(InsertIndex: Integer;
      FunctionalGroupArray: TEDIFunctionalGroupArray): Integer; overload;

    function AddMessage: Integer;
    function AppendMessage(Message: TEDIMessage): Integer;
    function InsertMessage(InsertIndex: Integer): Integer; overload;
    function InsertMessage(InsertIndex: Integer; Message: TEDIMessage): Integer; overload;

    function AddMessages(Count: Integer): Integer;
    function AppendMessages(MessageArray: TEDIMessageArray): Integer;
    function InsertMessages(InsertIndex, Count: Integer): Integer; overload;
    function InsertMessages(InsertIndex: Integer; MessageArray: TEDIMessageArray): Integer; overload;

    function Assemble: string; override;
    procedure Disassemble; override;
  published
    property SegmentUNA: TEDIInterchangeControlSegment read FUNASegment;
    property SegmentUNB: TEDIInterchangeControlSegment read FUNBSegment write SetUNBSegment;
    property SegmentUNZ: TEDIInterchangeControlSegment read FUNZSegment write SetUNZSegment;
  end;

  TEDIInterchangeControlArray = array of TEDIInterchangeControl;

  //  EDI File
  TEDIFileOptions = set of (foVariableDelimiterDetection, foRemoveCrLf, foRemoveCr, foRemoveLf,
    foIgnoreGarbageAtEndOfFile);

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

{$IFNDEF EDI_WEAK_PACKAGE_UNITS}
{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclEDI_UNEDIFACT.pas $';
    Revision: '$Revision: 2255 $';
    Date: '$Date: 2007-12-01 12:59:43 +0100 (sam., 01 déc. 2007) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}
{$ENDIF ~EDI_WEAK_PACKAGE_UNITS}

implementation

uses
  JclResources, JclStrings;

{$IFDEF CLR}
function AsEDIDataObjectArray(const ElementArray: System.&Array): TEDIDataObjectArray;
var
  I: Integer;
begin
  if ElementArray <> nil then
  begin
    SetLength(Result, ElementArray.Length);
    for I := 0 to High(Result) do
      Result[I] := TEDIDataObject(ElementArray[I]);
  end
  else
    Result := nil;
end;
{$ELSE}
type
  AsEDIDataObjectArray = TEDIDataObjectArray;
{$ENDIF CLR}

//=== { TEDIElement } ========================================================

constructor TEDIElement.Create(Parent: TEDIDataObject);
begin
  if Assigned(Parent) and ((Parent is TEDISegment) or (Parent is TEDICompositeElement)) then
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
  EDISegment: TEDISegment;
  EDICompositeElement: TEDICompositeElement;
begin
  Result := -1;
  if Assigned(Parent) and (Parent is TEDISegment) then
  begin
    EDISegment := TEDISegment(Parent);
    for I := 0 to EDISegment.EDIDataObjectCount - 1 do
      if EDISegment.EDIDataObjects[I] = Self then
      begin
        Result := I;
        Break;
      end;
  end
  else
  if Assigned(Parent) and (Parent is TEDICompositeElement) then
  begin
    EDICompositeElement := TEDICompositeElement(Parent);
    for I := 0 to EDICompositeElement.EDIDataObjectCount - 1 do
      if EDICompositeElement.EDIDataObjects[I] = Self then
      begin
        Result := I;
        Break;
      end;
  end;
end;

//=== { TEDISegment } ========================================================

constructor TEDISegment.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDIMessage) then
    inherited Create(Parent, ElementCount)
  else
    inherited Create(nil, ElementCount);
  FSegmentID := '';
  FEDIDOT := ediSegment;
  FCreateObjectType := ediElement;
  //FSegmentIdData := T???.Create(Self);
end;

destructor TEDISegment.Destroy;
begin
  //FSegmentIdData.Free;
  inherited Destroy;
end;

function TEDISegment.AddElements(Count: Integer): Integer;
begin
  FCreateObjectType := ediElement;
  Result := AddEDIDataObjects(Count);
end;

function TEDISegment.AddElement: Integer;
begin
  FCreateObjectType := ediElement;
  Result := AddEDIDataObject;
end;

function TEDISegment.AppendElement(Element: TEDIElement): Integer;
begin
  Result := AppendEDIDataObject(Element);
end;

function TEDISegment.AppendElements(ElementArray: TEDIElementArray): Integer;
begin
  Result := AppendEDIDataObjects(AsEDIDataObjectArray(ElementArray));
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

  FData := FSegmentID;
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
  ElementData: string;
begin
  // Data Input Scenarios
  // 4.)  SegID+data+data'
  // Composite Element Data Input Secnarios
  // 9.)  SegID+data+data:data'
  FSegmentID := '';
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
  FSegmentID := Copy(FData, 1, SearchResult - 1);
  StartPos := SearchResult + 1;
  SearchResult := StrSearch(FDelimiters.ED, FData, StartPos);
  while SearchResult <> 0 do
  begin
    ElementData := Copy(FData, ((StartPos + FDelimiters.EDLen) - 1), (SearchResult - StartPos));
    if StrSearch(FDelimiters.SS, ElementData, 1) <= 0 then
      I := AddElement
    else
      I := AddCompositeElement;
    if (SearchResult - StartPos) > 0 then // data exists
    begin
      FEDIDataObjects[I].Data := ElementData;
      FEDIDataObjects[I].Disassemble;
    end;
    StartPos := SearchResult + 1;
    SearchResult := StrSearch(FDelimiters.ED, FData, StartPos);
  end;
  // Get last element before next segment
  SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
  if SearchResult <> 0 then
    if (SearchResult - StartPos) > 0 then // data exists
    begin
      ElementData := Copy(FData, ((StartPos + FDelimiters.EDLen) - 1),
        (SearchResult - StartPos));
      if StrSearch(FDelimiters.SS, ElementData, 1) <= 0 then
        I := AddElement
      else
        I := AddCompositeElement;
      FEDIDataObjects[I].Data := ElementData;
      FEDIDataObjects[I].Disassemble;
    end;
  FData := '';

  FState := ediDisassembled;
end;

function TEDISegment.InsertElement(InsertIndex: Integer): Integer;
begin
  FCreateObjectType := ediElement;
  Result := InsertEDIDataObject(InsertIndex);
end;

function TEDISegment.InsertElement(InsertIndex: Integer; Element: TEDIElement): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, Element);
end;

function TEDISegment.InsertElements(InsertIndex: Integer; ElementArray: TEDIElementArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, AsEDIDataObjectArray(ElementArray));
end;

function TEDISegment.InsertElements(InsertIndex, Count: Integer): Integer;
begin
  FCreateObjectType := ediElement;
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

function TEDISegment.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    // Get the delimiters from the Message
    if Assigned(Parent) and (Parent is TEDIMessage) then
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
end;

function TEDISegment.InternalCreateElement: TEDIElement;
begin
  Result := TEDIElement.Create(Self);
end;

function TEDISegment.InternalCreateEDIDataObject: TEDIDataObject;
begin
  case FCreateObjectType of
    ediElement:
      Result := InternalCreateElement;
    ediCompositeElement:
      Result := InternalCreateCompositeElement;
  else
    Result := nil;
  end;
end;

function TEDISegment.InternalCreateCompositeElement: TEDICompositeElement;
begin
  Result := TEDICompositeElement.Create(Self);
end;

function TEDISegment.AddCompositeElement: Integer;
begin
  FCreateObjectType := ediCompositeElement;
  Result := AddEDIDataObject;
end;

function TEDISegment.AddCompositeElements(Count: Integer): Integer;
begin
  FCreateObjectType := ediCompositeElement;
  Result := AddEDIDataObjects(Count);
end;

function TEDISegment.AppendCompositeElement(CompositeElement: TEDICompositeElement): Integer;
begin
  Result := AppendEDIDataObject(CompositeElement);
end;

function TEDISegment.AppendCompositeElements(
  CompositeElementArray: TEDICompositeElementArray): Integer;
begin
  Result := AppendEDIDataObjects(AsEDIDataObjectArray(CompositeElementArray));
end;

function TEDISegment.InsertCompositeElement(InsertIndex: Integer): Integer;
begin
  FCreateObjectType := ediCompositeElement;
  Result := InsertEDIDataObject(InsertIndex);
end;

function TEDISegment.InsertCompositeElement(InsertIndex: Integer;
  CompositeElement: TEDICompositeElement): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, CompositeElement);
end;

function TEDISegment.InsertCompositeElements(InsertIndex, Count: Integer): Integer;
begin
  FCreateObjectType := ediCompositeElement;
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

function TEDISegment.InsertCompositeElements(InsertIndex: Integer;
  CompositeElementArray: TEDICompositeElementArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, AsEDIDataObjectArray(CompositeElementArray));
end;

//=== { TEDIMessageSegment } =================================================

constructor TEDIMessageSegment.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  inherited Create(Parent, ElementCount);
  if Assigned(Parent) and (Parent is TEDIMessage) then
    FParent := Parent;
end;

function TEDIMessageSegment.InternalAssignDelimiters: TEDIDelimiters;
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
  begin
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
end;

//=== { TEDIInterchangeControlSegment } ======================================

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

//=== { TEDIMessage } ========================================================

constructor TEDIMessage.Create(Parent: TEDIDataObject; SegmentCount: Integer);
begin
  if Assigned(Parent) and
    ((Parent is TEDIFunctionalGroup) or (Parent is TEDIInterchangeControl)) then
    inherited Create(Parent, SegmentCount)
  else
    inherited Create(nil, SegmentCount);
  FEDIDOT := ediMessage;
  InternalCreateHeaderTrailerSegments;
end;

destructor TEDIMessage.Destroy;
begin
  FUNTSegment.Free;
  FUNHSegment.Free;
  inherited Destroy;
end;

function TEDIMessage.AddSegment: Integer;
begin
  Result := AddEDIDataObject;
end;

function TEDIMessage.AddSegments(Count: Integer): Integer;
begin
  Result := AddEDIDataObjects(Count);
end;

function TEDIMessage.AppendSegment(Segment: TEDISegment): Integer;
begin
  Result := AppendEDIDataObject(Segment);
end;

function TEDIMessage.AppendSegments(SegmentArray: TEDISegmentArray): Integer;
begin
  Result := AppendEDIDataObjects(AsEDIDataObjectArray(SegmentArray));
end;

function TEDIMessage.Assemble: string;
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
      raise EJclEDIError.CreateID(31);
  end;

  FData := FUNHSegment.Assemble;
  FUNHSegment.Data := '';

  if GetCount > 0 then
    for I := 0 to GetCount - 1 do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;

  DeleteSegments;

  FData := FData + FUNTSegment.Assemble;
  FUNTSegment.Data := '';

  FLength := Length(FData);
  Result := FData;

  FState := ediAssembled;
end;

procedure TEDIMessage.DeleteSegment(Index: Integer);
begin
  DeleteEDIDataObject(Index);
end;

procedure TEDIMessage.DeleteSegment(Segment: TEDISegment);
begin
  DeleteEDIDataObject(Segment);
end;

procedure TEDIMessage.DeleteSegments;
begin
  DeleteEDIDataObjects;
end;

procedure TEDIMessage.DeleteSegments(Index, Count: Integer);
begin
  DeleteEDIDataObjects(Index, Count);
end;

procedure TEDIMessage.Disassemble;
var
  I, StartPos, SearchResult: Integer;
  S, S2: string;
begin
  FUNHSegment.Data := '';
  FUNHSegment.DeleteElements;
  FUNTSegment.Data := '';
  FUNTSegment.DeleteElements;
  DeleteSegments;
  // Check delimiter assignment
  if not Assigned(FDelimiters) then
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(30);
  end;
  // Find the first segment
  StartPos := 1;
  SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
  while SearchResult <> 0 do
  begin
    S := Copy(FData, StartPos, Length(UNHSegmentId));
    S2 := Copy(FData, StartPos, Length(UNTSegmentId));
    if (S <> UNHSegmentId) and (S2 <> UNTSegmentId) then
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
    if S = UNHSegmentId then
    begin
      if (SearchResult - StartPos) > 0 then // data exists
      begin
        FUNHSegment.Data := Copy(FData, StartPos,
          ((SearchResult - StartPos) + FDelimiters.SDLen));
        FUNHSegment.Disassemble;
      end;
    end
    else
    if S2 = UNTSegmentId then
    begin
      if (SearchResult - StartPos) > 0 then // data exists
      begin
        FUNTSegment.Data := Copy(FData, StartPos,
          ((SearchResult - StartPos) + FDelimiters.SDLen));
        FUNTSegment.Disassemble;
      end;
    end;
    StartPos := SearchResult + FDelimiters.SDLen;
    SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
  end;
  FData := '';
  FState := ediDisassembled;
end;

function TEDIMessage.GetSegment(Index: Integer): TEDISegment;
begin
  Result := TEDISegment(GetEDIDataObject(Index));
end;

function TEDIMessage.InsertSegment(InsertIndex: Integer): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex);
end;

function TEDIMessage.InsertSegment(InsertIndex: Integer; Segment: TEDISegment): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, Segment);
end;

function TEDIMessage.InsertSegments(InsertIndex, Count: Integer): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

function TEDIMessage.InsertSegments(InsertIndex: Integer;
  SegmentArray: TEDISegmentArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, AsEDIDataObjectArray(SegmentArray));
end;

function TEDIMessage.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  if FDelimiters = nil then // Attempt to assign the delimiters
    if Assigned(Parent) and
      ((Parent is TEDIFunctionalGroup) or (Parent is TEDIInterchangeControl)) then
      if Assigned(Parent.Delimiters) then
        Result := Parent.Delimiters
      else
      if Assigned(Parent.Parent) and (Parent.Parent is TEDIInterchangeControl) then
        Result := Parent.Parent.Delimiters;
end;

function TEDIMessage.InternalCreateSegment: TEDISegment;
begin
  Result := TEDISegment.Create(Self);
end;

procedure TEDIMessage.InternalCreateHeaderTrailerSegments;
begin
  FUNHSegment := TEDIMessageSegment.Create(Self);
  FUNTSegment := TEDIMessageSegment.Create(Self);
end;

procedure TEDIMessage.SetSegment(Index: Integer; Segment: TEDISegment);
begin
  SetEDIDataObject(Index, Segment);
end;

procedure TEDIMessage.SetUNTSegment(const UNTSegment: TEDIMessageSegment);
begin
  FreeAndNil(FUNTSegment);
  FUNTSegment := UNTSegment;
  if Assigned(FUNTSegment) then
    FUNTSegment.Parent := Self;
end;

procedure TEDIMessage.SetUNHSegment(const UNHSegment: TEDIMessageSegment);
begin
  FreeAndNil(FUNHSegment);
  FUNHSegment := UNHSegment;
  if Assigned(FUNHSegment) then
    FUNHSegment.Parent := Self;
end;

function TEDIMessage.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateSegment;
end;

//=== { TEDIFunctionalGroup } ================================================

constructor TEDIFunctionalGroup.Create(Parent: TEDIDataObject; MessageCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDIInterchangeControl) then
    inherited Create(Parent, MessageCount)
  else
    inherited Create(nil, MessageCount);
  FEDIDOT := ediFunctionalGroup;
  InternalCreateHeaderTrailerSegments;
end;

destructor TEDIFunctionalGroup.Destroy;
begin
  FUNGSegment.Free;
  FUNESegment.Free;
  inherited Destroy;
end;

function TEDIFunctionalGroup.AddMessage: Integer;
begin
  Result := AddEDIDataObject;
end;

function TEDIFunctionalGroup.AddMessages(Count: Integer): Integer;
begin
  Result := AddEDIDataObjects(Count);
end;

function TEDIFunctionalGroup.AppendMessage(Message: TEDIMessage): Integer;
begin
  Result := AppendEDIDataObject(Message);
end;

function TEDIFunctionalGroup.AppendMessages(
  MessageArray: TEDIMessageArray): Integer;
begin
  Result := AppendEDIDataObjects(AsEDIDataObjectArray(MessageArray));
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
  FData := FUNGSegment.Assemble;
  FUNGSegment.Data := '';

  if GetCount > 0 then
    for I := 0 to GetCount - 1 do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;

  DeleteMessages;

  FData := FData + FUNESegment.Assemble;
  FUNESegment.Data := '';

  FLength := Length(FData);
  Result := FData;

  FState := ediAssembled;
end;

procedure TEDIFunctionalGroup.DeleteMessage(Index: Integer);
begin
  DeleteEDIDataObject(Index);
end;

procedure TEDIFunctionalGroup.DeleteMessage(Message: TEDIMessage);
begin
  DeleteEDIDataObject(Message);
end;

procedure TEDIFunctionalGroup.DeleteMessages;
begin
  DeleteEDIDataObjects;
end;

procedure TEDIFunctionalGroup.DeleteMessages(Index, Count: Integer);
begin
  DeleteEDIDataObjects(Index, Count);
end;

procedure TEDIFunctionalGroup.Disassemble;
var
  I, StartPos, SearchResult: Integer;
begin
  FUNGSegment.Data := '';
  FUNGSegment.DeleteElements;
  FUNESegment.Data := '';
  FUNESegment.DeleteElements;
  DeleteMessages;
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
  if UNGSegmentId + FDelimiters.ED = Copy(FData, 1, Length(UNGSegmentId + FDelimiters.ED)) then
  begin
    // Search for Functional Group Header Segment Terminator
    SearchResult := StrSearch(FDelimiters.SD, FData, 1);
    if (SearchResult - StartPos) > 0 then // data exists
    begin
      FUNGSegment.Data := Copy(FData, 1, (SearchResult + FDelimiters.SDLen) - 1);
      FUNGSegment.Disassemble;
    end
    else
      raise EJclEDIError.CreateID(21);
  end
  else
    raise EJclEDIError.CreateID(22);
  // Search for Message Header
  SearchResult := StrSearch(FDelimiters.SD + UNHSegmentId + FDelimiters.ED, FData, StartPos);
  if SearchResult <= 0 then
    raise EJclEDIError.CreateID(32);
  // Set next start position
  StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
  // Continue
  while SearchResult <> 0 do
  begin
    // Search for Message Trailer
    SearchResult := StrSearch(FDelimiters.SD + UNTSegmentId + FDelimiters.ED, FData, StartPos);
    if SearchResult <> 0 then
    begin
      // Set the next start position
      SearchResult := SearchResult + FDelimiters.SDLen; // Move past the delimiter
      // Search for the end of Message Trailer
      SearchResult := StrSearch(FDelimiters.SD, FData, SearchResult);
      if SearchResult <> 0 then
      begin
        I := AddMessage;
        FEDIDataObjects[I].Data :=
          Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
        FEDIDataObjects[I].Disassemble;
      end
      else
        raise EJclEDIError.CreateID(33);
    end
    else
      raise EJclEDIError.CreateID(34);
    // Set the next start position
    StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
    //
    // Verify the next record is a Message Header
    if (UNHSegmentId + FDelimiters.ED) <>
      Copy(FData, StartPos, (Length(UNHSegmentId) + FDelimiters.EDLen)) then
      Break;
  end;
  // Set the next start position
  StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
  // Find Functional Group Trailer Segment
  if (UNESegmentId + FDelimiters.ED) =
    Copy(FData, StartPos, Length(UNESegmentId + FDelimiters.ED)) then
  begin
    // Find Functional Group Trailer Segment Terminator
    SearchResult := StrSearch(FDelimiters.SD, FData, StartPos + FDelimiters.SDLen);
    if (SearchResult - StartPos) > 0 then // data exists
    begin
      FUNESegment.Data := Copy(FData, StartPos, (SearchResult + FDelimiters.SDLen));
      FUNESegment.Disassemble;
    end
    else
      raise EJclEDIError.CreateID(23);
  end
  else
    raise EJclEDIError.CreateID(24);
  FData := '';
  FState := ediDisassembled;
end;

function TEDIFunctionalGroup.GetMessage(Index: Integer): TEDIMessage;
begin
  Result := TEDIMessage(GetEDIDataObject(Index));
end;

function TEDIFunctionalGroup.InsertMessage(InsertIndex: Integer): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex);
end;

function TEDIFunctionalGroup.InsertMessage(InsertIndex: Integer;
  Message: TEDIMessage): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, Message);
end;

function TEDIFunctionalGroup.InsertMessages(InsertIndex: Integer;
  MessageArray: TEDIMessageArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, AsEDIDataObjectArray(MessageArray));
end;

function TEDIFunctionalGroup.InsertMessages(InsertIndex, Count: Integer): Integer;
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

function TEDIFunctionalGroup.InternalCreateMessage: TEDIMessage;
begin
  Result := TEDIMessage.Create(Self);
end;

procedure TEDIFunctionalGroup.InternalCreateHeaderTrailerSegments;
begin
  FUNGSegment := TEDIFunctionalGroupSegment.Create(Self);
  FUNESegment := TEDIFunctionalGroupSegment.Create(Self);
end;

procedure TEDIFunctionalGroup.SetMessage(Index: Integer; Message: TEDIMessage);
begin
  SetEDIDataObject(Index, Message);
end;

procedure TEDIFunctionalGroup.SetUNESegment(const UNESegment: TEDIFunctionalGroupSegment);
begin
  FreeAndNil(FUNESegment);
  FUNESegment := UNESegment;
  if Assigned(FUNESegment) then
    FUNESegment.Parent := Self;
end;

procedure TEDIFunctionalGroup.SetUNGSegment(const UNGSegment: TEDIFunctionalGroupSegment);
begin
  FreeAndNil(FUNGSegment);
  FUNGSegment := UNGSegment;
  if Assigned(FUNGSegment) then
    FUNGSegment.Parent := Self;
end;

function TEDIFunctionalGroup.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateMessage;
end;

//=== { TEDIInterchangeControl } =============================================

constructor TEDIInterchangeControl.Create(Parent: TEDIDataObject; FunctionalGroupCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDIFile) then
    inherited Create(Parent, FunctionalGroupCount)
  else
    inherited Create(nil, FunctionalGroupCount);
  FEDIDOT := ediInterchangeControl;
  InternalCreateHeaderTrailerSegments;
  FCreateObjectType := ediFunctionalGroup;
end;

destructor TEDIInterchangeControl.Destroy;
begin
  FUNASegment.Free;
  FUNBSegment.Free;
  FUNZSegment.Free;
  FreeAndNil(FDelimiters);
  inherited Destroy;
end;

function TEDIInterchangeControl.AddFunctionalGroup: Integer;
begin
  FCreateObjectType := ediFunctionalGroup;
  Result := AddEDIDataObject;
end;

function TEDIInterchangeControl.AddFunctionalGroups(Count: Integer): Integer;
begin
  FCreateObjectType := ediFunctionalGroup;
  Result := AddEDIDataObjects(Count);
end;

function TEDIInterchangeControl.AppendFunctionalGroup(
  FunctionalGroup: TEDIFunctionalGroup): Integer;
begin
  FCreateObjectType := ediFunctionalGroup;
  Result := AppendEDIDataObject(FunctionalGroup);
end;

function TEDIInterchangeControl.AppendFunctionalGroups(
  FunctionalGroupArray: TEDIFunctionalGroupArray): Integer;
begin
  Result := AppendEDIDataObjects(AsEDIDataObjectArray(FunctionalGroupArray));
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

  FData := FUNBSegment.Assemble;
  FUNBSegment.Data := '';

  if GetCount > 0 then
    for I := 0 to GetCount - 1 do
      if Assigned(FEDIDataObjects[I]) then
        FData := FData + FEDIDataObjects[I].Assemble;

  DeleteEDIDataObjects;

  FData := FData + FUNZSegment.Assemble;
  FUNZSegment.Data := '';

  FLength := Length(FData);
  Result := FData;

  FState := ediAssembled;
end;

procedure TEDIInterchangeControl.Disassemble;
var
  I, StartPos, SearchResult: Integer;
begin
  FUNBSegment.Data := '';
  FUNBSegment.DeleteElements;
  FUNZSegment.Data := '';
  FUNZSegment.DeleteElements;
  DeleteEDIDataObjects;

  if not Assigned(FDelimiters) then
    raise EJclEDIError.CreateID(12);

  StartPos := 1;
  // Search for Interchange Control Header    
  if UNBSegmentId + FDelimiters.ED = Copy(FData, 1, Length(UNBSegmentId + FDelimiters.ED)) then
  begin
    SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
    if (SearchResult - StartPos) > 0 then // data exists
    begin
      FUNBSegment.Data := Copy(FData, 1, (SearchResult + FDelimiters.SDLen) - 1);
      FUNBSegment.Disassemble;
    end
    else
      raise EJclEDIError.CreateID(14);
  end
  else
    raise EJclEDIError.CreateID(15);
  // Search for Functional Group Header
  SearchResult := StrSearch(FDelimiters.SD + UNGSegmentId + FDelimiters.ED, FData, StartPos);
  if SearchResult > 0 then
  begin
    // Set next start positon
    StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
    // Continue
    while ((StartPos + Length(UNGSegmentId)) < Length(FData)) and (SearchResult > 0) do
    begin
      // Search for Functional Group Trailer
      SearchResult := StrSearch(FDelimiters.SD + UNESegmentId + FDelimiters.ED, FData, StartPos);
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
      if (UNGSegmentId + FDelimiters.ED) <>
        Copy(FData, StartPos, (Length(UNGSegmentId) + FDelimiters.EDLen)) then
        Break;
    end;
  end
  else
  begin
    // Search for Message Header
    SearchResult := StrSearch(FDelimiters.SD + UNHSegmentId + FDelimiters.ED, FData, StartPos);
    if SearchResult <= 0 then
      raise EJclEDIError.CreateID(32);
    // Set next start position
    StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
    // Continue
    while SearchResult <> 0 do
    begin
      // Search for Message Trailer
      SearchResult := StrSearch(FDelimiters.SD + UNTSegmentId + FDelimiters.ED, FData, StartPos);
      if SearchResult <> 0 then
      begin
        // Set the next start position
        SearchResult := SearchResult + FDelimiters.SDLen; // Move past the delimiter
        // Search for the end of Message Trailer
        SearchResult := StrSearch(FDelimiters.SD, FData, SearchResult);
        if SearchResult <> 0 then
        begin
          I := AddMessage;
          FEDIDataObjects[I].Data :=
            Copy(FData, StartPos, ((SearchResult - StartPos) + FDelimiters.SDLen));
          FEDIDataObjects[I].Disassemble;
        end
        else
          raise EJclEDIError.CreateID(33);
      end
      else
        raise EJclEDIError.CreateID(34);
      // Set the next start position
      StartPos := SearchResult + FDelimiters.SDLen; // Move past the delimiter
      // Verify the next record is a Message Header
      if (UNHSegmentId + FDelimiters.ED) <>
        Copy(FData, StartPos, (Length(UNHSegmentId) + FDelimiters.EDLen)) then
        Break;
    end;
  end;
  // Verify the next record is a Interchange Control Trailer
  if (UNZSegmentId + FDelimiters.ED) =
    Copy(FData, StartPos, Length(UNZSegmentId + FDelimiters.ED)) then
  begin
    // Search for the end of Interchange Control Trailer Segment Terminator
    SearchResult := StrSearch(FDelimiters.SD, FData, StartPos);
    if (SearchResult - StartPos) > 0 then // data exists
    begin
      FUNZSegment.Data := Copy(FData, StartPos, (SearchResult + FDelimiters.SDLen));
      FUNZSegment.Disassemble;
    end
    else
      raise EJclEDIError.CreateID(16);
  end
  else
    raise EJclEDIError.CreateID(17);
  FData := '';

  FState := ediDisassembled;
end;

function TEDIInterchangeControl.InsertFunctionalGroup(InsertIndex: Integer;
  FunctionalGroup: TEDIFunctionalGroup): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, FunctionalGroup);
end;

function TEDIInterchangeControl.InsertFunctionalGroup(InsertIndex: Integer): Integer;
begin
  FCreateObjectType := ediFunctionalGroup;
  Result := InsertEDIDataObject(InsertIndex);
end;

function TEDIInterchangeControl.InsertFunctionalGroups(InsertIndex, Count: Integer): Integer;
begin
  FCreateObjectType := ediFunctionalGroup;
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

function TEDIInterchangeControl.InsertFunctionalGroups(InsertIndex: Integer;
  FunctionalGroupArray: TEDIFunctionalGroupArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, AsEDIDataObjectArray(FunctionalGroupArray));
end;

function TEDIInterchangeControl.InternalCreateFunctionalGroup: TEDIFunctionalGroup;
begin
  Result := TEDIFunctionalGroup.Create(Self);
end;

procedure TEDIInterchangeControl.InternalCreateHeaderTrailerSegments;
begin
  FUNASegment := TEDIInterchangeControlSegment.Create(Self);
  FUNBSegment := TEDIInterchangeControlSegment.Create(Self);
  FUNZSegment := TEDIInterchangeControlSegment.Create(Self);
end;

procedure TEDIInterchangeControl.SetUNZSegment(const UNZSegment: TEDIInterchangeControlSegment);
begin
  FreeAndNil(FUNZSegment);
  FUNZSegment := UNZSegment;
  if Assigned(FUNZSegment) then
    FUNZSegment.Parent := Self;
end;

procedure TEDIInterchangeControl.SetUNBSegment(const UNBSegment: TEDIInterchangeControlSegment);
begin
  FreeAndNil(FUNBSegment);
  FUNBSegment := UNBSegment;
  if Assigned(FUNBSegment) then
    FUNBSegment.Parent := Self;
end;

function TEDIInterchangeControl.InternalCreateEDIDataObject: TEDIDataObject;
begin
  case FCreateObjectType of
    ediFunctionalGroup:
      Result := InternalCreateFunctionalGroup;
    ediMessage:
      Result := InternalCreateMessage;
  else
    Result := nil;
  end;
end;

function TEDIInterchangeControl.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
end;

function TEDIInterchangeControl.InternalCreateMessage: TEDIMessage;
begin
  Result := TEDIMessage.Create(Self);
end;

function TEDIInterchangeControl.AddMessage: Integer;
begin
  FCreateObjectType := ediMessage;
  Result := AddEDIDataObject;
end;

function TEDIInterchangeControl.AddMessages(Count: Integer): Integer;
begin
  FCreateObjectType := ediMessage;
  Result := AddEDIDataObjects(Count);
end;

function TEDIInterchangeControl.AppendMessage(Message: TEDIMessage): Integer;
begin
  FCreateObjectType := ediMessage;
  Result := AppendEDIDataObject(Message);
end;

function TEDIInterchangeControl.AppendMessages(MessageArray: TEDIMessageArray): Integer;
begin
  Result := AppendEDIDataObjects(AsEDIDataObjectArray(MessageArray));
end;

function TEDIInterchangeControl.InsertMessage(InsertIndex: Integer; Message: TEDIMessage): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, Message);
end;

function TEDIInterchangeControl.InsertMessage(InsertIndex: Integer): Integer;
begin
  FCreateObjectType := ediMessage;
  Result := InsertEDIDataObject(InsertIndex);
end;

function TEDIInterchangeControl.InsertMessages(InsertIndex, Count: Integer): Integer;
begin
  FCreateObjectType := ediMessage;
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

function TEDIInterchangeControl.InsertMessages(InsertIndex: Integer;
  MessageArray: TEDIMessageArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, AsEDIDataObjectArray(MessageArray));
end;

//=== { TEDIFile } ===========================================================

constructor TEDIFile.Create(Parent: TEDIDataObject; InterchangeCount: Integer);
begin
  if Assigned(Parent) then
    inherited Create(Parent, InterchangeCount)
  else
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
begin
  Result := AppendEDIDataObjects(AsEDIDataObjectArray(InterchangeControlArray));
end;

function TEDIFile.Assemble: string;
var
  I: Integer;
  EDIInterchangeControl: TEDIInterchangeControl;
begin
  FData := '';
  FLength := 0;
  Result := '';

  if GetCount > 0 then
    for I := 0 to GetCount - 1 do
    begin
      if Assigned(FEDIDataObjects[I]) then
      begin
        EDIInterchangeControl := TEDIInterchangeControl(FEDIDataObjects[I]);
        if EDIInterchangeControl.SegmentUNA.EDIDataObjectCount > 0 then
        begin
          FData := FData + EDIInterchangeControl.SegmentUNA.Assemble;
          EDIInterchangeControl.SegmentUNA.Data := '';
        end;
        FData := FData + FEDIDataObjects[I].Assemble;
      end;
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
  UNASegmentData: string;
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
    {$ENDIF OPTIMIZED_INTERNAL_STRUCTURE}
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
  if UNASegmentId = Copy(FData, StartPos, Length(UNASegmentId)) then
  begin
    if foVariableDelimiterDetection in FEDIFileOptions then
      InternalDelimitersDetection(StartPos);
    SearchResult := StrSearch(FDelimiters.SD + UNBSegmentId + FDelimiters.ED, FData, StartPos);
    UNASegmentData := Copy(FData, StartPos, (SearchResult - StartPos) + FDelimiters.SDLen);
    StartPos := SearchResult + FDelimiters.SDLen;    
  end
  else
  if UNBSegmentId = Copy(FData, StartPos, Length(UNBSegmentId)) then
  begin
    if foVariableDelimiterDetection in FEDIFileOptions then
      InternalAlternateDelimitersDetection(StartPos);
  end
  else
    raise EJclEDIError.CreateID(15);

  // Continue
  while (StartPos + Length(UNBSegmentId)) < Length(FData) do
  begin
    // Search for Interchange Control Trailer
    SearchResult := StrSearch(FDelimiters.SD + UNZSegmentId + FDelimiters.ED, FData, StartPos);
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
        TEDIInterchangeControl(FEDIDataObjects[I]).SegmentUNA.Data := UNASegmentData;
        TEDIInterchangeControl(FEDIDataObjects[I]).SegmentUNA.Disassemble;
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
    //
    if UNASegmentId = Copy(FData, StartPos, Length(UNASegmentId)) then
    begin
      if foVariableDelimiterDetection in FEDIFileOptions then
        InternalDelimitersDetection(StartPos);
      SearchResult := StrSearch(FDelimiters.SD + UNBSegmentId + FDelimiters.ED, FData, StartPos);
      UNASegmentData := Copy(FData, StartPos, (SearchResult - StartPos) + FDelimiters.SDLen);
      StartPos := SearchResult + FDelimiters.SDLen;
    end
    else
    if UNBSegmentId = Copy(FData, StartPos, Length(UNBSegmentId)) then
    begin
      if foVariableDelimiterDetection in FEDIFileOptions then
        InternalAlternateDelimitersDetection(StartPos);
    end
    else
    if (StartPos + Length(UNBSegmentId)) < Length(FData) then
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
begin
  Result := InsertEDIDataObjects(InsertIndex, AsEDIDataObjectArray(InterchangeControlArray));
end;

procedure TEDIFile.InternalLoadFromFile;
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

procedure TEDIFile.LoadFromFile(const FileName: string);
begin
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

procedure TEDIFile.SaveToFile;
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

procedure TEDIFile.SetInterchangeControl(Index: Integer; Interchange: TEDIInterchangeControl);
begin
  SetEDIDataObject(Index, Interchange);
end;

procedure TEDIFile.InternalDelimitersDetection(StartPos: Integer);
begin
  FDelimiters.SS := Copy(FData, StartPos + Length(UNASegmentId), 1);        
  FDelimiters.ED := Copy(FData, StartPos + Length(UNASegmentId) + 1, 1);
  if Copy(FData, StartPos + Length(UNASegmentId) + 5, 2) = AnsiCrLf then
    FDelimiters.SD := Copy(FData, StartPos + Length(UNASegmentId) + 5, 2) 
  else
    FDelimiters.SD := Copy(FData, StartPos + Length(UNASegmentId) + 5, 1);
end;

procedure TEDIFile.InternalAlternateDelimitersDetection(StartPos: Integer);
var
  SearchResult, I: Integer;
  Delimiter: string;
begin
  SearchResult := 1;
  FDelimiters.ED := Copy(FData, StartPos + Length(UNBSegmentId), 1);
  SearchResult := StrSearch(UNGSegmentId + FDelimiters.ED, FData, SearchResult);
  if SearchResult <= 0 then
    SearchResult := StrSearch(UNHSegmentId + FDelimiters.ED, FData, 1);
  if Copy(FData, SearchResult - 2, 2) = AnsiCrLf then
    FDelimiters.SD := Copy(FData, SearchResult - 2, 2)
  else
    FDelimiters.SD := Copy(FData, SearchResult - 1, 1); 
  SearchResult := SearchResult - 2;
  for I := SearchResult downto 1 do              
  begin
    Delimiter := Copy(FData, I, 1);
    if not (AnsiChar(Delimiter[1]) in
      AnsiLetters + AnsiDecDigits + [AnsiChar(FDelimiters.ED[1]), AnsiChar(FDelimiters.SD[1])]) then
    begin
      FDelimiters.SS := Copy(FData, I, 1);
      Break;
    end;
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
  Result := TEDIDelimiters.Create('''', '+', ':');
end;

function TEDIFile.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateInterchangeControl;
end;

//=== { TEDICompositeElement } ===============================================

constructor TEDICompositeElement.Create(Parent: TEDIDataObject; ElementCount: Integer);
begin
  if Assigned(Parent) and (Parent is TEDISegment) then
    inherited Create(Parent, ElementCount)
  else
    inherited Create(nil, ElementCount);
  FEDIDOT := ediElement;
end;

destructor TEDICompositeElement.Destroy;
begin
  inherited Destroy;
end;

function TEDICompositeElement.AddElement: Integer;
begin
  Result := AddEDIDataObject;
end;

function TEDICompositeElement.AddElements(Count: Integer): Integer;
begin
  Result := AddEDIDataObjects(Count);
end;

function TEDICompositeElement.AppendElement(Element: TEDIElement): Integer;
begin
  Result := AppendEDIDataObject(Element);
end;

function TEDICompositeElement.AppendElements(ElementArray: TEDIElementArray): Integer;
begin
  Result := AppendEDIDataObjects(AsEDIDataObjectArray(ElementArray));
end;

function TEDICompositeElement.Assemble: string;
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
      raise EJclEDIError.CreateID(38);
  end;

  if GetCount > 0 then
    for I := 0 to GetCount - 1 do
      if Assigned(FEDIDataObjects[I]) then
      begin
        if FData <> '' then
          FData := FData + FDelimiters.SS + FEDIDataObjects[I].Assemble
        else
          FData := FData + FEDIDataObjects[I].Assemble;
      end
      else
      begin
        // If I is not equal to the last item then add the subelement seperator.
        if I <> GetCount - 1 then
          FData := FData + FDelimiters.SS;
      end;
  FLength := Length(FData);
  Result := FData; // Return assembled string

  DeleteElements;

  FState := ediAssembled;
end;

procedure TEDICompositeElement.DeleteElement(Element: TEDIElement);
begin
  DeleteEDIDataObject(Element);
end;

procedure TEDICompositeElement.DeleteElement(Index: Integer);
begin
  DeleteEDIDataObject(Index);
end;

procedure TEDICompositeElement.DeleteElements;
begin
  DeleteEDIDataObjects;
end;

procedure TEDICompositeElement.DeleteElements(Index, Count: Integer);
begin
  DeleteEDIDataObjects(Index, Count);
end;

procedure TEDICompositeElement.Disassemble;
var
  StartPos, SearchResult, I: Integer;
begin
  DeleteElements;
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
  begin
    FDelimiters := InternalAssignDelimiters;
    if not Assigned(FDelimiters) then
      raise EJclEDIError.CreateID(37);
  end;
  StartPos := 1;
  SearchResult := StrSearch(FDelimiters.SS, FData, StartPos);
  while SearchResult > 0 do
  begin
    I := AddElement;
    if (SearchResult - StartPos) > 0 then // data exists
    begin
      FEDIDataObjects[I].Data := Copy(FData, StartPos, (SearchResult - StartPos));
      FEDIDataObjects[I].Disassemble;
    end;
    StartPos := SearchResult + 1;
    SearchResult := StrSearch(FDelimiters.SS, FData, StartPos);
  end;
  if StartPos <= Length(FData) then
  begin
    I := AddElement;
    FEDIDataObjects[I].Data := Copy(FData, StartPos, (Length(FData) - StartPos) + 1);
    FEDIDataObjects[I].Disassemble;
  end;
end;

function TEDICompositeElement.GetElement(Index: Integer): TEDIElement;
begin
  Result := TEDIElement(GetEDIDataObject(Index));
end;

function TEDICompositeElement.InsertElement(InsertIndex: Integer): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex);
end;

function TEDICompositeElement.InsertElement(InsertIndex: Integer; Element: TEDIElement): Integer;
begin
  Result := InsertEDIDataObject(InsertIndex, Element);
end;

function TEDICompositeElement.InsertElements(InsertIndex: Integer;
  ElementArray: TEDIElementArray): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, AsEDIDataObjectArray(ElementArray));
end;

function TEDICompositeElement.InsertElements(InsertIndex, Count: Integer): Integer;
begin
  Result := InsertEDIDataObjects(InsertIndex, Count);
end;

function TEDICompositeElement.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  if not Assigned(FDelimiters) then // Attempt to assign the delimiters
    // Get the delimiters from the segment
    if Assigned(Parent) and (Parent is TEDISegment) then
      Result := Parent.Delimiters;
end;

function TEDICompositeElement.InternalCreateEDIDataObject: TEDIDataObject;
begin
  Result := InternalCreateElement;
end;

function TEDICompositeElement.InternalCreateElement: TEDIElement;
begin
  Result := TEDIElement.Create(Self);
end;

procedure TEDICompositeElement.SetElement(Index: Integer; Element: TEDIElement);
begin
  SetEDIDataObject(Index, Element);
end;

//=== { TEDIMessageLoop } ====================================================

// EDI Transaction Set Loop
constructor TEDIMessageLoop.Create(Parent: TEDIDataObject);
begin
  inherited Create(Parent);
  FCreateObjectType := ediLoop;
  FGroupIsParent := False;
  if Assigned(Parent) and (Parent is TEDIMessage) then
    FParentMessage := TEDIMessage(Parent)
  else
  if Assigned(Parent) and (Parent is TEDIMessageLoop) then
    FParentMessage := TEDIMessageLoop(Parent).ParentMessage
  else
    FParentMessage := nil;
  FEDIDOT := ediLoop;
  FEDIDataObjects.OwnsObjects := False;
end;

destructor TEDIMessageLoop.Destroy;
begin
  DeleteEDIDataObjects;
  inherited Destroy;
end;

function TEDIMessageLoop.InternalAssignDelimiters: TEDIDelimiters;
begin
  Result := nil;
  if FDelimiters = nil then // Attempt to assign the delimiters
    if Assigned(FParentMessage) then
      Result := FParentMessage.Delimiters;
end;

function TEDIMessageLoop.InternalCreateEDIDataObject: TEDIDataObject;
begin
  case FCreateObjectType of
    ediLoop:
    begin
      Result := TEDIMessageLoop.Create(Self);
      TEDIMessageLoop(Result).OwnerLoopId := OwnerLoopId;
      TEDIMessageLoop(Result).ParentLoopId := ParentLoopId;
      TEDIMessageLoop(Result).Parent := Self;
    end;
  else
    Result := nil;
  end;
end;

function TEDIMessageLoop.Assemble: string;
begin
  Result := '';
end;

procedure TEDIMessageLoop.Disassemble;
begin
  // Do Nothing
end;

function TEDIMessageLoop.AddLoop(OwnerLoopId, ParentLoopId: string): Integer;
var
  Loop: TEDIMessageLoop;
begin
  FCreateObjectType := ediLoop;
  Loop := TEDIMessageLoop(InternalCreateEDIDataObject);
  Loop.OwnerLoopId := OwnerLoopId;
  Loop.ParentLoopId := ParentLoopId;
  Loop.Parent := Self;
  Result := AppendEDIDataObject(Loop);
end;

procedure TEDIMessageLoop.AppendSegment(Segment: TEDISegment);
begin
  AppendEDIDataObject(Segment);
end;

procedure TEDIMessageLoop.DeleteEDIDataObjects;
var
  I: Integer;
begin
  for I := 0 to FEDIDataObjects.Count - 1 do
    if Assigned(FEDIDataObjects[I]) then
      try
        // Delete
        if FEDIDataObjects[I] is TEDIMessageLoop then
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

function TEDIMessageLoop.FindLoop(LoopId: string; var StartIndex: Integer): TEDIMessageLoop;
var
  I, J: Integer;
begin
  Result := nil;
  J := StartIndex;
  for I := StartIndex to GetCount {FEDIDataObjects.Count} - 1 do
  begin
    StartIndex := I;
    if FEDIDataObjects[I] is TEDIMessageLoop then
    begin
      Result := TEDIMessageLoop(GetEDIDataObject(I));
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

function TEDIMessageLoop.FindSegment(SegmentId: string; var StartIndex: Integer): TEDISegment;
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

function TEDIMessageLoop.FindSegment(SegmentId: string; var StartIndex: Integer;
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

{$IFNDEF EDI_WEAK_PACKAGE_UNITS}
{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}
{$ENDIF ~EDI_WEAK_PACKAGE_UNITS}

end.
