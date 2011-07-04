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
{ The Original Code is JclEDI_ANSIX12_Ext.pas.                                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Raymond Alexander.                                 }
{ Portions created by Raymond Alexander are Copyright (C) Raymond Alexander. All rights reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Raymond Alexander (rayspostbox3)                                                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ EDI ANSI X12 - Standard Exchange Format (*.sef) File Extensions                                  }
{                                                                                                  }
{ This unit is still in development                                                                }
{                                                                                                  }
{ Unit owner: Raymond Alexander                                                                    }
{ Date created: March 26, 2004                                                                     }
{ Additional Info:                                                                                 }
{   E-Mail at RaysDelphiBox3 att hotmail dott com                                                  }
{   For latest EDI specific demos see http://sourceforge.net/projects/edisdk                       }
{   See home page for latest news & events and online help.                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-11-30 20:28:51 +0100 (ven., 30 nov. 2007)                          $ }
{ Revision:      $Rev:: 2243                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclEDI_UNEDIFACT_Ext;

{$I jcl.inc}

{$IFDEF EDI_WEAK_PACKAGE_UNITS}
  {$IFDEF SUPPORTS_WEAKPACKAGEUNIT}
    {$WEAKPACKAGEUNIT ON}
  {$ENDIF SUPPORTS_WEAKPACKAGEUNIT}
{$ENDIF EDI_WEAK_PACKAGE_UNITS}

interface

uses
  SysUtils, Classes, Contnrs, JclResources,
  {$IFNDEF EDI_WEAK_PACKAGE_UNITS}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$ENDIF ~EDI_WEAK_PACKAGE_UNITS}
  JclEDI, JclEDI_UNEDIFACT, JclEDISEF;

type

//  EDI Transaction Set Document and related types and classes
  TEDIMessageDocumentOptions = set of (doLinkSpecToDataObject);

  TEDI_UNEDIFACT_Document = class(TEDIMessageLoop)
  private
    FEDISEFSet: TEDISEFSet;
  protected
    FErrorOccured: Boolean;
    FEDITSDOptions: TEDIMessageDocumentOptions;
    FEDILoopStack: TEDILoopStack;
    // References
    FEDIMessage: TEDIMessage;
    FEDIMessageSpec: TObjectList;
    function ValidateSegSpecIndex(DataSegmentId: string; SpecStartIndex: Integer): Integer;
    function AdvanceSegSpecIndex(DataIndex, SpecStartIndex, SpecEndIndex: Integer): Integer;
    procedure AddLoopToDoc(StackRecord: TEDILoopStackRecord;
      SegmentId, OwnerLoopId, ParentLoopId: string; var EDIObject: TEDIObject);
    procedure SetSpecificationPointers(DataSegment: TEDISegment; SpecSegment: TEDISEFSegment);
  protected
    procedure ValidateData(TSDocument: TEDI_UNEDIFACT_Document;
      LoopStack: TEDILoopStack;
      DataSegment: TEDISegment;
      SpecSegment: TEDISEFSegment;
      var DataIndex, SpecIndex: Integer;
      var ErrorOccured: Boolean); virtual;
  public
    constructor Create(Parent: TEDIDataObject; AEDIMessage: TEDIMessage;
      SEFSet: TEDISEFSet); reintroduce;
    destructor Destroy; override;
    //
    //  ToDo:  More procedures and functions to manage internal structures
    //
    procedure FormatDocument; virtual;
  published
    property EDITSDOptions: TEDIMessageDocumentOptions read FEDITSDOptions
      write FEDITSDOptions;
    property ErrorOccured: Boolean read FErrorOccured;
  end;

{$IFNDEF EDI_WEAK_PACKAGE_UNITS}
{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclEDI_UNEDIFACT_Ext.pas $';
    Revision: '$Revision: 2243 $';
    Date: '$Date: 2007-11-30 20:28:51 +0100 (ven., 30 nov. 2007) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}
{$ENDIF ~EDI_WEAK_PACKAGE_UNITS}

implementation

// { TEDI_UNEDIFACT_Document }
constructor TEDI_UNEDIFACT_Document.Create(Parent: TEDIDataObject;
  AEDIMessage: TEDIMessage; SEFSet: TEDISEFSet);
begin
  inherited Create(Parent);
  FEDILoopStack := TEDILoopStack.Create;
  FEDILoopStack.OnAddLoop := AddLoopToDoc;
  FEDIMessage := AEDIMessage;
  FEDISEFSet := SEFSet;  
  FEDIMessageSpec := SEFSet.GetSegmentObjectList;
  FEDITSDOptions := [];
end;

destructor TEDI_UNEDIFACT_Document.Destroy;
begin
  FreeAndNil(FEDILoopStack);
  FEDIMessage := nil;
  FEDIMessageSpec.Free;
  inherited Destroy;
end;

procedure TEDI_UNEDIFACT_Document.FormatDocument;
var
  I, J: Integer;
  LSR: TEDILoopStackRecord;
  DataSegment: TEDISegment;
  SpecSegment: TEDISEFSegment;
begin
  I := 0;
  J := 0;
  if doLinkSpecToDataObject in FEDITSDOptions then
  begin
    FEDISEFSet.BindTextSets(FEDISEFSet.SEFFile.TEXTSETS);
    FEDISEFSet.BindSegmentTextSets;
  end;
  // Initialize the stack
  FEDILoopStack.Flags := FEDILoopStack.Flags - [ediLoopRepeated];
  LSR := FEDILoopStack.ValidateLoopStack(FEDIMessage.Segment[I].SegmentID,
    NA_LoopId, NA_LoopId, 0, Self);
  //
  while (I <= FEDIMessage.SegmentCount - 1) and
    (J <= FEDIMessageSpec.Count - 1) do
  begin
    FEDILoopStack.Flags := FEDILoopStack.Flags - [ediLoopRepeated];
    DataSegment := FEDIMessage.Segment[I];
    // If loop has repeated then move the spec index back
    J := ValidateSegSpecIndex(DataSegment.SegmentID, J);
    // Check current segment against segment spec
    SpecSegment := TEDISEFSegment(FEDIMessageSpec[J]);
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
      TEDIMessageLoop(LSR.EDIObject).AppendSegment(DataSegment);
      //
      if doLinkSpecToDataObject in FEDITSDOptions then
      begin
        SpecSegment.BindTextSets(SpecSegment.SEFFile.TEXTSETS);
        SpecSegment.BindElementTextSets;
        SetSpecificationPointers(DataSegment, SpecSegment);
      end;
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
      J := AdvanceSegSpecIndex(I, J, FEDIMessageSpec.Count - 1); //Inc(J);
    end;
  end;
end;

procedure TEDI_UNEDIFACT_Document.ValidateData(TSDocument: TEDI_UNEDIFACT_Document;
  LoopStack: TEDILoopStack; DataSegment: TEDISegment; SpecSegment: TEDISEFSegment;
  var DataIndex, SpecIndex: Integer; var ErrorOccured: Boolean);
begin
  ErrorOccured := False;
end;

procedure TEDI_UNEDIFACT_Document.SetSpecificationPointers(DataSegment: TEDISegment;
  SpecSegment: TEDISEFSegment);
var
  I, J: Integer;
begin
  DataSegment.SpecPointer := SpecSegment;
  J := SpecSegment.Elements.Count - 1;
  for I := 0 to DataSegment.ElementCount - 1 do
  begin
    if I > J then
      raise EJclEDIError.CreateIDFmt(58, [IntToStr(I), DataSegment.SegmentId,
        IntToStr(DataSegment.GetIndexPositionFromParent)]);
    DataSegment.EDIDataObject[I].SpecPointer := SpecSegment.Elements[I];
    // ToDo: Assign SubElement Specs
  end;
end;

procedure TEDI_UNEDIFACT_Document.AddLoopToDoc(StackRecord: TEDILoopStackRecord;
  SegmentId, OwnerLoopId, ParentLoopId: string; var EDIObject: TEDIObject);
var
  I: Integer;
  Loop: TEDIMessageLoop;
begin
  Loop := TEDIMessageLoop(StackRecord.EDIObject);
  I := Loop.AddLoop(OwnerLoopId, ParentLoopId);
  EDIObject := TEDIMessageLoop(Loop[I]);
end;

function TEDI_UNEDIFACT_Document.ValidateSegSpecIndex(DataSegmentId: string;
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

function TEDI_UNEDIFACT_Document.AdvanceSegSpecIndex(DataIndex, SpecStartIndex,
  SpecEndIndex: Integer): Integer;
var
  DataSegment: TEDISegment;
  TestSegment: TEDISEFSegment;
  I: Integer;
begin
  Result := SpecEndIndex + 1;
  DataSegment := FEDIMessage.Segment[DataIndex];
  for I := SpecStartIndex + 1 to SpecEndIndex do
  begin
    TestSegment := TEDISEFSegment(FEDIMessageSpec[I]);
    // Find matching segment
    if ((DataSegment.SegmentID) = (TestSegment.SegmentID)) then
    begin
      Result := I;
      Break;
    end;
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
