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
{ The Original Code is JclEDITranslators.pas.                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is Raymond Alexander.                                 }
{ Portions created by Raymond Alexander are Copyright Raymond Alexander. All rights reserved.      }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Raymond Alexander, Robert Marquardt, Robert Rossmair                                           }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ EDI Translators Unit for classes that translate EDI objects from one format to another.          }
{                                                                                                  }
{ This unit is still in development                                                                }
{                                                                                                  }
{ Unit owner: Raymond Alexander                                                                    }
{ Last created: October 2, 2003                                                                    }
{ Additional Info:                                                                                 }
{   E-Mail at RaysDelphiBox3 att hotmail dott com                                                  }
{   For latest EDI specific demos see http://sourceforge.net/projects/edisdk                       }
{   See home page for latest news & events and online help.                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclEDITranslators;

{$I jcl.inc}

{$IFDEF EDI_WEAK_PACKAGE_UNITS}
  {$IFDEF SUPPORTS_WEAKPACKAGEUNIT}
    {$WEAKPACKAGEUNIT ON}
  {$ENDIF SUPPORTS_WEAKPACKAGEUNIT}
{$ENDIF EDI_WEAK_PACKAGE_UNITS}

interface

uses
  SysUtils,
  {$IFNDEF EDI_WEAK_PACKAGE_UNITS}
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$ENDIF ~EDI_WEAK_PACKAGE_UNITS}
  JclEDI, JclEDI_ANSIX12, JclEDISEF;

type
  TEDISpecToSEFTranslator = class(TEDIObject)
  public
    constructor Create;
    destructor Destroy; override;
    function TranslateToSEFElement(ElementSpec: TEDIElementSpec;
      Parent: TEDISEFFile): TEDISEFElement; overload;
    function TranslateToSEFElement(ElementSpec: TEDIElementSpec;
      Parent: TEDISEFSegment): TEDISEFElement; overload;
    procedure TranslateToSEFElementTEXTSETS(ElementSpec: TEDIElementSpec;
      SEFElement: TEDISEFElement);
    function TranslateToSEFSegment(SegmentSpec: TEDISegmentSpec;
      Parent: TEDISEFFile): TEDISEFSegment; overload;
    function TranslateToSEFSegment(SegmentSpec: TEDISegmentSpec;
      Parent: TEDISEFTable): TEDISEFSegment; overload;
    function TranslateToSEFSegment(SegmentSpec: TEDISegmentSpec;
      Parent: TEDISEFLoop): TEDISEFSegment; overload;
    procedure TranslateToSEFSegmentTEXTSETS(SegmentSpec: TEDISegmentSpec;
      SEFSegment: TEDISEFSegment);
    function TranslateToSEFSet(TransactionSetSpec: TEDITransactionSetSpec;
      Parent: TEDISEFFile): TEDISEFSet;
    procedure TranslateLoopToSEFSet(StackRecord: TEDILoopStackRecord;
      SegmentId, OwnerLoopId, ParentLoopId: string; var EDIObject: TEDIObject);
    function TranslateToSEFFile(ICSpec: TEDIInterchangeControlSpec): TEDISEFFile;
  end;

  TEDISEFToSpecTranslator = class(TEDIObject)
  public
    constructor Create;
    destructor Destroy; override;
  end;

{$IFNDEF EDI_WEAK_PACKAGE_UNITS}
{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclEDITranslators.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}
{$ENDIF ~EDI_WEAK_PACKAGE_UNITS}

implementation

uses
  JclStrings;

//=== { TEDISpecToSEFTranslator } ============================================

constructor TEDISpecToSEFTranslator.Create;
begin
  inherited Create;
end;

destructor TEDISpecToSEFTranslator.Destroy;
begin
  inherited Destroy;
end;

function TEDISpecToSEFTranslator.TranslateToSEFElement(ElementSpec: TEDIElementSpec;
  Parent: TEDISEFFile): TEDISEFElement;
begin
  Result := TEDISEFElement.Create(Parent);
  Result.Id := ElementSpec.Id;
  Result.ElementType := ElementSpec.ElementType;
  Result.MinimumLength := ElementSpec.MinimumLength;
  Result.MaximumLength := ElementSpec.MaximumLength;
end;

function TEDISpecToSEFTranslator.TranslateToSEFElement(ElementSpec: TEDIElementSpec;
  Parent: TEDISEFSegment): TEDISEFElement;
var
  ListItem: TEDISEFDataObjectListItem;
begin
  Result := TEDISEFElement.Create(Parent);
  Result.Id := ElementSpec.Id;
  ListItem := Parent.SEFFile.ELMS.FindItemByName(ElementSpec.Id);
  if ListItem <> nil then
    Result.Assign(TEDISEFElement(ListItem.EDISEFDataObject))
  else
  begin
    Result.ElementType := ElementSpec.ElementType;
    Result.MinimumLength := ElementSpec.MinimumLength;
    Result.MaximumLength := ElementSpec.MaximumLength;
    Result.RequirementDesignator := ElementSpec.RequirementDesignator;
  end;
end;

function TEDISpecToSEFTranslator.TranslateToSEFSegment(SegmentSpec: TEDISegmentSpec;
  Parent: TEDISEFFile): TEDISEFSegment;
var
  E: Integer;
  ElementSpec: TEDIElementSpec;
  SEFElement: TEDISEFElement;
begin
  Result := TEDISEFSegment.Create(Parent);
  Result.Id := SegmentSpec.Id;
  Result.RequirementDesignator := SegmentSpec.RequirementDesignator;
  Result.MaximumUse := SegmentSpec.MaximumUsage;
  for E := 0 to SegmentSpec.ElementCount - 1 do
  begin
    ElementSpec := TEDIElementSpec(SegmentSpec[E]);
    SEFElement := TranslateToSEFElement(ElementSpec, Result);
    Result.Elements.Add(SEFElement);
  end;
end;

function TEDISpecToSEFTranslator.TranslateToSEFSegment(SegmentSpec: TEDISegmentSpec;
  Parent: TEDISEFTable): TEDISEFSegment;
var
  ListItem: TEDISEFDataObjectListItem;
begin
  Result := TEDISEFSegment.Create(Parent);
  Result.Id := SegmentSpec.Id;
  ListItem := Parent.SEFFile.SEGS.FindItemByName(SegmentSpec.Id);
  if ListItem <> nil then
    Result.Assign(TEDISEFSegment(ListItem.EDISEFDataObject))
  else
  begin
    Result.RequirementDesignator := SegmentSpec.RequirementDesignator;
    Result.MaximumUse := SegmentSpec.MaximumUsage;
  end;
end;

function TEDISpecToSEFTranslator.TranslateToSEFSegment(SegmentSpec: TEDISegmentSpec;
  Parent: TEDISEFLoop): TEDISEFSegment;
var
  ListItem: TEDISEFDataObjectListItem;
begin
  Result := TEDISEFSegment.Create(Parent);
  Result.Id := SegmentSpec.Id;
  ListItem := Parent.SEFFile.SEGS.FindItemByName(SegmentSpec.Id);
  if ListItem <> nil then
    Result.Assign(TEDISEFSegment(ListItem.EDISEFDataObject))
  else
  begin
    Result.RequirementDesignator := SegmentSpec.RequirementDesignator;
    Result.MaximumUse := SegmentSpec.MaximumUsage;
  end;
end;

procedure TEDISpecToSEFTranslator.TranslateLoopToSEFSet(StackRecord: TEDILoopStackRecord;
  SegmentId, OwnerLoopId, ParentLoopId: string; var EDIObject: TEDIObject);
var
  SEFLoop: TEDISEFLoop;
begin
  if StackRecord.EDIObject is TEDISEFDataObjectGroup then
  begin
    SEFLoop := TEDISEFLoop.Create(TEDISEFDataObject(StackRecord.EDIObject));
    SEFLoop.Id := SegmentId;
    TEDISEFDataObjectGroup(StackRecord.EDIObject).EDISEFDataObjects.Add(SEFLoop);
    EDIObject := SEFLoop;
  end;
end;

function TEDISpecToSEFTranslator.TranslateToSEFSet(TransactionSetSpec: TEDITransactionSetSpec;
  Parent: TEDISEFFile): TEDISEFSet;
var
  S: Integer;
  SegmentSpec: TEDISegmentSpec;
  PrevSegmentSpec: TEDISegmentSpec;
  SEFSegment: TEDISEFSegment;
  SEFTable: TEDISEFTable;
  SEFLoop: TEDISEFLoop;
  LS: TEDILoopStack;
  LSR: TEDILoopStackRecord;
begin
  Result := TEDISEFSet.Create(Parent);
  Result.Id := TransactionSetSpec.Id;

  LS := TEDILoopStack.Create;
  try
    LS.OnAddLoop := TranslateLoopToSEFSet;
    //
    for S := 0 to TransactionSetSpec.SegmentCount - 1 do
    begin
      SegmentSpec := TEDISegmentSpec(TransactionSetSpec[S]);
      if S = 0 then
      begin
        // Initialize the stack
        SEFTable := TEDISEFTable.Create(Result);
        Result.EDISEFDataObjects.Add(SEFTable);
        LSR := LS.ValidateLoopStack(SegmentSpec.SegmentID, NA_LoopId, NA_LoopId, 0, SEFTable);
      end
      else
      begin
        // Check to see if the sections have changed
        PrevSegmentSpec := TEDISegmentSpec(TransactionSetSpec[S-1]);
        if SegmentSpec.Section <> PrevSegmentSpec.Section then
        begin
          // Create new table for new section
          SEFTable := TEDISEFTable.Create(Result);
          Result.EDISEFDataObjects.Add(SEFTable);
          // Re-initialize the stack
          LS.Pop(1);
          LS.UpdateStackObject(SEFTable);
          LSR := LS.ValidateLoopStack(SegmentSpec.SegmentID, SegmentSpec.OwnerLoopId,
            SegmentSpec.ParentLoopId, 0, LSR.EDIObject);
        end
        else
        begin
          LSR := LS.ValidateLoopStack(SegmentSpec.SegmentID, SegmentSpec.OwnerLoopId,
            SegmentSpec.ParentLoopId, 0, LSR.EDIObject);
        end;
      end;

  // Debug - Keep the following line here in case someone wants to debug what happens to the stack.
  //    ShowMessage('Current Spec Segment: [' + IntToStr(S) + '] ' + SegmentSpec.SegmentID + #13#10 +
  //                LS.Debug);

      // Add objects to proper owners
      if LSR.EDIObject is TEDISEFTable then
      begin
        SEFTable := TEDISEFTable(LSR.EDIObject);
        SEFSegment := TranslateToSEFSegment(SegmentSpec, SEFTable);
        SEFTable.EDISEFDataObjects.Add(SEFSegment);
        SEFSegment.ParentSet.AssignSegmentOrdinals;
        TranslateToSEFSegmentTEXTSETS(SegmentSpec, SEFSegment);
      end
      else
      if LSR.EDIObject is TEDISEFLoop then
      begin
        SEFLoop := TEDISEFLoop(LSR.EDIObject);
        SEFSegment := TranslateToSEFSegment(SegmentSpec, SEFLoop);
        SEFLoop.EDISEFDataObjects.Add(SEFSegment);
        SEFSegment.ParentSet.AssignSegmentOrdinals;
        TranslateToSEFSegmentTEXTSETS(SegmentSpec, SEFSegment);
      end;
    end;
  finally
    LS.Free;
  end;
end;

function TEDISpecToSEFTranslator.TranslateToSEFFile(ICSpec: TEDIInterchangeControlSpec): TEDISEFFile;
var
  F, T, S, E: Integer;
  ElementList: TEDIObjectList;
  SegmentSpec: TEDISegmentSpec;
  ElementSpec: TEDIElementSpec;
  TransactionSetSpec: TEDITransactionSetSpec;
begin
  Result := TEDISEFFile.Create(nil);

  ElementList := TEDIObjectList.Create(False);
  try
    //Fill Element Dictionary
    for F := 0 to ICSpec.FunctionalGroupCount - 1 do
      for T := 0 to ICSpec[F].TransactionSetCount - 1 do
        for S := 0 to ICSpec[F][T].SegmentCount - 1 do
        begin
          SegmentSpec := TEDISegmentSpec(ICSpec[F][T][S]);
          for E := 0 to SegmentSpec.ElementCount - 1 do
          begin
            ElementSpec := TEDIElementSpec(SegmentSpec[E]);
            if Result.ELMS.FindItemByName(ElementSpec.Id) = nil then
              Result.ELMS.Add(TranslateToSEFElement(ElementSpec, Result))
            else
            begin
              //raise Exception.Create('Element Repeated - Incompatible File');
            end;
          end;
        end;
    //Fill Segment Dictionary
    for F := 0 to ICSpec.FunctionalGroupCount - 1 do
      for T := 0 to ICSpec[F].TransactionSetCount - 1 do
        for S := 0 to ICSpec[F][T].SegmentCount - 1 do
        begin
          SegmentSpec := TEDISegmentSpec(ICSpec[F][T][S]);
          if Result.SEGS.FindItemByName(SegmentSpec.Id) = nil then
            Result.SEGS.Add(TranslateToSEFSegment(SegmentSpec, Result))
          else
          begin
            //raise Exception.Create('Segment Repeated - Incompatible File');
          end;
        end;
    //Fill Transaction Set Dictionary
    for F := 0 to ICSpec.FunctionalGroupCount - 1 do
      for T := 0 to ICSpec[F].TransactionSetCount - 1 do
        for S := 0 to ICSpec[F][T].SegmentCount - 1 do
        begin
          TransactionSetSpec := TEDITransactionSetSpec(ICSpec[F][T]);
          if Result.SETS.FindItemByName(TransactionSetSpec.Id) = nil then
            Result.SETS.Add(TranslateToSEFSet(TransactionSetSpec, Result))
          else
          begin
            //raise Exception.Create('Segment Repeated - Incompatible File');
          end;
        end;
  finally
    ElementList.Free;
  end;
end;

procedure TEDISpecToSEFTranslator.TranslateToSEFElementTEXTSETS(ElementSpec: TEDIElementSpec;
  SEFElement: TEDISEFElement);
var
  Location: string;
  Data: string;
begin
  Location := SEFElement.GetTextSetsLocation;
  Data := ElementSpec.Notes;
  Data := JclEDI.StringReplace(Data, AnsiCrLf, SEFTextCRLF, [rfReplaceAll]);
  Data := JclEDI.StringReplace(Data, AnsiCarriageReturn, SEFTextCR, [rfReplaceAll]);
  Data := JclEDI.StringReplace(Data, AnsiLineFeed, SEFTextLF, [rfReplaceAll]);
  SEFElement.TEXTSETS.SetText(SEFElement.SEFFile, Location, SEFTextSetsCode_Elm0, Data);
  Data := ElementSpec.Description;
  Data := JclEDI.StringReplace(Data, AnsiCrLf, SEFTextCRLF, [rfReplaceAll]);
  Data := JclEDI.StringReplace(Data, AnsiCarriageReturn, SEFTextCR, [rfReplaceAll]);
  Data := JclEDI.StringReplace(Data, AnsiLineFeed, SEFTextLF, [rfReplaceAll]);
  SEFElement.TEXTSETS.SetText(SEFElement.SEFFile, Location, SEFTextSetsCode_Elm2, Data);
end;

procedure TEDISpecToSEFTranslator.TranslateToSEFSegmentTEXTSETS(SegmentSpec: TEDISegmentSpec;
  SEFSegment: TEDISEFSegment);
var
  Location: string;
  E: Integer;
  ElementSpec: TEDIElementSpec;
  SEFElement: TEDISEFElement;
  Data: string;
begin
  Location := SEFSegment.GetTextSetsLocation;
  Data := SegmentSpec.Description;
  Data := JclEDI.StringReplace(Data, AnsiCrLf, SEFTextCRLF, [rfReplaceAll]);
  Data := JclEDI.StringReplace(Data, AnsiCarriageReturn, SEFTextCR, [rfReplaceAll]);
  Data := JclEDI.StringReplace(Data, AnsiLineFeed, SEFTextLF, [rfReplaceAll]);
  SEFSegment.TEXTSETS.SetText(SEFSegment.SEFFile, Location, SEFTextSetsCode_Seg3, Data);
  Data := SegmentSpec.Notes;
  Data := JclEDI.StringReplace(Data, AnsiCrLf, SEFTextCRLF, [rfReplaceAll]);
  Data := JclEDI.StringReplace(Data, AnsiCarriageReturn, SEFTextCR, [rfReplaceAll]);
  Data := JclEDI.StringReplace(Data, AnsiLineFeed, SEFTextLF, [rfReplaceAll]);
  SEFSegment.TEXTSETS.SetText(SEFSegment.SEFFile, Location, SEFTextSetsCode_Seg4, Data);

  SEFSegment.AssignElementOrdinals;
  for E := 0 to SegmentSpec.ElementCount - 1 do
  begin
    ElementSpec := TEDIElementSpec(SegmentSpec[E]);
    SEFElement := TEDISEFElement(SEFSegment[E]);
    TranslateToSEFElementTEXTSETS(ElementSpec, SEFElement);
  end;
end;

//=== { TEDISEFToSpecTranslator } ============================================

constructor TEDISEFToSpecTranslator.Create;
begin
  inherited Create;
end;

destructor TEDISEFToSpecTranslator.Destroy;
begin
  inherited Destroy;
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
