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
{ The Original Code is JclPCRE.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Peter Thornqvist.                                  }
{ Portions created by Peter Thornqvist are Copyright (C) of Peter Thornqvist. All rights reserved. }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Robert Rossmair (rrossmair)                                                                    }
{   Mario R. Carro                                                                                 }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Class wrapper for PCRE (PERL Compatible Regular Expression)                                      }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-07-27 16:56:45 +0200 (dim., 27 juil. 2008)                         $ }
{ Revision:      $Rev:: 2398                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclPCRE;

{$I jcl.inc}

{$RANGECHECKS OFF}

interface

uses
  pcre,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  Classes, SysUtils, JclBase;

const
  JCL_PCRE_CALLOUT_NOERROR      = 0;
  JCL_PCRE_CALLOUT_FAILCONTINUE = 1;

  JCL_PCRE_ERROR_CALLOUTERROR   = -998;
  JCL_PCRE_ERROR_STUDYFAILED    = -999;

type
  TJclAnsiRegEx = class;

  EPCREError = class(EJclError)
  private
    FErrorCode: Integer;
  public
    constructor CreateRes(ResStringRec: PResStringRec; ErrorCode: Integer);
    property ErrorCode: Integer read FErrorCode;
  end;

  TPCREIntArray = array [0 .. 0] of Integer;
  PPCREIntArray = ^TPCREIntArray;

  TJclAnsiRegExOption = (roIgnoreCase, roMultiLine, roDotAll, roExtended,
    roAnchored, roDollarEndOnly, roExtra, roNotBOL, roNotEOL, roUnGreedy,
    roNotEmpty, roUTF8, roNoAutoCapture, roNoUTF8Check, roAutoCallout,
    roPartial, roDfaShortest, roDfaRestart, roDfaFirstLine, roDupNames,
    roNewLineCR, roNewLineLF, roNewLineCRLF, roNewLineAny, roBSRAnyCRLF,
    roBSRUnicode, roJavascriptCompat);
  TJclAnsiRegExOptions = set of TJclAnsiRegExOption;
  TJclAnsiCaptureRange = record
    FirstPos: Integer;
    LastPos: Integer;
  end;

  TJclAnsiRegExCallout = procedure (Sender: TJclAnsiRegEx;
    Index, MatchStart, SubjectPos, LastCapture, PatternPos, NextItemLength: Integer;
    var ErrorCode: Integer) of object;
  TPCRECalloutIndex = 0 .. 255;

  TJclAnsiRegEx = class(TObject)
  private
    FCode: PPCRE;
    FExtra: PPCREExtra;
    FOptions: TJclAnsiRegExOptions;
    FPattern: AnsiString;
    FDfaMode: Boolean;
    FSubject: AnsiString;

    FViewChanges: Boolean;
    FChangedCaptures: TList;
    FResultValues: array of String;

    FErrorCode: Integer;
    FErrorMessage: AnsiString;
    FErrorOffset: Integer;

    FVector: PPCREIntArray;
    FVectorSize: Integer;
    FCaptureCount: Integer;

    FOnCallout: TJclAnsiRegExCallout;

    function GetResult: AnsiString;
    function GetCapture(Index: Integer): AnsiString;
    procedure SetCapture(Index: Integer; const Value: AnsiString);
    function GetCaptureRange(Index: Integer): TJclAnsiCaptureRange;
    function GetNamedCapture(const Name: AnsiString): AnsiString;
    procedure SetNamedCapture(const Name, Value: Ansistring);
    function GetCaptureNameCount: Integer;
    function GetCaptureName(Index: Integer): String;
    function GetAPIOptions(RunTime: Boolean): Integer;
    function CalloutHandler(var CalloutBlock: pcre_callout_block): Integer;

  public
    destructor Destroy; override;

    property Options: TJclAnsiRegExOptions read FOptions write FOptions;
    function Compile(const Pattern: AnsiString; Study: Boolean;
      UserLocale: Boolean = False): Boolean;
    property Pattern: AnsiString read FPattern;
    property DfaMode: Boolean read FDfaMode write FDfaMode;
    function Match(const Subject: AnsiString; StartOffset: Cardinal = 1): Boolean;
    property Subject: AnsiString read FSubject;
    property Result: AnsiString read GetResult;

    property ViewChanges: Boolean read FViewChanges write FViewChanges;
    property CaptureCount: Integer read FCaptureCount write FCaptureCount;
    property Captures[Index: Integer]: AnsiString read GetCapture write SetCapture;
    property CaptureRanges[Index: Integer]: TJclAnsiCaptureRange read GetCaptureRange;

    property NamedCaptures[const Name: AnsiString]: AnsiString
      read GetNamedCapture write SetNamedCapture;
    property CaptureNameCount: Integer read GetCaptureNameCount;
    property CaptureNames[Index: Integer]: AnsiString read GetCaptureName;
    function IndexOfName(const Name: String): Integer;
    function IsNameValid(const Name: String): Boolean;

    property ErrorCode: Integer read FErrorCode;
    property ErrorMessage: AnsiString read FErrorMessage;
    property ErrorOffset: Integer read FErrorOffset;

    property OnCallout: TJclAnsiRegExCallout
      read FOnCallout write FOnCallout;
  end;

procedure InitializeLocaleSupport;
procedure TerminateLocaleSupport;

// Args is an array of pairs (CaptureIndex, Value) or (CaptureName, Value).
// For example: NewIp := StrReplaceRegEx(DirIP, '(\d+)\.(\d+)\.(\d+)\.(\d+)', [3, '128', 4, '254']); 
function StrReplaceRegEx(const Subject, Pattern: AnsiString; Args: array of const): AnsiString;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclPCRE.pas $';
    Revision: '$Revision: 2398 $';
    Date: '$Date: 2008-07-27 16:56:45 +0200 (dim., 27 juil. 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysConst,
  JclResources;

var
  GTables: PChar;

function JclPCREGetMem(Size: Integer): Pointer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  GetMem(Result, Size);
end;

procedure JclPCREFreeMem(P: Pointer); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  FreeMem(P);
end;

function JclPCRECallout(var callout_block: pcre_callout_block): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
   Result := TJclAnsiRegEx(callout_block.callout_data).CalloutHandler(callout_block);
end;

function PCRECheck(Value: Integer): Boolean;
var
  PErr: PResStringRec;
begin
  Result := Value >= 0;
  if Result then Exit;

  case Value of
    PCRE_ERROR_NOMATCH:
      PErr := @RsErrNoMatch;
    PCRE_ERROR_NULL:
      PErr := @RsErrNull;
    PCRE_ERROR_BADOPTION:
      PErr := @RsErrBadOption;
    PCRE_ERROR_BADMAGIC:
      PErr := @RsErrBadMagic;
    PCRE_ERROR_UNKNOWN_NODE:
      PErr := @RsErrUnknownNode;
    PCRE_ERROR_NOMEMORY:
      PErr := @RsErrNoMemory;
    PCRE_ERROR_NOSUBSTRING:
      PErr := @RsErrNoSubString;
    PCRE_ERROR_MATCHLIMIT:
      PErr := @RsErrMatchLimit;
    PCRE_ERROR_CALLOUT:
      PErr := @RsErrCallout;
    PCRE_ERROR_BADUTF8:
      PErr := @RsErrBadUTF8;
    PCRE_ERROR_BADUTF8_OFFSET:
      PErr := @RsErrBadUTF8Offset;
    PCRE_ERROR_PARTIAL:
      PErr := @RsErrPartial;
    PCRE_ERROR_BADPARTIAL:
      PErr := @RsErrBadPartial;
    PCRE_ERROR_INTERNAL:
      PErr := @RsErrInternal;
    PCRE_ERROR_BADCOUNT:
      PErr := @RsErrBadCount;
    PCRE_ERROR_DFA_UITEM:
      PErr := @RsErrDfaUItem;
    PCRE_ERROR_DFA_UCOND:
      PErr := @RsErrDfaUCond;
    PCRE_ERROR_DFA_UMLIMIT:
      PErr := @RsErrDfaUMLimit;
    PCRE_ERROR_DFA_WSSIZE:
      PErr := @RsErrDfaWSSize;
    PCRE_ERROR_DFA_RECURSE:
      PErr := @RsErrDfaRecurse;
    PCRE_ERROR_RECURSIONLIMIT:
      PErr := @RsErrRecursionLimit;
    PCRE_ERROR_NULLWSLIMIT:
      PErr := @RsErrNullWsLimit;
    PCRE_ERROR_BADNEWLINE:
      PErr := @RsErrBadNewLine;
    JCL_PCRE_ERROR_STUDYFAILED:
      PErr := @RsErrStudyFailed;
    JCL_PCRE_ERROR_CALLOUTERROR:
      PErr := @RsErrCalloutError;
  else
    PErr := @RsErrUnknownError;
  end;

  raise EPCREError.CreateRes(PErr, Value);
end;

//=== { TJclAnsiRegEx } ======================================================

destructor TJclAnsiRegEx.Destroy;
begin
  if Assigned(FCode) then
    CallPCREFree(FCode);
  if Assigned(FExtra) then
    CallPCREFree(FExtra);
  if Assigned(FVector) then
    FreeMem(FVector);
  if Assigned(FChangedCaptures) then
    FChangedCaptures.Free;

  inherited Destroy;
end;

function TJclAnsiRegEx.Compile(const Pattern: AnsiString; Study: Boolean;
  UserLocale: Boolean = False): Boolean;
var
  ErrMsgPtr: PChar;
  Tables: PChar;
begin
  if UserLocale then
  begin
    InitializeLocaleSupport;
    Tables := GTables;
  end
  else
    Tables := nil;

  FPattern := Pattern;
  if FPattern = '' then
    raise EPCREError.CreateRes(@RsErrNull, PCRE_ERROR_NULL);

  if Assigned(FCode) then
    CallPCREFree(FCode);
  FCode := pcre_compile2(PChar(FPattern), GetAPIOptions(False),
    @FErrorCode, @ErrMsgPtr, @FErrorOffset, Tables);
  Inc(FErrorOffset);
  FErrorMessage := ErrMsgPtr;
  Result := Assigned(FCode);
  if Result then
  begin
    if Study then
    begin
      if Assigned(FExtra) then CallPCREFree(FExtra);
      FExtra := pcre_study(FCode, 0, @ErrMsgPtr);
      Result := Assigned(FExtra) or (not Assigned(ErrMsgPtr));
      if not Result then
      begin
        FErrorCode := JCL_PCRE_ERROR_STUDYFAILED;
        FErrorMessage := ErrMsgPtr;
      end;
    end;

    if FDfaMode then
      FVectorSize := FCaptureCount
    else
    begin
      PCRECheck(pcre_fullinfo(FCode, FExtra, PCRE_INFO_CAPTURECOUNT, @FCaptureCount));
      FVectorSize := (FCaptureCount + 1) * 3;
    end;
    ReAllocMem(FVector, FVectorSize * SizeOf(Integer));
  end;
end;

function TJclAnsiRegEx.GetAPIOptions(RunTime: Boolean): Integer;
const
  { roIgnoreCase, roMultiLine, roDotAll, roExtended,
    roAnchored, roDollarEndOnly, roExtra, roNotBOL, roNotEOL, roUnGreedy,
    roNotEmpty, roUTF8, roNoAutoCapture, roNoUTF8Check, roAutoCallout,
    roPartial, roDfaShortest, roDfaRestart, roDfaFirstLine, roDupNames,
    roNewLineCR, roNewLineLF, roNewLineCRLF, roNewLineAny }
  cDesignOptions: array [TJclAnsiRegExOption] of Integer =
   (PCRE_CASELESS, PCRE_MULTILINE, PCRE_DOTALL, PCRE_EXTENDED, PCRE_ANCHORED,
    PCRE_DOLLAR_ENDONLY, PCRE_EXTRA, 0, 0, PCRE_UNGREEDY, 0, PCRE_UTF8,
    PCRE_NO_AUTO_CAPTURE, PCRE_NO_UTF8_CHECK, PCRE_AUTO_CALLOUT, 0, 0, 0, 0,
    PCRE_DUPNAMES, PCRE_NEWLINE_CR, PCRE_NEWLINE_LF, PCRE_NEWLINE_CRLF,
    PCRE_NEWLINE_ANY, PCRE_BSR_ANYCRLF, PCRE_BSR_UNICODE,
    PCRE_JAVASCRIPT_COMPAT);
  cRunOptions: array [TJclAnsiRegExOption] of Integer =
   (0, 0, 0, 0, 0, 0, 0, PCRE_NOTBOL, PCRE_NOTEOL, 0, PCRE_NOTEMPTY, 0, 0,
   PCRE_NO_UTF8_CHECK, 0, PCRE_PARTIAL, 0, 0, 0, 0, PCRE_NEWLINE_CR,
   PCRE_NEWLINE_LF, PCRE_NEWLINE_CRLF, PCRE_NEWLINE_ANY, PCRE_BSR_ANYCRLF,
   PCRE_BSR_UNICODE, PCRE_JAVASCRIPT_COMPAT);
var
  I: TJclAnsiRegExOption;
begin
  Result := 0;
  if RunTime then
  begin
    for I := Low(TJclAnsiRegExOption) to High(TJclAnsiRegExOption) do
      if I in Options then
        Result := Result or cRunOptions[I];
  end
  else
  begin
    for I := Low(TJclAnsiRegExOption) to High(TJclAnsiRegExOption) do
      if I in Options then
        Result := Result or cDesignOptions[I];
  end;
end;

function TJclAnsiRegEx.GetResult: AnsiString;
var
  Index, CaptureIndex, Pos: Integer;
  Range: TJclAnsiCaptureRange;
begin
  if Assigned(FChangedCaptures) and (FChangedCaptures.Count > 0) then
  begin
    Pos := 1;
    Result := '';
    for Index := 0 to FChangedCaptures.Count - 1 do
    begin
      CaptureIndex := Integer(FChangedCaptures[Index]);
      Range := GetCaptureRange(CaptureIndex);
      
      Result := Result +
        Copy(FSubject, Pos, Range.FirstPos - Pos) + 
        FResultValues[CaptureIndex];
        
      Pos := Range.LastPos + 1;
    end;
    if Pos <= Length(FSubject) then
      Result := Result + Copy(FSubject, Pos, Length(FSubject) - Pos + 1);
  end
  else
    Result := FSubject;
end;

function TJclAnsiRegEx.GetCapture(Index: Integer): AnsiString;
var
  From, Len: Integer;
begin
  if (Index < 0) or (Index >= FCaptureCount) then
    PCRECheck(PCRE_ERROR_NOSUBSTRING)
  else
  begin
    if FViewChanges and (FChangedCaptures.IndexOf(Pointer(Index)) >= 0) then
    begin
      Result := FResultValues[Index];
      Exit;
    end;

    Index := Index * 2;
    From := FVector^[Index];
    Len := FVector^[Index + 1] - From;
    SetLength(Result, Len);
    Move(FSubject[From + 1], PChar(Result)^, Len);
  end;
end;

procedure TJclAnsiRegEx.SetCapture(Index: Integer; const Value: String);
begin
  if (Index < 0) or (Index >= FCaptureCount) then
    PCRECheck(PCRE_ERROR_NOSUBSTRING)
  else
  begin
    if (not Assigned(FChangedCaptures)) or (FChangedCaptures.Count = 0) then
    begin
      if not Assigned(FChangedCaptures) then
        FChangedCaptures := TList.Create;
        
      // Always resize to the max length to avoid repeated allocations.
      FChangedCaptures.Capacity := FCaptureCount;
      SetLength(FResultValues, FCaptureCount);
    end;

    if FChangedCaptures.IndexOf(Pointer(Index)) < 0 then
      FChangedCaptures.Add(Pointer(Index));
    FResultValues[Index] := Value;
  end;
end;

function TJclAnsiRegEx.GetCaptureRange(Index: Integer): TJclAnsiCaptureRange;
begin
  if (Index < 0) or (Index >= FCaptureCount) then
    PCRECheck(PCRE_ERROR_NOSUBSTRING)
  else
  begin
    Index := Index * 2;
    Result.FirstPos := FVector^[Index] + 1;
    Result.LastPos := FVector^[Index + 1];
  end;
end;

function TJclAnsiRegEx.GetNamedCapture(const Name: AnsiString): AnsiString;
var
  Index: Integer;
begin
  Index := pcre_get_stringnumber(FCode, PChar(Name));
  PCRECheck(Index);

  Result := GetCapture(Index);
end;

procedure TJclAnsiRegEx.SetNamedCapture(const Name, Value: String);
var
  Index: Integer;
begin
  Index := pcre_get_stringnumber(FCode, PChar(Name));
  PCRECheck(Index);

  SetCapture(Index, Value);
end;

function TJclAnsiRegEx.GetCaptureNameCount: Integer;
begin
  PCRECheck(pcre_fullinfo(FCode, FExtra, PCRE_INFO_NAMECOUNT, @Result));
end;

function TJclAnsiRegEx.GetCaptureName(Index: Integer): String;
var
  NameTable: PChar;
  EntrySize: Integer;
begin
  PCRECheck(pcre_fullinfo(FCode, FExtra, PCRE_INFO_NAMETABLE, @NameTable));
  PCRECheck(pcre_fullinfo(FCode, FExtra, PCRE_INFO_NAMEENTRYSIZE, @EntrySize));

  Result := NameTable + EntrySize * Index + 2;
end;

function TJclAnsiRegEx.CalloutHandler(var CalloutBlock: pcre_callout_block): Integer;
begin
  try
    Result := JCL_PCRE_CALLOUT_NOERROR;
    if Assigned(FOnCallout) then
    begin
      with CalloutBlock do
      begin
        FCaptureCount := capture_top;
        FOnCallout(Self, callout_number, start_match + 1, current_position + 1,
          capture_last, pattern_position + 1, next_item_length, Result);
      end;
    end;
  except
    on E: Exception do
    begin
      FErrorMessage := E.Message;
      Result := JCL_PCRE_ERROR_CALLOUTERROR;
    end;
  end;
end;

function TJclAnsiRegEx.Match(const Subject: AnsiString; StartOffset: Cardinal = 1): Boolean;
var
  LocalExtra: real_pcre_extra;
  Extra: Pointer;
  WorkSpace: array [0 .. 19] of Integer;
  ExecRslt: Integer;
begin
  if Assigned(FOnCallout) then
  begin
    if Assigned(FExtra) then
    begin
      LocalExtra.flags := PCRE_EXTRA_STUDY_DATA or PCRE_EXTRA_CALLOUT_DATA;
      LocalExtra.study_data := FExtra;
    end
    else
      LocalExtra.flags := PCRE_EXTRA_CALLOUT_DATA;
    LocalExtra.callout_data := Self;
    Extra := @LocalExtra;
    SetPCRECalloutCallback(JclPCRECallout);
  end
  else
  begin
    Extra := FExtra;
    SetPCRECalloutCallback(nil);
  end;

  FSubject := Subject;
  if Assigned(FChangedCaptures) then
    FChangedCaptures.Clear;
  if FDfaMode then
  begin
    ExecRslt := pcre_dfa_exec(FCode, Extra, PChar(FSubject), Length(FSubject),
      StartOffset - 1, GetAPIOptions(True), pcre.PInteger(FVector), FVectorSize, @Workspace, 20);
  end
  else
  begin
    ExecRslt := pcre_exec(FCode, Extra, PChar(FSubject), Length(FSubject),
      StartOffset - 1, GetAPIOptions(True), pcre.PInteger(FVector), FVectorSize);
  end;
  Result := ExecRslt >= 0;
  if Result then
  begin
    FCaptureCount := ExecRslt;
    FErrorCode := 0;
  end
  else
  begin
    FErrorCode := ExecRslt;
    if FErrorCode <> PCRE_ERROR_NOMATCH then
      PCRECheck(FErrorCode);
  end;
end;

function TJclAnsiRegEx.IndexOfName(const Name: String): Integer;
begin
  Result := pcre_get_stringnumber(FCode, PChar(Name));
end;

function TJclAnsiRegEx.IsNameValid(const Name: String): Boolean;
begin
  Result := pcre_get_stringnumber(FCode, PChar(Name)) >= 0;
end;

procedure InitializeLocaleSupport;
begin
  if not Assigned(GTables) then
    GTables := pcre_maketables;
end;

procedure TerminateLocaleSupport;
begin
  if Assigned(GTables) then
  begin
    CallPCREFree(GTables);
    GTables := nil;
  end;
end;

// TODO: Better/specific error messages, show index when available.
function StrReplaceRegEx(const Subject, Pattern: AnsiString; Args: array of const): AnsiString;

  function ArgToString(Index: Integer): AnsiString;
  begin
    // TODO: Any other type?
    case TVarRec(Args[Index]).VType of
      vtString: Result := TVarRec(Args[Index]).VString^;
      vtPChar: Result := TVarRec(Args[Index]).VPChar;
      vtAnsiString: Result := AnsiString(TVarRec(Args[Index]).VAnsiString);
      else
        raise EConvertError.Create(SInvalidFormat);
    end;
  end;

var
  Re: TJclAnsiRegEx;
  Index, ArgIndex: Integer;
  Value: AnsiString;
begin
  if Odd(Length(Args)) then
    raise EConvertError.Create(SArgumentMissing)
  else
  begin
    Re := TJclAnsiRegEx.Create;
    try
      if Re.Compile(Pattern, False) and Re.Match(Subject) then
      begin
        for Index := 0 to Length(Args) div 2 - 1 do
        begin
          ArgIndex := Index * 2;
          Value := ArgToString(ArgIndex + 1);

          if TVarRec(Args[ArgIndex]).VType = vtInteger then
            Re.Captures[TVarRec(Args[ArgIndex]).VInteger] := Value
          else
            Re.NamedCaptures[ArgToString(ArgIndex)] := Value;
        end;

        Result := Re.Result;
      end
      else
        raise EConvertError.Create(SInvalidFormat);
    finally
      Re.Free;
    end;
  end;
end;

//=== { EPCREError } =========================================================

constructor EPCREError.CreateRes(ResStringRec: PResStringRec; ErrorCode: Integer);
begin
  FErrorCode := ErrorCode;
  inherited CreateRes(ResStringRec);
end;

procedure LibNotLoadedHandler; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
begin
  raise EPCREError.CreateRes(@RsErrLibNotLoaded, 0);
end;

initialization
  pcre.LibNotLoadedHandler := LibNotLoadedHandler;
  if LoadPCRE then
  begin
    SetPCREMallocCallback(JclPCREGetMem);
    SetPCREFreeCallback(JclPCREFreeMem);
  end;
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  TerminateLocaleSupport;
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  UnloadPCRE;

end.

