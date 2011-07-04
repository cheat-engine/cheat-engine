{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynMemo.pas, released 2000-04-07.
The Original Code is based on mwCustomEdit.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynMemo.pas,v 1.15 2004/01/04 21:49:04 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
  - several EM_XXX messages aren't handled yet;
  - EM_XXX messages aren't implemented on CLX, although this could be useful;
-------------------------------------------------------------------------------}

{$IFNDEF QSYNMEMO}
unit SynMemo;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Qt,
  Types,
  QSynEdit,
  QSynEditTextBuffer,
  QSynEditTypes,
{$ELSE}
  RichEdit,
  Windows,
  Messages,
  SynEdit,
  SynEditTextBuffer,
  SynEditTypes,
{$ENDIF}
  SysUtils,
  Classes;

type
  TSynMemo = class(TSynEdit)
{$IFNDEF SYN_CLX}
  private
    // EM_XXX see winuser.h (PSDK August 2001)
    procedure EMGetSel(var Message: TMessage); message EM_GETSEL;
    procedure EMSetSel(var Message: TMessage); message EM_SETSEL;
    //procedure EMGetRect(var Message: TMessage); message EM_GETRECT;
    //procedure EMSetRect(var Message: TMessage); message EM_SETRECT;
    //procedure EMSetRectnp(var Message: TMessage); message EM_SETRECTNP;
    //procedure EMScroll(var Message: TMessage); message EM_SCROLL;
    //procedure EMLineScroll(var Message: TMessage); message EM_LINESCROLL;
    //procedure EMScrollCaret(var Message: TMessage); message EM_SCROLLCARET;
    procedure EMGetModify(var Message: TMessage); message EM_GETMODIFY;
    procedure EMSetModify(var Message: TMessage); message EM_SETMODIFY;
    procedure EMGetLineCount(var Message: TMessage); message EM_GETLINECOUNT;
    //procedure EMLineIndex(var Message: TMessage); message EM_LINEINDEX;
    //procedure EMSetHandle(var Message: TMessage); message EM_SETHANDLE;
    //procedure EMGetHandle(var Message: TMessage); message EM_GETHANDLE;
    //procedure EMGetThumb(var Message: TMessage); message EM_GETTHUMB;
    //procedure EMLineLength(var Message: TMessage); message EM_LINELENGTH;
    procedure EMGetSelText(var Message: TMessage); message EM_GETSELTEXT;       //richedit.h
    procedure EMReplaceSel(var Message: TMessage); message EM_REPLACESEL;
    procedure EMGetLine(var Message: TMessage); message EM_GETLINE;
    //procedure EMLimitText(var Message: TMessage); message EM_LIMITTEXT;
    procedure EMCanUndo(var Message: TMessage); message EM_CANUNDO;
    procedure EMUndo(var Message: TMessage); message EM_UNDO;
    //procedure EMFmtLines(var Message: TMessage); message EM_FMTLINES;
    //procedure EMLineFromChar(var Message: TMessage); message EM_LINEFROMCHAR;
    //procedure EMSetTabStops(var Message: TMessage); message EM_SETTABSTOPS;
    //procedure EMSetPasswordChar(var Message: TMessage); message EM_SETPASSWORDCHAR;
    //procedure EMEmptyUndoBuffer(var Message: TMessage); message EM_EMPTYUNDOBUFFER;
    procedure EMGetFirstVisibleLine(var Message: TMessage); message EM_GETFIRSTVISIBLELINE;
    //procedure EMSetReadOnly(var Message: TMessage); message EM_SETREADONLY;
    //procedure EMSetWordBreakProc(var Message: TMessage); message EM_SETWORDBREAKPROC;
    //procedure EMGetWordBreakProc(var Message: TMessage); message EM_GETWORDBREAKPROC;
    //procedure EMGetPasswordChar(var Message: TMessage); message EM_GETPASSWORDCHAR;
    //procedure EMSetMargins(var Message: TMessage); message EM_SETMARGINS;
    //procedure EMGetMargins(var Message: TMessage); message EM_GETMARGINS;
    //procedure EMSetLimitText(var Message: TMessage); message EM_SETLIMITTEXT;
    //procedure EMGetLimitText(var Message: TMessage); message EM_GETLIMITTEXT;
    //procedure EMPosFromChar(var Message: TMessage); message EM_POSFROMCHAR;
    procedure EMCharFromPos(var Message: TMessage); message EM_CHARFROMPOS;
    //procedure EMSetImestatus(var Message: TMessage); message EM_SETIMESTATUS;
    //procedure EMGetImestatus(var Message: TMessage); message EM_GETIMESTATUS;

    // EM_XXX see richedit.h (PSDK August 2001)
{
    procedure EMCANPASTE(var Message: TMessage); message EM_CANPASTE;
    procedure EMDISPLAYBAND(var Message: TMessage); message EM_DISPLAYBAND;
    procedure EMEXGETSEL(var Message: TMessage); message EM_EXGETSEL;
    procedure EMEXLIMITTEXT(var Message: TMessage); message EM_EXLIMITTEXT;
    procedure EMEXLINEFROMCHAR(var Message: TMessage); message EM_EXLINEFROMCHAR;
    procedure EMEXSETSEL(var Message: TMessage); message EM_EXSETSEL;
    procedure EMFINDTEXT(var Message: TMessage); message EM_FINDTEXT;
    procedure EMFORMATRANGE(var Message: TMessage); message EM_FORMATRANGE;
    procedure EMGETCHARFORMAT(var Message: TMessage); message EM_GETCHARFORMAT;
    procedure EMGETEVENTMASK(var Message: TMessage); message EM_GETEVENTMASK;
    procedure EMGETOLEINTERFACE(var Message: TMessage); message EM_GETOLEINTERFACE;
    procedure EMGETPARAFORMAT(var Message: TMessage); message EM_GETPARAFORMAT;
    procedure EMGETSELTEXT(var Message: TMessage); message EM_GETSELTEXT;
    procedure EMHIDESELECTION(var Message: TMessage); message EM_HIDESELECTION;
    procedure EMPASTESPECIAL(var Message: TMessage); message EM_PASTESPECIAL;
    procedure EMREQUESTRESIZE(var Message: TMessage); message EM_REQUESTRESIZE;
    procedure EMSELECTIONTYPE(var Message: TMessage); message EM_SELECTIONTYPE;
    procedure EMSETBKGNDCOLOR(var Message: TMessage); message EM_SETBKGNDCOLOR;
    procedure EMSETCHARFORMAT(var Message: TMessage); message EM_SETCHARFORMAT;
    procedure EMSETEVENTMASK(var Message: TMessage); message EM_SETEVENTMASK;
    procedure EMSETOLECALLBACK(var Message: TMessage); message EM_SETOLECALLBACK;
    procedure EMSETPARAFORMAT(var Message: TMessage); message EM_SETPARAFORMAT;
    procedure EMSETTARGETDEVICE(var Message: TMessage); message EM_SETTARGETDEVICE;
    procedure EMSTREAMIN(var Message: TMessage); message EM_STREAMIN;
    procedure EMSTREAMOUT(var Message: TMessage); message EM_STREAMOUT;
    procedure EMGETTEXTRANGE(var Message: TMessage); message EM_GETTEXTRANGE;
    procedure EMFINDWORDBREAK(var Message: TMessage); message EM_FINDWORDBREAK;
    procedure EMSETOPTIONS(var Message: TMessage); message EM_SETOPTIONS;
    procedure EMGETOPTIONS(var Message: TMessage); message EM_GETOPTIONS;
    procedure EMFINDTEXTEX(var Message: TMessage); message EM_FINDTEXTEX;

    procedure EMGETWORDBREAKPROCEX(var Message: TMessage); message EM_GETWORDBREAKPROCEX;
    procedure EMSETWORDBREAKPROCEX(var Message: TMessage); message EM_SETWORDBREAKPROCEX;

    // RichEdit 2.0 messages
    procedure EMSETUNDOLIMIT(var Message: TMessage); message EM_SETUNDOLIMIT;
    procedure EMREDO(var Message: TMessage); message EM_REDO;
    procedure EMCANREDO(var Message: TMessage); message EM_CANREDO;
    procedure EMGETUNDONAME(var Message: TMessage); message EM_GETUNDONAME;
    procedure EMGETREDONAME(var Message: TMessage); message EM_GETREDONAME;
    procedure EMSTOPGROUPTYPING(var Message: TMessage); message EM_STOPGROUPTYPING;

    procedure EMSETTEXTMODE(var Message: TMessage); message EM_SETTEXTMODE;
    procedure EMGETTEXTMODE(var Message: TMessage); message EM_GETTEXTMODE;

    procedure EMAUTOURLDETECT(var Message: TMessage); message EM_AUTOURLDETECT;
    procedure EMGETAUTOURLDETECT(var Message: TMessage); message EM_GETAUTOURLDETECT;
    procedure EMSETPALETTE(var Message: TMessage); message EM_SETPALETTE;
    procedure EMGETTEXTEX(var Message: TMessage); message EM_GETTEXTEX;
    procedure EMGETTEXTLENGTHEX(var Message: TMessage); message EM_GETTEXTLENGTHEX;
    procedure EMSHOWSCROLLBAR(var Message: TMessage); message EM_SHOWSCROLLBAR;
    procedure EMSETTEXTEX(var Message: TMessage); message EM_SETTEXTEX;

    // Far East specific messages
    procedure EMSETPUNCTUATION(var Message: TMessage); message EM_SETPUNCTUATION;
    procedure EMGETPUNCTUATION(var Message: TMessage); message EM_GETPUNCTUATION;
    procedure EMSETWORDWRAPMODE(var Message: TMessage); message EM_SETWORDWRAPMODE;
    procedure EMGETWORDWRAPMODE(var Message: TMessage); message EM_GETWORDWRAPMODE;
    procedure EMSETIMECOLOR(var Message: TMessage); message EM_SETIMECOLOR;
    procedure EMGETIMECOLOR(var Message: TMessage); message EM_GETIMECOLOR;
    procedure EMSETIMEOPTIONS(var Message: TMessage); message EM_SETIMEOPTIONS;
    procedure EMGETIMEOPTIONS(var Message: TMessage); message EM_GETIMEOPTIONS;
    procedure EMCONVPOSITION(var Message: TMessage); message EM_CONVPOSITION;

    procedure EMSETLANGOPTIONS(var Message: TMessage); message EM_SETLANGOPTIONS;
    procedure EMGETLANGOPTIONS(var Message: TMessage); message EM_GETLANGOPTIONS;
    procedure EMGETIMECOMPMODE(var Message: TMessage); message EM_GETIMECOMPMODE;

    procedure EMFINDTEXTW(var Message: TMessage); message EM_FINDTEXTW;
    procedure EMFINDTEXTEXW(var Message: TMessage); message EM_FINDTEXTEXW;

    // RE3.0 FE messages
    procedure EMRECONVERSION(var Message: TMessage); message EM_RECONVERSION;
    procedure EMSETIMEMODEBIAS(var Message: TMessage); message EM_SETIMEMODEBIAS;
    procedure EMGETIMEMODEBIAS(var Message: TMessage); message EM_GETIMEMODEBIAS;

    // BiDi specific messages
    procedure EMSETBIDIOPTIONS(var Message: TMessage); message EM_SETBIDIOPTIONS;
    procedure EMGETBIDIOPTIONS(var Message: TMessage); message EM_GETBIDIOPTIONS;

    procedure EMSETTYPOGRAPHYOPTIONS(var Message: TMessage); message EM_SETTYPOGRAPHYOPTIONS;
    procedure EMGETTYPOGRAPHYOPTIONS(var Message: TMessage); message EM_GETTYPOGRAPHYOPTIONS;

    // Extended edit style specific messages
    procedure EMSETEDITSTYLE(var Message: TMessage); message EM_SETEDITSTYLE;
    procedure EMGETEDITSTYLE(var Message: TMessage); message EM_GETEDITSTYLE;
    }
{$ENDIF NOT SYN_CLX}
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditMiscProcs;
{$ELSE}
  SynEditMiscProcs;
{$ENDIF}

{$IFNDEF SYN_CLX}

{ TSynMemo }

procedure TSynMemo.EMGetSel(var Message: TMessage);
var
  s, e: integer;
begin
  // EM_GETSEL
  // wParam = (WPARAM) (LPDWORD) lpdwStart;      // receives starting position
  // lParam = (LPARAM) (LPDWORD) lpdwEnd;        // receives ending position
  s := GetSelStart;
  e := GetSelEnd;
  if Message.wParam <> 0 then PDWORD(Message.wParam)^ := s;
  if Message.lParam <> 0 then PDWORD(Message.lParam)^ := e;
  Message.result := MakeLong(s, e)
end;

procedure TSynMemo.EMSetSel(var Message: TMessage);
begin
  // EM_SETSEL
  // wParam = (WPARAM) (INT) nStart;             // starting position
  // lParam = (LPARAM) (INT) nEnd;               // ending position
  SetSelStart(Message.wParam);
  SetSelEnd(Message.lParam);
end;

procedure TSynMemo.EMSetModify(var Message: TMessage);
begin
  Modified := Message.wParam <> 0;
end;

procedure TSynMemo.EMGetModify(var Message: TMessage);
begin
  Message.result := integer(Modified);
end;

procedure TSynMemo.EMGetLineCount(var Message: TMessage);
begin
  //(WPARAM) wParam,      // not used; must be zero
  //(LPARAM) lParam       // not used; must be zero
  Message.Result := Lines.Count;
end;

procedure TSynMemo.EMGetSelText(var Message: TMessage);
var
  s: string;
begin
  if Message.lParam <> 0 then begin
    s := SelText;
    StrLCopy(PChar(Message.lParam), PChar(s), length(s));
    Message.Result := length(s);
  end;
end;

procedure TSynMemo.EMReplaceSel(var Message: TMessage);
var
  StartOfBlock: TBufferCoord;
  EndOfBlock: TBufferCoord;
begin
  // EM_REPLACESEL
  // fCanUndo = (BOOL) wParam ;                  // flag that specifies whether replacement can be undone
  // lpszReplace = (LPCTSTR) lParam ;            // pointer to replacement text string
  // see PasteFromClipboard CF_TEXT - use common function ?
  // or use SetSelText/SetSelTextPrimitive (no undo)

  if ReadOnly then exit;
  DoOnPaintTransient(ttBefore);
  BeginUndoBlock;
  try
    if SelAvail and (Message.WParam <> 0){???} then begin
      UndoList.AddChange(crDelete, BlockBegin, BlockEnd, SelText, SelectionMode);
    end;
    StartOfBlock := BlockBegin;
    EndOfBlock := BlockEnd;
    BlockBegin := StartOfBlock;
    BlockEnd := EndOfBlock;
    LockUndo;
    try
      SelText := PChar(Message.lParam);
    finally
      UnlockUndo;
    end;
    if (Message.WParam <> 0){???} then begin
      UndoList.AddChange(crPaste, StartOfBlock, BlockEnd, SelText, smNormal);
    end;
  finally
    EndUndoBlock;
  end;
  EnsureCursorPosVisible;
  // Selection should have changed...
  StatusChanged([scSelection]); 

  DoOnPaintTransient(ttAfter);
end;

procedure TSynMemo.EMGetLine(var Message: TMessage);
var
  dest: PChar;
begin
  //(WPARAM) wParam,      // line number
  //(LPARAM) lParam       // line buffer (LPCTSTR)
  // other than Editcontrol with terminating #0
  if (Message.WParam >= 0) and (Message.WParam < Lines.Count) then begin
    dest := PChar(Message.LParam);
    StrLCopy(dest, PChar(Lines[Message.WParam]), PWord(Message.LParam)^);
    Message.result := StrLen(dest);
  end else begin
    Message.result := 0;
  end;
end;

procedure TSynMemo.EMCanUndo(var Message: TMessage);
begin
  //(WPARAM) wParam,    // not used; must be zero
  //(LPARAM) lParam     // not used; must be zero
  Message.Result := integer(CanUndo);
end;

procedure TSynMemo.EMUndo(var Message: TMessage);
begin
  //(WPARAM) wParam,    // not used; must be zero
  //(LPARAM) lParam     // not used; must be zero
  Message.Result := integer(CanUndo);
  Undo;
end;

procedure TSynMemo.EMGetFirstVisibleLine(var Message: TMessage);
begin
  //(WPARAM) wParam,          // not used; must be zero
  //(LPARAM) lParam           // not used; must be zero
  Message.Result := TopLine;
end;

procedure TSynMemo.EMCharFromPos(var Message: TMessage);
var
  vPos: TBufferCoord;
  i: integer;
begin
  //(WPARAM) wParam,    // not used; must be zero
  //(LPARAM) lParam     // point coordinates
  // ???
  vPos := DisplayToBufferPos(PixelsToRowColumn(Message.LParamLo, Message.LParamHi));

  Dec(vPos.Line);
  if vPos.Line >= Lines.Count then 
    vPos.Char := 1
  else if vPos.Char > Length(Lines[vPos.Line]) then
    vPos.Char := Length(Lines[vPos.Line]) +1; // ???

  i := vPos.Line;
  while i > 0 do begin
    dec(i);
    inc(vPos.Char, length(Lines[i])+2);
  end;

  //todo: this can't be right, CharIndex can easily overflow
  Message.Result := MakeLong(vPos.Char{CharIndex}, vPos.Line{Line zero based});
end;

{$ENDIF NOT SYN_CLX}

end.

