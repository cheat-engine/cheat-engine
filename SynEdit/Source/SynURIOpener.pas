{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynURIOpener.pas, released 2003-09-25.
The Initial Author of this file is Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

You may retrieve the latest version of SynEdit from the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}
{
@abstract(Plugin for SynEdit to make links (URIs) clickable)
@author(Maël Hörz)
@created(2003)
@lastmod(2003-10-08)
The SynURIOpener unit extends SynEdit to make links highlighted by SynURISyn
clickable.

http://www.mh-net.de.vu
}

{$IFNDEF QSYNURIOPENER}
unit SynURIOpener;
{$ENDIF}

{$I SynEdit.inc}
              
interface

uses
  {$IFDEF SYN_LINUX}
  Xlib,
  {$ELSE}
  Windows,
  {$ENDIF}
  {$IFDEF SYN_CLX}
  Types,
  Qt,
  QControls,
  QSynEditTypes,
  QSynEdit,
  QSynHighlighterURI,
  {$ELSE}
  Controls,
  SynEditTypes,
  SynEdit,
  SynHighlighterURI,
  {$ENDIF}
  Classes;

type
  TSynURIOpener = class(TComponent)
  private
    FControlDown: Boolean;
    FCtrlActivatesLinks: Boolean;
    FEditor: TCustomSynEdit;
    FMouseDownX: Integer;
    FMouseDownY: Integer;

    FURIHighlighter: TSynURISyn;
    FVisitedURIs: TStringList;
    {$IFDEF SYN_LINUX}
    FFtpClientCmd: string;
    FGopherClientCmd: string;
    FMailClientCmd: string;
    FNewsClientCmd: string;
    FNntpClientCmd: string;
    FProsperoClientCmd: string;
    FTelnetClientCmd: string;
    FWaisClientCmd: string;
    FWebBrowserCmd: string;
    {$ENDIF}
    procedure OpenLink(URI: string; LinkType: Integer);
    function MouseInSynEdit: Boolean;
  protected
    procedure NewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure NewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure NewMouseCursor(Sender: TObject; const aLineCharPos: TBufferCoord;
      var aCursor: TCursor);
    procedure NewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure SetEditor(const Value: TCustomSynEdit);
    procedure SetURIHighlighter(const Value: TSynURISyn);
  public
    constructor Create(AOwner: TComponent); override;
    function VisitedURI(URI: string): Boolean;
  published
    property CtrlActivatesLinks: Boolean read FCtrlActivatesLinks
      write FCtrlActivatesLinks default True;
    property Editor: TCustomSynEdit read FEditor write SetEditor;
    property URIHighlighter: TSynURISyn read FURIHighlighter 
      write SetURIHighlighter;
    {$IFDEF SYN_LINUX}
    // examples how to set WebBrowserCmd; %s is the placeholder for the URI
    // 'kfmclient openURL %s'
    // 'mozilla %s'
    // 'netscape %s'
    // 'kfmclient exec %s' similar to Windows ShellExecute
    //
    // You should let the user set these properties as there is no command
    // or environment variable valid/available on all UN*X-systems.
    // It depends on what window-manager and browser is installed.
    property FtpClientCmd: string read FFtpClientCmd write FFtpClientCmd;
    property GopherClientCmd: string read FGopherClientCmd write FGopherClientCmd;
    property MailClientCmd: string read FMailClientCmd write FMailClientCmd;
    property NewsClientCmd: string read FNewsClientCmd write FNewsClientCmd;
    property NntpClientCmd: string read FNntpClientCmd write FNntpClientCmd;
    property ProsperoClientCmd: string read FProsperoClientCmd write FProsperoClientCmd;
    property TelnetClientCmd: string read FTelnetClientCmd write FTelnetClientCmd;
    property WaisClientCmd: string read FWaisClientCmd write FWaisClientCmd;
    property WebBrowserCmd: string read FWebBrowserCmd write FWebBrowserCmd;
    {$ENDIF}
  end;


implementation

uses
  {$IFDEF SYN_LINUX}
  Libc,
  {$ELSE}
  ShellAPI,
  {$ENDIF}
  {$IFDEF SYN_CLX}
  QForms,
  QSynEditHighlighter,
  QSynEditKeyConst,
  {$ELSE}
  Forms,
  SynEditHighlighter,
  SynEditKeyConst,
  {$ENDIF}
  SysUtils;

type
  TAccessCustomSynEdit = class(TCustomSynEdit);
  TAccessSynURISyn = class(TSynURISyn);

{ TSynURIOpener }

constructor TSynURIOpener.Create(AOwner: TComponent);
begin
  inherited;
  FCtrlActivatesLinks := True;
  FVisitedURIs := TStringList.Create;
  FVisitedURIs.Sorted := True;
end;

function TSynURIOpener.MouseInSynEdit: Boolean;
var
  pt: TPoint;
begin
  {$IFDEF SYN_COMPILER_6_UP}
  pt := Mouse.CursorPos;
  {$ELSE}
  GetCursorPos(pt);
  {$ENDIF}
  Result := PtInRect(FEditor.ClientRect, FEditor.ScreenToClient(pt))
end;

procedure TSynURIOpener.NewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = SYNEDIT_CONTROL) and not FControlDown and MouseInSynEdit then
  begin
    FControlDown := True;
    TAccessCustomSynEdit(FEditor).UpdateMouseCursor;
  end;
end;

procedure TSynURIOpener.NewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = SYNEDIT_CONTROL) and FControlDown then
  begin
    FControlDown := False;
    TAccessCustomSynEdit(FEditor).UpdateMouseCursor;
  end;
end;

function IsControlPressed: Boolean;
{$IFDEF SYN_LINUX}
var
  keymap: TXQueryKeyMap;
{$ENDIF}
begin
{$IFDEF SYN_LINUX}
  XQueryKeymap(Xlib.PDisplay(QtDisplay), keymap);
  Result := (Byte(keymap[4]) and $20 = $20);
{$ELSE}
  Result := GetAsyncKeyState(VK_CONTROL) <> 0;
{$ENDIF}
end;

procedure TSynURIOpener.NewMouseCursor(Sender: TObject;
  const aLineCharPos: TBufferCoord; var aCursor: TCursor);
var
  TokenType, Start: Integer;
  Token: string;
  Attri: TSynHighlighterAttributes;
begin
  FControlDown := IsControlPressed;
  if not(FCtrlActivatesLinks and not FControlDown or
    (csDesigning in FEditor.ComponentState)) and FEditor.Focused
  then
    with FEditor do
    begin
      GetHighlighterAttriAtRowColEx(aLineCharPos, Token, TokenType, Start, Attri);
      if Assigned(URIHighlighter) and ((Attri = URIHighlighter.URIAttri) or
        (Attri = URIHighlighter.VisitedURIAttri)) and
        not((eoDragDropEditing in Options) and IsPointInSelection(aLineCharPos))
      then
        aCursor := crHandPoint
    end
end;

procedure TSynURIOpener.NewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and not(FCtrlActivatesLinks) or FControlDown then
  begin
    FMouseDownX := X;
    FMouseDownY := Y;
  end
end;

procedure TSynURIOpener.NewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ptLineCol: TBufferCoord;
  TokenType, Start: Integer;
  Token: string;
  Attri: TSynHighlighterAttributes;
begin
  if (Button <> mbLeft) or (FCtrlActivatesLinks and not FControlDown) or
    (Abs(FMouseDownX - X) > 4) or (Abs(FMouseDownY - Y) > 4) then exit;

  with TAccessCustomSynEdit(FEditor) do
  begin
    if (eoDragDropEditing in Options) and IsPointInSelection(ptLineCol) then
      exit;

    if X >= fGutterWidth then
    begin
      ptLineCol := DisplayToBufferPos(PixelsToRowColumn(X,Y));

      GetHighlighterAttriAtRowColEx(ptLineCol, Token, TokenType, Start, Attri);
      if Assigned(URIHighlighter) and ((Attri = URIHighlighter.URIAttri) or
        (Attri = URIHighlighter.VisitedURIAttri)) and
        not((eoDragDropEditing in Options) and IsPointInSelection(ptLineCol)) then
      begin
        OpenLink(Token, TokenType);
        InvalidateLine(ptLineCol.Line);
      end;
    end
  end;
end;

procedure TSynURIOpener.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and Assigned(Editor) and (AComponent = Editor) then
    Editor := nil;
  if (Operation = opRemove) and Assigned(URIHighlighter) and
    (AComponent = URIHighlighter)
  then
    URIHighlighter := nil;
end;

procedure TSynURIOpener.OpenLink(URI: string; LinkType: Integer);
{$IFDEF SYN_LINUX}
var
  CmdLine: string;
{$ENDIF}
begin
  FVisitedURIs.Add(URI);

  case TtkTokenKind(LinkType) of
    tkMailtoLink:
      if (Pos('mailto:', URI) <> 1) then URI := 'mailto:' + URI;
    tkWebLink:
       URI := 'http://' + URI;
  end;
  {$IFDEF SYN_LINUX}
  case TtkTokenKind(LinkType) of
    tkFtpLink:
      CmdLine := Format(FFtpClientCmd, [URI]);
    tkGopherLink:
      CmdLine := Format(FGopherClientCmd, [URI]);
    tkMailtoLink:
      CmdLine := Format(FMailClientCmd, [URI]);
    tkNewsLink:
      CmdLine := Format(FNewsClientCmd, [URI]);
    tkNntpLink:
      CmdLine := Format(FNntpClientCmd, [URI]);
    tkProsperoLink:
      CmdLine := Format(FProsperoClientCmd, [URI]);
    tkTelnetLink:
      CmdLine := Format(FTelnetClientCmd, [URI]);
    tkWaisLink:
      CmdLine := Format(FWaisClientCmd, [URI]);
    tkWebLink, tkHttpLink, tkHttpsLink:
      CmdLine := Format(FWebBrowserCmd, [URI]);
  end;
  Libc.system(PChar(CmdLine + ' &')); // add an ampersand to return immediately
  {$ELSE}
  ShellExecute(0, nil, PChar(URI), nil, nil, 1{SW_SHOWNORMAL});
  {$ENDIF}
end;

procedure TSynURIOpener.SetEditor(const Value: TCustomSynEdit);
begin
  if Editor <> Value then
  begin
    if not(csDesigning in ComponentState) and Assigned(FEditor) then
    begin
      with FEditor do
      begin
        RemoveKeyDownHandler(NewKeyDown);
        RemoveKeyUpHandler(NewKeyUp);
        RemoveMouseCursorHandler(NewMouseCursor);
        RemoveMouseDownHandler(NewMouseDown);
        RemoveMouseUpHandler(NewMouseUp);
      end;
    end;

    FEditor := Value;

    if not(csDesigning in ComponentState) and Assigned(FEditor) then
    begin
      with FEditor do
      begin
        AddKeyDownHandler(NewKeyDown);
        AddKeyUpHandler(NewKeyUp);
        AddMouseCursorHandler(NewMouseCursor);
        AddMouseDownHandler(NewMouseDown);
        AddMouseUpHandler(NewMouseUp);
      end;
    end;
  end;
end;

procedure TSynURIOpener.SetURIHighlighter(const Value: TSynURISyn);
begin
  if not(csDesigning in ComponentState) and Assigned(URIHighlighter) then
    TAccessSynURISyn(FURIHighlighter).SetAlreadyVisitedURIFunc(nil);

  FURIHighlighter := Value;

  if not(csDesigning in ComponentState) and  Assigned(URIHighlighter) then
    TAccessSynURISyn(FURIHighlighter).SetAlreadyVisitedURIFunc(VisitedURI);
end;

function TSynURIOpener.VisitedURI(URI: string): Boolean;
var
  Dummy: Integer;
begin
  Result := FVisitedURIs.Find(URI, Dummy);
end;

{$IFNDEF SYN_CLX}
const
  IDC_LINK = MakeIntResource(32649);

var
  CursorHandle: THandle;

initialization
  CursorHandle := LoadCursor(0, IDC_LINK);
  if CursorHandle <> 0 then
    Screen.Cursors[crHandPoint] := CursorHandle;
{$ENDIF}

end.
