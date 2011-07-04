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
{ The Original Code is JclConsole.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Flier Lu. Portions created by Flier Lu are         }
{ Copyright (C) Flier Lu. All Rights Reserved.                                                     }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Flier Lu (flier)                                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains classes and routines to support windows Character-Mode Applications           }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclConsole;

{$I jcl.inc}
{$I windowsonly.inc}

{$HPPEMIT 'namespace JclConsole'}
(*$HPPEMIT '{'*)
{$HPPEMIT '__interface IJclScreenTextAttribute;'}
(*$HPPEMIT '}'*)
{$HPPEMIT 'using namespace JclConsole;'}
{$HPPEMIT ''}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows,
  Classes, SysUtils, Contnrs,
  JclBase;

// Console
type
  TJclScreenBuffer = class;
  TJclInputBuffer = class;

  TJclConsole = class(TObject)
  private
    FScreens: TObjectList;
    FActiveScreenIndex: Longword;
    FInput: TJclInputBuffer;
    FOnCtrlC: TNotifyEvent;
    FOnCtrlBreak: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnLogOff: TNotifyEvent;
    FOnShutdown: TNotifyEvent;
    function GetScreen(const Idx: Longword): TJclScreenBuffer;
    function GetScreenCount: Longword;
    function GetActiveScreen: TJclScreenBuffer;
    procedure SetActiveScreen(const Value: TJclScreenBuffer);
    procedure SetActiveScreenIndex(const Value: Longword);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetInputCodePage: DWORD;
    function GetOutputCodePage: DWORD;
    procedure SetInputCodePage(const Value: DWORD);
    procedure SetOutputCodePage(const Value: DWORD);
  protected
    constructor Create;
  public
    destructor Destroy; override;
    class function Default: TJclConsole;
    class procedure Shutdown;
    { TODO : Add 'Attach' and other functions for WinXP/Win.Net }
    {$IFNDEF CLR}
    class function IsConsole(const Module: HMODULE): Boolean; overload;
    class function IsConsole(const FileName: TFileName): Boolean; overload;
    {$ENDIF ~CLR}
    class function MouseButtonCount: DWORD;
    class procedure Alloc;
    class procedure Free;
    function Add(AWidth: Smallint = 0; AHeight: Smallint = 0): TJclScreenBuffer;
    function Remove(const ScrBuf: TJclScreenBuffer): Longword;
    procedure Delete(const Idx: Longword);
    property Title: string read GetTitle write SetTitle;
    property InputCodePage: DWORD read GetInputCodePage write SetInputCodePage;
    property OutputCodePage: DWORD read GetOutputCodePage write SetOutputCodePage;
    property Input: TJclInputBuffer read FInput;
    property Screens[const Idx: Longword]: TJclScreenBuffer read GetScreen;
    property ScreenCount: Longword read GetScreenCount;
    property ActiveScreenIndex: Longword read FActiveScreenIndex write SetActiveScreenIndex;
    property ActiveScreen: TJclScreenBuffer read GetActiveScreen write SetActiveScreen;
    property OnCtrlC: TNotifyEvent read FOnCtrlC write FOnCtrlC;
    property OnCtrlBreak: TNotifyEvent read FOnCtrlBreak write FOnCtrlBreak;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnLogOff: TNotifyEvent read FOnLogOff write FOnLogOff;
    property OnShutdown: TNotifyEvent read FOnShutdown write FOnShutdown;
  end;

  TJclConsoleInputMode = (imLine, imEcho, imProcessed, imWindow, imMouse);
  TJclConsoleInputModes = set of TJclConsoleInputMode;
  TJclConsoleOutputMode = (omProcessed, omWrapAtEol);
  TJclConsoleOutputModes = set of TJclConsoleOutputMode;

  IJclScreenTextAttribute = interface;
  TJclScreenFont = class;
  TJclScreenCharacter = class;
  TJclScreenCursor = class;
  TJclScreenWindow = class;

  // Console screen buffer
  TJclScreenBufferBeforeResizeEvent = procedure(Sender: TObject; const NewSize: TCoord; var CanResize: Boolean) of object;
  TJclScreenBufferAfterResizeEvent = procedure(Sender: TObject) of object;

  TJclScreenBufferTextHorizontalAlign = (thaCurrent, thaLeft, thaCenter, thaRight);
  TJclScreenBufferTextVerticalAlign = (tvaCurrent, tvaTop, tvaCenter, tvaBottom);

  TJclScreenBuffer = class(TObject)
  private
    FHandle: THandle;
    FFont: TJclScreenFont;
    FCursor: TJclScreenCursor;
    FWindow: TJclScreenWindow;
    FCharList: TObjectList;
    FOnAfterResize: TJclScreenBufferAfterResizeEvent;
    FOnBeforeResize: TJclScreenBufferBeforeResizeEvent;
    function GetInfo: TConsoleScreenBufferInfo;
    function GetSize: TCoord;
    procedure SetSize(const Value: TCoord);
    function GetHeight: Smallint;
    function GetWidth: Smallint;
    procedure SetHeight(const Value: Smallint);
    procedure SetWidth(const Value: Smallint);
    function GetMode: TJclConsoleOutputModes;
    procedure SetMode(const Value: TJclConsoleOutputModes);
  protected
    constructor Create; overload;
    constructor Create(const AHandle: THandle); overload;
    constructor Create(const AWidth, AHeight: Smallint); overload;
    procedure Init;
    procedure DoResize(const NewSize: TCoord); overload;
    procedure DoResize(const NewWidth, NewHeight: Smallint); overload;
    property Info: TConsoleScreenBufferInfo read GetInfo;
  public
    destructor Destroy; override;
    function Write(const Text: string;
      const ATextAttribute: IJclScreenTextAttribute = nil): DWORD; overload;
    function Writeln(const Text: string = '';
      const ATextAttribute: IJclScreenTextAttribute = nil): DWORD; overload;
    function Write(const Text: string; const X: Smallint; const Y: Smallint;
      const ATextAttribute: IJclScreenTextAttribute = nil): DWORD; overload;
    {$IFDEF CLR}
    function Write(const Text: string; const X: Smallint; const Y: Smallint;
      Attrs: array of Word): DWORD; overload;
    {$ELSE}
    function Write(const Text: string; const X: Smallint; const Y: Smallint;
      pAttrs: PWORD): DWORD; overload;
    {$ENDIF CLR}
    function Write(const Text: string;
      const HorizontalAlign: TJclScreenBufferTextHorizontalAlign;
      const VerticalAlign: TJclScreenBufferTextVerticalAlign = tvaCurrent;
      const ATextAttribute: IJclScreenTextAttribute = nil): DWORD; overload;
    function Read(const Count: Integer): string; overload;
    function Read(X: Smallint; Y: Smallint; const Count: Integer): string; overload;
    function Readln: string; overload;
    function Readln(X: Smallint; Y: Smallint): string; overload;
    procedure Fill(const ch: Char; const ATextAttribute: IJclScreenTextAttribute = nil);
    procedure Clear;
    property Handle: THandle read FHandle;
    property Font: TJclScreenFont read FFont;
    property Cursor: TJclScreenCursor read FCursor;
    property Window: TJclScreenWindow read FWindow;
    property Size: TCoord read GetSize write SetSize;
    property Width: Smallint read GetWidth write SetWidth;
    property Height: Smallint read GetHeight write SetHeight;
    property Mode: TJclConsoleOutputModes read GetMode write SetMode;
    property OnBeforeResize: TJclScreenBufferBeforeResizeEvent read FOnBeforeResize write FOnBeforeResize;
    property OnAfterResize: TJclScreenBufferAfterResizeEvent read FOnAfterResize write FOnAfterResize;
  end;

  // Console screen text attributes
  TJclScreenFontColor = (fclBlack, fclBlue, fclGreen, fclRed, fclCyan, fclMagenta, fclYellow, fclWhite);
  TJclScreenBackColor = (bclBlack, bclBlue, bclGreen, bclRed, bclCyan, bclMagenta, bclYellow, bclWhite);
  TJclScreenFontStyle = (fsLeadingByte, fsTrailingByte, fsGridHorizontal, fsGridLeftVertical, fsGridRightVertical, fsReverseVideo, fsUnderscore, fsSbcsDbcs);
  TJclScreenFontStyles = set of TJclScreenFontStyle;

  IJclScreenTextAttribute = interface
    ['{B880B1AC-9F1A-4F42-9D44-EA482B4F3510}']
    function GetTextAttribute: Word;
    procedure SetTextAttribute(const Value: Word);

    property TextAttribute: Word read GetTextAttribute write SetTextAttribute;

    function GetColor: TJclScreenFontColor;
    procedure SetColor(const Value: TJclScreenFontColor);
    function GetBgColor: TJclScreenBackColor;
    procedure SetBgColor(const Value: TJclScreenBackColor);
    function GetHighlight: Boolean;
    procedure SetHighlight(const Value: Boolean);
    function GetBgHighlight: Boolean;
    procedure SetBgHighlight(const Value: Boolean);
    function GetStyle: TJclScreenFontStyles;
    procedure SetStyle(const Value: TJclScreenFontStyles);

    property Color: TJclScreenFontColor read GetColor write SetColor;
    property BgColor: TJclScreenBackColor read GetBgColor write SetBgColor;
    property Highlight: Boolean read GetHighlight write SetHighlight;
    property BgHighlight: Boolean read GetBgHighlight write SetBgHighlight;
    property Style: TJclScreenFontStyles read GetStyle write SetStyle;
  end;

  TJclScreenCustomTextAttribute = class(TInterfacedObject, IJclScreenTextAttribute)
  private
    function GetBgColor: TJclScreenBackColor;
    function GetBgHighlight: Boolean;
    function GetColor: TJclScreenFontColor;
    function GetHighlight: Boolean;
    function GetStyle: TJclScreenFontStyles;
    procedure SetBgColor(const Value: TJclScreenBackColor);
    procedure SetBgHighlight(const Value: Boolean);
    procedure SetColor(const Value: TJclScreenFontColor);
    procedure SetHighlight(const Value: Boolean);
    procedure SetStyle(const Value: TJclScreenFontStyles);
  protected
    function GetTextAttribute: Word; virtual; abstract;
    procedure SetTextAttribute(const Value: Word); virtual; abstract;
  public
    constructor Create(const Attr: TJclScreenCustomTextAttribute = nil); overload;
    procedure Clear;
    property TextAttribute: Word read GetTextAttribute write SetTextAttribute;
    property Color: TJclScreenFontColor read GetColor write SetColor;
    property BgColor: TJclScreenBackColor read GetBgColor write SetBgColor;
    property Highlight: Boolean read GetHighlight write SetHighlight;
    property BgHighlight: Boolean read GetBgHighlight write SetBgHighlight;
    property Style: TJclScreenFontStyles read GetStyle write SetStyle;
  end;

  TJclScreenFont = class(TJclScreenCustomTextAttribute)
  private
    FScreenBuffer: TJclScreenBuffer;
  protected
    constructor Create(const AScrBuf: TJclScreenBuffer);
    function GetTextAttribute: Word; override;
    procedure SetTextAttribute(const Value: Word); override;
  public
    property ScreenBuffer: TJclScreenBuffer read FScreenBuffer;
  end;

  TJclScreenTextAttribute = class(TJclScreenCustomTextAttribute)
  private
    FAttribute: Word;
  protected
    function GetTextAttribute: Word; override;
    procedure SetTextAttribute(const Value: Word); override;
  public
    constructor Create(const Attribute: Word); overload;
    constructor Create(const AColor: TJclScreenFontColor = fclWhite;
      const ABgColor: TJclScreenBackColor = bclBlack;
      const AHighLight: Boolean = False;
      const ABgHighLight: Boolean = False;
      const AStyle: TJclScreenFontStyles = []); overload;
  end;

  TJclScreenCharacter = class(TJclScreenCustomTextAttribute)
  private
    FCharInfo: TCharInfo;
    function GetCharacter: Char;
    procedure SetCharacter(const Value: Char);
  protected
    constructor Create(const CharInfo: TCharInfo);
    function GetTextAttribute: Word; override;
    procedure SetTextAttribute(const Value: Word); override;
  public
    property Info: TCharInfo read FCharInfo write FCharInfo;
    property Character: Char read GetCharacter write SetCharacter;
  end;

  TJclScreenCursorSize = 1..100;

  TJclScreenCursor = class(TObject)
  private
    FScreenBuffer: TJclScreenBuffer;
    function GetInfo: TConsoleCursorInfo;
    procedure SetInfo(const Value: TConsoleCursorInfo);
    function GetPosition: TCoord;
    procedure SetPosition(const Value: TCoord);
    function GetSize: TJclScreenCursorSize;
    procedure SetSize(const Value: TJclScreenCursorSize);
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
  protected
    constructor Create(const AScrBuf: TJclScreenBuffer);
    property Info: TConsoleCursorInfo read GetInfo write SetInfo;
  public
    property ScreenBuffer: TJclScreenBuffer read FScreenBuffer;
    procedure MoveTo(const DestPos: TCoord); overload;
    procedure MoveTo(const x, y: Smallint); overload;
    procedure MoveBy(const Delta: TCoord); overload;
    procedure MoveBy(const cx, cy: Smallint); overload;
    property Position: TCoord read GetPosition write SetPosition;
    property Size: TJclScreenCursorSize read GetSize write SetSize;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  // Console screen window
  TJclScreenWindow = class(TObject)
  private
    FScreenBuffer: TJclScreenBuffer;
    function GetMaxConsoleWindowSize: TCoord;
    function GetMaxWindow: TCoord;
    function GetLeft: Smallint;
    function GetTop: Smallint;
    function GetWidth: Smallint;
    function GetHeight: Smallint;
    function GetPosition: TCoord;
    function GetSize: TCoord;
    function GetBottom: Smallint;
    function GetRight: Smallint;
    procedure SetLeft(const Value: Smallint);
    procedure SetTop(const Value: Smallint);
    procedure SetWidth(const Value: Smallint);
    procedure SetHeight(const Value: Smallint);
    procedure SetPosition(const Value: TCoord);
    procedure SetSize(const Value: TCoord);
    procedure SetBottom(const Value: Smallint);
    procedure SetRight(const Value: Smallint);
    procedure InternalSetPosition(const X, Y: SmallInt);
    procedure InternalSetSize(const X, Y: SmallInt);
  protected
    constructor Create(const AScrBuf: TJclScreenBuffer);
    procedure DoResize(const NewRect: TSmallRect; bAbsolute: Boolean = True);
  public
    procedure Scroll(const cx, cy: Smallint);
    property ScreenBuffer: TJclScreenBuffer read FScreenBuffer;
    property MaxConsoleWindowSize: TCoord read GetMaxConsoleWindowSize;
    property MaxWindow: TCoord read GetMaxWindow;
    property Position: TCoord read GetPosition write SetPosition;
    property Size: TCoord read GetSize write SetSize;
    property Left: Smallint read GetLeft write SetLeft;
    property Right: Smallint read GetRight write SetRight;
    property Top: Smallint read GetTop write SetTop;
    property Bottom: Smallint read GetBottom write SetBottom;
    property Width: Smallint read GetWidth write SetWidth;
    property Height: Smallint read GetHeight write SetHeight;
  end;

  // Console input buffer
  TJclInputCtrlEvent = ( ceCtrlC, ceCtrlBreak, ceCtrlClose, ceCtrlLogOff, ceCtrlShutdown );

  TJclInputRecordArray = array of TInputRecord;

  TJclInputBuffer = class(TObject)
  private
    FConsole: TJclConsole;
    FHandle: THandle;
    function GetMode: TJclConsoleInputModes;
    procedure SetMode(const Value: TJclConsoleInputModes);
    function GetEventCount: DWORD;
  protected
    constructor Create(const AConsole: TJclConsole);
  public
    destructor Destroy; override;
    procedure Clear;
    procedure RaiseCtrlEvent(const AEvent: TJclInputCtrlEvent; const ProcessGroupId: DWORD = 0);
    function WaitEvent(const TimeOut: DWORD = INFINITE): Boolean;
    function GetEvents(var Events: TJclInputRecordArray): DWORD; overload;
    function GetEvents(const Count: Integer): TJclInputRecordArray; overload;
    function PeekEvents(var Events: TJclInputRecordArray): DWORD; overload;
    function PeekEvents(const Count: Integer): TJclInputRecordArray; overload;
    function PutEvents(const Events: TJclInputRecordArray): DWORD; overload;
    function GetEvent: TInputRecord;
    function PeekEvent: TInputRecord;
    function PutEvent(const Event: TInputRecord): Boolean;
    property Console: TJclConsole read FConsole;
    property Handle: THandle read FHandle;
    property Mode: TJclConsoleInputModes read GetMode write SetMode;
    property EventCount: DWORD read GetEventCount;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclConsole.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF FPC}
  WinSysUt, JwaWinNT,
  {$ENDIF FPC}
  {$IFDEF CLR}
  System.Text,
  {$ENDIF CLR}
  Math, TypInfo,
  JclFileUtils, JclResources;

{$IFDEF FPC}
{$EXTERNALSYM CreateConsoleScreenBuffer}
const
  kernel32 = 'kernel32.dll';
  
function CreateConsoleScreenBuffer(dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwFlags: DWORD; lpScreenBufferData: Pointer): THandle; stdcall;
  external kernel32 name 'CreateConsoleScreenBuffer';
function SetConsoleWindowInfo(hConsoleOutput: THandle; bAbsolute: BOOL;
  const lpConsoleWindow: TSmallRect): BOOL; stdcall;
  external kernel32 name 'SetConsoleWindowInfo';
{$ENDIF FPC}

const
  COMMON_LVB_LEADING_BYTE    = $0100; // Leading Byte of DBCS
  COMMON_LVB_TRAILING_BYTE   = $0200; // Trailing Byte of DBCS
  COMMON_LVB_GRID_HORIZONTAL = $0400; // DBCS: Grid attribute: top horizontal.
  COMMON_LVB_GRID_LVERTICAL  = $0800; // DBCS: Grid attribute: left vertical.
  COMMON_LVB_GRID_RVERTICAL  = $1000; // DBCS: Grid attribute: right vertical.
  COMMON_LVB_REVERSE_VIDEO   = $4000; // DBCS: Reverse fore/back ground attribute.
  COMMON_LVB_UNDERSCORE      = $8000; // DBCS: Underscore.

  COMMON_LVB_SBCSDBCS        = $0300; // SBCS or DBCS flag.

const
  FontColorMask: Word = FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED;
  BackColorMask: Word = BACKGROUND_BLUE or BACKGROUND_GREEN or BACKGROUND_RED;
  FontStyleMask: Word = COMMON_LVB_LEADING_BYTE or COMMON_LVB_TRAILING_BYTE or
    COMMON_LVB_GRID_HORIZONTAL or COMMON_LVB_GRID_LVERTICAL or COMMON_LVB_GRID_RVERTICAL or
    COMMON_LVB_REVERSE_VIDEO or COMMON_LVB_UNDERSCORE or COMMON_LVB_SBCSDBCS;

  FontColorMapping: array [TJclScreenFontColor] of Word =
   (0,
    FOREGROUND_BLUE,
    FOREGROUND_GREEN,
    FOREGROUND_RED,
    FOREGROUND_BLUE or FOREGROUND_GREEN,
    FOREGROUND_BLUE or FOREGROUND_RED,
    FOREGROUND_GREEN or FOREGROUND_RED,
    FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED);

  BackColorMapping: array [TJclScreenBackColor] of Word =
   (0,
    BACKGROUND_BLUE,
    BACKGROUND_GREEN,
    BACKGROUND_RED,
    BACKGROUND_BLUE or BACKGROUND_GREEN,
    BACKGROUND_BLUE or BACKGROUND_RED,
    BACKGROUND_GREEN or BACKGROUND_RED,
    BACKGROUND_BLUE or BACKGROUND_GREEN or BACKGROUND_RED);

  FontStyleMapping: array [TJclScreenFontStyle] of Word =
   (COMMON_LVB_LEADING_BYTE,    // Leading Byte of DBCS
    COMMON_LVB_TRAILING_BYTE,   // Trailing Byte of DBCS
    COMMON_LVB_GRID_HORIZONTAL, // DBCS: Grid attribute: top horizontal.
    COMMON_LVB_GRID_LVERTICAL,  // DBCS: Grid attribute: left vertical.
    COMMON_LVB_GRID_RVERTICAL,  // DBCS: Grid attribute: right vertical.
    COMMON_LVB_REVERSE_VIDEO,   // DBCS: Reverse fore/back ground attribute.
    COMMON_LVB_UNDERSCORE,      // DBCS: Underscore.
    COMMON_LVB_SBCSDBCS);       // SBCS or DBCS flag.

const
  InputModeMapping: array [TJclConsoleInputMode] of DWORD =
    (ENABLE_LINE_INPUT, ENABLE_ECHO_INPUT, ENABLE_PROCESSED_INPUT,
     ENABLE_WINDOW_INPUT, ENABLE_MOUSE_INPUT);

  OutputModeMapping: array [TJclConsoleOutputMode] of DWORD =
    (ENABLE_PROCESSED_OUTPUT, ENABLE_WRAP_AT_EOL_OUTPUT);

var
  g_DefaultConsole: TJclConsole = nil;

function CtrlHandler(CtrlType: DWORD): BOOL; {$IFNDEF CLR} stdcall; {$ENDIF ~CLR}
var
  Console: TJclConsole;
begin
  try
    Console := TJclConsole.Default;
    Result := True;
    case CtrlType of
      CTRL_C_EVENT:
        if Assigned(Console.OnCtrlC) then
          Console.OnCtrlC(Console);
      CTRL_BREAK_EVENT:
        if Assigned(Console.OnCtrlBreak) then
          Console.OnCtrlBreak(Console);
      CTRL_CLOSE_EVENT:
        if Assigned(Console.OnClose) then
          Console.OnClose(Console);
      CTRL_LOGOFF_EVENT:
        if Assigned(Console.OnLogOff) then
          Console.OnLogOff(Console);
      CTRL_SHUTDOWN_EVENT:
        if Assigned(Console.OnShutdown) then
          Console.OnShutdown(Console);
    else
      // (rom) disabled. Makes function result unpredictable.
      //Assert(False, 'Unknown Ctrl Event');
      Result := False;
    end;
  except
    // (rom) dubious. An exception implies that an event has been handled.
    Result := False;
  end;
end;

//=== { TJclConsole } ========================================================

constructor TJclConsole.Create;
begin
  inherited Create;
  FScreens := TObjectList.Create;
  FInput:= TJclInputBuffer.Create(Self);
  FActiveScreenIndex := FScreens.Add(TJclScreenBuffer.Create);
  FOnCtrlC := nil;
  FOnCtrlBreak := nil;
  FOnClose := nil;
  FOnLogOff := nil;
  FOnShutdown := nil;
  SetConsoleCtrlHandler(@CtrlHandler, True);
end;

destructor TJclConsole.Destroy;
begin
  // (rom) why as first line?
  inherited Destroy;
  SetConsoleCtrlHandler(@CtrlHandler, False);
  FreeAndNil(FInput);
  FreeAndNil(FScreens);
end;

class procedure TJclConsole.Alloc;
begin
  Win32Check(AllocConsole);
end;

class procedure TJclConsole.Free;
begin
  Win32Check(FreeConsole);
end;

function TJclConsole.GetScreen(const Idx: Longword): TJclScreenBuffer;
begin
  // (rom) maybe some checks on Idx here?
  Result := TJclScreenBuffer(FScreens[Idx]);
end;

function TJclConsole.GetScreenCount: Longword;
begin
  Result := FScreens.Count;
end;

function TJclConsole.GetActiveScreen: TJclScreenBuffer;
begin
  Result := Screens[FActiveScreenIndex];
end;

procedure TJclConsole.SetActiveScreen(const Value: TJclScreenBuffer);
begin
  SetActiveScreenIndex(FScreens.IndexOf(Value));
end;

procedure TJclConsole.SetActiveScreenIndex(const Value: Longword);
begin
  if ActiveScreenIndex <> Value then
  begin
    Win32Check(SetConsoleActiveScreenBuffer(Screens[Value].Handle));
    FActiveScreenIndex := Value;
  end;
end;

class function TJclConsole.Default: TJclConsole;
begin
  if not Assigned(g_DefaultConsole) then
    g_DefaultConsole := TJclConsole.Create;
  Result := g_DefaultConsole;
end;

class procedure TJclConsole.Shutdown;
begin
  FreeAndNil(g_DefaultConsole);
end;

function TJclConsole.Add(AWidth, AHeight: Smallint): TJclScreenBuffer;
begin
  if AWidth = 0 then
    AWidth := ActiveScreen.Size.X;
  if AHeight = 0 then
    AHeight := ActiveScreen.Size.Y;
  Result := TJclScreenBuffer(FScreens[FScreens.Add(TJclScreenBuffer.Create(AWidth, AHeight))]);
end;

function TJclConsole.Remove(const ScrBuf: TJclScreenBuffer): Longword;
begin
  Result := FScreens.IndexOf(ScrBuf);
  Delete(Result);
end;

procedure TJclConsole.Delete(const Idx: Longword);
begin
  FScreens.Delete(Idx);
end;

function TJclConsole.GetTitle: string;
var
  Len: Integer;
begin
  { TODO : max 64kByte instead of max 255 }
  {$IFDEF CLR}
  { TODO : CLR TJclConsole.GetTitle }
  SetLength(Result, High(Byte));
  Len := GetConsoleTitle(Result, Length(Result));
  Win32Check((0 < Len) and (Len < Length(Result)));
  SetLength(Result, Len);
  {$ELSE}
  { TODO : max 64kByte instead of max 255 }
  SetLength(Result, High(Byte));
  Len := GetConsoleTitle(PChar(Result), Length(Result));
  Win32Check((0 < Len) and (Len < Length(Result)));
  SetLength(Result, Len);
  {$ENDIF CLR}
end;

procedure TJclConsole.SetTitle(const Value: string);
begin
  {$IFDEF CLR}
  Win32Check(SetConsoleTitle(Value));
  {$ELSE}
  Win32Check(SetConsoleTitle(PChar(Value)));
  {$ENDIF CLR}
end;

function TJclConsole.GetInputCodePage: DWORD;
begin
  Result := GetConsoleCP;
end;

procedure TJclConsole.SetInputCodePage(const Value: DWORD);
begin
  { TODO -cTest : SetConsoleCP under Win9x }
  Win32Check(SetConsoleCP(Value));
end;

function TJclConsole.GetOutputCodePage: DWORD;
begin
  Result := GetConsoleOutputCP;
end;

procedure TJclConsole.SetOutputCodePage(const Value: DWORD);
begin
  { TODO -cTest : SetConsoleOutputCP under Win9x }
  Win32Check(SetConsoleOutputCP(Value));
end;

{$IFNDEF CLR}
class function TJclConsole.IsConsole(const Module: HMODULE): Boolean;
begin
  Result := False;
  { TODO : Documentation of this solution }
  with PImageDosHeader(Module)^ do
  if e_magic = IMAGE_DOS_SIGNATURE then
    with PImageNtHeaders(Integer(Module) + {$IFDEF FPC} e_lfanew {$ELSE} _lfanew {$ENDIF})^ do
      if Signature = IMAGE_NT_SIGNATURE then
        Result := OptionalHeader.Subsystem = IMAGE_SUBSYSTEM_WINDOWS_CUI;
end;

class function TJclConsole.IsConsole(const FileName: TFileName): Boolean;
begin
  with TJclFileMappingStream.Create(FileName) do
  try
    Result := IsConsole(HMODULE(Memory));
  finally
    Free;
  end;
end;
{$ENDIF ~CLR}

class function TJclConsole.MouseButtonCount: DWORD;
begin
  Win32Check(GetNumberOfConsoleMouseButtons(Result));
end;

//=== { TJclScreenBuffer } ===================================================

constructor TJclScreenBuffer.Create;
begin
  inherited Create;
  FHandle := CreateFile('CONOUT$', GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  Win32Check(FHandle <> INVALID_HANDLE_VALUE);
  Init;
end;

constructor TJclScreenBuffer.Create(const AHandle: THandle);
begin
  inherited Create;
  FHandle := AHandle;
  Assert(FHandle <> INVALID_HANDLE_VALUE);
  Init;
end;

constructor TJclScreenBuffer.Create(const AWidth, AHeight: Smallint);
begin
  inherited Create;
  FHandle := CreateConsoleScreenBuffer(GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, CONSOLE_TEXTMODE_BUFFER, nil);
  Win32Check(FHandle <> INVALID_HANDLE_VALUE);
  Init;
  DoResize(AWidth, AHeight);
end;

destructor TJclScreenBuffer.Destroy;
begin
  // (rom) why as first line?
  inherited Destroy;
  FreeAndNil(FFont);
  FreeAndNil(FCursor);
  FreeAndNil(FWindow);
  FreeAndNil(FCharList);
  CloseHandle(FHandle);
end;

procedure TJclScreenBuffer.Init;
begin
  FCharList := TObjectList.Create;
  FOnAfterResize := nil;
  FOnBeforeResize := nil;
  FFont := TJclScreenFont.Create(Self);
  FCursor := TJclScreenCursor.Create(Self);
  FWindow := TJclScreenWindow.Create(Self);
end;

function TJclScreenBuffer.GetInfo: TConsoleScreenBufferInfo;
begin
  Win32Check(GetConsoleScreenBufferInfo(FHandle, Result));
end;

function TJclScreenBuffer.GetSize: TCoord;
begin
  Result := Info.dwSize;
end;

procedure TJclScreenBuffer.SetSize(const Value: TCoord);
begin
  DoResize(Value);
end;

function TJclScreenBuffer.GetWidth: Smallint;
begin
  Result := Size.X;
end;

procedure TJclScreenBuffer.SetWidth(const Value: Smallint);
begin
  DoResize(Value, Size.Y);
end;

function TJclScreenBuffer.GetHeight: Smallint;
begin
  Result := Size.Y;
end;

procedure TJclScreenBuffer.SetHeight(const Value: Smallint);
begin
  DoResize(Size.X, Value);
end;

procedure TJclScreenBuffer.DoResize(const NewSize: TCoord);
var
  CanResize: Boolean;
begin
  if (Size.X <> NewSize.X) or (Size.Y <> NewSize.Y) then
  begin
    if Assigned(FOnBeforeResize) then
    begin
      CanResize := True;
      FOnBeforeResize(Self, NewSize, CanResize);
      if not CanResize then
        Exit;
    end;
    Win32Check(SetConsoleScreenBufferSize(FHandle, NewSize));
    if Assigned(FOnAfterResize) then
      FOnAfterResize(Self);
  end;
end;

procedure TJclScreenBuffer.DoResize(const NewWidth, NewHeight: Smallint);
var
  NewSize: TCoord;
begin
  NewSize.X := NewWidth;
  NewSize.Y := NewHeight;
  DoResize(NewSize);
end;

function TJclScreenBuffer.GetMode: TJclConsoleOutputModes;
var
  OutputMode: DWORD;
  AMode: TJclConsoleOutputMode;
begin
  Result := [];
  Win32Check(GetConsoleMode(FHandle, OutputMode));
  for AMode := Low(TJclConsoleOutputMode) to High(TJclConsoleOutputMode) do
    if (OutputMode and OutputModeMapping[AMode]) = OutputModeMapping[AMode] then
      Include(Result, AMode);
end;

procedure TJclScreenBuffer.SetMode(const Value: TJclConsoleOutputModes);
var
  OutputMode: DWORD;
  AMode: TJclConsoleOutputMode;
begin
  OutputMode := 0;
  for AMode := Low(TJclConsoleOutputMode) to High(TJclConsoleOutputMode) do
    if AMode in Value then
      OutputMode := OutputMode or OutputModeMapping[AMode];
  Win32Check(SetConsoleMode(FHandle, OutputMode));
end;

function TJclScreenBuffer.Write(const Text: string;
  const ATextAttribute: IJclScreenTextAttribute): DWORD;
begin
  if Assigned(ATextAttribute) then
    Font.TextAttribute := ATextAttribute.TextAttribute;
  {$IFDEF CLR}
  Win32Check(WriteConsole(Handle, StringToByteArray(Text), Text.Length, Result, nil));
  {$ELSE}
  Win32Check(WriteConsole(Handle, PChar(Text), Length(Text), Result, nil));
  {$ENDIF CLR}
end;

function TJclScreenBuffer.Writeln(const Text: string;
  const ATextAttribute: IJclScreenTextAttribute): DWORD;
begin
  Result := Write(Text, ATextAttribute);
  Cursor.MoveTo(Window.Left, Cursor.Position.Y + 1);
end;

function TJclScreenBuffer.Write(const Text: string; const X, Y: Smallint;
  const ATextAttribute: IJclScreenTextAttribute): DWORD;
var
  I: Integer;
  Pos: TCoord;
  Attrs: array of Word;
begin
  if Length(Text) > 0 then
  begin
    if (X = -1) or (Y = -1) then
    begin
      Pos := Cursor.Position;
    end
    else
    begin
      Pos.X := X;
      Pos.Y := Y;
    end;

    if Assigned(ATextAttribute) then
    begin
      SetLength(Attrs, Length(Text));
      for I:=0 to Length(Text)-1 do
        Attrs[I] := ATextAttribute.TextAttribute;
      {$IFDEF CLR}
      Result := Write(Text, X, Y, Attrs);
      {$ELSE}
      Result := Write(Text, X, Y, @Attrs[0]);
      {$ENDIF CLR}
    end
    else
      {$IFDEF CLR}
      Win32Check(WriteConsoleOutputCharacter(Handle, Text, Length(Text), Pos, Result));
      {$ELSE}
      Win32Check(WriteConsoleOutputCharacter(Handle, PChar(Text), Length(Text), Pos, Result));
      {$ENDIF CLR}
  end
  else
    Result := 0;
end;

{$IFDEF CLR}
function TJclScreenBuffer.Write(const Text: string; const X, Y: Smallint;
  Attrs: array of Word): DWORD;
var
  Pos: TCoord;
begin
  if (X = -1) or (Y = -1) then
  begin
    Pos := Cursor.Position;
  end
  else
  begin
    Pos.X := X;
    Pos.Y := Y;
  end;
  if Length(Attrs) > 0 then
    Win32Check(WriteConsoleOutputAttribute(Handle, Attrs, Length(Text), Pos, Result));
  Win32Check(WriteConsoleOutputCharacter(Handle, Text, Length(Text), Pos, Result));
end;
{$ELSE}
function TJclScreenBuffer.Write(const Text: string; const X, Y: Smallint;
  pAttrs: PWORD): DWORD;
var
  Pos: TCoord;
begin
  if (X = -1) or (Y = -1) then
  begin
    Pos := Cursor.Position;
  end
  else
  begin
    Pos.X := X;
    Pos.Y := Y;
  end;
  if pAttrs <> nil then
    Win32Check(WriteConsoleOutputAttribute(Handle, pAttrs, Length(Text), Pos, Result));
  Win32Check(WriteConsoleOutputCharacter(Handle, PChar(Text), Length(Text), Pos, Result));
end;
{$ENDIF CLR}

function TJclScreenBuffer.Write(const Text: string;
  const HorizontalAlign: TJclScreenBufferTextHorizontalAlign;
  const VerticalAlign: TJclScreenBufferTextVerticalAlign;
  const ATextAttribute: IJclScreenTextAttribute): DWORD;
var
  X, Y: Smallint;
begin
  case HorizontalAlign of
    //thaCurrent: X := Cursor.Position.X;
    thaLeft:
      X := Window.Left;
    thaCenter:
      X := Window.Left + (Window.Width - Length(Text)) div 2;
    thaRight:
      X := Window.Right - Length(Text) + 1;
  else
    X := Cursor.Position.X;
  end;
  case VerticalAlign of
    //tvaCurrent: Y := Cursor.Position.Y;
    tvaTop:
      Y := Window.Top;
    tvaCenter:
      Y := Window.Top + Window.Height div 2;
    tvaBottom:
      Y := Window.Bottom;
  else
    Y := Cursor.Position.Y;
  end;
  Result := Write(Text, X, Y, ATextAttribute);
end;

function TJclScreenBuffer.Read(const Count: Integer): string;
var
  ReadCount: DWORD;
  {$IFDEF CLR}
  Data: array of Byte;
  {$ENDIF CLR}
begin
  SetLength(Result, Count);
  {$IFDEF CLR}
  SetLength(Data, Count);
  Win32Check(ReadConsole(Handle, Data, Count, ReadCount, nil));
  Result := ByteArrayToString(Data, Min(ReadCount, ByteArrayStringLen(Data)));
  {$ELSE}
  Win32Check(ReadConsole(Handle, PChar(Result), Count, ReadCount, nil));
  SetLength(Result, Min(ReadCount, StrLen(PChar(Result))));
  {$ENDIF CLR}
end;

function TJclScreenBuffer.Readln: string;
begin
  Result := Read(Window.Right - Cursor.Position.X + 1);
end;

function TJclScreenBuffer.Read(X, Y: Smallint; const Count: Integer): string;
var
  ReadPos: TCoord;
  ReadCount: DWORD;
  {$IFDEF CLR}
  sb: System.Text.StringBuilder;
  {$ENDIF CLR}
begin
  ReadPos.X := X;
  ReadPos.Y := Y;
  SetLength(Result, Count);
  {$IFDEF CLR}
  sb := System.Text.StringBuilder.Create(Count);
  Win32Check(ReadConsoleOutputCharacter(Handle, sb, Count, ReadPos, ReadCount));
  Result := sb.ToString();
  {$ELSE}
  Win32Check(ReadConsoleOutputCharacter(Handle, PChar(Result), Count, ReadPos, ReadCount));
  SetLength(Result, Min(ReadCount, StrLen(PChar(Result))));
  {$ENDIF CLR}
end;

function TJclScreenBuffer.Readln(X, Y: Smallint): string;
begin
  Result := Read(X, Y, Window.Right - X + 1);
end;

procedure TJclScreenBuffer.Fill(const ch: Char; const ATextAttribute: IJclScreenTextAttribute);
var
  WriteCount: DWORD;
begin
  Cursor.MoveTo(0, 0);
  Win32Check(FillConsoleOutputCharacter(Handle, ch, Width * Height, Cursor.Position, WriteCount));
  if Assigned(ATextAttribute) then
    Win32Check(FillConsoleOutputAttribute(Handle, ATextAttribute.TextAttribute, Width * Height, Cursor.Position, WriteCount))
  else
    Win32Check(FillConsoleOutputAttribute(Handle, Font.TextAttribute, Width * Height, Cursor.Position, WriteCount));
end;

procedure TJclScreenBuffer.Clear;
begin
  Fill(' ', TJclScreenTextAttribute.Create(fclWhite, bclBlack, False, False, []));
end;

//=== { TJclScreenCustomTextAttribute } ======================================

constructor TJclScreenCustomTextAttribute.Create(const Attr: TJclScreenCustomTextAttribute);
begin
  inherited Create;
  if Assigned(Attr) then
    SetTextAttribute(Attr.GetTextAttribute);
end;

function TJclScreenCustomTextAttribute.GetColor: TJclScreenFontColor;
var
  TA: Word;
begin
  TA := TextAttribute and FontColorMask;
  for Result := High(TJclScreenFontColor) downto Low(TJclScreenFontColor) do
    if (TA and FontColorMapping[Result]) = FontColorMapping[Result] then
      Break;
end;

function TJclScreenCustomTextAttribute.GetBgColor: TJclScreenBackColor;
var
  TA: Word;
begin
  TA := TextAttribute and BackColorMask;
  for Result := High(TJclScreenBackColor) downto Low(TJclScreenBackColor) do
    if (TA and BackColorMapping[Result]) = BackColorMapping[Result] then
      Break;
end;

function TJclScreenCustomTextAttribute.GetHighlight: Boolean;
begin
  Result := (TextAttribute and FOREGROUND_INTENSITY) = FOREGROUND_INTENSITY;
end;

function TJclScreenCustomTextAttribute.GetBgHighlight: Boolean;
begin
  Result := (TextAttribute and BACKGROUND_INTENSITY) = BACKGROUND_INTENSITY;
end;

procedure TJclScreenCustomTextAttribute.SetColor(const Value: TJclScreenFontColor);
begin
  TextAttribute := (TextAttribute and (not FontColorMask)) or FontColorMapping[Value];
end;

procedure TJclScreenCustomTextAttribute.SetBgColor(const Value: TJclScreenBackColor);
begin
  TextAttribute := (TextAttribute and (not BackColorMask)) or BackColorMapping[Value];
end;

procedure TJclScreenCustomTextAttribute.SetHighlight(const Value: Boolean);
begin
  if Value then
    TextAttribute := TextAttribute or FOREGROUND_INTENSITY
  else
    TextAttribute := TextAttribute and (not FOREGROUND_INTENSITY);
end;

procedure TJclScreenCustomTextAttribute.SetBgHighlight(const Value: Boolean);
begin
  if Value then
    TextAttribute := TextAttribute or BACKGROUND_INTENSITY
  else
    TextAttribute := TextAttribute and (not BACKGROUND_INTENSITY);
end;

function TJclScreenCustomTextAttribute.GetStyle: TJclScreenFontStyles;
var
  ta: Word;
  AStyle: TJclScreenFontStyle;
begin
  Result := [];
  ta := TextAttribute and FontStyleMask;
  for AStyle := Low(TJclScreenFontStyle) to High(TJclScreenFontStyle) do
    if (ta and FontStyleMapping[AStyle]) = FontStyleMapping[AStyle] then
      Include(Result, AStyle);
end;

procedure TJclScreenCustomTextAttribute.SetStyle(const Value: TJclScreenFontStyles);
var
  ta: Word;
  AStyle: TJclScreenFontStyle;
begin
  ta := 0;
  for AStyle := Low(TJclScreenFontStyle) to High(TJclScreenFontStyle) do
    if AStyle in Value then
      ta := ta or FontStyleMapping[AStyle];
  TextAttribute := (TextAttribute and (not FontStyleMask)) or ta;
end;

procedure TJclScreenCustomTextAttribute.Clear;
begin
  TextAttribute := FontColorMapping[fclWhite] or BackColorMapping[bclBlack];
end;

//=== { TJclScreenFont } =====================================================

constructor TJclScreenFont.Create(const AScrBuf: TJclScreenBuffer);
begin
  inherited Create;
  FScreenBuffer := AScrBuf;
end;

function TJclScreenFont.GetTextAttribute: Word;
begin
  Result := ScreenBuffer.Info.wAttributes;
end;

procedure TJclScreenFont.SetTextAttribute(const Value: Word);
begin
  Win32Check(SetConsoleTextAttribute(ScreenBuffer.Handle, Value));
end;

//=== { TJclScreenTextAttribute 0 ============================================

constructor TJclScreenTextAttribute.Create(const Attribute: Word);
begin
  inherited Create;
  FAttribute := Attribute;
end;

constructor TJclScreenTextAttribute.Create(const AColor: TJclScreenFontColor;
  const ABgColor: TJclScreenBackColor; const AHighLight, ABgHighLight: Boolean;
  const AStyle: TJclScreenFontStyles);
begin
  inherited Create;
  Color := AColor;
  BgColor := ABgColor;
  Highlight := AHighLight;
  BgHighlight := ABgHighLight;
  Style := AStyle;
end;

function TJclScreenTextAttribute.GetTextAttribute: Word;
begin
  Result := FAttribute;
end;

procedure TJclScreenTextAttribute.SetTextAttribute(const Value: Word);
begin
  FAttribute := Value;
end;

//=== { TJclScreenCharacter } ================================================

constructor TJclScreenCharacter.Create(const CharInfo: TCharInfo);
begin
  inherited Create;
  FCharInfo := CharInfo;
end;

function TJclScreenCharacter.GetCharacter: Char;
begin
  Result := FCharInfo.AsciiChar;
end;

procedure TJclScreenCharacter.SetCharacter(const Value: Char);
begin
  FCharInfo.AsciiChar := Value;
end;

function TJclScreenCharacter.GetTextAttribute: Word;
begin
  Result := FCharInfo.Attributes;
end;

procedure TJclScreenCharacter.SetTextAttribute(const Value: Word);
begin
  FCharInfo.Attributes := Value;
end;

//=== { TJclScreenCursor } ===================================================

constructor TJclScreenCursor.Create(const AScrBuf: TJclScreenBuffer);
begin
  inherited Create;
  FScreenBuffer := AScrBuf;
end;

function TJclScreenCursor.GetInfo: TConsoleCursorInfo;
begin
  Win32Check(GetConsoleCursorInfo(ScreenBuffer.Handle, Result));
end;

procedure TJclScreenCursor.SetInfo(const Value: TConsoleCursorInfo);
begin
  Win32Check(SetConsoleCursorInfo(ScreenBuffer.Handle, Value));
end;

function TJclScreenCursor.GetPosition: TCoord;
begin
  Result := ScreenBuffer.Info.dwCursorPosition;
end;

procedure TJclScreenCursor.SetPosition(const Value: TCoord);
begin
  Win32Check(SetConsoleCursorPosition(ScreenBuffer.Handle, Value));
end;

function TJclScreenCursor.GetSize: TJclScreenCursorSize;
begin
  Result := Info.dwSize;
end;

procedure TJclScreenCursor.SetSize(const Value: TJclScreenCursorSize);
var
  NewInfo: TConsoleCursorInfo;
begin
  NewInfo := Info;
  NewInfo.dwSize := Value;
  Info := NewInfo;
end;

function TJclScreenCursor.GetVisible: Boolean;
begin
  Result := Info.bVisible;
end;

procedure TJclScreenCursor.SetVisible(const Value: Boolean);
var
  NewInfo: TConsoleCursorInfo;
begin
  NewInfo := Info;
  NewInfo.bVisible := Value;
  Info := NewInfo;
end;

procedure TJclScreenCursor.MoveTo(const DestPos: TCoord);
begin
  Position := DestPos;
end;

procedure TJclScreenCursor.MoveTo(const x, y: Smallint);
var
  DestPos: TCoord;
begin
  DestPos.X := x;
  DestPos.Y := y;
  MoveTo(DestPos);
end;

procedure TJclScreenCursor.MoveBy(const Delta: TCoord);
var
  DestPos: TCoord;
begin
  DestPos := Position;
  Inc(DestPos.X, Delta.X);
  Inc(DestPos.Y, Delta.Y);
  MoveTo(DestPos);
end;

procedure TJclScreenCursor.MoveBy(const cx, cy: Smallint);
var
  DestPos: TCoord;
begin
  DestPos := Position;
  Inc(DestPos.X, cx);
  Inc(DestPos.Y, cy);
  MoveTo(DestPos);
end;

//=== { TJclScreenWindow } ===================================================

constructor TJclScreenWindow.Create(const AScrBuf: TJclScreenBuffer);
begin
  inherited Create;
  FScreenBuffer := AScrBuf;
end;

function TJclScreenWindow.GetMaxConsoleWindowSize: TCoord;
begin
  Result := GetLargestConsoleWindowSize(ScreenBuffer.Handle);
end;

function TJclScreenWindow.GetMaxWindow: TCoord;
begin
  Result := ScreenBuffer.Info.dwMaximumWindowSize;
end;

procedure TJclScreenWindow.InternalSetPosition(const X, Y: SmallInt);
var
  NewRect: TSmallRect;
begin
  if (GetLeft <> X) or (GetTop <> Y) then
  begin
    NewRect.Left := X;
    NewRect.Top := Y;
    NewRect.Right:= NewRect.Left + Width - 1;
    NewRect.Bottom := NewRect.Top + Height - 1;
    DoResize(NewRect);
  end;
end;

procedure TJclScreenWindow.InternalSetSize(const X, Y: SmallInt);
var
  NewRect: TSmallRect;
begin
  if (Width <> X) or (Height <> Y) then
  begin
    NewRect.Left := Left;
    NewRect.Top := Top;
    NewRect.Right := NewRect.Left + X - 1;
    NewRect.Bottom := NewRect.Top + Y - 1;
    DoResize(NewRect);
  end;
end;

function TJclScreenWindow.GetLeft: Smallint;
begin
  Result := ScreenBuffer.Info.srWindow.Left;
end;

function TJclScreenWindow.GetRight: Smallint;
begin
  Result := ScreenBuffer.Info.srWindow.Right;
end;

function TJclScreenWindow.GetTop: Smallint;
begin
  Result := ScreenBuffer.Info.srWindow.Top;
end;

function TJclScreenWindow.GetBottom: Smallint;
begin
  Result := ScreenBuffer.Info.srWindow.Bottom;
end;

function TJclScreenWindow.GetWidth: Smallint;
begin
  Result := ScreenBuffer.Info.srWindow.Right - ScreenBuffer.Info.srWindow.Left + 1;
end;

function TJclScreenWindow.GetHeight: Smallint;
begin
  Result := ScreenBuffer.Info.srWindow.Bottom - ScreenBuffer.Info.srWindow.Top + 1;
end;

procedure TJclScreenWindow.SetLeft(const Value: Smallint);
begin
  InternalSetPosition(Value, Top);
end;

procedure TJclScreenWindow.SetRight(const Value: Smallint);
begin
  InternalSetSize(Value - Left + 1, Height);
end;

procedure TJclScreenWindow.SetTop(const Value: Smallint);
begin
  InternalSetPosition(Left, Value);
end;

procedure TJclScreenWindow.SetBottom(const Value: Smallint);
begin
  InternalSetSize(Width, Value - Top + 1);
end;

procedure TJclScreenWindow.SetWidth(const Value: Smallint);
begin
  InternalSetSize(Value, Height);
end;

procedure TJclScreenWindow.SetHeight(const Value: Smallint);
begin
  InternalSetSize(Width, Value);
end;

function TJclScreenWindow.GetPosition: TCoord;
begin
  Result.X := Left;
  Result.Y := Top;
end;

function TJclScreenWindow.GetSize: TCoord;
begin
  Result.X := Width;
  Result.Y := Height;
end;

procedure TJclScreenWindow.SetPosition(const Value: TCoord);
begin
  InternalSetPosition(Value.X, Value.Y);
end;

procedure TJclScreenWindow.SetSize(const Value: TCoord);
begin
  InternalSetSize(Value.X, Value.Y);
end;

procedure TJclScreenWindow.DoResize(const NewRect: TSmallRect; bAbsolute: Boolean);
begin
  Win32Check(SetConsoleWindowInfo(ScreenBuffer.Handle, bAbsolute, NewRect));
end;

procedure TJclScreenWindow.Scroll(const cx, cy: Smallint);
var
  Delta: TSmallRect;
begin
  Delta.Left := cx;
  Delta.Top := cy;
  Delta.Right := cx;
  Delta.Bottom := cy;
  DoResize(Delta, False);
end;

//=== { TJclInputBuffer } ====================================================

constructor TJclInputBuffer.Create(const AConsole: TJclConsole);
begin
  inherited Create;
  FConsole := AConsole;
  FHandle := CreateFile('CONIN$', GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  Win32Check(INVALID_HANDLE_VALUE <> FHandle);
end;

destructor TJclInputBuffer.Destroy;
begin
  CloseHandle(FHandle);
  inherited Destroy;
end;

procedure TJclInputBuffer.Clear;
begin
  Win32Check(FlushConsoleInputBuffer(Handle));
end;

function TJclInputBuffer.GetMode: TJclConsoleInputModes;
var
  InputMode: DWORD;
  AMode: TJclConsoleInputMode;
begin
  Result := [];
  Win32Check(GetConsoleMode(Handle, InputMode));
  for AMode := Low(TJclConsoleInputMode) to High(TJclConsoleInputMode) do
    if (InputMode and InputModeMapping[AMode]) = InputModeMapping[AMode] then
      Include(Result, AMode);
end;

procedure TJclInputBuffer.SetMode(const Value: TJclConsoleInputModes);
var
  InputMode: DWORD;
  AMode: TJclConsoleInputMode;
begin
  InputMode := 0;
  for AMode := Low(TJclConsoleInputMode) to High(TJclConsoleInputMode) do
    if AMode in Value then
      InputMode := InputMode or InputModeMapping[AMode];
  Win32Check(SetConsoleMode(Handle, InputMode));
end;

procedure TJclInputBuffer.RaiseCtrlEvent(const AEvent: TJclInputCtrlEvent;
  const ProcessGroupId: DWORD);
const
  CtrlEventMapping: array [TJclInputCtrlEvent] of DWORD =
    (CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT, CTRL_LOGOFF_EVENT, CTRL_SHUTDOWN_EVENT);
begin
  if AEvent in [ceCtrlC, ceCtrlBreak] then
    Win32Check(GenerateConsoleCtrlEvent(CtrlEventMapping[AEvent], ProcessGroupId))
  else
    {$IFDEF CLR}
    raise EJclError.CreateFmt(RsCannotRaiseSignal,
      [GetEnumName(TypeInfo(TJclInputCtrlEvent), Integer(AEvent))]);
    {$ELSE}
    raise EJclError.CreateResFmt(@RsCannotRaiseSignal,
      [GetEnumName(TypeInfo(TJclInputCtrlEvent), Integer(AEvent))]);
    {$ENDIF CLR}
end;

function TJclInputBuffer.GetEventCount: DWORD;
begin
  Win32Check(GetNumberOfConsoleInputEvents(Handle, Result));
end;

function TJclInputBuffer.WaitEvent(const TimeOut: DWORD): Boolean;
begin
  Result := WaitForSingleObject(Handle, TimeOut) = WAIT_OBJECT_0;
end;

function TJclInputBuffer.GetEvents(var Events: TJclInputRecordArray): DWORD;
begin
  Win32Check(ReadConsoleInput(Handle, Events[0], Length(Events), Result));
end;

function TJclInputBuffer.PeekEvents(var Events: TJclInputRecordArray): DWORD;
begin
  if EventCount = 0 then
    Result := 0
  else
    Win32Check(PeekConsoleInput(Handle, Events[0], Length(Events), Result));
end;

function TJclInputBuffer.PutEvents(const Events: TJclInputRecordArray): DWORD;
begin
  Win32Check(WriteConsoleInput(Handle, Events[0], Length(Events), Result));
end;

function TJclInputBuffer.GetEvents(const Count: Integer): TJclInputRecordArray;
begin
  SetLength(Result, Count);
  SetLength(Result, GetEvents(Result));
end;

function TJclInputBuffer.PeekEvents(const Count: Integer): TJclInputRecordArray;
begin
  SetLength(Result, Count);
  SetLength(Result, PeekEvents(Result));
end;

function TJclInputBuffer.GetEvent: TInputRecord;
begin
  Result := GetEvents(1)[0];
end;

function TJclInputBuffer.PeekEvent: TInputRecord;
begin
  Result := PeekEvents(1)[0];
end;

function TJclInputBuffer.PutEvent(const Event: TInputRecord): Boolean;
var
  Evts: TJclInputRecordArray;
begin
  SetLength(Evts, 1);
  Evts[0] := Event;
  Result := PutEvents(Evts) = 1;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
