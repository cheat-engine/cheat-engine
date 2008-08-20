{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynCompletionProposal.pas, released 2000-04-11.
The Original Code is based on mwCompletionProposal.pas by Cyrille de Brebisson,
part of the mwEdit component suite.
Portions created by Cyrille de Brebisson are Copyright (C) 1999
Cyrille de Brebisson. All Rights Reserved.

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

$Id: SynCompletionProposal.pas,v 1.76 2006/01/25 13:16:23 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNCOMPLETIONPROPOSAL}
unit SynCompletionProposal;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Qt,
  Types,
  QControls,
  QGraphics,
  QForms,
  QStdCtrls,
  QExtCtrls,
  QMenus,
  QImgList,
  QDialogs,
  QSynEditTypes,
  QSynEditKeyCmds,
  QSynEditHighlighter,
  QSynEditKbdHandler,
  QSynEdit,
{$ELSE}
  Windows,
  Messages,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  Menus,
  Dialogs,
  SynEditTypes,
  SynEditKeyCmds,
  SynEditHighlighter,
  SynEditKbdHandler,
  SynEdit,
{$ENDIF}
  SysUtils,
  Classes;

type
  TSynCompletionType = (ctCode, ctHint, ctParams);
  SynCompletionType = TSynCompletionType; // Keep an alias to old name for now. 

  TSynForm = {$IFDEF SYN_COMPILER_3_UP}TCustomForm{$ELSE}TForm{$ENDIF};

  TSynBaseCompletionProposalPaintItem = procedure(Sender: TObject;
    Index: Integer; TargetCanvas: TCanvas; ItemRect: TRect;
    var CustomDraw: Boolean) of object;

  TSynBaseCompletionProposalMeasureItem = procedure(Sender: TObject;
    Index: Integer; TargetCanvas: TCanvas; var ItemWidth: Integer) of object;

  TCodeCompletionEvent = procedure(Sender: TObject; var Value: string;
    Shift: TShiftState; Index: Integer; EndToken: Char) of object;

  //GBN 14/11/2001
  TAfterCodeCompletionEvent = procedure(Sender: TObject; const Value: string;
    Shift: TShiftState; Index: Integer; EndToken: Char) of object;

  TValidateEvent = procedure(Sender: TObject; Shift: TShiftState;
    EndToken: Char) of object; //GBN15/11/2001, Added EndToken

  TCompletionParameter = procedure(Sender: TObject; CurrentIndex: Integer;
    var Level, IndexToDisplay: Integer; var Key: Char;
    var DisplayString: string) of object;

  TCompletionExecute = procedure(Kind: SynCompletionType; Sender: TObject;
    var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean) of object;

  TCompletionChange = procedure(Sender: TObject; AIndex: Integer) of object;

  TSynCompletionOption = (scoAnsiStrings,           //Use Ansi comparison during string operations
                          scoCaseSensitive,         //Use case sensitivity to do matches
                          scoLimitToMatchedText,    //Limit the matched text to only what they have typed in
                          scoTitleIsCentered,       //Center the title in the box if you choose to use titles
                          scoUseInsertList,         //Use the InsertList to insert text instead of the ItemList (which will be displayed)
                          scoUsePrettyText,         //Use the PrettyText function to output the words
                          scoUseBuiltInTimer,       //Use the built in timer and the trigger keys to execute the proposal as well as the shortcut
                          scoEndCharCompletion,     //When an end char is pressed, it triggers completion to occur (like the Delphi IDE)
                          scoConsiderWordBreakChars,//Use word break characters as additional end characters
                          scoCompleteWithTab,       //Use the tab character for completion
                          scoCompleteWithEnter);    //Use the Enter character for completion

  TSynCompletionOptions = set of TSynCompletionOption;


const
  DefaultProposalOptions = [scoLimitToMatchedText, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter];
  DefaultEndOfTokenChr = '()[]. ';

type
  TProposalColumns = class;

  TSynBaseCompletionProposalForm = class(TSynForm)
  private
    FCurrentString  : string;
    FOnKeyPress     : TKeyPressEvent;
    FOnPaintItem    : TSynBaseCompletionProposalPaintItem;
    FOnMeasureItem: TSynBaseCompletionProposalMeasureItem;
    FOnChangePosition: TCompletionChange;
    FItemList       : TStrings;
    FInsertList     : TStrings;
    FAssignedList   : TStrings;
    FPosition       : Integer;
    FLinesInWindow  : Integer;
    FTitleFontHeight: Integer;
    FFontHeight     : integer;
    FScrollbar      : TScrollBar;
    FOnValidate     : TValidateEvent;
    FOnCancel       : TNotifyEvent;
    FClSelect       : TColor;
    fClSelectText   : TColor;
    FClTitleBackground: TColor;
    fClBackGround   : TColor;
    FWordBreakChars: TSynIdentChars;
    Bitmap: TBitmap; // used for drawing
    TitleBitmap: TBitmap; // used for title-drawing
    FCurrentEditor: TCustomSynEdit;
    FTitle: string;
    FTitleFont: TFont;
    FFont: TFont;
    FResizeable: Boolean;
    FItemHeight: Integer;
    FMargin: Integer;
    FEffectiveItemHeight: Integer;
    FImages: TImageList;

//These are the reflections of the Options property of the CompletionProposal
    FAnsi           : boolean;
    FCase           : boolean;
    FMatchText      : Boolean;
    FFormattedText: Boolean;
    FCenterTitle: Boolean;
    FUseInsertList: boolean;
    FCompleteWithTab: Boolean;
    FCompleteWithEnter: Boolean;

    FMouseWheelAccumulator: integer;
    FDisplayKind    : SynCompletionType;
    FParameterToken : TCompletionParameter;
    FCurrentIndex   : Integer;
    FCurrentLevel   : Integer;
    FDefaultKind    : SynCompletionType;
    FEndOfTokenChr  : String;
    FTriggerChars   : String;
    OldShowCaret    : Boolean;
    FHeightBuffer   : Integer;
    FColumns: TProposalColumns;
    procedure SetCurrentString(const Value: string);
    procedure MoveLine (cnt: Integer);
    procedure ScrollbarOnChange(Sender: TObject);
    procedure ScrollbarOnScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure ScrollbarOnEnter(Sender: TObject);

    procedure SetItemList(const Value: TStrings);
    procedure SetInsertList(const Value: TStrings);
    procedure SetPosition(const Value: Integer);
    procedure SetResizeable(const Value: Boolean);
    procedure SetItemHeight(const Value: Integer);
    procedure SetImages(const Value: TImageList);
    procedure StringListChange(Sender: TObject);
    procedure DoDoubleClick(Sender : TObject);
    procedure DoFormShow(Sender: TObject);
    procedure DoFormHide(Sender: TObject);
    procedure AdjustScrollBarPosition;
    procedure AdjustMetrics;
    procedure SetTitle(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetTitleFont(const Value: TFont);
    procedure SetColumns(Value: TProposalColumns);
    procedure TitleFontChange(Sender: TObject);
    procedure FontChange(Sender: TObject);
    procedure RecalcItemHeight;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Paint; override;
    procedure Activate; override;
    procedure Deactivate; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
{$IFDEF SYN_CLX}
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      const MousePos: TPoint): Boolean; override;
    function WidgetFlags: Integer; override;
{$ELSE}
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMActivate (var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMEraseBackgrnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE; //GBN 24/02/2002
    procedure CreateParams(var Params: TCreateParams); override;
    {$IFDEF SYN_DELPHI_4_UP}
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    {$ENDIF}
{$ENDIF}
  public
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;

    function LogicalToPhysicalIndex(Index: Integer): Integer;
    function PhysicalToLogicalIndex(Index: Integer): Integer;

    property DisplayType : SynCompletionType read FDisplayKind write FDisplayKind;
    property DefaultType : SynCompletionType read FDefaultKind write FDefaultKind default ctCode;
    property CurrentString  : string read FCurrentString write SetCurrentString;
    Property CurrentIndex   : Integer read FCurrentIndex write FCurrentIndex;
    Property CurrentLevel   : Integer read FCurrentLevel write FCurrentLevel;
    Property OnParameterToken : TCompletionParameter read FParameterToken write FParameterToken;
    property OnKeyPress     : TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnPaintItem    : TSynBaseCompletionProposalPaintItem read FOnPaintItem write FOnPaintItem;
    property OnMeasureItem: TSynBaseCompletionProposalMeasureItem read FOnMeasureItem write FOnMeasureItem;
    property OnValidate     : TValidateEvent read FOnValidate write FOnValidate;
    property OnCancel       : TNotifyEvent read FOnCancel write FOnCancel;
    property ItemList       : TStrings   read FItemList write SetItemList;
    Property InsertList     : TStrings   read FInsertList write SetInsertList;
    property AssignedList   : TStrings   read FAssignedList write FAssignedList;
    property Position       : Integer    read FPosition write SetPosition;
    property Title          : string     read fTitle write SetTitle;
    property ClSelect       : TColor     read FClSelect write FClSelect default clHighlight;
    property ClSelectedText : TColor     read FClSelectText write FClSelectText default clHighlightText;
    property ClBackground   : TColor     read FClBackGround write FClBackGround default clWindow;
    property ClTitleBackground: TColor   read FClTitleBackground write FClTitleBackground default clBtnFace;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 0;
    property Margin: Integer read FMargin write FMargin default 2;

    property UsePrettyText  : boolean    read FFormattedText write FFormattedText default False;
    property UseInsertList  : boolean    read FUseInsertList write FUseInsertList default False;
    property CenterTitle    : boolean    read FCenterTitle   write FCenterTitle   default True;
    property AnsiStrings    : boolean    read fansi write fansi default True;
    property CaseSensitive  : Boolean    read fCase write fCase default False;
    property CurrentEditor  : TCustomSynEdit read fCurrentEditor write fCurrentEditor;
    property MatchText      : Boolean    read fMatchText write fMatchText;
    property EndOfTokenChr  : String     read FEndOfTokenChr write FEndOfTokenChr;
    property TriggerChars   : String     read FTriggerChars write FTriggerChars;
    property CompleteWithTab  : Boolean    read FCompleteWithTab write FCompleteWithTab;
    property CompleteWithEnter: Boolean    read FCompleteWithEnter write FCompleteWithEnter;

    property TitleFont      : TFont      read fTitleFont write SetTitleFont;
    property Font           : TFont      read fFont      write SetFont;
    property Columns: TProposalColumns read FColumns write SetColumns;
    property Resizeable: Boolean read FResizeable write SetResizeable default True;
    property Images: TImageList read FImages write SetImages;
  end;

  TSynBaseCompletionProposal = class(TComponent)
  private
    FForm: TSynBaseCompletionProposalForm;
    FOnExecute: TCompletionExecute;
    FOnClose: TNotifyEvent;                    //GBN 28/08/2002
    FOnShow: TNotifyEvent;                     //GBN 28/08/2002
    FWidth: Integer;
    FPreviousToken: string;
    FDotOffset    : Integer;
    FOptions      : TSynCompletionOptions;
    FNbLinesInWindow: Integer;

    FCanExecute: Boolean;
    function GetClSelect: TColor;
    procedure SetClSelect(const Value: TColor);
    function GetCurrentString: string;
    function GetItemList: TStrings;
    function GetInsertList: TStrings;
    function GetOnCancel: TNotifyEvent;
    function GetOnKeyPress: TKeyPressEvent;
    function GetOnPaintItem: TSynBaseCompletionProposalPaintItem;
    function GetOnMeasureItem: TSynBaseCompletionProposalMeasureItem;
    function GetOnValidate: TValidateEvent;
    function GetPosition: Integer;
    procedure SetCurrentString(const Value: string);
    procedure SetItemList(const Value: TStrings);
    procedure SetInsertList(const Value: TStrings);
    procedure SetNbLinesInWindow(const Value: Integer);
    procedure SetOnCancel(const Value: TNotifyEvent);
    procedure SetOnKeyPress(const Value: TKeyPressEvent);
    procedure SetOnPaintItem(const Value: TSynBaseCompletionProposalPaintItem);
    procedure SetOnMeasureItem(const Value: TSynBaseCompletionProposalMeasureItem);
    procedure SetPosition(const Value: Integer);
    procedure SetOnValidate(const Value: TValidateEvent);
    procedure SetWidth(Value: Integer);
    procedure SetImages(const Value: TImageList);
    function GetDisplayKind: SynCompletionType;
    procedure SetDisplayKind(const Value: SynCompletionType);
    function GetParameterToken: TCompletionParameter;
    procedure SetParameterToken(const Value: TCompletionParameter);
    function GetDefaultKind: SynCompletionType;
    procedure SetDefaultKind(const Value: SynCompletionType);
    function GetClBack: TColor;
    procedure SetClBack(const Value: TColor);
    function GetClSelectedText: TColor;
    procedure SetClSelectedText(const Value: TColor);
    function GetEndOfTokenChar: string;
    procedure SetEndOfTokenChar(const Value: string);
    function GetClTitleBackground: TColor;
    procedure SetClTitleBackground(const Value: TColor);
    procedure SetTitle(const Value: string);
    function GetTitle: string;
    function GetFont: TFont;
    function GetTitleFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure SetTitleFont(const Value: TFont);
    function GetOptions: TSynCompletionOptions;
    function GetTriggerChars: String;
    procedure SetTriggerChars(const Value: String);
    function GetOnChange: TCompletionChange;
    procedure SetOnChange(const Value: TCompletionChange);
    procedure SetColumns(const Value: TProposalColumns);
    function GetColumns: TProposalColumns;
    function GetResizeable: Boolean;
    procedure SetResizeable(const Value: Boolean);
    function GetItemHeight: Integer;
    procedure SetItemHeight(const Value: Integer);
    function GetMargin: Integer;
    procedure SetMargin(const Value: Integer);
    function GetImages: TImageList;
  protected
    procedure SetOptions(const Value: TSynCompletionOptions); virtual;
    procedure EditorCancelMode(Sender: TObject); virtual;                       //GBN 13/11/2001
    procedure HookedEditorCommand(Sender: TObject; AfterProcessing: Boolean;
      var Handled: Boolean; var Command: TSynEditorCommand; var AChar: Char;
      Data: Pointer; HandlerData: Pointer); virtual;                            //GBN 13/11/2001
  public
    constructor Create(Aowner: TComponent); override;
    procedure Execute(s: string; x, y: integer);
    procedure ExecuteEx(s: string; x, y: integer; Kind: SynCompletionType
      {$IFDEF SYN_COMPILER_4_UP} = ctCode {$ENDIF}); virtual;
    procedure Activate;
    procedure Deactivate;

    procedure ClearList;
    function DisplayItem(AIndex : Integer): string;
    function InsertItem(AIndex : Integer): string;
    procedure AddItemAt(Where : Integer; ADisplayText, AInsertText: string);
    procedure AddItem(ADisplayText, AInsertText : string);
    procedure ResetAssignedList;

    property OnKeyPress: TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property OnValidate: TValidateEvent read GetOnValidate write SetOnValidate;
    property OnCancel: TNotifyEvent read GetOnCancel write SetOnCancel;
    property CurrentString: string read GetCurrentString write SetCurrentString;
    property DotOffset : Integer read FDotOffset write FDotOffset;
    property DisplayType : SynCompletionType read GetDisplayKind write SetDisplayKind;
    property Form: TSynBaseCompletionProposalForm read FForm;
    property PreviousToken: string read FPreviousToken;
    property Position: Integer read GetPosition write SetPosition;
  published
    property DefaultType : SynCompletionType read GetDefaultKind write SetDefaultKind default ctCode;
    property Options: TSynCompletionOptions read GetOptions write SetOptions default DefaultProposalOptions;

    property ItemList: TStrings read GetItemList write SetItemList;
    property InsertList: TStrings read GetInsertList write SetInsertList;
    property NbLinesInWindow: Integer read FNbLinesInWindow write SetNbLinesInWindow default 8;
    property ClSelect: TColor read GetClSelect write SetClSelect default clHighlight;
    property ClSelectedText: TColor read GetClSelectedText write SetClSelectedText default clHighlightText;
    property ClBackground: TColor read GetClBack write SetClBack default clWindow;
    property ClTitleBackground: TColor read GetClTitleBackground write SetClTitleBackground default clBtnFace;
    property Width: Integer read FWidth write SetWidth default 260;
    property EndOfTokenChr: string read GetEndOfTokenChar write SetEndOfTokenChar;
    property TriggerChars: String read GetTriggerChars write SetTriggerChars;
    property Title: string read GetTitle write SetTitle;
    property Font: TFont read GetFont write SetFont;
    property TitleFont: TFont read GetTitleFont write SetTitleFont;
    property Columns: TProposalColumns read GetColumns write SetColumns;
    property Resizeable: Boolean read GetResizeable write SetResizeable default True;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight default 0;
    property Images: TImageList read GetImages write SetImages default nil;
    property Margin: Integer read GetMargin write SetMargin default 2;

    property OnChange: TCompletionChange read GetOnChange write SetOnChange;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;               //GBN 28/08/2002
    property OnExecute: TCompletionExecute read FOnExecute write FOnExecute;
    property OnMeasureItem: TSynBaseCompletionProposalMeasureItem read GetOnMeasureItem write SetOnMeasureItem;
    property OnPaintItem: TSynBaseCompletionProposalPaintItem read GetOnPaintItem write SetOnPaintItem;
    property OnParameterToken: TCompletionParameter read GetParameterToken write SetParameterToken;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;                 //GBN 28/08/2002
  end;

  TSynCompletionProposal = class(TSynBaseCompletionProposal)
  private
    fEditors: TList;
    FShortCut: TShortCut;
    FNoNextKey: Boolean;
    FCompletionStart: Integer;
    FAdjustCompletionStart: Boolean;
    {$IFDEF SYN_CLX} // Missing-ShowWindow-Workaround
    FIgnoreFocusCommands: Boolean;
    {$ENDIF}
    FOnCodeCompletion: TCodeCompletionEvent;
    FTimer: TTimer;
    FTimerInterval: Integer;
    FEditor: TCustomSynEdit;
    FOnAfterCodeCompletion: TAfterCodeCompletionEvent; //GBN 18/11/2001
    FOnCancelled: TNotifyEvent; //GBN 13/11/2001
    procedure SetEditor(const Value: TCustomSynEdit);
    procedure HandleOnCancel(Sender: TObject);
    procedure HandleOnValidate(Sender: TObject; Shift: TShiftState; EndToken: Char);
    procedure HandleOnKeyPress(Sender: TObject; var Key: Char);
    procedure HandleDblClick(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorKeyPress(Sender: TObject; var Key: char);
    procedure TimerExecute(Sender: TObject);
    function GetPreviousToken(AEditor: TCustomSynEdit): string;
    function GetCurrentInput(AEditor: TCustomSynEdit): string;
    function GetTimerInterval: Integer;
    procedure SetTimerInterval(const Value: Integer);
    function GetEditor(i: Integer): TCustomSynEdit;
    procedure InternalCancelCompletion; //GBN 25/02/2002
  protected
    procedure DoExecute(AEditor: TCustomSynEdit); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetShortCut(Value: TShortCut);
    procedure SetOptions(const Value: TSynCompletionOptions); override;
    procedure EditorCancelMode(Sender: TObject); override; //GBN 13/11/2001
    procedure HookedEditorCommand(Sender: TObject; AfterProcessing: Boolean;
      var Handled: Boolean; var Command: TSynEditorCommand; var AChar: Char;
      Data: Pointer; HandlerData: Pointer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddEditor(AEditor: TCustomSynEdit);
    function RemoveEditor(AEditor: TCustomSynEdit): boolean;
    function EditorsCount: integer;
    procedure ExecuteEx(s: string; x, y: integer; Kind : SynCompletionType
      {$IFDEF SYN_COMPILER_4_UP} = ctCode {$ENDIF}); override;
    procedure ActivateCompletion; //GBN 13/11/2001
    procedure CancelCompletion; //GBN 11/11/2001
    procedure ActivateTimer(ACurrentEditor: TCustomSynEdit);
    procedure DeactivateTimer;
    property Editors[i: Integer]: TCustomSynEdit read GetEditor;
    property CompletionStart: Integer read FCompletionStart write FCompletionStart; // ET 04/02/2003
  published
    property ShortCut: TShortCut read FShortCut write SetShortCut;
    property Editor: TCustomSynEdit read FEditor write SetEditor;
    property TimerInterval: Integer read GetTimerInterval write SetTimerInterval default 1000;

    property OnAfterCodeCompletion: TAfterCodeCompletionEvent read FOnAfterCodeCompletion write FOnAfterCodeCompletion;
    property OnCancelled: TNotifyEvent read FOnCancelled write FOnCancelled;
    property OnCodeCompletion: TCodeCompletionEvent read FOnCodeCompletion write FOnCodeCompletion;
  end;

  TSynAutoComplete = class(TComponent)
  private
    FShortCut: TShortCut;
    fEditor: TCustomSynEdit;
    fAutoCompleteList: TStrings;
    fNoNextKey : Boolean;
    FEndOfTokenChr: string;
    FOnBeforeExecute: TNotifyEvent;  //GBN 2002-14-04
    FOnAfterExecute: TNotifyEvent;   //GBN 2002-14-04
    FInternalCompletion: TSynCompletionProposal;
    FDoLookup: Boolean;
    FOptions: TSynCompletionOptions;
    procedure SetAutoCompleteList(List: TStrings);
    procedure SetEditor(const Value: TCustomSynEdit);
    procedure SetDoLookup(const Value: Boolean);
    procedure CreateInternalCompletion;
    function GetOptions: TSynCompletionOptions;
    procedure SetOptions(const Value: TSynCompletionOptions);
    procedure DoInternalAutoCompletion(Sender: TObject;
      const Value: string; Shift: TShiftState; Index: Integer;
      EndToken: Char);
    function GetExecuting: Boolean;
  protected
    procedure SetShortCut(Value: TShortCut);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      virtual;
    procedure EditorKeyPress(Sender: TObject; var Key: char); virtual;
    function GetPreviousToken(Editor: TCustomSynEdit): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(token: string; Editor: TCustomSynEdit);
    procedure ExecuteEx(token: String; Editor: TCustomSynEdit; LookupIfNotExact: Boolean);
    function GetTokenList: string;
    function GetTokenValue(Token: string): string;
    procedure CancelCompletion;
    property Executing: Boolean read GetExecuting;
  published
    property AutoCompleteList: TStrings read fAutoCompleteList
      write SetAutoCompleteList;
    property EndOfTokenChr: string read FEndOfTokenChr write FEndOfTokenChr;
    property Editor: TCustomSynEdit read fEditor write SetEditor;
    property ShortCut: TShortCut read FShortCut write SetShortCut;
    property OnBeforeExecute: TNotifyEvent read FOnBeforeExecute write FOnBeforeExecute;
    property OnAfterExecute: TNotifyEvent read FOnAfterExecute write FOnAfterExecute;
    property DoLookupWhenNotExact: Boolean read FDoLookup write SetDoLookup default true;
    property Options: TSynCompletionOptions read GetOptions write SetOptions default DefaultProposalOptions;
  end;


  TProposalColumn = class(TCollectionItem)
  private
    FBiggestWord: string;
    FInternalWidth: Integer;
    FFontStyle: TFontStyles;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property BiggestWord: string read FBiggestWord write FBiggestWord;
    property DefaultFontStyle: TFontStyles read FFontStyle write FFontStyle default [];
  end;

  TProposalColumns = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TProposalColumn;
    procedure SetItem(Index: Integer; Value: TProposalColumn);
  protected
    function GetOwner: TPersistent; {$IFDEF SYN_COMPILER_3_UP} override; {$ENDIF}
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    function Add: TProposalColumn;
    {$IFDEF SYN_COMPILER_3_UP}
    function FindItemID(ID: Integer): TProposalColumn;
    {$ENDIF}
    {$IFDEF SYN_COMPILER_4_UP}
    function Insert(Index: Integer): TProposalColumn;
    {$ENDIF}
    property Items[Index: Integer]: TProposalColumn read GetItem write SetItem; default;
  end;


procedure FormattedTextOut(TargetCanvas: TCanvas; const Rect: TRect;
  const Text: string; Selected: Boolean; Columns: TProposalColumns; Images: TImageList);
function FormattedTextWidth(TargetCanvas: TCanvas; const Text: string;
  Columns: TProposalColumns; Images: TImageList): Integer;
function PrettyTextToFormattedString(const APrettyText: string;
  AlternateBoldStyle: Boolean {$IFDEF SYN_COMPILER_4_UP} = False {$ENDIF}): string;

implementation

uses
{$IFDEF SYN_COMPILER_4_UP}
  Math,
{$ENDIF}
{$IFDEF SYN_CLX}
  QSynEditTextBuffer,
  QSynEditMiscProcs,
  QSynEditKeyConst;
{$ELSE}
  SynEditTextBuffer,
  SynEditMiscProcs,
  SynEditKeyConst;
{$ENDIF}

const
  TextHeightString = 'CompletionProposal';

//------------------------- Formatted painting stuff ---------------------------

type
  TFormatCommand = (fcNoCommand, fcColor, fcStyle, fcColumn, fcHSpace, fcImage);
  TFormatCommands = set of TFormatCommand;

  PFormatChunk = ^TFormatChunk;
  TFormatChunk = record
    Str: string;
    Command: TFormatCommand;
    Data: Pointer;
  end;

  PFormatStyleData = ^TFormatStyleData;
  TFormatStyleData = record
    Style: Char;
    Action: Integer;    // -1 = Reset, +1 = Set, 0 = Toggle
  end;

  TFormatChunkList = class
  private
    FChunks: TList;
    function GetCount: Integer;
    function GetChunk(Index: Integer): PFormatChunk;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(AChunk: PFormatChunk);
    property Count: Integer read GetCount;
    property Chunks[Index: Integer]: PFormatChunk read GetChunk; default;
  end;


const
  AllCommands = [fcColor..High(TFormatCommand)];


function TFormatChunkList.GetCount: Integer;
begin
  Result := FChunks.Count;
end;

function TFormatChunkList.GetChunk(Index: Integer): PFormatChunk;
begin
  Result := FChunks[Index];
end;

procedure TFormatChunkList.Clear;
var
  C: PFormatChunk;
  StyleFormatData: PFormatStyleData;
begin
  while FChunks.Count > 0 do
  begin
    C := FChunks.Last;
    FChunks.Delete(FChunks.Count-1);

    case C^.Command of
    fcStyle:
      begin
        StyleFormatData := C^.Data;
        Dispose(StyleFormatData);
      end;
    end;

    Dispose(C);
  end;
end;

constructor TFormatChunkList.Create;
begin
  inherited Create;
  FChunks := TList.Create;
end;

destructor TFormatChunkList.Destroy;
begin
  Clear;
  FChunks.Free;
  inherited Destroy;
end;

procedure TFormatChunkList.Add(AChunk: PFormatChunk);
begin
  FChunks.Add(AChunk);
end;


function ParseFormatChunks(const FormattedString: string; ChunkList: TFormatChunkList;
  const StripCommands: TFormatCommands): Boolean;
var
  CurChar: Char;
  CurPos: Integer;
  CurrentChunk: string;
  PossibleErrorPos: Integer;
  ErrorFound: Boolean;

  procedure NextChar;
  begin
    inc(CurPos);
    {$IFOPT R+}
    // Work-around Delphi's annoying behaviour of failing the RangeCheck when
    // reading the final #0 char
    if CurPos = Length(FormattedString) +1 then
      CurChar := #0
    else
    {$ENDIF}
    CurChar := FormattedString[CurPos];
  end;

  procedure AddStringChunk;
  var
    C: PFormatChunk;
  begin
    C := New(PFormatChunk);
    C^.Str := CurrentChunk;
    C^.Command := fcNoCommand;
    C^.Data := nil;
    ChunkList.Add(C);

    CurrentChunk := '';
  end;

  procedure AddCommandChunk(ACommand: TFormatCommand; Data: Pointer);
  var
    C: PFormatChunk;
  begin
    C := New(PFormatChunk);
    C^.Str := '';
    C^.Command := ACommand;
    C^.Data := Data;
    ChunkList.Add(C);
  end;

  procedure ParseEscapeSequence;
  var
    Command: string;
    Parameter: string;
    CommandType: TFormatCommand;
    Data: Pointer;
  begin
    Assert(CurChar = '\');
    NextChar;
    if CurChar = '\' then
    begin
      CurrentChunk := CurrentChunk +'\';
      NextChar;
      exit;
    end;

    if CurrentChunk <> '' then
      AddStringChunk;

    Command := '';
    while (CurChar <> '{') and (CurPos <= Length(FormattedString)) do
    begin
      Command := Command +CurChar;
      NextChar;
    end;

    if CurChar = '{' then
    begin
      PossibleErrorPos := CurPos;
      NextChar;
      Parameter := '';
      while (CurChar <> '}') and (CurPos <= Length(FormattedString)) do
      begin
        Parameter := Parameter +CurChar;
        NextChar;
      end;

      if CurChar = '}' then
      begin
        Command := AnsiUpperCase(Command);

        Data := nil;
        CommandType := fcNoCommand;

        if Command = 'COLOR' then
        begin
          try
            Data := Pointer(StringToColor(Parameter));
            CommandType := fcColor;
          except
            CommandType := fcNoCommand;
            ErrorFound := True;
          end;
        end else
        if Command = 'COLUMN' then
        begin
          if Parameter <> '' then
          begin
            CommandType := fcNoCommand;
            ErrorFound := True;
          end else
            CommandType := fcColumn;
        end else
        if Command = 'HSPACE' then
        begin
          try
            Data := Pointer(StrToInt(Parameter));
            CommandType := fcHSpace;
          except
            CommandType := fcNoCommand;
            ErrorFound := True;
          end;
        end else
        if Command = 'IMAGE' then
        begin
          try
            Data := Pointer(StrToInt(Parameter));
            CommandType := fcImage;
          except
            CommandType := fcNoCommand;
            ErrorFound := True;
          end;
        end else
        if Command = 'STYLE' then
        begin
          if (Length(Parameter) = 2)
            and (Parameter[1] in ['+', '-', '~'])
            and (UpCase(Parameter[2]) in ['B', 'I', 'U', 'S']) then
          begin
            CommandType := fcStyle;
            if not (fcStyle in StripCommands) then
            begin
              Data := New(PFormatStyleData);
              PFormatStyleData(Data)^.Style := UpCase(Parameter[2]);
              case Parameter[1] of
              '+': PFormatStyleData(Data)^.Action := 1;
              '-': PFormatStyleData(Data)^.Action := -1;
              '~': PFormatStyleData(Data)^.Action := 0;
              end;
            end;
          end else
          begin
            CommandType := fcNoCommand;
            ErrorFound := True;
          end;
        end else
          ErrorFound := True;

        if (CommandType <> fcNoCommand) and (not (CommandType in StripCommands)) then
          AddCommandChunk(CommandType, Data);

        NextChar;
      end;
    end;
    Result := not ErrorFound;
  end;

  procedure ParseString;
  begin
    Assert(CurChar <> '\');
    while (CurChar <> '\') and (CurPos <= Length(FormattedString)) do
    begin
      CurrentChunk := CurrentChunk +CurChar;
      NextChar;
    end;
  end;

begin
  Assert(Assigned(ChunkList));

  if FormattedString = '' then
    exit;

  ErrorFound := False;
  CurrentChunk := '';
  CurPos := 1;
  CurChar := FormattedString[1];

  while CurPos <= Length(FormattedString) do
  begin
    if CurChar = '\' then
      ParseEscapeSequence
    else
      ParseString;
  end;

  if CurrentChunk <> '' then
    AddStringChunk;
end;


function StripFormatCommands(const FormattedString: string): string;
var
  Chunks: TFormatChunkList;
  i: Integer;
begin
  Chunks := TFormatChunkList.Create;
  try
    ParseFormatChunks(FormattedString, Chunks, AllCommands);

    Result := '';
    for i := 0 to Chunks.Count -1 do
      Result := Result + Chunks[i]^.Str;

  finally
    Chunks.Free;
  end;
end;


function PaintChunks(TargetCanvas: TCanvas; const Rect: TRect;
  ChunkList: TFormatChunkList; Columns: TProposalColumns; Images: TImageList;
  Invisible: Boolean): Integer;
var
  i: Integer;
  X: Integer;
  C: PFormatChunk;
  CurrentColumn: TProposalColumn;
  CurrentColumnIndex: Integer;
  LastColumnStart: Integer;
  Style: TFontStyles;
  OldFont: TFont;
begin
  OldFont := TFont.Create;
  try
    OldFont.Assign(TargetCanvas.Font);

    if Assigned(Columns) and (Columns.Count > 0) then
    begin
      CurrentColumnIndex := 0;
      CurrentColumn := TProposalColumn(Columns.Items[0]);
      TargetCanvas.Font.Style := CurrentColumn.FFontStyle;
    end else
    begin
      CurrentColumnIndex := -1;
      CurrentColumn := nil;
    end;

    LastColumnStart := Rect.Left;
    X := Rect.Left;

    TargetCanvas.Brush.Style := bsClear;

    for i := 0 to ChunkList.Count -1 do
    begin
      C := ChunkList[i];

      case C^.Command of
      fcNoCommand:
        begin
          if not Invisible then
            TargetCanvas.TextOut(X, Rect.Top, C^.Str);

          inc(X, TargetCanvas.TextWidth(C^.Str));
          if X > Rect.Right then
            break;
        end;
      fcColor:
        if not Invisible then
          TargetCanvas.Font.Color := TColor(C^.Data);
      fcStyle:
        begin
          case PFormatStyleData(C^.Data)^.Style of
          'I': Style := [fsItalic];
          'B': Style := [fsBold];
          'U': Style := [fsUnderline];
          'S': Style := [fsStrikeout];
          else Assert(False);
          end;


          case PFormatStyleData(C^.Data)^.Action of
          -1: TargetCanvas.Font.Style := TargetCanvas.Font.Style - Style;
          0: if TargetCanvas.Font.Style * Style = [] then
               TargetCanvas.Font.Style := TargetCanvas.Font.Style + Style
             else
               TargetCanvas.Font.Style := TargetCanvas.Font.Style - Style;
          1: TargetCanvas.Font.Style := TargetCanvas.Font.Style + Style;
          else Assert(False);
          end;
        end;
      fcColumn:
        if Assigned(Columns) and (Columns.Count > 0) then
        begin
          if CurrentColumnIndex <= Columns.Count -1 then
          begin
            inc(LastColumnStart, TargetCanvas.TextWidth(CurrentColumn.FBiggestWord+' '));
            X := LastColumnStart;

            inc(CurrentColumnIndex);
            if CurrentColumnIndex <= Columns.Count -1 then
            begin
              CurrentColumn := TProposalColumn(Columns.Items[CurrentColumnIndex]);
              TargetCanvas.Font.Style := CurrentColumn.FFontStyle;
            end else
              CurrentColumn := nil;
          end;
        end;
      fcHSpace:
        begin
          inc(X, Integer(C^.Data));
          if X > Rect.Right then
            break;
        end;
      fcImage:
        begin
          Assert(Assigned(Images));

          Images.Draw(TargetCanvas, X, Rect.Top, Integer(C^.Data));

          inc(X, Images.Width);
          if X > Rect.Right then
            break;
        end;
      end;
    end;

    Result := X;
    TargetCanvas.Font.Assign(OldFont);
  finally
    OldFont.Free;
    TargetCanvas.Brush.Style := bsSolid;
  end;
end;

procedure FormattedTextOut(TargetCanvas: TCanvas; const Rect: TRect;
  const Text: string; Selected: Boolean; Columns: TProposalColumns; Images: TImageList);
var
  Chunks: TFormatChunkList;
  StripCommands: TFormatCommands;
begin
  Chunks := TFormatChunkList.Create;
  try
    if Selected then
      StripCommands := [fcColor]
    else
      StripCommands := [];

    ParseFormatChunks(Text, Chunks, StripCommands);
    PaintChunks(TargetCanvas, Rect, Chunks, Columns, Images, False);
  finally
    Chunks.Free;
  end;
end;

function FormattedTextWidth(TargetCanvas: TCanvas; const Text: string;
  Columns: TProposalColumns; Images: TImageList): Integer;
var
  Chunks: TFormatChunkList;
  TmpRect: TRect;
begin
  Chunks := TFormatChunkList.Create;
  try
    TmpRect := Rect(0, 0, MaxInt, MaxInt);

    ParseFormatChunks(Text, Chunks, [fcColor]);
    Result := PaintChunks(TargetCanvas, TmpRect, Chunks, Columns, Images, True);
  finally
    Chunks.Free;
  end;
end;

function PrettyTextToFormattedString(const APrettyText: string;
  AlternateBoldStyle: Boolean {$IFDEF SYN_COMPILER_4_UP} = False {$ENDIF}): string;
var
  i: Integer;
  Color: TColor;
Begin
  Result := '';
  i := 1;
  while i <= Length(APrettyText) do
    case APrettyText[i] of
      #1, #2:
        begin
          Color := (Ord(APrettyText[i+3]) shl 8
            +Ord(APrettyText[i+2])) shl 8
            +Ord(APrettyText[i+1]);

          Result := Result+'\color{'+ColorToString(Color)+'}';

          inc(i, 4);
        end;
      #3:
        begin
          if UpCase(APrettyText[i+1]) in ['B', 'I', 'U'] then
          begin
            Result := Result+'\style{';

            case APrettyText[i+1] of
            'B': Result := Result+'+B';
            'b': Result := Result+'-B';
            'I': Result := Result+'+I';
            'i': Result := Result+'-I';
            'U': Result := Result+'+U';
            'u': Result := Result+'-U';
            end;

            Result := Result+'}';
          end;
          inc(i, 2);
        end;
      #9:
        begin
          Result := Result+'\column{}';
          if AlternateBoldStyle then
            Result := Result+'\style{~B}';
          inc(i);
        end;
      else
        Result := Result+APrettyText[i];
        inc(i);
    end;
end;


// TProposalColumn

constructor TProposalColumn.Create(Collection: TCollection);
begin
  inherited;
  FBiggestWord := 'CONSTRUCTOR';
  FInternalWidth := -1;
  FFontStyle := [];
end;

destructor TProposalColumn.Destroy;
begin
  inherited;
end;

procedure TProposalColumn.Assign(Source: TPersistent);
begin
  if Source is TProposalColumn then
  begin
    FBiggestWord := TProposalColumn(Source).FBiggestWord;
    FInternalWidth := TProposalColumn(Source).FInternalWidth;
    FFontStyle := TProposalColumn(Source).FFontStyle;
  end
  else
    inherited Assign(Source);
end;


constructor TProposalColumns.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FOwner := AOwner;
end;

function TProposalColumns.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TProposalColumns.GetItem(Index: Integer): TProposalColumn;
begin
  Result := inherited GetItem(Index) as TProposalColumn;
end;

procedure TProposalColumns.SetItem(Index: Integer; Value: TProposalColumn);
begin
  inherited SetItem(Index, Value);
end;

function TProposalColumns.Add: TProposalColumn;
begin
  Result := inherited Add as TProposalColumn;
end;


{$IFDEF SYN_COMPILER_3_UP}
function TProposalColumns.FindItemID(ID: Integer): TProposalColumn;
begin
  Result := inherited FindItemID(ID) as TProposalColumn;
end;
{$ENDIF}

{$IFDEF SYN_COMPILER_4_UP}
function TProposalColumns.Insert(Index: Integer): TProposalColumn;
begin
  Result := inherited Insert(Index) as TProposalColumn;
end;
{$ENDIF}



//============================================================================


//GBN 10/11/2001
//Moved from completion component
function FormatParamList(const S: String; CurrentIndex: Integer): string;
var
  i: Integer;
  List: TStrings;
begin
  Result := '';
  List := TStringList.Create;
  try
    List.CommaText := S;
    for i := 0 to List.Count - 1 do
    begin
      if i = CurrentIndex then
        Result := Result + '\style{~B}' + List[i] + '\style{~B}'
      else
        Result := Result + List[i];

      if i < List.Count - 1 then
//        Result := Result + ', ';
        Result := Result + ' ';
    end;
  finally
    List.Free;
  end;
end;
// End GBN 10/11/2001

{ TSynBaseCompletionProposalForm }

constructor TSynBaseCompletionProposalForm.Create(AOwner: TComponent);
begin
  FResizeable := True;
{$IFDEF SYN_CPPB_1}
  CreateNew(AOwner, 0);
{$ELSE}
  CreateNew(AOwner);
{$ENDIF}
  Bitmap := TBitmap.Create;
  TitleBitmap := TBitmap.Create;
  FItemList := TStringList.Create;
  FInsertList := TStringList.Create;
  FAssignedList := TStringList.Create;
  FMatchText := False;
{$IFDEF SYN_CLX}
  BorderStyle := fbsNone;
{$ELSE}
  BorderStyle := bsNone;
{$ENDIF}
  FScrollbar := TScrollBar.Create(Self);
  FScrollbar.Kind := sbVertical;
{$IFNDEF SYN_CLX}
  FScrollbar.ParentCtl3D := False;
{$ENDIF}
  FScrollbar.OnChange := ScrollbarOnChange;
  FScrollbar.OnScroll := ScrollbarOnScroll;
  FScrollbar.OnEnter := ScrollbarOnEnter;
  FScrollbar.Parent := Self;
  Visible := False;

  FTitleFont := TFont.Create;
  FTitleFont.Name := 'MS Sans Serif';
  FTitleFont.Size := 8;
  FTitleFont.Style := [fsBold];
  FTitleFont.Color := clBtnText;

  FFont := TFont.Create;
  FFont.Name := 'MS Sans Serif';
  FFont.Size := 8;

  ClSelect := clHighlight;
  ClSelectedText := clHighlightText;
  ClBackground := clWindow;
  ClTitleBackground := clBtnFace;


  (FItemList as TStringList).OnChange := StringListChange;  // Really necessary? It seems to work
  FTitle := '';                                             // fine without it
  FUseInsertList := False;
  FFormattedText := False;
  FCenterTitle := True;
  FAnsi := True;
  FCase := False;

  FColumns := TProposalColumns.Create(AOwner, TProposalColumn);

  FItemHeight := 0;
  FMargin := 2;
  FEffectiveItemHeight := 0;
  RecalcItemHeight;

  Canvas.Font.Assign(FTitleFont);
  FTitleFontHeight := Canvas.TextHeight(TextHeightString);
  FHeightBuffer := 0;

  FTitleFont.OnChange := TitleFontChange;
  FFont.OnChange := FontChange;

  OnDblClick := DoDoubleClick;
  OnShow := DoFormShow;
  OnHide := DoFormHide;
end;

{$IFDEF SYN_CLX}

function TSynBaseCompletionProposalForm.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; const MousePos: TPoint): Boolean;
const
  WHEEL_DIVISOR = 120; { according to Qt API... }
var
  iWheelClicks: integer;
  iLinesToScroll: integer;
begin
  if ssCtrl in Application.KeyState then
    iLinesToScroll := FLinesInWindow 
  else
    iLinesToScroll := 3;
  Inc( fMouseWheelAccumulator, WheelDelta );
  iWheelClicks := fMouseWheelAccumulator div WHEEL_DIVISOR;
  fMouseWheelAccumulator := fMouseWheelAccumulator mod WHEEL_DIVISOR;
  Position := Position - iWheelClicks * iLinesToScroll;
  Update;
  Result := True;
end;

function TSynBaseCompletionProposalForm.WidgetFlags: Integer;
begin
  Result := Integer(WidgetFlags_WType_Popup);
end;

{$ELSE SYN_CLX}

procedure TSynBaseCompletionProposalForm.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $20000;
{$IFNDEF SYN_COMPILER_3_UP}
var
  VersionInfo: TOSVersionInfo;
{$ENDIF}
begin
  inherited;
  with Params do
  begin
    Style := WS_POPUP;
    ExStyle := WS_EX_TOOLWINDOW;

    {$IFDEF SYN_COMPILER_3_UP}
    if ((Win32Platform and VER_PLATFORM_WIN32_NT) <> 0)
      and (Win32MajorVersion > 4)
      and (Win32MinorVersion > 0) {Windows XP} then
    {$ELSE}
    VersionInfo.dwOSVersionInfoSize := sizeof(TOSVersionInfo);
    if GetVersionEx(VersionInfo)
      and ((VersionInfo.dwPlatformId and VER_PLATFORM_WIN32_NT) <> 0)
      and (VersionInfo.dwMajorVersion > 4)
      and (VersionInfo.dwMinorVersion > 0) {Windows XP} then
    {$ENDIF}
      Params.WindowClass.style := Params.WindowClass.style or CS_DROPSHADOW;

    if DisplayType = ctCode then
      if FResizeable then
        Style := Style or WS_THICKFRAME
      else
        Style := Style or WS_DLGFRAME;
  end;
end;
{$ENDIF}

procedure TSynBaseCompletionProposalForm.Activate;
begin
  Visible := True;
  if DisplayType = ctCode then
    (CurrentEditor as TCustomSynEdit).AddFocusControl(Self);
end;

procedure TSynBaseCompletionProposalForm.Deactivate;
begin
  if (DisplayType = ctCode) then
    (CurrentEditor as TCustomSynEdit).RemoveFocusControl(Self);
  Visible := False;
end;

destructor TSynBaseCompletionProposalForm.Destroy;
begin
  inherited Destroy;
  FColumns.Free;
  Bitmap.Free;
  TitleBitmap.Free;
  FItemList.Free;
  FInsertList.Free;
  FAssignedList.Free;
  FTitleFont.Free;
  FFont.Free;
end;

procedure TSynBaseCompletionProposalForm.KeyDown(var Key: Word; Shift: TShiftState);
var
  C: Char;
begin
  if DisplayType = ctCode then
  begin
    case Key of
      SYNEDIT_RETURN:
        if (FCompleteWithEnter) and Assigned(OnValidate) then
          OnValidate(Self, Shift, #0); //GBN 15/11/2001
      SYNEDIT_TAB:
        if  (FCompleteWithTab) and Assigned(OnValidate) then
          OnValidate(Self, Shift, #0); 
      SYNEDIT_ESCAPE:
      begin
        if Assigned(OnCancel) then
          OnCancel(Self);
      end;
      SYNEDIT_LEFT:
        begin
          if Length(FCurrentString) > 0 then
          begin
            CurrentString := Copy(CurrentString, 1, Length(CurrentString) - 1);
            if Assigned(CurrentEditor) then
              (CurrentEditor as TCustomSynEdit).CommandProcessor(ecLeft, #0, nil);
          end else
          begin
            //Since we have control, we need to re-send the key to
            //the editor so that the cursor behaves properly
            if Assigned(CurrentEditor) then
              (CurrentEditor as TCustomSynEdit).CommandProcessor(ecLeft, #0, nil);

            if Assigned(OnCancel) then
              OnCancel(Self);
          end;
        end;
      SYNEDIT_RIGHT:
        begin
          if Assigned(CurrentEditor) then
            with CurrentEditor as TCustomSynEdit do
            begin
              if CaretX <= Length(LineText) then
                C := LineText[CaretX]
              else
                C := #32;

              if (C = #9) or (C = #32) or (C in FWordBreakChars) then
                if Assigned(OnCancel) then
                  OnCancel(Self)
                else
              else
                CurrentString := CurrentString + C;

              CommandProcessor(ecRight, #0, nil);
            end;
        end;
      SYNEDIT_PRIOR:
        MoveLine(FLinesInWindow * -1);
      SYNEDIT_NEXT:
        MoveLine(FLinesInWindow);
      SYNEDIT_END:
        Position := FAssignedList.Count - 1;
      SYNEDIT_HOME:
        Position := 0;
      SYNEDIT_UP:
        if ssCtrl in Shift then
          Position := 0
        else
          MoveLine(-1);
      SYNEDIT_DOWN:
        if ssCtrl in Shift then
          Position := FAssignedList.Count - 1
        else
          MoveLine(1);
      SYNEDIT_BACK:
        if (Shift = []) then
        begin
          if Length(FCurrentString) > 0 then
          begin
            CurrentString := Copy(CurrentString, 1, Length(CurrentString) - 1);

            if Assigned(CurrentEditor) then
              (CurrentEditor as TCustomSynEdit).CommandProcessor(ecDeleteLastChar, #0, nil);
          end else
          begin
            //Since we have control, we need to re-send the key to
            //the editor so that the cursor behaves properly
            if Assigned(CurrentEditor) then
              (CurrentEditor as TCustomSynEdit).CommandProcessor(ecDeleteLastChar, #0, nil);

            if Assigned(OnCancel) then
              OnCancel(Self);
          end;
        end;
      SYNEDIT_DELETE: if Assigned(CurrentEditor) then
                      (CurrentEditor as TCustomSynEdit).CommandProcessor(ecDeleteChar, #0, nil);
    end;
  end;
  Invalidate;
end;

procedure TSynBaseCompletionProposalForm.KeyPress(var Key: char);
begin
  if DisplayType = ctCode then
  begin
    case Key of
      #13, #27:; // These keys are already handled by KeyDown
      #32..'z':
        begin
          if (Key in FWordBreakChars) and Assigned(OnValidate) then
          begin
            if Key = #32 then
              OnValidate(Self, [], #0)
            else OnValidate(Self, [], Key);
          end;

          CurrentString := CurrentString +Key;

          if Assigned(OnKeyPress) then
            OnKeyPress(Self, Key);
        end;
      #8:
        if Assigned(OnKeyPress) then
          OnKeyPress(Self, Key);
      else
        with CurrentEditor as TCustomSynEdit do
          CommandProcessor(ecChar, Key, nil);

        if Assigned(OnCancel) then
          OnCancel(Self);
    end;
  end;
  Invalidate;
end;

procedure TSynBaseCompletionProposalForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  y := (y - fHeightBuffer) div FEffectiveItemHeight;
  Position := FScrollbar.Position + y;
//  (CurrentEditor as TCustomSynEdit).UpdateCaret;
end;

{$IFNDEF SYN_CLX}
{$IFDEF SYN_DELPHI_4_UP}
function TSynBaseCompletionProposalForm.CanResize(var NewWidth, NewHeight: Integer): Boolean;
var
  NewLinesInWindow: Integer;
  BorderWidth: Integer;
begin
  Result := True;
  case FDisplayKind of
  ctCode:
    begin
      BorderWidth := 2 * GetSystemMetrics(SM_CYSIZEFRAME);

      if FEffectiveItemHeight <> 0 then
      begin
        NewLinesInWindow := (NewHeight-FHeightBuffer) div FEffectiveItemHeight;
        if NewLinesInWindow < 1 then
          NewLinesInWindow := 1;
      end else
        NewLinesInWindow := 0;

      FLinesInWindow := NewLinesInWindow;

      NewHeight := FEffectiveItemHeight * FLinesInWindow + FHeightBuffer + BorderWidth;

      if (NewWidth-BorderWidth) < FScrollbar.Width then
        NewWidth := FScrollbar.Width+BorderWidth;
    end;
  ctHint:;
  ctParams:;
  end;
end;
{$ENDIF}
{$ENDIF}

procedure TSynBaseCompletionProposalForm.Resize;
begin
  inherited;

  if FEffectiveItemHeight <> 0 then
    FLinesInWindow := (Height - FHeightBuffer) div FEffectiveItemHeight;

  if not( csCreating in ControlState ) then
    AdjustMetrics;

  AdjustScrollBarPosition;
  Invalidate;
end;


procedure TSynBaseCompletionProposalForm.Paint;

  procedure ResetCanvas;
  begin
    with Bitmap.Canvas do
    begin
      Pen.Color := FClBackGround;
      Brush.Color := FClBackGround;
      Font.Assign(FFont);
    end;
  end;

const
  TitleMargin = 2;
var
  TmpRect: TRect;
  TmpX: Integer;
  AlreadyDrawn: boolean;
  TmpString: string;
  i: Integer;
begin
  if FDisplayKind = ctCode then
  begin
    with Bitmap do
    begin
      ResetCanvas;
      Canvas.Pen.Color := clBtnFace;
      Canvas.Rectangle(0, 0, ClientWidth - FScrollbar.Width, ClientHeight);
      for i := 0 to Min(FLinesInWindow - 1, FAssignedList.Count - 1) do
      begin
        if i + FScrollbar.Position = Position then
        begin
          Canvas.Brush.Color := FClSelect;
          Canvas.Pen.Color := FClSelect;
          Canvas.Rectangle(0, FEffectiveItemHeight * i, ClientWidth - FScrollbar.Width,
            FEffectiveItemHeight * (i+1));
          Canvas.Pen.Color := fClSelectText;
          Canvas.Font.Assign(FFont);
          Canvas.Font.Color := FClSelectText;
        end;

        AlreadyDrawn := False;

        if Assigned(OnPaintItem) then
          OnPaintItem(Self, LogicalToPhysicalIndex(FScrollBar.Position+i),
            Canvas, Rect(0, FEffectiveItemHeight*i, ClientWidth - FScrollbar.Width,
            FEffectiveItemHeight * (i+1)), AlreadyDrawn);

        if AlreadyDrawn then
          ResetCanvas
        else
        begin
          if FFormattedText then
          begin
            FormattedTextOut(Canvas, Rect(FMargin,
              FEffectiveItemHeight*i +((FEffectiveItemHeight -FFontHeight) div 2),
              Bitmap.Width, FEffectiveItemHeight*(i+1)),
              FAssignedList[FScrollbar.Position+i],
              (i+FScrollbar.Position = Position), FColumns, FImages);
          end else
          begin
            Canvas.TextOut(FMargin, FEffectiveItemHeight* i,
              FAssignedList[FScrollbar.Position+i]);
          end;

          if i + FScrollbar.Position = Position then
            ResetCanvas;
        end;
      end;
    end;
    Canvas.Draw(0, FHeightBuffer, Bitmap);

    if FTitle <> '' then
    begin
      with TitleBitmap do
      begin
        Canvas.Brush.Color := FClTitleBackground;
        TmpRect := Rect(0, 0, ClientWidth+1, FHeightBuffer);                        //GBN
        Canvas.FillRect(TmpRect);
        Canvas.Pen.Color := clBtnShadow;
        dec(TmpRect.Bottom, 1);
        Canvas.PenPos := TmpRect.BottomRight;
        Canvas.LineTo(TmpRect.Left-1,TmpRect.Bottom);
        Canvas.Pen.Color := clBtnFace;

        Canvas.Font.Assign(FTitleFont);

        if CenterTitle then
        begin
          TmpX := (Width - Canvas.TextWidth(Title)) div 2;
          if TmpX < TitleMargin then
            TmpX := TitleMargin;  //We still want to be able to read it, even if it does go over the edge
        end else
        begin
          TmpX := TitleMargin;
        end;
        Canvas.TextRect(TmpRect, TmpX, TitleMargin-1, FTitle); // -1 because TmpRect.Top is already 1
      end;
      Canvas.Draw(0, 0, TitleBitmap);
    end;
  end else
  if (FDisplayKind = ctHint) or (FDisplayKind = ctParams) then
  begin
    with Bitmap do
    begin
      ResetCanvas;
      tmpRect := Rect(0, 0, ClientWidth, ClientHeight);
      Canvas.FillRect(tmpRect);
      Frame3D(Canvas, tmpRect, cl3DLight, cl3DDkShadow, 1);

      //GBN 10/11/2001
      for i := 0 to FAssignedList.Count - 1 do
      begin
        AlreadyDrawn := False;
        if Assigned(OnPaintItem) then
          OnPaintItem(Self, i, Canvas, Rect(0, FEffectiveItemHeight* i +FMargin,
            ClientWidth, FEffectiveItemHeight* (i+1)+FMargin), AlreadyDrawn);

        if AlreadyDrawn then
          ResetCanvas
        else
        begin
          if FDisplayKind = ctParams then
            TmpString := FormatParamList(FAssignedList[i], CurrentIndex)
          else
            TmpString := FAssignedList[i];

          FormattedTextOut(Canvas, Rect(FMargin+1,
            FEffectiveItemHeight*i+((FEffectiveItemHeight-FFontHeight) div 2)+FMargin,
            Bitmap.Width-1, FEffectiveItemHeight*(i+1)+FMargin), TmpString,
            False, nil, FImages);
        end;
      end;
      //End GBN 10/11/2001
    end;
    Canvas.Draw(0, 0, Bitmap);
  end;
end;

procedure TSynBaseCompletionProposalForm.ScrollbarOnChange(Sender: TObject);
begin
  if Position < FScrollbar.Position then
    Position := FScrollbar.Position
  else
    if Position > FScrollbar.Position + FLinesInWindow - 1 then
      Position := FScrollbar.Position + FLinesInWindow - 1
    else
      Repaint;
end;

procedure TSynBaseCompletionProposalForm.ScrollbarOnScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin  
  with CurrentEditor as TCustomSynEdit do
  begin
    SetFocus;
    //This tricks the caret into showing itself again.
    AlwaysShowCaret := False;
    AlwaysShowCaret := True;
//    UpdateCaret;
  end;
end;

procedure TSynBaseCompletionProposalForm.ScrollbarOnEnter(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TSynBaseCompletionProposalForm.MoveLine(cnt: Integer);
begin
  if (cnt > 0) then begin
    if (Position < (FAssignedList.Count - cnt)) then
      Position := Position + cnt
    else
      Position := FAssignedList.Count - 1;
  end else begin
    if (Position + cnt) > 0 then
      Position := Position + cnt
    else
      Position := 0;
  end;
end;

function TSynBaseCompletionProposalForm.LogicalToPhysicalIndex(Index: Integer): Integer;
begin
  if FMatchText and (Index >= 0) and (Index < FAssignedList.Count) then
    Result := Integer(FAssignedList.Objects[Index])
  else
    Result := Index;
end;

function TSynBaseCompletionProposalForm.PhysicalToLogicalIndex(Index: Integer): Integer;
var i : Integer;
begin
  if FMatchText then
  begin
    Result := -1;
    for i := 0 to FAssignedList.Count - 1 do
      if Integer(FAssignedList.Objects[i]) = Index then
      begin
        Result := i;
        break;
      end;
  end else
    Result := Index;
end;

procedure TSynBaseCompletionProposalForm.SetCurrentString(const Value: string);

  function MatchItem(AIndex: Integer; UseItemList: Boolean): Boolean;
  var
    CompareString: String;
  begin
{    if UseInsertList then
      CompareString := FInsertList[AIndex]
    else
    begin
      CompareString := FItemList[AIndex];

      if UsePrettyText then
        CompareString := StripFormatCommands(CompareString);
    end;}

    if UseInsertList then
      CompareString := FInsertList[aIndex]
    else
    begin
      if (FMatchText) and (not UseItemList) then
        CompareString := FAssignedList[aIndex]
      else
        CompareString := FItemList[aIndex];   //GBN 29/08/2002 Fix for when match text is not active

      if UsePrettyText then
        CompareString := StripFormatCommands(CompareString);
    end;


    CompareString := Copy(CompareString, 1, Length(Value));

    if FAnsi then
    begin
      if FCase then
        Result := AnsiCompareStr(CompareString, Value) = 0
      else
        Result := AnsiCompareText(CompareString, Value) = 0;
    end else
    begin
      if FCase then
        Result := CompareStr(CompareString, Value) = 0
      else
        Result := CompareText(CompareString, Value) = 0;
    end;
  end;

  procedure RecalcList;
  var
    i: Integer;
  begin
    FAssignedList.Clear;
    for i := 0 to FItemList.Count -1 do
    begin
      if MatchItem(i, True) then
        FAssignedList.AddObject(FItemList[i], TObject(i));
    end;
  end;

var
  i: Integer;
begin
  FCurrentString := Value;
  if DisplayType <> ctCode then
    exit;
  if FMatchText then
  begin
    RecalcList;
    AdjustScrollBarPosition;
    Position := 0;
    
    if Visible and Assigned(FOnChangePosition) and (DisplayType = ctCode) then
      FOnChangePosition(Owner as TSynBaseCompletionProposal,
        LogicalToPhysicalIndex(FPosition));
        
    Repaint;
  end
  else
  begin
    i := 0;
    while (i < ItemList.Count) and (not MatchItem(i, True)) do
      inc(i);

    if i < ItemList.Count then
      Position := i
    else
      Position := 0;
  end;
end;

procedure TSynBaseCompletionProposalForm.SetItemList(const Value: TStrings);
begin
  FItemList.Assign(Value);
  FAssignedList.Assign(Value);
  CurrentString := CurrentString;
end;

procedure TSynBaseCompletionProposalForm.SetInsertList(const Value: TStrings);
begin
  FInsertList.Assign(Value);
end;

procedure TSynBaseCompletionProposalForm.DoDoubleClick(Sender: TObject);
begin
//we need to do the same as the enter key;
  if DisplayType = ctCode then
    if Assigned(OnValidate) then OnValidate(Self, [], #0);                      //GBN 15/11/2001
end;

procedure TSynBaseCompletionProposalForm.SetPosition(const Value: Integer);
begin
  if ((Value <= 0) and (FPosition = 0)) or (FPosition = Value) then
    exit;

  if Value <= FAssignedList.Count - 1 then
  begin
    FPosition := Value;
    if Position < FScrollbar.Position then
      FScrollbar.Position := Position else
    if FScrollbar.Position < (Position - FLinesInWindow + 1) then
      FScrollbar.Position := Position - FLinesInWindow + 1;

    if Visible and Assigned(FOnChangePosition) and (DisplayType = ctCode) then
      FOnChangePosition(Owner as TSynBaseCompletionProposal,
        LogicalToPhysicalIndex(FPosition));

    Repaint;
  end;
end;

procedure TSynBaseCompletionProposalForm.SetResizeable(const Value: Boolean);
begin
  FResizeable := Value;
  {$IFDEF SYN_CLX}
  {$ELSE}
  RecreateWnd;
  {$ENDIF}
end;

procedure TSynBaseCompletionProposalForm.SetItemHeight(const Value: Integer);
begin
  if Value <> FItemHeight then
  begin
    FItemHeight := Value;
    RecalcItemHeight;
  end;
end;

procedure TSynBaseCompletionProposalForm.SetImages(const Value: TImageList);
begin
  if FImages <> Value then
  begin
    {$IFDEF SYN_COMPILER_5_UP}
    if Assigned(FImages) then
      FImages.RemoveFreeNotification(Self);
    {$ENDIF SYN_COMPILER_5_UP}

    FImages := Value;
    if Assigned(FImages) then
      FImages.FreeNotification(Self);
  end;
end;


procedure TSynBaseCompletionProposalForm.RecalcItemHeight;
begin
  Canvas.Font.Assign(FFont);
  FFontHeight := Canvas.TextHeight(TextHeightString);
  if FItemHeight > 0 then
    FEffectiveItemHeight := FItemHeight
  else
  begin
    FEffectiveItemHeight := FFontHeight;
  end;
end;

procedure TSynBaseCompletionProposalForm.StringListChange(Sender: TObject);
begin
  FScrollbar.Position := Position;
end;

{$IFDEF SYN_CLX}
{$ELSE}
procedure TSynBaseCompletionProposalForm.WMMouseWheel(var Msg: TMessage);
var
  nDelta: integer;
  nWheelClicks: integer;
{$IFNDEF SYN_COMPILER_4_UP}
const
  LinesToScroll = 3;
  WHEEL_DELTA = 120;
  WHEEL_PAGESCROLL = MAXDWORD;
  {$IFNDEF SYN_COMPILER_3_UP}
  SPI_GETWHEELSCROLLLINES = 104;
  {$ENDIF}
{$ENDIF}
begin
  if csDesigning in ComponentState then exit;

{$IFDEF SYN_COMPILER_4_UP}
  if GetKeyState(VK_CONTROL) >= 0 then nDelta := Mouse.WheelScrollLines
{$ELSE}
  if GetKeyState(VK_CONTROL) >= 0 then
    {$IFDEF SYN_CLX}
    nDelta := LinesToScroll
    {$ELSE}
    SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @nDelta, 0)
    {$ENDIF}
{$ENDIF}
    else nDelta := FLinesInWindow;

  Inc(fMouseWheelAccumulator, SmallInt(Msg.wParamHi));
  nWheelClicks := fMouseWheelAccumulator div WHEEL_DELTA;
  fMouseWheelAccumulator := fMouseWheelAccumulator mod WHEEL_DELTA;
  if (nDelta = integer(WHEEL_PAGESCROLL)) or (nDelta > FLinesInWindow) then
    nDelta := FLinesInWindow;

  Position := Position - (nDelta * nWheelClicks);
//  (CurrentEditor as TCustomSynEdit).UpdateCaret;
end;
{$ENDIF}

function GetMDIParent (const Form: TSynForm): TSynForm;
{ Returns the parent of the specified MDI child form. But, if Form isn't a
  MDI child, it simply returns Form. }
var
  I, J: Integer;
begin
  Result := Form;
  if Form = nil then
    exit;
  if (Form is TSynForm) and
     ((Form as TForm).FormStyle = fsMDIChild) then
    for I := 0 to Screen.FormCount-1 do
      with Screen.Forms[I] do
      begin
        if FormStyle <> fsMDIForm then Continue;
        for J := 0 to MDIChildCount-1 do
          if MDIChildren[J] = Form then
          begin
            Result := Screen.Forms[I];
            exit;
          end;
      end;
end;

{$IFDEF SYN_CLX}
{$ELSE}
procedure TSynBaseCompletionProposalForm.WMActivate(var Message: TWMActivate);
var
  ParentForm: TSynForm;
begin
  if csDesigning in ComponentState then begin
    inherited;
    Exit;
  end;
     {Owner of the component that created me}
  if Owner.Owner is TSynForm then
    ParentForm := GetMDIParent(Owner.Owner as TSynForm)
  else
    ParentForm := nil;

  if Assigned(ParentForm) and ParentForm.HandleAllocated then
    SendMessage(ParentForm.Handle, WM_NCACTIVATE, Ord(Message.Active <> WA_INACTIVE), 0);
end;
{$ENDIF}

procedure TSynBaseCompletionProposalForm.DoFormHide(Sender: TObject);
begin
  if CurrentEditor <> nil then
  begin
    (CurrentEditor as TCustomSynEdit).AlwaysShowCaret := OldShowCaret;
//    (CurrentEditor as TCustomSynEdit).UpdateCaret;
    if DisplayType = ctCode then
    begin
      (Owner as TSynBaseCompletionProposal).FWidth := Width;
      (Owner as TSynBaseCompletionProposal).FNbLinesInWindow := FLinesInWindow;
    end;
  end;
  //GBN 28/08/2002
  if Assigned((Owner as TSynBaseCompletionProposal).OnClose) then
    TSynBaseCompletionProposal(Owner).OnClose(Self);
end;

procedure TSynBaseCompletionProposalForm.DoFormShow(Sender: TObject);
begin
  if Assigned(CurrentEditor) then
  begin
    with CurrentEditor as TCustomSynEdit do
    begin
      OldShowCaret := AlwaysShowCaret;
      AlwaysShowCaret := Focused;
//      UpdateCaret;
    end;
  end;
  //GBN 28/08/2002
  if Assigned((Owner as TSynBaseCompletionProposal).OnShow) then
    (Owner as TSynBaseCompletionProposal).OnShow(Self);
end;

{$IFDEF SYN_CLX}
{$ELSE}
procedure TSynBaseCompletionProposalForm.WMEraseBackgrnd(
  var Message: TMessage);
begin
  Message.Result:=1;
end;

//GBN 24/02/2002
procedure TSynBaseCompletionProposalForm.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTTAB;
end;
{$ENDIF}

procedure TSynBaseCompletionProposalForm.AdjustMetrics;
begin
  if DisplayType = ctCode then
  begin
    if FTitle <> '' then
      FHeightBuffer := FTitleFontHeight + 4 {Margin}
    else
      FHeightBuffer := 0;

    if (ClientWidth >= FScrollbar.Width) and (ClientHeight >= FHeightBuffer) then
    begin
      Bitmap.Width := ClientWidth - FScrollbar.Width;
      Bitmap.Height := ClientHeight - FHeightBuffer;
    end;

    if (ClientWidth > 0) and (FHeightBuffer > 0) then
    begin
      TitleBitmap.Width := ClientWidth;
      TitleBitmap.Height := FHeightBuffer;
    end;
  end else
  begin
    if (ClientWidth > 0) and (ClientHeight > 0) then
    begin
      Bitmap.Width := ClientWidth;
      Bitmap.Height := ClientHeight;
    end;
  end;
end;


procedure TSynBaseCompletionProposalForm.AdjustScrollBarPosition;
begin
  if FDisplayKind = ctCode then
  begin
    if Assigned(FScrollbar) then
    begin
      FScrollbar.Top := FHeightBuffer;
      FScrollbar.Height := ClientHeight - FHeightBuffer;
      FScrollbar.Left := ClientWidth - FScrollbar.Width;

      if FAssignedList.Count - FLinesInWindow < 0 then
      begin
        {$IFNDEF SYN_CLX}
        {$IFDEF SYN_DELPHI_4_UP}
        FScrollbar.PageSize := 0;
        {$ENDIF}
        {$ENDIF}
        FScrollbar.Max := 0;
        FScrollbar.Enabled := False;
      end else
      begin
        {$IFNDEF SYN_CLX}
        {$IFDEF SYN_DELPHI_4_UP}
        FScrollbar.PageSize := 0;
        {$ENDIF}
        {$ENDIF}
        FScrollbar.Max := FAssignedList.Count - FLinesInWindow;
        if FScrollbar.Max <> 0 then
        begin
          FScrollbar.LargeChange := FLinesInWindow;
          {$IFNDEF SYN_CLX}
          {$IFDEF SYN_DELPHI_4_UP}
          FScrollbar.PageSize := 1;
          {$ENDIF}
          {$ENDIF}
          FScrollbar.Enabled := True;
        end else
          FScrollbar.Enabled := False;
      end;
    end;
  end;
end;

procedure TSynBaseCompletionProposalForm.SetTitle(const Value: string);
begin
  FTitle := Value;
  AdjustMetrics;
end;

procedure TSynBaseCompletionProposalForm.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  RecalcItemHeight;
  AdjustMetrics;
end;

procedure TSynBaseCompletionProposalForm.SetTitleFont(const Value: TFont);
begin
  FTitleFont.Assign(Value);
  FTitleFontHeight := Canvas.TextHeight(TextHeightString);
  AdjustMetrics;
end;

procedure TSynBaseCompletionProposalForm.SetColumns(Value: TProposalColumns);
begin
  FColumns.Assign(Value);
end;


procedure TSynBaseCompletionProposalForm.TitleFontChange(Sender: TObject);
begin
  Canvas.Font.Assign(FTitleFont);
  FTitleFontHeight := Canvas.TextHeight(TextHeightString);
  AdjustMetrics;
end;

procedure TSynBaseCompletionProposalForm.FontChange(Sender: TObject);
begin
  RecalcItemHeight;
  AdjustMetrics;
end;

procedure TSynBaseCompletionProposalForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) then
  begin
    if AComponent = FImages then
      Images := nil;
  end;

  inherited Notification(AComponent, Operation);
end;


{ TSynBaseCompletionProposal }

constructor TSynBaseCompletionProposal.Create(Aowner: TComponent);
begin
  FWidth := 260;
  FNbLinesInWindow := 8;
  inherited Create(AOwner);
  FForm := TSynBaseCompletionProposalForm.Create(Self);
  EndOfTokenChr := DefaultEndOfTokenChr;
  FDotOffset := 0;
  DefaultType := ctCode;
end;

procedure TSynBaseCompletionProposal.Execute(s: string; x, y: integer);
begin
  ExecuteEx(s, x, y, DefaultType);
end;

procedure TSynBaseCompletionProposal.ExecuteEx(s: string; x, y: integer; Kind : SynCompletionType);

  function GetWorkAreaWidth: Integer;
  begin
  {$IFDEF SYN_CLX}
    Result := Screen.Width
  {$ELSE}
    {$IFDEF SYN_COMPILER_5_UP}
    Result := Screen.DesktopWidth;
    {$ELSE}
    Result := Screen.Width;
    {$ENDIF}
  {$ENDIF}
  end;

  function GetWorkAreaHeight: Integer;
  begin
  {$IFDEF SYN_CLX}
    Result := Screen.Height
  {$ELSE}
    {$IFDEF SYN_COMPILER_5_UP}
    Result := Screen.DesktopHeight;
    {$ELSE}
    Result := Screen.Height;
    {$ENDIF}
  {$ENDIF}
  end;

  function GetParamWidth(const S: string): Integer;
  var
    i: Integer;
    List: TStringList;
    NewWidth: Integer;
  begin
    List := TStringList.Create;
    try
      List.CommaText := S;

      Result := 0;
      for i := -1 to List.Count -1 do
      begin
        NewWidth := FormattedTextWidth(Form.Canvas,
          FormatParamList(S, i), Columns, FForm.Images);

        if NewWidth > Result then
          Result := NewWidth;
      end;
    finally
      List.Free;
    end;
  end;

  procedure RecalcFormPlacement;
  var
    i: Integer;
    tmpWidth: Integer;
    tmpHeight: Integer;
    tmpX: Integer;
    tmpY: Integer;
    tmpStr: string;
    BorderWidth: Integer;
    NewWidth: Integer;
  begin

    tmpX := x;
    tmpY := y;
    tmpWidth := 0;
    tmpHeight := 0;
    case Kind of
    ctCode:
      begin
        BorderWidth :=
          {$IFDEF SYN_CLX}
          6; // I don't know how to retrieve the border width in CLX
          {$ELSE}
          2 * GetSystemMetrics(SM_CYSIZEFRAME);
          {$ENDIF}

        tmpWidth := FWidth;
        tmpHeight := Form.FHeightBuffer + Form.FEffectiveItemHeight * FNbLinesInWindow + BorderWidth;
      end;
    ctHint:
      begin
        BorderWidth := 2;
        tmpHeight := Form.FEffectiveItemHeight * ItemList.Count + BorderWidth
          + 2 * Form.Margin;

        Form.Canvas.Font.Assign(Font);
        for i := 0 to ItemList.Count -1 do
        begin
          tmpStr := ItemList[i];
          NewWidth := FormattedTextWidth(Form.Canvas, tmpStr, nil, FForm.Images);
          if NewWidth > tmpWidth then
            tmpWidth := NewWidth;
        end;

        inc(tmpWidth, 2 * FForm.Margin +BorderWidth);
      end;
    ctParams:
      begin
        BorderWidth := 2;
        tmpHeight := Form.FEffectiveItemHeight * ItemList.Count + BorderWidth
          + 2 * Form.Margin;

        Form.Canvas.Font.Assign(Font);
        for i := 0 to ItemList.Count -1 do
        begin
          NewWidth := GetParamWidth(StripFormatCommands(ItemList[i]));

          if Assigned(Form.OnMeasureItem) then
            Form.OnMeasureItem(Self, i, Form.Canvas, NewWidth);

          if NewWidth > tmpWidth then
            tmpWidth := NewWidth;
        end;

        inc(tmpWidth, 2 * FForm.Margin +BorderWidth);
      end;
    end;


    if tmpX + tmpWidth > GetWorkAreaWidth then
    begin
      tmpX := GetWorkAreaWidth - tmpWidth - 5;  //small space buffer
      if tmpX < 0 then
        tmpX := 0;
    end;

    if tmpY + tmpHeight > GetWorkAreaHeight then
    begin
      tmpY := tmpY - tmpHeight - (Form.CurrentEditor  as TCustomSynEdit).LineHeight -2;
      if tmpY < 0 then
        tmpY := 0;
    end;

    Form.Width := tmpWidth;
    Form.Height := tmpHeight;
    Form.Top := tmpY;
    Form.Left := tmpX;
  end;

var
  TmpOffset: Integer;
begin
  DisplayType := Kind;

  FCanExecute := True;
  if Assigned(OnExecute) then
    OnExecute(Kind, Self, s, x, y, FCanExecute);

  if (not FCanExecute) or (ItemList.Count = 0) then
  begin
    if Form.Visible and (Kind = ctParams) then
      Form.Visible := False;
    exit;
  end;

  Form.FormStyle := fsStayOnTop;

  if Assigned(Form.CurrentEditor) then
  begin
    TmpOffset := (Form.CurrentEditor as TCustomSynEdit).Canvas.TextWidth(Copy(s, 1, DotOffset));
    if DotOffset > 1 then
      TmpOffset := TmpOffset + (3 * (DotOffset -1))
  end else
    TmpOffset := 0;
  x := x - tmpOffset;

  ResetAssignedList;

  case Kind of
  ctCode:
    begin
      CurrentString := s;

      Form.FScrollbar.Visible := True;

      RecalcFormPlacement;

      //This may seem redundant, but it fixes scrolling bugs for the first time
      //up when MatchText is not true.  That is the only time these occur
      if not(scoLimitToMatchedText in Options) then
      begin
        Form.AdjustScrollBarPosition;
        Form.FScrollbar.Position := Form.Position;
      end;
      if Form.AssignedList.Count > 0 then
        Form.Show
    end;
  ctParams, ctHint:
    begin
      Form.FScrollbar.Visible := False;

      RecalcFormPlacement;

      {$IFNDEF SYN_CLX}
//      ShowWindow(Form.Handle, SW_SHOWNOACTIVATE);
      ShowWindow(Form.Handle, SW_SHOWNA);
      Form.Visible := True;
      {$ELSE}
      Form.Show;
      (Form.CurrentEditor as TCustomSynEdit).SetFocus;
      {$ENDIF}
      Form.Repaint;
    end;
  end;
end;

function TSynBaseCompletionProposal.GetCurrentString: string;
begin
  Result := Form.CurrentString;
end;

function TSynBaseCompletionProposal.GetItemList: TStrings;
begin
  Result := Form.ItemList;
end;

function TSynBaseCompletionProposal.GetInsertList: TStrings;
begin
  Result := Form.InsertList;
end;

function TSynBaseCompletionProposal.GetOnCancel: TNotifyEvent;
begin
  Result := Form.OnCancel;
end;

function TSynBaseCompletionProposal.GetOnKeyPress: TKeyPressEvent;
begin
  Result := Form.OnKeyPress;
end;

function TSynBaseCompletionProposal.GetOnPaintItem: TSynBaseCompletionProposalPaintItem;
begin
  Result := Form.OnPaintItem;
end;

function TSynBaseCompletionProposal.GetOnMeasureItem: TSynBaseCompletionProposalMeasureItem;
begin
  Result := Form.OnMeasureItem;
end;

function TSynBaseCompletionProposal.GetOnValidate: TValidateEvent;
begin
  Result := Form.OnValidate;
end;

function TSynBaseCompletionProposal.GetPosition: Integer;
begin
  Result := Form.Position;
end;

procedure TSynBaseCompletionProposal.SetCurrentString(const Value: string);
begin
  Form.CurrentString := Value;
end;

procedure TSynBaseCompletionProposal.SetItemList(const Value: TStrings);
begin
  Form.ItemList := Value;
end;

procedure TSynBaseCompletionProposal.SetInsertList(const Value: TStrings);
begin
  Form.InsertList := Value;
end;

procedure TSynBaseCompletionProposal.SetNbLinesInWindow(const Value: Integer);
begin
  FNbLinesInWindow := Value;
end;

procedure TSynBaseCompletionProposal.SetOnCancel(const Value: TNotifyEvent);
begin
  Form.OnCancel := Value;
end;

procedure TSynBaseCompletionProposal.SetOnKeyPress(const Value: TKeyPressEvent);
begin
  Form.OnKeyPress := Value;
end;

procedure TSynBaseCompletionProposal.SetOnPaintItem(const Value:
  TSynBaseCompletionProposalPaintItem);
begin
  Form.OnPaintItem := Value;
end;

procedure TSynBaseCompletionProposal.SetOnMeasureItem(const Value:
  TSynBaseCompletionProposalMeasureItem);
begin
  Form.OnMeasureItem := Value;
end;


procedure TSynBaseCompletionProposal.SetPosition(const Value: Integer);
begin
  form.Position := Value;
end;

procedure TSynBaseCompletionProposal.SetOnValidate(const Value: TValidateEvent);
begin
  form.OnValidate := Value;
end;

function TSynBaseCompletionProposal.GetClSelect: TColor;
begin
  Result := Form.ClSelect;
end;

procedure TSynBaseCompletionProposal.SetClSelect(const Value: TColor);
begin
  Form.ClSelect := Value;
end;

procedure TSynBaseCompletionProposal.SetWidth(Value: Integer);
begin
  FWidth := Value;
end;

procedure TSynBaseCompletionProposal.Activate;
begin
  if Assigned(Form) then
    Form.Activate;
end;

procedure TSynBaseCompletionProposal.Deactivate;
begin
  if Assigned(Form) then
    Form.Deactivate;
end;

function TSynBaseCompletionProposal.GetClBack: TColor;
begin
  Result := Form.ClBackground;
end;

procedure TSynBaseCompletionProposal.SetClBack(const Value: TColor);
begin
  Form.ClBackground := Value
end;

function TSynBaseCompletionProposal.GetClSelectedText: TColor;
begin
  Result := Form.ClSelectedText;
end;

procedure TSynBaseCompletionProposal.SetClSelectedText(const Value: TColor);
begin
  Form.ClSelectedText := Value;
end;

procedure TSynBaseCompletionProposal.AddItem(ADisplayText, AInsertText: String);
begin
  GetInsertList.Add(AInsertText);
  GetItemList.Add(ADisplayText);
end;

procedure TSynBaseCompletionProposal.AddItemAt(Where: Integer; ADisplayText, AInsertText: String);
begin
  try
    GetInsertList.Insert(Where, AInsertText);
    GetItemList.Insert(Where, ADisplayText);                 
  except
    raise Exception.Create('Cannot insert item at position ' + IntToStr(Where) + '.');
  end;
end;

procedure TSynBaseCompletionProposal.ClearList;
begin
  GetInsertList.Clear;
  GetItemList.Clear;
end;

function TSynBaseCompletionProposal.DisplayItem(AIndex : Integer): String;
begin
  Result := GetItemList[AIndex];
end;

function TSynBaseCompletionProposal.InsertItem(AIndex : Integer): String;
begin
  Result := GetInsertList[AIndex];
end;

function TSynBaseCompletionProposal.GetDisplayKind: SynCompletionType;
begin
  Result := Form.DisplayType;
end;

procedure TSynBaseCompletionProposal.SetDisplayKind(const Value: SynCompletionType);
begin
  Form.DisplayType := Value;
end;

function TSynBaseCompletionProposal.GetParameterToken: TCompletionParameter;
begin
  Result := Form.OnParameterToken;
end;

procedure TSynBaseCompletionProposal.SetParameterToken(
  const Value: TCompletionParameter);
begin
  Form.OnParameterToken := Value;
end;

procedure TSynBaseCompletionProposal.SetColumns(const Value: TProposalColumns);
begin
  FForm.Columns := Value;
end;

function TSynBaseCompletionProposal.GetColumns: TProposalColumns;
begin
  Result := FForm.Columns;
end;

function TSynBaseCompletionProposal.GetResizeable: Boolean;
begin
  Result := FForm.Resizeable;
end;

procedure TSynBaseCompletionProposal.SetResizeable(const Value: Boolean);
begin
  if FForm.Resizeable <> Value then
    FForm.Resizeable := Value;
end;

function TSynBaseCompletionProposal.GetItemHeight: Integer;
begin
  Result := FForm.ItemHeight;
end;

procedure TSynBaseCompletionProposal.SetItemHeight(const Value: Integer);
begin
  if FForm.ItemHeight <> Value then
    FForm.ItemHeight := Value;
end;

procedure TSynBaseCompletionProposal.SetImages(const Value: TImageList);
begin
  FForm.Images := Value;
end;

function TSynBaseCompletionProposal.GetImages: TImageList;
begin
  Result := FForm.Images;
end;

function TSynBaseCompletionProposal.GetMargin: Integer;
begin
  Result := FForm.Margin;
end;

procedure TSynBaseCompletionProposal.SetMargin(const Value: Integer);
begin
  if Value <> FForm.Margin then
    FForm.Margin := Value;
end;

function TSynBaseCompletionProposal.GetDefaultKind: SynCompletionType;
begin
  Result := Form.DefaultType;
end;

procedure TSynBaseCompletionProposal.SetDefaultKind(const Value: SynCompletionType);
begin
  Form.DefaultType := Value;
  Form.DisplayType := Value;
  {$IFDEF SYN_CLX}
  {$ELSE}
  Form.RecreateWnd;
  {$ENDIF}
end;

procedure TSynBaseCompletionProposal.SetEndOfTokenChar(
  const Value: string);
begin
  if Form.FEndOfTokenChr <> Value then
  begin
    Form.FEndOfTokenChr := Value;
  end;
end;

function TSynBaseCompletionProposal.GetClTitleBackground: TColor;
begin
  Result := Form.ClTitleBackground;
end;

procedure TSynBaseCompletionProposal.SetClTitleBackground(
  const Value: TColor);
begin
  Form.ClTitleBackground := Value;
end;

function TSynBaseCompletionProposal.GetTitle: string;
begin
  Result := Form.Title;
end;

procedure TSynBaseCompletionProposal.SetTitle(const Value: string);
begin
  Form.Title := Value;
end;

function TSynBaseCompletionProposal.GetFont: TFont;
begin
  Result := Form.Font;
end;

function TSynBaseCompletionProposal.GetTitleFont: TFont;
begin
  Result := Form.TitleFont;
end;

procedure TSynBaseCompletionProposal.SetFont(const Value: TFont);
begin
  Form.Font := Value;
end;

procedure TSynBaseCompletionProposal.SetTitleFont(const Value: TFont);
begin
  Form.TitleFont := Value;
end;

function TSynBaseCompletionProposal.GetEndOfTokenChar: string;
begin
  Result := Form.EndOfTokenChr;
end;

function TSynBaseCompletionProposal.GetOptions: TSynCompletionOptions;
begin
  Result := fOptions;
end;

procedure TSynBaseCompletionProposal.SetOptions(
  const Value: TSynCompletionOptions);
begin
  if fOptions <> Value then
  begin
    fOptions := Value;
    Form.CenterTitle := scoTitleIsCentered in Value;
    Form.AnsiStrings := scoAnsiStrings in Value;
    Form.CaseSensitive := scoCaseSensitive in Value;
    Form.UsePrettyText := scoUsePrettyText in Value;
    Form.UseInsertList := scoUseInsertList in Value;
    Form.MatchText := scoLimitToMatchedText in Value;
    Form.CompleteWithTab := scoCompleteWithTab in Value;
    Form.CompleteWithEnter := scoCompleteWithEnter in Value;
  end;
end;

function TSynBaseCompletionProposal.GetTriggerChars: String;
begin
  Result := Form.TriggerChars;
end;

procedure TSynBaseCompletionProposal.SetTriggerChars(const Value: String);
begin
  Form.TriggerChars := Value;
end;

procedure TSynBaseCompletionProposal.EditorCancelMode(Sender: TObject);
begin
  //Do nothing here, used in TSynCompletionProposal
end;

procedure TSynBaseCompletionProposal.HookedEditorCommand(Sender: TObject;
  AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand;
  var AChar: Char; Data, HandlerData: Pointer);
begin
  // Do nothing here, used in TSynCompletionProposal
end;

function TSynBaseCompletionProposal.GetOnChange: TCompletionChange;
begin
  Result := Form.FOnChangePosition;
end;

procedure TSynBaseCompletionProposal.SetOnChange(
  const Value: TCompletionChange);
begin
  Form.FOnChangePosition := Value;
end;

procedure TSynBaseCompletionProposal.ResetAssignedList;
begin
  Form.AssignedList.Assign(ItemList);
end;

{ ----------------  TSynCompletionProposal -------------- }

procedure TSynCompletionProposal.HandleOnCancel(Sender: TObject);
var
  F: TSynBaseCompletionProposalForm;
begin
  F := Sender as TSynBaseCompletionProposalForm;
  FNoNextKey := False;
  if F.CurrentEditor <> nil then
  begin
    if Assigned(FTimer) then
      FTimer.Enabled := False;

    F.Hide;

    if ((F.CurrentEditor as TCustomSynEdit).Owner is TWinControl) and
       (((F.CurrentEditor as TCustomSynEdit).Owner as TWinControl).Visible) then
    begin
      ((F.CurrentEditor as TCustomSynEdit).Owner as TWinControl).SetFocus;
    end;

    (F.CurrentEditor as TCustomSynEdit).SetFocus;

{$IFDEF SYN_CLX}
    GetParentForm( F.CurrentEditor ).Show;
{$ENDIF}

    if Assigned(OnCancelled) then
      OnCancelled(Self); //GBN 13/11/2001
  end;
end;

procedure TSynCompletionProposal.HandleOnValidate(Sender: TObject;
  Shift: TShiftState; EndToken: Char);
var
  F: TSynBaseCompletionProposalForm;
  Value: string;
  Index: Integer; //GBN 15/11/2001
begin
  F := Sender as TSynBaseCompletionProposalForm;
  if Assigned(F.CurrentEditor) then
    with F.CurrentEditor as TCustomSynEdit do
    begin
      //Treat entire completion as a single undo operation
      BeginUpdate;
      BeginUndoBlock;
      try
        if FAdjustCompletionStart then
          FCompletionStart := BufferCoord(FCompletionStart, CaretY).Char;
        BlockBegin := BufferCoord(FCompletionStart, CaretY);
        if EndToken = #0 then
          BlockEnd := BufferCoord(WordEnd.Char, CaretY)
        else
          BlockEnd := BufferCoord(CaretX, CaretY);

        if scoUseInsertList in FOptions then
        begin
          if scoLimitToMatchedText in FOptions then
          begin
            if (Form.FAssignedList.Count > Position) then
              //GBN 15/01/2002 - Added check to make sure item is only used when no EndChar
              if (InsertList.Count > Integer(Form.FAssignedList.Objects[position])) and
                 ((scoEndCharCompletion in fOptions) or (EndToken = #0)) then
                Value := InsertList[Integer(Form.FAssignedList.Objects[position])]
              else
                Value := SelText
            else
              Value := SelText;
          end else
          begin
            //GBN 15/01/2002 - Added check to make sure item is only used when no EndChar
            if (InsertList.Count > Position) and
               ((scoEndCharCompletion in FOptions) or (EndToken = #0)) then
              Value := InsertList[position]
            else
              Value := SelText;
          end;
        end else
        begin
          //GBN 15/01/2002 - Added check to make sure item is only used when no EndChar
          if (Form.FAssignedList.Count > Position) and
             ((scoEndCharCompletion in FOptions) or (EndToken = #0)) then
            Value := Form.FAssignedList[Position]
          else
            Value := SelText;
        end;
        Index := Position; //GBN 15/11/2001, need to assign position to temp var since it changes later

        //GBN 15/01/2002 - Cleaned this code up a bit
        if Assigned(FOnCodeCompletion) then
          FOnCodeCompletion(Self, Value, Shift,
            F.LogicalToPhysicalIndex(Index), EndToken); //GBN 15/11/2001

        if SelText <> Value then
          SelText := Value;

        with (F.CurrentEditor as TCustomSynEdit) do
        begin
          //GBN 25/02/2002
          //This replaces the previous way of cancelling the completion by
          //sending a WM_MOUSEDOWN message. The problem with the mouse down is
          //that the editor would bounce back to the left margin, very irritating
          InternalCancelCompletion;
          SetFocus;
{$IFDEF SYN_CLX}
          GetParentForm( F.CurrentEditor ).Show;
{$ENDIF}
          EnsureCursorPosVisible; //GBN 25/02/2002
          CaretXY := BlockEnd;
          BlockBegin := CaretXY;
        end;
        //GBN 15/11/2001
        if Assigned(FOnAfterCodeCompletion) then
          FOnAfterCodeCompletion(Self, Value, Shift,
            F.LogicalToPhysicalIndex(Index), EndToken);

      finally
        EndUndoBlock;
        EndUpdate;
      end;
    end;
end;

procedure TSynCompletionProposal.HandleOnKeyPress(Sender: TObject; var Key: Char);
var
  F: TSynBaseCompletionProposalForm;
begin
  F := Sender as TSynBaseCompletionProposalForm;
  if F.CurrentEditor <> nil then
  begin
    with F.CurrentEditor as TCustomSynEdit do
      CommandProcessor(ecChar, Key, nil);
    //GBN 22/11/2001
    //Daisy chain completions
    Application.ProcessMessages;
    if (System.Pos(Key, TriggerChars) > 0) and not F.Visible then
      begin
      //GBN 18/02/2002
        if (Sender is TCustomSynEdit) then
          DoExecute(Sender as TCustomSynEdit)
        else
          if Assigned(Form.CurrentEditor) then
            DoExecute(Form.CurrentEditor as TCustomSynEdit);
      end;
  end;
end;

procedure TSynCompletionProposal.SetEditor(const Value: TCustomSynEdit);
begin
  if Editor <> Value then
  begin
    if Assigned(Editor) then
      RemoveEditor(Editor);
    FEditor := Value;
    if Assigned(Value) then
      AddEditor(Value);
  end;
end;

procedure TSynCompletionProposal.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) then
  begin
    if Editor = AComponent then
      Editor := nil
    else if AComponent is TCustomSynEdit then
      RemoveEditor( TCustomSynEdit(AComponent) );
  end;

  inherited Notification(AComponent, Operation);
end;

constructor TSynCompletionProposal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Form.OnKeyPress := HandleOnKeyPress;
  Form.OnValidate := HandleOnValidate;
  Form.OnCancel := HandleOnCancel;
  Form.OnDblClick := HandleDblClick;
  EndOfTokenChr := DefaultEndOfTokenChr;
  TriggerChars := '.';
  fTimerInterval:= 1000;
  fNoNextKey := false;

{$IFDEF SYN_CLX}
  fShortCut := QMenus.ShortCut(Ord(' '), [ssCtrl]);
  // Belongs to Missing-ShowWindow-Workaround
  FIgnoreFocusCommands := False;
{$ELSE}
  fShortCut := Menus.ShortCut(Ord(' '), [ssCtrl]);
{$ENDIF}
  Options := DefaultProposalOptions;
  fEditors := TList.Create;
end;

procedure TSynCompletionProposal.SetShortCut(Value: TShortCut);
begin
  FShortCut := Value;
end;

procedure TSynCompletionProposal.EditorKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  ShortCutKey   : Word;
  ShortCutShift : TShiftState;
begin
  ShortCutToKey (fShortCut,ShortCutKey,ShortCutShift);
  with Sender as TCustomSynEdit do
  begin
    if ((DefaultType <> ctCode) or not(ReadOnly)) and (Shift = ShortCutShift) and (Key = ShortCutKey) then
    begin
      Form.CurrentEditor := Sender as TCustomSynEdit;
      Key := 0;
      DoExecute(Sender as TCustomSynEdit);
    end;
  end;
end;

function TSynCompletionProposal.GetCurrentInput(AEditor: TCustomSynEdit): string;
var
  s: string;
  i: integer;
begin
  Result := '';
  if AEditor <> nil then
  begin
    s := AEditor.LineText;
    i := AEditor.CaretX - 1;
    if i <= Length(s) then
    begin
      FAdjustCompletionStart := False;
      while (i > 0) and (s[i] > #32) and (not (s[i] in Form.FWordBreakChars)) do
        dec(i);

      FCompletionStart := i+1;
      Result := Copy(s, i+1, AEditor.CaretX -i-1);
    end else FAdjustCompletionStart := True;

    FCompletionStart := i+1;
  end;
end;

function TSynCompletionProposal.GetPreviousToken(AEditor: TCustomSynEdit): string;
var
  Line: string;
  X: Integer;
  BreakChars: TSynIdentChars;
begin
  Result := '';
  if not Assigned(AEditor) then
    exit;

  Line := AEditor.Lines[AEditor.CaretXY.Line - 1];
  X := AEditor.CaretXY.Char-1;
  if (X = 0) or (X > Length(Line)) or (Length(Line) = 0) then
    exit;

  BreakChars := Form.FWordBreakChars;
  if Line[X] in BreakChars then
    dec(X);

  BreakChars := BreakChars + [#9, #32];

  while (X > 0) and (not (Line[X] in BreakChars)) do
  begin
    Result := Line[X] + Result;
    dec(x);
  end;
end;

procedure TSynCompletionProposal.EditorKeyPress(Sender: TObject; var Key: char);
begin
  if fNoNextKey  then
  begin
    FNoNextKey := False;
    Key := #0;
  end else
  if Assigned(FTimer) then
  begin
    if (Pos(Key, TriggerChars) <> 0) then
      ActivateTimer(Sender as TCustomSynEdit)
    else
      DeactivateTimer;

  end;
end;

procedure TSynCompletionProposal.ActivateTimer(ACurrentEditor: TCustomSynEdit);
begin
  if Assigned(FTimer) then
  begin
    Form.CurrentEditor := ACurrentEditor;
    FTimer.Enabled := True;
  end;
end;

procedure TSynCompletionProposal.DeactivateTimer;
begin
  if Assigned(FTimer) then
  begin
    FTimer.Enabled := False;
  end;
end;


procedure TSynCompletionProposal.HandleDblClick(Sender: TObject);
begin
  HandleOnValidate(Sender, [], #0);
end;

destructor TSynCompletionProposal.Destroy;
begin
  if Form.Visible then
    CancelCompletion;
  Editor := nil;
  while fEditors.Count <> 0 do
    RemoveEditor(TCustomSynEdit(FEditors.Last));

  inherited;

  fEditors.Free;
end;

procedure TSynCompletionProposal.TimerExecute(Sender: TObject);
begin
  if not Assigned(FTimer) then exit;
  FTimer.Enabled := False; //GBN 13/11/2001  
  if Application.Active then
  begin
    DoExecute(Form.CurrentEditor as TCustomSynEdit);
    FNoNextKey := False;
  end else if Form.Visible then Form.Hide;
end;

function TSynCompletionProposal.GetTimerInterval: Integer;
begin
  Result := FTimerInterval;
end;

procedure TSynCompletionProposal.SetTimerInterval(const Value: Integer);
begin
  FTimerInterval := Value;
  if Assigned(FTimer) then
    FTimer.Interval := Value;
end;

procedure TSynCompletionProposal.SetOptions(const Value: TSynCompletionOptions);
begin
  inherited;

  if scoUseBuiltInTimer in Value then
  begin
    if not(Assigned(FTimer)) then
    begin
      FTimer := TTimer.Create(Self);
      FTimer.Enabled := False;
      FTimer.Interval := FTimerInterval;
      FTimer.OnTimer := TimerExecute;
    end;
  end else begin
    if Assigned(FTimer) then
    begin
      FreeAndNil(FTimer);
    end;
  end;

end;

procedure TSynCompletionProposal.ExecuteEx(s: string; x, y: integer;
  Kind: SynCompletionType);
begin
  {$IFDEF SYN_CLX} // Missing-ShowWindow-Workaround
  FIgnoreFocusCommands := True;
  try
  {$ENDIF}
    inherited;
    if Assigned(FTimer) then
      FTimer.Enabled := False;
  {$IFDEF SYN_CLX} // Missing-ShowWindow-Workaround
  finally
    FIgnoreFocusCommands := False;
  end;
  {$ENDIF}
end;

procedure TSynCompletionProposal.AddEditor(AEditor: TCustomSynEdit);
var
  i : integer;
begin
  i := fEditors.IndexOf(AEditor);
  if i = -1 then begin
    AEditor.FreeNotification(Self);
    fEditors.Add(AEditor);
    AEditor.AddKeyDownHandler(EditorKeyDown);
    AEditor.AddKeyPressHandler(EditorKeyPress);
    AEditor.RegisterCommandHandler(HookedEditorCommand,Self);
  end;
end;

function TSynCompletionProposal.EditorsCount: integer;
begin
  result := fEditors.count;
end;

function TSynCompletionProposal.GetEditor(i: integer): TCustomSynEdit;
begin
  if (i < 0) or (i >= EditorsCount) then
    Result := nil
  else
    Result := fEditors[i];
end;

function TSynCompletionProposal.RemoveEditor(AEditor: TCustomSynEdit): boolean;
var
  i: integer;
begin
  i := fEditors.Remove(AEditor);
  result := i <> -1;
  if result then begin
    if Form.CurrentEditor = AEditor then
    begin
      if Form.Visible then
        CancelCompletion;
      Form.CurrentEditor := nil;
    end;
    AEditor.RemoveKeyDownHandler(EditorKeyDown);
    AEditor.RemoveKeyPressHandler(EditorKeyPress);
    AEditor.UnregisterCommandHandler(HookedEditorCommand);
    {$IFDEF SYN_COMPILER_5_UP}
    RemoveFreeNotification( AEditor );
    {$ENDIF}
    if fEditor = AEditor then
      fEditor := nil;
  end;
end;

procedure TSynCompletionProposal.DoExecute(AEditor: TCustomSynEdit);

  procedure CopyStringToCharSet(const AStr: string; var ACharSet: TSynIdentChars);
  var
    i: Integer;
  begin
    for i := 1 to Length(AStr) do
      Include(ACharSet, AStr[i]);
  end;

  procedure CopyWordBreakCharsToCharSet(const AEditor: TCustomSynEdit;
    var ACharSet: TSynIdentChars);
  begin
    if Assigned(AEditor) and Assigned(AEditor.Highlighter) then
      ACharSet := ACharSet + AEditor.Highlighter.WordBreakChars
    else
      ACharSet := ACharSet + TSynWordBreakChars;
  end;

var
  p: TPoint;
  i: integer;
begin
  i := FEditors.IndexOf(AEditor);
  if i <> -1 then
    with AEditor do
    begin
      if (DefaultType <> ctCode) or not(ReadOnly) then
      begin
        if DefaultType = ctHint then
          GetCursorPos(P)
        else begin
          p := ClientToScreen( RowColumnToPixels(DisplayXY) );
          Inc(p.y, LineHeight);
        end;

        Form.CurrentEditor := AEditor;

        Form.FWordBreakChars := [];
        if scoConsiderWordBreakChars in Self.Options then
          CopyWordBreakCharsToCharSet(Form.CurrentEditor as TCustomSynEdit,
            Form.FWordBreakChars);

        CopyStringToCharSet(EndOfTokenChr, Form.FWordBreakChars);

        FPreviousToken := GetPreviousToken(Form.CurrentEditor as TCustomSynEdit);
        ExecuteEx(GetCurrentInput(AEditor), p.x, p.y, DefaultType);
        FNoNextKey := (DefaultType = ctCode) and FCanExecute and Form.Visible;
      end;
    end;
end;

//25/02/2002 GBN
procedure TSynCompletionProposal.InternalCancelCompletion;
begin
  if Assigned(FTimer) then FTimer.Enabled := False;
  FNoNextKey := False;
  if (Form.Visible) then
  begin
    Deactivate;
    Form.Hide;
  end;
end;

procedure TSynCompletionProposal.CancelCompletion;
begin
  InternalCancelCompletion; //25/02/2002 GBN
  if Assigned(OnCancelled) then OnCancelled(Self); //GBN 13/11/2001
end;

//GBN 13/11/2001
procedure TSynCompletionProposal.EditorCancelMode(Sender: TObject);
begin
  if (DisplayType=ctParams) then CancelCompletion;
end;

//GBN 13/11/2001
procedure TSynCompletionProposal.HookedEditorCommand(Sender: TObject;
  AfterProcessing: Boolean; var Handled: Boolean; var Command: TSynEditorCommand;
  var AChar: Char; Data, HandlerData: Pointer);
begin
  inherited;

  if AfterProcessing and Form.Visible then
  begin
    case DisplayType of
    ctCode:
      begin

      end;
    ctHint:
      begin
        {$IFDEF SYN_CLX}
        if ((Command <> ecLostFocus) and (Command <> ecGotFocus))
          or (not FIgnoreFocusCommands) then
        {$ENDIF}
          CancelCompletion
      end;
    ctParams:
      begin
        case Command of
        ecGotFocus, ecLostFocus:
          {$IFDEF SYN_CLX}
          if ((Command <> ecLostFocus) and (Command <> ecGotFocus))
            or (not FIgnoreFocusCommands) then
          {$ENDIF}
            CancelCompletion;
        ecLineBreak:
          DoExecute(Sender as TCustomSynEdit);
        ecChar:
          begin
            case AChar of
            #27:
              CancelCompletion;
            #32..'z':
              with Form do
              begin
{                if Pos(AChar, FTriggerChars) > 0 then
                begin
                  if Assigned(FParameterToken) then
                  begin
                    TmpIndex := CurrentIndex;
                    TmpLevel := CurrentLevel;
                    TmpStr := CurrentString;
                    OnParameterToken(Self, CurrentIndex, TmpLevel, TmpIndex, AChar, TmpStr);
                    CurrentIndex := TmpIndex;
                    CurrentLevel := TmpLevel;
                    CurrentString := TmpStr;
                  end;
                end;}
                DoExecute(Sender as TCustomSynEdit);
              end;
            else DoExecute(Sender as TCustomSynEdit);
            end;
          end;
        else DoExecute(Sender as TCustomSynEdit);
        end;
      end;
    end;
  end else
  if (not Form.Visible) and Assigned(FTimer) then
  begin
    if (Command = ecChar) then
      if (Pos(AChar, TriggerChars) = 0) then
        FTimer.Enabled := False
      else
    else
      FTimer.Enabled := False;
  end;

end;

procedure TSynCompletionProposal.ActivateCompletion;
begin
  DoExecute(Editor);
end;



{ TSynAutoComplete }

constructor TSynAutoComplete.Create(AOwner: TComponent);
begin
  inherited;
  FDoLookup := True;
  CreateInternalCompletion;
  FEndOfTokenChr := DefaultEndOfTokenChr;
  fAutoCompleteList := TStringList.Create;
  fNoNextKey := false;
{$IFDEF SYN_CLX}
  fShortCut := QMenus.ShortCut(Ord(' '), [ssShift]);
{$ELSE}
  fShortCut := Menus.ShortCut(Ord(' '), [ssShift]);
{$ENDIF}
end;

procedure TSynAutoComplete.SetShortCut(Value: TShortCut);
begin
  FShortCut := Value;
end;

destructor TSynAutoComplete.Destroy;
begin
  Editor := nil;
  if Assigned(FInternalCompletion) then
  begin
    FInternalCompletion.Free;
    FInternalCompletion := nil;
  end;
  inherited;
  fAutoCompleteList.free;
end;

procedure TSynAutoComplete.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ShortCutKey   : Word;
  ShortCutShift : TShiftState;
begin
  ShortCutToKey (fShortCut,ShortCutKey,ShortCutShift);
  if not (Sender as TCustomSynEdit).ReadOnly and
    (Shift = ShortCutShift) and (Key = ShortCutKey) then
  begin
    Execute(GetPreviousToken(Sender as TCustomSynEdit), Sender as TCustomSynEdit);
    fNoNextKey := true;
    Key := 0;
  end;
end;

procedure TSynAutoComplete.EditorKeyPress(Sender: TObject; var Key: char);
begin
  if fNoNextKey then begin
    fNoNextKey := false;
    Key := #0;
  end;
end;

procedure TSynAutoComplete.Execute(token: string; Editor: TCustomSynEdit);
begin
  ExecuteEx(token, Editor, FDoLookup);
end;

procedure TSynAutoComplete.ExecuteEx(token: String; Editor: TCustomSynEdit;
  LookupIfNotExact: Boolean);
var
  Temp: string;
  i, j: integer;
  StartOfBlock: TBufferCoord;
  ChangedIndent   : Boolean;
  ChangedTrailing : Boolean;
  TmpOptions : TSynEditorOptions;
  OrigOptions: TSynEditorOptions;
  BeginningSpaceCount : Integer;
  Spacing: String;
begin
  if Assigned(OnBeforeExecute) then OnBeforeExecute(Self);
  try
    i := AutoCompleteList.IndexOf(token);
    if (i <> -1) then
    begin
      TmpOptions := Editor.Options;
      OrigOptions:= Editor.Options;
      ChangedIndent   := eoAutoIndent in TmpOptions;
      ChangedTrailing := eoTrimTrailingSpaces in TmpOptions;

      if ChangedIndent then Exclude(TmpOptions, eoAutoIndent);
      if ChangedTrailing then Exclude(TmpOptions, eoTrimTrailingSpaces);

      if ChangedIndent or ChangedTrailing then
        Editor.Options := TmpOptions;

      Editor.UndoList.AddChange(crAutoCompleteBegin, StartOfBlock, StartOfBlock, '',
        smNormal);

      fNoNextKey := true;
      for j := 1 to length(token) do
        Editor.CommandProcessor(ecDeleteLastChar, ' ', nil);
      BeginningSpaceCount := Editor.DisplayX - 1;  //GBN 2002/04/24
      if (not (eoTabsToSpaces in Editor.Options)) and (BeginningSpaceCount>=Editor.TabWidth) then
          Spacing:=StringOfChar(#9,BeginningSpaceCount div Editor.TabWidth)+StringOfChar(' ',BeginningSpaceCount mod Editor.TabWidth)
      else Spacing:=StringOfChar(' ',BeginningSpaceCount);

      inc(i);
      if (i < AutoCompleteList.Count) and
         (length(AutoCompleteList[i]) > 0) and
         (AutoCompleteList[i][1] = '|') then
      begin
        inc(i);
      end;
      StartOfBlock.Char := -1;
      StartOfBlock.Line := -1;
      while (i < AutoCompleteList.Count) and
            (length(AutoCompleteList[i]) > 0) and
            (AutoCompleteList[i][1] = '=') do
      begin
  {      for j := 0 to PrevSpace - 1 do
          Editor.CommandProcessor(ecDeleteLastChar, ' ', nil);}
        Temp := AutoCompleteList[i];
        for j := 2 to length(Temp) do begin
          if (Temp[j]=#9) then Editor.CommandProcessor(ecTab, Temp[j], nil)
          else Editor.CommandProcessor(ecChar, Temp[j], nil);
          if (Temp[j] = '|') then
            StartOfBlock := Editor.CaretXY
        end;
        inc(i);
        if (i < AutoCompleteList.Count) and
           (length(AutoCompleteList[i]) > 0) and
           (AutoCompleteList[i][1] = '=') then
        begin
           Editor.CommandProcessor (ecLineBreak,' ',nil);
           for j := 1 to length(Spacing) do
             if (Spacing[j]=#9) then Editor.CommandProcessor(ecTab,#9,nil)
             else Editor.CommandProcessor (ecChar, ' ', nil);
        end;
      end;
      if (StartOfBlock.Char <> -1) and (StartOfBlock.Line <> -1) then begin
        Editor.CaretXY := StartOfBlock;
        Editor.CommandProcessor(ecDeleteLastChar, ' ', nil);
      end;

      if ChangedIndent or ChangedTrailing then Editor.Options := OrigOptions;

      Editor.UndoList.AddChange(crAutoCompleteEnd, StartOfBlock, StartOfBlock, '',
        smNormal);
      fNoNextKey:=false;   //GBN 2002-03-07
    end else if LookupIfNotExact and (assigned(FInternalCompletion)) then
    begin
      FInternalCompletion.AddEditor(Editor);
      FInternalCompletion.ClearList;
      for i := 0 to AutoCompleteList.Count - 1 do
        if (length(AutoCompleteList[i]) > 0) and (AutoCompleteList[i][1] <> '=') and (AutoCompleteList[i][1] <> '|') then
        begin
          if (i + 1 < AutoCompleteList.Count) and (length(AutoCompleteList[i + 1]) > 0) and
            (AutoCompleteList[i + 1][1] = '|') then
          begin
            Temp := AutoCompleteList[i + 1];
            Delete(Temp, 1, 1);
          end else Temp := AutoCompleteList[i];
          Temp := '\style{+B}' + AutoCompleteList[i] + '\style{-B}\column{}' + Temp;
          FInternalCompletion.ItemList.Add(Temp);
          FInternalCompletion.InsertList.Add(AutoCompleteList[i]);
        end;
      FInternalCompletion.DoExecute(Editor);
    end;
  finally
    if Assigned(OnAfterExecute) then OnAfterExecute(Self);
  end;
end;

procedure TSynAutoComplete.DoInternalAutoCompletion(Sender: TObject;
  const Value: string; Shift: TShiftState; Index: Integer; EndToken: Char);
begin
  ExecuteEx(GetPreviousToken(Editor), Editor, False);
  FInternalCompletion.Editor := nil;
end;

function TSynAutoComplete.GetPreviousToken(Editor: TCustomSynEdit): string;
var
  s: string;
  i: integer;
begin
  Result := '';
  if Editor <> nil then begin
    s := Editor.LineText;
    i := Editor.CaretX - 1;
    if i <= Length (s) then begin
      while (i > 0) and (s[i] > ' ') and (pos(s[i], FEndOfTokenChr) = 0) do
        Dec(i);
      Result := copy(s, i + 1, Editor.CaretX - i - 1);
    end;
  end
end;

procedure TSynAutoComplete.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (Editor = AComponent) then
    Editor := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TSynAutoComplete.SetAutoCompleteList(List: TStrings);
begin
  fAutoCompleteList.Assign(List);
end;

procedure TSynAutoComplete.SetEditor(const Value: TCustomSynEdit);
begin
  if Editor <> Value then
  begin
    if Editor <> nil then
    begin
      Editor.RemoveKeyDownHandler( EditorKeyDown );
      Editor.RemoveKeyPressHandler( EditorKeyPress );
      {$IFDEF SYN_COMPILER_5_UP}
      RemoveFreeNotification( Editor );
      {$ENDIF}
    end;
    fEditor := Value;
    if Editor <> nil then
    begin
      Editor.AddKeyDownHandler( EditorKeyDown );
      Editor.AddKeyPressHandler( EditorKeyPress );
      FreeNotification( Editor );
    end;
  end;
end;

function TSynAutoComplete.GetTokenList: string;
var
  List: TStringList;
  i: integer;
begin
  Result := '';
  if AutoCompleteList.Count < 1 then Exit;
  List := TStringList.Create;
  i := 0;
  while (i < AutoCompleteList.Count) do begin
    if (length(AutoCompleteList[i]) > 0) and (AutoCompleteList[i][1] <> '=') then
      List.Add(Trim(AutoCompleteList[i]));
    inc(i);
  end;
  Result := List.Text;
  List.Free;
end;

function TSynAutoComplete.GetTokenValue(Token: string): string;
var
  i: integer;
  List: TStringList;
begin
  Result := '';
  i := AutoCompleteList.IndexOf(Token);
  if i <> -1 then begin
    List := TStringList.Create;
    Inc(i);
    while (i < AutoCompleteList.Count) and
      (length(AutoCompleteList[i]) > 0) and
      (AutoCompleteList[i][1] = '=') do begin
      if Length(AutoCompleteList[i]) = 1 then
        List.Add('')
      else
        List.Add(Copy(AutoCompleteList[i], 2, Length(AutoCompleteList[i])));
      inc(i);
    end;
    Result := List.Text;
    List.Free;
  end;
end;

procedure TSynAutoComplete.SetDoLookup(const Value: Boolean);
begin
  FDoLookup := Value;
  if FDoLookup and not(Assigned(FInternalCompletion)) then
    CreateInternalCompletion
  else begin
    FInternalCompletion.Free;
    FInternalCompletion := nil;
  end;
end;

procedure TSynAutoComplete.CreateInternalCompletion;
begin
  FInternalCompletion := TSynCompletionProposal.Create(Self);
  FInternalCompletion.Options := DefaultProposalOptions + [scoUsePrettyText] - [scoUseBuiltInTimer];
  FInternalCompletion.EndOfTokenChr := FEndOfTokenChr;
  FInternalCompletion.ShortCut := 0;
  FInternalCompletion.OnAfterCodeCompletion := DoInternalAutoCompletion;
  with FInternalCompletion.Columns.Add do
    //this is the trigger column
    BiggestWord := 'XXXXXXXX';
end;

function TSynAutoComplete.GetOptions: TSynCompletionOptions;
begin
  Result := FOptions;
end;

procedure TSynAutoComplete.SetOptions(const Value: TSynCompletionOptions);
begin
  FOptions := Value;
  if Assigned(FInternalCompletion) then
    FInternalCompletion.Options := FOptions + [scoUsePrettyText] - [scoUseBuiltInTimer];
end;

procedure TSynAutoComplete.CancelCompletion;
begin
  if Assigned(FInternalCompletion) then
    FInternalCompletion.CancelCompletion;
end;

function TSynAutoComplete.GetExecuting: Boolean;
begin
  if Assigned(FInternalCompletion) then
    Result := FInternalCompletion.Form.Visible
  else Result := False;
end;

end.
