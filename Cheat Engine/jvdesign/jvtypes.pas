unit JvTypes;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  // LCL
  LCLType, LCLIntf, Graphics, ExtCtrls,
  SysUtils, Classes,  //Consts,
  LMessages, Controls, Forms,
  JvConsts, JvResources;

const
  MaxPixelCount = 32767;

{$IFNDEF COMPILER12_UP}
{$HPPEMIT '#ifndef TDate'}
{$HPPEMIT '#define TDate Controls::TDate'}
{$HPPEMIT '#define TTime Controls::TTime'}
{$HPPEMIT '#endif'}
{$ENDIF !COMPILER12_UP}

type
  TJvBytes = Pointer;
  IntPtr = Pointer;

type
  {$IFNDEF COMPILER9_UP}
  TVerticalAlignment = (taAlignTop, taAlignBottom, taVerticalCenter);
  TTopBottom = taAlignTop..taAlignBottom;
  {$ENDIF ~COMPILER9_UP}

  PCaptionChar = PChar;

  {$IFDEF COMPILER12_UP}
  THintInfo = Controls.THintInfo;
  {$EXTERNALSYM THintInfo}
  PHintInfo = Controls.PHintInfo;
  {$EXTERNALSYM PHintInfo}
  {$ENDIF}

  // used in JvSpeedButton, JvArrowButton, JvButton CM_JVBUTTONPRESSED
  // asn: can also be used with CM_BUTTONPRESSED
  TCMButtonPressed = packed record
    Msg: Cardinal;
    Index: Integer;     { clx has Index and Control switched }
    Control: TControl;
    Result: Longint;
  end;

  THintString = string;
  THintStringList = TStringList;

  { JvExVCL classes }
  TInputKey = (ikAll, ikArrows, ikChars, ikButton, ikTabs, ikEdit, ikNative{, ikNav, ikEsc});
  TInputKeys = set of TInputKey;

  TJvRGBTriple = packed record
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
  end;

const
  NullHandle = 0;
  // (rom) deleted fbs constants. They are already in JvConsts.pas.

type
  TTimerProc = procedure(hwnd: THandle; Msg: Cardinal; idEvent: Cardinal; dwTime: Cardinal);

type
  // Base class for persistent properties that can show events.
  // By default, Delphi and BCB don't show the events of a class
  // derived from TPersistent unless it also derives from
  // TComponent.
  // The design time editor associated with TJvPersistent will display
  // the events, thus mimicking a Sub Component.
  TJvPersistent = class(TComponent)
  private
    FOwner: TPersistent;
    function _GetOwner: TPersistent;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); reintroduce; virtual;

    function GetNamePath: string; override;
    property Owner: TPersistent read _GetOwner;
  end;

  // Added by dejoy (2005-04-20)
  // A lot of TJVxxx control persistent properties used TPersistent,
  // So and a TJvPersistentProperty to do this job. make to support batch-update mode
  // and property change notify.
  TJvPropertyChangeEvent = procedure(Sender: TObject; const PropName: string) of object;

  TJvPersistentProperty = class(TJvPersistent)//TPersistent => TJvPersistent
  private
    FUpdateCount: Integer;
    FOnChanging: TNotifyEvent;
    FOnChanged: TNotifyEvent;
    FOnChangingProperty: TJvPropertyChangeEvent;
    FOnChangedProperty: TJvPropertyChangeEvent;
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    procedure ChangedProperty(const PropName: string); virtual;
    procedure ChangingProperty(const PropName: string); virtual;
    procedure SetUpdateState(aUpdating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
  public
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChangedProperty: TJvPropertyChangeEvent read FOnChangedProperty write FOnChangedProperty;
    property OnChangingProperty: TJvPropertyChangeEvent read FOnChangingProperty write FOnChangingProperty;
  end;

  TJvRegKey = (hkClassesRoot, hkCurrentUser, hkLocalMachine, hkUsers,
    hkPerformanceData, hkCurrentConfig, hkDynData);
  TJvRegKeys = set of TJvRegKey;

  // base JVCL Exception class to derive from
  EJVCLException = class(Exception);

  TJvLinkClickEvent = procedure(Sender: TObject; Link: string) of object;
  //  TOnRegistryChangeKey = procedure(Sender: TObject; RootKey: HKEY; Path: string) of object;
  //  TAngle = 0..360;
  TJvOutputMode = (omFile, omStream);
  //  TLabelDirection = (sdLeftToRight, sdRightToLeft); // JvScrollingLabel

  TJvDoneFileEvent = procedure(Sender: TObject; FileName: string; FileSize: Integer; Url: string) of object;
  TJvDoneStreamEvent = procedure(Sender: TObject; Stream: TStream; StreamSize: Integer; Url: string) of object;
  TJvHTTPProgressEvent = procedure(Sender: TObject; UserData, Position: Integer; TotalSize: Integer; Url: string; var Continue: Boolean) of object;
  TJvFTPProgressEvent = procedure(Sender: TObject; Position: Integer; Url: string) of object;

  // from JvComponent.pas
  TJvClipboardCommand = (caCopy, caCut, caPaste, caClear, caUndo);
  TJvClipboardCommands = set of TJvClipboardCommand;

  // used in JvButton
  TCMForceSize = record
    Msg: Cardinal;
    NewSize: TSmallPoint;
    Sender: TControl;
    Result: Longint;
  end;

  PJvRGBArray = ^TJvRGBArray;
  TJvRGBArray = array [0..MaxPixelCount] of TJvRGBTriple;
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array [0..MaxPixelCount] of TRGBQuad;
  PRGBPalette = ^TRGBPalette;
  TRGBPalette = array [Byte] of TRGBQuad;

  { (rom) unused
  TJvPoint = class(TPersistent)
  protected
    FX: Integer;
    FY: Integer;
  published
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
  end;
  }

  TJvErrorEvent = procedure(Sender: TObject; ErrorMsg: string) of object;
  TJvWaveLocation = (frFile, frResource, frRAM);

  TJvPopupPosition = (ppNone, ppForm, ppApplication);
  //  TJvDirMask = (dmFileNameChange, dmDirnameChange, dmAttributesChange, dmSizeChange, dmLastWriteChange, dmSecurityChange); //JvDirectorySpy
  //  TJvDirMasks = set of TJvDirMask;
  //  EJvDirectoryError = class(EJVCLException); // JvDirectorySpy
  //  TListEvent = procedure(Sender: TObject; Title: string; Handle: THandle) of object; // JvWindowsTitle

  TJvProgressEvent = procedure(Sender: TObject; Current, Total: Integer) of object;
  TJvNextPageEvent = procedure(Sender: TObject; PageNumber: Integer) of object;
  TJvBitmapStyle = (bsNormal, bsCentered, bsStretched);

  //  TOnOpened = procedure(Sender: TObject; Value: string) of object; // archive
  //  TOnOpenCanceled = procedure(Sender: TObject) of object; // archive

  TJvGradientStyle = (grFilled, grEllipse, grHorizontal, grVertical, grPyramid, grMount);
  //  TOnDelete = procedure(Sender: TObject; Path: string) of object;
  TJvParentEvent = procedure(Sender: TObject; ParentWindow: THandle) of object;
  //  TOnImage = procedure(Sender: TObject; Image: TBitmap) of object; // JvClipboardViewer
  //  TOnText = procedure(Sender: TObject; Text: string) of object;
  //  TJvRestart = (rsLogoff, rsShutdown, rsReboot, rsRestart, rsRebootSystem, rsExitAndExecApp);
  //  TJvRunOption = (roNoBrowse, roNoDefault, roCalcDirectory, roNoLabel, roNoSeparateMem); // JvRunDlg
  //  TJvRunOptions = set of TJvRunOption; // JvRunDlg
  //  TJvFileKind = (ftFile, ftPrinter); // JvObjectPropertiesDlg

  //  TSHFormatDrive = function(Handle: THandle; Drive, ID, Options: Word): LongInt; stdcall; // JvFormatDrive
  //  TFormatOption = (shQuickFormat, shFull, shSystemFilesOnly); // JvFormatDrive
  //  TButtonStyle = (bsAbortRetryIgnore, bsOk, bsOkCancel, bsRetryCancel, bsYesNo, bsYesNoCancel); // JvMessageBox
  //  TButtonDisplay = (bdIconExclamation, bdIconWarning, bdIconInformation, bdIconAsterisk, bdIconQuestion, bdIconStop, bdIconError, bdIconHand); // JvMessageBox

  //  TDefault = (dbButton1, dbButton2, dbButton3, dbButton4); // JvMessageBox
  //  TModality = (bmApplModal, bmSystemModal, bmTaskModal); // JvMessageBox
  //  TButtonOption = (boDefaultDesktopOnly, boHelp, boRight, boRtlReading, boSetForeground, boTopMost); // JvMessageBox
  //  TButtonOptions = set of TButtonOption; // JvMessageBox
  //  TButtonResult = (brAbort, brCancel, brIgnore, brNo, brOk, brRetry, brYes); // JvMessageBox
  //  TMsgStyle = (msBeep, msIconAsterisk, msIconExclamation, msIconHand, msIconQuestion, msOk); // JvMessageBeep
  TJvDiskRes = (dsSuccess, dsCancel, dsSkipfile, dsError);
  TJvDiskStyle = (idfCheckFirst, idfNoBeep, idfNoBrowse, idfNoCompressed, idfNoDetails,
    idfNoForeground, idfNoSkip, idfOemDisk, idfWarnIfSkip);
  TJvDiskStyles = set of TJvDiskStyle;
  TJvDeleteStyle = (idNoBeep, idNoForeground);
  TJvDeleteStyles = set of TJvDeleteStyle;
  //   TOnOk = procedure(Sender: TObject; Password: string; var Accept: Boolean) of object; // JvPasswordForm

  //  TCoordChanged = procedure(Sender: TObject; Coord: string) of object;
  TJvNotifyParamsEvent = procedure(Sender: TObject; Params: Pointer) of object;

  TJvFileInfoRec = record
    Attributes: DWORD;
    DisplayName: string;
    ExeType: Integer;
    Icon: HICON;
    Location: string;
    TypeName: string;
    SysIconIndex: Integer;
  end;

  TJvAnimation = (anLeftRight, anRightLeft, anRightAndLeft, anLeftVumeter, anRightVumeter);
  TJvAnimations = set of TJvAnimation;
  //   TOnFound = procedure(Sender: TObject; Path: string) of object; // JvSearchFile
  //  TOnChangedDir = procedure(Sender: TObject; Directory: string) of object; // JvSearchFile
  //  TOnAlarm = procedure(Sender: TObject; Keyword: string) of object; // JvAlarm
  {  TAlarm = record
      Keyword: string;
      DateTime: TDateTime;
    end;
  } // JvAlarm

  // Bianconi - Moved from JvAlarms.pas
  TJvTriggerKind =
    (tkOneShot, tkEachSecond, tkEachMinute, tkEachHour, tkEachDay, tkEachMonth, tkEachYear);
  // End of Bianconi

  TJvFourCC = array [0..3] of AnsiChar;
  PJvAniTag = ^TJvAniTag;
  TJvAniTag = packed record
    ckID: TJvFourCC;
    ckSize: Longint;
  end;

  TJvAniHeader = packed record
    dwSizeof: Longint;
    dwFrames: Longint;
    dwSteps: Longint;
    dwCX: Longint;
    dwCY: Longint;
    dwBitCount: Longint;
    dwPlanes: Longint;
    dwJIFRate: Longint;
    dwFlags: Longint;
  end;

  TJvChangeColorEvent = procedure(Sender: TObject; Foreground, Background: TColor) of object;

  TJvLayout = (lTop, lCenter, lBottom);
  TJvBevelStyle = (bsShape, bsLowered, bsRaised);

  {for OnLoseFocus the AFocusControl argument will point at the control that
   receives focus while for OnGetFocus it is the control that lost the focus}
  TJvFocusChangeEvent = procedure(const ASender: TObject;
    const AFocusControl: TWinControl) of object;

  // JvJCLUtils
  TTickCount = Cardinal;

  {**** string handling routines}
  TSetOfChar = TSysCharSet;
  TCharSet = TSysCharSet;

  TDateOrder = (doMDY, doDMY, doYMD);
  TDayOfWeekName = (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
  TDaysOfWeek = set of TDayOfWeekName;

const
  DefaultDateOrder = doDMY;

  CenturyOffset: Byte = 60;
  NullDate: TDateTime = 0; {-693594}

type
  // JvDriveCtrls / JvLookOut
  TJvImageSize = (isSmall, isLarge);
  TJvImageAlign = (iaLeft, iaCentered);

  TJvDriveType = (dtUnknown, dtRemovable, dtFixed, dtRemote, dtCDROM, dtRamDisk);
  TJvDriveTypes = set of TJvDriveType;

  // Defines how a property (like a HotTrackFont) follows changes in the component's normal Font
  TJvTrackFontOption = (
    hoFollowFont,  // makes HotTrackFont follow changes to the normal Font
    hoPreserveCharSet,  // don't change HotTrackFont.Charset
    hoPreserveColor,    // don't change HotTrackFont.Color
    hoPreserveHeight,   // don't change HotTrackFont.Height (affects Size as well)
    hoPreserveName,     // don't change HotTrackFont.Name
    hoPreservePitch,    // don't change HotTrackFont.Pitch
    hoPreserveStyle);   // don't change HotTrackFont.Style
  TJvTrackFontOptions = set of TJvTrackFontOption;

const
  DefaultTrackFontOptions = [hoFollowFont, hoPreserveColor, hoPreserveStyle];
  DefaultHotTrackColor = $00D2BDB6;
  DefaultHotTrackFrameColor = $006A240A;

type
  // from JvListView.pas
  TJvSortMethod = (smAutomatic, smAlphabetic, smNonCaseSensitive, smNumeric, smDate, smTime, smDateTime, smCurrency);
  TJvListViewColumnSortEvent = procedure(Sender: TObject; Column: Integer; var AMethod: TJvSortMethod) of object;

  // from JvOfficeColorPanel.pas
  TJvAddInControlSiteInfo = record
    AddInControl: TControl;
    BoundsRect: TRect;
    SiteInfoData: TObject;
  end;

  TJvClickColorType =
    (cctColors, cctNoneColor, cctDefaultColor, cctCustomColor, cctAddInControl, cctNone);
  TJvHoldCustomColorEvent = procedure(Sender: TObject; AColor: TColor) of object;
  TJvColorQuadLayOut = (cqlNone, cqlLeft, cqlRight, cqlClient);
  TJvGetAddInControlSiteInfoEvent = procedure(Sender: TControl; var ASiteInfo: TJvAddInControlSiteInfo) of object;

  // from JvColorProvider.pas
  TColorType = (ctStandard, ctSystem, ctCustom);

  TDefColorItem = record
    Value: TColor;
    Constant: string;
    Description: string;
  end;

const
  ColCount = 20;
  StandardColCount = 40;
  SysColCount = 30;
  {$IFDEF COMPILER6}
   {$IF not declared(clHotLight)}
    {$MESSAGE ERROR 'You do not have installed Delphi 6 Runtime Library Update 2. Please install this before installing the JVCL. http://downloads.codegear.com/default.aspx?productid=300'}
   {$IFEND}
  {$ENDIF COMPILER6}

  ColorValues: array [0 .. ColCount - 1] of TDefColorItem = (
    (Value: clBlack;      Constant: 'clBlack';      Description: RsClBlack),
    (Value: clMaroon;     Constant: 'clMaroon';     Description: RsClMaroon),
    (Value: clGreen;      Constant: 'clGreen';      Description: RsClGreen),
    (Value: clOlive;      Constant: 'clOlive';      Description: RsClOlive),
    (Value: clNavy;       Constant: 'clNavy';       Description: RsClNavy),
    (Value: clPurple;     Constant: 'clPurple';     Description: RsClPurple),
    (Value: clTeal;       Constant: 'clTeal';       Description: RsClTeal),
    (Value: clGray;       Constant: 'clGray';       Description: RsClGray),
    (Value: clSilver;     Constant: 'clSilver';     Description: RsClSilver),
    (Value: clRed;        Constant: 'clRed';        Description: RsClRed),
    (Value: clLime;       Constant: 'clLime';       Description: RsClLime),
    (Value: clYellow;     Constant: 'clYellow';     Description: RsClYellow),
    (Value: clBlue;       Constant: 'clBlue';       Description: RsClBlue),
    (Value: clFuchsia;    Constant: 'clFuchsia';    Description: RsClFuchsia),
    (Value: clAqua;       Constant: 'clAqua';       Description: RsClAqua),
    (Value: clWhite;      Constant: 'clWhite';      Description: RsClWhite),
    (Value: clMoneyGreen; Constant: 'clMoneyGreen'; Description: RsClMoneyGreen),
    (Value: clSkyBlue;    Constant: 'clSkyBlue';    Description: RsClSkyBlue),
    (Value: clCream;      Constant: 'clCream';      Description: RsClCream),
    (Value: clMedGray;    Constant: 'clMedGray';    Description: RsClMedGray)
  );

  //added by dejoy (2005-04-20)
  StandardColorValues: array [0 .. StandardColCount - 1] of TDefColorItem = (
    (Value: $00000000;    Constant: 'clBlack';          Description: RsClBlack),
    (Value: $00003399;    Constant: 'clBrown';          Description: RsClBrown),
    (Value: $00003333;    Constant: 'clOliveGreen';     Description: RsClOliveGreen),
    (Value: $00003300;    Constant: 'clDarkGreen';      Description: RsClDarkGreen),
    (Value: $00663300;    Constant: 'clDarkTeal';       Description: RsClDarkTeal),
    (Value: $00800000;    Constant: 'clDarkBlue';       Description: RsClDarkBlue),
    (Value: $00993333;    Constant: 'clIndigo';         Description: RsClIndigo),
    (Value: $00333333;    Constant: 'clGray80';         Description: RsClGray80),

    (Value: $00000080;    Constant: 'clDarkRed';        Description: RsClDarkRed),
    (Value: $000066FF;    Constant: 'clOrange';         Description: RsClOrange),
    (Value: $00008080;    Constant: 'clDarkYellow';     Description: RsClDarkYellow),
    (Value: $00008000;    Constant: 'clGreen';          Description: RsClGreen),
    (Value: $00808000;    Constant: 'clTeal';           Description: RsClTeal),
    (Value: $00FF0000;    Constant: 'clBlue';           Description: RsClBlue),
    (Value: $00996666;    Constant: 'clBlueGray';       Description: RsClBlueGray),
    (Value: $00808080;    Constant: 'clGray50';         Description: RsClGray50),

    (Value: $000000FF;    Constant: 'clRed';            Description: RsClRed),
    (Value: $000099FF;    Constant: 'clLightOrange';    Description: RsClLightOrange),
    (Value: $0000CC99;    Constant: 'clLime';           Description: RsClLime),
    (Value: $00669933;    Constant: 'clSeaGreen';       Description: RsClSeaGreen),
    (Value: $00999933;    Constant: 'clAqua';           Description: RsClAqua),
    (Value: $00FF6633;    Constant: 'clLightBlue';      Description: RsClLightBlue),
    (Value: $00800080;    Constant: 'clViolet';         Description: RsClViolet),
    (Value: $00999999;    Constant: 'clGray40';         Description: RsClGray40),

    (Value: $00FF00FF;    Constant: 'clPink';           Description: RsClPink),
    (Value: $0000CCFF;    Constant: 'clGold';           Description: RsClGold),
    (Value: $0000FFFF;    Constant: 'clYellow';         Description: RsClYellow),
    (Value: $0000FF00;    Constant: 'clBrightGreen';    Description: RsClBrightGreen),
    (Value: $00FFFF00;    Constant: 'clTurquoise';      Description: RsClTurquoise),
    (Value: $00F0CAA6;    Constant: 'clSkyBlue';        Description: RsClSkyBlue),
    (Value: $00663399;    Constant: 'clPlum';           Description: RsClPlum),
    (Value: $00C0C0C0;    Constant: 'clGray25';         Description: RsClGray25),

    (Value: $00CC99FF;    Constant: 'clRose';           Description: RsClRose),
    (Value: $0099CCFF;    Constant: 'clTan';            Description: RsClTan),
    (Value: $0099FFFF;    Constant: 'clLightYellow';    Description: RsClLightYellow),
    (Value: $00CCFFCC;    Constant: 'clLightGreen';     Description: RsClLightGreen),
    (Value: $00FFFFCC;    Constant: 'clLightTurquoise'; Description: RsClLightTurquoise),
    (Value: $00FFCC99;    Constant: 'clPaleBlue';       Description: RsClPaleBlue),
    (Value: $00FF99CC;    Constant: 'clLavender';       Description: RsClLavender),
    (Value: $00FFFFFF;    Constant: 'clWhite';          Description: RsClWhite)
  );

  SysColorValues: array [0 .. SysColCount - 1] of TDefColorItem = (
    (Value: clScrollBar;           Constant: 'clScrollBar';           Description: RsClScrollBar),
    (Value: clBackground;          Constant: 'clBackground';          Description: RsClBackground),
    (Value: clActiveCaption;       Constant: 'clActiveCaption';       Description: RsClActiveCaption),
    (Value: clInactiveCaption;     Constant: 'clInactiveCaption';     Description: RsClInactiveCaption),
    (Value: clMenu;                Constant: 'clMenu';                Description: RsClMenu),
    (Value: clWindow;              Constant: 'clWindow';              Description: RsClWindow),
    (Value: clWindowFrame;         Constant: 'clWindowFrame';         Description: RsClWindowFrame),
    (Value: clMenuText;            Constant: 'clMenuText';            Description: RsClMenuText),
    (Value: clWindowText;          Constant: 'clWindowText';          Description: RsClWindowText),
    (Value: clCaptionText;         Constant: 'clCaptionText';         Description: RsClCaptionText),
    (Value: clActiveBorder;        Constant: 'clActiveBorder';        Description: RsClActiveBorder),
    (Value: clInactiveBorder;      Constant: 'clInactiveBorder';      Description: RsClInactiveBorder),
    (Value: clAppWorkSpace;        Constant: 'clAppWorkSpace';        Description: RsClAppWorkSpace),
    (Value: clHighlight;           Constant: 'clHighlight';           Description: RsClHighlight),
    (Value: clHighlightText;       Constant: 'clHighlightText';       Description: RsClHighlightText),
    (Value: clBtnFace;             Constant: 'clBtnFace';             Description: RsClBtnFace),
    (Value: clBtnShadow;           Constant: 'clBtnShadow';           Description: RsClBtnShadow),
    (Value: clGrayText;            Constant: 'clGrayText';            Description: RsClGrayText),
    (Value: clBtnText;             Constant: 'clBtnText';             Description: RsClBtnText),
    (Value: clInactiveCaptionText; Constant: 'clInactiveCaptionText'; Description: RsClInactiveCaptionText),
    (Value: clBtnHighlight;        Constant: 'clBtnHighlight';        Description: RsClBtnHighlight),
    (Value: cl3DDkShadow;          Constant: 'cl3DDkShadow';          Description: RsCl3DDkShadow),
    (Value: cl3DLight;             Constant: 'cl3DLight';             Description: RsCl3DLight),
    (Value: clInfoText;            Constant: 'clInfoText';            Description: RsClInfoText),
    (Value: clInfoBk;              Constant: 'clInfoBk';              Description: RsClInfoBk),

    (Value: clGradientActiveCaption;   Constant: 'clGradientActiveCaption';  Description: RsGradientActiveCaption),
    (Value: clGradientInactiveCaption; Constant: 'clGradientInactiveCaption';Description: RsGradientInactiveCaption),
    (Value: clHotLight;                Constant: 'clHotLight';               Description: RsHotLight),
    (Value: clMenuBar;                 Constant: 'clMenuBar';                Description: RsMenuBar),
    (Value: clMenuHighlight;           Constant: 'clMenuHighlight';          Description: RsMenuHighlight)
  );

type
  TJvSizeRect = packed record
    Top: Integer;
    Left: Integer;
    Width: Integer;
    Height: Integer;
  end;

  TJvMessage = packed record
    Msg: Integer;
    case Integer of
    0:
     (
      WParam: Integer;
      LParam: Integer;
      Result: Integer;
     );
    1:
     (
      WParamLo: Word;
      WParamHi: Word;
      LParamLo: Word;
      LParamHi: Word;
      ResultLo: Word;
      ResultHi: Word;
     );
    2:
     ( // WM_NOPARAMS
      Unused: array[0..3] of Word;
      Handled: LongBool;  // "Result"
     );
    3:
     ( // WM_SCROLL
      Pos: Integer;         // WParam
      ScrollCode: Integer;  // LParam
     );
    4:
     ( // WM_TIMER
      TimerID: Integer;     // WParam
      TimerProc: TTimerProc;// LParam
     );
    5:
     ( // WM_MOUSEACTIVATE
      TopLevel: HWND;       // WParam
      HitTestCode: Word;    // LParamLo
      MouseMsg: Word;       // LParamHi
     );
    6:
     ( // WM_MOUSE(WHEEL) | WM_MOVE
      case Integer of
      0:
       ( // WM_MOUSE
        Keys: Integer;     // WParam
        // LParam: Pos | (XPos, YPos)
        case Integer of
        0:
         (
          Position: TSmallPoint;
         );
        1:
         (
          XPos: Smallint;
          YPos: Smallint;
         )
       );
      1:
       ( // WM_MOUSEWHEEL
        WheelDelta: Integer; // WParam
       );
     );
    7:
     ( // WM_ACTIVATE
      Active: Word; { WA_INACTIVE, WA_ACTIVE, WA_CLICKACTIVE } // WParamLo
      Minimized: WordBool;  // WParamHi
      ActiveWindow: HWND;   // LParam
     );

    8:
     ( // WM_COMMAND
      ItemID: Word;         // WParamLo
      NotifyCode: Word;     // WParamHi
      Ctl: HWND;            // LParam
     );
    9:
     ( // WM_GETICON
      BigIcon: LongBool;
     );
    10:
     ( // CM_(FOCUS|CONTROL)CHANGED  | CM_HINTSHOW
      Reserved: Integer;      // WParam
      case Integer of
        0:
         ( // CM_(CONTROL)CHANGED
          Child: TControl;    // LParam
         );
        1:
         ( // CM_FOCUSCHANGED | CM_FORCESIZE }
          Sender: TControl;   // LParam
         );
        2:
         ( //CM_HINTSHOW
          HintInfo: PHintInfo;
         )
     );
    11:
     ( // CM_CONTROLLISTCHANGE | CM_(CONTROL)CHANGED (| CM_BUTTONPRESSED for clx)
      Control: TControl;    // WParam
      case Integer of
        0:
         ( // CM_(CONTROL)CHANGED
          Inserting: LongBool;    // LParam
         );
        1: // CM_BUTTONPRESSED (clx)
         (
          Index: Integer;
         )
     );
    12:
     ( // CM_HINTSHOWPAUSE
      WasActive: LongBool;
      Pause: PInteger;
     );
    13:
     ( // WM_KEY
      CharCode: Word;
      NotUsed: Word;
      KeyData: Integer;
     );
    14:
     ( // WM_GETTEXT
      TextMax: Integer;
      Text: PChar
     );
    15:
     ( // WM_ERASEBKGND | WM_PAINT
      DC: HDC;
     );
    16:
     ( // WM_KILLFOCUS
      FocusedWnd: HWND;
     );
    17:
     (
      NewSize: TSmallPoint; //CM_FORCESIZE wParam
     );
    18:
     ( { alternative naming for VCL CM_BUTTONPRESSED }
      GroupIndex: Integer;
      Button: TControl;
     );
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jvcl.svn.sourceforge.net/svnroot/jvcl/tags/JVCL3_39/run/JvTypes.pas $';
    Revision: '$Revision: 12461 $';
    Date: '$Date: 2009-08-14 19:21:33 +0200 (ven., 14 aoΓ»t 2009) $';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

{ TJvPersistent }
constructor TJvPersistent.Create(AOwner: TPersistent);
begin
  if AOwner is TComponent then
    inherited Create(AOwner as TComponent)
  else
    inherited Create(nil);
  SetSubComponent(True);

  FOwner := AOwner;
end;

type
  TPersistentAccessProtected = class(TPersistent);

function TJvPersistent.GetNamePath: string;
var
  S: string;
  lOwner: TPersistent;
begin
  Result := inherited GetNamePath;
  lOwner := GetOwner;   //Resturn Nested NamePath
  if (lOwner <> nil)
    and ( (csSubComponent in TComponent(lOwner).ComponentStyle)
         or (TPersistentAccessProtected(lOwner).GetOwner <> nil)
        )
   then
  begin
    S := lOwner.GetNamePath;
    if S <> '' then
      Result := S + '.' + Result;
  end;
end;

function TJvPersistent.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TJvPersistent._GetOwner: TPersistent;
begin
  Result := GetOwner;
end;

{ TJvPersistentProperty }

procedure TJvPersistentProperty.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TJvPersistentProperty.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TJvPersistentProperty.ChangedProperty(const PropName: string);
begin
  if Assigned(FOnChangedProperty) then
    FOnChangedProperty(Self, PropName);
end;

procedure TJvPersistentProperty.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TJvPersistentProperty.ChangingProperty(const PropName: string);
begin
  if Assigned(FOnChangingProperty) then
    FOnChangingProperty(Self, PropName);
end;

procedure TJvPersistentProperty.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    SetUpdateState(False);
end;

procedure TJvPersistentProperty.SetUpdateState(aUpdating: Boolean);
begin
  if aUpdating then
    Changing
  else
    Changed;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
