--Defines:

--checkbox state defines
cbUnchecked=0
cbChecked=1
cbGrayed=2


--onMouseEvent button defines:
mbLeft=0
mbRight=1
mbMiddle=2
mbExtra1=3
mbExtra2=4


--memo scrollbar defines
ssNone=0
ssHorizontal=1
ssVertical=2
ssBoth=3
ssAutoHorizontal=4
ssAutoVertical=5
ssAutoBoth=6


bsNone=0
bsSingle=1
bsSizeable=2
bsDialog=3
bsToolWindow=4
bsSizeToolWin=5



--scan types: (fast scan methods)
fsmNotAligned=0
fsmAligned=1
fsmLastDigits=2

--rounding types
rtRounded=0
rtExtremerounded=1
rtTruncated=2

--scan options
soUnknownValue=0
soExactValue=1
soValueBetween=2
soBiggerThan=3
soSmallerThan=4
soIncreasedValue=5
soIncreasedValueBy=6
soDecreasedValue=7
soDecreasedValueBy=8
soChanged=9
soUnchanged=10


--debug variables
--Breakpoint methods:
bpmInt3=0
bpmDebugRegister=1
bpmException=2


--Breakpoint triggers:
bptExecute=0
bptAccess=1
bptWrite=2

--breakpoint continue methods:
co_run=0
co_stepinto=1
co_stepover=2

--CloseAction types
caNone=0;
caHide=1;
caFree=2;
caMinimize=3;

--alignment types
alNone=0
alTop=1
alBottom=2
alLeft=3
alRight=4
alClient=5

--message dialog types
mtWarning=0
mtError=1
mtInformation=2
mtConfirmation=3

--message dialog button types
mbYes=0
mbNo=1
mbOK=2
mbCancel=3
mbAbort=4
mbRetry=5
mbIgnore=6
mbAll=7
mbNoToAll=8
mbYesToAll=9
mbHelp=10
mbClose=11


--message dialog results:
mrNone = 0;
mrOK = mrNone + 1
mrCancel = mrNone + 2
mrAbort = mrNone + 3
mrRetry = mrNone + 4
mrIgnore = mrNone + 5
mrYes = mrNone + 6
mrNo = mrNone + 7
mrAll = mrNone + 8
mrNoToAll = mrNone + 9
mrYesToAll = mrNone + 10
mrLast = mrYesToAll

--duplicate enum
dupIgnore = 0
dupAccept = 1
dupError = 2


--Variable types
vtByte=0
vtWord=1
vtDword=2
vtQword=3
vtSingle=4
vtDouble=5
vtString=6
vtUnicodeString=7 --Only used by autoguess
vtWideString=7
vtByteArray=8
vtBinary=9
vtAll=10
vtAutoAssembler=11
vtPointer=12 --Only used by autoguess and structures
vtCustom=13
vtGrouped=14

--Key codes
VK_LBUTTON = 1
VK_RBUTTON = 2
VK_CANCEL = 3
VK_MBUTTON = 4
VK_XBUTTON1 = 5
VK_XBUTTON2 = 6
VK_BACK = 8
VK_TAB = 9
VK_CLEAR = 12
VK_RETURN = 13
VK_SHIFT = 16
VK_CONTROL = 17
VK_MENU = 18
VK_PAUSE = 19
VK_CAPITAL = 20
VK_ESCAPE = 27
VK_SPACE = 32
VK_PRIOR = 33
VK_NEXT = 34
VK_END = 35
VK_HOME = 36
VK_LEFT = 37
VK_UP = 38
VK_RIGHT = 39
VK_DOWN = 40
VK_SELECT = 41
VK_PRINT = 42
VK_EXECUTE = 43
VK_SNAPSHOT = 44
VK_INSERT = 45
VK_DELETE = 46
VK_HELP = 47
VK_0 = 48
VK_1 = 49
VK_2 = 50
VK_3 = 51
VK_4 = 52
VK_5 = 53
VK_6 = 54
VK_7 = 55
VK_8 = 56
VK_9 = 57
VK_A = 65
VK_B = 66
VK_C = 67
VK_D = 68
VK_E = 69
VK_F = 70
VK_G = 71
VK_H = 72
VK_I = 73
VK_J = 74
VK_K = 75
VK_L = 76
VK_M = 77
VK_N = 78
VK_O = 79
VK_P = 80
VK_Q = 81
VK_R = 82
VK_S = 83
VK_T = 84
VK_U = 85
VK_V = 86
VK_W = 87
VK_X = 88
VK_Y = 89
VK_Z = 90
VK_LWIN = 91
VK_RWIN = 92
VK_APPS = 93
VK_NUMPAD0 = 96
VK_NUMPAD1 = 97
VK_NUMPAD2 = 98
VK_NUMPAD3 = 99
VK_NUMPAD4 = 100
VK_NUMPAD5 = 101
VK_NUMPAD6 = 102
VK_NUMPAD7 = 103
VK_NUMPAD8 = 104
VK_NUMPAD9 = 105
VK_MULTIPLY = 106
VK_ADD = 107
VK_SEPARATOR = 108
VK_SUBTRACT = 109
VK_DECIMAL = 110
VK_DIVIDE = 111
VK_F1 = 112
VK_F2 = 113
VK_F3 = 114
VK_F4 = 115
VK_F5 = 116
VK_F6 = 117
VK_F7 = 118
VK_F8 = 119
VK_F9 = 120
VK_F10 = 121
VK_F11 = 122
VK_F12 = 123
VK_F13 = 124
VK_F14 = 125
VK_F15 = 126
VK_F16 = 127
VK_F17 = 128
VK_F18 = 129
VK_F19 = 130
VK_F20 = 131
VK_F21 = 132
VK_F22 = 133
VK_F23 = 134
VK_F24 = 135
VK_NUMLOCK = 144
VK_SCROLL = 145
VK_LSHIFT = 160
VK_LCONTROL = 162
VK_LMENU = 164
VK_RSHIFT = 161
VK_RCONTROL = 163
VK_RMENU = 165    
VK_OEM1=0xBA
VK_OEM2=0xBF
VK_OEM3=0xc0


--shellExecute show defines:
SW_HIDE = 0;
SW_MAXIMIZE = 3;
SW_MINIMIZE = 6;
SW_NORMAL = 1;
SW_RESTORE = 9;
SW_SHOW = 5;
SW_SHOWDEFAULT = 10;
SW_SHOWMAXIMIZED = 3;
SW_SHOWMINIMIZED = 2;
SW_SHOWMINNOACTIVE = 7;
SW_SHOWNA = 8;
SW_SHOWNOACTIVATE = 4;
SW_SHOWNORMAL = 1;


--Pixelformat
pfDevice = 0
pf1bit = 1
pf4bit = 2
pf8bit = 3
pf15bit = 4
pf16bit = 5
pf24bit = 6
pf32bit = 7
pfCustom = 8

--Disassembler value type
dvtNone = 0
dvtAddress = 1
dvtValue = 2

--Dissectcode
jtCall = 0
jtUnconditional = 1
jtConditional = 2
jtMemory = 3

--RegisterSymbolLookupCallback
slStart = 0 --The very start of a symbol lookup. Before tokenization
slNotInt = 1 --Called when it has been determined it's not a hexadecimal only string. Before tokenization
slNotModule = 2 --Called when it has been determined the current token is not a modulename
slNotUserdefinedSymbol = 3 --Called when it has been determined it's not a userdefined symbol
slNotSymbol = 4 --Called when it has been determined it's not a symbol in the symbollist
slFailure = 5 --Called when it has no clue what the given string is  

poDesigned = 0
poDefault = 1
poDefaultPosOnly = 2
poDefaultSizeOnly = 3
poScreenCenter = 4
poDesktopCenter = 5
poMainFormCenter = 6
poOwnerFormCenter = 7

asrTop = 0
asrBottom = 1
asrCenter = 2
asrLeft = asrTop
asrRight = asrBottom

vsIcon = 0
vsSmallIcon = 1
vsList = 2
vsReport = 3

LWA_COLORKEY = 1
LWA_ALPHA = 2

GW_HWNDFIRST = 0
GW_HWNDLAST = 1
GW_HWNDNEXT = 2
GW_HWNDPREV = 3
GW_HWNDOWNER = 4
GW_OWNER = 4
GW_CHILD = 5;
GW_ENABLEDPOPUP = 6;

mrhToggleActivation=0
mrhToggleActivationAllowIncrease=1
mrhToggleActivationAllowDecrease=2
mrhActivate=3
mrhDeactivate=4
mrhSetValue=5
mrhIncreaseValue=6
mrhDecreaseValue=7

MOUSEEVENTF_MOVE      =0x0001
MOUSEEVENTF_LEFTDOWN  =0x0002
MOUSEEVENTF_LEFTUP    =0x0004
MOUSEEVENTF_RIGHTDOWN =0x0008
MOUSEEVENTF_RIGHTUP   =0x0010
MOUSEEVENTF_MIDDLEDOWN=0x0020
MOUSEEVENTF_MIDDLEUP  =0x0040
MOUSEEVENTF_XDOWN     =0x0080
MOUSEEVENTF_XUP       =0x0100
MOUSEEVENTF_WHEEL     =0x0800
MOUSEEVENTF_HWHEEL    =0x1000
MOUSEEVENTF_ABSOLUTE  =0x8000

--text to speech "Speak" params
SPF_DEFAULT	= 0
SPF_ASYNC	= ( 1 << 0 ) 
SPF_PURGEBEFORESPEAK	= ( 1 << 1 ) 
SPF_IS_FILENAME	= ( 1 << 2 ) 
SPF_IS_XML	= ( 1 << 3 ) 
SPF_IS_NOT_XML	= ( 1 << 4 ) 
SPF_PERSIST_XML	= ( 1 << 5 ) 
SPF_NLP_SPEAK_PUNC	= ( 1 << 6 ) 
SPF_PARSE_SAPI	= ( 1 << 7 ) 
SPF_PARSE_SSML	= ( 1 << 8 ) 
SPF_PARSE_AUTODETECT	= 0

wrSignaled=0
wrTimeout=1
wrAbandoned=2
wrError=3

fsSurface=0
fsBorder=1

cdsSelected=0
cdsGrayed=1
cdsDisabled=2
cdsChecked=3
cdsFocused=4
cdsDefault=5
cdsHot=6
cdsMarked=7
cdsIndeterminate=8

cdPrePaint=0
cdPostPaint=1
cdPreErase=2
cdPostErase=3

drBounds=0
drIcon=1
drLabel=2
drSelectBounds=3

fmCreate        = 0xff00
fmOpenRead      = 0x0000
fmOpenWrite     = 0x0001
fmOpenReadWrite = 0x0002

fmShareCompat   = 0x0000
fmShareExclusive= 0x0010
fmShareDenyWrite= 0x0020
fmShareDenyRead = 0x0030
fmShareDenyNone = 0x0040

crDefault     = 0
crNone        = -1
crArrow       = -2
crCross       = -3
crIBeam       = -4
crSize        = -22
crSizeNESW    = -6 
crSizeNS      = -7
crSizeNWSE    = -8
crSizeWE      = -9
crSizeNW      = -23
crSizeN       = -24
crSizeNE      = -25
crSizeW       = -26
crSizeE       = -27
crSizeSW      = -28
crSizeS       = -29
crSizeSE      = -30
crUpArrow     = -10
crHourGlass   = -11
crDrag        = -12
crNoDrop      = -13
crHSplit      = -14
crVSplit      = -15
crMultiDrag   = -16
crSQLWait     = -17
crNo          = -18
crAppStart    = -19
crHelp        = -20
crHandPoint   = -21
crSizeAll     = -22   

--colors
  CLR_NONE = 0xFFFFFFFF
  CLR_DEFAULT = 0xFF000000
  CLR_INVALID = 0xFFFFFFFF

  COLOR_SCROLLBAR = 0
  COLOR_BACKGROUND = 1
  COLOR_ACTIVECAPTION = 2
  COLOR_INACTIVECAPTION = 3
  COLOR_MENU = 4
  COLOR_WINDOW = 5
  COLOR_WINDOWFRAME = 6
  COLOR_MENUTEXT = 7
  COLOR_WINDOWTEXT = 8
  COLOR_CAPTIONTEXT = 9
  COLOR_ACTIVEBORDER = 10
  COLOR_INACTIVEBORDER = 11
  COLOR_APPWORKSPACE = 12
  COLOR_HIGHLIGHT = 13
  COLOR_HIGHLIGHTTEXT = 14
  COLOR_BTNFACE = 15
  COLOR_BTNSHADOW = 16
  COLOR_GRAYTEXT = 17
  COLOR_BTNTEXT = 18
  COLOR_INACTIVECAPTIONTEXT = 19
  COLOR_BTNHIGHLIGHT = 20
  COLOR_3DDKSHADOW = 21
  COLOR_3DLIGHT = 22
  COLOR_INFOTEXT = 23
  COLOR_INFOBK = 24
  COLOR_HOTLIGHT = 26
  COLOR_GRADIENTACTIVECAPTION = 27
  COLOR_GRADIENTINACTIVECAPTION = 28
  COLOR_MENUHILIGHT = 29
  COLOR_MENUBAR = 30

  COLOR_FORM = 31

  COLOR_ENDCOLORS = COLOR_FORM

  COLOR_DESKTOP = COLOR_BACKGROUND
  COLOR_3DFACE = COLOR_BTNFACE
  COLOR_3DSHADOW = COLOR_BTNSHADOW
  COLOR_3DHIGHLIGHT = COLOR_BTNHIGHLIGHT
  COLOR_3DHILIGHT = COLOR_BTNHIGHLIGHT
  COLOR_BTNHILIGHT = COLOR_BTNHIGHLIGHT

  MAX_SYS_COLORS = COLOR_ENDCOLORS
  SYS_COLOR_BASE = 0x80000000

  clBlack   = 0x000000
  clMaroon  = 0x000080
  clGreen   = 0x008000
  clOlive   = 0x008080
  clNavy    = 0x800000
  clPurple  = 0x800080
  clTeal    = 0x808000
  clGray    = 0x808080
  clSilver  = 0xC0C0C0
  clRed     = 0x0000FF
  clLime    = 0x00FF00
  clYellow  = 0x00FFFF
  clBlue    = 0xFF0000
  clFuchsia = 0xFF00FF
  clAqua    = 0xFFFF00
  clLtGray  = 0xC0C0C0 -- clSilver alias
  clDkGray  = 0x808080 -- clGray alias
  clWhite   = 0xFFFFFF
  StandardColorsCount = 16

  -- extended colors
  clMoneyGreen = 0xC0DCC0
  clSkyBlue    = 0xF0CAA6
  clCream      = 0xF0FBFF
  clMedGray    = 0xA4A0A0
  ExtendedColorCount = 4

  -- special colors
  clNone    = 0x1FFFFFFF
  clDefault = 0x20000000

  -- system colors
  clScrollBar               = SYS_COLOR_BASE|COLOR_SCROLLBAR
  clBackground              = SYS_COLOR_BASE|COLOR_BACKGROUND
  clActiveCaption           = SYS_COLOR_BASE|COLOR_ACTIVECAPTION
  clInactiveCaption         = SYS_COLOR_BASE|COLOR_INACTIVECAPTION
  clMenu                    = SYS_COLOR_BASE|COLOR_MENU
  clWindow                  = SYS_COLOR_BASE|COLOR_WINDOW
  clWindowFrame             = SYS_COLOR_BASE|COLOR_WINDOWFRAME
  clMenuText                = SYS_COLOR_BASE|COLOR_MENUTEXT
  clWindowText              = SYS_COLOR_BASE|COLOR_WINDOWTEXT
  clCaptionText             = SYS_COLOR_BASE|COLOR_CAPTIONTEXT
  clActiveBorder            = SYS_COLOR_BASE|COLOR_ACTIVEBORDER
  clInactiveBorder          = SYS_COLOR_BASE|COLOR_INACTIVEBORDER
  clAppWorkspace            = SYS_COLOR_BASE|COLOR_APPWORKSPACE
  clHighlight               = SYS_COLOR_BASE|COLOR_HIGHLIGHT
  clHighlightText           = SYS_COLOR_BASE|COLOR_HIGHLIGHTTEXT
  clBtnFace                 = SYS_COLOR_BASE|COLOR_BTNFACE
  clBtnShadow               = SYS_COLOR_BASE|COLOR_BTNSHADOW
  clGrayText                = SYS_COLOR_BASE|COLOR_GRAYTEXT
  clBtnText                 = SYS_COLOR_BASE|COLOR_BTNTEXT
  clInactiveCaptionText     = SYS_COLOR_BASE|COLOR_INACTIVECAPTIONTEXT
  clBtnHighlight            = SYS_COLOR_BASE|COLOR_BTNHIGHLIGHT
  cl3DDkShadow              = SYS_COLOR_BASE|COLOR_3DDKSHADOW
  cl3DLight                 = SYS_COLOR_BASE|COLOR_3DLIGHT
  clInfoText                = SYS_COLOR_BASE|COLOR_INFOTEXT
  clInfoBk                  = SYS_COLOR_BASE|COLOR_INFOBK

  clHotLight                = SYS_COLOR_BASE|COLOR_HOTLIGHT
  clGradientActiveCaption   = SYS_COLOR_BASE|COLOR_GRADIENTACTIVECAPTION
  clGradientInactiveCaption = SYS_COLOR_BASE|COLOR_GRADIENTINACTIVECAPTION
  clMenuHighlight           = SYS_COLOR_BASE|COLOR_MENUHILIGHT
  clMenuBar                 = SYS_COLOR_BASE|COLOR_MENUBAR
  clForm                    = SYS_COLOR_BASE|COLOR_FORM

  -- synonyms: do not show them in color lists
  clColorDesktop            = SYS_COLOR_BASE|COLOR_DESKTOP
  cl3DFace                  = SYS_COLOR_BASE|COLOR_3DFACE
  cl3DShadow                = SYS_COLOR_BASE|COLOR_3DSHADOW
  cl3DHiLight               = SYS_COLOR_BASE|COLOR_3DHIGHLIGHT
  clBtnHiLight              = SYS_COLOR_BASE|COLOR_BTNHILIGHT

  clFirstSpecialColor = clBtnHiLight

  clMask = clWhite
  clDontMask = clBlack


--pen mode
pmBlack=0
pmWhite=1
pmNop=2
pmNot=3
pmCopy=4
pmNotCopy=5
pmMergePenNot=6
pmMaskPenNot=7
pmMergeNotPen=8
pmMaskNotPen=9
pmMerge=10
pmNotMerge=11
pmMask=12
pmNotMask=13
pmXor=14
pmNotXor=15

tbpsNone=0
tbpsIndeterminate=1
tbpsNormal=2
tbpsError=3
tbpsPaused=4
