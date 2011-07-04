unit JvConsts;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Forms, Controls, Graphics, TypInfo,LMessages;

const
  { JvEditor }
  JvEditorCompletionChars = #8'0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';

  { Various units }
  DigitSymbols = ['0'..'9'];
  SignSymbols = ['+', '-'];
  IdentifierUppercaseLetters = ['A'..'Z'];
  IdentifierLowercaseLetters = ['a'..'z'];
  HexadecimalUppercaseLetters = ['A'..'F'];
  HexadecimalLowercaseLetters = ['a'..'f'];
  IdentifierLetters = IdentifierUppercaseLetters + IdentifierLowercaseLetters;
  IdentifierFirstSymbols = ['_'] + IdentifierLetters;
  IdentifierSymbols = IdentifierFirstSymbols + DigitSymbols;
  HexadecimalSymbols = DigitSymbols + HexadecimalUppercaseLetters + HexadecimalLowercaseLetters;

  {$IFDEF DELPHI6}
  SDelphiKey = 'Software\Borland\Delphi\6.0';
  {$ENDIF DELPHI6}
  {$IFDEF BCB6}
  SDelphiKey = 'Software\Borland\C++Builder\6.0';
  {$ENDIF BCB6}
  {$IFDEF DELPHI7}
  SDelphiKey = 'Software\Borland\Delphi\7.0';
  {$ENDIF DELPHI7}
  {$IFDEF DELPHI8}
  SDelphiKey = 'Software\Borland\BDS\2.0';
  {$ENDIF DELPHI8}
  {$IFDEF DELPHI9}
  SDelphiKey = 'Software\Borland\BDS\3.0';
  {$ENDIF DELPHI9}
  {$IFDEF DELPHI10}
  SDelphiKey = 'Software\Borland\BDS\4.0';
  {$ENDIF DELPHI10}
  {$IFDEF DELPHI11}
  SDelphiKey = 'Software\Borland\BDS\5.0';
  {$ENDIF DELPHI11}
  {$IFDEF DELPHI12}
  SDelphiKey = 'Software\CodeGear\BDS\6.0';
  {$ENDIF DELPHI12}
  {$IFDEF DELPHI14}
  SDelphiKey = 'Software\CodeGear\BDS\7.0';
  {$ENDIF DELPHI14}
  { JvDataProvider constants }
  { Consumer attributes }
  DPA_RenderDisabledAsGrayed = 1;
  DPA_RendersSingleItem = 2;
  DPA_ConsumerDisplaysList = 3;

  { Command message constants. Define the JVCL base at 512 from the command message base. This gives the VCL room to
    grow another 430 or so messages before interfering with us. It will give us room for 512 constants before we'd
    interfere with the action manager constants. }
  CM_JVBASE = CM_BASE + $0200;

  { Define command message that did not exist in earlier VCL versions }
  {$IFNDEF COMPILER9_UP}
  CM_INVALIDATEDOCKHOST = CM_BASE + 70;
  {$ENDIF !COMPILER9_UP}

  { Command message for JvSpeedbar editor }
  CM_SPEEDBARCHANGED = CM_JVBASE + $000;
  { Command message for TJvSpeedButton }
  CM_JVBUTTONPRESSED = CM_JVBASE + $001;
  { Command messages for TJvBackground }
  CM_RECREATEWINDOW  = CM_JVBASE + $002;
  CM_RELEASECLIENTLINK = CM_JVBASE + $003;
  { Command message used in JvProgressComponent }
  CM_SHOWEVENT = CM_JVBASE + $004;
  CM_CLOSEEVENT = CM_JVBASE + $005;
  { Command messages used in TJvColorButton, TJvOfficeColorButton and TJvCustomComboEdit }
  CM_POPUPCLOSEUP = CM_JVBASE + $006;
  CM_FIXCARETPOSITION = CM_JVBASE + $007;
  { Command messages used in JvButton }
  CM_FORCESIZE = CM_JVBASE + $008;
  { Command message used in JvEditorCommon }
  CM_RESETCAPTURECONTROL = CM_JVBASE + $009;
  { Command messages used in JvExControls }
  CM_PERFORM = CM_JVBASE + $00A; // LParam: "Msg: ^TMessage"
  CM_SETAUTOSIZE = CM_JVBASE + $00B; // WParam: "Value: Boolean"
  { Command messages used in JvLookOut }
  CM_IMAGESIZECHANGED = CM_JVBASE + $00C;
  CM_LEAVEBUTTON = CM_JVBASE + $00D;
  { Command messages used in JvNavigationPane }
  CM_PARENTSTYLEMANAGERCHANGE = CM_JVBASE + $00E;
  CM_PARENTSTYLEMANAGERCHANGED = CM_JVBASE + $00F;
  { Command messages used in JvOutlookBar }
  CM_CAPTION_EDITING = CM_JVBASE + $010;
  CM_CAPTION_EDIT_ACCEPT = CM_JVBASE + $011;
  CM_CAPTION_EDIT_CANCEL = CM_JVBASE + $012;
  { Command message used in JvTimeLine }
  CM_MOVEDRAGLINE = CM_JVBASE + $013;

  { Values for WParam for CM_SPEEDBARCHANGED message }
  SBR_CHANGED        = 0; { change buttons properties  }
  SBR_DESTROYED      = 1; { destroy SpeedBar           }
  SBR_BTNSELECT      = 2; { select button in SpeedBar  }
  SBR_BTNSIZECHANGED = 3; { button size changed        }

  { TBitmap.GetTransparentColor from GRAPHICS.PAS use this value }
  PaletteMask = $02000000;

  // (outchy) now used
  {$IFDEF COMPILER7_UP}
  // (outchy) it was defined as $000000FF
  DEFAULT_SYSCOLOR_MASK = clSystemColor;  // $FF000000
  {$ELSE}
  DEFAULT_SYSCOLOR_MASK = $80000000;
  {$ENDIF COMPILER7_UP}

  sLineBreakStr = string(sLineBreak); // "native string" line break constant
  sLineBreakLen = Length(sLineBreak);

  CrLf = #13#10;
  Cr = #13;
  Lf = #10;
  Backspace = #8;
  Tab = #9;
  Esc = #27;
  Del = #127;
  CtrlC = ^C;
  CtrlH = ^H;
  CtrlI = ^I;
  CtrlJ = ^J;
  CtrlM = ^M;
  CtrlV = ^V;
  CtrlX = ^X;
  {$IFDEF MSWINDOWS}
  RegPathDelim = '\';
  PathDelim = '\';
  DriveDelim = ':';
  PathSep = ';';
  AllFilePattern = '*.*';
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  RegPathDelim = '_';
  PathDelim = '/';
  AllFilePattern = '*';
  {$ENDIF UNIX}

  {const Separators is used in GetWordOnPos, JvUtils.ReplaceStrings and SubWord}
  Separators: TSysCharSet = [#00, ' ', '-', #13, #10, '.', ',', '/', '\', '#', '"', '''',
    ':', '+', '%', '*', '(', ')', ';', '=', '{', '}', '[', ']', '<', '>'];

  DigitChars = ['0'..'9'];

var
  crJVCLFirst: TCursor = 368;
  crMultiDragLink: TCursor = 368;
  crDragAlt: TCursor = 369;
  crMultiDragAlt: TCursor = 370;
  crMultiDragLinkAlt: TCursor = 371;
  crHand: TCursor = 372;
  crDragHand: TCursor = 373;
  // this should be incremented to always contain the last default JVCL cursor index
  crJVCLLast: TCursor = 373;

const
  ROP_DSPDxax = $00E20746;

const
  FOURCC_ACON = 'ACON';
  FOURCC_IART = 'IART';
  FOURCC_INAM = 'INAM';
  FOURCC_INFO = 'INFO';
  FOURCC_LIST = 'LIST';
  FOURCC_RIFF = 'RIFF';
  FOURCC_anih = 'anih';
  FOURCC_fram = 'fram';
  FOURCC_icon = 'icon';
  FOURCC_rate = 'rate';
  FOURCC_seq  = 'seq ';

  AF_ICON     = $00000001;
  AF_SEQUENCE = $00000002;

const
  KeyboardShiftStates = [ssShift, ssAlt, ssCtrl];
  MouseShiftStates = [ssLeft, ssRight, ssMiddle, ssDouble];
  tkStrings: set of TTypeKind = [tkString, tkLString, {$IFDEF UNICODE} tkUString, {$ENDIF} tkWString];

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jvcl.svn.sourceforge.net/svnroot/jvcl/tags/JVCL3_39/run/JvConsts.pas $';
    Revision: '$Revision: 12481 $';
    Date: '$Date: 2009-08-26 10:39:55 +0200 (mer., 26 août 2009) $';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
