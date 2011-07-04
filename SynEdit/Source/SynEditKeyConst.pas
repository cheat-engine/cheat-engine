{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditKeyCmds.pas, released 2000-04-07.
The Original Code is based on the mwKeyCmds.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Brad Stowers.
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

$Id: SynEditKeyConst.pas,v 1.4 2003/04/30 12:59:45 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITKEYCONST}
unit SynEditKeyConst;
{$ENDIF}

{ This unit provides a translation of DELPHI and CLX key constants to
  more readable SynEdit constants }

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Qt;
{$ELSE}
  Windows;
{$ENDIF}

const

{$IFDEF SYN_CLX}

  SYNEDIT_RETURN    = KEY_RETURN;
  SYNEDIT_ESCAPE    = KEY_ESCAPE;
  SYNEDIT_SPACE     = KEY_SPACE;
  SYNEDIT_PRIOR     = KEY_PRIOR;
  SYNEDIT_NEXT      = KEY_NEXT;
  SYNEDIT_END       = KEY_END;
  SYNEDIT_HOME      = KEY_HOME;
  SYNEDIT_UP        = KEY_UP;
  SYNEDIT_DOWN      = KEY_DOWN;
  SYNEDIT_BACK      = KEY_BACKSPACE;
  SYNEDIT_LEFT      = KEY_LEFT;
  SYNEDIT_RIGHT     = KEY_RIGHT;
  SYNEDIT_MENU      = KEY_MENU;
  SYNEDIT_CONTROL   = KEY_CONTROL;
  SYNEDIT_SHIFT     = KEY_SHIFT;
  SYNEDIT_F1        = KEY_F1;
  SYNEDIT_F2        = KEY_F2;
  SYNEDIT_F3        = KEY_F3;
  SYNEDIT_F4        = KEY_F4;
  SYNEDIT_F5        = KEY_F5;
  SYNEDIT_F6        = KEY_F6;
  SYNEDIT_F7        = KEY_F7;
  SYNEDIT_F8        = KEY_F8;
  SYNEDIT_F9        = KEY_F9;
  SYNEDIT_F10       = KEY_F10;
  SYNEDIT_F11       = KEY_F11;
  SYNEDIT_F12       = KEY_F12;
  SYNEDIT_F13       = KEY_F13;
  SYNEDIT_F14       = KEY_F14;
  SYNEDIT_F15       = KEY_F15;
  SYNEDIT_F16       = KEY_F16;
  SYNEDIT_F17       = KEY_F17;
  SYNEDIT_F18       = KEY_F18;
  SYNEDIT_F19       = KEY_F19;
  SYNEDIT_F20       = KEY_F20;
  SYNEDIT_F21       = KEY_F21;
  SYNEDIT_F22       = KEY_F22;
  SYNEDIT_F23       = KEY_F23;
  SYNEDIT_F24       = KEY_F24;
  SYNEDIT_PRINT     = KEY_PRINT;
  SYNEDIT_INSERT    = KEY_INSERT;
  SYNEDIT_DELETE    = KEY_DELETE;
  SYNEDIT_NUMPAD0   = KEY_0;
  SYNEDIT_NUMPAD1   = KEY_1;
  SYNEDIT_NUMPAD2   = KEY_2;
  SYNEDIT_NUMPAD3   = KEY_3;
  SYNEDIT_NUMPAD4   = KEY_4;
  SYNEDIT_NUMPAD5   = KEY_5;
  SYNEDIT_NUMPAD6   = KEY_6;
  SYNEDIT_NUMPAD7   = KEY_7;
  SYNEDIT_NUMPAD8   = KEY_8;
  SYNEDIT_NUMPAD9   = KEY_9;
  SYNEDIT_MULTIPLY  = KEY_MULTIPLY;
  SYNEDIT_ADD       = KEY_PLUS;
  SYNEDIT_SEPARATOR = KEY_ENTER;
  SYNEDIT_SUBTRACT  = KEY_MINUS;
  SYNEDIT_DECIMAL   = KEY_PERIOD;
  SYNEDIT_DIVIDE    = KEY_SLASH;
  SYNEDIT_NUMLOCK   = KEY_NUMLOCK;
  SYNEDIT_SCROLL    = KEY_SCROLLLOCK;
  SYNEDIT_TAB       = KEY_TAB;
  SYNEDIT_CLEAR     = KEY_SYSREQ;
  SYNEDIT_PAUSE     = KEY_PAUSE;
  SYNEDIT_CAPITAL   = KEY_CAPSLOCK;
  
{$ELSE}

  SYNEDIT_RETURN    = VK_RETURN;
  SYNEDIT_ESCAPE    = VK_ESCAPE;
  SYNEDIT_SPACE     = VK_SPACE;
  SYNEDIT_PRIOR     = VK_PRIOR;
  SYNEDIT_NEXT      = VK_NEXT;
  SYNEDIT_END       = VK_END;
  SYNEDIT_HOME      = VK_HOME;
  SYNEDIT_UP        = VK_UP;
  SYNEDIT_DOWN      = VK_DOWN;
  SYNEDIT_BACK      = VK_BACK;
  SYNEDIT_LEFT      = VK_LEFT;
  SYNEDIT_RIGHT     = VK_RIGHT;
  SYNEDIT_MENU      = VK_MENU;
  SYNEDIT_CONTROL   = VK_CONTROL;
  SYNEDIT_SHIFT     = VK_SHIFT;
  SYNEDIT_F1        = VK_F1;
  SYNEDIT_F2        = VK_F2;
  SYNEDIT_F3        = VK_F3;
  SYNEDIT_F4        = VK_F4;
  SYNEDIT_F5        = VK_F5;
  SYNEDIT_F6        = VK_F6;
  SYNEDIT_F7        = VK_F7;
  SYNEDIT_F8        = VK_F8;
  SYNEDIT_F9        = VK_F9;
  SYNEDIT_F10       = VK_F10;
  SYNEDIT_F11       = VK_F11;
  SYNEDIT_F12       = VK_F12;
  SYNEDIT_F13       = VK_F13;
  SYNEDIT_F14       = VK_F14;
  SYNEDIT_F15       = VK_F15;
  SYNEDIT_F16       = VK_F16;
  SYNEDIT_F17       = VK_F17;
  SYNEDIT_F18       = VK_F18;
  SYNEDIT_F19       = VK_F19;
  SYNEDIT_F20       = VK_F20;
  SYNEDIT_F21       = VK_F21;
  SYNEDIT_F22       = VK_F22;
  SYNEDIT_F23       = VK_F23;
  SYNEDIT_F24       = VK_F24;
  SYNEDIT_PRINT     = VK_PRINT;
  SYNEDIT_INSERT    = VK_INSERT;
  SYNEDIT_DELETE    = VK_DELETE;
  SYNEDIT_NUMPAD0   = VK_NUMPAD0;
  SYNEDIT_NUMPAD1   = VK_NUMPAD1;
  SYNEDIT_NUMPAD2   = VK_NUMPAD2;
  SYNEDIT_NUMPAD3   = VK_NUMPAD3;
  SYNEDIT_NUMPAD4   = VK_NUMPAD4;
  SYNEDIT_NUMPAD5   = VK_NUMPAD5;
  SYNEDIT_NUMPAD6   = VK_NUMPAD6;
  SYNEDIT_NUMPAD7   = VK_NUMPAD7;
  SYNEDIT_NUMPAD8   = VK_NUMPAD8;
  SYNEDIT_NUMPAD9   = VK_NUMPAD9;
  SYNEDIT_MULTIPLY  = VK_MULTIPLY;
  SYNEDIT_ADD       = VK_ADD;
  SYNEDIT_SEPARATOR = VK_SEPARATOR;
  SYNEDIT_SUBTRACT  = VK_SUBTRACT;
  SYNEDIT_DECIMAL   = VK_DECIMAL;
  SYNEDIT_DIVIDE    = VK_DIVIDE;
  SYNEDIT_NUMLOCK   = VK_NUMLOCK;
  SYNEDIT_SCROLL    = VK_SCROLL;
  SYNEDIT_TAB       = VK_TAB;
  SYNEDIT_CLEAR     = VK_CLEAR;
  SYNEDIT_PAUSE     = VK_PAUSE;
  SYNEDIT_CAPITAL   = VK_CAPITAL;
  
{$ENDIF }

implementation

end.
