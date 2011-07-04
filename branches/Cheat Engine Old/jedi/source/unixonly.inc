{$IFNDEF UNIXONLY_INC}
{$DEFINE UNIXONLY_INC}

{**************************************************************************************************}
{                                                                                                  }
{  The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");}
{  you may not use this file except in compliance with the License. You may obtain a copy of the   }
{  License at http://www.mozilla.org/MPL/                                                          }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing rights }
{  and limitations under the License.                                                              }
{                                                                                                  }
{  The Original Code is: unixonly.inc, released on 2004-06-21.                                     }
{                                                                                                  }
{  You may retrieve the latest version of this file at the JCL home page,                          }
{  located at http://jcl.sourceforge.net/                                                          }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

{$IFNDEF JEDI_INC}
ALERT_jedi_inc_missing
// This inc file depends on jedi.inc which has to
// be included first (usually indirectly through
// the inclusion of jcl.inc).
{$ENDIF ~JEDI_INC}

// Suppress platform warnings which are irrelevant
// because the including unit can only be compiled
// for Unix platforms anyway.

{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

// Cause a compilation error for non-Unix platforms.

{$IFNDEF UNIX}
  {$IFDEF SUPPORTS_COMPILETIME_MESSAGES}
    {$MESSAGE FATAL 'This unit is only supported on Unix!'}
  {$ELSE}
    'This unit is only supported on Unix!'
  {$ENDIF SUPPORTS_COMPILETIME_MESSAGES}
{$ENDIF ~UNIX}

{$ENDIF ~UNIXONLY_INC}
