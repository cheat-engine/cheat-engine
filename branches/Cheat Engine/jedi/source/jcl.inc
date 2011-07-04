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
{ The Original Code is jcl.inc                                                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel.                       }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel                                                                              }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones                                                                                     }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This include file defines various JCL specific defines. The more generic defines are defined in  }
{ the jedi.inc file which is shared with the JEDI VCL.                                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-01-30 18:34:47 +0100 (mer., 30 janv. 2008)                         $ }
{ Revision:      $Rev:: 2331                                                                     $ }
{ Author:        $Author:: marcovtje                                                             $ }
{                                                                                                  }
{**************************************************************************************************}

{$B-} // Boolean shortcut evaluation
{$H+} // Long strings
{$J-} // Read-only typed constants
{$T-} // Type checked pointers off

{$I jedi.inc} // Pull in the JCL/J-VCL shared directives

{$IFNDEF JEDI_INC}
ALERT_jedi_inc_incompatible
// secure against old versions of jedi.inc.
{$ENDIF ~JEDI_INC}

{$IFNDEF JCLINSTALL}
  {$IFDEF CLR}
    {----------------------------}
    { BDS                        }
    {----------------------------}
    {$IFDEF BDS3}
      {$I jcld9.net.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS3}
    {----------------------------}
    {$IFDEF BDS4}
      {$I jcld10.net.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS4}
    {----------------------------}
    {$IFDEF BDS5}
      {$I jcld11.net.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS5}
    {----------------------------}
  {$ELSE ~CLR}
    {----------------------------}
    { Kylix                      }
    {----------------------------}
    // KYLIX3 is not defined (version numbers comparisons are wrong)
    // won't fix because of possible bug with floating point comparisons
    // at compile time
    {$IFDEF KYLIX}
      {$IFDEF BCB}
        {$I jclkc3.inc}
      {$ELSE ~BCB}
        {$I jclkd3.inc}
      {$ENDIF ~BCB}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF KYLIX}
    {----------------------------}
    { C++Builder                 }
    {----------------------------}
    {$IFDEF BCB5}
      {$I jclc5.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BCB5}
    {----------------------------}
    {$IFDEF BCB6}
      {$I jclc6.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BCB6}
    {----------------------------}
    { Delphi                     }
    {----------------------------}
    {$IFDEF DELPHI5}
      {$I jcld5.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF DELPIH5}
    {----------------------------}
    {$IFDEF DELPHI6}
      {$I jcld6.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF DELPIH6}
    {----------------------------}
    {$IFDEF DELPHI7}
      {$I jcld7.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF DELPIH7}  
    {----------------------------}
    { BDS                        }
    {----------------------------}
    // BDS 1 and BDS 2 have the same version numbers for their native compilers
    // no compiler defines are used for BDS 1 and BDS 2
    {$IFDEF BDS1}
      //{$I jclcs1.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS1}
    {----------------------------}
    {$IFDEF BDS2}
      //{$I jcld8.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS2}
    {----------------------------}
    {$IFDEF BDS3}
      {$I jcld9.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS3}
    {----------------------------}
    {$IFDEF BDS4}
      {$I jcld10.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS4}
    {----------------------------}
    {$IFDEF BDS5}
      {$I jcld11.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF BDS5}
    {----------------------------}
    {$IFDEF FPC}
      {$I jclfpc.inc}
      {$DEFINE JCL_CONFIGURED}
    {$ENDIF FPC}
    {----------------------------}
  {$ENDIF ~CLR}

  // check configuration
  {$IFNDEF JCL_CONFIGURED}
    {$IFDEF SUPPORTS_COMPILETIME_MESSAGES}
      {$MESSAGE FATAL 'Your Delphi/BCB version is not supported by this JCL version!'}
    {$ELSE}
      'Your Delphi/BCB version is not supported by this JCL version!'
    {$ENDIF SUPPORTS_COMPILETIME_MESSAGES}
  {$ENDIF !JCL_CONFIGURED}

{$ENDIF ~JCLINSTALL}

// Math precision selection, mutually exclusive
{$IFDEF MATH_EXTENDED_PRECISION}
  {$UNDEF MATH_SINGLE_PRECISION}
  {$UNDEF MATH_DOUBLE_PRECISION}
{$ENDIF}
{$IFDEF MATH_DOUBLE_PRECISION}
  {$UNDEF MATH_SINGLE_PRECISION}
  {$UNDEF MATH_EXTENDED_PRECISION}
{$ENDIF}
{$IFDEF MATH_SINGLE_PRECISION}
  {$UNDEF MATH_DOUBLE_PRECISION}
  {$UNDEF MATH_EXTENDED_PRECISION}
{$ENDIF}

{$IFNDEF MATH_EXTENDED_PRECISION}
  {$IFNDEF MATH_DOUBLE_PRECISION}
    {$IFNDEF MATH_SINGLE_PRECISION}
      {$DEFINE MATH_EXTENDED_PRECISION}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

// PCRE options, mutually exclusive
{$IFDEF PCRE_STATICLINK}
  {$UNDEF PCRE_LINKDLL}
  {$UNDEF PCRE_LINKONREQUEST}
{$ENDIF PCRE_STATICLINK}
{$IFDEF PCRE_LINKDLL}
  {$UNDEF PCRE_LINKONREQUEST}
{$ENDIF PCRE_LINKDLL}

{$IFNDEF PCRE_STATICLINK}
  {$IFNDEF PCRE_LINKDLL}
    {$IFNDEF PCRE_LINKONREQUEST}
      {$DEFINE PCRE_LINKONREQUEST}
    {$ENDIF ~PCRE_LINKONREQUEST}
  {$ENDIF ~PCRE_LINKDLL}
{$ENDIF ~PCRE_STATICLINK}

{$IFNDEF PCRE_STATICLINK}
  {$DEFINE PCRE_EXPORT_CDECL}
{$ENDIF ~PCRE_STATICLINK}

// BZip2 options
{$IFDEF BZIP2_STATICLINK}
  {$UNDEF BZIP2_LINKDLL}
  {$UNDEF BZIP2_LINKONREQUEST}
{$ENDIF BZIP2_STATICLINK}
{$IFDEF BZIP2_LINKDLL}
  {$UNDEF BZIP2_LINKONREQUEST}
{$ENDIF BZIP2_LINKDLL}

{$IFNDEF BZIP2_STATICLINK}
  {$IFNDEF BZIP2_LINKDLL}
    {$IFNDEF BZIP2_LINKONREQUEST}
      {$DEFINE BZIP2_LINKONREQUEST}
    {$ENDIF ~BZIP2_LINKONREQUEST}
  {$ENDIF ~BZIP2_LINKDLL}
{$ENDIF ~BZIP2_STATICLINK}

{$IFDEF BZIP2_STATICLINK}
  {$DEFINE BZIP2_EXPORT_STDCALL}
{$ENDIF BZIP2_STATICLINK}

{$IFDEF BZIP2_LINKDLL}
  {$DEFINE BZIP2_EXPORT_CDECL}
{$ENDIF BZIP2_LINKDLL}

{$IFDEF BZIP2_LINKONREQUEST}
  {$DEFINE BZIP2_EXPORT_CDECL}
{$ENDIF BZIP2_LINKONREQUEST}

{$IFDEF UNICODE_RAW_DATA}
  {$UNDEF UNICODE_ZLIB_DATA}
  {$UNDEF UNICODE_BZIP2_DATA}
{$ENDIF UNICODE_RAW_DATA}

{$IFDEF UNICODE_ZLIB_DATA}
  {$UNDEF UNICODE_RAW_DATA}
  {$UNDEF UNICODE_BZIP2_DATA}
{$ENDIF UNICODE_ZLIB_DATA}

{$IFNDEF UNICODE_ZLIB_DATA}
  {$IFNDEF UNICODE_BZIP2_DATA}
    {$DEFINE UNICODE_RAW_DATA}
  {$ENDIF ~UNICODE_BZIP2_DATA}
{$ENDIF ~UNICODE_ZLIB_DATA}

{$IFDEF CONTAINER_ANSISTR}
  {$UNDEF CONTAINER_WIDESTR}
  {$UNDEF CONTAINER_NOSTR}
{$ENDIF CONTAINER_ANSISTR}

{$IFDEF CONTAINER_WIDESTR}
  {$UNDEF CONTAINER_NOSTR}
{$ENDIF CONTAINER_WIDESTR}

{$IFNDEF CONTAINER_ANSISTR}
  {$IFNDEF CONTAINER_WIDESTR}
    {$IFNDEF CONTAINER_NOSTR}
      {$DEFINE CONTAINER_ANSISTR}
    {$ENDIF ~CONTAINER_NOSTR}
  {$ENDIF ~CONTAINER_WIDESTR}
{$ENDIF ~CONTAINER_ANSISTR}

// 7zip options
{$IFDEF 7ZIP_STATICLINK}
  {$UNDEF 7ZIP_LINKDLL}
  {$UNDEF 7ZIP_LINKONREQUEST}
{$ENDIF 7ZIP_STATICLINK}

{$IFDEF 7ZIP_LINKDLL}
  {$UNDEF 7ZIP_LINKONREQUEST}
{$ENDIF 7ZIP_LINKDLL}

{$IFNDEF 7ZIP_STATICLINK}
  {$IFNDEF 7ZIP_LINKDLL}
    {$IFNDEF 7ZIP_LINKONREQUEST}
      {$DEFINE 7ZIP_LINKONREQUEST}
    {$ENDIF ~7ZIP_LINKONREQUEST}
  {$ENDIF ~7ZIP_LINKDLL}
{$ENDIF ~7ZIP_STATICLINK}

{$IFDEF SUPPORTS_UNSAFE_WARNINGS}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFNDEF DROP_OBSOLETE_CODE}
  {$IFNDEF JCLINSTALL}
    {$DEFINE KEEP_DEPRECATED}
  {$ENDIF}
{$ENDIF}

{$IFDEF CLR}
 {$WARN UNSAFE_TYPE ON}
 {$WARN UNSAFE_CODE ON}
 {$WARN UNSAFE_CAST ON}
 {$WARN UNIT_PLATFORM OFF}

 {$DEFINE MSWINDOWS}
 {$DEFINE PIC}
 {$DEFINE PUREPASCAL}
{$ENDIF CLR}
