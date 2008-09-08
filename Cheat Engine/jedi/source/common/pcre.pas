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
{ The Original Code is JclPRCE.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Peter Thornqvist.                                  }
{ Portions created by Peter Thornqvist are Copyright (C) of Peter Thornqvist. All rights reserved. }
{ Portions created by University of Cambridge are                                                  }
{ Copyright (C) 1997-2001 by University of Cambridge.                                              }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Robert Rossmair (rrossmair)                                                                    }
{   Mario R. Carro                                                                                 }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{ The latest release of PCRE is always available from                                              }
{ ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/pcre-xxx.tar.gz                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Header conversion of pcre.h                                                                      }
{                                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-07-20 17:17:43 +0200 (dim., 20 juil. 2008)                         $ }
{ Revision:      $Rev:: 2396                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit pcre;

{$I jcl.inc}

interface

(*************************************************
*       Perl-Compatible Regular Expressions      *
*************************************************)

{$WEAKPACKAGEUNIT ON}

// (p3) this is the switch to change between static and dynamic linking.
// It is set to dynamic by default. To disable simply insert a '.' before the '$'
//
// NOTE: if you enable static linking of DLL, this means that the pcre.dll *must*
// be in the users path or an AV will occur at startup

(*$HPPEMIT '#include "pcre.h"'*)

const
  MAX_PATTERN_LENGTH = $10003;
  {$EXTERNALSYM MAX_PATTERN_LENGTH}
  MAX_QUANTIFY_REPEAT = $10000;
  {$EXTERNALSYM MAX_QUANTIFY_REPEAT}
  MAX_CAPTURE_COUNT = $FFFF;
  {$EXTERNALSYM MAX_CAPTURE_COUNT}
  MAX_NESTING_DEPTH = 200;
  {$EXTERNALSYM MAX_NESTING_DEPTH}

const
  (* Options *)
  PCRE_CASELESS = $00000001;
  {$EXTERNALSYM PCRE_CASELESS}
  PCRE_MULTILINE = $00000002;
  {$EXTERNALSYM PCRE_MULTILINE}
  PCRE_DOTALL = $00000004;
  {$EXTERNALSYM PCRE_DOTALL}
  PCRE_EXTENDED = $00000008;
  {$EXTERNALSYM PCRE_EXTENDED}
  PCRE_ANCHORED = $00000010;
  {$EXTERNALSYM PCRE_ANCHORED}
  PCRE_DOLLAR_ENDONLY = $00000020;
  {$EXTERNALSYM PCRE_DOLLAR_ENDONLY}
  PCRE_EXTRA = $00000040;
  {$EXTERNALSYM PCRE_EXTRA}
  PCRE_NOTBOL = $00000080;
  {$EXTERNALSYM PCRE_NOTBOL}
  PCRE_NOTEOL = $00000100;
  {$EXTERNALSYM PCRE_NOTEOL}
  PCRE_UNGREEDY = $00000200;
  {$EXTERNALSYM PCRE_UNGREEDY}
  PCRE_NOTEMPTY = $00000400;
  {$EXTERNALSYM PCRE_NOTEMPTY}
  PCRE_UTF8 = $00000800;
  {$EXTERNALSYM PCRE_UTF8}
  PCRE_NO_AUTO_CAPTURE = $00001000;
  {$EXTERNALSYM PCRE_NO_AUTO_CAPTURE}
  PCRE_NO_UTF8_CHECK = $00002000;
  {$EXTERNALSYM PCRE_NO_UTF8_CHECK}
  PCRE_AUTO_CALLOUT = $00004000;
  {$EXTERNALSYM PCRE_AUTO_CALLOUT}
  PCRE_PARTIAL = $00008000;
  {$EXTERNALSYM PCRE_PARTIAL}
  PCRE_DFA_SHORTEST = $00010000;
  {$EXTERNALSYM PCRE_DFA_SHORTEST}
  PCRE_DFA_RESTART = $00020000;
  {$EXTERNALSYM PCRE_DFA_RESTART}
  PCRE_FIRSTLINE = $00040000;
  {$EXTERNALSYM PCRE_FIRSTLINE}
  PCRE_DUPNAMES = $00080000;
  {$EXTERNALSYM PCRE_DUPNAMES}
  PCRE_NEWLINE_CR = $00100000;
  {$EXTERNALSYM PCRE_NEWLINE_CR}
  PCRE_NEWLINE_LF = $00200000;
  {$EXTERNALSYM PCRE_NEWLINE_LF}
  PCRE_NEWLINE_CRLF = $00300000;
  {$EXTERNALSYM PCRE_NEWLINE_CRLF}
  PCRE_NEWLINE_ANY = $00400000;
  {$EXTERNALSYM PCRE_NEWLINE_ANY}
  PCRE_NEWLINE_ANYCRLF = $00500000;
  {$EXTERNALSYM PCRE_NEWLINE_ANYCRLF}
  PCRE_BSR_ANYCRLF = $00800000;
  {$EXTERNALSYM PCRE_BSR_ANYCRLF}
  PCRE_BSR_UNICODE = $01000000;
  {$EXTERNALSYM PCRE_BSR_UNICODE}
  PCRE_JAVASCRIPT_COMPAT = $02000000;
  {$EXTERNALSYM PCRE_JAVASCRIPT_COMPAT}

  (* Exec-time and get-time error codes *)

  PCRE_ERROR_NOMATCH = -1;
  {$EXTERNALSYM PCRE_ERROR_NOMATCH}
  PCRE_ERROR_NULL = -2;
  {$EXTERNALSYM PCRE_ERROR_NULL}
  PCRE_ERROR_BADOPTION = -3;
  {$EXTERNALSYM PCRE_ERROR_BADOPTION}
  PCRE_ERROR_BADMAGIC = -4;
  {$EXTERNALSYM PCRE_ERROR_BADMAGIC}
  PCRE_ERROR_UNKNOWN_NODE = -5;
  {$EXTERNALSYM PCRE_ERROR_UNKNOWN_NODE}
  PCRE_ERROR_NOMEMORY = -6;
  {$EXTERNALSYM PCRE_ERROR_NOMEMORY}
  PCRE_ERROR_NOSUBSTRING = -7;
  {$EXTERNALSYM PCRE_ERROR_NOSUBSTRING}
  PCRE_ERROR_MATCHLIMIT = -8;
  {$EXTERNALSYM PCRE_ERROR_MATCHLIMIT}
  PCRE_ERROR_CALLOUT = -9;  (* Never used by PCRE itself *)
  {$EXTERNALSYM PCRE_ERROR_CALLOUT}
  PCRE_ERROR_BADUTF8 = -10;
  {$EXTERNALSYM PCRE_ERROR_BADUTF8}
  PCRE_ERROR_BADUTF8_OFFSET = -11;
  {$EXTERNALSYM PCRE_ERROR_BADUTF8_OFFSET}
  PCRE_ERROR_PARTIAL = -12;
  {$EXTERNALSYM PCRE_ERROR_PARTIAL}
  PCRE_ERROR_BADPARTIAL = -13;
  {$EXTERNALSYM PCRE_ERROR_BADPARTIAL}
  PCRE_ERROR_INTERNAL = -14;
  {$EXTERNALSYM PCRE_ERROR_INTERNAL}
  PCRE_ERROR_BADCOUNT = -15;
  {$EXTERNALSYM PCRE_ERROR_BADCOUNT}
  PCRE_ERROR_DFA_UITEM = -16;
  {$EXTERNALSYM PCRE_ERROR_DFA_UITEM}
  PCRE_ERROR_DFA_UCOND = -17;
  {$EXTERNALSYM PCRE_ERROR_DFA_UCOND}
  PCRE_ERROR_DFA_UMLIMIT = -18;
  {$EXTERNALSYM PCRE_ERROR_DFA_UMLIMIT}
  PCRE_ERROR_DFA_WSSIZE = -19;
  {$EXTERNALSYM PCRE_ERROR_DFA_WSSIZE}
  PCRE_ERROR_DFA_RECURSE = -20;
  {$EXTERNALSYM PCRE_ERROR_DFA_RECURSE}
  PCRE_ERROR_RECURSIONLIMIT = -21;
  {$EXTERNALSYM PCRE_ERROR_RECURSIONLIMIT}
  PCRE_ERROR_NULLWSLIMIT = -22;  (* No longer actually used *)
  {$EXTERNALSYM PCRE_ERROR_NULLWSLIMIT}
  PCRE_ERROR_BADNEWLINE = -23;
  {$EXTERNALSYM PCRE_ERROR_BADNEWLINE}

  (* Request types for pcre_fullinfo() *)

  PCRE_INFO_OPTIONS = 0;
  {$EXTERNALSYM PCRE_INFO_OPTIONS}
  PCRE_INFO_SIZE = 1;
  {$EXTERNALSYM PCRE_INFO_SIZE}
  PCRE_INFO_CAPTURECOUNT = 2;
  {$EXTERNALSYM PCRE_INFO_CAPTURECOUNT}
  PCRE_INFO_BACKREFMAX = 3;
  {$EXTERNALSYM PCRE_INFO_BACKREFMAX}
  PCRE_INFO_FIRSTCHAR = 4;
  {$EXTERNALSYM PCRE_INFO_FIRSTCHAR}
  PCRE_INFO_FIRSTTABLE = 5;
  {$EXTERNALSYM PCRE_INFO_FIRSTTABLE}
  PCRE_INFO_LASTLITERAL = 6;
  {$EXTERNALSYM PCRE_INFO_LASTLITERAL}
  PCRE_INFO_NAMEENTRYSIZE = 7;
  {$EXTERNALSYM PCRE_INFO_NAMEENTRYSIZE}
  PCRE_INFO_NAMECOUNT = 8;
  {$EXTERNALSYM PCRE_INFO_NAMECOUNT}
  PCRE_INFO_NAMETABLE = 9;
  {$EXTERNALSYM PCRE_INFO_NAMETABLE}
  PCRE_INFO_STUDYSIZE = 10;
  {$EXTERNALSYM PCRE_INFO_STUDYSIZE}
  PCRE_INFO_DEFAULT_TABLES = 11;
  {$EXTERNALSYM PCRE_INFO_DEFAULT_TABLES}
  PCRE_INFO_OKPARTIAL = 12;
  {$EXTERNALSYM PCRE_INFO_OKPARTIAL}
  PCRE_INFO_JCHANGED = 13;
  {$EXTERNALSYM PCRE_INFO_JCHANGED}
  PCRE_INFO_HASCRORLF = 14;
  {$EXTERNALSYM PCRE_INFO_HASCRORLF}

  (* Request types for pcre_config() *)
  PCRE_CONFIG_UTF8 = 0;
  {$EXTERNALSYM PCRE_CONFIG_UTF8}
  PCRE_CONFIG_NEWLINE = 1;
  {$EXTERNALSYM PCRE_CONFIG_NEWLINE}
  PCRE_CONFIG_LINK_SIZE = 2;
  {$EXTERNALSYM PCRE_CONFIG_LINK_SIZE}
  PCRE_CONFIG_POSIX_MALLOC_THRESHOLD = 3;
  {$EXTERNALSYM PCRE_CONFIG_POSIX_MALLOC_THRESHOLD}
  PCRE_CONFIG_MATCH_LIMIT = 4;
  {$EXTERNALSYM PCRE_CONFIG_MATCH_LIMIT}
  PCRE_CONFIG_STACKRECURSE = 5;
  {$EXTERNALSYM PCRE_CONFIG_STACKRECURSE}
  PCRE_CONFIG_UNICODE_PROPERTIES = 6;
  {$EXTERNALSYM PCRE_CONFIG_UNICODE_PROPERTIES}
  PCRE_CONFIG_MATCH_LIMIT_RECURSION = 7;
  {$EXTERNALSYM PCRE_CONFIG_MATCH_LIMIT_RECURSION}
  PCRE_CONFIG_BSR = 8;
  {$EXTERNALSYM PCRE_CONFIG_BSR}

  (* Bit flags for the pcre_extra structure *)

  PCRE_EXTRA_STUDY_DATA = $0001;
  {$EXTERNALSYM PCRE_EXTRA_STUDY_DATA}
  PCRE_EXTRA_MATCH_LIMIT = $0002;
  {$EXTERNALSYM PCRE_EXTRA_MATCH_LIMIT}
  PCRE_EXTRA_CALLOUT_DATA = $0004;
  {$EXTERNALSYM PCRE_EXTRA_CALLOUT_DATA}
  PCRE_EXTRA_TABLES = $0008;
  {$EXTERNALSYM PCRE_EXTRA_TABLES}
  PCRE_EXTRA_MATCH_LIMIT_RECURSION = $0010;
  {$EXTERNALSYM PCRE_EXTRA_MATCH_LIMIT_RECURSION}

type
  (* Types *)
  PPChar = ^PChar;
  {$EXTERNALSYM PPChar}
  PPPChar = ^PPChar;
  {$EXTERNALSYM PPPChar}
  PInteger = ^Integer;
  {$EXTERNALSYM PInteger}

  real_pcre = packed record
    {magic_number: Longword;
    size: Integer;
    tables: PChar;
    options: Longword;
    top_bracket: Word;
    top_backref: word;
    first_char: PChar;
    req_char: PChar;
    code: array [0..0] of Char;}
  end;
  TPCRE = real_pcre;
  PPCRE = ^TPCRE;

  real_pcre_extra = packed record
    {options: PChar;
    start_bits: array [0..31] of Char;}
    flags: Cardinal;        (* Bits for which fields are set *)
    study_data: Pointer;    (* Opaque data from pcre_study() *)
    match_limit: Cardinal;  (* Maximum number of calls to match() *)
    callout_data: Pointer;  (* Data passed back in callouts *)
    tables: PChar;          (* Pointer to character tables *)
    match_limit_recursion: Cardinal; (* Max recursive calls to match() *)
  end;
  TPCREExtra = real_pcre_extra;
  PPCREExtra = ^TPCREExtra;

  pcre_callout_block = packed record
    version: Integer;           (* Identifies version of block *)
  (* ------------------------ Version 0 ------------------------------- *)
    callout_number: Integer;    (* Number compiled into pattern *)
    offset_vector: PInteger;     (* The offset vector *)
    subject: PChar;           (* The subject being matched *)
    subject_length: Integer;    (* The length of the subject *)
    start_match: Integer;       (* Offset to start of this match attempt *)
    current_position: Integer;  (* Where we currently are in the subject *)
    capture_top: Integer;       (* Max current capture *)
    capture_last: Integer;      (* Most recently closed capture *)
    callout_data: Pointer;      (* Data passed in with the call *)
  (* ------------------- Added for Version 1 -------------------------- *)
    pattern_position: Integer;  (* Offset to next item in the pattern *)
    next_item_length: Integer;  (* Length of next item in the pattern *)
  (* ------------------------------------------------------------------ *)
  end;

  pcre_malloc_callback = function(Size: Integer): Pointer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_malloc_callback}
  pcre_free_callback = procedure(P: Pointer); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_free_callback}
  pcre_stack_malloc_callback = function(Size: Integer): Pointer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_stack_malloc_callback}
  pcre_stack_free_callback = procedure(P: Pointer); {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_stack_free_callback}
  pcre_callout_callback = function(var callout_block: pcre_callout_block): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_callout_callback}

var
  // renamed from "pcre_X" to "pcre_X_func" to allow functions with name "pcre_X" to be
  // declared in implementation when static linked
  pcre_malloc_func: ^pcre_malloc_callback = nil;
  {$EXTERNALSYM pcre_malloc_func}
  pcre_free_func: ^pcre_free_callback = nil;
  {$EXTERNALSYM pcre_free_func}
  pcre_stack_malloc_func: ^pcre_stack_malloc_callback = nil;
  {$EXTERNALSYM pcre_stack_malloc_func}
  pcre_stack_free_func: ^pcre_stack_free_callback = nil;
  {$EXTERNALSYM pcre_stack_free_func}
  pcre_callout_func: ^pcre_callout_callback = nil;
  {$EXTERNALSYM pcre_callout_func}

procedure SetPCREMallocCallback(const Value: pcre_malloc_callback);
{$EXTERNALSYM SetPCREMallocCallback}
function GetPCREMallocCallback: pcre_malloc_callback;
{$EXTERNALSYM GetPCREMallocCallback}
function CallPCREMalloc(Size: Integer): Pointer;
{$EXTERNALSYM CallPCREMalloc}

procedure SetPCREFreeCallback(const Value: pcre_free_callback);
{$EXTERNALSYM SetPCREFreeCallback}
function GetPCREFreeCallback: pcre_free_callback;
{$EXTERNALSYM GetPCREFreeCallback}
procedure CallPCREFree(P: Pointer);
{$EXTERNALSYM CallPCREFree}

procedure SetPCREStackMallocCallback(const Value: pcre_stack_malloc_callback);
{$EXTERNALSYM SetPCREStackMallocCallback}
function GetPCREStackMallocCallback: pcre_stack_malloc_callback;
{$EXTERNALSYM GetPCREStackMallocCallback}
function CallPCREStackMalloc(Size: Integer): Pointer;
{$EXTERNALSYM CallPCREStackMalloc}

procedure SetPCREStackFreeCallback(const Value: pcre_stack_free_callback);
{$EXTERNALSYM SetPCREStackFreeCallback}
function GetPCREStackFreeCallback: pcre_stack_free_callback;
{$EXTERNALSYM GetPCREStackFreeCallback}
procedure CallPCREStackFree(P: Pointer);
{$EXTERNALSYM CallPCREStackFree}

procedure SetPCRECalloutCallback(const Value: pcre_callout_callback);
{$EXTERNALSYM SetPCRECalloutCallback}
function GetPCRECalloutCallback: pcre_callout_callback;
{$EXTERNALSYM GetPCRECalloutCallback}
function CallPCRECallout(var callout_block: pcre_callout_block): Integer;
{$EXTERNALSYM CallPCRECallout}

type
  TPCRELibNotLoadedHandler = procedure; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}

var
  // Value to initialize function pointers below with, in case LoadPCRE fails
  // or UnloadPCRE is called.  Typically the handler will raise an exception.
  LibNotLoadedHandler: TPCRELibNotLoadedHandler = nil;

(* Functions *)

{$IFNDEF PCRE_LINKONREQUEST}
// static link and static dll import
function pcre_compile(const pattern: PChar; options: Integer;
  const errptr: PPChar; erroffset: PInteger; const tableptr: PChar): PPCRE;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_compile}
function pcre_compile2(const pattern: PChar; options: Integer;
  const errorcodeptr: PInteger; const errorptr: PPChar; erroroffset: PInteger;
  const tables: PChar): PPCRE;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_compile2}
function pcre_config(what: Integer; where: Pointer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_config}
function pcre_copy_named_substring(const code: PPCRE; const subject: PChar;
  ovector: PInteger; stringcount: Integer; const stringname: PChar;
  buffer: PChar; size: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_copy_named_substring}
function pcre_copy_substring(const subject: PChar; ovector: PInteger;
  stringcount, stringnumber: Integer; buffer: PChar; buffersize: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_copy_substring}
function pcre_dfa_exec(const argument_re: PPCRE; const extra_data: PPCREExtra;
  const subject: PChar; length: Integer; start_offset: Integer;
  options: Integer; offsets: PInteger; offsetcount: Integer; workspace: PInteger;
  wscount: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_dfa_exec}
function pcre_exec(const code: PPCRE; const extra: PPCREExtra; const subject: PChar;
  length, startoffset, options: Integer; ovector: PInteger; ovecsize: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_exec}
procedure pcre_free_substring(stringptr: PChar);
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_free_substring}
procedure pcre_free_substring_list(stringlistptr: PPChar);
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_free_substring_list}
function pcre_fullinfo(const code: PPCRE; const extra: PPCREExtra;
  what: Integer; where: Pointer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_fullinfo}
function pcre_get_named_substring(const code: PPCRE; const subject: PChar;
  ovector: PInteger; stringcount: Integer; const stringname: PChar;
  const stringptr: PPChar): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_get_named_substring}
function pcre_get_stringnumber(const code: PPCRE; const stringname: PChar): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_get_stringnumber}
function pcre_get_stringtable_entries(const code: PPCRE; const stringname: PChar;
  firstptr: PPChar; lastptr: PPChar): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_get_stringtable_entries}
function pcre_get_substring(const subject: PChar; ovector: PInteger;
  stringcount, stringnumber: Integer; const stringptr: PPChar): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_get_substring}
function pcre_get_substring_list(const subject: PChar; ovector: PInteger;
  stringcount: Integer; listptr: PPPChar): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_get_substring_list}
function pcre_info(const code: PPCRE; optptr, firstcharptr: PInteger): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_info}
function pcre_maketables: PChar;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_maketables}
function pcre_refcount(argument_re: PPCRE; adjust: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_refcount}
function pcre_study(const code: PPCRE; options: Integer; const errptr: PPChar): PPCREExtra;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_study}
function pcre_version: PChar; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
{$EXTERNALSYM pcre_version}

{$ELSE}
// dynamic dll import
type
  pcre_compile_func = function(const pattern: PChar; options: Integer;
    const errptr: PPChar; erroffset: PInteger; const tableptr: PChar): PPCRE;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_compile_func}
  pcre_compile2_func = function(const pattern: PChar; options: Integer;
    const errorcodeptr: PInteger; const errorptr: PPChar; erroroffset: PInteger;
    const tables: PChar): PPCRE; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_compile2_func}
  pcre_config_func = function(what: Integer; where: Pointer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_config_func}
  pcre_copy_named_substring_func = function(const code: PPCRE; const subject: PChar;
    ovector: PInteger; stringcount: Integer; const stringname: PChar;
    buffer: PChar; size: Integer): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_copy_named_substring_func}
  pcre_copy_substring_func = function(const subject: PChar; ovector: PInteger;
    stringcount, stringnumber: Integer; buffer: PChar; buffersize: Integer): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_copy_substring_func}
  pcre_dfa_exec_func = function(const argument_re: PPCRE; const extra_data: PPCREExtra;
    const subject: PChar; length: Integer; start_offset: Integer;
    options: Integer; offsets: PInteger; offsetcount: Integer; workspace: PInteger;
    wscount: Integer): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_dfa_exec_func}
  pcre_exec_func = function(const code: PPCRE; const extra: PPCREExtra; const subject: PChar;
    length, startoffset, options: Integer; ovector: PInteger; ovecsize: Integer): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_exec_func}
  pcre_free_substring_func = procedure(stringptr: PChar);
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_free_substring_func}
  pcre_free_substring_list_func = procedure(stringptr: PPChar);
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_free_substring_list_func}
  pcre_fullinfo_func = function(const code: PPCRE; const extra: PPCREExtra;
    what: Integer; where: Pointer): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_fullinfo_func}
  pcre_get_named_substring_func = function(const code: PPCRE; const subject: PChar;
    ovector: PInteger; stringcount: Integer; const stringname: PChar;
    const stringptr: PPChar): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_get_named_substring_func}
  pcre_get_stringnumber_func = function(const code: PPCRE;
    const stringname: PChar): Integer; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_get_stringnumber_func}
  pcre_get_stringtable_entries_func = function(const code: PPCRE; const stringname: PChar;
    firstptr: PPChar; lastptr: PPChar): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_get_stringtable_entries_func}
  pcre_get_substring_func = function(const subject: PChar; ovector: PInteger;
    stringcount, stringnumber: Integer; const stringptr: PPChar): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_get_substring_func}
  pcre_get_substring_list_func = function(const subject: PChar; ovector: PInteger;
    stringcount: Integer; listptr: PPPChar): Integer;
    {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_get_substring_list_func}
  pcre_info_func = function(const code: PPCRE; optptr, firstcharptr: PInteger): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_info_func}
  pcre_maketables_func = function: PChar; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_maketables_func}
  pcre_refcount_func = function(argument_re: PPCRE; adjust: Integer): Integer;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_refcount_func}
  pcre_study_func = function(const code: PPCRE; options: Integer; const errptr: PPChar): PPCREExtra;
  {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_study_func}
  pcre_version_func = function: PChar; {$IFDEF PCRE_EXPORT_CDECL} cdecl; {$ENDIF PCRE_EXPORT_CDECL}
  {$EXTERNALSYM pcre_version_func}

var
  pcre_compile: pcre_compile_func = nil;
  {$EXTERNALSYM pcre_compile}
  pcre_compile2: pcre_compile2_func = nil;
  {$EXTERNALSYM pcre_compile2}
  pcre_config: pcre_config_func = nil;
  {$EXTERNALSYM pcre_config}
  pcre_copy_named_substring: pcre_copy_named_substring_func = nil;
  {$EXTERNALSYM pcre_copy_named_substring}
  pcre_copy_substring: pcre_copy_substring_func = nil;
  {$EXTERNALSYM pcre_copy_substring}
  pcre_dfa_exec: pcre_dfa_exec_func = nil;
  {$EXTERNALSYM pcre_dfa_exec}
  pcre_exec: pcre_exec_func = nil;
  {$EXTERNALSYM pcre_exec}
  pcre_free_substring: pcre_free_substring_func = nil;
  {$EXTERNALSYM pcre_free_substring}
  pcre_free_substring_list: pcre_free_substring_list_func = nil;
  {$EXTERNALSYM pcre_free_substring_list}
  pcre_fullinfo: pcre_fullinfo_func = nil;
  {$EXTERNALSYM pcre_fullinfo}
  pcre_get_named_substring: pcre_get_named_substring_func = nil;
  {$EXTERNALSYM pcre_get_named_substring}
  pcre_get_stringnumber: pcre_get_stringnumber_func = nil;
  {$EXTERNALSYM pcre_get_stringnumber}
  pcre_get_stringtable_entries: pcre_get_stringtable_entries_func = nil;
  {$EXTERNALSYM pcre_get_stringtable_entries}
  pcre_get_substring: pcre_get_substring_func = nil;
  {$EXTERNALSYM pcre_get_substring}
  pcre_get_substring_list: pcre_get_substring_list_func = nil;
  {$EXTERNALSYM pcre_get_substring_list}
  pcre_info: pcre_info_func = nil;
  {$EXTERNALSYM pcre_info}
  pcre_maketables: pcre_maketables_func = nil;
  {$EXTERNALSYM pcre_maketables}
  pcre_refcount: pcre_refcount_func = nil;
  {$EXTERNALSYM pcre_refcount}
  pcre_study: pcre_study_func = nil;
  {$EXTERNALSYM pcre_study}
  pcre_version: pcre_version_func = nil;
  {$EXTERNALSYM pcre_version}

{$ENDIF ~PCRE_LINKONREQUEST}

function IsPCRELoaded: Boolean;
function LoadPCRE: Boolean;
procedure UnloadPCRE;

implementation

uses
  SysUtils,
  {$IFDEF MSWINDOWS}
  Windows;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  {$IFDEF HAS_UNIT_TYPES}
  Types,
  {$ENDIF HAS_UNIT_TYPES}
  {$IFDEF HAS_UNIT_LIBC}
  Libc;
  {$ELSE ~HAS_UNIT_LIBC}
  dl;
  {$ENDIF ~HAS_UNIT_LIBC}
  {$ENDIF UNIX}

{$IFDEF PCRE_STATICLINK}
{$LINK ..\windows\obj\pcre\pcre_compile.obj}
{$LINK ..\windows\obj\pcre\pcre_config.obj}
{$LINK ..\windows\obj\pcre\pcre_dfa_exec.obj}
{$LINK ..\windows\obj\pcre\pcre_exec.obj}
{$LINK ..\windows\obj\pcre\pcre_fullinfo.obj}
{$LINK ..\windows\obj\pcre\pcre_get.obj}
{$LINK ..\windows\obj\pcre\pcre_globals.obj}
{$LINK ..\windows\obj\pcre\pcre_info.obj}
{$LINK ..\windows\obj\pcre\pcre_maketables.obj}
{$LINK ..\windows\obj\pcre\pcre_newline.obj}
{$LINK ..\windows\obj\pcre\pcre_ord2utf8.obj}
{$LINK ..\windows\obj\pcre\pcre_refcount.obj}
{$LINK ..\windows\obj\pcre\pcre_study.obj}
{$LINK ..\windows\obj\pcre\pcre_tables.obj}
{$LINK ..\windows\obj\pcre\pcre_try_flipped.obj}
{$LINK ..\windows\obj\pcre\pcre_ucp_searchfuncs.obj}
{$LINK ..\windows\obj\pcre\pcre_valid_utf8.obj}
{$LINK ..\windows\obj\pcre\pcre_version.obj}
{$LINK ..\windows\obj\pcre\pcre_xclass.obj}
{$LINK ..\windows\obj\pcre\pcre_default_tables.obj}

// user's defined callbacks
var
  pcre_malloc_user: pcre_malloc_callback;
  pcre_free_user: pcre_free_callback;
  pcre_stack_malloc_user: pcre_stack_malloc_callback;
  pcre_stack_free_user: pcre_stack_free_callback;
  pcre_callout_user: pcre_callout_callback;

function pcre_compile; external;
function pcre_compile2; external;
function pcre_config; external;
function pcre_copy_named_substring; external;
function pcre_copy_substring; external;
function pcre_dfa_exec; external;
function pcre_exec; external;
procedure pcre_free_substring; external;
procedure pcre_free_substring_list; external;
function pcre_fullinfo; external;
function pcre_get_named_substring; external;
function pcre_get_stringnumber; external;
function pcre_get_stringtable_entries; external;
function pcre_get_substring; external;
function pcre_get_substring_list; external;
function pcre_info; external;
function pcre_maketables; external;
function pcre_refcount; external;
function pcre_study; external;
function pcre_version; external;

type
  size_t = Longint;

const
  szMSVCRT = 'MSVCRT.DLL';

function _memcpy(dest, src: Pointer; count: size_t): Pointer; cdecl; external szMSVCRT name 'memcpy';
function _memmove(dest, src: Pointer; count: size_t): Pointer; cdecl; external szMSVCRT name 'memmove';
function _memset(dest: Pointer; val: Integer; count: size_t): Pointer; cdecl; external szMSVCRT name 'memset';
function _strncmp(s1: PAnsiChar; s2: PAnsiChar; n: size_t): Integer; cdecl; external szMSVCRT name 'strncmp';
function _memcmp(s1: Pointer; s2: Pointer; n: size_t): Integer; cdecl; external szMSVCRT name 'memcmp';
function _strlen(s: PAnsiChar): size_t; cdecl; external szMSVCRT name 'strlen';
function __ltolower(__ch: Integer): Integer; cdecl; external szMSVCRT name 'tolower';
function __ltoupper(__ch: Integer): Integer; cdecl; external szMSVCRT name 'toupper';
function _isalnum(__ch: Integer): Integer; cdecl; external szMSVCRT name 'isalnum';
function _isalpha(__ch: Integer): Integer; cdecl; external szMSVCRT name 'isalpha';
function _iscntrl(__ch: Integer): Integer; cdecl; external szMSVCRT name 'iscntrl';
function _isdigit(__ch: Integer): Integer; cdecl; external szMSVCRT name 'isdigit';
function _isgraph(__ch: Integer): Integer; cdecl; external szMSVCRT name 'isgraph';
function _islower(__ch: Integer): Integer; cdecl; external szMSVCRT name 'islower';
function _isprint(__ch: Integer): Integer; cdecl; external szMSVCRT name 'isprint';
function _ispunct(__ch: Integer): Integer; cdecl; external szMSVCRT name 'ispunct';
function _isspace(__ch: Integer): Integer; cdecl; external szMSVCRT name 'isspace';
function _isupper(__ch: Integer): Integer; cdecl; external szMSVCRT name 'isupper';
function _isxdigit(__ch: Integer): Integer; cdecl; external szMSVCRT name 'isxdigit';
function _strchr(__s: PChar; __c: Integer): PAnsiChar; cdecl; external szMSVCRT name 'strchr';

function malloc(size: size_t): Pointer; cdecl; external szMSVCRT name 'malloc';

function pcre_malloc(Size: Integer): Pointer;
begin
  if Assigned(pcre_malloc_user) then
    Result := pcre_malloc_user(Size)
  else
    Result := malloc(Size);
end;

function pcre_stack_malloc(Size: Integer): Pointer;
begin
  if Assigned(pcre_stack_malloc_user) then
    Result := pcre_stack_malloc_user(Size)
  else
    Result := malloc(Size);
end;

function _malloc(size: size_t): Pointer;
begin
  Result := pcre_malloc(size);
end;

procedure free(pBlock: Pointer); cdecl; external szMSVCRT name 'free';

procedure pcre_free(P: Pointer);
begin
  if Assigned(pcre_free_user) then
    pcre_free_user(P)
  else
    free(P);
end;

procedure pcre_stack_free(P: Pointer);
begin
  if Assigned(pcre_stack_free_user) then
    pcre_stack_free_user(P)
  else
    free(P);
end;

procedure _free(pBlock: Pointer);
begin
  pcre_free(pBlock);
end;

function pcre_callout(var callout_block: pcre_callout_block): Integer; cdecl;
begin
  if Assigned(pcre_callout_user) then
    Result := pcre_callout_user(callout_block)
  else
    Result := 0;
end;

{$ELSE ~PCRE_STATICLINK}

type
  {$IFDEF MSWINDOWS}
  TModuleHandle = HINST;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  TModuleHandle = Pointer;
  {$ENDIF LINUX}

const
  {$IFDEF MSWINDOWS}
  libpcremodulename = 'pcre3.dll';
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  libpcremodulename = 'libpcre.so.0';
  {$ENDIF UNIX}
  PCRECompileExportName = 'pcre_compile';
  PCRECompile2ExportName = 'pcre_compile2';
  PCREConfigExportName = 'pcre_config';
  PCRECopyNamedSubstringExportName = 'pcre_copy_named_substring';
  PCRECopySubStringExportName = 'pcre_copy_substring';
  PCREDfaExecExportName = 'pcre_dfa_exec';
  PCREExecExportName = 'pcre_exec';
  PCREFreeSubStringExportName = 'pcre_free_substring';
  PCREFreeSubStringListExportName = 'pcre_free_substring_list';
  PCREFullInfoExportName = 'pcre_fullinfo';
  PCREGetNamedSubstringExportName = 'pcre_get_named_substring';
  PCREGetStringNumberExportName = 'pcre_get_stringnumber';
  PCREGetStringTableEntriesExportName = 'pcre_get_stringtable_entries';
  PCREGetSubStringExportName = 'pcre_get_substring';
  PCREGetSubStringListExportName = 'pcre_get_substring_list';
  PCREInfoExportName = 'pcre_info';
  PCREMakeTablesExportName = 'pcre_maketables';
  PCRERefCountExportName = 'pcre_refcount';
  PCREStudyExportName = 'pcre_study';
  PCREVersionExportName = 'pcre_version';
  PCREMallocExportName = 'pcre_malloc';
  PCREFreeExportName = 'pcre_free';
  PCREStackMallocExportName = 'pcre_stack_malloc';
  PCREStackFreeExportName = 'pcre_stack_free';
  PCRECalloutExportName = 'pcre_callout';
  INVALID_MODULEHANDLE_VALUE = TModuleHandle(0);

var
  PCRELib: TModuleHandle = INVALID_MODULEHANDLE_VALUE;
{$ENDIF ~PCRE_STATICLINK}

procedure SetPCREMallocCallback(const Value: pcre_malloc_callback);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre_malloc_user := Value;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_malloc_func) then
    LoadPCRE;
  
  if Assigned(pcre_malloc_func) then
    pcre_malloc_func^ := Value
  else if Assigned(LibNotLoadedHandler) then
    LibNotLoadedHandler;
  {$ENDIF ~PCRE_STATICLINK}
end;

function GetPCREMallocCallback: pcre_malloc_callback;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre_malloc_user;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_malloc_func) then
    LoadPCRE;

  if not Assigned(pcre_malloc_func) then
  begin
    Result := nil;
    if Assigned(LibNotLoadedHandler) then
      LibNotLoadedHandler;
  end
  else
    Result := pcre_malloc_func^;
  {$ENDIF ~PCRE_STATICLINK}
end;

function CallPCREMalloc(Size: Integer): Pointer;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre_malloc(Size);
  {$ELSE ~PCRE_STATICLINK}
  Result := pcre_malloc_func^(Size);
  {$ENDIF ~PCRE_STATICLINK}
end;

procedure SetPCREFreeCallback(const Value: pcre_free_callback);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre_free_user := Value;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_free_func) then
    LoadPCRE;

  if Assigned(pcre_free_func) then
    pcre_free_func^ := Value
  else if Assigned(LibNotLoadedHandler) then
    LibNotLoadedHandler;
  {$ENDIF ~PCRE_STATICLINK}
end;

function GetPCREFreeCallback: pcre_free_callback;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre_free_user;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_free_func) then
    LoadPCRE;

  if not Assigned(pcre_free_func) then
  begin
    Result := nil;
    if Assigned(LibNotLoadedHandler) then
      LibNotLoadedHandler;
  end
  else
    Result := pcre_free_func^
  {$ENDIF ~PCRE_STATICLINK}
end;

procedure CallPCREFree(P: Pointer);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre_free(P);
  {$ELSE ~PCRE_STATICLINK}
  pcre_free_func^(P);
  {$ENDIF ~PCRE_STATICLINK}
end;

procedure SetPCREStackMallocCallback(const Value: pcre_stack_malloc_callback);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre_stack_malloc_user := Value;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_stack_malloc_func) then
    LoadPCRE;

  if Assigned(pcre_stack_malloc_func) then
    pcre_stack_malloc_func^ := Value
  else if Assigned(LibNotLoadedHandler) then
    LibNotLoadedHandler;
  {$ENDIF ~PCRE_STATICLINK}
end;

function GetPCREStackMallocCallback: pcre_stack_malloc_callback;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre_stack_malloc_user;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_stack_malloc_func) then
    LoadPCRE;

  if not Assigned(pcre_stack_malloc_func) then
  begin
    Result := nil;
    if Assigned(LibNotLoadedHandler) then
      LibNotLoadedHandler;
  end
  else
    Result := pcre_stack_malloc_func^;
  {$ENDIF ~PCRE_STATICLINK}
end;

function CallPCREStackMalloc(Size: Integer): Pointer;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre_stack_malloc(Size);
  {$ELSE ~PCRE_STATICLINK}
  Result := pcre_stack_malloc_func^(Size);
  {$ENDIF ~PCRE_STATICLINK}
end;

procedure SetPCREStackFreeCallback(const Value: pcre_stack_free_callback);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre_stack_free_user := Value;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_stack_free_func) then
    LoadPCRE;

  if Assigned(pcre_stack_free_func) then
    pcre_stack_free_func^ := Value
  else if Assigned(LibNotLoadedHandler) then
    LibNotLoadedHandler;
  {$ENDIF ~PCRE_STATICLINK}
end;

function GetPCREStackFreeCallback: pcre_stack_free_callback;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre_stack_free_user;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_stack_free_func) then
    LoadPCRE;

  if not Assigned(pcre_stack_free_func) then
  begin
    Result := nil;
    if Assigned(LibNotLoadedHandler) then
      LibNotLoadedHandler;
  end
  else
    Result := pcre_stack_free_func^;
  {$ENDIF ~PCRE_STATICLINK}
end;

procedure CallPCREStackFree(P: Pointer);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre_stack_free(P);
  {$ELSE ~PCRE_STATICLINK}
  pcre_stack_free_func^(P);
  {$ENDIF ~PCRE_STATICLINK}
end;

procedure SetPCRECalloutCallback(const Value: pcre_callout_callback);
begin
  {$IFDEF PCRE_STATICLINK}
  pcre_callout_user := Value;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_callout_func) then
    LoadPCRE;

  if Assigned(pcre_callout_func) then
    pcre_callout_func^ := Value
  else if Assigned(LibNotLoadedHandler) then
    LibNotLoadedHandler;
  {$ENDIF ~PCRE_STATICLINK}
end;

function GetPCRECalloutCallback: pcre_callout_callback;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre_callout_user;
  {$ELSE ~PCRE_STATICLINK}
  if not Assigned(pcre_callout_func) then
    LoadPCRE;

  if not Assigned(pcre_callout_func) then
  begin
    Result := nil;
    if Assigned(LibNotLoadedHandler) then
      LibNotLoadedHandler;
  end
  else
    Result := pcre_callout_func^;
  {$ENDIF ~PCRE_STATICLINK}
end;

function CallPCRECallout(var callout_block: pcre_callout_block): Integer;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := pcre_callout(callout_block);
  {$ELSE ~PCRE_STATICLINK}
  Result := pcre_callout_func^(callout_block);
  {$ENDIF ~PCRE_STATICLINK}
end;

{$IFNDEF PCRE_STATICLINK}
procedure InitPCREFuncPtrs(const Value: Pointer);
begin
  {$IFDEF PCRE_LINKONREQUEST}
  @pcre_compile := Value;
  @pcre_compile2 := Value;
  @pcre_config := Value;
  @pcre_copy_named_substring := Value;
  @pcre_copy_substring := Value;
  @pcre_dfa_exec := Value;
  @pcre_exec := Value;
  @pcre_free_substring := Value;
  @pcre_free_substring_list := Value;
  @pcre_fullinfo := Value;
  @pcre_get_named_substring := Value;
  @pcre_get_stringnumber := Value;
  @pcre_get_stringtable_entries := Value;
  @pcre_get_substring := Value;
  @pcre_get_substring_list := Value;
  @pcre_info := Value;
  @pcre_maketables := Value;
  @pcre_refcount := Value;
  @pcre_study := Value;
  @pcre_version := Value;
  {$ENDIF PCRE_LINKONREQUEST}
  pcre_malloc_func := nil;
  pcre_free_func := nil;
  pcre_stack_malloc_func := nil;
  pcre_stack_free_func := nil;
  pcre_callout_func := nil;
end;
{$ENDIF ~PCRE_STATICLINK}

function IsPCRELoaded: Boolean;
begin
  {$IFDEF PCRE_STATICLINK}
  Result := True;
  {$ELSE ~PCRE_STATICLINK}
  Result := PCRELib <> INVALID_MODULEHANDLE_VALUE;
  {$ENDIF ~PCRE_STATICLINK}
end;

function LoadPCRE: Boolean;
{$IFDEF PCRE_STATICLINK}
begin
  Result := True;
end;
{$ELSE ~PCRE_STATICLINK}
  function GetSymbol(SymbolName: PChar): Pointer;
  begin
    {$IFDEF MSWINDOWS}
    Result := GetProcAddress(PCRELib, PChar(SymbolName));
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    Result := dlsym(PCRELib, PChar(SymbolName));
    {$ENDIF UNIX}
  end;

begin
  Result := PCRELib <> INVALID_MODULEHANDLE_VALUE;
  if Result then
    Exit;

  if PCRELib = INVALID_MODULEHANDLE_VALUE then
    {$IFDEF MSWINDOWS}
    PCRELib := SafeLoadLibrary(libpcremodulename);
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    PCRELib := dlopen(PChar(libpcremodulename), RTLD_NOW);
    {$ENDIF UNIX}
  Result := PCRELib <> INVALID_MODULEHANDLE_VALUE;
  if Result then
  begin
    {$IFDEF PCRE_LINKONREQUEST}
    @pcre_compile := GetSymbol(PCRECompileExportName);
    @pcre_compile2 := GetSymbol(PCRECompile2ExportName);
    @pcre_config := GetSymbol(PCREConfigExportName);
    @pcre_copy_named_substring := GetSymbol(PCRECopyNamedSubstringExportName);
    @pcre_copy_substring := GetSymbol(PCRECopySubStringExportName);
    @pcre_dfa_exec := GetSymbol(PCREDfaExecExportName);
    @pcre_exec := GetSymbol(PCREExecExportName);
    @pcre_free_substring := GetSymbol(PCREFreeSubStringExportName);
    @pcre_free_substring_list := GetSymbol(PCREFreeSubStringListExportName);
    @pcre_fullinfo := GetSymbol(PCREFullInfoExportName);
    @pcre_get_named_substring := GetSymbol(PCREGetNamedSubstringExportName);
    @pcre_get_stringnumber := GetSymbol(PCREGetStringNumberExportName);
    @pcre_get_stringtable_entries := GetSymbol(PCREGetStringTableEntriesExportName);
    @pcre_get_substring := GetSymbol(PCREGetSubStringExportName);
    @pcre_get_substring_list := GetSymbol(PCREGetSubStringListExportName);
    @pcre_info := GetSymbol(PCREInfoExportName);
    @pcre_maketables := GetSymbol(PCREMakeTablesExportName);
    @pcre_refcount := GetSymbol(PCRERefCountExportName);
    @pcre_study := GetSymbol(PCREStudyExportName);
    @pcre_version := GetSymbol(PCREVersionExportName);
    {$ENDIF PCRE_LINKONREQUEST}
    pcre_malloc_func := GetSymbol(PCREMallocExportName);
    pcre_free_func := GetSymbol(PCREFreeExportName);
    pcre_stack_malloc_func := GetSymbol(PCREStackMallocExportName);
    pcre_stack_free_func := GetSymbol(PCREStackFreeExportName);
    pcre_callout_func := GetSymbol(PCRECalloutExportName);
  end
  else
    InitPCREFuncPtrs(@LibNotLoadedHandler);
end;
{$ENDIF ~PCRE_STATICLINK}

procedure UnloadPCRE;
begin
  {$IFNDEF PCRE_STATICLINK}
  if PCRELib <> INVALID_MODULEHANDLE_VALUE then
    {$IFDEF MSWINDOWS}
    FreeLibrary(PCRELib);
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    dlclose(Pointer(PCRELib));
    {$ENDIF UNIX}
  PCRELib := INVALID_MODULEHANDLE_VALUE;
  InitPCREFuncPtrs(@LibNotLoadedHandler);
  {$ENDIF ~PCRE_STATICLINK}
end;

{$IFDEF PCRE_LINKDLL}
function pcre_compile; external libpcremodulename name PCRECompileExportName;
function pcre_compile2; external libpcremodulename name PCRECompile2ExportName;
function pcre_config; external libpcremodulename name PCREConfigExportName;
function pcre_copy_named_substring; external libpcremodulename name PCRECopyNamedSubStringExportName;
function pcre_copy_substring; external libpcremodulename name PCRECopySubStringExportName;
function pcre_dfa_exec; external libpcremodulename name PCREDfaExecExportName;
function pcre_exec; external libpcremodulename name PCREExecExportName;
procedure pcre_free_substring; external libpcremodulename name PCREFreeSubStringExportName;
procedure pcre_free_substring_list; external libpcremodulename name PCREFreeSubStringListExportName;
function pcre_fullinfo; external libpcremodulename name PCREFullInfoExportName;
function pcre_get_named_substring; external libpcremodulename name PCREGetNamedSubStringExportName;
function pcre_get_stringnumber; external libpcremodulename name PCREGetStringNumberExportName;
function pcre_get_stringtable_entries; external libpcremodulename name PCREGetStringTableEntriesExportName;
function pcre_get_substring; external libpcremodulename name PCREGetSubStringExportName;
function pcre_get_substring_list; external libpcremodulename name PCREGetSubStringListExportName;
function pcre_info; external libpcremodulename name PCREInfoExportName;
function pcre_maketables; external libpcremodulename name PCREMakeTablesExportName;
function pcre_refcount; external libpcremodulename name PCRERefCountExportName;
function pcre_study; external libpcremodulename name PCREStudyExportName;
function pcre_version; external libpcremodulename name PCREVersionExportName;
{$ENDIF PCRE_LINKDLL}

end.

