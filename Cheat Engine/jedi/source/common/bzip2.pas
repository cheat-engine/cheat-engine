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
{ The Original Code is bzip2.pas.                                                                  }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet.                                    }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. All rights reserved.     }
{ Portions created by Julian Seward are Copyright (C) 1996-2006 Julian Seward <jseward@bzip.org>   }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{                                                                                                  }
{ The latest release of BZIP2 is available from http://www.bzip.org/                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Header conversion of bzlib.h                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-05-04 15:46:37 +0200 (dim., 04 mai 2008)                           $ }
{ Revision:      $Rev:: 2369                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit bzip2;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase; // PByte, PCardinal for Delphi 5 and C++Builder 5...

{
/*-------------------------------------------------------------*/
/*--- Public header file for the library.                   ---*/
/*---                                               bzlib.h ---*/
/*-------------------------------------------------------------*/

/* ------------------------------------------------------------------
   This file is part of bzip2/libbzip2, a program and library for
   lossless, block-sorting data compression.

   bzip2/libbzip2 version 1.0.4 of 20 December 2006
   Copyright (C) 1996-2006 Julian Seward <jseward@bzip.org>

   Please read the WARNING, DISCLAIMER and PATENTS sections in the
   README file.

   This program is released under the terms of the license contained
   in the file LICENSE.
   ------------------------------------------------------------------ */
}

const
  BZ_RUN              = 0;
  BZ_FLUSH            = 1;
  BZ_FINISH           = 2;

  BZ_OK               = 0;
  BZ_RUN_OK           = 1;
  BZ_FLUSH_OK         = 2;
  BZ_FINISH_OK        = 3;
  BZ_STREAM_END       = 4;
  BZ_SEQUENCE_ERROR   = -1;
  BZ_PARAM_ERROR      = -2;
  BZ_MEM_ERROR        = -3;
  BZ_DATA_ERROR       = -4;
  BZ_DATA_ERROR_MAGIC = -5;
  BZ_IO_ERROR         = -6;
  BZ_UNEXPECTED_EOF   = -7;
  BZ_OUTBUFF_FULL     = -8;
  BZ_CONFIG_ERROR     = -9;

type
   bz_stream = record
      next_in: PByte;
      avail_in: Cardinal;
      total_in_lo32: Cardinal;
      total_in_hi32: Cardinal;

      next_out: PByte;
      avail_out: Cardinal;
      total_out_lo32: Cardinal;
      total_out_hi32: Cardinal;

      state: Pointer;

      bzalloc: function (opaque: Pointer; n, m: Integer): Pointer; cdecl; // returns n*m bytes
      bzfree: procedure (opaque, p: Pointer); cdecl; // free p
      opaque: Pointer;
   end;

{$IFNDEF BZIP2_LINKONREQUEST}
//-- Core (low-level) library functions --

function BZ2_bzCompressInit(var strm: bz_stream;
  blockSize100k, verbosity, workFactor: Integer): Integer;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

function BZ2_bzCompress(var strm: bz_stream; action: Integer): Integer;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

function BZ2_bzCompressEnd(var strm: bz_stream): Integer;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

function BZ2_bzDecompressInit(var strm: bz_stream;
  verbosity, small: Integer): Integer;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

function BZ2_bzDecompress(var strm: bz_stream): Integer;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

function BZ2_bzDecompressEnd(var strm: bz_stream): Integer;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

//-- High(er) level library functions --

type
  BZFILE = Pointer;

// TODO: no stdio for static link (problems while linking stdin/stdout/stderr)

{#ifndef BZ_NO_STDIO
#define BZ_MAX_UNUSED 5000

typedef void BZFILE;

BZ_EXTERN BZFILE* BZ_API(BZ2_bzReadOpen) ( 
      int*  bzerror,   
      FILE* f, 
      int   verbosity, 
      int   small,
      void* unused,    
      int   nUnused 
   );

BZ_EXTERN void BZ_API(BZ2_bzReadClose) ( 
      int*    bzerror, 
      BZFILE* b 
   );

BZ_EXTERN void BZ_API(BZ2_bzReadGetUnused) ( 
      int*    bzerror, 
      BZFILE* b, 
      void**  unused,  
      int*    nUnused 
   );

BZ_EXTERN int BZ_API(BZ2_bzRead) ( 
      int*    bzerror, 
      BZFILE* b, 
      void*   buf, 
      int     len 
   );

BZ_EXTERN BZFILE* BZ_API(BZ2_bzWriteOpen) ( 
      int*  bzerror,      
      FILE* f, 
      int   blockSize100k, 
      int   verbosity, 
      int   workFactor 
   );

BZ_EXTERN void BZ_API(BZ2_bzWrite) ( 
      int*    bzerror, 
      BZFILE* b, 
      void*   buf, 
      int     len 
   );

BZ_EXTERN void BZ_API(BZ2_bzWriteClose) ( 
      int*          bzerror, 
      BZFILE*       b, 
      int           abandon, 
      unsigned int* nbytes_in, 
      unsigned int* nbytes_out 
   );

BZ_EXTERN void BZ_API(BZ2_bzWriteClose64) ( 
      int*          bzerror, 
      BZFILE*       b, 
      int           abandon, 
      unsigned int* nbytes_in_lo32, 
      unsigned int* nbytes_in_hi32, 
      unsigned int* nbytes_out_lo32, 
      unsigned int* nbytes_out_hi32
   );
#endif}


//- Utility functions --

function BZ2_bzBuffToBuffCompress(dest: PByte; destLen: PCardinal; source: PByte;
  sourceLen: Cardinal; blockSize100k, verbosity, workFactor: Integer): Integer;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

function BZ2_bzBuffToBuffDecompress(dest: PByte; destLen: PCardinal; source: PByte;
  sourceLen: Cardinal; small, verbosity: Integer): Integer;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

{
/*--
   Code contributed by Yoshioka Tsuneo (tsuneo@rr.iij4u.or.jp)
   to support better zlib compatibility.
   This code is not _officially_ part of libbzip2 (yet);
   I haven't tested it, documented it, or considered the
   threading-safeness of it.
   If this code breaks, please contact both Yoshioka and me.
--*/
}

function BZ2_bzlibVersion: PChar;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

// no STDIO (see above)
{
function BZ2_bzopen(path, mode: PChar): BZFILE;

function BZ2_bzdopen(fd: Integer; mode: PChar): BZFILE;

function BZ2_bzread(b: BZFILE; buf: Pointer; len: Integer): Integer;

function BZ2_bzwrite(b: BZFILE; buf: Pointer; len: Integer): Integer;

function BZ2_bzflush(b: BZFILE): Integer;

procedure BZ2_bzclose(b: BZFILE);

function BZ2_bzerror(b: BZFILE; errnum: PInteger): PChar;
}

{$ELSE BZIP2_LINKONREQUEST}
type
  BZ2_bzCompressInit_func = function(var strm: bz_stream;
    blockSize100k, verbosity, workFactor: Integer): Integer;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}
  BZ2_bzCompress_func = function(var strm: bz_stream;
    action: Integer): Integer;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}
  BZ2_bzCompressEnd_func = function(var strm: bz_stream): Integer;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}
  BZ2_bzDecompressInit_func = function(var strm: bz_stream;
    verbosity, small: Integer): Integer;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}
  BZ2_bzDecompress_func = function(var strm: bz_stream): Integer;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}
  BZ2_bzDecompressEnd_func = function(var strm: bz_stream): Integer;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}
  BZ2_bzBuffToBuffCompress_func = function(dest: PByte; destLen: PCardinal;
    source: PByte; sourceLen: Cardinal;
    blockSize100k, verbosity, workFactor: Integer): Integer;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}
  BZ2_bzBuffToBuffDecompress_func = function(dest: PByte; destLen: PCardinal;
    source: PByte; sourceLen: Cardinal; small, verbosity: Integer): Integer;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}
  BZ2_bzlibVersion_func = function: PChar;
    {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
    {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL}

var
  BZ2_bzCompressInit: BZ2_bzCompressInit_func = nil;
  BZ2_bzCompress: BZ2_bzCompress_func = nil;
  BZ2_bzCompressEnd: BZ2_bzCompressEnd_func = nil;
  BZ2_bzDecompressInit: BZ2_bzDecompressInit_func = nil;
  BZ2_bzDecompress: BZ2_bzDecompress_func = nil;
  BZ2_bzDecompressEnd: BZ2_bzDecompressEnd_func = nil;
  BZ2_bzBuffToBuffCompress: BZ2_bzBuffToBuffCompress_func = nil;
  BZ2_bzBuffToBuffDecompress: BZ2_bzBuffToBuffDecompress_func = nil;
  BZ2_bzlibVersion: BZ2_bzlibVersion_func = nil;
{$ENDIF BZIP2_LINKONREQUEST}

var
  bz2_internal_error_event: procedure(errcode: Integer) of object = nil;

function LoadBZip2: Boolean;
function IsBZip2Loaded: Boolean;
procedure UnloadBZip2;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/bzip2.pas $';
    Revision: '$Revision: 2369 $';
    Date: '$Date: 2008-05-04 15:46:37 +0200 (dim., 04 mai 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

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

type
  {$IFDEF MSWINDOWS}
  TModuleHandle = HINST;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  TModuleHandle = Pointer;
  {$ENDIF LINUX}

const
  {$IFDEF MSWINDOWS}
  szBZIP2 = 'bzip2.dll'; // from http://gnuwin32.sourceforge.net/
  szMSVCRT = 'MSVCRT.DLL';
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  szBZIP2 = 'libbz2.so.1';
  {$ENDIF UNIX}
  BZ2CompressInitExportName = 'BZ2_bzCompressInit';
  BZ2CompressExportName = 'BZ2_bzCompress';
  BZ2CompressEndExportName = 'BZ2_bzCompressEnd';
  BZ2DecompressInitExportName = 'BZ2_bzDecompressInit';
  BZ2DecompressExportName = 'BZ2_bzDecompress';
  BZ2DecompressEndExportName = 'BZ2_bzDecompressEnd';
  BZ2BuffToBuffCompressExportName = 'BZ2_bzBuffToBuffCompress';
  BZ2BuffToBuffDecompressExportName = 'BZ2_bzBuffToBuffDecompress';
  BZ2LibVersionExportName = 'BZ2_bzlibVersion';
  INVALID_MODULEHANDLE_VALUE = TModuleHandle(0);

{$IFDEF BZIP2_STATICLINK}
function BZ2_bzCompressInit; external;
function BZ2_bzCompress; external;
function BZ2_bzCompressEnd; external;
function BZ2_bzDecompressInit; external;
function BZ2_bzDecompress; external;
function BZ2_bzDecompressEnd; external;
function BZ2_bzBuffToBuffCompress; external;
function BZ2_bzBuffToBuffDecompress; external;
function BZ2_bzlibVersion; external;
// workaround to make the compiler aware of _BZ2_indexIntoF
// an external must be declared for this function in order to make the compiler considering
// the corresponding PUBDEF in bzlib.obj
// source: CodeGear QA team
function _BZ2_indexIntoF: PChar;
  {$IFDEF BZIP2_EXPORT_STDCALL}stdcall;{$ENDIF BZIP2_EXPORT_STDCALL}
  {$IFDEF BZIP2_EXPORT_CDECL}cdecl;{$ENDIF BZIP2_EXPORT_CDECL} external;

{$LINK ..\windows\obj\bzip2\bzlib.obj}
{$LINK ..\windows\obj\bzip2\randtable.obj}
{$LINK ..\windows\obj\bzip2\crctable.obj}
{$LINK ..\windows\obj\bzip2\compress.obj}
{$LINK ..\windows\obj\bzip2\decompress.obj}
{$LINK ..\windows\obj\bzip2\huffman.obj}
{$LINK ..\windows\obj\bzip2\blocksort.obj}

type
  size_t = Longint;

function _malloc(size: size_t): Pointer; cdecl; external szMSVCRT name 'malloc';
procedure _free(pBlock: Pointer); cdecl; external szMSVCRT name 'free';

procedure _bz_internal_error(errcode: Integer); cdecl;
begin
  if Assigned(bz2_internal_error_event) then
    bz2_internal_error_event(errcode);
end;
{$ENDIF BZIP2_STATICLINK}

{$IFDEF BZIP2_LINKDLL}
function BZ2_bzCompressInit; external szBZIP2 name BZ2CompressInitExportName;
function BZ2_bzCompress; external szBZIP2 name BZ2CompressExportName;
function BZ2_bzCompressEnd; external szBZIP2 name BZ2CompressEndExportName;
function BZ2_bzDecompressInit; external szBZIP2 name BZ2DecompressInitExportName;
function BZ2_bzDecompress; external szBZIP2 name BZ2DecompressExportName;
function BZ2_bzDecompressEnd; external szBZIP2 name BZ2DecompressEndExportName;
function BZ2_bzBuffToBuffCompress; external szBZIP2 name BZ2BuffToBuffCompressExportName;
function BZ2_bzBuffToBuffDecompress; external szBZIP2 name BZ2BuffToBuffDecompressExportName;
function BZ2_bzlibVersion; external szBZIP2 name BZ2LibVersionExportName;
{$ENDIF BZIP2_LINKDLL}

{$IFDEF BZIP2_LINKONREQUEST}
var
  BZip2Lib: TModuleHandle = INVALID_MODULEHANDLE_VALUE;
{$ENDIF BZIP2_LINKONREQUEST}

function LoadBZip2: Boolean;
{$IFDEF BZIP2_LINKONREQUEST}
  function GetSymbol(SymbolName: PChar): Pointer;
  begin
    {$IFDEF MSWINDOWS}
    Result := GetProcAddress(BZip2Lib, PChar(SymbolName));
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    Result := dlsym(BZip2Lib, PChar(SymbolName));
    {$ENDIF UNIX}
  end;
begin
  Result := BZip2Lib <> INVALID_MODULEHANDLE_VALUE;
  if Result then
    Exit;

  if BZip2Lib = INVALID_MODULEHANDLE_VALUE then
    {$IFDEF MSWINDOWS}
    BZip2Lib := SafeLoadLibrary(szBZIP2);
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    BZip2Lib := dlopen(PChar(szBZIP2), RTLD_NOW);
    {$ENDIF UNIX}
  Result := BZip2Lib <> INVALID_MODULEHANDLE_VALUE;
  if Result then
  begin
    @BZ2_bzCompressInit := GetSymbol(BZ2CompressInitExportName);
    @BZ2_bzCompress := GetSymbol(BZ2CompressExportName);
    @BZ2_bzCompressEnd := GetSymbol(BZ2CompressEndExportName);
    @BZ2_bzDecompressInit := GetSymbol(BZ2DecompressInitExportName);
    @BZ2_bzDecompress := GetSymbol(BZ2DecompressExportName);
    @BZ2_bzDecompressEnd := GetSymbol(BZ2DecompressEndExportName);
    @BZ2_bzBuffToBuffCompress := GetSymbol(BZ2BuffToBuffCompressExportName);
    @BZ2_bzBuffToBuffDecompress := GetSymbol(BZ2BuffToBuffDecompressExportName);
    @BZ2_bzlibVersion := GetSymbol(BZ2LibVersionExportName);
  end;
end;
{$ELSE ~BZIP2_LINKONREQUEST}
begin
  Result := True;
end;
  {$ENDIF ~BZIP2_LINKONREQUEST}

function IsBZip2Loaded: Boolean;
begin
  {$IFDEF BZIP2_LINKONREQUEST}
  Result := BZip2Lib <> INVALID_MODULEHANDLE_VALUE;
  {$ELSE ~BZIP2_LINKONREQUEST}
  Result := True;
  {$ENDIF ~BZIP2_LINKONREQUEST}
end;

procedure UnloadBZip2;
begin
  {$IFDEF BZIP2_LINKONREQUEST}
  if BZip2Lib <> INVALID_MODULEHANDLE_VALUE then
    {$IFDEF MSWINDOWS}
    FreeLibrary(BZip2Lib);
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    dlclose(Pointer(BZip2Lib));
    {$ENDIF UNIX}
  BZip2Lib := INVALID_MODULEHANDLE_VALUE;
  {$ENDIF BZIP2_LINKONREQUEST}
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
