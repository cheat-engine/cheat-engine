(*)
 [------------------------------------------------------------------------------
 [  DXFile Delphi Adaptation (c) by Tim Baumgarten
 [------------------------------------------------------------------------------
 [  Files    : dxfile.h
 [  Modified : 09-Jan-2003
 [  E-Mail   : Ampaze at gmx dot net
 [  Download : http://www.crazyentertainment.net
 [------------------------------------------------------------------------------
(*)

(*)
 [------------------------------------------------------------------------------
 [ History :
 [----------
 [ 09-Jan-2003 (Tim Baumgarten) : Redone.
 [ 05-Nov-2001 (Tim Baumgarten) : Added DirectX File Template GUIDs
 [                                (TID_DXFILEXXX) and Descriptions
 [                                (DXFILE_XTEMPLATES).
 [ 05-Nov-2001 (Tim Baumgarten) : Added DXFileErrorString.
 [------------------------------------------------------------------------------
(*)

{$MINENUMSIZE 4}
{$ALIGN ON}

unit DXFile9;

{$IFDEF VER150}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

interface

uses Windows;

(*)
 *******************************************************************************
 *
 *  Copyright (C) 1998-1999 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dxfile.h
 *
 *  Content:    DirectX File public header file
 *
 *******************************************************************************
(*)

const
  DXFileDLL = 'D3DXOF.DLL';

function DXFileErrorString(DXErrorCode : HResult) : String;

type
  TDXFileFormat = LongWord;

const
  DXFILEFORMAT_BINARY     = 0;
  DXFILEFORMAT_TEXT       = 1;
  DXFILEFORMAT_COMPRESSED = 2;

type
  TDXFileLoadOptions = LongWord;

const
  DXFILELOAD_FROMFILE           = $00;
  DXFILELOAD_FROMRESOURCE       = $01;
  DXFILELOAD_FROMMEMORY         = $02;
  DXFILELOAD_FROMSTREAM         = $04;
  DXFILELOAD_FROMURL            = $08;

type
  PDXFileLoadResource = ^TDXFileLoadResource;
  TDXFileLoadResource = packed record
    Module : HModule;
    Name   : PChar;
    _Type  : PChar;
  end;

  PDXFileLoadMemory = ^TDXFileLoadMemory;
  TDXFileLoadMemory = packed record
    Memory : Pointer;
    Size   : LongWord;
  end;

(*)
 *******************************************************************************
 * DirectX File object types.
 ******************************************************************************* 
(*)

type
  IDirectXFile = interface;
  IDirectXFileEnumObject = interface;
  IDirectXFileSaveObject = interface;
  IDirectXFileObject = interface;
  IDirectXFileData = interface;
  IDirectXFileDataReference = interface;
  IDirectXFileBinary = interface;

(*)
 *******************************************************************************
 * DirectX File interfaces.
 *******************************************************************************
(*)

  IDirectXFile = interface(IUnknown)
    ['{3D82AB40-62DA-11CF-AB39-0020AF71E433}']
    function CreateEnumObject(const Source : Pointer; const LoadOptions : TDXFileLoadOptions; out EnumObj : IDirectXFileEnumObject) : HResult; stdcall;
    function CreateSaveObject(const FileName : PChar; const FileFormat : TDXFileFormat; out SaveObj : IDirectXFileSaveObject) : HResult; stdcall;
    function RegisterTemplates(const Data : Pointer; const Size : LongWord) : HResult; stdcall;
  end;

  IDirectXFileEnumObject = interface (IUnknown)
    ['{3D82AB41-62DA-11CF-AB39-0020AF71E433}']
    function GetNextDataObject(out DataObj : IDirectXFileData) : HResult; stdcall;
    function GetDataObjectById(const RGUID : TGUID; out DataObj : IDirectXFileData) : HResult; stdcall;
    function GetDataObjectByName(const Name : PChar; out DataObj : IDirectXFileData) : HResult; stdcall;
  end;

  IDirectXFileSaveObject = interface (IUnknown)
    ['{3D82AB42-62DA-11CF-AB39-0020AF71E433}']
    function SaveTemplates(const Templates : LongWord; var GUIDTemplates : PGUID) : HResult; stdcall;
    function CreateDataObject(const RGUIDTemplate : TGUID; const szName : PChar; const GUID : PGUID; const Size : Cardinal; Data: Pointer; out DataObj : IDirectXFileData) : HResult; stdcall;
    function SaveData(DataObj : IDirectXFileData) : HResult; stdcall;
  end;

  IDirectXFileObject = interface (IUnknown)
    ['{3D82AB43-62DA-11CF-AB39-0020AF71E433}']
    function GetName(const NameBuf : PChar; BufLen : PLongWord) : HResult; stdcall;
    function GetId (out GUID : TGUID) : HResult; stdcall;
  end;

  IDirectXFileData = interface (IDirectXFileObject)
    ['{3D82AB44-62DA-11CF-AB39-0020AF71E433}']
    function GetData(const Member : PChar; out Size : LongWord; out Data : Pointer) : HResult; stdcall;
    function GetType(out GUID : PGUID) : HResult; stdcall;
    function GetNextObject(out ChildObj : IDirectXFileObject) : HResult; stdcall;
    function AddDataObject(DataObj : IDirectXFileData) : HResult; stdcall;
    function AddDataReference(Ref : PChar; GUIDRef : PGUID) : HResult; stdcall;
    function AddBinaryObject(Name : PChar; GUID : PGUID; MimeType : PChar; Data : Pointer; const Size : LongWord) : HResult; stdcall;
  end;

  IDirectXFileDataReference = interface (IDirectXFileObject)
    ['{3D82AB45-62DA-11CF-AB39-0020AF71E433}']
    function Resolve(out DataObj : IDirectXFileData) : HResult; stdcall;
  end;

  IDirectXFileBinary = interface (IDirectXFileObject)
    ['{3D82AB46-62DA-11CF-AB39-0020AF71E433}']
    function GetSize(out Size : LongWord) : HResult; stdcall;
    function GetMimeType(out MimeType : PChar) : HResult; stdcall;
    function Read(Data : Pointer; const Size : LongWord; out Read : LongWord) : HResult; stdcall;
  end;

(*)
 *******************************************************************************
 * API for creating IDirectXFile interface.
 *******************************************************************************
(*)

function DirectXFileCreate(out DirectXFile : IDirectXFile) : HResult; stdcall; external DXFileDLL

(*)
 *******************************************************************************
 * DirectXFile Object Class Id (for CoCreateInstance())
 *******************************************************************************
(*)

const
  CLSID_CDirectXFile : TGUID = '{4516EC43-8F20-11D0-9B6D-0000C0781BC3}';

(*)
 *******************************************************************************
 * DirectX File Interface GUIDs.
 *******************************************************************************
(*)

type
  IID_IDirectXFile               = IDirectXFile;
  IID_IDirectXFileEnumObject     = IDirectXFileEnumObject;
  IID_IDirectXFileSaveObject     = IDirectXFileSaveObject;
  IID_IDirectXFileObject         = IDirectXFileObject;
  IID_IDirectXFileData           = IDirectXFileData;
  IID_IDirectXFileDataReference  = IDirectXFileDataReference;
  IID_IDirectXFileBinary         = IDirectXFileBinary;

(*)
 *******************************************************************************
 * DirectX File Header template's GUID.
 *******************************************************************************
(*)

const
  TID_DXFILEHEADER                   : TGUID = '{3D82AB43-62DA-11CF-AB39-0020AF71E433}';
  TID_DXFILEMESH                     : TGUID = '{3D82AB44-62DA-11CF-AB39-0020AF71E433}';
  TID_DXFILEMVECTOR                  : TGUID = '{3D82AB5E-62DA-11CF-AB39-0020AF71E433}';
  TID_DXFILEMESHFACE                 : TGUID = '{3D82AB5F-62DA-11CF-AB39-0020AF71E433}';
  TID_DXFILEMATERIAL                 : TGUID = '{3D82AB4D-62DA-11CF-AB39-0020AF71E433}';
  TID_DXFILEFRAME                    : TGUID = '{3D82AB46-62DA-11CF-AB39-0020AF71E433}';
  TID_DXFILEFRAMETRANSFORMMATRIX     : TGUID = '{F6F23F41-7686-11CF-8F52-0040333594A3}';
  TID_DXFILEMESHMATERIALLIST         : TGUID = '{F6F23F42-7686-11CF-8F52-0040333594A3}';
  TID_DXFILEMESHTEXTURECOORDS        : TGUID = '{F6F23F40-7686-11CF-8F52-0040333594A3}';
  TID_DXFILEMESHNORMALS              : TGUID = '{F6F23F43-7686-11CF-8F52-0040333594A3}';
  TID_DXFILECOORDS2D                 : TGUID = '{F6F23F44-7686-11CF-8F52-0040333594A3}';
  TID_DXFILEMATRIX4X4                : TGUID = '{F6F23F45-7686-11CF-8F52-0040333594A3}';
  TID_DXFILEANIMATION                : TGUID = '{3D82AB4F-62DA-11CF-AB39-0020AF71E433}';
  TID_DXFILEANIMATIONSET             : TGUID = '{3D82AB50-62DA-11CF-AB39-0020AF71E433}';
  TID_DXFILEANIMATIONKEY             : TGUID = '{10DD46A8-775B-11CF-8F52-0040333594A3}';
  TID_DXFILEFLOATKEYS                : TGUID = '{10DD46A9-775B-11CF-8F52-0040333594A3}';
  TID_DXFILECOLORRGBA                : TGUID = '{35FF44E0-6C7C-11CF-8F52-0040333594A3}';
  TID_DXFILECOLORRGB                 : TGUID = '{D3E16E81-7835-11CF-8F52-0040333594A3}';
  TID_DXFILETEXTUREFILENAME          : TGUID = '{A42790E1-7810-11CF-8F52-0040333594A3}';
  TID_DXFILEINDEXEDCOLOR             : TGUID = '{1630B820-7842-11CF-8F52-0040333594A3}';
  TID_DXFILEBOOLEAN                  : TGUID = '{4885AE61-78E8-11CF-8F52-0040333594A3}';
  TID_DXFILEBOOLEAN2D                : TGUID = '{4885AE63-78E8-11CF-8F52-0040333594A3}';
  TID_DXFILEMESHVERTEXCOLORS         : TGUID = '{1630B821-7842-11CF-8F52-0040333594A3}';
  TID_DXFILEMESHFACEWRAPS            : TGUID = '{ED1EC5C0-C0A8-11D0-941C-0080C80CFA7B}';
  TID_DXFILETIMEDFLOATKEYS           : TGUID = '{F406B180-7B3B-11CF-8F52-0040333594A3}';
  TID_DXFILEANIMATIONOPTIONS         : TGUID = '{E2BF56C0-840F-11CF-8F52-0040333594A3}';
  TID_DXFILEQUATERNION               : TGUID = '{10DD46A3-775B-11cf-8F52-0040333594A3}';
  TID_DXFILEPATCH                    : TGUID = '{A3EB5D44-FC22-429D-9AFB-3221CB9719A6}';
  TID_DXFILEPATCHMESH                : TGUID = '{D02C95CC-EDBA-4305-9B5D-1820D7704BBF}';
  TID_DXFILEVERTEXDUPLICATIONINDICES : TGUID = '{B8D65549-D7C9-4995-89CF-53A9A8B031E3}';
  TID_DXFILEXSKINMESHHEADER          : TGUID = '{3CF169CE-FF7C-44ab-93C0-F78F62D172E2}';
  TID_DXFILESKINWEIGHTS              : TGUID = '{6F0D123B-BAD2-4167-A0D0-80224F25FABB}';

  DXFILE_XTEMPLATE_BYTES = 3278;

  DXFILE_XTEMPLATES : array [0..DXFILE_XTEMPLATE_BYTES-1] of Byte = (
        $78, $6f, $66, $20, $30, $33, $30, $32, $62, $69, $6e, $20, $30, $30, $36, $34, $1f, 0, $1,
        0, $6, 0, 0, 0, $48, $65, $61, $64, $65, $72, $a, 0, $5, 0, $43, $ab, $82, $3d, $da,
        $62, $cf, $11, $ab, $39, 0, $20, $af, $71, $e4, $33, $28, 0, $1, 0, $5, 0, 0, 0, $6d,
        $61, $6a, $6f, $72, $14, 0, $28, 0, $1, 0, $5, 0, 0, 0, $6d, $69, $6e, $6f, $72, $14,
        0, $29, 0, $1, 0, $5, 0, 0, 0, $66, $6c, $61, $67, $73, $14, 0, $b, 0, $1f, 0,
        $1, 0, $6, 0, 0, 0, $56, $65, $63, $74, $6f, $72, $a, 0, $5, 0, $5e, $ab, $82, $3d,
        $da, $62, $cf, $11, $ab, $39, 0, $20, $af, $71, $e4, $33, $2a, 0, $1, 0, $1, 0, 0, 0,
        $78, $14, 0, $2a, 0, $1, 0, $1, 0, 0, 0, $79, $14, 0, $2a, 0, $1, 0, $1, 0,
        0, 0, $7a, $14, 0, $b, 0, $1f, 0, $1, 0, $8, 0, 0, 0, $43, $6f, $6f, $72, $64,
        $73, $32, $64, $a, 0, $5, 0, $44, $3f, $f2, $f6, $86, $76, $cf, $11, $8f, $52, 0, $40, $33,
        $35, $94, $a3, $2a, 0, $1, 0, $1, 0, 0, 0, $75, $14, 0, $2a, 0, $1, 0, $1, 0,
        0, 0, $76, $14, 0, $b, 0, $1f, 0, $1, 0, $9, 0, 0, 0, $4d, $61, $74, $72, $69,
        $78, $34, $78, $34, $a, 0, $5, 0, $45, $3f, $f2, $f6, $86, $76, $cf, $11, $8f, $52, 0, $40,
        $33, $35, $94, $a3, $34, 0, $2a, 0, $1, 0, $6, 0, 0, 0, $6d, $61, $74, $72, $69, $78,
        $e, 0, $3, 0, $10, 0, 0, 0, $f, 0, $14, 0, $b, 0, $1f, 0, $1, 0, $9, 0,
        0, 0, $43, $6f, $6c, $6f, $72, $52, $47, $42, $41, $a, 0, $5, 0, $e0, $44, $ff, $35, $7c,
        $6c, $cf, $11, $8f, $52, 0, $40, $33, $35, $94, $a3, $2a, 0, $1, 0, $3, 0, 0, 0, $72,
        $65, $64, $14, 0, $2a, 0, $1, 0, $5, 0, 0, 0, $67, $72, $65, $65, $6e, $14, 0, $2a,
        0, $1, 0, $4, 0, 0, 0, $62, $6c, $75, $65, $14, 0, $2a, 0, $1, 0, $5, 0, 0,
        0, $61, $6c, $70, $68, $61, $14, 0, $b, 0, $1f, 0, $1, 0, $8, 0, 0, 0, $43, $6f,
        $6c, $6f, $72, $52, $47, $42, $a, 0, $5, 0, $81, $6e, $e1, $d3, $35, $78, $cf, $11, $8f, $52,
        0, $40, $33, $35, $94, $a3, $2a, 0, $1, 0, $3, 0, 0, 0, $72, $65, $64, $14, 0, $2a,
        0, $1, 0, $5, 0, 0, 0, $67, $72, $65, $65, $6e, $14, 0, $2a, 0, $1, 0, $4, 0,
        0, 0, $62, $6c, $75, $65, $14, 0, $b, 0, $1f, 0, $1, 0, $c, 0, 0, 0, $49, $6e,
        $64, $65, $78, $65, $64, $43, $6f, $6c, $6f, $72, $a, 0, $5, 0, $20, $b8, $30, $16, $42, $78,
        $cf, $11, $8f, $52, 0, $40, $33, $35, $94, $a3, $29, 0, $1, 0, $5, 0, 0, 0, $69, $6e,
        $64, $65, $78, $14, 0, $1, 0, $9, 0, 0, 0, $43, $6f, $6c, $6f, $72, $52, $47, $42, $41,
        $1, 0, $a, 0, 0, 0, $69, $6e, $64, $65, $78, $43, $6f, $6c, $6f, $72, $14, 0, $b, 0,
        $1f, 0, $1, 0, $7, 0, 0, 0, $42, $6f, $6f, $6c, $65, $61, $6e, $a, 0, $5, 0, $a0,
        $a6, $7d, $53, $37, $ca, $d0, $11, $94, $1c, 0, $80, $c8, $c, $fa, $7b, $29, 0, $1, 0, $9,
        0, 0, 0, $74, $72, $75, $65, $66, $61, $6c, $73, $65, $14, 0, $b, 0, $1f, 0, $1, 0,
        $9, 0, 0, 0, $42, $6f, $6f, $6c, $65, $61, $6e, $32, $64, $a, 0, $5, 0, $63, $ae, $85,
        $48, $e8, $78, $cf, $11, $8f, $52, 0, $40, $33, $35, $94, $a3, $1, 0, $7, 0, 0, 0, $42,
        $6f, $6f, $6c, $65, $61, $6e, $1, 0, $1, 0, 0, 0, $75, $14, 0, $1, 0, $7, 0, 0,
        0, $42, $6f, $6f, $6c, $65, $61, $6e, $1, 0, $1, 0, 0, 0, $76, $14, 0, $b, 0, $1f,
        0, $1, 0, $c, 0, 0, 0, $4d, $61, $74, $65, $72, $69, $61, $6c, $57, $72, $61, $70, $a,
        0, $5, 0, $60, $ae, $85, $48, $e8, $78, $cf, $11, $8f, $52, 0, $40, $33, $35, $94, $a3, $1,
        0, $7, 0, 0, 0, $42, $6f, $6f, $6c, $65, $61, $6e, $1, 0, $1, 0, 0, 0, $75, $14,
        0, $1, 0, $7, 0, 0, 0, $42, $6f, $6f, $6c, $65, $61, $6e, $1, 0, $1, 0, 0, 0,
        $76, $14, 0, $b, 0, $1f, 0, $1, 0, $f, 0, 0, 0, $54, $65, $78, $74, $75, $72, $65,
        $46, $69, $6c, $65, $6e, $61, $6d, $65, $a, 0, $5, 0, $e1, $90, $27, $a4, $10, $78, $cf, $11,
        $8f, $52, 0, $40, $33, $35, $94, $a3, $31, 0, $1, 0, $8, 0, 0, 0, $66, $69, $6c, $65,
        $6e, $61, $6d, $65, $14, 0, $b, 0, $1f, 0, $1, 0, $8, 0, 0, 0, $4d, $61, $74, $65,
        $72, $69, $61, $6c, $a, 0, $5, 0, $4d, $ab, $82, $3d, $da, $62, $cf, $11, $ab, $39, 0, $20,
        $af, $71, $e4, $33, $1, 0, $9, 0, 0, 0, $43, $6f, $6c, $6f, $72, $52, $47, $42, $41, $1,
        0, $9, 0, 0, 0, $66, $61, $63, $65, $43, $6f, $6c, $6f, $72, $14, 0, $2a, 0, $1, 0,
        $5, 0, 0, 0, $70, $6f, $77, $65, $72, $14, 0, $1, 0, $8, 0, 0, 0, $43, $6f, $6c,
        $6f, $72, $52, $47, $42, $1, 0, $d, 0, 0, 0, $73, $70, $65, $63, $75, $6c, $61, $72, $43,
        $6f, $6c, $6f, $72, $14, 0, $1, 0, $8, 0, 0, 0, $43, $6f, $6c, $6f, $72, $52, $47, $42,
        $1, 0, $d, 0, 0, 0, $65, $6d, $69, $73, $73, $69, $76, $65, $43, $6f, $6c, $6f, $72, $14,
        0, $e, 0, $12, 0, $12, 0, $12, 0, $f, 0, $b, 0, $1f, 0, $1, 0, $8, 0, 0,
        0, $4d, $65, $73, $68, $46, $61, $63, $65, $a, 0, $5, 0, $5f, $ab, $82, $3d, $da, $62, $cf,
        $11, $ab, $39, 0, $20, $af, $71, $e4, $33, $29, 0, $1, 0, $12, 0, 0, 0, $6e, $46, $61,
        $63, $65, $56, $65, $72, $74, $65, $78, $49, $6e, $64, $69, $63, $65, $73, $14, 0, $34, 0, $29,
        0, $1, 0, $11, 0, 0, 0, $66, $61, $63, $65, $56, $65, $72, $74, $65, $78, $49, $6e, $64,
        $69, $63, $65, $73, $e, 0, $1, 0, $12, 0, 0, 0, $6e, $46, $61, $63, $65, $56, $65, $72,
        $74, $65, $78, $49, $6e, $64, $69, $63, $65, $73, $f, 0, $14, 0, $b, 0, $1f, 0, $1, 0,
        $d, 0, 0, 0, $4d, $65, $73, $68, $46, $61, $63, $65, $57, $72, $61, $70, $73, $a, 0, $5,
        0, $c0, $c5, $1e, $ed, $a8, $c0, $d0, $11, $94, $1c, 0, $80, $c8, $c, $fa, $7b, $29, 0, $1,
        0, $f, 0, 0, 0, $6e, $46, $61, $63, $65, $57, $72, $61, $70, $56, $61, $6c, $75, $65, $73,
        $14, 0, $34, 0, $1, 0, $9, 0, 0, 0, $42, $6f, $6f, $6c, $65, $61, $6e, $32, $64, $1,
        0, $e, 0, 0, 0, $66, $61, $63, $65, $57, $72, $61, $70, $56, $61, $6c, $75, $65, $73, $e,
        0, $1, 0, $f, 0, 0, 0, $6e, $46, $61, $63, $65, $57, $72, $61, $70, $56, $61, $6c, $75,
        $65, $73, $f, 0, $14, 0, $b, 0, $1f, 0, $1, 0, $11, 0, 0, 0, $4d, $65, $73, $68,
        $54, $65, $78, $74, $75, $72, $65, $43, $6f, $6f, $72, $64, $73, $a, 0, $5, 0, $40, $3f, $f2,
        $f6, $86, $76, $cf, $11, $8f, $52, 0, $40, $33, $35, $94, $a3, $29, 0, $1, 0, $e, 0, 0,
        0, $6e, $54, $65, $78, $74, $75, $72, $65, $43, $6f, $6f, $72, $64, $73, $14, 0, $34, 0, $1,
        0, $8, 0, 0, 0, $43, $6f, $6f, $72, $64, $73, $32, $64, $1, 0, $d, 0, 0, 0, $74,
        $65, $78, $74, $75, $72, $65, $43, $6f, $6f, $72, $64, $73, $e, 0, $1, 0, $e, 0, 0, 0,
        $6e, $54, $65, $78, $74, $75, $72, $65, $43, $6f, $6f, $72, $64, $73, $f, 0, $14, 0, $b, 0,
        $1f, 0, $1, 0, $10, 0, 0, 0, $4d, $65, $73, $68, $4d, $61, $74, $65, $72, $69, $61, $6c,
        $4c, $69, $73, $74, $a, 0, $5, 0, $42, $3f, $f2, $f6, $86, $76, $cf, $11, $8f, $52, 0, $40,
        $33, $35, $94, $a3, $29, 0, $1, 0, $a, 0, 0, 0, $6e, $4d, $61, $74, $65, $72, $69, $61,
        $6c, $73, $14, 0, $29, 0, $1, 0, $c, 0, 0, 0, $6e, $46, $61, $63, $65, $49, $6e, $64,
        $65, $78, $65, $73, $14, 0, $34, 0, $29, 0, $1, 0, $b, 0, 0, 0, $66, $61, $63, $65,
        $49, $6e, $64, $65, $78, $65, $73, $e, 0, $1, 0, $c, 0, 0, 0, $6e, $46, $61, $63, $65,
        $49, $6e, $64, $65, $78, $65, $73, $f, 0, $14, 0, $e, 0, $1, 0, $8, 0, 0, 0, $4d,
        $61, $74, $65, $72, $69, $61, $6c, $f, 0, $b, 0, $1f, 0, $1, 0, $b, 0, 0, 0, $4d,
        $65, $73, $68, $4e, $6f, $72, $6d, $61, $6c, $73, $a, 0, $5, 0, $43, $3f, $f2, $f6, $86, $76,
        $cf, $11, $8f, $52, 0, $40, $33, $35, $94, $a3, $29, 0, $1, 0, $8, 0, 0, 0, $6e, $4e,
        $6f, $72, $6d, $61, $6c, $73, $14, 0, $34, 0, $1, 0, $6, 0, 0, 0, $56, $65, $63, $74,
        $6f, $72, $1, 0, $7, 0, 0, 0, $6e, $6f, $72, $6d, $61, $6c, $73, $e, 0, $1, 0, $8,
        0, 0, 0, $6e, $4e, $6f, $72, $6d, $61, $6c, $73, $f, 0, $14, 0, $29, 0, $1, 0, $c,
        0, 0, 0, $6e, $46, $61, $63, $65, $4e, $6f, $72, $6d, $61, $6c, $73, $14, 0, $34, 0, $1,
        0, $8, 0, 0, 0, $4d, $65, $73, $68, $46, $61, $63, $65, $1, 0, $b, 0, 0, 0, $66,
        $61, $63, $65, $4e, $6f, $72, $6d, $61, $6c, $73, $e, 0, $1, 0, $c, 0, 0, 0, $6e, $46,
        $61, $63, $65, $4e, $6f, $72, $6d, $61, $6c, $73, $f, 0, $14, 0, $b, 0, $1f, 0, $1, 0,
        $10, 0, 0, 0, $4d, $65, $73, $68, $56, $65, $72, $74, $65, $78, $43, $6f, $6c, $6f, $72, $73,
        $a, 0, $5, 0, $21, $b8, $30, $16, $42, $78, $cf, $11, $8f, $52, 0, $40, $33, $35, $94, $a3,
        $29, 0, $1, 0, $d, 0, 0, 0, $6e, $56, $65, $72, $74, $65, $78, $43, $6f, $6c, $6f, $72,
        $73, $14, 0, $34, 0, $1, 0, $c, 0, 0, 0, $49, $6e, $64, $65, $78, $65, $64, $43, $6f,
        $6c, $6f, $72, $1, 0, $c, 0, 0, 0, $76, $65, $72, $74, $65, $78, $43, $6f, $6c, $6f, $72,
        $73, $e, 0, $1, 0, $d, 0, 0, 0, $6e, $56, $65, $72, $74, $65, $78, $43, $6f, $6c, $6f,
        $72, $73, $f, 0, $14, 0, $b, 0, $1f, 0, $1, 0, $4, 0, 0, 0, $4d, $65, $73, $68,
        $a, 0, $5, 0, $44, $ab, $82, $3d, $da, $62, $cf, $11, $ab, $39, 0, $20, $af, $71, $e4, $33,
        $29, 0, $1, 0, $9, 0, 0, 0, $6e, $56, $65, $72, $74, $69, $63, $65, $73, $14, 0, $34,
        0, $1, 0, $6, 0, 0, 0, $56, $65, $63, $74, $6f, $72, $1, 0, $8, 0, 0, 0, $76,
        $65, $72, $74, $69, $63, $65, $73, $e, 0, $1, 0, $9, 0, 0, 0, $6e, $56, $65, $72, $74,
        $69, $63, $65, $73, $f, 0, $14, 0, $29, 0, $1, 0, $6, 0, 0, 0, $6e, $46, $61, $63,
        $65, $73, $14, 0, $34, 0, $1, 0, $8, 0, 0, 0, $4d, $65, $73, $68, $46, $61, $63, $65,
        $1, 0, $5, 0, 0, 0, $66, $61, $63, $65, $73, $e, 0, $1, 0, $6, 0, 0, 0, $6e,
        $46, $61, $63, $65, $73, $f, 0, $14, 0, $e, 0, $12, 0, $12, 0, $12, 0, $f, 0, $b,
        0, $1f, 0, $1, 0, $14, 0, 0, 0, $46, $72, $61, $6d, $65, $54, $72, $61, $6e, $73, $66,
        $6f, $72, $6d, $4d, $61, $74, $72, $69, $78, $a, 0, $5, 0, $41, $3f, $f2, $f6, $86, $76, $cf,
        $11, $8f, $52, 0, $40, $33, $35, $94, $a3, $1, 0, $9, 0, 0, 0, $4d, $61, $74, $72, $69,
        $78, $34, $78, $34, $1, 0, $b, 0, 0, 0, $66, $72, $61, $6d, $65, $4d, $61, $74, $72, $69,
        $78, $14, 0, $b, 0, $1f, 0, $1, 0, $5, 0, 0, 0, $46, $72, $61, $6d, $65, $a, 0,
        $5, 0, $46, $ab, $82, $3d, $da, $62, $cf, $11, $ab, $39, 0, $20, $af, $71, $e4, $33, $e, 0,
        $12, 0, $12, 0, $12, 0, $f, 0, $b, 0, $1f, 0, $1, 0, $9, 0, 0, 0, $46, $6c,
        $6f, $61, $74, $4b, $65, $79, $73, $a, 0, $5, 0, $a9, $46, $dd, $10, $5b, $77, $cf, $11, $8f,
        $52, 0, $40, $33, $35, $94, $a3, $29, 0, $1, 0, $7, 0, 0, 0, $6e, $56, $61, $6c, $75,
        $65, $73, $14, 0, $34, 0, $2a, 0, $1, 0, $6, 0, 0, 0, $76, $61, $6c, $75, $65, $73,
        $e, 0, $1, 0, $7, 0, 0, 0, $6e, $56, $61, $6c, $75, $65, $73, $f, 0, $14, 0, $b,
        0, $1f, 0, $1, 0, $e, 0, 0, 0, $54, $69, $6d, $65, $64, $46, $6c, $6f, $61, $74, $4b,
        $65, $79, $73, $a, 0, $5, 0, $80, $b1, $6, $f4, $3b, $7b, $cf, $11, $8f, $52, 0, $40, $33,
        $35, $94, $a3, $29, 0, $1, 0, $4, 0, 0, 0, $74, $69, $6d, $65, $14, 0, $1, 0, $9,
        0, 0, 0, $46, $6c, $6f, $61, $74, $4b, $65, $79, $73, $1, 0, $6, 0, 0, 0, $74, $66,
        $6b, $65, $79, $73, $14, 0, $b, 0, $1f, 0, $1, 0, $c, 0, 0, 0, $41, $6e, $69, $6d,
        $61, $74, $69, $6f, $6e, $4b, $65, $79, $a, 0, $5, 0, $a8, $46, $dd, $10, $5b, $77, $cf, $11,
        $8f, $52, 0, $40, $33, $35, $94, $a3, $29, 0, $1, 0, $7, 0, 0, 0, $6b, $65, $79, $54,
        $79, $70, $65, $14, 0, $29, 0, $1, 0, $5, 0, 0, 0, $6e, $4b, $65, $79, $73, $14, 0,
        $34, 0, $1, 0, $e, 0, 0, 0, $54, $69, $6d, $65, $64, $46, $6c, $6f, $61, $74, $4b, $65,
        $79, $73, $1, 0, $4, 0, 0, 0, $6b, $65, $79, $73, $e, 0, $1, 0, $5, 0, 0, 0,
        $6e, $4b, $65, $79, $73, $f, 0, $14, 0, $b, 0, $1f, 0, $1, 0, $10, 0, 0, 0, $41,
        $6e, $69, $6d, $61, $74, $69, $6f, $6e, $4f, $70, $74, $69, $6f, $6e, $73, $a, 0, $5, 0, $c0,
        $56, $bf, $e2, $f, $84, $cf, $11, $8f, $52, 0, $40, $33, $35, $94, $a3, $29, 0, $1, 0, $a,
        0, 0, 0, $6f, $70, $65, $6e, $63, $6c, $6f, $73, $65, $64, $14, 0, $29, 0, $1, 0, $f,
        0, 0, 0, $70, $6f, $73, $69, $74, $69, $6f, $6e, $71, $75, $61, $6c, $69, $74, $79, $14, 0,
        $b, 0, $1f, 0, $1, 0, $9, 0, 0, 0, $41, $6e, $69, $6d, $61, $74, $69, $6f, $6e, $a,
        0, $5, 0, $4f, $ab, $82, $3d, $da, $62, $cf, $11, $ab, $39, 0, $20, $af, $71, $e4, $33, $e,
        0, $12, 0, $12, 0, $12, 0, $f, 0, $b, 0, $1f, 0, $1, 0, $c, 0, 0, 0, $41,
        $6e, $69, $6d, $61, $74, $69, $6f, $6e, $53, $65, $74, $a, 0, $5, 0, $50, $ab, $82, $3d, $da,
        $62, $cf, $11, $ab, $39, 0, $20, $af, $71, $e4, $33, $e, 0, $1, 0, $9, 0, 0, 0, $41,
        $6e, $69, $6d, $61, $74, $69, $6f, $6e, $f, 0, $b, 0, $1f, 0, $1, 0, $a, 0, 0, 0,
        $49, $6e, $6c, $69, $6e, $65, $44, $61, $74, $61, $a, 0, $5, 0, $a0, $ee, $23, $3a, $b1, $94,
        $d0, $11, $ab, $39, 0, $20, $af, $71, $e4, $33, $e, 0, $1, 0, $6, 0, 0, 0, $42, $49,
        $4e, $41, $52, $59, $f, 0, $b, 0, $1f, 0, $1, 0, $3, 0, 0, 0, $55, $72, $6c, $a,
        0, $5, 0, $a1, $ee, $23, $3a, $b1, $94, $d0, $11, $ab, $39, 0, $20, $af, $71, $e4, $33, $29,
        0, $1, 0, $5, 0, 0, 0, $6e, $55, $72, $6c, $73, $14, 0, $34, 0, $31, 0, $1, 0,
        $4, 0, 0, 0, $75, $72, $6c, $73, $e, 0, $1, 0, $5, 0, 0, 0, $6e, $55, $72, $6c,
        $73, $f, 0, $14, 0, $b, 0, $1f, 0, $1, 0, $f, 0, 0, 0, $50, $72, $6f, $67, $72,
        $65, $73, $73, $69, $76, $65, $4d, $65, $73, $68, $a, 0, $5, 0, $60, $c3, $63, $8a, $7d, $99,
        $d0, $11, $94, $1c, 0, $80, $c8, $c, $fa, $7b, $e, 0, $1, 0, $3, 0, 0, 0, $55, $72,
        $6c, $13, 0, $1, 0, $a, 0, 0, 0, $49, $6e, $6c, $69, $6e, $65, $44, $61, $74, $61, $f,
        0, $b, 0, $1f, 0, $1, 0, $4, 0, 0, 0, $47, $75, $69, $64, $a, 0, $5, 0, $e0,
        $90, $27, $a4, $10, $78, $cf, $11, $8f, $52, 0, $40, $33, $35, $94, $a3, $29, 0, $1, 0, $5,
        0, 0, 0, $64, $61, $74, $61, $31, $14, 0, $28, 0, $1, 0, $5, 0, 0, 0, $64, $61,
        $74, $61, $32, $14, 0, $28, 0, $1, 0, $5, 0, 0, 0, $64, $61, $74, $61, $33, $14, 0,
        $34, 0, $2d, 0, $1, 0, $5, 0, 0, 0, $64, $61, $74, $61, $34, $e, 0, $3, 0, $8,
        0, 0, 0, $f, 0, $14, 0, $b, 0, $1f, 0, $1, 0, $e, 0, 0, 0, $53, $74, $72,
        $69, $6e, $67, $50, $72, $6f, $70, $65, $72, $74, $79, $a, 0, $5, 0, $e0, $21, $f, $7f, $e1,
        $bf, $d1, $11, $82, $c0, 0, $a0, $c9, $69, $72, $71, $31, 0, $1, 0, $3, 0, 0, 0, $6b,
        $65, $79, $14, 0, $31, 0, $1, 0, $5, 0, 0, 0, $76, $61, $6c, $75, $65, $14, 0, $b,
        0, $1f, 0, $1, 0, $b, 0, 0, 0, $50, $72, $6f, $70, $65, $72, $74, $79, $42, $61, $67,
        $a, 0, $5, 0, $e1, $21, $f, $7f, $e1, $bf, $d1, $11, $82, $c0, 0, $a0, $c9, $69, $72, $71,
        $e, 0, $1, 0, $e, 0, 0, 0, $53, $74, $72, $69, $6e, $67, $50, $72, $6f, $70, $65, $72,
        $74, $79, $f, 0, $b, 0, $1f, 0, $1, 0, $e, 0, 0, 0, $45, $78, $74, $65, $72, $6e,
        $61, $6c, $56, $69, $73, $75, $61, $6c, $a, 0, $5, 0, $a0, $6a, $11, $98, $ba, $bd, $d1, $11,
        $82, $c0, 0, $a0, $c9, $69, $72, $71, $1, 0, $4, 0, 0, 0, $47, $75, $69, $64, $1, 0,
        $12, 0, 0, 0, $67, $75, $69, $64, $45, $78, $74, $65, $72, $6e, $61, $6c, $56, $69, $73, $75,
        $61, $6c, $14, 0, $e, 0, $12, 0, $12, 0, $12, 0, $f, 0, $b, 0, $1f, 0, $1, 0,
        $b, 0, 0, 0, $52, $69, $67, $68, $74, $48, $61, $6e, $64, $65, $64, $a, 0, $5, 0, $a0,
        $5e, $5d, $7f, $3a, $d5, $d1, $11, $82, $c0, 0, $a0, $c9, $69, $72, $71, $29, 0, $1, 0, $c,
        0, 0, 0, $62, $52, $69, $67, $68, $74, $48, $61, $6e, $64, $65, $64, $14, 0, $b, 0);

(*)
 *******************************************************************************
 * DirectX File errors.
 *******************************************************************************
(*)

const
  _FACD3D = $876;

//#define MAKE_DDHRESULT( code )  MAKE_HRESULT( 1, _FACD3D, code )
function MAKE_DDHRESULT(Code : LongWord) : LongWord;

const
  MAKE_DDHRESULT_D = (1 shl 31) or (_FACD3D shl 16);

  DXFILE_OK                           = 0;

  DXFILEERR_BADOBJECT                 = HResult(MAKE_DDHRESULT_D or 850);
  DXFILEERR_BADVALUE                  = HResult(MAKE_DDHRESULT_D or 851);
  DXFILEERR_BADTYPE                   = HResult(MAKE_DDHRESULT_D or 852);
  DXFILEERR_BADSTREAMHANDLE           = HResult(MAKE_DDHRESULT_D or 853);
  DXFILEERR_BADALLOC                  = HResult(MAKE_DDHRESULT_D or 854);
  DXFILEERR_NOTFOUND                  = HResult(MAKE_DDHRESULT_D or 855);
  DXFILEERR_NOTDONEYET                = HResult(MAKE_DDHRESULT_D or 856);
  DXFILEERR_FILENOTFOUND              = HResult(MAKE_DDHRESULT_D or 857);
  DXFILEERR_RESOURCENOTFOUND          = HResult(MAKE_DDHRESULT_D or 858);
  DXFILEERR_URLNOTFOUND               = HResult(MAKE_DDHRESULT_D or 859);
  DXFILEERR_BADRESOURCE               = HResult(MAKE_DDHRESULT_D or 860);
  DXFILEERR_BADFILETYPE               = HResult(MAKE_DDHRESULT_D or 861);
  DXFILEERR_BADFILEVERSION            = HResult(MAKE_DDHRESULT_D or 862);
  DXFILEERR_BADFILEFLOATSIZE          = HResult(MAKE_DDHRESULT_D or 863);
  DXFILEERR_BADFILECOMPRESSIONTYPE    = HResult(MAKE_DDHRESULT_D or 864);
  DXFILEERR_BADFILE                   = HResult(MAKE_DDHRESULT_D or 865);
  DXFILEERR_PARSEERROR                = HResult(MAKE_DDHRESULT_D or 866);
  DXFILEERR_NOTEMPLATE                = HResult(MAKE_DDHRESULT_D or 867);
  DXFILEERR_BADARRAYSIZE              = HResult(MAKE_DDHRESULT_D or 868);
  DXFILEERR_BADDATAREFERENCE          = HResult(MAKE_DDHRESULT_D or 869);
  DXFILEERR_INTERNALERROR             = HResult(MAKE_DDHRESULT_D or 870);
  DXFILEERR_NOMOREOBJECTS             = HResult(MAKE_DDHRESULT_D or 871);
  DXFILEERR_BADINTRINSICS             = HResult(MAKE_DDHRESULT_D or 872);
  DXFILEERR_NOMORESTREAMHANDLES       = HResult(MAKE_DDHRESULT_D or 873);
  DXFILEERR_NOMOREDATA                = HResult(MAKE_DDHRESULT_D or 874);
  DXFILEERR_BADCACHEFILE              = HResult(MAKE_DDHRESULT_D or 875);
  DXFILEERR_NOINTERNET                = HResult(MAKE_DDHRESULT_D or 876);

implementation

function MAKE_DDHRESULT(Code : LongWord) : LongWord;
begin
  Result := LongWord((1 shl 31) or (_FACD3D shl 16)) or Code;
end;

function DXFileErrorString(DXErrorCode : HResult) : String;
begin
  case DXErrorCode of
    HResult(DXFILE_OK)                        : Result := 'Command completed successfully.';
    HResult(DXFILEERR_BADOBJECT)              : Result := 'Object is invalid.';
    HResult(DXFILEERR_BADVALUE)               : Result := 'Parameter is invalid.';
    HResult(DXFILEERR_BADTYPE)                : Result := 'Object type is invalid.';
    HResult(DXFILEERR_BADSTREAMHANDLE)        : Result := 'Streamhandle is invalid.'; //Not in Docu.
    HResult(DXFILEERR_BADALLOC)               : Result := 'Memory allocation failed.';
    HResult(DXFILEERR_NOTFOUND)               : Result := 'Object could not be found.';
    HResult(DXFILEERR_NOTDONEYET)             : Result := 'Operation has not completed.';
    HResult(DXFILEERR_FILENOTFOUND)           : Result := 'File could not be found.';
    HResult(DXFILEERR_RESOURCENOTFOUND)       : Result := 'Resource could not be found.';
    HResult(DXFILEERR_URLNOTFOUND)            : Result := 'URL could not be found';
    HResult(DXFILEERR_BADRESOURCE)            : Result := 'Resource is invalid.';
    HResult(DXFILEERR_BADFILETYPE)            : Result := 'File is not a DirectX (.x) file.';
    HResult(DXFILEERR_BADFILEVERSION)         : Result := 'File version is not valid.';
    HResult(DXFILEERR_BADFILEFLOATSIZE)       : Result := 'Floating-point size is invalid.';
    HResult(DXFILEERR_BADFILECOMPRESSIONTYPE) : Result := 'File compression type is invalid.';
    HResult(DXFILEERR_BADFILE)                : Result := 'File is invalid.';
    HResult(DXFILEERR_PARSEERROR)             : Result := 'File could not be parsed.';
    HResult(DXFILEERR_NOTEMPLATE)             : Result := 'No template available.';
    HResult(DXFILEERR_BADARRAYSIZE)           : Result := 'Array size is invalid.';
    HResult(DXFILEERR_BADDATAREFERENCE)       : Result := 'Data reference is invalid.';
    HResult(DXFILEERR_INTERNALERROR)          : Result := 'Internal error occurred.';
    HResult(DXFILEERR_NOMOREOBJECTS)          : Result := 'All objects have been enumerated.';
    HResult(DXFILEERR_BADINTRINSICS)          : Result := 'Intrinsics are invalid.';
    HResult(DXFILEERR_NOMORESTREAMHANDLES)    : Result := 'No stream handles are available.';
    HResult(DXFILEERR_NOMOREDATA)             : Result := 'No further data is available.';
    HResult(DXFILEERR_BADCACHEFILE)           : Result := 'The cache file containing the .x file date is invalid. A cache file contains data retrieved from the network, cached on the hard disk, and retrieved in subsequent requests.';
    HResult(DXFILEERR_NOINTERNET)             : Result := 'Internet connection not found.';
    else Result := 'Unknown Error';
  end;
end;

end.
