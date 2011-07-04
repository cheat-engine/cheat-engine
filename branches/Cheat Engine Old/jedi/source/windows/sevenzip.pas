{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ interface of the 'sevenzip' (http://sourceforge.net/projects/sevenzip/) compression library      }
{ version 4.57, December 6th, 2007                                                                 }
{                                                                                                  }
{ Copyright (C) 1999-2007 Igor Pavlov                                                              }
{                                                                                                  }
{ GNU LGPL information                                                                             }
{ --------------------                                                                             }
{                                                                                                  }
{    This library is free software; you can redistribute it and/or modify it under the terms of    }
{    the GNU Lesser General Public License as published by the Free Software Foundation; either    }
{    version 2.1 of the License, or (at your option) any later version.                            }
{                                                                                                  }
{    This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;     }
{    without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.     }
{    See the GNU Lesser General Public License for more details.                                   }
{                                                                                                  }
{    You should have received a copy of the GNU Lesser General Public License along with this      }
{    library; if not, write to                                                                     }
{    the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA   }
{                                                                                                  }
{ unRAR restriction                                                                                }
{ -----------------                                                                                }
{                                                                                                  }
{    The decompression engine for RAR archives was developed using source code of unRAR program.   }
{    All copyrights to original unRAR code are owned by Alexander Roshal.                          }
{                                                                                                  }
{    The license for original unRAR code has the following restriction:                            }
{                                                                                                  }
{      The unRAR sources cannot be used to re-create the RAR compression algorithm,                }
{      which is proprietary. Distribution of modified unRAR sources in separate form               }
{      or as a part of other software is permitted, provided that it is clearly                    }
{      stated in the documentation and source comments that the code may                           }
{      not be used to develop a RAR (WinRAR) compatible archiver.                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Translation 2007-2008 Florent Ouchet for the Jedi Code Library                                   }
{ Contributors:                                                                                    }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-05-04 14:52:03 +0200 (dim., 04 mai 2008)                          $ }
{ Revision:      $Rev:: 2368                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit sevenzip;

interface

{$I jcl.inc}

uses
  JclBase,
  Windows,
  ActiveX;

const
  CLSID_CCodec : TGUID = '{23170F69-40C1-2790-0000-000000000000}';
  CLSID_CCodecBCJ2    : TGUID = '{23170F69-40C1-2790-1B01-030300000000}'; // BCJ2 0303011B
  CLSID_CCodecBCJ     : TGUID = '{23170F69-40C1-2790-0301-030300000000}'; // BCJ  03030103
  CLSID_CCodecSWAP2   : TGUID = '{23170F69-40C1-2790-0203-030000000000}'; // swap2 020302
  CLSID_CCodecSWAP4   : TGUID = '{23170F69-40C1-2790-0403-020000000000}'; // swap4 020304
  CLSID_CCodecBPPC    : TGUID = '{23170F69-40C1-2790-0502-030300000000}'; // branch ppc 03030205
  CLSID_CCodecBIA64   : TGUID = '{23170F69-40C1-2790-0104-030300000000}'; // branch IA64 03030401
  CLSID_CCodecBARM    : TGUID = '{23170F69-40C1-2790-0105-030300000000}'; // branch ARM  03030501
  CLSID_CCodecBARMT   : TGUID = '{23170F69-40C1-2790-0107-030300000000}'; // branch ARM Thumb 03030701
  CLSID_CCodecBARMS   : TGUID = '{23170F69-40C1-2790-0508-030300000000}'; // branch ARM Sparc 03030805
  CLSID_CCodecBZIP    : TGUID = '{23170F69-40C1-2790-0202-040000000000}'; // bzip2 040202
  CLSID_CCodecCOPY    : TGUID = '{23170F69-40C1-2790-0000-000000000000}'; // copy 0
  CLSID_CCodecDEF64   : TGUID = '{23170F69-40C1-2790-0901-040000000000}'; // deflate64 040109
  CLSID_CCodecDEFNSIS : TGUID = '{23170F69-40C1-2790-0109-040000000000}'; // deflate nsis 040901
  CLSID_CCodecDEFREG  : TGUID = '{23170F69-40C1-2790-0801-040000000000}'; // deflate register 040108
  CLSID_CCodecLZMA    : TGUID = '{23170F69-40C1-2790-0101-030000000000}'; // lzma 030101
  CLSID_CCodecPPMD    : TGUID = '{23170F69-40C1-2790-0104-030000000000}'; // ppmd 030401
  CLSID_CCodecRAR1    : TGUID = '{23170F69-40C1-2790-0103-040000000000}'; // rar1 040301
  CLSID_CCodecRAR2    : TGUID = '{23170F69-40C1-2790-0203-040000000000}'; // rar2 040302
  CLSID_CCodecRAR3    : TGUID = '{23170F69-40C1-2790-0303-040000000000}'; // rar3 040303
  CLSID_CAESCodec     : TGUID = '{23170F69-40C1-2790-0107-F10600000000}'; // AES 06F10701

  CLSID_CArchiveHandler : TGUID = '{23170F69-40C1-278A-1000-000110000000}';
  CLSID_CFormatZip      : TGUID = '{23170F69-40C1-278A-1000-000110010000}';
  CLSID_CFormatBZ2      : TGUID = '{23170F69-40C1-278A-1000-000110020000}';
  CLSID_CFormatRar      : TGUID = '{23170F69-40C1-278A-1000-000110030000}';
  CLSID_CFormatArj      : TGUID = '{23170F69-40C1-278A-1000-000110040000}';
  CLSID_CFormatZ        : TGUID = '{23170F69-40C1-278A-1000-000110050000}';
  CLSID_CFormatLzh      : TGUID = '{23170F69-40C1-278A-1000-000110060000}';
  CLSID_CFormat7z       : TGUID = '{23170F69-40C1-278A-1000-000110070000}';
  CLSID_CFormatCab      : TGUID = '{23170F69-40C1-278A-1000-000110080000}';
  CLSID_CFormatNsis     : TGUID = '{23170F69-40C1-278A-1000-000110090000}';
  //CLSID_CFormatLzma     : TGUID = '{23170F69-40C1-278A-1000-0001100A0000}';  not in 4.57
  CLSID_CFormatCompound : TGUID = '{23170F69-40C1-278A-1000-000110E50000}';
  CLSID_CFormatWim      : TGUID = '{23170F69-40C1-278A-1000-000110E60000}';
  CLSID_CFormatIso      : TGUID = '{23170F69-40C1-278A-1000-000110E70000}';
  //CLSID_CFormatBkf      : TGUID = '{23170F69-40C1-278A-1000-000110E80000}';  not in 4.57
  CLSID_CFormatChm      : TGUID = '{23170F69-40C1-278A-1000-000110E90000}';
  CLSID_CFormatSplit    : TGUID = '{23170F69-40C1-278A-1000-000110EA0000}';
  CLSID_CFormatRpm      : TGUID = '{23170F69-40C1-278A-1000-000110EB0000}';
  CLSID_CFormatDeb      : TGUID = '{23170F69-40C1-278A-1000-000110EC0000}';
  CLSID_CFormatCpio     : TGUID = '{23170F69-40C1-278A-1000-000110ED0000}';
  CLSID_CFormatTar      : TGUID = '{23170F69-40C1-278A-1000-000110EE0000}';
  CLSID_CFormatGZip     : TGUID = '{23170F69-40C1-278A-1000-000110EF0000}';

// IStream.h
type
  // "23170F69-40C1-278A-0000-000300xx0000"
  ISequentialInStream = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000300010000}']
    function Read(Data: Pointer; Size: Cardinal; ProcessedSize: PCardinal): HRESULT; stdcall;
    {Out: if size != 0, return_value = S_OK and (*processedSize == 0),
     then there are no more bytes in stream.
     if (size > 0) && there are bytes in stream,
     this function must read at least 1 byte.
     This function is allowed to read less than number of remaining bytes in stream.
     You must call Read function in loop, if you need exact amount of data}
  end;

  ISequentialOutStream = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000300020000}']
    function Write(Data: Pointer; Size: Cardinal; ProcessedSize: PCardinal): HRESULT; stdcall;
    {if (size > 0) this function must write at least 1 byte.
     This function is allowed to write less than "size".
     You must call Write function in loop, if you need to write exact amount of data}
  end;

  IInStream = interface(ISequentialInStream)
    ['{23170F69-40C1-278A-0000-000300030000}']
    function Seek(Offset: Int64; SeekOrigin: Cardinal; NewPosition: PInt64): HRESULT; stdcall;
  end;

  IOutStream = interface(ISequentialOutStream)
    ['{23170F69-40C1-278A-0000-000300040000}']
    function Seek(Offset: Int64; SeekOrigin: Cardinal; NewPosition: PInt64): HRESULT; stdcall;
    function SetSize(NewSize: Int64): HRESULT; stdcall;
  end;

  IStreamGetSize = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000300060000}']
    function GetSize(Size: PInt64): HRESULT; stdcall;
  end;

  IOutStreamFlush = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000300070000}']
    function Flush: HRESULT; stdcall;
  end;

// PropID.h
const
  kpidNoProperty = 0;
  kpidHandlerItemIndex = 2;
  kpidPath = 3;
  kpidName = 4;
  kpidExtension = 5;
  kpidIsFolder = 6;
  kpidSize = 7;
  kpidPackedSize = 8;
  kpidAttributes = 9;
  kpidCreationTime = 10;
  kpidLastAccessTime = 11;
  kpidLastWriteTime = 12;
  kpidSolid = 13;
  kpidCommented = 14;
  kpidEncrypted = 15;
  kpidSplitBefore = 16;
  kpidSplitAfter = 17;
  kpidDictionarySize = 18;
  kpidCRC = 19;
  kpidType = 20;
  kpidIsAnti = 21;
  kpidMethod = 22;
  kpidHostOS = 23;
  kpidFileSystem = 24;
  kpidUser = 25;
  kpidGroup = 26;
  kpidBlock = 27;
  kpidComment = 28;
  kpidPosition = 29;
  kpidPrefix = 30;

  kpidTotalSize = $1100;
  kpidFreeSpace = $1101;
  kpidClusterSize = $1102;
  kpidVolumeName = $1103;

  kpidLocalName = $1200;
  kpidProvider = $1201;

  kpidUserDefined = $10000;

// HandlerOut.cpp

  kCopyMethodName = WideString('Copy');
  kLZMAMethodName = WideString('LZMA');
  kLZMA2MethodName = WideString('LZMA2');
  kBZip2MethodName = WideString('BZip2');
  kPpmdMethodName = WideString('PPMd');
  kDeflateMethodName = WideString('Deflate');
  kDeflate64MethodName = WideString('Deflate64');

  kAES128MethodName = WideString('AES128');
  kAES192MethodName = WideString('AES192');
  kAES256MethodName = WideString('AES256');
  kZipCryptoMethodName = WideString('ZIPCRYPTO');

// ICoder.h
type
  ICompressProgressInfo = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400040000}']
    function SetRatioInfo(InSize: PInt64; OutSize: PInt64): HRESULT; stdcall;
  end;

  ICompressCoder = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400050000}']
    function Code(InStream: ISequentialInStream; OutStream: ISequentialOutStream;
      InSize, OutSize: PInt64; Progress: ICompressProgressInfo): HRESULT; stdcall;
  end;

  PISequentialInStream = ^ISequentialInStream;
  PISequentialOutStream = ^ISequentialOutStream;

  ICompressCoder2 = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400180000}']
    function Code(InStreams: PISequentialInStream; InSizes: JclBase.PPInt64; NumInStreams: Cardinal;
      OutStreams: PISequentialOutStream; OutSizes: JclBase.PPInt64; NumOutStreams: Cardinal;
      Progress: ICompressProgressInfo): HRESULT; stdcall;
  end;

const
  kDictionarySize = $400;
  kUsedMemorySize = $401;
  kOrder = $402;
  kPosStateBits = $440;
  kLitContextBits = $441;
  kLitPosBits = $442;
  kNumFastBytes = $450;
  kMatchFinder = $451;
  kMatchFinderCycles = $452;
  kNumPasses = $460;
  kAlgorithm = $470;
  kMultiThread = $480;
  kNumThreads = $481;
  kEndMarker = $490;

type
  ICompressSetCoderProperties = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400200000}']
    function SetCoderProperties(PropIDs: PPropID; Properties: PPropVariant;
      NumProperties: Cardinal): HRESULT; stdcall;
  end;

  ICompressSetDecoderProperties2 = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400220000}']
    function SetDecoderProperties2(Data: PByte; Size: Cardinal): HRESULT; stdcall;
  end;

  ICompressWriteCoderProperties = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400230000}']
    function WriteCoderProperties(OutStream: ISequentialOutStream): HRESULT; stdcall;
  end;

  ICompressGetInStreamProcessedSize = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400240000}']
    function GetInStreamProcessedSize(Value: PInt64): HRESULT; stdcall;
  end;

  ICompressSetCoderMt = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400250000}']
    function SetNumberOfThreads(NumThreads: Cardinal): HRESULT; stdcall;
  end;

  ICompressGetSubStreamSize = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400300000}']
    function GetSubStreamSize(SubStream: Int64; out Value: Int64): HRESULT; stdcall;
  end;

  ICompressSetInStream = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400310000}']
    function SetInStream(InStream: ISequentialInStream): HRESULT; stdcall;
    function ReleaseInStream: HRESULT; stdcall;
  end;

  ICompressSetOutStream = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400320000}']
    function SetOutStream(OutStream: ISequentialOutStream): HRESULT; stdcall;
    function ReleaseOutStream: HRESULT; stdcall;
  end;

  ICompressSetInStreamSize = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400330000}']
    function SetInStreamSize(InSize: PInt64): HRESULT; stdcall;
  end;

  ICompressSetOutStreamSize = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400340000}']
    function SetOutStreamSize(OutSize: PInt64): HRESULT; stdcall;
  end;

  ICompressFilter = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400400000}']
    function Init: HRESULT; stdcall;
    function Filter(Data: PByte; Size: Cardinal): Cardinal; stdcall;
    // Filter return outSize (UInt32)
    // if (outSize <= size): Filter have converted outSize bytes
    // if (outSize > size): Filter have not converted anything.
    //      and it needs at least outSize bytes to convert one block
    //      (it's for crypto block algorithms).
  end;

  ICompressCodecsInfo = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400600000}']
    function GetNumberOfMethods(NumMethods: PCardinal): HRESULT; stdcall;
    function GetProperty(Index: Cardinal; PropID: TPropID; out Value: TPropVariant): HRESULT; stdcall;
    function CreateDecoder(Index: Cardinal; IID: PGUID; out Decoder): HRESULT; stdcall;
    function CreateEncoder(Index: Cardinal; IID: PGUID; out Coder): HRESULT; stdcall;
  end;

  ISetCompressCodecsInfo = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400610000}']
    function SetCompressCodecsInfo(CompressCodecsInfo: ICompressCodecsInfo): HRESULT; stdcall;
  end;

  ICryptoProperties = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400800000}']
    function SetKey(Data: PByte; Size: Cardinal): HRESULT; stdcall;
    function SetInitVector(Data: PByte; Size: Cardinal): HRESULT; stdcall;
  end;

  ICryptoSetPassword = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400900000}']
    function CryptoSetPassword(Data: PByte; Size: Cardinal): HRESULT; stdcall;
  end;

  ICryptoSetCRC = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000400A00000}']
    function CryptoSetCRC(crc: Cardinal): HRESULT; stdcall;
  end;

const
  kID = 0;
  kName = 1;
  kDecoder = 2;
  kEncoder = 3;
  kInStreams = 4;
  kOutStreams = 5;
  kDescription = 6;
  kDecoderIsAssigned = 7;
  kEncoderIsAssigned = 8;

// IProgress.h
type
  IProgress = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000000050000}']
    function SetTotal(Total: Int64): HRESULT; stdcall;
    function SetCompleted(CompleteValue: PInt64): HRESULT; stdcall;
  end;
  
// IArchive.h
const
  // file time type
  kWindows = 0;
  kUnix = 1;
  kDOS = 2;

  // archive
  kArchiveName = 0;
  kClassID = 1;
  kExtension = 2;
  kAddExtension = 3;
  kUpdate = 4;
  kKeepName = 5;
  kStartSignature = 6;
  kFinishSignature = 7;
  kAssociate = 8;

  // ask mode
  kExtract = 0;
  kTest = 1;
  kSkip = 2;

  // operation result
  kOK = 0;
  kUnSupportedMethod = 1;
  kDataError = 2;
  kCRCError = 3;

  kError = 1;

type
  // "23170F69-40C1-278A-0000-000600xx0000"
  IArchiveOpenCallback = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000600100000}']
    function SetTotal(Files: PInt64; Bytes: PInt64): HRESULT; stdcall;
    function SetCompleted(Files: PInt64; Bytes: PInt64): HRESULT; stdcall;
  end;

  IArchiveExtractCallback = interface(IProgress)
    ['{23170F69-40C1-278A-0000-000600200000}']
    function GetStream(Index: Cardinal; out OutStream: ISequentialOutStream;
      askExtractMode: Cardinal): HRESULT; stdcall;
    // GetStream OUT: S_OK - OK, S_FALSE - skeep this file
    function PrepareOperation(askExtractMode: Cardinal): HRESULT; stdcall;
    function SetOperationResult(resultEOperationResult: Integer): HRESULT; stdcall;
  end;

  IArchiveOpenVolumeCallback = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000600300000}']
    function GetProperty(PropID: TPropID; out Value: TPropVariant): HRESULT; stdcall;
    function GetStream(Name: PWideChar; out InStream: IInStream): HRESULT; stdcall;
  end;

  IInArchiveGetStream = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000600400000}']
    function GetStream(Index: Cardinal; out Stream: ISequentialInStream): HRESULT; stdcall;
  end;

  IArchiveOpenSetSubArchiveName = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000600500000}']
    function SetSubArchiveName(Name: PWideChar): HRESULT; stdcall;
  end;

  IInArchive = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000600600000}']
    function Open(Stream: IInStream; MaxCheckStartPosition: PInt64;
       OpenArchiveCallback: IArchiveOpenCallback): HRESULT; stdcall;
    function Close: HRESULT; stdcall;
    function GetNumberOfItems(NumItems: PCardinal): HRESULT; stdcall;
    function GetProperty(Index: Cardinal; PropID: TPropID;
      var Value: TPropVariant): HRESULT; stdcall;
    function Extract(Indices: PCardinal; NumItems: Cardinal;
      TestMode: Integer; ExtractCallback: IArchiveExtractCallback): HRESULT; stdcall;
    // indices must be sorted
    // numItems = 0xFFFFFFFF means all files
    // testMode != 0 means "test files operation"
    function GetArchiveProperty(PropID: TPropID; out Value: TPropVariant): HRESULT; stdcall;

    function GetNumberOfProperties(NumProperties: PCardinal): HRESULT; stdcall;
    function GetPropertyInfo(Index: Cardinal; out Name: TBStr; out PropID: TPropID;
      out VarType: TVarType): HRESULT; stdcall;

    function GetNumberOfArchiveProperties(NumProperties: PCardinal): HRESULT; stdcall;
    function GetArchivePropertyInfo(Index: Cardinal; out Name: TBStr; out PropID: TPropID;
      out VarType: TVarType): HRESULT; stdcall;
  end;

  IArchiveUpdateCallback = interface(IProgress)
    ['{23170F69-40C1-278A-0000-000600800000}']
    function GetUpdateItemInfo(Index: Cardinal;
      NewData: PInteger;        // 1 - new data, 0 - old data
      NewProperties: PInteger;  // 1 - new properties, 0 - old properties
      IndexInArchive: PCardinal // -1 if there is no in archive, or if doesn't matter
      ): HRESULT; stdcall;
    function GetProperty(Index: Cardinal; PropID: TPropID; out Value: TPropVariant): HRESULT; stdcall;
    function GetStream(Index: Cardinal; out InStream: ISequentialInStream): HRESULT; stdcall;
    function SetOperationResult(OperationResult: Integer): HRESULT; stdcall;
  end;

  IArchiveUpdateCallback2 = interface(IArchiveUpdateCallback)
    ['{23170F69-40C1-278A-0000-000600820000}']
    function GetVolumeSize(Index: Cardinal; Size: PInt64): HRESULT; stdcall;
    function GetVolumeStream(Index: Cardinal; out VolumeStream: ISequentialOutStream): HRESULT; stdcall;
  end;

  IOutArchive = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000600A00000}']
    function UpdateItems(OutStream: ISequentialOutStream; NumItems: Cardinal;
      UpdateCallback: IArchiveUpdateCallback): HRESULT; stdcall;
    function GetFileTimeType(Type_: PCardinal): HRESULT; stdcall;
  end;

  ISetProperties = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000600030000}']
    function SetProperties(Names: PPWideChar; Values: PPropVariant; NumProperties: Integer): HRESULT; stdcall;
  end;

// IPassword.h
type
  ICryptoGetTextPassword = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000500100000}']
    function CryptoGetTextPassword(password: PBStr): HRESULT; stdcall;
  end;

  ICryptoGetTextPassword2 = interface(IUnknown)
    ['{23170F69-40C1-278A-0000-000500110000}']
    function CryptoGetTextPassword2(PasswordIsDefined: PInteger;
      Password: PBStr): HRESULT; stdcall;
  end;

// ZipHandlerOut.cpp
const
  kDeflateAlgoX1 = 0;
  kDeflateAlgoX5 = 1;

  kDeflateNumPassesX1  = 1;
  kDeflateNumPassesX7  = 3;
  kDeflateNumPassesX9  = 10;

  kNumFastBytesX1 = 32;
  kNumFastBytesX7 = 64;
  kNumFastBytesX9 = 128;

  kBZip2NumPassesX1 = 1;
  kBZip2NumPassesX7 = 2;
  kBZip2NumPassesX9 = 7;

  kBZip2DicSizeX1 = 100000;
  kBZip2DicSizeX3 = 500000;
  kBZip2DicSizeX5 = 900000;

// HandlerOut.cpp
const
  kLzmaAlgoX1 = 0;
  kLzmaAlgoX5 = 1;

  kLzmaDicSizeX1 = 1 shl 16;
  kLzmaDicSizeX3 = 1 shl 20;
  kLzmaDicSizeX5 = 1 shl 24;
  kLzmaDicSizeX7 = 1 shl 25;
  kLzmaDicSizeX9 = 1 shl 26;

  kLzmaFastBytesX1 = 32;
  kLzmaFastBytesX7 = 64;

  kPpmdMemSizeX1 = (1 shl 22);
  kPpmdMemSizeX5 = (1 shl 24);
  kPpmdMemSizeX7 = (1 shl 26);
  kPpmdMemSizeX9 = (192 shl 20);

  kPpmdOrderX1 = 4;
  kPpmdOrderX5 = 6;
  kPpmdOrderX7 = 16;
  kPpmdOrderX9 = 32;

  kDeflateFastBytesX1 = 32;
  kDeflateFastBytesX7 = 64;
  kDeflateFastBytesX9 = 128;

{$IFDEF 7ZIP_STATICLINK}
function CreateObject(ClsID: PGUID; IID: PGUID; out Obj): HRESULT; stdcall;
function GetNumberOfFormats(NumFormats: PCardinal): HRESULT; stdcall;
function GetNumberOfMethods(NumMethods: PCardinal): HRESULT; stdcall;
{$ENDIF 7ZIP_STATICLINK}

{$IFDEF 7ZIP_LINKONREQUEST}
type
  TCreateObjectFunc = function (ClsID: PGUID; IID: PGUID; out Obj): HRESULT; stdcall;
  TGetNumberOfFormatsFunc = function (NumFormats: PCardinal): HRESULT; stdcall;
  TGetNumberOfMethodsFunc = function (NumMethods: PCardinal): HRESULT; stdcall;

var
  CreateObject: TCreateObjectFunc = nil;
  GetNumberOfFormats: TGetNumberOfFormatsFunc = nil;
  GetNumberOfMethods: TGetNumberOfMethodsFunc = nil;
{$ENDIF 7ZIP_LINKONREQUEST}

function Load7Zip: Boolean;
function Is7ZipLoaded: Boolean;
procedure Unload7Zip;

implementation

type
  {$IFDEF MSWINDOWS}
  TModuleHandle = HINST;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  TModuleHandle = Pointer;
  {$ENDIF LINUX}

const
  sz7Zip = '7z.dll';
  CreateObjectExportName = 'CreateObject';
  GetNumberOfFormatsExportName = 'GetNumberOfFormats';
  GetNumberOfMethodsExportName = 'GetNumberOfMethods';
  INVALID_MODULEHANDLE_VALUE = TModuleHandle(0);

{$IFDEF 7ZIP_LINKDLL}
function CreateObject; external sz7Zip name CreateObjectExportName;
function GetNumberOfFormats; external sz7Zip name GetNumberOfFormatsExportName;
function GetNumberOfMethods; external sz7Zip name GetNumberOfMethodsExportName;
{$ENDIF 7ZIP_LINKDLL}

{$IFDEF 7ZIP_LINKONREQUEST}
var
  SevenzipLib: TModuleHandle = INVALID_MODULEHANDLE_VALUE;
{$ENDIF 7ZIP_LINKONREQUEST}

function Load7Zip: Boolean;
{$IFDEF 7ZIP_LINKONREQUEST}
  function GetSymbol(SymbolName: PChar): Pointer;
  begin
    {$IFDEF MSWINDOWS}
    Result := GetProcAddress(SevenzipLib, PChar(SymbolName));
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    Result := dlsym(SevenzipLib, PChar(SymbolName));
    {$ENDIF UNIX}
  end;
begin
  Result := SevenzipLib <> INVALID_MODULEHANDLE_VALUE;
  if not Result then
  begin
    {$IFDEF MSWINDOWS}
    SevenzipLib := LoadLibrary(sz7Zip);
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    SevenzipLib := dlopen(PChar(sz7Zip), RTLD_NOW);
    {$ENDIF UNIX}
    Result := SevenzipLib <> INVALID_MODULEHANDLE_VALUE;
    if Result then
    begin
      @CreateObject := GetSymbol(CreateObjectExportName);
      @GetNumberOfFormats := GetSymbol(GetNumberOfFormatsExportName);
      @GetNumberOfMethods := GetSymbol(GetNumberOfMethodsExportName);
    end;
  end;
end;
{$ELSE ~7ZIP_LINKONREQUEST}
begin
  Result := True;
end;
  {$ENDIF ~7ZIP_LINKONREQUEST}

function Is7ZipLoaded: Boolean;
begin
  {$IFDEF 7ZIP_LINKONREQUEST}
  Result := SevenzipLib <> INVALID_MODULEHANDLE_VALUE;
  {$ELSE ~7ZIP_LINKONREQUEST}
  Result := True;
  {$ENDIF ~7ZIP_LINKONREQUEST}
end;

procedure Unload7Zip;
begin
  {$IFDEF 7ZIP_LINKONREQUEST}
  if SevenzipLib <> INVALID_MODULEHANDLE_VALUE then
    {$IFDEF MSWINDOWS}
    FreeLibrary(SevenzipLib);
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    dlclose(Pointer(SevenzipLib));
    {$ENDIF UNIX}
  SevenzipLib := INVALID_MODULEHANDLE_VALUE;
  {$ENDIF 7ZIP_LINKONREQUEST}
end;

end.
