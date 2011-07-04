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
{ The Original Code is JclNTFS.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel. Portions created by Marcel van  }
{ Brakel are Copyright (C) Marcel van Brakel. All Rights Reserved.                                 }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Petr Vones (pvones)                                                                            }
{   Oliver Schneider (assarbad)                                                                    }
{   ZENsan                                                                                         }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains routines to perform filesystem related tasks available only with NTFS. These are mostly }
{ relatively straightforward wrappers for various IOCTs related to compression, sparse files,      }
{ reparse points, volume mount points and so forth. Note that some functions require NTFS 5 or     }
{ higher!                                                                                          }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

// Comments on Win9x compatibility of the functions used in this unit

// These stubs exist on Windows 95B already but all of them
// return ERROR_CALL_NOT_IMPLEMENTED:
//   BackupSeek, BackupRead, BackupWrite

unit JclNTFS;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, SysUtils, Classes, ActiveX,
  JclBase, JclWin32;

// NTFS Exception
type
  EJclNtfsError = class(EJclWin32Error);

// NTFS - Compression
type
  TFileCompressionState = (fcNoCompression, fcDefaultCompression, fcLZNT1Compression);

function NtfsGetCompression(const FileName: string; var State: Short): Boolean; overload;
function NtfsGetCompression(const FileName: string): TFileCompressionState; overload;
function NtfsSetCompression(const FileName: string; const State: Short): Boolean;
procedure NtfsSetFileCompression(const FileName: string; const State: TFileCompressionState);
procedure NtfsSetDirectoryTreeCompression(const Directory: string; const State: TFileCompressionState);
procedure NtfsSetDefaultFileCompression(const Directory: string; const State: TFileCompressionState);
procedure NtfsSetPathCompression(const Path: string; const State: TFileCompressionState; Recursive: Boolean);

// NTFS - Sparse Files
type
  TNtfsAllocRanges = record
    Entries: Integer;
    Data: PFileAllocatedRangeBuffer;
    MoreData: Boolean;
  end;

function NtfsSetSparse(const FileName: string): Boolean;
function NtfsZeroDataByHandle(const Handle: THandle; const First, Last: Int64): Boolean;
function NtfsZeroDataByName(const FileName: string; const First, Last: Int64): Boolean;
function NtfsQueryAllocRanges(const FileName: string; Offset, Count: Int64; var Ranges: TNtfsAllocRanges): Boolean;
function NtfsGetAllocRangeEntry(const Ranges: TNtfsAllocRanges; Index: Integer): TFileAllocatedRangeBuffer;
function NtfsSparseStreamsSupported(const Volume: string): Boolean;
function NtfsGetSparse(const FileName: string): Boolean;

// NTFS - Reparse Points
function NtfsDeleteReparsePoint(const FileName: string; ReparseTag: DWORD): Boolean;
function NtfsSetReparsePoint(const FileName: string; var ReparseData; Size: Longword): Boolean;
function NtfsGetReparsePoint(const FileName: string; var ReparseData: TReparseGuidDataBuffer): Boolean;
function NtfsGetReparseTag(const Path: string; var Tag: DWORD): Boolean;
function NtfsReparsePointsSupported(const Volume: string): Boolean;
function NtfsFileHasReparsePoint(const Path: string): Boolean;

// NTFS - Volume Mount Points
function NtfsIsFolderMountPoint(const Path: string): Boolean;
function NtfsMountDeviceAsDrive(const Device: string; Drive: Char): Boolean;
function NtfsMountVolume(const Volume: Char; const MountPoint: string): Boolean;

// NTFS - Change Journal
// NTFS - Opportunistic Locks
type
  TOpLock = (olExclusive, olReadOnly, olBatch, olFilter);

function NtfsOpLockAckClosePending(Handle: THandle; Overlapped: TOverlapped): Boolean;
function NtfsOpLockBreakAckNo2(Handle: THandle; Overlapped: TOverlapped): Boolean;
function NtfsOpLockBreakAcknowledge(Handle: THandle; Overlapped: TOverlapped): Boolean;
function NtfsOpLockBreakNotify(Handle: THandle; Overlapped: TOverlapped): Boolean;
function NtfsRequestOpLock(Handle: THandle; Kind: TOpLock; Overlapped: TOverlapped): Boolean;

// Junction Points
function NtfsCreateJunctionPoint(const Source, Destination: string): Boolean;
function NtfsDeleteJunctionPoint(const Source: string): Boolean;
function NtfsGetJunctionPointDestination(const Source: string; var Destination: string): Boolean;

// Streams
type
  TStreamId = (siInvalid, siStandard, siExtendedAttribute, siSecurity, siAlternate,
    siHardLink, siProperty, siObjectIdentifier, siReparsePoints, siSparseFile);
  TStreamIds = set of TStreamId;

  TInternalFindStreamData = record
    FileHandle: THandle;
    Context: Pointer;
    StreamIds: TStreamIds;
  end;

  TFindStreamData = record
    Internal: TInternalFindStreamData;
    Attributes: DWORD;
    StreamID: TStreamId;
    Name: WideString;
    Size: Int64;
  end;

function NtfsFindFirstStream(const FileName: string; StreamIds: TStreamIds; var Data: TFindStreamData): Boolean;
function NtfsFindNextStream(var Data: TFindStreamData): Boolean;
function NtfsFindStreamClose(var Data: TFindStreamData): Boolean;

// Hard links
function NtfsCreateHardLink(const LinkFileName, ExistingFileName: String): Boolean;
// ANSI-specific version
function NtfsCreateHardLinkA(const LinkFileName, ExistingFileName: AnsiString): Boolean;
// UNICODE-specific version
function NtfsCreateHardLinkW(const LinkFileName, ExistingFileName: WideString): Boolean;

type
  TNtfsHardLinkInfo = record
    LinkCount: Cardinal;
    case Integer of
    0: (
      FileIndexHigh: Cardinal;
      FileIndexLow: Cardinal);
    1: (
      FileIndex: Int64);
  end;

function NtfsGetHardLinkInfo(const FileName: string; var Info: TNtfsHardLinkInfo): Boolean;

function NtfsFindHardLinks(const Path: string; const FileIndexHigh, FileIndexLow: Cardinal; const List: TStrings): Boolean;
function NtfsDeleteHardLinks(const FileName: string): Boolean;

// NTFS File summary
type
  EJclFileSummaryError = class(EJclError);

  TJclFileSummaryAccess = (fsaRead, fsaWrite, fsaReadWrite);
  TJclFileSummaryShare = (fssDenyNone, fssDenyRead, fssDenyWrite, fssDenyAll);
  TJclFileSummaryPropSetCallback = function(const FMTID: TGUID): Boolean of object;
  TJclFileSummaryPropCallback = function(const Name: WideString; ID: TPropID;
    Vt: TVarType): Boolean of object;

  TJclFileSummary = class;

  TJclFilePropertySet = class
  private
    FPropertyStorage: IPropertyStorage;
  public
    constructor Create(APropertyStorage: IPropertyStorage);
    destructor Destroy; override;

    class function GetFMTID: TGUID; virtual;
    function GetProperty(ID: TPropID): TPropVariant; overload;
    function GetProperty(const Name: WideString): TPropVariant; overload;
    procedure SetProperty(ID: TPropID; const Value: TPropVariant); overload;
    procedure SetProperty(const Name: WideString; const Value: TPropVariant;
      AllocationBase: TPropID = PID_FIRST_USABLE); overload;
    procedure DeleteProperty(ID: TPropID); overload;
    procedure DeleteProperty(const Name: WideString); overload;
    function EnumProperties(Proc: TJclFileSummaryPropCallback): Boolean;

    // casted properties
    // Type of ID changed to Integer to be compatible with indexed properties
    // VT_LPWSTR
    function GetWideStringProperty(const ID: Integer): WideString;
    procedure SetWideStringProperty(const ID: Integer; const Value: WideString);
    // VT_LPSTR
    function GetAnsiStringProperty(const ID: Integer): AnsiString;
    procedure SetAnsiStringProperty(const ID: Integer; const Value: AnsiString);
    // VT_I4
    function GetIntegerProperty(const ID: Integer): Integer;
    procedure SetIntegerProperty(const ID: Integer; const Value: Integer);
    // VT_UI4
    function GetCardinalProperty(const ID: Integer): Cardinal;
    procedure SetCardinalProperty(const ID: Integer; const Value: Cardinal);
    // VT_FILETIME
    function GetFileTimeProperty(const ID: Integer): TFileTime;
    procedure SetFileTimeProperty(const ID: Integer; const Value: TFileTime);
    // VT_CF
    function GetClipDataProperty(const ID: Integer): PClipData;
    procedure SetClipDataProperty(const ID: Integer; const Value: PClipData);
    // VT_BOOL
    function GetBooleanProperty(const ID: Integer): Boolean;
    procedure SetBooleanProperty(const ID: Integer; const Value: Boolean);
    // VT_VARIANT | VT_VECTOR
    function GetTCAPROPVARIANTProperty(const ID: Integer): TCAPROPVARIANT;
    procedure SetTCAPROPVARIANTProperty(const ID: Integer; const Value: TCAPROPVARIANT);
    // // VT_LPSTR | VT_VECTOR
    function GetTCALPSTRProperty(const ID: Integer): TCALPSTR;
    procedure SetTCALPSTRProperty(const ID: Integer; const Value: TCALPSTR);
    // VT_UI2
    function GetWordProperty(const ID: Integer): Word;
    procedure SetWordProperty(const ID: Integer; const Value: Word);
    // VT_BSTR
    function GetBSTRProperty(const ID: Integer): WideString;
    procedure SetBSTRProperty(const ID: Integer; const Value: WideString);

    // property names
    function GetPropertyName(ID: TPropID): WideString;
    procedure SetPropertyName(ID: TPropID; const Name: WideString);
    procedure DeletePropertyName(ID: TPropID);
  end;

  TJclFilePropertySetClass = class of TJclFilePropertySet;

  TJclFileSummary = class
  private
    FFileName: WideString;
    FAccessMode: TJclFileSummaryAccess;
    FShareMode: TJclFileSummaryShare;
    FStorage: IPropertySetStorage;
  public
    constructor Create(AFileName: WideString; AAccessMode: TJclFileSummaryAccess;
      AShareMode: TJclFileSummaryShare; AsDocument: Boolean = False;
      ACreate: Boolean = False);
    destructor Destroy; override;

    function CreatePropertySet(AClass: TJclFilePropertySetClass; ResetExisting: Boolean): TJclFilePropertySet;
    procedure GetPropertySet(AClass: TJclFilePropertySetClass; out Instance); overload;
    procedure GetPropertySet(const FMTID: TGUID; out Instance); overload;
    function GetPropertySet(const FMTID: TGUID): IPropertyStorage; overload;
    procedure DeletePropertySet(const FMTID: TGUID); overload;
    procedure DeletePropertySet(AClass: TJclFilePropertySetClass); overload;
    function EnumPropertySet(Proc: TJclFileSummaryPropSetCallback): Boolean;

    property FileName: WideString read FFileName;
    property AccessMode: TJclFileSummaryAccess read FAccessMode;
    property ShareMode: TJclFileSummaryShare read FShareMode;
  end;

  TJclFileSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;

    property Title: AnsiString index PIDSI_TITLE read GetAnsiStringProperty
      write SetAnsiStringProperty;
    property Subject: AnsiString index PIDSI_SUBJECT read GetAnsiStringProperty
      write SetAnsiStringProperty;
    property Author: AnsiString index PIDSI_AUTHOR read GetAnsiStringProperty
      write SetAnsiStringProperty;
    property KeyWords: AnsiString index PIDSI_KEYWORDS read GetAnsiStringProperty
      write SetAnsiStringProperty;
    property Comments: AnsiString index PIDSI_COMMENTS read GetAnsiStringProperty
      write SetAnsiStringProperty;
    property Template: AnsiString index PIDSI_TEMPLATE read GetAnsiStringProperty
      write SetAnsiStringProperty;
    property LastAuthor: AnsiString index PIDSI_LASTAUTHOR read GetAnsiStringProperty
      write SetAnsiStringProperty;
    property RevNumber: AnsiString index PIDSI_REVNUMBER read GetAnsiStringProperty
      write SetAnsiStringProperty;
    property EditTime: TFileTime index PIDSI_EDITTIME read GetFileTimeProperty
      write SetFileTimeProperty;
    property LastPrintedTime: TFileTime index PIDSI_LASTPRINTED read GetFileTimeProperty
      write SetFileTimeProperty;
    property CreationTime: TFileTime index PIDSI_CREATE_DTM read GetFileTimeProperty
      write SetFileTimeProperty;
    property LastSaveTime: TFileTime index PIDSI_LASTSAVE_DTM read GetFileTimeProperty
      write SetFileTimeProperty;
    property PageCount: Integer index PIDSI_PAGECOUNT read GetIntegerProperty
      write SetIntegerProperty;
    property WordCount: Integer index PIDSI_WORDCOUNT read GetIntegerProperty
      write SetIntegerProperty;
    property CharCount: Integer index PIDSI_CHARCOUNT read GetIntegerProperty
      write SetIntegerProperty;
    property Thumnail: PClipData index PIDSI_THUMBNAIL read GetClipDataProperty
      write SetClipDataProperty;
    property AppName: AnsiString index PIDSI_APPNAME read GetAnsiStringProperty
      write SetAnsiStringProperty;
    property Security: Integer index PIDSI_DOC_SECURITY read GetIntegerProperty
      write SetIntegerProperty;
  end;

  TJclDocSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;

    property Category: AnsiString index PIDDSI_CATEGORY read GetAnsiStringProperty
      write SetAnsiStringProperty;
    property PresFormat: AnsiString index PIDDSI_PRESFORMAT read GetAnsiStringProperty
      write SetAnsiStringProperty;
    property ByteCount: Integer index PIDDSI_BYTECOUNT read GetIntegerProperty
      write SetIntegerProperty;
    property LineCount: Integer index PIDDSI_LINECOUNT read GetIntegerProperty
      write SetIntegerProperty;
    property ParCount: Integer index PIDDSI_PARCOUNT read GetIntegerProperty
      write SetIntegerProperty;
    property SlideCount: Integer index PIDDSI_SLIDECOUNT read GetIntegerProperty
      write SetIntegerProperty;
    property NoteCount: Integer index PIDDSI_NOTECOUNT read GetIntegerProperty
      write SetIntegerProperty;
    property HiddenCount: Integer index PIDDSI_HIDDENCOUNT read GetIntegerProperty
      write SetIntegerProperty;
    property MMClipCount: Integer index PIDDSI_MMCLIPCOUNT read GetIntegerProperty
      write SetIntegerProperty;
    property Scale: Boolean index PIDDSI_SCALE read GetBooleanProperty
      write SetBooleanProperty;
    property HeadingPair: TCAPROPVARIANT index PIDDSI_HEADINGPAIR read GetTCAPROPVARIANTProperty
      write SetTCAPROPVARIANTProperty;
    property DocParts: TCALPSTR index PIDDSI_DOCPARTS read GetTCALPSTRProperty
      write SetTCALPSTRProperty;
    property Manager: AnsiString index PIDDSI_MANAGER read GetAnsiStringProperty
      write SetAnsiStringProperty;
    property Company: AnsiString index PIDDSI_COMPANY read GetAnsiStringProperty
      write SetAnsiStringProperty;
    property LinksDirty: Boolean index PIDDSI_LINKSDIRTY read GetBooleanProperty
      write SetBooleanProperty;
  end;

  TJclMediaFileSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;

    property Editor: WideString index PIDMSI_EDITOR read GetWideStringProperty
      write SetWideStringProperty;
    property Supplier: WideString index PIDMSI_SUPPLIER read GetWideStringProperty
      write SetWideStringProperty;
    property Source: WideString index PIDMSI_SOURCE read GetWideStringProperty
      write SetWideStringProperty;
    property SequenceNo: WideString index PIDMSI_SEQUENCE_NO read GetWideStringProperty
      write SetWideStringProperty;
    property Project: WideString index PIDMSI_PROJECT read GetWideStringProperty
      write SetWideStringProperty;
    property Status: Cardinal index PIDMSI_STATUS read GetCardinalProperty
      write SetCardinalProperty;
    property Owner: WideString index PIDMSI_OWNER read GetWideStringProperty
      write SetWideStringProperty;
    property Rating: WideString index PIDMSI_RATING read GetWideStringProperty
      write SetWideStringProperty;
    property Production: TFileTime index PIDMSI_PRODUCTION read GetFileTimeProperty
      write SetFileTimeProperty;
    property Copyright: WideString index PIDMSI_COPYRIGHT read GetWideStringProperty
      write SetWideStringProperty;
  end;

  TJclMSISummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;

    property Version: Integer index PID_MSIVERSION read GetIntegerProperty
      write SetIntegerProperty; // integer, Installer version number (major*100+minor)
    property Source: Integer index PID_MSISOURCE read GetIntegerProperty
      write SetIntegerProperty; // integer, type of file image, short/long, media/tree
    property Restrict: Integer index PID_MSIRESTRICT read GetIntegerProperty
      write SetIntegerProperty; // integer, transform restrictions
  end;

  TJclShellSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;

  {PID_FINDDATA        = 0;
  PID_NETRESOURCE     = 1;
  PID_DESCRIPTIONID   = 2;
  PID_WHICHFOLDER     = 3;
  PID_NETWORKLOCATION = 4;
  PID_COMPUTERNAME    = 5;}
  end;

  TJclStorageSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;
  end;

  TJclImageSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;
  end;

  TJclDisplacedSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;

  {PID_FINDDATA        = 0;
  PID_NETRESOURCE     = 1;
  PID_DESCRIPTIONID   = 2;
  PID_WHICHFOLDER     = 3;
  PID_NETWORKLOCATION = 4;
  PID_COMPUTERNAME    = 5;}
  end;

  TJclBriefCaseSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;

  {PID_SYNC_COPY_IN = 2;}
  end;

  TJclMiscSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;

  {PID_MISC_STATUS      = 2;
  PID_MISC_ACCESSCOUNT = 3;
  PID_MISC_OWNER       = 4;
  PID_HTMLINFOTIPFILE  = 5;
  PID_MISC_PICS        = 6;}
  end;

  TJclWebViewSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;

  {PID_DISPLAY_PROPERTIES = 0;
  PID_INTROTEXT          = 1;}
  end;

  TJclMusicSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;
  {PIDSI_ARTIST    = 2;
  PIDSI_SONGTITLE = 3;
  PIDSI_ALBUM     = 4;
  PIDSI_YEAR      = 5;
  PIDSI_COMMENT   = 6;
  PIDSI_TRACK     = 7;
  PIDSI_GENRE     = 11;
  PIDSI_LYRICS    = 12;}
  end;

  TJclDRMSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;
  {PIDDRSI_PROTECTED   = 2;
  PIDDRSI_DESCRIPTION = 3;
  PIDDRSI_PLAYCOUNT   = 4;
  PIDDRSI_PLAYSTARTS  = 5;
  PIDDRSI_PLAYEXPIRES = 6;}
  end;

  TJclVideoSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;

    property StreamName: WideString index PIDVSI_STREAM_NAME read GetWideStringProperty
      write SetWideStringProperty; // "StreamName", VT_LPWSTR
    property Width: Cardinal index PIDVSI_FRAME_WIDTH read GetCardinalProperty
      write SetCardinalProperty; // "FrameWidth", VT_UI4
    property Height: Cardinal index PIDVSI_FRAME_HEIGHT read GetCardinalProperty
      write SetCardinalProperty; // "FrameHeight", VT_UI4
    property TimeLength: Cardinal index PIDVSI_TIMELENGTH read GetCardinalProperty
      write SetCardinalProperty; // "TimeLength", VT_UI4, milliseconds
    property FrameCount: Cardinal index PIDVSI_FRAME_COUNT read GetCardinalProperty
      write SetCardinalProperty; // "FrameCount". VT_UI4
    property FrameRate: Cardinal index PIDVSI_FRAME_RATE read GetCardinalProperty
      write SetCardinalProperty; // "FrameRate", VT_UI4, frames/millisecond
    property DataRate: Cardinal index PIDVSI_DATA_RATE read GetCardinalProperty
      write SetCardinalProperty; // "DataRate", VT_UI4, bytes/second
    property SampleSize: Cardinal index PIDVSI_SAMPLE_SIZE read GetCardinalProperty
      write SetCardinalProperty; // "SampleSize", VT_UI4
    property Compression: WideString index PIDVSI_COMPRESSION read GetWideStringProperty
      write SetWideStringProperty; // "Compression", VT_LPWSTR
    property StreamNumber: Word index PIDVSI_STREAM_NUMBER read GetWordProperty
      write SetWordProperty; // "StreamNumber", VT_UI2}
  end;

  TJclAudioSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;

    property Format: WideString index PIDASI_FORMAT read GetBSTRProperty
      write SetBSTRProperty; // VT_BSTR
    property TimeLength: Cardinal index PIDASI_TIMELENGTH read GetCardinalProperty
      write SetCardinalProperty; // VT_UI4, milliseconds
    property AverageDataRate: Cardinal index PIDASI_AVG_DATA_RATE read GetCardinalProperty
      write SetCardinalProperty; // VT_UI4,  Hz
    property SampleRate: Cardinal index PIDASI_SAMPLE_RATE read GetCardinalProperty
      write SetCardinalProperty; // VT_UI4,  bits
    property SampleSize: Cardinal index PIDASI_SAMPLE_SIZE read GetCardinalProperty
      write SetCardinalProperty; // VT_UI4,  bits
    property ChannelCount: Cardinal index PIDASI_CHANNEL_COUNT read GetCardinalProperty
      write SetCardinalProperty; // VT_UI4
    property StreamNumber: Word index PIDASI_STREAM_NUMBER read GetWordProperty
      write SetWordProperty; // VT_UI2
    property StreamName: WideString index PIDASI_STREAM_NAME read GetWideStringProperty
      write SetWideStringProperty; // VT_LPWSTR
    property Compression: WideString index PIDASI_COMPRESSION read GetWideStringProperty
      write SetWideStringProperty; // VT_LPWSTR}
  end;

  TJclControlPanelSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;
  {PID_CONTROLPANEL_CATEGORY = 2;}
  end;

  TJclVolumeSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;
  {PID_VOLUME_FREE       = 2;
  PID_VOLUME_CAPACITY   = 3;
  PID_VOLUME_FILESYSTEM = 4;}
  end;

  TJclShareSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;
  {PID_SHARE_CSC_STATUS = 2;}
  end;

  TJclLinkSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;
  {PID_LINK_TARGET = 2;}
  end;

  TJclQuerySummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;
  {PID_QUERY_RANK = 2;}
  end;

  TJclImageInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;
  {FMTID_ImageInformation}
  end;

  TJclJpegSummaryInformation = class(TJclFilePropertySet)
  public
    class function GetFMTID: TGUID; override;
  {FMTID_JpegAppHeaders}
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclNTFS.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF FPC}
  WinSysUt,
  {$ENDIF FPC}
  ComObj, Hardlinks,
  JclFileUtils, JclSysInfo, JclResources, JclSecurity;

//=== NTFS - Compression =====================================================

// Helper consts, helper types, helper routines

const
  CompressionFormat: array [TFileCompressionState] of Short =
  (
    COMPRESSION_FORMAT_NONE,
    COMPRESSION_FORMAT_DEFAULT,
    COMPRESSION_FORMAT_LZNT1
  );

  // use IsDirectory(FileName) as array index
  FileFlag: array [Boolean] of DWORD = (0, FILE_FLAG_BACKUP_SEMANTICS);

type
  TStackFrame = packed record
    CallersEBP: DWord;
    CallerAddress: DWord;
  end;

  EJclInvalidArgument = class(EJclError);

{$STACKFRAMES OFF}

function CallersCallerAddress: Pointer;
asm
        MOV     EAX, [EBP]
        MOV     EAX, TStackFrame([EAX]).CallerAddress
end;

{$STACKFRAMES ON}

procedure ValidateArgument(Condition: Boolean; const Routine: string;
  const Argument: string);
begin
  if not Condition then
    raise EJclInvalidArgument.CreateResFmt(@RsInvalidArgument, [Routine, Argument])
      at CallersCallerAddress;
end;

{$IFNDEF STACKFRAMES_ON}
{$STACKFRAMES OFF}
{$ENDIF ~STACKFRAMES_ON}

function SetCompression(const FileName: string; const State: Short; FileFlag: DWORD): Boolean;
var
  Handle: THandle;
  BytesReturned: DWORD;
  Buffer: Short;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ, nil, OPEN_EXISTING, FileFlag, 0);
  if Handle <> INVALID_HANDLE_VALUE then
  try
    Buffer := State;
    Result := DeviceIoControl(Handle, FSCTL_SET_COMPRESSION, @Buffer,
      SizeOf(Short), nil, 0, BytesReturned, nil);
  finally
    CloseHandle(Handle);
  end
end;

function SetPathCompression(Dir: string; const Mask: string; const State: Short;
  const SetDefault, Recursive: Boolean): Boolean;
var
  FileName: string;
  SearchRec: TSearchRec;
  R: Integer;
begin
  if SetDefault then
    Result := SetCompression(Dir, State, FILE_FLAG_BACKUP_SEMANTICS)
  else
    Result := True;
  if Result then
  begin
    Dir := PathAddSeparator(Dir);
    if FindFirst(Dir + Mask, faAnyFile, SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          FileName := Dir + SearchRec.Name;
          if (SearchRec.Attr and faDirectory) = 0 then
            Result := SetCompression(FileName, State, 0)
          else
            if Recursive then
              Result := SetPathCompression(FileName, Mask, State, SetDefault, True);
          if not Result then
            Exit;
        end;
        R := FindNext(SearchRec);
      until R <> 0;
      Result := (R = ERROR_NO_MORE_FILES);
    finally
      SysUtils.FindClose(SearchRec);
    end;
  end;
end;

function NtfsGetCompression(const FileName: string; var State: Short): Boolean;
var
  Handle: THandle;
  BytesReturned: DWORD;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), 0, 0, nil, OPEN_EXISTING,
    FileFlag[IsDirectory(FileName)], 0);
  if Handle <> INVALID_HANDLE_VALUE then
    try
      Result := DeviceIoControl(Handle, FSCTL_GET_COMPRESSION, nil, 0, @State,
        SizeOf(Short), BytesReturned, nil);
    finally
      CloseHandle(Handle);
    end;
end;

function NtfsGetCompression(const FileName: string): TFileCompressionState;
var
  State: Short;
begin
  if not NtfsGetCompression(FileName, State) then
    RaiseLastOSError;
  case State of
    COMPRESSION_FORMAT_NONE:
      Result := fcNoCompression;
    COMPRESSION_FORMAT_LZNT1:
      Result := fcLZNT1Compression;
  else
    // (rom) very dubious.
    Assert(False, 'TFileCompressionState requires expansion');
    Result := TFileCompressionState(State);
  end;
end;

function NtfsSetCompression(const FileName: string; const State: Short): Boolean;
begin
  Result := SetCompression(FileName, State, FileFlag[IsDirectory(FileName)]);
end;

{$STACKFRAMES ON}

procedure NtfsSetFileCompression(const FileName: string; const State: TFileCompressionState);
begin
  ValidateArgument(not IsDirectory(FileName), 'NtfsSetFileCompression', 'FileName');
  if not SetCompression(FileName, CompressionFormat[State], 0) then
    RaiseLastOSError;
end;

procedure NtfsSetDefaultFileCompression(const Directory: string; const State: TFileCompressionState);
begin
  ValidateArgument(IsDirectory(Directory), 'NtfsSetDefaultFileCompression', 'Directory');
  if not SetCompression(Directory, CompressionFormat[State], FILE_FLAG_BACKUP_SEMANTICS) then
    RaiseLastOSError;
end;

procedure NtfsSetDirectoryTreeCompression(const Directory: string; const State: TFileCompressionState);
begin
  ValidateArgument(IsDirectory(Directory), 'NtfsSetDirectoryTreeCompression', 'Directory');
  if not SetPathCompression(Directory, '*', CompressionFormat[State], True, True) then
    RaiseLastOSError;
end;

{$IFNDEF STACKFRAMES_ON}
{$STACKFRAMES OFF}
{$ENDIF ~STACKFRAMES_ON}

procedure NtfsSetPathCompression(const Path: string;
  const State: TFileCompressionState; Recursive: Boolean);
var
  Dir, Mask: string;
  SetDefault: Boolean;
begin
  SetDefault := IsDirectory(Path);
  if SetDefault then
  begin
    Dir := Path;
    Mask := '*';
  end
  else
  begin
    Dir := ExtractFilePath(Path);
    Mask := ExtractFileName(Path);
    if Mask = '' then
      Mask := '*';
  end;
  if not SetPathCompression(Dir, Mask, CompressionFormat[State], SetDefault, Recursive) then
    RaiseLastOSError;
end;

//=== NTFS - Sparse Files ====================================================

function NtfsSetSparse(const FileName: string): Boolean;
var
  Handle: THandle;
  BytesReturned: DWORD;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, 0);
  if Handle <> INVALID_HANDLE_VALUE then
    try
      Result := DeviceIoControl(Handle, FSCTL_SET_SPARSE, nil, 0, nil, 0, BytesReturned, nil);
    finally
      CloseHandle(Handle);
    end;
end;

function NtfsZeroDataByHandle(const Handle: THandle; const First, Last: Int64): Boolean;
var
  BytesReturned: DWORD;
  ZeroDataInfo: TFileZeroDataInformation;
  Info: TByHandleFileInformation;
begin
  Result := False;
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    // Continue only if the file is a sparse file, this avoids the overhead
    // associated with an IOCTL when the file isn't even a sparse file.
    GetFileInformationByHandle(Handle, Info);
    Result := (Info.dwFileAttributes and FILE_ATTRIBUTE_SPARSE_FILE) <> 0;
    if Result then
    begin
      ZeroDataInfo.FileOffset.QuadPart := First;
      ZeroDataInfo.BeyondFinalZero.QuadPart := Last;
      Result := DeviceIoControl(Handle, FSCTL_SET_ZERO_DATA, @ZeroDataInfo,
        SizeOf(ZeroDataInfo), nil, 0, BytesReturned, nil);
    end;
  end;
end;

function NtfsZeroDataByName(const FileName: string; const First, Last: Int64): Boolean;
var
  Handle: THandle;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, 0);
  if Handle <> INVALID_HANDLE_VALUE then
    try
      Result := NtfsZeroDataByHandle(Handle, First, Last);
    finally
      CloseHandle(Handle);
    end;
end;

function NtfsGetAllocRangeEntry(const Ranges: TNtfsAllocRanges;
  Index: Integer): TFileAllocatedRangeBuffer;
var
  Offset: Longint;
begin
  Assert((Index >= 0) and (Index < Ranges.Entries));
  Offset := Longint(Ranges.Data) + Index * SizeOf(TFileAllocatedRangeBuffer);
  Result := PFileAllocatedRangeBuffer(Offset)^;
end;

function __QueryAllocRanges(const Handle: THandle; const Offset, Count: Int64;
  var Ranges: PFileAllocatedRangeBuffer; var MoreData: Boolean; var Size: Cardinal): Boolean;
var
  BytesReturned: DWORD;
  SearchRange: TFileAllocatedRangeBuffer;
  BufferSize: Cardinal;
begin
  SearchRange.FileOffset.QuadPart := Offset;
  SearchRange.Length.QuadPart := Count;
  BufferSize := 4 * 64 * SizeOf(TFileAllocatedRangeBuffer);
  Ranges := AllocMem(BufferSize);
  Result := DeviceIoControl(Handle, FSCTL_QUERY_ALLOCATED_RANGES, @SearchRange,
    SizeOf(SearchRange), Ranges, BufferSize, BytesReturned, nil);
  MoreData := GetLastError = ERROR_MORE_DATA;
  if MoreData then
    Result := True;
  Size := BytesReturned;
  if BytesReturned = 0 then
  begin
    FreeMem(Ranges);
    Ranges := nil;
  end;
end;

function NtfsQueryAllocRanges(const FileName: string; Offset, Count: Int64;
  var Ranges: TNtfsAllocRanges): Boolean;
var
  Handle: THandle;
  CurrRanges: PFileAllocatedRangeBuffer;
  R, MoreData: Boolean;
  Size: Cardinal;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  if Handle <> INVALID_HANDLE_VALUE then
  try
    R := __QueryAllocRanges(Handle, Offset, Count, CurrRanges, MoreData, Size);
    Ranges.MoreData := MoreData;
    Result := R;
    if R then
    begin
      Ranges.Entries := Size div SizeOf(TFileAllocatedRangeBuffer);
      Ranges.Data := CurrRanges;
    end
    else
    begin
      Ranges.Entries := 0;
      Ranges.Data := nil;
    end;
  finally
    CloseHandle(Handle);
  end;
end;

function NtfsSparseStreamsSupported(const Volume: string): Boolean;
begin
  Result := fsSupportsSparseFiles in GetVolumeFileSystemFlags(Volume);
end;

function NtfsGetSparse(const FileName: string): Boolean;
var
  Handle: THandle;
  Info: TByHandleFileInformation;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), 0, FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil, OPEN_EXISTING, 0, 0);
  if Handle <> INVALID_HANDLE_VALUE then
    try
      GetFileInformationByHandle(Handle, Info);
      Result := (Info.dwFileAttributes and FILE_ATTRIBUTE_SPARSE_FILE) <> 0;
    finally
      CloseHandle(Handle);
    end;
end;

//=== NTFS - Reparse Points ==================================================

function NtfsGetReparseTag(const Path: string; var Tag: DWORD): Boolean;
var
  SearchRec: TSearchRec;
begin
  Result := NtfsFileHasReparsePoint(Path);
  if Result then
  begin
    Result := FindFirst(Path, faAnyFile, SearchRec) = 0;
    if Result then
    begin
      // Check if file has a reparse point
      Result := ((SearchRec.Attr and FILE_ATTRIBUTE_REPARSE_POINT) <> 0);
      // If so the dwReserved0 field contains the reparse tag
      if Result then
        Tag := SearchRec.FindData.dwReserved0;
      FindClose(SearchRec);
    end;
  end;
end;

function NtfsReparsePointsSupported(const Volume: string): Boolean;
begin
  Result := fsSupportsReparsePoints in GetVolumeFileSystemFlags(Volume);
end;

function NtfsFileHasReparsePoint(const Path: string): Boolean;
var
  Attr: DWORD;
begin
  Result := False;
  Attr := GetFileAttributes(PChar(Path));
  if Attr <> DWORD(-1) then
    Result := (Attr and FILE_ATTRIBUTE_REPARSE_POINT) <> 0;
end;

function NtfsDeleteReparsePoint(const FileName: string; ReparseTag: DWORD): Boolean;
var
  Handle: THandle;
  BytesReturned: DWORD;
  ReparseData: TReparseGuidDataBuffer;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil,
    OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT, 0);
  if Handle <> INVALID_HANDLE_VALUE then
    try
      FillChar(ReparseData, SizeOf(ReparseData), #0);
      ReparseData.ReparseTag := ReparseTag;
      Result := DeviceIoControl(Handle, FSCTL_DELETE_REPARSE_POINT, @ReparseData,
        REPARSE_GUID_DATA_BUFFER_HEADER_SIZE, nil, 0, BytesReturned, nil);
    finally
      CloseHandle(Handle);
    end;
end;

function NtfsSetReparsePoint(const FileName: string; var ReparseData; Size: Longword): Boolean;
var
  Handle: THandle;
  BytesReturned: DWORD;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil,
    OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT, 0);
  if Handle <> INVALID_HANDLE_VALUE then
    try
      Result := DeviceIoControl(Handle, FSCTL_SET_REPARSE_POINT, @ReparseData,
        Size, nil, 0, BytesReturned, nil);
    finally
      CloseHandle(Handle);
    end;
end;

function NtfsGetReparsePoint(const FileName: string; var ReparseData: TReparseGuidDataBuffer): Boolean;
var
  Handle: THandle;
  BytesReturned: DWORD;
  LastError: DWORD;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT, 0);
  LastError := GetLastError;
  if Handle <> INVALID_HANDLE_VALUE then
    try
      Result := DeviceIoControl(Handle, FSCTL_GET_REPARSE_POINT, nil, 0, @ReparseData,
        ReparseData.ReparseDataLength + SizeOf(ReparseData), BytesReturned, nil);
      if not Result then
      begin
        ReparseData.ReparseDataLength := BytesReturned;
        LastError := GetLastError;
      end;
    finally
      CloseHandle(Handle);
      SetLastError(LastError);
    end;
end;

//=== NTFS - Volume Mount Points =============================================

function NtfsIsFolderMountPoint(const Path: string): Boolean;
var
  Tag: DWORD;
begin
  Result := NtfsGetReparseTag(Path, Tag);
  if Result then
    Result := (Tag = IO_REPARSE_TAG_MOUNT_POINT);
end;

function NtfsMountDeviceAsDrive(const Device: string; Drive: Char): Boolean;
const
  DDD_FLAGS = DDD_RAW_TARGET_PATH or DDD_REMOVE_DEFINITION or DDD_EXACT_MATCH_ON_REMOVE;
var
  DriveStr: string;
  VolumeName: string;
begin
  // To create a mount point we must obtain a unique volume name first. To obtain
  // a unique volume name the drive must exist. Therefore we must temporarily
  // create a symbolic link for the drive using DefineDosDevice.
  DriveStr := Drive + ':';
  Result := DefineDosDevice(DDD_RAW_TARGET_PATH, PChar(DriveStr), PChar(Device));
  if Result then
  begin
    SetLength(VolumeName, 1024);
    Result := RtdlGetVolumeNameForVolumeMountPoint(PChar(DriveStr + '\'),
      PChar(VolumeName), 1024);
    // Attempt to delete the symbolic link, if it fails then don't attempt to
    // set the mountpoint either but raise an exception instead, there's something
    // seriously wrong so let's try to control the damage done already :)
    if not DefineDosDevice(DDD_FLAGS, PChar(DriveStr), PChar(Device)) then
      raise EJclNtfsError.CreateRes(@RsNtfsUnableToDeleteSymbolicLink);
    if Result then
      Result := RtdlSetVolumeMountPoint(PChar(DriveStr + '\'), PChar(VolumeName));
  end;
end;

function NtfsMountVolume(const Volume: Char; const MountPoint: string): Boolean;
var
  VolumeName: string;
  VolumeStr: string;
begin
  SetLength(VolumeName, 1024);
  VolumeStr := Volume + ':\';
  Result := RtdlGetVolumeNameForVolumeMountPoint(PChar(VolumeStr), PChar(VolumeName), 1024);
  if Result then
  begin
    if not JclFileUtils.DirectoryExists(MountPoint) then
      JclFileUtils.ForceDirectories(MountPoint);
    Result := RtdlSetVolumeMountPoint(PChar(MountPoint), PChar(VolumeName));
  end;
end;

//=== NTFS - Change Journal ==================================================

//=== NTFS - Opportunistic Locks =============================================

function NtfsOpLockAckClosePending(Handle: THandle; Overlapped: TOverlapped): Boolean;
var
  BytesReturned: Cardinal;
begin
  Result := DeviceIoControl(Handle, FSCTL_OPBATCH_ACK_CLOSE_PENDING, nil, 0, nil,
    0, BytesReturned, @Overlapped);
end;

function NtfsOpLockBreakAckNo2(Handle: THandle; Overlapped: TOverlapped): Boolean;
var
  BytesReturned: Cardinal;
begin
  Result := DeviceIoControl(Handle, FSCTL_OPLOCK_BREAK_ACK_NO_2, nil, 0, nil, 0,
    BytesReturned, @Overlapped);
end;

function NtfsOpLockBreakAcknowledge(Handle: THandle; Overlapped: TOverlapped): Boolean;
var
  BytesReturned: Cardinal;
begin
  Result := DeviceIoControl(Handle, FSCTL_OPLOCK_BREAK_ACKNOWLEDGE, nil, 0, nil,
    0, BytesReturned, @Overlapped);
  Result := Result or (GetLastError = ERROR_IO_PENDING);
end;

function NtfsOpLockBreakNotify(Handle: THandle; Overlapped: TOverlapped): Boolean;
var
  BytesReturned: Cardinal;
begin
  Result := DeviceIoControl(Handle, FSCTL_OPLOCK_BREAK_NOTIFY, nil, 0, nil, 0,
    BytesReturned, @Overlapped);
end;

function NtfsRequestOpLock(Handle: THandle; Kind: TOpLock; Overlapped: TOverlapped): Boolean;
const
  IoCodes: array [TOpLock] of Cardinal = (
    FSCTL_REQUEST_OPLOCK_LEVEL_1, FSCTL_REQUEST_OPLOCK_LEVEL_2,
    FSCTL_REQUEST_BATCH_OPLOCK, FSCTL_REQUEST_FILTER_OPLOCK);
var
  BytesReturned: Cardinal;
begin
  Result := DeviceIoControl(Handle, IoCodes[Kind], nil, 0, nil, 0, BytesReturned, @Overlapped);
  Result := Result or (GetLastError = ERROR_IO_PENDING);
end;

//=== Junction Points ========================================================

type
  TReparseDataBufferOverlay = record
  case Boolean of
    False:
      (Reparse: TReparseDataBuffer;);
    True:
      (Buffer: array [0..MAXIMUM_REPARSE_DATA_BUFFER_SIZE] of Char;);
  end;
  
function IsReparseTagValid(Tag: DWORD): Boolean;
begin
  Result := (Tag and (not IO_REPARSE_TAG_VALID_VALUES) = 0) and
    (Tag > IO_REPARSE_TAG_RESERVED_RANGE);
end;

function NtfsCreateJunctionPoint(const Source, Destination: string): Boolean;
var
  Dest: array [0..1024] of Char; // Writable copy of Destination
  DestW: WideString;             // Unicode version of Dest
  FullDir: array [0..1024] of Char;
  FilePart: PChar;
  ReparseData: TReparseDataBufferOverlay;
  NameLength: Longword;
begin
  Result := False;
  // For some reason the destination string must be prefixed with \??\ otherwise
  // the IOCTL will fail, ensure it's there.
  if Copy(Destination, 1, 3) = '\??' then
    StrPCopy(Dest, Destination)
  else
  begin
    // Make sure Destination is a directory or again, the IOCTL will fail.
    if (GetFullPathName(PChar(Destination), 1024, FullDir, FilePart) = 0) or
      (GetFileAttributes(FullDir) = DWORD(-1)) then
    begin
      SetLastError(ERROR_PATH_NOT_FOUND);
      Exit;
    end;
    StrPCopy(Dest, '\??\' + Destination);
  end;
  FillChar(ReparseData, SizeOf(ReparseData), #0);
  NameLength := StrLen(Dest) * SizeOf(WideChar);
  ReparseData.Reparse.ReparseTag := IO_REPARSE_TAG_MOUNT_POINT;
  ReparseData.Reparse.ReparseDataLength := NameLength + 12;
  ReparseData.Reparse.SubstituteNameLength := NameLength;
  ReparseData.Reparse.PrintNameOffset := NameLength + 2;
  // Not the most elegant way to copy an AnsiString into an Unicode buffer but
  // let's avoid dependencies on JclUnicode.pas (adds significant resources).
  DestW := WideString(Dest);
  Move(DestW[1], ReparseData.Reparse.PathBuffer, Length(DestW) * SizeOf(WideChar));
  Result := NtfsSetReparsePoint(Source, ReparseData.Reparse,
    ReparseData.Reparse.ReparseDataLength + REPARSE_DATA_BUFFER_HEADER_SIZE);
end;

function NtfsDeleteJunctionPoint(const Source: string): Boolean;
begin
  Result := NtfsDeleteReparsePoint(Source, IO_REPARSE_TAG_MOUNT_POINT);
end;

function NtfsGetJunctionPointDestination(const Source: string; var Destination: string): Boolean;
var
  Handle: THandle;
  ReparseData: TReparseDataBufferOverlay;
  BytesReturned: DWORD;
begin
  Result := False;
  if NtfsFileHasReparsePoint(Source) then
  begin
    Handle := CreateFile(PChar(Source), GENERIC_READ, 0, nil,
      OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OPEN_REPARSE_POINT, 0);
    if Handle <> INVALID_HANDLE_VALUE then
    try
      if DeviceIoControl(Handle, FSCTL_GET_REPARSE_POINT, nil, 0, @ReparseData,
        MAXIMUM_REPARSE_DATA_BUFFER_SIZE, BytesReturned, nil) {and
        IsReparseTagValid(ReparseData.Reparse.ReparseTag) then}
        then
      begin
        if BytesReturned >= ReparseData.Reparse.SubstituteNameLength + SizeOf(WideChar) then
        begin
          SetLength(Destination, (ReparseData.Reparse.SubstituteNameLength div SizeOf(WideChar)) + 1);
          WideCharToMultiByte(CP_THREAD_ACP, 0, ReparseData.Reparse.PathBuffer,
            (ReparseData.Reparse.SubstituteNameLength div SizeOf(WCHAR)) + 1,
            PChar(Destination), Length(Destination), nil, nil);
          Result := True;
        end;
      end;
    finally
      CloseHandle(Handle);
    end
  end;
end;

//=== Streams ================================================================

// FindStream is an internal helper routine for NtfsFindFirstStream and
// NtfsFindNextStream. It uses the backup API to enumerate the streams in an
// NTFS file and returns when it either finds a stream that matches the filter
// specified in the Data parameter or hits EOF. Details are returned through
// the Data parameter and success/failure as the Boolean result value.

function FindStream(var Data: TFindStreamData): Boolean;
var
  Header: TWin32StreamId;
  BytesToRead, BytesRead: DWORD;
  BytesToSeek: TULargeInteger;
  Hi, Lo: DWORD;
  FoundStream: Boolean;
  StreamName: PWideChar;
begin
  Result := False;
  FoundStream := False;
  // We loop until we either found a stream or an error occurs.
  while not FoundStream do
  begin
    // Read stream header
    BytesToRead := DWORD(@Header.cStreamName[0]) - DWORD(@Header.dwStreamId);
    if not Windows.BackupRead(Data.Internal.FileHandle, (@Header), BytesToRead, BytesRead,
      False, True, Data.Internal.Context) then
    begin
      SetLastError(ERROR_READ_FAULT);
      Exit;
    end;
    if BytesRead = 0 then // EOF
    begin
      SetLastError(ERROR_NO_MORE_FILES);
      Exit;
    end;
    // If stream has a name then read it
    if Header.dwStreamNameSize > 0 then
    begin
      StreamName := HeapAlloc(GetProcessHeap, 0, Header.dwStreamNameSize + SizeOf(WCHAR));
      if StreamName = nil then
      begin
        SetLastError(ERROR_OUTOFMEMORY);
        Exit;
      end;
      if not Windows.BackupRead(Data.Internal.FileHandle, Pointer(StreamName),
        Header.dwStreamNameSize, BytesRead, False, True, Data.Internal.Context) then
      begin
        HeapFree(GetProcessHeap, 0, StreamName);
        SetLastError(ERROR_READ_FAULT);
        Exit;
      end;
      StreamName[Header.dwStreamNameSize div SizeOf(WCHAR)] := WideChar(#0);
    end
    else
      StreamName := nil;
    // Did we find any of the specified streams ([] means any stream)?
    if (Data.Internal.StreamIds = []) or
      (TStreamId(Header.dwStreamId) in Data.Internal.StreamIds) then
    begin
      FoundStream := True;
      {$IFDEF FPC}
      Data.Size := Header.Size.QuadPart;
      {$ELSE}
      Data.Size := Header.Size;
      {$ENDIF FPC}
      Data.Name := StreamName;
      Data.Attributes := Header.dwStreamAttributes;
      Data.StreamId := TStreamId(Header.dwStreamId);
    end;
    // Release stream name memory if necessary
    if Header.dwStreamNameSize > 0 then
      HeapFree(GetProcessHeap, 0, StreamName);
    // Move past data part to beginning of next stream (or EOF)
    {$IFDEF FPC}
    BytesToSeek.QuadPart := Header.Size.QuadPart;
    if (Header.Size.QuadPart <> 0) and (not JclWin32.BackupSeek(Data.Internal.FileHandle, BytesToSeek.LowPart,
         BytesToSeek.HighPart, Lo, Hi, Data.Internal.Context)) then
    {$ELSE}
    BytesToSeek.QuadPart := Header.Size;
    if (Header.Size <> 0) and (not JclWin32.BackupSeek(Data.Internal.FileHandle, BytesToSeek.LowPart,
      BytesToSeek.HighPart, Lo, Hi, Data.Internal.Context)) then
    {$ENDIF FPC}
    begin
      SetLastError(ERROR_READ_FAULT);
      Exit;
    end;
  end;
  // Due to the usage of Exit, we only get here if everything succeeded
  Result := True;
end;

function NtfsFindFirstStream(const FileName: string; StreamIds: TStreamIds;
  var Data: TFindStreamData): Boolean;
begin
  Result := False;
  // Open file for reading, note that the FILE_FLAG_BACKUP_SEMANTICS requires
  // the SE_BACKUP_NAME and SE_RESTORE_NAME privileges.
  Data.Internal.FileHandle := CreateFile(PChar(FileName), GENERIC_READ,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING,
    FILE_FLAG_BACKUP_SEMANTICS, 0);
  if Data.Internal.FileHandle <> INVALID_HANDLE_VALUE then
  begin
    // Initialize private context
    Data.Internal.StreamIds := StreamIds;
    Data.Internal.Context := nil;
    // Call upon the Borg worker to find the next (first) stream
    Result := FindStream(Data);
    if not Result then
    begin
      // Failure, cleanup relieving the caller of having to call FindStreamClose
      CloseHandle(Data.Internal.FileHandle);
      Data.Internal.FileHandle := INVALID_HANDLE_VALUE;
      Data.Internal.Context := nil;
      if GetLastError = ERROR_NO_MORE_FILES then
        SetLastError(ERROR_FILE_NOT_FOUND);
    end;
  end;
end;

function NtfsFindNextStream(var Data: TFindStreamData): Boolean;
begin
  Result := False;
  if Data.Internal.FileHandle <> INVALID_HANDLE_VALUE then
    Result := FindStream(Data)
  else
    SetLastError(ERROR_INVALID_HANDLE);
end;

function NtfsFindStreamClose(var Data: TFindStreamData): Boolean;
var
  BytesRead: DWORD;
  LastError: DWORD;
begin
  Result := Data.Internal.FileHandle <> INVALID_HANDLE_VALUE;
  LastError := ERROR_SUCCESS;
  if Result then
  begin
    // Call BackupRead one last time to signal that we're done with it
    Result := Windows.BackupRead(0, nil, 0, BytesRead, True, False, Data.Internal.Context);
    if not Result then
      LastError := GetLastError;
    CloseHandle(Data.Internal.FileHandle);
    Data.Internal.FileHandle := INVALID_HANDLE_VALUE;
    Data.Internal.Context := nil;
  end
  else
    LastError := ERROR_INVALID_HANDLE;
  SetLastError(LastError);
end;

//=== Hard links =============================================================
(*
   Implementation of CreateHardLink completely swapped to the unit Hardlink.pas

   As with all APIs on the NT platform this version is completely implemented in
   UNICODE and calling the ANSI version results in conversion of parameters and
   call of the underlying UNICODE version of the function.

   This holds both for the homegrown and the Windows API (where it exists).
*)

// For a description see: NtfsCreateHardLink()
(* ANSI implementation of the function - calling UNICODE anyway ;-) *)
function NtfsCreateHardLinkA(const LinkFileName, ExistingFileName: AnsiString): Boolean;
begin
  // Invoke either (homegrown vs. API) function and supply NIL for security attributes
  Result := CreateHardLinkA(PAnsiChar(LinkFileName), PAnsiChar(ExistingFileName), nil);
end;

// For a description see: NtfsCreateHardLink()
(* UNICODE implementation of the function - we are on NT, aren't we ;-) *)
function NtfsCreateHardLinkW(const LinkFileName, ExistingFileName: WideString): Boolean;
begin
  // Invoke either (homegrown vs. API) function and supply NIL for security attributes
  Result := CreateHardLinkW(PWideChar(LinkFileName), PWideChar(ExistingFileName), nil);
end;

// NtfsCreateHardLink
//
// Creates a hardlink on NT 4 and above.
// Both, LinkFileName and ExistingFileName must reside on the same, NTFS formatted volume.
//
// LinkName:          Name of the hard link to create
// ExistingFileName:  Fully qualified path of the file for which to create a hard link
// Result:            True if successfull,
//                    False if failed.
//                    In the latter case use GetLastError to obtain the reason of failure.
//
// Remark:
//   Hardlinks are the same as cross-referenced files were on DOS. With one exception
//   on NTFS they are allowed and are a feature of the filesystem, whereas on FAT
//   they were a feared kind of corruption of the filesystem.
//
//   Hardlinks are no more than references (with different names, but not necessarily
//   in different directories) of the filesystem to exactly the same data!
//
//   To test this you may create a hardlink to some file on your harddisk and then edit
//   it using Notepad (some editors do not work on the original file, but Notepad does).
//   The changes will appear in the "linked" and the "original" location.
//
//   Why did I use quotes? Easy: hardlinks are references to the same data - and such
//   as with handles the object (i.e. data) is only destroyed after all references are
//   "released". To "release" a reference (i.e. a hardlink) simply delete it using
//   the well-known methods to delete files. Because:
//
//   Files are hardlinks and hardlinks are files.
//
//   The above holds for NTFS volumes (and those filesystems supporting hardlinks).
//   Why all references need to reside on the same volume should be clear from these
//   remarks.
function NtfsCreateHardLink(const LinkFileName, ExistingFileName: String): Boolean;
{$DEFINE ANSI} // TODO: review for possible existing compatible DEFINES in the JCL
begin
  {$IFDEF ANSI}
  Result := CreateHardLinkA(PAnsiChar(LinkFileName), PAnsiChar(ExistingFileName), nil);
  {$ELSE}
  Result := CreateHardLinkW(PWideChar(LinkFileName), PWideChar(ExistingFileName));
  {$ENDIF ANSI}
end;

function NtfsGetHardLinkInfo(const FileName: string; var Info: TNtfsHardLinkInfo): Boolean;
var
  F: THandle;
  FileInfo: TByHandleFileInformation;
begin
  Result := False;
  F := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if F <> INVALID_HANDLE_VALUE then
  try
    if GetFileInformationByHandle(F, FileInfo) then
    begin
      Info.LinkCount := FileInfo.nNumberOfLinks;
      Info.FileIndexHigh := FileInfo.nFileIndexHigh;
      Info.FileIndexLow := FileInfo.nFileIndexLow;
      Result := True;
    end;
  finally
    CloseHandle(F);
  end
end;

function NtfsFindHardLinks(const Path: string; const FileIndexHigh, FileIndexLow: Cardinal; const List: TStrings): Boolean;
var
  SearchRec: TSearchRec;
  R: Integer;
  Info: TNtfsHardLinkInfo;
begin
  // start the search
  R := FindFirst(Path + '\*.*', faAnyFile, SearchRec);
  Result := (R = 0);
  if Result then
  begin
    List.BeginUpdate;
    try
      while R = 0 do
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          if (SearchRec.Attr and faDirectory) = faDirectory then
          begin
            // recurse into subdirectory
            Result := NtfsFindHardLinks(Path + '\' + SearchRec.Name, FileIndexHigh, FileIndexLow, List);
            if not Result then
              Break;
          end
          else
          begin
            // found a file, is it a hard link?
            if NtfsGetHardLinkInfo(Path + '\' + SearchRec.Name, Info) then
            begin
              if (Info.FileIndexHigh = FileIndexHigh) and (Info.FileIndexLow = FileIndexLow) then
                List.Add(Path + '\' + SearchRec.Name);
            end;
          end;
        end;
        R := FindNext(SearchRec);
      end;
      Result := R = ERROR_NO_MORE_FILES;
    finally
      SysUtils.FindClose(SearchRec);
      List.EndUpdate;
    end;
  end;
  if R = ERROR_ACCESS_DENIED then
    Result := True;
end;

function NtfsDeleteHardLinks(const FileName: string): Boolean;
var
  FullPathName: string;
  FilePart: PChar;
  Files: TStringList;
  I: Integer;
  Info: TNtfsHardLinkInfo;
begin
  Result := False;
  // get the full pathname of the specified file
  SetLength(FullPathName, MAX_PATH);
  GetFullPathName(PChar(FileName), MAX_PATH, PChar(FullPathName), FilePart);
  SetLength(FullPathName, StrLen(PChar(FullPathName)));
  // get hard link information
  if NtfsGetHardLinkInfo(FullPathName, Info) then
  begin
    Files := TStringList.Create;
    try
      if Info.LinkCount > 1 then
      begin
        // find all hard links for this file
        if not NtfsFindHardLinks(FullPathName[1] + ':', Info.FileIndexHigh, Info.FileIndexLow, Files) then
          Exit;
        // first delete the originally specified file from the list, we don't delete that one until all hard links
        // are succesfully deleted so we can use it to restore them if anything goes wrong. Theoretically one could
        // use any of the hard links but in case the restore goes wrong, at least the specified file still exists...
        for I := 0 to Files.Count - 1 do
        begin
          if CompareStr(FullPathName, Files[I]) = 0 then
          begin
            Files.Delete(I);
            Break;
          end;
        end;
        // delete all found hard links
        I := 0;
        while I < Files.Count do
        begin
          if not DeleteFile(Files[I]) then
            Break;
          Inc(I);
        end;
        if I = Files.Count then
        begin
          // all hard links succesfully deleted, now delete the originally specified file. if this fails we set
          // I to Files.Count - 1 so that the next code block will restore all hard links we just deleted.
          Result := DeleteFile(FullPathName);
          if not Result then
            I := Files.Count - 1;
        end;
        if I < Files.Count then
        begin
          // not all hard links could be deleted, attempt to restore the ones that were
          while I >= 0 do
          begin
            // ignore result, just attempt to restore...
            NtfsCreateHardLink(Files[I], FullPathName);
            Dec(I);
          end;
        end;
      end
      else
        // there are no hard links, just delete the file
        Result := DeleteFile(FullPathName);
    finally
      Files.Free;
    end;
  end;
end;

//=== { TJclFileSummary } ====================================================

const
  AccessModes: array [TJclFileSummaryAccess] of DWORD =
    ( STGM_READ, STGM_WRITE, STGM_READWRITE );
  ShareModes: array [TJclFileSummaryShare] of DWORD =
    ( STGM_SHARE_DENY_NONE, STGM_SHARE_DENY_READ, STGM_SHARE_DENY_WRITE,
      STGM_SHARE_EXCLUSIVE );
      
constructor TJclFileSummary.Create(AFileName: WideString; AAccessMode: TJclFileSummaryAccess;
  AShareMode: TJclFileSummaryShare; AsDocument: Boolean; ACreate: Boolean);
var
  Format: DWORD;
  IntfGUID: TGUID;
  AIntf: IInterface;
begin
  inherited Create;
  FAccessMode := AAccessMode;
  FShareMode := AShareMode;
  FFileName := AFileName;

  if AsDocument then
    Format := STGFMT_DOCFILE
  else
  if ACreate then
    Format := STGFMT_FILE
  else
    Format := STGFMT_ANY;
  IntfGUID := IPropertySetStorage;

  if ACreate then
    OleCheck(StgCreateStorageEx(PWideChar(AFileName),
      STGM_DIRECT or AccessModes[AAccessMode] or ShareModes[AShareMode], Format, 0,
      nil, nil, @IntfGUID, AIntf))
  else
    OleCheck(StgOpenStorageEx(PWideChar(AFileName),
      STGM_DIRECT or AccessModes[AAccessMode] or ShareModes[AShareMode], Format, 0,
      nil, nil, @IntfGUID, AIntf));

  FStorage := AIntf as IPropertySetStorage;
end;

function TJclFileSummary.CreatePropertySet(AClass: TJclFilePropertySetClass;
  ResetExisting: Boolean): TJclFilePropertySet;
var
  PropertyStorage: IPropertyStorage;
begin
  OleCheck(FStorage.Create(AClass.GetFMTID, AClass.GetFMTID, PROPSETFLAG_DEFAULT,
    STGM_CREATE or STGM_DIRECT or AccessModes[AccessMode] or ShareModes[ShareMode],
    PropertyStorage));
  if Assigned(PropertyStorage) then
    Result := AClass.Create(PropertyStorage)
  else
    raise EJclFileSummaryError.CreateRes(@RsEUnableToCreatePropertyStorage);
end;

procedure TJclFileSummary.DeletePropertySet(AClass: TJclFilePropertySetClass);
begin
  DeletePropertySet(AClass.GetFMTID);
end;

procedure TJclFileSummary.DeletePropertySet(const FMTID: TGUID);
begin
  OleCheck(FStorage.Delete(FMTID));
end;

destructor TJclFileSummary.Destroy;
begin
  FStorage := nil;
  inherited Destroy;
end;

function TJclFileSummary.EnumPropertySet(
  Proc: TJclFileSummaryPropSetCallback): Boolean;
var
  Enum: IEnumSTATPROPSETSTG;
  PropSet: STATPROPSETSTG;
  Returned: ULONG;
  Status: HRESULT;
begin
  OleCheck(FStorage.Enum(Enum));
  ZeroMemory(@PropSet, SizeOf(PropSet));

  OleCheck(Enum.Reset);
  Status := Enum.Next(1, PropSet, @Returned);
  Result := True;

  while Result and (Status = S_OK) and (Returned = 1) do
  begin
    Result := Proc(PropSet.fmtid);
    if Result then
      Status := Enum.Next(1, PropSet, @Returned);
  end;
end;

procedure TJclFileSummary.GetPropertySet(AClass: TJclFilePropertySetClass;
  out Instance);
var
  PropertyStorage: IPropertyStorage;
begin
  TJclFilePropertySet(Instance) := nil;
  PropertyStorage := GetPropertySet(AClass.GetFMTID);
  if Assigned(PropertyStorage) then
    TJclFilePropertySet(Instance) := AClass.Create(PropertyStorage);
end;

procedure TJclFileSummary.GetPropertySet(const FMTID: TGUID; out Instance);
var
  PropertyStorage: IPropertyStorage;
begin
  TJclFilePropertySet(Instance) := nil;
  PropertyStorage := GetPropertySet(FMTID);
  if Assigned(PropertyStorage) then
    TJclFilePropertySet(Instance) := TJclFilePropertySet.Create(PropertyStorage);
end;

function TJclFileSummary.GetPropertySet(const FMTID: TGUID): IPropertyStorage;
var
  Status: HRESULT;
begin
  Status := FStorage.Open(FMTID,
    STGM_DIRECT or AccessModes[AccessMode] or ShareModes[ShareMode],
    Result);
  if (Status = STG_E_FILENOTFOUND) then
  begin
    if AccessMode = fsaRead then
      Result := nil
    else
      OleCheck(FStorage.Create(FMTID, FMTID, PROPSETFLAG_DEFAULT,
        STGM_CREATE or STGM_DIRECT or AccessModes[AccessMode] or ShareModes[ShareMode],
        Result))
  end
  else
    OleCheck(Status);
end;

//=== { TJclFilePropertySet } ================================================

constructor TJclFilePropertySet.Create(APropertyStorage: IPropertyStorage);
begin
  inherited Create;
  FPropertyStorage := APropertyStorage;
end;

procedure TJclFilePropertySet.DeleteProperty(const Name: WideString);
var
  Prop: TPropSpec;
begin
  Prop.ulKind := PRSPEC_LPWSTR;
  Prop.lpwstr := PWideChar(Name);
  OleCheck(FPropertyStorage.DeleteMultiple(1, @Prop));
end;

procedure TJclFilePropertySet.DeletePropertyName(ID: TPropID);
begin
  OleCheck(FPropertyStorage.DeletePropertyNames(1, @ID));
end;

procedure TJclFilePropertySet.DeleteProperty(ID: TPropID);
var
  Prop: TPropSpec;
begin
  Prop.ulKind := PRSPEC_PROPID;
  Prop.propid := ID;
  OleCheck(FPropertyStorage.DeleteMultiple(1, @Prop));
end;

destructor TJclFilePropertySet.Destroy;
begin
  FPropertyStorage := nil;
  inherited Destroy;
end;

function TJclFilePropertySet.EnumProperties(
  Proc: TJclFileSummaryPropCallback): Boolean;
var
  Enum: IEnumSTATPROPSTG;
  Status: HRESULT;
  Returned: ULONG;
  Prop: STATPROPSTG;
begin
  OleCheck(FPropertyStorage.Enum(Enum));

  ZeroMemory(@Prop, SizeOf(Prop));
  OleCheck(Enum.Reset);
  Status := Enum.Next(1, Prop, @Returned);
  Result := True;

  while Result and (Status = S_OK) and (Returned = 1) do
  begin
    try
      Result := Proc(Prop.lpwstrName, Prop.propid, Prop.vt);
    finally
      if Assigned(Prop.lpwstrName) then
        CoTaskMemFree(Prop.lpwstrName);
    end;

    if Result then
      Status := Enum.Next(1, Prop, @Returned);
  end;
end;

function TJclFilePropertySet.GetAnsiStringProperty(
  const ID: Integer): AnsiString;
var
  PropValue: TPropVariant;
begin
  PropValue := GetProperty(ID);
  case PropValue.vt of
    VT_EMPTY, VT_NULL:
      Result := '';
    VT_LPSTR:
      Result := PropValue.pszVal;
    VT_LPWSTR:
      Result := PropValue.pwszVal;
    VT_BSTR:
      Result := PropValue.bstrVal;
  else
    raise EJclFileSummaryError.CreateRes(@RsEIncomatibleDataFormat);
  end;
end;

function TJclFilePropertySet.GetBooleanProperty(const ID: Integer): Boolean;
var
  PropValue: TPropVariant;
begin
  PropValue := GetProperty(ID);
  case PropValue.vt of
    VT_EMPTY, VT_NULL:
      Result := False;
    VT_BOOL:
      Result := PropValue.bool;
  else
    raise EJclFileSummaryError.CreateRes(@RsEIncomatibleDataFormat);
  end;
end;

function TJclFilePropertySet.GetBSTRProperty(const ID: Integer): WideString;
var
  PropValue: TPropVariant;
begin
  PropValue := GetProperty(ID);
  case PropValue.vt of
    VT_EMPTY, VT_NULL:
      Result := '';
    VT_LPSTR:
      Result := PropValue.pszVal;
    VT_LPWSTR:
      Result := PropValue.pwszVal;
    VT_BSTR:
      Result := PropValue.bstrVal;
  else
    raise EJclFileSummaryError.CreateRes(@RsEIncomatibleDataFormat);
  end;
end;

function TJclFilePropertySet.GetCardinalProperty(const ID: Integer): Cardinal;
var
  PropValue: TPropVariant;
begin
  PropValue := GetProperty(ID);
  case PropValue.vt of
    VT_EMPTY, VT_NULL:
      Result := 0;
    VT_I2:
      Result := PropValue.iVal;
    VT_I4, VT_INT:
      Result := PropValue.lVal;
    VT_I1:
      Result := PropValue.bVal; // no ShortInt? (cVal)
    VT_UI1:
      Result := PropValue.bVal;
    VT_UI2:
      Result := PropValue.uiVal;
    VT_UI4, VT_UINT:
      Result := PropValue.ulVal;
  else
    raise EJclFileSummaryError.CreateRes(@RsEIncomatibleDataFormat);
  end;
end;

function TJclFilePropertySet.GetClipDataProperty(const ID: Integer): PClipData;
var
  PropValue: TPropVariant;
begin
  PropValue := GetProperty(ID);
  case PropValue.vt of
    VT_EMPTY, VT_NULL:
      Result := nil;
    VT_CF:
      Result := PropValue.pclipdata
  else
    raise EJclFileSummaryError.CreateRes(@RsEIncomatibleDataFormat);
  end;
end;

function TJclFilePropertySet.GetFileTimeProperty(const ID: Integer): TFileTime;
var
  PropValue: TPropVariant;
begin
  PropValue := GetProperty(ID);
  case PropValue.vt of
    VT_EMPTY, VT_NULL:
      ZeroMemory(@Result, SizeOf(Result));
    VT_FILETIME:
      Result := PropValue.filetime;
  else
    raise EJclFileSummaryError.CreateRes(@RsEIncomatibleDataFormat);
  end;
end;

class function TJclFilePropertySet.GetFMTID: TGUID;
begin
  Result := GUID_NULL;
end;

function TJclFilePropertySet.GetIntegerProperty(const ID: Integer): Integer;
var
  PropValue: TPropVariant;
begin
  PropValue := GetProperty(ID);
  case PropValue.vt of
    VT_EMPTY, VT_NULL:
      Result := 0;
    VT_I2:
      Result := PropValue.iVal;
    VT_I4, VT_INT:
      Result := PropValue.lVal;
    VT_I1:
      Result := PropValue.bVal; // no ShortInt? (cVal)
    VT_UI1:
      Result := PropValue.bVal;
    VT_UI2:
      Result := PropValue.uiVal;
    VT_UI4, VT_UINT:
      Result := PropValue.ulVal;
  else
    raise EJclFileSummaryError.CreateRes(@RsEIncomatibleDataFormat);
  end;
end;

function TJclFilePropertySet.GetProperty(const Name: WideString): TPropVariant;
var
  Prop: TPropSpec;
begin
  Prop.ulKind := PRSPEC_LPWSTR;
  Prop.lpwstr := PWideChar(Name);

  OleCheck(FPropertyStorage.ReadMultiple(1, @Prop, @Result));
end;

function TJclFilePropertySet.GetProperty(ID: TPropID): TPropVariant;
var
  Prop: TPropSpec;
begin
  Prop.ulKind := PRSPEC_PROPID;
  Prop.propid := ID;

  OleCheck(FPropertyStorage.ReadMultiple(1, @Prop, @Result));
end;

function TJclFilePropertySet.GetPropertyName(ID: TPropID): WideString;
var
  AName: PWideChar;
begin
  AName := nil;
  try
    OleCheck(FPropertyStorage.ReadPropertyNames(1, @ID, @AName));
    Result := AName;
  finally
    if Assigned(AName) then
      CoTaskMemFree(AName);
  end;
end;

function TJclFilePropertySet.GetTCALPSTRProperty(const ID: Integer): TCALPSTR;
var
  PropValue: TPropVariant;
begin
  PropValue := GetProperty(ID);
  case PropValue.vt of
    VT_EMPTY, VT_NULL:
      ZeroMemory(@Result, SizeOf(Result));
    VT_LPSTR or VT_VECTOR:
      Result := PropValue.calpstr;
  else
    raise EJclFileSummaryError.CreateRes(@RsEIncomatibleDataFormat);
  end;
end;

function TJclFilePropertySet.GetTCAPROPVARIANTProperty(
  const ID: Integer): TCAPROPVARIANT;
var
  PropValue: TPropVariant;
begin
  PropValue := GetProperty(ID);
  case PropValue.vt of
    VT_EMPTY, VT_NULL:
      ZeroMemory(@Result, SizeOf(Result));
    VT_VARIANT or VT_VECTOR:
      Result := PropValue.capropvar;
  else
    raise EJclFileSummaryError.CreateRes(@RsEIncomatibleDataFormat);
  end;
end;

function TJclFilePropertySet.GetWideStringProperty(
  const ID: Integer): WideString;
var
  PropValue: TPropVariant;
begin
  PropValue := GetProperty(ID);
  case PropValue.vt of
    VT_EMPTY, VT_NULL:
      Result := '';
    VT_LPSTR:
      Result := PropValue.pszVal;
    VT_LPWSTR:
      Result := PropValue.pwszVal;
    VT_BSTR:
      Result := PropValue.bstrVal;
  else
    raise EJclFileSummaryError.CreateRes(@RsEIncomatibleDataFormat);
  end;
end;

function TJclFilePropertySet.GetWordProperty(const ID: Integer): Word;
var
  PropValue: TPropVariant;
begin
  PropValue := GetProperty(ID);
  case PropValue.vt of
    VT_EMPTY, VT_NULL:
      Result := 0;
    VT_I2:
      Result := PropValue.iVal;
    VT_I1:
      Result := PropValue.bVal; // no ShortInt? (cVal)
    VT_UI1:
      Result := PropValue.bVal;
    VT_UI2:
      Result := PropValue.uiVal;
  else
    raise EJclFileSummaryError.CreateRes(@RsEIncomatibleDataFormat);
  end;
end;

procedure TJclFilePropertySet.SetAnsiStringProperty(const ID: Integer;
  const Value: AnsiString);
var
  PropValue: TPropVariant;
begin
  PropValue.vt := VT_LPSTR;
  PropValue.pszVal := PAnsiChar(Value);
  SetProperty(ID, PropValue);
end;

procedure TJclFilePropertySet.SetBooleanProperty(const ID: Integer;
  const Value: Boolean);
var
  PropValue: TPropVariant;
begin
  PropValue.vt := VT_BOOL;
  PropValue.bool := Value;
  SetProperty(ID, PropValue);
end;

procedure TJclFilePropertySet.SetBSTRProperty(const ID: Integer;
  const Value: WideString);
var
  PropValue: TPropVariant;
begin
  PropValue.vt := VT_BSTR;
  PropValue.bstrVal := PWideChar(Value);
  SetProperty(ID, PropValue);
end;

procedure TJclFilePropertySet.SetCardinalProperty(const ID: Integer;
  const Value: Cardinal);
var
  PropValue: TPropVariant;
begin
  PropValue.vt := VT_UI4;
  PropValue.ulVal := Value;
  SetProperty(ID, PropValue);
end;

procedure TJclFilePropertySet.SetClipDataProperty(const ID: Integer;
  const Value: PClipData);
var
  PropValue: TPropVariant;
begin
  PropValue.vt := VT_CF;
  PropValue.pclipdata := Value;
  SetProperty(ID, PropValue);
end;

procedure TJclFilePropertySet.SetFileTimeProperty(const ID: Integer;
  const Value: TFileTime);
var
  PropValue: TPropVariant;
begin
  PropValue.vt := VT_FILETIME;
  PropValue.filetime := Value;
  SetProperty(ID, PropValue);
end;

procedure TJclFilePropertySet.SetIntegerProperty(const ID, Value: Integer);
var
  PropValue: TPropVariant;
begin
  PropValue.vt := VT_I4;
  PropValue.lVal := Value;
  SetProperty(ID, PropValue);
end;

procedure TJclFilePropertySet.SetProperty(const Name: WideString;
  const Value: TPropVariant; AllocationBase: TPropID);
var
  Prop: TPropSpec;
begin
  Prop.ulKind := PRSPEC_LPWSTR;
  Prop.lpwstr := PWideChar(Name);

  OleCheck(FPropertyStorage.WriteMultiple(1, @Prop, @Value, AllocationBase));
end;

procedure TJclFilePropertySet.SetPropertyName(ID: TPropID;
  const Name: WideString);
var
  AName: PWideChar;
begin
  OleCheck(FPropertyStorage.WritePropertyNames(1, @ID, @AName));
end;

procedure TJclFilePropertySet.SetTCALPSTRProperty(const ID: Integer;
  const Value: TCALPSTR);
var
  PropValue: TPropVariant;
begin
  PropValue.vt := VT_LPSTR or VT_VECTOR;
  PropValue.calpstr := Value;
  SetProperty(ID, PropValue);
end;

procedure TJclFilePropertySet.SetTCAPROPVARIANTProperty(const ID: Integer;
  const Value: TCAPROPVARIANT);
var
  PropValue: TPropVariant;
begin
  PropValue.vt := VT_VARIANT or VT_VECTOR;
  PropValue.capropvar := Value;
  SetProperty(ID, PropValue);
end;

procedure TJclFilePropertySet.SetWideStringProperty(const ID: Integer;
  const Value: WideString);
var
  PropValue: TPropVariant;
begin
  PropValue.vt := VT_LPWSTR;
  PropValue.pwszVal := PWideChar(Value);
  SetProperty(ID, PropValue);
end;

procedure TJclFilePropertySet.SetWordProperty(const ID: Integer;
  const Value: Word);
var
  PropValue: TPropVariant;
begin
  PropValue.vt := VT_UI2;
  PropValue.uiVal := Value;
  SetProperty(ID, PropValue);
end;

procedure TJclFilePropertySet.SetProperty(ID: TPropID; const Value: TPropVariant);
var
  Prop: TPropSpec;
begin
  Prop.ulKind := PRSPEC_PROPID;
  Prop.propid := ID;

  OleCheck(FPropertyStorage.WriteMultiple(1, @Prop, @Value, PID_FIRST_USABLE));
end;

//=== { TJclFileSummaryInformation } =========================================

class function TJclFileSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_SummaryInformation;
end;

//=== { TJclDocSummaryInformation } ==========================================

class function TJclDocSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_DocumentSummaryInformation;
end;

//=== { TJclMediaSummaryInformation } ========================================

class function TJclMediaFileSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_MediaFileSummaryInformation
end;

//=== { TJclMSISummaryInformation } ==========================================

class function TJclMSISummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_SummaryInformation;
end;

//=== { TJclShellSummaryInformation } ========================================

class function TJclShellSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_ShellDetails;
end;

//=== { TJclStorageSummaryInformation } ======================================

class function TJclStorageSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_Storage;
end;

//=== { TJclImageSummaryInformation } ========================================

class function TJclImageSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_ImageSummaryInformation;
end;

//=== { TJclDisplacedSummaryInformation } ====================================

class function TJclDisplacedSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_Displaced;
end;

//=== { TJclBriefCaseSummaryInformation }

class function TJclBriefCaseSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_Briefcase;
end;

//=== { TJclMiscSummaryInformation } =========================================

class function TJclMiscSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_Misc;
end;

//=== { TJclWebViewSummaryInformation } ======================================

class function TJclWebViewSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_WebView;
end;

//=== { TJclMusicSummaryInformation } ========================================

class function TJclMusicSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_MUSIC;
end;

//=== { TJclDRMSummaryInformation } ==========================================

class function TJclDRMSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_DRM;
end;

//=== { TJclVideoSummaryInformation } ========================================

class function TJclVideoSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_Video;
end;

//=== { TJclAudioSummaryInformation } ========================================

class function TJclAudioSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_Audio;
end;

//=== { TJclControlPanelSummaryInformation } =================================

class function TJclControlPanelSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_ControlPanel;
end;

//=== { TJclVolumeSummaryInformation } =======================================

class function TJclVolumeSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_Volume; 
end;

//=== { TJclShareSummaryInformation } ========================================

class function TJclShareSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_Share;
end;

//=== { TJclLinkSummaryInformation } =========================================

class function TJclLinkSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_Link;
end;

//=== { TJclQuerySummaryInformation } ========================================

class function TJclQuerySummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_Query;
end;

//=== { TJclImageInformation } ===============================================

class function TJclImageInformation.GetFMTID: TGUID;
begin
  Result := FMTID_ImageInformation;
end;

//=== { TJclJpegSummaryInformation } =========================================

class function TJclJpegSummaryInformation.GetFMTID: TGUID;
begin
  Result := FMTID_JpegAppHeaders;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
