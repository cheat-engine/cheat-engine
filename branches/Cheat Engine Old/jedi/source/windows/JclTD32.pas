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
{ The Original Code is JclTD32.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Flier Lu (<flier_lu att yahoo dott com dott cn>).  }
{ Portions created by Flier Lu are Copyright (C) Flier Lu.  All Rights Reserved.                   }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Flier Lu (flier)                                                                               }
{   Olivier Sannier (obones)                                                                       }
{   Petr Vones (pvones)                                                                            }
{   Heinz Zastrau (heinzz)                                                                         }
{   Andreas Hausladen (ahuser)                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Borland TD32 symbolic debugging information support routines and classes.                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclTD32;

interface

{$I jcl.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils, Contnrs,
  JclBase, JclFileUtils, JclPeImage;

{ TODO -cDOC : Original code: "Flier Lu" <flier_lu att yahoo dott com dott cn> }

// TD32 constants and structures
{*******************************************************************************

  [-----------------------------------------------------------------------]
  [         Symbol and Type OMF Format Borland Executable Files           ]
  [-----------------------------------------------------------------------]

  Introduction

  This section describes the format used to embed debugging information  into
  the executable file.

  Debug Information Format

  The format encompasses a block of  data which goes at  the end of the  .EXE
  file,  i.e.,  after   the   header   plus   load   image,   overlays,   and
  Windows/Presentation Manager  resource  compiler  information.   The  lower
  portion of the file is unaffected by the additional data.

  The last eight bytes of the file contain a signature and a long file offset
  from the end of the file (lfoBase).  The signature is FBxx, where xx is the
  version number.  The  long  offset  indicates  the  position  in  the  file
  (relative to the end of the file)  of the base address.  For the LX  format
  executables, the base address  is determined by  looking at the  executable
  header.

  The signatures have the following meanings:

    FB09        The signature for a Borland 32 bit symbol file.

  The value

          lfaBase=length of the file - lfoBase

  gives the base address of the start of the Symbol and Type OMF  information
  relative to  the beginning  of the  file.  All  other file  offsets in  the
  Symbol and Type OMF are relative to  the lfaBase.  At the base address  the
  signature is repeated, followed by the long displacement to the  subsection
  directory (lfoDir).  All subsections start on a long word boundary and  are
  designed to maintain  natural alignment internally  in each subsection  and
  within the subsection directory.

  Subsection Directory

  The subsection directory has the format

       Directory header

       Directory entry 0

       Directory entry 1

        .
        .
        .

       Directory entry n

  There is no requirement for a particular subsection of a particular module to exist.

  The following is the layout of the FB09 debug information in the image:

  FB09 Header

    sstModule [1]
    .
    .
    .
    sstModule [n]

    sstAlignSym [1]
    sstSrcModule [1]
    .
    .
    .
    sstAlignSym [n]
    sstSrcModule [n]

    sstGlobalSym
    sstGlobalTypes
    sstNames

    SubSection Directory

  FB09 Trailer

*******************************************************************************}

const
  Borland32BitSymbolFileSignatureForDelphi = $39304246; // 'FB09'
  Borland32BitSymbolFileSignatureForBCB    = $41304246; // 'FB0A'

type
  { Signature structure }
  PJclTD32FileSignature = ^TJclTD32FileSignature;
  TJclTD32FileSignature = packed record
    Signature: DWORD;
    Offset: DWORD;
  end;

const
  { Subsection Types }
  SUBSECTION_TYPE_MODULE         = $120;
  SUBSECTION_TYPE_TYPES          = $121;
  SUBSECTION_TYPE_SYMBOLS        = $124;
  SUBSECTION_TYPE_ALIGN_SYMBOLS  = $125;
  SUBSECTION_TYPE_SOURCE_MODULE  = $127;
  SUBSECTION_TYPE_GLOBAL_SYMBOLS = $129;
  SUBSECTION_TYPE_GLOBAL_TYPES   = $12B;
  SUBSECTION_TYPE_NAMES          = $130;

type
  { Subsection directory header structure }
  { The directory header structure is followed by the directory entries
    which specify the subsection type, module index, file offset, and size.
    The subsection directory gives the location (LFO) and size of each subsection,
    as well as its type and module number if applicable. }
  PDirectoryEntry = ^TDirectoryEntry;
  TDirectoryEntry = packed record
    SubsectionType: Word; // Subdirectory type
    ModuleIndex: Word;    // Module index
    Offset: DWORD;        // Offset from the base offset lfoBase
    Size: DWORD;          // Number of bytes in subsection
  end;

  { The subsection directory is prefixed with a directory header structure
    indicating size and number of subsection directory entries that follow. }
  PDirectoryHeader = ^TDirectoryHeader;
  TDirectoryHeader = packed record
    Size: Word;           // Length of this structure
    DirEntrySize: Word;   // Length of each directory entry
    DirEntryCount: DWORD; // Number of directory entries
    lfoNextDir: DWORD;    // Offset from lfoBase of next directory.
    Flags: DWORD;         // Flags describing directory and subsection tables.
    DirEntries: array [0..0] of TDirectoryEntry;
  end;


{*******************************************************************************

  SUBSECTION_TYPE_MODULE $120

  This describes the basic information about an object module including  code
  segments, module name,  and the  number of  segments for  the modules  that
  follow.  Directory entries for  sstModules  precede  all  other  subsection
  directory entries.

*******************************************************************************}

type
  PSegmentInfo = ^TSegmentInfo;
  TSegmentInfo = packed record
    Segment: Word; // Segment that this structure describes
    Flags: Word;   // Attributes for the logical segment.
                   // The following attributes are defined:
                   //   $0000  Data segment
                   //   $0001  Code segment
    Offset: DWORD; // Offset in segment where the code starts
    Size: DWORD;   // Count of the number of bytes of code in the segment
  end;
  PSegmentInfoArray = ^TSegmentInfoArray;
  TSegmentInfoArray = array [0..32767] of TSegmentInfo;

  PModuleInfo = ^TModuleInfo;
  TModuleInfo = packed record
    OverlayNumber: Word;  // Overlay number
    LibraryIndex: Word;   // Index into sstLibraries subsection
                          // if this module was linked from a library
    SegmentCount: Word;   // Count of the number of code segments
                          // this module contributes to
    DebuggingStyle: Word; // Debugging style  for this  module.
    NameIndex: DWORD;     // Name index of module.
    TimeStamp: DWORD;     // Time stamp from the OBJ file.
    Reserved: array [0..2] of DWORD; // Set to 0.
    Segments: array [0..0] of TSegmentInfo;
                          // Detailed information about each segment
                          // that code is contributed to.
                          // This is an array of cSeg count segment
                          // information descriptor structures.
  end;

{*******************************************************************************

  SUBSECTION_TYPE_SOURCE_MODULE $0127

  This table describes the source line number to addressing mapping
  information for a module. The table permits the description of a module
  containing multiple source files with each source file contributing code to
  one or more code segments. The base addresses of the tables described
  below are all relative to the beginning of the sstSrcModule table.


  Module header

  Information for source file 1

    Information for segment 1
         .
         .
         .
    Information for segment n

         .
         .
         .

  Information for source file n

    Information for segment 1
         .
         .
         .
    Information for segment n

*******************************************************************************}
type
  { The line number to address mapping information is contained in a table with
    the following format: }
  PLineMappingEntry = ^TLineMappingEntry;
  TLineMappingEntry = packed record
    SegmentIndex: Word;  // Segment index for this table
    PairCount: Word;     // Count of the number of source line pairs to follow
    Offsets: array [0..0] of DWORD;
                     // An array of 32-bit offsets for the offset
                     // within the code segment ofthe start of ine contained
                     // in the parallel array linenumber.
    (*
    { This is an array of 16-bit line numbers of the lines in the source file
      that cause code to be emitted to the code segment.
      This array is parallel to the offset array.
      If cPair is not even, then a zero word is emitted to
      maintain natural alignment in the sstSrcModule table. }
    LineNumbers: array [0..PairCount - 1] of Word;
    *)
  end;

  TOffsetPair = packed record
    StartOffset: DWORD;
    EndOffset: DWORD;
  end;
  POffsetPairArray = ^TOffsetPairArray;
  TOffsetPairArray = array [0..32767] of TOffsetPair;

  { The file table describes the code segments that receive code from this
    source file. Source file entries have the following format: }
  PSourceFileEntry = ^TSourceFileEntry;
  TSourceFileEntry = packed record
    SegmentCount: Word; // Number of segments that receive code from this source file.
    NameIndex: DWORD;   // Name index of Source file name.

    BaseSrcLines: array [0..0] of DWORD;
                        // An array of offsets for the line/address mapping
                        // tables for each of the segments that receive code
                        // from this source file.
    (*
    { An array  of two  32-bit offsets  per segment  that
      receives code from this  module.  The first  offset
      is the offset within the segment of the first  byte
      of code from this module.  The second offset is the
      ending address of the  code from this module.   The
      order of these pairs corresponds to the ordering of
      the segments in the  seg  array.   Zeros  in  these
      entries means that the information is not known and
      the file and line tables described below need to be
      examined to determine if an address of interest  is
      contained within the code from this module. }
    SegmentAddress: array [0..SegmentCount - 1] of TOffsetPair;

    Name: ShortString; // Count of the number of bytes in source file name
    *)
  end;

  { The module header structure describes the source file and code segment
    organization of the module. Each module header has the following format: }
  PSourceModuleInfo = ^TSourceModuleInfo;
  TSourceModuleInfo = packed record
    FileCount: Word;    // The number of source file scontributing code to segments
    SegmentCount: Word; // The number of code segments receiving code from this module

    BaseSrcFiles: array [0..0] of DWORD;
    (*
    // This is an array of base offsets from the beginning of the sstSrcModule table
    BaseSrcFiles: array [0..FileCount - 1] of DWORD;

    { An array  of two  32-bit offsets  per segment  that
      receives code from this  module.  The first  offset
      is the offset within the segment of the first  byte
      of code from this module.  The second offset is the
      ending address of the  code from this module.   The
      order of these pairs corresponds to the ordering of
      the segments in the  seg  array.   Zeros  in  these
      entries means that the information is not known and
      the file and line tables described below need to be
      examined to determine if an address of interest  is
      contained within the code from this module. }
    SegmentAddress: array [0..SegmentCount - 1] of TOffsetPair;

    { An array of segment indices that receive code  from
      this module.  If the  number  of  segments  is  not
      even, a pad word  is inserted  to maintain  natural
      alignment. }
    SegmentIndexes: array [0..SegmentCount - 1] of Word;
    *)
  end;

{*******************************************************************************

  SUBSECTION_TYPE_GLOBAL_TYPES $12b

  This subsection contains the packed  type records for the executable  file.
  The first long word of the subsection  contains the number of types in  the
  table.  This count is  followed by a count-sized  array of long offsets  to
  the  corresponding  type  record.   As  the  sstGlobalTypes  subsection  is
  written, each  type record  is forced  to start  on a  long word  boundary.
  However, the length of the  type string is NOT  adjusted by the pad  count.
  The remainder of the subsection contains  the type records.

*******************************************************************************}

type
  PGlobalTypeInfo = ^TGlobalTypeInfo;
  TGlobalTypeInfo = packed record
    Count: DWORD; // count of the number of types
    // offset of each type string from the beginning of table
    Offsets: array [0..0] of DWORD;
  end;

const
  { Symbol type defines }
  SYMBOL_TYPE_COMPILE        = $0001; // Compile flags symbol
  SYMBOL_TYPE_REGISTER       = $0002; // Register variable
  SYMBOL_TYPE_CONST          = $0003; // Constant symbol
  SYMBOL_TYPE_UDT            = $0004; // User-defined Type
  SYMBOL_TYPE_SSEARCH        = $0005; // Start search
  SYMBOL_TYPE_END            = $0006; // End block, procedure, with, or thunk
  SYMBOL_TYPE_SKIP           = $0007; // Skip - Reserve symbol space
  SYMBOL_TYPE_CVRESERVE      = $0008; // Reserved for Code View internal use
  SYMBOL_TYPE_OBJNAME        = $0009; // Specify name of object file

  SYMBOL_TYPE_BPREL16        = $0100; // BP relative 16:16
  SYMBOL_TYPE_LDATA16        = $0101; // Local data 16:16
  SYMBOL_TYPE_GDATA16        = $0102; // Global data 16:16
  SYMBOL_TYPE_PUB16          = $0103; // Public symbol 16:16
  SYMBOL_TYPE_LPROC16        = $0104; // Local procedure start 16:16
  SYMBOL_TYPE_GPROC16        = $0105; // Global procedure start 16:16
  SYMBOL_TYPE_THUNK16        = $0106; // Thunk start 16:16
  SYMBOL_TYPE_BLOCK16        = $0107; // Block start 16:16
  SYMBOL_TYPE_WITH16         = $0108; // With start 16:16
  SYMBOL_TYPE_LABEL16        = $0109; // Code label 16:16
  SYMBOL_TYPE_CEXMODEL16     = $010A; // Change execution model 16:16
  SYMBOL_TYPE_VFTPATH16      = $010B; // Virtual function table path descriptor 16:16

  SYMBOL_TYPE_BPREL32        = $0200; // BP relative 16:32
  SYMBOL_TYPE_LDATA32        = $0201; // Local data 16:32
  SYMBOL_TYPE_GDATA32        = $0202; // Global data 16:32
  SYMBOL_TYPE_PUB32          = $0203; // Public symbol 16:32
  SYMBOL_TYPE_LPROC32        = $0204; // Local procedure start 16:32
  SYMBOL_TYPE_GPROC32        = $0205; // Global procedure start 16:32
  SYMBOL_TYPE_THUNK32        = $0206; // Thunk start 16:32
  SYMBOL_TYPE_BLOCK32        = $0207; // Block start 16:32
  SYMBOL_TYPE_WITH32         = $0208; // With start 16:32
  SYMBOL_TYPE_LABEL32        = $0209; // Label 16:32
  SYMBOL_TYPE_CEXMODEL32     = $020A; // Change execution model 16:32
  SYMBOL_TYPE_VFTPATH32      = $020B; // Virtual function table path descriptor 16:32

{*******************************************************************************

  Global and Local Procedure Start 16:32

  SYMBOL_TYPE_LPROC32 $0204
  SYMBOL_TYPE_GPROC32 $0205

    The symbol records define local (file static) and global procedure
  definition. For C/C++, functions that are declared static to a module are
  emitted as Local Procedure symbols. Functions not specifically declared
  static are emitted as Global Procedures.
    For each SYMBOL_TYPE_GPROC32 emitted, an SYMBOL_TYPE_GPROCREF symbol
  must be fabricated and emitted to the SUBSECTION_TYPE_GLOBAL_SYMBOLS section.

*******************************************************************************}

type
  TSymbolProcInfo = packed record
    pParent: DWORD;
    pEnd: DWORD;
    pNext: DWORD;
    Size: DWORD;        // Length in bytes of this procedure
    DebugStart: DWORD;  // Offset in bytes from the start of the procedure to
                        // the point where the stack frame has been set up.
    DebugEnd: DWORD;    // Offset in bytes from the start of the procedure to
                        // the point where the  procedure is  ready to  return
                        // and has calculated its return value, if any.
                        // Frame and register variables an still be viewed.
    Offset: DWORD;      // Offset portion of  the segmented address of
                        // the start of the procedure in the code segment
    Segment: Word;      // Segment portion of the segmented address of
                        // the start of the procedure in the code segment
    ProcType: DWORD;    // Type of the procedure type record
    NearFar: Byte;      // Type of return the procedure makes:
                        //   0       near
                        //   4       far
    Reserved: Byte;
    NameIndex: DWORD;   // Name index of procedure
  end;

  TSymbolObjNameInfo = packed record
    Signature: DWORD;   // Signature for the CodeView information contained in
                        // this module
    NameIndex: DWORD;   // Name index of the object file
  end;

  TSymbolDataInfo = packed record
    Offset: DWORD;      // Offset portion of  the segmented address of
                        // the start of the data in the code segment
    Segment: Word;      // Segment portion of the segmented address of
                        // the start of the data in the code segment
    Reserved: Word;
    TypeIndex: DWORD;   // Type index of the symbol
    NameIndex: DWORD;   // Name index of the symbol
  end;

  TSymbolWithInfo = packed record
    pParent: DWORD;
    pEnd: DWORD;
    Size: DWORD;        // Length in bytes of this "with"
    Offset: DWORD;      // Offset portion of the segmented address of
                        // the start of the "with" in the code segment
    Segment: Word;      // Segment portion of the segmented address of
                        // the start of the "with" in the code segment
    Reserved: Word;
    NameIndex: DWORD;   // Name index of the "with"
  end;

  TSymbolLabelInfo = packed record
    Offset: DWORD;      // Offset portion of  the segmented address of
                        // the start of the label in the code segment
    Segment: Word;      // Segment portion of the segmented address of
                        // the start of the label in the code segment
    NearFar: Byte;      // Address mode of the label:
                        //   0       near
                        //   4       far
    Reserved: Byte;
    NameIndex: DWORD;   // Name index of the label
  end;

  TSymbolConstantInfo = packed record
    TypeIndex: DWORD;   // Type index of the constant (for enums)
    NameIndex: DWORD;   // Name index of the constant
    Reserved: DWORD;
    Value: DWORD;       // value of the constant
  end;

  TSymbolUdtInfo = packed record
    TypeIndex: DWORD;   // Type index of the type
    Properties: Word;   // isTag:1 True if this is a tag (not a typedef)
                        // isNest:1 True if the type is a nested type (its name
                        // will be 'class_name::type_name' in that case)
    NameIndex: DWORD;   // Name index of the type
    Reserved: DWORD;
  end;

  TSymbolVftPathInfo = packed record
    Offset: DWORD;      // Offset portion of start of the virtual function table
    Segment: Word;      // Segment portion of the virtual function table
    Reserved: Word;
    RootIndex: DWORD;   // The type index of the class at the root of the path
    PathIndex: DWORD;   // Type index of the record describing the base class
                        // path from the root to the leaf class for the virtual
                        // function table
  end;

type
  { Symbol Information Records }
  PSymbolInfo = ^TSymbolInfo;
  TSymbolInfo = packed record
    Size: Word;
    SymbolType: Word;
    case Word of
      SYMBOL_TYPE_LPROC32, SYMBOL_TYPE_GPROC32:
        (Proc: TSymbolProcInfo);
      SYMBOL_TYPE_OBJNAME:
        (ObjName: TSymbolObjNameInfo);
      SYMBOL_TYPE_LDATA32, SYMBOL_TYPE_GDATA32, SYMBOL_TYPE_PUB32:
        (Data: TSymbolDataInfo);
      SYMBOL_TYPE_WITH32:
        (With32: TSymbolWithInfo);
      SYMBOL_TYPE_LABEL32:
        (Label32: TSymbolLabelInfo);
      SYMBOL_TYPE_CONST:
        (Constant: TSymbolConstantInfo);
      SYMBOL_TYPE_UDT:
        (Udt: TSymbolUdtInfo);
      SYMBOL_TYPE_VFTPATH32:
        (VftPath: TSymbolVftPathInfo);
  end;

  PSymbolInfos = ^TSymbolInfos;
  TSymbolInfos = packed record
    Signature: DWORD;
    Symbols: array [0..0] of TSymbolInfo;
  end;

{$IFDEF SUPPORTS_EXTSYM}

{$EXTERNALSYM Borland32BitSymbolFileSignatureForDelphi}
{$EXTERNALSYM Borland32BitSymbolFileSignatureForBCB}

{$EXTERNALSYM SUBSECTION_TYPE_MODULE}
{$EXTERNALSYM SUBSECTION_TYPE_TYPES}
{$EXTERNALSYM SUBSECTION_TYPE_SYMBOLS}
{$EXTERNALSYM SUBSECTION_TYPE_ALIGN_SYMBOLS}
{$EXTERNALSYM SUBSECTION_TYPE_SOURCE_MODULE}
{$EXTERNALSYM SUBSECTION_TYPE_GLOBAL_SYMBOLS}
{$EXTERNALSYM SUBSECTION_TYPE_GLOBAL_TYPES}
{$EXTERNALSYM SUBSECTION_TYPE_NAMES}

{$EXTERNALSYM SYMBOL_TYPE_COMPILE}
{$EXTERNALSYM SYMBOL_TYPE_REGISTER}
{$EXTERNALSYM SYMBOL_TYPE_CONST}
{$EXTERNALSYM SYMBOL_TYPE_UDT}
{$EXTERNALSYM SYMBOL_TYPE_SSEARCH}
{$EXTERNALSYM SYMBOL_TYPE_END}
{$EXTERNALSYM SYMBOL_TYPE_SKIP}
{$EXTERNALSYM SYMBOL_TYPE_CVRESERVE}
{$EXTERNALSYM SYMBOL_TYPE_OBJNAME}

{$EXTERNALSYM SYMBOL_TYPE_BPREL16}
{$EXTERNALSYM SYMBOL_TYPE_LDATA16}
{$EXTERNALSYM SYMBOL_TYPE_GDATA16}
{$EXTERNALSYM SYMBOL_TYPE_PUB16}
{$EXTERNALSYM SYMBOL_TYPE_LPROC16}
{$EXTERNALSYM SYMBOL_TYPE_GPROC16}
{$EXTERNALSYM SYMBOL_TYPE_THUNK16}
{$EXTERNALSYM SYMBOL_TYPE_BLOCK16}
{$EXTERNALSYM SYMBOL_TYPE_WITH16}
{$EXTERNALSYM SYMBOL_TYPE_LABEL16}
{$EXTERNALSYM SYMBOL_TYPE_CEXMODEL16}
{$EXTERNALSYM SYMBOL_TYPE_VFTPATH16}

{$EXTERNALSYM SYMBOL_TYPE_BPREL32}
{$EXTERNALSYM SYMBOL_TYPE_LDATA32}
{$EXTERNALSYM SYMBOL_TYPE_GDATA32}
{$EXTERNALSYM SYMBOL_TYPE_PUB32}
{$EXTERNALSYM SYMBOL_TYPE_LPROC32}
{$EXTERNALSYM SYMBOL_TYPE_GPROC32}
{$EXTERNALSYM SYMBOL_TYPE_THUNK32}
{$EXTERNALSYM SYMBOL_TYPE_BLOCK32}
{$EXTERNALSYM SYMBOL_TYPE_WITH32}
{$EXTERNALSYM SYMBOL_TYPE_LABEL32}
{$EXTERNALSYM SYMBOL_TYPE_CEXMODEL32}
{$EXTERNALSYM SYMBOL_TYPE_VFTPATH32}

{$ENDIF SUPPORTS_EXTSYM}

// TD32 information related classes
type
  TJclModuleInfo = class(TObject)
  private
    FNameIndex: DWORD;
    FSegments: PSegmentInfoArray;
    FSegmentCount: Integer;
    function GetSegment(const Idx: Integer): TSegmentInfo;
  protected
    constructor Create(pModInfo: PModuleInfo);
  public
    property NameIndex: DWORD read FNameIndex;
    property SegmentCount: Integer read FSegmentCount; //GetSegmentCount;
    property Segment[const Idx: Integer]: TSegmentInfo read GetSegment; default;
  end;

  TJclLineInfo = class(TObject)
  private
    FLineNo: DWORD;
    FOffset: DWORD;
  protected
    constructor Create(ALineNo, AOffset: DWORD);
  public
    property LineNo: DWORD read FLineNo;
    property Offset: DWORD read FOffset;
  end;

  TJclSourceModuleInfo = class(TObject)
  private
    FLines: TObjectList;
    FSegments: POffsetPairArray;
    FSegmentCount: Integer;
    FNameIndex: DWORD;
    function GetLine(const Idx: Integer): TJclLineInfo;
    function GetLineCount: Integer;
    function GetSegment(const Idx: Integer): TOffsetPair;
  protected
    constructor Create(pSrcFile: PSourceFileEntry; Base: DWORD);
  public
    destructor Destroy; override;
    function FindLine(const AAddr: DWORD; var ALine: TJclLineInfo): Boolean;
    property NameIndex: DWORD read FNameIndex;
    property LineCount: Integer read GetLineCount;
    property Line[const Idx: Integer]: TJclLineInfo read GetLine; default;
    property SegmentCount: Integer read FSegmentCount; //GetSegmentCount;
    property Segment[const Idx: Integer]: TOffsetPair read GetSegment;
  end;

  TJclSymbolInfo = class(TObject)
  private
    FSymbolType: Word;
  protected
    constructor Create(pSymInfo: PSymbolInfo); virtual;
    property SymbolType: Word read FSymbolType;
  end;

  TJclProcSymbolInfo = class(TJclSymbolInfo)
  private
    FNameIndex: DWORD;
    FOffset: DWORD;
    FSize: DWORD;
  protected
    constructor Create(pSymInfo: PSymbolInfo); override;
  public
    property NameIndex: DWORD read FNameIndex;
    property Offset: DWORD read FOffset;
    property Size: DWORD read FSize;
  end;

  TJclLocalProcSymbolInfo = class(TJclProcSymbolInfo);
  TJclGlobalProcSymbolInfo = class(TJclProcSymbolInfo);

  { not used by Delphi }
  TJclObjNameSymbolInfo = class(TJclSymbolInfo)
  private
    FSignature: DWORD;
    FNameIndex: DWORD;
  protected
    constructor Create(pSymInfo: PSymbolInfo); override;
  public
    property NameIndex: DWORD read FNameIndex;
    property Signature: DWORD read FSignature;
  end;

  TJclDataSymbolInfo = class(TJclSymbolInfo)
  private
    FOffset: DWORD;
    FTypeIndex: DWORD;
    FNameIndex: DWORD;
  protected
    constructor Create(pSymInfo: PSymbolInfo); override;
  public
    property NameIndex: DWORD read FNameIndex;
    property TypeIndex: DWORD read FTypeIndex;
    property Offset: DWORD read FOffset;
  end;

  TJclLDataSymbolInfo = class(TJclDataSymbolInfo);
  TJclGDataSymbolInfo = class(TJclDataSymbolInfo);
  TJclPublicSymbolInfo = class(TJclDataSymbolInfo);

  TJclWithSymbolInfo = class(TJclSymbolInfo)
  private
    FOffset: DWORD;
    FSize: DWORD;
    FNameIndex: DWORD;
  protected
    constructor Create(pSymInfo: PSymbolInfo); override;
  public
    property NameIndex: DWORD read FNameIndex;
    property Offset: DWORD read FOffset;
    property Size: DWORD read FSize;
  end;

  { not used by Delphi }
  TJclLabelSymbolInfo = class(TJclSymbolInfo)
  private
    FOffset: DWORD;
    FNameIndex: DWORD;
  protected
    constructor Create(pSymInfo: PSymbolInfo); override;
  public
    property NameIndex: DWORD read FNameIndex;
    property Offset: DWORD read FOffset;
  end;

  { not used by Delphi }
  TJclConstantSymbolInfo = class(TJclSymbolInfo)
  private
    FValue: DWORD;
    FTypeIndex: DWORD;
    FNameIndex: DWORD;
  protected
    constructor Create(pSymInfo: PSymbolInfo); override;
  public
    property NameIndex: DWORD read FNameIndex;
    property TypeIndex: DWORD read FTypeIndex; // for enums
    property Value: DWORD read FValue;
  end;

  TJclUdtSymbolInfo = class(TJclSymbolInfo)
  private
    FTypeIndex: DWORD;
    FNameIndex: DWORD;
    FProperties: Word;
  protected
    constructor Create(pSymInfo: PSymbolInfo); override;
  public
    property NameIndex: DWORD read FNameIndex;
    property TypeIndex: DWORD read FTypeIndex;
    property Properties: Word read FProperties;
  end;

  { not used by Delphi }
  TJclVftPathSymbolInfo = class(TJclSymbolInfo)
  private
    FRootIndex: DWORD;
    FPathIndex: DWORD;
    FOffset: DWORD;
  protected
    constructor Create(pSymInfo: PSymbolInfo); override;
  public
    property RootIndex: DWORD read FRootIndex;
    property PathIndex: DWORD read FPathIndex;
    property Offset: DWORD read FOffset;
  end;

  // TD32 parser
  TJclTD32InfoParser = class(TObject)
  private
    FBase: Pointer;
    FData: TCustomMemoryStream;
    FNames: TList;
    FModules: TObjectList;
    FSourceModules: TObjectList;
    FSymbols: TObjectList;
    FProcSymbols: TList;
    FValidData: Boolean;
    function GetName(const Idx: Integer): string;
    function GetNameCount: Integer;
    function GetSymbol(const Idx: Integer): TJclSymbolInfo;
    function GetSymbolCount: Integer;
    function GetProcSymbol(const Idx: Integer): TJclProcSymbolInfo;
    function GetProcSymbolCount: Integer;
    function GetModule(const Idx: Integer): TJclModuleInfo;
    function GetModuleCount: Integer;
    function GetSourceModule(const Idx: Integer): TJclSourceModuleInfo;
    function GetSourceModuleCount: Integer;
  protected
    procedure Analyse;
    procedure AnalyseNames(const pSubsection: Pointer; const Size: DWORD); virtual;
    procedure AnalyseGlobalTypes(const pTypes: Pointer; const Size: DWORD); virtual;
    procedure AnalyseAlignSymbols(pSymbols: PSymbolInfos; const Size: DWORD); virtual;
    procedure AnalyseModules(pModInfo: PModuleInfo; const Size: DWORD); virtual;
    procedure AnalyseSourceModules(pSrcModInfo: PSourceModuleInfo; const Size: DWORD); virtual;
    procedure AnalyseUnknownSubSection(const pSubsection: Pointer; const Size: DWORD); virtual;
    function LfaToVa(Lfa: DWORD): Pointer;
  public
    constructor Create(const ATD32Data: TCustomMemoryStream); // Data mustn't be freed before the class is destroyed
    destructor Destroy; override;
    function FindModule(const AAddr: DWORD; var AMod: TJclModuleInfo): Boolean;
    function FindSourceModule(const AAddr: DWORD; var ASrcMod: TJclSourceModuleInfo): Boolean;
    function FindProc(const AAddr: DWORD; var AProc: TJclProcSymbolInfo): Boolean;
    class function IsTD32Sign(const Sign: TJclTD32FileSignature): Boolean;
    class function IsTD32DebugInfoValid(const DebugData: Pointer; const DebugDataSize: LongWord): Boolean;
    property Data: TCustomMemoryStream read FData;
    property Names[const Idx: Integer]: string read GetName;
    property NameCount: Integer read GetNameCount;
    property Symbols[const Idx: Integer]: TJclSymbolInfo read GetSymbol;
    property SymbolCount: Integer read GetSymbolCount;
    property ProcSymbols[const Idx: Integer]: TJclProcSymbolInfo read GetProcSymbol;
    property ProcSymbolCount: Integer read GetProcSymbolCount;
    property Modules[const Idx: Integer]: TJclModuleInfo read GetModule;
    property ModuleCount: Integer read GetModuleCount;
    property SourceModules[const Idx: Integer]: TJclSourceModuleInfo read GetSourceModule;
    property SourceModuleCount: Integer read GetSourceModuleCount;
    property ValidData: Boolean read FValidData;
  end;

  // TD32 scanner with source location methods
  TJclTD32InfoScanner = class(TJclTD32InfoParser)
  public
    function LineNumberFromAddr(AAddr: DWORD; var Offset: Integer): Integer; overload;
    function LineNumberFromAddr(AAddr: DWORD): Integer; overload;
    function ProcNameFromAddr(AAddr: DWORD): string; overload;
    function ProcNameFromAddr(AAddr: DWORD; var Offset: Integer): string; overload;
    function ModuleNameFromAddr(AAddr: DWORD): string;
    function SourceNameFromAddr(AAddr: DWORD): string;
  end;

  // PE Image with TD32 information and source location support 
  TJclPeBorTD32Image = class(TJclPeBorImage)
  private
    FIsTD32DebugPresent: Boolean;
    FTD32DebugData: TCustomMemoryStream;
    FTD32Scanner: TJclTD32InfoScanner;
  protected
    procedure AfterOpen; override;
    procedure Clear; override;
    procedure ClearDebugData;
    procedure CheckDebugData;
    function IsDebugInfoInImage(var DataStream: TCustomMemoryStream): Boolean;
    function IsDebugInfoInTds(var DataStream: TCustomMemoryStream): Boolean;
  public
    property IsTD32DebugPresent: Boolean read FIsTD32DebugPresent;
    property TD32DebugData: TCustomMemoryStream read FTD32DebugData;
    property TD32Scanner: TJclTD32InfoScanner read FTD32Scanner;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclTD32.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  JclResources, JclSysUtils;

const
  TurboDebuggerSymbolExt = '.tds';

//=== { TJclModuleInfo } =====================================================

constructor TJclModuleInfo.Create(pModInfo: PModuleInfo);
begin
  Assert(Assigned(pModInfo));
  inherited Create;
  FNameIndex := pModInfo.NameIndex;
  FSegments := @pModInfo.Segments[0];
  FSegmentCount := pModInfo.SegmentCount;
end;

function TJclModuleInfo.GetSegment(const Idx: Integer): TSegmentInfo;
begin
  Assert((0 <= Idx) and (Idx < FSegmentCount));
  Result := FSegments[Idx];
end;

//=== { TJclLineInfo } =======================================================

constructor TJclLineInfo.Create(ALineNo, AOffset: DWORD);
begin
  inherited Create;
  FLineNo := ALineNo;
  FOffset := AOffset;
end;

//=== { TJclSourceModuleInfo } ===============================================

constructor TJclSourceModuleInfo.Create(pSrcFile: PSourceFileEntry; Base: DWORD);
type
  PArrayOfWord = ^TArrayOfWord;
  TArrayOfWord = array [0..0] of Word;
var
  I, J: Integer;
  pLineEntry: PLineMappingEntry;
begin
  Assert(Assigned(pSrcFile));
  inherited Create;
  FNameIndex := pSrcFile.NameIndex;
  FLines := TObjectList.Create;
  {$RANGECHECKS OFF}
  for I := 0 to pSrcFile.SegmentCount - 1 do
  begin
    pLineEntry := PLineMappingEntry(Base + pSrcFile.BaseSrcLines[I]);
    for J := 0 to pLineEntry.PairCount - 1 do
      FLines.Add(TJclLineInfo.Create(
        PArrayOfWord(@pLineEntry.Offsets[pLineEntry.PairCount])^[J],
        pLineEntry.Offsets[J]));
  end;

  FSegments := @pSrcFile.BaseSrcLines[pSrcFile.SegmentCount];
  FSegmentCount := pSrcFile.SegmentCount;
  {$IFDEF RANGECHECKS_ON}
  {$RANGECHECKS ON}
  {$ENDIF RANGECHECKS_ON}
end;

destructor TJclSourceModuleInfo.Destroy;
begin
  FreeAndNil(FLines);
  inherited Destroy;
end;

function TJclSourceModuleInfo.GetLine(const Idx: Integer): TJclLineInfo;
begin
  Result := TJclLineInfo(FLines.Items[Idx]);
end;

function TJclSourceModuleInfo.GetLineCount: Integer;
begin
  Result := FLines.Count;
end;

function TJclSourceModuleInfo.GetSegment(const Idx: Integer): TOffsetPair;
begin
  Assert((0 <= Idx) and (Idx < FSegmentCount));
  Result := FSegments[Idx];
end;

function TJclSourceModuleInfo.FindLine(const AAddr: DWORD; var ALine: TJclLineInfo): Boolean;
var
  I: Integer;
begin
  for I := 0 to LineCount - 1 do
    with Line[I] do
    begin
      if AAddr = Offset then
      begin
        Result := True;
        ALine := Line[I];
        Exit;
      end
      else
      if (I > 1) and (Line[I - 1].Offset < AAddr) and (AAddr < Offset) then
      begin
        Result := True;
        ALine := Line[I-1];
        Exit;
      end;
    end;
  Result := False;
  ALine := nil;
end;

//=== { TJclSymbolInfo } =====================================================

constructor TJclSymbolInfo.Create(pSymInfo: PSymbolInfo);
begin
  Assert(Assigned(pSymInfo));
  inherited Create;
  FSymbolType := pSymInfo.SymbolType;
end;

//=== { TJclProcSymbolInfo } =================================================

constructor TJclProcSymbolInfo.Create(pSymInfo: PSymbolInfo);
begin
  Assert(Assigned(pSymInfo));
  inherited Create(pSymInfo);
  with pSymInfo^ do
  begin
    FNameIndex := Proc.NameIndex;
    FOffset := Proc.Offset;
    FSize := Proc.Size;
  end;
end;

//=== { TJclObjNameSymbolInfo } ==============================================

constructor TJclObjNameSymbolInfo.Create(pSymInfo: PSymbolInfo);
begin
  Assert(Assigned(pSymInfo));
  inherited Create(pSymInfo);
  with pSymInfo^ do
  begin
    FNameIndex := ObjName.NameIndex;
    FSignature := ObjName.Signature;
  end;
end;

//=== { TJclDataSymbolInfo } =================================================

constructor TJclDataSymbolInfo.Create(pSymInfo: PSymbolInfo);
begin
  Assert(Assigned(pSymInfo));
  inherited Create(pSymInfo);
  with pSymInfo^ do
  begin
    FTypeIndex := Data.TypeIndex;
    FNameIndex := Data.NameIndex;
    FOffset := Data.Offset;
  end;
end;

//=== { TJclWithSymbolInfo } =================================================

constructor TJclWithSymbolInfo.Create(pSymInfo: PSymbolInfo);
begin
  Assert(Assigned(pSymInfo));
  inherited Create(pSymInfo);
  with pSymInfo^ do
  begin
    FNameIndex := With32.NameIndex;
    FOffset := With32.Offset;
    FSize := With32.Size;
  end;
end;

//=== { TJclLabelSymbolInfo } ================================================

constructor TJclLabelSymbolInfo.Create(pSymInfo: PSymbolInfo);
begin
  Assert(Assigned(pSymInfo));
  inherited Create(pSymInfo);
  with pSymInfo^ do
  begin
    FNameIndex := Label32.NameIndex;
    FOffset := Label32.Offset;
  end;
end;

//=== { TJclConstantSymbolInfo } =============================================

constructor TJclConstantSymbolInfo.Create(pSymInfo: PSymbolInfo);
begin
  Assert(Assigned(pSymInfo));
  inherited Create(pSymInfo);
  with pSymInfo^ do
  begin
    FNameIndex := Constant.NameIndex;
    FTypeIndex := Constant.TypeIndex;
    FValue := Constant.Value;
  end;
end;

//=== { TJclUdtSymbolInfo } ==================================================

constructor TJclUdtSymbolInfo.Create(pSymInfo: PSymbolInfo);
begin
  Assert(Assigned(pSymInfo));
  inherited Create(pSymInfo);
  with pSymInfo^ do
  begin
    FNameIndex := Udt.NameIndex;
    FTypeIndex := Udt.TypeIndex;
    FProperties := Udt.Properties;
  end;
end;

//=== { TJclVftPathSymbolInfo } ==============================================

constructor TJclVftPathSymbolInfo.Create(pSymInfo: PSymbolInfo);
begin
  Assert(Assigned(pSymInfo));
  inherited Create(pSymInfo);
  with pSymInfo^ do
  begin
    FRootIndex := VftPath.RootIndex;
    FPathIndex := VftPath.PathIndex;
    FOffset := VftPath.Offset;
  end;
end;

//=== { TJclTD32InfoParser } =================================================

constructor TJclTD32InfoParser.Create(const ATD32Data: TCustomMemoryStream);
begin
  Assert(Assigned(ATD32Data));
  inherited Create;
  FNames := TList.Create;
  FModules := TObjectList.Create;
  FSourceModules := TObjectList.Create;
  FSymbols := TObjectList.Create;
  FProcSymbols := TList.Create;
  FNames.Add(nil);
  FData := ATD32Data;
  FBase := FData.Memory;
  FValidData := IsTD32DebugInfoValid(FBase, FData.Size);
  if FValidData then
    Analyse;
end;

destructor TJclTD32InfoParser.Destroy;
begin
  FreeAndNil(FProcSymbols);
  FreeAndNil(FSymbols);
  FreeAndNil(FSourceModules);
  FreeAndNil(FModules);
  FreeAndNil(FNames);
  inherited Destroy;
end;

procedure TJclTD32InfoParser.Analyse;
var
  I: Integer;
  pDirHeader: PDirectoryHeader;
  pSubsection: Pointer;
begin
  pDirHeader := PDirectoryHeader(LfaToVa(PJclTD32FileSignature(LfaToVa(0)).Offset));
  while True do
  begin
    Assert(pDirHeader.DirEntrySize = SizeOf(TDirectoryEntry));
    {$RANGECHECKS OFF}
    for I := 0 to pDirHeader.DirEntryCount - 1 do
      with pDirHeader.DirEntries[I] do
      begin
        pSubsection := LfaToVa(Offset);
        case SubsectionType of
          SUBSECTION_TYPE_MODULE:
            AnalyseModules(pSubsection, Size);
          SUBSECTION_TYPE_ALIGN_SYMBOLS:
            AnalyseAlignSymbols(pSubsection, Size);
          SUBSECTION_TYPE_SOURCE_MODULE:
            AnalyseSourceModules(pSubsection, Size);
          SUBSECTION_TYPE_NAMES:
            AnalyseNames(pSubsection, Size);
          SUBSECTION_TYPE_GLOBAL_TYPES:
            AnalyseGlobalTypes(pSubsection, Size);
        else
          AnalyseUnknownSubSection(pSubsection, Size);
        end;
      end;
    {$IFDEF RANGECHECKS_ON}
    {$RANGECHECKS ON}
    {$ENDIF RANGECHECKS_ON}
    if pDirHeader.lfoNextDir <> 0 then
      pDirHeader := PDirectoryHeader(LfaToVa(pDirHeader.lfoNextDir))
    else
      Break;
  end;
end;

procedure TJclTD32InfoParser.AnalyseNames(const pSubsection: Pointer; const Size: DWORD);
var
  I, Count, Len: Integer;
  pszName: PChar;
begin
  Count := PDWORD(pSubsection)^;
  pszName := PChar(DWORD(pSubsection) + SizeOf(DWORD));
  if Count > 0 then
  begin
    FNames.Capacity := FNames.Capacity + Count;
    for I := 0 to Count - 1 do
    begin
      // Get the length of the name
      Len := Ord(pszName^);
      Inc(pszName);
      // Get the name
      FNames.Add(pszName);
      // skip the length of name and a NULL at the end
      Inc(pszName, Len + 1);
    end;
  end;
end;

const
  // Leaf indices for type records that can be referenced from symbols
  LF_MODIFIER  = $0001;
  LF_POINTER   = $0002;
  LF_ARRAY     = $0003;
  LF_CLASS     = $0004;
  LF_STRUCTURE = $0005;
  LF_UNION     = $0006;
  LF_ENUM      = $0007;
  LF_PROCEDURE = $0008;
  LF_MFUNCTION = $0009;
  LF_VTSHAPE   = $000a;
  LF_COBOL0    = $000b;
  LF_COBOL1    = $000c;
  LF_BARRAY    = $000d;
  LF_LABEL     = $000e;
  LF_NULL      = $000f;
  LF_NOTTRAN   = $0010;
  LF_DIMARRAY  = $0011;
  LF_VFTPATH   = $0012;

  // Leaf indices for type records that can be referenced from other type records
  LF_SKIP       = $0200;
  LF_ARGLIST    = $0201;
  LF_DEFARG     = $0202;
  LF_LIST       = $0203;
  LF_FIELDLIST  = $0204;
  LF_DERIVED    = $0205;
  LF_BITFIELD   = $0206;
  LF_METHODLIST = $0207;
  LF_DIMCONU    = $0208;
  LF_DIMCONLU   = $0209;
  LF_DIMVARU    = $020a;
  LF_DIMVARLU   = $020b;
  LF_REFSYM     = $020c;

  // Leaf indices for fields of complex lists:
  LF_BCLASS     = $0400;
  LF_VBCLASS    = $0401;
  LF_IVBCLASS   = $0402;
  LF_ENUMERATE  = $0403;
  LF_FRIENDFCN  = $0404;
  LF_INDEX      = $0405;
  LF_MEMBER     = $0406;
  LF_STMEMBER   = $0407;
  LF_METHOD     = $0408;
  LF_NESTTYPE   = $0409;
  LF_VFUNCTAB   = $040a;
  LF_FRIENDCLS  = $040b;

  // Leaf indices for numeric fields of symbols and type records:
  LF_NUMERIC    = $8000;
  LF_CHAR       = $8001;
  LF_SHORT      = $8002;
  LF_USHORT     = $8003;
  LF_LONG       = $8004;
  LF_ULONG      = $8005;
  LF_REAL32     = $8006;
  LF_REAL64     = $8007;
  LF_REAL80     = $8008;
  LF_REAL128    = $8009;
  LF_QUADWORD   = $800a;
  LF_UQUADWORD  = $800b;
  LF_REAL48     = $800c;

  LF_PAD0  = $f0;
  LF_PAD1  = $f1;
  LF_PAD2  = $f2;
  LF_PAD3  = $f3;
  LF_PAD4  = $f4;
  LF_PAD5  = $f5;
  LF_PAD6  = $f6;
  LF_PAD7  = $f7;
  LF_PAD8  = $f8;
  LF_PAD9  = $f9;
  LF_PAD10 = $fa;
  LF_PAD11 = $fb;
  LF_PAD12 = $fc;
  LF_PAD13 = $fd;
  LF_PAD14 = $fe;
  LF_PAD15 = $ff;

type
  PSymbolTypeInfo = ^TSymbolTypeInfo;
  TSymbolTypeInfo = packed record
    TypeId: DWORD;
    NameIndex: DWORD;  // 0 if unnamed
    Size: Word;        //  size in bytes of the object
    MaxSize: Byte;
    ParentIndex: DWORD;
  end;

const
  TID_VOID   = $00;       // Unknown or no type
  TID_LSTR   = $01;       // Basic Literal string
  TID_DSTR   = $02;       // Basic Dynamic string
  TID_PSTR   = $03;       // Pascal style string

procedure TJclTD32InfoParser.AnalyseGlobalTypes(const pTypes: Pointer; const Size: DWORD);
var
  pTyp: PSymbolTypeInfo;
begin
  pTyp := PSymbolTypeInfo(pTypes);
  repeat
    {case pTyp.TypeId of
      TID_VOID: ;
    end;}
    pTyp := PSymbolTypeInfo(DWORD(pTyp) + pTyp.Size + SizeOf(pTyp^));
  until DWORD(pTyp) >= DWORD(pTypes) + Size;
end;

procedure TJclTD32InfoParser.AnalyseAlignSymbols(pSymbols: PSymbolInfos; const Size: DWORD);
var
  Offset: DWORD;
  pInfo: PSymbolInfo;
  Symbol: TJclSymbolInfo;
begin
  Offset := DWORD(@pSymbols.Symbols[0]) - DWORD(pSymbols);
  while Offset < Size do
  begin
    pInfo := PSymbolInfo(DWORD(pSymbols) + Offset);
    case pInfo.SymbolType of
      SYMBOL_TYPE_LPROC32:
        begin
          Symbol := TJclLocalProcSymbolInfo.Create(pInfo);
          FProcSymbols.Add(Symbol);
        end;
      SYMBOL_TYPE_GPROC32:
        begin
          Symbol := TJclGlobalProcSymbolInfo.Create(pInfo);
          FProcSymbols.Add(Symbol);
        end;
      SYMBOL_TYPE_OBJNAME:
        Symbol := TJclObjNameSymbolInfo.Create(pInfo);
      SYMBOL_TYPE_LDATA32:
        Symbol := TJclLDataSymbolInfo.Create(pInfo);
      SYMBOL_TYPE_GDATA32:
        Symbol := TJclGDataSymbolInfo.Create(pInfo);
      SYMBOL_TYPE_PUB32:
        Symbol := TJclPublicSymbolInfo.Create(pInfo);
      SYMBOL_TYPE_WITH32:
        Symbol := TJclWithSymbolInfo.Create(pInfo);
      SYMBOL_TYPE_LABEL32:
        Symbol := TJclLabelSymbolInfo.Create(pInfo);
      SYMBOL_TYPE_CONST:
        Symbol := TJclConstantSymbolInfo.Create(pInfo);
      SYMBOL_TYPE_UDT:
        Symbol := TJclUdtSymbolInfo.Create(pInfo);
      SYMBOL_TYPE_VFTPATH32:
        Symbol := TJclVftPathSymbolInfo.Create(pInfo);
    else
      Symbol := nil;
    end;
    if Assigned(Symbol) then
      FSymbols.Add(Symbol);
    Inc(Offset, pInfo.Size + SizeOf(pInfo.Size));
  end;
end;

procedure TJclTD32InfoParser.AnalyseModules(pModInfo: PModuleInfo; const Size: DWORD);
begin
  FModules.Add(TJclModuleInfo.Create(pModInfo));
end;

procedure TJclTD32InfoParser.AnalyseSourceModules(pSrcModInfo: PSourceModuleInfo; const Size: DWORD);
var
  I: Integer;
  pSrcFile: PSourceFileEntry;
begin
  {$RANGECHECKS OFF}
  for I := 0 to pSrcModInfo.FileCount - 1 do
  begin
    pSrcFile := PSourceFileEntry(DWORD(pSrcModInfo) + pSrcModInfo.BaseSrcFiles[I]);
    if pSrcFile.NameIndex > 0 then
      FSourceModules.Add(TJclSourceModuleInfo.Create(pSrcFile, DWORD(pSrcModInfo)));
  end;
  {$IFDEF RANGECHECKS_ON}
  {$RANGECHECKS ON}
  {$ENDIF RANGECHECKS_ON}
end;

procedure TJclTD32InfoParser.AnalyseUnknownSubSection(const pSubsection: Pointer; const Size: DWORD);
begin
  // do nothing
end;

function TJclTD32InfoParser.GetModule(const Idx: Integer): TJclModuleInfo;
begin
  Result := TJclModuleInfo(FModules.Items[Idx]);
end;

function TJclTD32InfoParser.GetModuleCount: Integer;
begin
  Result := FModules.Count;
end;

function TJclTD32InfoParser.GetName(const Idx: Integer): string;
begin
  Result := PChar(FNames.Items[Idx]);
end;

function TJclTD32InfoParser.GetNameCount: Integer;
begin
  Result := FNames.Count;
end;

function TJclTD32InfoParser.GetSourceModule(const Idx: Integer): TJclSourceModuleInfo;
begin
  Result := TJclSourceModuleInfo(FSourceModules.Items[Idx]);
end;

function TJclTD32InfoParser.GetSourceModuleCount: Integer;
begin
  Result := FSourceModules.Count;
end;

function TJclTD32InfoParser.GetSymbol(const Idx: Integer): TJclSymbolInfo;
begin
  Result := TJclSymbolInfo(FSymbols.Items[Idx]);
end;

function TJclTD32InfoParser.GetSymbolCount: Integer;
begin
  Result := FSymbols.Count;
end;

function TJclTD32InfoParser.GetProcSymbol(const Idx: Integer): TJclProcSymbolInfo;
begin
  Result := TJclProcSymbolInfo(FProcSymbols.Items[Idx]);
end;

function TJclTD32InfoParser.GetProcSymbolCount: Integer;
begin
  Result := FProcSymbols.Count;
end;

function TJclTD32InfoParser.FindModule(const AAddr: DWORD;
  var AMod: TJclModuleInfo): Boolean;
var
  I, J: Integer;
begin
  if ValidData then
    for I := 0 to ModuleCount - 1 do
    with Modules[I] do
      for J := 0 to SegmentCount - 1 do
      begin
        if (AAddr >= FSegments[J].Offset) and (AAddr - FSegments[J].Offset <= Segment[J].Size) then
        begin
          Result := True;
          AMod := Modules[I];
          Exit;
        end;
      end;
  Result := False;
  AMod := nil;
end;

function TJclTD32InfoParser.FindSourceModule(const AAddr: DWORD;
  var ASrcMod: TJclSourceModuleInfo): Boolean;
var
  I, J: Integer;
begin
  if ValidData then
    for I := 0 to SourceModuleCount - 1 do
    with SourceModules[I] do
      for J := 0 to SegmentCount - 1 do
        with Segment[J] do
          if (StartOffset <= AAddr) and (AAddr < EndOffset) then
          begin
            Result := True;
            ASrcMod := SourceModules[I];
            Exit;
          end;
  ASrcMod := nil;
  Result := False;
end;

function TJclTD32InfoParser.FindProc(const AAddr: DWORD; var AProc: TJclProcSymbolInfo): Boolean;
var
  I: Integer;
begin
  if ValidData then
    for I := 0 to ProcSymbolCount - 1 do
    begin
      AProc := ProcSymbols[I];
      with AProc do
        if (Offset <= AAddr) and (AAddr < Offset + Size) then
        begin
          Result := True;
          Exit;
        end;
    end;
  AProc := nil;
  Result := False;
end;

class function TJclTD32InfoParser.IsTD32DebugInfoValid(
  const DebugData: Pointer; const DebugDataSize: LongWord): Boolean;
var
  Sign: TJclTD32FileSignature;
  EndOfDebugData: LongWord;
begin
  Assert(not IsBadReadPtr(DebugData, DebugDataSize));
  Result := False;
  EndOfDebugData := LongWord(DebugData) + DebugDataSize;
  if DebugDataSize > SizeOf(Sign) then
  begin
    Sign := PJclTD32FileSignature(EndOfDebugData - SizeOf(Sign))^;
    if IsTD32Sign(Sign) and (Sign.Offset <= DebugDataSize) then
    begin
      Sign := PJclTD32FileSignature(EndOfDebugData - Sign.Offset)^;
      Result := IsTD32Sign(Sign);
    end;
  end;
end;

class function TJclTD32InfoParser.IsTD32Sign(const Sign: TJclTD32FileSignature): Boolean;
begin
  Result := (Sign.Signature = Borland32BitSymbolFileSignatureForDelphi) or
    (Sign.Signature = Borland32BitSymbolFileSignatureForBCB);
end;

function TJclTD32InfoParser.LfaToVa(Lfa: DWORD): Pointer;
begin
  Result := Pointer(DWORD(FBase) + Lfa)
end;

//=== { TJclTD32InfoScanner } ================================================

function TJclTD32InfoScanner.LineNumberFromAddr(AAddr: DWORD): Integer;
var
  Dummy: Integer;
begin
  Result := LineNumberFromAddr(AAddr, Dummy);
end;

function TJclTD32InfoScanner.LineNumberFromAddr(AAddr: DWORD; var Offset: Integer): Integer;
var
  ASrcMod: TJclSourceModuleInfo;
  ALine: TJclLineInfo;
begin
  if FindSourceModule(AAddr, ASrcMod) and ASrcMod.FindLine(AAddr, ALine) then
  begin
    Result := ALine.LineNo;
    Offset := AAddr - ALine.Offset;
  end
  else
  begin
    Result := 0;
    Offset := 0;
  end;
end;

function TJclTD32InfoScanner.ModuleNameFromAddr(AAddr: DWORD): string;
var
  AMod: TJclModuleInfo;
begin
  if FindModule(AAddr, AMod) then
    Result := Names[AMod.NameIndex]
  else
    Result := '';
end;

function TJclTD32InfoScanner.ProcNameFromAddr(AAddr: DWORD): string;
var
  Dummy: Integer;
begin
  Result := ProcNameFromAddr(AAddr, Dummy);
end;

function TJclTD32InfoScanner.ProcNameFromAddr(AAddr: DWORD; var Offset: Integer): string;
var
  AProc: TJclProcSymbolInfo;

  function FormatProcName(const ProcName: string): string;
  var
    pchSecondAt, P: PChar;
  begin
    Result := ProcName;
    if (Length(ProcName) > 0) and (ProcName[1] = '@') then
    begin
      pchSecondAt := StrScan(PChar(Copy(ProcName, 2, Length(ProcName) - 1)), '@');
      if pchSecondAt <> nil then
      begin
        Inc(pchSecondAt);
        Result := pchSecondAt;
        P := PChar(Result);
        while P^ <> #0 do
        begin
          if (pchSecondAt^ = '@') and ((pchSecondAt - 1)^ <> '@') then
            P^ := '.';
          Inc(P);
          Inc(pchSecondAt);
        end;
      end;
    end;
  end;

begin
  if FindProc(AAddr, AProc) then
  begin
    Result := FormatProcName(Names[AProc.NameIndex]);
    Offset := AAddr - AProc.Offset;
  end
  else
  begin
    Result := '';
    Offset := 0;
  end;
end;

function TJclTD32InfoScanner.SourceNameFromAddr(AAddr: DWORD): string;
var
  ASrcMod: TJclSourceModuleInfo;
begin
  if FindSourceModule(AAddr, ASrcMod) then
    Result := Names[ASrcMod.NameIndex];
end;

//=== { TJclPeBorTD32Image } =================================================

procedure TJclPeBorTD32Image.AfterOpen;
begin
  inherited AfterOpen;
  CheckDebugData;
end;

procedure TJclPeBorTD32Image.CheckDebugData;
begin
  FIsTD32DebugPresent := IsDebugInfoInImage(FTD32DebugData);
  if not FIsTD32DebugPresent then
    FIsTD32DebugPresent := IsDebugInfoInTds(FTD32DebugData);
  if FIsTD32DebugPresent then
  begin
    FTD32Scanner := TJclTD32InfoScanner.Create(FTD32DebugData);
    if not FTD32Scanner.ValidData then
    begin
      ClearDebugData;
      if not NoExceptions then
        raise EJclError.CreateResFmt(@RsHasNotTD32Info, [FileName]);
    end;
  end;
end;

procedure TJclPeBorTD32Image.Clear;
begin
  ClearDebugData;
  inherited Clear;
end;

procedure TJclPeBorTD32Image.ClearDebugData;
begin
  FIsTD32DebugPresent := False;
  FreeAndNil(FTD32Scanner);
  FreeAndNil(FTD32DebugData);
end;

function TJclPeBorTD32Image.IsDebugInfoInImage(var DataStream: TCustomMemoryStream): Boolean;
var
  DebugDir: TImageDebugDirectory;
  BugDataStart: Pointer;
  DebugDataSize: Integer;
begin
  Result := False;
  DataStream := nil;
  if IsBorlandImage and (DebugList.Count = 1) then
  begin
    DebugDir := DebugList[0];
    if DebugDir._Type = IMAGE_DEBUG_TYPE_UNKNOWN then
    begin
      BugDataStart := RvaToVa(DebugDir.AddressOfRawData);
      DebugDataSize := DebugDir.SizeOfData;
      Result := TJclTD32InfoParser.IsTD32DebugInfoValid(BugDataStart, DebugDataSize);
      if Result then
        DataStream := TJclReferenceMemoryStream.Create(BugDataStart, DebugDataSize);
    end;
  end;
end;

function TJclPeBorTD32Image.IsDebugInfoInTds(var DataStream: TCustomMemoryStream): Boolean;
var
  TdsFileName: TFileName;
  TempStream: TCustomMemoryStream;
begin
  Result := False;
  DataStream := nil;
  TdsFileName := ChangeFileExt(FileName, TurboDebuggerSymbolExt);
  if FileExists(TdsFileName) then
  begin
    TempStream := TJclFileMappingStream.Create(TdsFileName, fmOpenRead or fmShareDenyNone);
    try
      Result := TJclTD32InfoParser.IsTD32DebugInfoValid(TempStream.Memory, TempStream.Size);
      if Result then
        DataStream := TempStream
      else
        TempStream.Free;
    except
      TempStream.Free;
      raise;
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
