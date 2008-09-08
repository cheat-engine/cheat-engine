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
{ The Original Code is JclSysInfo.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All rights reserved.  }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Alexander Radchenko                                                                            }
{   Andre Snepvangers (asnepvangers)                                                               }
{   Azret Botash                                                                                   }
{   Bryan Coutch                                                                                   }
{   Carl Clark                                                                                     }
{   Eric S. Fisher                                                                                 }
{   Florent Ouchet (outchy)                                                                        }
{   James Azarja                                                                                   }
{   Jean-Fabien Connault (cycocrew)                                                                }
{   John C Molyneux                                                                                }
{   Marcel van Brakel                                                                              }
{   Matthias Thoma (mthoma)                                                                        }
{   Mike Lischke                                                                                   }
{   Nick Hodges                                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Peter Friese                                                                                   }
{   Peter Thornquist (peter3)                                                                      }
{   Petr Vones (pvones)                                                                            }
{   Rik Barker                                                                                     }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Scott Price                                                                                    }
{   Tom Hahn (tomhahn)                                                                             }
{   Wim de Cleen                                                                                   }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains routines and classes to retrieve various pieces of system information.        }
{ Examples are the location of standard folders, settings of environment variables, processor      }
{ details and the Windows version.                                                                 }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-06-16 08:49:55 +0200 (lun., 16 juin 2008)                          $ }
{ Revision:      $Rev:: 2386                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

// Windows NT 4 and earlier do not support GetSystemPowerStatus (while introduced
// in NT4 - it is a stub there - implemented in Windows 2000 and later.


unit JclSysInfo;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_TYPES}
  Types,
  {$ENDIF HAS_UNIT_TYPES}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  {$IFDEF CLR}
  System.IO, System.Configuration, System.Diagnostics, System.Collections,
  System.Net, System.ComponentModel,
  {$ELSE ~CLR}
  {$IFDEF MSWINDOWS}
  Windows, ActiveX,
  ShlObj,
  {$ENDIF MSWINDOWS}
  {$ENDIF ~CLR}
  Classes,
  JclResources;

// Environment Variables
{$IFDEF MSWINDOWS}
type
  TEnvironmentOption = (eoLocalMachine, eoCurrentUser, eoAdditional);
  TEnvironmentOptions = set of TEnvironmentOption;
{$ENDIF MSWINDOWS}
{$IFDEF CLR}
type
  DWORD = LongWord;
{$ENDIF CLR}

function DelEnvironmentVar(const Name: string): Boolean;
function ExpandEnvironmentVar(var Value: string): Boolean;
function GetEnvironmentVar(const Name: string; var Value: string): Boolean; overload;
function GetEnvironmentVar(const Name: string; var Value: string; Expand: Boolean): Boolean; overload;
function GetEnvironmentVars(const Vars: TStrings): Boolean; overload;
function GetEnvironmentVars(const Vars: TStrings; Expand: Boolean): Boolean; overload;
function SetEnvironmentVar(const Name, Value: string): Boolean;
{$IFNDEF CLR}
{$IFDEF MSWINDOWS}
function CreateEnvironmentBlock(const Options: TEnvironmentOptions; const AdditionalVars: TStrings): PChar;
procedure DestroyEnvironmentBlock(var Env: PChar);
procedure SetGlobalEnvironmentVariable(VariableName, VariableContent: string);
{$ENDIF MSWINDOWS}
{$ENDIF ~CLR}

// Common Folder Locations
{$IFNDEF CLR}
{$IFDEF MSWINDOWS}
function GetCommonFilesFolder: string;
{$ENDIF MSWINDOWS}
{$ENDIF ~CLR}
function GetCurrentFolder: string;
{$IFDEF MSWINDOWS}
function GetProgramFilesFolder: string;
{$IFNDEF CLR}
function GetWindowsFolder: string;
{$ENDIF ~CLR}
function GetWindowsSystemFolder: string;
function GetWindowsTempFolder: string;

function GetDesktopFolder: string;
function GetProgramsFolder: string;
{$ENDIF MSWINDOWS}
function GetPersonalFolder: string;
{$IFDEF MSWINDOWS}
function GetFavoritesFolder: string;
function GetStartupFolder: string;
function GetRecentFolder: string;
function GetSendToFolder: string;
function GetStartmenuFolder: string;
function GetDesktopDirectoryFolder: string;
{$IFNDEF CLR}
{$IFNDEF FPC}
function GetCommonDocumentsFolder: string;
{$ENDIF ~FPC}
function GetNethoodFolder: string;
function GetFontsFolder: string;
function GetCommonStartmenuFolder: string;
function GetCommonStartupFolder: string;
function GetPrinthoodFolder: string;
function GetProfileFolder: string;
{$ENDIF ~CLR}
function GetCommonProgramsFolder: string;
function GetCommonDesktopdirectoryFolder: string;
function GetCommonAppdataFolder: string;
function GetAppdataFolder: string;
function GetCommonFavoritesFolder: string;
function GetTemplatesFolder: string;
function GetInternetCacheFolder: string;
function GetCookiesFolder: string;
function GetHistoryFolder: string;

{$IFNDEF CLR}
// Advanced Power Management (APM)
type
  TAPMLineStatus = (alsOffline, alsOnline, alsUnknown);
  TAPMBatteryFlag = (abfHigh, abfLow, abfCritical, abfCharging, abfNoBattery, abfUnknown);
  TAPMBatteryFlags = set of TAPMBatteryFlag;

function GetAPMLineStatus: TAPMLineStatus;
function GetAPMBatteryFlag: TAPMBatteryFlag;
function GetAPMBatteryFlags: TAPMBatteryFlags;
function GetAPMBatteryLifePercent: Integer;
function GetAPMBatteryLifeTime: DWORD;
function GetAPMBatteryFullLifeTime: DWORD;

// Identification
type
  TFileSystemFlag =
   (
    fsCaseSensitive,            // The file system supports case-sensitive file names.
    fsCasePreservedNames,       // The file system preserves the case of file names when it places a name on disk.
    fsSupportsUnicodeOnDisk,    // The file system supports Unicode in file names as they appear on disk.
    fsPersistentACLs,           // The file system preserves and enforces ACLs. For example, NTFS preserves and enforces ACLs, and FAT does not.
    fsSupportsFileCompression,  // The file system supports file-based compression.
    fsSupportsVolumeQuotas,     // The file system supports disk quotas.
    fsSupportsSparseFiles,      // The file system supports sparse files.
    fsSupportsReparsePoints,    // The file system supports reparse points.
    fsSupportsRemoteStorage,    // ?
    fsVolumeIsCompressed,       // The specified volume is a compressed volume; for example, a DoubleSpace volume.
    fsSupportsObjectIds,        // The file system supports object identifiers.
    fsSupportsEncryption,       // The file system supports the Encrypted File System (EFS).
    fsSupportsNamedStreams,     // The file system supports named streams.
    fsVolumeIsReadOnly          // The specified volume is read-only.
                                //   Windows 2000/NT and Windows Me/98/95:  This value is not supported.
   );

  TFileSystemFlags = set of TFileSystemFlag;

function GetVolumeName(const Drive: string): string;
function GetVolumeSerialNumber(const Drive: string): string;
function GetVolumeFileSystem(const Drive: string): string;
function GetVolumeFileSystemFlags(const Volume: string): TFileSystemFlags;
{$ENDIF ~CLR}
{$ENDIF MSWINDOWS}
function GetIPAddress(const HostName: string): string;
{$IFNDEF CLR}
procedure GetIpAddresses(Results: TStrings);
{$ENDIF ~CLR}
function GetLocalComputerName: string;
{$IFNDEF CLR}
function GetLocalUserName: string;
{$IFDEF MSWINDOWS}
function GetUserDomainName(const CurUser: string): string;
{$ENDIF MSWINDOWS}
function GetDomainName: string;
{$IFDEF MSWINDOWS}
function GetRegisteredCompany: string;
function GetRegisteredOwner: string;
function GetBIOSName: string;
function GetBIOSCopyright: string;
function GetBIOSExtendedInfo: string;
function GetBIOSDate: TDateTime;
{$ENDIF MSWINDOWS}

// Processes, Tasks and Modules
type
  TJclTerminateAppResult = (taError, taClean, taKill);
{$ENDIF ~CLR}

function RunningProcessesList(const List: TStrings; FullPath: Boolean = True): Boolean;

{$IFDEF MSWINDOWS}
{$IFNDEF CLR}
function LoadedModulesList(const List: TStrings; ProcessID: DWORD; HandlesOnly: Boolean = False): Boolean;
function GetTasksList(const List: TStrings): Boolean;

function ModuleFromAddr(const Addr: Pointer): HMODULE;
{$IFNDEF FPC}
function IsSystemModule(const Module: HMODULE): Boolean;
{$ENDIF ~FPC}

function IsMainAppWindow(Wnd: THandle): Boolean;
function IsWindowResponding(Wnd: THandle; Timeout: Integer): Boolean;

function GetWindowIcon(Wnd: THandle; LargeIcon: Boolean): HICON;
function GetWindowCaption(Wnd: THandle): string;
function TerminateTask(Wnd: THandle; Timeout: Integer): TJclTerminateAppResult;
function TerminateApp(ProcessID: DWORD; Timeout: Integer): TJclTerminateAppResult;
{$ENDIF ~CLR}
{$ENDIF MSWINDOWS}

{$IFNDEF CLR}
{$IFDEF MSWINDOWS}
{.$IFNDEF FPC}
function GetPidFromProcessName(const ProcessName: string): DWORD;
function GetProcessNameFromWnd(Wnd: THandle): string;
function GetProcessNameFromPid(PID: DWORD): string;
function GetMainAppWndFromPid(PID: DWORD): THandle;
{.$ENDIF ~FPC}

function GetShellProcessName: string;
{.$IFNDEF FPC}
function GetShellProcessHandle: THandle;
{.$ENDIF ~FPC}

// Version Information
type
  TWindowsVersion =
   (wvUnknown, wvWin95, wvWin95OSR2, wvWin98, wvWin98SE, wvWinME,
    wvWinNT31, wvWinNT35, wvWinNT351, wvWinNT4, wvWin2000, wvWinXP,
    wvWin2003, wvWinXP64, wvWin2003R2, wvWinVista, wvWinServer2008);
  TNtProductType =
   (ptUnknown, ptWorkStation, ptServer, ptAdvancedServer,
    ptPersonal, ptProfessional, ptDatacenterServer, ptEnterprise, ptWebEdition);
  TProcessorArchitecture =
   (paUnknown, // unknown processor
    pax8632,   // x86 32 bit processors (some P4, Celeron, Athlon and older)
    pax8664,   // x86 64 bit processors (latest P4, Celeron and Athlon64)
    paIA64);   // Itanium processors

var
  { in case of additions, don't forget to update initialization section! }
  IsWin95: Boolean = False;
  IsWin95OSR2: Boolean = False;
  IsWin98: Boolean = False;
  IsWin98SE: Boolean = False;
  IsWinME: Boolean = False;
  IsWinNT: Boolean = False;
  IsWinNT3: Boolean = False;
  IsWinNT31: Boolean = False;
  IsWinNT35: Boolean = False;
  IsWinNT351: Boolean = False;
  IsWinNT4: Boolean = False;
  IsWin2K: Boolean = False;
  IsWinXP: Boolean = False;
  IsWin2003: Boolean = False;
  IsWinXP64: Boolean = False;
  IsWin2003R2: Boolean = False;
  IsWinVista: Boolean = False;
  IsWinServer2008: Boolean = False;

const
  PROCESSOR_ARCHITECTURE_INTEL = 0;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_INTEL}
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_AMD64}
  PROCESSOR_ARCHITECTURE_IA32_ON_WIN64 = 10;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_IA32_ON_WIN64}
  PROCESSOR_ARCHITECTURE_IA64 = 6;
  {$EXTERNALSYM PROCESSOR_ARCHITECTURE_IA64}

function GetWindowsVersion: TWindowsVersion;
function NtProductType: TNtProductType;
function GetWindowsVersionString: string;
function NtProductTypeString: string;
function GetWindowsServicePackVersion: Integer;
function GetWindowsServicePackVersionString: string;
function GetOpenGLVersion(const Win: THandle; out Version, Vendor: AnsiString): Boolean;
function GetNativeSystemInfo(var SystemInfo: TSystemInfo): Boolean;
function GetProcessorArchitecture: TProcessorArchitecture;
function IsWindows64: Boolean;
{$ENDIF MSWINDOWS}

function GetOSVersionString: string;

// Hardware
{$IFDEF MSWINDOWS}
function GetMacAddresses(const Machine: string; const Addresses: TStrings): Integer;
{$ENDIF MSWINDOWS}
function ReadTimeStampCounter: Int64;

type
  TTLBInformation = (tiEntries, tiAssociativity);
  TCacheInformation = (ciLineSize {in Bytes}, ciLinesPerTag, ciAssociativity, ciSize);

  TIntelSpecific = record
    L2Cache: Cardinal;
    CacheDescriptors: array [0..15] of Byte;
    BrandID: Byte;
    FlushLineSize: Byte;
    APICID: Byte;
    ExFeatures: Cardinal;
    Ex64Features: Cardinal;
    Ex64Features2: Cardinal;
    PhysicalAddressBits: Byte;
    VirtualAddressBits: Byte;
  end;

  TCyrixSpecific = record
    L1CacheInfo: array [0..3] of Byte;
    TLBInfo: array [0..3] of Byte;
  end;

  TAMDSpecific = packed record
    ExFeatures: Cardinal;
    ExFeatures2: Cardinal;
    Features2: Cardinal;
    BrandID: Byte;
    FlushLineSize: Byte;
    APICID: Byte;
    ExBrandID: Word;
    // do not split L1 MByte TLB
    L1MByteInstructionTLB: array [TTLBInformation] of Byte;
    L1MByteDataTLB: array [TTLBInformation] of Byte;
    // do not split L1 KByte TLB
    L1KByteInstructionTLB: array [TTLBInformation] of Byte;
    L1KByteDataTLB: array [TTLBInformation] of Byte;
    L1DataCache: array [TCacheInformation] of Byte;
    L1InstructionCache: array [TCacheInformation] of Byte;
    // do not split L2 MByte TLB
    L2MByteInstructionTLB: array [TTLBInformation] of Byte;    // L2 TLB for 2-MByte and 4-MByte pages
    L2MByteDataTLB: array [TTLBInformation] of Byte;           // L2 TLB for 2-MByte and 4-MByte pages
    // do not split L2 KByte TLB
    L2KByteDataTLB: array [TTLBInformation] of Byte;           // L2 TLB for 4-KByte pages
    L2KByteInstructionTLB: array [TTLBInformation] of Byte;    // L2 TLB for 4-KByte pages
    L2Cache: Cardinal;
    L3Cache: Cardinal;
    AdvancedPowerManagement: Cardinal;
    PhysicalAddressSize: Byte;
    VirtualAddressSize: Byte;
  end;

  TVIASpecific = record
    ExFeatures: Cardinal;
    DataTLB: array [TTLBInformation] of Byte;
    InstructionTLB: array [TTLBInformation] of Byte;
    L1DataCache: array [TCacheInformation] of Byte;
    L1InstructionCache: array [TCacheInformation] of Byte;
    L2DataCache: Cardinal;
  end;

  TTransmetaSpecific = record
    ExFeatures: Cardinal;
    DataTLB: array [TTLBInformation] of Byte;
    CodeTLB: array [TTLBInformation] of Byte;
    L1DataCache: array [TCacheInformation] of Byte;
    L1CodeCache: array [TCacheInformation] of Byte;
    L2Cache: Cardinal;
    RevisionABCD: Cardinal;
    RevisionXXXX: Cardinal;
    Frequency: Cardinal;
    CodeMorphingABCD: Cardinal;
    CodeMorphingXXXX: Cardinal;
    TransmetaFeatures: Cardinal;
    TransmetaInformations: array [0..64] of Char;
    CurrentVoltage: Cardinal;
    CurrentFrequency: Cardinal;
    CurrentPerformance: Cardinal;
  end;

  TCacheFamily = (
    cfInstructionTLB, cfDataTLB,
    cfL1InstructionCache, cfL1DataCache,
    cfL2Cache, cfL3Cache, cfTrace, cfOther);

  TCacheInfo = record
    D: Byte;
    Family: TCacheFamily;
    Size: Cardinal;
    WaysOfAssoc: Byte;
    LineSize: Byte;       // for Normal Cache
    LinePerSector: Byte;  // for L3 Normal Cache
    Entries: Cardinal;        // for TLB
    I: string;
  end;

  TFreqInfo = record
    RawFreq: Cardinal;
    NormFreq: Cardinal;
    InCycles: Cardinal;
    ExTicks: Cardinal;
  end;

const
  CPU_TYPE_INTEL     = 1;
  CPU_TYPE_CYRIX     = 2;
  CPU_TYPE_AMD       = 3;
  CPU_TYPE_TRANSMETA = 4;
  CPU_TYPE_VIA       = 5;

type
  TSSESupport = (sse, sse2, sse3, ssse3, sse4A, sse4B, sse5);
  TSSESupports = set of TSSESupport;

  TCpuInfo = record
    HasInstruction: Boolean;
    MMX: Boolean;
    ExMMX: Boolean;
    _3DNow: Boolean;
    Ex3DNow: Boolean;
    SSE: TSSESupports;
    IsFDIVOK: Boolean;
    Is64Bits: Boolean;
    DEPCapable: Boolean;
    HasCacheInfo: Boolean;
    HasExtendedInfo: Boolean;
    PType: Byte;
    Family: Byte;
    ExtendedFamily: Byte;
    Model: Byte;
    ExtendedModel: Byte;
    Stepping: Byte;
    Features: Cardinal;
    FrequencyInfo: TFreqInfo;
    VendorIDString: array [0..11] of Char;
    Manufacturer: array [0..9] of Char;
    CpuName: array [0..47] of Char;
    L1DataCacheSize: Cardinal;             // in kByte
    L1DataCacheLineSize: Byte;             // in Byte
    L1DataCacheAssociativity: Byte;
    L1InstructionCacheSize: Cardinal;      // in kByte
    L1InstructionCacheLineSize: Byte;      // in Byte
    L1InstructionCacheAssociativity: Byte;
    L2CacheSize: Cardinal;                 // in kByte
    L2CacheLineSize: Byte;                 // in Byte
    L2CacheAssociativity: Byte;
    L3CacheSize: Cardinal;                 // in kByte
    L3CacheLineSize: Byte;                 // in Byte
    L3CacheAssociativity: Byte;
    L3LinesPerSector: Byte;
    LogicalCore: Byte;
    PhysicalCore: Byte;
    HyperThreadingTechnology: Boolean;
    // todo: TLB
    case CpuType: Byte of
      CPU_TYPE_INTEL: (IntelSpecific: TIntelSpecific;);
      CPU_TYPE_CYRIX: (CyrixSpecific: TCyrixSpecific;);
      CPU_TYPE_AMD: (AMDSpecific: TAMDSpecific;);
      CPU_TYPE_TRANSMETA: (TransmetaSpecific: TTransmetaSpecific;);
      CPU_TYPE_VIA: (ViaSpecific: TViaSpecific;);
  end;

const
  VendorIDIntel: array [0..11] of Char = 'GenuineIntel';
  VendorIDCyrix: array [0..11] of Char = 'CyrixInstead';
  VendorIDAMD: array [0..11] of Char = 'AuthenticAMD';
  VendorIDTransmeta: array [0..11] of Char = 'GenuineTMx86';
  VendorIDVIA: array [0..11] of Char = 'CentaurHauls';

// Constants to be used with Feature Flag set of a CPU
// eg. IF (Features and FPU_FLAG = FPU_FLAG) THEN CPU has Floating-Point unit on
// chip. However, Intel claims that in future models, a zero in the feature
// flags will mean that the chip has that feature, however, the following flags
// will work for any production 80x86 chip or clone.
// eg. IF (Features and FPU_FLAG = 0) then CPU has Floating-Point unit on chip.

const
  { 32 bits in a DWord Value }
  BIT_0       = $00000001;
  BIT_1       = $00000002;
  BIT_2       = $00000004;
  BIT_3       = $00000008;
  BIT_4       = $00000010;
  BIT_5       = $00000020;
  BIT_6       = $00000040;
  BIT_7       = $00000080;
  BIT_8       = $00000100;
  BIT_9       = $00000200;
  BIT_10      = $00000400;
  BIT_11      = $00000800;
  BIT_12      = $00001000;
  BIT_13      = $00002000;
  BIT_14      = $00004000;
  BIT_15      = $00008000;
  BIT_16      = $00010000;
  BIT_17      = $00020000;
  BIT_18      = $00040000;
  BIT_19      = $00080000;
  BIT_20      = $00100000;
  BIT_21      = $00200000;
  BIT_22      = $00400000;
  BIT_23      = $00800000;
  BIT_24      = $01000000;
  BIT_25      = $02000000;
  BIT_26      = $04000000;
  BIT_27      = $08000000;
  BIT_28      = $10000000;
  BIT_29      = $20000000;
  BIT_30      = $40000000;
  BIT_31      = DWORD($80000000);

  { Standard Feature Flags }
  FPU_FLAG    = BIT_0;  // Floating-Point unit on chip
  VME_FLAG    = BIT_1;  // Virtual Mode Extention
  DE_FLAG     = BIT_2;  // Debugging Extention
  PSE_FLAG    = BIT_3;  // Page Size Extention
  TSC_FLAG    = BIT_4;  // Time Stamp Counter
  MSR_FLAG    = BIT_5;  // Model Specific Registers
  PAE_FLAG    = BIT_6;  // Physical Address Extention
  MCE_FLAG    = BIT_7;  // Machine Check Exception
  CX8_FLAG    = BIT_8;  // CMPXCHG8 Instruction
  APIC_FLAG   = BIT_9;  // Software-accessible local APIC on Chip
  BIT_10_FLAG = BIT_10; // Reserved, do not count on value
  SEP_FLAG    = BIT_11; // Fast System Call
  MTRR_FLAG   = BIT_12; // Memory Type Range Registers
  PGE_FLAG    = BIT_13; // Page Global Enable
  MCA_FLAG    = BIT_14; // Machine Check Architecture
  CMOV_FLAG   = BIT_15; // Conditional Move Instruction
  PAT_FLAG    = BIT_16; // Page Attribute Table
  PSE36_FLAG  = BIT_17; // 36-bit Page Size Extention
  PSN_FLAG    = BIT_18; // Processor serial number is present and enabled
  CLFLSH_FLAG = BIT_19; // CLFLUSH intruction
  BIT_20_FLAG = BIT_20; // Reserved, do not count on value
  DS_FLAG     = BIT_21; // Debug store
  ACPI_FLAG   = BIT_22; // Thermal monitor and clock control
  MMX_FLAG    = BIT_23; // MMX technology
  FXSR_FLAG   = BIT_24; // Fast Floating Point Save and Restore
  SSE_FLAG    = BIT_25; // Streaming SIMD Extensions
  SSE2_FLAG   = BIT_26; // Streaming SIMD Extensions 2
  SS_FLAG     = BIT_27; // Self snoop
  HTT_FLAG    = BIT_28; // Hyper-threading technology
  TM_FLAG     = BIT_29; // Thermal monitor
  BIT_30_FLAG = BIT_30; // Reserved, do not count on value
  PBE_FLAG    = BIT_31; // Pending Break Enable

  { Standard Intel Feature Flags }
  INTEL_FPU    = BIT_0;  // Floating-Point unit on chip
  INTEL_VME    = BIT_1;  // Virtual Mode Extention
  INTEL_DE     = BIT_2;  // Debugging Extention
  INTEL_PSE    = BIT_3;  // Page Size Extention
  INTEL_TSC    = BIT_4;  // Time Stamp Counter
  INTEL_MSR    = BIT_5;  // Model Specific Registers
  INTEL_PAE    = BIT_6;  // Physical Address Extention
  INTEL_MCE    = BIT_7;  // Machine Check Exception
  INTEL_CX8    = BIT_8;  // CMPXCHG8 Instruction
  INTEL_APIC   = BIT_9;  // Software-accessible local APIC on Chip
  INTEL_BIT_10 = BIT_10; // Reserved, do not count on value
  INTEL_SEP    = BIT_11; // Fast System Call
  INTEL_MTRR   = BIT_12; // Memory Type Range Registers
  INTEL_PGE    = BIT_13; // Page Global Enable
  INTEL_MCA    = BIT_14; // Machine Check Architecture
  INTEL_CMOV   = BIT_15; // Conditional Move Instruction
  INTEL_PAT    = BIT_16; // Page Attribute Table
  INTEL_PSE36  = BIT_17; // 36-bit Page Size Extention
  INTEL_PSN    = BIT_18; // Processor serial number is present and enabled
  INTEL_CLFLSH = BIT_19; // CLFLUSH intruction
  INTEL_BIT_20 = BIT_20; // Reserved, do not count on value
  INTEL_DS     = BIT_21; // Debug store
  INTEL_ACPI   = BIT_22; // Thermal monitor and clock control
  INTEL_MMX    = BIT_23; // MMX technology
  INTEL_FXSR   = BIT_24; // Fast Floating Point Save and Restore
  INTEL_SSE    = BIT_25; // Streaming SIMD Extensions
  INTEL_SSE2   = BIT_26; // Streaming SIMD Extensions 2
  INTEL_SS     = BIT_27; // Self snoop
  INTEL_HTT    = BIT_28; // Hyper-threading technology
  INTEL_TM     = BIT_29; // Thermal monitor
  INTEL_BIT_30 = BIT_30; // Reserved, do not count on value
  INTEL_PBE    = BIT_31; // Pending Break Enable

  { Extended Intel Feature Flags }
  EINTEL_SSE3    = BIT_0;  // Streaming SIMD Extensions 3
  EINTEL_BIT_1   = BIT_1;  // Reserved, do not count on value
  EINTEL_BIT_2   = BIT_2;  // Reserved, do not count on value
  EINTEL_MONITOR = BIT_3;  // Monitor/MWAIT
  EINTEL_DSCPL   = BIT_4;  // CPL Qualified debug Store
  EINTEL_VMX     = BIT_5;  // Virtual Machine Technology
  EINTEL_SMX     = BIT_6;  // Safer Mode Extensions
  EINTEL_EST     = BIT_7;  // Enhanced Intel Speedstep technology
  EINTEL_TM2     = BIT_8;  // Thermal monitor 2
  EINTEL_SSSE3   = BIT_9;  // SSSE 3 extensions
  EINTEL_CNXTID  = BIT_10; // L1 Context ID
  EINTEL_BIT_11  = BIT_11; // Reserved, do not count on value
  EINTEL_BIT_12  = BIT_12; // Reserved, do not count on value
  EINTEL_CMPXCHG16B = BIT_13; // CMPXCHG16B instruction
  EINTEL_XTPR    = BIT_14; // Send Task Priority messages
  EINTEL_PDCM    = BIT_15; // Perf/Debug Capability MSR
  EINTEL_BIT_16  = BIT_16; // Reserved, do not count on value
  EINTEL_BIT_17  = BIT_17; // Reserved, do not count on value
  EINTEL_DCA     = BIT_18; // Direct Cache Access
  EINTEL_SSE4_1  = BIT_19; // Streaming SIMD Extensions 4.1
  EINTEL_SSE4_2  = BIT_20; // Streaming SIMD Extensions 4.2
  EINTEL_BIT_21  = BIT_21; // Reserved, do not count on value
  EINTEL_BIT_22  = BIT_22; // Reserved, do not count on value
  EINTEL_POPCNT  = BIT_23; // A value of 1 indicates the processor supports the POPCNT instruction.
  EINTEL_BIT_24  = BIT_24; // Reserved, do not count on value
  EINTEL_BIT_25  = BIT_25; // Reserved, do not count on value
  EINTEL_BIT_26  = BIT_26; // Reserved, do not count on value
  EINTEL_BIT_27  = BIT_27; // Reserved, do not count on value
  EINTEL_BIT_28  = BIT_28; // Reserved, do not count on value
  EINTEL_BIT_29  = BIT_29; // Reserved, do not count on value
  EINTEL_BIT_30  = BIT_30; // Reserved, do not count on value
  EINTEL_BIT_31  = BIT_31; // Reserved, do not count on value

  { Extended Intel 64 Bits Feature Flags }
  EINTEL64_BIT_0  = BIT_0;  // Reserved, do not count on value
  EINTEL64_BIT_1  = BIT_1;  // Reserved, do not count on value
  EINTEL64_BIT_2  = BIT_2;  // Reserved, do not count on value
  EINTEL64_BIT_3  = BIT_3;  // Reserved, do not count on value
  EINTEL64_BIT_4  = BIT_4;  // Reserved, do not count on value
  EINTEL64_BIT_5  = BIT_5;  // Reserved, do not count on value
  EINTEL64_BIT_6  = BIT_6;  // Reserved, do not count on value
  EINTEL64_BIT_7  = BIT_7;  // Reserved, do not count on value
  EINTEL64_BIT_8  = BIT_8;  // Reserved, do not count on value
  EINTEL64_BIT_9  = BIT_9;  // Reserved, do not count on value
  EINTEL64_BIT_10 = BIT_10; // Reserved, do not count on value
  EINTEL64_SYS    = BIT_11; // 64 Bit - SYSCALL SYSRET
  EINTEL64_BIT_12 = BIT_12; // Reserved, do not count on value
  EINTEL64_BIT_13 = BIT_13; // Reserved, do not count on value
  EINTEL64_BIT_14 = BIT_14; // Reserved, do not count on value
  EINTEL64_BIT_15 = BIT_15; // Reserved, do not count on value
  EINTEL64_BIT_16 = BIT_16; // Reserved, do not count on value
  EINTEL64_BIT_17 = BIT_17; // Reserved, do not count on value
  EINTEL64_BIT_18 = BIT_18; // Reserved, do not count on value
  EINTEL64_BIT_19 = BIT_19; // Reserved, do not count on value
  EINTEL64_EDB    = BIT_20; // Execute Disable Bit
  EINTEL64_BIT_21 = BIT_21; // Reserved, do not count on value
  EINTEL64_BIT_22 = BIT_22; // Reserved, do not count on value
  EINTEL64_BIT_23 = BIT_23; // Reserved, do not count on value
  EINTEL64_BIT_24 = BIT_24; // Reserved, do not count on value
  EINTEL64_BIT_25 = BIT_25; // Reserved, do not count on value
  EINTEL64_BIT_26 = BIT_26; // Reserved, do not count on value
  EINTEL64_BIT_27 = BIT_27; // Reserved, do not count on value
  EINTEL64_BIT_28 = BIT_28; // Reserved, do not count on value
  EINTEL64_EM64T  = BIT_29; // Intel Extended Memory 64 Technology
  EINTEL64_BIT_30 = BIT_30; // Reserved, do not count on value
  EINTEL64_BIT_31 = BIT_31; // Reserved, do not count on value

  { Extended Intel 64 Bits Feature Flags continued }
  EINTEL64_2_LAHF   = BIT_0;  // LAHF/SAHF available in 64 bit mode
  EINTEL64_2_BIT_1  = BIT_1;  // Reserved, do not count on value
  EINTEL64_2_BIT_2  = BIT_2;  // Reserved, do not count on value
  EINTEL64_2_BIT_3  = BIT_3;  // Reserved, do not count on value
  EINTEL64_2_BIT_4  = BIT_4;  // Reserved, do not count on value
  EINTEL64_2_BIT_5  = BIT_5;  // Reserved, do not count on value
  EINTEL64_2_BIT_6  = BIT_6;  // Reserved, do not count on value
  EINTEL64_2_BIT_7  = BIT_7;  // Reserved, do not count on value
  EINTEL64_2_BIT_8  = BIT_8;  // Reserved, do not count on value
  EINTEL64_2_BIT_9  = BIT_9;  // Reserved, do not count on value
  EINTEL64_2_BIT_10 = BIT_10; // Reserved, do not count on value
  EINTEL64_2_BIT_11 = BIT_11; // Reserved, do not count on value
  EINTEL64_2_BIT_12 = BIT_12; // Reserved, do not count on value
  EINTEL64_2_BIT_13 = BIT_13; // Reserved, do not count on value
  EINTEL64_2_BIT_14 = BIT_14; // Reserved, do not count on value
  EINTEL64_2_BIT_15 = BIT_15; // Reserved, do not count on value
  EINTEL64_2_BIT_16 = BIT_16; // Reserved, do not count on value
  EINTEL64_2_BIT_17 = BIT_17; // Reserved, do not count on value
  EINTEL64_2_BIT_18 = BIT_18; // Reserved, do not count on value
  EINTEL64_2_BIT_19 = BIT_19; // Reserved, do not count on value
  EINTEL64_2_BIT_20 = BIT_20; // Reserved, do not count on value
  EINTEL64_2_BIT_21 = BIT_21; // Reserved, do not count on value
  EINTEL64_2_BIT_22 = BIT_22; // Reserved, do not count on value
  EINTEL64_2_BIT_23 = BIT_23; // Reserved, do not count on value
  EINTEL64_2_BIT_24 = BIT_24; // Reserved, do not count on value
  EINTEL64_2_BIT_25 = BIT_25; // Reserved, do not count on value
  EINTEL64_2_BIT_26 = BIT_26; // Reserved, do not count on value
  EINTEL64_2_BIT_27 = BIT_27; // Reserved, do not count on value
  EINTEL64_2_BIT_28 = BIT_28; // Reserved, do not count on value
  EINTEL64_2_BIT_29 = BIT_29; // Reserved, do not count on value
  EINTEL64_2_BIT_30 = BIT_30; // Reserved, do not count on value
  EINTEL64_2_BIT_31 = BIT_31; // Reserved, do not count on value

  { AMD Standard Feature Flags }
  AMD_FPU     = BIT_0;  // Floating-Point unit on chip
  AMD_VME     = BIT_1;  // Virtual Mode Extention
  AMD_DE      = BIT_2;  // Debugging Extention
  AMD_PSE     = BIT_3;  // Page Size Extention
  AMD_TSC     = BIT_4;  // Time Stamp Counter
  AMD_MSR     = BIT_5;  // Model Specific Registers
  AMD_PAE     = BIT_6;  // Physical address Extensions
  AMD_MCE     = BIT_7;  // Machine Check Exception
  AMD_CX8     = BIT_8;  // CMPXCHG8 Instruction
  AMD_APIC    = BIT_9;  // Software-accessible local APIC on Chip
  AMD_BIT_10  = BIT_10; // Reserved, do not count on value
  AMD_SEP_BIT = BIT_11; // SYSENTER and SYSEXIT instructions
  AMD_MTRR    = BIT_12; // Memory Type Range Registers
  AMD_PGE     = BIT_13; // Page Global Enable
  AMD_MCA     = BIT_14; // Machine Check Architecture
  AMD_CMOV    = BIT_15; // Conditional Move Instruction
  AMD_PAT     = BIT_16; // Page Attribute Table
  AMD_PSE32   = BIT_17; // Page Size Extensions
  AMD_BIT_18  = BIT_18; // Reserved, do not count on value
  AMD_CLFLSH  = BIT_19; // CLFLUSH instruction
  AMD_BIT_20  = BIT_20; // Reserved, do not count on value
  AMD_BIT_21  = BIT_21; // Reserved, do not count on value
  AMD_BIT_22  = BIT_22; // Reserved, do not count on value
  AMD_MMX     = BIT_23; // MMX technology
  AMD_FXSR    = BIT_24; // FXSAVE and FXSTORE instructions
  AMD_SSE     = BIT_25; // SSE Extensions
  AMD_SSE2    = BIT_26; // SSE2 Extensions
  AMD_BIT_27  = BIT_27; // Reserved, do not count on value
  AMD_HTT     = BIT_28; // Hyper-Threading Technology
  AMD_BIT_29  = BIT_29; // Reserved, do not count on value
  AMD_BIT_30  = BIT_30; // Reserved, do not count on value
  AMD_BIT_31  = BIT_31; // Reserved, do not count on value

  { AMD Standard Feature Flags continued }
  AMD2_SSE3       = BIT_0;  // SSE3 extensions
  AMD2_BIT_1      = BIT_1;  // Reserved, do not count on value
  AMD2_BIT_2      = BIT_2;  // Reserved, do not count on value
  AMD2_MONITOR    = BIT_3;  // MONITOR/MWAIT instructions. See "MONITOR" and "MWAIT" in APM3.
  AMD2_BIT_4      = BIT_4;  // Reserved, do not count on value
  AMD2_BIT_5      = BIT_5;  // Reserved, do not count on value
  AMD2_BIT_6      = BIT_6;  // Reserved, do not count on value
  AMD2_BIT_7      = BIT_7;  // Reserved, do not count on value
  AMD2_BIT_8      = BIT_8;  // Reserved, do not count on value
  AMD2_BIT_9      = BIT_9;  // Reserved, do not count on value
  AMD2_BIT_10     = BIT_10; // Reserved, do not count on value
  AMD2_BIT_11     = BIT_11; // Reserved, do not count on value
  AMD2_BIT_12     = BIT_12; // Reserved, do not count on value
  AMD2_CMPXCHG16B = BIT_13; // CMPXCHG16B available
  AMD2_BIT_14     = BIT_14; // Reserved, do not count on value
  AMD2_BIT_15     = BIT_15; // Reserved, do not count on value
  AMD2_BIT_16     = BIT_16; // Reserved, do not count on value
  AMD2_BIT_17     = BIT_17; // Reserved, do not count on value
  AMD2_BIT_18     = BIT_18; // Reserved, do not count on value
  AMD2_BIT_19     = BIT_19; // Reserved, do not count on value
  AMD2_BIT_20     = BIT_20; // Reserved, do not count on value
  AMD2_BIT_21     = BIT_21; // Reserved, do not count on value
  AMD2_BIT_22     = BIT_22; // Reserved, do not count on value
  AMD2_POPCNT     = BIT_23; // POPCNT instruction. See "POPCNT" in APM3.
  AMD2_BIT_24     = BIT_24; // Reserved, do not count on value
  AMD2_BIT_25     = BIT_25; // Reserved, do not count on value
  AMD2_BIT_26     = BIT_26; // Reserved, do not count on value
  AMD2_BIT_27     = BIT_27; // Reserved, do not count on value
  AMD2_BIT_28     = BIT_28; // Reserved, do not count on value
  AMD2_BIT_29     = BIT_29; // Reserved, do not count on value
  AMD2_BIT_30     = BIT_30; // Reserved, do not count on value
  AMD2_RAZ        = BIT_31; // RAZ

  { AMD Enhanced Feature Flags }
  EAMD_FPU     = BIT_0;  // Floating-Point unit on chip
  EAMD_VME     = BIT_1;  // Virtual Mode Extention
  EAMD_DE      = BIT_2;  // Debugging Extention
  EAMD_PSE     = BIT_3;  // Page Size Extention
  EAMD_TSC     = BIT_4;  // Time Stamp Counter
  EAMD_MSR     = BIT_5;  // Model Specific Registers
  EAMD_PAE     = BIT_6;  // Physical-address extensions
  EAMD_MCE     = BIT_7;  // Machine Check Exception
  EAMD_CX8     = BIT_8;  // CMPXCHG8 Instruction
  EAMD_APIC    = BIT_9;  // Advanced Programmable Interrupt Controler
  EAMD_BIT_10  = BIT_10; // Reserved, do not count on value
  EAMD_SEP     = BIT_11; // Fast System Call
  EAMD_MTRR    = BIT_12; // Memory-Type Range Registers
  EAMD_PGE     = BIT_13; // Page Global Enable
  EAMD_MCA     = BIT_14; // Machine Check Architecture
  EAMD_CMOV    = BIT_15; // Conditional Move Intructions
  EAMD_PAT     = BIT_16; // Page Attributes Table
  EAMD_PSE2    = BIT_17; // Page Size Extensions
  EAMD_BIT_18  = BIT_18; // Reserved, do not count on value
  EAMD_BIT_19  = BIT_19; // Reserved, do not count on value
  EAMD_NX      = BIT_20; // No-Execute Page Protection
  EAMD_BIT_21  = BIT_21; // Reserved, do not count on value
  EAMD_EXMMX   = BIT_22; // AMD Extensions to MMX technology
  EAMD_MMX     = BIT_23; // MMX technology
  EAMD_FX      = BIT_24; // FXSAVE and FXSTORE instructions
  EAMD_FFX     = BIT_25; // Fast FXSAVE and FXSTORE instructions
  EAMD_1GBPAGE = BIT_26; // 1-GB large page support.
  EAMD_RDTSCP  = BIT_27; // RDTSCP instruction.
  EAMD_BIT_28  = BIT_28; // Reserved, do not count on value
  EAMD_LONG    = BIT_29; // Long Mode (64-bit Core)
  EAMD_EX3DNOW = BIT_30; // AMD Extensions to 3DNow! intructions
  EAMD_3DNOW   = BIT_31; // AMD 3DNOW! Technology

  { AMD Extended Feature Flags continued }
  EAMD2_LAHF          = BIT_0;  // LAHF/SAHF available in 64-bit mode
  EAMD2_CMPLEGACY     = BIT_1;  // core multi-processing legacy mode
  EAMD2_SVM           = BIT_2;  // Secure Virtual Machine
  EAMD2_EXTAPICSPACE  = BIT_3;  // This bit indicates the presence of extended APIC register space starting at offset 400h from the “APIC Base Address Register,” as specified in the BKDG.
  EAMD2_ALTMOVCR8     = BIT_4;  // LOCK MOV CR0 means MOV CR8
  EAMD2_ABM           = BIT_5;  // ABM: Advanced bit manipulation. LZCNT instruction support.
  EAMD2_SSE4A         = BIT_6;  // EXTRQ, INSERTQ, MOVNTSS, and MOVNTSD instruction support.
  EAMD2_MISALIGNSSE   = BIT_7;  // Misaligned SSE mode.
  EAMD2_3DNOWPREFETCH = BIT_8;  // PREFETCH and PREFETCHW instruction support.
  EAMD2_OSVW          = BIT_9;  // OS visible workaround.
  EAMD2_BIT_10        = BIT_10; // Reserved, do not count on value
  EAMD2_SSE5          = BIT_11; // Streaming SIMD Extensions 5
  EAMD2_SKINIT        = BIT_12; // SKINIT, STGI, and DEV support.
  EAMD2_WDT           = BIT_13; // Watchdog timer support.
  EAMD2_BIT_14        = BIT_14; // Reserved, do not count on value
  EAMD2_BIT_15        = BIT_15; // Reserved, do not count on value
  EAMD2_BIT_16        = BIT_16; // Reserved, do not count on value
  EAMD2_BIT_17        = BIT_17; // Reserved, do not count on value
  EAMD2_BIT_18        = BIT_18; // Reserved, do not count on value
  EAMD2_BIT_19        = BIT_19; // Reserved, do not count on value
  EAMD2_BIT_20        = BIT_20; // Reserved, do not count on value
  EAMD2_BIT_21        = BIT_21; // Reserved, do not count on value
  EAMD2_BIT_22        = BIT_22; // Reserved, do not count on value
  EAMD2_BIT_23        = BIT_23; // Reserved, do not count on value
  EAMD2_BIT_24        = BIT_24; // Reserved, do not count on value
  EAMD2_BIT_25        = BIT_25; // Reserved, do not count on value
  EAMD2_BIT_26        = BIT_26; // Reserved, do not count on value
  EAMD2_BIT_27        = BIT_27; // Reserved, do not count on value
  EAMD2_BIT_28        = BIT_28; // Reserved, do not count on value
  EAMD2_BIT_29        = BIT_29; // Reserved, do not count on value
  EAMD2_BIT_30        = BIT_30; // Reserved, do not count on value
  EAMD2_BIT_31        = BIT_31; // Reserved, do not count on value

  { AMD Power Management Features Flags }
  PAMD_TEMPSENSOR       = BIT_0;  // Temperature Sensor
  PAMD_FREQUENCYID      = BIT_1;  // Frequency ID Control
  PAMD_VOLTAGEID        = BIT_2;  // Voltage ID Control
  PAMD_THERMALTRIP      = BIT_3;  // Thermal Trip
  PAMD_THERMALMONITOR   = BIT_4;  // Thermal Monitoring
  PAMD_SOFTTHERMCONTROL = BIT_5;  // Software Thermal Control
  PAMD_100MHZSTEP       = BIT_6;  // 100 Mhz multiplier control.
  PAMD_HWPSTATE         = BIT_7;  // Hardware P-State control.
  PAMD_TSC_INVARIANT    = BIT_8;  // TSC rate is invariant
  PAMD_BIT_9            = BIT_9;  // Reserved, do not count on value
  PAMD_BIT_10           = BIT_10; // Reserved, do not count on value
  PAMD_BIT_11           = BIT_11; // Reserved, do not count on value
  PAMD_BIT_12           = BIT_12; // Reserved, do not count on value
  PAMD_BIT_13           = BIT_13; // Reserved, do not count on value
  PAMD_BIT_14           = BIT_14; // Reserved, do not count on value
  PAMD_BIT_15           = BIT_15; // Reserved, do not count on value
  PAMD_BIT_16           = BIT_16; // Reserved, do not count on value
  PAMD_BIT_17           = BIT_17; // Reserved, do not count on value
  PAMD_BIT_18           = BIT_18; // Reserved, do not count on value
  PAMD_BIT_19           = BIT_19; // Reserved, do not count on value
  PAMD_BIT_20           = BIT_20; // Reserved, do not count on value
  PAMD_BIT_21           = BIT_21; // Reserved, do not count on value
  PAMD_BIT_22           = BIT_22; // Reserved, do not count on value
  PAMD_BIT_23           = BIT_23; // Reserved, do not count on value
  PAMD_BIT_24           = BIT_24; // Reserved, do not count on value
  PAMD_BIT_25           = BIT_25; // Reserved, do not count on value
  PAMD_BIT_26           = BIT_26; // Reserved, do not count on value
  PAMD_BIT_27           = BIT_27; // Reserved, do not count on value
  PAMD_BIT_28           = BIT_28; // Reserved, do not count on value
  PAMD_BIT_29           = BIT_29; // Reserved, do not count on value
  PAMD_BIT_30           = BIT_30; // Reserved, do not count on value
  PAMD_BIT_31           = BIT_31; // Reserved, do not count on value

  { AMD TLB and L1 Associativity constants }
  AMD_ASSOC_RESERVED = 0;
  AMD_ASSOC_DIRECT   = 1;
  // 2 to 254 = direct value to the associativity
  AMD_ASSOC_FULLY    = 255;

  { AMD L2 Cache Associativity constants }
  AMD_L2_ASSOC_DISABLED = 0;
  AMD_L2_ASSOC_DIRECT   = 1;
  AMD_L2_ASSOC_2WAY     = 2;
  AMD_L2_ASSOC_4WAY     = 4;
  AMD_L2_ASSOC_8WAY     = 6;
  AMD_L2_ASSOC_16WAY    = 8;
  AMD_L2_ASSOC_FULLY    = 15;

  { VIA Standard Feature Flags }
  VIA_FPU           = BIT_0;  // FPU present
  VIA_VME           = BIT_1;  // Virtual Mode Extension
  VIA_DE            = BIT_2;  // Debugging extensions
  VIA_PSE           = BIT_3;  // Page Size Extensions (4MB)
  VIA_TSC           = BIT_4;  // Time Stamp Counter
  VIA_MSR           = BIT_5;  // Model Specific Registers
  VIA_PAE           = BIT_6;  // Physical Address Extension
  VIA_MCE           = BIT_7;  // Machine Check Exception
  VIA_CX8           = BIT_8;  // CMPXCHG8B instruction
  VIA_APIC          = BIT_9;  // APIC supported
  VIA_BIT_10        = BIT_10; // Reserved, do not count on value
  VIA_SEP           = BIT_11; // Fast System Call
  VIA_MTRR          = BIT_12; // Memory Range Registers
  VIA_PTE           = BIT_13; // PTE Global Bit
  VIA_MCA           = BIT_14; // Machine Check Architecture
  VIA_CMOVE         = BIT_15; // Conditional Move
  VIA_PAT           = BIT_16; // Page Attribute Table
  VIA_PSE2          = BIT_17; // 36-bit Page Size Extension
  VIA_SNUM          = BIT_18; // Processor serial number
  VIA_BIT_19        = BIT_19; // Reserved, do not count on value
  VIA_BIT_20        = BIT_20; // Reserved, do not count on value
  VIA_BIT_21        = BIT_21; // Reserved, do not count on value
  VIA_BIT_22        = BIT_22; // Reserved, do not count on value
  VIA_MMX           = BIT_23; // MMX
  VIA_FX            = BIT_24; // FXSAVE and FXSTORE instructions
  VIA_SSE           = BIT_25; // Streaming SIMD Extension
  VIA_BIT_26        = BIT_26; // Reserved, do not count on value
  VIA_BIT_27        = BIT_27; // Reserved, do not count on value
  VIA_BIT_28        = BIT_28; // Reserved, do not count on value
  VIA_BIT_29        = BIT_29; // Reserved, do not count on value
  VIA_BIT_30        = BIT_30; // Reserved, do not count on value
  VIA_3DNOW         = BIT_31; // 3DNow! Technology

  { VIA Extended Feature Flags }
  EVIA_AIS    = BIT_0;  // Alternate Instruction Set
  EVIA_AISE   = BIT_1;  // Alternate Instruction Set Enabled
  EVIA_NO_RNG = BIT_2;  // NO Random Number Generator
  EVIA_RNGE   = BIT_3;  // Random Number Generator Enabled
  EVIA_MSR    = BIT_4;  // Longhaul MSR 0x110A available
  EVIA_FEMMS  = BIT_5;  // FEMMS instruction Present
  EVIA_NO_ACE = BIT_6;  // Advanced Cryptography Engine NOT Present
  EVIA_ACEE   = BIT_7;  // ACE Enabled
  EVIA_BIT_8  = BIT_8;  // Reserved, do not count on value
  EVIA_BIT_9  = BIT_9;  // Reserved, do not count on value
  EVIA_BIT_10 = BIT_10; // Reserved, do not count on value
  EVIA_BIT_11 = BIT_11; // Reserved, do not count on value
  EVIA_BIT_12 = BIT_12; // Reserved, do not count on value
  EVIA_BIT_13 = BIT_13; // Reserved, do not count on value
  EVIA_BIT_14 = BIT_14; // Reserved, do not count on value
  EVIA_BIT_15 = BIT_15; // Reserved, do not count on value
  EVIA_BIT_16 = BIT_16; // Reserved, do not count on value
  EVIA_BIT_17 = BIT_17; // Reserved, do not count on value
  EVIA_BIT_18 = BIT_18; // Reserved, do not count on value
  EVIA_BIT_19 = BIT_19; // Reserved, do not count on value
  EVIA_BIT_20 = BIT_20; // Reserved, do not count on value
  EVIA_BIT_21 = BIT_21; // Reserved, do not count on value
  EVIA_BIT_22 = BIT_22; // Reserved, do not count on value
  EVIA_BIT_23 = BIT_23; // Reserved, do not count on value
  EVIA_BIT_24 = BIT_24; // Reserved, do not count on value
  EVIA_BIT_25 = BIT_25; // Reserved, do not count on value
  EVIA_BIT_26 = BIT_26; // Reserved, do not count on value
  EVIA_BIT_27 = BIT_27; // Reserved, do not count on value
  EVIA_BIT_28 = BIT_28; // Reserved, do not count on value
  EVIA_BIT_29 = BIT_29; // Reserved, do not count on value
  EVIA_BIT_30 = BIT_30; // Reserved, do not count on value
  EVIA_BIT_31 = BIT_31; // Reserved, do not count on value

  { Cyrix Standard Feature Flags }
  CYRIX_FPU    = BIT_0;  // Floating-Point unit on chip
  CYRIX_VME    = BIT_1;  // Virtual Mode Extention
  CYRIX_DE     = BIT_2;  // Debugging Extention
  CYRIX_PSE    = BIT_3;  // Page Size Extention
  CYRIX_TSC    = BIT_4;  // Time Stamp Counter
  CYRIX_MSR    = BIT_5;  // Model Specific Registers
  CYRIX_PAE    = BIT_6;  // Physical Address Extention
  CYRIX_MCE    = BIT_7;  // Machine Check Exception
  CYRIX_CX8    = BIT_8;  // CMPXCHG8 Instruction
  CYRIX_APIC   = BIT_9;  // Software-accessible local APIC on Chip
  CYRIX_BIT_10 = BIT_10; // Reserved, do not count on value
  CYRIX_BIT_11 = BIT_11; // Reserved, do not count on value
  CYRIX_MTRR   = BIT_12; // Memory Type Range Registers
  CYRIX_PGE    = BIT_13; // Page Global Enable
  CYRIX_MCA    = BIT_14; // Machine Check Architecture
  CYRIX_CMOV   = BIT_15; // Conditional Move Instruction
  CYRIX_BIT_16 = BIT_16; // Reserved, do not count on value
  CYRIX_BIT_17 = BIT_17; // Reserved, do not count on value
  CYRIX_BIT_18 = BIT_18; // Reserved, do not count on value
  CYRIX_BIT_19 = BIT_19; // Reserved, do not count on value
  CYRIX_BIT_20 = BIT_20; // Reserved, do not count on value
  CYRIX_BIT_21 = BIT_21; // Reserved, do not count on value
  CYRIX_BIT_22 = BIT_22; // Reserved, do not count on value
  CYRIX_MMX    = BIT_23; // MMX technology
  CYRIX_BIT_24 = BIT_24; // Reserved, do not count on value
  CYRIX_BIT_25 = BIT_25; // Reserved, do not count on value
  CYRIX_BIT_26 = BIT_26; // Reserved, do not count on value
  CYRIX_BIT_27 = BIT_27; // Reserved, do not count on value
  CYRIX_BIT_28 = BIT_28; // Reserved, do not count on value
  CYRIX_BIT_29 = BIT_29; // Reserved, do not count on value
  CYRIX_BIT_30 = BIT_30; // Reserved, do not count on value
  CYRIX_BIT_31 = BIT_31; // Reserved, do not count on value

  { Cyrix Enhanced Feature Flags }
  ECYRIX_FPU    = BIT_0;  // Floating-Point unit on chip
  ECYRIX_VME    = BIT_1;  // Virtual Mode Extention
  ECYRIX_DE     = BIT_2;  // Debugging Extention
  ECYRIX_PSE    = BIT_3;  // Page Size Extention
  ECYRIX_TSC    = BIT_4;  // Time Stamp Counter
  ECYRIX_MSR    = BIT_5;  // Model Specific Registers
  ECYRIX_PAE    = BIT_6;  // Physical Address Extention
  ECYRIX_MCE    = BIT_7;  // Machine Check Exception
  ECYRIX_CX8    = BIT_8;  // CMPXCHG8 Instruction
  ECYRIX_APIC   = BIT_9;  // Software-accessible local APIC on Chip
  ECYRIX_SEP    = BIT_10; // Fast System Call
  ECYRIX_BIT_11 = BIT_11; // Reserved, do not count on value
  ECYRIX_MTRR   = BIT_12; // Memory Type Range Registers
  ECYRIX_PGE    = BIT_13; // Page Global Enable
  ECYRIX_MCA    = BIT_14; // Machine Check Architecture
  ECYRIX_ICMOV  = BIT_15; // Integer Conditional Move Instruction
  ECYRIX_FCMOV  = BIT_16; // Floating Point Conditional Move Instruction
  ECYRIX_BIT_17 = BIT_17; // Reserved, do not count on value
  ECYRIX_BIT_18 = BIT_18; // Reserved, do not count on value
  ECYRIX_BIT_19 = BIT_19; // Reserved, do not count on value
  ECYRIX_BIT_20 = BIT_20; // Reserved, do not count on value
  ECYRIX_BIT_21 = BIT_21; // Reserved, do not count on value
  ECYRIX_BIT_22 = BIT_22; // Reserved, do not count on value
  ECYRIX_MMX    = BIT_23; // MMX technology
  ECYRIX_EMMX   = BIT_24; // Extended MMX Technology
  ECYRIX_BIT_25 = BIT_25; // Reserved, do not count on value
  ECYRIX_BIT_26 = BIT_26; // Reserved, do not count on value
  ECYRIX_BIT_27 = BIT_27; // Reserved, do not count on value
  ECYRIX_BIT_28 = BIT_28; // Reserved, do not count on value
  ECYRIX_BIT_29 = BIT_29; // Reserved, do not count on value
  ECYRIX_BIT_30 = BIT_30; // Reserved, do not count on value
  ECYRIX_BIT_31 = BIT_31; // Reserved, do not count on value

  { Transmeta Features }
  TRANSMETA_FPU    = BIT_0;  // Floating-Point unit on chip
  TRANSMETA_VME    = BIT_1;  // Virtual Mode Extention
  TRANSMETA_DE     = BIT_2;  // Debugging Extention
  TRANSMETA_PSE    = BIT_3;  // Page Size Extention
  TRANSMETA_TSC    = BIT_4;  // Time Stamp Counter
  TRANSMETA_MSR    = BIT_5;  // Model Specific Registers
  TRANSMETA_BIT_6  = BIT_6;  // Reserved, do not count on value
  TRANSMETA_BIT_7  = BIT_7;  // Reserved, do not count on value
  TRANSMETA_CX8    = BIT_8;  // CMPXCHG8 Instruction
  TRANSMETA_BIT_9  = BIT_9;  // Reserved, do not count on value
  TRANSMETA_BIT_10 = BIT_10; // Reserved, do not count on value
  TRANSMETA_SEP    = BIT_11; // Fast system Call Extensions
  TRANSMETA_BIT_12 = BIT_12; // Reserved, do not count on value
  TRANSMETA_BIT_13 = BIT_13; // Reserved, do not count on value
  TRANSMETA_BIT_14 = BIT_14; // Reserved, do not count on value
  TRANSMETA_CMOV   = BIT_15; // Conditional Move Instruction
  TRANSMETA_BIT_16 = BIT_16; // Reserved, do not count on value
  TRANSMETA_BIT_17 = BIT_17; // Reserved, do not count on value
  TRANSMETA_PSN    = BIT_18; // Processor Serial Number
  TRANSMETA_BIT_19 = BIT_19; // Reserved, do not count on value
  TRANSMETA_BIT_20 = BIT_20; // Reserved, do not count on value
  TRANSMETA_BIT_21 = BIT_21; // Reserved, do not count on value
  TRANSMETA_BIT_22 = BIT_22; // Reserved, do not count on value
  TRANSMETA_MMX    = BIT_23; // MMX technology
  TRANSMETA_BIT_24 = BIT_24; // Reserved, do not count on value
  TRANSMETA_BIT_25 = BIT_25; // Reserved, do not count on value
  TRANSMETA_BIT_26 = BIT_26; // Reserved, do not count on value
  TRANSMETA_BIT_27 = BIT_27; // Reserved, do not count on value
  TRANSMETA_BIT_28 = BIT_28; // Reserved, do not count on value
  TRANSMETA_BIT_29 = BIT_29; // Reserved, do not count on value
  TRANSMETA_BIT_30 = BIT_30; // Reserved, do not count on value
  TRANSMETA_BIT_31 = BIT_31; // Reserved, do not count on value

  { Extended Transmeta Features }
  ETRANSMETA_FPU    = BIT_0;  // Floating-Point unit on chip
  ETRANSMETA_VME    = BIT_1;  // Virtual Mode Extention
  ETRANSMETA_DE     = BIT_2;  // Debugging Extention
  ETRANSMETA_PSE    = BIT_3;  // Page Size Extention
  ETRANSMETA_TSC    = BIT_4;  // Time Stamp Counter
  ETRANSMETA_MSR    = BIT_5;  // Model Specific Registers
  ETRANSMETA_BIT_6  = BIT_6;  // Reserved, do not count on value
  ETRANSMETA_BIT_7  = BIT_7;  // Reserved, do not count on value
  ETRANSMETA_CX8    = BIT_8;  // CMPXCHG8 Instruction
  ETRANSMETA_BIT_9  = BIT_9;  // Reserved, do not count on value
  ETRANSMETA_BIT_10 = BIT_10; // Reserved, do not count on value
  ETRANSMETA_BIT_11 = BIT_11; // Reserved, do not count on value
  ETRANSMETA_BIT_12 = BIT_12; // Reserved, do not count on value
  ETRANSMETA_BIT_13 = BIT_13; // Reserved, do not count on value
  ETRANSMETA_BIT_14 = BIT_14; // Reserved, do not count on value
  ETRANSMETA_CMOV   = BIT_15; // Conditional Move Instruction
  ETRANSMETA_FCMOV  = BIT_16; // Float Conditional Move Instruction
  ETRANSMETA_BIT_17 = BIT_17; // Reserved, do not count on value
  ETRANSMETA_BIT_18 = BIT_18; // Reserved, do not count on value
  ETRANSMETA_BIT_19 = BIT_19; // Reserved, do not count on value
  ETRANSMETA_BIT_20 = BIT_20; // Reserved, do not count on value
  ETRANSMETA_BIT_21 = BIT_21; // Reserved, do not count on value
  ETRANSMETA_BIT_22 = BIT_22; // Reserved, do not count on value
  ETRANSMETA_MMX    = BIT_23; // MMX technology
  ETRANSMETA_BIT_24 = BIT_24; // Reserved, do not count on value
  ETRANSMETA_BIT_25 = BIT_25; // Reserved, do not count on value
  ETRANSMETA_BIT_26 = BIT_26; // Reserved, do not count on value
  ETRANSMETA_BIT_27 = BIT_27; // Reserved, do not count on value
  ETRANSMETA_BIT_28 = BIT_28; // Reserved, do not count on value
  ETRANSMETA_BIT_29 = BIT_29; // Reserved, do not count on value
  ETRANSMETA_BIT_30 = BIT_30; // Reserved, do not count on value
  ETRANSMETA_BIT_31 = BIT_31; // Reserved, do not count on value

  { Transmeta Specific Features }
  STRANSMETA_RECOVERY = BIT_0;  // Recovery Mode
  STRANSMETA_LONGRUN  = BIT_1;  // Long Run
  STRANSMETA_BIT_2    = BIT_2;  // Debugging Extention
  STRANSMETA_LRTI     = BIT_3;  // Long Run Table Interface
  STRANSMETA_BIT_4    = BIT_4;  // Reserved, do not count on value
  STRANSMETA_BIT_5    = BIT_5;  // Reserved, do not count on value
  STRANSMETA_BIT_6    = BIT_6;  // Reserved, do not count on value
  STRANSMETA_PTTI1    = BIT_7;  // Persistent Translation Technology 1.x
  STRANSMETA_PTTI2    = BIT_8;  // Persistent Translation Technology 2.0
  STRANSMETA_BIT_9    = BIT_9;  // Reserved, do not count on value
  STRANSMETA_BIT_10   = BIT_10; // Reserved, do not count on value
  STRANSMETA_BIT_11   = BIT_11; // Reserved, do not count on value
  STRANSMETA_BIT_12   = BIT_12; // Reserved, do not count on value
  STRANSMETA_BIT_13   = BIT_13; // Reserved, do not count on value
  STRANSMETA_BIT_14   = BIT_14; // Reserved, do not count on value
  STRANSMETA_BIT_15   = BIT_15; // Reserved, do not count on value
  STRANSMETA_BIT_16   = BIT_16; // Reserved, do not count on value
  STRANSMETA_BIT_17   = BIT_17; // Reserved, do not count on value
  STRANSMETA_BIT_18   = BIT_18; // Reserved, do not count on value
  STRANSMETA_BIT_19   = BIT_19; // Reserved, do not count on value
  STRANSMETA_BIT_20   = BIT_20; // Reserved, do not count on value
  STRANSMETA_BIT_21   = BIT_21; // Reserved, do not count on value
  STRANSMETA_BIT_22   = BIT_22; // Reserved, do not count on value
  STRANSMETA_BIT_23   = BIT_23; // Reserved, do not count on value
  STRANSMETA_BIT_24   = BIT_24; // Reserved, do not count on value
  STRANSMETA_BIT_25   = BIT_25; // Reserved, do not count on value
  STRANSMETA_BIT_26   = BIT_26; // Reserved, do not count on value
  STRANSMETA_BIT_27   = BIT_27; // Reserved, do not count on value
  STRANSMETA_BIT_28   = BIT_28; // Reserved, do not count on value
  STRANSMETA_BIT_29   = BIT_29; // Reserved, do not count on value
  STRANSMETA_BIT_30   = BIT_30; // Reserved, do not count on value
  STRANSMETA_BIT_31   = BIT_31; // Reserved, do not count on value

  { Constants of bits of the MXCSR register - Intel and AMD processors that support SSE instructions}
  MXCSR_IE  = BIT_0;                  // Invalid Operation flag
  MXCSR_DE  = BIT_1;                  // Denormal flag
  MXCSR_ZE  = BIT_2;                  // Divide by Zero flag
  MXCSR_OE  = BIT_3;                  // Overflow flag
  MXCSR_UE  = BIT_4;                  // Underflow flag
  MXCSR_PE  = BIT_5;                  // Precision flag
  MXCSR_DAZ = BIT_6;                  // Denormal are Zero flag
  MXCSR_IM  = BIT_7;                  // Invalid Operation mask
  MXCSR_DM  = BIT_8;                  // Denormal mask
  MXCSR_ZM  = BIT_9;                  // Divide by Zero mask
  MXCSR_OM  = BIT_10;                 // Overflow mask
  MXCSR_UM  = BIT_11;                 // Underflow mask
  MXCSR_PM  = BIT_12;                 // Precision mask
  MXCSR_RC1 = BIT_13;                 // Rounding control, bit 1
  MXCSR_RC2 = BIT_14;                 // Rounding control, bit 2
  MXCSR_RC  = MXCSR_RC1 or MXCSR_RC2; // Rounding control
  MXCSR_FZ  = BIT_15;                 // Flush to Zero

const
  IntelCacheDescription: array [0..64] of TCacheInfo = (
    (D: $00; Family: cfOther;                                                                           I: RsIntelCacheDescr00),
    (D: $01; Family: cfInstructionTLB;     Size: 4;     WaysOfAssoc: 4;                Entries: 32;      I: RsIntelCacheDescr01),
    (D: $02; Family: cfInstructionTLB;     Size: 4096;  WaysOfAssoc: 4;                Entries: 2;       I: RsIntelCacheDescr02),
    (D: $03; Family: cfDataTLB;            Size: 4;     WaysOfAssoc: 4;                Entries: 64;      I: RsIntelCacheDescr03),
    (D: $04; Family: cfDataTLB;            Size: 4096;  WaysOfAssoc: 4;                Entries: 8;       I: RsIntelCacheDescr04),
    (D: $05; Family: cfDataTLB;            Size: 4096;  WaysOfAssoc: 4;                Entries: 32;      I: RsIntelCacheDescr05),
    (D: $06; Family: cfL1InstructionCache; Size: 8;     WaysOfAssoc: 4;  LineSize: 32;                   I: RsIntelCacheDescr06),
    (D: $08; Family: cfL1InstructionCache; Size: 16;    WaysOfAssoc: 4;  LineSize: 32;                   I: RsIntelCacheDescr08),
    (D: $0A; Family: cfL1DataCache;        Size: 8;     WaysOfAssoc: 2;  LineSize: 32;                   I: RsIntelCacheDescr0A),
    (D: $0B; Family: cfInstructionTLB;     Size: 4;     WaysOfAssoc: 4;                Entries: 4;       I: RsIntelCacheDescr0B),
    (D: $0C; Family: cfL1DataCache;        Size: 16;    WaysOfAssoc: 4;  LineSize: 32;                   I: RsIntelCacheDescr0C),
    (D: $22; Family: cfL3Cache;            Size: 512;   WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 2; I: RsIntelCacheDescr22),
    (D: $23; Family: cfL3Cache;            Size: 1024;  WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 2; I: RsIntelCacheDescr23),
    (D: $25; Family: cfL3Cache;            Size: 2048;  WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 2; I: RsIntelCacheDescr25),
    (D: $29; Family: cfL3Cache;            Size: 4096;  WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 2; I: RsIntelCacheDescr29),
    (D: $2C; Family: cfL1DataCache;        Size: 32;    WaysOfAssoc: 8;  LineSize: 64;                   I: RsIntelCacheDescr2C),
    (D: $30; Family: cfL1InstructionCache; Size: 32;    WaysOfAssoc: 8;  LineSize: 64;                   I: RsIntelCacheDescr30),
    (D: $40; Family: cfOther;                                                                            I: RsIntelCacheDescr40),
    (D: $41; Family: cfL2Cache;            Size: 128;   WaysOfAssoc: 4;  LineSize: 32;                   I: RsIntelCacheDescr41),
    (D: $42; Family: cfL2Cache;            Size: 256;   WaysOfAssoc: 4;  LineSize: 32;                   I: RsIntelCacheDescr42),
    (D: $43; Family: cfL2Cache;            Size: 512;   WaysOfAssoc: 4;  LineSize: 32;                   I: RsIntelCacheDescr43),
    (D: $44; Family: cfL2Cache;            Size: 1024;  WaysOfAssoc: 4;  LineSize: 32;                   I: RsIntelCacheDescr44),
    (D: $45; Family: cfL2Cache;            Size: 2048;  WaysOfAssoc: 4;  LineSize: 32;                   I: RsIntelCacheDescr45),
    (D: $46; Family: cfL3Cache;            Size: 4096;  WaysOfAssoc: 4;  LineSize: 64;                   I: RsIntelCacheDescr46),
    (D: $47; Family: cfL3Cache;            Size: 8192;  WaysOfAssoc: 8;  LineSize: 64;                   I: RsIntelCacheDescr47),
    (D: $48; Family: cfL2Cache;            Size: 3072;  WaysOfAssoc: 12; LineSize: 64;                   I: RsIntelCacheDescr48),
    (D: $49; Family: cfL2Cache;            Size: 4096;  WaysOfAssoc: 16; LineSize: 64;                   I: RsIntelCacheDescr49),
    (D: $4A; Family: cfL3Cache;            Size: 6144;  WaysOfAssoc: 12; LineSize: 64;                   I: RsIntelCacheDescr4A),
    (D: $4B; Family: cfL3Cache;            Size: 8192;  WaysOfAssoc: 16; LineSize: 64;                   I: RsIntelCacheDescr4B),
    (D: $4D; Family: cfL3Cache;            Size: 16384; WaysOfAssoc: 16; LineSize: 64;                   I: RsIntelCacheDescr4D),
    (D: $4E; Family: cfL3Cache;            Size: 6144;  WaysOfAssoc: 24; LineSize: 64;                   I: RsIntelCacheDescr4E),
    (D: $50; Family: cfInstructionTLB;     Size: 4;                                    Entries: 64;      I: RsIntelCacheDescr50),
    (D: $51; Family: cfInstructionTLB;     Size: 4;                                    Entries: 128;     I: RsIntelCacheDescr51),
    (D: $52; Family: cfInstructionTLB;     Size: 4;                                    Entries: 256;     I: RsIntelCacheDescr52),
    (D: $56; Family: cfDataTLB;            Size: 4096;  WaysOfAssoc: 4;                Entries: 16;      I: RsIntelCacheDescr56),
    (D: $57; Family: cfDataTLB;            Size: 4;     WaysOfAssoc: 4;                Entries: 16;      I: RsIntelCacheDescr57),
    (D: $5B; Family: cfDataTLB;            Size: 4096;                                 Entries: 64;      I: RsIntelCacheDescr5B),
    (D: $5C; Family: cfDataTLB;            Size: 4096;                                 Entries: 128;     I: RsIntelCacheDescr5C),
    (D: $5D; Family: cfDataTLB;            Size: 4096;                                 Entries: 256;     I: RsIntelCacheDescr5D),
    (D: $60; Family: cfL1DataCache;        Size: 16;    WaysOfAssoc: 8;  LineSize: 64;                   I: RsIntelCacheDescr60),
    (D: $66; Family: cfL1DataCache;        Size: 8;     WaysOfAssoc: 4;  LineSize: 64;                   I: RsIntelCacheDescr66),
    (D: $67; Family: cfL1DataCache;        Size: 16;    WaysOfAssoc: 4;  LineSize: 64;                   I: RsIntelCacheDescr67),
    (D: $68; Family: cfL1DataCache;        Size: 32;    WaysOfAssoc: 4;  LineSize: 64;                   I: RsIntelCacheDescr68),
    (D: $70; Family: cfTrace;              Size: 12;    WaysOfAssoc: 8;                                  I: RsIntelCacheDescr70),
    (D: $71; Family: cfTrace;              Size: 16;    WaysOfAssoc: 8;                                  I: RsIntelCacheDescr71),
    (D: $72; Family: cfTrace;              Size: 32;    WaysOfAssoc: 8;                                  I: RsIntelCacheDescr72),
    (D: $78; Family: cfL2Cache;            Size: 1024;  WaysOfAssoc: 4;  LineSize: 64;                   I: RsIntelCacheDescr78),
    (D: $79; Family: cfL2Cache;            Size: 128;   WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 2; I: RsIntelCacheDescr79),
    (D: $7A; Family: cfL2Cache;            Size: 256;   WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 2; I: RsIntelCacheDescr7A),
    (D: $7B; Family: cfL2Cache;            Size: 512;   WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 2; I: RsIntelCacheDescr7B),
    (D: $7C; Family: cfL2Cache;            Size: 1024;  WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 2; I: RsIntelCacheDescr7C),
    (D: $7D; Family: cfL2Cache;            Size: 2048;  WaysOfAssoc: 8;  LineSize: 64;                   I: RsIntelCacheDescr7D),
    (D: $7F; Family: cfL2Cache;            Size: 512;   WaysOfAssoc: 2;  LineSize: 64;                   I: RsIntelCacheDescr7F),
    (D: $82; Family: cfL2Cache;            Size: 256;   WaysOfAssoc: 8;  LineSize: 32;                   I: RsIntelCacheDescr82),
    (D: $83; Family: cfL2Cache;            Size: 512;   WaysOfAssoc: 8;  LineSize: 32;                   I: RsIntelCacheDescr83),
    (D: $84; Family: cfL2Cache;            Size: 1024;  WaysOfAssoc: 8;  LineSize: 32;                   I: RsIntelCacheDescr84),
    (D: $85; Family: cfL2Cache;            Size: 2048;  WaysOfAssoc: 8;  LineSize: 32;                   I: RsIntelCacheDescr85),
    (D: $86; Family: cfL2Cache;            Size: 512;   WaysOfAssoc: 4;  LineSize: 64;                   I: RsIntelCacheDescr86),
    (D: $87; Family: cfL2Cache;            Size: 1024;  WaysOfAssoc: 8;  LineSize: 64;                   I: RsIntelCacheDescr87),
    (D: $B0; Family: cfInstructionTLB;     Size: 4;     WaysOfAssoc: 4;                 Entries: 128;    I: RsIntelCacheDescrB0),
    (D: $B1; Family: cfInstructionTLB;     Size: 2048;  WaysOfAssoc: 4;                 Entries: 8;      I: RsIntelCacheDescrB1),
    (D: $B3; Family: cfDataTLB;            Size: 4;     WaysOfAssoc: 4;                 Entries: 128;    I: RsIntelCacheDescrB3),
    (D: $B4; Family: cfDataTLB;            Size: 4;     WaysOfAssoc: 4;                 Entries: 256;    I: RsIntelCacheDescrB4),
    (D: $F0; Family: cfOther;                                                                            I: RsIntelCacheDescrF0),
    (D: $F1; Family: cfOther;                                                                            I: RsIntelCacheDescrF1)
  );

procedure GetCpuInfo(var CpuInfo: TCpuInfo);

function GetIntelCacheDescription(const D: Byte): string;
function RoundFrequency(const Frequency: Integer): Integer;
{$IFDEF MSWINDOWS}
function GetCPUSpeed(var CpuSpeed: TFreqInfo): Boolean;
{$ENDIF MSWINDOWS}
function CPUID: TCpuInfo;
function TestFDIVInstruction: Boolean;

// Memory Information
{$IFDEF MSWINDOWS}
function GetMaxAppAddress: Cardinal;
function GetMinAppAddress: Cardinal;
{$ENDIF MSWINDOWS}
function GetMemoryLoad: Byte;
function GetSwapFileSize: Cardinal;
function GetSwapFileUsage: Byte;
function GetTotalPhysicalMemory: Cardinal;
function GetFreePhysicalMemory: Cardinal;
{$IFDEF MSWINDOWS}
function GetTotalPageFileMemory: Cardinal;
function GetFreePageFileMemory: Cardinal;
function GetTotalVirtualMemory: Cardinal;
function GetFreeVirtualMemory: Cardinal;
{$ENDIF MSWINDOWS}

// Alloc granularity
procedure RoundToAllocGranularity64(var Value: Int64; Up: Boolean);
procedure RoundToAllocGranularityPtr(var Value: Pointer; Up: Boolean);

{$IFDEF MSWINDOWS}
// Keyboard Information
function GetKeyState(const VirtualKey: Cardinal): Boolean;
function GetNumLockKeyState: Boolean;
function GetScrollLockKeyState: Boolean;
function GetCapsLockKeyState: Boolean;

// Windows 95/98/Me system resources information
type
  TFreeSysResKind = (rtSystem, rtGdi, rtUser);
  TFreeSystemResources = record
    SystemRes: Integer;
    GdiRes: Integer;
    UserRes: Integer;
  end;

function IsSystemResourcesMeterPresent: Boolean;

function GetFreeSystemResources(const ResourceType: TFreeSysResKind): Integer; overload;
function GetFreeSystemResources: TFreeSystemResources; overload;
function GetBPP: Cardinal;

// Installed programs information
function ProgIDExists(const ProgID: string): Boolean;
function IsWordInstalled: Boolean;
function IsExcelInstalled: Boolean;
function IsAccessInstalled: Boolean;
function IsPowerPointInstalled: Boolean;
function IsFrontPageInstalled: Boolean;
function IsOutlookInstalled: Boolean;
function IsInternetExplorerInstalled: Boolean;
function IsMSProjectInstalled: Boolean;
function IsOpenOfficeInstalled: Boolean;

{$ENDIF MSWINDOWS}

// Public global variables
var
  ProcessorCount: Cardinal = 0;
  AllocGranularity: Cardinal = 0;
  PageSize: Cardinal = 0;
{$ENDIF ~CLR}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclSysInfo.pas $';
    Revision: '$Revision: 2386 $';
    Date: '$Date: 2008-06-16 08:49:55 +0200 (lun., 16 juin 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils,
  {$IFNDEF CLR}
  {$IFDEF MSWINDOWS}
  Messages, Winsock, Snmp,
  {$IFDEF FPC}
   JwaTlHelp32, JwaPsApi,
  {$ELSE ~FPC}
  TLHelp32, PsApi,
  JclShell,
  {$ENDIF ~FPC}
  JclRegistry, JclWin32,
  {$ENDIF MSWINDOWS}
  Jcl8087, JclIniFiles,
  {$ENDIF ~CLR}
  JclBase, JclFileUtils, JclStrings;

{$IFDEF FPC}
{$IFDEF MSWINDOWS}
{$I JclSysInfo.fpc}
{$ENDIF MSWINDOWS}
{$ENDIF FPC}

//=== Environment ============================================================

function DelEnvironmentVar(const Name: string): Boolean;
begin
  {$IFDEF CLR}
  System.Environment.GetEnvironmentVariables.Remove(Name);
  Result := True;
  {$ELSE ~CLR}
  {$IFDEF UNIX}
  UnSetEnv(PChar(Name));
  Result := True;
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
  Result := SetEnvironmentVariable(PChar(Name), nil);
  {$ENDIF MSWINDOWS}
  {$ENDIF ~CLR}
end;

function ExpandEnvironmentVar(var Value: string): Boolean;
{$IFDEF CLR}
begin
  Value := System.Environment.ExpandEnvironmentVariables(Value);
  Result := True;
end;
{$ELSE ~CLR}
{$IFDEF UNIX}
begin
  Result := True;
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  R: Integer;
  Expanded: string;
begin
  SetLength(Expanded, 1);
  R := ExpandEnvironmentStrings(PChar(Value), PChar(Expanded), 0);
  SetLength(Expanded, R);
  Result := ExpandEnvironmentStrings(PChar(Value), PChar(Expanded), R) <> 0;
  if Result then
  begin
    StrResetLength(Expanded);
    Value := Expanded;
  end;
end;
{$ENDIF MSWINDOWS}
{$ENDIF ~CLR}

{$IFDEF UNIX}

function GetEnvironmentVar(const Name: string; var Value: string): Boolean;
begin
  Value := getenv(PChar(Name));
  Result := Value <> '';
end;

function GetEnvironmentVar(const Name: string; var Value: string; Expand: Boolean): Boolean;
begin
  Result := GetEnvironmentVar(Name, Value); // Expand is there just for x-platform compatibility
end;

{$ENDIF UNIX}

{$IFDEF MSWINDOWS}

function GetEnvironmentVar(const Name: string; var Value: string): Boolean;
begin
  {$IFDEF CLR}
  Value := System.Environment.GetEnvironmentVariable(Name);
  Result := TObject(Value) <> nil;
  {$ELSE ~CLR}
  Result := GetEnvironmentVar(Name, Value, True);
  {$ENDIF ~CLR}
end;

function GetEnvironmentVar(const Name: string; var Value: string; Expand: Boolean): Boolean;
{$IFDEF CLR}
begin
  Result := GetEnvironmentVar(Name, Value);
  if Expand then
    ExpandEnvironmentVar(Value);
end;
{$ELSE ~CLR}
var
  R: DWORD;
begin
  R := Windows.GetEnvironmentVariable(PChar(Name), nil, 0);
  SetLength(Value, R);
  R := Windows.GetEnvironmentVariable(PChar(Name), PChar(Value), R);
  Result := R <> 0;
  if not Result then
    Value := ''
  else
  begin
    SetLength(Value, R);
    if Expand then
      ExpandEnvironmentVar(Value);
  end;
end;
{$ENDIF ~CLR}

{$ENDIF MSWINDOWS}

{$IFDEF LINUX}
function GetEnvironmentVars(const Vars: TStrings): Boolean;
var
  P: PPChar;
begin
  Vars.BeginUpdate;
  try
    Vars.Clear;
    P := System.envp;
    Result := P <> nil;
    while (P <> nil) and (P^ <> nil) do
    begin
      Vars.Add(P^);
      Inc(P);
    end;
  finally
    Vars.EndUpdate;
  end;
end;

function GetEnvironmentVars(const Vars: TStrings; Expand: Boolean): Boolean;
begin
  Result := GetEnvironmentVars(Vars); // Expand is there just for x-platform compatibility
end;
{$ENDIF LINUX}

{$IFDEF MSWINDOWS}
function GetEnvironmentVars(const Vars: TStrings): Boolean;
begin
  Result := GetEnvironmentVars(Vars, True);
end;

function GetEnvironmentVars(const Vars: TStrings; Expand: Boolean): Boolean;
{$IFDEF CLR}
var
  Dic: IDictionaryEnumerator;
begin
  Vars.BeginUpdate;
  try
    Vars.Clear;
    for Dic in System.Environment.GetEnvironmentVariables do
      Vars.Add(string(Dic.Key) + '=' + string(Dic.Value));
  finally
    Vars.EndUpdate;
  end;
  Result := True;
end;
{$ELSE ~CLR}
var
  Raw: PChar;
  Expanded: string;
  I: Integer;
begin
  Vars.BeginUpdate;
  try
    Vars.Clear;
    Raw := GetEnvironmentStrings;
    try
      MultiSzToStrings(Vars, Raw);
      Result := True;
    finally
      FreeEnvironmentStrings(Raw);
    end;
    if Expand then
    begin
      for I := 0 to Vars.Count - 1 do
      begin
        Expanded := Vars[I];
        if ExpandEnvironmentVar(Expanded) then
          Vars[I] := Expanded;
      end;
    end;
  finally
    Vars.EndUpdate;
  end;
end;
{$ENDIF ~CLR}

{$ENDIF MSWINDOWS}

function SetEnvironmentVar(const Name, Value: string): Boolean;
begin
  {$IFDEF CLR}
  if System.Environment.GetEnvironmentVariables.Contains(Name) then
    System.Environment.GetEnvironmentVariables.Item[Name] := Value
  else
    System.Environment.GetEnvironmentVariables.Add(Name, Value);
  Result := True;
  {$ELSE ~CLR}
  {$IFDEF UNIX}
  SetEnv(PChar(Name), PChar(Value), 1);
  Result := True;
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
  Result := SetEnvironmentVariable(PChar(Name), PChar(Value));
  {$ENDIF MSWINDOWS}
  {$ENDIF ~CLR}
end;

{$IFNDEF CLR}
{$IFDEF MSWINDOWS}

function CreateEnvironmentBlock(const Options: TEnvironmentOptions; const AdditionalVars: TStrings): PChar;
const
  RegLocalEnvironment = 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment';
  RegUserEnvironment = '\Environment\';
var
  KeyNames, TempList: TStrings;
  Temp, Name, Value: string;
  I: Integer;
begin
  TempList := TStringList.Create;
  try
    // add additional environment variables
    if eoAdditional in Options then
      for I := 0 to AdditionalVars.Count - 1 do
      begin
        Temp := AdditionalVars[I];
        ExpandEnvironmentVar(Temp);
        TempList.Add(Temp);
      end;
    // get environment strings from local machine
    if eoLocalMachine in Options then
    begin
      KeyNames := TStringList.Create;
      try
        if RegGetValueNames(HKEY_LOCAL_MACHINE, RegLocalEnvironment, KeyNames) then
        begin
          for I := 0 to KeyNames.Count - 1 do
          begin
            Name := KeyNames[I];
            Value := RegReadString(HKEY_LOCAL_MACHINE, RegLocalEnvironment, Name);
            ExpandEnvironmentVar(Value);
            TempList.Add(Name + '=' + Value);
          end;
        end;
      finally
        FreeAndNil(KeyNames);
      end;
    end;
    // get environment strings from current user
    if eoCurrentUser in Options then
    begin
      KeyNames := TStringLIst.Create;
      try
        if RegGetValueNames(HKEY_CURRENT_USER, RegUserEnvironment, KeyNames) then
        begin
          for I := 0 to KeyNames.Count - 1 do
          begin
            Name := KeyNames[I];
            Value := RegReadString(HKEY_CURRENT_USER, RegUserEnvironment, Name);
            ExpandEnvironmentVar(Value);
            TempList.Add(Name + '=' + Value);
          end;
        end;
      finally
        KeyNames.Free;
      end;
    end;
    // transform stringlist into multi-PChar
    StringsToMultiSz(Result, TempList);
  finally
    FreeAndNil(TempList);
  end;
end;

// frees an environment block allocated by CreateEnvironmentBlock and
// sets Env to nil

procedure DestroyEnvironmentBlock(var Env: PChar);
begin
  FreeMultiSz(Env);
end;

procedure SetGlobalEnvironmentVariable(VariableName, VariableContent: string);
const
  cEnvironment = 'Environment';
begin
  if VariableName = '' then
    Exit;
  if VariableContent = '' then
  begin
    RegDeleteEntry(HKEY_CURRENT_USER, cEnvironment, VariableName);
    SetEnvironmentVariable(PChar(VariableName), nil);
  end
  else
  begin
    RegWriteAnsiString(HKEY_CURRENT_USER, cEnvironment, VariableName, VariableContent);
    SetEnvironmentVariable(PChar(VariableName), PChar(VariableContent));
  end;
  SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, LPARAM(PChar(cEnvironment)));
end;

//=== Common Folders =========================================================

// Utility function which returns the Windows independent CurrentVersion key
// inside HKEY_LOCAL_MACHINE

const
  HKLM_CURRENT_VERSION_WINDOWS = 'SOFTWARE\Microsoft\Windows\CurrentVersion';
  HKLM_CURRENT_VERSION_NT      = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion';

function REG_CURRENT_VERSION: string;
begin
  if IsWinNT then
    Result := HKLM_CURRENT_VERSION_NT
  else
    Result := HKLM_CURRENT_VERSION_WINDOWS;
end;

{ TODO : Check for documented solution }
function GetCommonFilesFolder: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, HKLM_CURRENT_VERSION_WINDOWS,
    'CommonFilesDir', '');
end;

{$ENDIF MSWINDOWS}
{$ENDIF ~CLR}

function GetCurrentFolder: string;
{$IFDEF CLR}
begin
  Result := System.Environment.CurrentDirectory;
end;
{$ELSE ~CLR}
{$IFDEF UNIX}
const
  InitialSize = 64;
var
  Size: Integer;
begin
  Size := InitialSize;
  while True do
  begin
    SetLength(Result, Size);
    if getcwd(PChar(Result), Size) <> nil then
    begin
      StrResetLength(Result);
      Exit;
    end;
    {$IFDEF FPC}
    if GetLastOSError <> ERANGE then
    {$ELSE}
    if GetLastError <> ERANGE then
    {$ENDIF FPC}
      RaiseLastOSError;
    Size := Size * 2;
  end;
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetCurrentDirectory(0, nil);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    GetCurrentDirectory(Required, PChar(Result));
    StrResetLength(Result);
  end;
end;
{$ENDIF MSWINDOWS}
{$ENDIF ~CLR}

{$IFDEF MSWINDOWS}
{ TODO : Check for documented solution }
function GetProgramFilesFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles);
  {$ELSE ~CLR}
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, HKLM_CURRENT_VERSION_WINDOWS, 'ProgramFilesDir', '');
  {$ENDIF ~CLR}
end;

{$IFNDEF CLR}
{ TODO : Check for documented solution }
function GetWindowsFolder: string;
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetWindowsDirectory(nil, 0);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    GetWindowsDirectory(PChar(Result), Required);
    StrResetLength(Result);
  end;
end;
{$ENDIF ~CLR}

{ TODO : Check for documented solution }
function GetWindowsSystemFolder: string;
{$IFDEF CLR}
begin
  Result := System.Environment.SystemDirectory;
end;
{$ELSE ~CLR}
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetSystemDirectory(nil, 0);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    GetSystemDirectory(PChar(Result), Required);
    StrResetLength(Result);
  end;
end;
{$ENDIF ~CLR}

function GetWindowsTempFolder: string;
{$IFDEF CLR}
begin
  Result := Path.GetTempPath;
end;
{$ELSE ~CLR}
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetTempPath(0, nil);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    GetTempPath(Required, PChar(Result));
    StrResetLength(Result);
    Result := PathRemoveSeparator(Result);
  end;
end;
{$ENDIF ~CLR}

function GetDesktopFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Desktop);
  {$ELSE ~CLR}
  Result := GetSpecialFolderLocation(CSIDL_DESKTOP);
  {$ENDIF ~CLR}
end;

{ TODO : Check GetProgramsFolder = GetProgramFilesFolder }
function GetProgramsFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Programs);
  {$ELSE ~CLR}
  Result := GetSpecialFolderLocation(CSIDL_PROGRAMS);
  {$ENDIF ~CLR}
end;

{$ENDIF MSWINDOWS}
function GetPersonalFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Personal);
  {$ELSE ~CLR}
  {$IFDEF UNIX}
  Result := GetEnvironmentVariable('HOME');
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
  Result := GetSpecialFolderLocation(CSIDL_PERSONAL);
  {$ENDIF MSWINDOWS}
  {$ENDIF ~CLR}
end;

{$IFDEF MSWINDOWS}
function GetFavoritesFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Favorites);
  {$ELSE ~CLR}
  Result := GetSpecialFolderLocation(CSIDL_FAVORITES);
  {$ENDIF ~CLR}
end;

function GetStartupFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Startup);
  {$ELSE ~CLR}
  Result := GetSpecialFolderLocation(CSIDL_STARTUP);
  {$ENDIF ~CLR}
end;

function GetRecentFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Recent);
  {$ELSE ~CLR}
  Result := GetSpecialFolderLocation(CSIDL_RECENT);
  {$ENDIF ~CLR}
end;

function GetSendToFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.SendTo);
  {$ELSE ~CLR}
  Result := GetSpecialFolderLocation(CSIDL_SENDTO);
  {$ENDIF ~CLR}
end;

function GetStartmenuFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.StartMenu);
  {$ELSE ~CLR}
  Result := GetSpecialFolderLocation(CSIDL_STARTMENU);
  {$ENDIF ~CLR}
end;

function GetDesktopDirectoryFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.DesktopDirectory);
  {$ELSE ~CLR}
  Result := GetSpecialFolderLocation(CSIDL_DESKTOPDIRECTORY);
  {$ENDIF ~CLR}
end;

{$IFNDEF CLR}
{$IFNDEF FPC}
function GetCommonDocumentsFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_COMMON_DOCUMENTS);
end;
{$ENDIF ~FPC}
{$ENDIF ~CLR}

{$IFNDEF CLR}
function GetNethoodFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_NETHOOD);
end;
{$ENDIF ~CLR}

{$IFNDEF CLR}
function GetFontsFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_FONTS);
end;

function GetCommonStartmenuFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_COMMON_STARTMENU);
end;
{$ENDIF ~CLR}

function GetCommonProgramsFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.CommonProgramFiles);
  {$ELSE ~CLR}
  Result := GetSpecialFolderLocation(CSIDL_COMMON_PROGRAMS);
  {$ENDIF ~CLR}
end;

{$IFNDEF CLR}
function GetCommonStartupFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_COMMON_STARTUP);
end;
{$ENDIF ~CLR}

function GetCommonDesktopdirectoryFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.DesktopDirectory);
  {$ELSE ~CLR}
  Result := GetSpecialFolderLocation(CSIDL_COMMON_DESKTOPDIRECTORY);
  {$ENDIF ~CLR}
end;

function GetCommonAppdataFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.CommonApplicationData);
  {$ELSE ~CLR}
  Result := GetSpecialFolderLocation(CSIDL_COMMON_APPDATA);
  {$ENDIF ~CLR}
end;

function GetAppdataFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData);
  {$ELSE ~CLR}
  Result := GetSpecialFolderLocation(CSIDL_APPDATA);
  {$ENDIF ~CLR}
end;

{$IFNDEF CLR}
function GetPrinthoodFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_PRINTHOOD);
end;
{$ENDIF ~CLR}

function GetCommonFavoritesFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Favorites);
  {$ELSE ~CLR}
  Result := GetSpecialFolderLocation(CSIDL_COMMON_FAVORITES);
  {$ENDIF ~CLR}
end;

function GetTemplatesFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Templates);
  {$ELSE ~CLR}
  Result := GetSpecialFolderLocation(CSIDL_TEMPLATES);
  {$ENDIF ~CLR}
end;

function GetInternetCacheFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.InternetCache);
  {$ELSE ~CLR}
  Result := GetSpecialFolderLocation(CSIDL_INTERNET_CACHE);
  {$ENDIF ~CLR}
end;

function GetCookiesFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.Cookies);
  {$ELSE ~CLR}
  Result := GetSpecialFolderLocation(CSIDL_COOKIES);
  {$ENDIF ~CLR}
end;

function GetHistoryFolder: string;
begin
  {$IFDEF CLR}
  Result := System.Environment.GetFolderPath(Environment.SpecialFolder.History);
  {$ELSE ~CLR}
  Result := GetSpecialFolderLocation(CSIDL_HISTORY);
  {$ENDIF ~CLR}
end;

{$IFNDEF CLR}
function GetProfileFolder: string;
begin
  Result := GetSpecialFolderLocation(CSIDL_PROFILE);
end;
{$ENDIF ~CLR}

// the following special folders are pure virtual and cannot be
// mapped to a directory path:
// CSIDL_INTERNET
// CSIDL_CONTROLS
// CSIDL_PRINTERS
// CSIDL_BITBUCKET
// CSIDL_DRIVES
// CSIDL_NETWORK
// CSIDL_ALTSTARTUP
// CSIDL_COMMON_ALTSTARTUP

{$IFNDEF CLR}
// Identification
type
  TVolumeInfoKind = (vikName, vikSerial, vikFileSystem);

function GetVolumeInfoHelper(const Drive: string; InfoKind: TVolumeInfoKind): string;
var
  VolumeSerialNumber: DWORD;
  MaximumComponentLength: DWORD;
  Flags: DWORD;
  Name: array [0..MAX_PATH] of Char;
  FileSystem: array [0..15] of Char;
  ErrorMode: Cardinal;
  DriveStr: string;
begin
  { TODO : Change to RootPath }
  { TODO : Perform better checking of Drive param or document that no checking
    is performed. RM Suggested:
    DriveStr := Drive;
    if (Length(Drive) < 2) or (Drive[2] <> ':') then
      DriveStr := GetCurrentFolder;
    DriveStr  := DriveStr[1] + ':\'; }
  Result := '';
  DriveStr := Drive + ':\';
  ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    if GetVolumeInformation(PChar(DriveStr), Name, SizeOf(Name), @VolumeSerialNumber,
      MaximumComponentLength, Flags, FileSystem, SizeOf(FileSystem)) then
    case InfoKind of
      vikName:
        Result := StrPas(Name);
      vikSerial:
        begin
          Result := IntToHex(HiWord(VolumeSerialNumber), 4) + '-' +
          IntToHex(LoWord(VolumeSerialNumber), 4);
        end;
      vikFileSystem:
        Result := StrPas(FileSystem);
    end;
  finally
    SetErrorMode(ErrorMode);
  end;
end;

function GetVolumeName(const Drive: string): string;
begin
  Result := GetVolumeInfoHelper(Drive, vikName);
end;

function GetVolumeSerialNumber(const Drive: string): string;
begin
  Result := GetVolumeInfoHelper(Drive, vikSerial);
end;

function GetVolumeFileSystem(const Drive: string): string;
begin
  Result := GetVolumeInfoHelper(Drive, vikFileSystem);
end;

{ TODO -cHelp : Donator (incl. TFileSystemFlag[s]): Robert Rossmair }

function GetVolumeFileSystemFlags(const Volume: string): TFileSystemFlags;
const
  FileSystemFlags: array [TFileSystemFlag] of DWORD =
    ( FILE_CASE_SENSITIVE_SEARCH,   // fsCaseSensitive
      FILE_CASE_PRESERVED_NAMES,    // fsCasePreservedNames
      FILE_UNICODE_ON_DISK,         // fsSupportsUnicodeOnDisk
      FILE_PERSISTENT_ACLS,         // fsPersistentACLs
      FILE_FILE_COMPRESSION,        // fsSupportsFileCompression
      FILE_VOLUME_QUOTAS,           // fsSupportsVolumeQuotas
      FILE_SUPPORTS_SPARSE_FILES,   // fsSupportsSparseFiles
      FILE_SUPPORTS_REPARSE_POINTS, // fsSupportsReparsePoints
      FILE_SUPPORTS_REMOTE_STORAGE, // fsSupportsRemoteStorage
      FILE_VOLUME_IS_COMPRESSED,    // fsVolumeIsCompressed
      FILE_SUPPORTS_OBJECT_IDS,     // fsSupportsObjectIds
      FILE_SUPPORTS_ENCRYPTION,     // fsSupportsEncryption
      FILE_NAMED_STREAMS,           // fsSupportsNamedStreams
      FILE_READ_ONLY_VOLUME         // fsVolumeIsReadOnly
    );
var
  MaximumComponentLength, Flags: Cardinal;
  Flag: TFileSystemFlag;
begin
  if not GetVolumeInformation(PChar(PathAddSeparator(Volume)), nil, 0, nil,
    MaximumComponentLength, Flags, nil, 0) then
    RaiseLastOSError;
  Result := [];
  for Flag := Low(TFileSystemFlag) to High(TFileSystemFlag) do
    if (Flags and FileSystemFlags[Flag]) <> 0 then
      Include(Result, Flag);
end;

{$ENDIF ~CLR}
{$ENDIF MSWINDOWS}

{ TODO -cDoc: Contributor: twm }

function GetIPAddress(const HostName: string): string;
{$IFDEF CLR}
var
  Host: IPHostEntry;
begin
  // TODO: CLR detection:
  //   Resolve was deprecated in Framework 2.0
  //   GetHostEntry was introduced in Framework 2.0
  {$IFDEF BDS5_UP}
  Host := System.Net.Dns.GetHostEntry(HostName);
  {$ELSE ~BDS5_UP}
  Host := System.Net.Dns.Resolve(HostName);
  {$ENDIF ~BDS5_UP}
  if (Host <> nil) and (Length(Host.AddressList) > 0) then
    Result := Host.AddressList[0].ToString()
  else
    Result := '';
end;
{$ELSE ~CLR}
var
  {$IFDEF MSWINDOWS}
  R: Integer;
  WSAData: TWSAData;
  {$ENDIF MSWINDOWS}
  HostEnt: PHostEnt;
  Host: string;
  SockAddr: TSockAddrIn;
begin
  Result := '';
  {$IFDEF MSWINDOWS}
  R := WSAStartup(MakeWord(1, 1), WSAData);
  if R = 0 then
    try
  {$ENDIF MSWINDOWS}
      Host := HostName;
      if Host = '' then
      begin
        SetLength(Host, MAX_PATH);
        GetHostName(PChar(Host), MAX_PATH);
      end;
      HostEnt := GetHostByName(PChar(Host));
      if HostEnt <> nil then
      begin
        SockAddr.sin_addr.S_addr := Longint(PLongint(HostEnt^.h_addr_list^)^);
        Result := inet_ntoa(SockAddr.sin_addr);
      end;
    {$IFDEF MSWINDOWS}
    finally
      WSACleanup;
    end;
    {$ENDIF MSWINDOWS}
end;
{$ENDIF ~CLR}

{ TODO -cDoc: Donator: twm }

{$IFDEF MSWINDOWS}
{$IFNDEF CLR}
procedure GetIpAddresses(Results: TStrings);
type
  TaPInAddr = array[0..10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  R: Integer;
  HostEnt: PHostEnt;
  pptr: PaPInAddr;
  Host: string;
  i: Integer;
  WSAData: TWSAData;
begin
  //need a socket for ioctl()
  R := WSAStartup(MakeWord(1, 1), WSAData);
  if R = 0 then begin
    try
      SetLength(Host, MAX_PATH);
      GetHostName(PChar(Host), MAX_PATH);
      HostEnt := GetHostByName(PChar(Host));
      if HostEnt <> nil then begin
        pPtr := PaPInAddr(HostEnt^.h_addr_list);
        i := 0;
        while pPtr^[I] <> nil do begin
          Results.Add(inet_ntoa(pptr^[i]^));
          Inc(i);
        end;
      end;
    finally
      WSACleanup;
    end;
  end;
end;
{$ENDIF ~CLR}
{$ENDIF MSWINDOWS}

{$IFDEF UNIX}

{ TODO -cDoc: Donator: twm, Contributor rrossmair }

// Returns all IP addresses of the local machine in the form
// <interface>=<IP-Address> (which allows for access to the interface names
// by means of Results.Names and the addresses through Results.Values)
//
// Example:
//
// lo=127.0.0.1
// eth0=10.10.10.1
// ppp0=217.82.187.130
//
// note that this will append to Results!
//

procedure GetIpAddresses(Results: TStrings);
var
  Sock: Integer;
  IfReq: TIfReq;
  SockAddrPtr: PSockAddrIn;
  ListSave, IfList: PIfNameIndex;
begin
  //need a socket for ioctl()
  Sock := socket(AF_INET, SOCK_STREAM, 0);
  if Sock < 0 then
    RaiseLastOSError;

  try
    //returns pointer to dynamically allocated list of structs
    ListSave := if_nameindex();
    try
      IfList := ListSave;
      //walk thru the array returned and query for each
      //interface's address
      while IfList^.if_index <> 0 do
      begin
        //copy in the interface name to look up address of
        {$IFDEF FPC}
        strncpy(IfReq.ifr_ifrn.ifrn_name, IfList^.if_name, IFNAMSIZ);
        {$ELSE}
        strncpy(IfReq.ifrn_name, IfList^.if_name, IFNAMSIZ);
        {$ENDIF FPC}
        //get the address for this interface
        if ioctl(Sock, SIOCGIFADDR, @IfReq) <> 0 then
          RaiseLastOSError;
        //print out the address
        {$IFDEF FPC}
        SockAddrPtr := PSockAddrIn(@IfReq.ifr_ifru.ifru_addr);
        Results.Add(Format('%s=%s', [IfReq.ifr_ifrn.ifrn_name, inet_ntoa(SockAddrPtr^.sin_addr)]));
        {$ELSE}
        SockAddrPtr := PSockAddrIn(@IfReq.ifru_addr);
        Results.Add(Format('%s=%s', [IfReq.ifrn_name, inet_ntoa(SockAddrPtr^.sin_addr)]));
        {$ENDIF FPC}
        Inc(IfList);
      end;
    finally
      //free the dynamic memory kernel allocated for us
      if_freenameindex(ListSave);
    end;
  finally
    Libc.__close(Sock)
  end;
end;

{$ENDIF UNIX}

function GetLocalComputerName: string;
{$IFDEF CLR}
begin
  Result := System.Environment.MachineName;
end;
{$ELSE ~CLR}
// (rom) UNIX or LINUX?
{$IFDEF LINUX}
var
  MachineInfo: utsname;
begin
  uname(MachineInfo);
  Result := MachineInfo.nodename;
end;
{$ENDIF LINUX}
{$IFDEF MSWINDOWS}
var
  Count: DWORD;
begin
  Count := MAX_COMPUTERNAME_LENGTH + 1;
  // set buffer size to MAX_COMPUTERNAME_LENGTH + 2 characters for safety
  { TODO : Win2k solution }
  SetLength(Result, Count);
  if GetComputerName(PChar(Result), Count) then
    StrResetLength(Result)
  else
    Result := '';
end;
{$ENDIF MSWINDOWS}
{$ENDIF ~CLR}

{$IFNDEF CLR}

function GetLocalUserName: string;
{$IFDEF UNIX}
begin
  Result := GetEnv('USER');
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  Count: DWORD;
begin
  Count := 256 + 1; // UNLEN + 1
  // set buffer size to 256 + 2 characters
  { TODO : Win2k solution }
  SetLength(Result, Count);
  if GetUserName(PChar(Result), Count) then
    StrResetLength(Result)
  else
    Result := '';
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
function GetRegisteredCompany: string;
begin
  { TODO : check for MSDN documentation }
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, REG_CURRENT_VERSION, 'RegisteredOrganization', '');
end;

function GetRegisteredOwner: string;
begin
  { TODO : check for MSDN documentation }
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, REG_CURRENT_VERSION, 'RegisteredOwner', '');
end;

{ TODO: Check supported platforms, maybe complete rewrite }

function GetUserDomainName(const CurUser: string): string;
var
  Count1, Count2: DWORD;
  Sd: PSID; // PSecurityDescriptor; // FPC requires PSID
  Snu: SID_Name_Use;
begin
  Count1 := 0;
  Count2 := 0;
  Sd := nil;
  Snu := SIDTypeUser;
  LookUpAccountName(nil, PChar(CurUser), Sd, Count1, PChar(Result), Count2, Snu);
  // set buffer size to Count2 + 2 characters for safety
  SetLength(Result, Count2 + 1);
  Sd := AllocMem(Count1);
  try
    if LookUpAccountName(nil, PChar(CurUser), Sd, Count1, PChar(Result), Count2, Snu) then
      StrResetLength(Result)
    else
      Result := EmptyStr;
  finally
    FreeMem(Sd);
  end;
end;

{$ENDIF MSWINDOWS}
function GetDomainName: string;
{$IFDEF UNIX}
var
  MachineInfo: utsname;
begin
  uname(MachineInfo);
  Result := MachineInfo.domainname;
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
begin
  Result := GetUserDomainName(GetLocalUserName);
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
// Reference: How to Obtain BIOS Information from the Registry
// http://support.microsoft.com/default.aspx?scid=kb;en-us;q195268

function GetBIOSName: string;
const
  Win9xBIOSInfoKey = 'Enum\Root\*PNP0C01\0000';
begin
  if IsWinNT then
    Result := ''
  else
    Result := RegReadStringDef(HKEY_LOCAL_MACHINE, Win9xBIOSInfoKey, 'BIOSName', '');
end;

function GetBIOSCopyright: string;
const
  ADR_BIOSCOPYRIGHT = $FE091;
begin
  Result := '';
  if not IsWinNT and not IsBadReadPtr(Pointer(ADR_BIOSCOPYRIGHT), 2) then
  try
    Result := PChar(ADR_BIOSCOPYRIGHT);
  except
    Result := '';
  end;
end;

function GetBIOSExtendedInfo: string;
const
  ADR_BIOSEXTENDEDINFO = $FEC71;
begin
  Result := '';
  if not IsWinNT and not IsBadReadPtr(Pointer(ADR_BIOSEXTENDEDINFO), 2) then
  try
    Result := PChar(ADR_BIOSEXTENDEDINFO);
  except
    Result := '';
  end;
end;

// Reference: How to Obtain BIOS Information from the Registry
// http://support.microsoft.com/default.aspx?scid=kb;en-us;q195268

{ TODO : the date string can be e.g. 00/00/00 }
function GetBIOSDate: TDateTime;
const
  WinNT_REG_PATH = 'HARDWARE\DESCRIPTION\System';
  WinNT_REG_KEY  = 'SystemBiosDate';
  Win9x_REG_PATH = 'Enum\Root\*PNP0C01\0000';
  Win9x_REG_KEY  = 'BiosDate';
var
  RegStr: string;
  {$IFDEF RTL150_UP}
  FormatSettings: TFormatSettings;
  {$ELSE RTL150_UP}
  RegFormat: string;
  RegSeparator: Char;
  {$ENDIF RTL150_UP}
begin
  if IsWinNT then
    RegStr := RegReadString(HKEY_LOCAL_MACHINE, WinNT_REG_PATH, WinNT_REG_KEY)
  else
    RegStr := RegReadString(HKEY_LOCAL_MACHINE, Win9x_REG_PATH, Win9x_REG_KEY);
  {$IFDEF RTL150_UP}
  FillChar(FormatSettings, SizeOf(FormatSettings), 0);
  FormatSettings.DateSeparator := '/';
  FormatSettings.ShortDateFormat := 'm/d/y';
  if not TryStrToDate(RegStr, Result, FormatSettings) then
  begin
    FormatSettings.ShortDateFormat := 'y/m/d';
    if not TryStrToDate(RegStr, Result, FormatSettings) then
      Result := 0;
  end;
  {$ELSE RTL150_UP}
  Result := 0;
  { TODO : change to a threadsafe solution }
  RegFormat := ShortDateFormat;
  RegSeparator := DateSeparator;
  try
    DateSeparator := '/';
    try
      ShortDateFormat := 'm/d/y';
      Result := StrToDate(RegStr);
    except
      try
        ShortDateFormat := 'y/m/d';
        Result := StrToDate(RegStr);
      except
      end;
    end;
  finally
    ShortDateFormat := RegFormat;
    DateSeparator := RegSeparator;
  end;
  {$ENDIF RTL150_UP}
end;

{$ENDIF MSWINDOWS}

//=== Processes, Tasks and Modules ===========================================

{$IFDEF UNIX}
const
  CommLen = 16;  // synchronize with size of comm in struct task_struct in
                 //     /usr/include/linux/sched.h
  SProcDirectory = '/proc';

function RunningProcessesList(const List: TStrings; FullPath: Boolean): Boolean;
var
  ProcDir: PDirectoryStream;
  PtrDirEnt: PDirEnt;
  Scratch: TDirEnt;
  ProcID: __pid_t;
  E: Integer;
  FileName: string;
  F: PIOFile;
begin
  Result := False;
  ProcDir := opendir(SProcDirectory);
  if ProcDir <> nil then
  begin
    PtrDirEnt := nil;
    {$IFDEF FPC}
    if readdir_r(ProcDir, @Scratch, @PtrDirEnt) <> 0 then
      Exit;
    {$ELSE}
    if readdir_r(ProcDir, @Scratch, PtrDirEnt) <> 0 then
      Exit;
    {$ENDIF FPC}
    List.BeginUpdate;
    try
      while PtrDirEnt <> nil do
      begin
        Val(PtrDirEnt^.d_name, ProcID, E);
        if E = 0 then // name was process id
        begin
          FileName := '';

          if FullPath then
            FileName := SymbolicLinkTarget(Format('/proc/%s/exe', [PtrDirEnt^.d_name]));

          if FileName = '' then // usually due to insufficient access rights
          begin
            // read stat
            FileName := Format('/proc/%s/stat', [PtrDirEnt^.d_name]);
            F := fopen(PChar(FileName), 'r');
            if F = nil then
              raise EJclError.CreateResFmt(@RsInvalidProcessID, [ProcID]);
            try
              SetLength(FileName, CommLen);
              if fscanf(F, PChar(Format('%%*d (%%%d[^)])', [CommLen])), PChar(FileName)) <> 1 then
                RaiseLastOSError;
              StrResetLength(FileName);
            finally
              fclose(F);
            end;
          end;

          List.AddObject(FileName, Pointer(ProcID));
        end;
        {$IFDEF FPC}
        if readdir_r(ProcDir, @Scratch, @PtrDirEnt) <> 0 then
          Break;
        {$ELSE}
        if readdir_r(ProcDir, @Scratch, PtrDirEnt) <> 0 then
          Break;
        {$ENDIF FPC}
      end;
    finally
      List.EndUpdate;
    end;
  end;
end;

{$ENDIF UNIX}
{$ENDIF ~CLR}

{$IFDEF MSWINDOWS}

function RunningProcessesList(const List: TStrings; FullPath: Boolean): Boolean;
{$IFDEF CLR}
var
  Processes: array of Process;
  I: Integer;
  HasModules: Boolean;
begin
  Result := True;
  HasModules := False;
  Processes := Process.GetProcesses;
  for I := 0 to High(Processes) do
  begin
    try
      HasModules := Processes[I].Modules.Count > 0;
    except
      on Win32Exception do
        HasModules := False;
    end;
    if not HasModules then
      List.Add(Processes[I].ProcessName)
    else
     if FullPath then
      List.Add(Processes[I].MainModule.FileName)
    else
      List.Add(Processes[I].MainModule.ModuleName);
  end;
end;
{$ELSE ~CLR}

  // This function always returns an empty string on Win9x
  function ProcessFileName(PID: DWORD): string;
  var
    Handle: THandle;
  begin
    Result := '';
    Handle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
    if Handle <> 0 then
    try
      SetLength(Result, MAX_PATH);
      if FullPath then
      begin
        if GetModuleFileNameEx(Handle, 0, PChar(Result), MAX_PATH) > 0 then
          StrResetLength(Result)
        else
          Result := '';
      end
      else
      begin
        if GetModuleBaseNameA(Handle, 0, PChar(Result), MAX_PATH) > 0 then
          StrResetLength(Result)
        else
          Result := '';
      end;
    finally
      CloseHandle(Handle);
    end;
  end;

  { TODO: Check return value of CreateToolhelp32Snapshot on Windows NT (0?) }
  function BuildListTH: Boolean;
  var
    SnapProcHandle: THandle;
    ProcEntry: TProcessEntry32;
    NextProc: Boolean;
    FileName: string;
  begin
    SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    Result := (SnapProcHandle <> INVALID_HANDLE_VALUE);
    if Result then
    try
      ProcEntry.dwSize := SizeOf(ProcEntry);
      NextProc := Process32First(SnapProcHandle, ProcEntry);
      while NextProc do
      begin
        if ProcEntry.th32ProcessID = 0 then
        begin
          // PID 0 is always the "System Idle Process" but this name cannot be
          // retrieved from the system and has to be fabricated.
          FileName := RsSystemIdleProcess;
        end
        else
        begin
          if IsWin2k or IsWinXP or IsWin2003 or IsWin2003R2 or IsWinXP64 or
            IsWinVista or IsWinServer2008 then
          begin
            FileName := ProcessFileName(ProcEntry.th32ProcessID);
            if FileName = '' then
              FileName := ProcEntry.szExeFile;
          end
          else
          begin
            FileName := ProcEntry.szExeFile;
            if not FullPath then
              FileName := ExtractFileName(FileName);
          end;
        end;
        List.AddObject(FileName, Pointer(ProcEntry.th32ProcessID));
        NextProc := Process32Next(SnapProcHandle, ProcEntry);
      end;
    finally
      CloseHandle(SnapProcHandle);
    end;
  end;

  function BuildListPS: Boolean;
  var
    PIDs: array [0..1024] of DWORD;
    Needed: DWORD;
    I: Integer;
    FileName: string;
  begin
    Result := EnumProcesses(@PIDs, SizeOf(PIDs), Needed);
    if Result then
    begin
      for I := 0 to (Needed div SizeOf(DWORD)) - 1 do
      begin
        case PIDs[I] of
          0:
            // PID 0 is always the "System Idle Process" but this name cannot be
            // retrieved from the system and has to be fabricated.
            FileName := RsSystemIdleProcess;
          2:
            // On NT 4 PID 2 is the "System Process" but this name cannot be
            // retrieved from the system and has to be fabricated.
            if IsWinNT4 then
              FileName := RsSystemProcess
            else
              FileName := ProcessFileName(PIDs[I]);
          8:
            // On Win2K PID 8 is the "System Process" but this name cannot be
            // retrieved from the system and has to be fabricated.
            if IsWin2k or IsWinXP then
              FileName := RsSystemProcess
            else
              FileName := ProcessFileName(PIDs[I]);
        else
          FileName := ProcessFileName(PIDs[I]);
        end;
        if FileName <> '' then
          List.AddObject(FileName, Pointer(PIDs[I]));
      end;
    end;
  end;

begin
  { TODO : safer solution? }
  List.BeginUpdate;
  try
    if GetWindowsVersion in [wvWinNT31, wvWinNT35, wvWinNT351, wvWinNT4] then
      Result := BuildListPS
    else
      Result := BuildListTH;
  finally
    List.EndUpdate;
  end;
end;
{$ENDIF ~CLR}

{$IFNDEF CLR}

{ TODO Windows 9x ? }

function LoadedModulesList(const List: TStrings; ProcessID: DWORD; HandlesOnly: Boolean): Boolean;

  procedure AddToList(ProcessHandle: THandle; Module: HMODULE);
  var
    FileName: array [0..MAX_PATH] of Char;
    ModuleInfo: TModuleInfo;
  begin
    {$IFDEF FPC}
    if GetModuleInformation(ProcessHandle, Module, ModuleInfo, SizeOf(ModuleInfo)) then
    {$ELSE ~FPC}
    if GetModuleInformation(ProcessHandle, Module, @ModuleInfo, SizeOf(ModuleInfo)) then
    {$ENDIF ~FPC}
    begin
      if HandlesOnly then
        List.AddObject('', Pointer(ModuleInfo.lpBaseOfDll))
      else
      if GetModuleFileNameEx(ProcessHandle, Module, Filename, SizeOf(Filename)) > 0 then
        List.AddObject(FileName, Pointer(ModuleInfo.lpBaseOfDll));
    end;
  end;

  function EnumModulesVQ(ProcessHandle: THandle): Boolean;
  var
    MemInfo: TMemoryBasicInformation;
    Base: PChar;
    LastAllocBase: Pointer;
    Res: DWORD;
  begin
    Base := nil;
    LastAllocBase := nil;
    FillChar(MemInfo, SizeOf(MemInfo), #0);
    Res := VirtualQueryEx(ProcessHandle, Base, MemInfo, SizeOf(MemInfo));
    Result := (Res = SizeOf(MemInfo));
    while Res = SizeOf(MemInfo) do
    begin
      if MemInfo.AllocationBase <> LastAllocBase then
      begin
        {$IFDEF FPC}
        if MemInfo._Type = MEM_IMAGE then
        {$ELSE ~FPC}
        if MemInfo.Type_9 = MEM_IMAGE then
        {$ENDIF ~FPC}
          AddToList(ProcessHandle, HMODULE(MemInfo.AllocationBase));
        LastAllocBase := MemInfo.AllocationBase;
      end;
      Inc(Base, MemInfo.RegionSize);
      Res := VirtualQueryEx(ProcessHandle, Base, MemInfo, SizeOf(MemInfo));
    end;
  end;

  function EnumModulesPS: Boolean;
  var
    ProcessHandle: THandle;
    Needed: DWORD;
    Modules: array of THandle;
    I, Cnt: Integer;
  begin
    Result := False;
    ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, ProcessID);
    if ProcessHandle <> 0 then
    try
      Result := EnumProcessModules(ProcessHandle, nil, 0, Needed);
      if Result then
      begin
        Cnt := Needed div SizeOf(HMODULE);
        SetLength(Modules, Cnt);
        if EnumProcessModules(ProcessHandle, @Modules[0], Needed, Needed) then
          for I := 0 to Cnt - 1 do
            AddToList(ProcessHandle, Modules[I]);
      end
      else
        Result := EnumModulesVQ(ProcessHandle);
    finally
      CloseHandle(ProcessHandle);
    end;
  end;

 { TODO: Check return value of CreateToolhelp32Snapshot on Windows NT (0?) }

  function EnumModulesTH: Boolean;
  var
    SnapProcHandle: THandle;
    Module: TModuleEntry32;
    Next: Boolean;
  begin
    SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, ProcessID);
    Result := (SnapProcHandle <> INVALID_HANDLE_VALUE);
    if Result then
    try
      FillChar(Module, SizeOf(Module), #0);
      Module.dwSize := SizeOf(Module);
      Next := Module32First(SnapProcHandle, Module);
      while Next do
      begin
        if HandlesOnly then
          List.AddObject('', Pointer(Module.hModule))
        else
          List.AddObject(Module.szExePath, Pointer(Module.hModule));
        Next := Module32Next(SnapProcHandle, Module);
      end;
    finally
      CloseHandle(SnapProcHandle);
    end;
  end;

begin
  List.BeginUpdate;
  try
    if IsWinNT then
      Result := EnumModulesPS
    else
      Result := EnumModulesTH;
  finally
    List.EndUpdate;
  end;
end;

function GetTasksList(const List: TStrings): Boolean;

  function EnumWindowsProc(Wnd: THandle; List: TStrings): Boolean; stdcall;
  var
    Caption: array [0..1024] of Char;
  begin
    if IsMainAppWindow(Wnd) and (GetWindowText(Wnd, Caption, SizeOf(Caption)) > 0) then
      List.AddObject(Caption, Pointer(Wnd));
    Result := True;
  end;

begin
  List.BeginUpdate;
  try
    Result := EnumWindows(@EnumWindowsProc, Integer(List));
  finally
    List.EndUpdate;
  end;
end;

function ModuleFromAddr(const Addr: Pointer): HMODULE;
var
  MI: TMemoryBasicInformation;
begin
  VirtualQuery(Addr, MI, SizeOf(MI));
  if MI.State <> MEM_COMMIT then
    Result := 0
  else
    Result := HMODULE(MI.AllocationBase);
end;

{$IFNDEF FPC}
function IsSystemModule(const Module: HMODULE): Boolean;
var
  CurModule: PLibModule;
begin
  Result := False;
  if Module <> 0 then
  begin
    CurModule := LibModuleList;
    while CurModule <> nil do
    begin
      if CurModule.Instance = Module then
      begin
        Result := True;
        Break;
      end;
      CurModule := CurModule.Next;
    end;
  end;
end;
{$ENDIF ~FPC}

// Reference: http://msdn.microsoft.com/library/periodic/period97/win321197.htm
{ TODO : wrong link }

function IsMainAppWindow(Wnd: THandle): Boolean;
var
  ParentWnd: THandle;
  ExStyle: DWORD;
begin
  if IsWindowVisible(Wnd) then
  begin
    ParentWnd := GetWindowLong(Wnd, GWL_HWNDPARENT);
    ExStyle := GetWindowLong(Wnd, GWL_EXSTYLE);
    Result := ((ParentWnd = 0) or (ParentWnd = GetDesktopWindow)) and
      ((ExStyle and WS_EX_TOOLWINDOW = 0) or (ExStyle and WS_EX_APPWINDOW <> 0));
  end
  else
    Result := False;
end;

function IsWindowResponding(Wnd: THandle; Timeout: Integer): Boolean;
var
  Res: DWORD;
begin
  Result := SendMessageTimeout(Wnd, WM_NULL, 0, 0, SMTO_ABORTIFHUNG, Timeout, Res) <> 0;
end;

function GetWindowIcon(Wnd: THandle; LargeIcon: Boolean): HICON;
var
  Width, Height: Integer;
  TempIcon: HICON;
  IconType: DWORD;
begin
  if LargeIcon then
  begin
    Width := GetSystemMetrics(SM_CXICON);
    Height := GetSystemMetrics(SM_CYICON);
    IconType := ICON_BIG;
    TempIcon := GetClassLong(Wnd, GCL_HICON);
  end
  else
  begin
    Width := GetSystemMetrics(SM_CXSMICON);
    Height := GetSystemMetrics(SM_CYSMICON);
    IconType := ICON_SMALL;
    TempIcon := GetClassLong(Wnd, GCL_HICONSM);
  end;
  if TempIcon = 0 then
    TempIcon := SendMessage(Wnd, WM_GETICON, IconType, 0);
  if (TempIcon = 0) and not LargeIcon then
    TempIcon := SendMessage(Wnd, WM_GETICON, ICON_BIG, 0);
  Result := CopyImage(TempIcon, IMAGE_ICON, Width, Height, 0);
end;

function GetWindowCaption(Wnd: THandle): string;
const
  BufferAllocStep = 256;
var
  Buffer: PChar;
  Size, TextLen: Integer;
begin
  { TODO : use string }
  Result := '';
  Buffer := nil;
  try
    Size := GetWindowTextLength(Wnd) + 2 - BufferAllocStep;
    repeat
      Inc(Size, BufferAllocStep);
      ReallocMem(Buffer, Size);
      TextLen := GetWindowText(Wnd, Buffer, Size);
    until TextLen < Size - 1;
    if TextLen > 0 then
      Result := Buffer;
  finally
    FreeMem(Buffer);
  end;
end;

// Q178893
// http://support.microsoft.com/default.aspx?scid=kb;en-us;178893

function TerminateApp(ProcessID: DWORD; Timeout: Integer): TJclTerminateAppResult;
var
  ProcessHandle: THandle;

  function EnumWindowsProc(Wnd: THandle; ProcessID: DWORD): Boolean; stdcall;
  var
    PID: DWORD;
  begin
    GetWindowThreadProcessId(Wnd, @PID);
    if ProcessID = PID then
      PostMessage(Wnd, WM_CLOSE, 0, 0);
    Result := True;
  end;

begin
  Result := taError;
  if ProcessID <> GetCurrentProcessId then
  begin
    ProcessHandle := OpenProcess(SYNCHRONIZE or PROCESS_TERMINATE, False, ProcessID);
    if ProcessHandle <> 0 then
    try
      EnumWindows(@EnumWindowsProc, LPARAM(ProcessID));
      if WaitForSingleObject(ProcessHandle, Timeout) = WAIT_OBJECT_0 then
        Result := taClean
      else
      if TerminateProcess(ProcessHandle, 0) then
        Result := taKill;
    finally
      CloseHandle(ProcessHandle);
    end;
  end;
end;

function TerminateTask(Wnd: THandle; Timeout: Integer): TJclTerminateAppResult;
var
  PID: DWORD;
begin
  if GetWindowThreadProcessId(Wnd, @PID) <> 0 then
    Result := TerminateApp(PID, Timeout)
  else
    Result := taError;
end;

function GetProcessNameFromWnd(Wnd: THandle): string;
var
  List: TStringList;
  PID: DWORD;
  I: Integer;
begin
  Result := '';
  if IsWindow(Wnd) then
  begin
    PID := INVALID_HANDLE_VALUE;
    GetWindowThreadProcessId(Wnd, @PID);
    List := TStringList.Create;
    try
      if RunningProcessesList(List, True) then
      begin
        I := List.IndexOfObject(Pointer(PID));
        if I > -1 then
          Result := List[I];
      end;
    finally
      List.Free;
    end;
  end;
end;

function GetPidFromProcessName(const ProcessName: string): DWORD;
var
  List: TStringList;
  I: Integer;
  HasFullPath: Boolean;
begin
  Result := INVALID_HANDLE_VALUE;
  List := TStringList.Create;
  try
    HasFullPath := ExtractFilePath(ProcessName) <> '';
    if RunningProcessesList(List, HasFullPath) then
    begin
      I := List.IndexOf(ProcessName);
      if I > -1 then
        Result := DWORD(List.Objects[I]);
    end;
  finally
    List.Free;
  end;
end;

function GetProcessNameFromPid(PID: DWORD): string;
var
  List: TStringList;
  I: Integer;
begin
  // Note: there are other ways to retrieve the name of the process given it's
  // PID but this implementation seems to work best without making assumptions
  // although it may not be the most efficient implementation.
  Result := '';
  List := TStringList.Create;
  try
    if RunningProcessesList(List, True) then
    begin
      I := List.IndexOfObject(Pointer(PID));
      if I > -1 then
        Result := List[I];
    end;
  finally
    List.Free;
  end;
end;

function GetMainAppWndFromPid(PID: DWORD): THandle;
type
  PSearch = ^TSearch;
  TSearch = record
    PID: DWORD;
    Wnd: THandle;
  end;
var
  SearchRec: TSearch;

  function EnumWindowsProc(Wnd: THandle; Res: PSearch): Boolean; stdcall;
  var
    WindowPid: DWORD;
  begin
    WindowPid := 0;
    GetWindowThreadProcessId(Wnd, @WindowPid);
    if (WindowPid = Res^.PID) and IsMainAppWindow(Wnd) then
    begin
      Res^.Wnd := Wnd;
      Result := False;
    end
    else
      Result := True;
  end;

begin
  SearchRec.PID := PID;
  SearchRec.Wnd := 0;
  EnumWindows(@EnumWindowsProc, Integer(@SearchRec));
  Result := SearchRec.Wnd;
end;

function GetShellProcessName: string;
const
  cShellKey = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\WinLogon';
  cShellValue = 'Shell';
  cShellDefault = 'explorer.exe';
  cShellSystemIniFileName = 'system.ini';
  cShellBootSection = 'boot';
begin
  if IsWinNT then
    Result := RegReadStringDef(HKEY_LOCAL_MACHINE, cShellKey, cShellValue, '')
  else
    Result := IniReadString(PathAddSeparator(GetWindowsFolder) + cShellSystemIniFileName, cShellBootSection, cShellValue);
  if Result = '' then
    Result := cShellDefault;
end;

function GetShellProcessHandle: THandle;
var
  Pid: Longword;
begin
  Pid := GetPidFromProcessName(GetShellProcessName);
  Result := OpenProcess(PROCESS_ALL_ACCESS, False, Pid);
  if Result = 0 then
    RaiseLastOSError;
end;

//=== Version Information ====================================================

{ Q159/238

  Windows 95 retail, OEM    4.00.950                      7/11/95
  Windows 95 retail SP1     4.00.950A                     7/11/95-12/31/95
  OEM Service Release 2     4.00.1111* (4.00.950B)        8/24/96
  OEM Service Release 2.1   4.03.1212-1214* (4.00.950B)   8/24/96-8/27/97
  OEM Service Release 2.5   4.03.1214* (4.00.950C)        8/24/96-11/18/97
  Windows 98 retail, OEM    4.10.1998                     5/11/98
  Windows 98 Second Edition 4.10.2222A                    4/23/99
  Windows Millennium        4.90.3000
}
{ TODO : Distinquish between all these different releases? }

var
  KernelVersionHi: DWORD;

function GetWindowsVersion: TWindowsVersion;
var
  TrimmedWin32CSDVersion: string;
  SystemInfo: TSystemInfo;
  OSVersionInfoEx: TOSVersionInfoEx;
const
  SM_SERVERR2 = 89;
begin
  Result := wvUnknown;
  TrimmedWin32CSDVersion := Trim(Win32CSDVersion);
  case Win32Platform of
    VER_PLATFORM_WIN32_WINDOWS:
      case Win32MinorVersion of
        0..9:
          if (TrimmedWin32CSDVersion = 'B') or (TrimmedWin32CSDVersion = 'C') then
            Result := wvWin95OSR2
          else
            Result := wvWin95;
        10..89:
          // On Windows ME Win32MinorVersion can be 10 (indicating Windows 98
          // under certain circumstances (image name is setup.exe). Checking
          // the kernel version is one way of working around that.
          if KernelVersionHi = $0004005A then // 4.90.x.x
            Result := wvWinME
          else
          if (TrimmedWin32CSDVersion = 'A') or (TrimmedWin32CSDVersion = 'B') then
            Result := wvWin98SE
          else
            Result := wvWin98;
        90:
          Result := wvWinME;
      end;
    VER_PLATFORM_WIN32_NT:
      case Win32MajorVersion of
        3:
          case Win32MinorVersion of
            1:
              Result := wvWinNT31;
            5:
              Result := wvWinNT35;
            51:
              Result := wvWinNT351;
          end;
        4:
          Result := wvWinNT4;
        5:
          case Win32MinorVersion of
            0:
              Result := wvWin2000;
            1:
              Result := wvWinXP;
            2:
              begin
                OSVersionInfoEx.dwOSVersionInfoSize := SizeOf(OSVersionInfoEx);
                GetNativeSystemInfo(SystemInfo);
                if GetSystemMetrics(SM_SERVERR2) <> 0 then
                  Result := wvWin2003R2
                else
                if (SystemInfo.wProcessorArchitecture <> PROCESSOR_ARCHITECTURE_INTEL) and
                  GetVersionEx(OSVersionInfoEx) and (OSVersionInfoEx.wProductType = VER_NT_WORKSTATION) then
                  Result := wvWinXP64
                else
                  Result := wvWin2003;
              end;
          end;
        6:
          if Win32MinorVersion = 0 then
          begin
            OSVersionInfoEx.dwOSVersionInfoSize := SizeOf(OSVersionInfoEx);
            if GetVersionEx(OSVersionInfoEx) and (OSVersionInfoEx.wProductType = VER_NT_WORKSTATION) then
              Result := wvWinVista
            else
              Result := wvWinServer2008;
          end;
      end;
  end;
end;

function NtProductType: TNtProductType;
const
  ProductType = 'SYSTEM\CurrentControlSet\Control\ProductOptions';
var
  Product: string;
  OSVersionInfo: TOSVersionInfoEx;
  SystemInfo: TSystemInfo;
begin
  Result := ptUnknown;
  FillChar(OSVersionInfo, SizeOf(OSVersionInfo), 0);
  FillChar(SystemInfo, SizeOf(SystemInfo), 0);
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  GetNativeSystemInfo(SystemInfo);

  // Favor documented API over registry
  if IsWinNT4 and (GetWindowsServicePackVersion >= 6) then
  begin
    if GetVersionEx(OSVersionInfo) then
    begin
      if (OSVersionInfo.wProductType = VER_NT_WORKSTATION) then
        Result := ptWorkstation
      else
      if (OSVersionInfo.wSuiteMask and VER_SUITE_ENTERPRISE) = VER_SUITE_ENTERPRISE then
        Result := ptEnterprise
      else
        Result := ptServer;
    end;
  end
  else
  if IsWin2K then
  begin
    if GetVersionEx(OSVersionInfo) then
    begin
      if OSVersionInfo.wProductType  in [VER_NT_SERVER,VER_NT_DOMAIN_CONTROLLER] then
      begin
        if (OSVersionInfo.wSuiteMask and VER_SUITE_DATACENTER) <> 0 then
          Result := ptDatacenterServer
        else
        if (OSVersionInfo.wSuiteMask and VER_SUITE_ENTERPRISE) <> 0 then
          Result := ptAdvancedServer
        else
          Result := ptServer;
      end
      else
        Result := ptProfessional;
    end;
  end
  else
  if IsWinXP64 or IsWin2003 or IsWin2003R2 then // all (5.2)
  begin
    if GetVersionEx(OSVersionInfo) then
    begin
      if OSVersionInfo.wProductType in [VER_NT_SERVER,VER_NT_DOMAIN_CONTROLLER] then
      begin
        if (OSVersionInfo.wSuiteMask and VER_SUITE_DATACENTER) = VER_SUITE_DATACENTER then
          Result := ptDatacenterServer
        else
        if (OSVersionInfo.wSuiteMask and VER_SUITE_ENTERPRISE) = VER_SUITE_ENTERPRISE then
          Result := ptEnterprise
        else
        if (OSVersionInfo.wSuiteMask = VER_SUITE_BLADE) then
          Result := ptWebEdition
        else
          Result := ptServer;
      end
      else
      if (OSVersionInfo.wProductType = VER_NT_WORKSTATION) then
        Result := ptProfessional;
    end;
  end
  else
  if IsWinXP or IsWinVista or IsWinServer2008 then // workstation
  begin
    if GetVersionEx(OSVersionInfo) then
    begin
      if OSVersionInfo.wProductType = VER_NT_WORKSTATION then
      begin
        if (OSVersionInfo.wSuiteMask and VER_SUITE_PERSONAL) = VER_SUITE_PERSONAL then
          Result := ptPersonal
        else
          Result := ptProfessional;
      end;
    end;
  end;

  if Result = ptUnknown then
  begin
    // Non Windows 2000/XP system or the above method failed, try registry
    Product := RegReadStringDef(HKEY_LOCAL_MACHINE, ProductType, 'ProductType', '');
    if CompareText(Product, 'WINNT') = 0 then
      Result :=  ptWorkStation
    else
    if CompareText(Product, 'SERVERNT') = 0 then
      Result := {ptServer} ptAdvancedServer
    else
    if CompareText(Product, 'LANMANNT') = 0 then
      Result := {ptAdvancedServer} ptServer
    else
      Result := ptUnknown;
  end;
end;

function GetWindowsVersionString: string;
begin
  case GetWindowsVersion of
    wvWin95:
      Result := RsOSVersionWin95;
    wvWin95OSR2:
      Result := RsOSVersionWin95OSR2;
    wvWin98:
      Result := RsOSVersionWin98;
    wvWin98SE:
      Result := RsOSVersionWin98SE;
    wvWinME:
      Result := RsOSVersionWinME;
    wvWinNT31, wvWinNT35, wvWinNT351:
      Result := Format(RsOSVersionWinNT3, [Win32MinorVersion]);
    wvWinNT4:
      Result := Format(RsOSVersionWinNT4, [Win32MinorVersion]);
    wvWin2000:
      Result := RsOSVersionWin2000;
    wvWinXP:
      Result := RsOSVersionWinXP;
    wvWin2003:
      Result := RsOSVersionWin2003;
    wvWin2003R2:
      Result := RsOSVersionWin2003R2;
    wvWinXP64:
      Result := RsOSVersionWinXP64;
    wvWinServer2008:
      Result := RsOSVersionWinServer2008;
    wvWinVista:
      Result := RsOSVersionWinVista;
  else
    Result := '';
  end;
end;

function NtProductTypeString: string;
begin
  case NtProductType of
   ptWorkStation:
     Result := RsProductTypeWorkStation;
   ptServer:
     Result := RsProductTypeServer;
   ptAdvancedServer:
     Result := RsProductTypeAdvancedServer;
   ptPersonal:
     Result := RsProductTypePersonal;
   ptProfessional:
     Result := RsProductTypeProfessional;
   ptDatacenterServer:
     Result := RsProductTypeDatacenterServer;
   ptEnterprise:
     Result := RsProductTypeEnterprise;
   ptWebEdition:
     Result := RsProductTypeWebEdition;
  else
    Result := '';
  end;
end;

function GetWindowsServicePackVersion: Integer;
const
  RegWindowsControl = 'SYSTEM\CurrentControlSet\Control\Windows';
var
  SP: Integer;
  VersionInfo: TOSVersionInfoEx;
begin
  Result := 0;
  if IsWin2K or IsWinXP or IsWin2003 or IsWinXP64 or IsWin2003R2 or IsWinVista or IsWinServer2008 then
  begin
    FillChar(VersionInfo, SizeOf(VersionInfo), 0);
    VersionInfo.dwOSVersionInfoSize := SizeOf(VersionInfo);
    if GetVersionEx(VersionInfo) then
      Result := VersionInfo.wServicePackMajor;
    end
  else
  begin
    SP := RegReadIntegerDef(HKEY_LOCAL_MACHINE, RegWindowsControl, 'CSDVersion', 0);
    Result := StrToInt(IntToHex(SP, 4)) div 100;
  end;
end;

function GetWindowsServicePackVersionString: string;
var
  SP: Integer;
begin
  SP := GetWindowsServicePackVersion;
  if SP > 0 then
    Result := Format(RsSPInfo, [SP])
  else
    Result := '';
end;

// Imports copied from OpenGL unit. Direct using of OpenGL unit might cause unexpected problems due
// setting 8087CW in the intialization section
{
function glGetString(name: Cardinal): PChar; stdcall; external opengl32;
function glGetError: Cardinal; stdcall; external opengl32;
function gluErrorString(errCode: Cardinal): PChar; stdcall; external 'glu32.dll';
}

type
  TglGetStringFunc = function(name: Cardinal): PChar; stdcall;
  TglGetErrorFunc = function: Cardinal; stdcall;
  TgluErrorStringFunc = function(errCode: Cardinal): PChar; stdcall;

  TwglCreateContextFunc = function(DC: HDC): HGLRC; stdcall;
  TwglDeleteContextFunc = function(p1: HGLRC): BOOL; stdcall;
  TwglMakeCurrentFunc = function(DC: HDC; p2: HGLRC): BOOL; stdcall;

const
  glu32 = 'glu32.dll'; // do not localize
  glGetStringName = 'glGetString'; // do not localize
  glGetErrorName = 'glGetError'; // do not localize
  gluErrorStringName = 'gluErrorString'; // do not localize
  wglCreateContextName = 'wglCreateContext'; // do not localize
  wglDeleteContextName = 'wglDeleteContext'; // do not localize
  wglMakeCurrentName = 'wglMakeCurrent'; // do not localize
  ChoosePixelFormatName = 'ChoosePixelFormat'; // do not localize
  SetPixelFormatName = 'SetPixelFormat'; // do not localize

function GetOpenGLVersion(const Win: THandle; out Version, Vendor: AnsiString): Boolean;
const
  GL_NO_ERROR = 0;
  GL_VENDOR   = $1F00;
  GL_VERSION  = $1F02;
var
  OpenGlLib, Glu32Lib: HModule;

  glGetStringFunc: TglGetStringFunc;
  glGetErrorFunc: TglGetErrorFunc;
  gluErrorStringFunc: TgluErrorStringFunc;

  wglCreateContextFunc: TwglCreateContextFunc;
  wglDeleteContextFunc: TwglDeleteContextFunc;
  wglMakeCurrentFunc: TwglMakeCurrentFunc;

  pfd: TPixelFormatDescriptor;
  iFormatIndex: Integer;
  hGLContext: HGLRC;
  hGLDC: HDC;
  pcTemp: PChar;
  glErr: Cardinal;
  bError: Boolean;
  sOpenGLVersion, sOpenGLVendor: string;
  Save8087CW: Word;

  procedure FunctionFailedError(Name: string);
  begin
    raise EJclError.CreateResFmt(@RsEOpenGLInfo, [Name]);
  end;

begin
  @glGetStringFunc := nil;
  @glGetErrorFunc := nil;
  @gluErrorStringFunc := nil;

  @wglCreateContextFunc := nil;
  @wglDeleteContextFunc := nil;
  @wglMakeCurrentFunc := nil;

  Glu32Lib := 0;
  OpenGlLib := SafeLoadLibrary(opengl32);
  try
    if OpenGlLib <> 0 then
    begin
      Glu32Lib := SafeLoadLibrary(glu32); // do not localize
      if (OpenGlLib <> 0) and (Glu32Lib <> 0) then
      begin
        glGetStringFunc := GetProcAddress(OpenGlLib, glGetStringName);
        glGetErrorFunc := GetProcAddress(OpenGlLib, glGetErrorName);
        gluErrorStringFunc := GetProcAddress(Glu32Lib, gluErrorStringName);

        wglCreateContextFunc := GetProcAddress(OpenGlLib, wglCreateContextName);
        wglDeleteContextFunc := GetProcAddress(OpenGlLib, wglDeleteContextName);
        wglMakeCurrentFunc := GetProcAddress(OpenGlLib, wglMakeCurrentName);
      end;
    end;

    if not (Assigned(glGetStringFunc) and Assigned(glGetErrorFunc) and Assigned(gluErrorStringFunc) and
            Assigned(wglCreateContextFunc) and Assigned(wglDeleteContextFunc) and Assigned(wglMakeCurrentFunc)) then
    begin
      @glGetStringFunc := nil;
      Result := False;
      Vendor := RsOpenGLInfoError;
      Version := RsOpenGLInfoError;
      Exit;
    end;

    { To call for the version information string we must first have an active
      context established for use.  We can, of course, close this after use }
    Save8087CW := Get8087ControlWord;
    try
      Set8087CW($133F);
      hGLContext := 0;
      Result := False;
      bError := False;

      if Win = 0 then
      begin
        Result := False;
        Vendor := RsOpenGLInfoError;
        Version := RsOpenGLInfoError;
        Exit;
      end;

      FillChar(pfd, SizeOf(pfd), 0);
      with pfd do
      begin
        nSize := SizeOf(pfd);
        nVersion := 1;  { The Current Version of the descriptor is 1 }
        dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL;
        iPixelType := PFD_TYPE_RGBA;
        cColorBits := 24;  { support 24-bit colour }
        cDepthBits := 32;  { Depth of the z-buffer }
        iLayerType := PFD_MAIN_PLANE;
      end;

      hGLDC := GetDC(Win);
      try
        iFormatIndex := ChoosePixelFormat(hGLDC, @pfd);
        if iFormatIndex = 0 then
          FunctionFailedError(ChoosePixelFormatName);

        if not SetPixelFormat(hGLDC, iFormatIndex, @pfd) then
          FunctionFailedError(SetPixelFormatName);

        hGLContext := wglCreateContextFunc(hGLDC);
        if hGLContext = 0 then
          FunctionFailedError(wglCreateContextName);

        if not wglMakeCurrentFunc(hGLDC, hGLContext) then
          FunctionFailedError(wglMakeCurrentName);

        { TODO : Review the following.  Not sure I am 100% happy with this code
                 in its current structure. }
        pcTemp := glGetStringFunc(GL_VERSION);
        if pcTemp <> nil then
        begin
          { TODO : Store this information in a Global Variable, and return that??
                   This would save this work being performed again with later calls }
          sOpenGLVersion := StrPas(pcTemp);
        end
        else
        begin
          bError := True;
          glErr := glGetErrorFunc;
          if glErr <> GL_NO_ERROR then
          begin
            sOpenGLVersion := gluErrorStringFunc(glErr);
            sOpenGLVendor := '';
          end;
        end;

        pcTemp := glGetStringFunc(GL_VENDOR);
        if pcTemp <> nil then
        begin
          { TODO : Store this information in a Global Variable, and return that??
                   This would save this work being performed again with later calls }
          sOpenGLVendor := StrPas(pcTemp);
        end
        else
        begin
          bError := True;
          glErr := glGetErrorFunc;
          if glErr <> GL_NO_ERROR then
          begin
            sOpenGLVendor := gluErrorStringFunc(glErr);
            Exit;
          end;
        end;

        Result := (not bError);
        Version := sOpenGLVersion;
        Vendor := sOpenGLVendor;
      finally
        { Close all resources }
        wglMakeCurrentFunc(hGLDC, 0);
        if hGLContext <> 0 then
          wglDeleteContextFunc(hGLContext);
      end;
    finally
      Set8087CW(Save8087CW);
    end;
  finally
    if (OpenGlLib <> 0) then
      FreeLibrary(OpenGlLib);
    if (Glu32Lib <> 0) then
      FreeLibrary(Glu32Lib);
  end;
end;

function GetNativeSystemInfo(var SystemInfo: TSystemInfo): Boolean;
type
  TGetNativeSystemInfo = procedure (var SystemInfo: TSystemInfo) stdcall;
var
  LibraryHandle: HMODULE;
  _GetNativeSystemInfo: TGetNativeSystemInfo;
begin
  Result := False;
  LibraryHandle := GetModuleHandle(kernel32);

  if LibraryHandle <> 0 then
  begin
    _GetNativeSystemInfo := GetProcAddress(LibraryHandle,'GetNativeSystemInfo');
    if Assigned(_GetNativeSystemInfo) then
    begin
      _GetNativeSystemInfo(SystemInfo);
      Result := True;
    end
    else
      GetSystemInfo(SystemInfo);
  end
  else
    GetSystemInfo(SystemInfo);
end;

function GetProcessorArchitecture: TProcessorArchitecture;
var
  ASystemInfo: TSystemInfo;
begin
  GetNativeSystemInfo(ASystemInfo);
  case ASystemInfo.wProcessorArchitecture of
    PROCESSOR_ARCHITECTURE_INTEL:
      Result := pax8632;
    PROCESSOR_ARCHITECTURE_IA64:
      Result := paIA64;
    PROCESSOR_ARCHITECTURE_AMD64:
      Result := pax8664;
    else
      Result := paUnknown;
  end;
end;

function IsWindows64: Boolean;
var
  ASystemInfo: TSystemInfo;
begin
  GetNativeSystemInfo(ASystemInfo);
  Result := ASystemInfo.wProcessorArchitecture in [PROCESSOR_ARCHITECTURE_IA64,PROCESSOR_ARCHITECTURE_AMD64];
end;

{$ENDIF ~CLR}
{$ENDIF MSWINDOWS}
{$IFNDEF CLR}

function GetOSVersionString: string;
{$IFDEF UNIX}
var
  MachineInfo: utsname;
begin
  uname(MachineInfo);
  Result := Format('%s %s', [MachineInfo.sysname, MachineInfo.release]);
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
begin
  Result := Format('%s %s', [GetWindowsVersionString, GetWindowsServicePackVersionString]);
end;
{$ENDIF MSWINDOWS}

//=== Hardware ===============================================================

// Helper function for GetMacAddress()
// Converts the adapter_address array to a string

function AdapterToString(Adapter: PJclByteArray): string;
begin
  Result := Format('%2.2x-%2.2x-%2.2x-%2.2x-%2.2x-%2.2x',
   [Integer(Adapter[0]), Integer(Adapter[1]),
    Integer(Adapter[2]), Integer(Adapter[3]),
    Integer(Adapter[4]), Integer(Adapter[5])]);
end;

{ TODO: RTLD version of NetBios }
{$IFDEF MSWINDOWS}
type
  TNetBios = function(P: PNCB): Byte; stdcall;

var
  NetBiosLib: HINST = 0;
  _NetBios: TNetBios;
  {$IFDEF FPC}
  NullAdapterAddress: array [0..5] of Byte = ($00, $00, $00, $00, $00, $00);
  OID_ipMACEntAddr: array [0..9] of UINT = (1, 3, 6, 1, 2, 1, 2, 2, 1, 6);
  OID_ifEntryType: array [0..9] of UINT = (1, 3, 6, 1, 2, 1, 2, 2, 1, 3);
  OID_ifEntryNum: array [0..7] of UINT = (1, 3, 6, 1, 2, 1, 2, 1);
  {$ENDIF FPC}

function GetMacAddresses(const Machine: string; const Addresses: TStrings): Integer;

  procedure ExitNetbios;
    begin
    if NetBiosLib <> 0 then
    begin
      FreeLibrary(NetBiosLib);
      NetBiosLib := 0;
    end;
  end;

  function InitNetbios: Boolean;
  begin
    Result := True;
    if NetBiosLib = 0 then
    begin
      NetBiosLib := SafeLoadLibrary('netapi32.dll');
      Result := NetBiosLib <> 0;
      if Result then
      begin
        @_NetBios := GetProcAddress(NetBiosLib, PChar('Netbios'));
        Result := @_NetBios <> nil;
        if not Result then
          ExitNetbios;
      end;
    end;
  end;

  function NetBios(P: PNCB): Byte;
  begin
    if InitNetbios then
      Result := _NetBios(P)
    else
      Result := 1; // anything other than NRC_GOODRET will do
  end;

  procedure GetMacAddressesNetBios;
  // Platform SDK
  // http://msdn.microsoft.com/library/default.asp?url=/library/en-us/netbios/netbios_1l82.asp

  // Microsoft Knowledge Base Article - 118623
  // HOWTO: Get the MAC Address for an Ethernet Adapter
  // http://support.microsoft.com/default.aspx?scid=kb;en-us;118623
  type
    AStat = packed record
      adapt: TAdapterStatus;
      NameBuff: array [0..29] of TNameBuffer;
    end;
  var
    NCB: TNCB;
    Enum: TLanaEnum;
    I, L, NameLen: Integer;
    Adapter: AStat;
    MachineName: string;
  begin
    MachineName := UpperCase(Machine);
    if MachineName = '' then
      MachineName := '*';
    NameLen := Length(MachineName);
    L := NCBNAMSZ - NameLen;
    if L > 0 then
    begin
      SetLength(MachineName, NCBNAMSZ);
      FillChar(MachineName[NameLen + 1], L, ' ');
    end;
    // From Junior/RO in NG: Microsoft's implementation limits NETBIOS names to 15 characters
    MachineName[NCBNAMSZ] := #0;
    FillChar(NCB, SizeOf(NCB), #0);
    NCB.ncb_command := NCBENUM;
    NCB.ncb_buffer := Pointer(@Enum);
    NCB.ncb_length := SizeOf(Enum);
    if NetBios(@NCB) = NRC_GOODRET then
    begin
      Result := Enum.Length;
      for I := 0 to Ord(Enum.Length) - 1 do
      begin
        FillChar(NCB, SizeOf(NCB), #0);
        NCB.ncb_command := NCBRESET;
        NCB.ncb_lana_num := Enum.lana[I];
        if NetBios(@NCB) = NRC_GOODRET then
        begin
          FillChar(NCB, SizeOf(NCB), #0);
          NCB.ncb_command := NCBASTAT;
          NCB.ncb_lana_num := Enum.lana[I];
          Move(MachineName[1], NCB.ncb_callname, SizeOf(NCB.ncb_callname));
          NCB.ncb_buffer := PChar(@Adapter);
          NCB.ncb_length := SizeOf(Adapter);
          if NetBios(@NCB) = NRC_GOODRET then
            Addresses.Add(AdapterToString(@Adapter.adapt));
        end;
      end;
    end;
  end;

  procedure GetMacAddressesSnmp;
  const
    InetMib1 = 'inetmib1.dll';
    DunAdapterAddress: array [0..4] of Byte = ($44, $45, $53, $54, $00);
    {$IFNDEF FPC // can't resolve address of const }
    NullAdapterAddress: array [0..5] of Byte = ($00, $00, $00, $00, $00, $00);
    OID_ipMACEntAddr: array [0..9] of UINT = (1, 3, 6, 1, 2, 1, 2, 2, 1, 6);
    OID_ifEntryType: array [0..9] of UINT = (1, 3, 6, 1, 2, 1, 2, 2, 1, 3);
    OID_ifEntryNum: array [0..7] of UINT = (1, 3, 6, 1, 2, 1, 2, 1);
    {$ENDIF ~FPC}
  var
    PollForTrapEvent: THandle;
    SupportedView: PAsnObjectIdentifier;
    MIB_ifMACEntAddr: TAsnObjectIdentifier;
    MIB_ifEntryType: TAsnObjectIdentifier;
    MIB_ifEntryNum: TAsnObjectIdentifier;
    VarBindList: TSnmpVarBindList;
    VarBind: array [0..1] of TSnmpVarBind;
    ErrorStatus, ErrorIndex: TAsnInteger32;
    DTmp: Integer;
    Ret: Boolean;
    MAC: PJclByteArray;
  begin
    if LoadSnmp then
    try
      if LoadSnmpExtension(InetMib1) then
      try
        MIB_ifMACEntAddr.idLength := Length(OID_ipMACEntAddr);
        MIB_ifMACEntAddr.ids := @OID_ipMACEntAddr;
        MIB_ifEntryType.idLength := Length(OID_ifEntryType);
        MIB_ifEntryType.ids := @OID_ifEntryType;
        MIB_ifEntryNum.idLength := Length(OID_ifEntryNum);
        MIB_ifEntryNum.ids := @OID_ifEntryNum;
        if SnmpExtensionInit(GetTickCount, PollForTrapEvent, SupportedView) then
        begin
          VarBindList.list := @VarBind[0];
          VarBind[0].name := DEFINE_NULLOID;
          VarBind[1].name := DEFINE_NULLOID;
          VarBindList.len := 1;
          SnmpUtilOidCpy(@VarBind[0].name, @MIB_ifEntryNum);
          Ret := SnmpExtensionQuery(SNMP_PDU_GETNEXT, VarBindList, ErrorStatus, ErrorIndex);
          if Ret then
          begin
            Result := VarBind[0].value.number;
            VarBindList.len := 2;
            SnmpUtilOidCpy(@VarBind[0].name, @MIB_ifEntryType);
            SnmpUtilOidCpy(@VarBind[1].name, @MIB_ifMACEntAddr);
            while Ret do
            begin
              Ret := SnmpExtensionQuery(SNMP_PDU_GETNEXT, VarBindList, ErrorStatus, ErrorIndex);
              if Ret then
              begin
                Ret := SnmpUtilOidNCmp(@VarBind[0].name, @MIB_ifEntryType, MIB_ifEntryType.idLength) = SNMP_ERRORSTATUS_NOERROR;
                if Ret then
                begin
                  DTmp := VarBind[0].value.number;
                  if DTmp = 6 then
                  begin
                    Ret := SnmpUtilOidNCmp(@VarBind[1].name, @MIB_ifMACEntAddr, MIB_ifMACEntAddr.idLength) = SNMP_ERRORSTATUS_NOERROR;
                    if Ret and (VarBind[1].value.address.stream <> nil) then
                    begin
                      MAC := PJclByteArray(VarBind[1].value.address.stream);
                      if not CompareMem(MAC, @NullAdapterAddress, SizeOf(NullAdapterAddress)) then
                        Addresses.Add(AdapterToString(MAC));
                    end;
                  end;
                end;
              end;
            end;
          end;
          SnmpUtilVarBindFree(@VarBind[0]);
          SnmpUtilVarBindFree(@VarBind[1]);
        end;
      finally
        UnloadSnmpExtension;
      end;
    finally
      UnloadSnmp;
    end;
  end;

begin
  Result := -1;
  Addresses.BeginUpdate;
  try
    Addresses.Clear;
    GetMacAddressesNetBios;
    if (Result <= 0) and (Machine = '') then
      GetMacAddressesSnmp;
  finally
    Addresses.EndUpdate;
  end;
end;
{$ENDIF MSWINDOWS}
function ReadTimeStampCounter: Int64; assembler;
asm
        DW      $310F
end;

function GetIntelCacheDescription(const D: Byte): string;
var
  I: Integer;
begin
  Result := '';
  if D <> 0 then
    for I := Low(IntelCacheDescription) to High(IntelCacheDescription) do
      if IntelCacheDescription[I].D = D then
      begin
        Result := IntelCacheDescription[I].I;
        Break;
      end;
  // (outchy) added a return value for unknow D value
  if Result = '' then
    Result := Format(RsIntelUnknownCache,[D]);
end;

procedure GetCpuInfo(var CpuInfo: TCpuInfo);
begin
  CpuInfo := CPUID;
  CpuInfo.IsFDIVOK := TestFDIVInstruction;
  if CpuInfo.HasInstruction then
  begin
    {$IFDEF MSWINDOWS}
    if (CpuInfo.Features and TSC_FLAG) = TSC_FLAG then
      GetCpuSpeed(CpuInfo.FrequencyInfo);
    {$ENDIF MSWINDOWS}
  end;
end;

function RoundFrequency(const Frequency: Integer): Integer;
const
  NF: array [0..8] of Integer = (0, 20, 33, 50, 60, 66, 80, 90, 100);
var
  Freq, RF: Integer;
  I: Byte;
  Hi, Lo: Byte;
begin
  RF := 0;
  Freq := Frequency mod 100;
  for I := 0 to 8 do
  begin
    if Freq < NF[I] then
    begin
      Hi := I;
      Lo := I - 1;
      if (NF[Hi] - Freq) > (Freq - NF[Lo]) then
        RF := NF[Lo] - Freq
      else
        RF := NF[Hi] - Freq;
      Break;
    end;
  end;
  Result := Frequency + RF;
end;

function GetCPUSpeed(var CpuSpeed: TFreqInfo): Boolean;
{$IFDEF UNIX}
begin
  { TODO : GetCPUSpeed: Solution for Linux }
  Result := False;
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  T0, T1: Int64;
  CountFreq: Int64;
  Freq, Freq2, Freq3, Total: Int64;
  TotalCycles, Cycles: Int64;
  Stamp0, Stamp1: Int64;
  TotalTicks, Ticks: Double;
  Tries, Priority: Integer;
  Thread: THandle;
begin
  Stamp0 := 0;
  Stamp1 := 0;
  Freq  := 0;
  Freq2 := 0;
  Freq3 := 0;
  Tries := 0;
  TotalCycles := 0;
  TotalTicks := 0;
  Total := 0;

  Thread := GetCurrentThread();
  Result := QueryPerformanceFrequency(CountFreq);
  if Result then
  begin
    while ((Tries < 3) or ((Tries < 20) and ((Abs(3 * Freq - Total) > 3) or
      (Abs(3 * Freq2 - Total) > 3) or (Abs(3 * Freq3 - Total) > 3)))) do
    begin
      Inc(Tries);
      Freq3 := Freq2;
      Freq2 := Freq;
      QueryPerformanceCounter(T0);
      T1 := T0;

      Priority := GetThreadPriority(Thread);
      if Priority <> THREAD_PRIORITY_ERROR_RETURN then
        SetThreadPriority(Thread, THREAD_PRIORITY_TIME_CRITICAL);
      try
        while T1 - T0 < 50 do
        begin
          QueryPerformanceCounter(T1);
          Stamp0 := ReadTimeStampCounter;
        end;
        T0 := T1;

        while T1 - T0 < 1000 do
        begin
          QueryPerformanceCounter(T1);
          Stamp1 := ReadTimeStampCounter;
        end;
      finally
        if Priority <> THREAD_PRIORITY_ERROR_RETURN then
          SetThreadPriority(Thread, Priority);
      end;

      Cycles := Stamp1 - Stamp0;
      Ticks := T1 - T0;
      Ticks := Ticks * 100000;

      // avoid division by zero
      if CountFreq = 0 then
        Ticks := High(Int64)
      else
        Ticks := Ticks / (CountFreq / 10);

      TotalTicks := TotalTicks + Ticks;
      TotalCycles := TotalCycles + Cycles;

      // avoid division by zero
      if Ticks = 0 then
        Freq := High(Freq)
      else
        Freq := Round(Cycles / Ticks);

      Total := Freq + Freq2 + Freq3;
    end;

    // avoid division by zero
    if TotalTicks = 0 then
    begin
      Freq3 := High(Freq3);
      Freq2 := High(Freq2);
      CpuSpeed.RawFreq := High(CpuSpeed.RawFreq);
    end
    else
    begin
      Freq3 := Round((TotalCycles *  10) / TotalTicks); // freq. in multiples of 10^5 Hz
      Freq2 := Round((TotalCycles * 100) / TotalTicks); // freq. in multiples of 10^4 Hz
      CpuSpeed.RawFreq := Round(TotalCycles / TotalTicks);
    end;

    CpuSpeed.NormFreq := CpuSpeed.RawFreq;

    if Freq2 - (Freq3 * 10) >= 6 then
      Inc(Freq3);


    Freq := CpuSpeed.RawFreq * 10;
    if (Freq3 - Freq) >= 6 then
      Inc(CpuSpeed.NormFreq);

    CpuSpeed.ExTicks := Round(TotalTicks);
    CpuSpeed.InCycles := TotalCycles;

    CpuSpeed.NormFreq := RoundFrequency(CpuSpeed.NormFreq);
    Result := True;
  end;
end;
{$ENDIF MSWINDOWS}

function CPUID: TCpuInfo;
  function HasCPUIDInstruction: Boolean;
  const
    ID_FLAG = $200000;
  begin
    asm
      PUSHFD
      POP     EAX
      MOV     ECX, EAX
      XOR     EAX, ID_FLAG
      AND     ECX, ID_FLAG
      PUSH    EAX
      POPFD
      PUSHFD
      POP     EAX
      AND     EAX, ID_FLAG
      XOR     EAX, ECX
      SETNZ   Result
    end;
  end;
  procedure CallCPUID(ValueEAX, ValueECX: Cardinal; var ReturnedEAX, ReturnedEBX, ReturnedECX, ReturnedEDX);
  begin
    asm
      PUSH    EDI
      PUSH    EBX

      MOV     EAX, ValueEAX
      MOV     ECX, ValueECX
      // CPUID
      DB      0FH
      DB      0A2H
      MOV     EDI, ReturnedEAX
      MOV     Cardinal PTR [EDI], EAX
      MOV     EAX, ReturnedEBX
      MOV     EDI, ReturnedECX
      MOV     Cardinal PTR [EAX], EBX
      MOV     Cardinal PTR [EDI], ECX
      MOV     EAX, ReturnedEDX
      MOV     Cardinal PTR [EAX], EDX

      POP  EBX
      POP  EDI
    end;
  end;

  procedure ProcessStandard(var CPUInfo: TCpuInfo; HiVal: Cardinal);
  var
    VersionInfo, AdditionalInfo, ExFeatures: Cardinal;
  begin
    if HiVal >= 1 then
    begin
      CallCPUID(1, 0, VersionInfo, AdditionalInfo, ExFeatures, CPUInfo.Features);

      CPUInfo.PType := (VersionInfo and $00003000) shr 12;
      CPUInfo.Family := (VersionInfo and $00000F00) shr 8;
      CPUInfo.Model := (VersionInfo and $000000F0) shr 4;
      CPUInfo.Stepping := (VersionInfo and $0000000F);
      CPUInfo.ExtendedModel := (VersionInfo and $000F0000) shr 16;
      CPUInfo.ExtendedFamily := (VersionInfo and $0FF00000) shr 20;

      if CPUInfo.CpuType = CPU_TYPE_INTEL then
      begin
        CPUInfo.IntelSpecific.ExFeatures := ExFeatures;
        CPUInfo.IntelSpecific.BrandID := AdditionalInfo and $000000FF;
        CPUInfo.IntelSpecific.FlushLineSize := (AdditionalInfo and $0000FF00) shr 8;
        CPUInfo.IntelSpecific.APICID := (AdditionalInfo and $FF000000) shr 24;
        CPUInfo.HyperThreadingTechnology := (CPUInfo.Features and INTEL_HTT) <> 0;
        if CPUInfo.HyperThreadingTechnology then
        begin
          CPUInfo.LogicalCore := (AdditionalInfo and $00FF0000) shr 16;
          if CPUInfo.LogicalCore = 0 then
            CPUInfo.LogicalCore := 1;
        end;

        if HiVal >= 2 then
        begin
          CPUInfo.HasCacheInfo := True;
          // TODO: multiple loops
          CallCPUID(2, 0, CPUInfo.IntelSpecific.CacheDescriptors[0], CPUInfo.IntelSpecific.CacheDescriptors[4],
            CPUInfo.IntelSpecific.CacheDescriptors[8], CPUInfo.IntelSpecific.CacheDescriptors[12]);
        end;
      end;
    end;
  end;

  procedure ProcessIntel(var CPUInfo: TCpuInfo; HiVal: Cardinal);
  var
    ExHiVal, Unused, AddressSize, CoreInfo: Cardinal;
    I, J: Integer;
  begin
    CPUInfo.CpuType := CPU_TYPE_INTEL;
    CPUInfo.Manufacturer := 'Intel';

    ProcessStandard(CPUInfo, HiVal);

    if HiVal >= 4 then
    begin
      CallCPUID(4, 0, CoreInfo, Unused, Unused, Unused);
      CPUInfo.PhysicalCore := ((CoreInfo and $FC000000) shr 26) + 1;
    end;

    // check Intel extended
    CallCPUID($80000000, 0, ExHiVal, Unused, Unused, Unused);
    if ExHiVal >= $80000001 then
    begin
      CPUInfo.HasExtendedInfo := True;
      CallCPUID($80000001, 0, Unused, Unused, CPUInfo.IntelSpecific.Ex64Features2,
        CPUInfo.IntelSpecific.Ex64Features);
    end;
    if ExHiVal >= $80000002 then
      CallCPUID($80000002, 0, CPUInfo.CpuName[0], CPUInfo.CpuName[4], CPUInfo.CpuName[8], CPUInfo.CpuName[12]);
    if ExHiVal >= $80000003 then
      CallCPUID($80000003, 0, CPUInfo.CpuName[16], CPUInfo.CpuName[20], CPUInfo.CpuName[24], CPUInfo.CpuName[28]);
    if ExHiVal >= $80000004 then
      CallCPUID($80000004, 0, CPUInfo.CpuName[32], CPUInfo.CpuName[36], CPUInfo.CpuName[40], CPUInfo.CpuName[44]);
    if ExHiVal >= $80000006 then
      CallCPUID($80000006, 0, Unused, Unused, CPUInfo.IntelSpecific.L2Cache, Unused);
    if ExHiVal >= $80000008 then
    begin
      CallCPUID($80000008, 0, AddressSize, Unused, Unused, Unused);
      CPUInfo.IntelSpecific.PhysicalAddressBits := AddressSize and $000000FF;
      CPUInfo.IntelSpecific.VirtualAddressBits := (AddressSize and $0000FF00) shr 8;
    end;

    if CPUInfo.HasCacheInfo then
    begin
      if (CPUInfo.IntelSpecific.L2Cache <> 0) then
      begin
        CPUInfo.L2CacheSize := CPUInfo.IntelSpecific.L2Cache shr 16;
        CPUInfo.L2CacheLineSize := CPUInfo.IntelSpecific.L2Cache and $FF;
        CPUInfo.L2CacheAssociativity := (CPUInfo.IntelSpecific.L2Cache shr 12) and $F;
      end;
      for I := Low(CPUInfo.IntelSpecific.CacheDescriptors) to High(CPUInfo.IntelSpecific.CacheDescriptors) do
        if CPUInfo.IntelSpecific.CacheDescriptors[I]<>0 then
          for J := Low(IntelCacheDescription) to High(IntelCacheDescription) do
            if IntelCacheDescription[J].D = CPUInfo.IntelSpecific.CacheDescriptors[I] then
              with IntelCacheDescription[J] do
        case Family of
          //cfInstructionTLB:
          //cfDataTLB:
          cfL1InstructionCache:
            begin
              Inc(CPUInfo.L1InstructionCacheSize,Size);
              CPUInfo.L1InstructionCacheLineSize := LineSize;
              CPUInfo.L1InstructionCacheAssociativity := WaysOfAssoc;
            end;
          cfL1DataCache:
            begin
              Inc(CPUInfo.L1DataCacheSize,Size);
              CPUInfo.L1DataCacheLineSize := LineSize;
              CPUInfo.L1DataCacheAssociativity := WaysOfAssoc;
            end;
          cfL2Cache:
            if (CPUInfo.IntelSpecific.L2Cache = 0) then
            begin
              Inc(CPUInfo.L2CacheSize,Size);
              CPUInfo.L2CacheLineSize := LineSize;
              CPUInfo.L2CacheAssociativity := WaysOfAssoc;
            end;
          cfL3Cache:
            begin
              Inc(CPUInfo.L3CacheSize,Size);
              CPUInfo.L3CacheLineSize := LineSize;
              CPUInfo.L3CacheAssociativity := WaysOfAssoc;
              CPUInfo.L3LinesPerSector := LinePerSector;
            end;
          //cfTrace:    // no numeric informations
          //cfOther:
        end;
    end;
    if not CPUInfo.HasExtendedInfo then
    begin
      case CPUInfo.Family of
        4:
          case CPUInfo.Model of
            1:
              CPUInfo.CpuName := 'Intel 486DX Processor';
            2:
              CPUInfo.CpuName := 'Intel 486SX Processor';
            3:
              CPUInfo.CpuName := 'Intel DX2 Processor';
            4:
              CPUInfo.CpuName := 'Intel 486 Processor';
            5:
              CPUInfo.CpuName := 'Intel SX2 Processor';
            7:
              CPUInfo.CpuName := 'Write-Back Enhanced Intel DX2 Processor';
            8:
              CPUInfo.CpuName := 'Intel DX4 Processor';
          else
            CPUInfo.CpuName := 'Intel 486 Processor';
          end;
        5:
          CPUInfo.CpuName := 'Pentium';
        6:
          case CPUInfo.Model of
            1:
              CPUInfo.CpuName := 'Pentium Pro';
            3:
              CPUInfo.CpuName := 'Pentium II';
            5:
              case CPUInfo.L2CacheSize of
                0:
                  CPUInfo.CpuName := 'Celeron';
                1024:
                  CPUInfo.CpuName := 'Pentium II Xeon';
                2048:
                  CPUInfo.CpuName := 'Pentium II Xeon';
              else
                CPUInfo.CpuName := 'Pentium II';
              end;
            6:
              case CPUInfo.L2CacheSize of
                0:
                  CPUInfo.CpuName := 'Celeron';
                128:
                  CPUInfo.CpuName := 'Celeron';
              else
                CPUInfo.CpuName := 'Pentium II';
              end;
            7:
              case CPUInfo.L2CacheSize of
                1024:
                  CPUInfo.CpuName := 'Pentium III Xeon';
                2048:
                  CPUInfo.CpuName := 'Pentium III Xeon';
              else
                CPUInfo.CpuName := 'Pentium III';
              end;
            8:
              case CPUInfo.IntelSpecific.BrandID of
                1:
                  CPUInfo.CpuName := 'Celeron';
                2:
                  CPUInfo.CpuName := 'Pentium III';
                3:
                  CPUInfo.CpuName := 'Pentium III Xeon';
                4:
                  CPUInfo.CpuName := 'Pentium III';
              else
                CPUInfo.CpuName := 'Pentium III';
              end;
            10:
              CPUInfo.CpuName := 'Pentium III Xeon';
            11:
              CPUInfo.CpuName := 'Pentium III';
          else
            StrPCopy(CPUInfo.CpuName, Format('P6 (Model %d)', [CPUInfo.Model]));
          end;
        15:
          case CPUInfo.IntelSpecific.BrandID of
            1:
              CPUInfo.CpuName := 'Celeron';
            8:
              CPUInfo.CpuName := 'Pentium 4';
            14:
              CPUInfo.CpuName := 'Xeon';
          else
            CPUInfo.CpuName := 'Pentium 4';
          end;
      else
        StrPCopy(CPUInfo.CpuName, Format('P%d', [CPUInfo.Family]));
      end;
    end;

    CPUInfo.MMX := (CPUInfo.Features and MMX_FLAG) <> 0;
    CPUInfo.SSE := [];
    if (CPUInfo.Features and SSE_FLAG) <> 0 then
      Include(CPUInfo.SSE, sse);
    if (CPUInfo.Features and SSE2_FLAG) <> 0 then
      Include(CPUInfo.SSE, sse2);
    if (CPUInfo.IntelSpecific.ExFeatures and EINTEL_SSE3) <> 0 then
      Include(CPUInfo.SSE, sse3);
    if (CPUInfo.IntelSpecific.ExFeatures and EINTEL_SSSE3) <> 0 then
      Include(CPUInfo.SSE, ssse3);
    if (CPUInfo.IntelSpecific.ExFeatures and EINTEL_SSE4_1) <> 0 then
      Include(CPUInfo.SSE, sse4A);
    if (CPUInfo.IntelSpecific.ExFeatures and EINTEL_SSE4_2) <> 0 then
      Include(CPUInfo.SSE, sse4B);
    CPUInfo.Is64Bits := CPUInfo.HasExtendedInfo and ((CPUInfo.IntelSpecific.Ex64Features and EINTEL64_EM64T)<>0);
    CPUInfo.DepCapable := CPUInfo.HasExtendedInfo and ((CPUInfo.IntelSpecific.Ex64Features and EINTEL64_EDB) <> 0);
  end;

  procedure ProcessAMD(var CPUInfo: TCpuInfo; HiVal: Cardinal);
  var
    ExHiVal, Unused, VersionInfo, AdditionalInfo: Cardinal;
  begin
    CPUInfo.CpuType := CPU_TYPE_AMD;
    CPUInfo.Manufacturer := 'AMD';

    // check AMD extended
    if HiVal >= 1 then
    begin
      CallCPUID(1, 0, VersionInfo, AdditionalInfo, CPUInfo.AMDSpecific.Features2, CPUInfo.Features);

      CPUInfo.AMDSpecific.BrandID := AdditionalInfo and $000000FF;
      CPUInfo.AMDSpecific.FlushLineSize := (AdditionalInfo and $0000FF00) shr 8;
      CPUInfo.AMDSpecific.APICID := (AdditionalInfo and $FF000000) shr 24;
      CPUInfo.HyperThreadingTechnology := (CPUInfo.Features and AMD_HTT) <> 0;
      if CPUInfo.HyperThreadingTechnology then
      begin
        CPUInfo.LogicalCore := (AdditionalInfo and $00FF0000) shr 16;
        if CPUInfo.LogicalCore = 0 then
          CPUInfo.LogicalCore := 1;
      end;
    end;

    CallCPUID($80000000, 0, ExHiVal, Unused, Unused, Unused);
    if ExHiVal <> 0 then
    begin
      // AMD only
      CPUInfo.HasExtendedInfo := True;

      if ExHiVal >= $80000001 then
      begin
        CallCPUID($80000001, 0, VersionInfo, AdditionalInfo, CPUInfo.AMDSpecific.ExFeatures2, CPUInfo.AMDSpecific.ExFeatures);
        CPUInfo.Family := (VersionInfo and $00000F00) shr 8;
        CPUInfo.Model := (VersionInfo and $000000F0) shr 4;
        CPUInfo.Stepping := (VersionInfo and $0000000F);
        CPUInfo.ExtendedModel := (VersionInfo and $000F0000) shr 16;
        CPUInfo.ExtendedFamily := (VersionInfo and $0FF00000) shr 20;
        CPUInfo.AMDSpecific.ExBrandID := AdditionalInfo and $0000FFFF;
      end;
      if ExHiVal >= $80000002 then
        CallCPUID($80000002, 0, CPUInfo.CpuName[0], CPUInfo.CpuName[4], CPUInfo.CpuName[8], CPUInfo.CpuName[12]);
      if ExHiVal >= $80000003 then
        CallCPUID($80000003, 0, CPUInfo.CpuName[16], CPUInfo.CpuName[20], CPUInfo.CpuName[24], CPUInfo.CpuName[28]);
      if ExHiVal >= $80000004 then
        CallCPUID($80000004, 0, CPUInfo.CpuName[32], CPUInfo.CpuName[36], CPUInfo.CpuName[40], CPUInfo.CpuName[44]);
      if ExHiVal >= $80000005 then
      begin
        CPUInfo.HasCacheInfo := True;
        CallCPUID($80000005, 0, CPUInfo.AMDSpecific.L1MByteInstructionTLB, CPUInfo.AMDSpecific.L1KByteInstructionTLB,
          CPUInfo.AMDSpecific.L1DataCache, CPUInfo.AMDSpecific.L1InstructionCache);
      end;
      if ExHiVal >= $80000006 then
        CallCPUID($80000006, 0, CPUInfo.AMDSpecific.L2MByteInstructionTLB, CPUInfo.AMDSpecific.L2KByteInstructionTLB,
          CPUInfo.AMDSpecific.L2Cache, CPUInfo.AMDSpecific.L3Cache);
      if CPUInfo.HasCacheInfo then
      begin
        CPUInfo.L1DataCacheSize := CPUInfo.AMDSpecific.L1DataCache[ciSize];
        CPUInfo.L1DataCacheLineSize := CPUInfo.AMDSpecific.L1DataCache[ciLineSize];
        CPUInfo.L1DataCacheAssociativity := CPUInfo.AMDSpecific.L1DataCache[ciAssociativity];
        CPUInfo.L1InstructionCacheSize := CPUInfo.AMDSpecific.L1InstructionCache[ciSize];
        CPUInfo.L1InstructionCacheLineSize := CPUInfo.AMDSpecific.L1InstructionCache[ciLineSize];
        CPUInfo.L1InstructionCacheAssociativity := CPUInfo.AMDSpecific.L1InstructionCache[ciAssociativity];
        CPUInfo.L2CacheLineSize := CPUInfo.AMDSpecific.L2Cache and $FF;
        CPUInfo.L2CacheAssociativity := (CPUInfo.AMDSpecific.L2Cache shr 12) and $F;
        CPUInfo.L2CacheSize := CPUInfo.AMDSpecific.L2Cache shr 16;
        CPUInfo.L3CacheLineSize := CPUInfo.AMDSpecific.L3Cache and $FF;
        CPUInfo.L3CacheAssociativity := (CPUInfo.AMDSpecific.L3Cache shr 12) and $F;
        CPUInfo.L3CacheSize := CPUInfo.AMDSpecific.L3Cache shr 19 {MB}; //(CPUInfo.AMDSpecific.L3Cache shr 18) * 512 {kB};
      end;
      if ExHiVal >= $80000007 then
        CallCPUID($80000007, 0, Unused, Unused, Unused, CPUInfo.AMDSpecific.AdvancedPowerManagement);
      if ExHiVal >= $80000008 then
      begin
        CallCPUID($80000008, 0, Unused, VersionInfo, AdditionalInfo, Unused);
        CPUInfo.AMDSpecific.PhysicalAddressSize := VersionInfo and $000000FF;
        CPUInfo.AMDSpecific.VirtualAddressSize := (VersionInfo and $0000FF00) shr 8;
        CPUInfo.PhysicalCore := (AdditionalInfo and $000000FF) + 1;
      end;
    end
    else
    begin
      ProcessStandard(CPUInfo, HiVal);
      case CPUInfo.Family of
        4:
          CPUInfo.CpuName := 'Am486(R) or Am5x86';
        5:
          case CPUInfo.Model of
            0:
              CPUInfo.CpuName := 'AMD-K5 (Model 0)';
            1:
              CPUInfo.CpuName := 'AMD-K5 (Model 1)';
            2:
              CPUInfo.CpuName := 'AMD-K5 (Model 2)';
            3:
              CPUInfo.CpuName := 'AMD-K5 (Model 3)';
            6:
              CPUInfo.CpuName := 'AMD-K6® (Model 6)';
            7:
              CPUInfo.CpuName := 'AMD-K6® (Model 7)';
            8:
              CPUInfo.CpuName := 'AMD-K6®-2 (Model 8)';
            9:
              CPUInfo.CpuName := 'AMD-K6®-III (Model 9)';
            else
              StrFmt(CPUInfo.CpuName,PChar(RsUnknownAMDModel),[CPUInfo.Model]);
          end;
        6:
          case CPUInfo.Model of
            1:
              CPUInfo.CpuName := 'AMD Athlon™ (Model 1)';
            2:
              CPUInfo.CpuName := 'AMD Athlon™ (Model 2)';
            3:
              CPUInfo.CpuName := 'AMD Duron™ (Model 3)';
            4:
              CPUInfo.CpuName := 'AMD Athlon™ (Model 4)';
            6:
              CPUInfo.CpuName := 'AMD Athlon™ XP (Model 6)';
            7:
              CPUInfo.CpuName := 'AMD Duron™ (Model 7)';
            8:
              CPUInfo.CpuName := 'AMD Athlon™ XP (Model 8)';
            10:
              CPUInfo.CpuName := 'AMD Athlon™ XP (Model 10)';
            else
              StrFmt(CPUInfo.CpuName, PChar(RsUnknownAMDModel), [CPUInfo.Model]);
          end;
        8:

        else
          CPUInfo.CpuName := 'Unknown AMD Chip';
      end;
    end;

    CPUInfo.MMX := (CPUInfo.Features and AMD_MMX) <> 0;
    CPUInfo.ExMMX := CPUInfo.HasExtendedInfo and ((CPUInfo.AMDSpecific.ExFeatures and EAMD_EXMMX) <> 0);
    CPUInfo._3DNow := CPUInfo.HasExtendedInfo and ((CPUInfo.AMDSpecific.ExFeatures and EAMD_3DNOW) <> 0);
    CPUInfo.Ex3DNow := CPUInfo.HasExtendedInfo and ((CPUInfo.AMDSpecific.ExFeatures and EAMD_EX3DNOW) <> 0);
    CPUInfo.SSE := [];
    if (CPUInfo.Features and AMD_SSE) <> 0 then
      Include(CPUInfo.SSE, sse);
    if (CPUInfo.Features and AMD_SSE2) <> 0 then
      Include(CPUInfo.SSE, sse2);
    if (CPUInfo.AMDSpecific.Features2 and AMD2_SSE3) <> 0 then
        Include(CPUInfo.SSE, sse3);
    if CPUInfo.HasExtendedInfo then
    begin
      if (CPUInfo.AMDSpecific.ExFeatures2 and EAMD2_SSE4A) <> 0 then
        Include(CPUInfo.SSE, sse4A);
      if (CPUInfo.AMDSpecific.ExFeatures2 and EAMD2_SSE5) <> 0 then
        Include(CPUInfo.SSE, sse5);
    end;
    CPUInfo.Is64Bits := CPUInfo.HasExtendedInfo and ((CPUInfo.AMDSpecific.ExFeatures and EAMD_LONG) <> 0);
    CPUInfo.DEPCapable := CPUInfo.HasExtendedInfo and ((CPUInfo.AMDSpecific.ExFeatures and EAMD_NX) <> 0);
  end;

  procedure ProcessCyrix(var CPUInfo: TCpuInfo; HiVal: Cardinal);
  var
    ExHiVal, Unused, VersionInfo, AdditionalInfo: Cardinal;
  begin
    CPUInfo.CpuType := CPU_TYPE_CYRIX;
    CPUInfo.Manufacturer := 'Cyrix';

    // check Cyrix extended
    CallCPUID($80000000, 0, ExHiVal, Unused, Unused, Unused);
    if ExHiVal <> 0 then
    begin
      // Cyrix only
      CPUInfo.HasExtendedInfo := True;
      if ExHiVal >= $80000001 then
      begin
        CallCPUID($80000001, 0, VersionInfo, AdditionalInfo, Unused, CPUInfo.Features);
        CPUInfo.PType := (VersionInfo and $0000F000) shr 12;
        CPUInfo.Family := (VersionInfo and $00000F00) shr 8;
        CPUInfo.Model := (VersionInfo and $000000F0) shr 4;
        CPUInfo.Stepping := (VersionInfo and $0000000F);
      end;
      if ExHiVal >= $80000002 then
        CallCPUID($80000002, 0, CPUInfo.CpuName[0], CPUInfo.CpuName[4], CPUInfo.CpuName[8], CPUInfo.CpuName[12]);
      if ExHiVal >= $80000003 then
        CallCPUID($80000003, 0, CPUInfo.CpuName[16], CPUInfo.CpuName[20], CPUInfo.CpuName[24], CPUInfo.CpuName[28]);
      if ExHiVal >= $80000004 then
        CallCPUID($80000004, 0, CPUInfo.CpuName[32], CPUInfo.CpuName[36], CPUInfo.CpuName[40], CPUInfo.CpuName[44]);
      if ExHiVal >= $80000005 then
      begin
        CPUInfo.HasCacheInfo := True;
        CallCPUID($80000005, 0, Unused, CPUInfo.CyrixSpecific.TLBInfo, CPUInfo.CyrixSpecific.L1CacheInfo, Unused);
      end;
    end
    else
    begin
      ProcessStandard(CPUInfo, HiVal);
      case CPUInfo.Family of
        4:
          CPUInfo.CpuName := 'Cyrix MediaGX';
        5:
          case CPUInfo.Model of
            2:
              CPUInfo.CpuName := 'Cyrix 6x86';
            4:
              CPUInfo.CpuName := 'Cyrix GXm';
          end;
        6:
          CPUInfo.CpuName := '6x86MX';
      else
        StrPCopy(CPUInfo.CpuName, Format('%dx86', [CPUInfo.Family]));
      end;
    end;
  end;

  procedure ProcessVIA(var CPUInfo: TCpuInfo; HiVal: Cardinal);
  var
    ExHiVal, Unused, VersionInfo: Cardinal;
  begin
    CPUInfo.CpuType := CPU_TYPE_VIA;
    CPUInfo.Manufacturer := 'Via';

    // check VIA extended
    CallCPUID($80000000, 0, ExHiVal, Unused, Unused, Unused);
    if ExHiVal <> 0 then
    begin
      if ExHiVal >= $80000001 then
      begin
        CPUInfo.HasExtendedInfo := True;
        CallCPUID($80000001, 0, VersionInfo, Unused, Unused, CPUInfo.ViaSpecific.ExFeatures);
        CPUInfo.PType := (VersionInfo and $00003000) shr 12;
        CPUInfo.Family := (VersionInfo and $00000F00) shr 8;
        CPUInfo.Model := (VersionInfo and $000000F0) shr 4;
        CPUInfo.Stepping := (VersionInfo and $0000000F);
      end;
      if ExHiVal >= $80000002 then
        CallCPUID($80000002, 0, CPUInfo.CpuName[0], CPUInfo.CpuName[4], CPUInfo.CpuName[8], CPUInfo.CpuName[12]);
      if ExHiVal >= $80000003 then
        CallCPUID($80000003, 0, CPUInfo.CpuName[16], CPUInfo.CpuName[20], CPUInfo.CpuName[24], CPUInfo.CpuName[28]);
      if ExHiVal >= $80000004 then
        CallCPUID($80000004, 0, CPUInfo.CpuName[32], CPUInfo.CpuName[36], CPUInfo.CpuName[40], CPUInfo.CpuName[44]);
      if ExHiVal >= $80000005 then
      begin
        CPUInfo.HasCacheInfo := True;
        CallCPUID($80000005, 0, Unused, CPUInfo.ViaSpecific.InstructionTLB, CPUInfo.ViaSpecific.L1DataCache,
          CPUInfo.ViaSpecific.L1InstructionCache);
      end;
      if ExHiVal >= $80000006 then
        CallCPUID($80000006, 0, Unused, Unused, CPUInfo.ViaSpecific.L2DataCache, Unused);

      if CPUInfo.HasCacheInfo then
      begin
        CPUInfo.L1DataCacheSize := CPUInfo.VIASpecific.L1DataCache[ciSize];
        CPUInfo.L1DataCacheLineSize := CPUInfo.VIASpecific.L1DataCache[ciLineSize];
        CPUInfo.L1DataCacheAssociativity := CPUInfo.VIASpecific.L1DataCache[ciAssociativity];
        CPUInfo.L1InstructionCacheSize := CPUInfo.VIASpecific.L1InstructionCache[ciSize];
        CPUInfo.L1InstructionCacheLineSize := CPUInfo.VIASpecific.L1InstructionCache[ciLineSize];
        CPUInfo.L1InstructionCacheAssociativity := CPUInfo.VIASpecific.L1InstructionCache[ciAssociativity];
        CPUInfo.L2CacheLineSize := CPUInfo.VIASpecific.L2DataCache and $FF;
        CPUInfo.L2CacheAssociativity := (CPUInfo.VIASpecific.L2DataCache shr 12) and $F;
        CPUInfo.L2CacheSize := CPUInfo.VIASpecific.L2DataCache shr 16;
      end;

      CallCPUID($C0000000, 0, ExHiVal, Unused, Unused, Unused);
      if ExHiVal >= $C0000001 then
        CallCPUID($C0000001, 0, Unused, Unused, Unused, CPUInfo.ViaSpecific.ExFeatures);
    end
    else
      ProcessStandard(CPUInfo, HiVal);

    if not CPUInfo.HasExtendedInfo then
      CPUInfo.CpuName := 'C3';
    CPUInfo.MMX := (CPUInfo.Features and VIA_MMX) <> 0;
    CPUInfo.SSE := [];
    if (CPUInfo.Features and VIA_SSE) <> 0 then
      Include(CPUInfo.SSE, sse);
    CPUInfo._3DNow := (CPUInfo.Features and VIA_3DNOW) <> 0;
  end;

  procedure ProcessTransmeta(var CPUInfo: TCpuInfo; HiVal: Cardinal);
  var
    ExHiVal, Unused, VersionInfo: Cardinal;
  begin
    CPUInfo.CpuType := CPU_TYPE_TRANSMETA;
    CPUInfo.Manufacturer := 'Transmeta';

    if (HiVal >= 1) then
    begin
      CallCPUID(1, 0, VersionInfo, Unused, Unused, CPUInfo.Features);
      CPUInfo.PType := (VersionInfo and $00003000) shr 12;
      CPUInfo.Family := (VersionInfo and $00000F00) shr 8;
      CPUInfo.Model := (VersionInfo and $000000F0) shr 4;
      CPUInfo.Stepping := (VersionInfo and $0000000F);
    end;
    // no information when eax is 2
    // eax is 3 means Serial Number, not detected there

    // small CPU description, overriden if ExHiVal >= 80000002
    CallCPUID($80000000, 0, ExHiVal, CPUInfo.CpuName[0], CPUInfo.CpuName[8], CPUInfo.CpuName[4]);
    if ExHiVal <> 0 then
    begin
      CPUInfo.HasExtendedInfo := True;

      if ExHiVal >= $80000001 then
        CallCPUID($80000001, 0, Unused, Unused, Unused, CPUInfo.TransmetaSpecific.ExFeatures);
      if ExHiVal >= $80000002 then
        CallCPUID($80000002, 0, CPUInfo.CpuName[0], CPUInfo.CpuName[4], CPUInfo.CpuName[8], CPUInfo.CpuName[12]);
      if ExHiVal >= $80000003 then
        CallCPUID($80000003, 0, CPUInfo.CpuName[16], CPUInfo.CpuName[20], CPUInfo.CpuName[24], CPUInfo.CpuName[28]);
      if ExHiVal >= $80000004 then
        CallCPUID($80000004, 0, CPUInfo.CpuName[32], CPUInfo.CpuName[36], CPUInfo.CpuName[40], CPUInfo.CpuName[44]);
      if ExHiVal >= $80000005 then
      begin
        CPUInfo.HasCacheInfo := True;
        CallCPUID($80000005, 0, Unused, CPUInfo.TransmetaSpecific.CodeTLB, CPUInfo.TransmetaSpecific.L1DataCache,
          CPUInfo.TransmetaSpecific.L1CodeCache);
      end;
      if CPUInfo.HasCacheInfo then
      begin
        CPUInfo.L1DataCacheSize := CPUInfo.TransmetaSpecific.L1DataCache[ciSize];
        CPUInfo.L1DataCacheLineSize := CPUInfo.TransmetaSpecific.L1DataCache[ciLineSize];
        CPUInfo.L1DataCacheAssociativity := CPUInfo.TransmetaSpecific.L1DataCache[ciAssociativity];
        CPUInfo.L1InstructionCacheSize := CPUInfo.TransmetaSpecific.L1CodeCache[ciSize];
        CPUInfo.L1InstructionCacheLineSize := CPUInfo.TransmetaSpecific.L1CodeCache[ciLineSize];
        CPUInfo.L1InstructionCacheAssociativity := CPUInfo.TransmetaSpecific.L1CodeCache[ciAssociativity];
        CPUInfo.L2CacheLineSize := CPUInfo.TransmetaSpecific.L2Cache and $FF;
        CPUInfo.L2CacheAssociativity := (CPUInfo.TransmetaSpecific.L2Cache shr 12) and $F;
        CPUInfo.L2CacheSize := CPUInfo.TransmetaSpecific.L2Cache shr 16;
      end;
      if ExHiVal >= $80000006 then
        CallCPUID($80000006, 0, Unused, Unused, CPUInfo.TransmetaSpecific.L2Cache, Unused);
    end
    else
      CPUInfo.CpuName := 'Crusoe';

    CallCPUID($80860000, 0, ExHiVal, Unused, Unused, Unused);
    if ExHiVal <> 0 then
    begin
      if ExHiVal >= $80860001 then
        CallCPUID($80860001, 0, Unused, CPUInfo.TransmetaSpecific.RevisionABCD, CPUInfo.TransmetaSpecific.RevisionXXXX,
          CPUInfo.TransmetaSpecific.TransmetaFeatures);
      if ExHiVal >= $80860002 then
        CallCPUID($80860002, 0, Unused, CPUInfo.TransmetaSpecific.CodeMorphingABCD, CPUInfo.TransmetaSpecific.CodeMorphingXXXX, Unused);
      if ExHiVal >= $80860003 then
        CallCPUID($80860003, 0, CPUInfo.TransmetaSpecific.TransmetaInformations[0], CPUInfo.TransmetaSpecific.TransmetaInformations[4],
          CPUInfo.TransmetaSpecific.TransmetaInformations[8], CPUInfo.TransmetaSpecific.TransmetaInformations[12]);
      if ExHiVal >= $80860004 then
        CallCPUID($80860004, 0, CPUInfo.TransmetaSpecific.TransmetaInformations[16], CPUInfo.TransmetaSpecific.TransmetaInformations[20],
          CPUInfo.TransmetaSpecific.TransmetaInformations[24], CPUInfo.TransmetaSpecific.TransmetaInformations[28]);
      if ExHiVal >= $80860005 then
        CallCPUID($80860005, 0, CPUInfo.TransmetaSpecific.TransmetaInformations[32], CPUInfo.TransmetaSpecific.TransmetaInformations[36],
          CPUInfo.TransmetaSpecific.TransmetaInformations[40], CPUInfo.TransmetaSpecific.TransmetaInformations[44]);
      if ExHiVal >= $80860006 then
        CallCPUID($80860006, 0, CPUInfo.TransmetaSpecific.TransmetaInformations[48], CPUInfo.TransmetaSpecific.TransmetaInformations[52],
          CPUInfo.TransmetaSpecific.TransmetaInformations[56], CPUInfo.TransmetaSpecific.TransmetaInformations[60]);
      if (ExHiVal >= $80860007) and ((CPUInfo.TransmetaSpecific.TransmetaFeatures and STRANSMETA_LONGRUN) <> 0) then
        CallCPUID($80860007, 0, CPUInfo.TransmetaSpecific.CurrentFrequency, CPUInfo.TransmetaSpecific.CurrentVoltage,
          CPUInfo.TransmetaSpecific.CurrentPerformance, Unused);
    end;
    CPUInfo.MMX := (CPUInfo.Features and TRANSMETA_MMX) <> 0;
  end;

var
  HiVal: Cardinal;
begin
  FillChar(Result, sizeof(Result), 0);
  Result.LogicalCore := 1;
  Result.PhysicalCore := 1;

  if HasCPUIDInstruction then
  begin
    Result.HasInstruction := True;
    CallCPUID(0, 0, HiVal, Result.VendorIDString[0], Result.VendorIDString[8],
      Result.VendorIDString[4]);
    if Result.VendorIDString = VendorIDIntel then
      ProcessIntel(Result, HiVal)
    else if Result.VendorIDString = VendorIDAMD then
      ProcessAMD(Result, HiVal)
    else if Result.VendorIDString = VendorIDCyrix then
      ProcessCyrix(Result, HiVal)
    else if Result.VendorIDString = VendorIDVIA then
      ProcessVIA(Result, HiVal)
    else if Result.VendorIDString = VendorIDTransmeta then
      ProcessTransmeta(Result, HiVal)
    else
      ProcessStandard(Result, HiVal);
  end
  else
    Result.Family := 4;

  if Result.CpuType = 0 then
  begin
    Result.Manufacturer := 'Unknown';
    Result.CpuName := 'Unknown';
  end;
end;

function TestFDIVInstruction: Boolean;
var
  TopNum: Double;
  BottomNum: Double;
  One: Double;
  ISOK: Boolean;
begin
  // The following code was found in Borlands fdiv.asm file in the
  // Delphi 3\Source\RTL\SYS directory, (I made some minor modifications)
  // therefore I cannot take credit for it.
  TopNum := 2658955;
  BottomNum := PI;
  One := 1;
  asm
        PUSH    EAX
        FLD     [TopNum]
        FDIV    [BottomNum]
        FMUL    [BottomNum]
        FSUBR   [TopNum]
        FCOMP   [One]
        FSTSW   AX
        SHR     EAX, 8
        AND     EAX, 01H
        MOV     ISOK, AL
        POP     EAX
  end;
  Result := ISOK;
end;

//=== Alloc granularity ======================================================

procedure RoundToAllocGranularity64(var Value: Int64; Up: Boolean);
begin
  if (Value mod AllocGranularity) <> 0 then
    if Up then
      Value := ((Value div AllocGranularity) + 1) * AllocGranularity
    else
      Value := (Value div AllocGranularity) * AllocGranularity;
end;

procedure RoundToAllocGranularityPtr(var Value: Pointer; Up: Boolean);
begin
  if (Cardinal(Value) mod AllocGranularity) <> 0 then
    if Up then
      Value := Pointer(((Cardinal(Value) div AllocGranularity) + 1) * AllocGranularity)
    else
      Value := Pointer((Cardinal(Value) div AllocGranularity) * AllocGranularity);
end;

//=== Advanced Power Management (APM) ========================================

{$IFDEF MSWINDOWS}
function GetAPMLineStatus: TAPMLineStatus;
var
  SystemPowerStatus: TSystemPowerStatus;
begin
  Result := alsUnknown;

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 5) then // Windows NT doesn't support GetSystemPowerStatus
    Exit;                                                                     // so we return alsUnknown

  if not GetSystemPowerStatus(SystemPowerStatus) then
    RaiseLastOSError
  else
  begin
    case SystemPowerStatus.ACLineStatus  of
      0:
        Result := alsOffline;
      1:
        Result := alsOnline;
      255:
        Result := alsUnknown;
    end;
  end;
end;

function GetAPMBatteryFlag: TAPMBatteryFlag;
var
  SystemPowerStatus: TSystemPowerStatus;
begin
  Result := abfUnknown;

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 5) then // Windows NT doesn't support GetSystemPowerStatus
    Exit;                                                                     // so we return abfUnknown

  if not GetSystemPowerStatus(SystemPowerStatus) then
    RaiseLastOSError
  else
  begin
    case SystemPowerStatus.BatteryFlag of
      1:
       Result := abfHigh;
      2:
        Result := abfLow;
      4:
        Result := abfCritical;
      8:
        Result := abfCharging;
      128:
        Result := abfNoBattery;
      255:
        Result := abfUnknown;
    end;
  end;
end;


function GetAPMBatteryFlags: TAPMBatteryFlags;
var
  SystemPowerStatus: TSystemPowerStatus;
begin
  Result := [];

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 5) then // Windows NT doesn't support GetSystemPowerStatus
  begin
    Result := [abfUnknown];
    Exit;                                                                     // so we return [abfUnknown]
  end;

  if not GetSystemPowerStatus(SystemPowerStatus) then
    RaiseLastOSError
  else
  begin
    if (SystemPowerStatus.BatteryFlag and 1) <> 0 then
      Result := Result + [abfHigh];
    if (SystemPowerStatus.BatteryFlag and 2) <> 0 then
      Result := Result + [abfLow];
    if (SystemPowerStatus.BatteryFlag and 4) <> 0 then
      Result := Result + [abfCritical];
    if (SystemPowerStatus.BatteryFlag and 8) <> 0 then
      Result := Result + [abfCharging];
    if (SystemPowerStatus.BatteryFlag and 128) <> 0 then
      Result := Result + [abfNoBattery];
    if SystemPowerStatus.BatteryFlag = 255 then
      Result := Result + [abfUnknown];
  end;
end;

function GetAPMBatteryLifePercent: Integer;
var
  SystemPowerStatus: TSystemPowerStatus;
begin
  Result := 0;

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 5) then // Windows NT doesn't support GetSystemPowerStatus
    Exit;

  if not GetSystemPowerStatus(SystemPowerStatus) then
    RaiseLastOSError
  else
    Result := SystemPowerStatus.BatteryLifePercent;
end;

function GetAPMBatteryLifeTime: DWORD;
var
  SystemPowerStatus: TSystemPowerStatus;
begin
  Result := 0;

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 5) then // Windows NT doesn't support GetSystemPowerStatus
    Exit;

  if not GetSystemPowerStatus(SystemPowerStatus) then
    RaiseLastOSError
  else
    Result := SystemPowerStatus.BatteryLifeTime;
end;

function GetAPMBatteryFullLifeTime: DWORD;
var
  SystemPowerStatus: TSystemPowerStatus;
begin
  Result := 0;

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 5) then // Windows NT doesn't support GetSystemPowerStatus
    Exit;

  if not GetSystemPowerStatus(SystemPowerStatus) then
    RaiseLastOSError
  else
    Result := SystemPowerStatus.BatteryFullLifeTime;
end;

//=== Memory Information =====================================================

function GetMaxAppAddress: Cardinal;
var
  SystemInfo: TSystemInfo;
begin
  FillChar(SystemInfo, SizeOf(SystemInfo), #0);
  GetSystemInfo(SystemInfo);
  Result := Integer(SystemInfo.lpMaximumApplicationAddress);
end;

function GetMinAppAddress: Cardinal;
var
  SystemInfo: TSystemInfo;
begin
  FillChar(SystemInfo, SizeOf(SystemInfo), #0);
  GetSystemInfo(SystemInfo);
  Result := Integer(SystemInfo.lpMinimumApplicationAddress);
end;
{$ENDIF MSWINDOWS}

function GetMemoryLoad: Byte;
{$IFDEF UNIX}
var
  SystemInf: TSysInfo ;
begin
  {$IFDEF FPC}
  SysInfo(@SystemInf);
  {$ELSE}
  SysInfo(SystemInf);
  {$ENDIF FPC}
  with SystemInf do
    Result := 100 - Round(100 * freeram / totalram);
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwMemoryLoad;
end;
{$ENDIF MSWINDOWS}

function GetSwapFileSize: Cardinal;
{$IFDEF UNIX}
var
  SystemInf: TSysInfo;
begin
  {$IFDEF FPC}
  SysInfo(@SystemInf);
  {$ELSE}
  SysInfo(SystemInf);
  {$ENDIF FPC}
  Result := SystemInf.totalswap;
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  with MemoryStatus do
    Result := dwTotalPageFile - dwAvailPageFile;
end;
{$ENDIF MSWINDOWS}

function GetSwapFileUsage: Byte;
{$IFDEF UNIX}
var
  SystemInf: TSysInfo;
begin
  {$IFDEF FPC}
  SysInfo(@SystemInf);
  {$ELSE}
  SysInfo(SystemInf);
  {$ENDIF FPC}
  with SystemInf do
    Result := 100 - Trunc(100 * FreeSwap / TotalSwap);
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  with MemoryStatus do
    if dwTotalPageFile > 0 then
      Result := 100 - Trunc(dwAvailPageFile / dwTotalPageFile * 100)
    else
      Result := 0;
end;
{$ENDIF MSWINDOWS}

function GetTotalPhysicalMemory: Cardinal;
{$IFDEF UNIX}
var
  SystemInf: TSysInfo;
begin
  {$IFDEF FPC}
  SysInfo(@SystemInf);
  {$ELSE}
  SysInfo(SystemInf);
  {$ENDIF FPC}
  Result := SystemInf.totalram;
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwTotalPhys;
end;
{$ENDIF MSWINDOWS}

function GetFreePhysicalMemory: Cardinal;
{$IFDEF UNIX}
var
  SystemInf: TSysInfo;
begin
  {$IFDEF FPC}
  SysInfo(@SystemInf);
  {$ELSE}
  SysInfo(SystemInf);
  {$ENDIF FPC}
  Result := SystemInf.freeram;
end;
{$ENDIF UNIX}
{$IFDEF MSWINDOWS}
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwAvailPhys;
end;

function GetTotalPageFileMemory: Cardinal;
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwTotalPageFile;
end;

function GetFreePageFileMemory: Cardinal;
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwAvailPageFile;
end;

function GetTotalVirtualMemory: Cardinal;
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwTotalVirtual;
end;

function GetFreeVirtualMemory: Cardinal;
var
  MemoryStatus: TMemoryStatus;
begin
  FillChar(MemoryStatus, SizeOf(MemoryStatus), 0);
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  Result := MemoryStatus.dwAvailVirtual;
end;

//=== Keyboard Information ===================================================

function GetKeybStateHelper(VirtualKey: Cardinal; Mask: Byte): Boolean;
var
  Keys: TKeyboardState;
begin
  Result := GetKeyBoardState(Keys) and (Keys[VirtualKey] and Mask <> 0);
end;

function GetKeyState(const VirtualKey: Cardinal): Boolean;
begin
  Result := GetKeybStateHelper(VirtualKey, $80);
end;

function GetNumLockKeyState: Boolean;
begin
  Result := GetKeybStateHelper(VK_NUMLOCK, $01);
end;

function GetScrollLockKeyState: Boolean;
begin
  Result := GetKeybStateHelper(VK_SCROLL, $01);
end;

function GetCapsLockKeyState: Boolean;
begin
  Result := GetKeybStateHelper(VK_CAPITAL, $01);
end;

//=== Windows 95/98/ME system resources information ==========================

{ TODO -oPJH : compare to Win9xFreeSysResources }
var
  ResmeterLibHandle: THandle;
  MyGetFreeSystemResources: function(ResType: UINT): UINT; stdcall;

procedure UnloadSystemResourcesMeterLib;
begin
  if ResmeterLibHandle <> 0 then
  begin
    FreeLibrary(ResmeterLibHandle);
    ResmeterLibHandle := 0;
    @MyGetFreeSystemResources := nil;
  end;
end;

function IsSystemResourcesMeterPresent: Boolean;

  procedure LoadResmeter;
  begin
    ResmeterLibHandle := SafeLoadLibrary('rsrc32.dll', SEM_FAILCRITICALERRORS);
    if ResmeterLibHandle <> 0 then
    begin
      @MyGetFreeSystemResources := GetProcAddress(ResmeterLibHandle, '_MyGetFreeSystemResources32@4');
      if not Assigned(MyGetFreeSystemResources) then
        UnloadSystemResourcesMeterLib;
    end;
  end;

begin
  if not IsWinNT and (ResmeterLibHandle = 0) then
    LoadResmeter;
  Result := (ResmeterLibHandle <> 0);
end;

function GetFreeSystemResources(const ResourceType: TFreeSysResKind): Integer;
const
  ParamValues: array [TFreeSysResKind] of UINT = (0, 1, 2);
begin
  if IsSystemResourcesMeterPresent then
    Result := MyGetFreeSystemResources(ParamValues[ResourceType])
  else
    Result := -1;
end;

function GetFreeSystemResources: TFreeSystemResources;
begin
  with Result do
  begin
    SystemRes := GetFreeSystemResources(rtSystem);
    GdiRes := GetFreeSystemResources(rtGdi);
    UserRes := GetFreeSystemResources(rtUser);
  end;
end;

function GetBPP: Cardinal;
var
  DC: HDC;
begin
  DC := GetDC(HWND_DESKTOP);
  if DC <> 0 then
  begin
    Result := GetDeviceCaps(DC, BITSPIXEL) * GetDeviceCaps(DC, PLANES);
    ReleaseDC(HWND_DESKTOP, DC);
  end
  else
    Result := 0;
end;

//=== Installed programs =====================================================

function ProgIDExists(const ProgID: string): Boolean;
var
  Tmp: TGUID;
  WideProgID: WideString;
begin
  WideProgID := ProgID;
  Result := Succeeded(CLSIDFromProgID(PWideChar(WideProgID), Tmp));
end;

function IsWordInstalled: Boolean;
begin
  Result := ProgIDExists('Word.Application');
end;

function IsExcelInstalled: Boolean;
begin
  Result := ProgIDExists('Excel.Application');
end;

function IsAccessInstalled: Boolean;
begin
  Result := ProgIDExists('Access.Application');
end;

function IsPowerPointInstalled: Boolean;
begin
  Result := ProgIDExists('PowerPoint.Application');
end;

function IsFrontPageInstalled: Boolean;
begin
  Result := ProgIDExists('FrontPage.Application');
end;

function IsOutlookInstalled: Boolean;
begin
  Result := ProgIDExists('Outlook.Application');
end;

function IsInternetExplorerInstalled: Boolean;
begin
  Result := ProgIDExists('InternetExplorer.Application');
end;

function IsMSProjectInstalled: Boolean;
begin
  Result := ProgIDExists('MSProject.Application');
end;

function IsOpenOfficeInstalled: Boolean;
begin
  Result := ProgIDExists('com.sun.star.ServiceManager');
end;

//=== Initialization/Finalization ============================================

procedure InitSysInfo;
var
  SystemInfo: TSystemInfo;
  Kernel32FileName: string;
  VerFixedFileInfo: TVSFixedFileInfo;
begin
  { processor information related initialization }

  FillChar(SystemInfo, SizeOf(SystemInfo), 0);
  GetSystemInfo(SystemInfo);
  ProcessorCount := SystemInfo.dwNumberOfProcessors;
  AllocGranularity := SystemInfo.dwAllocationGranularity;
  PageSize := SystemInfo.dwPageSize;

  { Windows version information }

  IsWinNT := Win32Platform = VER_PLATFORM_WIN32_NT;

  Kernel32FileName := GetModulePath(GetModuleHandle(kernel32));
  if (not IsWinNT) and VersionFixedFileInfo(Kernel32FileName, VerFixedFileInfo) then
    KernelVersionHi := VerFixedFileInfo.dwProductVersionMS
  else
    KernelVersionHi := 0;

  case GetWindowsVersion of
    wvUnknown:
      ;
    wvWin95:
      IsWin95 := True;
    wvWin95OSR2:
      IsWin95OSR2 := True;
    wvWin98:
      IsWin98 := True;
    wvWin98SE:
      IsWin98SE := True;
    wvWinME:
      IsWinME := True;
    wvWinNT31:
      begin
        IsWinNT3 := True;
        IsWinNT31 := True;
      end;
    wvWinNT35:
      begin
        IsWinNT3 := True;
        IsWinNT35 := True;
      end;
    wvWinNT351:
      begin
        IsWinNT3 := True;
        IsWinNT35 := True;
        IsWinNT351 := True;
      end;
    wvWinNT4:
      IsWinNT4 := True;
    wvWin2000:
      IsWin2K := True;
    wvWinXP:
      IsWinXP := True;
    wvWin2003:
      IsWin2003 := True;
    wvWinXP64:
      IsWinXP64 := True;
    wvWin2003R2:
      IsWin2003R2 := True;
    wvWinVista:
      IsWinVista := True;
    wvWinServer2008:
      IsWinServer2008 := True;
  end;
end;

procedure FinalizeSysInfo;
begin
  UnloadSystemResourcesMeterLib;
end;

initialization
  InitSysInfo;
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  FinalizeSysInfo;

{$ENDIF MSWINDOWS}
{$ENDIF ~CLR}

end.
