{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit mscoree_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : $Revision: 2175 $
// File generated on 14.12.2003 01:39:55 from Type Library described below.

// ************************************************************************  //
// Type Lib: F:\WINNT\Microsoft.NET\Framework\v1.1.4322\mscoree.tlb (1)
// LIBID: {5477469E-83B1-11D2-8B49-00A0C9B7C9C4}
// LCID: 0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (F:\WINNT\system32\STDOLE2.TLB)
//   (2) v4.0 StdVCL, (F:\WINNT\system32\STDVCL40.DLL)
// Errors:
//   Hint: Member 'type' of 'tagSTATSTG' changed to 'type_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{ $WARN SYMBOL_PLATFORM OFF}
{ $WRITEABLECONST ON}
{ $VARPROPSETTER ON}

{$I jedi.inc}

{$IFDEF SUPPORTS_WEAKPACKAGEUNIT}
{$WEAKPACKAGEUNIT ON}
{$ENDIF SUPPORTS_WEAKPACKAGEUNIT}

interface

uses ActiveX, Classes;

{$HPPEMIT '#include <winnt.h>'}

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  mscoreeMajorVersion = 1;
  mscoreeMinorVersion = 1;

  LIBID_mscoree: TGUID = '{5477469E-83B1-11D2-8B49-00A0C9B7C9C4}';

  IID_IApartmentCallback: TGUID = '{178E5337-1528-4591-B1C9-1C6E484686D8}';
  IID_IManagedObject: TGUID = '{C3FCC19E-A970-11D2-8B5A-00A0C9B7C9C4}';
  IID_ICatalogServices: TGUID = '{04C6BE1E-1DB1-4058-AB7A-700CCCFBF254}';
  IID_IMarshal: TGUID = '{00000003-0000-0000-C000-000000000046}';
  CLASS_ComCallUnmarshal: TGUID = '{3F281000-E95A-11D2-886B-00C04F869F04}';
  IID_ISequentialStream: TGUID = '{0C733A30-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IStream: TGUID = '{0000000C-0000-0000-C000-000000000046}';
  IID_ICorRuntimeHost: TGUID = '{CB2F6722-AB3A-11D2-9C40-00C04FA30A3E}';
  IID_IGCHost: TGUID = '{FAC34F6E-0DCD-47B5-8021-531BC5ECCA63}';
  IID_ICorConfiguration: TGUID = '{5C2B07A5-1E98-11D3-872F-00C04F79ED0D}';
  IID_IGCThreadControl: TGUID = '{F31D1788-C397-4725-87A5-6AF3472C2791}';
  IID_IGCHostControl: TGUID = '{5513D564-8374-4CB9-AED9-0083F4160A1D}';
  IID_IDebuggerThreadControl: TGUID = '{23D86786-0BB5-4774-8FB5-E3522ADD6246}';
  IID_IValidator: TGUID = '{63DF8730-DC81-4062-84A2-1FF943F59FAC}';
  IID_IDebuggerInfo: TGUID = '{BF24142D-A47D-4D24-A66D-8C2141944E44}';
  IID_IVEHandler: TGUID = '{856CA1B2-7DAB-11D3-ACEC-00C04F86C309}';
  CLASS_CorRuntimeHost: TGUID = '{CB2F6723-AB3A-11D2-9C40-00C04FA30A3E}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IApartmentCallback = interface;
  IManagedObject = interface;
  ICatalogServices = interface;
  IMarshal = interface;
  ISequentialStream = interface;
  IStream = interface;
  ICorRuntimeHost = interface;
  IGCHost = interface;
  ICorConfiguration = interface;
  IGCThreadControl = interface;
  IGCHostControl = interface;
  IDebuggerThreadControl = interface;
  IValidator = interface;
  IDebuggerInfo = interface;
  IVEHandler = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  ComCallUnmarshal = IMarshal;
  CorRuntimeHost = ICorRuntimeHost;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PUserType1 = ^TGUID; {*}
  PPUserType1 = ^ISequentialStream; {*}
  PByte1 = ^Byte; {*}
  PUINT1 = ^LongWord; {*}

  ULONG_PTR = LongWord;
  {$EXTERNALSYM ULONG_PTR}

  _LARGE_INTEGER = packed record
    QuadPart: Int64;
  end;

  _ULARGE_INTEGER = packed record
    QuadPart: Largeuint;
  end;

  _FILETIME = packed record
    dwLowDateTime: LongWord;
    dwHighDateTime: LongWord;
  end;
  {$EXTERNALSYM _FILETIME}

  tagSTATSTG = packed record
    pwcsName: PWideChar;
    type_: LongWord;
    cbSize: _ULARGE_INTEGER;
    mtime: _FILETIME;
    ctime: _FILETIME;
    atime: _FILETIME;
    grfMode: LongWord;
    grfLocksSupported: LongWord;
    clsid: TGUID;
    grfStateBits: LongWord;
    reserved: LongWord;
  end;
  {$EXTERNALSYM tagSTATSTG}

  _COR_GC_STATS = packed record
    Flags: LongWord;
    ExplicitGCCount: ULONG_PTR;
    GenCollectionsTaken: array[0..2] of ULONG_PTR;
    CommittedKBytes: ULONG_PTR;
    ReservedKBytes: ULONG_PTR;
    Gen0HeapSizeKBytes: ULONG_PTR;
    Gen1HeapSizeKBytes: ULONG_PTR;
    Gen2HeapSizeKBytes: ULONG_PTR;
    LargeObjectHeapSizeKBytes: ULONG_PTR;
    KBytesPromotedFromGen0: ULONG_PTR;
    KBytesPromotedFromGen1: ULONG_PTR;
  end;

  _COR_GC_THREAD_STATS = packed record
    PerThreadAllocation: Largeuint;
    Flags: LongWord;
  end;

  tag_VerError = packed record
    Flags: LongWord;
    opcode: LongWord;
    uOffset: LongWord;
    Token: LongWord;
    item1_flags: LongWord;
    item1_data: ^SYSINT;
    item2_flags: LongWord;
    item2_data: ^SYSINT;
  end;


// *********************************************************************//
// Interface: IApartmentCallback
// Flags:     (256) OleAutomation
// GUID:      {178E5337-1528-4591-B1C9-1C6E484686D8}
// *********************************************************************//
  IApartmentCallback = interface(IUnknown)
    ['{178E5337-1528-4591-B1C9-1C6E484686D8}']
    function DoCallback(pFunc: ULONG_PTR; pData: ULONG_PTR): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IManagedObject
// Flags:     (256) OleAutomation
// GUID:      {C3FCC19E-A970-11D2-8B5A-00A0C9B7C9C4}
// *********************************************************************//
  IManagedObject = interface(IUnknown)
    ['{C3FCC19E-A970-11D2-8B5A-00A0C9B7C9C4}']
    function GetSerializedBuffer(out pBSTR: WideString): HResult; stdcall;
    function GetObjectIdentity(out pBSTRGUID: WideString; out AppDomainID: SYSINT; out pCCW: SYSINT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICatalogServices
// Flags:     (256) OleAutomation
// GUID:      {04C6BE1E-1DB1-4058-AB7A-700CCCFBF254}
// *********************************************************************//
  ICatalogServices = interface(IUnknown)
    ['{04C6BE1E-1DB1-4058-AB7A-700CCCFBF254}']
    function Autodone: HResult; stdcall;
    function NotAutodone: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IMarshal
// Flags:     (0)
// GUID:      {00000003-0000-0000-C000-000000000046}
// *********************************************************************//
  IMarshal = interface(IUnknown)
    ['{00000003-0000-0000-C000-000000000046}']
    function GetUnmarshalClass(var riid: TGUID; var pv: Pointer; dwDestContext: LongWord; 
                               var pvDestContext: Pointer; mshlflags: LongWord; out pCid: TGUID): HResult; stdcall;
    function GetMarshalSizeMax(var riid: TGUID; var pv: Pointer; dwDestContext: LongWord; 
                               var pvDestContext: Pointer; mshlflags: LongWord; out pSize: LongWord): HResult; stdcall;
    function MarshalInterface(var pstm: ISequentialStream; var riid: TGUID; var pv: Pointer; 
                              dwDestContext: LongWord; var pvDestContext: Pointer; 
                              mshlflags: LongWord): HResult; stdcall;
    function UnmarshalInterface(const pstm: ISequentialStream; var riid: TGUID; out ppv: Pointer): HResult; stdcall;
    function ReleaseMarshalData(const pstm: ISequentialStream): HResult; stdcall;
    function DisconnectObject(dwReserved: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISequentialStream
// Flags:     (0)
// GUID:      {0C733A30-2A1C-11CE-ADE5-00AA0044773D}
// *********************************************************************//
  ISequentialStream = interface(IUnknown)
    ['{0C733A30-2A1C-11CE-ADE5-00AA0044773D}']
    function Read(out pv: Pointer; cb: LongWord; out pcbRead: LongWord): HResult; stdcall;
    function RemoteRead(out pv: Byte; cb: LongWord; out pcbRead: LongWord): HResult; stdcall;
    function Write(var pv: Pointer; cb: LongWord; out pcbWritten: LongWord): HResult; stdcall;
    function RemoteWrite(var pv: Byte; cb: LongWord; out pcbWritten: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IStream
// Flags:     (0)
// GUID:      {0000000C-0000-0000-C000-000000000046}
// *********************************************************************//
  IStream = interface(ISequentialStream)
    ['{0000000C-0000-0000-C000-000000000046}']
    function Seek(dlibMove: _LARGE_INTEGER; dwOrigin: LongWord; out plibNewPosition: _ULARGE_INTEGER): HResult; stdcall;
    function RemoteSeek(dlibMove: _LARGE_INTEGER; dwOrigin: LongWord; 
                        out plibNewPosition: _ULARGE_INTEGER): HResult; stdcall;
    function SetSize(libNewSize: _ULARGE_INTEGER): HResult; stdcall;
    function CopyTo(const pstm: ISequentialStream; cb: _ULARGE_INTEGER; 
                    out pcbRead: _ULARGE_INTEGER; out pcbWritten: _ULARGE_INTEGER): HResult; stdcall;
    function RemoteCopyTo(const pstm: ISequentialStream; cb: _ULARGE_INTEGER; 
                          out pcbRead: _ULARGE_INTEGER; out pcbWritten: _ULARGE_INTEGER): HResult; stdcall;
    function Commit(grfCommitFlags: LongWord): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: _ULARGE_INTEGER; cb: _ULARGE_INTEGER; dwLockType: LongWord): HResult; stdcall;
    function UnlockRegion(libOffset: _ULARGE_INTEGER; cb: _ULARGE_INTEGER; dwLockType: LongWord): HResult; stdcall;
    function Stat(out pstatstg: tagSTATSTG; grfStatFlag: LongWord): HResult; stdcall;
    function Clone(out ppstm: ISequentialStream): HResult; stdcall;
  end;
  {$EXTERNALSYM IStream}

// *********************************************************************//
// Interface: ICorRuntimeHost
// Flags:     (0)
// GUID:      {CB2F6722-AB3A-11D2-9C40-00C04FA30A3E}
// *********************************************************************//
  ICorRuntimeHost = interface(IUnknown)
    ['{CB2F6722-AB3A-11D2-9C40-00C04FA30A3E}']
    function CreateLogicalThreadState: HResult; stdcall;
    function DeleteLogicalThreadState: HResult; stdcall;
    function SwitchInLogicalThreadState(var pFiberCookie: LongWord): HResult; stdcall;
    function SwitchOutLogicalThreadState(out pFiberCookie: PUINT1): HResult; stdcall;
    function LocksHeldByLogicalThread(out pCount: LongWord): HResult; stdcall;
    function MapFile(var hFile: Pointer; out hMapAddress: Pointer): HResult; stdcall;
    function GetConfiguration(out pConfiguration: ICorConfiguration): HResult; stdcall;
    function Start: HResult; stdcall;
    function Stop: HResult; stdcall;
    function CreateDomain(pwzFriendlyName: PWideChar; const pIdentityArray: IUnknown; 
                          out pAppDomain: IUnknown): HResult; stdcall;
    function GetDefaultDomain(out pAppDomain: IUnknown): HResult; stdcall;
    function EnumDomains(out hEnum: Pointer): HResult; stdcall;
    function NextDomain(hEnum: Pointer; out pAppDomain: IUnknown): HResult; stdcall;
    function CloseEnum(hEnum: Pointer): HResult; stdcall;
    function CreateDomainEx(pwzFriendlyName: PWideChar; const pSetup: IUnknown; 
                            const pEvidence: IUnknown; out pAppDomain: IUnknown): HResult; stdcall;
    function CreateDomainSetup(out pAppDomainSetup: IUnknown): HResult; stdcall;
    function CreateEvidence(out pEvidence: IUnknown): HResult; stdcall;
    function UnloadDomain(const pAppDomain: IUnknown): HResult; stdcall;
    function CurrentDomain(out pAppDomain: IUnknown): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IGCHost
// Flags:     (0)
// GUID:      {FAC34F6E-0DCD-47B5-8021-531BC5ECCA63}
// *********************************************************************//
  IGCHost = interface(IUnknown)
    ['{FAC34F6E-0DCD-47B5-8021-531BC5ECCA63}']
    function SetGCStartupLimits(SegmentSize: LongWord; MaxGen0Size: LongWord): HResult; stdcall;
    function Collect(Generation: Integer): HResult; stdcall;
    function GetStats(var pStats: _COR_GC_STATS): HResult; stdcall;
    function GetThreadStats(var pFiberCookie: LongWord; var pStats: _COR_GC_THREAD_STATS): HResult; stdcall;
    function SetVirtualMemLimit(sztMaxVirtualMemMB: ULONG_PTR): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ICorConfiguration
// Flags:     (0)
// GUID:      {5C2B07A5-1E98-11D3-872F-00C04F79ED0D}
// *********************************************************************//
  ICorConfiguration = interface(IUnknown)
    ['{5C2B07A5-1E98-11D3-872F-00C04F79ED0D}']
    function SetGCThreadControl(const pGCThreadControl: IGCThreadControl): HResult; stdcall;
    function SetGCHostControl(const pGCHostControl: IGCHostControl): HResult; stdcall;
    function SetDebuggerThreadControl(const pDebuggerThreadControl: IDebuggerThreadControl): HResult; stdcall;
    function AddDebuggerSpecialThread(dwSpecialThreadId: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IGCThreadControl
// Flags:     (0)
// GUID:      {F31D1788-C397-4725-87A5-6AF3472C2791}
// *********************************************************************//
  IGCThreadControl = interface(IUnknown)
    ['{F31D1788-C397-4725-87A5-6AF3472C2791}']
    function ThreadIsBlockingForSuspension: HResult; stdcall;
    function SuspensionStarting: HResult; stdcall;
    function SuspensionEnding(Generation: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IGCHostControl
// Flags:     (0)
// GUID:      {5513D564-8374-4CB9-AED9-0083F4160A1D}
// *********************************************************************//
  IGCHostControl = interface(IUnknown)
    ['{5513D564-8374-4CB9-AED9-0083F4160A1D}']
    function RequestVirtualMemLimit(sztMaxVirtualMemMB: ULONG_PTR; 
                                    var psztNewMaxVirtualMemMB: ULONG_PTR): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDebuggerThreadControl
// Flags:     (0)
// GUID:      {23D86786-0BB5-4774-8FB5-E3522ADD6246}
// *********************************************************************//
  IDebuggerThreadControl = interface(IUnknown)
    ['{23D86786-0BB5-4774-8FB5-E3522ADD6246}']
    function ThreadIsBlockingForDebugger: HResult; stdcall;
    function ReleaseAllRuntimeThreads: HResult; stdcall;
    function StartBlockingForDebugger(dwUnused: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IValidator
// Flags:     (0)
// GUID:      {63DF8730-DC81-4062-84A2-1FF943F59FAC}
// *********************************************************************//
  IValidator = interface(IUnknown)
    ['{63DF8730-DC81-4062-84A2-1FF943F59FAC}']
    function Validate(const veh: IVEHandler; const pAppDomain: IUnknown; ulFlags: LongWord; 
                      ulMaxError: LongWord; Token: LongWord; fileName: PWideChar; var pe: Byte; 
                      ulSize: LongWord): HResult; stdcall;
    function FormatEventInfo(hVECode: HResult; Context: tag_VerError; msg: PWideChar; 
                             ulMaxLength: LongWord; psa: PSafeArray): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IDebuggerInfo
// Flags:     (0)
// GUID:      {BF24142D-A47D-4D24-A66D-8C2141944E44}
// *********************************************************************//
  IDebuggerInfo = interface(IUnknown)
    ['{BF24142D-A47D-4D24-A66D-8C2141944E44}']
    function IsDebuggerAttached(out pbAttached: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IVEHandler
// Flags:     (0)
// GUID:      {856CA1B2-7DAB-11D3-ACEC-00C04F86C309}
// *********************************************************************//
  IVEHandler = interface(IUnknown)
    ['{856CA1B2-7DAB-11D3-ACEC-00C04F86C309}']
    function VEHandler(VECode: HResult; Context: tag_VerError; psa: PSafeArray): HResult; stdcall;
    function SetReporterFtn(lFnPtr: Int64): HResult; stdcall;
  end;

// *********************************************************************//
// The Class CoComCallUnmarshal provides a Create and CreateRemote method to          
// create instances of the default interface IMarshal exposed by              
// the CoClass ComCallUnmarshal. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoComCallUnmarshal = class
    class function Create: IMarshal;
    class function CreateRemote(const MachineName: string): IMarshal;
  end;

// *********************************************************************//
// The Class CoCorRuntimeHost provides a Create and CreateRemote method to          
// create instances of the default interface ICorRuntimeHost exposed by              
// the CoClass CorRuntimeHost. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCorRuntimeHost = class
    class function Create: ICorRuntimeHost;
    class function CreateRemote(const MachineName: string): ICorRuntimeHost;
  end;

implementation

uses ComObj;

class function CoComCallUnmarshal.Create: IMarshal;
begin
  Result := CreateComObject(CLASS_ComCallUnmarshal) as IMarshal;
end;

class function CoComCallUnmarshal.CreateRemote(const MachineName: string): IMarshal;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ComCallUnmarshal) as IMarshal;
end;

class function CoCorRuntimeHost.Create: ICorRuntimeHost;
begin
  Result := CreateComObject(CLASS_CorRuntimeHost) as ICorRuntimeHost;
end;

class function CoCorRuntimeHost.CreateRemote(const MachineName: string): ICorRuntimeHost;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CorRuntimeHost) as ICorRuntimeHost;
end;

end.
