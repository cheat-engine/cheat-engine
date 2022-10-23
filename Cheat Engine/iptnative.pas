unit iptnative;

{$mode ObjFPC}{$H+}

{
Copyright 2018 Alex Ionescu. All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided
that the following conditions are met:
1. Redistributions of source code must retain the above copyright notice, this list of conditions and
   the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions
   and the following disclaimer in the documentation and/or other materials provided with the
   distribution.

THIS SOFTWARE IS PROVIDED BY ALEX IONESCU ``AS IS'' AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL ALEX IONESCU
OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are those of the authors and
should not be interpreted as representing official policies, either expressed or implied, of Alex Ionescu.

Ported to pascal by Eric Heijnen 2022
}

interface

uses
  Classes, SysUtils{$ifdef darwin},ctypes, macport{$endif}{$ifdef windows}, Windows{$endif};


  const
    IPT_BUFFER_MAJOR_VERSION_V1=1;
    IPT_BUFFER_MAJOR_VERSION_CURRENT=IPT_BUFFER_MAJOR_VERSION_V1;
    IPT_BUFFER_MINOR_VERSION_V0=0;
    IPT_BUFFER_MINOR_VERSION_CURRENT=IPT_BUFFER_MINOR_VERSION_V0;

    IPT_TRACE_VERSION_V1=1;
    IPT_TRACE_VERSION_CURRENT=IPT_TRACE_VERSION_V1;

  type
    //
    // See OnProcessCreate
    //
    IPT_MATCH_SETTINGS=(
      IptMatchByAnyApp,
      IptMatchByImageFileName,
      IptMatchByAnyPackage,
      IptMatchByPackageName
    );

    PIPT_MATCH_SETTINGS=^IPT_MATCH_SETTINGS;


    //
    // See GetIptOptionForTracingThreads vs GetIptOptionForTracingCores
    //
    IPT_MODE_SETTINGS=(
      IptCtlUserModeOnly,                 // Sets BranchEn[2000], ToPA[100], User[8]
      IptCtlKernelModeOnly,               // Sets BranchEn[2000], ToPA[100], OS[4]
      IptCtlUserAndKernelMode,            // Sets BranchEn[2000], ToPA[100], User[8], OS[4]

      //
      // Set through registry (IptOptions)
      //
      IptRegUserModeOnly,                 // Sets BranchEn[2000], ToPA[100], User[8]
      IptRegKernelModeOnly,               // Sets BranchEn[2000], ToPA[100], OS[4]
      IptRegUserAndKernelMode             // Sets BranchEn[2000], ToPA[100], User[8], OS[4]
    );
    PIPT_MODE_SETTINGS=^IPT_MODE_SETTINGS;

    IPT_TIMING_SETTINGS=(
      IptNoTimingPackets,                 // No additional IA32_RTIT_CTL bits enabled
      IptEnableMtcPackets,                // Sets MTCEn[200], TSCEn[400]. Requires CPUID.(EAX=014H,ECX=0H):EBX[3]= 1
      IptEnableCycPackets                 // Sets MTCEn[200], TSCEn[400], CYCEn[2]. Requires CPUID.(EAX=014H,ECX=0H):EBX[1]= 1
    );

    IPT_FILTER_RANGE_SETTINGS=(
      IptFilterRangeDisable,              // Sets ADDRn_CFG[0]
      IptFilterRangeIp,                   // Sets ADDRn_CFG[1]
      IptFilterRangeTraceStop            // Sets ADDRn_CFG[2]
    );

    IPT_OPTIONS=record
      case boolean of
        false: (flags: bitpacked record
          OptionVersion:0..15;
          TimingSettings:0..15;
          MtcFrequency: 0..15;
          CycThreshold: 0..15;
          TopaPagesPow2: 0..15;
          MatchSettings: 0..7;
          Inherit: 0..1;
          ModeSettings: 0..15;
        end);
        true: (AsUlongLong: QWORD);
    end;

    PIPT_OPTIONS=^IPT_OPTIONS;

  {$if sizeof(IPT_OPTIONS)<>8}
  {$error invalid size of IPT_OPTIONS}
  {$endif}

    IPT_TRACE_DATA=record
      TraceVersion: WORD;
      ValidTrace: WORD;
      TraceSize: DWORD;
      TraceData: array [0..0] of BYTE;   //IPT_TRACE_HEADER
    end;
    PIPT_TRACE_DATA=^IPT_TRACE_DATA;

    IPT_TRACE_HEADER=record
      ThreadId: QWORD;
      TimingSettings: IPT_TIMING_SETTINGS;
      MtcFrequency: DWORD;
      FrequencyToTscRatio: DWORD;
      RingBufferOffset: DWORD;
      TraceSize: DWORD;
      Trace: array [0..0] of BYTE;
    end;
    PIPT_TRACE_HEADER=^IPT_TRACE_HEADER;



  IPT_INPUT_TYPE=(
    IptInvalid=-1,
    IptGetTraceVersion=0,
    IptGetProcessTraceSize,
    IptGetProcessTrace,                 // Use IOCTL_IPT_READ_TRACE
    IptStartCoreTracing,
    IptRegisterExtendedImageForTracing,
    IptStartProcessTrace,
    IptStopProcessTrace,
    IptPauseThreadTrace,
    IptResumeThreadTrace,
    IptQueryProcessTrace,
    IptQueryCoreTrace,
    // Requests available since Windows 10 v2004
    IptStopTraceOnEachCore = 12,
    IptConfigureThreadAddressFilterRange,
    IptQueryThreadAddressFilterRange,
    IptQueryThreadTraceStopRangeEntered
  );

  IPT_BUFFER_VERSION=record
    BufferMajorVersion: DWORD;
    BufferMinorVersion: DWORD;
  end;
  PIPT_BUFFER_VERSION=^IPT_BUFFER_VERSION;

  IPT_INPUT_BUFFER=record
    BufferMajorVersion: DWORD;
    BufferMinorVersion: DWORD;
    inputType: IPT_INPUT_TYPE;
    case IPT_INPUT_TYPE of
      IptGetProcessTraceSize: (GetProcessIptTraceSize: record
        TraceVersion: USHORT;
        ProcessHandle: ULONG64;
      end);

      IptGetProcessTrace: (GetProcessIptTrace: record
        TraceVersion: USHORT;
        ProcessHandle: ULONG64;
      end);

      IptStartCoreTracing: (StartCoreIptTracing: record
        Options: IPT_OPTIONS;
        NumberOfTries: ULONG;
        TraceDurationInSeconds: ULONG;
      end);

      IptRegisterExtendedImageForTracing: (RegisterExtendedImageForIptTracing: record
        Options: IPT_OPTIONS;
        NumberOfTries: ULONG;
        TraceDurationInSeconds: ULONG;
        ImagePathLength: USHORT;
        FilteredPathLength: USHORT;
        ImageName: array [0..0] of WCHAR;
      end);

      IptStartProcessTrace: (StartProcessIptTrace: record
        ProcessHandle: ULONG64;
        Options: IPT_OPTIONS;
      end);

      IptStopProcessTrace: (StopProcessIptTrace: record
        ProcessHandle: ULONG64;
      end);

      IptPauseThreadTrace: (PauseThreadIptTrace: record
        ThreadHandle: ULONG64;
      end);

      IptResumeThreadTrace: (ResumeThreadIptTrace: record
        ThreadHandle: ULONG64;
      end);

      IptQueryProcessTrace: (QueryProcessIptTrace: record
        ProcessHandle: ULONG64;
      end);

      IptConfigureThreadAddressFilterRange: (ConfigureThreadAddressFilterRange: record
        ThreadHandle  :ULONG64;
        RangeIndex    :ULONG;
        RangeConfig   :ULONG;
        StartAddress  :ULONG64;
        EndAddress    :ULONG64;
      end);

      IptQueryThreadAddressFilterRange: (QueryThreadAddressFilterRange: record
        ThreadHandle: ULONG64;
        RangeIndex: ULONG;
      end);

      IptQueryThreadTraceStopRangeEntered: (QueryThreadTraceStopRangeEntered: record
        ThreadHandle: ULONG64;
      end);
  end;

  PIPT_INPUT_BUFFER=^IPT_INPUT_BUFFER;

  {$if sizeof(IPT_INPUT_BUFFER)<>$30}
  {$error invalid size of IPT_INPUT_BUFFER}
  {$endif}

  IPT_OUTPUT_BUFFER=record
    case integer of
      0: (GetBufferMajorVersion: record
            version: IPT_BUFFER_VERSION;
          end);

      2: (GetTraceVersion: record
            TraceVersion: USHORT;
         end);

      3: (GetTraceSize: record
            TraceVersion: USHORT;
            TraceSize: QWORD;
          end);

      4: (GetTrace: record
            data: IPT_TRACE_DATA;
          end);

      5: (PauseTrace: record
            OldState: BOOL;
          end);

      6: (ResumeTrace: record
            OldState: BOOL;
          end);

      7: (QueryProcessTrace: record
            Options: IPT_OPTIONS;
          end);

      8: (QueryCoreTrace: record
            Options: IPT_OPTIONS;
          end);

      9: (QueryThreadAddressFilterRange: record
             RangeConfig :   ULONG;
             StartAddress:   ULONG64;
             EndAddress  :   ULONG64;
          end);

      10: (QueryThreadTraceStopRangeEntered: record
            TraceStopRangeEntered: BOOLEAN;
          end);
  end;

  PIPT_OUTPUT_BUFFER=^IPT_OUTPUT_BUFFER;

  {$if sizeof(IPT_OUTPUT_BUFFER)<>$18}
  {$error invalid size of IPT_OUTPUT_BUFFER}
  {$endif}

  {$ifdef windows}
//
// IOCTLs that the IPT Driver Handles
//
const
  FILE_ANY_ACCESS      =           0;
  FILE_GENERIC_READ    =           STANDARD_RIGHTS_READ or FILE_READ_DATA or FILE_READ_ATTRIBUTES or FILE_READ_EA or SYNCHRONIZE;
  METHOD_BUFFERED      =           0;
  METHOD_IN_DIRECT     =           1;
  METHOD_OUT_DIRECT    =           2;
  METHOD_NEITHER       =           3;
  FILE_DEVICE_UNKNOWN  =           $00000022;

  IOCTL_IPT_REQUEST = ((FILE_DEVICE_UNKNOWN) shl 16) or ((FILE_ANY_ACCESS) shl 14) or ((1) shl 2) or (METHOD_BUFFERED);
  IOCTL_IPT_READ_TRACE = ((FILE_DEVICE_UNKNOWN) shl 16) or ((FILE_ANY_ACCESS) shl 14) or ((1) shl 2) or (METHOD_OUT_DIRECT);

procedure InitializeIptBuffer(out input: IPT_INPUT_BUFFER;  InputType: IPT_INPUT_TYPE);
function OpenIptDevice(out h: THandle): boolean;
function GetIptBufferVersion(out pdwBufferMajorVersion: DWORD): boolean;
function GetIptTraceVersion(out pwTraceVersion: WORD): boolean;

function GetProcessIptTraceSize(hProcess: THandle; out dwTraceSize: DWORD): boolean;
function GetProcessIptTrace(hProcess: THandle; pTrace: PIPT_TRACE_DATA; dwTraceSize: DWORD): boolean;

function StartProcessIptTracing(hProcess: THandle; ullOptions: IPT_OPTIONS): boolean;
function StopProcessIptTracing(hProcess: THandle): boolean;

function PauseThreadIptTracing(hThread: THandle; out r: BOOLEAN): boolean;
function ResumeThreadIptTracing(hThread: THandle; out r: BOOLEAN): boolean;

function ConfigureThreadAddressFilterRange(hThread: THandle; dwRangeIndex: DWORD; dwRangeConfig: IPT_FILTER_RANGE_SETTINGS; ullStartAddress: QWORD; ullEndAddress: QWORD): boolean;
function QueryThreadAddressFilterRange(hThread: THandle; dwRangeIndex: DWORD; out dwRangeConfig: IPT_FILTER_RANGE_SETTINGS; out ullStartAddress: QWORD; out ullEndAddress: QWORD): boolean;
{$endif}

implementation

{$ifdef windows}
procedure InitializeIptBuffer(out input: IPT_INPUT_BUFFER;  InputType: IPT_INPUT_TYPE);
begin
  zeromemory(@input, sizeof(input));
  input.BufferMajorVersion:=IPT_BUFFER_MAJOR_VERSION_CURRENT;
  input.BufferMinorVersion:=IPT_BUFFER_MINOR_VERSION_CURRENT;
  input.InputType:=InputType;
end;

var hasEnabledIPTService: boolean=false;
function OpenIptDevice(out h: THandle): boolean;
var
  hSCManager: THandle;
  hService: THandle;
begin
  if not hasEnabledIPTService then
  begin
    hSCManager := OpenSCManager(nil, nil, GENERIC_READ or GENERIC_WRITE);
    if (hSCManager<>0) and (hSCManager<>INVALID_HANDLE_VALUE) then
    begin
      hService := OpenServiceW(hSCManager, pwidechar('IPT'), SERVICE_START);

      if (hService<>0) and (hService<>INVALID_HANDLE_VALUE) then
      begin
        StartService(hService,0,nil);
        CloseServiceHandle(hservice);
      end;

      CloseServiceHandle(hSCManager);
    end;
    hasEnabledIPTService:=true;
  end;


  h:=CreateFileW('\??\IPT', FILE_GENERIC_READ,
                               FILE_SHARE_READ,
                               nil,
                               OPEN_EXISTING,
                               FILE_ATTRIBUTE_NORMAL or
                               FILE_FLAG_SEQUENTIAL_SCAN or
                               FILE_FLAG_NO_BUFFERING,
                               0);

  exit((h<>0) and (h<>INVALID_HANDLE_VALUE));
end;

function GetIptBufferVersion(out pdwBufferMajorVersion: DWORD): boolean;
var hIpt: THandle;
  inputBuffer: IPT_INPUT_BUFFER;
  outputBuffer: IPT_OUTPUT_BUFFER;

  br: dword;
begin
  result:=false;
  if OpenIptDevice(hIpt) then
  begin
    InitializeIptBuffer(inputBuffer, IptInvalid);

    result:=DeviceIoControl(hIpt,
                            IOCTL_IPT_REQUEST,
                            @inputBuffer,
                            sizeof(IPT_BUFFER_VERSION),
                            @outputBuffer,
                            sizeof(outputBuffer),
                            br,
                            nil);

    if result then
      pdwBufferMajorVersion := outputBuffer.GetBufferMajorVersion.version.BufferMajorVersion;

    closehandle(hIpt);
  end;
end;

function GetIptTraceVersion(out pwTraceVersion: WORD): boolean;
var hIpt: THandle;
  inputBuffer: IPT_INPUT_BUFFER;
  outputBuffer: IPT_OUTPUT_BUFFER;

  br: dword;
begin
  result:=false;
  if OpenIptDevice(hIpt) then
  begin
    InitializeIptBuffer(inputBuffer, IptGetTraceVersion);

    result:=DeviceIoControl(hIpt,
                            IOCTL_IPT_REQUEST,
                            @inputBuffer,
                            sizeof(inputBuffer),
                            @outputBuffer,
                            sizeof(outputBuffer),
                            br,
                            nil);

    if result then
      pwTraceVersion := outputBuffer.GetTraceVersion.TraceVersion;

    closehandle(hIpt);
  end;
end;

function GetProcessIptTraceSize(hProcess: THandle; out dwTraceSize: DWORD): boolean;
var hIpt: THandle;
  inputBuffer: IPT_INPUT_BUFFER;
  outputBuffer: IPT_OUTPUT_BUFFER;

  br: dword;
begin
  result:=false;
  if OpenIptDevice(hIpt) then
  begin
    InitializeIptBuffer(inputBuffer, IptGetProcessTraceSize);
    inputBuffer.GetProcessIptTraceSize.TraceVersion:=IPT_TRACE_VERSION_CURRENT;
    inputBuffer.GetProcessIptTraceSize.ProcessHandle:=hProcess;

    result:=DeviceIoControl(hIpt,
                            IOCTL_IPT_REQUEST,
                            @inputBuffer,
                            sizeof(inputBuffer),
                            @outputBuffer,
                            sizeof(outputBuffer),
                            br,
                            nil);

    if result then
    begin
      dwTraceSize := outputBuffer.GetTraceSize.TraceSize;
      if outputBuffer.GetTraceSize.TraceSize>$ffffffff then result:=false; //not yet supported
    end;

    closehandle(hIpt);
  end;
end;

function GetProcessIptTrace(hProcess: THandle; pTrace: PIPT_TRACE_DATA; dwTraceSize: DWORD): boolean;
var hIpt: THandle;
  inputBuffer: IPT_INPUT_BUFFER;

  br: dword;
begin
  result:=false;
  if dwTraceSize<8 then exit;


  if OpenIptDevice(hIpt) then
  begin
    InitializeIptBuffer(inputBuffer, IptGetProcessTrace);
    inputBuffer.GetProcessIptTrace.TraceVersion:=IPT_TRACE_VERSION_CURRENT;
    inputBuffer.GetProcessIptTrace.ProcessHandle:=hProcess;

    result:=DeviceIoControl(hIpt,
                            IOCTL_IPT_READ_TRACE,
                            @inputBuffer,
                            sizeof(inputBuffer),
                            pTrace,
                            dwTraceSize,
                            br,
                            nil);

    closehandle(hIpt);
  end;
end;

function StartProcessIptTracing(hProcess: THandle; ullOptions: IPT_OPTIONS): boolean;
var hIpt: THandle;
  inputBuffer: IPT_INPUT_BUFFER;
  outputBuffer: IPT_OUTPUT_BUFFER;

  br: dword;
begin
  result:=false;

  if OpenIptDevice(hIpt) then
  begin
    InitializeIptBuffer(inputBuffer, IptStartProcessTrace);
    inputBuffer.StartProcessIptTrace.Options:=ullOptions;
    inputBuffer.StartProcessIptTrace.ProcessHandle:=hProcess;

    result:=DeviceIoControl(hIpt,
                            IOCTL_IPT_REQUEST,
                            @inputBuffer,
                            sizeof(inputBuffer),
                            @outputBuffer,
                            sizeof(outputBuffer),
                            br,
                            nil);

    closehandle(hIpt);
  end;
end;

function StopProcessIptTracing(hProcess: THandle): boolean;
var hIpt: THandle;
  inputBuffer: IPT_INPUT_BUFFER;
  outputBuffer: IPT_OUTPUT_BUFFER;

  br: dword;
begin
  result:=false;

  if OpenIptDevice(hIpt) then
  begin
    InitializeIptBuffer(inputBuffer, IptStopProcessTrace);
    inputBuffer.StopProcessIptTrace.ProcessHandle:=hProcess;

    result:=DeviceIoControl(hIpt,
                            IOCTL_IPT_REQUEST,
                            @inputBuffer,
                            sizeof(inputBuffer),
                            @outputBuffer,
                            sizeof(outputBuffer),
                            br,
                            nil);

    closehandle(hIpt);
  end;
end;

function PauseThreadIptTracing(hThread: THandle; out r: BOOLEAN): boolean;
var hIpt: THandle;
  inputBuffer: IPT_INPUT_BUFFER;
  outputBuffer: IPT_OUTPUT_BUFFER;

  br: dword;
begin
  result:=false;

  if OpenIptDevice(hIpt) then
  begin
    InitializeIptBuffer(inputBuffer, IptPauseThreadTrace);
    inputBuffer.PauseThreadIptTrace.ThreadHandle:=hThread;

    result:=DeviceIoControl(hIpt,
                            IOCTL_IPT_REQUEST,
                            @inputBuffer,
                            sizeof(inputBuffer),
                            @outputBuffer,
                            sizeof(outputBuffer),
                            br,
                            nil);

    if result then
      r:=outputBuffer.PauseTrace.OldState;

    closehandle(hIpt);
  end;
end;

function ResumeThreadIptTracing(hThread: THandle; out r: BOOLEAN): boolean;
var hIpt: THandle;
  inputBuffer: IPT_INPUT_BUFFER;
  outputBuffer: IPT_OUTPUT_BUFFER;

  br: dword;
begin
  result:=false;

  if OpenIptDevice(hIpt) then
  begin
    InitializeIptBuffer(inputBuffer, IptResumeThreadTrace);
    inputBuffer.ResumeThreadIptTrace.ThreadHandle:=hThread;

    result:=DeviceIoControl(hIpt,
                            IOCTL_IPT_REQUEST,
                            @inputBuffer,
                            sizeof(inputBuffer),
                            @outputBuffer,
                            sizeof(outputBuffer),
                            br,
                            nil);

    if result then
      r:=outputBuffer.ResumeTrace.OldState;

    closehandle(hIpt);
  end;
end;

function ConfigureThreadAddressFilterRange(hThread: THandle; dwRangeIndex: DWORD; dwRangeConfig: IPT_FILTER_RANGE_SETTINGS; ullStartAddress: QWORD; ullEndAddress: QWORD): boolean;
var hIpt: THandle;
  inputBuffer: IPT_INPUT_BUFFER;
  outputBuffer: IPT_OUTPUT_BUFFER;

  br: dword;
begin
  result:=false;

  if OpenIptDevice(hIpt) then
  begin
    InitializeIptBuffer(inputBuffer, IptConfigureThreadAddressFilterRange);
    inputBuffer.ConfigureThreadAddressFilterRange.ThreadHandle:=hThread;
    inputBuffer.ConfigureThreadAddressFilterRange.RangeIndex:=dwRangeIndex;
    inputBuffer.ConfigureThreadAddressFilterRange.RangeConfig:=integer(dwRangeConfig);
    inputBuffer.ConfigureThreadAddressFilterRange.StartAddress:=ullStartAddress;
    inputBuffer.ConfigureThreadAddressFilterRange.EndAddress:=ullEndAddress;

    result:=DeviceIoControl(hIpt,
                            IOCTL_IPT_REQUEST,
                            @inputBuffer,
                            sizeof(inputBuffer),
                            @outputBuffer,
                            sizeof(outputBuffer),
                            br,
                            nil);

    closehandle(hIpt);
  end;
end;

function QueryThreadAddressFilterRange(hThread: THandle; dwRangeIndex: DWORD; out dwRangeConfig: IPT_FILTER_RANGE_SETTINGS; out ullStartAddress: QWORD; out ullEndAddress: QWORD): boolean;
var hIpt: THandle;
  inputBuffer: IPT_INPUT_BUFFER;
  outputBuffer: IPT_OUTPUT_BUFFER;

  br: dword;
begin
  result:=false;

  if OpenIptDevice(hIpt) then
  begin
    InitializeIptBuffer(inputBuffer, IptQueryThreadAddressFilterRange);
    inputBuffer.QueryThreadAddressFilterRange.ThreadHandle:=hThread;
    inputBuffer.QueryThreadAddressFilterRange.RangeIndex:=dwRangeIndex;

    result:=DeviceIoControl(hIpt,
                            IOCTL_IPT_REQUEST,
                            @inputBuffer,
                            sizeof(inputBuffer),
                            @outputBuffer,
                            sizeof(outputBuffer),
                            br,
                            nil);

    if result then
    begin
      dwRangeConfig:=IPT_FILTER_RANGE_SETTINGS(outputBuffer.QueryThreadAddressFilterRange.RangeConfig);
      ullStartAddress:=outputBuffer.QueryThreadAddressFilterRange.StartAddress;
      ullEndAddress:=outputBuffer.QueryThreadAddressFilterRange.EndAddress;
    end;

    closehandle(hIpt);
  end;
end;

{$endif}

finalization

end.

