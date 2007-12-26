unit globals;

interface

uses windows,SysUtils;

type TAPIInfo = record
  location: Pointer;
  Original: Array [0..4] of byte;
  Jump:     Array [0..4] of byte;
end;

type tfloatscan=(rounded,extremerounded,truncated);

type TScanSettings = record
  UseHyperscan: boolean;
  scanning: boolean;
  CEProcessID: dword;
  CEMainThreadID: Dword;
  applicantionhandle: thandle;
  mainformHandle: THandle;
  formscanningHandle: THandle;
  hyperscanwindow: Thandle;
  StartAddress: Dword;
  StopAddress: Dword;
  Scantype: Integer;
  ValueType: Integer;
  roundingtype: tfloatscan;
  scan:byte;
  readonly: boolean;
  FastScan: boolean;
  Hexadecimal: boolean;
  unicode: boolean;
  percentage: boolean;
  LowMemoryUsage: boolean;
  Skip_PAGE_NOCACHE:boolean;
  scan_mem_private:boolean;
  scan_mem_image:boolean;
  scan_mem_mapped: boolean;
  scanvalue: string[255];
  scanvalue2: string[255];
  CheatEngineDir: string[255];
  buffersize:dword;
  priority:integer;
  nrofbits:integer;
  bitstring: string[255];
  bitoffsetchange: integer;
  asktocontinue: boolean;
  HookDirect3d: boolean;
  HookOpenGL:   boolean;
  PacketEditor: boolean;
  Stealthed: boolean;
  hooknewprocesses: boolean;
end;

procedure InitializeScansettings;
procedure UninitializeScansettings;

var scansettings: ^TScanSettings;
    HyperscanFileMapping: THandle;
    hyperscanthreadID: dword;
    h: HHook;

implementation

procedure InitializeScansettings;
begin
  scansettings:=nil;
  outputdebugstring('Initializing scansettings');
  HyperscanFileMapping:=OpenFileMapping(FILE_MAP_ALL_ACCESS	,false,'CEHYPERSCANSETTINGS');
  if hyperscanfilemapping=0 then raise exception.Create('Error getting the settings');

  scansettings:=MapViewOfFile(HyperscanFileMapping,FILE_MAP_ALL_ACCESS,0,0,0);
  if scansettings=nil then raise exception.Create('Error getting the settings');
  h:=scansettings.formscanningHandle;

  if scansettings<>nil then outputdebugstring('InitializeScansettings successfull');
  outputdebugstring(pchar('scansettings='+inttohex(dword(scansettings),8)));

  if scansettings^.UseHyperscan then
    outputdebugstring('use hyperscan=true') else outputdebugstring('use hyperscan=false');

  outputdebugstring(pchar(inttostr(scansettings^.mainformHandle)));
end;

procedure UninitializeScansettings;
begin
  unmapviewoffile(globals.scansettings);
  globals.scansettings:=nil;
end;

initialization
  initializescansettings;

finalization
  UninitializeScansettings;

end.
