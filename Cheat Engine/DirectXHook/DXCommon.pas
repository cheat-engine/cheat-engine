unit DXCommon;

interface

//{$DEFINE ERRORFUNCS}


uses
  Windows;

const
  UnrecognizedError = 'Unrecognized Error';

type
{$IFDEF UNICODE}
  PCharAW = PWideChar;
{$ELSE}
  PCharAW = PAnsiChar;
{$ENDIF}

function IsNTandDelphiRunning : boolean;
function RegGetStringValue(Hive: HKEY; const KeyName, ValueName: string): string;
function ExistFile(const FileName: string): Boolean;

function Trunc(const x : Single) : Integer;
function Frac(const x : Single) : Single;
function Round(const x : Single) : Integer;

{$IFDEF ERRORFUNCS}
function DXErrorString(Error: HResult): string;
function DXErrorMessage(Error: HResult): boolean;
{$ENDIF}

implementation

{$IFDEF ERRORFUNCS}
uses DirectInput8, DirectInput, DirectSound, DirectMusic, DirectPlay, DirectPlay8,
     Direct3D, Direct3DRM, DirectDraw, DirectShow, DirectXGraphics,
     Dialogs, SysUtils;

function DXErrorMessage(Error: HResult): boolean;
begin
  Result := FAILED(Error);
  if Result then
    MessageDlg(DXErrorString(Error), mtError, [mbAbort], 0);
end;

function DXErrorString(Error: HResult): string;
var Facility: DWORD;
begin
  Facility := (Error shr 16) and $7FFF;

  case Facility of
    // Direct3D, DirectDraw
    _FACD3D : if (Error and $FFFF) > D3DERR_WRONGTEXTUREFORMAT
      then Result := DirectXGraphics.DXGErrorString(Error)
      else Result := DirectDraw.DDErrorString(Error);

    // DirectMusic, DirectSound
    _FACDS : if (Error and $FFFF) > DMUS_ERRBASE
      then Result := DirectMusic.DMErrorString(Error)
      else Result := DirectSound.DSErrorString(Error);

    // DirectPlay
    _FACDPV : Result := DirectPlay8.DPErrorString(Error);
    _FACDPV7: Result := DirectPlay.DPErrorString(Error);

//Definitions still to come
    else case Error of
      S_OK : Result := '';
      S_FALSE : Result := '';
      E_INVALIDARG : Result := '';
      E_NOINTERFACE : Result := '';
      E_FAIL : Result := '';
      E_OUTOFMEMORY : Result := '';
      E_NOTIMPL : Result := '';
      E_ACCESSDENIED : Result := '';

      else Result := DirectInput.DIErrorString(Error);
    end;
  end;
end;
{$ENDIF}

function RegGetStringValue(Hive: HKEY; const KeyName, ValueName: string): string;
var EnvKey  : HKEY;
    Buf     : array[0..255] of char;
    BufSize : DWord;
    RegType : DWord;
    rc      : DWord;
begin
  Result := '';
  BufSize := Sizeof(Buf);
  ZeroMemory(@Buf, BufSize);
  RegType := REG_SZ;
  try
    if (RegOpenKeyEx(Hive, PChar(KeyName), 0, KEY_READ, EnvKey) = ERROR_SUCCESS) then
    begin
      try
        if (ValueName = '') then rc := RegQueryValueEx(EnvKey, nil, nil, @RegType, @Buf, @BufSize)
          else rc := RegQueryValueEx(EnvKey, PChar(ValueName), nil, @RegType, @Buf, @BufSize);
        if rc = ERROR_SUCCESS then Result := string(Buf);
      finally
        RegCloseKey(EnvKey);
      end;
    end;
  finally
    RegCloseKey(Hive);
  end;
end;

function ExistFile(const FileName: string): Boolean;
var hFile: THandle;
begin
  hFile := CreateFile(PChar(FileName), 0, 0, nil, OPEN_EXISTING, 0, 0);
  Result := hFile <> INVALID_HANDLE_VALUE;
  if Result = true then CloseHandle(hFile);
end;

function IsNTandDelphiRunning : boolean;
var
  OSVersion  : TOSVersionInfo;
  AppName    : array[0..255] of char;
begin
  OSVersion.dwOsVersionInfoSize := sizeof(OSVersion);
  GetVersionEx(OSVersion);
  // Not running in NT or program is not Delphi itself ?
  AppName[0] := #0;
  lstrcat(AppName, PChar(ParamStr(0)));  // ParamStr(0) = Application.ExeName
  CharUpperBuff(AppName, SizeOf(AppName));
  result := ( (OSVersion.dwPlatformID = VER_PLATFORM_WIN32_NT) and
              (Pos('DELPHI32.EXE', AppName) = Length(AppName) - Length('DELPHI32.EXE') + 1) );
end;

function Trunc(const x : Single) : Integer; register;
const cwChop : Word = $1F3F;
asm
      SUB     ESP,8
      FSTCW   [ESP]
      FLDCW   cwChop
      FLD     x
      FISTP   dword ptr [ESP+4]
      FLDCW   [ESP]
      POP     ECX
      POP     EAX
end;

function Frac(const x : Single) : Single; register;
begin
   Result := x - Trunc(x);
end;

function Round(const x : Single) : Integer; register;
asm
      SUB     ESP,4
      FLD     x
      FISTP   dword ptr [ESP]
      POP     EAX
end;


end.
