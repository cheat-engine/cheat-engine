//Copyright Cheat Engine

unit dotnethost;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type EDotNetException=class(Exception);

function DotNetExecuteClassMethod(assemblypath: string; namespace: string; classname: string; methodname: string; parameters: string): integer; //MUST be a "public static int methodname(string str)"  but it's enough to get the initialization done anyhow

implementation

{$ifdef windows}
uses windows, activex;

type
  REFCLSID=REFIID;
  PLPVOID=^LPVOID;
  PHMODULE=^HMODULE;



  ICLRRuntimeHost = interface (IUnknown)
    ['{90F1A06C-7712-4762-86B5-7A5EBA6BDB02}']
    function Start: HRESULT; stdcall;
    function Stop: HRESULT; stdcall;
    function SetHostControl(pHostControl: {IHostControl}IUnknown): HRESULT; stdcall;
    function GetCLRControl(out pCLRControl: {ICLRControl}IUnknown): HRESULT; stdcall;
    function UnloadAppDomain(dwAppDomainId: DWORD;  fWaitUntilDone: BOOL): HRESULT; stdcall;
    function ExecuteInAppDomain(dwAppDomainId: DWORD; pCallback: pointer; cookie: pointer): HRESULT; stdcall;
    function GetCurrentAppDomainId(pdwAppDomainId: PDWORD): HRESULT; stdcall;
    function ExecuteApplication(pwzAppFullName: LPCWSTR; dwManifestPaths: DWORD; ppwzManifestPaths: LPCWSTR; dwActivationData: DWORD; ppwzActivationData: LPCWSTR; pReturnValue: PINT): HRESULT; stdcall;
    function ExecuteInDefaultAppDomain(pwzAssemblyPath: LPCWSTR; pwzTypeName: LPCWSTR; pwzMethodName: LPCWSTR; pwzArgument: LPCWSTR; pReturnValue: PDWORD): HRESULT; stdcall;
  end;


  ICLRRuntimeInfo = interface (IUnknown)
    ['{BD39D1D2-BA2F-486a-89B0-B4B0CB466891}']
    function GetVersionString(pwzBuffer: LPWSTR; pcchbuffer: PDWORD): HRESULT; stdcall;
    function GetRuntimeDirectory(pwzBuffer: LPWSTR; pcchBuffer: PDWORD): HRESULT; stdcall;
    function IsLoaded(hndProcess: HANDLE; pbLoaded: PBOOL): HRESULT; stdcall;
    function LoadErrorString(iResourceID: UINT; pwzBuffer: LPWSTR; pcchBuffer: PDWORD; iLocaleID: LONG): HRESULT; stdcall;
    function LoadLibrary(pwzDllName: LPCWSTR; phndModule: PHMODULE): HRESULT; stdcall;
    function GetProcAddress(pszProcName: LPCSTR; ppProc: pointer): HRESULT; stdcall;
    function GetInterface(rclsid: REFCLSID; riid: REFIID; var ppUnk: IUnknown): HRESULT; stdcall;
    function IsLoadable(pbLoadable: PBOOL): HRESULT; stdcall;
    function SetDefaultStartupFlags(dwStartupFlags: DWORD; pwzHostConfigFile: LPCWSTR): HRESULT; stdcall;
    function BindAsLegacyV2Runtime: HRESULT; stdcall;
    function IsStarted(pbStarted: pbool; pdwStartupFlags: PDWORD): HRESULT; stdcall;
  end;

  ICLRMetaHost= Interface (IUnknown)
    ['{D332DB9E-B9B3-4125-8207-A14884F53216}']
    function GetRunTime(pwzVersion: LPCWSTR; riid: REFIID; out ppRuntime: ICLRRuntimeInfo): HRESULT; stdcall;
    function GetVersionFromFile(pwzFilePath: LPCWSTR; pwzBuffer: LPWSTR; pcchBuffer: pdword): HRESULT; stdcall;
    function EnumerateInstalledRuntimes(out ppEnumerator: {IEnumUnknown}IUnknown): HRESULT; stdcall;
    function EnumerateLoadedRuntimes(hndProcess: Handle; out ppEnumerator: {IEnumUnknown}IUnknown): HRESULT; stdcall;
    function RequestRuntimeLoadedNotification(pCallbackFunction: {RuntimeLoadedCallbackFnPtr}pointer): HRESULT; stdcall;
    function QueryLegacyV2RuntimeBinding(riid: REFIID; ppUnk: pointer): HRESULT; stdcall;
    function ExitProcess(iExitCode: INT32): HRESULT; stdcall;
  end;


const
  IID_ICLRRuntimeInfo : TGUID = '{BD39D1D2-BA2F-486a-89B0-B4B0CB466891}';
  IID_ICLRMetaHost    : TGUID = '{D332DB9E-B9B3-4125-8207-A14884F53216}';
  CLSID_CLRMetaHost   : TGUID = '{9280188d-0e8e-4867-b30c-7fa83884e8de}';

  CLSID_CLRRuntimeHost: TGUID = '{90F1A06E-7712-4762-86B5-7A5EBA6BDB02}';
  IID_ICLRRuntimeHost : TGUID = '{90F1A06C-7712-4762-86B5-7A5EBA6BDB02}';



var
  CLRCreateInstance: function(clsid: REFCLSID; riid: REFIID; out int: IUnknown): HRESULT; stdcall;
  hMSCorEE: HModule;
  host: ICLRRuntimeHost=nil;

{$endif}

function DotNetExecuteClassMethod(assemblypath: string; namespace: string; classname: string; methodname: string; parameters: string): integer;
//raises EDotNetException on error
{$ifdef windows}
var
  fullclassname: widestring;
  mh: ICLRMetaHost=nil;
  rti: ICLRRuntimeInfo=nil;
  hostu: IUnknown=nil;

  versionstring: PWideChar;
  size: dword;

  path: widestring;
  loadable: BOOL;

  ret: dword;

  r: HRESULT;
{$endif}
begin
{$ifdef windows}
  path:=assemblypath;

  if host=nil then
  begin
    hMSCorEE:=loadlibrary('MSCorEE.dll');
    if hMSCorEE=0 then
      raise EDotNetException.create('Failed to load MSCorEE.dll');

    pointer(CLRCreateInstance):=getProcAddress(hMSCorEE, 'CLRCreateInstance');
    if not assigned(CLRCreateInstance) then
      raise EDotNetException.create('CLRCreateInstance not found in MSCorEE.dll');


    if CLRCreateInstance(CLSID_CLRMetaHost, IID_ICLRMetaHost, mh)<>S_OK then
      raise EDotNetException.create('Failed to create CLRMetaHost instance');

    getmem(versionstring,100*2);
    size:=90;

    if mh.GetVersionFromFile(pwidechar(path),versionstring,@size)<>S_OK then
      raise EDotNetException.create('Failed to get the DotNet version from '+path);

    if mh.GetRunTime(versionstring, IID_ICLRRuntimeInfo, rti)<>S_OK then
      raise EDotNetException.create('Failed to find dot net '+versionstring+' on your system');

    if rti.IsLoadable(@loadable)<>S_OK then
      raise EDotNetException.create('Failure determining if the DotNet runtime is loadable');

    if not loadable then
      raise EDotNetException.create('DotNet '+versionstring+' does not seem to be loadable');

    if rti.GetInterface(CLSID_CLRRuntimeHost, IID_ICLRRuntimeHost, hostu)<>S_OK then
      raise EDotNetException.create('Failure getting the RuntimeHost interface for dotner version '+versionstring);

    host:=ICLRRuntimeHost(hostu);

    if host.Start<>S_OK then
      raise EDotNetException.create('Failure starting the DotNet host');
  end;

  if namespace<>'' then
    fullclassname:=namespace+'.'+classname
  else
    fullclassname:=classname;

  ret:=0;
  r:=host.ExecuteInDefaultAppDomain(pwidechar(path),
                                 pwidechar(fullclassname),
                                 pwidechar(widestring(methodname)),
                                 pwidechar(widestring(parameters)),
                                 @ret);

  if r<>S_OK then
    raise EDotNetException.create('Failure executing '+fullclassname+':'+methodname+' (Result='+inttohex(r,8)+')');

  result:=ret;
{$else}
//mono
  raise EDotNetException.create('Not yet implemented');
{$endif}



end;



end.

