unit winsapi;

{
partial sapi.h conversion for the syntesized voice functions

https://www.w3.org/TR/speech-synthesis/

works:
<?xml version="1.0"?>
<speak version="1.0" xmlns="http://www.w3.org/2001/10/synthesis"
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       xsi:schemaLocation="http://www.w3.org/2001/10/synthesis
                 http://www.w3.org/TR/speech-synthesis/synthesis.xsd"
       xml:lang="en-US">

hello
</speak>

this wil freeze it(for a too long time)
<?xml version="1.0"?>
<!DOCTYPE speak PUBLIC "-//W3C//DTD SYNTHESIS 1.0//EN"
                  "http://www.w3.org/TR/speech-synthesis/synthesis.dtd">
<speak version="1.0" xmlns="http://www.w3.org/2001/10/synthesis"
         xml:lang="en-US">

hello
</speak>

}

{$mode objfpc}{$H+}

interface

{$ifdef windows}

uses
  windows, Classes, SysUtils, ole2, variants, ActiveX, comobj;

function speak(s: widestring; waittilldone: boolean=false): HRESULT; overload;
function speak(s: widestring; flags: dword): HRESULT; overload;

const
  CLSID_SpVoice: TGUID = (D1:$96749377;D2:$3391;D3:$11D2;D4:($9e,$e3,$00,$c0,$4f,$79,$73,$96));

  SPF_DEFAULT	= 0;
          SPF_ASYNC	= ( 1 shl 0 );
          SPF_PURGEBEFORESPEAK	= ( 1 shl 1 );
          SPF_IS_FILENAME	= ( 1 shl 2 ) ;
          SPF_IS_XML	= ( 1 shl 3 ) ;
          SPF_IS_NOT_XML	= ( 1 shl 4 );
          SPF_PERSIST_XML	= ( 1 shl 5 );
          SPF_NLP_SPEAK_PUNC	= ( 1 shl 6 );
          SPF_PARSE_SAPI	= ( 1 shl 7 );
          SPF_PARSE_SSML	= ( 1 shl 8 );
          SPF_PARSE_AUTODETECT	= 0;
          SPF_NLP_MASK	= SPF_NLP_SPEAK_PUNC;
          SPF_PARSE_MASK	= ( SPF_PARSE_SAPI or SPF_PARSE_SSML );
          SPF_VOICE_MASK	= ( ( ( ( ( ( ( SPF_ASYNC or SPF_PURGEBEFORESPEAK )  or SPF_IS_FILENAME )  or SPF_IS_XML )  or SPF_IS_NOT_XML )  or SPF_NLP_MASK )  or SPF_PERSIST_XML )  or SPF_PARSE_MASK );
          SPF_UNUSED_FLAGS	= not SPF_VOICE_MASK;




type
  SPVPRIORITY=(SPVPRI_NORMAL=0, SPVPRI_ALERT=1, SPVPRI_OVER=2);
  SPEVENTENUM=(SPEI_UNDEFINED	= 0,
          SPEI_START_INPUT_STREAM	= 1,
          SPEI_END_INPUT_STREAM	= 2,
          SPEI_VOICE_CHANGE	= 3,
          SPEI_TTS_BOOKMARK	= 4,
          SPEI_WORD_BOUNDARY	= 5,
          SPEI_PHONEME	= 6,
          SPEI_SENTENCE_BOUNDARY	= 7,
          SPEI_VISEME	= 8,
          SPEI_TTS_AUDIO_LEVEL	= 9,
          SPEI_TTS_PRIVATE	= 15,
          SPEI_MIN_TTS	= 1,
          SPEI_MAX_TTS	= 15,
          SPEI_END_SR_STREAM	= 34,
          SPEI_SOUND_START	= 35,
          SPEI_SOUND_END	= 36,
          SPEI_PHRASE_START	= 37,
          SPEI_RECOGNITION	= 38,
          SPEI_HYPOTHESIS	= 39,
          SPEI_SR_BOOKMARK	= 40,
          SPEI_PROPERTY_NUM_CHANGE	= 41,
          SPEI_PROPERTY_STRING_CHANGE	= 42,
          SPEI_FALSE_RECOGNITION	= 43,
          SPEI_INTERFERENCE	= 44,
          SPEI_REQUEST_UI	= 45,
          SPEI_RECO_STATE_CHANGE	= 46,
          SPEI_ADAPTATION	= 47,
          SPEI_START_SR_STREAM	= 48,
          SPEI_RECO_OTHER_CONTEXT	= 49,
          SPEI_SR_AUDIO_LEVEL	= 50,
          SPEI_SR_RETAINEDAUDIO	= 51,
          SPEI_SR_PRIVATE	= 52,
          SPEI_ACTIVE_CATEGORY_CHANGED	= 53,
          SPEI_RESERVED5	= 54,
          SPEI_RESERVED6	= 55,
          SPEI_MIN_SR	= 34,
          SPEI_MAX_SR	= 55,
          SPEI_RESERVED1	= 30,
          SPEI_RESERVED2	= 33,
          SPEI_RESERVED3	= 63);



  SPEVENT=record
    eEventId: WORD;
    elParamType: WORD;
    ulStreamNum: ULONG;
    ullAudioStreamOffset: ULONGLONG;
    wParam: wParam;
    lParam: lParam;
  end;
  PSPEVENT=^SPEVENT;

  SPEVENTSOURCEINFO=record
    ullEventInterest: ULONGLONG;
    ullQueuedInterest: ULONGLONG;
    ulCount: ULONG;
  end;
  PSPEVENTSOURCEINFO=^SPEVENTSOURCEINFO;

  SPVOICESTATUS = record
    ulCurrentStream: ULONG;
    ulLastStreamQueued: ULONG;
    hrLastResult: HRESULT;
    dwRunningState: DWORD;
    ulInputWordPos: ULONG;
    ulInputWordLen: ULONG;
    ulInputSentPos: ULONG;
    ulInputSentLen: ULONG;
    lBookmarkId: LONG;
    PhonemeId: WORD;
    VisemeId: WORD;
    dwReserved1: DWORD;
    dwReserved2: DWORD;
  end;


  SPNOTIFYCALLBACK=procedure(wParam: WPARAM; lParam: LPARAM); stdcall;

  ISpDataKey = interface (IUnknown)
    ['{14056581-E16C-11D2-BB90-00C04F8EE6C0}']
    {
    virtual HRESULT STDMETHODCALLTYPE SetData(
        /* [in] */ LPCWSTR pszValueName,
        /* [in] */ ULONG cbData,
        /* [in] */ const BYTE *pData) = 0;

    virtual HRESULT STDMETHODCALLTYPE GetData(
        /* [in] */ LPCWSTR pszValueName,
        /* [in] */ ULONG *pcbData,
        /* [out] */ BYTE *pData) = 0;

    virtual HRESULT STDMETHODCALLTYPE SetStringValue(
        /* [in][annotation] */
        _In_opt_  LPCWSTR pszValueName,
        /* [in] */ LPCWSTR pszValue) = 0;

    virtual HRESULT STDMETHODCALLTYPE GetStringValue(
        /* [in][annotation] */
        _In_opt_  LPCWSTR pszValueName,
        /* [out][annotation] */
        _Outptr_  LPWSTR *ppszValue) = 0;

    virtual HRESULT STDMETHODCALLTYPE SetDWORD(
        /* [in] */ LPCWSTR pszValueName,
        /* [in] */ DWORD dwValue) = 0;

    virtual HRESULT STDMETHODCALLTYPE GetDWORD(
        /* [in] */ LPCWSTR pszValueName,
        /* [out] */ DWORD *pdwValue) = 0;

    virtual HRESULT STDMETHODCALLTYPE OpenKey(
        /* [in] */ LPCWSTR pszSubKeyName,
        /* [out][annotation] */
        _Outptr_  ISpDataKey **ppSubKey) = 0;

    virtual HRESULT STDMETHODCALLTYPE CreateKey(
        /* [in] */ LPCWSTR pszSubKey,
        /* [out][annotation] */
        _Outptr_  ISpDataKey **ppSubKey) = 0;

    virtual HRESULT STDMETHODCALLTYPE DeleteKey(
        /* [in] */ LPCWSTR pszSubKey) = 0;

    virtual HRESULT STDMETHODCALLTYPE DeleteValue(
        /* [in] */ LPCWSTR pszValueName) = 0;

    virtual HRESULT STDMETHODCALLTYPE EnumKeys(
        /* [in] */ ULONG Index,
        /* [out][annotation] */
        _Outptr_  LPWSTR *ppszSubKeyName) = 0;

    virtual HRESULT STDMETHODCALLTYPE EnumValues(
        /* [in] */ ULONG Index,
        /* [out][annotation] */
        _Outptr_  LPWSTR *ppszValueName) = 0;
    }
  end;

  ISpObjectToken = interface (ISpDataKey)
    ['{14056589-E16C-11D2-BB90-00C04F8EE6C0}']
    {
    virtual HRESULT STDMETHODCALLTYPE SetId(
               /* [annotation] */
               _In_opt_  LPCWSTR pszCategoryId,
               /* [in] */ LPCWSTR pszTokenId,
               /* [in] */ BOOL fCreateIfNotExist) = 0;

           virtual HRESULT STDMETHODCALLTYPE GetId(
               /* [out][annotation] */
               _Outptr_  LPWSTR *ppszCoMemTokenId) = 0;

           virtual HRESULT STDMETHODCALLTYPE GetCategory(
               /* [out][annotation] */
               _Outptr_  ISpObjectTokenCategory **ppTokenCategory) = 0;

           virtual HRESULT STDMETHODCALLTYPE CreateInstance(
               /* [in] */ IUnknown *pUnkOuter,
               /* [in] */ DWORD dwClsContext,
               /* [in] */ REFIID riid,
               /* [iid_is][out] */ void **ppvObject) = 0;

           virtual HRESULT STDMETHODCALLTYPE GetStorageFileName(
               /* [in] */ REFCLSID clsidCaller,
               /* [annotation][in] */
               _In_  LPCWSTR pszValueName,
               /* [string][in][annotation] */
               _In_opt_  LPCWSTR pszFileNameSpecifier,
               /* [in] */ ULONG nFolder,
               /* [out][annotation] */
               _Outptr_  LPWSTR *ppszFilePath) = 0;

           virtual HRESULT STDMETHODCALLTYPE RemoveStorageFileName(
               /* [in] */ REFCLSID clsidCaller,
               /* [in][annotation] */
               _In_  LPCWSTR pszKeyName,
               /* [in] */ BOOL fDeleteFile) = 0;

           virtual HRESULT STDMETHODCALLTYPE Remove(
               /* [annotation] */
               _In_opt_  const CLSID *pclsidCaller) = 0;

           virtual /* [local] */ HRESULT STDMETHODCALLTYPE IsUISupported(
               /* [in] */ LPCWSTR pszTypeOfUI,
               /* [in] */ void *pvExtraData,
               /* [in] */ ULONG cbExtraData,
               /* [in] */ IUnknown *punkObject,
               /* [out] */ BOOL *pfSupported) = 0;

           virtual /* [local] */ HRESULT STDMETHODCALLTYPE DisplayUI(
               /* [in] */ HWND hwndParent,
               /* [in] */ LPCWSTR pszTitle,
               /* [in] */ LPCWSTR pszTypeOfUI,
               /* [in] */ void *pvExtraData,
               /* [in] */ ULONG cbExtraData,
               /* [in] */ IUnknown *punkObject) = 0;

           virtual HRESULT STDMETHODCALLTYPE MatchesAttributes(
               /* [in] */ LPCWSTR pszAttributes,
               /* [out] */ BOOL *pfMatches) = 0;

    }
  end;

  PISpObjectToken = ^ISpObjectToken;

  ISpStreamFormat = interface(IStream)
    ['{BED530BE-2606-4F4D-A1C0-54C5CDA5566F}']
    function GetFormat(pguidFormatId: GUID; out ppCoMemWaveFormatEx: TWAVEFORMATEX): HRESULT; stdcall;
  end;

  PISpStreamFormat = ^ISpStreamFormat;

  ISpNotifySink = interface (IUnknown)
    ['{259684DC-37C3-11D2-9603-00C04F8EE628}']
    function Notify: HRESULT; stdcall;
  end;

  ISpNotifySource = interface (IUnknown)
    ['{5EFF4AEF-8487-11D2-961C-00C04F8EE628}']
    function SetNotifySink(NotifySink: ISpNotifySink): HRESULT; stdcall;
    function SetNotifyWindowMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): HRESULT; stdcall;
    function SetNotifyCallbackFunction(pfnCallback: SPNOTIFYCALLBACK; wParam: WPARAM; lParam: LPARAM): HRESULT; stdcall;
    function SetNotifyCallbackInterface(pSpCallback: pointer; wParam: WPARAM; lParam: LPARAM): HRESULT; stdcall;
    function SetNotifyWin32Event: HRESULT; stdcall;
    function WaitForNotifyEvent(dwMilliseconds: DWORD): HRESULT; stdcall;
    function GetNotifyEventHandle: HANDLE; stdcall;
  end;

  ISpEventSource = interface (ISpNotifySource)
    ['{BE7A9CCE-5F9E-11D2-960F-00C04F8EE628}']
    function SetInterest(ullEventInterest: ULONGLONG; ullQueuedInterest: ULONGLONG): HRESULT; stdcall;
    function GetEvents(ulCount: ULONG; pEventArray: PSPEVENT; out pulFetched: ULONG): HRESULT; stdcall;
    function GetInfo(pInfo: PSPEVENTSOURCEINFO): HRESULT; stdcall;
  end;

  ISpVoice = Interface (ISpEventSource)
    ['{6C44DF74-72B9-4992-A1EC-EF996E0422D4}']
    function SetOutput(pUnkOutput: IUnknown; fAllowFormatChanges: BOOL): HRESULT; stdcall;
    function GetOutputObjectToken(out ppObjectToken: ISpObjectToken): HRESULT; stdcall;
    function GetOutputStream(out ppStream: ISpStreamFormat): HRESULT; stdcall;
    function Pause: HRESULT; stdcall;
    function Resume: HRESULT; stdcall;
    function SetVoice(pToken: ISpObjectToken): HRESULT; stdcall;
    function GetVoice(out pToken: ISpObjectToken): HRESULT; stdcall;
    function Speak(pwcs: PWCHAR; dwFlags: DWORD; pulStreamNumber: PULONG): HRESULT; stdcall;
    function SpeakStream(pStream: IStream; dwFlags: DWORD; pulStreamNumber: PULONG): HRESULT; stdcall;

    function GetStatus(out pStatus: SPVOICESTATUS; out ppszLastBookmark: pwchar): HRESULT; stdcall;
    function Skip(pItemType: LPCWSTR; lNumItems: long; out ulNumSkipped: ULONG): HRESULT; stdcall;

    function SetPriority(ePriority: SPVPRIORITY): HRESULT; stdcall;
    function GetPriority(out ePriority: SPVPRIORITY): HRESULT; stdcall;

    function SetAlertBoundary(ePriority: SPEVENTENUM): HRESULT; stdcall;
    function GetAlertBoundary(out ePriority: SPEVENTENUM): HRESULT; stdcall;

    function SetRate(RateAdjust: long): HRESULT; stdcall;
    function GetRate(out RateAdjust: long): HRESULT; stdcall;

    function SetVolume(RateAdjust: USHORT): HRESULT; stdcall;
    function GetVolume(out RateAdjust: USHORT): HRESULT; stdcall;

    function WaitUntilDone(msTimeout: ULONG): HRESULT; stdcall;
    function SetSyncSpeakTimeout(msTimeout: ULONG): HRESULT; stdcall;
    function GetSyncSpeakTimeout(out msTimeout: ULONG): HRESULT; stdcall;

    function SpeakCompleteEvent: HRESULT; stdcall;
    function IsUISupported(pszTypeOfUI: LPCWSTR; pvExtraData: pointer; cbExtraData: ULONG; out fSupported: BOOL): HRESULT; stdcall;
    function DisplayUI(hwndParent: HWND; pszTitle: LPCWSTR; pszTypeOfUI: LPCWSTR; pvExtraData: pointer; cbExtraData: ULONG): HRESULT; stdcall;
  end;

{$endif}

implementation
{$ifdef windows}
var
  voice: ISpVoice;
  novoice: boolean=false;

function speak(s: widestring; flags: dword): HRESULT;
var
  sn: ulong;
begin
  {
  for c users reading this code, and wondering why I don't call _release.
  FPC will call _Release automatically when the reference count is nil
  }
  if not assigned(voice) then
  begin
    try
      voice:=CreateComObject(CLSID_SpVoice) as ISpVoice;
    except
      exit(-1);
    end;
  end;

  if assigned(voice) then
    result:=voice.Speak(pwchar(s), flags, nil);
end;


function speak(s: widestring; waitTillDone: boolean=false): HRESULT; overload;
var flags: dword;
begin
  flags:=SPF_PURGEBEFORESPEAK;
  if not waittilldone then
    flags:=flags or SPF_ASYNC;

  result:=speak(s, flags);
end;

{$endif}

end.

