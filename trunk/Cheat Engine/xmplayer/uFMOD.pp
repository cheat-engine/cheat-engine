{ *** uFMOD (WINMM) unit for FreePascal *** }

unit uFMOD;

interface

{  function uFMOD_PlaySong(
      lpXM: Pointer;
      param, fdwSong: LongWord
   ): PHWAVEOUT;
   ---
   Description:
   ---
      Loads the given XM song and starts playing it immediately,
      unless XM_SUSPENDED is specified. It will stop any currently
      playing song before loading the new one.
   ---
   Parameters:
   ---
     lpXM
        Specifies the song to play. If this parameter is 0, any
        currently playing song is stopped. In such a case, function
        does not return a meaningful value. fdwSong parameter
        determines whether this value is interpreted as a filename,
        as a resource identifier or a pointer to an image of the song
        in memory.
     param
        If XM_RESOURCE is specified, this parameter should be the
        handle to the executable file that contains the resource to
        be loaded. A 0 value refers to the executable module itself.
        If XM_MEMORY is specified, this parameter should be the size
        of the image of the song in memory.
        If XM_FILE is specified, this parameter is ignored.
     fdwSong
        Flags for playing the song. The following values are defined:
        XM_FILE      lpXM points to filename. param is ignored.
        XM_MEMORY    lpXM points to an image of a song in memory.
                     param is the image size. Once, uFMOD_PlaySong
                     returns, it's safe to free/discard the memory
                     buffer.
        XM_RESOURCE  lpXM specifies the name of the resource.
                     param identifies the module whose executable file
                     contains the resource.
                     The resource type must be RT_RCDATA.
        XM_NOLOOP    An XM track plays repeatedly by default. Specify
                     this flag to play it only once.
        XM_SUSPENDED The XM track is loaded in a suspended state,
                     and will not play until the uFMOD_Resume function
                     is called. This is useful for preloading a song
                     or testing an XM track for validity.
  ---
  Return Values:
  ---
     On success, returns a pointer to an open WINMM output device handle.
     Returns nil on failure. If you are familiar with WINMM, you'll know
     what this handle might be useful for :)
  ---
  Remarks:
  ---
     If no valid song is specified and there is one currently being
     played, uFMOD_PlaySong just stops playback.
}
function uFMOD_PlaySong(lpXM:Pointer;param,fdwSong:LongWord):Pointer; stdcall; external name '_uFMOD_PlaySong_12';
procedure uFMOD_StopSong;

{  procedure uFMOD_Jump2Pattern(
      pat: LongWord
   );
   ---
   Description:
   ---
      Jumps to the specified pattern index.
   ---
   Parameters:
   ---
      pat
         Next zero based pattern index.
   ---
   Remarks:
   ---
      uFMOD doesn't automatically perform Note Off effects before jumping
      to the target pattern. In other words, the original pattern will
      remain in the mixer until it fades out. You can use this feature to
      your advantage. If you don't like it, just insert leading Note Off
      commands in all patterns intended to be used as uFMOD_Jump2Pattern
      targets.
      if the pattern index lays outside of the bounds of the pattern order
      table, calling this function jumps to pattern 0, effectively
      rewinding playback. }
procedure uFMOD_Jump2Pattern(pat:LongWord); stdcall; external name '_uFMOD_Jump2Pattern_4';
procedure uFMOD_Rewind;

{  procedure uFMOD_Pause;
   ---
   Description:
   ---
      Pauses the currently playing song, if any.
   ---
   Remarks:
   ---
      While paused you can still control the volume (uFMOD_SetVolume) and
      the pattern order (uFMOD_Jump2Pattern). The RMS volume coefficients
      (uFMOD_GetStats) will go down to 0 and the progress tracker
      (uFMOD_GetTime) will "freeze" while the song is paused.
      uFMOD_Pause doesn't perform the request immediately. Instead, it
      signals to pause when playback reaches next chunk of data, which may
      take up to about 40ms. This way, uFMOD_Pause performs asynchronously
      and returns very fast. It is not cumulative. So, calling
      uFMOD_Pause many times in a row has the same effect as calling it
      once.
      If you need synchronous pause/resuming, you can use WINMM
      waveOutPause/waveOutRestart functions. }
procedure uFMOD_Pause; external name '_uFMOD_Pause_0';

{  procedure uFMOD_Resume;
   ---
   Description:
   ---
      Resumes the currently paused song, if any.
   ---
   Remarks:
   ---
      uFMOD_Resume doesn't perform the request immediately. Instead, it
      signals to resume when an internal thread gets a time slice, which
      may take some milliseconds to happen. Usually, calling Sleep(0)
      immediately after uFMOD_Resume causes it to resume faster.
      uFMOD_Resume is not cumulative. So, calling it many times in a row
      has the same effect as calling it once.
      If you need synchronous pause/resuming, you can use WINMM
      waveOutPause/waveOutRestart functions. }
procedure uFMOD_Resume; external name '_uFMOD_Resume_0';

{  function uFMOD_GetStats:LongWord;
   ---
   Description:
   ---
      Returns the current RMS volume coefficients in (L)eft and (R)ight
      channels.
         low-order word: RMS volume in R channel
         hi-order word:  RMS volume in L channel
      Range from 0 (silence) to $7FFF (maximum) on each channel.
   ---
   Remarks:
   ---
      This function is useful for updating a VU meter. It's recommended
      to rescale the output to log10 (decibels or dB for short), because
      human ears track volume changes in a dB scale. You may call
      uFMOD_GetStats() as often as you like, but take in mind that uFMOD
      updates both channel RMS volumes every 20-40ms, depending on the
      output sampling rate. So, calling uFMOD_GetStats about 16 times a
      second whould be quite enough to track volume changes very closely. }
function uFMOD_GetStats:LongWord; stdcall; external name '_uFMOD_GetStats_0';

{  function uFMOD_GetRowOrder:LongWord;
   ---
   Description:
   ---
      Returns the currently playing row and order.
         low-order word: row
         hi-order word:  order
   ---
   Remarks:
   ---
      This function is useful for synchronization. uFMOD updates both
      row and order values every 20-40ms, depending on the output sampling
      rate. So, calling uFMOD_GetRowOrder about 16 times a second whould be
      quite enough to track row and order progress very closely. }
function uFMOD_GetRowOrder:LongWord; stdcall; external name '_uFMOD_GetRowOrder_0';

{  function uFMOD_GetTime:LongWord;
   ---
   Description:
   ---
      Returns the time in milliseconds since the song was started.
   ---
   Remarks:
   ---
      This function is useful for synchronizing purposes. In fact, it is
      more precise than a regular timer in Win32. Multimedia applications
      can use uFMOD_GetTime to synchronize GFX to sound, for example. An
      XM player can use this function to update a progress meter. }
function uFMOD_GetTime:LongWord; stdcall; external name '_uFMOD_GetTime_0';

{  function uFMOD_GetTitle:AnsiString;
   ---
   Description:
   ---
      Returns the current song's title.
   ---
   Remarks:
   ---
      Not every song has a title, so be prepared to get an empty string.
      The string format may be ANSI or Unicode debending on the UF_UFS
      settings used while recompiling the library. }
function uFMOD_GetTitle:AnsiString; stdcall; external name '_uFMOD_GetTitle_0';

{  procedure uFMOD_SetVolume(
      vol: LongWord
   );
   ---
   Description:
   ---
      Sets the global volume. The volume scale is linear.
   ---
   Parameters:
   ---
      vol
         New volume. Range: from uFMOD_MIN_VOL (muting) to uFMOD_MAX_VOL
         (maximum volume). Any value above uFMOD_MAX_VOL maps to maximum
         volume.
   ---
   Remarks:
   ---
      uFMOD internally converts the given values to a logarithmic scale (dB).
      Maximum volume is set by default. The volume value is preserved across
      uFMOD_PlaySong calls. You can set the desired volume level before
      actually starting to play a song.
      You can use WINMM waveOutSetVolume function to control the L and R
      channels volumes separately. It also has a wider range than
      uFMOD_SetVolume, sometimes allowing to amplify the sound volume as well,
      as opposed to uFMOD_SetVolume only being able to attenuate it. The bad
      things about waveOutSetVolume is that it may produce clicks and it's
      hardware dependent. }
procedure uFMOD_SetVolume(vol:LongWord); stdcall; external name '_uFMOD_SetVolume_4';

const
	XM_RESOURCE       = 0;
	XM_MEMORY         = 1;
	XM_FILE           = 2;
	XM_NOLOOP         = 8;
	XM_SUSPENDED      = 16;
	uFMOD_MIN_VOL     = 0;
	uFMOD_MAX_VOL     = 25;
	uFMOD_DEFAULT_VOL = 25;

implementation




function _WaitForSingleObject(hObject,dwTimeout:LongWord):LongWord; stdcall; external 'kernel32.dll' name 'WaitForSingleObject';
function _CloseHandle(hObject:LongWord):LongWord; stdcall; external 'kernel32.dll' name 'CloseHandle';
function _CreateThread(lpThreadAttributes:Pointer;dwStackSize:LongWord;lpStartAddress,lpParameter:Pointer;dwCreationFlags:LongWord;lpThreadId:Pointer):LongWord; stdcall; external 'kernel32.dll' name 'CreateThread';
function _SetThreadPriority(hThread,nPriority:LongWord):LongWord; stdcall; external 'kernel32.dll' name 'SetThreadPriority';
function _HeapAlloc(hHeap,dwFlags,dwBytes:LongWord):LongWord; stdcall; external 'kernel32.dll' name 'HeapAlloc';
function _HeapCreate(flOptions,dwInitialSize,dwMaximumSize:LongWord):LongWord; stdcall; external 'kernel32.dll' name 'HeapCreate';
function _HeapDestroy(hHeap:LongWord):LongWord; stdcall; external 'kernel32.dll' name 'HeapDestroy';
procedure _Sleep(cMillis:LongWord); stdcall; external 'kernel32.dll' name 'Sleep';
function _FindResourceA(hModule:LongWord;lpName,lpType:AnsiString):LongWord; stdcall; external 'kernel32.dll' name 'FindResourceA';
function _LoadResource(hModule,hrsrc:LongWord):LongWord; stdcall; external 'kernel32.dll' name 'LoadResource';
function _SizeofResource(hModule,hrsrc:LongWord):LongWord; stdcall; external 'kernel32.dll' name 'SizeofResource';
function _CreateFileA(lpFileName:AnsiString;dwDesiredAccess,dwShareMode:LongWord;lpSecurityAttributes:Pointer;dwCreationDistribution,dwFlagsAndAttributes,hTemplateFile:LongWord):LongWord; stdcall; external 'kernel32.dll' name 'CreateFileA';
function _CreateFileW(lpFileName:Pointer;dwDesiredAccess,dwShareMode:LongWord;lpSecurityAttributes:Pointer;dwCreationDistribution,dwFlagsAndAttributes,hTemplateFile:LongWord):LongWord; stdcall; external 'kernel32.dll' name 'CreateFileW';
function _ReadFile(hFile:LongWord;lpBuffer:Pointer;nNumberOfBytesToRead:LongWord;lpNumberOfBytesRead,lpOverlapped:Pointer):LongWord; stdcall; external 'kernel32.dll' name 'ReadFile';
function _SetFilePointer(hFile,lDistanceToMove:LongWord;lpDistanceToMoveHigh:Pointer;dwMoveMethod:LongWord):LongWord; stdcall; external 'kernel32.dll' name 'SetFilePointer';

function _waveOutUnprepareHeader(hwo,pwh:Pointer;cbwh:LongWord):LongWord; stdcall; external 'winmm.dll' name 'waveOutUnprepareHeader';
function _waveOutWrite(hwo,pwh:Pointer;cbwh:LongWord):LongWord; stdcall; external 'winmm.dll' name 'waveOutWrite';
function _waveOutClose(hwo:Pointer):LongWord; stdcall; external 'winmm.dll' name 'waveOutClose';
function _waveOutGetPosition(hwo,pmmt:Pointer;cbmmt:LongWord):LongWord; stdcall; external 'winmm.dll' name 'waveOutGetPosition';
function _waveOutOpen(phwo:Pointer;uDeviceID:LongWord;pwfx,dwCallback,dwCallbackInstance:Pointer;fdwOpen:LongWord):LongWord; stdcall; external 'winmm.dll' name 'waveOutOpen';
function _waveOutPrepareHeader(hwo,pwh:Pointer;cbwh:LongWord):LongWord; stdcall; external 'winmm.dll' name 'waveOutPrepareHeader';
function _waveOutReset(hwo:Pointer):LongWord; stdcall; external 'winmm.dll' name 'waveOutReset';

function WaitForSingleObject(hObject,dwTimeout:LongWord):LongWord; stdcall; public name 'WaitForSingleObject';
begin
  result:=_WaitForSingleObject(hObject, dwTimeout);
end;

function CloseHandle(hObject:LongWord):LongWord; stdcall; public name 'CloseHandle';
begin
  result:=_CloseHandle(hObject);
end;

function CreateThread(lpThreadAttributes:Pointer;dwStackSize:LongWord;lpStartAddress,lpParameter:Pointer;dwCreationFlags:LongWord;lpThreadId:Pointer):LongWord; stdcall; public name 'CreateThread';
begin
  result:=_CreateThread(lpThreadAttributes, dwStackSize, lpStartAddress, lpParameter, dwCreationFlags, lpThreadId);
end;

function SetThreadPriority(hThread,nPriority:LongWord):LongWord; stdcall; public name 'SetThreadPriority';
begin
  result:=_SetThreadPriority(hThread, nPriority);
end;

function HeapAlloc(hHeap,dwFlags,dwBytes:LongWord):LongWord; stdcall; public name 'HeapAlloc';
begin
  result:=_HeapAlloc(hHeap, dwFlags, dwBytes);
end;

function HeapCreate(flOptions,dwInitialSize,dwMaximumSize:LongWord):LongWord; stdcall; public name 'HeapCreate';
begin
  result:=_HeapCreate(flOptions, dwInitialSize, dwMaximumSize);
end;

function HeapDestroy(hHeap:LongWord):LongWord; stdcall; public name 'HeapDestroy';
begin
  result:=_HeapDestroy(hHeap);
end;

procedure Sleep(cMillis:LongWord); stdcall; public name 'Sleep';
begin
  _Sleep(cMillis);
end;

function FindResourceA(hModule:LongWord;lpName,lpType:AnsiString):LongWord; stdcall; public name 'FindResourceA';
begin
  result:=_FindResourceA(hModule, lpName, lpType);
end;

function LoadResource(hModule,hrsrc:LongWord):LongWord; stdcall; public name 'LoadResource';
begin
  result:=_LoadResource(hModule, hrsrc);
end;

function SizeofResource(hModule,hrsrc:LongWord):LongWord; stdcall; public name 'SizeofResource';
begin
  result:=_SizeofResource(hModule, hrsrc);
end;

function CreateFileA(lpFileName:AnsiString;dwDesiredAccess,dwShareMode:LongWord;lpSecurityAttributes:Pointer;dwCreationDistribution,dwFlagsAndAttributes,hTemplateFile:LongWord):LongWord; stdcall; public name 'CreateFileA';
begin
  result:=_CreateFileA(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDistribution, dwFlagsAndAttributes, hTemplateFile);
end;

function CreateFileW(lpFileName:Pointer;dwDesiredAccess,dwShareMode:LongWord;lpSecurityAttributes:Pointer;dwCreationDistribution,dwFlagsAndAttributes,hTemplateFile:LongWord):LongWord; stdcall; public name 'CreateFileW';
begin
  result:=_CreateFileW(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDistribution, dwFlagsAndAttributes, hTemplateFile);
end;

function ReadFile(hFile:LongWord;lpBuffer:Pointer;nNumberOfBytesToRead:LongWord;lpNumberOfBytesRead,lpOverlapped:Pointer):LongWord; stdcall; public name 'ReadFile';
begin
  result:=_ReadFile(hFile, lpBuffer, nNumberOfBytesToRead, lpNumberOfBytesRead, lpOverlapped);
end;

function SetFilePointer(hFile,lDistanceToMove:LongWord;lpDistanceToMoveHigh:Pointer;dwMoveMethod:LongWord):LongWord; stdcall; public name 'SetFilePointer';
begin
  result:=_SetFilePointer(hFile, lDistanceToMove, lpDistanceToMoveHigh, dwMoveMethod);
end;

function waveOutUnprepareHeader(hwo,pwh:Pointer;cbwh:LongWord):LongWord; stdcall; public name 'waveOutUnprepareHeader';
begin
  result:=_waveOutUnprepareHeader(hwo, pwh, cbwh);
end;

function waveOutWrite(hwo,pwh:Pointer;cbwh:LongWord):LongWord; stdcall; public name 'waveOutWrite';
begin
  result:=_waveOutWrite(hwo, pwh, cbwh);
end;

function waveOutClose(hwo:Pointer):LongWord; stdcall; public name 'waveOutClose';
begin
  result:=_waveOutClose(hwo);
end;

function waveOutGetPosition(hwo,pmmt:Pointer;cbmmt:LongWord):LongWord; stdcall; public name 'waveOutGetPosition';
begin
  result:=_waveOutGetPosition(hwo, pmmt, cbmmt);
end;

function waveOutOpen(phwo:Pointer;uDeviceID:LongWord;pwfx,dwCallback,dwCallbackInstance:Pointer;fdwOpen:LongWord):LongWord; stdcall; public name 'waveOutOpen';
begin
  result:=_waveOutOpen(phwo, uDeviceID, pwfx, dwCallback, dwCallbackInstance, fdwOpen);
end;

function waveOutPrepareHeader(hwo,pwh:Pointer;cbwh:LongWord):LongWord; stdcall; public name 'waveOutPrepareHeader';
begin
  result:=_waveOutPrepareHeader(hwo, pwh, cbwh);
end;

function waveOutReset(hwo:Pointer):LongWord; stdcall; public name 'waveOutReset';
begin
  result:=_waveOutReset(hwo);
end;

{$L libufmod}

procedure uFMOD_StopSong;
begin
	uFMOD_PlaySong(nil,0,0)
end;

procedure uFMOD_Rewind;
begin
	uFMOD_Jump2Pattern(0)
end;

end.
