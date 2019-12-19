unit xinput;

{$mode Delphi}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

interface

{$ifdef windows}
uses windows,classes, Controls;

  {**************************************************************************
  *                                                                          *
  *   XInput.h -- This module defines Xbox 360 Common Controller APIs        *
  *               and constants for the Windows platform.                    *
  *                                                                          *
  *   Copyright (c) Microsoft Corp. All rights reserved.                     *
  *                                                                          *
  ************************************************************************** }
  { Current name of the DLL shipped in the same SDK as this header. }
  { The name reflects the current version }
  { }
  { Device types available in XINPUT_CAPABILITIES }
  { }

  const
    XINPUT_DEVTYPE_GAMEPAD = $01;
  { }
  { Device subtypes available in XINPUT_CAPABILITIES }
  { }
    XINPUT_DEVSUBTYPE_GAMEPAD = $01;
    XINPUT_DEVSUBTYPE_UNKNOWN = $00;
    XINPUT_DEVSUBTYPE_WHEEL = $02;
    XINPUT_DEVSUBTYPE_ARCADE_STICK = $03;
    XINPUT_DEVSUBTYPE_FLIGHT_STICK = $04;
    XINPUT_DEVSUBTYPE_DANCE_PAD = $05;
    XINPUT_DEVSUBTYPE_GUITAR = $06;
    XINPUT_DEVSUBTYPE_GUITAR_ALTERNATE = $07;
    XINPUT_DEVSUBTYPE_DRUM_KIT = $08;
    XINPUT_DEVSUBTYPE_GUITAR_BASS = $0B;
    XINPUT_DEVSUBTYPE_ARCADE_PAD = $13;
  { }
  { Flags for XINPUT_CAPABILITIES }
  { }
    XINPUT_CAPS_VOICE_SUPPORTED = $0004;
    XINPUT_CAPS_FFB_SUPPORTED = $0001;
    XINPUT_CAPS_WIRELESS = $0002;
    XINPUT_CAPS_PMD_SUPPORTED = $0008;
    XINPUT_CAPS_NO_NAVIGATION = $0010;
  { }
  { Constants for gamepad buttons }
  { }
    XINPUT_GAMEPAD_DPAD_UP = $0001;
    XINPUT_GAMEPAD_DPAD_DOWN = $0002;
    XINPUT_GAMEPAD_DPAD_LEFT = $0004;
    XINPUT_GAMEPAD_DPAD_RIGHT = $0008;
    XINPUT_GAMEPAD_START = $0010;
    XINPUT_GAMEPAD_BACK = $0020;
    XINPUT_GAMEPAD_LEFT_THUMB = $0040;
    XINPUT_GAMEPAD_RIGHT_THUMB = $0080;
    XINPUT_GAMEPAD_LEFT_SHOULDER = $0100;
    XINPUT_GAMEPAD_RIGHT_SHOULDER = $0200;
    XINPUT_GAMEPAD_A = $1000;
    XINPUT_GAMEPAD_B = $2000;
    XINPUT_GAMEPAD_X = $4000;
    XINPUT_GAMEPAD_Y = $8000;
  { }
  { Gamepad thresholds }
  { }
    XINPUT_GAMEPAD_LEFT_THUMB_DEADZONE = 7849;
    XINPUT_GAMEPAD_RIGHT_THUMB_DEADZONE = 8689;
    XINPUT_GAMEPAD_TRIGGER_THRESHOLD = 30;
  { }
  { Flags to pass to XInputGetCapabilities }
  { }
    XINPUT_FLAG_GAMEPAD = $00000001;
  { }
  { Devices that support batteries }
  { }
    BATTERY_DEVTYPE_GAMEPAD = $00;
    BATTERY_DEVTYPE_HEADSET = $01;
  { }
  { Flags for battery status level }
  { }
    BATTERY_TYPE_DISCONNECTED = $00;    { This device is not connected }
    BATTERY_TYPE_WIRED = $01;    { Wired device, no battery }
    BATTERY_TYPE_ALKALINE = $02;    { Alkaline battery source }
    BATTERY_TYPE_NIMH = $03;    { Nickel Metal Hydride battery source }
    BATTERY_TYPE_UNKNOWN = $FF;    { Cannot determine the battery type }
  { These are only valid for wireless, connected devices, with known battery types }
  { The amount of use time remaining depends on the type of device. }
    BATTERY_LEVEL_EMPTY = $00;
    BATTERY_LEVEL_LOW = $01;
    BATTERY_LEVEL_MEDIUM = $02;
    BATTERY_LEVEL_FULL = $03;
  { User index definitions }
    XUSER_MAX_COUNT = 4;
    XUSER_INDEX_ANY = $000000FF;
  { }
  { Codes returned for the gamepad keystroke }
  { }
    VK_PAD_A = $5800;
    VK_PAD_B = $5801;
    VK_PAD_X = $5802;
    VK_PAD_Y = $5803;
    VK_PAD_RSHOULDER = $5804;
    VK_PAD_LSHOULDER = $5805;
    VK_PAD_LTRIGGER = $5806;
    VK_PAD_RTRIGGER = $5807;
    VK_PAD_DPAD_UP = $5810;
    VK_PAD_DPAD_DOWN = $5811;
    VK_PAD_DPAD_LEFT = $5812;
    VK_PAD_DPAD_RIGHT = $5813;
    VK_PAD_START = $5814;
    VK_PAD_BACK = $5815;
    VK_PAD_LTHUMB_PRESS = $5816;
    VK_PAD_RTHUMB_PRESS = $5817;
    VK_PAD_LTHUMB_UP = $5820;
    VK_PAD_LTHUMB_DOWN = $5821;
    VK_PAD_LTHUMB_RIGHT = $5822;
    VK_PAD_LTHUMB_LEFT = $5823;
    VK_PAD_LTHUMB_UPLEFT = $5824;
    VK_PAD_LTHUMB_UPRIGHT = $5825;
    VK_PAD_LTHUMB_DOWNRIGHT = $5826;
    VK_PAD_LTHUMB_DOWNLEFT = $5827;
    VK_PAD_RTHUMB_UP = $5830;
    VK_PAD_RTHUMB_DOWN = $5831;
    VK_PAD_RTHUMB_RIGHT = $5832;
    VK_PAD_RTHUMB_LEFT = $5833;
    VK_PAD_RTHUMB_UPLEFT = $5834;
    VK_PAD_RTHUMB_UPRIGHT = $5835;
    VK_PAD_RTHUMB_DOWNRIGHT = $5836;
    VK_PAD_RTHUMB_DOWNLEFT = $5837;
  { }
  { Flags used in XINPUT_KEYSTROKE }
  { }
    XINPUT_KEYSTROKE_KEYDOWN = $0001;
    XINPUT_KEYSTROKE_KEYUP = $0002;
    XINPUT_KEYSTROKE_REPEAT = $0004;
  { }
  { Structures used by XInput APIs }
  { }

  type
    _XINPUT_GAMEPAD = record
        wButtons : WORD;
        bLeftTrigger : BYTE;
        bRightTrigger : BYTE;
        sThumbLX : SHORT;
        sThumbLY : SHORT;
        sThumbRX : SHORT;
        sThumbRY : SHORT;
      end;
    XINPUT_GAMEPAD = _XINPUT_GAMEPAD;
    PXINPUT_GAMEPAD = ^_XINPUT_GAMEPAD;

    _XINPUT_STATE = record
        dwPacketNumber : DWORD;
        Gamepad : XINPUT_GAMEPAD;
      end;
    XINPUT_STATE = _XINPUT_STATE;
    PXINPUT_STATE = ^_XINPUT_STATE;

    _XINPUT_VIBRATION = record
        wLeftMotorSpeed : WORD;
        wRightMotorSpeed : WORD;
      end;
    XINPUT_VIBRATION = _XINPUT_VIBRATION;
    PXINPUT_VIBRATION = ^_XINPUT_VIBRATION;

    _XINPUT_CAPABILITIES = record
        _Type : BYTE;
        SubType : BYTE;
        Flags : WORD;
        Gamepad : XINPUT_GAMEPAD;
        Vibration : XINPUT_VIBRATION;
      end;
    XINPUT_CAPABILITIES = _XINPUT_CAPABILITIES;
    PXINPUT_CAPABILITIES = ^_XINPUT_CAPABILITIES;

    _XINPUT_BATTERY_INFORMATION = record
        BatteryType : BYTE;
        BatteryLevel : BYTE;
      end;
    XINPUT_BATTERY_INFORMATION = _XINPUT_BATTERY_INFORMATION;
    PXINPUT_BATTERY_INFORMATION = ^_XINPUT_BATTERY_INFORMATION;

    _XINPUT_KEYSTROKE = record
        VirtualKey : WORD;
        Unicode : WCHAR;
        Flags : WORD;
        UserIndex : BYTE;
        HidCode : BYTE;
      end;
    XINPUT_KEYSTROKE = _XINPUT_KEYSTROKE;
    PXINPUT_KEYSTROKE = ^_XINPUT_KEYSTROKE;

function InitXinput: boolean;
procedure XInputMessages(state: boolean);

var
  XInputGetState: function(dwUserIndex: dword; out pState: XINPUT_STATE): DWORD; stdcall;
  XInputSetState: function(dwUserIndex: dword; pVibration: PXINPUT_VIBRATION): DWORD; stdcall;
  XInputGetCapabilities: function(dwUserIndex: dword; dwFlags: dword; pCapabilities: PXINPUT_CAPABILITIES): DWORD; stdcall;
  XInputGetKeystroke: function(dwUserIndex: dword; reserved: dword; pKeyStroke: PXINPUT_KEYSTROKE): DWORD; stdcall;

  {$endif}

implementation

{$ifdef windows}
uses forms;

type TXBoxKeyDownThread=class(TThread)
private
public
  procedure execute; override;
end;

var
  xih: thandle;

  xt: TXBoxKeyDownThread;

procedure TXBoxKeyDownThread.execute;
var
  ks: XINPUT_KEYSTROKE;
  i: dword;

  c: TWinControl;
  h: THandle;
  s: XINPUT_STATE;
begin
  if not assigned(XInputGetKeystroke) then exit;

  while not terminated do
  begin
    if XInputGetState(0,s)=0 then
    begin
      i:=XInputGetKeystroke(0,0,@ks);
      if i=ERROR_SUCCESS then
      begin
        c:=screen.ActiveControl;
        if c<>nil then
        begin
          h:=screen.ActiveControl.Handle;

          if (ks.Flags or XINPUT_KEYSTROKE_KEYDOWN)=XINPUT_KEYSTROKE_KEYDOWN then
            SendMessage(h, WM_KEYDOWN, ks.VirtualKey, 0);

          if (ks.Flags or XINPUT_KEYSTROKE_KEYUP)=XINPUT_KEYSTROKE_KEYUP then
            SendMessage(h, WM_KEYUP, ks.VirtualKey, 0);
        end;
      end;

      sleep(50);
    end
    else
      sleep(2500);
  end;
end;

procedure XInputMessages(state: boolean);
begin
  InitXinput;
  if state then
  begin
    if assigned(XInputGetKeystroke) and (xt=nil) then
      xt:=TXBoxKeyDownThread.create(false);
  end
  else
  begin
    if xt<>nil then
    begin
      xt.Terminate;
      xt.Free;
      xt:=nil;
    end;
  end;
end;

function loadxinputmodule: THandle;
begin
  result:=loadlibrary('Xinput1_4.dll');
  if result<>0 then exit;

  result:=loadlibrary('Xinput1_3.dll');
  if result<>0 then exit;

  result:=loadlibrary('Xinput1_2.dll');
  if result<>0 then exit;

  result:=loadlibrary('Xinput1_1.dll');
  if result<>0 then exit;

  result:=loadlibrary('Xinput9_1_0.dll');
  if result<>0 then exit;

  result:=loadlibrary('Xinputuap.dll');
end;

function InitXinput: boolean;
begin

  if xih=0 then
  begin
    xih:=loadxinputmodule;
    if xih=0 then exit(false);

    XInputGetState:=GetProcAddress(xih, 'XInputGetState');
    XInputSetState:=GetProcAddress(xih, 'XInputSetState');
    XInputGetCapabilities:=GetProcAddress(xih, 'XInputGetCapabilities');
    XInputGetKeystroke:=GetProcAddress(xih, 'XInputGetKeystroke');
  end;

  result:=xih<>0;
end;

{$endif}

end.

