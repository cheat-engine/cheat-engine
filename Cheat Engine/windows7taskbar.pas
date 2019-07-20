unit windows7taskbar;

{$MODE Delphi}
{$WARN 5044 off : Symbol "$1" is not portable}   //Windows 7 taskbar, says enough
interface

//credits to http://alex.ciobanu.org/?p=215

uses Forms,win32int, win32proc;

type
  TTaskBarProgressState = (tbpsNone, tbpsIndeterminate, tbpsNormal, tbpsError, tbpsPaused);

  procedure SetProgressState(const AState: TTaskBarProgressState);
  procedure SetProgressValue(const ACurrent, AMax: UInt64);

implementation
uses
  ComObj, Types, cefuncproc;

const
  TASKBAR_CID: TGUID = '{56FDF344-FD6D-11d0-958A-006097C9A090}';

const
  TBPF_NOPROGRESS = 0;
  TBPF_INDETERMINATE = 1;
  TBPF_NORMAL = 2;
  TBPF_ERROR = 4;
  TBPF_PAUSED = 8;

type
  { Definition for Windows 7 ITaskBarList3 }
  ITaskBarList3 = interface(IUnknown)
  ['{EA1AFB91-9E28-4B86-90E9-9E9F8A5EEFAF}']
    procedure HrInit(); stdcall;
    procedure AddTab(hwnd: THandle); stdcall;
    procedure DeleteTab(hwnd: THandle); stdcall;
    procedure ActivateTab(hwnd: THandle); stdcall;
    procedure SetActiveAlt(hwnd: THandle); stdcall;

    procedure MarkFullscreenWindow(hwnd: THandle; fFullscreen: Boolean); stdcall;

    procedure SetProgressValue(hwnd: THandle; ullCompleted: UInt64; ullTotal: UInt64); stdcall;
    procedure SetProgressState(hwnd: THandle; tbpFlags: Cardinal); stdcall;

    procedure RegisterTab(hwnd: THandle; hwndMDI: THandle); stdcall;
    procedure UnregisterTab(hwndTab: THandle); stdcall;
    procedure SetTabOrder(hwndTab: THandle; hwndInsertBefore: THandle); stdcall;
    procedure SetTabActive(hwndTab: THandle; hwndMDI: THandle; tbatFlags: Cardinal); stdcall;
    procedure ThumbBarAddButtons(hwnd: THandle; cButtons: Cardinal; pButtons: Pointer); stdcall;
    procedure ThumbBarUpdateButtons(hwnd: THandle; cButtons: Cardinal; pButtons: Pointer); stdcall;
    procedure ThumbBarSetImageList(hwnd: THandle; himl: THandle); stdcall;
    procedure SetOverlayIcon(hwnd: THandle; hIcon: THandle; pszDescription: PChar); stdcall;
    procedure SetThumbnailTooltip(hwnd: THandle; pszDescription: PChar); stdcall;
    procedure SetThumbnailClip(hwnd: THandle; var prcClip: TRect); stdcall;
  end;

var
  { Global variable storing the COM interface }
  GlobalTaskBarInterface: ITaskBarList3;


{ TFormHelper }

procedure SetProgressState(const AState: TTaskBarProgressState);
const
  Flags: array[TTaskBarProgressState] of Cardinal = (0, 1, 2, 4, 8);
begin


  if GlobalTaskBarInterface <> nil then
    GlobalTaskBarInterface.SetProgressState(Win32WidgetSet.AppHandle, Flags[AState]);
end;

procedure SetProgressValue(const ACurrent, AMax: UInt64);
begin
  if GlobalTaskBarInterface <> nil then
    GlobalTaskBarInterface.SetProgressValue(Win32WidgetSet.AppHandle, ACurrent, AMax);
end;

procedure InitializeAPI();
var
  Unk: IInterface;

begin
  if WindowsVersion<wv7 then exit;

  { Make sure that COM is initialized }
  CoInitializeEx(nil, 0);

  try
    { Obtain an IUnknown }
    Unk := CreateComObject(TASKBAR_CID);

    if Unk = nil then
      Exit;

    { Cast to the required interface }
    GlobalTaskBarInterface := Unk as ITaskBarList3;

    { Initialize }
    GlobalTaskBarInterface.HrInit();
  except
    GlobalTaskBarInterface := nil;
  end;
end;

initialization
  { Initialize the Windows 7 taskbar API }
  InitializeAPI();

finalization
  { Force interface release }
  GlobalTaskBarInterface := nil;

end.
