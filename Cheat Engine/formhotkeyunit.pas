unit formhotkeyunit;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources, ExtCtrls, LCLType;

type

  { TFormHotkey }

  TFormHotkey = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    procedure Edit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    lastkey: word;
    laststate: TShiftstate;
  public
    { Public declarations }
    hotkeystring: string;
    modifier: dword;
    key: word;
  end;

var
  FormHotkey: TFormHotkey;

implementation

resourcestring
  rsCtrl = 'Ctrl';
  rsAlt = 'Alt';
  rsShift = 'Shift';
  rsBackspace = 'Backspace';
  rsTab = 'Tab';
  rsClear = 'Clear';
  rsEnter = 'Enter';
  rsPause = 'Pause';
  rsCapsLock = 'Caps Lock';
  rsEsc = 'Esc';
  rsSpaceBar = 'Space bar';
  rsPageUp = 'Page Up';
  rsPageDown = 'Page Down';
  rsEnd = 'End';
  rsHome = 'Home';
  rsLeftArrow = 'Left Arrow';
  rsUpArrow = 'Up Arrow';
  rsRightArrow = 'Right Arrow';
  rsDownArrow = 'Down Arrow';
  rsSelect = 'Select';
  rsPrint = 'Print';
  rsExecute = 'Execute';
  rsPrintScreen = 'Print Screen';
  rsInsert = 'Insert';
  rsDelete = 'Delete';
  rsHelp = 'Help';
  rsLeftWindowsKey = 'Left Windows key';
  rsRightWindowsKey = 'Right Windows key';
  rsApplicationsKey = 'Applications key';
  rsNumeric = 'numeric';
  rsSeparator = 'Separator';
  rsNumLock = 'Num Lock';
  rsScrollLock = 'Scroll Lock';


procedure TFormHotkey.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var newstr: String;
begin
  lastkey:=Key;
  laststate:=Shift;

  newstr:='';
  if ssCtrl in Shift then newstr:=rsCtrl;
  if ssAlt in shift then if newstr='' then newstr:=rsAlt else newstr:=newstr+'+'+rsAlt;
  if ssShift in Shift then if newstr='' then newstr:=rsShift else newstr:=newstr+'+'+rsShift;

  if newstr<>'' then
    newstr:=newstr+'+';

  case key of
    VK_OEM_PLUS : newstr:='=';
    VK_OEM_MINUS : newstr:='-';
    VK_OEM_PERIOD : newstr:=',';
    VK_OEM_COMMA : newstr:='.';
    VK_OEM_1 : newstr:=';';
    VK_OEM_2 : newstr:='/';
    VK_OEM_3 : newstr:='`';
    VK_OEM_4 : newstr:='[';
    VK_OEM_5 : newstr:='\';
    VK_OEM_6 : newstr:=']';
    VK_OEM_7 : newstr:='''';
    VK_BACK	: newstr:=newstr+rsBackspace;
    VK_TAB	: newstr:=newstr+rsTab;
    VK_CLEAR	: newstr:=newstr+rsClear;
    VK_RETURN	: newstr:=newstr+rsEnter;
    VK_PAUSE	: newstr:=newstr+rsPause;
    VK_CAPITAL	: newstr:=newstr+rsCapsLock;
    VK_ESCAPE	: newstr:=newstr+rsEsc;
    VK_SPACE	: newstr:=newstr+rsSpaceBar;
    VK_PRIOR	: newstr:=newstr+rsPageUp;
    VK_NEXT	: newstr:=newstr+rsPageDown;
    VK_END	: newstr:=newstr+rsEnd;
    VK_HOME	: newstr:=newstr+rsHome;
    VK_LEFT	: newstr:=newstr+rsLeftArrow;
    VK_UP	: newstr:=newstr+rsUpArrow;
    VK_RIGHT	: newstr:=newstr+rsRightArrow;
    VK_DOWN	: newstr:=newstr+rsDownArrow;
    VK_SELECT	: newstr:=newstr+rsSelect;
    VK_PRINT	: newstr:=newstr+rsPrint;
    VK_EXECUTE	: newstr:=newstr+rsExecute;
    VK_SNAPSHOT	: newstr:=newstr+rsPrintScreen;
    VK_INSERT	: newstr:=newstr+rsInsert;
    VK_DELETE	: newstr:=newstr+rsDelete;
    VK_HELP	: newstr:=newstr+rsHelp;
    VK_LWIN	: newstr:=newstr+rsLeftWindowsKey;
    VK_RWIN	: newstr:=newstr+rsRightWindowsKey;
    VK_APPS	: newstr:=newstr+rsApplicationsKey;
    VK_NUMPAD0	: newstr:=newstr+rsNumeric+' 0';
    VK_NUMPAD1	: newstr:=newstr+rsNumeric+' 1';
    VK_NUMPAD2	: newstr:=newstr+rsNumeric+' 2';
    VK_NUMPAD3	: newstr:=newstr+rsNumeric+' 3';
    VK_NUMPAD4	: newstr:=newstr+rsNumeric+' 4';
    VK_NUMPAD5	: newstr:=newstr+rsNumeric+' 5';
    VK_NUMPAD6	: newstr:=newstr+rsNumeric+' 6';
    VK_NUMPAD7	: newstr:=newstr+rsNumeric+' 7';
    VK_NUMPAD8	: newstr:=newstr+rsNumeric+' 8';
    VK_NUMPAD9	: newstr:=newstr+rsNumeric+' 9';
    VK_MULTIPLY	: newstr:=newstr+rsNumeric+' *';
    VK_ADD	: newstr:=newstr+rsNumeric+' +';
    VK_SEPARATOR : newstr:=newstr+rsNumeric+' '+rsSeparator;
    VK_SUBTRACT	: newstr:=newstr+rsNumeric+' -';
    VK_DECIMAL	: newstr:=newstr+rsNumeric+' .';
    VK_DIVIDE	: newstr:=newstr+rsNumeric+' /';
    VK_F1	: newstr:=newstr+'F1';
    VK_F2	: newstr:=newstr+'F2';
    VK_F3	: newstr:=newstr+'F3';
    VK_F4	: newstr:=newstr+'F4';
    VK_F5	: newstr:=newstr+'F5';
    VK_F6	: newstr:=newstr+'F6';
    VK_F7	: newstr:=newstr+'F7';
    VK_F8	: newstr:=newstr+'F8';
    VK_F9	: newstr:=newstr+'F9';
    VK_F10	: newstr:=newstr+'F10';
    VK_F11	: newstr:=newstr+'F11';
    VK_F12	: newstr:=newstr+'F12';
    VK_F13	: newstr:=newstr+'F13';
    VK_F14	: newstr:=newstr+'F14';
    VK_F15	: newstr:=newstr+'F15';
    VK_F16	: newstr:=newstr+'F16';
    VK_F17	: newstr:=newstr+'F17';
    VK_F18	: newstr:=newstr+'F18';
    VK_F19	: newstr:=newstr+'F19';
    VK_F20	: newstr:=newstr+'F20';
    VK_F21	: newstr:=newstr+'F21';
    VK_F22	: newstr:=newstr+'F22';
    VK_F23	: newstr:=newstr+'F23';
    VK_F24	: newstr:=newstr+'F24';
    VK_NUMLOCK	: newstr:=newstr+rsNumLock;
    VK_SCROLL	: newstr:=newstr+rsScrollLock;
    48..57      : newstr:=newstr+chr(key);
    65..90      : newstr:=newstr+chr(key);
    else  newstr:=newstr+'#'+IntToStr(key);
  end;


  edit1.text:=newstr;
end;

procedure TFormHotkey.Button1Click(Sender: TObject);
begin
  key:=lastkey;
  modifier:=0;
  if ssctrl in laststate then modifier:=modifier or MOD_CONTROL;
  if ssAlt in LastState then modifier:=modifier or MOD_ALT;
  if ssShift in LastState then modifier:=modifier or MOD_Shift;

  hotkeystring:=edit1.text;
  modalresult:=mrok;
end;

initialization
  {$i formhotkeyunit.lrs}

end.
