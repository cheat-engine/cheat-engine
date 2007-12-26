unit formhotkeyunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormHotkey = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
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

{$R *.dfm}

procedure TFormHotkey.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var newstr: String;
begin
  lastkey:=Key;
  laststate:=Shift;

  newstr:='';
  if ssCtrl in Shift then newstr:='Ctrl';
  if ssAlt in shift then if newstr='' then newstr:='Alt' else newstr:=newstr+'+Alt';
  if ssShift in Shift then if newstr='' then newstr:='Shift' else newstr:=newstr+'+Shift';

  if newstr='' then
  case key of
    VK_BACK	: newstr:='Backspace';
    VK_TAB	: newstr:='Tab';
    VK_CLEAR	: newstr:='Clear';
    VK_RETURN	: newstr:='Enter';
    VK_PAUSE	: newstr:='Pause';
    VK_CAPITAL	: newstr:='Caps Lock';
    VK_ESCAPE	: newstr:='Esc';
    VK_SPACE	: newstr:='Space bar';
    VK_PRIOR	: newstr:='Page Up';
    VK_NEXT	: newstr:='Page Down';
    VK_END	: newstr:='End';
    VK_HOME	: newstr:='Home';
    VK_LEFT	: newstr:='Left Arrow';
    VK_UP	: newstr:='Up Arrow';
    VK_RIGHT	: newstr:='Right Arrow';
    VK_DOWN	: newstr:='Down Arrow';
    VK_SELECT	: newstr:='Select';
    VK_PRINT	: newstr:='Print';
    VK_EXECUTE	: newstr:='Execute';
    VK_SNAPSHOT	: newstr:='Print Screen';
    VK_INSERT	: newstr:='Insert';
    VK_DELETE	: newstr:='Delete';
    VK_HELP	: newstr:='Help';
    VK_LWIN	: newstr:='Left Windows key';
    VK_RWIN	: newstr:='Right Windows key';
    VK_APPS	: newstr:='Applications key';
    VK_NUMPAD0	: newstr:='numeric 0';
    VK_NUMPAD1	: newstr:='numeric 1';
    VK_NUMPAD2	: newstr:='numeric 2';
    VK_NUMPAD3	: newstr:='numeric 3';
    VK_NUMPAD4	: newstr:='numeric 4';
    VK_NUMPAD5	: newstr:='numeric 5';
    VK_NUMPAD6	: newstr:='numeric 6';
    VK_NUMPAD7	: newstr:='numeric 7';
    VK_NUMPAD8	: newstr:='numeric 8';
    VK_NUMPAD9	: newstr:='numeric 9';
    VK_MULTIPLY	: newstr:='numeric *';
    VK_ADD	: newstr:='numeric +';
    VK_SEPARATOR : newstr:='numeric Separator';
    VK_SUBTRACT	: newstr:='numeric -';
    VK_DECIMAL	: newstr:='numeric .';
    VK_DIVIDE	: newstr:='numeric /';
    VK_F1	: newstr:='F1';
    VK_F2	: newstr:='F2';
    VK_F3	: newstr:='F3';
    VK_F4	: newstr:='F4';
    VK_F5	: newstr:='F5';
    VK_F6	: newstr:='F6';
    VK_F7	: newstr:='F7';
    VK_F8	: newstr:='F8';
    VK_F9	: newstr:='F9';
    VK_F10	: newstr:='F10';
    VK_F11	: newstr:='F11';
    VK_F12	: newstr:='F12';
    VK_F13	: newstr:='F13';
    VK_F14	: newstr:='F14';
    VK_F15	: newstr:='F15';
    VK_F16	: newstr:='F16';
    VK_F17	: newstr:='F17';
    VK_F18	: newstr:='F18';
    VK_F19	: newstr:='F19';
    VK_F20	: newstr:='F20';
    VK_F21	: newstr:='F21';
    VK_F22	: newstr:='F22';
    VK_F23	: newstr:='F23';
    VK_F24	: newstr:='F24';
    VK_NUMLOCK	: newstr:='Num Lock';
    VK_SCROLL	: newstr:='Scroll Lock';
    48..57      : newstr:=chr(key);
    65..90      : newstr:=chr(key);
    else  newstr:='#'+IntToStr(key);
  end
  else
  case key of
    VK_BACK	: newstr:=newstr+'+Backspace';
    VK_TAB	: newstr:=newstr+'+Tab';
    VK_CLEAR	: newstr:=newstr+'+Clear';
    VK_RETURN	: newstr:=newstr+'+Enter';
    VK_PAUSE	: newstr:=newstr+'+Pause';
    VK_CAPITAL	: newstr:=newstr+'+Caps Lock';
    VK_ESCAPE	: newstr:=newstr+'+Esc';
    VK_SPACE	: newstr:=newstr+'+Space bar';
    VK_PRIOR	: newstr:=newstr+'+Page Up';
    VK_NEXT	: newstr:=newstr+'+Page Down';
    VK_END	: newstr:=newstr+'+End';
    VK_HOME	: newstr:=newstr+'+Home';
    VK_LEFT	: newstr:=newstr+'+Left Arrow';
    VK_UP	: newstr:=newstr+'+Up Arrow';
    VK_RIGHT	: newstr:=newstr+'+Right Arrow';
    VK_DOWN	: newstr:=newstr+'+Down Arrow';
    VK_SELECT	: newstr:=newstr+'+Select';
    VK_PRINT	: newstr:=newstr+'+Print';
    VK_EXECUTE	: newstr:=newstr+'+Execute';
    VK_SNAPSHOT	: newstr:=newstr+'+Print Screen';
    VK_INSERT	: newstr:=newstr+'+Insert';
    VK_DELETE	: newstr:=newstr+'+Delete';
    VK_HELP	: newstr:=newstr+'+Help';
    VK_LWIN	: newstr:=newstr+'+Left Windows key';
    VK_RWIN	: newstr:=newstr+'+Right Windows key';
    VK_APPS	: newstr:=newstr+'+Applications key';
    VK_NUMPAD0	: newstr:=newstr+'+numeric 0';
    VK_NUMPAD1	: newstr:=newstr+'+numeric 1';
    VK_NUMPAD2	: newstr:=newstr+'+numeric 2';
    VK_NUMPAD3	: newstr:=newstr+'+numeric 3';
    VK_NUMPAD4	: newstr:=newstr+'+numeric 4';
    VK_NUMPAD5	: newstr:=newstr+'+numeric 5';
    VK_NUMPAD6	: newstr:=newstr+'+numeric 6';
    VK_NUMPAD7	: newstr:=newstr+'+numeric 7';
    VK_NUMPAD8	: newstr:=newstr+'+numeric 8';
    VK_NUMPAD9	: newstr:=newstr+'+numeric 9';
    VK_MULTIPLY	: newstr:=newstr+'+numeric *';
    VK_ADD	: newstr:=newstr+'+numeric +';
    VK_SEPARATOR : newstr:=newstr+'+numeric Separator';
    VK_SUBTRACT	: newstr:=newstr+'+numeric -';
    VK_DECIMAL	: newstr:=newstr+'+numeric .';
    VK_DIVIDE	: newstr:=newstr+'+numeric /';
    VK_F1	: newstr:=newstr+'+F1';
    VK_F2	: newstr:=newstr+'+F2';
    VK_F3	: newstr:=newstr+'+F3';
    VK_F4	: newstr:=newstr+'+F4';
    VK_F5	: newstr:=newstr+'+F5';
    VK_F6	: newstr:=newstr+'+F6';
    VK_F7	: newstr:=newstr+'+F7';
    VK_F8	: newstr:=newstr+'+F8';
    VK_F9	: newstr:=newstr+'+F9';
    VK_F10	: newstr:=newstr+'+F10';
    VK_F11	: newstr:=newstr+'+F11';
    VK_F12	: newstr:=newstr+'+F12';
    VK_F13	: newstr:=newstr+'+F13';
    VK_F14	: newstr:=newstr+'+F14';
    VK_F15	: newstr:=newstr+'+F15';
    VK_F16	: newstr:=newstr+'+F16';
    VK_F17	: newstr:=newstr+'+F17';
    VK_F18	: newstr:=newstr+'+F18';
    VK_F19	: newstr:=newstr+'+F19';
    VK_F20	: newstr:=newstr+'+F20';
    VK_F21	: newstr:=newstr+'+F21';
    VK_F22	: newstr:=newstr+'+F22';
    VK_F23	: newstr:=newstr+'+F23';
    VK_F24	: newstr:=newstr+'+F24';
    VK_NUMLOCK	: newstr:=newstr+'+Num Lock';
    VK_SCROLL	: newstr:=newstr+'+Scroll Lock';
    48..57      : newstr:=newstr+'+'+chr(key);
    65..90      : newstr:=newstr+'+'+chr(key);
    else  newstr:=newstr+'+#'+IntToStr(key);
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

end.
