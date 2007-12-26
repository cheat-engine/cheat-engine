unit HotKeys;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons,registry,cefuncproc, ExtCtrls,hotkeyhandler;

type
  THotKeyForm = class(TForm)
    Label1: TLabel;
    BitBtn1: TBitBtn;
    Button1: TButton;
    edtHotkey: TEdit;
    Panel1: TPanel;
    cbFreezedirection: TComboBox;
    edtFreezeValue: TEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure edtHotkeyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button10Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    frecnr: integer;
    LastState: dword;
    LastShiftstate: TShiftState;

    keys: tkeycombo;
    fhotkeyvalue: string;
    fautoassemble: boolean;

    procedure SetRecNr(newrecnr: integer);
    procedure SetHotkeyValue(x: string);
    procedure SetAutoAssemble(x: boolean);
  public
    { Public declarations }

    hotkey: string;
    freezedirection: integer;
    property hotkeyvalue: string read fhotkeyvalue write sethotkeyvalue;
    property recnr: integer read frecnr write SetRecnr;
    property autoassemble: boolean read fautoassemble write SetAutoAssemble;
  end;

var
  HotKeyForm: THotKeyForm;

implementation

uses MainUnit;

{$R *.DFM}

procedure THotkeyform.SetAutoAssemble(x: boolean);
begin
  fautoassemble:=x;
  if x then
  begin
    cbFreezedirection.Items.Clear;
    cbfreezedirection.Items.Add('Toggle cheat on/off');
    cbfreezedirection.ItemindeX:=0;
    edtFreezeValue.Visible:=false;
  end
  else
  begin
    cbFreezedirection.Items.Clear;
    cbfreezedirection.Items.Add('Set value to:');
    cbfreezedirection.Items.Add('Decrease value with:');
    cbfreezedirection.Items.Add('Increase value with:');

    cbfreezedirection.Itemindex:=0;
    edtFreezeValue.Visible:=true;
  end;
end;

procedure THotkeyform.SetHotkeyValue(x:string);
begin
  fhotkeyvalue:=x;
  edtFreezeValue.Text:=x;
end;

procedure THotKeyForm.setRecNr(newrecnr: integer);
begin
  frecnr:=newrecnr;
  if newrecnr=-1 then
  begin
    //default
    height:=105;
    panel1.Visible:=false;
  end
  else
  begin
    //changed the value
    panel1.Visible:=true;
    height:=157;
  end;
end;


procedure THotKeyForm.BitBtn1Click(Sender: TObject);
var keymod: dword;
    i: integer;
    newid: word;
    reg: Tregistry;
    found: boolean;
begin
  freezedirection:=cbFreezedirection.ItemIndex;
  hotkeyvalue:=edtFreezeValue.text;

  keymod:=0;
  if ssctrl in lastshiftstate then keymod:=keymod or MOD_CONTROL;
  if ssAlt in LastShiftState then keymod:=keymod or MOD_ALT;
  if ssShift in LastShiftState then keymod:=keymod or MOD_Shift;

//  if keymod=0 then
//    raise Exception.create('At least control, alt or shift have to be pressed to be a valid hotkey!');

//  if (LastState=VK_SHIFT) or (LastState=VK_CONTROL) or (LastState=VK_MENU) then
//    raise Exception.create('One other key has to be pressed!(except ctrl, alt or shift!)');

  

  if recnr=-1 then
  begin

    unregisterhotkey(mainform.handle,0);
    if registerhotkey(mainform.handle,0,KeyMod,laststate)=false then
      raise Exception.Create('This is not a valid key combination or another program is using this HotKey. Try another one!)')
    else
    begin
      reg:=Tregistry.Create;

      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey('\Software\Cheat Engine',true) then
      begin
        reg.WriteInteger('BringToFrontKey',laststate);
        reg.WriteInteger('BringToFrontModifier',keymod);
        reg.WriteString('BringToFrontHotkey',edtHotkey.text);
        reg.CloseKey;
      end;
      reg.free;

      MainForm.label7.caption:=edtHotkey.text+' '+mainform.fronttext;
      Modalresult:=mrok;
      HotKeyForm.Close;
    end;
  end
  else
  begin
    if mainform.hotkeys[recnr]<>-1 then
      unregisterhotkey(mainform.Handle,mainform.hotkeys[recnr]);

    mainform.hotkeys[recnr]:=-1;

    //find a new id#
    newid:=10;

    found:=false;
    while (not found) and (newid<$bfff) do
    begin
      i:=0;
      found:=true;
      for i:=0 to mainform.NumberOfRecords-1 do
        if newid=mainform.Hotkeys[i] then
        begin
          found:=false;
          inc(newid);
          break;
        end;
    end;

    if newid>=$C000 then raise exception.Create('You have too many hotkeys!'); //as if this message is needed, SLOW!!!!

    if not registerhotkey(mainform.Handle,newid,KeyMod,laststate) then
    begin
      ModalResult:=mrCancel;
      raise Exception.Create('This hotkey is not valid or already taken! (by another program or Cheat Engine itself)');
    end;

    mainform.hotkeystrings[recnr]:=edtHotkey.text;
    mainform.Hotkeys[recnr]:=newid;
    Modalresult:=mrok;

  end;
end;

procedure THotKeyForm.edtHotkeyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var newstr: String;
begin
  laststate:=Key;
  lastshiftstate:=Shift;

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

  edthotkey.text:=newstr;
end;


procedure THotKeyForm.Button10Click(Sender: TObject);
begin
  zeromemory(@keys[0],10);
  edthotkey.Text:=ConvertKeyComboToString(keys); //=''
  edthotkey.SetFocus;
end;

procedure THotKeyForm.FormShow(Sender: TObject);
begin
  edthotkey.Text:='';
  laststate:=0;
  lastshiftstate:=[];
  cbFreezeDirection.ItemIndex:=0;
end;

end.
