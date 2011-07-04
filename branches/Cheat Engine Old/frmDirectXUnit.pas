unit frmDirectXUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,cefuncproc, Menus, Buttons, ComCtrls;

const dxmessversion=1;
type
  TfrmDirectX = class(TForm)
    ScrollBox1: TScrollBox;
    Panel1: TPanel;
    Label1: TLabel;
    LoadButton: TSpeedButton;
    SaveButton: TSpeedButton;
    Panel2: TPanel;
    Label21: TLabel;
    Label22: TLabel;
    Button6: TButton;
    editKeyPolling: TEdit;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label23: TLabel;
    Zoom1: TEdit;
    Zoom2: TEdit;
    Zoom3: TEdit;
    Zoom4: TEdit;
    Zoom5: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Zoomlevel1: TEdit;
    Zoomlevel2: TEdit;
    Zoomlevel3: TEdit;
    Zoomlevel4: TEdit;
    Zoomlevel5: TEdit;
    Zoomin: TEdit;
    Button7: TButton;
    zoomdelta: TEdit;
    Zoomout: TEdit;
    Button8: TButton;
    nozoom: TEdit;
    Button9: TButton;
    GroupBox2: TGroupBox;
    Label25: TLabel;
    fog: TEdit;
    Button10: TButton;
    Button11: TButton;
    zbuffer: TEdit;
    Label26: TLabel;
    Label27: TLabel;
    Lighting: TEdit;
    Button12: TButton;
    GroupBox3: TGroupBox;
    Label28: TLabel;
    autoaimtoggle: TEdit;
    Button13: TButton;
    CheckBox1: TCheckBox;
    CheckBox3: TCheckBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit1: TEdit;
    Edit2: TEdit;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    TrackBar1: TTrackBar;
    Label24: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    PreviousTexture: TEdit;
    Button16: TButton;
    CheckBox2: TCheckBox;
    Label35: TLabel;
    Button17: TButton;
    nexttexture: TEdit;
    Label36: TLabel;
    Button18: TButton;
    locktexture: TEdit;
    Label37: TLabel;
    IncreaseX: TEdit;
    Button19: TButton;
    Label38: TLabel;
    DecreaseX: TEdit;
    Button20: TButton;
    Label39: TLabel;
    IncreaseY: TEdit;
    Button21: TButton;
    Label40: TLabel;
    DecreaseY: TEdit;
    Button22: TButton;
    Label41: TLabel;
    IncreaseZ: TEdit;
    Button23: TButton;
    Label42: TLabel;
    DecreaseZ: TEdit;
    Button24: TButton;
    aimfile1: TEdit;
    Label43: TLabel;
    setaimsetting1: TEdit;
    Button25: TButton;
    Label46: TLabel;
    aimfile2: TEdit;
    Label44: TLabel;
    setaimsetting2: TEdit;
    Button26: TButton;
    Label45: TLabel;
    aimfile3: TEdit;
    Label47: TLabel;
    setaimsetting3: TEdit;
    Button27: TButton;
    Label48: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Label49: TLabel;
    loadaimsettingsfile: TEdit;
    Button28: TButton;
    Label50: TLabel;
    Saveaimsettingsfile: TEdit;
    Button29: TButton;
    Label51: TLabel;
    callibrationkey: TEdit;
    Button30: TButton;
    Label52: TLabel;
    mousecallibrationhorizontal1point: TEdit;
    Label53: TLabel;
    Label54: TLabel;
    mousecallibrationhorizontal5point: TEdit;
    Label55: TLabel;
    Label56: TLabel;
    mousecallibrationhorizontal10point: TEdit;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    mousecallibrationvertical1point: TEdit;
    Label60: TLabel;
    Label61: TLabel;
    mousecallibrationvertical5point: TEdit;
    Label62: TLabel;
    Label63: TLabel;
    mousecallibrationvertical10point: TEdit;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    mousecallibrationhorizontal2point: TEdit;
    Label67: TLabel;
    Label68: TLabel;
    mousecallibrationvertical2point: TEdit;
    Label69: TLabel;
    Label70: TLabel;
    mousecallibrationhorizontal20point: TEdit;
    Label71: TLabel;
    Label72: TLabel;
    mousecallibrationvertical20point: TEdit;
    Label73: TLabel;
    Label74: TLabel;
    mousecallibrationhorizontal40point: TEdit;
    Label75: TLabel;
    Label76: TLabel;
    mousecallibrationvertical40point: TEdit;
    Label77: TLabel;
    Panel3: TPanel;
    rbtoggleoneff: TRadioButton;
    rbKeepDown: TRadioButton;
    autoshoot: TCheckBox;
    Panel4: TPanel;
    Label29: TLabel;
    increaselag: TEdit;
    Button14: TButton;
    Label78: TLabel;
    lag: TEdit;
    Label30: TLabel;
    Label31: TLabel;
    decreaselag: TEdit;
    Button15: TButton;
    lagdelta: TEdit;
    getlagfrommemory: TCheckBox;
    Button31: TButton;
    Addresslabel: TLabel;
    UseFpsLag: TCheckBox;
    Wireframe: TEdit;
    Button32: TButton;
    Label79: TLabel;
    ShowKeyList: TEdit;
    Button33: TButton;
    Label32: TLabel;
    GroupBox4: TGroupBox;
    Label80: TLabel;
    SaveAllTextures: TEdit;
    Button34: TButton;
    Mousekeymenu: TPopupMenu;
    LeftMouse1: TMenuItem;
    CenterMouse1: TMenuItem;
    RightMouse1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Zoom1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button6Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Zoom2KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Zoom3KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Zoom4KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Zoom5KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ZoominKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ZoomoutKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure nozoomKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure fogKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure zbufferKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LightingKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure autoaimtoggleKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button13Click(Sender: TObject);
    procedure increaselagKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure decreaselagKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure setaimsetting1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure setaimsetting2KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure setaimsetting3KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button25Click(Sender: TObject);
    procedure Button26Click(Sender: TObject);
    procedure Button27Click(Sender: TObject);
    procedure PreviousTextureKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure nexttextureKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure locktextureKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure IncreaseXKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure IncreaseYKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure IncreaseZKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DecreaseXKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DecreaseYKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DecreaseZKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button19Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure Button24Click(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure Button28Click(Sender: TObject);
    procedure Button29Click(Sender: TObject);
    procedure loadaimsettingsfileKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SaveaimsettingsfileKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button16Click(Sender: TObject);
    procedure Button30Click(Sender: TObject);
    procedure callibrationkeyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure rbtoggleoneffClick(Sender: TObject);
    procedure Button31Click(Sender: TObject);
    procedure getlagfrommemoryClick(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button32Click(Sender: TObject);
    procedure WireframeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ShowKeyListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button33Click(Sender: TObject);
    procedure Button34Click(Sender: TObject);
    procedure SaveAllTexturesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LeftMouse1Click(Sender: TObject);
  private
    { Private declarations }


    tempkeys: TKeys;

    procedure ApplyTempkeys;
    procedure updatecallibration(var Message: TMessage); message WM_USER+1;
    procedure setkey(key:word; var tempkeysitem: tkeycombo;ed:tedit);
  public
    { Public declarations }
  end;

var
  frmDirectX: TfrmDirectX;

implementation

uses MainUnit;

{$R *.dfm}

procedure TfrmDirectX.updatecallibration(var Message: TMessage);
begin
  mousecallibrationhorizontal1point.text:=format('%.2f',[keys.mousecallibrationhorizontal1point]);
  mousecallibrationhorizontal2point.text:=format('%.2f',[keys.mousecallibrationhorizontal2point]);
  mousecallibrationhorizontal5point.text:=format('%.2f',[keys.mousecallibrationhorizontal5point]);
  mousecallibrationhorizontal10point.text:=format('%.2f',[keys.mousecallibrationhorizontal10point]);
  mousecallibrationhorizontal20point.text:=format('%.2f',[keys.mousecallibrationhorizontal20point]);
  mousecallibrationhorizontal40point.text:=format('%.2f',[keys.mousecallibrationhorizontal40point]);

  mousecallibrationvertical1point.text:=format('%.2f',[keys.mousecallibrationvertical1point]);
  mousecallibrationvertical2point.text:=format('%.2f',[keys.mousecallibrationvertical2point]);
  mousecallibrationvertical5point.text:=format('%.2f',[keys.mousecallibrationvertical5point]);
  mousecallibrationvertical10point.text:=format('%.2f',[keys.mousecallibrationvertical10point]);
  mousecallibrationvertical20point.text:=format('%.2f',[keys.mousecallibrationvertical20point]);
  mousecallibrationvertical40point.text:=format('%.2f',[keys.mousecallibrationvertical40point]);


end;


procedure TfrmDirectX.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action:=cafree;
  unmapviewoffile(keys);
  closehandle(keysfilemapping);
end;

procedure TfrmDirectX.FormCreate(Sender: TObject);
begin
  scrollbox1.VertScrollBar.Position:=0;

  zeromemory(keys,sizeof(keys));

  keys.configured:=false;
  keys.CEDir:=CheatEngineDir;


  Zoomlevel1.Text:=format('%.1f',[1.00]);
  Zoomlevel2.Text:=format('%.1f',[2.00]);
  Zoomlevel3.Text:=format('%.1f',[4.00]);
  Zoomlevel4.Text:=format('%.1f',[8.00]);
  Zoomlevel5.Text:=format('%.1f',[16.00]);
  zoomdelta.Text:=format('%.1f',[0.2]);

  mousecallibrationhorizontal1point.Text:=format('%.1f',[1.00]);
  mousecallibrationhorizontal2point.Text:=format('%.1f',[2.00]);
  mousecallibrationhorizontal5point.Text:=format('%.1f',[5.00]);
  mousecallibrationhorizontal10point.Text:=format('%.1f',[10.00]);
  mousecallibrationhorizontal20point.Text:=format('%.1f',[20.00]);
  mousecallibrationhorizontal40point.Text:=format('%.1f',[40.00]);

  mousecallibrationvertical1point.Text:=format('%.1f',[1.00]);
  mousecallibrationvertical2point.Text:=format('%.1f',[2.00]);
  mousecallibrationvertical5point.Text:=format('%.1f',[5.00]);
  mousecallibrationvertical10point.Text:=format('%.1f',[10.00]);
  mousecallibrationvertical20point.Text:=format('%.1f',[20.00]);
  mousecallibrationvertical40point.Text:=format('%.1f',[40.00]);


  keys.movespeed:=0.2;
  keys.pollinginterval:=250;
  keys.lagtoset:=200;
  keys.setlag:=true;

  Zoomlevel5.Text:=format('%.1f',[16.00]);

  tempkeys:=keys^;
end;

procedure TfrmDirectX.Zoom1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.zoom1[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.zoom1[i]=0 then
      begin
        tempkeys.zoom1[i]:=key;
        break;
      end else
      if tempkeys.zoom1[i]=key then break;
  end;

  zoom1.Text:=ConvertKeyComboToString(tempkeys.zoom1);
end;

procedure TfrmDirectX.Button6Click(Sender: TObject);
begin
  applytempkeys;

  keys^:=tempkeys;
end;

procedure TfrmDirectX.Button1Click(Sender: TObject);
begin
  zeromemory(@tempkeys.zoom1[0],10);
  zoom1.Text:=ConvertKeyComboToString(tempkeys.zoom1); //=''
  zoom1.SetFocus;
end;

procedure TfrmDirectX.Button2Click(Sender: TObject);
begin
  zeromemory(@tempkeys.zoom2[0],10);
  zoom2.Text:=ConvertKeyComboToString(tempkeys.zoom2); //=''
  zoom2.SetFocus;
end;

procedure TfrmDirectX.Button3Click(Sender: TObject);
begin
  zeromemory(@tempkeys.zoom3[0],10);
  zoom3.Text:=ConvertKeyComboToString(tempkeys.zoom3); //=''
  zoom3.SetFocus;
end;

procedure TfrmDirectX.Button4Click(Sender: TObject);
begin
  zeromemory(@tempkeys.zoom4[0],10);
  zoom4.Text:=ConvertKeyComboToString(tempkeys.zoom4); //=''
  zoom4.SetFocus;
end;

procedure TfrmDirectX.Button5Click(Sender: TObject);
begin
  zeromemory(@tempkeys.zoom5[0],10);
  zoom5.Text:=ConvertKeyComboToString(tempkeys.zoom5); //=''
  zoom5.SetFocus;
end;

procedure TfrmDirectX.Zoom2KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.zoom2[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.zoom2[i]=0 then
      begin
        tempkeys.zoom2[i]:=key;
        break;
      end else
      if tempkeys.zoom2[i]=key then break;
  end;

  zoom2.Text:=ConvertKeyComboToString(tempkeys.zoom2);
end;

procedure TfrmDirectX.Zoom3KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.zoom3[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.zoom3[i]=0 then
      begin
        tempkeys.zoom3[i]:=key;
        break;
      end else
      if tempkeys.zoom3[i]=key then break;
  end;

  zoom3.Text:=ConvertKeyComboToString(tempkeys.zoom3);
end;

procedure TfrmDirectX.Zoom4KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.zoom4[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.zoom4[i]=0 then
      begin
        tempkeys.zoom4[i]:=key;
        break;
      end else
      if tempkeys.zoom4[i]=key then break;
  end;

  zoom4.Text:=ConvertKeyComboToString(tempkeys.zoom4);
end;

procedure TfrmDirectX.Zoom5KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.zoom5[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.zoom5[i]=0 then
      begin
        tempkeys.zoom5[i]:=key;
        break;
      end else
      if tempkeys.zoom5[i]=key then break;
  end;

  zoom5.Text:=ConvertKeyComboToString(tempkeys.zoom5);
end;

procedure TfrmDirectX.ZoominKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.zoomin[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.zoomin[i]=0 then
      begin
        tempkeys.zoomin[i]:=key;
        break;
      end else
      if tempkeys.zoomin[i]=key then break;
  end;

  zoomin.Text:=ConvertKeyComboToString(tempkeys.zoomin);
end;

procedure TfrmDirectX.ZoomoutKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.zoomout[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.zoomout[i]=0 then
      begin
        tempkeys.zoomout[i]:=key;
        break;
      end else
      if tempkeys.zoomout[i]=key then break;
  end;

  zoomout.Text:=ConvertKeyComboToString(tempkeys.zoomout);
end;

procedure TfrmDirectX.nozoomKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.nozoom[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.nozoom[i]=0 then
      begin
        tempkeys.nozoom[i]:=key;
        break;
      end else
      if tempkeys.nozoom[i]=key then break;
  end;

  nozoom.Text:=ConvertKeyComboToString(tempkeys.nozoom);
end;


procedure TfrmDirectX.Button7Click(Sender: TObject);
begin
  zeromemory(@tempkeys.zoomin[0],10);
  zoomin.Text:=ConvertKeyComboToString(tempkeys.zoomin); //=''
  zoomin.SetFocus;
end;

procedure TfrmDirectX.Button8Click(Sender: TObject);
begin
  zeromemory(@tempkeys.zoomout[0],10);
  zoomout.Text:=ConvertKeyComboToString(tempkeys.zoomout); //=''
  zoomout.SetFocus;
end;

procedure TfrmDirectX.Button9Click(Sender: TObject);
begin
  zeromemory(@tempkeys.nozoom[0],10);
  nozoom.Text:=ConvertKeyComboToString(tempkeys.nozoom); //=''
  nozoom.SetFocus;
end;

procedure TfrmDirectX.fogKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.fog[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.fog[i]=0 then
      begin
        tempkeys.fog[i]:=key;
        break;
      end else
      if tempkeys.fog[i]=key then break;
  end;

  fog.Text:=ConvertKeyComboToString(tempkeys.fog);
end;

procedure TfrmDirectX.zbufferKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.zbuffer[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.zbuffer[i]=0 then
      begin
        tempkeys.zbuffer[i]:=key;
        break;
      end else
      if tempkeys.zbuffer[i]=key then break;
  end;

  zbuffer.Text:=ConvertKeyComboToString(tempkeys.zbuffer);
end;

procedure TfrmDirectX.LightingKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.lighting[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.lighting[i]=0 then
      begin
        tempkeys.lighting[i]:=key;
        break;
      end else
      if tempkeys.lighting[i]=key then break;
  end;

  lighting.Text:=ConvertKeyComboToString(tempkeys.lighting);
end;

procedure TfrmDirectX.Button10Click(Sender: TObject);
begin
  zeromemory(@tempkeys.fog[0],10);
  fog.Text:=ConvertKeyComboToString(tempkeys.fog); //=''
  fog.SetFocus;
end;

procedure TfrmDirectX.Button11Click(Sender: TObject);
begin
  zeromemory(@tempkeys.zbuffer[0],10);
  zbuffer.Text:=ConvertKeyComboToString(tempkeys.zbuffer); //=''
  zbuffer.SetFocus;
end;

procedure TfrmDirectX.Button12Click(Sender: TObject);
begin
  zeromemory(@tempkeys.lighting[0],10);
  lighting.Text:=ConvertKeyComboToString(tempkeys.lighting); //=''
  lighting.SetFocus;
end;

procedure TfrmDirectX.autoaimtoggleKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.autoaimtoggle[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.autoaimtoggle[i]=0 then
      begin
        tempkeys.autoaimtoggle[i]:=key;
        break;
      end else
      if tempkeys.autoaimtoggle[i]=key then break;
  end;

  autoaimtoggle.Text:=ConvertKeyComboToString(tempkeys.autoaimtoggle);
end;

procedure TfrmDirectX.Button13Click(Sender: TObject);
begin
  zeromemory(@tempkeys.autoaimtoggle[0],10);
  autoaimtoggle.Text:=ConvertKeyComboToString(tempkeys.autoaimtoggle); //=''
  autoaimtoggle.SetFocus;
end;

procedure TfrmDirectX.increaselagKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.increaselag[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.increaselag[i]=0 then
      begin
        tempkeys.increaselag[i]:=key;
        break;
      end else
      if tempkeys.increaselag[i]=key then break;
  end;

  increaselag.Text:=ConvertKeyComboToString(tempkeys.increaselag);
end;

procedure TfrmDirectX.decreaselagKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.decreaselag[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.decreaselag[i]=0 then
      begin
        tempkeys.decreaselag[i]:=key;
        break;
      end else
      if tempkeys.decreaselag[i]=key then break;
  end;

  decreaselag.Text:=ConvertKeyComboToString(tempkeys.decreaselag);
end;

procedure TfrmDirectX.Button14Click(Sender: TObject);
begin
  zeromemory(@tempkeys.increaselag[0],10);
  increaselag.Text:=ConvertKeyComboToString(tempkeys.increaselag); //=''
  increaselag.SetFocus;
end;

procedure TfrmDirectX.Button15Click(Sender: TObject);
begin
  zeromemory(@tempkeys.decreaselag[0],10);
  decreaselag.Text:=ConvertKeyComboToString(tempkeys.decreaselag); //=''
  decreaselag.SetFocus;
end;

procedure TfrmDirectX.setaimsetting1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.setaimsetting1[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.setaimsetting1[i]=0 then
      begin
        tempkeys.setaimsetting1[i]:=key;
        break;
      end else
      if tempkeys.setaimsetting1[i]=key then break;
  end;

  setaimsetting1.Text:=ConvertKeyComboToString(tempkeys.setaimsetting1);
end;

procedure TfrmDirectX.setaimsetting2KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.setaimsetting2[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.setaimsetting2[i]=0 then
      begin
        tempkeys.setaimsetting2[i]:=key;
        break;
      end else
      if tempkeys.setaimsetting2[i]=key then break;
  end;

  setaimsetting2.Text:=ConvertKeyComboToString(tempkeys.setaimsetting2);
end;

procedure TfrmDirectX.setaimsetting3KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.setaimsetting3[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.setaimsetting3[i]=0 then
      begin
        tempkeys.setaimsetting3[i]:=key;
        break;
      end else
      if tempkeys.setaimsetting3[i]=key then break;
  end;

  setaimsetting3.Text:=ConvertKeyComboToString(tempkeys.setaimsetting3);
end;

procedure TfrmDirectX.Button25Click(Sender: TObject);
begin
  zeromemory(@tempkeys.setaimsetting1[0],10);
  setaimsetting1.Text:=ConvertKeyComboToString(tempkeys.setaimsetting1); //=''
  setaimsetting1.SetFocus;
end;

procedure TfrmDirectX.Button26Click(Sender: TObject);
begin
  zeromemory(@tempkeys.setaimsetting2[0],20);
  setaimsetting2.Text:=ConvertKeyComboToString(tempkeys.setaimsetting2); //=''
  setaimsetting2.SetFocus;
end;

procedure TfrmDirectX.Button27Click(Sender: TObject);
begin
  zeromemory(@tempkeys.setaimsetting3[0],30);
  setaimsetting3.Text:=ConvertKeyComboToString(tempkeys.setaimsetting3); //=''
  setaimsetting3.SetFocus;
end;

procedure TfrmDirectX.PreviousTextureKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.previoustexture[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.previoustexture[i]=0 then
      begin
        tempkeys.previoustexture[i]:=key;
        break;
      end else
      if tempkeys.previoustexture[i]=key then break;
  end;

  previoustexture.Text:=ConvertKeyComboToString(tempkeys.previoustexture);
end;


procedure TfrmDirectX.nexttextureKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.nexttexture[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.nexttexture[i]=0 then
      begin
        tempkeys.nexttexture[i]:=key;
        break;
      end else
      if tempkeys.nexttexture[i]=key then break;
  end;

  nexttexture.Text:=ConvertKeyComboToString(tempkeys.nexttexture);
end;

procedure TfrmDirectX.locktextureKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.locktexture[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.locktexture[i]=0 then
      begin
        tempkeys.locktexture[i]:=key;
        break;
      end else
      if tempkeys.locktexture[i]=key then break;
  end;

  locktexture.Text:=ConvertKeyComboToString(tempkeys.locktexture);
end;

procedure TfrmDirectX.Button17Click(Sender: TObject);
begin
  zeromemory(@tempkeys.nexttexture[0],10);
  nexttexture.Text:=ConvertKeyComboToString(tempkeys.nexttexture); //=''
  nexttexture.SetFocus;
end;

procedure TfrmDirectX.Button18Click(Sender: TObject);
begin
  zeromemory(@tempkeys.locktexture[0],10);
  locktexture.Text:=ConvertKeyComboToString(tempkeys.locktexture); //=''
  locktexture.SetFocus;
end;

procedure TfrmDirectX.IncreaseXKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.IncreaseX[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.IncreaseX[i]=0 then
      begin
        tempkeys.IncreaseX[i]:=key;
        break;
      end else
      if tempkeys.IncreaseX[i]=key then break;
  end;

  IncreaseX.Text:=ConvertKeyComboToString(tempkeys.IncreaseX);
end;

procedure TfrmDirectX.IncreaseYKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.IncreaseY[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.IncreaseY[i]=0 then
      begin
        tempkeys.IncreaseY[i]:=key;
        break;
      end else
      if tempkeys.IncreaseY[i]=key then break;
  end;

  IncreaseY.Text:=ConvertKeyComboToString(tempkeys.IncreaseY);
end;

procedure TfrmDirectX.IncreaseZKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.IncreaseZ[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.IncreaseZ[i]=0 then
      begin
        tempkeys.IncreaseZ[i]:=key;
        break;
      end else
      if tempkeys.IncreaseZ[i]=key then break;
  end;

  IncreaseZ.Text:=ConvertKeyComboToString(tempkeys.IncreaseZ);
end;

procedure TfrmDirectX.DecreaseXKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.decreaseX[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.decreaseX[i]=0 then
      begin
        tempkeys.decreaseX[i]:=key;
        break;
      end else
      if tempkeys.decreaseX[i]=key then break;
  end;

  decreaseX.Text:=ConvertKeyComboToString(tempkeys.decreasex);
end;

procedure TfrmDirectX.DecreaseYKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.decreaseY[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.decreaseY[i]=0 then
      begin
        tempkeys.decreaseY[i]:=key;
        break;
      end else
      if tempkeys.decreaseY[i]=key then break;
  end;

  decreaseY.Text:=ConvertKeyComboToString(tempkeys.decreaseY);
end;

procedure TfrmDirectX.DecreaseZKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.decreaseZ[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.decreaseZ[i]=0 then
      begin
        tempkeys.decreaseZ[i]:=key;
        break;
      end else
      if tempkeys.decreaseZ[i]=key then break;
  end;

  decreaseZ.Text:=ConvertKeyComboToString(tempkeys.decreaseZ);
end;

procedure TfrmDirectX.Button19Click(Sender: TObject);
begin
  zeromemory(@tempkeys.increasex[0],10);
  increasex.Text:=ConvertKeyComboToString(tempkeys.increasex); //=''
  increasex.SetFocus;
end;

procedure TfrmDirectX.Button21Click(Sender: TObject);
begin
  zeromemory(@tempkeys.increaseY[0],10);
  increaseY.Text:=ConvertKeyComboToString(tempkeys.increaseY); //=''
  increaseY.SetFocus;
end;

procedure TfrmDirectX.Button23Click(Sender: TObject);
begin
  zeromemory(@tempkeys.increasez[0],10);
  increasez.Text:=ConvertKeyComboToString(tempkeys.increasez); //=''
  increasez.SetFocus;
end;

procedure TfrmDirectX.Button20Click(Sender: TObject);
begin
  zeromemory(@tempkeys.decreasex[0],10);
  decreasex.Text:=ConvertKeyComboToString(tempkeys.decreasex); //=''
  decreasex.SetFocus;
end;

procedure TfrmDirectX.Button22Click(Sender: TObject);
begin
  zeromemory(@tempkeys.decreaseY[0],10);
  decreaseY.Text:=ConvertKeyComboToString(tempkeys.decreaseY); //=''
  decreaseY.SetFocus;
end;

procedure TfrmDirectX.Button24Click(Sender: TObject);
begin
  zeromemory(@tempkeys.decreasez[0],10);
  decreasez.Text:=ConvertKeyComboToString(tempkeys.decreasez); //=''
  decreasez.SetFocus;
end;

procedure TfrmDirectX.LoadButtonClick(Sender: TObject);
type TKeysVersion1=record
  configured: boolean;
  CEDir: string[255];
  cewindow: thandle;

  callibrationmode: boolean;  //false=no textureselect hud
  callibrationkey: TKeycombo;

  setcallibration: boolean;
  mousecallibrationhorizontal1point: single;
  mousecallibrationvertical1point: single;

  mousecallibrationhorizontal2point: single;
  mousecallibrationvertical2point: single;

  mousecallibrationhorizontal5point: single;
  mousecallibrationvertical5point: single;

  mousecallibrationhorizontal10point: single;
  mousecallibrationvertical10point: single;

  mousecallibrationhorizontal20point: single;
  mousecallibrationvertical20point: single;

  mousecallibrationhorizontal40point: single;
  mousecallibrationvertical40point: single;

  loadaimsettingsfile: tkeycombo;
  saveaimsettingsfile: tkeycombo;
  aimsettings1: string[255];
  Aimsettings2: string[255];
  Aimsettings3: string[255];

  setaimsetting1: tkeycombo;
  setaimsetting2: tkeycombo;
  setaimsetting3: tkeycombo;

  nexttexture: tkeycombo;
  previoustexture: tkeycombo;
  locktexture: tkeycombo;

  IncreaseX: tkeycombo;
  DecreaseX: TKeyCombo;
  Increasey: tkeycombo;
  Decreasey: TKeyCombo;
  Increasez: tkeycombo;
  Decreasez: TKeyCombo;

  HoldAutoaimtoggle: boolean;
  autoshoot: boolean;
  autoaimtoggle: tKeycombo;
  increaselag: tkeycombo;
  decreaselag: tkeycombo;

  zoomin,zoomout: TKeyCombo;
  nozoom: tKeyCombo;
  zoom1: tKeyCombo;
  zoomlevel1: single;
  zoom2: tkeycombo;
  zoomlevel2: single;
  zoom3: tkeycombo;
  zoomlevel3: single;
  zoom4: tkeycombo;
  zoomlevel4: single;
  zoom5: tkeycombo;
  zoomlevel5: single;

  zoomdelta: single;
  lagdelta: integer;

  setlag: boolean;
  lagtoset: dword;
  usefpslag: boolean;

  rotateleft: tKeycombo;
  rotateright: tkeycombo;
  rotateup: tkeycombo;
  rotatedown: tkeycombo;
  moveleft: tkeycombo;
  moveright: tkeycombo;
  moveup: tkeycombo;
  movedown: tkeycombo;
  moveforward: tkeycombo;
  movebackwards: tkeycombo;

  movespeed: single;
  rotatespeed: single;

  setcameraback: tkeycombo;

  zbuffer: tkeycombo;
  fog: tkeycombo;
  lighting: tkeycombo;
  wireframe: tkeycombo;

  ShowKeylist: tkeycombo;

  selectedlagrecord: string[50];
  lagmemorytype: byte;
  getlagfrommemory: boolean;
  nrofoffsets: dword;
  lagaddress: dword;
  offset1: dword;
  offset2: dword;
  offset3: dword;
  offset4: dword;
  offset5: dword;
  offset6: dword;
  offset7: dword;
  offset8: dword;
  offset9: dword;
  offset10: dword;
  offset11: dword;
  offset12: dword;
  offset13: dword;
  offset14: dword;
  offset15: dword;


  pollinginterval: integer;
end;

var id: pchar;
    v: integer;
var x: tfilestream;
var oldkeys: tkeysversion1;
begin
  if opendialog1.execute then
  begin
    x:=tfilestream.Create(opendialog1.filename,fmopenread);
    try
      try
        getmem(id,7);
        try
          x.ReadBuffer(id^,6);
          id[6]:=#0;
          if id<>'DXMESS' then
          begin
            //old version
            x.Position:=0;
            x.ReadBuffer(oldkeys,sizeof(tkeysversion1));
            tempkeys.configured:=oldkeys.configured;
            tempkeys.CEDir:=oldkeys.CEDir;
            tempkeys.cewindow:=oldkeys.cewindow;
            tempkeys.callibrationmode:=oldkeys.callibrationmode;
            tempkeys.callibrationkey:=oldkeys.callibrationkey;
            tempkeys.setcallibration:=oldkeys.setcallibration;
            tempkeys.mousecallibrationhorizontal1point:=oldkeys.mousecallibrationhorizontal1point;
            tempkeys.mousecallibrationvertical1point:=oldkeys.mousecallibrationvertical1point;
            tempkeys.mousecallibrationhorizontal2point:=oldkeys.mousecallibrationhorizontal2point;
            tempkeys.mousecallibrationvertical2point:=oldkeys.mousecallibrationvertical2point;
            tempkeys.mousecallibrationhorizontal5point:=oldkeys.mousecallibrationhorizontal5point;
            tempkeys.mousecallibrationvertical5point:=oldkeys.mousecallibrationvertical5point;
            tempkeys.mousecallibrationhorizontal10point:=oldkeys.mousecallibrationhorizontal10point;
            tempkeys.mousecallibrationvertical10point:=oldkeys.mousecallibrationvertical10point;
            tempkeys.mousecallibrationhorizontal20point:=oldkeys.mousecallibrationhorizontal20point;
            tempkeys.mousecallibrationvertical20point:=oldkeys.mousecallibrationvertical20point;
            tempkeys.mousecallibrationhorizontal40point:=oldkeys.mousecallibrationhorizontal40point;
            tempkeys.mousecallibrationvertical40point:=oldkeys.mousecallibrationvertical40point;
            tempkeys.loadaimsettingsfile:=oldkeys.loadaimsettingsfile;
            tempkeys.saveaimsettingsfile:=oldkeys.saveaimsettingsfile;
            tempkeys.aimsettings1:=oldkeys.aimsettings1;
            tempkeys.Aimsettings2:=oldkeys.Aimsettings2;
            tempkeys.Aimsettings3:=oldkeys.Aimsettings3;
            tempkeys.setaimsetting1:=oldkeys.setaimsetting1;
            tempkeys.setaimsetting2:=oldkeys.setaimsetting2;
            tempkeys.setaimsetting3:=oldkeys.setaimsetting3;
            tempkeys.nexttexture:=oldkeys.nexttexture;
            tempkeys.previoustexture:=oldkeys.previoustexture;
            tempkeys.locktexture:=oldkeys.locktexture;
            tempkeys.IncreaseX:=oldkeys.IncreaseX;
            tempkeys.DecreaseX:=oldkeys.DecreaseX;
            tempkeys.IncreaseY:=oldkeys.IncreaseY;
            tempkeys.DecreaseY:=oldkeys.DecreaseY;
            tempkeys.IncreaseZ:=oldkeys.IncreaseZ;
            tempkeys.DecreaseZ:=oldkeys.DecreaseZ;
            tempkeys.HoldAutoaimtoggle:=oldkeys.HoldAutoaimtoggle;
            tempkeys.autoshoot:=oldkeys.autoshoot;
            tempkeys.autoaimtoggle:=oldkeys.autoaimtoggle;
            tempkeys.increaselag:=oldkeys.increaselag;
            tempkeys.decreaselag:=oldkeys.decreaselag;
            tempkeys.zoomin:=oldkeys.zoomin;
            tempkeys.zoomout:=oldkeys.zoomout;
            tempkeys.nozoom:=oldkeys.nozoom;
            tempkeys.zoom1:=oldkeys.zoom1;
            tempkeys.zoomlevel1:=oldkeys.zoomlevel1;
            tempkeys.zoom2:=oldkeys.zoom2;
            tempkeys.zoomlevel2:=oldkeys.zoomlevel2;
            tempkeys.zoom3:=oldkeys.zoom3;
            tempkeys.zoomlevel3:=oldkeys.zoomlevel3;
            tempkeys.zoom4:=oldkeys.zoom4;
            tempkeys.zoomlevel4:=oldkeys.zoomlevel4;
            tempkeys.zoom5:=oldkeys.zoom5;
            tempkeys.zoomlevel5:=oldkeys.zoomlevel5;
            tempkeys.zoomdelta:=oldkeys.zoomdelta;
            tempkeys.lagdelta:=oldkeys.lagdelta;
            tempkeys.setlag:=oldkeys.setlag;
            tempkeys.lagtoset:=oldkeys.lagtoset;
            tempkeys.usefpslag:=oldkeys.usefpslag;
            tempkeys.rotateleft:=oldkeys.rotateleft;
            tempkeys.rotateright:=oldkeys.rotateright;
            tempkeys.rotateup:=oldkeys.rotateup;
            tempkeys.rotatedown:=oldkeys.rotatedown;
            tempkeys.moveleft:=oldkeys.moveleft;
            tempkeys.moveright:=oldkeys.moveright;
            tempkeys.moveup:=oldkeys.moveup;
            tempkeys.movedown:=oldkeys.movedown;
            tempkeys.moveforward:=oldkeys.moveforward;
            tempkeys.movebackwards:=oldkeys.movebackwards;
            tempkeys.movespeed:=oldkeys.movespeed;
            tempkeys.rotatespeed:=oldkeys.rotatespeed;
            tempkeys.setcameraback:=oldkeys.setcameraback;
            tempkeys.zbuffer:=oldkeys.zbuffer;
            tempkeys.fog:=oldkeys.fog;
            tempkeys.lighting:=oldkeys.lighting;
            tempkeys.wireframe:=oldkeys.wireframe;
            tempkeys.ShowKeylist:=oldkeys.ShowKeylist;
            tempkeys.SaveAlltextures[0]:=0;
            tempkeys.selectedlagrecord:=oldkeys.selectedlagrecord;
            tempkeys.lagmemorytype:=oldkeys.lagmemorytype;
            tempkeys.getlagfrommemory:=oldkeys.getlagfrommemory;
            tempkeys.nrofoffsets:=oldkeys.nrofoffsets;
            tempkeys.lagaddress:=oldkeys.lagaddress;
            tempkeys.offset1:=oldkeys.offset1;
            tempkeys.offset2:=oldkeys.offset2;
            tempkeys.offset3:=oldkeys.offset3;
            tempkeys.offset4:=oldkeys.offset4;
            tempkeys.offset5:=oldkeys.offset5;
            tempkeys.offset6:=oldkeys.offset6;
            tempkeys.offset7:=oldkeys.offset7;
            tempkeys.offset8:=oldkeys.offset8;
            tempkeys.offset9:=oldkeys.offset9;
            tempkeys.offset10:=oldkeys.offset10;
            tempkeys.offset11:=oldkeys.offset11;
            tempkeys.offset12:=oldkeys.offset12;
            tempkeys.offset13:=oldkeys.offset13;
            tempkeys.offset14:=oldkeys.offset14;
            tempkeys.offset15:=oldkeys.offset15;

            tempkeys.pollinginterval:=oldkeys.pollinginterval;
          end
          else
          begin
            x.ReadBuffer(v,4);
            if v>dxmessversion then raise exception.Create('This key configuration is not compattible with this version of cheat engine');
            x.ReadBuffer(tempkeys,sizeof(tkeys));
          end;
        finally
          freemem(id);
        end;


        tempkeys.CEDir:=CheatEngineDir;
        tempkeys.cewindow:=handle;
        //and now convert what si in the tkeys to actual strings

        loadaimsettingsfile.text:=ConvertKeyComboToString(tempkeys.loadaimsettingsfile);
        saveaimsettingsfile.text:=ConvertKeyComboToString(tempkeys.saveaimsettingsfile);
        setaimsetting1.text:=ConvertKeyComboToString(tempkeys.setaimsetting1);
        setaimsetting2.text:=ConvertKeyComboToString(tempkeys.setaimsetting2);
        setaimsetting3.text:=ConvertKeyComboToString(tempkeys.setaimsetting3);
        nexttexture.text:=ConvertKeyComboToString(tempkeys.nexttexture);
        previoustexture.Text:=ConvertKeyComboToString(tempkeys.previoustexture);
        locktexture.Text:=ConvertKeyComboToString(tempkeys.locktexture);

        increasex.Text:=ConvertKeyComboToString(tempkeys.IncreaseX);
        increasey.Text:=ConvertKeyComboToString(tempkeys.increaseY);
        increasez.Text:=ConvertKeyComboToString(tempkeys.increaseZ);
        decreasex.Text:=ConvertKeyComboToString(tempkeys.decreaseX);
        decreasey.Text:=ConvertKeyComboToString(tempkeys.decreaseY);
        decreasez.Text:=ConvertKeyComboToString(tempkeys.decreaseZ);

        autoaimtoggle.Text:=ConvertKeyComboToString(tempkeys.autoaimtoggle);
        increaselag.Text:=ConvertKeyComboToString(tempkeys.increaselag);
        decreaselag.Text:=ConvertKeyComboToString(tempkeys.decreaselag);

        zoomin.Text:=ConvertKeyComboToString(tempkeys.zoomin);
        zoomout.Text:=ConvertKeyComboToString(tempkeys.zoomout);
        nozoom.Text:=ConvertKeyComboToString(tempkeys.nozoom);

        zoom1.Text:=ConvertKeyComboToString(tempkeys.zoom1);
        zoom2.Text:=ConvertKeyComboToString(tempkeys.zoom2);
        zoom3.Text:=ConvertKeyComboToString(tempkeys.zoom3);
        zoom4.Text:=ConvertKeyComboToString(tempkeys.zoom4);
        zoom5.Text:=ConvertKeyComboToString(tempkeys.zoom5);

        zbuffer.text:=ConvertKeyComboToString(tempkeys.zbuffer);
        fog.text:=ConvertKeyComboToString(tempkeys.fog);
        lighting.text:=ConvertKeyComboToString(tempkeys.lighting);
        wireframe.Text:=ConvertKeycomboToString(tempkeys.wireframe);
        showkeylist.Text:=ConvertKeyComboToString(tempkeys.ShowKeylist);

        savealltextures.Text:=convertkeycombotostring(tempkeys.SaveAlltextures);

        callibrationkey.Text:=ConvertKeyComboToString(tempkeys.callibrationkey);


        checkbox2.Checked:=tempkeys.callibrationmode;
        zoomlevel1.text:=floattostr(tempkeys.zoomlevel1);
        zoomlevel2.Text:=floattostr(tempkeys.zoomlevel2);
        zoomlevel3.Text:=floattostr(tempkeys.zoomlevel3);
        zoomlevel4.Text:=floattostr(tempkeys.zoomlevel4);
        zoomlevel5.Text:=floattostr(tempkeys.zoomlevel5);

        zoomdelta.Text:=floattostr(tempkeys.zoomdelta);

        lagdelta.Text:=IntToStr(tempkeys.lagdelta);
        lag.Text:=inttostr(tempkeys.lagtoset);

        editkeypolling.Text:=IntToStr(tempkeys.pollinginterval);


        aimfile1.Text:=tempkeys.aimsettings1;
        aimfile2.Text:=tempkeys.aimsettings2;
        aimfile3.Text:=tempkeys.aimsettings3;


        mousecallibrationhorizontal1point.text:=format('%.2f',[tempkeys.mousecallibrationhorizontal1point]);
        mousecallibrationhorizontal2point.text:=format('%.2f',[tempkeys.mousecallibrationhorizontal2point]);
        mousecallibrationhorizontal5point.text:=format('%.2f',[tempkeys.mousecallibrationhorizontal5point]);
        mousecallibrationhorizontal10point.text:=format('%.2f',[tempkeys.mousecallibrationhorizontal10point]);
        mousecallibrationhorizontal20point.text:=format('%.2f',[tempkeys.mousecallibrationhorizontal20point]);
        mousecallibrationhorizontal40point.text:=format('%.2f',[tempkeys.mousecallibrationhorizontal40point]);

        mousecallibrationvertical1point.text:=format('%.2f',[tempkeys.mousecallibrationvertical1point]);
        mousecallibrationvertical2point.text:=format('%.2f',[tempkeys.mousecallibrationvertical2point]);
        mousecallibrationvertical5point.text:=format('%.2f',[tempkeys.mousecallibrationvertical5point]);
        mousecallibrationvertical10point.text:=format('%.2f',[tempkeys.mousecallibrationvertical10point]);
        mousecallibrationvertical20point.text:=format('%.2f',[tempkeys.mousecallibrationvertical20point]);
        mousecallibrationvertical40point.text:=format('%.2f',[tempkeys.mousecallibrationvertical40point]);

        rbkeepdown.checked:=tempkeys.HoldAutoaimtoggle;
        autoshoot.checked:=tempkeys.autoshoot;
        getlagfrommemory.Checked:=tempkeys.getlagfrommemory;
        addresslabel.Caption:=tempkeys.selectedlagrecord;
        usefpslag.Checked:=tempkeys.usefpslag;

      finally
        x.free;
      end;
    except
      raise exception.Create('Error while loading! (and I''m not going to tell you why!)');
    end;
  end;
end;

procedure TfrmDirectX.SaveButtonClick(Sender: TObject);
var x: tfilestream;
    y: integer;
    z: string;
begin
  applytempkeys;
  if savedialog1.execute then
  begin
    x:=tfilestream.Create(savedialog1.filename,fmcreate);
    try
      try
        applytempkeys;
        y:=dxmessversion;
        z:='DXMESS';
        x.writebuffer(z[1],6);
        x.writebuffer(y,4);
        x.WriteBuffer(tempkeys,sizeof(TKeys));
      finally
        x.free;
      end;
      except
        raise exception.Create('Error while saving! (and I''m not going to tell you why!)');
      end;
  end;
end;

procedure Tfrmdirectx.ApplyTempkeys;
begin
  tempkeys.configured:=true;
  tempkeys.CEDir:=CheatEngineDir;
  tempkeys.cewindow:=handle;

  tempkeys.zoomlevel1:=StrToFloat(zoomlevel1.text);
  tempkeys.zoomlevel2:=StrToFloat(zoomlevel2.text);
  tempkeys.zoomlevel3:=StrToFloat(zoomlevel3.text);
  tempkeys.zoomlevel4:=StrToFloat(zoomlevel4.text);
  tempkeys.zoomlevel5:=StrToFloat(zoomlevel5.text);

  tempkeys.zoomdelta:=StrToFloat(zoomdelta.Text);

  tempkeys.lagdelta:=StrToInt(lagdelta.text);
  tempkeys.lagtoset:=strtoint(lag.text);
  tempkeys.setlag:=true;

  tempkeys.pollinginterval:=StrToInt(editkeypolling.text);
  if tempkeys.pollinginterval<0 then raise exception.create('Don''t set the key polling interval to a negative value. I havn''t figured out how to implement quantum mechanics in CE. (Yet!)');

  tempkeys.aimsettings1:=aimfile1.text;
  tempkeys.Aimsettings2:=aimfile2.Text;
  tempkeys.aimsettings3:=aimfile3.Text;

  tempkeys.callibrationmode:=checkbox2.checked;

  tempkeys.setcallibration:=true;
  tempkeys.mousecallibrationhorizontal1point:=strtofloat(mousecallibrationhorizontal1point.Text);
  tempkeys.mousecallibrationhorizontal2point:=strtofloat(mousecallibrationhorizontal2point.Text);
  tempkeys.mousecallibrationhorizontal5point:=strtofloat(mousecallibrationhorizontal5point.Text);
  tempkeys.mousecallibrationhorizontal10point:=strtofloat(mousecallibrationhorizontal10point.Text);
  tempkeys.mousecallibrationhorizontal20point:=strtofloat(mousecallibrationhorizontal20point.Text);
  tempkeys.mousecallibrationhorizontal40point:=strtofloat(mousecallibrationhorizontal40point.Text);

  tempkeys.mousecallibrationvertical1point:=strtofloat(mousecallibrationvertical1point.Text);
  tempkeys.mousecallibrationvertical2point:=strtofloat(mousecallibrationvertical2point.Text);
  tempkeys.mousecallibrationvertical5point:=strtofloat(mousecallibrationvertical5point.Text);
  tempkeys.mousecallibrationvertical10point:=strtofloat(mousecallibrationvertical10point.Text);
  tempkeys.mousecallibrationvertical20point:=strtofloat(mousecallibrationvertical20point.Text);
  tempkeys.mousecallibrationvertical40point:=strtofloat(mousecallibrationvertical40point.Text);

  tempkeys.HoldAutoaimtoggle:=rbkeepdown.Checked;
  tempkeys.autoshoot:=autoshoot.Checked;
  tempkeys.usefpslag:=usefpslag.checked;
end;

procedure TfrmDirectX.Button28Click(Sender: TObject);
begin
  zeromemory(@tempkeys.loadaimsettingsfile[0],30);
  loadaimsettingsfile.Text:=ConvertKeyComboToString(tempkeys.loadaimsettingsfile); //=''
  loadaimsettingsfile.SetFocus
end;

procedure TfrmDirectX.Button29Click(Sender: TObject);
begin
  zeromemory(@tempkeys.saveaimsettingsfile[0],30);
  saveaimsettingsfile.Text:=ConvertKeyComboToString(tempkeys.saveaimsettingsfile); //=''
  saveaimsettingsfile.SetFocus
end;

procedure TfrmDirectX.loadaimsettingsfileKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var i: integer;
begin
  if tempkeys.loadaimsettingsfile[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.loadaimsettingsfile[i]=0 then
      begin
        tempkeys.loadaimsettingsfile[i]:=key;
        break;
      end else
      if tempkeys.loadaimsettingsfile[i]=key then break;
  end;

  loadaimsettingsfile.Text:=ConvertKeyComboToString(tempkeys.loadaimsettingsfile);
end;

procedure TfrmDirectX.SaveaimsettingsfileKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var i: integer;
begin
  if tempkeys.saveaimsettingsfile[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.saveaimsettingsfile[i]=0 then
      begin
        tempkeys.saveaimsettingsfile[i]:=key;
        break;
      end else
      if tempkeys.saveaimsettingsfile[i]=key then break;
  end;

  saveaimsettingsfile.Text:=ConvertKeyComboToString(tempkeys.saveaimsettingsfile);
end;


procedure TfrmDirectX.Button16Click(Sender: TObject);
begin
  zeromemory(@tempkeys.previoustexture[0],10);
  previoustexture.Text:=ConvertKeyComboToString(tempkeys.previoustexture); //=''
  previoustexture.SetFocus;
end;

procedure TfrmDirectX.Button30Click(Sender: TObject);
begin
  zeromemory(@tempkeys.callibrationkey[0],10);
  callibrationkey.Text:=ConvertKeyComboToString(tempkeys.callibrationkey); //=''
  callibrationkey.SetFocus;
end;

procedure TfrmDirectX.callibrationkeyKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var i: integer;
begin
  if tempkeys.callibrationkey[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.callibrationkey[i]=0 then
      begin
        tempkeys.callibrationkey[i]:=key;
        break;
      end else
      if tempkeys.callibrationkey[i]=key then break;
  end;

  callibrationkey.Text:=ConvertKeyComboToString(tempkeys.callibrationkey);
end;

procedure TfrmDirectX.FormShow(Sender: TObject);
begin
  keys.cewindow:=handle;
end;

procedure TfrmDirectX.rbtoggleoneffClick(Sender: TObject);
begin
  if rbkeepdown.Checked then autoshoot.Enabled:=true else
  begin
    autoshoot.Checked:=false;
    autoshoot.Enabled:=false;
  end;
end;

procedure TfrmDirectX.Button31Click(Sender: TObject);
var addresspicker: tform;
    addresslist: tlistbox;
    i: integer;
begin
  addresspicker:=tform.create(self);
  try
    with addresspicker do
    begin
      width:=250;
      height:=200;
      position:=poscreencenter;
      caption:='Select the address';
      borderstyle:=bsToolwindow;

      addresslist:=tlistbox.Create(addresspicker);
      with addresslist do
      begin
        addresslist.Width:=addresspicker.clientwidth;
        addresslist.Height:=addresspicker.clientheight-30;

        addresslist.Parent:=addresspicker;
        for i:=0 to mainform.numberofrecords-1 do
          items.Add(mainform.memrec[i].Description);

      end;

      with tbutton.create(self) do
      begin
        top:=addresspicker.clientheight-27;
        left:=(addresspicker.ClientWidth div 2)-(width div 2);
        caption:='OK';
        modalresult:=mrok;
        parent:=addresspicker;
      end;

      if addresspicker.ShowModal=mrok then
      begin
        i:=addresslist.ItemIndex;
        if i=-1 then exit;

        if mainform.memrec[i].VarType in [5,7,8] then raise exception.Create('This type can''t be used');
        tempkeys.lagmemorytype:=mainform.memrec[i].VarType;
        tempkeys.nrofoffsets:=0;        
        tempkeys.lagaddress:=mainform.memrec[i].Address;
        if mainform.memrec[i].IsPointer then
        begin
          tempkeys.nrofoffsets:=length(mainform.memrec[i].pointers);
          tempkeys.lagaddress:=mainform.memrec[i].pointers[length(mainform.memrec[i].pointers)-1].Address;
          if tempkeys.nrofoffsets>=1 then tempkeys.offset1:=mainform.memrec[i].pointers[0].offset;
          if tempkeys.nrofoffsets>=2 then tempkeys.offset2:=mainform.memrec[i].pointers[1].offset;
          if tempkeys.nrofoffsets>=3 then tempkeys.offset3:=mainform.memrec[i].pointers[2].offset;
          if tempkeys.nrofoffsets>=4 then tempkeys.offset4:=mainform.memrec[i].pointers[3].offset;
          if tempkeys.nrofoffsets>=5 then tempkeys.offset5:=mainform.memrec[i].pointers[4].offset;
          if tempkeys.nrofoffsets>=6 then tempkeys.offset6:=mainform.memrec[i].pointers[5].offset;
          if tempkeys.nrofoffsets>=7 then tempkeys.offset7:=mainform.memrec[i].pointers[6].offset;
          if tempkeys.nrofoffsets>=8 then tempkeys.offset8:=mainform.memrec[i].pointers[7].offset;
          if tempkeys.nrofoffsets>=9 then tempkeys.offset9:=mainform.memrec[i].pointers[8].offset;
          if tempkeys.nrofoffsets>=10 then tempkeys.offset10:=mainform.memrec[i].pointers[9].offset;
          if tempkeys.nrofoffsets>=11 then tempkeys.offset11:=mainform.memrec[i].pointers[10].offset;
          if tempkeys.nrofoffsets>=12 then tempkeys.offset12:=mainform.memrec[i].pointers[11].offset;
          if tempkeys.nrofoffsets>=13 then tempkeys.offset13:=mainform.memrec[i].pointers[12].offset;
          if tempkeys.nrofoffsets>=14 then tempkeys.offset14:=mainform.memrec[i].pointers[13].offset;
          if tempkeys.nrofoffsets=15 then tempkeys.offset15:=mainform.memrec[i].pointers[14].offset;

          if tempkeys.nrofoffsets>15 then
          begin
            tempkeys.nrofoffsets:=0;
            raise exception.Create('A pointer address may not have more than 15 offsets');
          end;
        end;


      end;

      tempkeys.selectedlagrecord:=mainform.memrec[i].Description;
      addresslabel.Caption:=mainform.memrec[i].Description;
    end;
  finally
    addresspicker.Free;
  end;
end;

procedure TfrmDirectX.getlagfrommemoryClick(Sender: TObject);
begin
  tempkeys.getlagfrommemory:=getlagfrommemory.checked;

  button31.enabled:=getlagfrommemory.checked;
  addresslabel.Visible:=getlagfrommemory.checked;
end;

procedure TfrmDirectX.CheckBox2Click(Sender: TObject);
begin
  if checkbox2.checked then
  begin
    label50.Enabled:=true;
    saveaimsettingsfile.Enabled:=true;
    button29.Enabled:=true;

    label34.Enabled:=true;
    previoustexture.Enabled:=true;
    button16.Enabled:=true;

    label35.Enabled:=true;
    nexttexture.Enabled:=true;
    button17.Enabled:=true;

    label36.Enabled:=true;
    locktexture.Enabled:=true;
    button18.Enabled:=true;

    label37.Enabled:=true;
    label38.Enabled:=true;
    label39.Enabled:=true;
    label40.Enabled:=true;
    label41.Enabled:=true;
    label42.Enabled:=true;

    button19.enabled:=true;
    button20.enabled:=true;
    button21.enabled:=true;
    button22.enabled:=true;
    button23.enabled:=true;
    button24.Enabled:=true;

    increasex.Enabled:=true;
    increasey.Enabled:=true;
    increasez.Enabled:=true;
    decreasex.Enabled:=true;
    decreasey.Enabled:=true;
    decreasez.Enabled:=true;

    label51.Enabled:=true;
    callibrationkey.Enabled:=true;
    button30.Enabled:=true;
  end
  else
  begin
    label50.Enabled:=false;
    saveaimsettingsfile.Enabled:=false;
    button29.Enabled:=false;

    label34.Enabled:=false;
    previoustexture.Enabled:=false;
    button16.Enabled:=false;

    label35.Enabled:=false;
    nexttexture.Enabled:=false;
    button17.Enabled:=false;

    label36.Enabled:=false;
    locktexture.Enabled:=false;
    button18.Enabled:=false;

    label37.Enabled:=false;
    label38.Enabled:=false;
    label39.Enabled:=false;
    label40.Enabled:=false;
    label41.Enabled:=false;
    label42.Enabled:=false;

    button19.enabled:=false;
    button20.enabled:=false;
    button21.enabled:=false;
    button22.enabled:=false;
    button23.enabled:=false;
    button24.Enabled:=false;

    increasex.Enabled:=false;
    increasey.Enabled:=false;
    increasez.Enabled:=false;
    decreasex.Enabled:=false;
    decreasey.Enabled:=false;
    decreasez.Enabled:=false;

    label51.Enabled:=false;
    callibrationkey.Enabled:=false;
    button30.Enabled:=false;
  end;
end;

procedure TfrmDirectX.Button32Click(Sender: TObject);
begin
  zeromemory(@tempkeys.wireframe[0],10);
  wireframe.Text:=ConvertKeyComboToString(tempkeys.wireframe); //=''
  wireframe.SetFocus;
end;

procedure TfrmDirectX.WireframeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.wireframe[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.wireframe[i]=0 then
      begin
        tempkeys.wireframe[i]:=key;
        break;
      end else
      if tempkeys.wireframe[i]=key then break;
  end;

  wireframe.Text:=ConvertKeyComboToString(tempkeys.wireframe);
end;

procedure TfrmDirectX.ShowKeyListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.ShowKeyList[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.ShowKeyList[i]=0 then
      begin
        tempkeys.ShowKeyList[i]:=key;
        break;
      end else
      if tempkeys.ShowKeyList[i]=key then break;
  end;

  ShowKeyList.Text:=ConvertKeyComboToString(tempkeys.ShowKeyList);
end;

procedure TfrmDirectX.Button33Click(Sender: TObject);
begin
  zeromemory(@tempkeys.ShowKeyList[0],10);
  ShowKeyList.Text:=ConvertKeyComboToString(tempkeys.ShowKeyList); //=''
  ShowKeyList.SetFocus;
end;

procedure TfrmDirectX.Button34Click(Sender: TObject);
begin
  zeromemory(@tempkeys.SaveallTextures[0],10);
  SaveallTextures.Text:=ConvertKeyComboToString(tempkeys.SaveallTextures); //=''
  SaveallTextures.SetFocus;
end;

procedure TfrmDirectX.SaveAllTexturesKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var i: integer;
begin
  if tempkeys.savealltextures[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.savealltextures[i]=0 then
      begin
        tempkeys.savealltextures[i]:=key;
        break;
      end else
      if tempkeys.savealltextures[i]=key then break;
  end;

  savealltextures.Text:=ConvertKeyComboToString(tempkeys.savealltextures);
end;

procedure tfrmdirectx.setkey(key:word; var tempkeysitem: tkeycombo;ed:tedit);
var i: integer;
begin
  if tempkeysitem[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeysitem[i]=0 then
      begin
        tempkeysitem[i]:=key;
        break;
      end else
      if tempkeysitem[i]=key then break;
  end;

  ed.Text:=ConvertKeyComboToString(tempkeysitem);
end;

procedure TfrmDirectX.LeftMouse1Click(Sender: TObject);
var key: word;
begin
  case (sender as tmenuitem).Tag of
    0: key:=vk_lbutton;
    1: key:=vk_mbutton;
    2: key:=vk_rbutton;
  end;
  if zoom1.Focused then setkey(key,tempkeys.zoom1,zoom1);
  if zoom2.focused then setkey(key,tempkeys.zoom2,zoom2);
  if zoom3.focused then setkey(key,tempkeys.zoom3,zoom3);
  if zoom4.focused then setkey(key,tempkeys.zoom4,zoom4);
  if zoom5.focused then setkey(key,tempkeys.zoom5,zoom5);

  if zoomin.focused then setkey(key,tempkeys.zoomin,zoomin);
  if zoomout.focused then setkey(key,tempkeys.zoomout,zoomout);
  if nozoom.focused then setkey(key,tempkeys.nozoom,nozoom);

  if fog.focused then setkey(key,tempkeys.fog,fog);
  if zbuffer.focused then setkey(key,tempkeys.zbuffer,zbuffer);
  if lighting.focused then setkey(key,tempkeys.lighting,lighting);
  if wireframe.focused then setkey(key,tempkeys.wireframe,wireframe);
  if showkeylist.focused then setkey(key,tempkeys.showkeylist,showkeylist);

  if saveAllTextures.focused then setkey(key,tempkeys.savealltextures,savealltextures);
  if setaimsetting1.focused then setkey(key,tempkeys.setaimsetting1,setaimsetting1);
  if setaimsetting2.focused then setkey(key,tempkeys.setaimsetting2,setaimsetting2);
  if setaimsetting3.focused then setkey(key,tempkeys.setaimsetting3,setaimsetting3);

  if loadaimsettingsfile.focused then setkey(key,tempkeys.loadaimsettingsfile,loadaimsettingsfile);
  if saveaimsettingsfile.focused then setkey(key,tempkeys.saveaimsettingsfile,saveaimsettingsfile);

  if autoaimtoggle.focused then setkey(key,tempkeys.autoaimtoggle,autoaimtoggle);

  if increaselag.focused then setkey(key,tempkeys.increaselag,increaselag);
  if decreaselag.focused then setkey(key,tempkeys.decreaselag,decreaselag);

  if previoustexture.focused then setkey(key,tempkeys.previoustexture,previoustexture);
  if nexttexture.focused then setkey(key,tempkeys.nexttexture,nexttexture);
  if locktexture.focused then setkey(key,tempkeys.locktexture,locktexture);

  if increasex.focused then setkey(key,tempkeys.increasex,increasex);
  if increasey.Focused then setkey(key,tempkeys.increasey,increasey);
  if increasez.Focused then setkey(key,tempkeys.increasez,increasez);
  if decreasex.focused then setkey(key,tempkeys.decreasex,increasex);
  if decreasey.Focused then setkey(key,tempkeys.decreasey,increasey);
  if decreasez.Focused then setkey(key,tempkeys.decreasez,increasez);

  if callibrationkey.Focused then setkey(key,tempkeys.callibrationkey,callibrationkey);

end;

end.
