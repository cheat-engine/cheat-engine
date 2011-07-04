unit frmOpenGLUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ComCtrls, StdCtrls, ExtCtrls,cefuncproc;

type
  TfrmOpenGL = class(TForm)
    Panel2: TPanel;
    Label21: TLabel;
    Label22: TLabel;
    Button6: TButton;
    editKeyPolling: TEdit;
    ScrollBox1: TScrollBox;
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
    Label27: TLabel;
    Label24: TLabel;
    Label33: TLabel;
    textures: TEdit;
    Button10: TButton;
    Lighting: TEdit;
    Button12: TButton;
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
    Panel1: TPanel;
    Label1: TLabel;
    LoadButton: TSpeedButton;
    SaveButton: TSpeedButton;
    Label26: TLabel;
    DepthTest: TEdit;
    Button11: TButton;
    Label28: TLabel;
    fog: TEdit;
    Button13: TButton;
    procedure FormCreate(Sender: TObject);
    procedure texturesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button10Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure LightingKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DepthTestKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GroupBox2Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure fogKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button13Click(Sender: TObject);
  private
    { Private declarations }
    keys: pkeys2;
    tempkeys: tkeys2;
    procedure ApplyTempKeys;
  public
    { Public declarations }
  end;

var
  frmOpenGL: TfrmOpenGL;

implementation

{$R *.dfm}

procedure TfrmOpenGL.ApplyTempkeys;
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
  tempkeys.pollinginterval:=StrToInt(editkeypolling.text);
end;


procedure TfrmOpenGL.FormCreate(Sender: TObject);
begin
  keys:=keys2; 
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


  keys.pollinginterval:=250;

  tempkeys:=keys^;
end;

procedure TfrmOpenGL.texturesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.textures[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.textures[i]=0 then
      begin
        tempkeys.textures[i]:=key;
        break;
      end else
      if tempkeys.textures[i]=key then break;
  end;

  textures.Text:=ConvertKeyComboToString(tempkeys.textures);
end;

procedure TfrmOpenGL.Button10Click(Sender: TObject);
begin
  zeromemory(@tempkeys.textures[0],10);
  textures.Text:=ConvertKeyComboToString(tempkeys.textures); //=''
  textures.SetFocus;
end;

procedure TfrmOpenGL.Button6Click(Sender: TObject);
begin
  applytempkeys;
  keys^:=tempkeys;
end;

procedure TfrmOpenGL.LightingKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.Lighting[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.Lighting[i]=0 then
      begin
        tempkeys.Lighting[i]:=key;
        break;
      end else
      if tempkeys.Lighting[i]=key then break;
  end;

  Lighting.Text:=ConvertKeyComboToString(tempkeys.Lighting);
end;


procedure TfrmOpenGL.DepthTestKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if tempkeys.DepthTest[4]=0 then
  begin
    for i:=0 to 4 do
      if tempkeys.DepthTest[i]=0 then
      begin
        tempkeys.DepthTest[i]:=key;
        break;
      end else
      if tempkeys.DepthTest[i]=key then break;
  end;

  DepthTest.Text:=ConvertKeyComboToString(tempkeys.DepthTest);
end;

procedure TfrmOpenGL.GroupBox2Click(Sender: TObject);
begin
  zeromemory(@tempkeys.Lighting[0],10);
  Lighting.Text:=ConvertKeyComboToString(tempkeys.Lighting); //=''
  Lighting.SetFocus;
end;

procedure TfrmOpenGL.Button11Click(Sender: TObject);
begin
  zeromemory(@tempkeys.DepthTest[0],10);
  DepthTest.Text:=ConvertKeyComboToString(tempkeys.DepthTest); //=''
  DepthTest.SetFocus;
end;

procedure TfrmOpenGL.fogKeyDown(Sender: TObject; var Key: Word;
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


procedure TfrmOpenGL.Button13Click(Sender: TObject);
begin
  zeromemory(@tempkeys.fog[0],10);
  fog.Text:=ConvertKeyComboToString(tempkeys.fog); //=''
  fog.SetFocus;
end;

end.
