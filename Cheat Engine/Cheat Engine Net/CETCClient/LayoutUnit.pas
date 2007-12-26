unit LayoutUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ExtDlgs, Buttons, Menus,unit2;



type
  TLayout = class(TForm)
    OpenPictureDialog1: TOpenPictureDialog;
    GroupBox1: TGroupBox;
    Backgroundimage: TImage;
    ColorDialog1: TColorDialog;
    GroupBox2: TGroupBox;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    Button6: TButton;
    Label2: TLabel;
    Button7: TButton;
    Processnamecolor: TLabel;
    Edit1: TEdit;
    SpeedButton1: TSpeedButton;
    Normaltextcolor: TLabel;
    SpeedButton2: TSpeedButton;
    Edit2: TEdit;
    ShowHelp: TCheckBox;
    MainMenu1: TMainMenu;
    Load1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Saveas1: TMenuItem;
    N1: TMenuItem;
    Close1: TMenuItem;
    Label4: TLabel;
    Edit4: TEdit;
    SpeedButton4: TSpeedButton;
    Panel1: TPanel;
    Panel4: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Panel3: TPanel;
    Edit3: TEdit;
    SpeedButton3: TSpeedButton;
    Label3: TLabel;
    Edit7: TEdit;
    SpeedButton7: TSpeedButton;
    Panel7: TPanel;
    Label7: TLabel;
    SpeedButton8: TSpeedButton;
    Edit8: TEdit;
    Label8: TLabel;
    Panel8: TPanel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button2: TButton;
    Edit5: TEdit;
    SpeedButton5: TSpeedButton;
    Panel5: TPanel;
    Label5: TLabel;
    Saveasdefault1: TMenuItem;
    procedure Button7Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Edit1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure BackgroundimageStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure BackgroundimageEndDrag(Sender, Target: TObject; X,
      Y: Integer);
    procedure Button6Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure Edit7Change(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure Edit8Change(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Saveas1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShowHelpClick(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Saveasdefault1Click(Sender: TObject);
  private
    { Private declarations }
    X2,Y2: Integer;
    DragColor: Dword;
    Changed: Boolean;
    tempskin: TSkin;
    Procedure ApplySettings;
    procedure SaveSkin(filename: String);

  public
    { Public declarations }
    memimage2: TMemorystream;
    procedure Openskin(filename: string);
  end;

var
  Layout: TLayout;

implementation

{$R *.dfm}

procedure TLayout.Openskin(filename: String);
var skinfile: Tfilestream;
    newskin: TSkin;
begin
    skinfile:=TFilestream.Create(FileName,fmOpenRead);
    skinfile.ReadBuffer(newskin,sizeof(newskin));

    mainform.memimage.CopyFrom(skinfile,skinfile.Size-skinfile.Position);
    mainform.memimage.Seek(0,soFromBeginning);

    backgroundimage.Picture.Bitmap.LoadFromStream(mainform.memimage);
    backgroundimage.visible:=newskin.backgroundimage;
    skinfile.free;
end;

procedure TLayout.SaveSkin(filename: String);
var newskin: unit2.TSkin;
    skinfile: TFileStream;
begin
  skinfile:=TFilestream.Create(filename,fmCreate);

  newskin:=tempskin;
  newskin.Marker:='CES1.0';
  skinfile.WriteBuffer(newskin,sizeof(newskin));
  if newskin.backgroundimage then backgroundimage.Picture.Bitmap.SaveToStream(skinfile);
  skinfile.free;
end;



procedure TLayout.ApplySettings;
//var Color: Tcolor;
begin
//
end;

procedure TLayout.Button7Click(Sender: TObject);
var a: String;
begin
  a:=OpenPictureDialog1.FileName;
  if OpenPictureDialog1.Execute then
  begin
    if uppercase(extractfileext(openpicturedialog1.FileName))<>'.BMP' then raise exception.Create('You may only load BMP files');
    if fileexists(OpenPictureDialog1.FileName) then
    begin
      backgroundimage.Picture.LoadFromFile(OpenPictureDialog1.FileName);
      Backgroundimage.visible:=true;
      tempskin.backgroundimage:=true;
      changed:=true;
    end else openpicturedialog1.FileName:=a;
  end;
end;

procedure TLayout.SpeedButton1Click(Sender: TObject);
begin
  ColorDialog1.color:=Panel1.Color;
  if Colordialog1.Execute then
  begin
    edit1.text:=IntTohex(Colordialog1.color,6);
  end;
end;

procedure TLayout.Edit1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  If Source is TImage then Accept:=true else Accept:=false;
end;

procedure TLayout.BackgroundimageStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var q: TCanvas;
    r: hDC;
begin
  x2:=mouse.cursorpos.X;
  y2:=mouse.CursorPos.Y;

  r:=getDC(0);
  q:=TCanvas.Create;
  q.Handle:=r;
  dragcolor:=q.Pixels[x2,y2];

  q.free;
  releasedc(0,r);

end;

procedure TLayout.BackgroundimageEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  if (Target is TEdit) then
    (Target as TEdit).Text:=IntToHex(dragcolor,6);
end;

procedure TLayout.Button6Click(Sender: TObject);
begin
  tempskin.backgroundimage:=false;
  Backgroundimage.visible:=false;
end;

procedure TLayout.Button3Click(Sender: TObject);
begin
  Changed:=false;
  Close;
end;

procedure TLayout.Button4Click(Sender: TObject);
begin
  Applysettings;
  Close;
end;

procedure TLayout.SpeedButton2Click(Sender: TObject);
begin
  ColorDialog1.color:=panel2.color;
  if Colordialog1.Execute then
  begin
    edit2.Text:=IntTohex(Colordialog1.color,6);
  end;
end;

procedure TLayout.FormShow(Sender: TObject);
begin
  tempskin:=mainform.Skin;

  Edit1.Text:=IntToHex(Mainform.Skin.ProcessTextColor,6);
  Panel1.color:=Mainform.Skin.ProcessTextColor;

  Edit2.Text:=IntToHex(Mainform.Skin.Normaltextcolor,6);
  Panel2.color:=MainForm.Skin.Normaltextcolor;

  Edit4.Text:=IntToHex(Mainform.Skin.Backgroundcolor,6);
  panel4.color:=Mainform.skin.Backgroundcolor;

  Edit3.Text:=IntToHex(Mainform.Skin.GroupBoxcolor,6);
  Panel3.color:=Mainform.Skin.Groupboxcolor;

  Edit7.Text:=IntToHex(Mainform.Skin.Textfieldcolor,6);
  Panel7.Color:=Mainform.Skin.Textfieldcolor;

  Edit8.Text:=IntToHex(Mainform.Skin.Textfieldbackgroundcolor,6);
  Panel8.Color:=Mainform.Skin.Textfieldbackgroundcolor;

  edit5.Text:=IntToHex(Mainform.Skin.Selectedrecordcolor,6);
  panel5.color:=Mainform.Skin.Selectedrecordcolor;

  showhelp.checked:=mainform.skin.showHelp;

end;

procedure TLayout.Close1Click(Sender: TObject);
begin
  Close;
end;

procedure TLayout.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var res: Integer;
begin
  if changed then
  begin
    res:=MessageDlg('Apply your changes?',mtConfirmation, [mbYes,mbNo,mbCancel], 0);
    if res=mrCancel then
    begin
      canclose:=false;
      exit;
    end;

    if res=mrYes then applysettings;

    changed:=false;

    canclose:=true;
  end;

end;

procedure TLayout.SpeedButton3Click(Sender: TObject);
begin
  ColorDialog1.color:=panel3.color;
  if Colordialog1.Execute then
  begin
    edit3.Text:=IntTohex(Colordialog1.color,6);
  end;
end;

procedure TLayout.SpeedButton4Click(Sender: TObject);
begin
  ColorDialog1.color:=panel4.color;
  if Colordialog1.Execute then
  begin
    edit4.Text:=IntTohex(Colordialog1.color,6);
  end;
end;

procedure TLayout.Edit1Change(Sender: TObject);
var i: Integer;
    c: Dword;
begin
  val('$'+edit1.text,c,i);
  if i=0 then panel1.Color:=c;
  Changed:=true;

  tempskin.ProcessTextColor:=panel1.color;
end;

procedure TLayout.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  case key of
    chr(65)..chr(70) : ;
    chr(97)..chr(102) : ;
    chr(8)        : ;
    chr(16)       : ;
    chr(48)..chr(57) : ;
    else key:=chr(0);
  end;
end;

procedure TLayout.Edit2Change(Sender: TObject);
var i: Integer;
    c: Dword;
begin
  val('$'+edit2.text,c,i);
  if i=0 then panel2.Color:=c;
  Changed:=true;
  tempskin.Normaltextcolor:=panel2.color; 
end;

procedure TLayout.Edit3Change(Sender: TObject);
var i: Integer;
    c: Dword;

begin
  val('$'+edit3.text,c,i);
  if i=0 then panel3.Color:=c;
  Changed:=true;
  tempskin.Groupboxcolor:=panel3.color;
end;

procedure TLayout.Edit4Change(Sender: TObject);
var i: Integer;
    c: Dword;
begin
  val('$'+edit4.text,c,i);
  if i=0 then panel4.Color:=c;
  Changed:=true;
  tempskin.Backgroundcolor:=panel4.color;
end;

procedure TLayout.Button1Click(Sender: TObject);
begin
  Applysettings;
end;

procedure TLayout.SpeedButton7Click(Sender: TObject);
begin
  ColorDialog1.color:=panel7.color;
  if Colordialog1.Execute then
  begin
    edit7.Text:=IntTohex(Colordialog1.color,6);
  end;
end;

procedure TLayout.Edit7Change(Sender: TObject);
var i: Integer;
    c: Dword;
begin
  val('$'+edit7.text,c,i);
  if i=0 then panel7.Color:=c;
  Changed:=true;
  tempskin.Textfieldcolor:=panel7.color;
end;

procedure TLayout.SpeedButton8Click(Sender: TObject);
begin
  ColorDialog1.color:=panel8.color;
  if Colordialog1.Execute then
  begin
    edit8.Text:=IntTohex(Colordialog1.color,6);
  end;
end;

procedure TLayout.Edit8Change(Sender: TObject);
var i: Integer;
    c: Dword;
begin
  val('$'+edit8.text,c,i);
  if i=0 then panel8.Color:=c;
  Changed:=true;
  tempskin.Textfieldbackgroundcolor:=panel8.Color;
end;

procedure TLayout.Save1Click(Sender: TObject);
begin
  if savedialog1.FileName<>'' then
    SaveSkin(savedialog1.filename) else saveas1.Click; 
end;


procedure TLayout.Saveas1Click(Sender: TObject);
begin
  if savedialog1.Execute then
  begin
    saveskin(savedialog1.FileName);
  end;
end;

procedure TLayout.FormCreate(Sender: TObject);
begin
  savedialog1.FileName:='';
  Memimage2:=TMemorystream.Create;
  if mainform.Skin.backgroundimage then
  begin
    layout.Backgroundimage.Visible:=true;
    mainform.memimage.Seek(0,soFromBeginning);
    layout.Backgroundimage.Picture.Bitmap.LoadFromStream(mainform.memimage);
  end;
end;

procedure TLayout.ShowHelpClick(Sender: TObject);
begin
  changed:=true;
  tempskin.showHelp:=showhelp.Checked;
end;

procedure TLayout.Open1Click(Sender: TObject);
begin
  if opendialog1.execute then
  begin
    openskin(opendialog1.FileName);

    Edit1.Text:=IntToHex(tempskin.ProcessTextColor,6);
    Panel1.color:=tempskin.ProcessTextColor;

    Edit2.Text:=IntToHex(tempskin.Normaltextcolor,6);
    Panel2.color:=tempskin.Normaltextcolor;

    Edit4.Text:=IntToHex(tempskin.Backgroundcolor,6);
    panel4.color:=tempskin.Backgroundcolor;

    Edit3.Text:=IntToHex(tempskin.GroupBoxcolor,6);
    Panel3.color:=tempskin.Groupboxcolor;

    Edit7.Text:=IntToHex(tempskin.Textfieldcolor,6);
    Panel7.Color:=tempskin.Textfieldcolor;

    Edit8.Text:=IntToHex(tempskin.Textfieldbackgroundcolor,6);
    Panel8.Color:=tempskin.Textfieldbackgroundcolor;

    edit5.Text:=IntToHex(tempskin.Selectedrecordcolor,6);
    panel5.color:=tempskin.Selectedrecordcolor;

    showhelp.checked:=tempskin.showHelp;

  end;
end;

procedure TLayout.Button2Click(Sender: TObject);
begin
  tempskin.backgroundimage:=false;
  tempskin.ProcessTextColor:=clMenuText;
  tempskin.Normaltextcolor:=clMenuText;
  tempskin.InvertedNormalTextColor:=clWhite;
  tempskin.Groupboxcolor:=clBtnFace;
  tempskin.Backgroundcolor:=clBtnFace; //clSilver;
  tempskin.Selectedrecordcolor:=clBlack;
  tempskin.Selectedrecordcolor2:=clHighlight;
  tempskin.Textfieldcolor:=clWindowText;
  tempskin.Textfieldbackgroundcolor:=clWindow;
  tempskin.showHelp:=true;

  edit1.Text:=IntToHex(tempskin.ProcessTextColor,6);
  edit2.Text:=IntToHex(tempskin.Normaltextcolor,6);
  edit4.Text:=IntToHex(tempskin.Backgroundcolor,6);
  edit3.Text:=IntToHex(tempskin.Groupboxcolor,6);
  edit7.Text:=IntToHex(tempskin.Textfieldcolor,6);
  edit8.Text:=IntToHex(tempskin.Textfieldbackgroundcolor,6);
  edit5.Text:=IntToHex(tempskin.Selectedrecordcolor,6);
  backgroundimage.Visible:=false;

  changed:=true;
end;

procedure TLayout.Edit5Change(Sender: TObject);
var c: dword;
    i: Integer;
begin
  val('$'+edit5.text,c,i);
  if i=0 then panel5.Color:=c;
  Changed:=true;
  tempskin.Selectedrecordcolor:=panel5.color;
end;

procedure TLayout.Saveasdefault1Click(Sender: TObject);
begin
  saveskin('default.CES');
end;

end.
