unit frmMemviewPreferencesUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, disassemblerviewunit, disassemblerviewlinesunit;

type

  { TfrmMemviewPreferences }

  TfrmMemviewPreferences = class(TForm)
    btnFont: TButton;
    Button2: TButton;
    Button3: TButton;
    cbColorGroup: TComboBox;
    ColorDialog1: TColorDialog;
    FontDialog1: TFontDialog;
    GroupBox1: TGroupBox;
    lblRegister: TLabel;
    lblNormal: TLabel;
    lblSymbol: TLabel;
    lblHex: TLabel;
    procedure btnFontClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cbColorGroupChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GroupBox1Click(Sender: TObject);
    procedure lblHexClick(Sender: TObject);
    procedure lblNormalClick(Sender: TObject);
    procedure lblRegisterClick(Sender: TObject);
    procedure lblSymbolClick(Sender: TObject);
  private
    { private declarations }
    oldstate: TDisassemblerViewColorsState;
    procedure applyfont;
  public
    { public declarations }
    colors: TDisassemblerViewColors;

  end; 

implementation

{ TfrmMemviewPreferences }

procedure TfrmMemviewPreferences.applyfont;
begin
  lblNormal.font.Name:=fontdialog1.Font.Name;
  lblnormal.Font.Size:=fontdialog1.font.Size;

  lblRegister.font.Name:=fontdialog1.Font.Name;
  lblRegister.font.size:=fontdialog1.Font.size;

  lblSymbol.font.name:=fontdialog1.Font.Name;
  lblSymbol.font.size:=fontdialog1.Font.size;

  lblregister.Top:=lblNormal.top+lblNormal.height+2;
  lblSymbol.Top:=lblregister.top+lblregister.height+2;
  lblHex.top:=lblSymbol.top+lblregister.height+2;
end;

procedure TfrmMemviewPreferences.FormCreate(Sender: TObject);
begin
  oldstate:=csUndefined;
end;

procedure TfrmMemviewPreferences.FormShow(Sender: TObject);
begin
  applyfont;
end;

procedure TfrmMemviewPreferences.GroupBox1Click(Sender: TObject);
begin
  colordialog1.Color:=groupbox1.color;
  colordialog1.Title:='Background color';
  if colordialog1.execute then
    groupbox1.color:=colordialog1.Color;
end;

procedure TfrmMemviewPreferences.lblHexClick(Sender: TObject);
begin
  colordialog1.Color:=lblHex.font.color;
  colordialog1.Title:='Hexadecimal color';
  if colordialog1.execute then
    lblHex.font.color:=colordialog1.Color;
end;

procedure TfrmMemviewPreferences.lblNormalClick(Sender: TObject);
begin
  colordialog1.Color:=lblNormal.font.color;
  colordialog1.Title:='Normal color';
  if colordialog1.execute then
    lblNormal.font.color:=colordialog1.Color;
end;

procedure TfrmMemviewPreferences.lblRegisterClick(Sender: TObject);
begin
  colordialog1.Color:=lblRegister.font.color;
  colordialog1.Title:='Register color';
  if colordialog1.execute then
    lblRegister.font.color:=colordialog1.Color;
end;

procedure TfrmMemviewPreferences.lblSymbolClick(Sender: TObject);
begin
  colordialog1.Color:=lblSymbol.font.color;
  colordialog1.Title:='Symbol color';
  if colordialog1.execute then
    lblSymbol.font.color:=colordialog1.Color;
end;

procedure TfrmMemviewPreferences.btnFontClick(Sender: TObject);
begin
  if fontdialog1.execute then
  begin
    btnFont.Caption:=fontdialog1.Font.Name+' '+inttostr(fontdialog1.Font.Size);
    applyfont;
  end;
end;

procedure TfrmMemviewPreferences.Button2Click(Sender: TObject);
begin
  cbColorGroupChange(cbColorGroup); //apply changes of the current page first
  modalresult:=mrok;
end;

procedure TfrmMemviewPreferences.cbColorGroupChange(Sender: TObject);
begin
  //store the current state into oldstate
  if oldstate<>csUndefined then
  begin
    colors[oldstate].backgroundcolor:=groupbox1.Color;
    colors[oldstate].normalcolor:=lblnormal.Font.color;
    colors[oldstate].registercolor:=lblRegister.Font.color;
    colors[oldstate].symbolcolor:=lblSymbol.font.color;
    colors[oldstate].hexcolor:=lblHex.font.color;
  end;

  //load the new state
  oldstate:=TDisassemblerViewColorsState(cbColorGroup.ItemIndex);
  groupbox1.Color:=colors[oldstate].backgroundcolor;
  lblnormal.font.color:=colors[oldstate].normalcolor;
  lblRegister.font.color:=colors[oldstate].registercolor;
  lblSymbol.font.color:=colors[oldstate].symbolcolor;
  lblHex.Font.color:=colors[oldstate].hexcolor;
end;

initialization
  {$I frmMemviewPreferencesUnit.lrs}

end.

