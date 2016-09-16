unit frmMemviewPreferencesUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, disassemblerviewunit, disassemblerviewlinesunit, windows;

type

  { TfrmMemviewPreferences }

  TfrmMemviewPreferences = class(TForm)
    btnFont: TButton;
    btnHexFont: TButton;
    Button2: TButton;
    Button3: TButton;
    cbColorGroup: TComboBox;
    ColorDialog1: TColorDialog;
    FontDialog1: TFontDialog;
    FontDialog2: TFontDialog;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblHexExample: TLabel;
    lblRegister: TLabel;
    lblNormal: TLabel;
    lblSymbol: TLabel;
    lblHex: TLabel;
    miRestoreToDefaults: TMenuItem;
    pmColors: TPopupMenu;
    procedure btnFontClick(Sender: TObject);
    procedure btnHexFontClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cbColorGroupChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GroupBox1Click(Sender: TObject);
    procedure lblHexClick(Sender: TObject);
    procedure lblNormalClick(Sender: TObject);
    procedure lblRegisterClick(Sender: TObject);
    procedure lblSymbolClick(Sender: TObject);
    procedure miRestoreToDefaultsClick(Sender: TObject);
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

uses MemoryBrowserFormUnit;

resourcestring
  rsBackgroundColor = 'Background color';
  rsHexadecimalColor = 'Hexadecimal color';
  rsNormalColor = 'Normal color';
  rsRegisterColor = 'Register color';
  rsSymbolColor = 'Symbol color';

  rsDCNormal='Normal';
  rsDCHighlighted='Highlighted';
  rsDCHighlightedSecondary='Highlighted secondary';
  rsDCBreakpoint='Breakpoint';
  rsDCHighlightedBreakpoint='Highlighted breakpoint';
  rsDCHighlightedBreakpointSecondary='Highlighted breakpoint secondary';
  rsDCUltimap2='Ultimap2';
  rsDCHighlightedUltimap2='Highlighted Ultimap2';
  rsDCHighlightedUltimap2Secondary='Highlighted Ultimap2 secondary';

procedure TfrmMemviewPreferences.applyfont;
begin
  cbColorGroupChange(cbColorGroup); //save the current colors

  lblNormal.font:=fontdialog1.Font;
  lblRegister.font:=fontdialog1.Font;
  lblSymbol.font:=fontdialog1.Font;
  lblHex.font:=FontDialog1.font;

  lblHexExample.Font:=fontdialog2.font;

  oldstate:=csUndefined;
  cbColorGroupChange(cbColorGroup); //restore the colors

  DoAutoSize;
end;

procedure TfrmMemviewPreferences.FormCreate(Sender: TObject);
begin
  oldstate:=csUndefined;
  cbColorGroup.Items.Clear;

  cbColorGroup.Items.Add(rsDCNormal);
  cbColorGroup.Items.Add(rsDCHighlighted);
  cbColorGroup.Items.Add(rsDCHighlightedSecondary);
  cbColorGroup.Items.Add(rsDCBreakpoint);
  cbColorGroup.Items.Add(rsDCHighlightedBreakpoint);
  cbColorGroup.Items.Add(rsDCHighlightedBreakpointSecondary);
  cbColorGroup.Items.Add(rsDCUltimap2);
  cbColorGroup.Items.Add(rsDCHighlightedUltimap2);
  cbColorGroup.Items.Add(rsDCHighlightedUltimap2Secondary);
end;

procedure TfrmMemviewPreferences.FormShow(Sender: TObject);
begin
  applyfont;

  oldstate:=csUndefined;
  cbColorGroupChange(cbColorGroup);
end;

procedure TfrmMemviewPreferences.GroupBox1Click(Sender: TObject);
begin
  colordialog1.Color:=groupbox1.color;
  colordialog1.Title:=rsBackgroundColor;
  if colordialog1.execute then
    groupbox1.color:=colordialog1.Color;
end;

procedure TfrmMemviewPreferences.lblHexClick(Sender: TObject);
begin
  colordialog1.Color:=lblHex.font.color;
  colordialog1.Title:=rsHexadecimalColor;
  if colordialog1.execute then
    lblHex.font.color:=colordialog1.Color;
end;

procedure TfrmMemviewPreferences.lblNormalClick(Sender: TObject);
begin
  colordialog1.Color:=lblNormal.font.color;
  colordialog1.Title:=rsNormalColor;
  if colordialog1.execute then
    lblNormal.font.color:=colordialog1.Color;
end;

procedure TfrmMemviewPreferences.lblRegisterClick(Sender: TObject);
begin
  colordialog1.Color:=lblRegister.font.color;
  colordialog1.Title:=rsRegisterColor;
  if colordialog1.execute then
    lblRegister.font.color:=colordialog1.Color;
end;

procedure TfrmMemviewPreferences.lblSymbolClick(Sender: TObject);
begin
  colordialog1.Color:=lblSymbol.font.color;
  colordialog1.Title:=rsSymbolColor;
  if colordialog1.execute then
    lblSymbol.font.color:=colordialog1.Color;
end;

procedure TfrmMemviewPreferences.miRestoreToDefaultsClick(Sender: TObject);
begin
  //restore to defaults
  MemoryBrowser.disassemblerview.getDefaultColors(colors);
  groupbox1.Color:=colors[oldstate].backgroundcolor;
  lblnormal.font.color:=colors[oldstate].normalcolor;
  lblRegister.font.color:=colors[oldstate].registercolor;
  lblSymbol.font.color:=colors[oldstate].symbolcolor;
  lblHex.Font.color:=colors[oldstate].hexcolor;

  fontdialog1.Font.Name:=MemoryBrowser.Font.name; //parent fontname and size
  fontdialog1.font.Size:=MemoryBrowser.Font.size;
  btnFont.Caption:=fontdialog1.Font.Name+' '+inttostr(fontdialog1.Font.Size);


  fontdialog1.font.Charset:=DEFAULT_CHARSET;
  fontdialog1.font.Color:=clwindowText;
  fontdialog2.font.Size:=10;
  fontdialog1.font.Name:='MS Sans Serif';
  fontdialog1.font.Style:=[];

  fontdialog2.font.Charset:=DEFAULT_CHARSET;
  fontdialog2.font.Color:=clwindowText;
  fontdialog2.font.Height:=-11;
  fontdialog2.font.Size:=10;
  fontdialog2.font.Name:='Courier New';
  fontdialog2.font.Style:=[];




  applyfont;
  oldstate:=csUndefined;
  cbColorGroupChange(cbColorGroup);
end;

procedure TfrmMemviewPreferences.btnFontClick(Sender: TObject);
var s: string;
  f:tfont;
  fd: TFontData;
begin
  fd:=Graphics.GetFontData(lblNormal.Font.Handle);
  fd.Handle:=fontdialog1.Font.Handle;
  fontdialog1.Font.FontData:=fd;

  if fontdialog1.execute then
  begin
    btnFont.Caption:=fontdialog1.Font.Name+' '+inttostr(fontdialog1.Font.Size);
    oldstate:=csUndefined;
    applyfont;

    cbColorGroupChange(cbColorGroup);
  end;
end;

procedure TfrmMemviewPreferences.btnHexFontClick(Sender: TObject);
var fd: TFontData;
begin
  if fontdialog2.execute then
  begin
    btnHexFont.Caption:=fontdialog2.Font.Name+' '+inttostr(fontdialog2.Font.Size);
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
  if oldstate<>csUndefined then
  begin
    groupbox1.Color:=colors[oldstate].backgroundcolor;
    lblnormal.font.color:=colors[oldstate].normalcolor;
    lblRegister.font.color:=colors[oldstate].registercolor;
    lblSymbol.font.color:=colors[oldstate].symbolcolor;
    lblHex.Font.color:=colors[oldstate].hexcolor;
  end;
end;

initialization
  {$I frmMemviewPreferencesUnit.lrs}

end.

