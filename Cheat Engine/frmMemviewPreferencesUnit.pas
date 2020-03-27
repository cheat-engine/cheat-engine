unit frmMemviewPreferencesUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, ExtCtrls, disassemblerviewunit, disassemblerviewlinesunit,
  LCLIntf, LCLType,
  {$ifdef darwin}
  macport, math
  {$endif}
  {$ifdef windows}
  windows
  {$endif};

type

  { TfrmMemviewPreferences }

  TfrmMemviewPreferences = class(TForm)
    btnFont: TButton;
    btnHexFont: TButton;
    btnRegisterViewFont: TButton;
    Button2: TButton;
    Button3: TButton;
    cbColorGroup: TComboBox;
    cbShowStatusBar: TCheckBox;
    cbOriginalRenderingSystem: TCheckBox;
    ColorDialog1: TColorDialog;
    cbFontQuality: TComboBox;
    edtSpaceAboveLines: TEdit;
    edtSpaceBelowLines: TEdit;
    edtHexSpaceBetweenLines: TEdit;
    edtJLThickness: TEdit;
    edtJLSpacing: TEdit;
    FontDialog1: TFontDialog;
    FontDialog2: TFontDialog;
    FontDialog3: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lblConditionalJump: TLabel;
    lblRegisterExample: TLabel;
    lblUnconditionalJump: TLabel;
    lblCall: TLabel;
    lblHex: TLabel;
    lblHexExample: TLabel;
    lblNormal: TLabel;
    lblRegister: TLabel;
    lblSymbol: TLabel;
    miRestoreToDefaults: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    pmColors: TPopupMenu;
    procedure btnFontClick(Sender: TObject);
    procedure btnRegisterViewFontClick(Sender: TObject);
    procedure btnHexFontClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cbColorGroupChange(Sender: TObject);
    procedure cbFontQualitySelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GroupBox1Click(Sender: TObject);
    procedure GroupBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GroupBox5Click(Sender: TObject);
    procedure lblCallClick(Sender: TObject);
    procedure lblConditionalJumpClick(Sender: TObject);
    procedure lblHexClick(Sender: TObject);
    procedure lblNormalClick(Sender: TObject);
    procedure lblRegisterClick(Sender: TObject);
    procedure lblSymbolClick(Sender: TObject);
    procedure lblUnconditionalJumpClick(Sender: TObject);
    procedure miRestoreToDefaultsClick(Sender: TObject);
  private
    { private declarations }
    oldstate: TDisassemblerViewColorsState;
    fspaceAboveLines: integer;
    fspaceBelowLines: integer;
    fhexspaceBetweenLines: integer;
    fjlThickness: integer;
    fjlSpacing: integer;
    procedure setHexSpaceBetweenLines(s: integer);
    procedure setSpaceAboveLines(s: integer);
    procedure setSpaceBelowLines(s: integer);
    procedure setjlThickness(t: integer);
    procedure setjlSpacing(s: integer);
    procedure applyfont;
  public
    { public declarations }
    colors: TDisassemblerViewColors;
    property hexSpaceBetweenLines: integer read fhexspaceBetweenLines write setHexSpaceBetweenLines;
    property spaceAboveLines: integer read fspaceAboveLines write setSpaceAboveLines;
    property spaceBelowLines: integer read fspaceBelowLines write setSpaceBelowLines;
    property jlThickness: integer read fjlThickness write setjlThickness;
    property jlSpacing: integer read fjlSpacing write setjlSpacing;
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

  rsConditionalJumpColor = 'Conditional jump color';
  rsUnconditionalJumpColor = 'Unconditional jump color';
  rsCallColor = 'Call color';


  rsDCNormal='Normal';
  rsDCHighlighted='Highlighted';
  rsDCHighlightedSecondary='Highlighted secondary';
  rsDCBreakpoint='Breakpoint';
  rsDCHighlightedBreakpoint='Highlighted breakpoint';
  rsDCHighlightedBreakpointSecondary='Highlighted breakpoint secondary';
  rsDCUltimap2='Ultimap2';
  rsDCHighlightedUltimap2='Highlighted Ultimap2';
  rsDCHighlightedUltimap2Secondary='Highlighted Ultimap2 secondary';

procedure TfrmMemviewPreferences.setHexSpaceBetweenLines(s: integer);
begin
  edtHexSpaceBetweenLines.text:=inttostr(s);
  fhexspaceBetweenLines:=s;
end;

procedure TfrmMemviewPreferences.setSpaceAboveLines(s: integer);
begin
  edtSpaceAboveLines.text:=inttostr(s);
  fspaceAboveLines:=s;
end;

procedure TfrmMemviewPreferences.setSpaceBelowLines(s: integer);
begin
  edtSpaceBelowLines.text:=inttostr(s);
  fspaceBelowLines:=s;
end;

procedure TfrmMemviewPreferences.setjlThickness(t: integer);
begin
  edtJLThickness.text:=inttostr(t);
  fjlThickness:=t;
end;

procedure TfrmMemviewPreferences.setjlSpacing(s: integer);
begin
  edtJLSpacing.text:=inttostr(s);
  fjlThickness:=s;
end;

procedure TfrmMemviewPreferences.applyfont;
begin
  cbColorGroupChange(cbColorGroup); //save the current colors

  lblNormal.font:=fontdialog1.Font;
  lblRegister.font:=fontdialog1.Font;
  lblSymbol.font:=fontdialog1.Font;
  lblHex.font:=FontDialog1.font;

  lblHexExample.Font:=fontdialog2.font;
  lblRegisterExample.Font:=FontDialog3.font;

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

  {$ifdef USELAZFREETYPE}
  cbOriginalRenderingSystem.Visible:=true;
  {$endif}
end;

procedure TfrmMemviewPreferences.FormShow(Sender: TObject);
var
  i: integer;
  extrasize: integer;
  {$ifdef windows}
  cbi: TComboboxInfo;
  {$endif}
begin
  applyfont;

  oldstate:=csUndefined;
  cbColorGroupChange(cbColorGroup);

  //
  {$ifdef windows}
  cbi.cbSize:=sizeof(cbi);
  if GetComboBoxInfo(cbColorGroup.handle, @cbi) then
    extrasize:=cbi.rcButton.Right-cbi.rcButton.Left+cbi.rcItem.Left
  else
  {$endif}
    extrasize:=16;

  i:=Canvas.TextWidth(rsDCNormal)+extrasize;
  i:=max(i, Canvas.TextWidth(rsDCHighlighted)+extrasize);
  i:=max(i, Canvas.TextWidth(rsDCHighlightedSecondary)+extrasize);
  i:=max(i, Canvas.TextWidth(rsDCBreakpoint)+extrasize);
  i:=max(i, Canvas.TextWidth(rsDCHighlightedBreakpoint)+extrasize);
  i:=max(i, Canvas.TextWidth(rsDCHighlightedBreakpointSecondary)+extrasize);
  i:=max(i, Canvas.TextWidth(rsDCUltimap2)+extrasize);
  i:=max(i, Canvas.TextWidth(rsDCHighlightedUltimap2)+extrasize);
  i:=max(i, Canvas.TextWidth(rsDCHighlightedUltimap2Secondary)+extrasize);

  btnFont.Constraints.MinWidth:=i;
  cbColorGroup.Constraints.MinWidth:=i;
  btnHexFont.Constraints.MinWidth:=i;
end;

procedure TfrmMemviewPreferences.GroupBox1Click(Sender: TObject);
begin
  colordialog1.Color:=groupbox1.color;
  colordialog1.Title:=rsBackgroundColor;
  if colordialog1.execute then
    groupbox1.color:=colordialog1.Color;
end;

procedure TfrmMemviewPreferences.GroupBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  groupbox1.OnClick(sender);
end;

procedure TfrmMemviewPreferences.GroupBox5Click(Sender: TObject);
begin

end;

procedure TfrmMemviewPreferences.lblCallClick(Sender: TObject);
begin
  colordialog1.Color:=lblCall.font.color;
  colordialog1.Title:=rsCallColor;
  if colordialog1.execute then
    lblCall.font.color:=colordialog1.Color;
end;


procedure TfrmMemviewPreferences.lblConditionalJumpClick(Sender: TObject);
begin
  colordialog1.Color:=lblConditionalJump.font.color;
  colordialog1.Title:=rsConditionalJumpColor;
  if colordialog1.execute then
    lblConditionalJump.font.color:=colordialog1.Color;
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

procedure TfrmMemviewPreferences.lblUnconditionalJumpClick(Sender: TObject);
begin
  colordialog1.Color:=lblunConditionalJump.font.color;
  colordialog1.Title:=rsunConditionalJumpColor;
  if colordialog1.execute then
    lblunConditionalJump.font.color:=colordialog1.Color;
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

procedure TfrmMemviewPreferences.btnRegisterViewFontClick(Sender: TObject);
begin
  if fontdialog3.execute then
  begin
    btnRegisterViewFont.Caption:=fontdialog3.Font.Name+' '+inttostr(fontdialog3.Font.Size);
    applyfont;
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
  fhexspaceBetweenLines:=strtoint(edtHexSpaceBetweenLines.Text);
  fspaceAboveLines:=strtoint(edtSpaceAboveLines.Text);
  fspaceBelowLines:=strtoint(edtSpaceBelowLines.Text);
  fjlThickness:=strtoint(edtJLThickness.Text);
  fjlSpacing:=strtoint(edtJLSpacing.Text);
  fhexSpaceBetweenLines:=strtoint(edtHexSpaceBetweenLines.text);

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

procedure TfrmMemviewPreferences.cbFontQualitySelect(Sender: TObject);
begin
  if cbFontQuality.ItemIndex<>-1 then
  begin
    fontdialog2.Font.quality:=TFontQuality(cbFontQuality.ItemIndex);
    applyfont;
  end;

end;


initialization
  {$I frmMemviewPreferencesUnit.lrs}

end.

