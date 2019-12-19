unit frmStructuresConfigUnit;

{$MODE Delphi}

{
Note: The "Selected" part has been removed
}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, registry, fontSaveLoadRegistry;

type

  { TfrmStructuresConfig }

  TfrmStructuresConfig = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    cbAutoCreate: TCheckBox;
    cbAutoDestroyLocal: TCheckBox;
    cbAutoFillGaps: TCheckBox;
    cbPositionAddressesOverColumns: TCheckBox;
    cbDefaultHex: TCheckBox;
    cbDoNotSaveLocal: TCheckBox;
    cbAutoGuessCustomTypes: TCheckBox;
    ColorDialog1: TColorDialog;
    comboBackground: TComboBox;
    edtMaxAutoExpandLevel: TEdit;
    edtAutostructsize: TEdit;
    FontDialog1: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ColorClickOld(Sender: TObject);
    procedure comboBackgroundChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ColorClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure Color2Click(Sender: TObject);
  private
    { private declarations }
    fbackgroundcolor: TColor;
    fequalText: TColor;
    fdefaultText: TColor;
    fdifferentText: TColor;
    fgroupequalText: TColor;
    fgroupdifferentText: TColor;

    fselectedbackgroundcolor: TColor;
    fselectedDefaultText: TColor;
    fselectedEqualText: TColor;
    fselectedDifferentText: TColor;
    fselectedGroupEqualText: TColor;
    fselectedGroupDifferentText: TColor;

    fcustomfont: boolean;


//    procedure setbackgroundcolor(x: TColor);
    procedure setdefaultText(x: TColor);
    procedure setequalText(x: TColor);
    procedure setdifferentText(x: TColor);
    procedure setgroupequalText(x: TColor);
    procedure setgroupdifferentText(x: TColor);

 //   procedure setselectedbackgroundcolor(x: TColor);
    procedure setselectedDefaultText(x: TColor);
    procedure setselectedEqualText(x: TColor);
    procedure setselectedDifferentText(x: TColor);
    procedure setselectedGroupEqualText(x: TColor);
    procedure setselectedGroupDifferentText(x: TColor);



    function Get_Color(tag: integer):TColor;     //_ because inherited method exists
    procedure Set_Color(tag: integer; color: TColor);
  public
    { public declarations }
    maxautoexpandlevel: integer;
   // property backgroundcolor: TColor read fbackgroundcolor write setBackgroundColor;
    property defaultText: TColor read fdefaultText write setdefaultText;
    property equalText: TColor read fequalText write setequalText;
    property differentText: TColor read fdifferentText write setdifferentText;
    property groupequalText: TColor read fgroupequalText write setgroupequalText;
    property groupDifferentText: TColor read fgroupDifferentText write setgroupdifferentText;

   // property selectedbackgroundcolor: TColor read fselectedbackgroundcolor write setselectedbackgroundcolor;
    property selectedDefaultText: TColor read fselectedDefaultText write setselectedDefaultText;
    property selectedEqualText: TColor read fselectedEqualText write setselectedEqualText;
    property selectedDifferentText: TColor read fselectedDifferentText write setselectedDifferentText;
    property selectedGroupEqualText: TColor read fselectedGroupEqualText write setselectedGroupEqualText;
    property selectedgroupDifferentText: TColor read fselectedgroupDifferentText write setselectedgroupdifferentText;

    property customfont: boolean read fcustomfont;
  end;

var
  frmStructuresConfig: TfrmStructuresConfig;

implementation

resourcestring
  rsNormal = 'Normal';
  rsHighlighted = 'Highlighted';

{ TfrmStructuresConfig }
  {
procedure TfrmStructuresConfig.setbackgroundcolor(x: TColor);
begin
  fbackgroundcolor:=x;
 // groupbox1.color:=x;
end;    }

procedure TfrmStructuresConfig.setdefaultText(x: TColor);
begin
  fdefaultText:=x;
  if comboBackground.itemindex=0 then
    label1.Font.Color:=x;
end;

procedure TfrmStructuresConfig.setequalText(x: TColor);
begin
  fequaltext:=x;
  if comboBackground.itemindex=0 then
    label7.Font.Color:=x;
end;

procedure TfrmStructuresConfig.setdifferentText(x: TColor);
begin
  fdifferentText:=x;
  if comboBackground.itemindex=0 then
    label2.Font.Color:=x;
end;

procedure TfrmStructuresConfig.setgroupequalText(x: TColor);
begin
  fgroupequalText:=x;
  if comboBackground.itemindex=0 then
    label3.Font.Color:=x;
end;

procedure TfrmStructuresConfig.setgroupdifferentText(x: TColor);
begin
  fgroupdifferentText:=x;
  if comboBackground.itemindex=0 then
    label9.Font.Color:=x;
end;

    {
procedure TfrmStructuresConfig.setselectedbackgroundcolor(x: TColor);
begin
  fselectedbackgroundcolor:=x;
  //groupbox2.color:=x;
end;  }

procedure TfrmStructuresConfig.setselectedDefaultText(x: TColor);
begin
  fselectedDefaultText:=x;
  if comboBackground.itemindex=1 then
    label1.font.color:=x;
end;

procedure TfrmStructuresConfig.setselectedEqualText(x: TColor);
begin
  fselectedEqualText:=x;
  if comboBackground.itemindex=1 then
    label7.font.color:=x;
end;

procedure TfrmStructuresConfig.setselectedDifferentText(x: TColor);
begin
  fselectedDifferentText:=x;
  if comboBackground.itemindex=1 then
    label2.font.color:=x;
end;

procedure TfrmStructuresConfig.setselectedGroupEqualText(x: TColor);
begin
  fselectedGroupEqualText:=x;
  if comboBackground.itemindex=1 then
    label3.font.color:=x;
end;

procedure TfrmStructuresConfig.setselectedGroupDifferentText(x: TColor);
begin
  fselectedGroupDifferentText:=x;
  if comboBackground.itemindex=1 then
    label9.font.color:=x;
end;

function TfrmStructuresConfig.Get_Color(tag: integer):TColor;
begin
  case tag of
    1: result:=defaulttext;
    2: result:=EqualText;
    3: result:=differentText;
    4: result:=groupequaltext;
    5: result:=groupDifferentText;
    6: result:=selecteddefaulttext;
    7: result:=selectedEqualText;
    8: result:=selecteddifferentText;
    9: result:=selectedgroupequaltext;
    10: result:=selectedgroupDifferentText;
    else
      result:=defaulttext;
  end;
end;

procedure TfrmStructuresConfig.Set_Color(tag: integer; color: TColor);
begin
  case tag of
    1: defaulttext:=color;
    2: EqualText:=color;
    3: differentText:=color;
    4: groupequaltext:=color;
    5: groupDifferentText:=color;
    6: selecteddefaulttext:=color;
    7: selectedEqualText:=color;
    8: selecteddifferentText:=color;
    9: selectedgroupequaltext:=color;
    10: selectedgroupDifferentText:=color;
  end;
end;

procedure TfrmStructuresConfig.ColorClickOld(Sender: TObject);
begin
  ColorDialog1.Color:=Get_Color(TControl(sender).tag);
  if ColorDialog1.Execute then
    Set_Color(TControl(sender).tag,ColorDialog1.Color);
end;

procedure TfrmStructuresConfig.comboBackgroundChange(Sender: TObject);
begin
  if comboBackground.itemindex=0 then
  begin
    //normal colors
    Label1.Font.color:=fdefaultText;
    Label7.font.color:=fequalText;
    Label2.font.color:=fdifferentText;
    label3.font.color:=fgroupequalText;
    Label9.font.color:=fgroupdifferentText;
    groupbox1.Color:=clWindow;
  end
  else
  begin
    //highlighted
    Label1.Font.color:=fselecteddefaultText;
    Label7.font.color:=fselectedequalText;
    Label2.font.color:=fselecteddifferentText;
    label3.font.color:=fselectedgroupequalText;
    Label9.font.color:=fselectedgroupdifferentText;
    groupbox1.color:=clHighlight;
  end;
end;

procedure TfrmStructuresConfig.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin

end;

procedure TfrmStructuresConfig.Button1Click(Sender: TObject);
var
  reg: TRegistry;
  autosize: integer;
begin
  autosize:=strtoint(edtAutostructsize.text);
  maxautoexpandlevel:=strtoint(edtMaxAutoExpandLevel.text);

  reg:=tregistry.create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Cheat Engine\DissectData',true) then
    begin

      reg.WriteInteger('Default Color',defaultText);
      reg.WriteInteger('Match Color',equalText);
      reg.WriteInteger('No Match Color',differentText);
      reg.WriteInteger('All Match Color Same',groupequalText);
      reg.WriteInteger('All Match Color Diff',groupDifferentText);

      reg.WriteInteger('Selected Default Color',selecteddefaultText);
      reg.WriteInteger('Selected Match Color',selectedequalText);
      reg.WriteInteger('Selected No Match Color',selecteddifferentText);
      reg.WriteInteger('Selected All Match Color Same',selectedgroupequalText);
      reg.WriteInteger('Selected All Match Color Diff',selectedgroupDifferentText);

      reg.WriteBool('Autocreate', cbAutoCreate.Checked);
      reg.WriteInteger('Autocreate Size', autosize);
      reg.WriteBool('Autodestroy', cbAutoDestroyLocal.Checked);
      reg.WriteBool('Don''t save local', cbDoNotSaveLocal.Checked);
      reg.WriteBool('Autofill', cbAutoFillGaps.Checked);
      reg.WriteBool('DefaultHex', cbDefaultHex.Checked);

      reg.writeBool('Autoguess Custom Types', cbAutoGuessCustomTypes.checked);
      reg.WriteInteger('Max Auto-Expand Level',maxautoexpandlevel);
      reg.writeBool('Position Addresses Over Columns', self.cbPositionAddressesOverColumns.checked);

      if customfont then
      begin
        if Reg.OpenKey('\Software\Cheat Engine\DissectData\Font',true) then
          SaveFontToRegistry(groupbox1.Font, reg);
      end;
    end;
  finally
    reg.free;
  end;

  modalresult:=mrok;


end;

procedure TfrmStructuresConfig.Button3Click(Sender: TObject);
var
  i: integer;
  {$ifdef windows}
  cbi: TComboboxInfo;
  {$endif}
begin
  fontdialog1.font.Assign(groupbox1.Font);
  if fontdialog1.Execute then
  begin
    groupbox1.font.Assign(fontdialog1.font);

    groupbox1.AutoSize:=false;
    groupbox1.AutoSize:=true;

    autosize:=false;
    autosize:=true;

    {$ifdef windows}
    cbi.cbSize:=sizeof(cbi);
    if GetComboBoxInfo(comboBackground.Handle, @cbi) then
    begin
      i:=cbi.rcButton.Bottom-cbi.rcButton.Top;
      panel5.autosize:=false;
      panel5.clientheight:=i;
    end;
    {$endif}

    fcustomfont:=true;
  end;
end;

procedure TfrmStructuresConfig.FormCreate(Sender: TObject);
var reg: tregistry;
begin
  //load the settings from the registry
  //def colors

  maxautoexpandlevel:=1;
  defaultText:=clWindowText;
  equalText:=clGreen;
  differentText:=clRed;
  groupequalText:=clBlue;
  groupDifferentText:=$640064;

  selecteddefaultText:=InvertColor(defaultText);
  selectedequalText:=InvertColor(equalText);
  selecteddifferentText:=InvertColor(differentText);
  selectedgroupequalText:=InvertColor(groupequalText);
  selectedgroupDifferentText:=InvertColor(groupDifferentText);


  reg:=tregistry.create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Cheat Engine\DissectData',false) then
    begin
      if reg.ValueExists('Default Color') then defaultText:=reg.ReadInteger('Default Color');
      if reg.ValueExists('Match Color') then equalText:=reg.ReadInteger('Match Color');
      if reg.ValueExists('No Match Color') then differentText:=reg.ReadInteger('No Match Color');
      if reg.ValueExists('All Match Color Same') then groupequalText:=reg.ReadInteger('All Match Color Same');
      if reg.ValueExists('All Match Color Diff') then groupDifferentText:=reg.ReadInteger('All Match Color Diff');

      if reg.ValueExists('Selected Default Color') then selecteddefaultText:=reg.ReadInteger('Selected Default Color');
      if reg.ValueExists('Selected Match Color') then selectedequalText:=reg.ReadInteger('Selected Match Color');
      if reg.ValueExists('Selected No Match Color') then selecteddifferentText:=reg.ReadInteger('Selected No Match Color');
      if reg.ValueExists('Selected All Match Color Same') then selectedgroupequalText:=reg.ReadInteger('Selected All Match Color Same');
      if reg.ValueExists('Selected All Match Color Diff') then selectedgroupDifferentText:=reg.ReadInteger('Selected All Match Color Diff');

      if reg.ValueExists('Autocreate') then cbAutoCreate.Checked:=reg.ReadBool('Autocreate');
      if reg.ValueExists('Autocreate Size') then edtAutostructsize.text:=inttostr(reg.ReadInteger('Autocreate Size'));
      if reg.ValueExists('Autodestroy') then cbAutoDestroyLocal.Checked:=reg.ReadBool('Autodestroy');
      if reg.ValueExists('Don''t save local') then cbDoNotSaveLocal.Checked:=reg.ReadBool('Don''t save local');
      if reg.ValueExists('Autofill') then cbAutoFillGaps.Checked:=reg.ReadBool('Autofill');
      if reg.ValueExists('DefaultHex') then cbDefaultHex.Checked:=reg.ReadBool('DefaultHex');
      if reg.ValueExists('Autoguess Custom Types') then cbAutoGuessCustomTypes.checked:=reg.ReadBool('Autoguess Custom Types');
      if reg.ValueExists('Max Auto-Expand Level') then maxautoexpandlevel:=reg.ReadInteger('Max Auto-Expand Level');
      if reg.ValueExists('Position Addresses Over Columns') then cbPositionAddressesOverColumns.checked:=reg.ReadBool('Position Addresses Over Columns');


      if Reg.OpenKey('\Software\Cheat Engine\DissectData\Font',false) then
      begin
        LoadFontFromRegistry(groupbox1.Font,reg);
        fcustomfont:=true;
      end;

    end;
  finally
    reg.free;
  end;

  comboBackground.Items.Clear;
  comboBackground.Items.Add(rsNormal);
  comboBackground.Items.Add(rsHighlighted);
  comboBackground.itemindex:=0;

  edtMaxAutoExpandLevel.text:=inttostr(maxautoexpandlevel);
end;

procedure TfrmStructuresConfig.ColorClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TfrmStructuresConfig.FormDestroy(Sender: TObject);
begin
 // showmessage('this should never happen');
end;

procedure TfrmStructuresConfig.Color2Click(Sender: TObject);
var i: integer;
begin
  i:=comboBackground.itemindex*5;
  ColorDialog1.Color:=Get_Color(TControl(sender).tag+i);
  if ColorDialog1.Execute then
    Set_Color(TControl(sender).tag+i,ColorDialog1.Color);

  GroupBox1.ReAlign;
  groupbox1.Refresh;
end;

initialization
  {$I frmstructuresconfigunit.lrs}

end.

