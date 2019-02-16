unit frmAAEditPrefsUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources, SynEdit;

type

  { TfrmAAEditPrefs }

  TfrmAAEditPrefs = class(TForm)
    cbFontQuality: TComboBox;
    cbShowGutter: TCheckBox;
    cbShowLineNumbers: TCheckBox;
    cbSmartTab: TCheckBox;
    cbTabsToSpace: TCheckBox;
    edtTabWidth: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel1: TPanel;
    FontDialog1: TFontDialog;
    btnFont: TButton;
    Panel3: TPanel;
    Panel4: TPanel;
    procedure btnFontClick(Sender: TObject);
    procedure cbFontQualitySelect(Sender: TObject);
    procedure edtTabWidthChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbShowLineNumbersClick(Sender: TObject);
    procedure cbShowGutterClick(Sender: TObject);
    procedure cbSmartTabClick(Sender: TObject);
    procedure cbTabsToSpaceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
  private
    { Private declarations }
    fSynEdit: TSynEdit;
    oldsettings: record
      font: tfont;
      showlinenumbers: boolean;
      showgutter: boolean;
      options: TSynEditorOptions;
      tabwidth: integer;
    end;

  public
    { Public declarations }
    function execute(synedit: TSynEdit): boolean;
  end;

var
  frmAAEditPrefs: TfrmAAEditPrefs;

implementation


function TfrmAAEditPrefs.execute(synedit: TSynEdit): boolean;
begin
  fSynEdit:=synedit;
  FontDialog1.Font.Assign(fSynEdit.Font);

  cbFontQuality.ItemIndex:=integer(fSynEdit.Font.Quality);

  //save all parameters that could get changed
  oldsettings.font.Assign(fSynEdit.Font);
  oldsettings.showlinenumbers:=fSynEdit.Gutter.LineNumberPart.Visible;
  oldsettings.showgutter:=fSynEdit.Gutter.Visible;
  oldsettings.options:=fSynEdit.options;
  oldsettings.tabwidth:=fSynEdit.TabWidth;

  //setup GUI
  cbShowLineNumbers.Checked:=fSynEdit.Gutter.linenumberpart.visible;
  cbShowGutter.Checked:=fSynEdit.Gutter.Visible;
  cbSmartTab.Checked:=eoSmartTabs in fSynEdit.Options;
  cbTabsToSpace.Checked:=eoTabsToSpaces in fSynEdit.Options;
  edtTabWidth.Text:=inttostr(fSynEdit.TabWidth);
  btnFont.Caption:=fontdialog1.Font.Name+' '+inttostr(fontdialog1.Font.Size);


  //show form
  result:=showmodal=mrok;

  if not result then //undo all changes
  begin
    fsynedit.font.Assign(oldsettings.font);
    fsynedit.Gutter.linenumberpart.visible:=oldsettings.showlinenumbers;
    fsynedit.Gutter.visible:=oldsettings.showgutter;
    fsynedit.Options:=oldsettings.options;
    fSynEdit.TabWidth:=oldsettings.tabwidth;
  end;
  //else leave it and let the caller save to registry, ini, or whatever
end;

procedure TfrmAAEditPrefs.btnFontClick(Sender: TObject);
begin
  if fontdialog1.Execute then
  begin
    fsynedit.font.name:=fontdialog1.Font.name;
    fsynedit.font.size:=fontdialog1.Font.size;
    btnFont.Caption:=fontdialog1.Font.Name+' '+inttostr(fontdialog1.Font.Size);
  end;
end;

procedure TfrmAAEditPrefs.cbFontQualitySelect(Sender: TObject);
begin
  if cbFontQuality.itemindex<>-1 then
    fsynedit.font.Quality:=TFontQuality(cbFontQuality.ItemIndex);
end;

procedure TfrmAAEditPrefs.edtTabWidthChange(Sender: TObject);
var i: integer;
begin
  if TryStrToInt(edtTabWidth.text,i) then
    fSynEdit.TabWidth:=i;
end;

procedure TfrmAAEditPrefs.FormCreate(Sender: TObject);
begin
  oldsettings.font:=tfont.Create;
end;

procedure TfrmAAEditPrefs.FormDestroy(Sender: TObject);
begin
  if oldsettings.font<>nil then oldsettings.font.Free;
end;

procedure TfrmAAEditPrefs.cbShowLineNumbersClick(Sender: TObject);
begin
  fSynEdit.Gutter.linenumberpart.visible:=cbShowLineNumbers.checked;
end;

procedure TfrmAAEditPrefs.cbShowGutterClick(Sender: TObject);
begin
  fSynEdit.Gutter.Visible:=cbShowGutter.Checked;
end;

procedure TfrmAAEditPrefs.cbSmartTabClick(Sender: TObject);
begin
  if cbSmartTab.Checked then
    fSynEdit.Options:=fSynEdit.Options+[eoSmartTabs, eoSmartTabDelete]
  else
    fSynEdit.Options:=fSynEdit.Options-[eoSmartTabs, eoSmartTabDelete];
end;

procedure TfrmAAEditPrefs.cbTabsToSpaceClick(Sender: TObject);
begin
  if cbTabsToSpace.Checked then
    fSynEdit.Options:=fSynEdit.Options+[eoTabsToSpaces]
  else
    fSynEdit.Options:=fSynEdit.Options-[eoTabsToSpaces];

end;

procedure TfrmAAEditPrefs.FormShow(Sender: TObject);
begin
  button1.AutoSize:=false;
  button2.autosize:=false;
  if button1.width<button2.width then button1.width:=button2.width else button2.width:=button1.width;
end;

procedure TfrmAAEditPrefs.Panel1Click(Sender: TObject);
begin

end;

initialization
  {$i frmAAEditPrefsUnit.lrs}

end.


