unit frmSyntaxHighlighterEditor;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ColorBox, ExtCtrls, ComCtrls, SynEditHighlighter, Types, betterControls;

type

  { TfrmHighlighterEditor }

  TfrmHighlighterEditor = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    cbStrikeout: TCheckBox;
    cbUnderline: TCheckBox;
    cbBackgroundColor: TColorBox;
    cbForegroundColor: TColorBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    pnlAttribs: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure cbBackgroundColorSelect(Sender: TObject);
    procedure cbBoldChange(Sender: TObject);
    procedure cbForegroundColorSelect(Sender: TObject);
    procedure cbItalicChange(Sender: TObject);
    procedure cbStrikeoutChange(Sender: TObject);
    procedure cbUnderlineChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    lastSelectedAttrib: TLabel;
    fHighlighter: TSynCustomHighlighter;
    procedure attribclick(sender: TObject);
    procedure setHighlighter(hl: TSynCustomHighlighter);
  public
    property highlighter: TSynCustomHighlighter read fHighlighter write setHighlighter;
  end;



implementation

{ TfrmHighlighterEditor }

uses math, CEFuncProc;

procedure TfrmHighlighterEditor.attribclick(sender: TObject);
begin
  if lastSelectedAttrib<>nil then
    lastSelectedAttrib.caption:=TSynHighlighterAttributes(lastSelectedAttrib.tag).StoredName;

  lastSelectedAttrib:=tlabel(sender);
  cbForegroundColor.Selected:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Foreground;
  cbBackgroundColor.Selected:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Background;

  cbBold.checked:=fsBold in TSynHighlighterAttributes(lastSelectedAttrib.tag).Style;
  cbItalic.checked:=fsItalic in TSynHighlighterAttributes(lastSelectedAttrib.tag).Style;
  cbUnderline.checked:=fsUnderline in TSynHighlighterAttributes(lastSelectedAttrib.tag).Style;
  cbStrikeout.checked:=fsStrikeOut in TSynHighlighterAttributes(lastSelectedAttrib.tag).Style;
  lastSelectedAttrib.caption:='*'+TSynHighlighterAttributes(lastSelectedAttrib.tag).StoredName+'*';
end;

procedure TfrmHighlighterEditor.setHighlighter(hl: TSynCustomHighlighter);
var i: integer;
  a: TSynHighlighterAttributes;
  li: TListItem;

  l: TLabel;
begin
  fHighlighter:=hl;

  for i:=0 to hl.AttrCount-1 do
  begin
    a:=TSynHighlighterAttributes.Create(hl.Attribute[i].StoredName);
    a.Assign(hl.Attribute[i]);

    l:=tlabel.create(pnlAttribs);
    l.caption:=hl.Attribute[i].StoredName;
    l.OnClick:=attribclick;
    l.Tag:=ptrint(a);
    l.parent:=pnlAttribs;
    l.Cursor:=crHandPoint;

    if a.Foreground<>clnone then
      l.font.color:=a.foreground
    else
      l.font.color:=pnlAttribs.font.color;

    if a.Background<>clNone then
      l.color:=a.Background;

    l.font.style:=a.Style;

    if i=0 then attribclick(l);
  end;
end;

procedure TfrmHighlighterEditor.FormShow(Sender: TObject);
var i,l: integer;
begin
  cbBackgroundColor.Width:=canvas.TextWidth('    XXXXXXXXXXXXXXXXXX');

  cbBackgroundColor.ItemHeight:=max(cbBackgroundColor.canvas.TextHeight('QqJjWwSs')+3, canvas.TextHeight('QqJjWwSs')+3);
  cbForegroundColor.ItemHeight:=cbBackgroundColor.ItemHeight;


  cbBold.parentfont:=false;
  cbBold.font.assign(font);
  cbBold.font.style:=[fsbold];

  cbItalic.parentfont:=false;
  cbItalic.font.assign(font);
  cbItalic.font.style:=[fsItalic];

  cbUnderline.parentfont:=false;
  cbUnderline.font.assign(font);
  cbUnderline.font.style:=[fsUnderline];

  cbStrikeout.parentfont:=false;
  cbStrikeout.font.assign(font);
  cbStrikeout.font.style:=[fsStrikeOut];

  if lastSelectedAttrib=nil then modalresult:=mrCancel;

  l:=0;
  for i:=0 to highlighter.AttrCount-1 do
    l:=max(l, pnlAttribs.Canvas.GetTextWidth('  *'+highlighter.Attribute[i].StoredName+'*  '));

  pnlAttribs.Width:=max(pnlAttribs.Width,l);

end;



procedure TfrmHighlighterEditor.Button1Click(Sender: TObject);
var i: integer;
begin
  for i:=0 to pnlAttribs.ComponentCount-1 do
  begin
    highlighter.Attribute[i].Assign(TSynHighlighterAttributes(pnlAttribs.Components[i].Tag));
    highlighter.Attribute[i].StyleMask:=highlighter.Attribute[i].Style;
  end;

  modalresult:=mrok;
end;


procedure TfrmHighlighterEditor.cbForegroundColorSelect(Sender: TObject);
begin
  TSynHighlighterAttributes(lastSelectedAttrib.tag).Foreground:=cbForegroundColor.Selected;
  if cbForegroundColor.Selected<>clnone then
    lastSelectedAttrib.font.color:=cbForegroundColor.Selected
  else
    lastSelectedAttrib.font.color:=pnlAttribs.Font.Color;
end;

procedure TfrmHighlighterEditor.cbBackgroundColorSelect(Sender: TObject);
begin
  TSynHighlighterAttributes(lastSelectedAttrib.tag).Background:=cbBackgroundColor.Selected;
  if cbBackgroundColor.selected<>clnone then
    lastSelectedAttrib.color:=cbBackgroundColor.Selected
  else
    lastSelectedAttrib.ParentColor:=true;
end;



procedure TfrmHighlighterEditor.cbBoldChange(Sender: TObject);
begin
  if cbBold.checked then
    TSynHighlighterAttributes(lastSelectedAttrib.tag).Style:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Style+[fsbold]
  else
    TSynHighlighterAttributes(lastSelectedAttrib.tag).Style:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Style-[fsbold];

  TSynHighlighterAttributes(lastSelectedAttrib.tag).StyleMask:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Style;
  lastSelectedAttrib.font.style:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Style;
end;


procedure TfrmHighlighterEditor.cbItalicChange(Sender: TObject);
begin
  if cbItalic.checked then
    TSynHighlighterAttributes(lastSelectedAttrib.tag).Style:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Style+[fsItalic]
  else
    TSynHighlighterAttributes(lastSelectedAttrib.tag).Style:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Style-[fsItalic];

  TSynHighlighterAttributes(lastSelectedAttrib.tag).StyleMask:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Style;
  lastSelectedAttrib.font.style:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Style;

end;

procedure TfrmHighlighterEditor.cbUnderlineChange(Sender: TObject);
begin
  if cbUnderLine.checked then
    TSynHighlighterAttributes(lastSelectedAttrib.tag).Style:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Style+[fsUnderLine]
  else
    TSynHighlighterAttributes(lastSelectedAttrib.tag).Style:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Style-[fsUnderLine];

  TSynHighlighterAttributes(lastSelectedAttrib.tag).StyleMask:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Style;
  lastSelectedAttrib.font.style:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Style;

end;

procedure TfrmHighlighterEditor.FormCreate(Sender: TObject);
begin
  LoadFormPosition(self);
  pnlAttribs.color:=clWindow;
end;

procedure TfrmHighlighterEditor.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(self);
end;

procedure TfrmHighlighterEditor.cbStrikeoutChange(Sender: TObject);
begin
  if cbStrikeOut.checked then
    TSynHighlighterAttributes(lastSelectedAttrib.tag).Style:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Style+[fsStrikeOut]
  else
    TSynHighlighterAttributes(lastSelectedAttrib.tag).Style:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Style-[fsStrikeOut];

  TSynHighlighterAttributes(lastSelectedAttrib.tag).StyleMask:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Style;
  lastSelectedAttrib.font.style:=TSynHighlighterAttributes(lastSelectedAttrib.tag).Style;
end;


initialization
  {$I frmSyntaxHighlighterEditor.lrs}

end.

