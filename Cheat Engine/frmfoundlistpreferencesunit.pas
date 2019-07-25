unit frmFoundlistPreferencesUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ColorBox, ComCtrls, ExtCtrls, math;

type

  { TfrmFoundlistPreferences }

  TfrmFoundlistPreferences = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    btnFont: TButton;
    cbStatic: TColorBox;
    cbDynamic: TColorBox;
    cbBackground: TColorBox;
    cbNormal: TColorBox;
    cbChanged: TColorBox;
    FontDialog1: TFontDialog;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ListView1: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    pnlChanged: TPanel;
    pnlDynamic: TPanel;
    pnlExample: TPanel;
    pnlNormal: TPanel;
    pnlStatic: TPanel;
    procedure btnFontClick(Sender: TObject);
    procedure DisplayChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1CustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListView1CustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
  private
    procedure updatescreen;

    function getNormalValueColor: TColor;
    procedure setNormalValueColor(c: TColor);
    function getChangedValueColor: TColor;
    procedure setChangedValueColor(c: TColor);
    function getStaticColor: TColor;
    procedure setStaticColor(c: TColor);
    function getDynamicColor: TColor;
    procedure setDynamicColor(c: TColor);
    function getBackgroundColor: TColor;
    procedure setBackgroundColor(c: TColor);
    function getFont: TFont;
    procedure setFont(f: TFont);
  public
    property NormalValueColor: TColor read getNormalValueColor write setNormalValueColor;
    property ChangedValueColor: TColor read getChangedValueColor write setChangedValueColor;
    property StaticColor: TColor read getStaticColor write setStaticColor;
    property DynamicColor: TColor read getDynamicColor write setDynamicColor;
    property BackgroundColor: TColor read getBackgroundColor write setBackgroundColor;
    property Font: TFont read getFont write setFont;
  end;

implementation

{ TfrmFoundlistPreferences }

procedure TfrmFoundlistPreferences.updatescreen;
begin
  autosize:=false;
  BeginUpdateBounds;
  listview1.width:=listview1.Column[0].Width+listview1.Column[1].Width;
  EndUpdateBounds;
  autosize:=true;
end;

function TfrmFoundlistPreferences.getNormalValueColor: TColor;
begin
  result:=cbNormal.selected;
end;

procedure TfrmFoundlistPreferences.setNormalValueColor(c: TColor);
begin
  cbNormal.selected:=c;
end;

function TfrmFoundlistPreferences.getChangedValueColor: TColor;
begin
  result:=cbChanged.selected;
end;

procedure TfrmFoundlistPreferences.setChangedValueColor(c: TColor);
begin
  cbChanged.selected:=c;
end;

function TfrmFoundlistPreferences.getStaticColor: TColor;
begin
  result:=cbStatic.selected;
end;

procedure TfrmFoundlistPreferences.setStaticColor(c: TColor);
begin
  cbstatic.selected:=c;
end;

function TfrmFoundlistPreferences.getDynamicColor: TColor;
begin
  result:=cbDynamic.selected;
end;

procedure TfrmFoundlistPreferences.setDynamicColor(c: TColor);
begin
  cbDynamic.selected:=c;

end;

function TfrmFoundlistPreferences.getBackgroundColor: TColor;
begin
  result:=cbBackground.selected;
end;

procedure TfrmFoundlistPreferences.setBackgroundColor(c: TColor);
begin
  cbBackground.selected:=c;
  listview1.Color:=c;
end;

function TfrmFoundlistPreferences.getFont: TFont;
begin
  result:=fontdialog1.Font;
end;

procedure TfrmFoundlistPreferences.setFont(f: TFont);
begin
  fontdialog1.font.Assign(f);
  listview1.font.assign(f);
end;

procedure TfrmFoundlistPreferences.FormShow(Sender: TObject);
var nw: integer;
begin
  //these colorboxes don't autosize properly in laz 2.0.0
  cbNormal.ItemHeight:=max(cbNormal.canvas.TextHeight('QqJjWwSs')+3, canvas.TextHeight('QqJjWwSs')+3);
  nw:=max(cbNormal.canvas.TextWidth('       XXXXXXXXXXXXXXXXXX'), canvas.TextWidth('       XXXXXXXXXXXXXXXXXX'));
  if nw<label4.width then
    nw:=label4.width;

  cbNormal.Width:=nw;


  cbChanged.ItemHeight:=cbNormal.ItemHeight;
  cbChanged.Width:=cbNormal.Width;

  cbStatic.ItemHeight:=cbNormal.ItemHeight;
  cbStatic.Width:=cbNormal.Width;

  cbDynamic.ItemHeight:=cbNormal.ItemHeight;
  cbDynamic.Width:=cbNormal.Width;

  cbBackground.ItemHeight:=cbNormal.ItemHeight;

  updateScreen;
end;

procedure TfrmFoundlistPreferences.DisplayChange(Sender: TObject);
begin
  listview1.Color:=cbBackground.selected;
  listview1.Repaint;
end;

procedure TfrmFoundlistPreferences.btnFontClick(Sender: TObject);
begin
  if fontdialog1.Execute then
  begin
    listview1.font.assign(fontdialog1.Font);
    DisplayChange(sender);

    listview1.Columns[0].AutoSize:=false;
    listview1.Columns[0].AutoSize:=true;
    listview1.Columns[1].AutoSize:=false;
    listview1.Columns[1].AutoSize:=true;
    updatescreen;
  end;
end;


procedure TfrmFoundlistPreferences.ListView1CustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if item.index=0 then
    listview1.Canvas.Font.Color := StaticColor
  else
    listview1.Canvas.Font.Color := DynamicColor;
end;

procedure TfrmFoundlistPreferences.ListView1CustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if item.index=1 then
  begin
    sender.Canvas.Font.color:=ChangedValueColor;
    sender.canvas.font.Style:=sender.canvas.font.Style+[fsBold];
    sender.canvas.Refresh;
  end
  else
  begin
    sender.Canvas.Font.color:=NormalValueColor;
  end;
end;

initialization
  {$I frmFoundlistPreferencesunit.lrs}

end.

