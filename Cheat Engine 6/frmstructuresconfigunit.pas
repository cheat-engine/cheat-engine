unit frmStructuresConfigUnit;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls;

type

  { TfrmStructuresConfig }

  TfrmStructuresConfig = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ColorDialog1: TColorDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure ColorClick(Sender: TObject);
  private
    { private declarations }
    fbackgroundcolor: TColor;
    fequalText: TColor;
    fdefaultText: TColor;
    fdifferentText: TColor;
    fgroupequalText: TColor;

    fselectedbackgroundcolor: TColor;
    fselectedDefaultText: TColor;
    fselectedEqualText: TColor;
    fselectedDifferentText: TColor;
    fselectedGroupEqualText: TColor;

    procedure setbackgroundcolor(x: TColor);
    procedure setdefaultText(x: TColor);
    procedure setequalText(x: TColor);
    procedure setdifferentText(x: TColor);
    procedure setgroupequalText(x: TColor);
    procedure setselectedbackgroundcolor(x: TColor);
    procedure setselectedDefaultText(x: TColor);
    procedure setselectedEqualText(x: TColor);
    procedure setselectedDifferentText(x: TColor);
    procedure setselectedGroupEqualText(x: TColor);



    function GetColor(tag: integer):TColor;
    procedure SetColor(tag: integer; color: TColor);
  public
    { public declarations }
    property backgroundcolor: TColor read fbackgroundcolor write setBackgroundColor;
    property defaultText: TColor read fdefaultText write setdefaultText;
    property equalText: TColor read fequalText write setequalText;
    property differentText: TColor read fdifferentText write setdifferentText;
    property groupequalText: TColor read fgroupequalText write setgroupequalText;

    property selectedbackgroundcolor: TColor read fselectedbackgroundcolor write setselectedbackgroundcolor;
    property selectedDefaultText: TColor read fselectedDefaultText write setselectedDefaultText;
    property selectedEqualText: TColor read fselectedEqualText write setselectedEqualText;
    property selectedDifferentText: TColor read fselectedDifferentText write setselectedDifferentText;
    property selectedGroupEqualText: TColor read fselectedGroupEqualText write setselectedGroupEqualText;
  end; 

var
  frmStructuresConfig: TfrmStructuresConfig;

implementation

{ TfrmStructuresConfig }

procedure TfrmStructuresConfig.setbackgroundcolor(x: TColor);
begin
  fbackgroundcolor:=x;
  groupbox1.color:=x;
end;

procedure TfrmStructuresConfig.setdefaultText(x: TColor);
begin
  fdefaultText:=x;
  label1.Font.Color:=x;
end;

procedure TfrmStructuresConfig.setequalText(x: TColor);
begin
  fequaltext:=x;
  label7.Font.Color:=x;
end;

procedure TfrmStructuresConfig.setdifferentText(x: TColor);
begin
  fdifferentText:=x;
  label2.Font.Color:=x;
end;

procedure TfrmStructuresConfig.setgroupequalText(x: TColor);
begin
  fgroupequalText:=x;
  label3.Font.Color:=x;
end;

procedure TfrmStructuresConfig.setselectedbackgroundcolor(x: TColor);
begin
  fselectedbackgroundcolor:=x;
  groupbox2.color:=x;
end;

procedure TfrmStructuresConfig.setselectedDefaultText(x: TColor);
begin
  fselectedDefaultText:=x;
  label4.font.color:=x;
end;

procedure TfrmStructuresConfig.setselectedEqualText(x: TColor);
begin
  fselectedEqualText:=x;
  label8.font.color:=x;
end;

procedure TfrmStructuresConfig.setselectedDifferentText(x: TColor);
begin
  fselectedDifferentText:=x;
  label5.font.color:=x;
end;

procedure TfrmStructuresConfig.setselectedGroupEqualText(x: TColor);
begin
  fselectedGroupEqualText:=x;
  label6.font.color:=x;
end;

function TfrmStructuresConfig.GetColor(tag: integer):TColor;
begin
  case tag of
    0: result:=backgroundcolor;
    1: result:=defaulttext;
    2: result:=differentText;
    3: result:=groupequaltext;
    4: result:=selectedbackgroundcolor;
    5: result:=selectedDefaultText;
    6: result:=selectedDifferentText;
    7: result:=selectedGroupEqualText;
    8: result:=EqualText;
    9: result:=selectedEqualText;
  end;
end;

procedure TfrmStructuresConfig.SetColor(tag: integer; color: TColor);
begin
  case tag of
    0: backgroundcolor:=color;
    1: defaulttext:=color;
    2: differentText:=color;
    3: groupequaltext:=color;
    4: selectedbackgroundcolor:=color;
    5: selectedDefaultText:=color;
    6: selectedDifferentText:=color;
    7: selectedGroupEqualText:=color;
    8: EqualText:=color;
    9: selectedEqualText:=color;
  end;
end;

procedure TfrmStructuresConfig.ColorClick(Sender: TObject);
begin


  ColorDialog1.Color:=GetColor(TControl(sender).tag);
  if ColorDialog1.Execute then
    SetColor(TControl(sender).tag,ColorDialog1.Color);
end;

initialization
  {$I frmstructuresconfigunit.lrs}

end.

