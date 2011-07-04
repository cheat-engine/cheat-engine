unit formMemoryTrainerUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,handles,extraTrainerComponents;

type
  TfrmMemoryTrainerPreview = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Button1: TButton;
    ScrollBox1: TScrollBox;
    Label3: TLabel;
    Label4: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Image1: TImage;
    Button2: TButton;
    Button3: TButton;
    procedure Panel1Resize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    rowscreated: integer;
    descriptions: array of TLabel;
    hotkeys: array of TLabel;
    cheatlist:tcheatlist;
    
  public
    { Public declarations }
    procedure UpdateScreen;
  end;

var
  frmMemoryTrainerPreview: TfrmMemoryTrainerPreview;

implementation

uses formMemoryModifier;

{$R *.dfm}

procedure TfrmMemoryTrainerPreview.UpdateScreen;
var i: integer;
begin
  while cheatlist.Count>length(frmmemorymodifier.trainerdata) do cheatlist.deletelast;

  for i:=0 to cheatlist.Count-1 do
  begin
    cheatlist.Items[i].Description:=frmmemorymodifier.trainerdata[i].description;
    cheatlist.Items[i].Hotkey:=frmmemorymodifier.trainerdata[i].hotkeytext;
    cheatlist.Items[i].HasEditBox:=frmmemorymodifier.trainerdata[i].hasedit;
    cheatlist.Items[i].Editvalue:=frmmemorymodifier.trainerdata[i].editvalue;
  end;

  //add the missing cheats
  while cheatlist.Count<length(frmmemorymodifier.trainerdata) do
    cheatlist.addcheat(frmmemorymodifier.trainerdata[cheatlist.Count].hotkeytext,frmmemorymodifier.trainerdata[cheatlist.Count].description,frmmemorymodifier.trainerdata[cheatlist.Count].editvalue,frmmemorymodifier.trainerdata[cheatlist.Count].hasedit);

end;

procedure TfrmMemoryTrainerPreview.Panel1Resize(Sender: TObject);
begin
  frmMemoryModifier.lblWidthHeight.caption:='('+IntToStr(image1.Width)+'x'+IntToStr(image1.Height)+')';
end;

procedure TfrmMemoryTrainerPreview.FormCreate(Sender: TObject);
begin
  rowscreated:=1;
  setlength(descriptions,1);
  setlength(hotkeys,1);
  descriptions[0]:=label3;
  hotkeys[0]:=label4;

  cheatlist:=tcheatlist.create(self);
  cheatlist.Left:=0;
  cheatlist.Top:=15;
  cheatlist.Width:=266;
  cheatlist.Height:=50;
  cheatlist.AutoSize:=true;
  cheatlist.Anchors:=[akleft,aktop,akright];

  cheatlist.Parent:=scrollbox1;
end;

end.


