unit formDifferentBitSizeUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,cefuncproc, Menus;

type
  TformDifferentBitSize = class(TForm)
    Labelold: TLabel;
    LabelNew: TLabel;
    Button1: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    PopupMenu1: TPopupMenu;
    OhnoYoufoundme1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure OhnoYoufoundme1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
    newbit: string;
    oldbit: string;
    delta: integer;
    change: boolean;
  end;

var
  formDifferentBitSize: TformDifferentBitSize;

implementation

{$R *.dfm}

procedure TformDifferentBitSize.FormShow(Sender: TObject);
var i: integer;
begin
  //convert the bits in bitscan to a string
  oldbit:='';
  for i:=length(bitscan)-1 downto 0 do
    oldbit:=oldbit+IntToStr(bitscan[i]);

  delta:=0;

  labelold.caption:=oldbit;
  labelnew.caption:=newbit;

  labelnew.left:=labelold.left+labelold.Width-labelnew.Width;

  label1.caption:='The last time you scanned the number of bits was '+IntToStr(length(oldbit))+' and now it is '+IntToStr(length(newbit))+'. Please tell me how and how much i must change the bit offset to scan succesfully. (Left+right arrow keys move the bits)';

  setlength(bitscan,0);
end;

procedure TformDifferentBitSize.Edit1KeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case key of
    vk_left: begin
               labelnew.Left:=labelnew.left-labelnew.Canvas.TextWidth('D');
               inc(delta);
             end;

    vk_right: begin
                labelnew.Left:=labelnew.left+labelnew.Canvas.TextWidth('B');
                dec(delta);
              end;
  end;
  key:=0;
end;

procedure TformDifferentBitSize.Edit1KeyPress(Sender: TObject;
  var Key: Char);
begin
  key:=chr(0);
end;

procedure TformDifferentBitSize.OhnoYoufoundme1Click(Sender: TObject);
begin
  showmessage('Well done, you found a easter egg! ');
end;

procedure TformDifferentBitSize.Button1Click(Sender: TObject);
begin

  close;
end;

procedure TformDifferentBitSize.FormCreate(Sender: TObject);
begin
  change:=false;
end;

end.
