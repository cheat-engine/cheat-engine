unit ConfigUnrandomizerFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmConfigUnrandomizer = class(TForm)
    edtDefault: TEdit;
    Label1: TLabel;
    cbIncremental: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmConfigUnrandomizer: TfrmConfigUnrandomizer;

implementation

{$R *.dfm}

procedure TfrmConfigUnrandomizer.Button1Click(Sender: TObject);
var i: integer;
begin
  try
    i:=strtoint(edtdefault.Text);
    if i=800701 then showmessage('Thats Dark Byte''s birthday!!');
    modalresult:=mrok;
  except

  end;
end;

end.
