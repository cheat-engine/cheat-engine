unit frmFunctionlistUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmFunctionList = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    Button1: TButton;
    Label1: TLabel;
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private declarations }
    function getSelectedIndex: integer;
    function getFunction(index: integer): string;
  public
    { Public declarations }
    constructor create(AOwner: TComponent; functionList: TStrings);
    property itemindex: integer read getselectedindex;
    property functions[index: integer]: string read getFunction;

  end;

implementation

{$R *.dfm}

constructor TfrmFunctionList.create(AOwner: TComponent; functionList: TStrings);
begin
  inherited create(AOwner);

  listbox1.Items.AddStrings(functionlist);
end;

function TfrmFunctionlist.getFunction(index: integer):string;
begin
  if (index<0) or (index>=listbox1.Count) then
  begin
    result:='';
    exit;
  end else result:=listbox1.Items[index];

end;

function TfrmFunctionList.getSelectedIndex: integer;
begin
  result:=listbox1.ItemIndex;
end;

procedure TfrmFunctionList.ListBox1DblClick(Sender: TObject);
begin
  Button1.click;
end;

end.
