unit frmIDTunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls,cefuncproc,newkernelhandler;

type
  TfrmIDT = class(TForm)
    TreeView1: TTreeView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmIDT: TfrmIDT;

implementation

{$R *.dfm}

procedure TfrmIDT.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TfrmIDT.FormCreate(Sender: TObject);
var limit: word;
    address: dword;
    x: packed array of record
      wLowOffset: WORD;
      wSelector:  WORD;
      bUnused:    BYTE;
      bAccess:    BYTE;
      wHighOffset:WORD;
    end;

begin
  getidts(@address,1);
  limit:=256*8;


end;

end.
