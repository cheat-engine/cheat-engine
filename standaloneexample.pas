unit standaloneexample;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, ExtCtrls;

type
  TStandaloneExampleForm = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StandaloneExampleForm: TStandaloneExampleForm;

implementation

uses toolbarunit;

{$R *.dfm}

procedure TStandaloneExampleForm.FormShow(Sender: TObject);
begin
  toolbarform.show;
end;

end.
