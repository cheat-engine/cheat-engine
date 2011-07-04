unit Userdefinedformunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,extratrainercomponents;

type
  TUserdefinedform = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }

  public
    { Public declarations }
    cheatlist: array of tcheatlist;
    cheat: array of tcheat;    
  end;

var
  Userdefinedform: TUserdefinedform;

implementation

{$R *.dfm}

procedure TUserdefinedform.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  application.Terminate;
end;

end.
