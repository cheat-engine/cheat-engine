unit changetimerunit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TUpdateTimerForm = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
    Timer: Integer;
  end;

var
  UpdateTimerForm: TUpdateTimerForm;

implementation

uses CEClient;

{$R *.dfm}

end.
