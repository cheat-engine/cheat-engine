unit ChangeTimers;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TChangeTimerForm = class(TForm)
    interval: TEdit;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    TimerToChange: TTimer;
  end;

var
  ChangeTimerForm: TChangeTimerForm;

implementation

{$R *.DFM}

procedure TChangeTimerForm.Button2Click(Sender: TObject);
begin
  ChangeTimerForm.close;
end;

procedure TChangeTimerForm.Button1Click(Sender: TObject);
var newinterval: Integer;
    error: Integer;
begin
  val(interval.text,newinterval,error);
  if (error>0) or (newinterval<10) then
  begin
    showmessage(Interval.text+' is not an valid integer of value! (must be higher than 10)');
    exit;
  end;

  TimerToChange.Interval:=newinterval;
  ChangeTimerForm.close;

end;

end.
