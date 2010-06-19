unit frmDebugEventsUnit; 

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TfrmDebugEvents }

  TfrmDebugEvents = class(TForm)
    lbDebugEvents: TListBox;
    Panel1: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmDebugEvents: TfrmDebugEvents;

implementation

{ TfrmDebugEvents }

procedure TfrmDebugEvents.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  frmDebugEvents:=nil;
  CloseAction:=caFree;
end;

initialization
  {$I frmDebugEventsUnit.lrs}

end.

