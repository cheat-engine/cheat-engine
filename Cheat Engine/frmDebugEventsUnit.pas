unit frmDebugEventsUnit; 

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, newkernelhandler;


type
  TDebugEventData=record
    context: TContext;
  end;
  PDebugEventData=^TDebugEventData;


type

  { TfrmDebugEvents }

  TfrmDebugEvents = class(TForm)
    lbDebugEvents: TListBox;
    Panel1: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure lbDebugEventsDblClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmDebugEvents: TfrmDebugEvents;

implementation

{ TfrmDebugEvents }

uses frmRegistersunit;

procedure TfrmDebugEvents.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  frmDebugEvents:=nil;
  CloseAction:=caFree;
end;

procedure TfrmDebugEvents.lbDebugEventsDblClick(Sender: TObject);
var r: TRegisters;
    d: PDebugEventData;
begin
  //fetch detailed info
  if lbDebugEvents.ItemIndex<>-1 then
  begin
    d:=PDebugEventData(lbDebugEvents.Items.Objects[lbdebugevents.ItemIndex]);

    if d<>nil then
    begin
      r:=TRegisters.Create(self);
      r.sbShowStack.Visible:=false;
      r.SetContextPointer(@d.context, nil,0);
      r.Show;
    end;
  end;
end;

initialization
  {$I frmDebugEventsUnit.lrs}

end.

