unit frmDebugEventsUnit; 

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Menus, newkernelhandler;


type
  TDebugEventData=record
    context: TContext;
  end;
  PDebugEventData=^TDebugEventData;


type

  { TfrmDebugEvents }

  TfrmDebugEvents = class(TForm)
    Button1: TButton;
    deImageList: TImageList;
    lbDebugEvents: TListBox;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure lbDebugEventsDblClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure clear;
  end; 

var
  frmDebugEvents: TfrmDebugEvents;

implementation

{ TfrmDebugEvents }

uses frmRegistersunit;

procedure TfrmDebugEvents.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  clear;
  frmDebugEvents:=nil;
  CloseAction:=caFree;
end;

procedure TfrmDebugEvents.Button1Click(Sender: TObject);
begin
  close;
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

procedure TfrmDebugEvents.clear;
var i: integer;
    d: PDebugEventData;
begin
  for i:=0 to lbDebugEvents.Count-1 do
  begin
    d:=PDebugEventData(lbDebugEvents.Items.Objects[i]);
    Freemem(d);
  end;

  lbDebugEvents.Clear;
end;

procedure TfrmDebugEvents.MenuItem1Click(Sender: TObject);
begin
  clear;
end;

initialization
  {$I frmDebugEventsUnit.lrs}

end.

