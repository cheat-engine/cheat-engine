unit frmColorPickerUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type

  { TfrmColorPicker }

  TfrmColorPicker = class(TForm)
    cpColorDialog: TColorDialog;
  private

  public
    constructor create(AOwner: TComponent); overload;
  end;

function ShowColorPicker(owner: TComponent): integer;

implementation

function ShowColorPicker(owner: TComponent): integer;
var cl: TfrmColorPicker;
begin
  cl:=TfrmColorPicker.create(owner);
  result:=0;
  //Execute opens the color-selection dialog, returning true when the user selects a color and clicks OK, or false when the user cancels.
  if cl.cpColorDialog.Execute then
     result:=cl.cpColorDialog.Color;
  freeandnil(cl);
end;

constructor TfrmColorPicker.create(AOwner: TComponent);
begin
  inherited create(AOwner);
end;

initialization
  {$I frmColorPickerUnit.lrs}

end.

