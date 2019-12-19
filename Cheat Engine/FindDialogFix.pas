unit FindDialogFix;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, forms, Dialogs;

type
  TFindDialog=class(Dialogs.TFindDialog)
  private
    FFormLeft: integer;
    FFormTop: integer;
  protected
    function GetLeftFix: integer;
    procedure SetLeftFix(l: integer);

    function GetTopFix: integer;
    procedure SetTopFix(t: integer);

    function GetPositionFix: TPoint;
    procedure SetPositionFix(p: TPoint);

  public
    property Left: Integer read GetLeftFix write SetLeftFix;
    property Position: TPoint read GetPositionFix write SetPositionFix;
    property Top: Integer read GetTopFix write SetTopFix;
  end;

implementation

function TFindDialog.GetLeftFix: integer;
begin
  if assigned(FFindForm) then
    result:=FFindForm.Left
  else
    Result:=FFormLeft;
end;

procedure TFindDialog.SetLeftFix(l: integer);
begin
  FFormLeft:=l;
  if Assigned(FFindForm) then FFindForm.Left :=FFormLeft;
end;

function TFindDialog.GetTopFix: integer;
begin
  if assigned(FFindForm) then
    result:=FFindForm.Top
  else
    Result:=FFormLeft;

end;

procedure TFindDialog.SetTopFix(t: integer);
begin
  FFormTop:=t;
  if Assigned(FFindForm) then FFindForm.Top :=FFormTop;
end;

function TFindDialog.GetPositionFix: TPoint;
begin
  if assigned(FFindForm) then
    Result:=Point(FFindForm.Left, FFindForm.Top)
  else
    Result:=Point(FFormLeft, FFormTop);
end;

procedure TFindDialog.SetPositionFix(p: TPoint);
begin
  FFormLeft:=p.x;
  FFormTop:=p.y;
  if assigned(FFindForm) then
  begin
    FFindForm.Left:=FFormLeft;
    FFindForm.Top:=FFormTop;
  end;
end;


end.

