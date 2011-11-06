unit RegionEx;
{
Extends the TRegion class by adding the AddPolygon method
}
{$mode DELPHI}

interface

uses
  windows, StdCtrls, Classes, controls, SysUtils, dialogs, Graphics;

type
 TRegionEx = class(TRegion)
 private
 public
   procedure AddPolygon(polygon: array of POINT);
 published
 end;


implementation

procedure TRegionEx.AddPolygon(polygon: array of POINT);
var
  lRGN: HRGN;
begin
  lRGN := CreatePolygonRgn(polygon[0], length(polygon), WINDING);
  CombineRgn(Handle, Handle, lRGN, RGN_OR);
  DeleteObject(lRGN);
end;

end.

