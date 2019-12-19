unit RegionEx;
{
Extends the TRegion class by adding the AddPolygon method
}
{$mode DELPHI}

interface

uses
  LCLIntf, StdCtrls, Classes, controls, SysUtils, dialogs, Graphics, LCLType;

type
 TRegionEx = class(TRegion)
 private
 public
   procedure AddPolygon(polygon: array of TPoint);
 published
 end;


implementation

procedure TRegionEx.AddPolygon(polygon: array of TPoint);
var
  lRGN: HRGN;
begin
  lRGN := CreatePolygonRgn(@polygon[0], length(polygon), WINDING);
  CombineRgn(Handle, Handle, lRGN, RGN_OR);
  DeleteObject(lRGN);
end;

end.

