unit addressedit;

{$mode delphi}

interface

uses
  Classes, SysUtils, StdCtrls, Graphics;


type
  TAddressEdit=class(Tedit)
  private
    finvalidAddress: boolean;
    finvalidcolor: Tcolor;
    function getAddress: ptruint;
  protected
    procedure Change; override;
  public
    property invalidAddress: boolean read fInvalidAddress;
    property address: ptruint read getAddress;
    constructor Create(AOwner: TComponent); override;
  published
    property invalidColor: Tcolor read fInvalidColor write fInvalidColor;
  end;


implementation

uses symbolhandler;

function TAddressEdit.getAddress: ptruint;
var
  a: ptruint;
begin
  a:=symhandler.getAddressFromName(Text, false, finvalidaddress);
  if finvalidaddress=false then
    result:=a
  else
    result:=0;
end;

procedure TAddressEdit.change;
begin
  //get the string in the editbox and parse it. On error, indicate with red text
  symhandler.getAddressFromName(Text, false, finvalidaddress);


  if finvalidaddress then
    Font.Color:=invalidColor
  else
    Font.Color:=clDefault;

  inherited change;
end;

constructor TAddressEdit.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  fInvalidColor:=clRed;
end;

end.

