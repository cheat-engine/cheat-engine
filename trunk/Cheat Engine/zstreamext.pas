unit zstreamext;

{$mode delphi}

interface

uses
  Classes, SysUtils, zstream;

type
  TCompressionstreamWithPositionSupport=class(Tcompressionstream)
  private
  public
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;  override;
  end;

implementation

function TcompressionstreamWithPositionSupport.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (origin=soCurrent) and (offset=0) then
    result:=raw_written //fsource.position
  else
    raise exception.create('A seek was done for another purpose than getting the position');
end;


end.

