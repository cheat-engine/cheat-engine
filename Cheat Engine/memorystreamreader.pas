unit MemoryStreamReader;

{
Implements the TMemoryStreamReader object.
It's basically a memorystream that can not write. It's main purpose is to provide a seperate read pointer for a memorystream object

One requirement is that the memorystream object does not get written to while this object exists
}
{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TMemoryStreamReader=class(TCustomMemoryStream)
  private
  protected
  public
    constructor create(ms: TCustomMemoryStream);
  end;

implementation


constructor TMemoryStreamReader.create(ms: TCustomMemoryStream);
begin
  inherited create;
  SetPointer(ms.Memory, ms.size);
end;

end.

