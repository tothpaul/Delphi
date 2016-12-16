unit Unit2;

interface

uses
  Vcl.Graphics;

function InlineBitmap: TBitmap; inline;

implementation

function InlineBitmap: TBitmap;
begin
  Result := TBitmap.Create;
end;

end.
