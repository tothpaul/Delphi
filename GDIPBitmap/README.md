# GDIPBitmap

this little unit let you load any GDI+ supported graphic format (BMP, JPG, PNG or TIF) into a Delphi TBitmap

it's a TBitmap helper, so just add the unit to you're project and call the method GDIPLoadFromStream

```Pascal
uses
  Execute.GDIPBitmap;
var
  bitmap: TBitmap;
  stream: TFileStream;
begin
  bitmap := TBitmap.Create;
  stream := TFileStream.Create('SAMPLE.JPG', fmOpenRead or fmShareDenyNone);
  bitmap.GDIPLoadFromStream(stream):
  stream.Free;
end;
```