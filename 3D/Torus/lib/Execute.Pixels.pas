unit Execute.Pixels;

{
  (c)2017 Execute SARL
  http://www.execute.fr
}


interface

uses
  Winapi.Windows,
  System.Math,
  System.SysUtils,
  Vcl.Graphics,
  Execute.Trigo;

type
{$POINTERMATH ON}
  TPixels = record
    Width  : Integer;
    Height : Integer;
    Data   : PColor;
    ZBuffer: array of Cardinal;
    Palette: array[0..255] of TColor;
    procedure Resize(AWidth, AHeight: Integer);
    procedure InitZBuffer;
    procedure SetPixel(x, y, z: Integer; Color: TColor);
    procedure Gouraud(const a, b, c: TVertex; ca, cb, cc: Integer);
    procedure Flat(const a, b, c: TVertex; Shade: Integer);
    procedure Draw(x, y: Integer; Canvas: TCanvas);
  end;

implementation

procedure delta(v: Integer; var delta, incr: Integer);
begin
  if v < 0 then
  begin
    delta := -v;
    incr := -1;
  end else begin
    delta := v;
    incr := +1;
  end;
end;

procedure error(var ex, dx, x, ix: Integer; ee: Integer);
begin
  if ex > ee then
  begin
    dec(ex, ee);
    inc(x, ix);
  end;
  inc(ex, dx);
end;


{ TPixels }

procedure TPixels.Resize(AWidth, AHeight: Integer);
begin
  Width := AWidth;
  Height := AHeight;
  if Data <> nil then
    VirtualFree(Data, 0, MEM_RELEASE);
  Data := VirtualAlloc(0, Width * Height * SizeOf(TColor), MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);
  ZBuffer := nil;
end;

procedure TPixels.Draw(x, y: Integer; Canvas: TCanvas);
var
  BitsInfos: TBitmapInfo;
begin
  FillChar(BitsInfos, SizeOf(BitsInfos), 0);
  with BitsInfos do
  begin
    with bmiHeader do
    begin
      biSize          := 40;
      biWidth         := Width;
      biHeight        := - Height; // inverser le DIB pour avoir le tête en haut
      biPlanes        := 1;
      biBitCount      := 32;
    end;
  end;
  if SetDIBitsToDevice(
    Canvas.Handle,
    0, 0, Width, Height,
    0, 0, 0, Height,
    Data,
//    ZBuffer,
    BitsInfos,
    0
  ) = 0 then
    RaiseLastOSError;
end;

procedure TPixels.Flat(const a, b, c: TVertex; Shade: Integer);
{             1
    0....:....0....:
         a                Lines (x1,x2)
         #                 ( 5,  5)
        #.#                ( 4,  6)
       #...#               ( 3,  7)
      #.....#              ( 2,  8)
     #.......#             ( 1,  9)
  c ###.......#            ( 0, 10)
       ###.....#           ( 3, 11)
          ###...#          ( 6, 12)
             ###.#         ( 9, 13)
                ###        (12, 14)
                    b
}
type
  TFlatLine = record
    x1, x2: Integer;
    z1, z2: Integer;
  end;
var
  Top, Bottom, Count: Integer;
  Lines: array of TFlatLine;
  Index: Integer;

  procedure Scan(const a, b: TVertex);
  var
    dx, dy, dz: Integer;
    ix, iy, iz: Integer;
    ex, ey, ez: Integer;
    x,  y,  z : Integer;
    ee, ii : Integer;
    InRange: Boolean;
  begin
    delta(Trunc(b.x) - Trunc(a.x), dx, ix);
    delta(Trunc(b.y) - Trunc(a.y), dy, iy);
    delta(Trunc(b.z) - Trunc(a.z), dz, iz);
    ex := dx;
    ey := dy;
    ez := dz;
    ee := Max(ex, Max(ey, ez));
    x := Trunc(a.x);
    y := Trunc(a.y) - Top;
    z := Trunc(a.z);
    for ii := 0 to ee do
    begin
      if x < Lines[y].x1 then
      begin
        Lines[y].x1 := x;
        Lines[y].z1 := z;
      end;
      if x > Lines[y].x2 then
      begin
        Lines[y].x2 := x;
        Lines[y].z2 := z;
      end;
      error(ex, dx, x, ix, ee);
      error(ey, dy, y, iy, ee);
      error(ez, dz, z, iz, ee);
    end;
  end;

  procedure drawLine(y: Integer; Line: TFlatLine);
  var
    dx, dz: Integer;
    ix, iz: Integer;
    ex, ez: Integer;
    x,  z: Integer;
    ee, ii : Integer;
  begin
    delta(Line.x2 - Line.x1, dx, ix);
    if ix * dx < 0 then
      Exit;
    delta(Line.z2 - Line.z1, dz, iz);
    ex := dx;
    ez := dz;
    ee := Max(ex, ez);
    x := Line.x1;
    z := Line.z1;
    for ii := 0 to ee + 1 do
    begin
      SetPixel(x, y, z, Palette[Shade]);
      error(ex, dx, x, ix, ee);
      error(dz, dz, z, iz, ee);
    end;
  end;

begin
  Top := Trunc(Min(a.y, Min(b.y, c.y)));
  Bottom := Trunc(Max(a.y, Max(b.y, c.y)));
  Count := Bottom - Top + 1;
  SetLength(Lines, Count);
  for Index := 0 to Count - 1 do
  begin
    Lines[Index].x1 := MaxInt;
    Lines[Index].x2 := 1 - MaxInt;
  end;
  Scan(a, b);
  Scan(b, c);
  Scan(c, a);
  for Index := 0 to Count - 1 do
  begin
    drawLine(Index + Top, Lines[Index]);
  end;
end;

procedure TPixels.Gouraud(const a, b, c: TVertex; ca, cb, cc: Integer);
type
  TGouraudLine = record
    x1, x2: Integer;
    c1, c2: Integer;
    z1, z2: Integer;
  end;
var
  Top, Bottom, Count: Integer;
  Lines: array of TGouraudLine;
  Index: Integer;

  procedure Scan(const a, b: TVertex; ca, cb: Integer);
  var
    dx, dy, dz, dc: Integer;
    ix, iy, iz, ic: Integer;
    ex, ey, ez, ec: Integer;
    x,  y,  z,  c : Integer;
    ee, ii : Integer;
    InRange: Boolean;
  begin
    delta(Trunc(b.x) - Trunc(a.x), dx, ix);
    delta(Trunc(b.y) - Trunc(a.y), dy, iy);
    delta(Trunc(b.z) - Trunc(a.z), dz, iz);
    delta(cb - ca,  dc, ic);
    ex := dx;
    ey := dy;
    ez := dz;
    ec := dc;
    ee := Max(ex, Max(ey, Max(ez, dc)));
    x := Trunc(a.x);
    y := Trunc(a.y) - Top;
    z := Trunc(a.z);
    c := ca;
    for ii := 0 to ee  do
    begin
      if x < Lines[y].x1 then
      begin
        Lines[y].x1 := x;
        Lines[y].c1 := c;
        Lines[y].z1 := z;
      end;
      if x > Lines[y].x2 then
      begin
        Lines[y].x2 := x;
        Lines[y].c2 := c;
        Lines[y].z2 := z;
      end;
      error(ex, dx, x, ix, ee);
      error(ey, dy, y, iy, ee);
      error(ez, dz, z, iz, ee);
      error(ec, dc, c, ic, ee);
    end;
  end;

  procedure drawLine(y: Integer; Line: TGouraudLine);
  var
    dx, dz, dc: Integer;
    ix, iz, ic: Integer;
    ex, ez, ec: Integer;
    x, z, c: Integer;
    ee, ii : Integer;
  begin
    delta(Line.x2 - Line.x1, dx, ix);
    if ix * dx <= 0 then
      Exit;
    delta(Line.c2 - Line.c1, dc, ic);
    delta(Line.z2 - Line.z1, dz, iz);
    ex := dx;
    ez := dz;
    ec := dc;
    ee := Max(ex, Max(ez, ec));
    x := Line.x1;
    z := Line.z1;
    c := Line.c1;
    for ii := 0 to ee + 1 do
    begin
      SetPixel(x, y, z, Palette[c]);
      error(ex, dx, x, ix, ee);
      error(dz, dz, z, iz, ee);
      error(ec, dc, c, ic, ee);
    end;
  end;

begin
  Top := Trunc(Min(a.y, Min(b.y, c.y)));
  Bottom := Trunc(Max(a.y, Max(b.y, c.y)));
  Count := Bottom - Top + 2;
  SetLength(Lines, Count);
  for Index := 0 to Count - 1 do
  begin
    Lines[Index].x1 := MaxInt;
    Lines[Index].x2 := 1 - MaxInt;
  end;
  Scan(a, b, ca, cb);
  Scan(b, c, cb, cc);
  Scan(c, a, cc, ca);
  for Index := 0 to Count - 1 do
  begin
    drawLine(Index + Top, Lines[Index]);
  end;
end;

procedure TPixels.InitZBuffer;
begin
  if ZBuffer = nil then
    SetLength(ZBuffer, Width * Height);
  FillChar(Zbuffer[0], Width * Height * SizeOf(Integer), $FF);
  FillChar(Data[0], Width * Height * SizeOf(Integer), 0);
end;


procedure TPixels.SetPixel(x, y, z: Integer; Color: TColor);
var
  ofs: Integer;
begin
  if z < 0 then
    Exit;
  z := Round(z * 255 / 1500);
  ofs := Width * (y + Height div 2) + x + Width div 2;
  if (Ofs >= 0) and (Ofs < Length(ZBuffer)) and (z < ZBuffer[Ofs]) then
  begin
    ZBuffer[Ofs] := z;
    Data[ofs] := Color;
  end;
end;

end.
