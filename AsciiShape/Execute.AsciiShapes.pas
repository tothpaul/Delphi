unit Execute.AsciiShapes;

{
  ASCII Shape for Delphi Berlin (c)2017 by Execute SARL <contact@execute.fr>

  Based on
  http://cocoamine.net/blog/2015/03/20/replacing-photoshop-with-nsstring/

}
interface

uses
  System.Classes,
  System.Types,
  System.SysUtils,
  Vcl.Graphics;

type
  TShapePoint = class
    x, y : Integer;
    Count: Integer;
    Next : TShapePoint;
    constructor Create(Ax, Ay: Integer);
    destructor Destroy; override;
    procedure Add(Point: TShapePoint);
    procedure Ellipse(Ax, Ay: Integer);
  end;

  TShapeColors = record
    Pen  : Integer;
    Brush: Integer;
  end;

  TAsciiShape = class
  private
    FWidth : Integer;
    FHeight: Integer;
    FPoints: TList;
    FColors: array of TShapeColors;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromStrings(Strings: TStrings);
    procedure Draw(x, y: Integer; Canvas: TCanvas; Width, Height: Integer);
  end;

implementation

const
  COUNT_ELLIPSE =  0;

  CHARS = '123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnpqrstuvwxyz';
  CHAR_COUNT = 60;

{ TAsciiShape }

procedure TAsciiShape.Clear;
var
  Index: Integer;
begin
  for Index := 0 to FPoints.Count - 1 do
    TShapePoint(FPoints[Index]).Free;
  FPoints.Clear;
  FColors := nil;
  FWidth := 0;
  FHeight := 0;
end;

constructor TAsciiShape.Create;
begin
  inherited;
  FPoints := TList.Create;
end;

destructor TAsciiShape.Destroy;
begin
  Clear;
  FPoints.Free;
  inherited;
end;

procedure TAsciiShape.Draw(x, y: Integer; Canvas: TCanvas; Width,
  Height: Integer);
var
  sx, sy: Single;
  iPoint: Integer;
  Point : TShapePoint;
  x1, y1: Integer;
  x2, y2: Integer;
  Count : Integer;
  Poly  : TArray<TPoint>;
  iPoly : Integer;
begin
  if (FWidth <= 0) or (FHeight <= 0) then
    Exit;
  with Canvas do
  begin
    sx := Width / FWidth;
    sy := Height / FHeight;

    Canvas.Pen.Width := Round(sx);

    for iPoint := 0 to FPoints.Count - 1 do
    begin

      Canvas.Pen.Color := FColors[iPoint].Pen;
      if FColors[iPoint].Brush = -1 then
        Canvas.Brush.Style := bsClear
      else
        Canvas.Brush.Color := FColors[iPoint].Brush;

      Point := FPoints[iPoint];

      x1 := Round(Point.X * sx + sx / 2);
      y1 := Round(Point.Y * sy + sy / 2);

      Count := Point.Count;

      if Count = 1 then
      begin
        Canvas.FillRect(TRect.Create(Round(x1 - sx/2), Round(y1 - sy/2), Round(x1 + sx/2), Round(y1 + sy/2)));
        Continue;
      end;

      Point := Point.Next;

      x2 := Round(Point.X * sx + sx / 2);
      y2 := Round(Point.Y * sy + sy / 2);

      if Count = COUNT_ELLIPSE then
      begin
        Canvas.Ellipse(x1, y1, x2, y2);
        Continue;
      end;

      if Count = 2 then
      begin
        Canvas.MoveTo(x1, y1);
        Canvas.LineTo(x2, y2);
        Continue;
      end;

      SetLength(Poly, Count);
      Poly[0].X := x1;
      Poly[0].Y := y1;
      Poly[1].X := x2;
      Poly[1].Y := y2;
      for iPoly := 2 to Count - 1 do
      begin
        Point := Point.Next;
        Poly[iPoly].X := Round(Point.X * sx + sx / 2);
        Poly[iPoly].Y := Round(Point.Y * sy + sy / 2);
      end;
      Canvas.Polygon(Poly);
    end;
  end;
end;

procedure TAsciiShape.LoadFromStrings(Strings: TStrings);
var
  LPoints:array[1..CHAR_COUNT] of TShapePoint;
  iChar  : Integer;
  x      : Integer;
  iLine  : Integer;
  Line   : string;
  Char   : string;
  Col    : Integer;
  Point  : TShapePoint;
  iPoint : Integer;
  iColor : Integer;
  Next   : TShapePoint;
begin
  Clear;

  FillChar(LPoints, SizeOf(LPoints), 0);
  FHeight := -1;

  iLine := 0;
  while iLine < Strings.Count do
  begin
    Line := Strings[iLine];
    Inc(iLine);

    if Line = '' then
      Continue;

    if Line[1] = '$' then
    begin
      Dec(iLine);
      Break;
    end;

    x := -1;

    for Col := 1 to Length(Line) do
    begin
      Char := Line[Col];

      if Char = ' ' then
        Continue;

      Inc(x);
      if x = 0 then
        Inc(FHeight);

      iChar := Pos(Char, CHARS);
      if iChar = 0 then
        Continue;

      Point := LPoints[iChar];

      if Point = nil then
      begin
        LPoints[iChar] := TShapePoint.Create(x, FHeight); // New Pixel
      end else begin
        if Point.Count = 1 then
          Point.Add(TShapePoint.Create(x, FHeight)) // Pixel => Line
        else
          Point.Ellipse(x, FHeight); // Line => Ellipse
      end;

    end;

    if x >= FWidth then
      FWidth := x + 1;

  end;

  Inc(FHeight);

  if (FHeight = 0) or (FWidth = 0) then
  begin
    Exit;
  end;

  Point := LPoints[1];
  iChar := 2;
  while iChar <= CHAR_COUNT do
  begin
    Next := LPoints[iChar];
    if (Point <> nil) and (Point.Count = 1) and (Next <> nil) and (Next.Count = 1) then
    begin
      repeat
        LPoints[iChar] := nil;
        Point.Add(Next);
        Inc(iChar);
        if iChar <= CHAR_COUNT then
        begin
          Next := LPoints[iChar];
        end else begin
          Next := nil;
        end;
      until (Next = nil) or (Next.Count <> 1);
    end;
    if Point <> nil then
      FPoints.Add(Point);
    Point := Next;
    Inc(iChar);
  end;
  if (Point <> nil) and (Point.Count = 1) then
    FPoints.Add(Point);

  SetLength(FColors, FPoints.Count);
  iPoint := 0;
  iColor := 0;
  while (iLine < Strings.Count) and (iPoint < Length(FColors)) do
  begin
    Line := Strings[iLine];
    Inc(iLine);
    if Line = '' then
    begin
      FColors[iPoint] := FColors[iColor];
    end else begin
      x := Pos(',', Line);
      if x > 0  then
      begin
        FColors[iPoint].Pen := StrToIntDef(Copy(Line, 1, x - 1), 0);
        FColors[iPoint].Brush := StrToIntDef(Copy(Line, x + 1, MaxInt), 0);
      end else begin
        FColors[iPoint].Pen := StrToIntDef(Line, 0);
        FColors[iPoint].Brush := FColors[iPoint].Pen;
      end;
      iColor := iPoint;
    end;
    Inc(iPoint);
  end;
  while iPoint < FPoints.Count do
  begin
    FColors[iPoint] := FColors[iColor];
    Inc(iPoint);
  end;
end;

{ TShapePoint }

procedure TShapePoint.Add(Point: TShapePoint);
begin
  Inc(Count);
  if Next = nil then
    Next := Point
  else
    Next.Add(Point);
end;

constructor TShapePoint.Create(Ax, Ay: Integer);
begin
  x := Ax;
  y := Ay;
  Count := 1;
end;

destructor TShapePoint.Destroy;
begin
  Next.Free;
  inherited;
end;

procedure TShapePoint.Ellipse(Ax, Ay: Integer);
var
  t: Integer;
begin
  if Count = 2 then
  begin
    if x > Next.x then
    begin
      t := x;
      x := Next.x;
      Next.x := t;
    end;
    Count := COUNT_ELLIPSE;
  end;
  if Ax < x then
    x := Ax;
  if Ax > Next.x then
    Next.x := Ax;
  if Ay < y then
    y := Ay;
  if Ay > Next.y then
    Next.y := Ay;
end;

end.
