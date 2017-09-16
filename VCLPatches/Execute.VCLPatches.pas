unit Execute.VCLPatches;
{
  Some usefull patches for Delphi Tokyo VCL (c)2017 Execute SARL

  http://www.execute.fr
}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.CommCtrl,
  System.Classes,
  System.UITypes,
  System.SysUtils,
  System.Math,
  System.TypInfo,
  System.DateUtils,
  Data.DB,
  Vcl.Forms,
  Vcl.Grids,
  Vcl.Buttons,
  Vcl.DBGrids,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Controls,
  Vcl.ImgList,
  Vcl.Menus,
  Vcl.ComCtrls,
  Vcl.Graphics;

type
// Fix the Vertical Scrollbar visibility bug
  TDBGrid = class(Vcl.DBGrids.TDBGrid)
  protected
    procedure LinkActive(Value: Boolean); override;
  end;

// BringToFront even when Minimized
  TFormHelper = class helper for TForm
    procedure ShowInFront;
  end;

// Set DateTimeFormat on all TDateTimeFields
  TDataSetHelper = class helper for TDataSet
  public
    procedure SetDateTimeFormat(const Value: string);
  end;

// TField Helper
  TFieldHelper = class helper for TField
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
    function AsChar: Char;
    procedure Trim();
  end;

// AlphaBlending fix
  TImageList = class(Vcl.Controls.TImageList)
  private
    procedure FixMask(Strip: TBitmap);
    function EmptyMask(Strip: TBitmap; Row, Col: Integer): Boolean;
    function EmptyAlpha(Strip: TBitmap; Image: Integer): Boolean;
    procedure FullAlpha(Strip: TBitmap; Image: Integer);
    procedure BlankMask(Strip: TBitmap; Row, Col: Integer);
    procedure CopyAlpha(Strip: TBitmap; Row, Col: Integer);
  protected
    procedure DoDraw(Index: Integer; Canvas: TCanvas; x, y: Integer; Style: Cardinal; Enabled: Boolean = True); override;
  public
    procedure Resize;
    function GetStrip: TBitmap;
    procedure SetStrip(ABitmap: TBitmap);
  end;

// use "v" ou "Caption v" as a DropDown arrow
  TSpeedButton = class(Vcl.Buttons.TSpeedButton)
  private
    FDropDownArrow: Boolean;
    procedure DrawArrow;
  public
    procedure Paint; override;
  end;

// ScrollBox wheel
  TScrollBox  = class(Vcl.Forms.TScrollBox)
  protected
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
  end;

// Draw a Sort arrow on a Canvas
  TCanvasHelper = class helper for TCanvas
    procedure SortArrow(const Rect: TRect; Desc: Boolean);
  end;

// Compare two Memory Streams
  TMemoryStreamHelper = class helper for TMemoryStream
    function Compare(Stream: TMemoryStream): Boolean;
  end;

// TMemoryStream to any in-memory Data
  TPointerStream = class(TCustomMemoryStream)
  public
    constructor Create(Ptr: Pointer; const Size: NativeInt);
  end;

// Split PopupMenu in multi columns
  TPopupMenuHelper = class Helper for TPopupMenu
    procedure SplitCols(MaxItems: Integer = 30);
  end;

// TStringList helper
  TStringListHelper = class Helper for TStringList
    constructor CreateSorted(ADuplicates: TDuplicates = TDuplicates.dupIgnore);
    function Remove(const Str: string): Integer;
  end;

// Set the PopupButton property when a PopupButton is clicked
  TToolBar = class(Vcl.ComCtrls.TToolBar)
  private
    FPopupButton: TToolButton;
    procedure SetPopupButton(const Value: TToolButton);
  protected
    function CheckMenuDropdown(Button: TToolButton): Boolean; override;
  public
    property PopupButton: TToolButton read FPopupButton write SetPopupButton;
  end;

// ComboBox selection by Value
  TCustomComboBoxHelper = class Helper for TCustomComboBox
  public
    function Select(const Value: string): Boolean;
    procedure SetValue(const Value: string);
  end;

function FixDPI(Value: Integer): Integer;

function Explode(const Sep, Str: string; Max: Integer = 0; DropBlanks: Boolean = False): TArray<String>;
function Join(const Sep: string; const Values :TArray<String>): string;


implementation

function FixDPI(Value: Integer): Integer;
var
  PPI: Integer;
begin
  PPI := Screen.PixelsPerInch;
  if PPI = 96 then
    Result := Value
  else
    Result := MulDiv(Value, Screen.PixelsPerInch, 96);
end;

function Explode(const Sep, Str: string; Max: Integer = 0; DropBlanks: Boolean = False): TArray<String>;
var
  Index: Integer;
  Count: Integer;
  Start: Integer;
begin
  if DropBlanks and (Str = '') then
    Exit(nil);
  Count := 1;
  Start  := 1;
  Index := Pos(Sep, Str);
  while (Index > 0) and ((Max = 0) or (Count < Max)) do
  begin
    if (DropBlanks = False) or (Index > Start) then
    begin
      Inc(Count);
    end;
    Start := Index + Length(Sep);
    Index := Pos(Sep, Str, Start);
  end;
  if DropBlanks and (Start > Length(Str)) then
    Dec(Count);
  SetLength(Result, Count);
  Count := 0;
  Start := 1;
  Index := Pos(Sep, Str);
  while (Index > 0) and ((Max = 0) or (Count < Max - 1)) do
  begin
    if (DropBlanks = False) or (Index > Start) then
    begin
      Result[Count] := Copy(Str, Start, Index - Start);
      Inc(Count);
    end;
    Start := Index + Length(Sep);
    Index := Pos(Sep, Str, Start);
  end;
  if Count < Length(Result) then
    Result[Count] := Copy(Str, Start, MaxInt);
end;

function Join(const Sep: string; const Values :TArray<String>): string;
var
  Len  : Integer;
  Index: Integer;
  Idx  : Integer;
  Str  : string;
begin
  if Length(Values) = 0 then
    Exit('');

  Len := Length(Values[0]);
  for Index := 1 to Length(Values) - 1 do
    Inc(Len, Length(Sep) + Length(Values[Index]));
  SetLength(Result, Len);

  Idx := 1;
  Str := Values[0];
  Len := Length(Str);
  Move(Str[1], Result[Idx], Len * SizeOf(Char));
  for Index := 1 to Length(Values) - 1 do
  begin
    Inc(Idx, Len);
    Move(Sep[1], Result[Idx], Length(Sep) * SizeOf(Char));
    Inc(Idx, Length(Sep));
    Str := Values[Index];
    Len := Length(Str);
    if Len > 0 then
      Move(Str[1], Result[Idx], Len * SizeOf(Char));
  end;
end;

{ TDBGrid }

procedure TDBGrid.LinkActive(Value: Boolean);
var
  w : Integer;
begin
  inherited;
  if Value then
  begin
//    SendMessage(Handle, WM_SIZE, 0, 0);
    w := Width;
    Width := w + 1;
    Width := w;
//    ShowScrollBar(Handle, SB_VERT, DataSource.DataSet.IsEmpty = False);
//    UpdateScrollBar;
  end;
end;


{ TFormHelper }

procedure TFormHelper.ShowInFront;
begin
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  BringToFront;
end;

{ TDataSetHelper }

procedure TDataSetHelper.SetDateTimeFormat(const Value: string);
var
  Index: Integer;
begin
  for Index := 0 to FieldCount - 1 do
  begin
    if Fields[Index] is TDateTimeField then
      TDateTimeField(Fields[Index]).DisplayFormat := Value;
  end;
end;

{ TImageList }

procedure TImageList.DoDraw(Index: Integer; Canvas: TCanvas; x, y: Integer;
  Style: Cardinal; Enabled: Boolean);
var
  Options: TImageListDrawParams;
begin
  if Enabled or (ColorDepth <> cd32Bit) then
    inherited
  else begin
    FillChar(Options, SizeOf(Options), 0);
    Options.cbSize := SizeOf(Options);
    Options.himl := Self.Handle;
    Options.i := Index;
    Options.hdcDst := Canvas.Handle;
    Options.x := x;
    Options.y := y;
    Options.fState := ILS_SATURATE;
    ImageList_DrawIndirect(@Options);
  end;
end;

function TImageList.EmptyMask(Strip: TBitmap; Row, Col: Integer): Boolean;
var
  Pixel: PCardinal;
  x, y: Integer;
begin
  for y := 0 to Height - 1 do
  begin
    Pixel := Strip.ScanLine[Strip.Height div 2 + Row * Height + y];
    Inc(Pixel, Width * Col);
    for x := 0 to Width - 1 do
    begin
      if Pixel^ <> 0 then
        Exit(False);
      Inc(Pixel);
    end;
  end;
  Result := True;
end;

function TImageList.EmptyAlpha(Strip: TBitmap; Image: Integer): Boolean;
var
  Pixel: PCardinal;
  x, y: Integer;
begin
  for y := 0 to Height - 1 do
  begin
    Pixel := Strip.ScanLine[y];
    Inc(Pixel, Width * Image);
    for x := 0 to Width - 1 do
    begin
      if Pixel^ and $FF000000 <> 0 then
        Exit(False);
      Inc(Pixel);
    end;
  end;
  Result := True;
end;

procedure TImageList.FullAlpha(Strip: TBitmap; Image: Integer);
var
  Pixel: PCardinal;
  x, y: Integer;
begin
  for y := 0 to Height - 1 do
  begin
    Pixel := Strip.ScanLine[y];
    Inc(Pixel, Width * Image);
    for x := 0 to Width - 1 do
    begin
      Pixel^ := Pixel^ or $FF000000;
      Inc(Pixel);
    end;
  end;
end;

procedure TImageList.BlankMask(Strip: TBitmap; Row, Col: Integer);
var
  Pixel: PCardinal;
  x, y: Integer;
begin
  for y := 0 to Height - 1 do
  begin
    Pixel := Strip.ScanLine[Strip.Height div 2 + Row * Height + y];
    Inc(Pixel, Width * Col);
    for x := 0 to Width - 1 do
    begin
      Pixel^ := $FFFFFFFF;
      Inc(Pixel);
    end;
  end;
end;

procedure TImageList.CopyAlpha(Strip: TBitmap; Row, Col: Integer);
var
  Pixel: PCardinal;
  Mask : PCardinal;
  Alpha : Cardinal;
  x, y: Integer;
begin
  for y := 0 to Height - 1 do
  begin
    Pixel := Strip.ScanLine[Row * Height + y];
    Mask  := Strip.ScanLine[Strip.Height div 2 + Row * Height + y];
    Inc(Pixel, Width * Col);
    Inc(Mask, Width * Col);
    for x := 0 to Width - 1 do
    begin
      Alpha := Pixel^ and $FF000000;
      Alpha := Alpha or (Alpha shr 8);
      Mask^ := Alpha or (Alpha shr 16);
      Inc(Pixel);
      Inc(Mask);
    end;
  end;
end;

procedure TImageList.FixMask(Strip: TBitmap);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
   if EmptyAlpha(Strip, i) then
     FullAlpha(Strip, i)
  end;
end;

function TImageList.GetStrip: TBitmap;
var
  i: Integer;
begin
  Result := TBitmap.Create;
  if ColorDepth = cd32Bit then
  begin
    Result.PixelFormat := pf32Bit;
    Result.SetSize(Count * Width, Height);
    Result.Canvas.Brush.Color := 0;
  end else begin
    if ColorDepth = cd8Bit then
    begin
      Result.PixelFormat := pf8Bit;
    end;
    Result.SetSize(Count * Width, 2 * Height);
    Result.Canvas.Brush.Color := clWhite;
  end;
  Result.Canvas.FillRect(Rect(0 , 0, Width, Height));
  for i := 0 to Self.Count - 1 do
  begin
    //Draw(Canvas, j * Self.Width, k * Self.Height, i, dsTransparent, itImage);
    if ColorDepth = cd32Bit then
    begin
      ImageList_DrawEx(Self.Handle, i, Result.Canvas.Handle, i * Self.Width, 0, Width, Height, 0, CLR_NONE, ILD_NORMAL)
//      DoDraw(i, Result.Canvas, j * Self.Width, k * Self.Height, ILD_NORMAL)
    end else begin
      ImageList_DrawEx(Self.Handle, i, Result.Canvas.Handle, i * Self.Width, 0     , Width, Height, CLR_NONE, CLR_NONE, ILD_NORMAL);
      ImageList_DrawEx(Self.Handle, i, Result.Canvas.Handle, i * Self.Width, Height, Width, Height, CLR_NONE, CLR_NONE, ILD_MASK);
    end;
  end;
//  if (Result.PixelFormat = pf32Bit) then
//  begin
//    FixMask(Result);
//  end;

end;

procedure TImageList.Resize;
var
  dpi: Integer;
  scl: TBitmap;
  str: string;
  bmp: TBitmap;
  w,h: Integer;
begin
  dpi := Screen.PixelsPerInch;
  if dpi = 96 then
    Exit;
  scl := TBitmap.Create;
  try
    Str := ExtractFilePath(Application.ExeName) + '\ImageList\';
    ForceDirectories(Str);
    Str := Str +  Name + '_' + IntToStr(dpi) + '.bmp';
    if FileExists(str) then
    begin
      SetSize(FixDPI(Width), FixDPI(Height));
    end else begin
      bmp := GetStrip;
      try
      {$IFDEF DEBUG}
        bmp.SaveToFile(ExtractFilePath(Application.ExeName) + '\ImageList\' + Name + '_96.bmp');
      {$ENDIF}
        w := bmp.Width div Width;
        h := bmp.Height div Height;
        SetSize(FixDPI(Width), FixDPI(Height));
        w := w * Width;
        h := h * Height;
        scl.SetSize(w, h);
        scl.Canvas.StretchDraw(Rect(0, 0, w, h), bmp);
      finally
        bmp.Free;
      end;
      scl.SaveToFile(Str);
    end;
    scl.LoadFromFile(Str); // on relit le fichier pour une raison iconnue cela ne fonctionne pas en directe
    SetStrip(scl);
  finally
    scl.Free;
  end;
end;

procedure CopyMask(Src, Dst: TBitmap);
var
  x, y: Integer;
  s, d: PCardinal;
  a   : Cardinal;
begin
  for y := 0 to Src.Height - 1 do
  begin
    s := Src.ScanLine[y];
    d := Dst.ScanLine[y];
    for x := 0 to Src.Width - 1 do
    begin
      a := s^ and $FF000000;
      a := a or (a shr 8);
      d^ := a or (a shr 16);
      Inc(s);
      Inc(d);
    end;
  end;
end;

procedure TImageList.SetStrip(ABitmap: TBitmap);
var
  w, h: Integer;
  i, c : Integer;
  Bmp: TBitmap;
  Msk: TBitmap;
  r1 : TRect;
  r2 : TRect;
begin
  Clear;
  if (ABitmap.PixelFormat = pf32Bit) and( ABitmap.Height = Height) then
  begin
    Add(ABitmap, nil);
    Exit;
  end;
  w := Width;
  h := Height;
  c := ABitmap.Width div w;
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := ABitmap.PixelFormat;
  Bmp.SetSize(w, h);
  r1 := Rect(0, 0, w, h);
  Msk := TBitmap.Create;
  Msk.Monochrome := True;
  Msk.SetSize(w, h);
  r2 := Rect(0, 0, w, h);
  for i := 0 to c - 1 do
  begin
    Bmp.Canvas.CopyRect(r1, ABitmap.Canvas, r2);
    r2.Offset(0, +Height);
    Msk.Canvas.CopyRect(r1, ABitmap.Canvas, r2);
    r2.Offset(Width, -Height);
    Add(Bmp, Msk);
  end;
  Bmp.Free;
  Msk.Free;
end;

{ TCanvasHelper }

procedure TCanvasHelper.SortArrow(const Rect: TRect; Desc: Boolean);
var
  p: array[0..2] of TPoint;
  BrushColor: TColor;
  PenColor  : TColor;
begin
  p[0].x := Rect.Right - 8;
  p[0].y := Rect.Top + (Rect.Height - 4) div 2;
  p[1].x := p[0].x + 6;
  p[1].y := p[0].y;
  p[2].x := p[0].x + 3;
  p[2].y := p[0].y + 3;
  if Desc = False then
  begin
    Inc(p[0].y, 3);
    Inc(p[1].y, 3);
    Dec(p[2].y, 3);
  end;
  BrushColor := Brush.Color;
  PenColor := Pen.Color;
  Brush.Color := clBlue;
  Pen.Color := clBlue;
  Polygon(p);
  Brush.Color := BrushColor;
  Pen.Color := PenColor;
end;

{ TFieldHelper }

function TFieldHelper.AsChar: Char;
var
  Str: string;
begin
  Str := AsString;
  if Str = '' then
    Result := #0
  else
    Result := Str[1];
end;

procedure TFieldHelper.SaveToStream(AStream: TStream);
var
  Blob: TStream;
begin
  if AStream is TMemoryStream then
    TMemoryStream(AStream).Clear;
  Blob := DataSet.CreateBlobStream(Self, TBlobStreamMode.bmRead);
  try
    AStream.CopyFrom(Blob, 0);
  finally
    Blob.Free;
  end;
end;

procedure TFieldHelper.Trim;
var
  S1, S2: string;
begin
  S1 := AsString;
  S2 := System.SysUtils.Trim(S1);
  if S2 <> S1 then
    AsString := S2;
end;

procedure TFieldHelper.LoadFromStream(AStream: TStream);
var
  Blob: TStream;
begin
  Blob := DataSet.CreateBlobStream(Self, TBlobStreamMode.bmWrite);
  try
    Blob.CopyFrom(AStream, 0);
  finally
    Blob.Free;
  end;
end;

{ TSpeedButton }

procedure TSpeedButton.DrawArrow;
var
  Rect: TRect;
  p: array[0..2] of TPoint;
  w: Integer;
  h: Integer;
begin
  w := FixDPI(3);
  h := FixDPI(2);
  Rect := ClientRect;
  with Canvas do
  begin
    if Caption = '' then
      p[0].x := Rect.Left + Rect.Width div 2 - w
    else
      p[0].x := Rect.Left + (Rect.Width + TextWidth(Caption))div 2 - w;
    p[0].y := Rect.Top + Rect.Height div 2 - h;
    p[1].x := p[0].x + 2 * w;
    p[1].y := p[0].y;
    p[2].x := p[0].x + w;
    p[2].y := p[0].y + h;
    Brush.Color := clBlack;
    Canvas.Polygon(p);
  end;
end;

procedure TSpeedButton.Paint;
var
  Str: string;
begin
  Str := Caption;
  if Str = 'v' then
  begin
    Caption := '';
    FDropDownArrow := True;
  end;
  if Copy(Str, Length(Str) - 1, 2) = ' v' then
  begin
    Str[Length(Str)] := ' ';
    Caption := Str;
    FDropDownArrow := True;
  end;
  inherited;
  if FDropDownArrow then
    DrawArrow;
end;

{ TScrollBox }

function TScrollBox.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Perform(WM_VSCROLL, 1, 0);
  Result := True;
end;

function TScrollBox.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Perform(WM_VSCROLL, 0, 0);
  Result := True;
end;

{ TMemoryStreamHelper }

function TMemoryStreamHelper.Compare(Stream: TMemoryStream): Boolean;
begin
  if (Stream = nil) or (Stream.Size <> Size) then
    Exit(False);
  if Size = 0 then
    Exit(True);
  Result := CompareMem(Memory, Stream.Memory, Size);
end;

{ TPointerStream }

constructor TPointerStream.Create(Ptr: Pointer; const Size: NativeInt);
begin
  SetPointer(Ptr, Size);
end;

{ TPopupMenuHelper }

procedure TPopupMenuHelper.SplitCols(MaxItems: Integer);
var
  Cols : Integer;
  Index: Integer;
begin
  Cols := (Items.Count + MaxItems - 1) div MaxItems;
  if Cols > 1 then
  begin
    Cols := Items.Count div Cols;
    Index := Cols;
    while Index < Items.Count do
    begin
      Items[Index].Break := TMenuBreak.mbBarBreak;
      Inc(Index, Cols);
    end;
  end;
end;

{ TStringListHelper }

constructor TStringListHelper.CreateSorted(ADuplicates: TDuplicates);
begin
  inherited Create;
  Sorted := True;
  Duplicates := ADuplicates;
end;

function TStringListHelper.Remove(const Str: string): Integer;
begin
  Result := IndexOf(Str);
  if Result >= 0 then
    Delete(Result);
end;

{ TToolBar }

function TToolBar.CheckMenuDropdown(Button: TToolButton): Boolean;
begin
  FPopupButton := Button;
  Result := inherited;
end;

procedure TToolBar.SetPopupButton(const Value: TToolButton);
begin
  FPopupButton := Value;
end;

{ TCustomComboBoxHelper }

function TCustomComboBoxHelper.Select(const Value: string): Boolean;
begin
  ItemIndex := Items.IndexOf(Value);
  Result := ItemIndex >= 0;
end;

procedure TCustomComboBoxHelper.SetValue(const Value: string);
begin
  if not Select(Value) then
    Text := Value;
end;

end.

