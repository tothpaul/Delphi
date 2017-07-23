unit Execute.PNGLoader;

{
   PNGLoader for Delphi Tokyo (c) 2017 Execute SARL
   http://www.execute.fr

   version 1.0 - 2017.07.23
}

{
   The purpose of this unit it to give a fast, small an efficient unit to Load PNG files under Delphi

   This unit do no support all PNG format

     - 2, 4 and 16 bits format are not supported
     - interlaced image are not supported
     - 1 bit palette is ignored (image is always black and white)

   But, it work perfectly for

     - 1, 8 bits format
     - GRAYSCALE, GRAYSCALE_ALPHA, PALETTE, RGB, and RGBA format

   The code is also easy to follow with sufficient comments

   a PNG file looks like this

   +----+
   |Sign| 8 bytes signature
   +----+
   |IHDR| 13 bytes required header
   +----+
   : ?? : (optional chunks)
   +----+
   |PLTE| optional palette
   +----+
   : ?? : (optional chunks)
   +----+
   |IDAT| Image Data (GZIPed) - can be splitted over multiples IDAT chunks !
   +----+
   :  + : (optional additionnals IDAT chunks)
   +----+
   : ?? : (optional chunks)
   +----+
   |IEND| Image End
   +----+

   After "Sign", each Chunk (including IHDR) uses the same format
   +------+
   | Size |  Number of bytes in the Data part (let the loader ignore unknown chunk)
   +------+
   | Name |  4 bytes to identify the Chunk (IHDR, PLTE, IDAT, IEND...)
   +------+
   | Data |  Data of the chunk
   +------+
   | CRC  |  CRC of the chunk (ignored by PNGLoader)
   +------+

   http://www.libpng.org/pub/png/spec/1.2/PNG-Chunks.html

   IDAT contains the Bitmap with an extra byte on top of each line for filtering
   +------+--------------------------+
   |Filter| Pixels of the first Line |
   +------+--------------------------+
   :Filter: Pixels of the next  Line : as many time as required
   +......+..........................+

   Pixels can be a Palette Index, a Graycsale value, RGB, ARGB value,
   or a pair of GrayScale/Alpha values.

   the goals of the loader is to identify the image format, extract the IDAT part,
   decompress (deflate) the data, apply the filters and copy the data to the bitmap

}

interface

{-$DEFINE INCLUDE_FILTER} // for Debug purpose add an extra Pixel in front of each line with the Filter value

uses
  System.SysUtils,
  System.Classes,
  System.ZLib,
  Vcl.Graphics;

// load a PNG from a File or a Stream and return a TBitmap
function LoadPNG(const AFileName: string): TBitmap; overload;
function LoadPNG(AStream: TStream): TBitmap; overload;

// load a PNG from a File or a Stream to the provided Bitmap
procedure LoadPNG(const AFileName: string; ABitmap: TBitmap); overload;
procedure LoadPNG(AStream: TStream; ABitmap: TBitmap); overload;

implementation

function LoadPNG(const AFileName: string): TBitmap; overload;
begin
  Result := TBitmap.Create;
  try
    LoadPNG(AFileName, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function LoadPNG(AStream: TStream): TBitmap; overload;
begin
  Result := TBitmap.Create;
  try
    LoadPNG(AStream, Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure LoadPNG(const AFileName: string; ABitmap: TBitmap); overload;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    LoadPNG(Stream, ABitmap);
  finally
    Stream.Free;
  end;
end;

const
 // supported Chunks
  IHDR: array[0..3] of AnsiChar = 'IHDR'; // Image Header
  IEND: array[0..3] of AnsiChar = 'IEND'; // Image End
  PLTE: array[0..3] of AnsiChar = 'PLTE'; // Palette
  IDAT: array[0..3] of AnsiChar = 'IDAT'; // Image Data

 // Image Format
  COLOR_GRAYSCALE      = 0; // support 2017.07.23
  COLOR_RGB            = 2; // support 2017.07.23
  COLOR_PALETTE        = 3; // support 2017.07.23
  COLOR_GRAYSCALEALPHA = 4; // support 2017.07.23
  COLOR_RGBA           = 6; // support 2017.07.22

 // Filter Mode
  FILTER_NONE    = 0; // support 2017.07.22
  FILTER_SUB     = 1; // support 2017.07.22
  FILTER_UP      = 2; // support 2017.07.22
  FILTER_AVERAGE = 3; // support 2017.07.23
  FILTER_PAETH   = 4; // support 2017.07.22

type
  TPNGSignature = record
    PNG  : Cardinal;
    CRLF : Cardinal;
  end;

  TPNGHeader = packed record
    Width             : Cardinal;
    Height            : Cardinal;
    BitDepth          : Byte;
    ColorType         : Byte;
    CompressionMethod : Byte;
    FilterMethod      : Byte;
    InterlaceMethod   : Byte;
  end;

  TRGBColor = record
    R, G, B: Byte;
  end;
  TPalette = array of TRGBColor;

  TChunk = packed record
    Size: Cardinal;
    Name: Cardinal;
  end;

  TPNGContext = record
    Stream   : TStream;     // source Stream
    Bitmap   : TBitmap;     // target Bitmap
    Header   : TPNGHeader;  // PNG header
    Chunk    : TChunk;      // current Chunk
    Palette  : TPalette;    // PLTE chunk
    zStream  : TZStreamRec; // deflate record
    zData    : TBytes;      // deflate buffer
    LineSize : NativeInt;   // number of bytes per line in the decompressed stream (without Filter byte)
    BPP      : NativeInt;   // number of byte per pixel (3, 4 or 0 for monochrome)
    Image    : TBytes;      // uncompressed image (filtered with Filter byte)
    procedure ReadChunk;
    procedure ReadHeader;
    procedure ReadChunks;
    procedure LoadIDAT;
    procedure FillZStream;
    procedure ImageToMonochrome;
    procedure ImageToDIB;
    procedure FillPalette(Source, Target: PByte);
  end;

procedure BSwap(var Value: Cardinal);
begin
  Value := Swap(Value) shl 16 + Swap(Value shr 16);
end;

function Paeth(a, b, c: Byte): Byte;
// a = left, b = above, c = upper left
var
  pa, pb, pc: NativeInt;
begin
  pa := abs(b - c);
  pb := abs(a - c);
  pc := abs(a + b - 2 * c);
  if (pa <= pb) and (pa <= pc) then
    Exit(a);
  if pb <= pc then
    Result := b
  else
    Result := c;
end;

procedure RGB2BGR(AColor: Pointer; ACount: NativeInt);
var
  Color: ^TRGBColor absolute AColor;
  Index: Integer;
  t: Byte;
begin
  for Index := 0 to ACount - 1 do
  begin
    t := Color.r;
    Color.r := Color.b;
    Color.b := t;
    Inc(Color);
  end;
end;

procedure ARGB2ABGR(AColor: Pointer; ACount: NativeInt);
var
  Color: PCardinal absolute AColor;
  Index: Integer;
begin
  for Index := 0 to ACount - 1 do
  begin
    Color^ := Color^ and $FF00FF00 + Color^ and $FF shl 16 + (Color^ shr 16) and $FF;
    Inc(Color);
  end;
end;

procedure FilterLine(Source: PByte; Width, BPP, Line: Integer);
var
  Filter : Byte;
  Pixel  : PByte;
  Above  : PByte;
  Left   : PByte;
  TopLeft: PByte;
  x      : NativeInt;
begin
  Filter := Source^;
  Inc(Source);
  case Filter of
    FILTER_NONE   : { nothing } ;
    FILTER_SUB    : // Pixel[x, y] := Pixel[x, y] + Pixel[x - 1, y]
    begin
      Pixel := Source;
      Left := Pixel;
      // Ignore first Pixel
      Inc(Pixel, BPP);
      for x := (BPP * (Width - 1)) - 1 downto 0 do
      begin
        Inc(Pixel^, Left^);
        Inc(Pixel);
        Inc(Left);
      end;
    end;
    FILTER_UP     :  // Pixel[x, y] := Pixel[x, y] + Pixel[x, y - 1]
    begin
      if Line = 0 then // do not filter first line !
        Exit;
      Pixel := Source;
      Above := Pixel;
      Dec(Above, Width * BPP + 1);
      for x := (BPP * Width) - 1 downto 0 do
      begin
        Inc(Pixel^, Above^);
        Inc(Pixel);
        Inc(Above);
      end;
    end;
    FILTER_AVERAGE:  // Pixel[x, y] := Pixel[x, y] + (Pixel[x - 1, y] + Pixel[x, y - 1]) div 2
    begin
      Pixel := Source;
      Left := Pixel;
      if Line = 0 then // special case, first line
      begin
        Inc(Pixel, BPP);
        for x := (BPP * (Width - 1)) - 1 downto 0 do
        begin
          Inc(Pixel^, Left^ div 2);
          Inc(Pixel);
          Inc(Left);
        end;
      end else begin
        Above := Pixel;
        Dec(Above, Width * BPP + 1);
        for x := BPP - 1 downto 0 do  // special case, first pixel
        begin
          Inc(Pixel^, Above^ div 2);
          Inc(Pixel);
          Inc(Above);
        end;
        for x := (BPP * (Width - 1)) - 1 downto 0 do
        begin
          Inc(Pixel^, (Above^ + Left^) div 2);
          Inc(Pixel);
          Inc(Above);
          Inc(Left);
        end;
      end;
    end;
    FILTER_PAETH  : // Pixel[x, y] := Pixel[x, y] + Paeth(Pixel[x - 1, y], Pixel[x, y - 1], Pixel[x - 1, y - 1])
    begin
      Pixel := Source;
      Left := Pixel;
      if Line = 0 then  // special case, first line
      begin
        Inc(Pixel, BPP);
        for x := (BPP * (Width - 1)) - 1 downto 0 do
        begin
          Inc(Pixel^, Paeth(Left^, 0, 0));
          Inc(Pixel);
          Inc(Left);
        end;
      end else begin
        Above := Pixel;
        Dec(Above, Width * BPP + 1);
        TopLeft := Above;
        for x := BPP - 1 downto 0 do // special case, first Pixel
        begin
          Inc(Pixel^, Paeth(0, Above^, 0));
          Inc(Pixel);
          Inc(Above);
        end;
        for x := (BPP * (Width - 1)) - 1 downto 0 do
        begin
          Inc(Pixel^, Paeth(Left^, Above^, TopLeft^));
          Inc(Pixel);
          Inc(Above);
          Inc(Left);
          Inc(TopLeft);
        end;
      end;
    end;
  else
    raise Exception.Create('Unknow Filter ' + IntToStr(Filter));
  end;
end;

procedure GrayScale(Source, Target: PByte; Width: NativeInt);
var
  x, bpp: NativeInt;
  Color : Byte;
begin
  for x := 0 to Width - 1 do
  begin
    Color := Source^;
    Inc(Source);
    for bpp := 0 to 2 do
    begin
      Target^ := Color;
      Inc(Target);
    end;
  end;
end;

procedure GrayScaleAlpha(Source, Target: PByte; Width: NativeInt);
var
  x, bpp: NativeInt;
  Color : Byte;
  Alpha : Byte;
begin
  for x := 0 to Width - 1 do
  begin
    Color := Source^;
    Inc(Source);
    Alpha := Source^;
    Inc(Source);
    for bpp := 0 to 2 do
    begin
      Target^ := Color;
      Inc(Target);
    end;
    Target^ := Alpha;
    Inc(Target);
  end;
end;

procedure TPNGContext.FillPalette(Source: PByte; Target: PByte);
var
  x    : NativeInt;
  Color: Byte;
begin
  for x := 0 to Header.Width - 1 do
  begin
    Color := Source^;
    if Color > Length(Palette) then
      raise Exception.Create('Invalid palette index');
    Inc(Source);
    Target^ := Palette[Color].B;
    Inc(Target);
    Target^ := Palette[Color].G;
    Inc(Target);
    Target^ := Palette[Color].R;
    Inc(Target);
  end;
end;

procedure TPNGContext.ImageToMonochrome;
var
  Source  : PByte;
  y       : Integer;
  ScanLine: Pointer;
begin
  Source := Pointer(Image);
  for y := 0 to Header.Height - 1 do
  begin
    if Source^ <> 0 then
      raise Exception.Create('Unsupported Filter on Monochrome image');
    Inc(Source);
    ScanLine := Bitmap.ScanLine[y];
    Move(Source^, ScanLine^, LineSize);
    Inc(Source, LineSize);
  end;
end;

procedure TPNGContext.ImageToDIB;
var
  Source   : PByte;
  y        : NativeInt;
  ScanLine : Pointer;
begin
  Source := Pointer(Image);
  for y := 0 to Header.Height - 1 do
  begin
    FilterLine(Source, Header.Width, BPP, y);
    ScanLine := Bitmap.ScanLine[y];
    {$IFDEF INCLUDE_FILTER}
    case ABitmap.PixelFormat of
      pf24Bit: PCardinal(ScanLine)^ := Source^ shl 8;
      pf32Bit: PCardinal(ScanLine)^ := $FF000000 + Source^;
    else
      raise Exception.Create('Internal error #2');
    end;
    Inc(NativeInt(ScanLine), ABitmap.BPP);
    {$ENDIF}
    Inc(Source);
    case Header.ColorType of
      COLOR_GRAYSCALE     : GrayScale(Source, ScanLine, Header.Width);
      COLOR_GRAYSCALEALPHA: GrayScaleAlpha(Source, ScanLine, Header.Width);
      COLOR_PALETTE       : FillPalette(Source, ScanLine);
    else
      Move(Source^, ScanLine^, LineSize);
      case BPP of
        3 : RGB2BGR(ScanLine, Header.Width);
        4 : ARGB2ABGR(ScanLine, Header.Width);
      end;
    end;
    Inc(Source, LineSize);
  end;
end;

procedure TPNGContext.ReadChunk;
begin
  Stream.ReadBuffer(Chunk, SizeOf(Chunk));
  BSwap(Chunk.Size);
end;

procedure TPNGContext.ReadHeader;
var
  Sign: TPNGSignature;
begin
  // Signature is required
  Stream.ReadBuffer(Sign, SizeOf(Sign));
  if (Cardinal(Sign.PNG) <> $474E5089) or (Sign.CRLF <> $A1A0A0D) then
    raise Exception.Create('Not a PNG file');
  // First chunk
  ReadChunk;
  // Chunk Size for IHDR
  if Chunk.Size <> SizeOf(Header) then
    raise Exception.Create('Invalid IHDR size');
  // Chunk Name for IHDR
  if Chunk.Name <> Cardinal(IHDR) then
    raise Exception.Create('IHDR not found');
  // Read Header
  Stream.ReadBuffer(Header, SizeOf(Header));

  // PNG supports only compression method 0 = deflate
  if Header.CompressionMethod <> 0 then
    raise Exception.Create('Unsupported PNG (CompressionMethod = ' + IntToStr(Header.CompressionMethod) + ')');

  // PNG supports only filter mode 0
  if Header.FilterMethod <> 0 then
    raise Exception.Create('Unsupported PNG (FilterMethod = ' + IntToStr(Header.FilterMethod) + ')');

  // PNGLoader do not support interlaced image
  if Header.InterlaceMethod <> 0 then
    raise Exception.Create('Unsupported PNG (InterlaceMethod = ' + IntToStr(Header.InterlaceMethod) + ')');

  // Endianness
  BSwap(Header.Width);
  BSwap(Header.Height);

  case Header.BitDepth of
    1:
    case Header.ColorType of
      COLOR_GRAYSCALE,
      COLOR_PALETTE :
      begin
        BPP := 0; // 1/8 - not used
        LineSize := (Header.Width + 7) div 8;
        Bitmap.PixelFormat := pf1Bit;
      end;
    else
      raise Exception.Create('Unsupported PNG ColorType = ' + IntToStr(Header.ColorType) + ' for BitDepth 1');
    end;
    8:
    begin
      case Header.ColorType of
        COLOR_GRAYSCALE,
        COLOR_PALETTE        :
        begin
          BPP := 1; // Grayscale or Palette Index
          Bitmap.PixelFormat := pf24Bit; // 1 byte => 3 bytes
        end;
        COLOR_RGB            :
        begin
          BPP := 3; // R, G, B
          Bitmap.PixelFormat := pf24Bit;
        end;
        COLOR_RGBA           :
        begin
          BPP := 4; // R, G, B, A
          Bitmap.PixelFormat := pf32Bit;
        end;
        COLOR_GRAYSCALEALPHA :
        begin
          BPP := 2; // Grayscale, Alpha
          Bitmap.PixelFormat := pf32Bit;
        end
      else
        raise Exception.Create('Unsupported PNG ColorType = ' + IntToStr(Header.ColorType) + ' for BitDepth 8');
      end;
      LineSize := NativeInt(Header.Width) * BPP;
    end;
  else
    raise Exception.Create('Unsupported PNG (BitDepth = ' + IntToStr(Header.BitDepth) + ')');
  end;

  Bitmap.SetSize(Header.Width{$IFDEF INCLUDE_FILTER}+1{$ENDIF}, Header.Height);

  // Skip Chunk CRC
  Stream.Seek(4, soFromCurrent);
end;

procedure TPNGContext.ReadChunks;
begin
  // Next Chunk
  ReadChunk;
  // while not Image End
  while Chunk.Name <> Cardinal(IEND) do
  begin
    // Found Image Data
    if Chunk.Name = Cardinal(IDAT) then
    begin
      LoadIDAT;
      // don't need to parse the remaining chunks
      Break;
    end;
    // Found Image Palette
    if Chunk.Name = Cardinal(PLTE) then
    begin
      if ((Chunk.Size mod 3) <> 0)  or (Chunk.Size > 3 * 256) then
        raise Exception.Create('Invalid PLTE chunk');
      SetLength(Palette, Chunk.Size div 3);
      Stream.ReadBuffer(Palette[0], Chunk.Size);
      Chunk.Size := 0; // consumed
    end;
    // skip unsupported Chunk + CRC
    Stream.Seek(Chunk.Size + 4, soCurrent);
    // Next Chunk
    ReadChunk;
  end;
end;

procedure TPNGContext.FillZStream;
begin
  SetLength(zData, Chunk.Size);
  Stream.Read(zData[0], Chunk.Size);
  Stream.Seek(4, soFromCurrent); // skip CRC

  zStream.next_in := Pointer(zData);
  zStream.avail_in := Chunk.Size;
end;

procedure TPNGContext.LoadIDAT;
var
  ImageSize: NativeInt;
  Error    : Integer;
begin
  FillChar(zstream, SizeOf(zstream), 0);
  FillZStream;

  ImageSize := (LineSize + 1) * NativeInt(Header.Height);
  assert(ImageSize > 0);
  SetLength(Image, ImageSize);

  zStream.next_out := Pointer(Image);
  zStream.avail_out := ImageSize;

  Error := InflateInit(zstream);
  if Error < 0 then
    raise Exception.Create('ZLib error #' + IntToStr(Error));
  try
    Error := inflate(zstream, Z_FINISH);
    while Error <> Z_STREAM_END do
    begin
      if (Error = Z_BUF_ERROR) and (zStream.avail_in = 0) then
      begin
        // need the next IDAT chunk
        ReadChunk;
        assert(Chunk.Size > 0);
        if Chunk.Name <> Cardinal(IDAT) then
          raise Exception.Create('Out of IDAT chunk');
        FillZStream;
        Error := inflate(zstream, Z_FINISH);
      end else begin
        raise Exception.Create('ZLib error #' + IntToStr(Error));
      end;
    end;
  finally
    inflateEnd(zstream);
  end;
  assert(zStream.avail_in = 0);
  assert(zStream.avail_out = 0);
  zData := nil;

  case Header.BitDepth of
    1: ImageToMonoChrome;
    8: ImageToDIB;
  end;

end;

procedure LoadPNG(AStream: TStream; ABitmap: TBitmap);
var
  Context: TPNGContext;
begin
  Context.Stream := AStream;
  Context.Bitmap := ABitmap;
  Context.ReadHeader;
  Context.ReadChunks;
end;

end.