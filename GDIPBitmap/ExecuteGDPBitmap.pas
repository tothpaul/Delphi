unit ExecuteGDPBitmap;

{
   Delphi 5-6-7 version of Delphi Tokyo Execute.GDIPBitmap unit
   (c)2017 Execute SARL
}

interface

uses
  Windows, ActiveX, Classes, Graphics;

function GDIPLoadBitmapFromStream(Bitmap: TBitmap; Stream: TStream): Boolean;

implementation

const
  GDIP = 'gdiplus.dll';

type
  TGdiplusStartupInput = packed record
    GdiplusVersion          : Cardinal;       // Must be 1
    DebugEventCallback      : Pointer;        // Ignored on free builds
    SuppressBackgroundThread: BOOL;           // FALSE unless you're prepared to call
                                              // the hook/unhook functions properly
    SuppressExternalCodecs  : BOOL;           // FALSE unless you want GDI+ only to use
  end;

  TStatus = (
    Ok,
    GenericError,
    InvalidParameter,
    OutOfMemory,
    ObjectBusy,
    InsufficientBuffer,
    NotImplemented,
    Win32Error,
    WrongState,
    Aborted,
    FileNotFound,
    ValueOverflow,
    AccessDenied,
    UnknownImageFormat,
    FontFamilyNotFound,
    FontStyleNotFound,
    NotTrueTypeFont,
    UnsupportedGdiPlusVersion,
    GdiplusNotInitialized,
    PropertyNotFound,
    PropertyNotSupported,
    ProfileNotFound
  );

  ARGB = Cardinal;

  TGpBitmap = record
    Handle: Pointer;
  end;

function GdiplusStartup(out token: ULONG; const input: TGdiplusStartupInput; output: Pointer): TStatus; stdcall; external GDIP;
function GdipCreateBitmapFromStream(stream: IStream; out image: TGpBitmap): TStatus; stdcall; external GDIP;
function GdipCreateHBITMAPFromBitmap(const Bitmap: TGpBitmap; var Handle: HBitmap; Background: ARGB): TStatus; stdcall; external GDIP;
function GdipDisposeImage(var Image: TGpBitmap): TStatus; stdcall; external GDIP;

var
  GdiplusToken: ULONG = 0;

function InitGDIPlus: Boolean;
var
  StartupInput: TGdiplusStartupInput;
begin
  if GdiplusToken = 0 then
  begin
    FillChar(StartupInput, SizeOf(StartupInput), 0);
    StartupInput.GdiplusVersion := 1;
    Result := GdiplusStartup(GdiplusToken, StartupInput, nil) = Ok;
  end else begin
    Result := True;
  end;
end;

function GDIPLoadBitmapFromStream(Bitmap: TBitmap; Stream: TStream): Boolean;
var
  GpBitmap: TGpBitmap;
  HBmp    : HBitmap;
begin
  Result := False;
  if not InitGDIPlus then
    Exit;
  if GdipCreateBitmapFromStream(TStreamAdapter.Create(Stream), GpBitmap) = Ok then
  begin
    if GdipCreateHBITMAPFromBitmap(GpBitmap, HBmp, $FFFFFFFF) = Ok then
    begin
      Bitmap.Handle := HBmp;
      Result := True;
    end;
    GdipDisposeImage(GpBitmap);
  end;
end;

end.
