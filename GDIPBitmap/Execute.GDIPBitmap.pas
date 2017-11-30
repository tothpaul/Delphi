unit Execute.GDIPBitmap;

{
   GDI+ LoadFromStream helper for TBitmap (c)2017 Execute SARL
   http://www.execute.fr

}

interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.Classes,
  Vcl.Graphics;

type
  TBitmapHelper = class helper for TBitmap
  public
    function GDIPLoadFromStream(Stream: TStream): Boolean; // Loads BMP, JPG, PNG and TIF
  end;

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
  private
    Handle: Pointer;
  public
    function CreateFromStream(stream: IStream): TStatus; inline;
    function GetHBitmap(var Handle: HBitmap; Background: ARGB): TStatus; inline;
    function Free: TStatus; inline;
  end;

function GdiplusStartup(out token: ULONG; const input: TGdiplusStartupInput; output: Pointer): TStatus; stdcall; external GDIP;
function GdipCreateBitmapFromStream(stream: IStream; out image: TGpBitmap): TStatus; stdcall; external GDIP;
function GdipCreateHBITMAPFromBitmap(const Bitmap: TGpBitmap; var Handle: HBitmap; Background: ARGB): TStatus; stdcall; external GDIP;
function GdipDisposeImage(const Image: TGpBitmap): TStatus; stdcall; external GDIP;


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
    Result := GdiplusStartup(GdiplusToken, StartupInput, nil) = TStatus.Ok;
  end else begin
    Result := True;
  end;
end;

{ TGpBitmap }

function TGpBitmap.CreateFromStream(stream: IStream): TStatus;
begin
  Result := GdipCreateBitmapFromStream(stream, Self);
end;

function TGpBitmap.GetHBitmap(var Handle: HBitmap; Background: ARGB): TStatus;
begin
  Result := GdipCreateHBITMAPFromBitmap(Self, Handle, Background);
end;

function TGpBitmap.Free: TStatus;
begin
  Result := GdipDisposeImage(Self);
end;

{ TBitmapHelper }

function TBitmapHelper.GDIPLoadFromStream(Stream: TStream): Boolean;
var
  Bitmap : TGpBitmap;
  HBmp   : HBitmap;
begin
  Result := False;
  if not InitGDIPlus then
    Exit;
  if Bitmap.CreateFromStream(TStreamAdapter.Create(Stream)) = TStatus.Ok then
  begin
    if Bitmap.GetHBitmap(HBmp, $FFFFFFFF) = TStatus.Ok then
    begin
      Handle := HBmp;
      Result := True;
    end;
    Bitmap.Free;
  end;
end;

end.
