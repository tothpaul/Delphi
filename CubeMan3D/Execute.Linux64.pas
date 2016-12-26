unit Execute.Linux64;

{
   Linux OpenGL Window for Delphi <no_name>

   (c)2016 Execute SARL  <contact@execute.fr>

   http://www.execute.fr

}

interface

type
  GLenum     = Cardinal;
  GLint      = Integer;
  GLfloat    = Single;
  GLbitfield = Cardinal;
  GLclampf   = Single;
  GLsizei    = Integer;
  PGLvoid    = Pointer;

  PGLfloat =  ^GLFloat;

  TXWindow = class
  private
    Handle : THandle;
    FGL    : THandle;
    FWidth : Integer;
    FHeight: Integer;
  protected
    procedure SetupGL; virtual; abstract;
    procedure OnResize; virtual; abstract;
    procedure OnIdle; virtual; abstract;
    procedure OnPaint; virtual; abstract;
  public
    constructor Create(AWidth, AHeight: Integer);
    procedure SwapBuffers;
    procedure Run;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

function GetTickCount: Cardinal; inline;

implementation

uses
  Posix.Unistd,
  Posix.SysTimes, Execute.CrossGL;

const
  libX = 'libX11.so.6';
  libGL = 'libGL.so';

  GLX_RGBA         = 4;
  GLX_DOUBLEBUFFER = 5;
  GLX_DEPTH_SIZE   = 12;

  OpenGL_ATTR: array[0..4] of GLint = (GLX_RGBA, GLX_DEPTH_SIZE, 24, GLX_DOUBLEBUFFER, 0);

  AllocNone = 0;
  AllocAll  = 1;

  InputOutput = 1;
  InputOnly = 2;

  CWBackPixmap = 1 shl 0;
  CWBackPixel = 1 shl 1;
  CWBorderPixmap = 1 shl 2;
  CWBorderPixel = 1 shl 3;
  CWBitGravity = 1 shl 4;
  CWWinGravity = 1 shl 5;
  CWBackingStore = 1 shl 6;
  CWBackingPlanes = 1 shl 7;
  CWBackingPixel = 1 shl 8;
  CWOverrideRedirect = 1 shl 9;
  CWSaveUnder = 1 shl 10;
  CWEventMask = 1 shl 11;
  CWDontPropagate = 1 shl 12;
  CWColormap = 1 shl 13;
  CWCursor = 1 shl 14;

  NoEventMask = 0;
  KeyPressMask = 1 shl 0;
  KeyReleaseMask = 1 shl 1;
  ButtonPressMask = 1 shl 2;
  ButtonReleaseMask = 1 shl 3;
  EnterWindowMask = 1 shl 4;
  LeaveWindowMask = 1 shl 5;
  PointerMotionMask = 1 shl 6;
  PointerMotionHintMask = 1 shl 7;
  Button1MotionMask = 1 shl 8;
  Button2MotionMask = 1 shl 9;
  Button3MotionMask = 1 shl 10;
  Button4MotionMask = 1 shl 11;
  Button5MotionMask = 1 shl 12;
  ButtonMotionMask = 1 shl 13;
  KeymapStateMask = 1 shl 14;
  ExposureMask = 1 shl 15;
  VisibilityChangeMask = 1 shl 16;
  StructureNotifyMask = 1 shl 17;
  ResizeRedirectMask = 1 shl 18;
  SubstructureNotifyMask = 1 shl 19;
  SubstructureRedirectMask = 1 shl 20;
  FocusChangeMask = 1 shl 21;
  PropertyChangeMask = 1 shl 22;
  ColormapChangeMask = 1 shl 23;
  OwnerGrabButtonMask = 1 shl 24;

  KeyPress = 2;
  KeyRelease = 3;
  ButtonPress = 4;
  ButtonRelease = 5;
  MotionNotify = 6;
  EnterNotify = 7;
  LeaveNotify = 8;
  FocusIn = 9;
  FocusOut = 10;
  KeymapNotify = 11;
  Expose = 12;
  GraphicsExpose = 13;
  NoExpose = 14;
  VisibilityNotify = 15;
  CreateNotify = 16;
  DestroyNotify = 17;
  UnmapNotify = 18;
  MapNotify = 19;
  MapRequest = 20;
  ReparentNotify = 21;
  ConfigureNotify = 22;
  ConfigureRequest = 23;
  GravityNotify = 24;
  ResizeRequest = 25;
  CirculateNotify = 26;
  CirculateRequest = 27;
  PropertyNotify = 28;
  SelectionClear = 29;
  SelectionRequest = 30;
  SelectionNotify = 31;
  ColormapNotify = 32;
  ClientMessage = 33;
  MappingNotify = 34;
  LASTEvent = 35;

type
  Bool          = Integer;
  int           = Integer;
  unsigned_int  = Cardinal;
  long          = LongInt;
  unsigned_long = UInt64;

  Pint          = ^int;

  XID = unsigned_long;

  TDisplay   = THandle;
  TWindow    = THandle;
  TScreen    = THandle;
  GLXContext = THandle;

  VisualID = type unsigned_long;

  TVisual = record
	  ext_data     : Pointer;
    visualid     : VisualID;
    c_class      : int;
    red_mask     : unsigned_long;
    green_mask   : unsigned_long;
    blue_mask    : unsigned_long;
	  bits_per_rgb : int;
	  map_entries  : int;
  end;
  PVisual = ^TVisual;

  TXVisualInfo = record
    visual       : PVisual;  { hook for extension to hang data  }
    visualid     : VisualID;   { visual id of this visual  }
    screen       : int;
    depth        : int;
    c_class      : int;
    red_mask     : unsigned_long;
    green_mask   : unsigned_long;
    blue_mask    : unsigned_long;
    colormap_size: int;
    bits_per_rgb : int;
  end;
  PXVisualInfo = ^TXVisualInfo;

  TPixmap   = type XID;
  TColormap = type XID;
  TCursor   = type XID;

  // Xlib.h
  TXSetWindowAttributes = record
    background_pixmap    : TPixmap;        // background or None or ParentRelative
    background_pixel     : unsigned_long;  // background pixel
    border_pixmap        : TPixmap;        // border of the window
    border_pixel         : unsigned_long;  // border pixel value
    bit_gravity          : int;            // one of bit gravity values
    win_gravity          : int;            // one of the window gravity values
    backing_store        : int;            // NotUseful, WhenMapped, Always
    backing_planes       : unsigned_long;  // planes to be preseved if possible
    backing_pixel        : unsigned_long;  // value to use in restoring planes
    save_under           : Bool;           // should bits under be saved? (popups)
    event_mask           : long;           // set of events that should be saved
    do_not_propagate_mask: long;           // set of events that should not propagate
    override_redirect    : Bool;           // boolean value for override-redirect
    colormap             : TColormap;      // color map to be associated with window
    cursor               : TCursor;        // cursor to be displayed (or None)
  end;
  PXSetWindowAttributes = ^TXSetWindowAttributes;

  TXEvent = record
  case Integer of
     0: (xtype: int);
    35: (pad  : array[0..23] of long);
  end;
  PXEvent = ^TXEvent;

  TXWindowAttributes = record
    x, y                 : int;		        // location of window
    width, height        : int;       	  // width and height of window
    border_width         : int;	          // border width of window
    depth                : int;           // depth of window
    visual               : PVisual;       // the associated visual structure
    root                 : TWindow;       // root of screen containing window
    c_class              : int;		        // C++ InputOutput, InputOnly
    bit_gravity          : int;		        // one of bit gravity values
    win_gravity          : int;		        // one of the window gravity values
    backing_store        : int;	        	// NotUseful, WhenMapped, Always
    backing_planes       : unsigned_long; // planes to be preserved if possible
    backing_pixel        : unsigned_long; // value to be used when restoring planes
    save_under           : Bool;		      // boolean, should bits under be saved?
    colormap             : TColormap;		  // color map to be associated with window
    map_installed        : Bool;	       	// boolean, is color map currently installed
    map_state            : int;		        // IsUnmapped, IsUnviewable, IsViewable
    all_event_masks	     : long;          // set of events all people have interest in
    your_event_mask      : long;          // my event mask
    do_not_propagate_mask: long;          // set of events that should not propagate
    override_redirect    : Bool;          // boolean value for override-redirect
    screen               : TScreen;	      // back pointer to correct screen
  end;
  PXWindowAttributes = ^TXWindowAttributes;

  TStatus = type int;

function XOpenDisplay(display_name: PUTF8Char): TDisplay; cdecl; external libX;
function XDefaultRootWindow(display: TDisplay): TWindow; cdecl; external libX;
function XCreateColormap(display: TDisplay; w: TWindow; visual: PVisual; alloc: Integer): TColormap; cdecl; external libX;
function XCreateWindow(
  display: TDisplay; parent: TWindow;
  x, y: int; width, height, border_width: unsigned_int; depth: int;
  c_class: unsigned_int; visual: PVisual; valuemask: unsigned_long;
  Attributes: PXSetWindowAttributes): TWindow; cdecl; external libX;
function XMapWindow(display: TDisplay; w: TWindow): int; cdecl; external libX;
function XStoreName(display: TDisplay; w: TWindow; window_name: PUTF8Char): int; cdecl; external libX;
function XPending(Display: TDisplay): int; cdecl; external libX;
function XNextEvent(Display: TDisplay; EventReturn: PXEvent): Integer; cdecl; external libX;
function XGetWindowAttributes(display: TDisplay; w: TWindow;
  WindowAttributesReturn: PXWindowAttributes): TStatus; cdecl; external libX;
function XDestroyWindow(Display: TDisplay; W: TWindow): int; cdecl; external libX;
function XCloseDisplay(Display: TDisplay): Integer; cdecl; external libX;

function glXChooseVisual(dpy: TDisplay; screen: int; attribList: Pint): PXVisualInfo; cdecl; external libGL;
function glXCreateContext(dpy: TDisplay; vis: PXVisualInfo; shareList: GLXContext; direct: Bool): GLXContext; cdecl; external libGL;
function glXMakeCurrent(dpy: TDisplay; drawable: int; ctx: GLXContext): Integer; cdecl; external libGL;
procedure glXSwapBuffers(dpy: TDisplay; drawable: int); cdecl; external libGL;
procedure glXDestroyContext(dpy: TDisplay; ctx: GLXContext); cdecl; external libGL;

var
  Display : TDisplay;
  Visual  : PXVisualInfo;
  ColorMap: TColorMap;

function GetTickCount: Cardinal; inline;
var
  t: tms;
begin
  Result := Cardinal(Int64(Cardinal(times(t)) * 1000) div sysconf(_SC_CLK_TCK));
end;

{ TXWindow }

constructor TXWindow.Create(AWidth, AHeight: Integer);
var
  swa: TXSetWindowAttributes;
begin
  swa.colormap := ColorMap;
  swa.event_mask := ExposureMask or KeyPressMask;

  Handle := XCreateWindow(Display, XDefaultRootWindow(Display), 0, 0, AWidth, AHeight, 0, Visual.depth, InputOutput, Visual.visual, CWColormap or CWEventMask, @swa);

  XMapWindow(Display, Handle);

  XStoreName(Display, Handle, 'CrossGL by Execute');

  FGL := glXCreateContext(Display, Visual, 0, GL_TRUE);

  glXMakeCurrent(Display, Handle, FGL);
end;

procedure TXWindow.Run;
var
  xev: TXEvent;
  gwa: TXWindowAttributes;
begin
  SetupGL();
  repeat
    xev.xtype := 0;
    while XPending(display) <> 0 do
    begin
      XNextEvent(Display, @xev);
      if(xev.xtype = Expose) then
      begin
        XGetWindowAttributes(Display, Handle, @gwa);
        if (FWidth <> gwa.width) or (FHeight <> gwa.height) then
        begin
          FWidth := gwa.width;
          FHeight := gwa.height;
          OnResize();
        end;
      end;
    end;
    OnIdle();
  until xev.xtype = KeyPress;
  glXMakeCurrent(Display, Handle, 0);
  glXDestroyContext(Display, FGL);
  XDestroyWindow(Display, Handle);
  XCloseDisplay(Display);
end;

procedure TXWindow.SwapBuffers;
begin
  glXSwapBuffers(Display, Handle);
end;

initialization
  Display := XOpenDisplay(nil);
  Assert(Display <> 0);
  Visual := glXChooseVisual(Display, 0, @OpenGL_ATTR[0]);
  Assert(Visual <> nil);
  ColorMap := XCreateColormap(Display, XDefaultRootWindow(Display), Visual.visual, AllocNone);
  Assert(ColorMap <> 0);
end.