unit Execute.GLPanel;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.OpenGL, Winapi.OpenGLext,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TGLPanel = class(TFrame)
  private
    { Déclarations privées }
    FDC     : THandle;
    FGL     : THandle;
    FOnSetup: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    procedure WMEraseBkGnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMPaint(var Msg: TMessage); message WM_PAINT;
    procedure ResizeGL;
  protected
    procedure CreateWnd; override;
    procedure Resize; override;
    procedure DestroyWnd; override;
  public
    { Déclarations publiques }
    property OnSetup:TNotifyEvent read FOnSetup write FOnSetup;
    property OnPaint:TNotifyEvent read FOnPaint write FOnPaint;
  end;

function glxCompileProgram(const VertexShader, FragmentShader: AnsiString): Cardinal;

implementation

{$R *.dfm}

function glxCompileShader(shaderType: Integer; const Source: AnsiString; var Shader: Cardinal): Boolean;
var
  Src: PAnsiChar;
  Len: Integer;
begin
  Shader := glCreateShader(shaderType);
  Src := PAnsiChar(Source);
  Len := Length(Source);
  glShaderSource(Shader, 1, @Src, @Len);
  glCompileShader(Shader);
  glGetShaderiv(Shader, GL_COMPILE_STATUS, @Len);
  Result := Len = GL_TRUE;
end;

function glxGetShaderError(shader: Cardinal): string;
var
  i: Integer;
  s: AnsiString;
begin
  glGetShaderiv(Shader, GL_INFO_LOG_LENGTH, @i);
  SetLength(s, i + 1);
  glGetShaderInfoLog(Shader, i, @i, @s[1]);
  Result := s;
end;

function glxLinkProgram(VertexShader, FragmentShader: Cardinal; var Pgm: Cardinal): Boolean;
var
  Err: Integer;
begin
  Pgm := glCreateProgram();
  glAttachShader(Pgm, VertexShader);
  glAttachShader(Pgm, FragmentShader);
  glLinkProgram(Pgm);
  glGetProgramiv(Pgm, GL_LINK_STATUS, @Err);
  Result := Err = GL_TRUE;
end;

function glxGetProgramError(pgm: Cardinal): string;
var
  i: Integer;
  s: AnsiString;
begin
  glGetProgramiv(Pgm, GL_INFO_LOG_LENGTH, @i);
  SetLength(s, i);
  glGetProgramInfoLog(Pgm, i, @i, @s[1]);
  Result := s;
end;

function glxCompileProgram(const VertexShader, FragmentShader: AnsiString): Cardinal;
var
  Vertex: Cardinal;
  Fragment: Cardinal;
begin
  if not glxCompileShader(GL_VERTEX_SHADER, VertexShader, Vertex) then
    raise Exception.Create(glxGetShaderError(Vertex));
  if not glxCompileShader(GL_FRAGMENT_SHADER, FragmentShader, Fragment) then
    raise Exception.Create(glxGetShaderError(Fragment));
  if not glxLinkProgram(Vertex, Fragment, Result) then
    raise Exception.Create(glxGetProgramError(Result));
  glDeleteShader(Vertex);
  glDeleteShader(Fragment);
end;

{ TGLPanel }

procedure TGLPanel.CreateWnd;
 var
  pfd: TPIXELFORMATDESCRIPTOR;
  pixelformat: Integer;
  Cl: TRGBQuad;
 begin
  inherited;
  FDC := GetDC(Handle);
  if FDC = 0 then
    Exit;
 // set pixel format
  FillChar(pfd, SizeOf(pfd), 0);
  pfd.nSize       := sizeof(pfd);
  pfd.nVersion    := 1;
  pfd.dwFlags     := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iLayerType  := PFD_MAIN_PLANE;
  pfd.iPixelType  := PFD_TYPE_RGBA;
  pfd.cColorBits  := 32;
  pfd.iLayerType  := PFD_MAIN_PLANE;
  pfd.cStencilBits:= 0;
  pixelformat := ChoosePixelFormat(FDC, @pfd);
  if PixelFormat = 0 then
    Exit;
  if not SetPixelFormat(FDC, pixelformat, @pfd) then
    Exit;
 // create OpenGL Context
  FGL := wglCreateContext(FDC);
 // select it
  wglMakeCurrent(FDC, FGL);
 // setup GL mode
 // setup the clear color
  Integer(cl) := ColorToRGB(Color);
  glClearColor(cl.rgbRed/255, cl.rgbGreen/255, cl.rgbBlue/255, 1);
 // setup the clear depth
  glClearDepth(1);

  InitOpenGLext;

  if Assigned(FOnSetup) then
    FOnSetup(Self);

  ResizeGL();

  if Assigned(OnResize) then
    OnResize(Self);
end;

procedure TGLPanel.DestroyWnd;
begin
  if FGL <> 0 then
  begin
    wglMakeCurrent(FDC, 0);
    wglDeleteContext(FGL);
    FGL := 0;
  end;
  if FDC <> 0 then
  begin
    DeleteDC(FDC);
    FDC := 0;
  end;
  inherited;
end;

procedure TGLPanel.Resize;
begin
  if FGL <> 0 then
  begin
    ResizeGL;
    InvalidateRect(Handle, nil, False);
  end;
  inherited;
end;

procedure TGLPanel.ResizeGL;
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(45, Width / Height, 1, 1000);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glViewport(0, 0, Width, Height);
end;

procedure TGLPanel.WMEraseBkGnd(var Msg: TMessage);
begin
  if FGL <> 0 then
    Msg.Result := 1;
end;

procedure TGLPanel.WMPaint(var Msg: TMessage);
begin
  if FGL <> 0 then
  begin
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    if Assigned(FOnPaint) then
      FOnPaint(Self);
    glFlush();
    SwapBuffers(FDC);
    ValidateRect(Handle, nil);
  end else begin
    inherited;
  end;
end;

end.
