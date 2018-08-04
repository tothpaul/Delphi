unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.OpenGL,

  System.SysUtils, System.Variants, System.Classes, System.DateUtils,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TForm3 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
    fDC: THandle;
    fGL: THandle;
    Title : string;
    Start : TDateTime;
    Frames: Integer;
  public
    { Déclarations publiques }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

uses
  Execute.Trigo,
  Execute.Torus;

var
  Phix, Phiy, Phiz: Byte;

  LightPosition: array[0..3] of Single = (0, 1, 1, 0);

  LightDiffuse : array[0..3] of Single = (1, 1, 1, 1);
  LightAmbient : array[0..3] of Single = (0.5, 0.5, 0.5, 1);
  LightSpecular: array[0..3] of Single = (1, 1, 1, 1);

procedure TForm3.FormCreate(Sender: TObject);
var
  pfd: TPIXELFORMATDESCRIPTOR;
  pixelformat: Integer;
  rgb: cardinal;
begin
  Title := Caption;
  Start := Now();

  fDC := GetDC(Handle);
  if fDC = 0 then
    RaiseLastOSError;

  FillChar(pfd, SizeOf(pfd), 0);
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iLayerType := PFD_MAIN_PLANE;
  pfd.iPixelType := PFD_TYPE_RGBA;
  pfd.cColorBits := 32;
  pfd.cDepthBits := 24;

  pixelformat := ChoosePixelFormat(fDC, @pfd);
  if pixelformat = 0 then
    RaiseLastOSError;

  if not SetPixelFormat(fDC, pixelformat, @pfd) then
    RaiseLastOSError;

  fGL := wglCreateContext(fDC);

  wglMakeCurrent(fDC, fGL);

  rgb := ColorToRGB(Color);
  glClearColor((rgb and $FF) / 255, ((rgb shr 8) and $FF) / 255,
    ((rgb shr 16) and $FF) / 255, 0);

  glClearDepth(1);
  glEnable(GL_DEPTH_TEST);

  glEnable(GL_CULL_FACE);

  glEnable(GL_LIGHT0);
  glLightfv(GL_LIGHT0, GL_AMBIENT, @LightAmbient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @LightDiffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @LightSpecular);
  glEnable(GL_COLOR_MATERIAL);

  FormResize(Self);

  Phix := 32;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  wglMakeCurrent(fDC, 0);
  wglDeleteContext(fGL);
  ReleaseDC(Handle, fDC);
end;

procedure TForm3.FormResize(Sender: TObject);
begin
  glViewport(0, 0, ClientWidth, ClientHeight);
  InvalidateRect(Handle, nil, False);
  Start := Now();
  Frames := 0;
end;

procedure TForm3.Timer1Timer(Sender: TObject);
var
  Time: Int64;
begin
  Inc(phix, 1);
  Inc(phiy, 1);
  Inc(phiz, 2);

  if Timer1.Tag = 0 then
  begin
    Timer1.Tag := 1;
    InvalidateRect(Handle, nil, False);
  end;

  Time := SecondsBetween(Now, Start);
  if Time > 0 then
  begin
    Caption := Title + ' [' + IntToStr(Frames div Time) + ' FPS]';
  end;
end;

procedure TForm3.FormPaint(Sender: TObject);
var
  w, h, i: Integer;
  Index: Integer;
  nx, ny, nz: Single;
begin
  Inc(Frames);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  w := ClientWidth;
  h := ClientHeight;

  // 2D Rendering
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0.0, w, h, 0, -1, +1);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glDepthMask(GL_FALSE);
  glDisable(GL_CULL_FACE);
  glDisable(GL_LIGHTING);

  i := (127 * h) div 200;
  glBegin(GL_QUADS);
    glColor3f($00/255, $28/255, $50/255);
    glVertex2f(0, 0);
    glVertex2f(w, 0);
    glColor3f($FE/255, $DD/255, $EF/255);
    glVertex2f(w, i);
    glVertex2f(0, i);

    Inc(i);
    glColor3f($00/255, $50/255, $28/255);
    glVertex2f(0, i);
    glVertex2f(w, i);
    glColor3f($FE/255, $EF/255, $DD/255);
    glVertex2f(w, 2 * i);
    glVertex2f(0, 2 * i);
  glEnd();

//   3D Rendering
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(45, ClientWidth / ClientHeight, 0.1, 2000);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glDepthMask(GL_TRUE);
  glEnable(GL_CULL_FACE);

  glEnable(GL_LIGHTING);
  glLightfv(GL_LIGHT0, GL_POSITION, @LightPosition);

  glTranslatef(0, 0, -800);
  glRotatef(phix * 180/128, 1, 0, 0);
  glRotatef(phiy * 180/128, 0, 1, 0);
  glRotatef(phiz * 180/128, 0, 0, 1);

  glVertexPointer(3, GL_FLOAT, SizeOf(TVertex), @Vertices);
  glNormalPointer(GL_FLOAT, 0, @VNormals);

  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_NORMAL_ARRAY);

  glColor3f($20/255, $48/255, $70/255);
  glDrawElements(GL_TRIANGLES,3 * FaceCount, GL_UNSIGNED_INT, @Faces);

  glRotatef(90, 1, 0, 0);
  glTranslatef(160, 0, 0);
  glColor3f($20/255, $70/255, $48/255);
  glDrawElements(GL_TRIANGLES,3 * FaceCount, GL_UNSIGNED_INT, @Faces);

  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);

//  glDisable(GL_LIGHTING);
//  glColor3f(1, 0, 0);
//  glBegin(GL_LINES);
//  for Index := 0 to VertexCount - 1 do
//  begin
//    glVertex3fv(@Vertices[Index]);
//    with Vertices[Index] do
//    begin
//      glVertex3f(x + 20 * VNormals[Index].x, y + 20 * VNormals[Index].y, z + 20 * VNormals[Index].z);
//    end;
//  end;
//  glEnd();
//  glColor3f(0, 1, 0);
//  glBegin(GL_LINES);
//  for Index := 0 to FaceCount - 1 do
//  begin
//    with Faces[Index] do
//    begin
//      with Vertices[a] do
//      begin
//        nx := x;
//        ny := y;
//        nz := z;
//      end;
//      with Vertices[b] do
//      begin
//        nx := nx + x;
//        ny := ny + y;
//        nz := nz + z;
//      end;
//      with Vertices[c] do
//      begin
//        nx := nx + x;
//        ny := ny + y;
//        nz := nz + z;
//      end;
//      nx := nx / 3;
//      ny := ny / 3;
//      nz := nz / 3;
//      glVertex3f(nx, ny, nz);
//      with Vertices[a] do
//      begin
//        glVertex3f(nx + 20 * FNormals[Index].x, ny + 20 * FNormals[Index].y, nz + 20 * FNormals[Index].z);
//      end;
//    end;
//  end;
//  glEnd();

  glFlush;
  SwapBuffers(fDC);
  Timer1.Tag := 0;
end;

end.
