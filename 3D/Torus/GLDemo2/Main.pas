unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.OpenGL, Winapi.OpenGLext,

  System.SysUtils, System.Variants, System.Classes, System.DateUtils,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  Execute.GLPanel;

type
  TForm5 = class(TForm)
    GLPanel1: TGLPanel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Déclarations privées }
    Title : string;
    Start : TDateTime;
    Frames: Integer;
    Shader    : Integer;
    resolution: Integer;
    rotation  : Integer;
    procedure GLSetup(Sender: TObject);
    procedure GLPaint(Sender: TObject);
  public
    { Déclarations publiques }
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

var
  phix, phiy, phiz: Byte;

  VertexShader : AnsiString =
    'void main() {'
  + ' gl_TexCoord[0] = gl_MultiTexCoord0;'
  + ' gl_Position = ftransform();'
  +'}';

 FragmentShader : AnsiString =

   '#version 120'#13#10+

   'uniform vec2 resolution;'+
   'uniform vec3 rotation;'+

   'void R(inout vec3 p) {'+
   '  vec3 c = vec3(cos(rotation.x), cos(rotation.y), cos(rotation.z));'+
   '  vec3 s = vec3(sin(rotation.x), sin(rotation.y), sin(rotation.z));'+
   '  mat3 r = mat3(1.0, 0.0, 0.0,' + // Rotate X
   '                0.0, c.x,-s.x,' +
   '                0.0, s.x, c.x)' +
   '         * mat3(c.y, 0.0, s.y,' + // Rotate Y
   '                0.0, 1.0, 0.0,' +
   '               -s.y, 0.0, c.y)' +
   '         * mat3(c.z,-s.z, 0.0,' + // Rotate Z
   '                s.z, c.z, 0.0,' +
   '                0.0, 0.0, 1.0);'+
   '  p = r * p;'+
   '}'+

   'void R45(inout vec3 p) {'+
   '  p = p.xzy;'+
   '}'+

   'float fdistance(vec3 p, out vec3 color) {'+

   '  R(p);'+

//   '  p.x = mod(p.x, 400)- 200;'+
//   '  p.y = mod(p.y, 400)- 200;'+
//   '  p.z = mod(p.z, 400)- 200;'+

   '  float d = length(vec2(length(p.xy) - 60, p.z)) - 28;'+
   '  color = vec3(0.0, 72.0, 32.0);'+

   '  R45(p);'+
   '  p.x += 60;'+

//   '  p *= 1.25;'+
   '  float d2 = length(vec2(length(p.xy) - 60, p.z)) - 28;'+

   '  if (d2 < d) {'+
   '    d = d2;' +
   '    color = vec3(0.0, 75.0, 112.0);'+
   '  }'+

   '  return d;'+
   '}'+

   'void main() {'+
   ' vec3 p = vec3(0.0,0.0,-800.0);'+
   ' vec3 n = vec3((gl_FragCoord.xy - (resolution/2.0)) * max(200.0/resolution.x, 320.0/resolution.y), 0) - p;'+
   ' float l = length(n);'+
   ' float f = 0.0;'+
   ' vec3 k = p;'+
   ' float d;'+
   ' vec3 c;'+
   ' for (int step = 0; step < 90; step++) {'+
   '  d = fdistance(k, c);'+
   '  if (d < 1.0) break;'+
   '  f += d;'+
   '  if (f > 2*l) break;'+
   '  k = p + n * (f/l);'+
   ' }'+
   ' if (d < 1.0) {'+
   '  k = normalize(k);'+
   '  f = 2*abs(k.x + k.y + k.z * 96);'+
   '  gl_FragColor = vec4((c.x + f)/256, (c.y + f/1.4)/256, (c.z + f/1.6)/256, 1);'+
   ' } else {'+
   '  f = gl_TexCoord[0].y * 200;'+
   '  if (f < 128) {'+
   '   gl_FragColor = vec4((2*f)/256, (40 + 2*f/ 1.4)/256, (80 + 2*f / 1.6)/256, 1);'+
   '  } else {'+
   '   f-=128;'+
   '   gl_FragColor = vec4((2*f)/256, (80 + 2*f / 1.4)/256, (40 + 2*f/ 1.6)/256, 1);'+
   '  }'+
   ' }'+
   '}';

procedure TForm5.FormCreate(Sender: TObject);
begin
  Title := Caption;
  Start := Now();
  GLPanel1.OnSetup := GLSetup;
  GLPanel1.OnPaint := GLPaint;
end;

procedure TForm5.GLSetup(Sender: TObject);
begin
  Shader := glxCompileProgram(VertexShader, FragmentShader);
  glUseProgram(Shader);
  resolution := glGetUniformLocation(Shader, 'resolution');
  rotation := glGetUniformLocation(Shader, 'rotation');
end;

procedure TForm5.Timer1Timer(Sender: TObject);
var
  Time: Int64;
begin
  Inc(phix, 1);
  Inc(phiy, 1);
  Inc(phiz, 2);
  if Timer1.Tag = 0 then
  begin
    Timer1.Tag := 1;
    InvalidateRect(GLPanel1.Handle, nil, False);
  end;
  Time := SecondsBetween(Now, Start);
  if Time > 0 then
  begin
    Caption := Title + ' [' + IntToStr(Frames div Time) + ' FPS]';
  end;
end;

procedure TForm5.GLPaint(Sender: TObject);
var
  w, h: Single;
begin
  Inc(Frames);
  w := GLPanel1.ClientWidth;
  h := GLPanel1.ClientHeight;

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(-w/2, w/2, +h/2, -h/2, 1, -1);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  glUniform2f(resolution, w, h);
  glUniform3f(rotation, phix*2*PI/255, phiy*2*PI/255, phiz*2*PI/255);

  glBegin(GL_QUADS);
    glTexCoord2f(0, 1); glVertex3f(-w/2, +h/2, 0);
    glTexCoord2f(0, 0); glVertex3f(-w/2, -h/2, 0);
    glTexCoord2f(1, 0); glVertex3f(+w/2, -h/2, 0);
    glTexCoord2f(1, 1); glVertex3f(+w/2, +h/2, 0);
  glEnd();

  Timer1.Tag := 0;
end;

end.
