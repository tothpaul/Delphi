unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.OpenGL, Winapi.OpenGLext,

  System.SysUtils, System.Variants, System.Classes,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ClipBrd,

  Execute.GLPanel;

type
  TMainForm = class(TForm)
    GLPanel1: TGLPanel;
    Memo1: TMemo;
    Splitter1: TSplitter;
    Memo2: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Timer1: TTimer;
    Button2: TButton;
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure GLPanel1Resize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLPanel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Button2Click(Sender: TObject);
  private
    { Déclarations privées }
    winW, winH : Single;
    FragmentShader: AnsiString;
    Shader        : Cardinal;
    Start         : Cardinal;
    Mouse         : TPoint;
    procedure Load;
    procedure GLSetup(Sender: TObject);
    procedure GLPaint(Sender: TObject);
    procedure SetResolution(x, y: Single);
    procedure SetGlobalTime(Time: Single);
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

var
  VertexShader : AnsiString =
  // ce shader rempli tout l'écran :)
  (*
    'attribute vec3 position;'
   +'void main(){'
   + 'gl_Position = vec4(position, 1.0);'
   +'}';
  *)

    'void main()'
  +'{'
  + 'gl_TexCoord[0] = gl_MultiTexCoord0;'
  + 'gl_Position = ftransform();'
  +'}';

  iResolution       : Integer; // vec3
  iGlobalTime       : Integer; // float
  iTimeDelta        : Integer; // float
  iFrame            : Integer; // int
  iChannelTime      : Integer; // float[4]
  iChannelResolution: Integer; // vec3[4]
  iMouse            : Integer; // vec4
  iChanel0          : Integer; // smaplerXX
  iChanel1          : Integer; // smaplerXX
  iChanel2          : Integer; // smaplerXX
  iChanel3          : Integer; // smaplerXX
  iDate             : Integer; // vec4
  iSampleRate       : Integer; // float

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Load();
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  Memo1.Text := Clipboard.AsText;
  Load();
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  GLPanel1.OnSetup := GLSetup;
  GLPanel1.OnPaint := GLPaint;
end;

procedure TMainForm.GLSetup(Sender: TObject);
begin
  Start := GetTickCount;
  GLPanel1Resize(Self);
  Load();
end;


procedure TMainForm.GLPaint(Sender: TObject);
begin
{ reset de la matrice de projection }
  glLoadIdentity;

  //glRotatef(getTickCount/100,1,1,1);

  glUseProgram(Shader);
  glColor3f(1,0,0);

  SetResolution(winW, winH);
  SetGlobalTime((GetTickCount - Start) / 1000);
  glUniform4f(iMouse, Mouse.X, Mouse.Y, 0 , 0);
//  SetCam(3.9489953517914,3.5295605659485,-1.437316775322);
//  if Start = 0 then
//    Start := GetTickCount;
//  SetTime((GetTickCount - Start) / 1000);
//  SetResolution(winW, winH);
//  glUniform2f(mouse, mx, my);

 { ICI ON DOIT EFFECTUER LE RENDU DE LA SCENE }
 //glTranslatef(0, 0, -1000);
 //glRotatef(GetTickCount/100, 1, 1 ,1);
  glColor3f(1, 1, 1);
  glBegin(GL_QUADS);
  glTexCoord2f(0, 1);
  glVertex2f(-winW/2, +winH/2);
  glTexCoord2f(0, 0);
  glVertex2f(-winW/2, -winH/2);
  glTexCoord2f(1, 0);
  glVertex2f(+winW/2, -winH/2);
  glTexCoord2f(1, 1);
  glVertex2f(+winW/2, +winH/2);
  glEnd();

  glUseProgram(0);

 Timer1.Tag := 0;
end;

procedure TMainForm.GLPanel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if  ssLeft in Shift then
  begin
    Mouse.X := x;
    Mouse.y := y;
  end;
end;

procedure TMainForm.GLPanel1Resize(Sender: TObject);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  winW := GLPanel1.ClientWidth;
  winH := GLPanel1.ClientHeight;
  glOrtho(-winW/2, winw/2, -winH/2, winH/2, 1, -1);
  glMatrixMode(GL_MODELVIEW);
//  setResolution(winW, winH);
end;

procedure TMainForm.Load;
begin
  FragmentShader :=
    '// Shader Inputs'#13#10 +
    'uniform vec3      iResolution;           // viewport resolution (in pixels)'#13#10 +
    'uniform float     iGlobalTime;           // shader playback time (in seconds)'#13#10 +
    '// uniform float     iTimeDelta;            // render time (in seconds)'#13#10 +
    '// uniform int       iFrame;                // shader playback frame'#13#10 +
    '// uniform float     iChannelTime[4];       // channel playback time (in seconds)'#13#10 +
    '// uniform vec3      iChannelResolution[4]; // channel resolution (in pixels)'#13#10 +
    'uniform vec4      iMouse;                // mouse pixel coords. xy: current (if MLB down), zw: click'#13#10 +
    '// uniform samplerXX iChannel0..3;          // input channel. XX = 2D/Cube'#13#10 +
    '// uniform vec4      iDate;                 // (year, month, day, time in seconds)'#13#10 +
    '// uniform float     iSampleRate;           // sound sample rate (i.e., 44100)'#13#10 +

    Memo1.Text +  #13#10 +

    'void main() {'#13#10 +
    ' mainImage(gl_FragColor, gl_FragCoord.xy);'#13#10 +
    '}';


  try
    if Shader <> 0 then
      glDeleteProgram(Shader);
    Shader := glxCompileProgram(VertexShader, FragmentShader);
    iResolution := glGetUniformLocation(Shader, 'iResolution');
    iGlobalTime := glGetUniformLocation(Shader, 'iGlobalTime');
    iMouse := glGetUniformLocation(Shader, 'iMouse');
  except
    on e: Exception do
      Memo2.Text := e.Message;
  end;
end;

procedure TMainForm.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F9 then
    Load();
end;

procedure TMainForm.SetGlobalTime(Time: Single);
begin
  glUniform1f(iGlobalTime, Time);
end;

procedure TMainForm.SetResolution(x, y: Single);
begin
  glUniform3f(iResolution, x, y, 1);
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  if Timer1.Tag = 0 then
  begin
    InvalidateRect(GLPanel1.Handle, nil, False);
    Timer1.Tag := 1;
  end;
end;

end.
