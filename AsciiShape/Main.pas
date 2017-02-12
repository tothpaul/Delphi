unit Main;
//
interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.ShellAPI,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Execute.AsciiShapes;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    rgAntialiaze: TRadioGroup;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure Memo1Change(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure ComboBox1Click(Sender: TObject);
  private
    { Déclarations privées }
    procedure DrawShape(const Shape: TAsciiShape; Image: TImage);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


function Merge(c1, c2, c3, c4: Cardinal): Cardinal;
var
 r, g, b, a: Cardinal;
begin
  a := (c1 shr 24) and $ff + (c2 shr 24) and $ff + (c3 shr 24) and $ff + (c4 shr 24) and $ff;
  r := (c1 shr 16) and $ff + (c2 shr 16) and $ff + (c3 shr 16) and $ff + (c4 shr 16) and $ff;
  g := (c1 shr  8) and $ff + (c2 shr  8) and $ff + (c3 shr  8) and $ff + (c4 shr  8) and $ff;
  b := (c1       ) and $ff + (c2       ) and $ff + (c3       ) and $ff + (c4       ) and $ff;
  Result := (a shr 2) shl 24
          + (r shr 2) shl 16
          + (g shr 2) shl  8
          + (b shr 2);
end;

procedure AntiAliaze(Bmp: TBitmap);
type
  TPair = record
    c1, c2 : Cardinal;
  end;
var
  w,h:Integer;
  x,y: Integer;
  p1 : PCardinal;
  p2 : ^TPair;
  p3 : ^TPair;
begin
  w := Bmp.Width div 2;
  h := Bmp.Height div 2;
  for y := 0 to h - 1 do
  begin
    p1 := Bmp.ScanLine[y];
    p2 := Bmp.ScanLine[2 * y];
    p3 := Bmp.ScanLine[2 * y + 1];
    for x:= 0 to w - 1 do
    begin
      p1^ := Merge(p2.c1, p2.c2, p3.c1, p3.c2);
      Inc(p1);
      Inc(p2);
      Inc(p3);
    end;
  end;
  Bmp.SetSize(w, h);
end;

procedure TForm1.ComboBox1Click(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
    0: Memo1.Lines.Text :=
        '1 : : : : : : : : : : 2'#13+
        ': : : : : : : : : : : :'#13+
        ': : 5 : : : : : : 5 : :'#13+
        ': : : : : : : : : : : :'#13+
        ': : 6 : : : : : : 6 : :'#13+
        ': : : : : : : : : : : :'#13+
        ': : 7 : : : : : : 7 : :'#13+
        ': : : : : : : : : : : :'#13+
        ': : : 8 : : : : 8 : : :'#13+
        ': : : : : : : : : : : :'#13+
        '4 : : : : : : : : : : 3'#13+
        '$0,-1'; // Pen = Black, Brush is empty
    1: Memo1.Lines.Text :=
        '1 : : : : : : : : : : 2'#13+
        ': : : : : : : : : : : :'#13+
        ': : 5 5 : : : : 6 8 : :'#13+
        ': : : : : : : : : : : :'#13+
        ': : a : b : : e : f : :'#13+
        ': : : : : : : : : : : :'#13+
        ': : : : c : : d : : : :'#13+
        ': : : : : : : : : : : :'#13+
        ': : h : : : : : : g : :'#13+
        ': : : : : : : : : : : :'#13+
        '4 : : : : : : : : : : 3'#13+
        '$0,-1'#13+ // Pen = Black, Brush is empty for the first shape (1234)
        '$0';       // Pen & Brush are black for the remaining shapes
    2: Memo1.Lines.Text :=
        ': : : : : : : : : : : :'#13+
        ': : 2 : : : : : : : 4 :'#13+
        ': : : : : : 1 : : : : :'#13+
        ': : : : : : : : : : : :'#13+
        ': : : : : : : : : : : :'#13+
        ': : 1 : : : : : : : 1 :'#13+
        ': : : : : : : : : : : :'#13+
        ': : : : : : : : : : : :'#13+
        ': : : : : : : : : : : :'#13+
        ': : : : : : 1 : : : : :'#13+
        ': : : : : : : : : : : :'#13;  // Defaut pen & brush are black
     3: Memo1.Lines.Text :=
        ': : : : 3 : : : : : : : : : : : : :'#13+
        ': : : : : : : : : 7 : : : : : : : :'#13+
        ': : : : : : : : : : : : : : : : : :'#13+
        ': : : 1 3 : : 1 : : : : : : : : : :'#13+
        '5 : : 5 : : : : : : : : : : : B : :'#13+
        ': : : : : 2 : : : 7 : : : : 2 : : :'#13+
        ': : : : : : : : : : : : : : : : : :'#13+
        ': : : 1 : : : 1 : : : : : : : : : :'#13+
        ': : : : : : : : : : B : 9 : : : : :'#13+
        ': 8 : : : 8 : : : : : : : : : : : :'#13+
        ': : : : : : : : : : : : : : : : : :'#13+
        ': : : : : : : C : : : : : : : : : :'#13+
        ': : : : : : : : : : : : : : : : : 9'#13+
        ': : : : : : : A : : : : : : : : : :'#13+
        ': : : : : 2 : : : : : : : : 2 : : :'#13+
        ': : : : C : : : : : : : : : : : : :'#13+
        ': : : : : : : : : : : : : : : : : :'#13+
        ': : : : : : : : : : : A : : : : : :';
     4: Memo1.Lines.Text :=
        '. . . 1 1 1 1 1 . . .'#13+
        '. . 1 . . . . . 1 . .'#13+
        '. 1 . . . . . . . 1 .'#13+
        '1 . . 3 . . . 2 . . 1'#13+
        '1 . . . . . . . . . 1'#13+
        '1 . . . . . . . . . 1'#13+
        '1 . . . . . . . . . 1'#13+
        '1 . . 2 . . . 3 . . 1'#13+
        '. 1 . . . . . . . 1 .'#13+
        '. . 1 . . . . . 1 . .'#13+
        '. . . 1 1 1 1 1 . . .'#13+
        '$a0a0a0'#13+  // first Shape brush & pen
        '$ffffff';     // remaining brush & pen
     5: Memo1.Lines.Text :=
        '. . . . . . . . . . . . . . .'#13+
        '. . . . 1 . . . . . . 1 . . .'#13+
        '. . . . . . . . . . . . . . .'#13+
        '. . . . . . . . . . . . . . .'#13+
        '. . 3 . 1 . . . . . . 1 . 4 .'#13+
        '. . . . . . . . . . . . . . .'#13+
        '. . . . . . A . . A . . . . .'#13+
        '. . . . 1 . . . . . . 1 . . .'#13+
        '. . . . . . . C D . . . . . .'#13+
        '. . . . . . A . . A . . . . .'#13+
        '. . . . . . . . . . . . . . .'#13+
        '. . . . . . . B E . . . . . .'#13+
        '. . . . . . . . . . . . . . .'#13+
        '. . 6 . . . . . . . . . . 5 .'#13+
        '$0,-1'#13+   // first Pen black without brush
        '$0'#13+      // black shape
        '$ffffff';    // remaining shapes are white
     6: Memo1.Lines.Text :=
        '1 . . . . . . . a . . . . . . . 1'#13+
        '. . . . . 6 . . . . . 5 . . . . .'#13+
        '. . . 7 . . . . . . . . . 4 . . .'#13+
        '. . . . . . . . . . . . . . . . .'#13+
        '. . . . . . . . . . . . . . . . .'#13+
        '. 8 . . . . . . c . . . . . . 3 .'#13+
        '. . . . . . . . . . . . . . . . .'#13+
        '. . . . . . . . d . . . . . . . .'#13+
        'b 9 . . . c . d . d . c . . . 2 b'#13+
        'a . . . . . . . d . . . . . . . a'#13+
        '. . . . . . . . . . . . . . . . .'#13+
        '. . . . . . . . c . . . . . . . .'#13+
        '. . . . . . . . . . . . . . . . .'#13+
        '. . . . . . . . . . . . . . . . .'#13+
        '. . . . . . . . . . . . . . . . .'#13+
        '. . . . . . . . . . . . . . . . .'#13+
        '1 . . . . . . . a . . . . . . . 1'#13+
        '$0,$ffffff'#13+ // white disk with a black border "1"
        '$ff'#13+        // red polygone for the half top of the ball "23456789"
        '$0,-1'#13+      // black circle around the ball "a"
        '$0,$ffffff'#13+ // horizontal blakc line "bb"
        ''#13+           // white disk "c"
        '$0';            // black disk "d"

  end;
end;

procedure TForm1.DrawShape(const Shape: TAsciiShape; Image: TImage);
var
  Zoom  : Integer;
  Bmp   : TBitmap;
begin
  Bmp := TBitmap.Create;
  try

    Zoom := 1 shl rgAntialiaze.ItemIndex;

    if Zoom > 1 then
    begin
      Bmp.PixelFormat := pf32Bit;
      Bmp.SetSize(Image.Width * Zoom, Image.Height * Zoom);
    end else begin
      Bmp.SetSize(Image.Width, Image.Height);
    end;

    Bmp.Canvas.Brush.Color := clBtnFace;
    Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));

    Shape.Draw(0, 0, Bmp.Canvas, Bmp.Width, Bmp.Height);

    while Zoom > 1 do
    begin
      Antialiaze(Bmp);
      Zoom := Zoom div 2;
    end;

    Image.Picture.Graphic := Bmp;

  finally
    Bmp.Free;
  end;
end;

procedure TForm1.Label1Click(Sender: TObject);
begin
  ShellExecute(Handle, nil, 'http://cocoamine.net/blog/2015/03/20/replacing-photoshop-with-nsstring/', nil, nil, SW_SHOW);
end;

procedure TForm1.Memo1Change(Sender: TObject);
var
  Shape : TAsciiShape;
begin
  Shape := TAsciiShape.Create;
  try
    Shape.LoadFromStrings(Memo1.Lines);
    DrawShape(Shape, Image1);
    DrawShape(Shape, Image2);
    DrawShape(Shape, Image3);
    DrawShape(Shape, Image4);
    DrawShape(Shape, Image5);
    DrawShape(Shape, Image6);
    DrawShape(Shape, Image7);
    DrawShape(Shape, Image8);
  finally
    Shape.Free;
  end;
end;

end.
