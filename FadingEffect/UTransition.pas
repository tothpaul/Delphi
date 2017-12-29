unit UTransition;

{
  (c)2017 Execute SARL
  <contact@execute.fr>

  Transition effet for two TWinControl on the same parent (Align = alClient)

}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TTransition = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FadeOut(Sender: TObject);
  private
  const
    SPEED = 20; // ms
    SLIDE = 10; // px
  var
    { Déclarations privées }
    FForm    : TForm;
    FControl1: TWinControl;
    FControl2: TWinControl;
    FImage   : TBitmap;
    FPanel   : TBitmap;
    FRect    : TRect;
    FOpacity : Byte;
    FPosition: Integer;
    procedure WMEraseBkGnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure FadeIn(Sender: TObject);
  public
    { Déclarations publiques }
    procedure Execute(Form: TForm; Control1, Control2: TWinControl);
  end;

var
  Transition: TTransition;

implementation

{$R *.dfm}

// take a picture of a Control
procedure Capture(Control: TWinControl; Bitmap: TBitmap);
var
  DC: HDC;
begin
  Bitmap.SetSize(Control.Width, Control.Height);
  DC := GetWindowDC(Control.Handle);
  try
    BitBlt(Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, DC, 0, 0, SRCCOPY);
  finally
    ReleaseDC(Control.Handle, DC);
  end;
end;

{ TTransition }

procedure TTransition.FormCreate(Sender: TObject);
begin
  FImage := TBitmap.Create; // Picture of the parent Form
  FPanel := TBitmap.Create; // Picture of the controls
end;

procedure TTransition.FormDestroy(Sender: TObject);
begin
  FImage.Free;
  FPanel.Free;
end;

procedure TTransition.Execute(Form: TForm; Control1, Control2: TWinControl);
begin
  FForm := Form;
  FControl1 := Control1;
  FControl2 := Control2;
  // take a picture of the actual form
  Capture(FForm, FImage);
  // place this Transition form on top of it
  SetWindowPos(Handle, HWND_TOPMOST, FForm.Left, FForm.Top, FForm.Width, FForm.Height, SWP_NOACTIVATE or SWP_SHOWWINDOW);
  Visible := True;
  // get Control1 position in the form
  FRect := FControl1.ClientRect;
  FRect.TopLeft := ScreenToClient(FControl1.ClientToScreen(FRect.TopLeft));
  FRect.BottomRight := ScreenToClient(FControl1.ClientToScreen(FRect.BottomRight));
  // copy it's picture
  FPanel.SetSize(FRect.Width, FRect.Height);
  FPanel.Canvas.CopyRect(TRect.Create(0, 0, FRect.Width, FRect.Height), FImage.Canvas, FRect);
  // start fade out
  FOpacity := 255;
  FPosition := 0;
  Timer1.OnTimer := FadeOut;
  Timer1.Enabled := True;
end;

procedure TTransition.WMEraseBkGnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TTransition.FormPaint(Sender: TObject);
var
  BF: TBlendFunction;
  x1: Integer;
  x2: Integer;
  w : Integer;
begin
// offline Form picture
  with FImage.Canvas do
  begin
  // clear the control rectangle
    Brush.Color := clBtnFace;
    FillRect(FRect);
  // redraw the control at a new position
//    Draw(FRect.Left + FPosition, FRect.Top, FPanel, FOpacity);
    BF.BlendOp := AC_SRC_OVER;
    BF.BlendFlags := 0;
    BF.SourceConstantAlpha := FOpacity;
    BF.AlphaFormat := 0;
    if FPosition >= 0 then
    begin
      x1 := FRect.Left + FPosition;
      x2 := 0;
      w := FRect.Width - FPosition;
    end else begin
      x1 := 0;
      x2 := - FPosition;
      w := FRect.Width + FPosition;
    end;
    Winapi.Windows.AlphaBlend(Handle, x1, FRect.Top, w, FRect.Height,
          FPanel.Canvas.Handle, x2, 0, w, FRect.Height, BF);
  end;
// draw the updated picture on the screen
  Canvas.Draw(0, 0, FImage);
end;

procedure TTransition.FadeOut(Sender: TObject);
begin
// first time, swap the two controls
  if FPosition = 0 then
  begin
    FControl1.Hide;
    FControl2.Show;
  end;
// need to fade out
  if FOpacity > SPEED then
  begin
    Inc(FPosition, SLIDE);
    Dec(FOpacity, SPEED);
    Paint;
  end else begin
// it's time to show the new control (it is now visibile)
    Capture(FForm, FImage);
// get the picture of the new control
    FPanel.Canvas.CopyRect(TRect.Create(0, 0, FRect.Width, FRect.Height), FImage.Canvas, FRect);
// it's time to fade in
    //FPosition := - FPosition;
    Timer1.OnTimer := FadeIn;
  end;
end;

procedure TTransition.FadeIn(Sender: TObject);
begin
// not done
  if FOpacity < 255 - SPEED then
  begin
    Dec(FPosition, SLIDE);
    Inc(FOpacity, SPEED);
    Paint;
  end else begin
// transition done
    Timer1.Enabled := False;
    Hide;
  end;
end;


end.
