unit Main;
{
  (c)2017 Execute SARL
  http://www.execute.fr
}
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Execute.DesktopDuplicationAPI, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Timer1: TTimer;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Déclarations privées }
    FDuplication: TDesktopDuplicationWrapper;
    FBitmap: TBitmap;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FDuplication := TDesktopDuplicationWrapper.Create;
  Timer1.Enabled := FDuplication.Error = 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FBitmap.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  Index : Integer;
begin
  if FDuplication.GetFrame then
  begin
    FDuplication.DrawFrame(FBitmap);

    // draw Dirty rectangles
    with FBitmap.Canvas do
    begin
      Pen.Color := clRed;
      Brush.Style := bsClear;
      for Index := 0 to FDuplication.DirtyCount - 1 do
      begin
      {$POINTERMATH ON}
        with FDuplication.DirtyRects[Index] do
          Rectangle(Left, Top, Right, Bottom);
      end;
    end;

    Image1.Picture.Graphic := FBitmap;

    Memo1.Lines.Add(Format('Moves %d, Dirty %d', [FDuplication.MoveCount, FDuplication.DirtyCount]));
  end else begin
    Memo1.Lines.Add('no frame ' + IntToHex(FDuplication.Error));
  end;
end;

end.
