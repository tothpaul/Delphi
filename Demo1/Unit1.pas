unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Execute.GraphicPanels;

type
  TForm1 = class(TForm)
    GraphicPanel1: TGraphicPanel;
    GraphicPanel2: TGraphicPanel;
    GraphicPanel3: TGraphicPanel;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
