unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.WinXPickers;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Button1: TButton;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Edit1: TEdit;
    DatePicker1: TDatePicker;
    TimePicker1: TTimePicker;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  uTransition;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Button1.Tag = 1 then
    Transition.Execute(Self, Panel2, Panel1)
  else
    Transition.Execute(Self, Panel1, Panel2);
  Button1.Tag := 1 - Button1.Tag;
end;

end.
