unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.StrUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
  Execute.ComboEdit;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    Memo1: TMemo;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    { Déclarations privées }
    FComboEdit: TComboEdit;
    procedure OnDropDown(Sender: TObject);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FComboEdit := TComboEdit.Create(Edit1, OnDropDown);
end;

procedure TForm1.OnDropDown(Sender: TObject);
var
  Str  : string;
  Value: string;
  Index: Integer;
  Comp : Boolean;
begin
  Str := Edit1.Text;
  with FComboEdit.Items do
  begin
    Clear;
    for Index := 0 to Memo1.Lines.Count - 1 do
    begin
      Value := Memo1.Lines[Index];
      if CheckBox1.Checked then
        Comp := ContainsText(Value, Str)
      else
        Comp := AnsiStartsText(Str, Value);
      if Comp then
        Add(Value);
    end;
  end;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  FComboEdit.Timer.Interval := SpinEdit1.Value;
end;

end.
