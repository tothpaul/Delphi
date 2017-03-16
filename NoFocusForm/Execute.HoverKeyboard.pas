unit Execute.HoverKeyboard;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  THoverKeyboard = class(TForm)
  private
    { Déclarations privées }
    FTextBox: TControl;
    procedure Button_Click(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    property TextBox: TControl read FTextBox write FTextBox;
  end;

var
  HoverKeyboard: THoverKeyboard;

implementation

{$R *.DFM}

procedure THoverKeyboard.Button_Click(Sender: TObject);
var
  Key: string;
begin
//  if Parent = nil then
//    ShowMessage('Parent is null')
//  else
//    ShowMessage('Parent = ' + Parent.Name);
  Key := TButton(Sender).Caption;
  if Key <> '' then
    Keybd_Event(Ord(Key[1]), 0, KEYEVENTF_KEYUP, 0);
end;

procedure THoverKeyboard.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := WS_THICKFRAME or WS_CHILD;
  Params.ExStyle := WS_EX_NOACTIVATE or WS_EX_TOOLWINDOW;
//  Params.WndParent := 0;
//  Params.X := Left;
//  Params.Y := Top;
end;

constructor THoverKeyboard.Create(AOwner: TComponent);
const
  Lines:array[0..3] of string = (
    '1234567890',
    'QWERTYUIOP',
    'ASDFGHJKL',
    'ZXCVBNM'
  );
var
  x, y  : Integer;
  line  : string;
  cur   : Char;
  button: TButton;
begin
  inherited;
  Visible := False;
  x := 0;
  y := 0;
  for line in Lines do
  begin
    for cur in line do
    begin
      button := TButton.Create(Self);
      button.SetBounds(x * 25, y * 25, 23, 23);
      button.Caption := Cur;
      button.OnClick := Button_Click;
      button.Parent := Self;
      Inc(x);
    end;
    x := 0;
    Inc(y);
  end;
  Width := 25 * 10;
  Height := 25 * 3;
end;

end.
