unit Unit1;

{

  (c)2017 by Paul TOTH

   Attempt to translate to Delphi a .Net project found on codeproject.com

   https://www.codeproject.com/Articles/71808/Creating-a-Form-That-Doesn-t-Take-Focus

   -> numeric keyboard is not implementated
   -> WS_EX_NOACTIVATE doesn't seems to work at all !
   -> the keyboard take the focus
   -> the WS_CHILD attribute set the keyboard as a child of the form...not in the .Net demo ?!
      => does it have something to do with PopupMenu property ?
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  Execute.TextBoxWKeyboard;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Release;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    Edit1.TextEntryMode := Numeric
  else
    Edit1.TextEntryMode := Standard;
end;

end.
