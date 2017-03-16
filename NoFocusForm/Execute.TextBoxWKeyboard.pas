unit Execute.TextBoxWKeyboard;

interface

uses
  Winapi.Windows,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Execute.HoverKeyboard;

type
  TEntryMode = (
    Numeric,
    Standard
  );

  TEdit = class(Vcl.StdCtrls.TEdit)
  private
    HK: TForm;
    FEntryMode: TEntryMode;
    NumberEntered: Boolean;
    procedure HK_FormClosed(Sender: TObject; var Action: TCloseAction);
  protected
    procedure Click; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    property TextEntryMode: TEntryMode read FEntryMode write FEntryMode;
  end;

implementation

function CheckIfNumericKey(Key: Word; isDecimalPoint: Boolean): Boolean;
begin
  if Key = VK_BACK then
    Exit(True);
  if (Key = VK_OEM_PERIOD) or (Key = VK_DECIMAL) then
    Exit(not IsDecimalPoint);
  if (Key >= $30) and (Key <= $39)  then
    Exit(True);
  if (Key >= VK_NUMPAD0) and (Key <= VK_NUMPAD9) then
    Exit(True);
  Result := False;
end;

{ TEdit }

procedure TEdit.Click;
begin
  inherited;
  DoEnter();
end;

procedure TEdit.DoEnter;
var
  P: TPoint;
begin
  inherited;
  if HK = nil then
  begin
//    if EntryMode = Standard then
//    begin
      HK := THoverKeyboard.Create(Self);
      THoverKeyboard(HK).TextBox := Self;
//    end else begin
//      HK := THoverNumPad.Create(Self);
//      THoverNumPad(HK).TextBox := Self;
//    end;
    P.X := Left;
    P.Y := Top + Height;
//    P := Parent.ClientToScreen(P);
    HK.Left := P.X;
    HK.Top := P.Y;
//    HK.Parent := Parent;
    HK.OnClose := HK_FormClosed;
    HK.Show();
  end;
end;

procedure TEdit.DoExit;
begin
  inherited;
  if Assigned(HK) then
  begin
    HK.Free;
    HK := nil;
  end;
end;

procedure TEdit.HK_FormClosed(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
  HK := nil;
end;

procedure TEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if FEntryMode = Numeric then
  begin
    NumberEntered := CheckIfNumericKey(Key, Pos(Text, '.') > 0);
  end;
end;

procedure TEdit.KeyPress(var Key: Char);
begin
  if (FEntryMode = Numeric) and (NumberEntered = False) then
    Key := #0;
  inherited;
end;

end.
