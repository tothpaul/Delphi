unit Execute.ComboEdit;

{
   ComboEdit (c)2016 Execute SARL <contact@execute.fr>
   http://www.execute.fr

   usage:

    proc1edure TForm1.FormCreate(Sender: TObject);
    begin
      FComboEdit := TComboEdit.Create(Edit1, OnDropDown);
    end;

    procedure TForm1.OnDropDown(Sender: TObject);
    begin
      with FComboEdit.Items do
      begin
        BeginUpdate;
        try
          Clear;
          Add('...');
        finally
          EndUpdate;
        end;
      end;
    end;
}

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TComboEdit = class(TComboBox)
  private
    FEdit : TEdit;
    FTimer: TTimer;
    FOldChange: TNotifyEvent;
    FOldKeyDown: TKeyEvent;
    procedure EditChange(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnTimer(Sender: TObject);
    procedure DoDropDown;
  public
    constructor Create(Edit: TEdit; DropDown: TNotifyEvent); reintroduce;
    procedure Click; override;
    property Timer: TTimer read FTimer;
  end;

implementation

{ TComboEdit }

constructor TComboEdit.Create(Edit: TEdit; DropDown: TNotifyEvent);
begin
  inherited Create(Edit.Owner);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 500;
  FTimer.OnTimer := OnTimer;
  FEdit := Edit;
  FOldChange := Edit.OnChange;
  Edit.OnChange := EditChange;
  FOldKeyDown := Edit.OnKeyDown;
  Edit.OnKeyDown := EditKeyDown;
  OnDropDown := DropDown;
  Visible := False;
  Parent := FEdit.Parent;
  Cursor := crHandPoint;
  SetBounds(FEdit.Left, FEdit.Top, FEdit.Width, FEdit.Height);
  SendToBack;
end;

procedure TComboEdit.Click;
begin
  inherited;
  if ItemIndex >= 0 then
    FEdit.Text := Items[ItemIndex];
  DroppedDown := False;
end;

procedure TComboEdit.DoDropDown;
var
  Down: Boolean;
  Crs : THandle;
begin
  DropDown;
  if Items.Count > 0 then
  begin
    Visible := True;
  end;
  Down := DroppedDown;
  if Down <> ((Items.Count > 1) or ((Items.Count = 1) and not AnsiSameText(Items[0], Text))) then
  begin
    DroppedDown := not Down;
    if not Down then
      Winapi.Windows.SetCursor(Screen.Cursors[crDefault]); // force the cursor to shows
  end;
end;

procedure TComboEdit.EditChange(Sender: TObject);
begin
  if FTimer.Interval = 0 then
    DoDropDown
  else begin
    FTimer.Enabled := False; // Reset
    FTimer.Enabled := True;
  end;

  if Assigned(FOldChange) then
    FOldChange(FEdit);
end;

procedure TComboEdit.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if DroppedDown then
  begin
    case Key of
      VK_DOWN:
      begin
        if ItemIndex < Items.Count then
          ItemIndex := ItemIndex + 1;
        Key := 0;
        Exit;
      end;
      VK_UP:
      begin
        if ItemIndex >= 0 then
          ItemIndex := ItemIndex - 1;
        Key := 0;
        Exit;
      end;
      VK_RETURN:
      begin
        if ItemIndex >= 0 then
          FEdit.Text := Items[ItemIndex];
        DroppedDown := False;
        Key := 0;
        Exit;
      end;
      VK_ESCAPE:
      begin
        DroppedDown := False;
        Key := 0;
        Exit;
      end;
    end;
  end else begin
    if Key = VK_DOWN then
    begin
      DoDropDown;
      Key := 0;
      Exit;
    end;
  end;
  if Assigned(FOldKeyDown) then
    FOldKeyDown(FEdit, Key, Shift);
end;

procedure TComboEdit.OnTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  DoDropDown();
end;

end.
