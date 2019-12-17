unit MacroRecorderDemo1.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Execute.MacroPlayer;

type
  TForm1 = class(TForm)
    btRecord: TButton;
    btPlay: TButton;
    btSave: TButton;
    btLoad: TButton;
    btShow: TButton;
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure btRecordClick(Sender: TObject);
    procedure btPlayClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btLoadClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
  private
    { Déclarations privées }
    Player: TMacroPlayer;
    procedure PlayDone(Sender: TObject);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btLoadClick(Sender: TObject);
begin
  if Player.Playing then
    Exit;
  if OpenDialog.Execute then
  begin
    Player.LoadFromFile(OpenDialog.FileName);
    btPlay.Enabled := True;
    btSave.Enabled := True;
  end;
end;

procedure TForm1.btPlayClick(Sender: TObject);
begin
  if Player.Playing then
  begin
    Player.Status := mpsIdle;
  end else begin
    btRecord.Enabled := False;
    btPlay.Tag := 1;
    btPlay.Caption := 'Stop';
    Player.PlayBack;
  end;
end;

procedure TForm1.btRecordClick(Sender: TObject);
begin
  if Player.Playing then
    Exit;
  if btRecord.Tag = 0 then
  begin
    Player.StartRecord;
    btRecord.Tag := 1;
    btRecord.Caption := 'Stop';
  end else begin
    Player.StopRecord;
    btRecord.Tag := 0;
    btRecord.Caption := 'Record';
    btSave.Enabled := True;
    btPlay.Enabled := True;
  end;
end;

procedure TForm1.btSaveClick(Sender: TObject);
var
  FileName: string;
begin
  if Player.Playing then
    Exit;
  if SaveDialog.Execute then
  begin
    FileName := SaveDialog.FileName;
    Player.SaveToFile(FileName);
  end;
end;

procedure TForm1.btShowClick(Sender: TObject);
begin
  Memo1.Clear;
  Player.SaveAsCode(Memo1.Lines);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  Player.Toggle(mpoRecordMouseMove);
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  Player.Toggle(mpoRecordMouseClick);
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  Player.Toggle(mpoRecordKeyboard);
  CheckBox4.Enabled := CheckBox3.Checked;
  CheckBox5.Enabled := CheckBox3.Checked;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  Player.Toggle(mpoUnicodeChar);
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  Player.Toggle(mpoMergeChars);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Player := TMacroPlayer.Create(Self);
  Player.OnPlayDone := PlayDone;
end;

procedure TForm1.PlayDone(Sender: TObject);
begin
  btPlay.Tag := 0;
  btPlay.Caption := 'Play';
  btRecord.Enabled := True;
end;

end.
