unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Math,
  Vcl.Themes,
  Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DelphiTDS, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Execute.Trees;

type
  TListBox = class(Vcl.StdCtrls.TListBox)
  private
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
  end;

  TForm1 = class(TForm)
    lbDUMP: TListBox;
    Splitter1: TSplitter;
    lbTREE: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure lbDUMPDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure lbTREEDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure lbTREEDblClick(Sender: TObject);
    procedure lbTREEClick(Sender: TObject);
    procedure lbTREEKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Déclarations privées }
    TDS         : TTDSFile;
    TreeList    : TTreeList;
    FSelStart   : Integer;
    FSelStop    : Integer;
    procedure TreeChanged(Sender: TObject);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  TreeList := TTreeList.Create;
  TreeList.OnChange := TreeChanged;
  TDS := TTDSFile.Create(Application.ExeName, TreeList);
  lbDUMP.Count := (TDS.Size + 15) div 16;
  Caption := Caption + ' [' + IntToStr(Round(TDS.KnownSize * 100 / TDS.Size)) + '%]';
end;

const
  HX: array[0..$F] of Char = '0123456789ABCDEF';

  MAP_COLORS: array[TMemoryInfo] of TColor = (
    clSilver, // mmNotRead,
    clRed,    // mmPadding
    clWebOrange, // mmHeader,
    clGreen, // mmDirectory,
    clOlive, // mmDirectoryEntry,
    clBlue, // mmModuleInfo,
    clSkyBlue, //mmSegmentInfo
    clBlue, // mmSymbolInfo
    clAqua, // mmSize
    clGreen, // mmNameSize
    clNavy // mmName
  );

procedure TForm1.lbDUMPDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  Adr: string;
  Str: string;
  Chr: string;
  Ofs: Integer;
  Val: Byte;
  Len: Integer;
  W  : Integer;
  X1 : Integer;
  X2 : Integer;
  Col: TMemoryInfo;
  Sel: Boolean;
  SS : Boolean;
  Idx: Integer;
begin
  with lbDUMP.Canvas do
  begin
    Font.Color := clBlack;
    Brush.Color := clWhite;
    FillRect(Rect);

    W  := TextWidth('W');
    Inc(Rect.Top, 2);

    X1 := Rect.Left + 2;
    TextOut(X1, Rect.Top, IntToHex(16 * Index, 8));
    Inc(X1, 9 * W);

    X2 := X1 + 3 * 16 * W;
    TextOut(X2, Rect.Top, '`');
    Inc(X2, W);

    Ofs := 16 * Index;
    Str := '';
    Chr := '';
    Col := TDS.MemoryMap[Ofs];
    Sel := (Ofs >= FSelStart) and (Ofs < FSelStop);
    for Len := 0 to 15 do
    begin
      SS := (Ofs >= FSelStart) and (Ofs < FSelStop);
      if (Str <> '') and ((Col <> TDS.MemoryMap[Ofs]) or (SS <> Sel)) then
      begin
        if Sel then
        begin
          Brush.Color := MAP_COLORS[Col];
          Font.Color := clWhite;
        end else begin
          Brush.Color := clWhite;
          Font.Color := MAP_COLORS[Col];
        end;
        TextOut(X1, Rect.Top, Str);
        TextOut(X2, Rect.Top, Chr);
        Inc(X1, Length(Str) * W);
        Inc(X2, Length(Chr) * W);
        Str := '';
        Chr := '';
        Col := TDS.MemoryMap[Ofs];
        Sel := SS;
      end;
      if Ofs >= TDS.Size then
      begin
        Str := Str + '-- ';
        Chr := Chr + ' ';
      end else begin
        Val := TDS.Memory[Ofs];
        Str := Str + HX[Val shr 4] + HX[Val and $F] + ' ';
        case Val of
          32..125: Chr := Chr + Char(Val)
        else
          Chr := Chr + '.';
        end;
      end;
      Inc(Ofs);
    end;

    if Str <> '' then
    begin
      if Sel then
      begin
        Brush.Color := MAP_COLORS[Col];
        Font.Color := clWhite;
      end else begin
        Brush.Color := clWhite;
        Font.Color := MAP_COLORS[Col];
      end;
      TextOut(X1, Rect.Top, Str);
      TextOut(X2, Rect.Top, Chr);
    end;

    Font.Color := clBlack;
    Brush.Color := clWhite;
  end;
end;
procedure TForm1.lbTREEClick(Sender: TObject);
var
  Index: Integer;
  Node : TTreeNode;
  Start: Integer;
  Stop : Integer;
begin
  Index := lbTREE.ItemIndex;
  if Index >= 0 then
  begin
    Node := TreeList.Items[Index];
    if Node is TTDSNode then
    begin
      Start := TTDSNode(Node).Offset;
      Stop := STart + TTDSNode(Node).Size;
      if (Start <> FSelStart) or (Stop <> FSelStop) then
      begin
        FSelStart := Start;
        FSelStop := Stop;
        lbDUMP.ItemIndex := FSelStart div 16;
        lbDUMP.Invalidate;
      end;
    end;
  end;
end;

procedure TForm1.lbTREEDblClick(Sender: TObject);
var
  Index: Integer;
  Node : TTreeNode;
begin
  Index := lbTREE.ItemIndex;
  if Index >= 0 then
  begin
    Node := TreeList.Items[Index];
    Node.DblClick;
    Caption := 'SelfDebug [' + IntToStr(Round((TDS.KnownSize * 100) / TDS.Size)) + '%]';
  end;
end;

procedure TForm1.lbTREEDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  TreeList.Items[Index].Draw(lbTREE.Canvas, Rect, State);
end;

procedure TForm1.lbTREEKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Index: Integer;
  Node : TTreeNode;
begin
  Index := lbTREE.ItemIndex;
  if Index < 0 then
    Exit;
  Node := TreeList[Index];
  case Key of
    VK_LEFT :
      if Node.Expanded then
      begin
        Key := 0;
        Node.DblClick;
      end else begin
        while Index > 0 do
        begin
          Dec(Index);
          if TreeList[Index].Level < Node.Level then
          begin
            lbTREE.ItemIndex := Index;
            Key := 0;
            Exit;
          end;
        end;
      end;
    VK_RIGHT:
      if not Node.Expanded then
      begin
        Key := 0;
        Node.DblClick;
      end;
    VK_RETURN:
    begin
      Key := 0;
      Node.DblClick;
    end;
  end;
end;

procedure TForm1.TreeChanged(Sender: TObject);
var
  TopLine: Integer;
  Index  : Integer;
begin
  TopLine := lbTREE.TopIndex;
  Index := lbTREE.ItemIndex;
  lbTREE.Count := TreeList.Count;
  lbTREE.Invalidate;
  lbTREE.ItemIndex := Index;
  lbTREE.TopIndex := TopLine;
end;

{ TListBox }

procedure TListBox.WMVScroll(var Msg: TWMVScroll);
var
  Info: TScrollInfo;
begin
  // do not intervene when themes are disabled
  if ThemeServices.ThemesEnabled then
  begin
    Msg.Result := 0;

    case Msg.ScrollCode of
      SB_THUMBPOSITION: Exit; // Nothing to do, thumb is already tracked
      SB_THUMBTRACK:
        begin
          ZeroMemory(@Info, SizeOf(Info));
          Info.cbSize := SizeOf(Info);
          Info.fMask := SIF_POS or SIF_TRACKPOS;
          if GetScrollInfo(Handle, SB_VERT, Info) and
              (Info.nTrackPos <> Info.nPos) then
            TopIndex := TopIndex + Info.nTrackPos - Info.nPos;
        end;
      else
        inherited;
    end;
  end else
    inherited;
end;


end.
