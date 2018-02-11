unit Execute.RichRenderers;

{ RichDocument (c)2018 by Execute SARL <contact@execute.fr> }

interface

uses
  System.Types,
  System.UITypes,
  System.Classes,
  Execute.SiblingLists,
  Execute.RichDocuments;

type
  TDrawItem = class(TSiblingItem)
  private
    FItem   : TRichItem;
    FStart  : Integer;
    FCount  : Integer;
    FRect   : TRect;
    function GetText: PChar;
    function Contains(x, y: Integer; var AfterItem: TDrawItem): Boolean;
    procedure GrowSize(var Size: TSize);
  public
    property RichItem: TRichItem read FItem;
    property Text: PChar read GetText;
    property Start: Integer read FStart;
    property Count: Integer read FCount;
    property Rect: TRect read FRect;
    property Top: Integer read FRect.Top;
    property Left: Integer read FRect.Left;
    property Right: Integer read FRect.Right;
    property Bottom: Integer read FRect.Bottom;
  end;

  TDrawItemList = class(TSiblingList<TDrawItem>)
  end;

  TItemPos = record
  // used by Format functions
    Top     : Integer;
    Left    : Integer;
  // used by MeasureItem
    Start   : Integer;
    MaxWidth: Integer;
  // set by MeasureItem
    Size    : TSize;
  end;

  TRichRenderer = class(TLinkedObject)
  private
    FItems     : TDrawItemList;
    FMouseItem : TDrawItem;
    FDocument  : TRichDocument;
    procedure SetDocument(AValue: TRichDocument);
  protected
    FWidth     : Integer;
    FHeight    : Integer;
    FNeedFormat: Boolean;
    FSize      : TSize;
    procedure LinkChange(Sender: TObject); override;
    procedure LinkDestroy(Sender: TObject); override;
    procedure FormatText(RichItem: TRichItem; var ItemPos: TItemPos);
    procedure FormatControl(RichItem: TRichItem; var ItemPos: TItemPos);
    procedure FormatBreak(RichItem: TRichItem; var ItemPos: TItemPos);
    function MeasureText(RichItem: TRichItem; var ItemPos: TItemPos; Len: Integer): Integer; virtual;
    function SetStyle(AStyle: TRichStyleItem): Integer; virtual;
    procedure SetSelection(X, Y: Integer; Start: Boolean); virtual;
    function GetMouseItem(x, y: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetViewSize(AWidth, AHeight: Integer);
    procedure NeedFormat;
    procedure Format;
    procedure Clear;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    property Document: TRichDocument read FDocument write SetDocument;
    property Items: TDrawItemList read FItems;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Size: TSize read FSize;
  end;

implementation

{ TRichRenderer }

constructor TRichRenderer.Create;
begin
  FItems := TDrawItemList.Create;
end;

destructor TRichRenderer.Destroy;
begin
  Document := nil;
  FItems.Free;
  inherited;
end;

procedure TRichRenderer.Clear;
begin
  FMouseItem := nil;
  FItems.Clear;
  FNeedFormat := False;
end;

procedure TRichRenderer.Format;
var
  RichItem : TRichItem;
  ItemPos  : TItemPos;
  Index    : Integer;
begin
  FNeedFormat := False;
  Clear;
  if FDocument = nil then
    Exit;
  RichItem := FDocument.Items.First;
  if RichItem = nil then
    Exit;
  ItemPos.Top := Document.Margins.Top;
  ItemPos.Left := Document.Margins.Left;
  ItemPos.MaxWidth := FWidth - Document.Margins.Left - Document.Margins.Right;
  repeat
     if RichItem.Control = nil then
       FormatText(RichItem, ItemPos)
     else
       FormatControl(RichItem, ItemPos);
     if RichItem.Break then
     begin
       FormatBreak(RichItem, ItemPos);
     end;
  until FDocument.Items.Next(RichItem) = False;
  FSize.cx := 0;
  FSize.cy := 0;
  for Index := 0 to FItems.Count - 1 do
  begin
    FItems[Index].GrowSize(FSize);
  end;
end;

procedure TRichRenderer.FormatBreak(RichItem: TRichItem; var ItemPos: TItemPos);
var
  DrawItem: TDrawItem;
begin
  DrawItem := TDrawItem.Create;
  DrawItem.FItem := RichItem;
  DrawItem.FStart := -1;
  if ItemPos.Left > Document.Margins.Left then
  begin
    ItemPos.Left := Document.Margins.Left;
    Inc(ItemPos.Top, ItemPos.Size.cy);
  end;
  DrawItem.FRect := TRect.Create(ItemPos.Left + 5, ItemPos.Top, ItemPos.Left + ItemPos.MaxWidth - 10, ItemPos.Top + 11);
  ItemPos.Left := Document.Margins.Left;
  Inc(ItemPos.Top, 11);
  FItems.Add(DrawItem);
end;

procedure TRichRenderer.FormatControl(RichItem: TRichItem;
  var ItemPos: TItemPos);
var
  DrawItem: TDrawItem;
begin
  if RichItem.Control.GetSize(ItemPos.Size) = False then
    Exit;
  DrawItem := TDrawItem.Create;
  DrawItem.FItem := RichItem;
  DrawItem.FRect := TRect.Create(ItemPos.Left, ItemPos.Top, ItemPos.Left + ItemPos.Size.cx, ItemPos.Top + ItemPos.Size.cy);
  if RichItem.Center then
    DrawItem.FRect.Offset((ItemPos.MaxWidth - ItemPos.Left - DrawItem.FRect.Width) div 2, 0);
  FItems.Add(DrawItem);
  if RichItem.NewLine then
  begin
    ItemPos.Left := Document.Margins.Left;
    Inc(ItemPos.Top, ItemPos.Size.cy);
  end else begin
    Inc(ItemPos.Left, ItemPos.Size.cx);
  end;
end;

procedure TRichRenderer.FormatText(RichItem: TRichItem; var ItemPos: TItemPos);
var
  DrawItem : TDrawItem;
  Text     : PChar;
  Len      : Integer;
  Count    : Integer;
  TrimCount: Integer;
  Index    : Integer;
begin
  Len := Length(RichItem.Text);
  if Len = 0 then
    Exit;
  Text := Pointer(RichItem.Text);
  Dec(Text); // 1 based PChar
  ItemPos.Start := 0;
  SetStyle(RichItem.Style);
  while Len > 0 do
  begin
    Count := MeasureText(RichItem, ItemPos, Len);
    TrimCount := Count;
    if Count < Len then
    begin
      for Index := Count downto 2 do
      begin
        if Text[Index] = ' ' then
        begin
          Count := Index;
          Break;
        end;
      end;
      // the text can be larger then the document
      TrimCount := Count;
      while (TrimCount > 2) and (Text[TrimCount] = ' ') do
        Dec(TrimCount);
      MeasureText(RichItem, ItemPos, TrimCount)
    end;
    DrawItem := TDrawItem.Create;
    DrawItem.FItem := RichItem;
    DrawItem.FStart := ItemPos.Start;
    DrawItem.FCount := TrimCount;
    DrawItem.FRect := TRect.Create(ItemPos.Left, ItemPos.Top, ItemPos.Left + ItemPos.Size.cx, ItemPos.Top + ItemPos.Size.cy);
    if RichItem.Center then
      DrawItem.FRect.Offset((ItemPos.MaxWidth - ItemPos.Left - DrawItem.FRect.Width) div 2, 0);
    FItems.Add(DrawItem);
    if Text[Count] = ' ' then
    begin
      while (Count < Len - 1) and (Text[Count + 1] = ' ') do
        Inc(Count);
    end;
    if (Count = Len) and (RichItem.NewLine = False) then
    begin
      Inc(ItemPos.Left, ItemPos.Size.cx);
      Exit;
    end;
    ItemPos.Left := Document.Margins.Left;
    Inc(ItemPos.Top, ItemPos.Size.cy);
    Inc(ItemPos.Start, Count);
    Inc(Text, Count);
    Dec(Len, Count);
  end;
end;

function TRichRenderer.GetMouseItem(x, y: Integer): Boolean;
var
  Item: TDrawItem;
begin
  Item := nil;
  if FMouseItem.Contains(x, y, Item) and (x >= FMouseItem.FRect.Left) then
    Exit(True);

  FMouseItem := nil;
  Item := nil;
  while FItems.Next(FMouseItem) do
  begin
    if FMouseItem.Contains(x, y, Item) then
      Exit(True);
    if FMouseItem.Top > y then
      Break;
  end;
  if Item = nil then
  begin
    FMouseItem := FItems.Tail;
  end else begin
    FMouseItem := Item;
  end;
  Result := False;
end;

procedure TRichRenderer.LinkChange(Sender: TObject);
begin
  FNeedFormat := True;
end;

procedure TRichRenderer.LinkDestroy(Sender: TObject);
begin
// don't really need to Unlink now
  FDocument := nil;
  Clear;
end;

function TRichRenderer.MeasureText(RichItem: TRichItem;
  var ItemPos: TItemPos; Len: Integer): Integer;
begin
  Result := Len;
end;

procedure TRichRenderer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  SetSelection(X, Y, True);
end;

procedure TRichRenderer.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  SetSelection(X, Y, False);
end;

procedure TRichRenderer.NeedFormat;
begin
  FNeedFormat := True;
end;

procedure TRichRenderer.SetDocument(AValue: TRichDocument);
begin
  if FDocument <> AValue then
  begin
    if FDocument <> nil then
    begin
      FDocument.UnLink(Self);
      Clear;
    end;
    FDocument := AValue;
    if FDocument <> nil then
    begin
      FDocument.Link(Self);
      FNeedFormat := True;
    end;
  end;
end;

function TRichRenderer.SetStyle(AStyle: TRichStyleItem): Integer;
begin
  // for custom Renderer only
  Result := 0;
end;

procedure TRichRenderer.SetViewSize(AWidth, AHeight: Integer);
begin
  if (AWidth <> FWidth) or (AHeight <> FHeight) then
  begin
    FWidth := AWidth;
    FHeight := AHeight;
    if FDocument.Empty = False then
      FNeedFormat := True;
  end;
end;


procedure TRichRenderer.SetSelection(X, Y: Integer; Start: Boolean);
var
  ItemPos: TItemPos;
  Ofs    : Integer;
begin
  if GetMouseItem(X, Y) then
  begin
    if FMouseItem.Start < 0 then
    begin
      ItemPos.Size.cx := FMouseItem.FRect.Width;
      if X < FMouseItem.Left then
        Ofs := 0
      else
        Ofs := 1;
    end else begin
      SetStyle(FMouseItem.FItem.Style);
      ItemPos.Top := 0;
      ItemPos.Left := 0;
      ItemPos.MaxWidth := X - FMouseItem.FRect.Left;
      ItemPos.Start := 0;
      if ItemPos.MaxWidth < 0 then
      begin
        ItemPos.Size.cx := 0;
        Ofs := 0;
      end else begin
        Ofs := MeasureText(FMouseItem.FItem, ItemPos, Length(FMouseItem.FItem.Text));
      end;
    end;
    FDocument.SetSelection(
      FMouseItem.FItem,
      FMouseItem.FRect.Left + ItemPos.Size.cx,
      FMouseItem.FRect.Top,
      FMouseItem.FRect.Height,
      Ofs,
      Start
    );
  end else begin
    if (FMouseItem.Start < 0) or (FMouseItem.FItem.Control <> nil) then
      Ofs := 1
    else
      Ofs := Length(FMouseItem.FItem.Text);
    FDocument.SetSelection(
      FMouseItem.FItem,
      FMouseItem.FRect.Right,
      FMouseItem.FRect.Top,
      FMouseItem.FRect.Height,
      Ofs,
      Start
    );
  end;
end;

{ TDrawItem }

function TDrawItem.Contains(x, y: Integer; var AfterItem: TDrawItem): Boolean;
begin
  Result := (Self <> nil) and (Y >= FRect.Top);

  if Result then
  begin

    Result := (X < FRect.Right) and (Y < FRect.Bottom);

    if (Result = False) and (x >= FRect.Left) and (
        (AfterItem = nil)
     or (
         (Y > FRect.Bottom) or ((FRect.Top >= AfterItem.FRect.Bottom) or (FRect.Right > AfterItem.FRect.Right))
        )
    ) then
      AfterItem := Self;
  end;
end;

function TDrawItem.GetText: PChar;
begin
  Result := Pointer(FItem.Text);
end;

procedure TDrawItem.GrowSize(var Size: TSize);
begin
  if (FStart <> -1) and (FRect.Right > Size.cx) then
    Size.cx := FRect.Right;
  if FRect.Bottom > Size.cy then
    Size.cy := FRect.Bottom;
end;

end.
