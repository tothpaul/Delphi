unit Execute.RichDocuments;

{ RichDocument (c)2018 by Execute SARL <contact@execute.fr> }

interface

uses
  System.Types,
  System.UITypes,
  System.Classes,  // TNotifyEvent
  Execute.SiblingLists;

type
  TRichFontItem = class(TSiblingItem)
  private
    FName   : string;
    FCharset: TFontCharset;
    FQuality: TFontQuality;
    FPitch  : TFontPitch;
    FStyle  : TFontStyles;
    FSize   : Integer;
    FAngle  : Integer;
  public
    property Name: string read FName;
    property Style: TFontStyles read FStyle;
    property Size: Integer read FSize;
    property Angle: Integer read FAngle;
    property Charset: TFontCharset read FCharset;
    property Quality: TFontQuality read FQuality;
    property Pitch: TFontPitch read FPitch;
  end;

  TRichFontList = class(TSiblingList<TRichFontItem>)
  end;

  TRichStyleItem = class(TSiblingItem)
  private
    FFont : TRichFontItem;
    FColor: TColor;
  public
    property Font: TRichFontItem read FFont;
    property Color: TColor read FColor;
  end;

  TRichStyleList = class(TSiblingList<TRichStyleItem>)
  end;

  TRichItemOption = (
    riNewLine,
    riCenter,
    riBreak
  );

  TRichItemOptions = set of TRichItemOption;

  TRichControlItem = class
  public
    function GetSize(var ASize: TSize): Boolean; virtual;
    procedure Render; virtual;
  end;

  TRichItem = class(TSiblingItem)
  private
    FStyle  : TRichStyleItem;
    FOptions: TRichItemOptions;
    FText   : string;
    FControl: TRichControlItem;
    function GetOption(Index: TRichItemOption): Boolean;
    procedure SetOption(Index: TRichItemOption; Value: Boolean);
  public
    destructor Destroy; override;
    property Text: string read FText;
    property Style: TRichStyleItem read FStyle;
    property NewLine: Boolean index riNewLine read GetOption write SetOption;
    property Center: Boolean index riCenter read GetOption write SetOption;
    property Break: Boolean index riBreak read GetOption write SetOption;
    property Control: TRichControlItem read FControl;
  end;

  TRichItemList = class(TSiblingList<TRichItem>)
  end;

  TLinkedObject = class
  protected
    procedure LinkChange(Sender: TObject); virtual;
    procedure LinkDestroy(Sender: TObject); virtual;
  end;

  TObjectLink = class
  private
    Target: TLinkedObject;
    Next  : TObjectLink;
  end;

  TRichSelection = record
    Left     : Integer;
    Top      : Integer;
    Height   : Integer;
    StartItem: TRichItem;
    StartOfs : Integer;
    StopItem : TRichItem;
    StopOfs  : Integer;
    Reverse  : Boolean;
  end;

  TRichCaret = record
    Left  : Integer;
    Top   : Integer;
    Height: Integer;
  end;

  TRichDocument = class
  private
    FFonts     : TRichFontList;
    FStyles    : TRichStyleList;
    FTextStyle : TRichStyleItem;
    FTextColor : Integer;
    FTextFont  : TRichFontItem;
    FItems     : TRichItemList;
    FLinks     : TObjectLink;
    FOnChange  : TNotifyEvent;
    FMargins   : TRect;
    FSelection : TRichSelection;
    procedure DoChange();
    function GetTextFont: TRichFontItem;
    procedure SetTextFont(Value: TRichFontItem);
    procedure SetTextColor(Value: Integer);
    function GetTextStyle: TRichStyleItem;
    procedure SetTextStyle(Value: TRichStyleItem);
    function ItemOrdered(A, B: TRichItem): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Empty: Boolean;
    function DefaultFont: TRichFontItem;
    function SetFont(const AName: string; AStyle: TFontStyles; ASize: Integer): TRichFontItem;
    procedure Center;
    procedure NewLine;
    procedure AddBreak;
    procedure Add(const Str: string);
    procedure AddLines(const Str: string);
    procedure AddCenterLine(const Str: string);
    procedure AddControl(AControl: TRichControlItem);
    procedure Link(Sender: TLinkedObject);
    procedure UnLink(Sender: TLinkedObject);
    procedure SetSelection(AItem: TRichItem; X, Y, Height, Offset: Integer; Start: Boolean);
    procedure SetSelectionPosition(AItem: TRichItem; const Pos: TPoint; Height: Integer);
    function IsSelection(AItem: TRichItem; var Start, Stop: Integer): Boolean;
    procedure GetCaret(var ACaret: TRichCaret);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Fonts: TRichFontList read FFonts;
    property Styles: TRichStyleList read FStyles;
    property Items: TRichItemList read FItems;
    property TextFont: TRichFontItem read GetTextFont write SetTextFont;
    property TextColor: Integer read FTextColor write SetTextColor;
    property TextStyle: TRichStyleItem read GetTextStyle write SetTextStyle;
    property Margins: TRect read FMargins;
  end;

implementation


{ TLinkedObject }

procedure TLinkedObject.LinkChange(Sender: TObject);
begin
  // empty
end;

procedure TLinkedObject.LinkDestroy(Sender: TObject);
begin
  // empty
end;

{ TRichItem }

destructor TRichItem.Destroy;
begin
  FControl.Free;
  inherited;
end;

function TRichItem.GetOption(Index: TRichItemOption): Boolean;
begin
  if Self = nil then
    Exit(False);
  Result := Index in FOptions;
end;

procedure TRichItem.SetOption(Index: TRichItemOption; Value: Boolean);
begin
  if Self = nil then
    Exit;
  if Value then
    FOptions := FOptions + [Index]
  else
    FOptions := FOptions - [Index];
end;

{ TRichDocument }

procedure TRichDocument.Add(const Str: string);
var
  Item: TRichItem;
begin
  Item := TRichItem.Create;
  Item.FText := Str;
  Item.FStyle := TextStyle;
  FItems.Add(Item);
end;

procedure TRichDocument.AddLines(const Str: string);
var
  Start: Integer;
  Index: Integer;
begin
  NewLine;
  Start := 1;
  Index := Pos(#13#10, Str);
  while Index > 0 do
  begin
    Add(Copy(Str, Start, Index - Start));
    NewLine;
    Start := Index + 2;
    Index := Pos(#13#10, Str, Start);
  end;
  Index := Length(Str);
  if Start < Index then
  begin
    Add(Copy(Str, Start, Index - Start + 1));
    NewLine;
  end;
end;

procedure TRichDocument.AddBreak;
begin
  Add('');
//  if FItems.Tail <> nil then
    FItems.Tail.Break := True;
end;

procedure TRichDocument.AddCenterLine(const Str: string);
begin
  Add(Str);
  Center;
  NewLine;
end;

procedure TRichDocument.AddControl(AControl: TRichControlItem);
var
  Item: TRichItem;
begin
  Item := TRichItem.Create;
  Item.FControl := AControl;
  FItems.Add(Item);
end;

procedure TRichDocument.Clear;
begin
  FItems.Clear;
  FStyles.Clear;
  FFonts.Clear;
  DoChange();
end;

constructor TRichDocument.Create;
begin
  FFonts := TRichFontList.Create;
  FStyles := TRichStyleList.Create;
  FItems := TRichItemList.Create;
  FMargins := TRect.Create(10, 10, 10, 10);
end;

destructor TRichDocument.Destroy;
var
  Link  : TObjectLink;
  Target: TLinkedObject;
begin
  // notify destruction
  Link := FLinks;
  while Link <> nil do
  begin
    Target := Link.Target;
    Link := Link.Next;
    Target.LinkDestroy(Self);
  end;
  // destroy any link left
  while FLinks <> nil do
  begin
    Link := FLinks.Next;
    FLinks.Free;
    FLinks := Link;
  end;
  // cleanup
  Clear;
  FItems.Free;
  FStyles.Free;
  FFonts.Free;
  inherited;
end;

function TRichDocument.DefaultFont: TRichFontItem;
begin
  if FFonts = nil then
    Result := SetFont('Arial', [], 10)
  else
    Result := FFonts.First;
end;

procedure TRichDocument.DoChange;
var
  Link: TObjectLink;
begin
  Link := FLinks;
  while Link <> nil do
  begin
    Link.Target.LinkChange(Self);
    Link := Link.Next;
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TRichDocument.Empty: Boolean;
begin
  Result := (Self = nil) or (FItems.Count = 0);
end;

function TRichDocument.GetTextStyle: TRichStyleItem;
begin
  Result := FTextStyle;
  if Result = nil then
  begin
    Result := FStyles.First;
    if Result <> nil then
    begin
      repeat
        if (Result.FFont = FTextFont)
        and(Result.FColor = FTextColor) then
          Exit;
      until FStyles.Next(Result) = False;
    end;
    Result := TRichStyleItem.Create;
    Result.FFont := FTextFont;
    Result.FColor := FTextColor;
    FStyles.Add(Result);
  end;
end;

function TRichDocument.IsSelection(AItem: TRichItem; var Start,
  Stop: Integer): Boolean;
var
  Temp: Integer;
begin
  Start := -1;
  Stop := -1;
  Result := True;
  if AItem = FSelection.StartItem then
  begin
    Start := FSelection.StartOfs;
    if AItem = FSelection.StopItem then
      Stop := FSelection.StopOfs;
  end else begin
    if AItem = FSelection.StopItem then
      Stop := FSelection.StopOfs
    else begin
      Result := False;
      Exit;
    end;
  end;
  if FSelection.Reverse then
  begin
    Temp := Start;
    Start := Stop;
    Stop := Temp;
  end;
end;

function TRichDocument.ItemOrdered(A, B: TRichItem): Boolean;
var
  I: TRichItem;
begin
  I := nil;
  while FItems.Next(I) do
  begin
    if I = A then
      Exit(True);
    if I = B then
      Exit(False);
  end;
  Assert(False, 'Item not found');
  Result := False;
end;

procedure TRichDocument.GetCaret(var ACaret: TRichCaret);
begin
  ACaret.Left := FSelection.Left;
  ACaret.Top := FSelection.Top;
  ACaret.Height := FSelection.Height;
end;

function TRichDocument.GetTextFont: TRichFontItem;
begin
  if FTextFont = nil then
    Result := DefaultFont
  else
    Result := FTextFont;
end;

procedure TRichDocument.Link(Sender: TLinkedObject);
var
  LinkItem: TObjectLink;
begin
  LinkItem := TObjectLink.Create;
  LinkItem.Target := Sender;
  LinkItem.Next := FLinks;
  FLinks := LinkItem;
end;

procedure TRichDocument.Center;
begin
  FItems.Tail.Center := True;
end;

procedure TRichDocument.NewLine;
begin
  FItems.Tail.NewLine := True;
end;

function TRichDocument.SetFont(const AName: string; AStyle: TFontStyles;
  ASize: Integer): TRichFontItem;
begin
  Result := FFonts.First;
  if Result <> nil then
  begin
    repeat
      if (Result.Name = AName)
      and(Result.Style = AStyle)
      and(Result.Size = ASize) then
        Exit;
    until FFonts.Next(Result) = False;
  end;
  Result := TRichFontItem.Create;
  Result.FName := AName;
  Result.FStyle := AStyle;
  Result.FSize := ASize;
  FFonts.Add(Result);
  FTextFont := Result;
  FTextStyle := nil;
end;

procedure TRichDocument.SetTextColor(Value: Integer);
begin
  if Value <> FTextColor then
  begin
    FTextColor := Value;
    FTextStyle := nil;
  end;
end;

procedure TRichDocument.SetTextFont(Value: TRichFontItem);
begin
  if Value <> FTextFont then
  begin
    FTextFont := Value;
    FTextStyle := nil;
  end;
end;

procedure TRichDocument.SetTextStyle(Value: TRichStyleItem);
begin
  FTextStyle := Value;
  if Value <> nil then
  begin
    FTextFont := Value.Font;
    FTextColor := Value.Color;
  end;
end;

procedure TRichDocument.SetSelection(AItem: TRichItem; X, Y, Height, Offset: Integer; Start: Boolean);
begin
  if Start then
  begin
    FSelection.StartItem := AItem;
    FSelection.StartOfs := Offset;
  end;
  FSelection.Left := X;
  FSelection.Top := Y;
  FSelection.Height := Height;
  FSelection.StopItem := AItem;
  FSelection.StopOfs := Offset;
  if FSelection.StartItem = FSelection.StopItem then
    FSelection.Reverse := FSelection.StartOfs > FSelection.StopOfs
  else
    FSelection.Reverse := ItemOrdered(FSelection.StopItem, FSelection.StartItem);
end;

procedure TRichDocument.SetSelectionPosition(AItem: TRichItem; const Pos: TPoint;
  Height: Integer);
begin
  if AItem = FSelection.StopItem then
  begin
    FSelection.Left := Pos.X;
    FSelection.Top := Pos.Y;
    FSelection.Height := Height;
  end;
end;

procedure TRichDocument.UnLink(Sender: TLinkedObject);
var
  PLink: ^TObjectLink;
  Link : TObjectLink;
begin
  PLink := @FLinks;
  while PLink^ <> nil do
  begin
    Link := PLink^;
    if Link.Target = Sender then
    begin
      PLink^ := Link.Next;
      Link.Free;
      Exit;
    end;
    PLink := @Link.Next;
  end;
end;

{ TRichControlItem }

function TRichControlItem.GetSize(var ASize: TSize): Boolean;
begin
  Result := False;
end;

procedure TRichControlItem.Render;
begin
 // for custom controls
end;

end.
