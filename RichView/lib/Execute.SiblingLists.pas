unit Execute.SiblingLists;

{ RichDocument (c)2018 by Execute SARL <contact@execute.fr> }

interface

type
  TSiblingItem = class
  private
    FSibling: TSiblingItem;
  end;

  TCustomSibingList = class
  private
    FItems: TSiblingItem;
    FCount: Integer;
    function InternalGetItem(Index: Integer): TSiblingItem;
    procedure InternalAdd(AItem: TSiblingItem);
    function InternalNext(var AItem: TSiblingItem): Boolean;
    function InternalFirst: TSiblingItem;
  public
    destructor Destroy; override;
    procedure Clear;
    property Count: Integer read FCount;
  end;

  TSiblingList<T:TSiblingItem> = class(TCustomSibingList)
  private
    function GetItem(Index: Integer): T; inline;
  public
    procedure Add(AItem: T); inline;
    function First: T; inline;
    function Tail: T; inline;
    function Next(var AItem: T): Boolean; inline;
    property Items[Index: Integer]: T read GetItem; default;
  end;

implementation

{ TCustomSiblingList }

destructor TCustomSibingList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TCustomSibingList.InternalAdd(AItem: TSiblingItem);
begin
  if FItems = nil then // empty list
    AItem.FSibling := AItem  // point to ourself
  else begin
    AItem.FSibling := FItems.FSibling; // point to the first item (next item for the tail)
    FItems.FSibling := AItem;
  end;
  FItems := AItem; // tail item
  Inc(FCount);
end;

function TCustomSibingList.InternalFirst: TSiblingItem;
begin
  if FItems = nil then
    Result := nil
  else
    Result := FItems.FSibling;
end;

function TCustomSibingList.InternalGetItem(Index: Integer): TSiblingItem;
var
  Next: Integer;
begin
  Result := FItems;
  if Result = nil then
    Exit;
  for Next := Index + 1 downto 0 do
    Result := Result.FSibling;
end;

function TCustomSibingList.InternalNext(var AItem: TSiblingItem): Boolean;
begin
  Result := AItem <> FItems;
  if Result then
  begin
    if AItem = nil then
      AItem := FItems;
    AItem := AItem.FSibling
  end else begin
    AItem := nil;
  end;
end;

procedure TCustomSibingList.Clear;
var
  Index: Integer;
  Item : TSiblingItem;
begin
  for Index := 0 to FCount - 1 do
  begin
    Item := FItems;
    FItems := Item.FSibling;
    Item.Free;
  end;
  FItems := nil;
  FCount := 0;
end;

{ TSiblingList }

procedure TSiblingList<T>.Add(AItem: T);
begin
  InternalAdd(AItem);
end;

function TSiblingList<T>.First: T;
begin
  Result := T(InternalFirst);
end;

function TSiblingList<T>.GetItem(Index: Integer): T;
begin
  Result := T(InternalGetItem(Index));
end;

function TSiblingList<T>.Next(var AItem: T): Boolean;
begin
  Result := InternalNext(TSiblingItem(AItem));
end;

function TSiblingList<T>.Tail: T;
begin
  Result := T(FItems);
end;

end.
