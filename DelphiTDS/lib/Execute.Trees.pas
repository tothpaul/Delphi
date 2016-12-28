unit Execute.Trees;

{
  Fast Non-Visual VirtualTree (c) 2016 by Execute SARL, Paul TOTH <contact@execute.fr>
  http://www.execute.fr

}
interface

uses
  System.Types,
  System.Classes,
  System.SysUtils,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.ImgList,
  System.UITypes;

type
  TTreeNode = class;

  TCustomTree = class(TList)
  private
    FOnChange : TNotifyEvent;
    FUpdates  : Integer;
    FChanged  : Boolean;
    FImageList: TImageList;
  public
    procedure Clear; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Changed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property ImageList: TImageList read FImageList write FImageList;
  end;

  TTreeNode = class
  private
    FTree      : TCustomTree;
    FParent    : TTreeNode;
    FLevel     : Integer;
    FText      : string;
    FChilds    : TTreeNode;
    FSibling   : TTreeNode;
    FHasChilds : Boolean;
    FExpanded  : Boolean;
    FOnExpand  : TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FSorted    : Boolean;
    FImageIndex: TImageIndex;
    function GetPath: string;
  protected
    procedure Add(AChild: TTreeNode);
    procedure Remove(AChild: TTreeNode);
    procedure Expand; virtual;
    procedure CollapseChilds(Position: Integer);
    function Compare(AChild: TTreeNode): Integer; virtual;
  public
    constructor Create(ATree: TCustomTree; AParent: TTreeNode; const AText: string; AExpandable : Boolean = False); virtual;
    destructor Destroy; override;
    procedure DrawText(Canvas: TCanvas; var Rect: TRect); virtual;
    procedure Draw(Canvas: TCanvas; Rect: TRect; State: TOwnerDrawState); virtual;
    procedure Click; virtual;
    procedure DblClick; virtual;
    function ChildCount: Integer;
    function FirstChild: TTreeNode;
    function NextChild(var Child: TTreeNode): Boolean;
    procedure Clear;
    procedure Collapse;
    function IsEmpty: Boolean;
    property Tree: TCustomTree read FTree;
    property Sorted: Boolean read FSorted write FSorted;
    property Text: string read FText write FText;
    property Level: Integer read FLevel;
    property Expanded: Boolean read FExpanded;
    property OnExpand: TNotifyEvent read FOnExpand write FOnExpand;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property Parent: TTreeNode read FParent;
    property ImageIndex: TImageIndex read FImageIndex write FImageIndex;
    property Path: string read GetPath;
  end;

  TGenericTree<T:TTreeNode> = class(TCustomTree)
  private
    function GetItem(Index: Integer): T;
  public
    function AddNode(AParent: T; const AText: string; AExpandable: Boolean = False): T;
    property Items[Index: Integer]: T read GetItem; default;
  end;

  TTreeList = TGenericTree<TTreeNode>;

implementation

{ Utils }

procedure Arrow(Canvas: TCanvas; const Rect: TRect; Offset: Integer; Opened: Boolean);
var
  Poly : array[0..2] of TPoint;
  Color: TColor;
begin
  if Opened then
  begin
    Poly[0] := Point(Rect.Left + Offset, Rect.Top + (Rect.Height - 6) div 2);
    Poly[1] := Point(Poly[0].x, Poly[0].y + 6);
    Poly[2] := Point(Poly[0].x - 6, Poly[1].y);
  end else begin
    Poly[0] := Point(Rect.Left + Offset, Rect.Top + (Rect.Height - 8) div 2);
    Poly[1] := Point(Poly[0].x + 4, Poly[0].y + 4);
    Poly[2] := Point(Poly[0].x, Poly[1].y + 4);
  end;
  Color := Canvas.Brush.Color;
  Canvas.Brush.Color := clBlack;
  Canvas.Polygon(Poly);
  Canvas.Brush.Color := Color;
end;

procedure RectOut(Canvas: TCanvas; const Rect: TRect; Offset: Integer; const Str: string);
var
  Size: TSize;
begin
  Size := Canvas.TextExtent(Str);
  Canvas.TextOut(Offset, Rect.Top + (Rect.Height - Size.cy) div 2, Str);
end;

{ TCustomTree }

procedure TCustomTree.BeginUpdate;
begin
  Inc(FUpdates);
end;

procedure TCustomTree.Changed;
begin
  if FUpdates > 0 then
    FChanged := True
  else begin
    FChanged := False;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TCustomTree.Clear;
begin
  BeginUpdate;
  try
    while Count > 0 do
      TTreeNode(List[0]).Free;
    inherited;
  finally
    EndUpdate;
  end;
end;

procedure TCustomTree.EndUpdate;
begin
  Dec(FUpdates);
  if (FUpdates = 0) {and FChanged} then
    Changed;
end;

{ TGenericTree<T> }

function TGenericTree<T>.AddNode(AParent: T; const AText: string;
  AExpandable: Boolean): T;
begin
  Result := T.Create(Self, AParent, AText, AExpandable);
end;

function TGenericTree<T>.GetItem(Index: Integer): T;
begin
  Result := List[Index];
end;

{ TTreeNode }

procedure TTreeNode.Add(AChild: TTreeNode);
var
  Prev: TTreeNode;
  Next: TTreeNode;
begin
  AChild.FParent := Self;
  FHasChilds := True;
  if FChilds = nil then
  begin
    AChild.FSibling := AChild;
    FChilds := AChild;
  end else begin
    if FSorted then
    begin
      Next := FChilds;
      repeat
        Prev := Next;
        Next := Prev.FSibling;
        if AChild.Compare(Next) < 0 then
        begin
          Prev.FSibling := AChild;
          AChild.FSibling := Next;
          FTree.Changed;
          Exit;
        end;
      until Next = FChilds;
    end;
    AChild.FSibling := FChilds.FSibling;
    FChilds.FSibling := AChild;
    FChilds := AChild;
  end;
  FTree.Changed;
end;

function TTreeNode.ChildCount: Integer;
var
  Child: TTreeNode;
begin
  Result := 0;
  if FChilds <> nil then
  begin
    Child := FChilds;
    repeat
      Child := Child.FSibling;
      Inc(Result);
    until Child = FChilds;
  end;
end;

procedure TTreeNode.Clear;
var
  Child: TTreeNode;
  Next : TTreeNode;
begin
  if FChilds <> nil then
  begin
    FTree.BeginUpdate;
    try
      FExpanded := False; // important
      Next := FChilds;
      repeat
        Child := Next;
        Next  := Child.FSibling;
        Child.FParent := nil; // to avoid Remove
        Child.Free;
      until Next = FChilds; // NB objects are destroyed but the test still valid
      FChilds := nil;
      FHasChilds := False;
    finally
      FTree.EndUpdate;
    end;
  end;
end;

procedure TTreeNode.Click;
begin
// Empty
end;

procedure TTreeNode.Collapse;
begin
  if FExpanded then
    DblClick;
end;

procedure TTreeNode.CollapseChilds(Position: Integer);
var
  Child: TTreeNode;
begin
  if FExpanded then
  begin
    if FChilds <> nil then
    begin
      Child := FChilds;
      repeat
        Child := Child.FSibling;
        Child.CollapseChilds(Position + 1);
        FTree.Delete(Position);
      until Child = FChilds;
    end;
    FExpanded := False;
  end;
end;

function TTreeNode.Compare(AChild: TTreeNode): Integer;
begin
  Result := AnsiCompareText(FText, AChild.FText);
end;

constructor TTreeNode.Create(ATree: TCustomTree; AParent: TTreeNode;
  const AText: string; AExpandable: Boolean);
begin
  FTree := ATree;
  FImageIndex := -1;
  FParent := AParent;
  FText := AText;
  FHasChilds := AExpandable;
  if FParent = nil then
  begin
    FTree.Add(Self);
    FTree.Changed;
  end else begin
    FParent.Add(Self);
    FLevel := AParent.FLevel + 1;
  end;
end;

procedure TTreeNode.DblClick;
var
  Position: Integer;
  Child   : TTreeNode;
begin
  Position := FTree.IndexOf(Self) + 1;
  if FExpanded then
  begin
    CollapseChilds(Position);
  end else begin
    if FHasChilds and (FChilds = nil) then
    begin
      Expand;
      FHasChilds := FChilds <> nil;
      if FHasChilds = False then
      begin
        FTree.Changed; // erase the triangle
        Exit;
      end;
    end;
    if FHasChilds = False then
      Exit;
    Child := FChilds;
    repeat
      Child := Child.FSibling;
      FTree.Insert(Position, Child);
      Inc(Position);
    until Child = FChilds;
    FExpanded := True;
  end;
  FTree.Changed;
end;

destructor TTreeNode.Destroy;
begin
  FTree.BeginUpdate;
  try
    Clear;
    if (FParent = nil) or (FParent.FExpanded) then
      FTree.Remove(Self);
    if FParent <> nil then
      FParent.Remove(Self);
  finally
    FTree.EndUpdate;
  end;
  inherited;
end;

procedure TTreeNode.Draw(Canvas: TCanvas; Rect: TRect; State: TOwnerDrawState);
begin
  with Canvas do
  begin
    FillRect(Rect);
    if FExpanded then
    begin
      Arrow(Canvas, Rect, 2 + 10 * FLevel + 6, True);
    end else
    if FHasChilds then
    begin
      Arrow(Canvas, Rect, 4 + 10 * FLevel, False);
    end;
    Inc(Rect.Left, 15 + 10 * FLevel);
    DrawText(Canvas, Rect);
  end;
end;

procedure TTreeNode.DrawText(Canvas: TCanvas; var Rect: TRect);
var
  Size: TSize;
begin
  with Canvas do
  begin
    if FLevel = 0 then
      Font.Style := [fsBold]
    else
      Font.Style := [];
    Size := Canvas.TextExtent(FText);
    if (FImageIndex >= 0) and (FTree.FImageList <> nil) then
    begin
      FTree.FImageList.Draw(Canvas, Rect.Left, Rect.Top + (Rect.Height - FTree.FImageList.Height) div 2, FImageIndex);
      Inc(Rect.Left, FTree.FImageList.Width + 2);
    end;
    TextOut(Rect.Left, Rect.Top + (Rect.Height - Size.cy) div 2, FText);
  end;
end;

procedure TTreeNode.Expand;
begin
  if Assigned(FOnExpand) then
    FOnExpand(Self);
end;

function TTreeNode.FirstChild: TTreeNode;
begin
  if FChilds = nil then
    Result := nil
  else
    Result := FChilds.FSibling;
end;

function TTreeNode.GetPath: string;
var
  Node: TTreeNode;
begin
  Result := Text;
  Node := Parent;
  while Node <> nil do
  begin
    Result := Node.Text + '/' + Result;
    Node := Node.Parent;
  end;
end;

function TTreeNode.IsEmpty: Boolean;
begin
  Result := FChilds = nil;
end;

function TTreeNode.NextChild(var Child: TTreeNode): Boolean;
begin
  if Child = FChilds then
    Result := False
  else begin
    Result := True;
    Child := Child.FSibling;
  end;
end;

procedure TTreeNode.Remove(AChild: TTreeNode);
var
  Prev: TTreeNode;
  Next: TTreeNode;
begin
  if FChilds = nil then
    Exit;
  Next := FChilds;
  repeat
    Prev := Next;
    Next := Prev.FSibling;
    if Next = AChild then
    begin
      Prev.FSibling := Next.FSibling;
      if FChilds = AChild then
      begin
        if Prev = Next then
        begin
          FChilds := nil;
          FHasChilds := False;
        end else begin
          FChilds := Prev;
        end;
      end;
      Exit;
    end;
  until Next = FChilds;
end;

end.
