unit CollectionTest;

interface

uses
  System.Classes;

type
  TMyItem = class(TCollectionItem)

  end;

  TMyCollection = class(TOwnedCollection)

  end;

  TMyProperty = class(TPersistent)
  private
    FMyCollection: TMyCollection;
    procedure SetMyCollection(Value: TMyCollection);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property MyCollection: TMyCollection read FMyCollection write SetMyCollection;
  end;

  TMyComponent = class(TComponent)
  private
    FMyProperty: TMyProperty;
    FMyCollection: TMyCollection;
//    procedure SetMyProperty(Value: TMyProperty);
    procedure SetMyCollection(Value: TMyCollection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MyProperty: TMyProperty read FMyProperty{ write SetMyProperty};
    property MyCollection: TMyCollection read FMyCollection write SetMyCollection;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Execute SARL', [TMyComponent]);
end;

{ TMyProperty }

constructor TMyProperty.Create;
begin
  inherited Create;
  FMyCollection := TMyCollection.Create(Self, TMyItem);
end;

destructor TMyProperty.Destroy;
begin
  FMyCollection.Free;
  inherited;
end;

procedure TMyProperty.SetMyCollection(Value: TMyCollection);
begin
  FMyCollection.Assign(Value);
end;

{ TMyComponent }

constructor TMyComponent.Create(AOwner: TComponent);
begin
  inherited;
  FMyProperty := TMyProperty.Create;
  FMyCollection := TMyCollection.Create(Self, TMyItem);
end;

destructor TMyComponent.Destroy;
begin
  FMyProperty.Free;
  FMyCollection.Free;
  inherited;
end;

procedure TMyComponent.SetMyCollection(Value: TMyCollection);
begin
  FMyCollection.Assign(Value);
end;

{
procedure TMyComponent.SetMyProperty(Value: TMyProperty);
begin
  FMyProperty.Assign(Value);
end;
}
end.
