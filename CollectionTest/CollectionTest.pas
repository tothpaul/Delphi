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
    FOwner       : TComponent;
    FMyCollection: TMyCollection;
    procedure SetMyCollection(Value: TMyCollection);
  protected
    function GetOwner: TPersistent; override; // Required by  TCollectionProperty
  public
    constructor Create(AOwner: TComponent);
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

constructor TMyProperty.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FMyCollection := TMyCollection.Create(Self, TMyItem);
end;

destructor TMyProperty.Destroy;
begin
  FMyCollection.Free;
  inherited;
end;

function TMyProperty.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TMyProperty.SetMyCollection(Value: TMyCollection);
begin
  FMyCollection.Assign(Value);
end;

{ TMyComponent }

constructor TMyComponent.Create(AOwner: TComponent);
begin
  inherited;
  FMyProperty := TMyProperty.Create(Self);
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