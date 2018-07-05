unit Execute.Tuple;

{ Variant Tuple for Delphi Tokyo (c)2018 Execute SARL }

{
procedure AfficheMonTuple(const ATuple: Variant);
begin
  ShowMessage(string(ATuple.Prenom) + ' ' + string(ATuple.Nom) + ' ' + string(ATuple.Age) + ' ' + string(ATuple.Taille));
end;

var
  v: Variant;
begin
  TTuple.Init(v);
  v.Nom := 'Durand';
  v.Prenom := 'Pierre';
  v.Age := 45;
  v.Taille := 1.80;
  AfficheMonTuple(v);
end;

}

interface

uses
  System.SysUtils,
  System.Variants;

type
  TTuple = class(TInvokeableVariantType)
  private const
    VTYPE = $0112;
  public
  // basic functions
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
  // properties
    function SetProperty(const V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
  // initialization
    class procedure Init(var V: Variant);
  end;

implementation

var
  Instance: TTuple;

type
  TProperty = record
    Name : string;
    Value: Variant;
  end;
  TProperties = TArray<TProperty>;

function GetPropIndex(var Props: TProperties; const Name: string): Integer;
begin
  Result := Length(Props) - 1;
  while Result >= 0 do
  begin
    if Props[Result].Name = Name then
      Exit;
    Dec(Result);
  end;
end;

procedure SetTupleProperty(var Props: TProperties; const Name: string; const Value: TVarData);
var
  Index: Integer;
begin
  Index := GetPropIndex(Props, Name);
  if Index < 0 then
  begin
    Index := Length(Props);
    SetLength(Props, Index + 1);
    Props[Index].Name := Name;
  end;
  Props[Index].Value := Variant(Value);
end;

function GetTupleProperty(var Props: TProperties; const Name: string; var Dest: TVarData): Boolean;
var
  Index: Integer;
begin
  Index := GetPropIndex(Props, Name);
  if Index < 0 then
    Result := False
  else begin
    Result := True;
    Variant(Dest) := Props[Index].Value;
  end;
end;

class procedure TTuple.Init(var V: Variant);
begin
  if Instance = nil then
    Instance := TTuple.Create(VTYPE);
  VarClear(V);
  TVarData(V).VType := VTYPE;
end;

procedure TTuple.Clear(var V: TVarData);
begin
  if V.VType = VType then
    TProperties(V.VPointer) := nil;
end;

procedure TTuple.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  init(Variant(Dest));
  if Source.VType = VTYPE then
  begin
    if Source.VPointer <> nil then
    begin
      if Indirect then
      begin
        Dest.VPointer := Source.VPointer;
      end else begin
        TProperties(Dest.VPointer) := System.Copy(TProperties(Source.VPointer));
      end;
    end;
  end;
end;

function TTuple.SetProperty(const V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
var
  P: PPointer;
begin
  if V.VType = VTYPE then
  begin
    P := @V.VPointer;
    SetTupleProperty(TProperties(P^), Name, Value);
    Result := True;
  end else begin
    Result := False;
  end;
end;

function TTuple.GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean;
var
  P: PPointer;
begin
  if V.VType = VTYPE then
  begin
    P := @V.VPointer;
    Result := GetTupleProperty(TProperties(P^), Name, Dest)
  end else begin
    Result := False;
  end;
end;


end.
