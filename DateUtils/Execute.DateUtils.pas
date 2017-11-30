unit Execute.DateUtils;

{
   (c)2017 by Execute SARL <contact@execute.fr>
}

interface
{-$DEFINE TEST}
uses
{$IFDEF TEST}
  Winapi.Windows,
{$ENDIF}
  System.Types,
  System.SysUtils;

type
  TDateOnly = record
  private
    Date: TDate;
  public
    function Compare(a: TDateOnly): TValueRelationship;
    class operator Implicit(a: TDateOnly): TDate; inline;
    class operator Implicit(a: TDate): TDateOnly; inline;
    class operator Equal(a, b: TDateOnly): Boolean; inline;
    class operator NotEqual(a, b: TDateOnly): Boolean; inline;
    class operator GreaterThan(a, b: TDateOnly) : boolean; inline;
    class operator LessThan(a, b: TDateOnly): Boolean; inline;
    class operator GreaterThanOrEqual(a, b: TDateOnly) : boolean; inline;
    class operator LessThanOrEqual(a, b: TDateOnly): Boolean; inline;
  end;

  TTimeOnly = record
  private
    Time: TTime;
  public
    function TimeStamp: TTimeStamp;
    function Compare(a: TTimeOnly): TValueRelationship;
    class operator Implicit(a: TTimeOnly): TTime; inline;
    class operator Implicit(a: TTime): TTimeOnly; inline;
    class operator Equal(a, b: TTimeOnly): Boolean; inline;
    class operator NotEqual(a, b: TTimeOnly): Boolean; inline;
    class operator GreaterThan(a, b: TTimeOnly) : boolean; inline;
    class operator LessThan(a, b: TTimeOnly): Boolean; inline;
    class operator GreaterThanOrEqual(a, b: TTimeOnly) : boolean; inline;
    class operator LessThanOrEqual(a, b: TTimeOnly): Boolean; inline;
  end;

  TDateAndTime = record
  private
    DateTime: TDateTime;
  public
    function TimeStamp: TTimeStamp;
    function Compare(a: TDateAndTime): TValueRelationship;
    class operator Implicit(a: TDateAndTime): TDateTime; inline;
    class operator Implicit(a: TDateTime): TDateAndTime; inline;
    class operator Equal(a, b: TDateAndTime): Boolean; inline;
    class operator NotEqual(a, b: TDateAndTime): Boolean; inline;
    class operator GreaterThan(a, b: TDateAndTime) : boolean; inline;
    class operator LessThan(a, b: TDateAndTime): Boolean; inline;
    class operator GreaterThanOrEqual(a, b: TDateAndTime) : boolean; inline;
    class operator LessThanOrEqual(a, b: TDateAndTime): Boolean; inline;
  end;

implementation


{ TDateOnly }

function TDateOnly.Compare(a: TDateOnly): TValueRelationship;
begin
  if Trunc(Date) = Trunc(a.Date) then
    Result := EqualsValue
  else begin
    if Date < a.Date then
      Result := LessThanValue
    else
      Result := GreaterThanValue;
  end;
end;

class operator TDateOnly.Equal(a, b: TDateOnly): Boolean;
begin
  Result := a.Compare(b) = EqualsValue;
end;

class operator TDateOnly.GreaterThan(a, b: TDateOnly): boolean;
begin
  Result := a.Compare(b) = GreaterThanValue;
end;

class operator TDateOnly.GreaterThanOrEqual(a, b: TDateOnly): boolean;
begin
  Result := a.Compare(b) <> LessThanValue;
end;

class operator TDateOnly.Implicit(a: TDateOnly): TDate;
begin
  Result := Trunc(a.Date);
end;

class operator TDateOnly.Implicit(a: TDate): TDateOnly;
begin
  Result.Date := a;
end;

class operator TDateOnly.LessThan(a, b: TDateOnly): Boolean;
begin
  Result := a.Compare(b) = LessThanValue;
end;

class operator TDateOnly.LessThanOrEqual(a, b: TDateOnly): Boolean;
begin
  Result := a.Compare(b) <> GreaterThanValue;
end;

class operator TDateOnly.NotEqual(a, b: TDateOnly): Boolean;
begin
  Result := a.Compare(b) <> EqualsValue;
end;

{ TTimeOnly }

function TTimeOnly.Compare(a: TTimeOnly): TValueRelationship;
begin
  if TimeStamp.Time = a.TimeStamp.Time then
    Result := EqualsValue
  else begin
    if Abs(Time) < Abs(a.Time) then
      Result := LessThanValue
    else
      Result := GreaterThanValue;
  end;
end;

class operator TTimeOnly.Equal(a, b: TTimeOnly): Boolean;
begin
  Result := a.Compare(b) = EqualsValue;
end;

class operator TTimeOnly.GreaterThan(a, b: TTimeOnly): boolean;
begin
  Result := a.Compare(b) = GreaterThanValue;
end;

class operator TTimeOnly.GreaterThanOrEqual(a, b: TTimeOnly): boolean;
begin
  Result := a.Compare(b) <> LessThanValue;
end;

class operator TTimeOnly.Implicit(a: TTimeOnly): TTime;
begin
  Result := Frac(a.Time);
end;

class operator TTimeOnly.Implicit(a: TTime): TTimeOnly;
begin
  Result.Time := a;
end;

class operator TTimeOnly.LessThan(a, b: TTimeOnly): Boolean;
begin
  Result := a.Compare(b) = LessThanValue;
end;

class operator TTimeOnly.LessThanOrEqual(a, b: TTimeOnly): Boolean;
begin
  Result := a.Compare(b) <> GreaterThanValue;
end;

class operator TTimeOnly.NotEqual(a, b: TTimeOnly): Boolean;
begin
  Result := a.Compare(b) <> EqualsValue;
end;

function TTimeOnly.TimeStamp: TTimeStamp;
begin
  Result := DateTimeToTimeStamp(Time);
end;

{ TDateAndTime }

function TDateAndTime.Compare(a: TDateAndTime): TValueRelationship;
begin
  Result := TDateOnly(DateTime).Compare(TDateOnly(a.DateTime));
  if Result = EqualsValue then
    Result := TTimeOnly(DateTime).Compare(TTimeOnly(a.DateTime));
end;

class operator TDateAndTime.Equal(a, b: TDateAndTime): Boolean;
begin
  Result := a.Compare(b) = EqualsValue;
end;

class operator TDateAndTime.GreaterThan(a, b: TDateAndTime): boolean;
begin
  Result := a.Compare(b) = GreaterThanValue;
end;

class operator TDateAndTime.GreaterThanOrEqual(a, b: TDateAndTime): boolean;
begin
  Result := a.Compare(b) <> LessThanValue;
end;

class operator TDateAndTime.Implicit(a: TDateAndTime): TDateTime;
begin
  Result := a.DateTime;
end;

class operator TDateAndTime.Implicit(a: TDateTime): TDateAndTime;
begin
  Result.DateTime := a;
end;

class operator TDateAndTime.LessThan(a, b: TDateAndTime): Boolean;
begin
  Result := a.Compare(b) = LessThanValue;
end;

class operator TDateAndTime.LessThanOrEqual(a, b: TDateAndTime): Boolean;
begin
  Result := a.Compare(b) <> GreaterThanValue;
end;

class operator TDateAndTime.NotEqual(a, b: TDateAndTime): Boolean;
begin
  Result := a.Compare(b) <> EqualsValue;
end;

function TDateAndTime.TimeStamp: TTimeStamp;
begin
  Result := DateTimeToTimeStamp(DateTime);
end;

{$IFDEF TEST}
procedure test();
const
  V : array[TValueRelationship] of string = ('<', '=', '>');
var
  a, b: TDateTime;

  procedure compare();
  var
    d1, d2: TDateOnly;
    t1, t2: TTimeOnly;
    dt1, dt2: TDateAndTime;
  begin
    WriteLn('comparing ', DateTimeToStr(a), ' and ', DateTimeToStr(b), ': a = b ', a = b);
    d1 := a;
    d2 := b;
    WriteLn('d1  ', V[d1.compare(d2)], '  d2');
    WriteLn('d1  <  d2 ', d1 <  d2);
    WriteLn('d1  <= d2 ', d1 <= d2);
    WriteLn('d1  =  d2 ', d1 =  d2);
    WriteLn('d1  <> d2 ', d1 <> d2);
    WriteLn('d1  >= d2 ', d1 >= d2);
    WriteLn('d1  >  d2 ', d1 >  d2);
    t1 := a;
    t2 := b;
    WriteLn('t1  ', V[t1.compare(t2)], '  t2');
    WriteLn('t1  <  t2 ', t1 <  t2);
    WriteLn('t1  <= t2 ', t1 <= t2);
    WriteLn('t1  =  t2 ', t1 =  t2);
    WriteLn('t1  <> t2 ', t1 <> t2);
    WriteLn('t1  >= t2 ', t1 >= t2);
    WriteLn('t1  >  t2 ', t1 >  t2);
    dt1 := a;
    dt2 := b;
    WriteLn('dt1  ', V[dt1.compare(dt2)], '  dt2');
    WriteLn('dt1  <  dt2 ', dt1 <  dt2);
    WriteLn('dt1  <= dt2 ', dt1 <= dt2);
    WriteLn('dt1  =  dt2 ', dt1 =  dt2);
    WriteLn('dt1  <> dt2 ', dt1 <> dt2);
    WriteLn('dt1  >= dt2 ', dt1 >= dt2);
    WriteLn('dt1  >  dt2 ', dt1 >  dt2);
  end;

begin
  a := Now();
  b := Now();
  compare();
  b := b + 1;
  compare();
  b := b - 2;
  compare();
end;

initialization
  AllocConsole;
  test();
  ReadLn;
{$ENDIF}
end.
