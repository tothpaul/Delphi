program cafe;

uses
  Forms,
  cafe1 in 'cafe1.pas' {Cafetiere};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TCafetiere, Cafetiere);
  Application.Run;
end.
