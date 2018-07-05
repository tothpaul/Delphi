unit cafe1;

// Simulateur de Cafetière v1.0 par Paul TOTH <tothpaul@free.fr>

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TCafetiere = class(TForm)
    Panel3: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Verre: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    btOff: TPanel;
    btOn: TPanel;
    Led: TShape;
    FondCafe: TShape;
    HautCafe: TShape;
    Shape8: TShape;
    Shape9: TShape;
    Button1: TButton;
    Panel1: TPanel;
    Eau: TShape;
    Cafe: TShape;
    Coule: TShape;
    Eau2: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure btOnClick(Sender: TObject);
    procedure btOffClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
    fNbDoseEau:integer;
    fNbDoseCafe:integer;
    Active:boolean;
    procedure SetNbDoseEau(value:integer);
    procedure SetNbDoseCafe(value:integer);
    procedure Open;
    Function EOF:boolean;
    procedure close;
    procedure Tasse_BringToFront_to_Joel;
  public
    { Déclarations publiques }
    property NbDoseEau:integer read fNbDoseEau write SetNbDoseEau;
    property NbDoseCafe:integer read fNbDoseCafe write SetNbDoseCafe;
  end;

var
  Cafetiere: TCafetiere;

implementation

{$R *.DFM}

procedure TCafetiere.FormCreate(Sender: TObject);
begin
 NbDoseEau:=0;
 NbDoseCafe:=0;
 FondCafe.Top:=240+17;
 HautCafe.Top:=184+49; HautCafe.Height:=0;
 Eau2.hide;
 Coule.Hide;
 Active:=False;
end;

procedure TCafetiere.btOnClick(Sender: TObject);
begin
 Led.Brush.Color:=clLime;
 btOn.Color:=clBtnFace;
 btOff.Color:=clWhite;
end;

procedure TCafetiere.btOffClick(Sender: TObject);
begin
 Led.Brush.Color:=clGray;
 btOn.Color:=clWhite;
 btOff.Color:=clBtnFace;
end;

procedure TCafetiere.SetNbDoseEau(value:integer);
 var
  i:integer;
 begin
  fNbDoseEau:=Value;
  if Value=0 then
   Eau.Top:=11+180
  else
   for i:=0 to 45 do begin
    Eau.Top:=Eau.Top-1;
    Application.ProcessMessages;
    Sleep(50);
   end;
 end;

procedure TCafetiere.SetNbDoseCafe(value:integer);
 var
  i:integer;
 begin
  fNbDoseCafe:=Value;
  if Value=0 then
   Cafe.Height:=0
  else begin
   While Cafe.Height<50 do begin
    Cafe.Height:=Cafe.Height+10;
    Application.ProcessMessages;
    Sleep(50);
   end;
   Cafe.Height:=0;
  end;
 end;

procedure TCafetiere.Open;
 begin
  Active:=True;
  btOnclick(self);
 end;

Function TCafetiere.EOF:boolean;
 begin
  application.processmessages;
  if Eau.Top<11+200 then begin
   if Eau.Top>11+180 then Eau2.hide else Eau2.Show;
   Result:=False;
   Sleep(500);
   Coule.Show;
   Eau.Top:=Eau.Top+2;
   if FondCafe.Top>240 then
    FondCafe.Top:=FondCafe.Top-1
   else begin
    HautCafe.Height:=HautCafe.Height+1;
    HautCafe.Top:=HautCafe.Top-1;
   end;
  end else begin
   Coule.Hide;
   Result:=True;
  end;
 end;

procedure TCafetiere.Close;
 begin
  btOffClick(self);
 end;

procedure TCafetiere.Tasse_BringToFront_to_Joel;
 begin
  ShowMessage('Merci Joel ! ton programme marche bien !');
 end;


procedure TCafetiere.Button1Click(Sender: TObject);
begin
//----------------------------------------------------------------------------//
// From nzn.fr.delphi
//Salut kloo. Delphi sachant tout faire, pourrait-il déclencher toutes les
//demi-heures l'algorithme suivant :
    While Cafetiere.NbDoseEau < 3 Do Begin
      //  mettre une dose d'eau dans la cafetière
        Cafetiere.NbDoseEau := Cafetiere.NbDoseEau+1
    End;
    While Cafetiere.NbDoseCafe < 3 Do Begin
      //  mettre une dose de café dans la cafetière
        Cafetiere.NbDoseCafe := Cafetiere.NbDoseCafe+1
    End;
    If Not Cafetiere.Active Then Cafetiere.Open;
    Repeat
    Until Cafetiere.Eof;
    Cafetiere.Close;
    Cafetiere.Tasse_BringToFront_to_Joel

//kloo a écrit dans le message ...
//>>Est-il possible de jouer des Real VIDEOS sous Delphi ?
//>
//>
//>oui
//>
//>puisqu'on peut tout faire sous delphi :-))
//>
//>comment ? la n'est pas la question ;-)))))))
//>
//>kloo, bruit en tout genre
//>
//----------------------------------------------------------------------------//
end;

end.
