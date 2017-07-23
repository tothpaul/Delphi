unit UMain;
{
   PNGLoader test application (c)2007 Execute SARL
   http://www.execute.fr

}
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Image1: TImage;
    ListBox1: TListBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { Déclarations privées }
    procedure LoadImage(const AFileName: string);
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Execute.PNGLoader;

const
// Path to the PngSuite : http://www.schaik.com/pngsuite/
  PngSuite = '..\..\PngSuite-2017jul19\';

procedure TForm1.FormCreate(Sender: TObject);
var
  SR: TSearchRec;
begin
  Application.Title := Caption;

  if FindFirst(PngSuite + '*.png', faAnyFile, SR) = 0 then
  begin
    repeat
    // remove unsupported formats
      if (Pos('02.png', SR.Name) = 0)
      and(Pos('04.png', SR.Name) = 0)
      and(Pos('16.png', SR.Name) = 0)
      and (SR.Name[4] <> 'i') // interlaced image are not supported
      and (SR.Name[1] <> 'x') // remove also false PNG to avoid complaints :)
      then
        ListBox1.Items.Add(SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;

end;

procedure TForm1.ListBox1Click(Sender: TObject);
var
  Index: Integer;
  Name : string;
begin
  Index := ListBox1.ItemIndex;
  if Index < 0 then
    Exit;
  Name := ListBox1.Items[Index];
  Caption := Application.Title + ' [' +  Name + ']';
  LoadImage(PngSuite + Name);
end;

procedure TForm1.LoadImage(const AFileName: string);
var
  Bitmap: TBitmap;
begin
  Image1.Picture.Graphic := nil;
  try
    Bitmap := LoadPNG(AFileName);
  except
    on e:Exception do
    begin
      Label1.Caption := e.Message;
      Exit;
    end;
  end;
  Image1.Picture.Assign(Bitmap);
  Label1.Caption := AFileName;
  Bitmap.Free;
end;

end.
