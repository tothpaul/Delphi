unit FMXMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,  Execute.Fmx.RichEditor,
  FMX.Objects, System.ImageList, FMX.ImgList, FMX.Layouts;

type
  TForm1 = class(TForm)
    ImageList1: TImageList;
    Image1: TImage;
    Image2: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
    FRichEdit: TRichEditor;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

uses Execute.RichDocuments;

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
var
  Normal    : TRichStyleItem;
  Heading   : TRichStyleItem;
  SubHeading: TRichStyleItem;
  Jump1     : TRichStyleItem;
begin
  FRichEdit := TRichEditor.Create(Self);
  FRichEdit.Parent := Self;
  FRichEdit.Align := TAlignLayout.Client;

  with FRichEdit.Document do
  begin
    Images := ImageList1;

    // set the attributes
    SetFont('MS Sans Serif', [], 10);
    TextColor := TAlphaColorRec.Black;
    // this define a style
    Normal := TextStyle;

    // heading style
    SetFont('Arial', [TFontStyle.fsBold, TFontStyle.fsItalic], 16);
    TextColor := TAlphaColorRec.Red;
    Heading := TextStyle;

    // subheading style
    SetFont('Arial', [TFontStyle.fsBold, TFontStyle.fsItalic], 12);
    TextColor := TAlphaColorRec.Blue;
    SubHeading := TextStyle;

    // jump1 style
    SetFont('Times New Roman', [TFontStyle.fsItalic, TFontStyle.fsUnderline], 12);
    TextColor := TAlphaColorRec.Green;
    Jump1 := TextStyle;

    // use Textstyle
    TextStyle := Heading;
    AddCenterLine('TRichView Demo and Help Program');

    AddImage(Image1);
    Center;
    NewLine;

    TextStyle := Normal;
    AddCenterLine('(Copyright(c) 1997,98 by Tkachenko S.V.)');
    AddCenterLine('(Copyright(c) 2018 by Execute SARL.)');

    TextStyle := SubHeading;
    AddCenterLine('Contents');
    NewLine;

    AddBullet(0);
    TextStyle := Normal;
    Add('    1.   ');
    TextStyle := Jump1;
    Add('Introduction');
    NewLine;

    AddBullet(0);
    TextStyle := Normal;
    Add('    2.   ');
    TextStyle := Jump1;
    Add('Step1: Text Styles');
    NewLine;

    AddBullet(0);
    TextStyle := Normal;
    Add('    3.   ');
    TextStyle := Jump1;
    Add('Step2: Adding Text');
    NewLine;

    AddBullet(0);
    TextStyle := Normal;
    Add('    4.   ');
    TextStyle := Jump1;
    Add('Step3: Adding Pictures');
    NewLine;

    AddBullet(0);
    TextStyle := Normal;
    Add('    5.   ');
    TextStyle := Jump1;
    Add('Step4: Adding Delphi Controls');
    NewLine;

    AddBullet(0);
    TextStyle := Normal;
    Add('    6.   ');
    TextStyle := Jump1;
    Add('Step5: Hypertext');
    NewLine;

    AddBullet(0);
    TextStyle := Normal;
    Add('    7.   ');
    TextStyle := Jump1;
    Add('Step6: Other Properties');
    NewLine;

    AddBullet(0);
    TextStyle := Normal;
    Add('    8.   ');
    TextStyle := Jump1;
    Add('New in version 0.3');
    NewLine;

    AddBullet(0);
    TextStyle := Normal;
    Add('    9.   ');
    TextStyle := Jump1;
    Add('New in version 0.4 - Printing');
    NewLine;

    AddBullet(0);
    TextStyle := Normal;
    Add('   10.   ');
    TextStyle := Jump1;
    Add('New in version 0.4 - Saving');
    NewLine;

    AddBullet(0);
    TextStyle := Normal;
    Add('   11.   ');
    TextStyle := Jump1;
    Add('New in version 0.4 - Other features');
    NewLine;

    AddBullet(0);
    TextStyle := Normal;
    Add('   12.   ');
    TextStyle := Jump1;
    Add('New in version 0.5');
    NewLine;

    AddBullet(2);
    TextStyle := Normal;
    Add('Appendix.   ');
    TextStyle := Jump1;
    Add('Shareware versions');
    NewLine;

    AddBreak;

    TextStyle := SubHeading;
    AddCenterLine('Introduction');
    NewLine;

    TextStyle := Normal;
    AddLines(
       '    TRichView controls can contain:'#13#10+
       '    - text with various fonts'#13#10+
       '    - hypertext'#13#10+
       '    - pictures'#13#10+
       '    - pictures from Image Lists'#13#10+
       '    - any Delphi controls'#13#10+
       '    First program with TRichView:'#13#10+
       '    1) at design time create TRVStyle control (RVStyle1)'#13#10+
       '    2) at design time create TRichView control (RichView1)'#13#10+
       '    3) at design time in Object Inspector set RichView1.Style := RVStyle1'#13#10+
       '    5) in FormCreate event handler write: "RichView1.AddFromNewLine(''Hello world!'', 0); RichView1.Format;"'
    );

    AddBreak;

    TextStyle := SubHeading;
    AddCenterLine('Step1: TextStyles');
    TextStyle := Normal;
    AddCenterLine('(TRVStyle Control)');
    AddImage(Image2);
    Center;
    NewLine;
    NewLine;
  end;
end;

end.
