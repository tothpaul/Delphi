object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Ascii Shape Editor'
  ClientHeight = 469
  ClientWidth = 891
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = ComboBox1Click
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 407
    Top = 38
    Width = 16
    Height = 16
  end
  object Image2: TImage
    Left = 442
    Top = 38
    Width = 32
    Height = 32
  end
  object Image3: TImage
    Left = 488
    Top = 38
    Width = 48
    Height = 48
  end
  object Image4: TImage
    Left = 552
    Top = 38
    Width = 64
    Height = 64
  end
  object Image5: TImage
    Left = 633
    Top = 38
    Width = 96
    Height = 96
  end
  object Image6: TImage
    Left = 743
    Top = 38
    Width = 128
    Height = 128
  end
  object Image7: TImage
    Left = 407
    Top = 232
    Width = 196
    Height = 196
  end
  object Image8: TImage
    Left = 615
    Top = 172
    Width = 256
    Height = 256
  end
  object Label1: TLabel
    Left = 8
    Top = 438
    Width = 705
    Height = 23
    Cursor = crHandPoint
    Caption = 
      'based on http://cocoamine.net/blog/2015/03/20/replacing-photosho' +
      'p-with-nsstring/'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    OnClick = Label1Click
  end
  object Label2: TLabel
    Left = 8
    Top = 6
    Width = 558
    Height = 23
    Caption = 'ASCII Shape implementation by Paul TOTH <contact@execute.fr>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Memo1: TMemo
    Left = 8
    Top = 60
    Width = 393
    Height = 368
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    OnChange = Memo1Change
  end
  object rgAntialiaze: TRadioGroup
    Left = 407
    Top = 108
    Width = 106
    Height = 118
    Caption = 'Antialiasing'
    ItemIndex = 3
    Items.Strings = (
      'None'
      'x2'
      'x4'
      'x8')
    TabOrder = 1
    OnClick = Memo1Change
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 35
    Width = 393
    Height = 21
    ItemIndex = 4
    TabOrder = 2
    Text = 'Delete'
    OnClick = ComboBox1Click
    Items.Strings = (
      'Lines'
      'Polygones'
      'Ellipse'
      'Bug'
      'Delete'
      'Lock'
      
        'Pokeball from http://memnarch.bplaced.net/blog/2015/05/asciiimag' +
        'e-for-delphi/')
  end
end
