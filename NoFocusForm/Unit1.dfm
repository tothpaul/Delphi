object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 277
  ClientWidth = 305
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 178
    Height = 13
    Caption = 'Click on or tab into Textbox for demo'
  end
  object Label2: TLabel
    Left = 6
    Top = 242
    Width = 117
    Height = 13
    Caption = '2010 Ricket Ward (.Net)'
  end
  object Label3: TLabel
    Left = 6
    Top = 223
    Width = 146
    Height = 13
    Caption = '2017 Paul TOTH (Delphi Berlin)'
  end
  object Edit1: TEdit
    Left = 49
    Top = 72
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object CheckBox1: TCheckBox
    Left = 176
    Top = 74
    Width = 97
    Height = 17
    Caption = 'Numbers Only'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object Button1: TButton
    Left = 222
    Top = 235
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = Button1Click
  end
end
