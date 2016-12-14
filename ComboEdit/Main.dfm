object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ComboEdit (c) Execute SARL'
  ClientHeight = 253
  ClientWidth = 504
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 8
    Width = 83
    Height = 13
    Caption = 'Combo Edit demo'
  end
  object Edit1: TEdit
    Left = 24
    Top = 32
    Width = 193
    Height = 25
    TabOrder = 0
    TextHint = 'Type something'
  end
  object CheckBox1: TCheckBox
    Left = 240
    Top = 34
    Width = 97
    Height = 17
    Hint = 'Use ContainsText instead of AnsiStartsText'
    Caption = 'Contains'
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 240
    Top = 57
    Width = 225
    Height = 168
    Hint = 'Value list'
    Lines.Strings = (
      'Paul'
      'Pascal'
      'Mathias'
      'Marc'
      'S'#233'verine'
      'Sophie'
      'Fran'#231'ois'
      'Fran'#231'oise'
      'Jules'
      'Julie'
      'Ramon'
      'Radagast'
      'Adeline'
      'Adolphe'
      'Anne'
      'Alexandre')
    TabOrder = 2
  end
  object SpinEdit1: TSpinEdit
    Left = 416
    Top = 29
    Width = 49
    Height = 22
    Hint = 'Delay before drop down'
    MaxValue = 0
    MinValue = 0
    TabOrder = 3
    Value = 250
    OnChange = SpinEdit1Change
  end
end
