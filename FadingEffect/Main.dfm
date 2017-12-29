object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Transition effet for Delphi Tokyo by Execute SARL'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 258
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 1
    Visible = False
    object Shape1: TShape
      Left = 88
      Top = 56
      Width = 65
      Height = 65
    end
    object Shape2: TShape
      Left = 232
      Top = 112
      Width = 65
      Height = 65
      Shape = stCircle
    end
    object Shape3: TShape
      Left = 416
      Top = 64
      Width = 137
      Height = 113
      Shape = stRoundRect
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 258
    Width = 635
    Height = 41
    Align = alBottom
    Caption = 'Panel3'
    TabOrder = 2
    object Button1: TButton
      Left = 280
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 258
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 0
    ExplicitTop = 2
    object Shape4: TShape
      Left = 136
      Top = 32
      Width = 65
      Height = 65
      Brush.Color = clYellow
    end
    object Shape5: TShape
      Left = 464
      Top = 48
      Width = 65
      Height = 65
      Brush.Color = clRed
      Shape = stCircle
    end
    object Shape6: TShape
      Left = 146
      Top = 127
      Width = 137
      Height = 113
      Brush.Color = clTeal
      Shape = stRoundRect
    end
    object Edit1: TEdit
      Left = 234
      Top = 56
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'Edit1'
    end
    object DatePicker1: TDatePicker
      Left = 303
      Top = 160
      Date = 43098.000000000000000000
      DateFormat = 'dd/MM/yyyy'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      TabOrder = 1
    end
    object TimePicker1: TTimePicker
      Left = 459
      Top = 160
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Segoe UI'
      Font.Style = []
      TabOrder = 2
      Time = 43098.427778402780000000
      TimeFormat = 'hh:mm'
    end
  end
end
