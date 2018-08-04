object Form5: TForm5
  Left = 0
  Top = 0
  Caption = 'Raymarching Demo (c)2017 Execute SARL'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inline GLPanel1: TGLPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 299
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 635
    ExplicitHeight = 299
  end
  object Timer1: TTimer
    Interval = 40
    OnTimer = Timer1Timer
    Left = 64
    Top = 24
  end
end
