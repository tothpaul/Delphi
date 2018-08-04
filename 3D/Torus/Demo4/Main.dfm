object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'RayMarching (c)2017 Execute SARL'
  ClientHeight = 221
  ClientWidth = 431
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Timer1: TTimer
    Interval = 40
    OnTimer = Timer1Timer
    Left = 72
    Top = 24
  end
end
