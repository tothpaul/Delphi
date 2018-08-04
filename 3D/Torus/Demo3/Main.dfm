object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'RayTracing (c)2017 by Execute SARL'
  ClientHeight = 200
  ClientWidth = 320
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
    Left = 64
    Top = 24
  end
end
