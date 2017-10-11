object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Desktop Duplication API Demo 1'
  ClientHeight = 358
  ClientWidth = 687
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    687
    358)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 481
    Height = 345
    Anchors = [akLeft, akTop, akRight, akBottom]
    Stretch = True
  end
  object Memo1: TMemo
    Left = 495
    Top = 8
    Width = 185
    Height = 345
    Anchors = [akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 528
    Top = 40
  end
end
