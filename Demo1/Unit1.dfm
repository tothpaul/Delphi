object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GraphicPanel1: TGraphicPanel
    Left = 40
    Top = 48
    Width = 233
    Height = 137
    Caption = 'GraphicPanel1'
  end
  object GraphicPanel2: TGraphicPanel
    Left = 288
    Top = 48
    Width = 281
    Height = 137
    Caption = 'GraphicPanel2'
    object GraphicPanel3: TGraphicPanel
      Left = 312
      Top = 128
      Width = 193
      Height = 97
      Caption = 'GraphicPanel3'
      GraphicParent = GraphicPanel2
    end
  end
end
