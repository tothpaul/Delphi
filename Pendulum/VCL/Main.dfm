object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Quinze Pendules'
  ClientHeight = 535
  ClientWidth = 522
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnPaint = FormPaint
  DesignSize = (
    522
    535)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 478
    Top = 482
    Width = 11
    Height = 25
    Alignment = taCenter
    Anchors = [akRight, akBottom]
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitTop = 424
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 57
    Height = 49
    Caption = 'Start'
    TabOrder = 0
    OnClick = FormCreate
  end
  object Timer1: TTimer
    Interval = 40
    OnTimer = Timer1Timer
    Left = 232
    Top = 192
  end
end
