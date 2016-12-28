object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'SelfDebug'
  ClientHeight = 531
  ClientWidth = 962
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
  object Splitter1: TSplitter
    Left = 333
    Top = 0
    Height = 531
    Align = alRight
    ExplicitLeft = 488
    ExplicitTop = 232
    ExplicitHeight = 100
  end
  object lbDUMP: TListBox
    Left = 336
    Top = 0
    Width = 626
    Height = 531
    Style = lbVirtualOwnerDraw
    Align = alRight
    DoubleBuffered = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 0
    OnDrawItem = lbDUMPDrawItem
  end
  object lbTREE: TListBox
    Left = 0
    Top = 0
    Width = 333
    Height = 531
    Style = lbVirtualOwnerDraw
    Align = alClient
    TabOrder = 1
    OnClick = lbTREEClick
    OnDblClick = lbTREEDblClick
    OnDrawItem = lbTREEDrawItem
    OnKeyDown = lbTREEKeyDown
  end
end
