object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'WebSocket with TidHTTP Server by Execute SARL'
  ClientHeight = 348
  ClientWidth = 578
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    578
    348)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 326
    Height = 13
    Caption = 
      'Warning ! the code do not expect more than ONE active browser  :' +
      ')'
  end
  object Memo1: TMemo
    Left = 16
    Top = 88
    Width = 541
    Height = 254
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 73
    Top = 40
    Width = 403
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 482
    Top = 38
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Send'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 15
    Top = 40
    Width = 52
    Height = 23
    Caption = 'Go'
    TabOrder = 3
    OnClick = Button2Click
  end
  object IdHTTPServer1: TIdHTTPServer
    Active = True
    Bindings = <>
    OnCommandGet = IdHTTPServer1CommandGet
    Left = 200
    Top = 152
  end
end
