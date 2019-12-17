object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Macro Recorder'
  ClientHeight = 409
  ClientWidth = 434
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
  object btRecord: TButton
    Left = 16
    Top = 24
    Width = 75
    Height = 65
    Caption = 'Record'
    TabOrder = 0
    OnClick = btRecordClick
  end
  object btPlay: TButton
    Left = 97
    Top = 24
    Width = 75
    Height = 65
    Caption = 'Play'
    Enabled = False
    TabOrder = 1
    OnClick = btPlayClick
  end
  object btSave: TButton
    Left = 178
    Top = 24
    Width = 75
    Height = 65
    Caption = 'Save'
    Enabled = False
    TabOrder = 2
    OnClick = btSaveClick
  end
  object btLoad: TButton
    Left = 259
    Top = 24
    Width = 75
    Height = 65
    Caption = 'Load'
    TabOrder = 3
    OnClick = btLoadClick
  end
  object btShow: TButton
    Left = 340
    Top = 24
    Width = 75
    Height = 65
    Caption = 'Show'
    TabOrder = 4
    OnClick = btShowClick
  end
  object Memo1: TMemo
    Left = 161
    Top = 104
    Width = 254
    Height = 289
    Lines.Strings = (
      'Sample demo for a MacroRecord'
      ''
      '- the compenent do not need Admin privilege nor '
      'signed application'
      ''
      '- it can'#39't capture events from other application, it '
      'have to be embedded'
      ''
      '- SaveAsCode is used with PascalScript in an other '
      'project')
    TabOrder = 5
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 104
    Width = 121
    Height = 17
    Caption = 'Capture MouseMove'
    Checked = True
    State = cbChecked
    TabOrder = 6
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 16
    Top = 127
    Width = 121
    Height = 17
    Caption = 'Capture MouseClick'
    Checked = True
    State = cbChecked
    TabOrder = 7
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 16
    Top = 150
    Width = 121
    Height = 17
    Caption = 'Capture Keystroke'
    Checked = True
    State = cbChecked
    TabOrder = 8
    OnClick = CheckBox3Click
  end
  object CheckBox4: TCheckBox
    Left = 34
    Top = 173
    Width = 103
    Height = 17
    Caption = 'Use UnicodeChars'
    Checked = True
    State = cbChecked
    TabOrder = 9
    OnClick = CheckBox4Click
  end
  object CheckBox5: TCheckBox
    Left = 34
    Top = 196
    Width = 103
    Height = 17
    Caption = 'Regroup chars'
    Checked = True
    State = cbChecked
    TabOrder = 10
    OnClick = CheckBox5Click
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'mac'
    Filter = 'Macro file|*.mac|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 80
    Top = 256
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'mac'
    Filter = 'Macro file|*.mac|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 64
    Top = 320
  end
end
