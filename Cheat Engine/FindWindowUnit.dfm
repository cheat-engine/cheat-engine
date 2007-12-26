object FindWindow: TFindWindow
  Left = 106
  Top = 645
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Find'
  ClientHeight = 154
  ClientWidth = 232
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object labelType: TLabel
    Left = 16
    Top = 0
    Width = 27
    Height = 13
    Caption = 'Type:'
  end
  object Label2: TLabel
    Left = 128
    Top = 20
    Width = 23
    Height = 13
    Caption = 'From'
  end
  object Label3: TLabel
    Left = 136
    Top = 44
    Width = 13
    Height = 13
    Caption = 'To'
  end
  object labelArray: TLabel
    Left = 152
    Top = 0
    Width = 62
    Height = 13
    Caption = 'Array to scan'
  end
  object btnOK: TButton
    Left = 40
    Top = 120
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 5
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 120
    Top = 120
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object Scanvalue: TEdit
    Left = 8
    Top = 72
    Width = 217
    Height = 21
    TabOrder = 0
  end
  object editStart: TEdit
    Left = 152
    Top = 16
    Width = 73
    Height = 21
    MaxLength = 8
    TabOrder = 3
    Text = 'editStart'
  end
  object EditStop: TEdit
    Left = 152
    Top = 40
    Width = 73
    Height = 21
    MaxLength = 8
    TabOrder = 4
    Text = '7FFFFFFF'
  end
  object rbText: TRadioButton
    Left = 0
    Top = 18
    Width = 49
    Height = 17
    Caption = 'Text'
    Checked = True
    TabOrder = 1
    TabStop = True
  end
  object rbArByte: TRadioButton
    Left = 0
    Top = 34
    Width = 89
    Height = 17
    Caption = '(Array of) byte'
    TabOrder = 2
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 96
    Width = 217
    Height = 16
    Step = 1
    TabOrder = 7
  end
  object cbUnicode: TCheckBox
    Left = 56
    Top = 16
    Width = 65
    Height = 17
    Caption = 'Unicode'
    TabOrder = 8
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 5
    OnTimer = Timer1Timer
    Left = 88
    Top = 40
  end
end
