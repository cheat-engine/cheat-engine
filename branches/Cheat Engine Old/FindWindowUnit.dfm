object FindWindow: TFindWindow
  Left = 583
  Top = 483
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Find'
  ClientHeight = 186
  ClientWidth = 286
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object ProgressBar: TProgressBar
    Left = 0
    Top = 116
    Width = 286
    Height = 20
    Align = alBottom
    Step = 1
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 286
    Height = 116
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object labelType: TLabel
      Left = 20
      Top = 0
      Width = 35
      Height = 16
      Caption = 'Type:'
    end
    object Label2: TLabel
      Left = 150
      Top = 25
      Width = 31
      Height = 16
      Caption = 'From'
    end
    object Label3: TLabel
      Left = 167
      Top = 54
      Width = 17
      Height = 16
      Caption = 'To'
    end
    object labelArray: TLabel
      Left = 187
      Top = 0
      Width = 78
      Height = 16
      Caption = 'Array to scan'
    end
    object editStart: TEdit
      Left = 187
      Top = 20
      Width = 90
      Height = 24
      MaxLength = 8
      TabOrder = 0
      Text = '00400000'
    end
    object EditStop: TEdit
      Left = 187
      Top = 49
      Width = 90
      Height = 24
      MaxLength = 8
      TabOrder = 1
      Text = '7FFFFFFF'
    end
    object rbText: TRadioButton
      Left = 0
      Top = 22
      Width = 60
      Height = 21
      Caption = 'Text'
      Checked = True
      TabOrder = 2
      TabStop = True
    end
    object rbArByte: TRadioButton
      Left = 0
      Top = 42
      Width = 110
      Height = 21
      Caption = '(Array of) byte'
      TabOrder = 3
    end
    object cbUnicode: TCheckBox
      Left = 69
      Top = 20
      Width = 80
      Height = 21
      Caption = 'Unicode'
      TabOrder = 4
    end
    object Scanvalue: TEdit
      Left = 10
      Top = 89
      Width = 267
      Height = 24
      TabOrder = 5
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 136
    Width = 286
    Height = 50
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnOK: TButton
      Left = 45
      Top = 11
      Width = 93
      Height = 30
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 145
      Top = 11
      Width = 92
      Height = 30
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 5
    OnTimer = Timer1Timer
    Left = 88
    Top = 40
  end
end
