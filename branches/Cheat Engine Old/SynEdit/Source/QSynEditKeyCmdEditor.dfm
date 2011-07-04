object SynEditKeystrokeEditorForm: TSynEditKeystrokeEditorForm
  Left = 405
  Top = 306
  BorderStyle = fbsDialog
  Caption = 'Edit Keystroke'
  ClientHeight = 129
  ClientWidth = 269
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlAlign: TPanel
    Left = 3
    Top = 5
    Width = 262
    Height = 120
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object Label1: TLabel
      Left = 9
      Top = 14
      Width = 50
      Height = 13
      Caption = 'Command:'
    end
    object Label2: TLabel
      Left = 9
      Top = 41
      Width = 50
      Height = 13
      Caption = 'Keystroke:'
    end
    object Label4: TLabel
      Left = 9
      Top = 65
      Width = 50
      Height = 13
      Caption = 'Keystroke:'
    end
    object bntClearKey: TButton
      Left = 9
      Top = 86
      Width = 75
      Height = 25
      Caption = 'Clear Key'
      TabOrder = 3
      OnClick = bntClearKeyClick
    end
    object btnOK: TButton
      Left = 93
      Top = 86
      Width = 75
      Height = 25
      Caption = 'OK'
      TabOrder = 1
      OnClick = btnOKClick
    end
    object cmbCommand: TComboBox
      Left = 65
      Top = 10
      Width = 186
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      OnExit = cmbCommandExit
      OnKeyPress = cmbCommandKeyPress
    end
    object btnCancel: TButton
      Left = 177
      Top = 86
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
  end
end
