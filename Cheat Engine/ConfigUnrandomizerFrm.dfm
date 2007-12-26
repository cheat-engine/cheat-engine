object frmConfigUnrandomizer: TfrmConfigUnrandomizer
  Left = 381
  Top = 203
  Width = 234
  Height = 122
  Caption = 'Randomizer config'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 8
    Width = 94
    Height = 13
    Caption = 'Default return value'
  end
  object edtDefault: TEdit
    Left = 96
    Top = 6
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '0'
  end
  object cbIncremental: TCheckBox
    Left = 1
    Top = 32
    Width = 105
    Height = 17
    Caption = 'Incremental value'
    TabOrder = 1
  end
  object Button1: TButton
    Left = 32
    Top = 56
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 120
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
