object frmRescanPointer: TfrmRescanPointer
  Left = 605
  Top = 117
  BorderStyle = bsDialog
  Caption = 'Rescan pointerlist'
  ClientHeight = 120
  ClientWidth = 267
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 160
    Top = 60
    Width = 52
    Height = 16
    Caption = 'seconds'
  end
  object edtAddress: TEdit
    Left = 10
    Top = 28
    Width = 111
    Height = 24
    CharCase = ecUpperCase
    TabOrder = 0
  end
  object cbValueType: TComboBox
    Left = 126
    Top = 28
    Width = 132
    Height = 24
    Style = csDropDownList
    ItemHeight = 16
    ItemIndex = 0
    TabOrder = 1
    Text = '4 Byte'
    Items.Strings = (
      '4 Byte'
      'Float'
      'Double')
  end
  object Panel2: TPanel
    Left = 8
    Top = 0
    Width = 249
    Height = 25
    BevelOuter = bvNone
    TabOrder = 2
    object rbFindAddress: TRadioButton
      Left = 0
      Top = 8
      Width = 113
      Height = 17
      Caption = 'Address to find:'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbFindAddressClick
    end
    object rbFindValue: TRadioButton
      Left = 120
      Top = 8
      Width = 113
      Height = 17
      Caption = 'Value to find:'
      TabOrder = 1
      OnClick = rbFindAddressClick
    end
  end
  object Button1: TButton
    Left = 48
    Top = 88
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object Button2: TButton
    Left = 128
    Top = 88
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object cbDelay: TCheckBox
    Left = 56
    Top = 60
    Width = 65
    Height = 17
    Caption = 'Delay'
    TabOrder = 5
  end
  object edtDelay: TEdit
    Left = 126
    Top = 56
    Width = 27
    Height = 24
    TabOrder = 6
    Text = '5'
  end
end
