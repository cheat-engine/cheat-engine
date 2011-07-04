object ChangeOffset: TChangeOffset
  Left = 933
  Top = 423
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Recalculate address'
  ClientHeight = 113
  ClientWidth = 200
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
  object TabControl1: TTabControl
    Left = 0
    Top = 0
    Width = 201
    Height = 113
    TabOrder = 0
    Tabs.Strings = (
      'Change by offset'
      'Change to address')
    TabIndex = 0
    OnChange = TabControl1Change
    OnChanging = TabControl1Changing
    object Change: TButton
      Left = 24
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Change'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = ChangeClick
    end
    object Button2: TButton
      Left = 104
      Top = 80
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 56
      Width = 81
      Height = 17
      Caption = 'Hexadecimal'
      TabOrder = 2
      OnClick = CheckBox1Click
    end
    object Edit1: TEdit
      Left = 8
      Top = 32
      Width = 185
      Height = 21
      TabOrder = 3
      Text = 'Edit1'
    end
  end
end
