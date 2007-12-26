object ValueChangeForm: TValueChangeForm
  Left = 1094
  Top = 364
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Change Offset: ########'
  ClientHeight = 83
  ClientWidth = 214
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object VarType: TComboBox
    Left = 16
    Top = 25
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 0
    Text = '1 Byte'
    OnChange = VarTypeChange
    Items.Strings = (
      '1 Byte'
      '2 Bytes'
      '4 Bytes'
      '8 Bytes'
      'Float'
      'Double'
      'Text'
      'Array of Bytes')
  end
  object Button1: TButton
    Left = 34
    Top = 56
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 117
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
  object ValueText: TEdit
    Left = 17
    Top = 1
    Width = 192
    Height = 21
    TabOrder = 3
    Text = 'ValueText'
  end
  object cbunicode: TCheckBox
    Left = 144
    Top = 27
    Width = 65
    Height = 17
    Caption = 'Unicode'
    TabOrder = 4
    Visible = False
  end
end
