object ValueChangeForm: TValueChangeForm
  Left = 1094
  Top = 364
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Change Offset: ########'
  ClientHeight = 102
  ClientWidth = 263
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object VarType: TComboBox
    Left = 20
    Top = 31
    Width = 149
    Height = 24
    Style = csDropDownList
    ItemHeight = 16
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
    Left = 42
    Top = 69
    Width = 92
    Height = 31
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 144
    Top = 69
    Width = 92
    Height = 31
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
  object ValueText: TEdit
    Left = 21
    Top = 1
    Width = 236
    Height = 21
    TabOrder = 3
    Text = 'ValueText'
  end
  object cbunicode: TCheckBox
    Left = 177
    Top = 33
    Width = 80
    Height = 21
    Caption = 'Unicode'
    TabOrder = 4
    Visible = False
  end
end
