object frmModuleSafety: TfrmModuleSafety
  Left = 192
  Top = 114
  BorderStyle = bsSingle
  Caption = 'Module safety'
  ClientHeight = 259
  ClientWidth = 298
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
  object ListBox1: TListBox
    Left = 8
    Top = 72
    Width = 281
    Height = 153
    ItemHeight = 13
    MultiSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 8
    Top = 48
    Width = 201
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 216
    Top = 48
    Width = 75
    Height = 21
    Caption = 'Add'
    TabOrder = 2
    OnClick = Button1Click
  end
  object rbAllowList: TRadioButton
    Left = 8
    Top = 8
    Width = 245
    Height = 17
    Caption = 'Prevent all modules from getting loaded except:'
    TabOrder = 3
    OnClick = rbAllowListClick
  end
  object rbDenyList: TRadioButton
    Left = 8
    Top = 24
    Width = 203
    Height = 17
    Caption = 'Allow all modules to be loaded except:'
    Checked = True
    TabOrder = 4
    TabStop = True
    OnClick = rbAllowListClick
  end
  object cbGlobalDeny: TCheckBox
    Left = 208
    Top = 24
    Width = 49
    Height = 17
    Caption = 'Global'
    TabOrder = 5
  end
  object Button2: TButton
    Left = 112
    Top = 232
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 6
  end
  object PopupMenu1: TPopupMenu
    Left = 72
    Top = 96
    object Remove1: TMenuItem
      Caption = 'Remove'
      Default = True
      OnClick = Remove1Click
    end
  end
end
