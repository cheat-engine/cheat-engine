object frmPatcherMaker: TfrmPatcherMaker
  Left = 728
  Top = 104
  Width = 284
  Height = 222
  Caption = 'Patcher maker'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 273
    Height = 13
    Caption = 'Select the code entries you want to remove from the file(s)'
  end
  object PatchCodeList: TListBox
    Left = 1
    Top = 16
    Width = 273
    Height = 137
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 0
  end
  object Button1: TButton
    Left = 56
    Top = 160
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 144
    Top = 160
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object OpenDialog1: TOpenDialog
    Left = 24
    Top = 136
  end
end
