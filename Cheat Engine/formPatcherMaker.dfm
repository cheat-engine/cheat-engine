object frmPatcherMaker: TfrmPatcherMaker
  Left = 728
  Top = 104
  Width = 284
  Height = 222
  Caption = 'Patcher maker'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 339
    Height = 16
    Caption = 'Select the code entries you want to remove from the file(s)'
  end
  object PatchCodeList: TListBox
    Left = 1
    Top = 20
    Width = 336
    Height = 168
    ItemHeight = 16
    MultiSelect = True
    TabOrder = 0
  end
  object Button1: TButton
    Left = 69
    Top = 197
    Width = 92
    Height = 31
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 177
    Top = 197
    Width = 93
    Height = 31
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
