object frmPatcherMaker: TfrmPatcherMaker
  Left = 599
  Top = 160
  Width = 454
  Height = 301
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 436
    Height = 208
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 0
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 436
      Height = 16
      Align = alTop
      Caption = 'Select the code entries you want to remove from the file(s)'
    end
    object PatchCodeList: TListBox
      Left = 0
      Top = 16
      Width = 436
      Height = 192
      Align = alClient
      ItemHeight = 16
      MultiSelect = True
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 208
    Width = 436
    Height = 48
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = Panel2Resize
    object btnOk: TButton
      Left = 117
      Top = 10
      Width = 92
      Height = 31
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      Left = 225
      Top = 10
      Width = 93
      Height = 31
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 24
    Top = 136
  end
end
