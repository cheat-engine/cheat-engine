object frmAddToCodeList: TfrmAddToCodeList
  Left = 221
  Top = 752
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Region to add'
  ClientHeight = 85
  ClientWidth = 169
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 30
    Top = 8
    Width = 23
    Height = 13
    Caption = 'From'
  end
  object Label2: TLabel
    Left = 40
    Top = 32
    Width = 13
    Height = 13
    Caption = 'To'
  end
  object Edit1: TEdit
    Left = 56
    Top = 4
    Width = 81
    Height = 21
    MaxLength = 8
    TabOrder = 0
    Text = 'DDDDDDDD'
  end
  object Button1: TButton
    Left = 8
    Top = 56
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 88
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object Edit2: TEdit
    Left = 56
    Top = 28
    Width = 81
    Height = 21
    TabOrder = 1
    Text = 'FFFFFFFF'
  end
end
