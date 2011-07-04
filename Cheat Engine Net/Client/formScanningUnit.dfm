object FormScanning: TFormScanning
  Left = 795
  Top = 93
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'FormScanning'
  ClientHeight = 75
  ClientWidth = 133
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = -8
    Top = 8
    Width = 145
    Height = 24
    AutoSize = False
    Caption = '01000100010000100011010001000101010101100100010101010010'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -25
    Font.Name = 'Courier'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 0
    Top = 32
    Width = 82
    Height = 13
    Caption = 'Estimate time left:'
  end
  object Label3: TLabel
    Left = 88
    Top = 32
    Width = 42
    Height = 13
    Caption = '00:00:00'
  end
  object btnCancel: TButton
    Left = 12
    Top = 48
    Width = 105
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = btnCancelClick
  end
  object Button1: TButton
    Left = 336
    Top = 164
    Width = 0
    Height = 0
    Caption = 'Button1'
    Default = True
    TabOrder = 1
  end
end
