object frmFillMemory: TfrmFillMemory
  Left = 599
  Top = 428
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Fill memory'
  ClientHeight = 111
  ClientWidth = 168
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 23
    Height = 13
    Caption = 'From'
  end
  object Label2: TLabel
    Left = 26
    Top = 32
    Width = 13
    Height = 13
    Caption = 'To'
  end
  object Label3: TLabel
    Left = 5
    Top = 56
    Width = 34
    Height = 13
    Caption = 'Fill with'
  end
  object Edit1: TEdit
    Left = 48
    Top = 4
    Width = 105
    Height = 21
    MaxLength = 8
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 80
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 88
    Top = 80
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object Edit2: TEdit
    Left = 48
    Top = 28
    Width = 105
    Height = 21
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 48
    Top = 52
    Width = 105
    Height = 21
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Text = '0'
  end
end
