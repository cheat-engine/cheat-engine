object frmLoadMemory: TfrmLoadMemory
  Left = 1033
  Top = 545
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Load Memory Region'
  ClientHeight = 153
  ClientWidth = 211
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 72
    Width = 41
    Height = 13
    Caption = 'Address:'
  end
  object Button1: TButton
    Left = 24
    Top = 120
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 112
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object editAddress: TEdit
    Left = 0
    Top = 88
    Width = 161
    Height = 21
    TabOrder = 2
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 209
    Height = 65
    ItemHeight = 13
    TabOrder = 3
    OnClick = ListBox1Click
  end
  object Button3: TButton
    Left = 167
    Top = 89
    Width = 41
    Height = 21
    Caption = 'Edit'
    TabOrder = 4
    OnClick = Button3Click
  end
end
