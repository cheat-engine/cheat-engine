object formPointerOrPointee: TformPointerOrPointee
  Left = 192
  Top = 678
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Cheat Engine Pointer'
  ClientHeight = 101
  ClientWidth = 335
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 77
    Height = 13
    Caption = 'This is a pointer.'
    WordWrap = True
  end
  object Button1: TButton
    Left = 8
    Top = 32
    Width = 321
    Height = 25
    Caption = 'Find what writes to this pointer'
    ModalResult = 6
    TabOrder = 0
  end
  object Button2: TButton
    Left = 8
    Top = 64
    Width = 321
    Height = 25
    Caption = 'Find what writes to the address pointed at by this pointer'
    ModalResult = 7
    TabOrder = 1
  end
end
