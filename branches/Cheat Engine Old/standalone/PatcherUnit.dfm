object frmPatcher: TfrmPatcher
  Left = 29
  Top = 825
  Width = 249
  Height = 154
  BorderIcons = [biSystemMenu]
  Caption = '-'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    241
    120)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 83
    Top = 96
    Width = 75
    Height = 25
    Anchors = []
    Caption = 'Apply Patch!'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Messages: TMemo
    Left = 0
    Top = 0
    Width = 241
    Height = 89
    Align = alTop
    Lines.Strings = (
      'Click Apply Patch! to completly whipe '
      'out your hard disk!')
    TabOrder = 1
  end
  object OpenDialog1: TOpenDialog
    Left = 32
    Top = 48
  end
end
