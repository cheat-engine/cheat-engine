object frmCreatedProcessList: TfrmCreatedProcessList
  Left = 637
  Top = 97
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Created processes'
  ClientHeight = 211
  ClientWidth = 190
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 313
    Height = 169
    Style = lbOwnerDrawFixed
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = Button1Click
    OnDrawItem = ListBox1DrawItem
  end
  object Button1: TButton
    Left = 56
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Select'
    TabOrder = 1
    OnClick = Button1Click
  end
end
