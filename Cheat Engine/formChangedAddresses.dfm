object frmChangedAddresses: TfrmChangedAddresses
  Left = 1094
  Top = 593
  BorderStyle = bsDialog
  Caption = 'Changed Addresses'
  ClientHeight = 239
  ClientWidth = 172
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
  object lblInfo: TLabel
    Left = 0
    Top = 0
    Width = 172
    Height = 26
    Align = alTop
    Caption = 
      'The following addresses have been changed by the code you select' +
      'ed'
    WordWrap = True
  end
  object Changedlist: TListBox
    Left = 0
    Top = 26
    Width = 172
    Height = 175
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = ChangedlistDblClick
  end
  object OKButton: TButton
    Left = 48
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = OKButtonClick
  end
end
