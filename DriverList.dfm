object frmDriverlist: TfrmDriverlist
  Left = 508
  Top = 368
  BorderStyle = bsSingle
  Caption = 'Driver list'
  ClientHeight = 229
  ClientWidth = 326
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
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 326
    Height = 193
    Align = alTop
    ItemHeight = 13
    PopupMenu = PopupMenu1
    TabOrder = 0
  end
  object Button1: TButton
    Left = 128
    Top = 200
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
    OnClick = Button1Click
  end
  object FindDialog1: TFindDialog
    Options = [frDown, frFindNext, frDisableMatchCase, frDisableUpDown, frDisableWholeWord]
    OnFind = FindDialog1Find
    Left = 8
    Top = 8
  end
  object PopupMenu1: TPopupMenu
    Left = 8
    Top = 40
    object Find1: TMenuItem
      Caption = 'Find'
      ShortCut = 16454
      OnClick = Find1Click
    end
  end
end
