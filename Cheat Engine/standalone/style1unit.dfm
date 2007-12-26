object Style1: TStyle1
  Left = 705
  Top = 295
  BorderStyle = bsSingle
  Caption = 'Style1'
  ClientHeight = 243
  ClientWidth = 230
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
  object Label1: TLabel
    Left = 0
    Top = 152
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Image1: TImage
    Left = 176
    Top = 190
    Width = 50
    Height = 50
    Stretch = True
  end
  object CheckListBox1: TCheckListBox
    Left = 0
    Top = 0
    Width = 225
    Height = 145
    OnClickCheck = CheckListBox1ClickCheck
    ItemHeight = 13
    TabOrder = 0
  end
end
