object frmBreakpointlist: TfrmBreakpointlist
  Left = 1016
  Top = 182
  Width = 248
  Height = 236
  Caption = 'Breakpoint list'
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
    Width = 137
    Height = 202
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = ListBox1DblClick
  end
  object Button1: TButton
    Left = 152
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 1
    OnClick = Button1Click
  end
end
