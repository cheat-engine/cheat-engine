object frmBreakpointlist: TfrmBreakpointlist
  Left = 511
  Top = 252
  Width = 311
  Height = 335
  Caption = 'Breakpoint list'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 188
    Height = 290
    Align = alClient
    ItemHeight = 16
    TabOrder = 0
    OnDblClick = ListBox1DblClick
  end
  object Panel1: TPanel
    Left = 188
    Top = 0
    Width = 105
    Height = 290
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 6
      Top = 10
      Width = 92
      Height = 31
      Caption = 'Delete'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
end
