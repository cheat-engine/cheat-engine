object frmDisassemblyscan: TfrmDisassemblyscan
  Left = 571
  Top = 479
  BorderStyle = bsDialog
  Caption = 'Assemblyscan'
  ClientHeight = 150
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 137
    Width = 450
    Height = 13
    Align = alBottom
    Alignment = taCenter
    Caption = '                      '
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 377
    Height = 137
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = ListBox1DblClick
  end
  object Button1: TButton
    Left = 384
    Top = 112
    Width = 59
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 344
    Top = 24
  end
end
