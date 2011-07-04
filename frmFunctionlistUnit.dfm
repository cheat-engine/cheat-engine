object frmFunctionList: TfrmFunctionList
  Left = 335
  Top = 193
  Width = 313
  Height = 228
  BorderIcons = [biSystemMenu]
  Caption = 'Functionlist'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 305
    Height = 13
    Align = alTop
    Caption = 'Select a function from the list'
  end
  object ListBox1: TListBox
    Left = 0
    Top = 13
    Width = 305
    Height = 150
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = ListBox1DblClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 163
    Width = 305
    Height = 31
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      305
      31)
    object Button1: TButton
      Left = 116
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akTop]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
end
