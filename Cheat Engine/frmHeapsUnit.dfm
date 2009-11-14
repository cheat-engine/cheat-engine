object frmHeaps: TfrmHeaps
  Left = 408
  Top = 186
  Width = 220
  Height = 316
  BorderIcons = [biSystemMenu]
  Caption = 'Current Heaplist'
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
  object Panel1: TPanel
    Left = 0
    Top = 221
    Width = 202
    Height = 50
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      202
      50)
    object Button1: TButton
      Left = 54
      Top = 10
      Width = 92
      Height = 31
      Anchors = [akTop]
      Caption = 'Close'
      ModalResult = 1
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 202
    Height = 221
    Align = alClient
    Columns = <
      item
        Caption = 'Address'
        Width = 128
      end
      item
        AutoSize = True
        Caption = 'Size'
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = ListView1DblClick
  end
end
