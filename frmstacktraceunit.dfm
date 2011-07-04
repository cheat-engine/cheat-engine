object frmStacktrace: TfrmStacktrace
  Left = 689
  Top = 638
  Width = 683
  Height = 274
  BorderStyle = bsSizeToolWin
  Caption = 'Stacktrace'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 665
    Height = 229
    Align = alClient
    Columns = <
      item
        Caption = 'PC'
        Width = 123
      end
      item
        Caption = 'Stack'
        Width = 123
      end
      item
        Caption = 'Frame'
        Width = 123
      end
      item
        Caption = 'Return'
        Width = 123
      end
      item
        AutoSize = True
        Caption = 'Parameters'
      end>
    ReadOnly = True
    PopupMenu = PopupMenu1
    TabOrder = 0
    ViewStyle = vsReport
  end
  object PopupMenu1: TPopupMenu
    Left = 88
    Top = 56
    object Refresh1: TMenuItem
      Caption = 'Refresh'
      OnClick = Refresh1Click
    end
  end
end
