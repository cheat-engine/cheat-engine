object frmStacktrace: TfrmStacktrace
  Left = 213
  Top = 625
  Width = 506
  Height = 274
  BorderStyle = bsSizeToolWin
  Caption = 'Stacktrace'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 498
    Height = 240
    Align = alClient
    Columns = <
      item
        Caption = 'PC'
        Width = 100
      end
      item
        Caption = 'Stack'
        Width = 100
      end
      item
        Caption = 'Frame'
        Width = 100
      end
      item
        Caption = 'Return'
        Width = 100
      end
      item
        AutoSize = True
        Caption = 'Parameters'
      end>
    ReadOnly = True
    TabOrder = 0
    ViewStyle = vsReport
  end
end
