object frmThreadlist: TfrmThreadlist
  Left = 795
  Top = 582
  Width = 313
  Height = 187
  BorderStyle = bsSizeToolWin
  Caption = 'Threadlist'
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
  object threadlistview: TListView
    Left = 0
    Top = 0
    Width = 305
    Height = 160
    Align = alClient
    Columns = <
      item
        Caption = 'ThreadID'
        Width = 100
      end
      item
        Caption = 'Start Address'
        Width = 100
      end
      item
        Caption = 'Local Base'
        Width = 100
      end>
    ColumnClick = False
    HideSelection = False
    RowSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 0
    ViewStyle = vsReport
  end
  object PopupMenu1: TPopupMenu
    Left = 80
    Top = 40
    object Break1: TMenuItem
      Caption = 'Break'
      OnClick = Break1Click
    end
  end
end
