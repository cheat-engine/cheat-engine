object frmReferencedStrings: TfrmReferencedStrings
  Left = 639
  Top = 248
  Width = 616
  Height = 308
  Caption = 'Referenced Strings'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 474
    Top = 0
    Height = 238
    Align = alRight
  end
  object lvStringlist: TListView
    Left = 0
    Top = 0
    Width = 474
    Height = 238
    Align = alClient
    Columns = <
      item
        Caption = 'Address'
        Width = 100
      end
      item
        Caption = 'Refcount'
        Width = 75
      end
      item
        AutoSize = True
        Caption = 'String'
      end>
    HideSelection = False
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnData = lvStringlistData
    OnDblClick = lvStringlistDblClick
    OnSelectItem = lvStringlistSelectItem
  end
  object lbReflist: TListBox
    Left = 477
    Top = 0
    Width = 121
    Height = 238
    Align = alRight
    ItemHeight = 16
    TabOrder = 1
    OnDblClick = lbReflistDblClick
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 24
    object Search1: TMenuItem
      Caption = 'Search'
      object Find1: TMenuItem
        Caption = 'Find'
        ShortCut = 16454
        OnClick = Find1Click
      end
      object FindNext1: TMenuItem
        Caption = 'Find Next'
        ShortCut = 114
        OnClick = FindNext1Click
      end
    end
  end
  object FindDialog1: TFindDialog
    Options = [frDown, frHideMatchCase, frHideWholeWord, frHideUpDown]
    OnFind = FindDialog1Find
    Left = 16
    Top = 56
  end
end
