object FormMemoryRegions: TFormMemoryRegions
  Left = 702
  Top = 327
  Width = 613
  Height = 370
  BorderIcons = [biSystemMenu]
  Caption = 'Memory regions'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    595
    325)
  PixelsPerInch = 120
  TextHeight = 16
  object Button1: TButton
    Left = 343
    Top = 364
    Width = 93
    Height = 31
    Anchors = [akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 595
    Height = 325
    Align = alClient
    Columns = <
      item
        Caption = 'Address'
        Width = 98
      end
      item
        Caption = 'Allocation Protect'
        Width = 135
      end
      item
        Caption = 'State'
        Width = 74
      end
      item
        Caption = 'Protect'
        Width = 135
      end
      item
        Caption = 'Type'
        Width = 62
      end
      item
        Caption = 'Size'
        Width = 86
      end>
    HideSelection = False
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = ListView1DblClick
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 136
    Top = 72
    object Saveselectedregions1: TMenuItem
      Caption = 'Save selected regions'
      OnClick = Saveselectedregions1Click
    end
    object SelectAllReadableMemory1: TMenuItem
      Caption = 'Select all readable memory'
      OnClick = SelectAllReadableMemory1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Setselectedregionstobewritable1: TMenuItem
      Caption = 'Set selected regions to be writable'
      OnClick = Setselectedregionstobewritable1Click
    end
  end
  object SaveDialog1: TSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Give the basename of the regions to be saved. (Can be a CT file)'
    Left = 32
    Top = 56
  end
end
