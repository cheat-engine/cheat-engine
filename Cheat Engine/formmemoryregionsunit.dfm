object FormMemoryRegions: TFormMemoryRegions
  Left = 433
  Top = 399
  Width = 512
  Height = 299
  BorderIcons = [biSystemMenu]
  Caption = 'Memory regions'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    504
    265)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 214
    Top = 238
    Width = 75
    Height = 25
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
    Width = 504
    Height = 232
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Address'
        Width = 80
      end
      item
        Caption = 'Allocation Protect'
        Width = 110
      end
      item
        Caption = 'State'
        Width = 60
      end
      item
        Caption = 'Protect'
        Width = 110
      end
      item
        Caption = 'Type'
      end
      item
        Caption = 'Size'
        Width = 70
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
