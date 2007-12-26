object frmServiceDescriptorTables: TfrmServiceDescriptorTables
  Left = 790
  Top = 428
  Width = 469
  Height = 343
  Caption = 'Service Descriptor Table'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TreeView1: TTreeView
    Left = 0
    Top = 0
    Width = 461
    Height = 289
    Align = alClient
    HideSelection = False
    Indent = 19
    PopupMenu = PopupMenu1
    RightClickSelect = True
    TabOrder = 0
    OnDblClick = TreeView1DblClick
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 80
    object File1: TMenuItem
      Caption = 'File'
      object Scancallnumbersandnames1: TMenuItem
        Caption = 'Autoscan names for callnumbers'
        OnClick = Scancallnumbersandnames1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Open1: TMenuItem
        Caption = 'Load callnumber names'
        ShortCut = 16463
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = 'Save callnumber names'
        ShortCut = 16467
        OnClick = Save1Click
      end
    end
    object CancelScan1: TMenuItem
      Caption = 'Cancel Scan'
      Visible = False
      OnClick = CancelScan1Click
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'SDT'
    Filter = 'SDT (*.sdt)|*.sdt'
    Left = 8
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'SDT'
    Filter = 'SDT (*.sdt)|*.sdt'
    Left = 8
    Top = 40
  end
  object FindDialog1: TFindDialog
    Options = [frDown, frFindNext, frDisableMatchCase, frDisableUpDown, frDisableWholeWord]
    OnFind = FindDialog1Find
    Left = 8
    Top = 120
  end
  object PopupMenu1: TPopupMenu
    Left = 48
    Top = 80
    object Find1: TMenuItem
      Caption = 'Find'
      ShortCut = 16454
      OnClick = Find1Click
    end
    object GotoSTDaddress1: TMenuItem
      Caption = 'Go to sdt entry'
      ShortCut = 16455
      OnClick = GotoSTDaddress1Click
    end
    object Changesdt1: TMenuItem
      Caption = 'Change sdt entry'
      ShortCut = 16451
      OnClick = Changesdt1Click
    end
  end
end
