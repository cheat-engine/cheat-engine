object frmEnumerateDLLs: TfrmEnumerateDLLs
  Left = 431
  Top = 263
  Width = 369
  Height = 303
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Enumerate DLL'#39's'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Label2: TLabel
    Left = 0
    Top = 0
    Width = 351
    Height = 16
    Align = alTop
    Alignment = taCenter
    Caption = 'Symbols'
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 16
    Width = 351
    Height = 192
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Courier'
    Font.Style = []
    HideSelection = False
    Indent = 19
    ParentFont = False
    PopupMenu = pmSymbol
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    OnDblClick = TreeView1DblClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 208
    Width = 351
    Height = 50
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      351
      50)
    object Button1: TButton
      Left = 135
      Top = 9
      Width = 92
      Height = 30
      Anchors = [akBottom]
      Caption = 'Close'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 0
      Top = 30
      Width = 92
      Height = 20
      Caption = 'Cancel Enum'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object FindDialog1: TFindDialog
    Options = [frDown, frFindNext, frDisableMatchCase, frDisableUpDown, frDisableWholeWord]
    OnFind = FindDialog1Find
    Left = 8
    Top = 48
  end
  object ActionList1: TActionList
    Left = 8
    Top = 16
    object Find: TAction
      Caption = 'Find'
      ShortCut = 16454
      OnExecute = FindExecute
    end
  end
  object pmSymbol: TPopupMenu
    Left = 184
    Top = 120
    object Find1: TMenuItem
      Action = Find
    end
  end
end
