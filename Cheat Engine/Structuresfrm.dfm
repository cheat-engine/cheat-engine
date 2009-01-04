object frmStructures: TfrmStructures
  Left = 405
  Top = 560
  Width = 614
  Height = 392
  Caption = 'Memory dissect'
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
  object tvStructureView: TTreeView
    Left = 0
    Top = 50
    Width = 606
    Height = 288
    Align = alClient
    BorderStyle = bsNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier'
    Font.Style = []
    HideSelection = False
    Indent = 19
    ParentFont = False
    PopupMenu = PopupMenu1
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    OnAdvancedCustomDrawItem = tvStructureViewAdvancedCustomDrawItem
    OnCollapsing = tvStructureViewCollapsing
    OnDblClick = tvStructureViewDblClick
    OnExpanding = tvStructureViewExpanding
    OnMouseDown = tvStructureViewMouseDown
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 606
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 1
    object edtAddress: TEdit
      Left = 4
      Top = 4
      Width = 77
      Height = 21
      PopupMenu = PopupMenu2
      TabOrder = 0
      Text = '00000000'
      OnChange = edtAddressChange
    end
  end
  object HeaderControl1: THeaderControl
    Left = 0
    Top = 33
    Width = 606
    Height = 17
    HotTrack = True
    Sections = <
      item
        ImageIndex = -1
        Text = 'Offset-description'
        Width = 250
      end
      item
        ImageIndex = -1
        Text = 'Address: Value'
        Width = 200
      end>
    Style = hsFlat
    OnSectionResize = HeaderControl1SectionResize
  end
  object MainMenu1: TMainMenu
    Left = 256
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Caption = 'New'
        ShortCut = 16462
        OnClick = New1Click
      end
      object Open1: TMenuItem
        Caption = 'Open'
        ShortCut = 16463
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = 'Save'
        ShortCut = 16467
        OnClick = Save1Click
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Addextraaddress1: TMenuItem
        Caption = 'Add extra address'
        ShortCut = 16449
        OnClick = Addextraaddress1Click
      end
    end
    object Structures1: TMenuItem
      Caption = 'Structures'
      object Definenewstructure1: TMenuItem
        Caption = 'Define new structure'
        ShortCut = 16462
        OnClick = Definenewstructure1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 240
    Top = 192
    object Addelement1: TMenuItem
      Caption = 'Insert element'
      ShortCut = 45
      OnClick = Addelement1Click
    end
    object ChangeElement1: TMenuItem
      Caption = 'Change element'
      OnClick = ChangeElement1Click
    end
    object Deleteelement1: TMenuItem
      Caption = 'Delete element'
      OnClick = Deleteelement1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Addtoaddresslist1: TMenuItem
      Caption = 'Add to address list'
      OnClick = Addtoaddresslist1Click
    end
    object N3: TMenuItem
      Caption = '-'
      Visible = False
    end
    object Recalculateaddress1: TMenuItem
      Caption = 'Recalculate address'
      Visible = False
      OnClick = Recalculateaddress1Click
    end
  end
  object updatetimer: TTimer
    Interval = 500
    OnTimer = updatetimerTimer
    Left = 8
    Top = 112
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'CES'
    Filter = 'Cheat Engine structure files (*.CES)|*.CES|All Files (*.*)|*.*'
    Left = 8
    Top = 144
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'CES'
    Filter = 'Cheat Engine structure files (*.CES)|*.CES|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 40
    Top = 144
  end
  object PopupMenu2: TPopupMenu
    Left = 128
    Top = 8
    object Undo1: TMenuItem
      Caption = '&Undo'
      ShortCut = 16474
      OnClick = Undo1Click
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object Cut1: TMenuItem
      Caption = 'Cu&t'
      ShortCut = 16472
      OnClick = Cut1Click
    end
    object Copy1: TMenuItem
      Caption = '&Copy'
      ShortCut = 16451
      OnClick = Copy1Click
    end
    object Paste1: TMenuItem
      Caption = '&Paste'
      ShortCut = 16470
      OnClick = Paste1Click
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object SelectAll1: TMenuItem
      Caption = 'Select &All'
      OnClick = SelectAll1Click
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object Remove1: TMenuItem
      Caption = 'Remove'
      OnClick = Remove1Click
    end
  end
end
