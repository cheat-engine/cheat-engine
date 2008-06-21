object frmStructures: TfrmStructures
  Left = 406
  Top = 473
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
  PixelsPerInch = 96
  TextHeight = 13
  object TreeView1: TTreeView
    Left = 0
    Top = 33
    Width = 606
    Height = 305
    Align = alClient
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
    ShowRoot = False
    TabOrder = 0
    OnCollapsing = TreeView1Collapsing
    OnDblClick = TreeView1DblClick
    OnExpanding = TreeView1Expanding
    OnMouseDown = TreeView1MouseDown
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
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 137
      Height = 29
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object Button2: TButton
        Left = 4
        Top = 4
        Width = 21
        Height = 21
        Caption = '-'
        TabOrder = 0
        OnClick = Button2Click
      end
      object edtAddress: TEdit
        Left = 28
        Top = 4
        Width = 77
        Height = 21
        TabOrder = 1
        Text = '00000000'
        OnChange = edtAddressChange
      end
      object Button1: TButton
        Left = 108
        Top = 4
        Width = 21
        Height = 21
        Caption = '+'
        TabOrder = 2
        OnClick = Button1Click
      end
    end
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
      object Addextraaddress1: TMenuItem
        Caption = 'Add extra address'
        ShortCut = 16449
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
      Caption = 'Add element'
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
    Left = 16
    Top = 48
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'CES'
    Filter = 'Cheat Engine structure files (*.CES)|*.CES|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 56
    Top = 48
  end
end
