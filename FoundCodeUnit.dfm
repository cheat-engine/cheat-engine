object FoundCodeDialog: TFoundCodeDialog
  Left = 599
  Top = 453
  Width = 383
  Height = 445
  BorderIcons = [biSystemMenu]
  Caption = 'The following opcodes changed the selected address'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object FoundcodeList: TListBox
    Left = 0
    Top = 0
    Width = 235
    Height = 400
    Align = alClient
    ItemHeight = 16
    MultiSelect = True
    PopupMenu = pmOptions
    TabOrder = 0
    OnClick = FoundcodeListClick
    OnContextPopup = FoundcodeListContextPopup
    OnDblClick = FoundcodeListDblClick
  end
  object Panel1: TPanel
    Left = 235
    Top = 0
    Width = 130
    Height = 400
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Description: TLabel
      Left = 0
      Top = 169
      Width = 130
      Height = 190
      Align = alClient
      Alignment = taCenter
      AutoSize = False
      WordWrap = True
    end
    object Panel2: TPanel
      Left = 0
      Top = 359
      Width = 130
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object btnOK: TButton
        Left = 21
        Top = 3
        Width = 90
        Height = 30
        Cancel = True
        Caption = 'OK'
        Default = True
        TabOrder = 0
        OnClick = btnOKClick
      end
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 130
      Height = 169
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        130
        169)
      object btnExtraInfo: TButton
        Left = 4
        Top = 128
        Width = 124
        Height = 31
        Hint = 
          'The selected addresses will be added to the code list in the adv' +
          'anced options window.'
        Anchors = [akTop, akRight]
        Caption = 'More information'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = btnExtraInfoClick
      end
      object btnAddToCodeList: TButton
        Left = 4
        Top = 89
        Width = 124
        Height = 30
        Hint = 
          'The selected addresses will be added to the code list in the adv' +
          'anced options window.'
        Caption = 'Add to the codelist'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = btnAddToCodeListClick
      end
      object btnOpenDisassembler: TButton
        Left = 4
        Top = 49
        Width = 123
        Height = 31
        Hint = 
          'This will open the memoryview and bring the disassemblerview to ' +
          'the selected address'
        Caption = 'Show disassembler'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = btnOpenDisassemblerClick
      end
      object btnReplacewithnops: TButton
        Left = 4
        Top = 10
        Width = 124
        Height = 31
        Hint = 
          'This will replace the selected addresses with code that does not' +
          'hing. (Nops)'
        Caption = 'Replace'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = btnReplacewithnopsClick
      end
    end
  end
  object pmOptions: TPopupMenu
    OnPopup = pmOptionsPopup
    Left = 64
    Top = 64
    object ReplacewithcodethatdoesnothingNOP1: TMenuItem
      Caption = 'Replace with code that does nothing (NOP)'
      OnClick = btnReplacewithnopsClick
    end
    object Showthisaddressinthedisassembler1: TMenuItem
      Caption = 'Show this address in the disassembler'
      OnClick = btnOpenDisassemblerClick
    end
    object Addtothecodelist1: TMenuItem
      Caption = 'Add to the codelist'
      OnClick = btnAddToCodeListClick
    end
    object MoreInfo1: TMenuItem
      Caption = 'More Info'
      Default = True
      OnClick = btnExtraInfoClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Copyselectiontoclipboard1: TMenuItem
      Caption = 'Copy selection to clipboard'
      ShortCut = 16451
      OnClick = Copyselectiontoclipboard1Click
    end
  end
end
