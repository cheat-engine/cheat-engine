object FoundCodeDialog: TFoundCodeDialog
  Left = 599
  Top = 453
  Width = 344
  Height = 275
  BorderIcons = [biSystemMenu]
  Caption = 'The following opcodes changed the selected address'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object FoundcodeList: TListBox
    Left = 0
    Top = 0
    Width = 231
    Height = 241
    Align = alClient
    ItemHeight = 13
    PopupMenu = pmOptions
    TabOrder = 0
    OnClick = FoundcodeListClick
    OnContextPopup = FoundcodeListContextPopup
    OnDblClick = FoundcodeListDblClick
  end
  object Panel1: TPanel
    Left = 231
    Top = 0
    Width = 105
    Height = 241
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      105
      241)
    object Description: TLabel
      Left = 7
      Top = 133
      Width = 93
      Height = 78
      Anchors = [akTop, akRight, akBottom]
      AutoSize = False
      WordWrap = True
    end
    object btnOK: TButton
      Left = 17
      Top = 215
      Width = 73
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnExtraInfo: TButton
      Left = 5
      Top = 104
      Width = 97
      Height = 25
      Hint = 
        'The selected addresses will be added to the code list in the adv' +
        'anced options window.'
      Anchors = [akTop, akRight]
      Caption = 'More information'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnExtraInfoClick
    end
    object btnAddToCodeList: TButton
      Left = 5
      Top = 72
      Width = 97
      Height = 25
      Hint = 
        'The selected addresses will be added to the code list in the adv' +
        'anced options window.'
      Anchors = [akTop, akRight]
      Caption = 'Add to the codelist'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnAddToCodeListClick
    end
    object btnOpenDisassembler: TButton
      Left = 5
      Top = 40
      Width = 97
      Height = 25
      Hint = 
        'This will open the memoryview and bring the disassemblerview to ' +
        'the selected address'
      Anchors = [akTop, akRight]
      Caption = 'Show disassembler'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = btnOpenDisassemblerClick
    end
    object btnReplacewithnops: TButton
      Left = 5
      Top = 8
      Width = 97
      Height = 25
      Hint = 
        'This will replace the selected addresses with code that does not' +
        'hing. (Nops)'
      Anchors = [akTop, akRight]
      Caption = 'Replace'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = btnReplacewithnopsClick
    end
  end
  object pmOptions: TPopupMenu
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
  end
end
