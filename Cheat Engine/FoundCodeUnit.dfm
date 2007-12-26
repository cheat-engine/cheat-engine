object FoundCodeDialog: TFoundCodeDialog
  Left = 599
  Top = 453
  Width = 374
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
  DesignSize = (
    366
    241)
  PixelsPerInch = 96
  TextHeight = 13
  object Description: TLabel
    Left = 261
    Top = 133
    Width = 93
    Height = 78
    Anchors = [akTop, akRight, akBottom]
    AutoSize = False
    WordWrap = True
  end
  object btnOK: TButton
    Left = 273
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
  object FoundcodeList: TListBox
    Left = 0
    Top = 0
    Width = 251
    Height = 241
    Align = alLeft
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
    OnClick = FoundcodeListClick
    OnDblClick = FoundcodeListDblClick
  end
  object btnReplacewithnops: TButton
    Left = 260
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
    TabOrder = 2
    OnClick = btnReplacewithnopsClick
  end
  object btnOpenDisassembler: TButton
    Left = 260
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
  object btnAddToCodeList: TButton
    Left = 260
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
    TabOrder = 4
    OnClick = btnAddToCodeListClick
  end
  object btnExtraInfo: TButton
    Left = 260
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
    TabOrder = 5
    OnClick = btnExtraInfoClick
  end
end
