object formFindCodeInFile: TformFindCodeInFile
  Left = 821
  Top = 121
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Found offsets'
  ClientHeight = 282
  ClientWidth = 279
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    279
    282)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 204
    Width = 36
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Legend'
  end
  object Label2: TLabel
    Left = 0
    Top = 220
    Width = 215
    Height = 13
    Hint = 'Recommended (I am almost sure this is what you need)'
    Anchors = [akLeft, akBottom]
    Caption = '1=Bytes arround this address are as expected'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Label3: TLabel
    Left = 0
    Top = 236
    Width = 213
    Height = 13
    Hint = 'Recommended (if you needed to nop a opcode near this one)'
    Anchors = [akLeft, akBottom]
    Caption = '2=The bytes arround this opcode got nopped'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Label4: TLabel
    Left = 0
    Top = 252
    Width = 227
    Height = 13
    Hint = 
      'Not really recommended. (The bytes before this opcode, or the by' +
      'tes after this opcode are different, and I dont think CE did tha' +
      't)'
    Anchors = [akLeft, akBottom]
    Caption = '3=Only the bytes before or after are as expected'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Label5: TLabel
    Left = 0
    Top = 268
    Width = 238
    Height = 13
    Hint = 
      'Not Recommended!! (The bytes arround the opcode dont even look t' +
      'he same as when you added the opcode to the list)'
    Anchors = [akLeft, akBottom]
    Caption = '4=The bytes arround this opcode are not the same'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Label6: TLabel
    Left = 0
    Top = 0
    Width = 276
    Height = 13
    Caption = 'Select the addresses you want to patch and click Replace'
  end
  object FoundList: TListBox
    Left = 0
    Top = 16
    Width = 185
    Height = 189
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    MultiSelect = True
    ParentShowHint = False
    ShowHint = False
    Sorted = True
    TabOrder = 0
    OnClick = FoundListClick
  end
  object Button2: TButton
    Left = 192
    Top = 88
    Width = 81
    Height = 25
    Cancel = True
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object btnReplace: TButton
    Left = 192
    Top = 16
    Width = 81
    Height = 25
    Hint = 
      'This will replace the selected addresses with code that does not' +
      'hing'
    Caption = 'Replace'
    Default = True
    Enabled = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    WordWrap = True
    OnClick = btnReplaceClick
  end
  object SaveDialog1: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 136
    Top = 40
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 168
    Top = 56
  end
end
