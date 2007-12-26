object frmPatcherMaker2: TfrmPatcherMaker2
  Left = 406
  Top = 236
  Width = 272
  Height = 344
  BorderIcons = []
  Caption = 'Patcher Maker'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    264
    310)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 234
    Width = 36
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Legend'
  end
  object Label2: TLabel
    Left = 0
    Top = 250
    Width = 236
    Height = 13
    Hint = 'Recommended (I am almost sure this is what you need)'
    Anchors = [akLeft, akBottom]
    Caption = '1=The bytes arround this address are as expected'
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
    Top = 266
    Width = 222
    Height = 13
    Hint = 'Recommended (if you needed to nop a opcode near this one)'
    Anchors = [akLeft, akBottom]
    Caption = '2=Some bytes arround this address got nopped'
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
    Top = 282
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
    Left = -1
    Top = 298
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
    Width = 257
    Height = 13
    Caption = 'Select the address(es) you want to patch and click OK'
  end
  object FoundList: TListBox
    Left = 0
    Top = 16
    Width = 170
    Height = 213
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    MultiSelect = True
    ParentShowHint = False
    ShowHint = False
    Sorted = True
    TabOrder = 0
    OnClick = FoundListClick
  end
  object Button1: TButton
    Left = 178
    Top = 24
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    Enabled = False
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 178
    Top = 56
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Skip'
    ModalResult = 2
    TabOrder = 2
  end
end
