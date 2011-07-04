object frmPatcherMaker2: TfrmPatcherMaker2
  Left = 406
  Top = 236
  Width = 550
  Height = 492
  BorderIcons = []
  Caption = 'Patcher Maker'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    532
    447)
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 0
    Top = 457
    Width = 46
    Height = 16
    Anchors = [akLeft, akBottom]
    Caption = 'Legend'
  end
  object Label2: TLabel
    Left = 0
    Top = 477
    Width = 299
    Height = 16
    Hint = 'Recommended (I am almost sure this is what you need)'
    Anchors = [akLeft, akBottom]
    Caption = '1=The bytes arround this address are as expected'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Label3: TLabel
    Left = 0
    Top = 496
    Width = 283
    Height = 16
    Hint = 'Recommended (if you needed to nop a opcode near this one)'
    Anchors = [akLeft, akBottom]
    Caption = '2=Some bytes arround this address got nopped'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Label4: TLabel
    Left = 0
    Top = 516
    Width = 284
    Height = 16
    Hint = 
      'Not really recommended. (The bytes before this opcode, or the by' +
      'tes after this opcode are different, and I dont think CE did tha' +
      't)'
    Anchors = [akLeft, akBottom]
    Caption = '3=Only the bytes before or after are as expected'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Label5: TLabel
    Left = -1
    Top = 536
    Width = 298
    Height = 16
    Hint = 
      'Not Recommended!! (The bytes arround the opcode dont even look t' +
      'he same as when you added the opcode to the list)'
    Anchors = [akLeft, akBottom]
    Caption = '4=The bytes arround this opcode are not the same'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 430
    Height = 447
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Label6: TLabel
      Left = 0
      Top = 0
      Width = 430
      Height = 16
      Align = alTop
      Caption = 'Select the address(es) you want to patch and click OK'
    end
    object FoundList: TListBox
      Left = 0
      Top = 16
      Width = 430
      Height = 431
      Align = alClient
      ItemHeight = 16
      MultiSelect = True
      ParentShowHint = False
      ShowHint = False
      Sorted = True
      TabOrder = 0
      OnClick = FoundListClick
    end
  end
  object Panel2: TPanel
    Left = 430
    Top = 0
    Width = 102
    Height = 447
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      102
      447)
    object Button1: TButton
      Left = 6
      Top = 22
      Width = 92
      Height = 30
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      Enabled = False
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 6
      Top = 61
      Width = 92
      Height = 31
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Skip'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
