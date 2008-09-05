object frmAAEditPrefs: TfrmAAEditPrefs
  Left = 253
  Top = 426
  BorderStyle = bsDialog
  Caption = 'Editor preferences'
  ClientHeight = 147
  ClientWidth = 189
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 106
    Width = 189
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 14
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object Button2: TButton
      Left = 100
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 189
    Height = 106
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object cbShowLineNumbers: TCheckBox
      Left = 32
      Top = 8
      Width = 145
      Height = 17
      Caption = 'Show line numbers'
      TabOrder = 0
      OnClick = cbShowLineNumbersClick
    end
    object cbShowGutter: TCheckBox
      Left = 32
      Top = 24
      Width = 145
      Height = 17
      Caption = 'Show bar on left'
      TabOrder = 1
      OnClick = cbShowGutterClick
    end
    object cbSmartTab: TCheckBox
      Left = 32
      Top = 40
      Width = 145
      Height = 17
      Caption = 'Smart tab'
      TabOrder = 2
      OnClick = cbSmartTabClick
    end
    object btnFont: TButton
      Left = 16
      Top = 80
      Width = 161
      Height = 25
      Caption = 'font'
      TabOrder = 3
      OnClick = btnFontClick
    end
    object cbTabsToSpace: TCheckBox
      Left = 32
      Top = 56
      Width = 97
      Height = 17
      Caption = 'Tabs to space'
      TabOrder = 4
      OnClick = cbTabsToSpaceClick
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdEffects, fdFixedPitchOnly]
    Left = 32
    Top = 64
  end
end
