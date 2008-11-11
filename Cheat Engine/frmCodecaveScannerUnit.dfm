object frmCodecaveScanner: TfrmCodecaveScanner
  Left = 192
  Top = 113
  Width = 355
  Height = 324
  Caption = 'Scan for codecaves'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 266
    Height = 256
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier'
    Font.Style = [fsBold]
    ItemHeight = 16
    ParentFont = False
    TabOrder = 0
    OnDblClick = ListBox1DblClick
  end
  object Panel1: TPanel
    Left = 266
    Top = 0
    Width = 81
    Height = 256
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 10
      Top = 0
      Width = 63
      Height = 13
      Caption = 'Start Address'
    end
    object Label2: TLabel
      Left = 10
      Top = 48
      Width = 63
      Height = 13
      Caption = 'Stop Address'
    end
    object Label3: TLabel
      Left = 14
      Top = 88
      Width = 59
      Height = 13
      Caption = 'Size of cave'
    end
    object btnStart: TButton
      Left = 6
      Top = 160
      Width = 75
      Height = 25
      Caption = 'Start'
      Default = True
      TabOrder = 0
      OnClick = btnStartClick
    end
    object editStart: TEdit
      Left = 0
      Top = 16
      Width = 81
      Height = 21
      CharCase = ecUpperCase
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      MaxLength = 8
      ParentFont = False
      TabOrder = 1
      Text = '00400000'
    end
    object editStop: TEdit
      Left = 0
      Top = 64
      Width = 81
      Height = 21
      CharCase = ecUpperCase
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      MaxLength = 8
      ParentFont = False
      TabOrder = 2
      Text = '7FFFFFFF'
    end
    object editSize: TEdit
      Left = 0
      Top = 104
      Width = 81
      Height = 21
      CharCase = ecUpperCase
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      Text = '12'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 256
    Width = 347
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object cbNoExecute: TCheckBox
      Left = 0
      Top = 1
      Width = 343
      Height = 17
      Caption = 'Also scan non-executable read-only memory'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object ProgressBar1: TProgressBar
      Left = 0
      Top = 17
      Width = 347
      Height = 17
      Align = alBottom
      TabOrder = 1
    end
  end
end
