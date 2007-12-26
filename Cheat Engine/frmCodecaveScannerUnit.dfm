object frmCodecaveScanner: TfrmCodecaveScanner
  Left = 192
  Top = 113
  Width = 240
  Height = 262
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
  DesignSize = (
    232
    228)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 154
    Top = 0
    Width = 63
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Start Address'
  end
  object Label2: TLabel
    Left = 154
    Top = 48
    Width = 63
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Stop Address'
  end
  object Label3: TLabel
    Left = 156
    Top = 88
    Width = 59
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Size of cave'
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 139
    Height = 185
    Anchors = [akLeft, akTop, akRight, akBottom]
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
  object btnStart: TButton
    Left = 148
    Top = 160
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Start'
    Default = True
    TabOrder = 1
    OnClick = btnStartClick
  end
  object editStart: TEdit
    Left = 145
    Top = 16
    Width = 81
    Height = 21
    Anchors = [akTop, akRight]
    CharCase = ecUpperCase
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    MaxLength = 8
    ParentFont = False
    TabOrder = 2
    Text = '00400000'
  end
  object editStop: TEdit
    Left = 145
    Top = 64
    Width = 81
    Height = 21
    Anchors = [akTop, akRight]
    CharCase = ecUpperCase
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    MaxLength = 8
    ParentFont = False
    TabOrder = 3
    Text = '7FFFFFFF'
  end
  object cbNoExecute: TCheckBox
    Left = 0
    Top = 190
    Width = 233
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Also scan non-executable read-only memory'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 211
    Width = 232
    Height = 17
    Align = alBottom
    TabOrder = 5
  end
  object editSize: TEdit
    Left = 145
    Top = 104
    Width = 81
    Height = 21
    Anchors = [akTop, akRight]
    CharCase = ecUpperCase
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    Text = '12'
  end
end
