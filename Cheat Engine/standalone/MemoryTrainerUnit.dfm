object frmMemoryTrainer: TfrmMemoryTrainer
  Left = 866
  Top = 181
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'frmMemoryTrainer'
  ClientHeight = 335
  ClientWidth = 406
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
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 85
    Top = 0
    Width = 321
    Height = 335
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      321
      335)
    object Button1: TButton
      Left = 126
      Top = 304
      Width = 75
      Height = 25
      Anchors = [akBottom]
      Caption = 'Close'
      TabOrder = 0
      OnClick = Button1Click
    end
    object ScrollBox1: TScrollBox
      Left = 0
      Top = 0
      Width = 321
      Height = 297
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 1
      object Label1: TLabel
        Left = 8
        Top = 0
        Width = 37
        Height = 13
        Caption = 'Hotkey:'
      end
      object Label2: TLabel
        Left = 112
        Top = 0
        Width = 28
        Height = 13
        Caption = 'Effect'
      end
    end
    object Button3: TButton
      Left = 8
      Top = 296
      Width = 57
      Height = 25
      Caption = 'Button3'
      TabOrder = 2
      Visible = False
      OnClick = Button3Click
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 85
    Height = 335
    Align = alLeft
    BevelInner = bvLowered
    BevelOuter = bvLowered
    TabOrder = 1
    DesignSize = (
      85
      335)
    object Image1: TImage
      Left = 2
      Top = 27
      Width = 80
      Height = 282
      Anchors = [akLeft, akTop, akRight, akBottom]
      Stretch = True
    end
    object btnLaunch: TButton
      Left = 2
      Top = 2
      Width = 81
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Launch'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = btnLaunchClick
    end
    object Button2: TButton
      Left = 2
      Top = 307
      Width = 80
      Height = 26
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'About'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object Timer1: TTimer
    Interval = 10000
    OnTimer = Timer1Timer
    Left = 133
    Top = 80
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open'
    Left = 32
    Top = 8
  end
  object Freezer: TTimer
    Interval = 250
    OnTimer = FreezerTimer
    Left = 277
    Top = 72
  end
  object XPManifest1: TXPManifest
    Left = 141
    Top = 128
  end
  object Timer2: TTimer
    Enabled = False
    Interval = 10000
    OnTimer = Timer2Timer
    Left = 101
    Top = 216
  end
  object Timer3: TTimer
    Interval = 30000
    OnTimer = Timer3Timer
    Left = 165
    Top = 216
  end
end
