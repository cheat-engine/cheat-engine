object frmMemoryTrainer: TfrmMemoryTrainer
  Left = 578
  Top = 254
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'frmMemoryTrainer'
  ClientHeight = 412
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Panel2: TPanel
    Left = 105
    Top = 0
    Width = 395
    Height = 412
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      395
      412)
    object Button1: TButton
      Left = 155
      Top = 374
      Width = 92
      Height = 31
      Anchors = [akBottom]
      Caption = 'Close'
      TabOrder = 0
      OnClick = Button1Click
    end
    object ScrollBox1: TScrollBox
      Left = 0
      Top = 0
      Width = 395
      Height = 366
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 1
      object Label1: TLabel
        Left = 10
        Top = 0
        Width = 46
        Height = 16
        Caption = 'Hotkey:'
      end
      object Label2: TLabel
        Left = 138
        Top = 0
        Width = 33
        Height = 16
        Caption = 'Effect'
      end
    end
    object Button3: TButton
      Left = 10
      Top = 364
      Width = 70
      Height = 31
      Caption = 'Button3'
      TabOrder = 2
      Visible = False
      OnClick = Button3Click
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 105
    Height = 412
    Align = alLeft
    BevelInner = bvLowered
    BevelOuter = bvLowered
    TabOrder = 1
    DesignSize = (
      105
      412)
    object Image1: TImage
      Left = 2
      Top = 33
      Width = 99
      Height = 347
      Anchors = [akLeft, akTop, akRight, akBottom]
      Stretch = True
    end
    object btnLaunch: TButton
      Left = 2
      Top = 2
      Width = 100
      Height = 31
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Launch'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'MS Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = btnLaunchClick
    end
    object Button2: TButton
      Left = 2
      Top = 378
      Width = 99
      Height = 32
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
  object Timer2: TTimer
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
