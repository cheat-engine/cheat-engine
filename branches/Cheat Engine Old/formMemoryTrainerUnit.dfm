object frmMemoryTrainerPreview: TfrmMemoryTrainerPreview
  Left = 641
  Top = 340
  Width = 367
  Height = 366
  HelpContext = 14
  Caption = 'Preview'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 85
    Top = 0
    Height = 332
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 85
    Height = 332
    Align = alLeft
    BevelInner = bvLowered
    BevelOuter = bvLowered
    TabOrder = 0
    OnResize = Panel1Resize
    DesignSize = (
      85
      332)
    object Image1: TImage
      Left = 2
      Top = 26
      Width = 82
      Height = 284
      Anchors = [akLeft, akTop, akRight, akBottom]
      Stretch = True
    end
    object Button2: TButton
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
    end
    object Button3: TButton
      Left = 0
      Top = 308
      Width = 84
      Height = 25
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'About'
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 88
    Top = 0
    Width = 271
    Height = 332
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      271
      332)
    object Button1: TButton
      Left = 101
      Top = 308
      Width = 75
      Height = 25
      Anchors = [akBottom]
      Caption = 'Close'
      TabOrder = 0
    end
    object ScrollBox1: TScrollBox
      Left = 0
      Top = 0
      Width = 271
      Height = 301
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      TabOrder = 1
      object Label3: TLabel
        Left = 112
        Top = 16
        Width = 3
        Height = 13
      end
      object Label4: TLabel
        Left = 8
        Top = 16
        Width = 3
        Height = 13
      end
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
  end
end
