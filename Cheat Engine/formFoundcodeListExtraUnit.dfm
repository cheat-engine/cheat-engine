object FormFoundCodeListExtra: TFormFoundCodeListExtra
  Left = 532
  Top = 211
  Width = 486
  Height = 358
  Caption = 'Extra info'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = pmCopy
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 468
    Height = 101
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label10: TLabel
      Left = 0
      Top = 44
      Width = 18
      Height = 16
      Caption = '>>'
      Font.Charset = ANSI_CHARSET
      Font.Color = clRed
      Font.Height = -17
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 20
      Top = 46
      Width = 54
      Height = 16
      Caption = 'Label3'
      Font.Charset = ANSI_CHARSET
      Font.Color = clRed
      Font.Height = -17
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 20
      Top = 65
      Width = 54
      Height = 16
      Caption = 'Label4'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 20
      Top = 85
      Width = 54
      Height = 16
      Caption = 'Label5'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 20
      Top = 26
      Width = 54
      Height = 16
      Caption = 'Label2'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 20
      Top = 6
      Width = 54
      Height = 16
      Caption = 'Label1'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 101
    Width = 468
    Height = 11
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 0
    Top = 112
    Width = 468
    Height = 60
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Panel4: TPanel
      Left = 30
      Top = 0
      Width = 516
      Height = 70
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      TabOrder = 0
      object Label6: TLabel
        Left = 0
        Top = 0
        Width = 516
        Height = 16
        Align = alTop
        AutoSize = False
        Caption = 'xxx'
        WordWrap = True
      end
      object Label17: TLabel
        Left = 0
        Top = 16
        Width = 516
        Height = 54
        Align = alClient
        AutoSize = False
        Caption = 'yyy'
        PopupMenu = pmCopy2
        WordWrap = True
      end
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 271
    Width = 468
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      468
      42)
    object Button1: TButton
      Left = 190
      Top = 5
      Width = 92
      Height = 32
      Anchors = [akBottom]
      Cancel = True
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object Panel6: TPanel
    Left = 0
    Top = 172
    Width = 468
    Height = 99
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 4
    OnResize = Panel6Resize
    DesignSize = (
      468
      99)
    object Label7: TLabel
      Left = 26
      Top = 12
      Width = 108
      Height = 16
      Caption = 'EAX=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label8: TLabel
      Left = 26
      Top = 32
      Width = 108
      Height = 16
      Caption = 'EBX=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label9: TLabel
      Left = 26
      Top = 52
      Width = 108
      Height = 16
      Caption = 'ECX=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label11: TLabel
      Left = 146
      Top = 12
      Width = 108
      Height = 16
      Caption = 'EDX=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label12: TLabel
      Left = 268
      Top = 12
      Width = 108
      Height = 16
      Caption = 'EBP=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label13: TLabel
      Left = 268
      Top = 32
      Width = 108
      Height = 16
      Caption = 'ESP=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label14: TLabel
      Left = 146
      Top = 52
      Width = 108
      Height = 16
      Caption = 'EDI=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label15: TLabel
      Left = 268
      Top = 52
      Width = 108
      Height = 16
      Caption = 'EIP=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label16: TLabel
      Left = 146
      Top = 32
      Width = 108
      Height = 16
      Caption = 'ESI=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label18: TLabel
      Left = 0
      Top = 76
      Width = 429
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'The registers shown here are AFTER the instruction has been exec' +
        'uted'
      Visible = False
      WordWrap = True
    end
    object sbShowFloats: TSpeedButton
      Left = 448
      Top = 28
      Width = 21
      Height = 30
      Hint = 'Floating point registers'
      Caption = '>'
      ParentShowHint = False
      ShowHint = True
      OnClick = sbShowFloatsClick
    end
  end
  object pmCopy: TPopupMenu
    Left = 112
    Top = 112
    object Copyaddresstoclipboard1: TMenuItem
      Caption = 'Copy info to clipboard'
      ShortCut = 16451
      OnClick = Copyaddresstoclipboard1Click
    end
  end
  object pmCopy2: TPopupMenu
    Left = 190
    Top = 112
    object Copyguesstoclipboard1: TMenuItem
      Caption = 'Copy easy guess to clipboard'
      OnClick = Copyguesstoclipboard1Click
    end
  end
end
