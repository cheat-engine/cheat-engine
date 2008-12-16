object FormFoundCodeListExtra: TFormFoundCodeListExtra
  Left = 697
  Top = 596
  Width = 377
  Height = 301
  Caption = 'Extra info'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = pmCopy
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 369
    Height = 82
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label10: TLabel
      Left = 0
      Top = 36
      Width = 16
      Height = 13
      Caption = '>>'
      Font.Charset = ANSI_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 16
      Top = 37
      Width = 48
      Height = 13
      Caption = 'Label3'
      Font.Charset = ANSI_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 16
      Top = 53
      Width = 48
      Height = 13
      Caption = 'Label4'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label5: TLabel
      Left = 16
      Top = 69
      Width = 48
      Height = 13
      Caption = 'Label5'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 16
      Top = 21
      Width = 48
      Height = 13
      Caption = 'Label2'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 16
      Top = 5
      Width = 48
      Height = 13
      Caption = 'Label1'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 82
    Width = 369
    Height = 9
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 0
    Top = 91
    Width = 369
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Panel4: TPanel
      Left = 24
      Top = 0
      Width = 331
      Height = 57
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      TabOrder = 0
      object Label6: TLabel
        Left = 0
        Top = 0
        Width = 331
        Height = 13
        Align = alTop
        AutoSize = False
        Caption = 'xxx'
        WordWrap = True
      end
      object Label17: TLabel
        Left = 0
        Top = 13
        Width = 331
        Height = 44
        Align = alClient
        AutoSize = False
        Caption = 'yyy'
        WordWrap = True
      end
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 241
    Width = 369
    Height = 26
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      369
      26)
    object Button1: TButton
      Left = 147
      Top = 0
      Width = 75
      Height = 26
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
    Top = 140
    Width = 369
    Height = 101
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 4
    DesignSize = (
      369
      101)
    object Label7: TLabel
      Left = 8
      Top = 10
      Width = 96
      Height = 13
      Caption = 'EAX=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label8: TLabel
      Left = 8
      Top = 26
      Width = 96
      Height = 13
      Caption = 'EBX=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label9: TLabel
      Left = 8
      Top = 42
      Width = 96
      Height = 13
      Caption = 'ECX=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label11: TLabel
      Left = 137
      Top = 10
      Width = 96
      Height = 13
      Anchors = [akTop]
      Caption = 'EDX=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label12: TLabel
      Left = 264
      Top = 10
      Width = 96
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'EBP=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label13: TLabel
      Left = 264
      Top = 26
      Width = 96
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'ESP=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label14: TLabel
      Left = 137
      Top = 42
      Width = 96
      Height = 13
      Anchors = [akTop]
      Caption = 'EDI=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label15: TLabel
      Left = 264
      Top = 42
      Width = 96
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'EIP=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label16: TLabel
      Left = 137
      Top = 26
      Width = 96
      Height = 13
      Anchors = [akTop]
      Caption = 'ESI=DDDDDDDD'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Label18: TLabel
      Left = 8
      Top = 62
      Width = 356
      Height = 41
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'The registers shown here are AFTER the instruction has been exec' +
        'uted. To show them before the instruction is executed use Access' +
        ' Exceptions instead of Debug Registers'
      Visible = False
      WordWrap = True
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
end
