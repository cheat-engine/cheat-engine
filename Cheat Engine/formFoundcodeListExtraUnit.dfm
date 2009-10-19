object FormFoundCodeListExtra: TFormFoundCodeListExtra
  Left = 536
  Top = 342
  Width = 456
  Height = 400
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
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 438
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
    Width = 438
    Height = 11
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 0
    Top = 112
    Width = 438
    Height = 60
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Panel4: TPanel
      Left = 30
      Top = 0
      Width = 486
      Height = 70
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      TabOrder = 0
      object Label6: TLabel
        Left = 0
        Top = 0
        Width = 486
        Height = 16
        Align = alTop
        AutoSize = False
        Caption = 'xxx'
        WordWrap = True
      end
      object Label17: TLabel
        Left = 0
        Top = 16
        Width = 486
        Height = 54
        Align = alClient
        AutoSize = False
        Caption = 'yyy'
        WordWrap = True
      end
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 323
    Width = 438
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      438
      32)
    object Button1: TButton
      Left = 175
      Top = 0
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
    Width = 438
    Height = 151
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 4
    DesignSize = (
      438
      151)
    object Label7: TLabel
      Left = 10
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
      Left = 10
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
      Left = 10
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
      Left = 130
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
      Left = 252
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
      Left = 252
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
      Left = 130
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
      Left = 252
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
      Left = 130
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
      Left = 10
      Top = 76
      Width = 517
      Height = 51
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
