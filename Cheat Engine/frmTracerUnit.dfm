object frmTracer: TfrmTracer
  Left = 666
  Top = 279
  Width = 398
  Height = 419
  BorderIcons = [biSystemMenu]
  Caption = 'Tracer'
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
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 158
    Top = 0
    Width = 4
    Height = 374
    Align = alRight
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 158
    Height = 374
    Align = alClient
    ItemHeight = 16
    TabOrder = 0
    OnClick = ListBox1Click
    OnDblClick = ListBox1DblClick
  end
  object Panel1: TPanel
    Left = 162
    Top = 0
    Width = 218
    Height = 374
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object EAXLabel: TLabel
      Left = 5
      Top = 0
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'EAX 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object EBXlabel: TLabel
      Tag = 1
      Left = 5
      Top = 20
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'EBX 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object ECXlabel: TLabel
      Tag = 2
      Left = 5
      Top = 39
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'ECX 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object EDXlabel: TLabel
      Tag = 3
      Left = 5
      Top = 59
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'EDX 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object ESIlabel: TLabel
      Tag = 4
      Left = 5
      Top = 79
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'ESI 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object EDIlabel: TLabel
      Tag = 5
      Left = 5
      Top = 98
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'EDI 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object EBPlabel: TLabel
      Tag = 6
      Left = 5
      Top = 118
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'EBP 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object ESPlabel: TLabel
      Tag = 7
      Left = 5
      Top = 138
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'ESP 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object EIPlabel: TLabel
      Tag = 8
      Left = 5
      Top = 158
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'EIP 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object cflabel: TLabel
      Left = 162
      Top = 0
      Width = 32
      Height = 13
      Caption = 'CF 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object pflabel: TLabel
      Left = 162
      Top = 20
      Width = 32
      Height = 13
      Caption = 'PF 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object aflabel: TLabel
      Left = 162
      Top = 39
      Width = 32
      Height = 13
      Caption = 'AF 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object zflabel: TLabel
      Left = 162
      Top = 59
      Width = 32
      Height = 13
      Caption = 'ZF 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object sflabel: TLabel
      Left = 162
      Top = 79
      Width = 32
      Height = 13
      Caption = 'SF 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object oflabel: TLabel
      Left = 162
      Top = 118
      Width = 32
      Height = 13
      Caption = 'OF 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object CSLabel: TLabel
      Tag = 9
      Left = 5
      Top = 197
      Width = 56
      Height = 13
      Cursor = crHandPoint
      Caption = 'CS 0000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object SSlabel: TLabel
      Tag = 10
      Left = 5
      Top = 217
      Width = 56
      Height = 13
      Cursor = crHandPoint
      Caption = 'SS 0000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object DSLabel: TLabel
      Tag = 11
      Left = 5
      Top = 236
      Width = 56
      Height = 13
      Cursor = crHandPoint
      Caption = 'DS 0000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object ESlabel: TLabel
      Tag = 12
      Left = 5
      Top = 256
      Width = 56
      Height = 13
      Cursor = crHandPoint
      Caption = 'ES 0000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object FSlabel: TLabel
      Tag = 13
      Left = 5
      Top = 276
      Width = 56
      Height = 13
      Cursor = crHandPoint
      Caption = 'FS 0000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object GSlabel: TLabel
      Tag = 14
      Left = 5
      Top = 295
      Width = 56
      Height = 13
      Cursor = crHandPoint
      Caption = 'GS 0000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object dflabel: TLabel
      Left = 162
      Top = 98
      Width = 32
      Height = 13
      Caption = 'DF 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object sbShowFloats: TSpeedButton
      Left = 197
      Top = 142
      Width = 21
      Height = 30
      Hint = 'Floating point registers'
      Caption = '>'
      ParentShowHint = False
      ShowHint = True
      OnClick = sbShowFloatsClick
    end
    object Button1: TButton
      Left = 74
      Top = 335
      Width = 92
      Height = 31
      Caption = 'Close'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
end
