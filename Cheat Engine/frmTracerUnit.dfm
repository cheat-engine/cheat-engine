object frmTracer: TfrmTracer
  Left = 695
  Top = 405
  Width = 396
  Height = 341
  BorderIcons = [biSystemMenu]
  Caption = 'Tracer'
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
  object Splitter1: TSplitter
    Left = 200
    Top = 0
    Height = 305
    Align = alRight
  end
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 200
    Height = 305
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBox1Click
    OnDblClick = ListBox1DblClick
  end
  object Panel1: TPanel
    Left = 203
    Top = 0
    Width = 177
    Height = 305
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object EAXLabel: TLabel
      Left = 4
      Top = 0
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'EAX 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object EBXlabel: TLabel
      Tag = 1
      Left = 4
      Top = 16
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'EBX 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object ECXlabel: TLabel
      Tag = 2
      Left = 4
      Top = 32
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'ECX 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object EDXlabel: TLabel
      Tag = 3
      Left = 4
      Top = 48
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'EDX 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object ESIlabel: TLabel
      Tag = 4
      Left = 4
      Top = 64
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'ESI 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object EDIlabel: TLabel
      Tag = 5
      Left = 4
      Top = 80
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'EDI 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object EBPlabel: TLabel
      Tag = 6
      Left = 4
      Top = 96
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'EBP 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object ESPlabel: TLabel
      Tag = 7
      Left = 4
      Top = 112
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'ESP 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object EIPlabel: TLabel
      Tag = 8
      Left = 4
      Top = 128
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Caption = 'EIP 00000000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
      OnDblClick = EAXLabelDblClick
    end
    object cflabel: TLabel
      Left = 132
      Top = 0
      Width = 32
      Height = 13
      Caption = 'CF 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object pflabel: TLabel
      Left = 132
      Top = 16
      Width = 32
      Height = 13
      Caption = 'PF 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object aflabel: TLabel
      Left = 132
      Top = 32
      Width = 32
      Height = 13
      Caption = 'AF 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object zflabel: TLabel
      Left = 132
      Top = 48
      Width = 32
      Height = 13
      Caption = 'ZF 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object sflabel: TLabel
      Left = 132
      Top = 64
      Width = 32
      Height = 13
      Caption = 'SF 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object oflabel: TLabel
      Left = 132
      Top = 96
      Width = 32
      Height = 13
      Caption = 'OF 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object CSLabel: TLabel
      Tag = 9
      Left = 4
      Top = 160
      Width = 56
      Height = 13
      Cursor = crHandPoint
      Caption = 'CS 0000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object SSlabel: TLabel
      Tag = 10
      Left = 4
      Top = 176
      Width = 56
      Height = 13
      Cursor = crHandPoint
      Caption = 'SS 0000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object DSLabel: TLabel
      Tag = 11
      Left = 4
      Top = 192
      Width = 56
      Height = 13
      Cursor = crHandPoint
      Caption = 'DS 0000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object ESlabel: TLabel
      Tag = 12
      Left = 4
      Top = 208
      Width = 56
      Height = 13
      Cursor = crHandPoint
      Caption = 'ES 0000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object FSlabel: TLabel
      Tag = 13
      Left = 4
      Top = 224
      Width = 56
      Height = 13
      Cursor = crHandPoint
      Caption = 'FS 0000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object GSlabel: TLabel
      Tag = 14
      Left = 4
      Top = 240
      Width = 56
      Height = 13
      Cursor = crHandPoint
      Caption = 'GS 0000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object dflabel: TLabel
      Left = 132
      Top = 80
      Width = 32
      Height = 13
      Caption = 'DF 0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier'
      Font.Style = []
      ParentFont = False
    end
    object Button1: TButton
      Left = 60
      Top = 272
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
end
