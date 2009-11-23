object Registers: TRegisters
  Left = 1320
  Top = 208
  Width = 159
  Height = 243
  BorderStyle = bsSizeToolWin
  Caption = 'Registers'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnResize = FormResize
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 120
    Height = 198
    Align = alClient
    TabOrder = 0
    object EAXLabel: TLabel
      Left = 10
      Top = 20
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
      Left = 10
      Top = 39
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
    end
    object ECXlabel: TLabel
      Tag = 2
      Left = 10
      Top = 59
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
    end
    object EDXlabel: TLabel
      Tag = 3
      Left = 10
      Top = 79
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
    end
    object ESIlabel: TLabel
      Tag = 4
      Left = 10
      Top = 98
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
    end
    object EDIlabel: TLabel
      Tag = 5
      Left = 10
      Top = 118
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
    end
    object EBPlabel: TLabel
      Tag = 6
      Left = 10
      Top = 138
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
    end
    object ESPlabel: TLabel
      Tag = 7
      Left = 10
      Top = 158
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
    end
    object EIPlabel: TLabel
      Tag = 8
      Left = 10
      Top = 177
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
    end
    object Label14: TLabel
      Left = 10
      Top = 0
      Width = 61
      Height = 16
      Caption = 'Registers:'
    end
    object Shape1: TShape
      Left = 10
      Top = 16
      Width = 56
      Height = 2
      Brush.Color = clBlack
    end
  end
  object Panel2: TPanel
    Left = 120
    Top = 0
    Width = 21
    Height = 198
    Align = alRight
    TabOrder = 1
    object sbShowFloats: TSpeedButton
      Left = 0
      Top = 84
      Width = 21
      Height = 30
      Hint = 'Floating point registers'
      Caption = '>'
      ParentShowHint = False
      ShowHint = True
      OnClick = sbShowFloatsClick
    end
  end
end
