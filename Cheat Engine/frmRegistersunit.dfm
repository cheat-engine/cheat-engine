object Registers: TRegisters
  Left = 1067
  Top = 282
  Width = 150
  Height = 199
  BorderStyle = bsSizeToolWin
  Caption = 'Registers'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object EAXLabel: TLabel
    Left = 8
    Top = 16
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
  end
  object EBXlabel: TLabel
    Tag = 1
    Left = 8
    Top = 32
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
  end
  object ECXlabel: TLabel
    Tag = 2
    Left = 8
    Top = 48
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
  end
  object EDXlabel: TLabel
    Tag = 3
    Left = 8
    Top = 64
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
  end
  object ESIlabel: TLabel
    Tag = 4
    Left = 8
    Top = 80
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
  end
  object EDIlabel: TLabel
    Tag = 5
    Left = 8
    Top = 96
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
  end
  object EBPlabel: TLabel
    Tag = 6
    Left = 8
    Top = 112
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
  end
  object ESPlabel: TLabel
    Tag = 7
    Left = 8
    Top = 128
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
  end
  object EIPlabel: TLabel
    Tag = 8
    Left = 8
    Top = 144
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
  end
  object Label14: TLabel
    Left = 8
    Top = 0
    Width = 47
    Height = 13
    Caption = 'Registers:'
  end
  object Shape1: TShape
    Left = 8
    Top = 13
    Width = 46
    Height = 2
    Brush.Color = clBlack
  end
  object sbShowFloats: TSpeedButton
    Left = 120
    Top = 72
    Width = 17
    Height = 25
    Hint = 'Floating point registers'
    Caption = '>'
    ParentShowHint = False
    ShowHint = True
    OnClick = sbShowFloatsClick
  end
end
