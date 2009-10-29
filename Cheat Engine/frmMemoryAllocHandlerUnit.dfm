object frmMemoryAllocHandler: TfrmMemoryAllocHandler
  Left = 623
  Top = 544
  BorderStyle = bsSingle
  Caption = 'Memory Allocations'
  ClientHeight = 155
  ClientWidth = 463
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object StatusBar1: TStatusBar
    Left = 0
    Top = 136
    Width = 463
    Height = 19
    Panels = <
      item
        Width = 300
      end
      item
        Width = 50
      end>
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 41
    Width = 463
    Height = 95
    Align = alClient
    Caption = 'Data'
    TabOrder = 1
    object Label1: TLabel
      Left = 40
      Top = 64
      Width = 37
      Height = 16
      Caption = 'Flags:'
    end
    object Label2: TLabel
      Left = 40
      Top = 16
      Width = 89
      Height = 16
      Caption = 'Base Address:'
    end
    object lblHeapHandle: TLabel
      Left = 144
      Top = 48
      Width = 3
      Height = 16
    end
    object lblFlags: TLabel
      Left = 144
      Top = 64
      Width = 3
      Height = 16
    end
    object lblBaseAddress: TLabel
      Left = 144
      Top = 16
      Width = 3
      Height = 16
    end
    object Label3: TLabel
      Left = 40
      Top = 48
      Width = 78
      Height = 16
      Caption = 'HeapHandle'
    end
    object Label4: TLabel
      Left = 40
      Top = 32
      Width = 29
      Height = 16
      Caption = 'Size:'
    end
    object lblSize: TLabel
      Left = 144
      Top = 32
      Width = 3
      Height = 16
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 463
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Edit1: TEdit
      Left = 8
      Top = 8
      Width = 121
      Height = 24
      TabOrder = 0
    end
    object Button1: TButton
      Left = 136
      Top = 8
      Width = 105
      Height = 25
      Caption = 'Check Address'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 4
    Top = 83
  end
end
