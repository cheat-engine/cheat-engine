object frmMemoryAllocHandler: TfrmMemoryAllocHandler
  Left = 283
  Top = 322
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Memory Allocations'
  ClientHeight = 168
  ClientWidth = 499
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
    Top = 149
    Width = 499
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
    Width = 499
    Height = 108
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
    object lblErr: TLabel
      Left = 40
      Top = 88
      Width = 387
      Height = 16
      Caption = 
        'Couldn'#39't find in heap. Found using the Alloc hook. (Not very use' +
        'ful)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -14
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 499
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label5: TLabel
      Left = 274
      Top = 0
      Width = 225
      Height = 41
      Align = alRight
      Caption = 
        'You can close this window if you want. Allocations will keep get' +
        'ting tracked'
      WordWrap = True
    end
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
