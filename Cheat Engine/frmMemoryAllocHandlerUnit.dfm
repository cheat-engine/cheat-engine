object frmMemoryAllocHandler: TfrmMemoryAllocHandler
  Left = 1122
  Top = 406
  Width = 493
  Height = 345
  Caption = 'Memory Allocations'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 177
    Height = 309
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBox1Click
    OnDblClick = ListBox1DblClick
  end
  object Panel1: TPanel
    Left = 177
    Top = 0
    Width = 300
    Height = 309
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object Label1: TLabel
      Left = 8
      Top = 24
      Width = 68
      Height = 13
      Caption = 'Base Address:'
    end
    object Label2: TLabel
      Left = 8
      Top = 8
      Width = 37
      Height = 13
      Caption = 'Handle:'
    end
    object Label3: TLabel
      Left = 8
      Top = 40
      Width = 76
      Height = 13
      Caption = 'Allocation Type:'
    end
    object Label4: TLabel
      Left = 8
      Top = 56
      Width = 37
      Height = 13
      Caption = 'Protect:'
    end
    object Label5: TLabel
      Left = 8
      Top = 72
      Width = 23
      Height = 13
      Caption = 'Size:'
    end
    object Label6: TLabel
      Left = 8
      Top = 88
      Width = 55
      Height = 13
      Caption = 'Stacktrace:'
    end
    object lblHandle: TLabel
      Left = 96
      Top = 8
      Width = 44
      Height = 13
      Caption = 'lblHandle'
    end
    object lblBaseAddress: TLabel
      Left = 96
      Top = 24
      Width = 72
      Height = 13
      Caption = 'lblBaseAddress'
    end
    object lblAllocationType: TLabel
      Left = 96
      Top = 40
      Width = 80
      Height = 13
      Caption = 'lblAllocationType'
    end
    object lblProtect: TLabel
      Left = 96
      Top = 56
      Width = 44
      Height = 13
      Caption = 'lblProtect'
    end
    object lblSize: TLabel
      Left = 96
      Top = 72
      Width = 30
      Height = 13
      Caption = 'lblSize'
    end
  end
end
