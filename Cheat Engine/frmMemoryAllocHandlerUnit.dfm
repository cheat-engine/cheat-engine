object frmMemoryAllocHandler: TfrmMemoryAllocHandler
  Left = 417
  Top = 370
  Width = 744
  Height = 397
  Caption = 'Memory Allocations'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 218
    Height = 327
    Align = alLeft
    ItemHeight = 16
    TabOrder = 0
    OnClick = ListBox1Click
    OnDblClick = ListBox1DblClick
  end
  object Panel1: TPanel
    Left = 218
    Top = 0
    Width = 508
    Height = 327
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 508
      Height = 129
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Left = 10
        Top = 30
        Width = 89
        Height = 16
        Caption = 'Base Address:'
      end
      object Label2: TLabel
        Left = 10
        Top = 10
        Width = 47
        Height = 16
        Caption = 'Handle:'
      end
      object Label3: TLabel
        Left = 10
        Top = 49
        Width = 97
        Height = 16
        Caption = 'Allocation Type:'
      end
      object Label4: TLabel
        Left = 10
        Top = 69
        Width = 45
        Height = 16
        Caption = 'Protect:'
      end
      object Label5: TLabel
        Left = 10
        Top = 89
        Width = 29
        Height = 16
        Caption = 'Size:'
      end
      object Label6: TLabel
        Left = 10
        Top = 108
        Width = 67
        Height = 16
        Caption = 'Stacktrace:'
      end
      object lblHandle: TLabel
        Left = 118
        Top = 10
        Width = 58
        Height = 16
        Caption = 'lblHandle'
      end
      object lblBaseAddress: TLabel
        Left = 118
        Top = 30
        Width = 97
        Height = 16
        Caption = 'lblBaseAddress'
      end
      object lblAllocationType: TLabel
        Left = 118
        Top = 49
        Width = 105
        Height = 16
        Caption = 'lblAllocationType'
      end
      object lblProtect: TLabel
        Left = 118
        Top = 69
        Width = 56
        Height = 16
        Caption = 'lblProtect'
      end
      object lblSize: TLabel
        Left = 118
        Top = 89
        Width = 40
        Height = 16
        Caption = 'lblSize'
      end
      object rbStacktraceAll: TRadioButton
        Left = 98
        Top = 108
        Width = 41
        Height = 21
        Caption = 'All'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rbStackTraceModulesClick
      end
      object rbStackTraceModules: TRadioButton
        Left = 148
        Top = 108
        Width = 139
        Height = 21
        Caption = 'Module Addresses'
        TabOrder = 1
        OnClick = rbStackTraceModulesClick
      end
      object rdNonsystemModulesOnly: TRadioButton
        Left = 286
        Top = 108
        Width = 219
        Height = 21
        Caption = 'Non-System Module addresses'
        TabOrder = 2
        OnClick = rbStackTraceModulesClick
      end
    end
    object ListView1: TListView
      Left = 0
      Top = 129
      Width = 508
      Height = 198
      Align = alClient
      Columns = <
        item
          Caption = 'Address'
          Width = 98
        end
        item
          Caption = '4 Byte Value'
          Width = 98
        end
        item
          AutoSize = True
          Caption = 'Meaning'
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 1
      ViewStyle = vsReport
      OnDblClick = ListView1DblClick
    end
  end
  object MainMenu1: TMainMenu
    Left = 10
    Top = 24
    object Search1: TMenuItem
      Caption = 'Search'
    end
  end
end
