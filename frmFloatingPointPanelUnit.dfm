object frmFloatingPointPanel: TfrmFloatingPointPanel
  Left = 1313
  Top = 239
  Width = 353
  Height = 322
  BorderStyle = bsSizeToolWin
  Caption = 'FPU'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 335
    Height = 277
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'FPU'
      object pnlFloatdata: TPanel
        Left = 0
        Top = 139
        Width = 327
        Height = 107
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 327
        Height = 139
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label1: TLabel
          Left = 0
          Top = 0
          Width = 78
          Height = 16
          Caption = 'ControlWord:'
        end
        object Label2: TLabel
          Left = 0
          Top = 39
          Width = 61
          Height = 16
          Caption = 'TagWord:'
        end
        object Label3: TLabel
          Left = 0
          Top = 20
          Width = 73
          Height = 16
          Caption = 'StatusWord:'
        end
        object Label4: TLabel
          Left = 148
          Top = 0
          Width = 66
          Height = 16
          Caption = 'ErrorOffset:'
          OnClick = Label4Click
        end
        object Label5: TLabel
          Left = 148
          Top = 20
          Width = 82
          Height = 16
          Caption = 'ErrorSelector:'
        end
        object Label6: TLabel
          Left = 148
          Top = 39
          Width = 66
          Height = 16
          Caption = 'DataOffset:'
          OnDblClick = Label6DblClick
        end
        object Label7: TLabel
          Left = 148
          Top = 59
          Width = 82
          Height = 16
          Caption = 'DataSelector:'
        end
        object Label8: TLabel
          Left = 0
          Top = 59
          Width = 78
          Height = 16
          Caption = 'Cr0NpxState:'
        end
        object Label9: TLabel
          Left = 0
          Top = 108
          Width = 83
          Height = 16
          Caption = 'RegisterArea:'
        end
        object ComboBox1: TComboBox
          Left = 89
          Top = 103
          Width = 178
          Height = 24
          Style = csDropDownList
          ItemHeight = 16
          ItemIndex = 1
          TabOrder = 0
          Text = 'Floating point'
          OnSelect = ComboBox1Select
          Items.Strings = (
            'Byte'
            'Floating point')
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Extended'
      ImageIndex = 1
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 327
        Height = 57
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object ComboBox3: TComboBox
          Left = 0
          Top = 0
          Width = 169
          Height = 24
          Style = csDropDownList
          ItemHeight = 16
          ItemIndex = 1
          TabOrder = 0
          Text = 'XMMRegisters'
          OnSelect = ComboBox1Select
          Items.Strings = (
            'FPURegisters'
            'XMMRegisters')
        end
        object ComboBox2: TComboBox
          Left = 0
          Top = 24
          Width = 169
          Height = 24
          Style = csDropDownList
          ItemHeight = 16
          ItemIndex = 6
          TabOrder = 1
          Text = 'Extended'
          OnSelect = ComboBox1Select
          Items.Strings = (
            'Byte'
            '2 Bytes'
            '4 Bytes'
            '8 Bytes'
            'Float'
            'Double'
            'Extended')
        end
      end
      object Memo1: TMemo
        Left = 0
        Top = 57
        Width = 327
        Height = 189
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -13
        Font.Name = 'Courier'
        Font.Style = []
        Lines.Strings = (
          'Memo1')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 1
      end
    end
  end
end
