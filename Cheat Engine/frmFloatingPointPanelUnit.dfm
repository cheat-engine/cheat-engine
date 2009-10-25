object frmFloatingPointPanel: TfrmFloatingPointPanel
  Left = 1023
  Top = 230
  Width = 287
  Height = 323
  BorderStyle = bsSizeToolWin
  Caption = 'FPU'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object pnlFloatdata: TPanel
    Left = 0
    Top = 139
    Width = 269
    Height = 139
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 269
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
