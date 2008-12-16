object frmFloatingPointPanel: TfrmFloatingPointPanel
  Left = 814
  Top = 101
  Width = 269
  Height = 323
  BorderStyle = bsSizeToolWin
  Caption = 'FPU'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlFloatdata: TPanel
    Left = 0
    Top = 113
    Width = 261
    Height = 176
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 261
    Height = 113
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 62
      Height = 13
      Caption = 'ControlWord:'
    end
    object Label2: TLabel
      Left = 0
      Top = 32
      Width = 48
      Height = 13
      Caption = 'TagWord:'
    end
    object Label3: TLabel
      Left = 0
      Top = 16
      Width = 59
      Height = 13
      Caption = 'StatusWord:'
    end
    object Label4: TLabel
      Left = 120
      Top = 0
      Width = 53
      Height = 13
      Caption = 'ErrorOffset:'
    end
    object Label5: TLabel
      Left = 120
      Top = 16
      Width = 64
      Height = 13
      Caption = 'ErrorSelector:'
    end
    object Label6: TLabel
      Left = 120
      Top = 32
      Width = 54
      Height = 13
      Caption = 'DataOffset:'
    end
    object Label7: TLabel
      Left = 120
      Top = 48
      Width = 65
      Height = 13
      Caption = 'DataSelector:'
    end
    object Label8: TLabel
      Left = 0
      Top = 48
      Width = 63
      Height = 13
      Caption = 'Cr0NpxState:'
    end
    object Label9: TLabel
      Left = 0
      Top = 88
      Width = 64
      Height = 13
      Caption = 'RegisterArea:'
    end
    object ComboBox1: TComboBox
      Left = 72
      Top = 84
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 2
      TabOrder = 0
      Text = 'Float'
      OnSelect = ComboBox1Select
      Items.Strings = (
        'Byte'
        'Dword'
        'Float'
        'Double')
    end
  end
end
