object Form1: TForm1
  Left = 192
  Top = 114
  Width = 366
  Height = 337
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClick = FormClick
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 117
    Height = 13
    Caption = 'Device to write image to:'
  end
  object Button1: TButton
    Left = 136
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Write'
    Enabled = False
    TabOrder = 0
    OnClick = Button1Click
  end
  object cbDeviceList: TComboBox
    Left = 8
    Top = 24
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Sorted = True
    TabOrder = 1
    OnDropDown = cbDeviceListDropDown
    OnSelect = cbDeviceListSelect
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 48
    Width = 161
    Height = 129
    TabOrder = 2
    object lblMediaType: TLabel
      Left = 8
      Top = 16
      Width = 39
      Height = 13
      Caption = '.............'
    end
    object lblCylinders: TLabel
      Left = 8
      Top = 32
      Width = 39
      Height = 13
      Caption = '.............'
    end
    object lblTracks: TLabel
      Left = 8
      Top = 48
      Width = 39
      Height = 13
      Caption = '.............'
    end
    object lblSectors: TLabel
      Left = 8
      Top = 64
      Width = 39
      Height = 13
      Caption = '.............'
    end
    object lblSectorSize: TLabel
      Left = 8
      Top = 80
      Width = 39
      Height = 13
      Caption = '.............'
    end
    object lblTotalSize: TLabel
      Left = 8
      Top = 96
      Width = 39
      Height = 13
      Caption = '.............'
    end
  end
  object GroupBox2: TGroupBox
    Left = 176
    Top = 48
    Width = 177
    Height = 129
    Caption = 'Settings'
    TabOrder = 3
    object CheckBox1: TCheckBox
      Left = 8
      Top = 16
      Width = 162
      Height = 17
      Caption = 'Show boot menu'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 32
      Width = 162
      Height = 17
      Caption = 'Override autodetect bootdrive'
      TabOrder = 1
      OnClick = CheckBox2Click
    end
    object Edit1: TEdit
      Left = 32
      Top = 48
      Width = 33
      Height = 21
      Enabled = False
      TabOrder = 2
      Text = '80'
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 8
    Top = 184
  end
end
