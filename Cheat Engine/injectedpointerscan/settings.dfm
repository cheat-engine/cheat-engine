object frmpointerscannersettings: Tfrmpointerscannersettings
  Left = 938
  Top = 335
  Width = 373
  Height = 391
  Caption = 'Pointerscan settings'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 152
    Top = 176
    Width = 23
    Height = 13
    Caption = 'From'
  end
  object Label2: TLabel
    Left = 224
    Top = 176
    Width = 16
    Height = 13
    Caption = 'To:'
  end
  object Label4: TLabel
    Left = 152
    Top = 216
    Width = 190
    Height = 13
    Caption = 'The base pointer has to be in this range:'
  end
  object Label5: TLabel
    Left = 152
    Top = 232
    Width = 23
    Height = 13
    Caption = 'From'
  end
  object Label6: TLabel
    Left = 224
    Top = 232
    Width = 16
    Height = 13
    Caption = 'To:'
  end
  object Label3: TLabel
    Left = 88
    Top = 280
    Width = 76
    Height = 13
    Caption = 'Size of structure'
  end
  object Label12: TLabel
    Left = 176
    Top = 280
    Width = 45
    Height = 13
    Caption = 'Max level'
  end
  object Label7: TLabel
    Left = 8
    Top = 8
    Width = 73
    Height = 13
    Caption = 'Address to find:'
  end
  object Label8: TLabel
    Left = 152
    Top = 160
    Width = 189
    Height = 13
    Caption = 'Address range to scan for static pointers'
  end
  object Label9: TLabel
    Left = 8
    Top = 136
    Width = 113
    Height = 13
    Caption = 'Nr of threads scanning: '
  end
  object Button1: TButton
    Left = 88
    Top = 328
    Width = 65
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object edtStart: TEdit
    Left = 152
    Top = 192
    Width = 65
    Height = 21
    CharCase = ecUpperCase
    MaxLength = 8
    TabOrder = 1
    Text = '00401000'
  end
  object edtStop: TEdit
    Left = 224
    Top = 192
    Width = 65
    Height = 21
    CharCase = ecUpperCase
    MaxLength = 8
    TabOrder = 2
    Text = '00700000'
  end
  object edtFilterStart: TEdit
    Left = 152
    Top = 248
    Width = 65
    Height = 21
    CharCase = ecUpperCase
    MaxLength = 8
    TabOrder = 3
    Text = '00400000'
  end
  object edtFilterStop: TEdit
    Left = 224
    Top = 248
    Width = 65
    Height = 21
    CharCase = ecUpperCase
    MaxLength = 8
    TabOrder = 4
    Text = '70000000'
  end
  object editStructsize: TEdit
    Left = 88
    Top = 295
    Width = 65
    Height = 21
    TabOrder = 5
    Text = '1024'
  end
  object edtAddress: TEdit
    Left = 8
    Top = 23
    Width = 89
    Height = 21
    CharCase = ecUpperCase
    MaxLength = 8
    TabOrder = 6
  end
  object editMaxLevel: TEdit
    Left = 176
    Top = 294
    Width = 65
    Height = 21
    TabOrder = 7
    Text = '1'
  end
  object cbunaligned: TCheckBox
    Left = 8
    Top = 96
    Width = 297
    Height = 17
    Caption = 'Also dissect unalligned pointers (slow and almost useless)'
    TabOrder = 8
  end
  object btnCancel: TButton
    Left = 160
    Top = 328
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object ListBox1: TListBox
    Left = 24
    Top = 160
    Width = 121
    Height = 113
    ItemHeight = 13
    TabOrder = 10
    OnClick = ListBox1Click
  end
  object RadioButton1: TRadioButton
    Left = 0
    Top = 312
    Width = 353
    Height = 17
    Caption = 
      'Find static addresses by reading through static module data only' +
      ' (slow)'
    Checked = True
    TabOrder = 11
    TabStop = True
    Visible = False
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 64
    Width = 345
    Height = 17
    Caption = 
      'Fast: Skip already dissected pointers (May miss some important o' +
      'nes)'
    TabOrder = 12
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 48
    Width = 353
    Height = 17
    Caption = 'Writable memory as base only'
    Checked = True
    State = cbChecked
    TabOrder = 13
  end
  object CheckBox3: TCheckBox
    Left = 8
    Top = 112
    Width = 353
    Height = 17
    Caption = 'Base pointers can be unalligned'
    TabOrder = 14
  end
  object edtThreadcount: TEdit
    Left = 120
    Top = 133
    Width = 49
    Height = 21
    TabOrder = 15
    Text = '2'
  end
  object CheckBox4: TCheckBox
    Left = 8
    Top = 80
    Width = 337
    Height = 17
    Caption = 'Psychotic: Do not skip any memory (slow, REALLY slow....)'
    TabOrder = 16
  end
  object ComboBox1: TComboBox
    Left = 176
    Top = 132
    Width = 113
    Height = 21
    ItemHeight = 13
    ItemIndex = 3
    TabOrder = 17
    Text = 'Normal'
    Items.Strings = (
      'Idle'
      'Lowest'
      'Lower'
      'Normal'
      'Higher'
      'Highest'
      'TimeCritical')
  end
end
