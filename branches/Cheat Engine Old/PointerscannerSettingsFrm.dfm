object frmPointerScannerSettings: TfrmPointerScannerSettings
  Left = 532
  Top = 285
  BorderStyle = bsSingle
  Caption = 'Pointerscanner scanoptions'
  ClientHeight = 473
  ClientWidth = 443
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object edtAddress: TEdit
    Left = 10
    Top = 28
    Width = 111
    Height = 24
    CharCase = ecUpperCase
    MaxLength = 8
    TabOrder = 0
    OnChange = edtAddressChange
  end
  object PSSettings: TPageControl
    Left = 10
    Top = 59
    Width = 434
    Height = 267
    ActivePage = PSReverse
    TabOrder = 1
    object PSReverse: TTabSheet
      Caption = 'Reverse'
      ImageIndex = 1
      TabVisible = False
      object Label10: TLabel
        Left = 39
        Top = 85
        Width = 31
        Height = 16
        Caption = 'From'
      end
      object Label11: TLabel
        Left = 128
        Top = 85
        Width = 20
        Height = 16
        Caption = 'To:'
      end
      object Label13: TLabel
        Left = 4
        Top = 65
        Width = 253
        Height = 16
        Caption = 'Pointer path may only be inside this region:'
      end
      object Label14: TLabel
        Left = 0
        Top = 196
        Width = 150
        Height = 16
        Caption = 'Maximum offset to accept'
        Enabled = False
        Visible = False
      end
      object CbAlligned: TCheckBox
        Left = 0
        Top = 0
        Width = 228
        Height = 21
        Caption = 'Addresses must be 32-bit alligned'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object edtReverseStop: TEdit
        Left = 128
        Top = 105
        Width = 80
        Height = 24
        CharCase = ecUpperCase
        MaxLength = 8
        TabOrder = 1
        Text = '7FFFFFFF'
      end
      object edtReverseStart: TEdit
        Left = 39
        Top = 105
        Width = 80
        Height = 24
        CharCase = ecUpperCase
        MaxLength = 8
        TabOrder = 2
        Text = '00000000'
      end
      object cbStaticOnly: TCheckBox
        Left = 0
        Top = 20
        Width = 247
        Height = 21
        Caption = 'Only find paths with a static address'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object cbStackAsBase: TCheckBox
        Left = 0
        Top = 176
        Width = 415
        Height = 21
        Caption = 
          'Accept stack addresses of the main thread as base pointer as wel' +
          'l'
        TabOrder = 4
        Visible = False
      end
      object Edit3: TEdit
        Left = 0
        Top = 215
        Width = 149
        Height = 24
        Enabled = False
        TabOrder = 5
        Text = '4096'
        Visible = False
      end
      object cbOnlyStackAsBase: TCheckBox
        Left = 0
        Top = 237
        Width = 257
        Height = 21
        Caption = 'Only use the maintread stack as a base'
        TabOrder = 6
        Visible = False
      end
      object cbUseHeapData: TCheckBox
        Left = 0
        Top = 137
        Width = 425
        Height = 16
        Caption = 'Improve pointerscan with gathered heap data'
        TabOrder = 7
        OnClick = cbUseHeapDataClick
      end
      object cbHeapOnly: TCheckBox
        Left = 16
        Top = 153
        Width = 409
        Height = 16
        Hint = 
          'If the address you search for isn'#39't a heap address, the scan wil' +
          'l return 0 results'
        Caption = 'Only allow static and heap addresses in the path'
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
        OnClick = cbHeapOnlyClick
      end
      object cbOnlyOneStatic: TCheckBox
        Left = 0
        Top = 40
        Width = 345
        Height = 17
        Caption = 'Stop traversing a path when a static has been found'
        TabOrder = 9
      end
    end
  end
  object cbMustEndWithSpecificOffset: TCheckBox
    Left = 8
    Top = 346
    Width = 247
    Height = 21
    Caption = 'Pointers must end with specific offsets'
    TabOrder = 2
    OnClick = cbMustEndWithSpecificOffsetClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 366
    Width = 443
    Height = 107
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object Label3: TLabel
      Left = 24
      Top = 42
      Width = 131
      Height = 16
      Caption = 'Maximum offset value:'
    end
    object Label12: TLabel
      Left = 226
      Top = 42
      Width = 57
      Height = 16
      Caption = 'Max level'
    end
    object Label9: TLabel
      Left = 16
      Top = 10
      Width = 139
      Height = 16
      Caption = 'Nr of threads scanning: '
    end
    object Button1: TButton
      Left = 138
      Top = 69
      Width = 80
      Height = 31
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = Button1Click
    end
    object editStructsize: TEdit
      Left = 158
      Top = 38
      Width = 60
      Height = 24
      TabOrder = 1
      Text = '2048'
    end
    object editMaxLevel: TEdit
      Left = 286
      Top = 37
      Width = 80
      Height = 24
      TabOrder = 2
      Text = '5'
    end
    object btnCancel: TButton
      Left = 226
      Top = 69
      Width = 93
      Height = 31
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object edtThreadcount: TEdit
      Left = 158
      Top = 6
      Width = 60
      Height = 24
      TabOrder = 4
      Text = '2'
    end
    object ComboBox1: TComboBox
      Left = 226
      Top = 5
      Width = 140
      Height = 24
      ItemHeight = 16
      ItemIndex = 3
      TabOrder = 5
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
  object cbValueType: TComboBox
    Left = 126
    Top = 28
    Width = 132
    Height = 24
    Style = csDropDownList
    ItemHeight = 16
    ItemIndex = 0
    TabOrder = 4
    Text = '4 Byte'
    Items.Strings = (
      '4 Byte'
      'Float'
      'Double')
  end
  object Panel2: TPanel
    Left = 8
    Top = 0
    Width = 249
    Height = 25
    BevelOuter = bvNone
    TabOrder = 5
    object rbFindAddress: TRadioButton
      Left = 0
      Top = 8
      Width = 113
      Height = 17
      Caption = 'Address to find:'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbFindValueClick
    end
    object rbFindValue: TRadioButton
      Left = 120
      Top = 8
      Width = 113
      Height = 17
      Caption = 'Value to find:'
      TabOrder = 1
      OnClick = rbFindValueClick
    end
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 328
    Width = 281
    Height = 17
    Caption = 'Use pointermap from previous pointerscan'
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 6
  end
end
