object frmPointerScannerSettings: TfrmPointerScannerSettings
  Left = 865
  Top = 401
  BorderStyle = bsSingle
  Caption = 'Pointerscanner scanoptions'
  ClientHeight = 484
  ClientWidth = 443
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
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
  end
  object rbDefault: TRadioButton
    Left = 266
    Top = 10
    Width = 149
    Height = 21
    Hint = 
      'Starts from static addresses and then follows the path till it e' +
      'ncounters the target address'
    Caption = 'Old pointer scan'
    TabOrder = 1
    OnClick = rbDefaultClick
  end
  object rbReverse: TRadioButton
    Left = 266
    Top = 30
    Width = 159
    Height = 20
    Hint = 
      'Starts from the target address and finds all addresses that poin' +
      't near there, and then go back further and further'
    Caption = 'Reverse pointer scan'
    Checked = True
    TabOrder = 2
    TabStop = True
    OnClick = rbReverseClick
  end
  object PSSettings: TPageControl
    Left = 10
    Top = 59
    Width = 434
    Height = 267
    ActivePage = PSReverse
    TabOrder = 3
    object PSDefault: TTabSheet
      Caption = 'Default'
      TabVisible = False
      object Label1: TLabel
        Left = 187
        Top = 138
        Width = 31
        Height = 16
        Caption = 'From'
      end
      object Label2: TLabel
        Left = 276
        Top = 138
        Width = 20
        Height = 16
        Caption = 'To:'
      end
      object Label4: TLabel
        Left = 151
        Top = 187
        Width = 255
        Height = 16
        Caption = 'Pointer path must only be inside this region:'
      end
      object Label5: TLabel
        Left = 187
        Top = 207
        Width = 31
        Height = 16
        Caption = 'From'
      end
      object Label6: TLabel
        Left = 276
        Top = 207
        Width = 20
        Height = 16
        Caption = 'To:'
      end
      object Label8: TLabel
        Left = 153
        Top = 118
        Width = 238
        Height = 16
        Caption = 'Address range to scan for static pointers'
      end
      object edtStart: TEdit
        Left = 187
        Top = 158
        Width = 80
        Height = 24
        CharCase = ecUpperCase
        MaxLength = 8
        TabOrder = 0
        Text = '00401000'
      end
      object edtStop: TEdit
        Left = 276
        Top = 158
        Width = 80
        Height = 24
        CharCase = ecUpperCase
        MaxLength = 8
        TabOrder = 1
        Text = '00700000'
      end
      object edtFilterStart: TEdit
        Left = 187
        Top = 226
        Width = 80
        Height = 24
        CharCase = ecUpperCase
        MaxLength = 8
        TabOrder = 2
        Text = '00400000'
        OnChange = edtFilterStartChange
      end
      object edtFilterStop: TEdit
        Left = 276
        Top = 226
        Width = 80
        Height = 24
        CharCase = ecUpperCase
        MaxLength = 8
        TabOrder = 3
        Text = '70000000'
        OnChange = edtFilterStopChange
      end
      object cbunaligned: TCheckBox
        Left = 0
        Top = 69
        Width = 366
        Height = 21
        Caption = 'Also dissect unalligned pointers (slow and almost useless)'
        TabOrder = 4
      end
      object ListBox1: TListBox
        Left = 0
        Top = 118
        Width = 149
        Height = 139
        ItemHeight = 16
        TabOrder = 5
        OnClick = ListBox1Click
      end
      object CheckBox1: TCheckBox
        Left = 0
        Top = 30
        Width = 425
        Height = 20
        Caption = 
          'Fast: Skip already dissected pointers (May miss some important o' +
          'nes)'
        TabOrder = 6
      end
      object CheckBox2: TCheckBox
        Left = 0
        Top = 0
        Width = 434
        Height = 21
        Caption = 'Writable memory as base only'
        Checked = True
        State = cbChecked
        TabOrder = 7
      end
      object CheckBox3: TCheckBox
        Left = 0
        Top = 89
        Width = 434
        Height = 21
        Caption = 'Base pointers can be unalligned'
        TabOrder = 8
      end
      object CheckBox4: TCheckBox
        Left = 0
        Top = 49
        Width = 415
        Height = 21
        Caption = 'Psychotic: Do not skip any memory (slow, REALLY slow....)'
        TabOrder = 9
      end
    end
    object PSReverse: TTabSheet
      Caption = 'Reverse'
      ImageIndex = 1
      TabVisible = False
      object Label10: TLabel
        Left = 39
        Top = 69
        Width = 31
        Height = 16
        Caption = 'From'
      end
      object Label11: TLabel
        Left = 128
        Top = 69
        Width = 20
        Height = 16
        Caption = 'To:'
      end
      object Label13: TLabel
        Left = 4
        Top = 49
        Width = 253
        Height = 16
        Caption = 'Pointer path may only be inside this region:'
      end
      object Label14: TLabel
        Left = 0
        Top = 180
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
        Top = 89
        Width = 80
        Height = 24
        CharCase = ecUpperCase
        MaxLength = 8
        TabOrder = 1
        Text = '7FFFFFFF'
        OnChange = edtReverseStopChange
      end
      object edtReverseStart: TEdit
        Left = 39
        Top = 89
        Width = 80
        Height = 24
        CharCase = ecUpperCase
        MaxLength = 8
        TabOrder = 2
        Text = '00000000'
        OnChange = edtReverseStartChange
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
        Top = 160
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
        Top = 199
        Width = 149
        Height = 24
        Enabled = False
        TabOrder = 5
        Text = '4096'
        Visible = False
      end
      object cbOnlyStackAsBase: TCheckBox
        Left = 0
        Top = 229
        Width = 257
        Height = 21
        Caption = 'Only use the maintread stack as a base'
        TabOrder = 6
        Visible = False
      end
      object cbUseHeapData: TCheckBox
        Left = 0
        Top = 121
        Width = 425
        Height = 16
        Caption = 'Improve pointerscan with gathered heap data'
        Enabled = False
        TabOrder = 7
        OnClick = cbUseHeapDataClick
      end
      object cbHeapOnly: TCheckBox
        Left = 16
        Top = 137
        Width = 409
        Height = 16
        Caption = 'Only allow static and heap addresses in the path'
        Enabled = False
        TabOrder = 8
      end
    end
  end
  object cbreuse: TCheckBox
    Left = 10
    Top = 329
    Width = 424
    Height = 21
    Caption = 
      'Do not free the memory copy when finished, but reuse it for next' +
      ' scan'
    TabOrder = 4
  end
  object cbMustEndWithSpecificOffset: TCheckBox
    Left = 10
    Top = 354
    Width = 247
    Height = 21
    Caption = 'Pointers must end with specific offsets'
    TabOrder = 5
    OnClick = cbMustEndWithSpecificOffsetClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 377
    Width = 443
    Height = 107
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 6
    object Label3: TLabel
      Left = 59
      Top = 43
      Width = 93
      Height = 16
      Caption = 'Size of structure'
    end
    object Label12: TLabel
      Left = 226
      Top = 42
      Width = 57
      Height = 16
      Caption = 'Max level'
    end
    object Label9: TLabel
      Left = 10
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
    TabOrder = 7
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
    TabOrder = 8
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
end
