object frmPointerScannerSettings: TfrmPointerScannerSettings
  Left = 409
  Top = 227
  Width = 372
  Height = 418
  Caption = 'Pointerscanner scanoptions'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 96
    Top = 312
    Width = 76
    Height = 13
    Caption = 'Size of structure'
  end
  object Label12: TLabel
    Left = 184
    Top = 312
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
  object Label9: TLabel
    Left = 32
    Top = 288
    Width = 113
    Height = 13
    Caption = 'Nr of threads scanning: '
  end
  object Button1: TButton
    Left = 104
    Top = 352
    Width = 65
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object editStructsize: TEdit
    Left = 96
    Top = 327
    Width = 65
    Height = 21
    TabOrder = 1
    Text = '1024'
  end
  object edtAddress: TEdit
    Left = 8
    Top = 23
    Width = 89
    Height = 21
    CharCase = ecUpperCase
    MaxLength = 8
    TabOrder = 2
  end
  object editMaxLevel: TEdit
    Left = 184
    Top = 326
    Width = 65
    Height = 21
    TabOrder = 3
    Text = '1'
  end
  object btnCancel: TButton
    Left = 176
    Top = 352
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object rbDefault: TRadioButton
    Left = 104
    Top = 8
    Width = 121
    Height = 17
    Hint = 
      'Starts from static addresses and then follows the path till it e' +
      'ncounters the target address'
    Caption = 'Default pointer scan'
    Checked = True
    TabOrder = 5
    TabStop = True
    OnClick = rbDefaultClick
  end
  object rbReverse: TRadioButton
    Left = 104
    Top = 24
    Width = 129
    Height = 17
    Hint = 
      'Starts from the target address and finds all addresses that poin' +
      't near there, and then go back further and further'
    Caption = 'Reverse pointer scan'
    TabOrder = 6
    OnClick = rbReverseClick
  end
  object PSSettings: TPageControl
    Left = 8
    Top = 48
    Width = 353
    Height = 217
    ActivePage = PSReverse
    TabOrder = 7
    object PSDefault: TTabSheet
      Caption = 'Default'
      TabVisible = False
      object Label1: TLabel
        Left = 152
        Top = 112
        Width = 23
        Height = 13
        Caption = 'From'
      end
      object Label2: TLabel
        Left = 224
        Top = 112
        Width = 16
        Height = 13
        Caption = 'To:'
      end
      object Label4: TLabel
        Left = 123
        Top = 152
        Width = 203
        Height = 13
        Caption = 'Pointer path must only be inside this region:'
      end
      object Label5: TLabel
        Left = 152
        Top = 168
        Width = 23
        Height = 13
        Caption = 'From'
      end
      object Label6: TLabel
        Left = 224
        Top = 168
        Width = 16
        Height = 13
        Caption = 'To:'
      end
      object Label8: TLabel
        Left = 124
        Top = 96
        Width = 189
        Height = 13
        Caption = 'Address range to scan for static pointers'
      end
      object edtStart: TEdit
        Left = 152
        Top = 128
        Width = 65
        Height = 21
        CharCase = ecUpperCase
        MaxLength = 8
        TabOrder = 0
        Text = '00401000'
      end
      object edtStop: TEdit
        Left = 224
        Top = 128
        Width = 65
        Height = 21
        CharCase = ecUpperCase
        MaxLength = 8
        TabOrder = 1
        Text = '00700000'
      end
      object edtFilterStart: TEdit
        Left = 152
        Top = 184
        Width = 65
        Height = 21
        CharCase = ecUpperCase
        MaxLength = 8
        TabOrder = 2
        Text = '00400000'
        OnChange = edtFilterStartChange
      end
      object edtFilterStop: TEdit
        Left = 224
        Top = 184
        Width = 65
        Height = 21
        CharCase = ecUpperCase
        MaxLength = 8
        TabOrder = 3
        Text = '70000000'
        OnChange = edtFilterStopChange
      end
      object cbunaligned: TCheckBox
        Left = 0
        Top = 56
        Width = 297
        Height = 17
        Caption = 'Also dissect unalligned pointers (slow and almost useless)'
        TabOrder = 4
      end
      object ListBox1: TListBox
        Left = 0
        Top = 96
        Width = 121
        Height = 113
        ItemHeight = 13
        TabOrder = 5
        OnClick = ListBox1Click
      end
      object CheckBox1: TCheckBox
        Left = 0
        Top = 24
        Width = 345
        Height = 17
        Caption = 
          'Fast: Skip already dissected pointers (May miss some important o' +
          'nes)'
        TabOrder = 6
      end
      object CheckBox2: TCheckBox
        Left = 0
        Top = 0
        Width = 353
        Height = 17
        Caption = 'Writable memory as base only'
        Checked = True
        State = cbChecked
        TabOrder = 7
      end
      object CheckBox3: TCheckBox
        Left = 0
        Top = 72
        Width = 353
        Height = 17
        Caption = 'Base pointers can be unalligned'
        TabOrder = 8
      end
      object CheckBox4: TCheckBox
        Left = 0
        Top = 40
        Width = 337
        Height = 17
        Caption = 'Psychotic: Do not skip any memory (slow, REALLY slow....)'
        TabOrder = 9
      end
    end
    object PSReverse: TTabSheet
      Caption = 'Reverse'
      ImageIndex = 1
      TabVisible = False
      object Label10: TLabel
        Left = 32
        Top = 56
        Width = 23
        Height = 13
        Caption = 'From'
      end
      object Label11: TLabel
        Left = 104
        Top = 56
        Width = 16
        Height = 13
        Caption = 'To:'
      end
      object Label13: TLabel
        Left = 3
        Top = 40
        Width = 203
        Height = 13
        Caption = 'Pointer path must only be inside this region:'
      end
      object CbAlligned: TCheckBox
        Left = 0
        Top = 0
        Width = 185
        Height = 17
        Caption = 'Addresses must be 32-bit alligned'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object Edit1: TEdit
        Left = 104
        Top = 72
        Width = 65
        Height = 21
        CharCase = ecUpperCase
        MaxLength = 8
        TabOrder = 1
        Text = '70000000'
        OnChange = Edit1Change
      end
      object Edit2: TEdit
        Left = 32
        Top = 72
        Width = 65
        Height = 21
        CharCase = ecUpperCase
        MaxLength = 8
        TabOrder = 2
        Text = '00400000'
        OnChange = Edit2Change
      end
      object cbStaticOnly: TCheckBox
        Left = 0
        Top = 16
        Width = 201
        Height = 17
        Caption = 'Only find paths with a static address'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
    end
  end
  object edtThreadcount: TEdit
    Left = 144
    Top = 285
    Width = 49
    Height = 21
    TabOrder = 8
    Text = '2'
  end
  object ComboBox1: TComboBox
    Left = 200
    Top = 284
    Width = 113
    Height = 21
    ItemHeight = 13
    ItemIndex = 3
    TabOrder = 9
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
  object cbreuse: TCheckBox
    Left = 8
    Top = 267
    Width = 345
    Height = 17
    Caption = 
      'Do not free the memory copy when finished, but reuse it for next' +
      ' scan'
    TabOrder = 10
  end
end
