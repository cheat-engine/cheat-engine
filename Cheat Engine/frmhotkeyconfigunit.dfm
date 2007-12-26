object frmHotkeyConfig: TfrmHotkeyConfig
  Left = 346
  Top = 143
  Width = 422
  Height = 379
  Caption = 'Hotkey configuration'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 231
    Height = 345
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 231
      Height = 13
      Align = alTop
      Caption = 'Functions'
    end
    object ListBox1: TListBox
      Left = 0
      Top = 13
      Width = 231
      Height = 332
      Align = alClient
      ItemHeight = 13
      Items.Strings = (
        'Popup/Hide cheat engine'
        'Pause the selected process'
        'Toggle the speedhack'
        'Speedhack speed 1'
        'Speedhack speed 2'
        'Speedhack speed 3'
        'Speedhack speed 4'
        'Speedhack speed 5'
        'Speedhack speed +'
        'Speedhack speed -'
        'Change type to Binary'
        'Change type to Byte'
        'Change type to 2 Bytes'
        'Change type to 4 Bytes'
        'Change type to 8 Bytes'
        'Change type to Float'
        'Change type to Double'
        'Change type to Text'
        'Change type to Array of byte'
        'New Scan'
        'New Scan-Exact Value'
        'New Scan-Unknown Initial Value'
        'Next Scan-Exact Value'
        'Next Scan-Increased Value'
        'Next Scan-Decreased Value'
        'Next Scan-Changed Value'
        'Next Scan-Unchanged Value'
        'Undo last scan'
        'Cancel the current scan')
      TabOrder = 0
      OnClick = ListBox1Click
    end
  end
  object Panel2: TPanel
    Left = 231
    Top = 0
    Width = 183
    Height = 345
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      183
      345)
    object Label2: TLabel
      Left = 6
      Top = 0
      Width = 34
      Height = 13
      Caption = 'Hotkey'
    end
    object Edit1: TEdit
      Left = 5
      Top = 16
      Width = 172
      Height = 21
      ReadOnly = True
      TabOrder = 0
      OnKeyDown = Edit1KeyDown
    end
    object Button1: TButton
      Left = 14
      Top = 316
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'OK'
      Default = True
      TabOrder = 2
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 94
      Top = 316
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object Button3: TButton
      Left = 128
      Top = 40
      Width = 49
      Height = 17
      Caption = 'Clear'
      TabOrder = 1
      OnClick = Button3Click
    end
    object Panel3: TPanel
      Left = 8
      Top = 64
      Width = 170
      Height = 247
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 4
      DesignSize = (
        170
        247)
      object Label52: TLabel
        Left = 11
        Top = 2
        Width = 31
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Speed'
      end
      object Label51: TLabel
        Left = 60
        Top = 2
        Width = 46
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Sleeptime'
      end
      object Edit2: TEdit
        Left = 4
        Top = 18
        Width = 46
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 0
        Text = '2'
      end
      object Edit3: TEdit
        Left = 60
        Top = 18
        Width = 46
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 1
        Text = '3'
      end
    end
    object Panel4: TPanel
      Left = 8
      Top = 59
      Width = 173
      Height = 97
      BevelOuter = bvNone
      TabOrder = 5
      object Label3: TLabel
        Left = 3
        Top = 1
        Width = 57
        Height = 13
        Caption = 'Speed delta'
      end
      object Edit4: TEdit
        Left = 2
        Top = 16
        Width = 166
        Height = 21
        TabOrder = 0
        Text = '1'
      end
    end
  end
end
