object frmDissectCode: TfrmDissectCode
  Left = 722
  Top = 238
  Width = 317
  Height = 317
  Caption = 'Dissect Code'
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
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 264
    Width = 301
    Height = 17
    Align = alBottom
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 161
    Height = 264
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ListBox1: TListBox
      Left = 0
      Top = 33
      Width = 161
      Height = 231
      Align = alClient
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 0
      OnClick = ListBox1Click
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 161
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Label2: TLabel
        Left = 0
        Top = 0
        Width = 144
        Height = 13
        Caption = 'Select the module(s) to dissect'
      end
      object Label3: TLabel
        Left = 0
        Top = 16
        Width = 134
        Height = 13
        Caption = 'Hold CTRL to select multiple'
      end
    end
  end
  object Panel3: TPanel
    Left = 161
    Top = 0
    Width = 140
    Height = 264
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 6
      Top = 8
      Width = 99
      Height = 26
      Caption = 'Or fill in the region to scan yourself:'
      WordWrap = True
    end
    object Label6: TLabel
      Left = 9
      Top = 176
      Width = 88
      Height = 13
      Caption = 'Estimated time left:'
    end
    object Label7: TLabel
      Left = 8
      Top = 187
      Width = 113
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'Label7'
    end
    object Label8: TLabel
      Left = 4
      Top = 130
      Width = 45
      Height = 13
      Caption = 'Accuracy'
    end
    object Button1: TButton
      Left = 16
      Top = 212
      Width = 89
      Height = 25
      Caption = 'Start'
      TabOrder = 0
      OnClick = Button1Click
    end
    object edtAccuracy: TEdit
      Left = 8
      Top = 146
      Width = 105
      Height = 21
      Hint = 
        'This will determine how much effort ce will put into filtering o' +
        'ut wrong instructions (Setting this too high will severely decre' +
        'ase the speed while browsing through the code)'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = '2'
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 40
      Width = 113
      Height = 89
      Caption = 'User specified'
      TabOrder = 2
      object Label4: TLabel
        Left = 3
        Top = 12
        Width = 23
        Height = 13
        Caption = 'From'
      end
      object Label5: TLabel
        Left = 4
        Top = 44
        Width = 13
        Height = 13
        Caption = 'To'
      end
      object edtFrom: TEdit
        Left = 3
        Top = 24
        Width = 99
        Height = 21
        TabOrder = 0
        OnChange = edtFromChange
      end
      object edtTo: TEdit
        Left = 3
        Top = 55
        Width = 99
        Height = 21
        TabOrder = 1
        OnChange = edtFromChange
      end
    end
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 16
    Top = 40
  end
end
