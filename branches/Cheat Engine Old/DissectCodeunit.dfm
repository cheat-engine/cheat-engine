object frmDissectCode: TfrmDissectCode
  Left = 886
  Top = 270
  Width = 379
  Height = 369
  Caption = 'Dissect Code'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 303
    Width = 361
    Height = 21
    Align = alBottom
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 200
    Height = 303
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object ListBox1: TListBox
      Left = 0
      Top = 57
      Width = 200
      Height = 246
      Align = alClient
      ItemHeight = 16
      MultiSelect = True
      TabOrder = 0
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 200
      Height = 57
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Label2: TLabel
        Left = 0
        Top = 0
        Width = 182
        Height = 16
        Caption = 'Select the module(s) to dissect'
      end
      object Label3: TLabel
        Left = 0
        Top = 20
        Width = 169
        Height = 16
        Caption = 'Hold CTRL to select multiple'
      end
      object cbIncludesystemModules: TCheckBox
        Left = 0
        Top = 40
        Width = 193
        Height = 17
        Caption = 'Include system modules'
        TabOrder = 0
        OnClick = cbIncludesystemModulesClick
      end
    end
  end
  object Panel3: TPanel
    Left = 200
    Top = 0
    Width = 161
    Height = 303
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    object Label6: TLabel
      Left = 19
      Top = 9
      Width = 111
      Height = 16
      Caption = 'Estimated time left:'
    end
    object Label7: TLabel
      Left = 10
      Top = 22
      Width = 139
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Caption = '00:00:00'
    end
    object Label4: TLabel
      Left = 8
      Top = 56
      Width = 140
      Height = 16
      Caption = 'String references found:'
    end
    object lblStringRef: TLabel
      Left = 8
      Top = 72
      Width = 7
      Height = 16
      Caption = '0'
    end
    object Label5: TLabel
      Left = 8
      Top = 96
      Width = 113
      Height = 16
      Caption = 'Conditional Jumps:'
    end
    object lblConditionalJumps: TLabel
      Left = 8
      Top = 112
      Width = 7
      Height = 16
      Caption = '0'
    end
    object Label9: TLabel
      Left = 8
      Top = 136
      Width = 128
      Height = 16
      Caption = 'Unconditional Jumps:'
    end
    object lblUnConditionalJumps: TLabel
      Left = 8
      Top = 152
      Width = 7
      Height = 16
      Caption = '0'
    end
    object Label11: TLabel
      Left = 8
      Top = 176
      Width = 33
      Height = 16
      Caption = 'Calls:'
    end
    object lblCalls: TLabel
      Left = 8
      Top = 192
      Width = 7
      Height = 16
      Caption = '0'
    end
    object Label1: TLabel
      Left = 8
      Top = 216
      Width = 95
      Height = 16
      Caption = 'Maximum offset:'
      Visible = False
    end
    object lblMaxOffset: TLabel
      Left = 8
      Top = 232
      Width = 7
      Height = 16
      Caption = '0'
      Visible = False
    end
    object btnStart: TButton
      Left = 20
      Top = 261
      Width = 109
      Height = 31
      Caption = 'Start'
      TabOrder = 0
      OnClick = btnStartClick
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
  end
end
