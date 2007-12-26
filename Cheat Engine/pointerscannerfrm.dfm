object frmPointerScanner: TfrmPointerScanner
  Left = 419
  Top = 492
  Width = 627
  Height = 432
  Caption = 'Pointer scan'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label20: TLabel
    Left = 8
    Top = 152
    Width = 33
    Height = 13
    Caption = 'fcount:'
    Visible = False
  end
  object Label21: TLabel
    Left = 123
    Top = 152
    Width = 6
    Height = 13
    Caption = '0'
    Visible = False
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 361
    Width = 619
    Height = 17
    Align = alBottom
    Step = 1
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 17
    Width = 619
    Height = 264
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 100
      Height = 13
      Caption = 'Static pointers found:'
    end
    object Label2: TLabel
      Left = 122
      Top = 16
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label10: TLabel
      Left = 8
      Top = 32
      Width = 94
      Height = 13
      Caption = 'Method 2 scanners:'
    end
    object Label11: TLabel
      Left = 122
      Top = 32
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label3: TLabel
      Left = 8
      Top = 48
      Width = 95
      Height = 13
      Caption = 'Scanaddress count:'
    end
    object Label4: TLabel
      Left = 122
      Top = 48
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label7: TLabel
      Left = 8
      Top = 96
      Width = 110
      Height = 13
      Caption = 'Continues after dissect:'
    end
    object Label8: TLabel
      Left = 122
      Top = 96
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label12: TLabel
      Left = 8
      Top = 64
      Width = 39
      Height = 13
      Caption = 'Skipped'
    end
    object Label13: TLabel
      Left = 122
      Top = 64
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label14: TLabel
      Left = 8
      Top = 112
      Width = 100
      Height = 13
      Caption = 'Best spot to continue'
    end
    object Label15: TLabel
      Left = 122
      Top = 112
      Width = 48
      Height = 13
      Caption = '00000000'
    end
    object Label9: TLabel
      Left = 122
      Top = 128
      Width = 3
      Height = 13
    end
    object Label16: TLabel
      Left = 8
      Top = 128
      Width = 40
      Height = 13
      Caption = 'Time left'
    end
    object Label17: TLabel
      Left = 8
      Top = 1
      Width = 30
      Height = 13
      Caption = 'Status'
    end
    object Label18: TLabel
      Left = 122
      Top = 1
      Width = 3
      Height = 13
    end
    object Label19: TLabel
      Left = 123
      Top = 128
      Width = 11
      Height = 13
      Caption = 'inf'
    end
    object Button1: TButton
      Left = 24
      Top = 192
      Width = 75
      Height = 25
      Caption = 'Stop'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object TreeView2: TTreeView
    Left = 209
    Top = 11
    Width = 417
    Height = 209
    Indent = 19
    ReadOnly = True
    TabOrder = 2
    Visible = False
    OnDblClick = TreeView2DblClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 619
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
  end
  object MainMenu1: TMainMenu
    Left = 256
    Top = 168
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Caption = 'New'
        Enabled = False
        ShortCut = 16462
        OnClick = New1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Open1: TMenuItem
        Caption = 'Open'
        ShortCut = 16463
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = 'Save'
        Enabled = False
        ShortCut = 16467
        OnClick = Save1Click
      end
    end
    object Pointerscanner1: TMenuItem
      Caption = 'Pointer scanner'
      object Method3Fastspeedandaveragememoryusage1: TMenuItem
        Caption = 'Scan for pointer'
        ShortCut = 16464
        OnClick = Method3Fastspeedandaveragememoryusage1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Rescanmemory1: TMenuItem
        Caption = 
          'Rescan memory - Removes pointers not pointing to the right addre' +
          'ss'
        Enabled = False
        OnClick = Rescanmemory1Click
      end
      object Showresults1: TMenuItem
        Caption = 'Show results'
        Enabled = False
        OnClick = Showresults1Click
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'PTR'
    Filter = 'Cheat Engine Injected Pointerlist|*.PTR'
    Left = 16
    Top = 216
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'PTR'
    Filter = 'Cheat Engine injected Pointerlist|*.PTR'
    Left = 48
    Top = 216
  end
  object Timer2: TTimer
    OnTimer = Timer2Timer
    Left = 288
    Top = 80
  end
end
