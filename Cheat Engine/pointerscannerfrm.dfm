object frmPointerScanner: TfrmPointerScanner
  Left = 1018
  Top = 484
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
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 361
    Width = 619
    Height = 17
    Align = alBottom
    Step = 1
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 619
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pgcPScandata: TPageControl
    Left = 8
    Top = 16
    Width = 401
    Height = 273
    ActivePage = tsPSReverse
    TabOrder = 2
    object tsPSDefault: TTabSheet
      Caption = 'tsPSDefault'
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
      object btnStopScan: TButton
        Left = 8
        Top = 156
        Width = 75
        Height = 25
        Hint = 
          'This will stop the current scan and show you the results it has ' +
          'found'
        Caption = 'Stop'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = btnStopScanClick
      end
    end
    object tsPSReverse: TTabSheet
      Caption = 'tsPSReverse'
      ImageIndex = 1
      object Label5: TLabel
        Left = 0
        Top = 8
        Width = 157
        Height = 13
        Caption = 'Total pointer paths encountered: '
      end
      object Label6: TLabel
        Left = 0
        Top = 24
        Width = 140
        Height = 13
        Caption = 'Of those # have a static base'
      end
      object Button1: TButton
        Left = 0
        Top = 92
        Width = 75
        Height = 25
        Hint = 
          'This will stop the current scan and show you the results it has ' +
          'found'
        Caption = 'Stop'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = btnStopScanClick
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 144
    Top = 320
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
    Left = 176
    Top = 288
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'PTR'
    Filter = 'Cheat Engine injected Pointerlist|*.PTR'
    Left = 144
    Top = 288
  end
  object Timer2: TTimer
    OnTimer = Timer2Timer
    Left = 104
    Top = 312
  end
end
