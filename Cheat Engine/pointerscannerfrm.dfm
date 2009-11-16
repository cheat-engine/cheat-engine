object frmPointerScanner: TfrmPointerScanner
  Left = 690
  Top = 299
  Width = 627
  Height = 432
  Caption = 'Pointer scan'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 341
    Width = 609
    Height = 21
    Align = alBottom
    Step = 1
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 609
    Height = 21
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pgcPScandata: TPageControl
    Left = 0
    Top = 21
    Width = 609
    Height = 320
    ActivePage = tsPSReverse
    Align = alClient
    TabOrder = 2
    object tsPSDefault: TTabSheet
      Caption = 'tsPSDefault'
      object Label1: TLabel
        Left = 10
        Top = 20
        Width = 123
        Height = 16
        Caption = 'Static pointers found:'
      end
      object Label2: TLabel
        Left = 150
        Top = 20
        Width = 7
        Height = 16
        Caption = '0'
      end
      object Label10: TLabel
        Left = 10
        Top = 39
        Width = 116
        Height = 16
        Caption = 'Method 2 scanners:'
      end
      object Label11: TLabel
        Left = 150
        Top = 39
        Width = 7
        Height = 16
        Caption = '0'
      end
      object Label3: TLabel
        Left = 10
        Top = 59
        Width = 119
        Height = 16
        Caption = 'Scanaddress count:'
      end
      object Label4: TLabel
        Left = 150
        Top = 59
        Width = 7
        Height = 16
        Caption = '0'
      end
      object Label7: TLabel
        Left = 10
        Top = 118
        Width = 137
        Height = 16
        Caption = 'Continues after dissect:'
      end
      object Label8: TLabel
        Left = 150
        Top = 118
        Width = 7
        Height = 16
        Caption = '0'
      end
      object Label12: TLabel
        Left = 10
        Top = 79
        Width = 51
        Height = 16
        Caption = 'Skipped'
      end
      object Label13: TLabel
        Left = 150
        Top = 79
        Width = 7
        Height = 16
        Caption = '0'
      end
      object Label14: TLabel
        Left = 10
        Top = 138
        Width = 123
        Height = 16
        Caption = 'Best spot to continue'
      end
      object Label15: TLabel
        Left = 150
        Top = 138
        Width = 56
        Height = 16
        Caption = '00000000'
      end
      object Label9: TLabel
        Left = 150
        Top = 158
        Width = 3
        Height = 16
      end
      object Label16: TLabel
        Left = 10
        Top = 158
        Width = 51
        Height = 16
        Caption = 'Time left'
      end
      object Label17: TLabel
        Left = 10
        Top = 1
        Width = 37
        Height = 16
        Caption = 'Status'
      end
      object Label18: TLabel
        Left = 150
        Top = 1
        Width = 3
        Height = 16
      end
      object Label19: TLabel
        Left = 151
        Top = 158
        Width = 13
        Height = 16
        Caption = 'inf'
      end
      object btnStopScan: TButton
        Left = 10
        Top = 192
        Width = 92
        Height = 31
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
      object tvRSThreads: TTreeView
        Left = 0
        Top = 119
        Width = 601
        Height = 135
        Align = alClient
        Indent = 19
        TabOrder = 0
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 601
        Height = 119
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label5: TLabel
          Left = 0
          Top = 103
          Width = 51
          Height = 16
          Caption = 'Threads'
        end
        object lblRSTotalStaticPaths: TLabel
          Left = 0
          Top = 30
          Width = 171
          Height = 16
          Caption = 'Of those # have a static base'
        end
        object lblRSTotalPaths: TLabel
          Left = 0
          Top = 10
          Width = 195
          Height = 16
          Caption = 'Total pointer paths encountered: '
        end
        object Label6: TLabel
          Left = 0
          Top = 79
          Width = 149
          Height = 16
          Caption = 'Pointer addresses found:'
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 254
        Width = 601
        Height = 35
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        object Button1: TButton
          Left = 0
          Top = 0
          Width = 92
          Height = 31
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
  end
  object ListView1: TListView
    Left = 384
    Top = 0
    Width = 225
    Height = 329
    Columns = <>
    OwnerData = True
    TabOrder = 3
    ViewStyle = vsReport
    OnData = ListView1Data
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
    object view1: TMenuItem
      Caption = 'view'
      object Sortlist1: TMenuItem
        Caption = 'Sort list'
        OnClick = Sortlist1Click
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
