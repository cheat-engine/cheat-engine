object frmPointerScanner: TfrmPointerScanner
  Left = 679
  Top = 422
  Width = 622
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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 341
    Width = 604
    Height = 21
    Align = alBottom
    Step = 1
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 604
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object cbType: TComboBox
      Left = 0
      Top = 0
      Width = 145
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
      TabOrder = 0
      Visible = False
      OnChange = cbTypeChange
      Items.Strings = (
        '4 Byte'
        'Float'
        'Double')
    end
  end
  object pgcPScandata: TPageControl
    Left = 0
    Top = 24
    Width = 604
    Height = 317
    ActivePage = tsPSReverse
    Align = alClient
    TabOrder = 2
    object tsPSReverse: TTabSheet
      Caption = 'tsPSReverse'
      ImageIndex = 1
      object tvRSThreads: TTreeView
        Left = 0
        Top = 65
        Width = 596
        Height = 186
        Align = alClient
        Indent = 19
        TabOrder = 0
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 596
        Height = 65
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label5: TLabel
          Left = 0
          Top = 47
          Width = 51
          Height = 16
          Caption = 'Threads'
        end
        object lblRSTotalStaticPaths: TLabel
          Left = 0
          Top = 6
          Width = 171
          Height = 16
          Caption = 'Of those # have a static base'
        end
        object Label6: TLabel
          Left = 0
          Top = 23
          Width = 149
          Height = 16
          Caption = 'Pointer addresses found:'
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 251
        Width = 596
        Height = 35
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        object btnStopScan: TButton
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
    ColumnClick = False
    HideSelection = False
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 3
    ViewStyle = vsReport
    Visible = False
    OnData = ListView1Data
    OnDblClick = ListView1DblClick
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
        ShortCut = 16466
        OnClick = Rescanmemory1Click
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'PTR'
    Filter = 'Cheat Engine Injected Pointerlist|*.PTR'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Specify the filename you want to store the results'
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
    Interval = 500
    OnTimer = Timer2Timer
    Left = 104
    Top = 312
  end
  object PopupMenu1: TPopupMenu
    Left = 464
    Top = 96
    object Resyncmodulelist1: TMenuItem
      Caption = 'Resync modulelist'
      OnClick = Resyncmodulelist1Click
    end
  end
end
