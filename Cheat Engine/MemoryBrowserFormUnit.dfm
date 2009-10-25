object MemoryBrowser: TMemoryBrowser
  Left = 624
  Top = 233
  Width = 675
  Height = 568
  HelpContext = 12
  Caption = 'Memory Viewer'
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
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 0
    Top = 350
    Width = 657
    Height = 5
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    Beveled = True
    OnCanResize = Splitter1CanResize
    OnMoved = Splitter1Moved
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 657
    Height = 350
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 426
      Top = 0
      Height = 350
      Align = alRight
      AutoSnap = False
    end
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 426
      Height = 350
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
    end
    object RegisterView: TPanel
      Left = 429
      Top = 0
      Width = 228
      Height = 350
      Align = alRight
      TabOrder = 1
      object ScrollBox1: TScrollBox
        Left = 1
        Top = 1
        Width = 226
        Height = 348
        Align = alClient
        TabOrder = 0
        object EAXLabel: TLabel
          Left = 10
          Top = 20
          Width = 96
          Height = 13
          Cursor = crHandPoint
          Caption = 'EAX 00000000'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnClick = EAXLabelDblClick
        end
        object EBXlabel: TLabel
          Tag = 1
          Left = 10
          Top = 31
          Width = 96
          Height = 13
          Cursor = crHandPoint
          Caption = 'EBX 00000000'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnClick = EAXLabelDblClick
        end
        object ECXlabel: TLabel
          Tag = 2
          Left = 10
          Top = 43
          Width = 96
          Height = 13
          Cursor = crHandPoint
          Caption = 'ECX 00000000'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnClick = EAXLabelDblClick
        end
        object EDXlabel: TLabel
          Tag = 3
          Left = 10
          Top = 55
          Width = 96
          Height = 13
          Cursor = crHandPoint
          Caption = 'EDX 00000000'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnClick = EAXLabelDblClick
        end
        object ESIlabel: TLabel
          Tag = 4
          Left = 10
          Top = 66
          Width = 96
          Height = 13
          Cursor = crHandPoint
          Caption = 'ESI 00000000'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnClick = EAXLabelDblClick
        end
        object EDIlabel: TLabel
          Tag = 5
          Left = 10
          Top = 78
          Width = 96
          Height = 13
          Cursor = crHandPoint
          Caption = 'EDI 00000000'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnClick = EAXLabelDblClick
        end
        object EBPlabel: TLabel
          Tag = 6
          Left = 10
          Top = 90
          Width = 96
          Height = 13
          Cursor = crHandPoint
          Caption = 'EBP 00000000'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnClick = EAXLabelDblClick
        end
        object ESPlabel: TLabel
          Tag = 7
          Left = 10
          Top = 102
          Width = 96
          Height = 13
          Cursor = crHandPoint
          Caption = 'ESP 00000000'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnClick = EAXLabelDblClick
        end
        object EIPlabel: TLabel
          Tag = 8
          Left = 10
          Top = 113
          Width = 96
          Height = 13
          Cursor = crHandPoint
          Caption = 'EIP 00000000'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnClick = EAXLabelDblClick
        end
        object CSLabel: TLabel
          Tag = 9
          Left = 10
          Top = 153
          Width = 56
          Height = 13
          Cursor = crHandPoint
          Caption = 'CS 0000'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnClick = EAXLabelDblClick
        end
        object DSLabel: TLabel
          Tag = 11
          Left = 10
          Top = 176
          Width = 56
          Height = 13
          Cursor = crHandPoint
          Caption = 'DS 0000'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnClick = EAXLabelDblClick
        end
        object SSlabel: TLabel
          Tag = 10
          Left = 10
          Top = 164
          Width = 56
          Height = 13
          Cursor = crHandPoint
          Caption = 'SS 0000'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnClick = EAXLabelDblClick
        end
        object ESlabel: TLabel
          Tag = 12
          Left = 10
          Top = 188
          Width = 56
          Height = 13
          Cursor = crHandPoint
          Caption = 'ES 0000'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnClick = EAXLabelDblClick
        end
        object FSlabel: TLabel
          Tag = 13
          Left = 10
          Top = 199
          Width = 56
          Height = 13
          Cursor = crHandPoint
          Caption = 'FS 0000'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnClick = EAXLabelDblClick
        end
        object GSlabel: TLabel
          Tag = 14
          Left = 10
          Top = 211
          Width = 56
          Height = 13
          Cursor = crHandPoint
          Caption = 'GS 0000'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnClick = EAXLabelDblClick
        end
        object cflabel: TLabel
          Tag = 20
          Left = 167
          Top = 20
          Width = 32
          Height = 13
          Caption = 'CF 0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnDblClick = EAXLabelDblClick
        end
        object pflabel: TLabel
          Tag = 21
          Left = 167
          Top = 31
          Width = 32
          Height = 13
          Caption = 'PF 0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnDblClick = EAXLabelDblClick
        end
        object aflabel: TLabel
          Tag = 22
          Left = 167
          Top = 43
          Width = 32
          Height = 13
          Caption = 'AF 0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnDblClick = EAXLabelDblClick
        end
        object zflabel: TLabel
          Tag = 23
          Left = 167
          Top = 55
          Width = 32
          Height = 13
          Caption = 'ZF 0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnDblClick = EAXLabelDblClick
        end
        object sflabel: TLabel
          Tag = 24
          Left = 167
          Top = 66
          Width = 32
          Height = 13
          Caption = 'SF 0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnDblClick = EAXLabelDblClick
        end
        object oflabel: TLabel
          Tag = 26
          Left = 167
          Top = 90
          Width = 32
          Height = 13
          Caption = 'OF 0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnDblClick = EAXLabelDblClick
        end
        object Label14: TLabel
          Left = 10
          Top = 0
          Width = 61
          Height = 16
          Caption = 'Registers:'
        end
        object Shape1: TShape
          Left = 10
          Top = 16
          Width = 56
          Height = 2
          Brush.Color = clBlack
        end
        object Label15: TLabel
          Left = 167
          Top = 0
          Width = 34
          Height = 16
          Caption = 'Flags'
        end
        object Shape2: TShape
          Left = 166
          Top = 16
          Width = 35
          Height = 2
          Brush.Color = clBlack
        end
        object Label16: TLabel
          Left = 7
          Top = 134
          Width = 115
          Height = 16
          Caption = 'Segment Registers'
        end
        object Shape3: TShape
          Left = 7
          Top = 150
          Width = 111
          Height = 3
          Brush.Color = clBlack
        end
        object dflabel: TLabel
          Tag = 25
          Left = 167
          Top = 78
          Width = 32
          Height = 13
          Caption = 'DF 0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Courier'
          Font.Style = []
          ParentFont = False
          OnDblClick = EAXLabelDblClick
        end
        object sbShowFloats: TSpeedButton
          Left = 197
          Top = 167
          Width = 21
          Height = 31
          Hint = 'Floating point registers'
          Caption = '>'
          ParentShowHint = False
          ShowHint = True
          OnClick = sbShowFloatsClick
        end
      end
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 355
    Width = 657
    Height = 143
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 1
    TabOrder = 1
    OnEnter = Panel4Enter
    OnMouseDown = Panel4MouseDown
    OnResize = Panel4Resize
    object HexEdit: TEdit
      Left = 128
      Top = 39
      Width = 22
      Height = 18
      BevelOuter = bvNone
      BorderStyle = bsNone
      Color = clHighlight
      Ctl3D = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlightText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      MaxLength = 2
      ParentCtl3D = False
      ParentFont = False
      PopupMenu = memorypopup
      TabOrder = 0
      Text = 'EE'
      Visible = False
      OnDblClick = HexEditDblClick
      OnExit = HexEditExit
      OnKeyDown = HexEditKeyDown
      OnKeyPress = HexEditKeyPress
    end
    object TextEdit: TEdit
      Left = 679
      Top = 59
      Width = 13
      Height = 17
      BevelOuter = bvNone
      BorderStyle = bsNone
      Color = clHighlight
      Ctl3D = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlightText
      Font.Height = -15
      Font.Name = 'Courier'
      Font.Style = []
      MaxLength = 1
      ParentCtl3D = False
      ParentFont = False
      PopupMenu = memorypopup
      TabOrder = 1
      Text = 'E'
      Visible = False
      OnExit = TextEditExit
      OnKeyDown = TextEditKeyDown
      OnKeyPress = TextEditKeyPress
    end
    object ScrollBar2: TScrollBar
      Left = 640
      Top = 1
      Width = 16
      Height = 141
      Align = alRight
      Kind = sbVertical
      Max = 101
      PageSize = 2
      Position = 50
      TabOrder = 2
      OnScroll = ScrollBar2Scroll
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 639
      Height = 141
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 3
      object Protectlabel: TLabel
        Tag = 2
        Left = 0
        Top = 0
        Width = 639
        Height = 16
        Align = alTop
        Caption = 'Protection'
      end
      object MBCanvas: TPaintBox
        Left = 0
        Top = 16
        Width = 639
        Height = 125
        Align = alClient
        PopupMenu = memorypopup
        OnDblClick = MBCanvasDblClick
        OnMouseDown = MBCanvasMouseDown
        OnMouseMove = MBCanvasMouseMove
        OnMouseUp = MBCanvasMouseUp
        OnPaint = MBCanvasPaint
      end
    end
  end
  object memorypopup: TPopupMenu
    OnPopup = memorypopupPopup
    Left = 8
    Top = 480
    object Change1: TMenuItem
      Caption = 'Edit'
      OnClick = Change1Click
    end
    object Goto1: TMenuItem
      Caption = 'Goto address'
      OnClick = Goto1Click
    end
    object Search1: TMenuItem
      Caption = 'Search memory...'
      ShortCut = 16454
      OnClick = Search1Click
    end
    object N13: TMenuItem
      Caption = '-'
    end
    object Cut1: TMenuItem
      Caption = 'Copy to clipboard'
      ShortCut = 16451
      OnClick = Cut1Click
    end
    object Pastefromclipboard1: TMenuItem
      Caption = 'Paste from clipboard'
      ShortCut = 16464
      OnClick = Pastefromclipboard1Click
    end
    object N15: TMenuItem
      Caption = '-'
    end
    object DisplayType1: TMenuItem
      Caption = 'Display Type'
      object dispBytes: TMenuItem
        Caption = 'Byte hex'
        ShortCut = 16433
        OnClick = DisplayTypeClick
      end
      object dispWords: TMenuItem
        Tag = 1
        Caption = '2 Byte hex'
        ShortCut = 16434
        OnClick = DisplayTypeClick
      end
      object dispDwords: TMenuItem
        Tag = 2
        Caption = '4 Byte hex'
        ShortCut = 16435
        OnClick = DisplayTypeClick
      end
      object dispInts: TMenuItem
        Tag = 3
        Caption = '4 Byte decimal'
        ShortCut = 16436
        OnClick = DisplayTypeClick
      end
      object dispFloat: TMenuItem
        Tag = 4
        Caption = 'Float'
        ShortCut = 16437
        OnClick = DisplayTypeClick
      end
      object dispDouble: TMenuItem
        Tag = 5
        Caption = 'Double'
        ShortCut = 16438
        OnClick = DisplayTypeClick
      end
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Addthisaddresstothelist1: TMenuItem
      Caption = 'Add this address to the list'
      OnClick = Addthisaddresstothelist1Click
    end
    object Makepagewritable1: TMenuItem
      Caption = 'Make page writable'
      OnClick = Makepagewritable1Click
    end
  end
  object debuggerpopup: TPopupMenu
    OnPopup = debuggerpopupPopup
    Top = 240
    object Gotoaddress1: TMenuItem
      Caption = 'Go to address'
      OnClick = Gotoaddress1Click
    end
    object Back1: TMenuItem
      Caption = 'Back'
      OnClick = Back1Click
    end
    object Follow1: TMenuItem
      Caption = 'Follow'
      Visible = False
      OnClick = Follow1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Replacewithnops1: TMenuItem
      Caption = 'Replace with code that does nothing'
      OnClick = Replacewithnops1Click
    end
    object Addthisopcodetothecodelist1: TMenuItem
      Caption = 'Add to the code list'
      OnClick = Addthisopcodetothecodelist1Click
    end
    object Assemble1: TMenuItem
      Caption = 'Assemble'
      OnClick = Assemble1Click
    end
    object Copytoclipboard1: TMenuItem
      Caption = 'Copy to clipboard'
      object CopyBytesAndOpcodes: TMenuItem
        Caption = 'Bytes+Opcodes'
        Default = True
        OnClick = CopyBytesAndOpcodesClick
      end
      object copyBytes: TMenuItem
        Tag = 1
        Caption = 'Bytes'
        OnClick = CopyBytesAndOpcodesClick
      end
      object copyOpcodes: TMenuItem
        Tag = 2
        Caption = 'Opcodes'
        OnClick = CopyBytesAndOpcodesClick
      end
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object Changestateofregisteratthislocation1: TMenuItem
      Caption = 'Change register at this location'
      OnClick = Changestateofregisteratthislocation1Click
    end
    object ogglebreakpoint1: TMenuItem
      Caption = 'Toggle breakpoint'
      OnClick = ogglebreakpoint1Click
    end
    object Breakandtraceinstructions1: TMenuItem
      Caption = 'Break and trace instructions'
      OnClick = Breakandtraceinstructions1Click
    end
    object Findoutwhataddressesthisinstructionaccesses1: TMenuItem
      Caption = 'Find out what addresses this instruction accesses'
      OnClick = Findoutwhataddressesthisinstructionaccesses1Click
    end
    object N9: TMenuItem
      Caption = '-'
    end
    object Createjumptocodecave1: TMenuItem
      Caption = 'Create jump and initialize Code-Cave'
      OnClick = Createjumptocodecave1Click
    end
    object Stealthedit1: TMenuItem
      Caption = 'Stealthedit'
      object miStealthEditPage: TMenuItem
        Caption = 'Stealthedit this page'
        Default = True
        OnClick = miStealthEditPageClick
      end
      object Stealteditmultiplepages1: TMenuItem
        Caption = 'Stealtedit multiple pages'
        OnClick = Stealteditmultiplepages1Click
      end
      object miManualStealthEdit: TMenuItem
        Caption = 'Manually input stealth parameters'
        OnClick = miManualStealthEditClick
      end
    end
  end
  object Timer2: TTimer
    Interval = 500
    OnTimer = Timer2Timer
    Left = 928
    Top = 352
  end
  object MainMenu1: TMainMenu
    object File1: TMenuItem
      Caption = 'File'
      object Newwindow1: TMenuItem
        Caption = 'New window'
        OnClick = Newwindow1Click
      end
      object Loadsymbolfile1: TMenuItem
        Caption = 'Load symbol file'
        Visible = False
      end
      object Savedisassemledoutput1: TMenuItem
        Caption = 'Save disassembled output'
        OnClick = Savedisassemledoutput1Click
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object Setsymbolsearchpath1: TMenuItem
        Caption = 'Set symbol searchpath'
        OnClick = Setsymbolsearchpath1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Savememoryregion1: TMenuItem
        Caption = 'Save memory region'
        ShortCut = 16467
        OnClick = Savememoryregion1Click
      end
      object Loadmemolryregion1: TMenuItem
        Caption = 'Load memory region'
        ShortCut = 16463
        OnClick = Loadmemolryregion1Click
      end
    end
    object Search2: TMenuItem
      Caption = 'Search'
      object Findmemory1: TMenuItem
        Caption = 'Find memory'
        OnClick = Findmemory1Click
      end
      object Assemblycode1: TMenuItem
        Caption = 'Find assembly code'
        OnClick = Assemblycode1Click
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object Stacktrace1: TMenuItem
        Caption = 'Stacktrace'
        Enabled = False
        OnClick = Stacktrace1Click
      end
      object Breakpointlist1: TMenuItem
        Caption = 'Breakpointlist'
        ShortCut = 16450
        OnClick = Breakpointlist1Click
      end
      object Threadlist1: TMenuItem
        Caption = 'Threadlist'
        OnClick = Threadlist1Click
      end
      object Debugstrings1: TMenuItem
        Caption = 'Debug strings'
        ShortCut = 49220
        OnClick = Debugstrings1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MemoryRegions1: TMenuItem
        Caption = 'Memory Regions'
        ShortCut = 16466
        OnClick = MemoryRegions1Click
      end
      object Heaps1: TMenuItem
        Caption = 'Heaplist'
        ShortCut = 16456
        OnClick = Heaps1Click
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object EnumeratedllsandSymbols1: TMenuItem
        Caption = 'Enumerate DLL'#39's and Symbols'
        ShortCut = 49235
        OnClick = EnumeratedllsandSymbols1Click
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object Showsymbols1: TMenuItem
        Caption = 'Show symbols'
        Checked = True
        OnClick = Showsymbols1Click
      end
      object Kernelmodesymbols1: TMenuItem
        Caption = 'Kernelmode symbols'
        OnClick = Kernelmodesymbols1Click
      end
      object Showmoduleaddresses1: TMenuItem
        Caption = 'Show module addresses'
        ShortCut = 16461
        OnClick = Showmoduleaddresses1Click
      end
      object Symbolhandler1: TMenuItem
        Caption = 'Userdefined symbols'
        ShortCut = 16469
        OnClick = Symbolhandler1Click
      end
      object Showvaluesofstaticaddresses1: TMenuItem
        Caption = 'Show '#39'Comment'#39' row'
        ShortCut = 16470
        OnClick = Showvaluesofstaticaddresses1Click
      end
      object Jumplines1: TMenuItem
        Caption = 'Jumplines'
        object Showjumplines1: TMenuItem
          Caption = 'Show jumplines'
          Checked = True
          Default = True
          ShortCut = 49228
          OnClick = Showjumplines1Click
        end
        object Onlyshowjumplineswithinrange1: TMenuItem
          Caption = 'Only show jumplines within range'
          Checked = True
          OnClick = Onlyshowjumplineswithinrange1Click
        end
      end
    end
    object Debug1: TMenuItem
      Caption = 'Debug'
      object Run1: TMenuItem
        Caption = 'Run'
        Enabled = False
        ShortCut = 120
        OnClick = Run1Click
      end
      object Step1: TMenuItem
        Caption = 'Step'
        Enabled = False
        ShortCut = 118
        OnClick = Step1Click
      end
      object StepOver1: TMenuItem
        Caption = 'Step Over'
        Enabled = False
        ShortCut = 119
        OnClick = StepOver1Click
      end
      object Runtill1: TMenuItem
        Caption = 'Run till...'
        Enabled = False
        ShortCut = 115
        OnClick = Runtill1Click
      end
      object Setbreakpoint1: TMenuItem
        Caption = 'Toggle breakpoint'
        ShortCut = 116
        OnClick = ogglebreakpoint1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Break1: TMenuItem
        Caption = 'Break'
        OnClick = Break1Click
      end
    end
    object Extra1: TMenuItem
      Caption = 'Tools'
      object Reservememory1: TMenuItem
        Caption = 'Allocate Memory'
        ShortCut = 49229
        OnClick = Reservememory1Click
      end
      object Scanforcodecaves1: TMenuItem
        Caption = 'Scan for code caves'
        ShortCut = 49219
        OnClick = Scanforcodecaves1Click
      end
      object FillMemory1: TMenuItem
        Caption = 'Fill Memory'
        ShortCut = 49222
        OnClick = FillMemory1Click
      end
      object CreateThread1: TMenuItem
        Caption = 'Create Thread'
        ShortCut = 49236
        OnClick = CreateThread1Click
      end
      object Watchmemoryallocations1: TMenuItem
        Caption = 'Watch memory allocations'
        OnClick = Watchmemoryallocations1Click
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Dissectcode1: TMenuItem
        Caption = 'Dissect code'
        ShortCut = 16458
        OnClick = Dissectcode1Click
      end
      object Dissectdata2: TMenuItem
        Caption = 'Dissect data/structures'
        ShortCut = 16452
        OnClick = Dissectdata2Click
      end
      object Disectwindow1: TMenuItem
        Caption = 'Dissect window(s)'
        ShortCut = 16471
        OnClick = Disectwindow1Click
      end
      object DissectPEheaders1: TMenuItem
        Caption = 'Dissect PE headers'
        ShortCut = 49232
        OnClick = DissectPEheaders1Click
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object Dissectdata1: TMenuItem
        Caption = 'Pointer scan'
        ShortCut = 16464
        OnClick = Dissectdata1Click
      end
      object Findstaticpointers1: TMenuItem
        Caption = 'Find static addresses'
        ShortCut = 49235
        OnClick = Findstaticpointers1Click
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object InjectDLL1: TMenuItem
        Caption = 'Inject DLL'
        ShortCut = 16457
        OnClick = InjectDLL1Click
      end
      object AutoInject1: TMenuItem
        Caption = 'Auto Assemble'
        ShortCut = 16449
        OnClick = AutoInject1Click
      end
      object ScriptEngine1: TMenuItem
        Caption = 'Script Engine'
        ShortCut = 49217
        OnClick = ScriptEngine1Click
      end
      object ScriptConsole1: TMenuItem
        Caption = 'Script Console'
        OnClick = ScriptConsole1Click
      end
    end
    object Kerneltools1: TMenuItem
      Caption = 'Kernel tools'
      object Allocatenonpagedmemory1: TMenuItem
        Caption = 'Allocate nonpaged memory'
        OnClick = Allocatenonpagedmemory1Click
      end
      object Getaddress1: TMenuItem
        Caption = 'Get address'
        OnClick = Getaddress1Click
      end
      object Driverlist1: TMenuItem
        Caption = 'Driver list'
        OnClick = Driverlist1Click
      end
      object Sericedescriptortable1: TMenuItem
        Caption = 'Service Descriptor Table'
        OnClick = Sericedescriptortable1Click
      end
      object GDTlist1: TMenuItem
        Caption = 'GDT list'
        OnClick = GDTlist1Click
      end
      object IDTlist1: TMenuItem
        Caption = 'IDT list'
        OnClick = IDTlist1Click
      end
    end
    object Plugins1: TMenuItem
      Caption = 'Plugins'
      Visible = False
    end
  end
  object OpenMemory: TOpenDialog
    DefaultExt = 'CEM'
    Filter = 'Cheat Engine Memory file|*.CEM|All Files (*.*)|*.*'
    FilterIndex = 0
    Top = 40
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'textfiles (*.txt)|*.txt'
    Left = 32
    Top = 40
  end
  object OpenDllDialog: TOpenDialog
    DefaultExt = 'dll'
    Filter = 'Dll-file (*.dll)|*.dll|All files (*.*)|*.*'
    Title = 'Select the module you want to inject'
    Top = 64
  end
end
