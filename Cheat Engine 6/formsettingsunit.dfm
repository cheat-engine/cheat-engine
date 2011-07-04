object formSettings: TformSettings
  Left = 257
  Top = 306
  Width = 785
  Height = 574
  BorderIcons = [biSystemMenu]
  Caption = 'Cheat Engine settings'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object pnlConfig: TPanel
    Left = 0
    Top = 0
    Width = 767
    Height = 482
    Align = alClient
    TabOrder = 0
    object tvMenuSelection: TTreeView
      Left = 1
      Top = 1
      Width = 148
      Height = 480
      Align = alLeft
      HideSelection = False
      Indent = 19
      ReadOnly = True
      RightClickSelect = True
      ShowRoot = False
      TabOrder = 0
      OnChange = tvMenuSelectionChange
      OnCollapsing = tvMenuSelectionCollapsing
      Items.Data = {
        08000000290000000000000000000000FFFFFFFFFFFFFFFF0000000001000000
        1047656E6572616C2073657474696E6773230000000000000000000000FFFFFF
        FFFFFFFFFF00000000000000000A546F6F6C73206D656E752000000000000000
        00000000FFFFFFFFFFFFFFFF000000000000000007486F746B65797325000000
        0000000000000000FFFFFFFFFFFFFFFF00000000000000000C556E72616E646F
        6D697A6572260000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        000D5363616E2073657474696E6773200000000000000000000000FFFFFFFFFF
        FFFFFF000000000000000007506C7567696E73240000000000000000000000FF
        FFFFFFFFFFFFFF00000000000000000B436F64652046696E6465722A00000000
        00000000000000FFFFFFFFFFFFFFFF000000000000000011417373656D626C65
        72206F7074696F6E731E0000000000000000000000FFFFFFFFFFFFFFFF000000
        0000000000054578747261}
    end
    object pcSetting: TPageControl
      Left = 149
      Top = 1
      Width = 617
      Height = 480
      ActivePage = GeneralSettings
      Align = alClient
      Style = tsFlatButtons
      TabOrder = 1
      object GeneralSettings: TTabSheet
        Caption = 'General Settings'
        object Label11: TLabel
          Left = 209
          Top = 395
          Width = 18
          Height = 16
          Caption = 'ms'
        end
        object Label12: TLabel
          Left = 209
          Top = 425
          Width = 18
          Height = 16
          Caption = 'ms'
        end
        object Label13: TLabel
          Left = 63
          Top = 395
          Width = 91
          Height = 16
          Caption = 'Update interval'
        end
        object Label14: TLabel
          Left = 66
          Top = 425
          Width = 88
          Height = 16
          Caption = 'Freeze interval'
        end
        object Label16: TLabel
          Left = 10
          Top = 464
          Width = 143
          Height = 16
          Caption = 'Network Update Interval'
          Visible = False
        end
        object Label17: TLabel
          Left = 209
          Top = 464
          Width = 18
          Height = 16
          Caption = 'ms'
          Visible = False
        end
        object Label18: TLabel
          Left = 256
          Top = 395
          Width = 201
          Height = 16
          Caption = 'Found address list update interval'
        end
        object Label19: TLabel
          Left = 508
          Top = 396
          Width = 18
          Height = 16
          Caption = 'ms'
        end
        object Label23: TLabel
          Left = 10
          Top = 345
          Width = 245
          Height = 16
          Caption = 'Automatically attach to processes named'
        end
        object Label24: TLabel
          Left = 408
          Top = 345
          Width = 141
          Height = 16
          Caption = '(Seperate entries with ; )'
        end
        object cbShowUndo: TCheckBox
          Left = 1
          Top = 2
          Width = 601
          Height = 21
          Caption = 'Show undo button'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object cbShowAdvanced: TCheckBox
          Left = 1
          Top = 22
          Width = 601
          Height = 21
          Caption = 'Show advanced options.'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object cbCenterOnPopup: TCheckBox
          Left = 1
          Top = 60
          Width = 601
          Height = 21
          Caption = 'Center Cheat Engine when bringing to front'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object EditUpdateInterval: TEdit
          Left = 156
          Top = 390
          Width = 53
          Height = 24
          Hint = 
            'The number of milliseconds that Cheat Engine will wait before re' +
            'freshing the list of addresses'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          Text = '500'
        end
        object EditFreezeInterval: TEdit
          Left = 156
          Top = 420
          Width = 53
          Height = 24
          Hint = 
            'The number of milliseconds that Cheat Engine will wait before re' +
            'setting the frozen addresses to their original value.'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          Text = '250'
        end
        object GroupBox1: TGroupBox
          Left = 0
          Top = 228
          Width = 602
          Height = 86
          Caption = 'Address list specific'
          TabOrder = 5
          object cbShowAsSigned: TCheckBox
            Left = 20
            Top = 20
            Width = 542
            Height = 21
            Caption = 'Show values as if they are signed'
            TabOrder = 0
          end
          object cbBinariesAsDecimal: TCheckBox
            Left = 20
            Top = 39
            Width = 542
            Height = 21
            Caption = 'Show and work with binaries as if they are decimals'
            TabOrder = 1
          end
          object cbsimplecopypaste: TCheckBox
            Left = 20
            Top = 59
            Width = 542
            Height = 21
            Caption = 'Simple paste'
            TabOrder = 2
          end
        end
        object EditNetworkUpdateInterval: TEdit
          Left = 156
          Top = 459
          Width = 53
          Height = 24
          Hint = 
            'The number of milliseconds that Cheat Engine will wait before re' +
            'freshing the list of addresses'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
          Text = '500'
          Visible = False
        end
        object cbUpdatefoundList: TCheckBox
          Left = 1
          Top = 41
          Width = 601
          Height = 21
          Caption = 'Update the list of found addresses even after scanning'
          Checked = True
          State = cbChecked
          TabOrder = 7
          OnClick = cbUpdatefoundListClick
        end
        object editUpdatefoundInterval: TEdit
          Left = 455
          Top = 391
          Width = 51
          Height = 24
          Hint = 
            'The number of milliseconds that Cheat Engine will wait before re' +
            'freshing the list of addresses'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 8
          Text = '1000'
        end
        object cbHideAllWindows: TCheckBox
          Left = 1
          Top = 79
          Width = 429
          Height = 21
          Caption = 
            'Hide some/all windows instead of trying to bring cheat engine to' +
            ' front'
          Checked = True
          State = cbChecked
          TabOrder = 9
          OnClick = cbHideAllWindowsClick
        end
        object btnExcludeProcesses: TButton
          Left = 433
          Top = 78
          Width = 90
          Height = 21
          Caption = 'More...'
          TabOrder = 10
          OnClick = btnExcludeProcessesClick
        end
        object EditAutoAttach: TEdit
          Left = 259
          Top = 340
          Width = 143
          Height = 24
          Hint = 
            'Type the name of the process you want to automatically open. Not' +
            'e: Only works when NO process has been opened yet'
          TabOrder = 11
        end
        object cbAlwaysAutoAttach: TCheckBox
          Left = 10
          Top = 367
          Width = 592
          Height = 21
          Caption = 'Even autoattach when another process has already been selected'
          TabOrder = 12
        end
        object cbSaveWindowPos: TCheckBox
          Left = 1
          Top = 138
          Width = 601
          Height = 21
          Caption = 'Save window positions'
          TabOrder = 13
        end
        object cbOldSpeedhack: TCheckBox
          Left = 1
          Top = 118
          Width = 601
          Height = 21
          Caption = 'Use old speedhack'
          TabOrder = 14
        end
        object cbProcessIcons: TCheckBox
          Left = 1
          Top = 177
          Width = 542
          Height = 21
          Caption = 'Get process icons for processlist'
          Checked = True
          State = cbChecked
          TabOrder = 15
          OnClick = cbProcessIconsClick
        end
        object cbProcessIconsOnly: TCheckBox
          Left = 20
          Top = 197
          Width = 237
          Height = 21
          Caption = 'Only show processes with an icon'
          TabOrder = 16
        end
        object cbShowMainMenu: TCheckBox
          Left = 1
          Top = 158
          Width = 601
          Height = 20
          Caption = 'Show main menu'
          Checked = True
          State = cbChecked
          TabOrder = 17
        end
        object cbOldPointerAddMethod: TCheckBox
          Left = 1
          Top = 98
          Width = 614
          Height = 21
          Caption = 'Pointer adding: Append pointerline instead of insert'
          Checked = True
          State = cbChecked
          TabOrder = 18
        end
      end
      object tsHotkeys: TTabSheet
        Caption = 'Hotkeys'
        ImageIndex = 7
        inline frameHotkeyConfig: TframeHotkeyConfig
          Left = 0
          Top = 0
          Width = 609
          Height = 446
          Align = alClient
          TabOrder = 0
          inherited Panel1: TPanel
            Top = 62
            Width = 383
            Height = 384
            inherited Label1: TLabel
              Width = 383
            end
            inherited ListBox1: TListBox
              Width = 383
              Height = 368
            end
          end
          inherited Panel2: TPanel
            Left = 383
            Top = 62
            Width = 226
            Height = 384
            DesignSize = (
              226
              384)
            inherited Label2: TLabel
              Left = 7
            end
            inherited Edit1: TEdit
              Left = 6
              Top = 20
              Width = 212
            end
            inherited Button3: TButton
              Left = 158
              Top = 49
              Width = 60
              Height = 21
            end
            inherited Panel3: TPanel
              Left = 10
              Top = 79
              Width = 209
              Height = 366
              DesignSize = (
                209
                366)
              inherited Label52: TLabel
                Left = 14
              end
              inherited Label51: TLabel
                Left = 74
              end
              inherited edtSHSpeed: TEdit
                Left = 5
                Top = 22
                Width = 57
              end
              inherited edtSHSleep: TEdit
                Left = 74
                Top = 22
                Width = 56
              end
            end
            inherited Panel4: TPanel
              Left = 10
              Top = 82
              Width = 213
              Height = 120
              inherited Label3: TLabel
                Left = 4
              end
              inherited Edit4: TEdit
                Top = 20
                Width = 205
              end
            end
          end
          inherited Panel5: TPanel
            Width = 609
            Height = 62
            inherited Label4: TLabel
              Left = 128
              Top = 7
            end
            inherited Label5: TLabel
              Left = 128
              Top = 39
            end
            inherited edtKeypollInterval: TEdit
              Width = 117
            end
            inherited edtHotkeyDelay: TEdit
              Top = 32
              Width = 117
            end
          end
        end
      end
      object Unrandomizer: TTabSheet
        Caption = 'Unrandomizer'
        ImageIndex = 8
        object Label5: TLabel
          Left = 10
          Top = 10
          Width = 114
          Height = 16
          Caption = 'Default return value'
        end
        object edtDefault: TEdit
          Left = 128
          Top = 7
          Width = 149
          Height = 24
          TabOrder = 0
          Text = '0'
        end
        object cbIncremental: TCheckBox
          Left = 11
          Top = 39
          Width = 129
          Height = 21
          Caption = 'Incremental value'
          TabOrder = 1
        end
      end
      object ScanSettings: TTabSheet
        Caption = 'Scan Settings'
        ImageIndex = 1
        object Label2: TLabel
          Left = 0
          Top = 246
          Width = 590
          Height = 32
          Caption = 
            'Running the scan in a seperate thread will give you a cancel but' +
            'ton, and prevents CE from starvation. (meaning parts of the wind' +
            'ow turn white) but it also makes scanning a little slower'
          Transparent = True
          WordWrap = True
        end
        object Label3: TLabel
          Left = 218
          Top = 309
          Width = 87
          Height = 16
          Caption = 'Thread priority'
        end
        object Label1: TLabel
          Left = 0
          Top = 7
          Width = 139
          Height = 16
          Caption = 'Size of scanbuffer (KB) :'
        end
        object Label15: TLabel
          Left = 233
          Top = 7
          Width = 169
          Height = 16
          Caption = '(Can effect scan speed a lot)'
        end
        object Label21: TLabel
          Left = 0
          Top = 158
          Width = 198
          Height = 16
          Caption = 'Scan the following memory types:'
        end
        object checkThread: TCheckBox
          Left = 208
          Top = 286
          Width = 198
          Height = 20
          Caption = 'Run scan in seperate thread'
          Checked = True
          State = cbChecked
          TabOrder = 0
          Visible = False
          OnClick = checkThreadClick
        end
        object combothreadpriority: TComboBox
          Left = 306
          Top = 305
          Width = 90
          Height = 24
          ItemHeight = 16
          ItemIndex = 4
          PopupMenu = MainForm.emptypopup
          TabOrder = 1
          Text = 'Higher'
          Items.Strings = (
            'Idle'
            'Lowest'
            'Lower'
            'Normal'
            'Higher'
            'Highest'
            'TimeCritical')
        end
        object cbFastscan: TCheckBox
          Left = 0
          Top = 39
          Width = 602
          Height = 21
          Caption = 'Fast scan on by default'
          TabOrder = 2
        end
        object cbSkip_PAGE_NOCACHE: TCheckBox
          Left = 0
          Top = 79
          Width = 602
          Height = 21
          Hint = 
            'Some systems crash when trying to read memory with this protecti' +
            'on. If that happens check this option.'
          Caption = 'Don'#39't scan memory that is protected with the No Cache option'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
        end
        object cbLowMemoryUsage: TCheckBox
          Left = 0
          Top = 98
          Width = 602
          Height = 21
          Caption = 
            'Keep low memory usage when doing an "Unkown Initial Value scan" ' +
            'with Hyper Scan'
          TabOrder = 4
        end
        object cbMemImage: TCheckBox
          Left = 0
          Top = 197
          Width = 602
          Height = 21
          Caption = 
            'MEM_IMAGE:Memory that is mapped into the view of an image sectio' +
            'n'
          Checked = True
          State = cbChecked
          TabOrder = 5
        end
        object cbMemMapped: TCheckBox
          Left = 0
          Top = 217
          Width = 602
          Height = 21
          Caption = 
            'MEM_MAPPED:Memory that is mapped into the view of a section. (E.' +
            'g:File mapping, slow)'
          TabOrder = 6
        end
        object cbMemPrivate: TCheckBox
          Left = 0
          Top = 177
          Width = 602
          Height = 21
          Caption = 'MEM_PRIVATE:Memory that is private.'
          Checked = True
          State = cbChecked
          TabOrder = 7
        end
        object cbEnableHyperscanWhenPossible: TCheckBox
          Left = 0
          Top = 59
          Width = 602
          Height = 21
          Caption = 'Enable Hyperscan when possible'
          TabOrder = 8
        end
        object EditBufsize: TEdit
          Left = 177
          Top = 2
          Width = 51
          Height = 24
          TabOrder = 9
          Text = '1024'
        end
      end
      object Plugins: TTabSheet
        Caption = 'Plugins'
        ImageIndex = 3
        object Panel7: TPanel
          Left = 512
          Top = 0
          Width = 97
          Height = 446
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 0
          object Button5: TButton
            Left = 5
            Top = 43
            Width = 92
            Height = 31
            Caption = 'Delete'
            TabOrder = 0
            OnClick = Button5Click
          end
          object Button4: TButton
            Left = 5
            Top = 4
            Width = 92
            Height = 30
            Caption = 'Add new'
            TabOrder = 1
            OnClick = Button4Click
          end
        end
        object Panel8: TPanel
          Left = 0
          Top = 0
          Width = 512
          Height = 446
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          object Label22: TLabel
            Left = 0
            Top = 0
            Width = 512
            Height = 16
            Align = alTop
            Caption = 'The following plugins are available:'
          end
          object clbPlugins: TCheckListBox
            Left = 0
            Top = 16
            Width = 512
            Height = 430
            Align = alClient
            ItemHeight = 16
            TabOrder = 0
          end
        end
      end
      object CodeFinder: TTabSheet
        Caption = 'CodeFinder'
        ImageIndex = 4
        object Label4: TLabel
          Left = 0
          Top = 0
          Width = 609
          Height = 48
          Align = alTop
          Caption = 
            'There are 2 ways Cheat Engine can find the addresss of code that' +
            ' writes to a specific address. Each type has it advantages and i' +
            't'#39's disadvantages. So choose which one suits you better. (or cho' +
            'ose the one that doesnt give you problems.)'
          WordWrap = True
        end
        object Label6: TLabel
          Left = 0
          Top = 79
          Width = 568
          Height = 16
          Caption = 
            'Advantage: Not as memory intensive as the "Write Exceptions" typ' +
            'e. And very high compatibility.'
        end
        object Label7: TLabel
          Left = 0
          Top = 97
          Width = 325
          Height = 16
          Caption = 'Disadvantage: May sometimes return a wrong address'
        end
        object Label8: TLabel
          Left = 0
          Top = 138
          Width = 416
          Height = 16
          Caption = 
            'Advantage: Finds every address that accesses the specified addre' +
            'ss.'
        end
        object Label9: TLabel
          Left = 0
          Top = 155
          Width = 595
          Height = 32
          Caption = 
            'Disadvantage: Memory intensive so slows down the game. And might' +
            ' cause stability problems in the game'
          WordWrap = True
        end
        object rbDebugRegisters: TRadioButton
          Left = 0
          Top = 59
          Width = 602
          Height = 21
          Caption = 'Use Debug Registers (aka Hardware Breakpoints)'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object rdWriteExceptions: TRadioButton
          Left = 0
          Top = 118
          Width = 602
          Height = 21
          Caption = 'Memory Access Exceptions'
          TabOrder = 1
        end
        object CheckBox1: TCheckBox
          Left = 0
          Top = 188
          Width = 602
          Height = 21
          Caption = 'Try to prevent detection of the debugger'
          TabOrder = 2
          OnClick = CheckBox1Click
        end
        object cbHandleBreakpoints: TCheckBox
          Left = 0
          Top = 208
          Width = 602
          Height = 21
          Caption = 'Handle beakpoints not caused by CE'
          TabOrder = 3
          OnClick = CheckBox1Click
        end
      end
      object Assembler: TTabSheet
        Caption = 'Assembler'
        ImageIndex = 5
        object cbShowDisassembler: TCheckBox
          Left = 1
          Top = 2
          Width = 601
          Height = 21
          Caption = 'Show disassembler'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = cbShowDisassemblerClick
        end
        object cbShowDebugoptions: TCheckBox
          Left = 20
          Top = 30
          Width = 582
          Height = 20
          Caption = 'Show debugger options'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object replacewithnops: TCheckBox
          Left = 20
          Top = 89
          Width = 582
          Height = 21
          Hint = 
            'If you type in a opcode and it is smaller than the opcode you re' +
            'placed, it will fill the missing bytes with NOP instructions. If' +
            ' the opcode is longer it will replace the opcode(s) that have be' +
            'en overwritten with NOP'#39's'
          Caption = 'Replace incomplete opcodes with nops'
          Checked = True
          ParentShowHint = False
          ShowHint = True
          State = cbChecked
          TabOrder = 2
          OnClick = replacewithnopsClick
        end
        object askforreplacewithnops: TCheckBox
          Left = 39
          Top = 108
          Width = 563
          Height = 21
          Caption = 'Ask for replace with nop'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
        object CheckBox2: TCheckBox
          Left = 0
          Top = 180
          Width = 602
          Height = 21
          Caption = 'Try to prevent detection of the debugger'
          TabOrder = 4
          OnClick = CheckBox2Click
        end
        object rbDebugAsBreakpoint: TRadioButton
          Left = 39
          Top = 49
          Width = 563
          Height = 21
          Caption = 'Use hardware breakpoints (Max 3)'
          Checked = True
          TabOrder = 5
          TabStop = True
        end
        object rbInt3AsBreakpoint: TRadioButton
          Left = 39
          Top = 69
          Width = 563
          Height = 21
          Caption = 'Use int3 instructions for breakpoints (Unlimited)'
          TabOrder = 6
        end
        object cbBreakOnAttach: TCheckBox
          Left = 20
          Top = 148
          Width = 582
          Height = 21
          Caption = 'Break when attaching/creating process using the process list'
          TabOrder = 7
          Visible = False
        end
      end
      object Extra: TTabSheet
        Caption = 'Extra'
        ImageIndex = 6
        object TauntOldOsUser: TLabel
          Left = 0
          Top = 430
          Width = 424
          Height = 16
          Align = alBottom
          Alignment = taCenter
          Caption = 
            'I really recommend upgrading to Windows 2000 or later mister Fli' +
            'ntstone'
          Visible = False
        end
        object GroupBox3: TGroupBox
          Left = 0
          Top = 0
          Width = 609
          Height = 86
          Align = alTop
          Caption = 
            'Use the following CE Kernel routines instead of the original win' +
            'dows version'
          TabOrder = 0
          object cbKernelQueryMemoryRegion: TCheckBox
            Left = 10
            Top = 20
            Width = 592
            Height = 21
            Caption = 'Query memory region routines'
            TabOrder = 0
            OnClick = cbKernelQueryMemoryRegionClick
          end
          object cbKernelReadWriteProcessMemory: TCheckBox
            Left = 10
            Top = 39
            Width = 592
            Height = 21
            Caption = 'Read/Write Process Memory  (Will cause slower scans)'
            TabOrder = 1
            OnClick = cbKernelQueryMemoryRegionClick
          end
          object cbKernelOpenProcess: TCheckBox
            Left = 10
            Top = 59
            Width = 592
            Height = 21
            Caption = 'Open Process'
            TabOrder = 2
          end
        end
        object cbProcessWatcher: TCheckBox
          Left = 1
          Top = 91
          Width = 593
          Height = 21
          Caption = 'Enable use of the Process Watcher'
          TabOrder = 1
          OnClick = cbProcessWatcherClick
        end
        object cbKdebug: TCheckBox
          Left = 1
          Top = 111
          Width = 593
          Height = 21
          Caption = 'Use kernelmode debugger options when possible'
          TabOrder = 2
          OnClick = cbKdebugClick
        end
        object CheckBox3: TCheckBox
          Left = 1
          Top = 296
          Width = 593
          Height = 21
          Caption = 'Use APC to inject dll'#39's'
          TabOrder = 3
          Visible = False
        end
        object CheckBox4: TCheckBox
          Left = 1
          Top = 316
          Width = 593
          Height = 21
          Caption = 'Use APC to create new threads'
          TabOrder = 4
          Visible = False
        end
        object cbGlobalDebug: TCheckBox
          Left = 22
          Top = 129
          Width = 572
          Height = 21
          Caption = 'Use Global Debug routines'
          Enabled = False
          TabOrder = 5
        end
      end
      object tsTools: TTabSheet
        Caption = 'tsTools'
        ImageIndex = 9
        object Panel2: TPanel
          Left = 0
          Top = 0
          Width = 609
          Height = 26
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object cbShowTools: TCheckBox
            Left = 0
            Top = 0
            Width = 484
            Height = 21
            Caption = 'Show '#39'tools'#39' menu item'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = cbShowToolsClick
          end
        end
        object Panel3: TPanel
          Left = 381
          Top = 26
          Width = 228
          Height = 420
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 1
          object lblApplicationTool: TLabel
            Left = 10
            Top = 49
            Width = 133
            Height = 16
            Caption = 'Application/Command'
            Enabled = False
          end
          object lblShortcut: TLabel
            Left = 10
            Top = 98
            Width = 51
            Height = 16
            Caption = 'Shortcut:'
            Enabled = False
          end
          object lblShortcutText: TLabel
            Left = 10
            Top = 118
            Width = 48
            Height = 16
            Caption = 'xxxxxxxx'
            Enabled = False
          end
          object lblToolsName: TLabel
            Left = 10
            Top = 0
            Width = 37
            Height = 16
            Caption = 'Name'
            Enabled = False
          end
          object OpenButton: TSpeedButton
            Left = 197
            Top = 69
            Width = 31
            Height = 26
            Enabled = False
            Glyph.Data = {
              D6020000424DD6020000000000003600000028000000100000000E0000000100
              180000000000A0020000C40E0000C40E00000000000000000000C0C0C0C0C0C0
              C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
              C0C0C0C0C0C0C0C0C0C000000000000000000000000000000000000000000000
              0000000000000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000000000
              008484008484008484008484008484008484008484008484008484000000C0C0
              C0C0C0C0C0C0C0C0C0C000000000FFFF00000000848400848400848400848400
              8484008484008484008484008484000000C0C0C0C0C0C0C0C0C0000000FFFFFF
              00FFFF0000000084840084840084840084840084840084840084840084840084
              84000000C0C0C0C0C0C000000000FFFFFFFFFF00FFFF00000000848400848400
              8484008484008484008484008484008484008484000000C0C0C0000000FFFFFF
              00FFFFFFFFFF00FFFF0000000000000000000000000000000000000000000000
              0000000000000000000000000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00
              FFFFFFFFFF00FFFF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000FFFFFF
              00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF000000C0C0C0C0C0
              C0C0C0C0C0C0C0C0C0C000000000FFFFFFFFFF00FFFF00000000000000000000
              0000000000000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000
              000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000
              00000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
              C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000000000C0C0C0C0C0C0C0C0C0
              C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000C0C0C0C0C0C0C0C0C00000
              00C0C0C0000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
              C0C0C0C0C0000000000000000000C0C0C0C0C0C0C0C0C0C0C0C0}
            ParentShowHint = False
            ShowHint = True
            OnClick = OpenButtonClick
          end
          object edtApplicationTool: TEdit
            Left = 10
            Top = 69
            Width = 186
            Height = 24
            Enabled = False
            MaxLength = 255
            TabOrder = 1
            OnChange = edtApplicationToolChange
          end
          object btnSetToolShortcut: TButton
            Left = 10
            Top = 138
            Width = 92
            Height = 21
            Caption = 'Set shortcut'
            Enabled = False
            TabOrder = 2
            OnClick = btnSetToolShortcutClick
          end
          object edtToolsName: TEdit
            Left = 10
            Top = 20
            Width = 218
            Height = 24
            Enabled = False
            MaxLength = 255
            TabOrder = 0
            OnChange = edtToolsNameChange
          end
        end
        object Panel5: TPanel
          Left = 0
          Top = 26
          Width = 381
          Height = 420
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 2
          object Panel4: TPanel
            Left = 0
            Top = 392
            Width = 381
            Height = 28
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 0
            object btnToolNew: TButton
              Left = 10
              Top = 4
              Width = 92
              Height = 21
              Caption = 'New'
              TabOrder = 0
              OnClick = btnToolNewClick
            end
            object btnToolDelete: TButton
              Left = 108
              Top = 4
              Width = 93
              Height = 21
              Caption = 'Delete'
              TabOrder = 1
              OnClick = btnToolDeleteClick
            end
          end
          object lvTools: TListView
            Left = 0
            Top = 0
            Width = 381
            Height = 392
            Align = alClient
            Columns = <
              item
                Caption = 'Name'
                Width = 74
              end
              item
                Caption = 'Application'
                Width = 234
              end
              item
                AutoSize = True
                Caption = 'Shortcut'
              end>
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 1
            ViewStyle = vsReport
            OnClick = lvToolsClick
          end
        end
      end
    end
  end
  object Panel6: TPanel
    Left = 0
    Top = 482
    Width = 767
    Height = 47
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = Panel6Resize
    DesignSize = (
      767
      47)
    object AboutLabel: TLabel
      Left = 866
      Top = 31
      Width = 56
      Height = 16
      Cursor = crHandPoint
      Anchors = [akRight, akBottom]
      Caption = 'About CE'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = AboutLabelClick
    end
    object Button2: TButton
      Left = 401
      Top = 6
      Width = 92
      Height = 31
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object Button1: TButton
      Left = 295
      Top = 6
      Width = 92
      Height = 31
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object defaultbuffer: TPopupMenu
    Left = 16
    Top = 264
    object Default1: TMenuItem
      Caption = 'Default'
      OnClick = Default1Click
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'DLL'
    Filter = 'Cheat Engine Plugins (*.dll)|*.dll'
    Left = 52
    Top = 264
  end
  object OpenDialog2: TOpenDialog
    DefaultExt = 'exe'
    Filter = 'Application (*.exe)|*.exe'
    Left = 3
    Top = 441
  end
end
