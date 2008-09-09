object formSettings: TformSettings
  Left = 466
  Top = 221
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Cheat Engine settings'
  ClientHeight = 364
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    624
    364)
  PixelsPerInch = 96
  TextHeight = 13
  object AboutLabel: TLabel
    Left = 573
    Top = 349
    Width = 45
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'About CE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = AboutLabelClick
  end
  object Button1: TButton
    Left = 181
    Top = 328
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 275
    Top = 328
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object pnlConfig: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 321
    Align = alTop
    TabOrder = 2
    object tvMenuSelection: TTreeView
      Left = 1
      Top = 1
      Width = 121
      Height = 319
      Align = alLeft
      HideSelection = False
      Indent = 19
      ReadOnly = True
      ShowRoot = False
      TabOrder = 0
      OnChange = tvMenuSelectionChange
      Items.Data = {
        09000000290000000000000000000000FFFFFFFFFFFFFFFF0000000000000000
        1047656E6572616C2073657474696E6773200000000000000000000000FFFFFF
        FFFFFFFFFF000000000000000007486F746B6579732500000000000000000000
        00FFFFFFFFFFFFFFFF00000000000000000C556E72616E646F6D697A65722600
        00000000000000000000FFFFFFFFFFFFFFFF00000000000000000D5363616E20
        73657474696E6773200000000000000000000000FFFFFFFFFFFFFFFF00000000
        0000000007506C7567696E732A0000000000000000000000FFFFFFFFFFFFFFFF
        00000000000000001146696C65206173736F63696174696F6E73240000000000
        000000000000FFFFFFFFFFFFFFFF00000000000000000B436F64652046696E64
        65722A0000000000000000000000FFFFFFFFFFFFFFFF00000000000000001141
        7373656D626C6572206F7074696F6E731E0000000000000000000000FFFFFFFF
        FFFFFFFF0000000000000000054578747261}
    end
    object pcSetting: TPageControl
      Left = 122
      Top = 1
      Width = 501
      Height = 319
      ActivePage = Unrandomizer
      Align = alClient
      Style = tsFlatButtons
      TabOrder = 1
      object GeneralSettings: TTabSheet
        Caption = 'General Settings'
        object Label11: TLabel
          Left = 170
          Top = 249
          Width = 13
          Height = 13
          Caption = 'ms'
        end
        object Label12: TLabel
          Left = 170
          Top = 273
          Width = 13
          Height = 13
          Caption = 'ms'
        end
        object Label13: TLabel
          Left = 51
          Top = 249
          Width = 72
          Height = 13
          Caption = 'Update interval'
        end
        object Label14: TLabel
          Left = 54
          Top = 273
          Width = 69
          Height = 13
          Caption = 'Freeze interval'
        end
        object Label16: TLabel
          Left = 8
          Top = 249
          Width = 116
          Height = 13
          Caption = 'Network Update Interval'
          Visible = False
        end
        object Label17: TLabel
          Left = 178
          Top = 257
          Width = 13
          Height = 13
          Caption = 'ms'
          Visible = False
        end
        object Label18: TLabel
          Left = 208
          Top = 249
          Width = 158
          Height = 13
          Caption = 'Found address list update interval'
        end
        object Label19: TLabel
          Left = 413
          Top = 250
          Width = 13
          Height = 13
          Caption = 'ms'
        end
        object Label23: TLabel
          Left = 8
          Top = 208
          Width = 193
          Height = 13
          Caption = 'Automatically attach to processes named'
        end
        object Label24: TLabel
          Left = 328
          Top = 208
          Width = 114
          Height = 13
          Caption = '(Seperate entries with ; )'
        end
        object cbShowUndo: TCheckBox
          Left = 1
          Top = 2
          Width = 488
          Height = 17
          Caption = 'Show undo button'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object cbShowAdvanced: TCheckBox
          Left = 1
          Top = 18
          Width = 488
          Height = 17
          Caption = 'Show advanced options.'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object cbCenterOnPopup: TCheckBox
          Left = 1
          Top = 49
          Width = 488
          Height = 17
          Caption = 'Center Cheat Engine when bringing to front'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object EditUpdateInterval: TEdit
          Left = 127
          Top = 245
          Width = 43
          Height = 21
          Hint = 
            'The number of milliseconds that Cheat Engine will wait before re' +
            'freshing the list of addresses'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          Text = '500'
        end
        object EditFreezeInterval: TEdit
          Left = 127
          Top = 269
          Width = 43
          Height = 21
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
          Top = 113
          Width = 489
          Height = 70
          Caption = 'Address list specific'
          TabOrder = 5
          object cbShowAsSigned: TCheckBox
            Left = 16
            Top = 16
            Width = 441
            Height = 17
            Caption = 'Show values as if they are signed'
            TabOrder = 0
          end
          object cbBinariesAsDecimal: TCheckBox
            Left = 16
            Top = 32
            Width = 441
            Height = 17
            Caption = 'Show and work with binaries as if they are decimals'
            TabOrder = 1
          end
          object cbsimplecopypaste: TCheckBox
            Left = 16
            Top = 48
            Width = 441
            Height = 17
            Caption = 'Simple paste'
            TabOrder = 2
          end
        end
        object EditNetworkUpdateInterval: TEdit
          Left = 135
          Top = 253
          Width = 43
          Height = 21
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
          Top = 33
          Width = 488
          Height = 17
          Caption = 'Update the list of found addresses even after scanning'
          Checked = True
          State = cbChecked
          TabOrder = 7
          OnClick = cbUpdatefoundListClick
        end
        object editUpdatefoundInterval: TEdit
          Left = 370
          Top = 246
          Width = 41
          Height = 21
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
          Top = 64
          Width = 348
          Height = 17
          Caption = 
            'Hide some/all windows instead of trying to bring cheat engine to' +
            ' front'
          Checked = True
          State = cbChecked
          TabOrder = 9
          OnClick = cbHideAllWindowsClick
        end
        object btnExcludeProcesses: TButton
          Left = 352
          Top = 64
          Width = 73
          Height = 17
          Caption = 'More...'
          TabOrder = 10
          OnClick = btnExcludeProcessesClick
        end
        object EditAutoAttach: TEdit
          Left = 204
          Top = 204
          Width = 121
          Height = 21
          Hint = 
            'Type the name of the process you want to automatically open. Not' +
            'e: Only works when NO process has been opened yet'
          TabOrder = 11
        end
        object cbAlwaysAutoAttach: TCheckBox
          Left = 8
          Top = 226
          Width = 481
          Height = 17
          Caption = 'Even autoattach when another process has already been selected'
          TabOrder = 12
        end
        object cbSaveWindowPos: TCheckBox
          Left = 1
          Top = 96
          Width = 488
          Height = 17
          Caption = 'Save window positions'
          TabOrder = 13
        end
        object cbOldSpeedhack: TCheckBox
          Left = 1
          Top = 80
          Width = 488
          Height = 17
          Caption = 'Use old speedhack'
          TabOrder = 14
        end
      end
      object TabSheet16: TTabSheet
        Caption = 'Hotkeys'
        ImageIndex = 7
        object Button3: TButton
          Left = 0
          Top = 2
          Width = 201
          Height = 17
          Caption = 'Configure Hotkeys'
          TabOrder = 0
          OnClick = Button3Click
        end
      end
      object Unrandomizer: TTabSheet
        Caption = 'Unrandomizer'
        ImageIndex = 8
        object Label5: TLabel
          Left = 8
          Top = 8
          Width = 93
          Height = 13
          Caption = 'Default return value'
        end
        object edtDefault: TEdit
          Left = 104
          Top = 6
          Width = 121
          Height = 21
          TabOrder = 0
          Text = '0'
        end
        object cbIncremental: TCheckBox
          Left = 9
          Top = 32
          Width = 105
          Height = 17
          Caption = 'Incremental value'
          TabOrder = 1
        end
      end
      object ScanSettings: TTabSheet
        Caption = 'Scan Settings'
        ImageIndex = 1
        object Label2: TLabel
          Left = 0
          Top = 200
          Width = 481
          Height = 26
          Caption = 
            'Running the scan in a seperate thread will give you a cancel but' +
            'ton, and prevents CE from starvation. (meaning parts of the wind' +
            'ow turn white) but it also makes scanning a little slower'
          Transparent = True
          WordWrap = True
        end
        object Label3: TLabel
          Left = 177
          Top = 251
          Width = 67
          Height = 13
          Caption = 'Thread priority'
        end
        object Label1: TLabel
          Left = 0
          Top = 6
          Width = 114
          Height = 13
          Caption = 'Size of scanbuffer (KB) :'
        end
        object Label15: TLabel
          Left = 189
          Top = 6
          Width = 136
          Height = 13
          Caption = '(Can effect scan speed a lot)'
        end
        object Label21: TLabel
          Left = 0
          Top = 128
          Width = 157
          Height = 13
          Caption = 'Scan the following memory types:'
        end
        object checkThread: TCheckBox
          Left = 169
          Top = 232
          Width = 161
          Height = 17
          Caption = 'Run scan in seperate thread'
          Checked = True
          State = cbChecked
          TabOrder = 0
          Visible = False
          OnClick = checkThreadClick
        end
        object combothreadpriority: TComboBox
          Left = 249
          Top = 248
          Width = 73
          Height = 21
          ItemHeight = 13
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
          Top = 32
          Width = 489
          Height = 17
          Caption = 'Fast scan on by default'
          TabOrder = 2
        end
        object cbSkip_PAGE_NOCACHE: TCheckBox
          Left = 0
          Top = 64
          Width = 489
          Height = 17
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
          Top = 80
          Width = 489
          Height = 17
          Caption = 
            'Keep low memory usage when doing an "Unkown Initial Value scan" ' +
            'with Hyper Scan'
          TabOrder = 4
        end
        object cbMemImage: TCheckBox
          Left = 0
          Top = 160
          Width = 489
          Height = 17
          Caption = 
            'MEM_IMAGE:Memory that is mapped into the view of an image sectio' +
            'n'
          Checked = True
          State = cbChecked
          TabOrder = 5
        end
        object cbMemMapped: TCheckBox
          Left = 0
          Top = 176
          Width = 489
          Height = 17
          Caption = 
            'MEM_MAPPED:Memory that is mapped into the view of a section. (E.' +
            'g:File mapping, slow)'
          TabOrder = 6
        end
        object cbMemPrivate: TCheckBox
          Left = 0
          Top = 144
          Width = 489
          Height = 17
          Caption = 'MEM_PRIVATE:Memory that is private.'
          Checked = True
          State = cbChecked
          TabOrder = 7
        end
        object cbEnableHyperscanWhenPossible: TCheckBox
          Left = 0
          Top = 48
          Width = 489
          Height = 17
          Caption = 'Enable Hyperscan when possible'
          TabOrder = 8
        end
        object EditBufsize: TEdit
          Left = 144
          Top = 2
          Width = 41
          Height = 21
          TabOrder = 9
          Text = '1024'
        end
      end
      object Plugins: TTabSheet
        Caption = 'Plugins'
        ImageIndex = 3
        object Label22: TLabel
          Left = 0
          Top = 0
          Width = 165
          Height = 13
          Caption = 'The following plugins are available:'
        end
        object Button4: TButton
          Left = 416
          Top = 16
          Width = 75
          Height = 25
          Caption = 'Add new'
          TabOrder = 0
          OnClick = Button4Click
        end
        object Button5: TButton
          Left = 416
          Top = 48
          Width = 75
          Height = 25
          Caption = 'Delete'
          TabOrder = 1
          OnClick = Button5Click
        end
        object clbPlugins: TCheckListBox
          Left = 0
          Top = 16
          Width = 409
          Height = 273
          ItemHeight = 13
          TabOrder = 2
        end
      end
      object FileAssocs: TTabSheet
        Caption = 'FileAssocs'
        ImageIndex = 2
        object Label10: TLabel
          Left = 7
          Top = 8
          Width = 306
          Height = 13
          Caption = 'Select the file extensions you want associated with Cheat Engine'
        end
        object cbGH: TCheckBox
          Left = 5
          Top = 90
          Width = 484
          Height = 17
          Caption = '.GH (Gamehack tables)'
          TabOrder = 0
        end
        object cbCT3: TCheckBox
          Left = 5
          Top = 74
          Width = 484
          Height = 17
          Caption = '.CT3 (Cheat Engine Table 3th version)'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object cbCT2: TCheckBox
          Left = 5
          Top = 58
          Width = 484
          Height = 17
          Caption = '.CT2 (Cheat Engine Table 2nd version)'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object cbCET: TCheckBox
          Left = 5
          Top = 42
          Width = 484
          Height = 17
          Caption = '.CET (Cheat Engine Table first version)'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
        object cbCT: TCheckBox
          Left = 5
          Top = 26
          Width = 484
          Height = 17
          Caption = '.CT (Standard Cheat Table)'
          Checked = True
          State = cbChecked
          TabOrder = 4
        end
      end
      object CodeFinder: TTabSheet
        Caption = 'CodeFinder'
        ImageIndex = 4
        object Label4: TLabel
          Left = 0
          Top = 0
          Width = 477
          Height = 39
          Caption = 
            'There are 2 ways Cheat Engine can find the addresss of code that' +
            ' writes to a specific address. Each type has it advantages and i' +
            't'#39's disadvantages. So choose which one suits you better. (or cho' +
            'ose the one that doesnt give you problems.)'
          WordWrap = True
        end
        object Label6: TLabel
          Left = 0
          Top = 64
          Width = 455
          Height = 13
          Caption = 
            'Advantage: Not as memory intensive as the "Write Exceptions" typ' +
            'e. And very high compatibility.'
        end
        object Label7: TLabel
          Left = 0
          Top = 79
          Width = 255
          Height = 13
          Caption = 'Disadvantage: May sometimes return a wrong address'
        end
        object Label8: TLabel
          Left = 0
          Top = 112
          Width = 327
          Height = 13
          Caption = 
            'Advantage: Finds every address that accesses the specified addre' +
            'ss.'
        end
        object Label9: TLabel
          Left = 0
          Top = 126
          Width = 276
          Height = 13
          Caption = 'Disadvantage: Memory intensive so slows down the game.'
          WordWrap = True
        end
        object rbDebugRegisters: TRadioButton
          Left = 0
          Top = 48
          Width = 489
          Height = 17
          Caption = 'Use Debug Registers (aka Hardware Breakpoints)'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object rdWriteExceptions: TRadioButton
          Left = 0
          Top = 96
          Width = 489
          Height = 17
          Caption = 'Memory Access Exceptions'
          TabOrder = 1
        end
        object CheckBox1: TCheckBox
          Left = 0
          Top = 146
          Width = 489
          Height = 17
          Caption = 'Try to prevent detection of the debugger'
          TabOrder = 2
          OnClick = CheckBox1Click
        end
        object cbHandleBreakpoints: TCheckBox
          Left = 0
          Top = 161
          Width = 489
          Height = 17
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
          Width = 488
          Height = 17
          Caption = 'Show disassembler'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = cbShowDisassemblerClick
        end
        object cbShowDebugoptions: TCheckBox
          Left = 16
          Top = 24
          Width = 473
          Height = 17
          Caption = 'Show debugger options'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object replacewithnops: TCheckBox
          Left = 16
          Top = 72
          Width = 473
          Height = 17
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
          Left = 32
          Top = 88
          Width = 457
          Height = 17
          Caption = 'Ask for replace with nop'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
        object CheckBox2: TCheckBox
          Left = 0
          Top = 146
          Width = 489
          Height = 17
          Caption = 'Try to prevent detection of the debugger'
          TabOrder = 4
          OnClick = CheckBox2Click
        end
        object rbDebugAsBreakpoint: TRadioButton
          Left = 32
          Top = 40
          Width = 457
          Height = 17
          Caption = 'Use hardware breakpoints (Max 3)'
          Checked = True
          TabOrder = 5
          TabStop = True
        end
        object rbInt3AsBreakpoint: TRadioButton
          Left = 32
          Top = 56
          Width = 457
          Height = 17
          Caption = 'Use int3 instructions for breakpoints (Unlimited)'
          TabOrder = 6
        end
        object cbBreakOnAttach: TCheckBox
          Left = 16
          Top = 120
          Width = 473
          Height = 17
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
          Top = 275
          Width = 337
          Height = 13
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
          Width = 493
          Height = 70
          Align = alTop
          Caption = 
            'Use the following CE Kernel routines instead of the original win' +
            'dows version'
          TabOrder = 0
          object cbKernelQueryMemoryRegion: TCheckBox
            Left = 8
            Top = 16
            Width = 481
            Height = 17
            Caption = 'Query memory region routines'
            TabOrder = 0
            OnClick = cbKernelQueryMemoryRegionClick
          end
          object cbKernelReadWriteProcessMemory: TCheckBox
            Left = 8
            Top = 32
            Width = 481
            Height = 17
            Caption = 'Read/Write Process Memory  (Will cause slower scans)'
            TabOrder = 1
            OnClick = cbKernelQueryMemoryRegionClick
          end
          object cbKernelOpenProcess: TCheckBox
            Left = 8
            Top = 48
            Width = 481
            Height = 17
            Caption = 'Open Process'
            TabOrder = 2
          end
        end
        object Panel1: TPanel
          Left = 0
          Top = 192
          Width = 489
          Height = 89
          BevelOuter = bvLowered
          TabOrder = 1
          object Label25: TLabel
            Left = 2
            Top = 48
            Width = 478
            Height = 39
            Caption = 
              'Warning: If the system keeps crashing each time you run Cheat En' +
              'gine making it impossible to turn these options off then run Che' +
              'at Engine with the /NOSTEALTH parameter, or rename dbk32.sys and' +
              ' stealth.dll to something else'
            WordWrap = True
          end
          object cbStealth: TCheckBox
            Left = 3
            Top = 8
            Width = 486
            Height = 17
            Caption = 'Stealth mode (Usermode)'
            TabOrder = 0
          end
          object cbProtectMe: TCheckBox
            Left = 3
            Top = 24
            Width = 486
            Height = 17
            Caption = 'Stealth mode (Kernelmode)'
            TabOrder = 1
            OnClick = cbProtectMeClick
          end
          object btnMoreStealth: TButton
            Left = 152
            Top = 24
            Width = 75
            Height = 17
            Caption = 'More...'
            Enabled = False
            TabOrder = 2
            OnClick = btnMoreStealthClick
          end
        end
        object cbUndoMemoryChanges: TCheckBox
          Left = 7
          Top = 72
          Width = 482
          Height = 17
          Hint = 
            'This will check some often used memory locations of and undo any' +
            ' changes if it finds them'
          Caption = 'Undo changes to CE'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnClick = cbUndoMemoryChangesClick
        end
        object cbForceUndo: TCheckBox
          Left = 7
          Top = 87
          Width = 482
          Height = 17
          Hint = 
            'If by some means that normal way of changing the protection of m' +
            'emory isn'#39't working then change the protection from the kernelsi' +
            'de. (this requires the kernel mode routines)'
          Caption = 
            'Force memory to be writable in case the standard method is block' +
            'ed'
          Enabled = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
        end
        object cbProcessWatcher: TCheckBox
          Left = 7
          Top = 139
          Width = 482
          Height = 17
          Caption = 'Enable use of the Process Watcher'
          TabOrder = 4
          OnClick = cbProcessWatcherClick
        end
        object cbKdebug: TCheckBox
          Left = 7
          Top = 155
          Width = 482
          Height = 17
          Caption = 'Use kernelmode debugger options when possible'
          TabOrder = 5
          OnClick = cbKdebugClick
        end
        object CheckBox3: TCheckBox
          Left = 7
          Top = 104
          Width = 482
          Height = 17
          Caption = 'Use APC to inject dll'#39's'
          TabOrder = 6
          Visible = False
        end
        object CheckBox4: TCheckBox
          Left = 7
          Top = 120
          Width = 482
          Height = 17
          Caption = 'Use APC to create new threads'
          TabOrder = 7
          Visible = False
        end
        object cbGlobalDebug: TCheckBox
          Left = 24
          Top = 170
          Width = 465
          Height = 17
          Caption = 'Use Global Debug routines'
          Enabled = False
          TabOrder = 8
        end
      end
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
end
