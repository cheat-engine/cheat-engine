object frmMemoryModifier: TfrmMemoryModifier
  Left = 448
  Top = 295
  HelpContext = 14
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Trainer maker'
  ClientHeight = 506
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblWidthHeight: TLabel
    Left = 288
    Top = 26
    Width = 145
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '(80x310)'
  end
  object Icon: TImage
    Left = 287
    Top = 40
    Width = 32
    Height = 32
  end
  object Label4: TLabel
    Left = 288
    Top = 80
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object Label2: TLabel
    Left = 288
    Top = 120
    Width = 55
    Height = 13
    Caption = 'Launch file:'
  end
  object Label3: TLabel
    Left = 288
    Top = 160
    Width = 41
    Height = 13
    Caption = 'Process:'
  end
  object LoadButton: TSpeedButton
    Left = 418
    Top = 134
    Width = 25
    Height = 25
    Hint = 'Load'
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
    OnClick = LoadButtonClick
  end
  object Label5: TLabel
    Left = 288
    Top = 240
    Width = 146
    Height = 13
    Caption = 'Freeze Interval (in milliseconds)'
  end
  object Label6: TLabel
    Left = 288
    Top = 283
    Width = 51
    Height = 13
    Caption = 'About text:'
  end
  object Button1: TButton
    Left = 288
    Top = 471
    Width = 155
    Height = 25
    Caption = 'Generate trainer'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 288
    Top = 2
    Width = 155
    Height = 25
    Caption = 'Change Image'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 281
    Height = 506
    Align = alLeft
    TabOrder = 2
    DesignSize = (
      281
      506)
    object Label1: TLabel
      Left = 1
      Top = 1
      Width = 279
      Height = 13
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'List of items in the trainer:'
    end
    object spbUp: TSpeedButton
      Left = 259
      Top = 418
      Width = 17
      Height = 22
      Anchors = [akLeft, akBottom]
      Enabled = False
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
        3333333333777F33333333333309033333333333337F7F333333333333090333
        33333333337F7F33333333333309033333333333337F7F333333333333090333
        33333333337F7F33333333333309033333333333FF7F7FFFF333333000090000
        3333333777737777F333333099999990333333373F3333373333333309999903
        333333337F33337F33333333099999033333333373F333733333333330999033
        3333333337F337F3333333333099903333333333373F37333333333333090333
        33333333337F7F33333333333309033333333333337373333333333333303333
        333333333337F333333333333330333333333333333733333333}
      NumGlyphs = 2
      OnClick = spbUpClick
    end
    object spbDown: TSpeedButton
      Left = 259
      Top = 442
      Width = 17
      Height = 22
      Anchors = [akLeft, akBottom]
      Enabled = False
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
        333333333337F33333333333333033333333333333373F333333333333090333
        33333333337F7F33333333333309033333333333337373F33333333330999033
        3333333337F337F33333333330999033333333333733373F3333333309999903
        333333337F33337F33333333099999033333333373333373F333333099999990
        33333337FFFF3FF7F33333300009000033333337777F77773333333333090333
        33333333337F7F33333333333309033333333333337F7F333333333333090333
        33333333337F7F33333333333309033333333333337F7F333333333333090333
        33333333337F7F33333333333300033333333333337773333333}
      NumGlyphs = 2
      OnClick = spbDownClick
    end
    object recordview: TListView
      Left = 9
      Top = 16
      Width = 246
      Height = 451
      Anchors = [akLeft, akTop, akBottom]
      Columns = <
        item
          Caption = 'Description'
          Width = 120
        end
        item
          Caption = 'Hotkey'
          Width = 120
        end>
      ReadOnly = True
      RowSelect = True
      PopupMenu = PopupMenu1
      TabOrder = 0
      ViewStyle = vsReport
      OnClick = recordviewClick
      OnDblClick = recordviewDblClick
    end
    object Button4: TButton
      Left = 112
      Top = 474
      Width = 73
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Add Entry'
      Default = True
      TabOrder = 1
      OnClick = Button4Click
    end
    object Button6: TButton
      Left = 192
      Top = 474
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Delete Entry'
      TabOrder = 2
      OnClick = Button6Click
    end
  end
  object Button3: TButton
    Left = 288
    Top = 442
    Width = 155
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button5: TButton
    Left = 328
    Top = 44
    Width = 115
    Height = 25
    Caption = 'Change Icon'
    TabOrder = 4
    OnClick = Button5Click
  end
  object editTitle: TEdit
    Left = 288
    Top = 96
    Width = 155
    Height = 21
    TabOrder = 5
    OnChange = editTitleChange
  end
  object Edit2: TEdit
    Left = 288
    Top = 136
    Width = 123
    Height = 21
    Hint = 
      'This will be the file the trainer starts when you press launch. ' +
      '(in case the process isnt already running)'
    TabOrder = 6
    OnChange = Edit2Change
  end
  object ComboBox1: TComboBox
    Left = 288
    Top = 176
    Width = 155
    Height = 21
    Hint = 'Type here (or select) the process that this trainer will affect.'
    ItemHeight = 13
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    OnChange = ComboBox1Change
  end
  object CheckBox1: TCheckBox
    Left = 288
    Top = 200
    Width = 145
    Height = 17
    Caption = 'Popup trainer on keypress'
    TabOrder = 8
    OnClick = CheckBox1Click
  end
  object EditHotkey: TEdit
    Left = 288
    Top = 216
    Width = 121
    Height = 21
    Enabled = False
    TabOrder = 9
    OnKeyDown = EditHotkeyKeyDown
    OnKeyPress = EditHotkeyKeyPress
  end
  object Button7: TButton
    Left = 288
    Top = 384
    Width = 155
    Height = 25
    Hint = 'Think the default userinterface sucks? Then design your own!'
    Caption = 'Design own userinterface'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 10
    OnClick = Button7Click
  end
  object CheckBox2: TCheckBox
    Left = 348
    Top = 118
    Width = 97
    Height = 17
    Caption = 'and Autolaunch'
    TabOrder = 11
  end
  object Memo1: TMemo
    Left = 288
    Top = 298
    Width = 155
    Height = 81
    Hint = 'This will show up when you press the about box'
    Lines.Strings = (
      'This trainer was made by '
      'Cheat Engine'
      'www.cheatengine.org')
    ParentShowHint = False
    ScrollBars = ssVertical
    ShowHint = True
    TabOrder = 12
  end
  object editFreezeInterval: TEdit
    Left = 288
    Top = 256
    Width = 153
    Height = 21
    TabOrder = 13
    Text = '250'
  end
  object cbPreventReopening: TCheckBox
    Left = 288
    Top = 416
    Width = 113
    Height = 17
    Hint = 
      'Prevents CE from reopening and editing this trainer. (Only someo' +
      'ne with an IQ higher than that of a rock will be able to edit it' +
      ' now...)'
    Caption = 'Prevent re-opening'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 14
  end
  object Button8: TButton
    Left = 411
    Top = 218
    Width = 33
    Height = 17
    Caption = 'Clear'
    TabOrder = 15
    OnClick = Button8Click
  end
  object OpenPictureDialog1: TOpenPictureDialog
    DefaultExt = 'bmp'
    Filter = 'bmp (*.bmp)|*.bmp'
    Left = 288
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'EXE'
    Filter = 'Exe files|*.exe'
    Left = 392
    Top = 312
  end
  object PopupMenu1: TPopupMenu
    Left = 8
    Top = 21
    object Delete1: TMenuItem
      Caption = 'Delete'
      OnClick = Button6Click
    end
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 80
    Top = 136
  end
  object OpenDialog2: TOpenDialog
    Filter = 'Icon files|*.dll;*.exe;*.ico'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 296
    Top = 40
  end
  object OpenDialog3: TOpenDialog
    Filter = 'All executables|*.exe;*.pif;*.com;*.bat'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 
      'Please select the file you want to execute when the process hasn' +
      #39't been started'
    Left = 360
    Top = 136
  end
end
