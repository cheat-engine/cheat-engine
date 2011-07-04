object Standalone: TStandalone
  Left = 192
  Top = 107
  BorderStyle = bsSingle
  Caption = 'Standalone'
  ClientHeight = 333
  ClientWidth = 474
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 82
    Height = 13
    Caption = 'Addresses to add'
  end
  object Label2: TLabel
    Left = 216
    Top = 24
    Width = 92
    Height = 13
    Caption = 'Addresses in trainer'
  end
  object ImageSize: TLabel
    Left = 336
    Top = 296
    Width = 35
    Height = 13
    Caption = '(50x50)'
    Visible = False
  end
  object Label3: TLabel
    Left = 336
    Top = 80
    Width = 120
    Height = 13
    Caption = 'Backgroundcolor window'
  end
  object Label4: TLabel
    Left = 336
    Top = 120
    Width = 93
    Height = 13
    Caption = 'Backgoundcolor list'
  end
  object SpeedButton1: TSpeedButton
    Left = 448
    Top = 272
    Width = 23
    Height = 22
    Glyph.Data = {
      8A000000424D8A000000000000003E0000002800000013000000130000000100
      0100000000004C0000000000000000000000020000000000000000000000FFFF
      FF00FFFFE000FFFFE000FFFFE000FFFFE000FFFFE000FFFFE000FFFFE000FFFF
      E000F7BDE000E318E000F7BDE000FFFFE000FFFFE000FFFFE000FFFFE000FFFF
      E000FFFFE000FFFFE000FFFFE000}
    Visible = False
    OnClick = SpeedButton1Click
  end
  object SpeedButton5: TSpeedButton
    Left = 424
    Top = 95
    Width = 23
    Height = 22
    Glyph.Data = {
      EE000000424DEE0000000000000076000000280000000F0000000F0000000100
      04000000000078000000C40E0000C40E00001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
      5550500000000000005050AAA0EEE0DDD05050AAA0EEE0DDD05050AAA0EEE0DD
      D0505000000000000050508880FFF0000050508880FFF0000050508880FFF000
      00505000000000000050509990BBB0CCC050509990BBB0CCC050509990BBB0CC
      C05050000000000000505555555555555550}
    OnClick = SpeedButton5Click
  end
  object SpeedButton2: TSpeedButton
    Left = 424
    Top = 135
    Width = 23
    Height = 22
    Glyph.Data = {
      EE000000424DEE0000000000000076000000280000000F0000000F0000000100
      04000000000078000000C40E0000C40E00001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
      5550500000000000005050AAA0EEE0DDD05050AAA0EEE0DDD05050AAA0EEE0DD
      D0505000000000000050508880FFF0000050508880FFF0000050508880FFF000
      00505000000000000050509990BBB0CCC050509990BBB0CCC050509990BBB0CC
      C05050000000000000505555555555555550}
    OnClick = SpeedButton2Click
  end
  object Label5: TLabel
    Left = 336
    Top = 160
    Width = 44
    Height = 13
    Caption = 'Textcolor'
  end
  object SpeedButton3: TSpeedButton
    Left = 424
    Top = 175
    Width = 23
    Height = 22
    Glyph.Data = {
      EE000000424DEE0000000000000076000000280000000F0000000F0000000100
      04000000000078000000C40E0000C40E00001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
      5550500000000000005050AAA0EEE0DDD05050AAA0EEE0DDD05050AAA0EEE0DD
      D0505000000000000050508880FFF0000050508880FFF0000050508880FFF000
      00505000000000000050509990BBB0CCC050509990BBB0CCC050509990BBB0CC
      C05050000000000000505555555555555550}
    OnClick = SpeedButton3Click
  end
  object Image1: TImage
    Left = 336
    Top = 200
    Width = 32
    Height = 32
    Picture.Data = {
      055449636F6E0000010001002020100000000000E80200001600000028000000
      2000000040000000010004000000000080020000000000000000000000000000
      0000000000000000000080000080000000808000800000008000800080800000
      80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
      FFFFFF000000000000000000330077000000000000000000000000003B077070
      000000000000000000000000BB807007000000000000000000000300B0007000
      70000000000000000000330070070700070000000000000000003B0700700070
      00700000000000000000BB800700000700070000000000000300B00070000000
      7000700000000000330070070000000007000700000000003B07007000000000
      0070070000000000BB800700000000000007070000000300B000700000000070
      000077000000330070070000000007000000803300003B070070000000000000
      000800330000BB8007000000000000000080BBBB0300B0007000000000700000
      08000BB0330070070000000707000000803300003B0700700000007070000008
      00330000BB8007000000070700000080BBBB0000B00070000000007000000800
      0BB0000070070000000007000000803300000000707000007770000000080033
      0000000087000007070700000080BBBB00000000080000077777000008000BB0
      0000000000800007070700008033000000000000000800007770000800330000
      000000000000800000000080BBBB00000000000000000800000008000BB00000
      0000000000000080000080330000000000000000000000080008003300000000
      00000000000000008080BBBB00000000000000000000000008000BB000000000
      00000000FFFF33FFFFFF21FFFFFF00FFFFFB007FFFF3003FFFF2001FFFF0000F
      FFB00007FF300003FF200003FF000003FB000003F3000000F2000000F0000010
      B00000393000000F2000000F0000010F0000039F000000FF000000FF000010FF
      800039FFC0000FFFE0000FFFF0010FFFF8039FFFFC00FFFFFE00FFFFFF10FFFF
      FFB9FFFF}
    Transparent = True
  end
  object Label6: TLabel
    Left = 336
    Top = 232
    Width = 81
    Height = 13
    Caption = '(32x32 16 colors)'
  end
  object Button1: TButton
    Left = 0
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Create trainer'
    TabOrder = 0
    OnClick = Button1Click
  end
  object CEList: TListBox
    Left = 0
    Top = 40
    Width = 137
    Height = 169
    ItemHeight = 13
    PopupMenu = CEListHelp
    TabOrder = 1
    OnDblClick = CEListDblClick
  end
  object Button2: TButton
    Left = 152
    Top = 40
    Width = 25
    Height = 25
    Caption = '<'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 152
    Top = 72
    Width = 25
    Height = 25
    Caption = '>'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 152
    Top = 104
    Width = 25
    Height = 25
    Caption = '<<'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 152
    Top = 136
    Width = 25
    Height = 25
    Caption = '>>'
    TabOrder = 5
    OnClick = Button5Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 216
    Width = 329
    Height = 81
    Lines.Strings = (
      'Type your comments/info here')
    MaxLength = 100
    ScrollBars = ssVertical
    TabOrder = 6
  end
  object Edit1: TEdit
    Left = 104
    Top = 0
    Width = 121
    Height = 21
    MaxLength = 50
    TabOrder = 7
    Text = 'Trainer title'
  end
  object Button6: TButton
    Left = 400
    Top = 312
    Width = 73
    Height = 17
    Caption = 'Preview'
    TabOrder = 8
    OnClick = Button6Click
  end
  object TRlist: TCheckListBox
    Left = 192
    Top = 40
    Width = 137
    Height = 169
    Hint = 'This shows the addresses that will be added to the trainer'
    OnClickCheck = TRlistClickCheck
    ItemHeight = 13
    ParentShowHint = False
    PopupMenu = TRpopuphelp
    ShowHint = True
    TabOrder = 9
    OnDblClick = TRlistDblClick
  end
  object GroupBox1: TGroupBox
    Left = 336
    Top = 0
    Width = 137
    Height = 73
    Caption = 'Trainer layout'
    TabOrder = 10
    object TrainerStyle1: TRadioButton
      Left = 40
      Top = 16
      Width = 57
      Height = 17
      Caption = 'Style 1'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = TrainerStyle1Click
    end
    object TrainerStyle2: TRadioButton
      Left = 40
      Top = 32
      Width = 57
      Height = 17
      Caption = 'Style 2'
      TabOrder = 1
      OnClick = TrainerStyle2Click
    end
    object TrainerStyle3: TRadioButton
      Left = 40
      Top = 48
      Width = 57
      Height = 17
      Caption = 'Style 3'
      TabOrder = 2
      OnClick = TrainerStyle3Click
    end
  end
  object CheckBox1: TCheckBox
    Left = 336
    Top = 256
    Width = 105
    Height = 17
    Caption = 'Use picture/logo'
    TabOrder = 11
    OnClick = CheckBox1Click
  end
  object Edit2: TEdit
    Left = 336
    Top = 272
    Width = 113
    Height = 21
    TabOrder = 12
    Visible = False
  end
  object Edit3: TEdit
    Left = 336
    Top = 136
    Width = 89
    Height = 21
    TabOrder = 13
    Text = 'Edit3'
    OnChange = Edit3Change
    OnKeyPress = Edit4KeyPress
  end
  object Edit4: TEdit
    Left = 336
    Top = 96
    Width = 89
    Height = 21
    TabOrder = 14
    Text = 'Edit4'
    OnChange = Edit4Change
    OnKeyPress = Edit4KeyPress
  end
  object Edit5: TEdit
    Left = 336
    Top = 176
    Width = 89
    Height = 21
    TabOrder = 15
    Text = 'Edit3'
    OnChange = Edit5Change
    OnKeyPress = Edit4KeyPress
  end
  object Panel1: TPanel
    Left = 448
    Top = 95
    Width = 25
    Height = 22
    BevelOuter = bvNone
    TabOrder = 16
  end
  object Panel2: TPanel
    Left = 448
    Top = 135
    Width = 25
    Height = 22
    BevelOuter = bvNone
    TabOrder = 17
  end
  object Panel3: TPanel
    Left = 448
    Top = 175
    Width = 25
    Height = 22
    BevelOuter = bvNone
    TabOrder = 18
  end
  object Button7: TButton
    Left = 376
    Top = 202
    Width = 73
    Height = 25
    Hint = 
      'This will let you extract the icon of a file you specify to an .' +
      'ICO file.'
    Caption = 'Set Icon'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 19
    OnClick = Button7Click
  end
  object Combobox1: TEdit
    Left = 208
    Top = 304
    Width = 121
    Height = 21
    TabOrder = 20
    Text = 'exefile.exe'
  end
  object TRpopuphelp: TPopupMenu
    Left = 232
    Top = 96
    object Help1: TMenuItem
      Caption = 'Help'
      OnClick = Help1Click
    end
  end
  object CEListHelp: TPopupMenu
    Left = 48
    Top = 80
    object MenuItem1: TMenuItem
      Caption = 'Help'
      OnClick = MenuItem1Click
    end
  end
  object ColorDialog1: TColorDialog
    Ctl3D = True
    Options = [cdFullOpen]
    Left = 136
    Top = 168
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 168
    Top = 168
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'Icon files|*.ICO|Exe files|*.EXE|All Files with icons|*.ICO;*.EX' +
      'E'
    FilterIndex = 3
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 448
    Top = 200
  end
end
