object Layout: TLayout
  Left = 197
  Top = 133
  BorderStyle = bsSingle
  Caption = 'Skin settings'
  ClientHeight = 443
  ClientWidth = 474
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 313
    Height = 441
    Caption = 'Standard background picture'
    TabOrder = 0
    object Backgroundimage: TImage
      Left = 6
      Top = 17
      Width = 300
      Height = 328
      DragMode = dmAutomatic
      Stretch = True
      Visible = False
      OnEndDrag = BackgroundimageEndDrag
      OnStartDrag = BackgroundimageStartDrag
    end
    object Label1: TLabel
      Left = 6
      Top = 352
      Width = 296
      Height = 13
      Caption = 'You can drag colors from the image above to the text boxes on'
    end
    object Label2: TLabel
      Left = 6
      Top = 368
      Width = 126
      Height = 13
      Caption = 'the right side of this screen'
    end
    object Button6: TButton
      Left = 248
      Top = 408
      Width = 59
      Height = 25
      Caption = 'Clear'
      TabOrder = 0
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 128
      Top = 408
      Width = 57
      Height = 25
      Caption = 'Load'
      TabOrder = 1
      OnClick = Button7Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 320
    Top = 0
    Width = 153
    Height = 299
    Caption = 'Colors'
    TabOrder = 1
    object Processnamecolor: TLabel
      Left = 8
      Top = 16
      Width = 64
      Height = 13
      Caption = 'Processname'
    end
    object SpeedButton1: TSpeedButton
      Left = 88
      Top = 32
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
      OnClick = SpeedButton1Click
    end
    object Normaltextcolor: TLabel
      Left = 8
      Top = 56
      Width = 53
      Height = 13
      Caption = 'Normal text'
    end
    object SpeedButton2: TSpeedButton
      Left = 88
      Top = 72
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
    object Label4: TLabel
      Left = 8
      Top = 96
      Width = 84
      Height = 13
      Caption = 'Background color'
    end
    object SpeedButton4: TSpeedButton
      Left = 88
      Top = 112
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
      OnClick = SpeedButton4Click
    end
    object SpeedButton3: TSpeedButton
      Left = 88
      Top = 152
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
    object Label3: TLabel
      Left = 8
      Top = 136
      Width = 44
      Height = 13
      Caption = 'Box color'
    end
    object SpeedButton7: TSpeedButton
      Left = 88
      Top = 192
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
      OnClick = SpeedButton7Click
    end
    object Label7: TLabel
      Left = 8
      Top = 176
      Width = 66
      Height = 13
      Caption = 'Textfield color'
    end
    object SpeedButton8: TSpeedButton
      Left = 88
      Top = 232
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
      OnClick = SpeedButton8Click
    end
    object Label8: TLabel
      Left = 8
      Top = 216
      Width = 126
      Height = 13
      Caption = 'Textfield background color'
    end
    object SpeedButton5: TSpeedButton
      Left = 88
      Top = 272
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
      OnClick = SpeedButton8Click
    end
    object Label5: TLabel
      Left = 8
      Top = 256
      Width = 122
      Height = 13
      Caption = 'Last selected record color'
    end
    object Edit1: TEdit
      Left = 8
      Top = 32
      Width = 81
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 8
      TabOrder = 0
      Text = '000000'
      OnChange = Edit1Change
      OnDragOver = Edit1DragOver
      OnKeyPress = Edit1KeyPress
    end
    object Edit2: TEdit
      Left = 8
      Top = 72
      Width = 81
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 8
      TabOrder = 1
      Text = '000000'
      OnChange = Edit2Change
      OnDragOver = Edit1DragOver
      OnKeyPress = Edit1KeyPress
    end
    object Edit4: TEdit
      Left = 8
      Top = 112
      Width = 81
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 8
      TabOrder = 2
      Text = '000000'
      OnChange = Edit4Change
      OnDragOver = Edit1DragOver
      OnKeyPress = Edit1KeyPress
    end
    object Panel1: TPanel
      Left = 120
      Top = 32
      Width = 25
      Height = 25
      BevelOuter = bvNone
      TabOrder = 3
    end
    object Panel4: TPanel
      Left = 120
      Top = 112
      Width = 25
      Height = 25
      BevelOuter = bvNone
      TabOrder = 4
    end
    object Panel2: TPanel
      Left = 120
      Top = 72
      Width = 25
      Height = 25
      BevelOuter = bvNone
      TabOrder = 5
    end
    object Panel3: TPanel
      Left = 120
      Top = 152
      Width = 25
      Height = 25
      BevelOuter = bvNone
      TabOrder = 6
    end
    object Edit3: TEdit
      Left = 8
      Top = 152
      Width = 81
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 8
      TabOrder = 7
      Text = '000000'
      OnChange = Edit3Change
      OnDragOver = Edit1DragOver
      OnKeyPress = Edit1KeyPress
    end
    object Edit7: TEdit
      Left = 8
      Top = 192
      Width = 81
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 8
      TabOrder = 8
      Text = '000000'
      OnChange = Edit7Change
      OnDragOver = Edit1DragOver
      OnKeyPress = Edit1KeyPress
    end
    object Panel7: TPanel
      Left = 120
      Top = 192
      Width = 25
      Height = 25
      BevelOuter = bvNone
      TabOrder = 9
    end
    object Edit8: TEdit
      Left = 8
      Top = 232
      Width = 81
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 8
      TabOrder = 10
      Text = '000000'
      OnChange = Edit8Change
      OnDragOver = Edit1DragOver
      OnKeyPress = Edit1KeyPress
    end
    object Panel8: TPanel
      Left = 120
      Top = 232
      Width = 25
      Height = 25
      BevelOuter = bvNone
      TabOrder = 11
    end
    object Edit5: TEdit
      Left = 8
      Top = 272
      Width = 81
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 8
      TabOrder = 12
      Text = '000000'
      OnChange = Edit5Change
      OnDragOver = Edit1DragOver
      OnKeyPress = Edit1KeyPress
    end
    object Panel5: TPanel
      Left = 120
      Top = 272
      Width = 25
      Height = 25
      BevelOuter = bvNone
      TabOrder = 13
    end
  end
  object Button3: TButton
    Left = 320
    Top = 416
    Width = 73
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 400
    Top = 416
    Width = 73
    Height = 25
    Caption = 'OK'
    TabOrder = 3
    OnClick = Button4Click
  end
  object ShowHelp: TCheckBox
    Left = 320
    Top = 304
    Width = 89
    Height = 17
    Caption = 'Show help tab'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = ShowHelpClick
  end
  object Button1: TButton
    Left = 400
    Top = 384
    Width = 73
    Height = 25
    Caption = 'Apply'
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 400
    Top = 336
    Width = 73
    Height = 25
    Caption = 'Reset'
    TabOrder = 6
    OnClick = Button2Click
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 8
    Top = 16
  end
  object ColorDialog1: TColorDialog
    Color = clWhite
    Options = [cdFullOpen, cdAnyColor]
    Left = 40
    Top = 16
  end
  object MainMenu1: TMainMenu
    Left = 72
    Top = 16
    object Load1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Caption = 'Open...'
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = 'Save'
        OnClick = Save1Click
      end
      object Saveas1: TMenuItem
        Caption = 'Save as...'
        OnClick = Saveas1Click
      end
      object Saveasdefault1: TMenuItem
        Caption = 'Save as default'
        OnClick = Saveasdefault1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Caption = 'Close'
        OnClick = Close1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.CES'
    Filter = 'Cheat Engine skin|*.CES'
    Left = 8
    Top = 80
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.CES'
    Filter = 'Cheat Engine skin|*.CES'
    Title = 'Save - (Save as default.ces to make default.)'
    Left = 40
    Top = 80
  end
end
