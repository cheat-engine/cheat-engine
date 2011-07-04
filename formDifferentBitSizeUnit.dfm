object formDifferentBitSize: TformDifferentBitSize
  Left = 192
  Top = 107
  Width = 329
  Height = 163
  VertScrollBar.Visible = False
  BorderIcons = [biSystemMenu]
  Caption = 'Different sized row of bits'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    321
    129)
  PixelsPerInch = 96
  TextHeight = 13
  object Labelold: TLabel
    Left = 120
    Top = 56
    Width = 72
    Height = 16
    Caption = 'LabelOld'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
  end
  object LabelNew: TLabel
    Left = 120
    Top = 72
    Width = 72
    Height = 16
    Caption = 'LabelNew'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 321
    Height = 41
    AutoSize = False
    Caption = 
      'The last time you scanned the number of bits was 90 and now it i' +
      's 12. Please tell me how and how much i must change the bit offs' +
      'et to succesfully scan. (Left arrow+right arrow move the bits)'
    WordWrap = True
  end
  object Edit1: TEdit
    Left = 136
    Top = 104
    Width = 33
    Height = 21
    Anchors = []
    PopupMenu = PopupMenu1
    TabOrder = 0
    Text = 'Edit1'
    OnKeyDown = Edit1KeyDown
    OnKeyPress = Edit1KeyPress
  end
  object Button1: TButton
    Left = 120
    Top = 102
    Width = 75
    Height = 25
    Anchors = []
    Caption = 'OK'
    Default = True
    TabOrder = 1
    TabStop = False
    OnClick = Button1Click
  end
  object PopupMenu1: TPopupMenu
    Left = 72
    Top = 80
    object OhnoYoufoundme1: TMenuItem
      Caption = 'Oh no! You found me!!!'
      OnClick = OhnoYoufoundme1Click
    end
  end
end
