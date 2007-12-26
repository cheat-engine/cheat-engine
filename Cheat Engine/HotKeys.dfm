object HotKeyForm: THotKeyForm
  Left = 465
  Top = 374
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Set/Change hotkey'
  ClientHeight = 123
  ClientWidth = 271
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    271
    123)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 262
    Height = 13
    Caption = 'Type the key combination you want to set the hotkey to'
  end
  object Panel1: TPanel
    Left = 8
    Top = 40
    Width = 257
    Height = 49
    BevelOuter = bvNone
    TabOrder = 3
    object cbFreezedirection: TComboBox
      Left = 0
      Top = 0
      Width = 145
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      Text = 'Set value to:'
      Items.Strings = (
        'Set value to:'
        'Decrease value with:'
        'Increase value with:')
    end
    object edtFreezeValue: TEdit
      Left = 0
      Top = 24
      Width = 257
      Height = 21
      TabOrder = 1
    end
  end
  object BitBtn1: TBitBtn
    Left = 72
    Top = 93
    Width = 65
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object Button1: TButton
    Left = 144
    Top = 93
    Width = 65
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edtHotkey: TEdit
    Left = 8
    Top = 16
    Width = 257
    Height = 21
    ReadOnly = True
    TabOrder = 2
    OnKeyDown = edtHotkeyKeyDown
  end
end
