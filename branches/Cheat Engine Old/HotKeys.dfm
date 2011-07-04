object HotKeyForm: THotKeyForm
  Left = 834
  Top = 345
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Set/Change hotkey'
  ClientHeight = 144
  ClientWidth = 334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    334
    144)
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 322
    Height = 16
    Caption = 'Type the key combination you want to set the hotkey to'
  end
  object Panel1: TPanel
    Left = 10
    Top = 49
    Width = 316
    Height = 61
    BevelOuter = bvNone
    TabOrder = 3
    object cbFreezedirection: TComboBox
      Left = 0
      Top = 0
      Width = 178
      Height = 24
      ItemHeight = 16
      TabOrder = 0
      Text = 'Set value to:'
      Items.Strings = (
        'Set value to:'
        'Decrease value with:'
        'Increase value with:')
    end
    object edtFreezeValue: TEdit
      Left = 0
      Top = 30
      Width = 316
      Height = 21
      TabOrder = 1
    end
  end
  object BitBtn1: TBitBtn
    Left = 89
    Top = 107
    Width = 80
    Height = 28
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object Button1: TButton
    Left = 177
    Top = 107
    Width = 80
    Height = 28
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edtHotkey: TEdit
    Left = 10
    Top = 20
    Width = 316
    Height = 21
    ReadOnly = True
    TabOrder = 2
    OnKeyDown = edtHotkeyKeyDown
  end
end
