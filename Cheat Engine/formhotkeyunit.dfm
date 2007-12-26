object FormHotkey: TFormHotkey
  Left = 942
  Top = 155
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Change hotkey'
  ClientHeight = 69
  ClientWidth = 217
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 207
    Height = 13
    Caption = 'Press the new hotkey combination in below:'
  end
  object Edit1: TEdit
    Left = 8
    Top = 16
    Width = 201
    Height = 21
    TabOrder = 0
    OnKeyDown = Edit1KeyDown
  end
  object Button1: TButton
    Left = 24
    Top = 40
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 120
    Top = 40
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
  end
end
