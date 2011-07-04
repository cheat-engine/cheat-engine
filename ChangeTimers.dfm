object ChangeTimerForm: TChangeTimerForm
  Left = 602
  Top = 341
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Change XXX Timer Interval'
  ClientHeight = 89
  ClientWidth = 203
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
    Top = 8
    Width = 202
    Height = 13
    Caption = 'Fill in the new interval. (in ms.) 0=Stop timer'
  end
  object interval: TEdit
    Left = 24
    Top = 24
    Width = 153
    Height = 21
    TabOrder = 0
    Text = 'interval'
  end
  object Button1: TButton
    Left = 24
    Top = 56
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 104
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
end
