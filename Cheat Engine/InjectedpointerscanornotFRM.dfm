object frmInjectedpointerscanornot: TfrmInjectedpointerscanornot
  Left = 241
  Top = 136
  BorderStyle = bsDialog
  Caption = 'Pointerscan'
  ClientHeight = 67
  ClientWidth = 399
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
    Left = 8
    Top = 8
    Width = 374
    Height = 13
    Caption = 
      'Do you want to use the injected pointer scanner (fast) or the de' +
      'fault one (slow)?'
  end
  object Button1: TButton
    Left = 200
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Default'
    ModalResult = 6
    TabOrder = 1
  end
  object Button2: TButton
    Left = 112
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Injected'
    Default = True
    ModalResult = 7
    TabOrder = 0
  end
end
