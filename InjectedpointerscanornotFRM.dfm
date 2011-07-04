object frmInjectedpointerscanornot: TfrmInjectedpointerscanornot
  Left = 241
  Top = 136
  BorderStyle = bsDialog
  Caption = 'Pointerscan'
  ClientHeight = 82
  ClientWidth = 491
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 10
    Top = 10
    Width = 460
    Height = 16
    Caption = 
      'Do you want to use the injected pointer scanner (slow) or the de' +
      'fault one (fast)?'
  end
  object Button1: TButton
    Left = 138
    Top = 39
    Width = 92
    Height = 31
    Caption = 'Default'
    Default = True
    ModalResult = 6
    TabOrder = 0
  end
  object Button2: TButton
    Left = 246
    Top = 39
    Width = 92
    Height = 31
    Caption = 'Injected'
    ModalResult = 7
    TabOrder = 1
  end
end
