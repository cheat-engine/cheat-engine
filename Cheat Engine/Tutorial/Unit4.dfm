object Form4: TForm4
  Left = 283
  Top = 674
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Tutorial End'
  ClientHeight = 85
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 271
    Height = 13
    Caption = 'Well done, you'#39've completed the tutorial of Cheat Engine.'
  end
  object Label2: TLabel
    Left = 8
    Top = 24
    Width = 364
    Height = 13
    Caption = 
      'Just play arround with the tutorial and learn how the other scan' +
      'methods work.'
  end
  object Button1: TButton
    Left = 160
    Top = 48
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 0
    OnClick = Button1Click
  end
end
