object Form1: TForm1
  Left = 192
  Top = 103
  Width = 320
  Height = 153
  Caption = 'Form1'
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
  object Edit1: TEdit
    Left = 40
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '127.0.0.1'
  end
  object Button1: TButton
    Left = 128
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 1
  end
  object Edit2: TEdit
    Left = 168
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '1500'
  end
  object ClientSocket1: TClientSocket
    Active = False
    Address = '127.0.0.1'
    ClientType = ctNonBlocking
    Port = 1500
    Left = 8
    Top = 24
  end
end
