object Form1: TForm1
  Left = 378
  Top = 501
  Width = 514
  Height = 293
  Caption = 'PE-Tester'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 241
    Height = 13
    Caption = 'This is just a small app to test basic PE functionality'
  end
  object Label2: TLabel
    Left = 8
    Top = 24
    Width = 10
    Height = 13
    Caption = 'IP'
  end
  object Label3: TLabel
    Left = 136
    Top = 24
    Width = 19
    Height = 13
    Caption = 'Port'
  end
  object Button1: TButton
    Left = 8
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'cheatengine.org'
  end
  object Edit2: TEdit
    Left = 136
    Top = 40
    Width = 57
    Height = 21
    TabOrder = 2
    Text = '80'
  end
  object Memo1: TMemo
    Left = 8
    Top = 96
    Width = 193
    Height = 121
    Lines.Strings = (
      'index.php')
    TabOrder = 3
  end
  object Button2: TButton
    Left = 8
    Top = 224
    Width = 75
    Height = 25
    Caption = 'Send'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 96
    Top = 224
    Width = 75
    Height = 25
    Caption = 'Receive'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 248
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Button4'
    TabOrder = 6
    OnClick = Button4Click
  end
  object TcpClient1: TTcpClient
    BlockMode = bmNonBlocking
    Left = 104
    Top = 64
  end
  object IdTCPClient1: TIdTCPClient
    MaxLineAction = maException
    ReadTimeout = 0
    Port = 0
    Left = 336
    Top = 64
  end
end
