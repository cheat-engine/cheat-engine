object ConnectForm: TConnectForm
  Left = 100
  Top = 578
  BorderStyle = bsDialog
  Caption = 'Cheat Engine Client'
  ClientHeight = 70
  ClientWidth = 193
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 40
    Top = 0
    Width = 10
    Height = 13
    Caption = 'IP'
  end
  object Label2: TLabel
    Left = 160
    Top = 0
    Width = 19
    Height = 13
    Caption = 'Port'
  end
  object Button1: TButton
    Left = 56
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Connect'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object EditPort: TEdit
    Left = 127
    Top = 16
    Width = 65
    Height = 21
    TabOrder = 1
    Text = '1500'
  end
  object editAddress: TComboBox
    Left = 1
    Top = 16
    Width = 121
    Height = 21
    ItemHeight = 13
    TabOrder = 2
    Text = '127.0.0.1'
  end
  object TimeOutTimer: TTimer
    Enabled = False
    Interval = 61000
    OnTimer = TimeOutTimerTimer
    Left = 152
    Top = 40
  end
  object IdTCPClient1: TIdTCPClient
    MaxLineAction = maException
    ReadTimeout = 0
    Port = 0
    Left = 8
    Top = 40
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 88
    Top = 8
  end
end
