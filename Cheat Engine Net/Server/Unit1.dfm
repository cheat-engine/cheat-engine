object Form1: TForm1
  Left = 384
  Top = 113
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Cheat Engine Network Server'
  ClientHeight = 97
  ClientWidth = 398
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
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 40
    Top = 0
    Width = 19
    Height = 13
    Caption = 'Port'
  end
  object Button1: TButton
    Left = 16
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Start Server'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Portvalue: TEdit
    Left = 8
    Top = 16
    Width = 89
    Height = 21
    TabOrder = 1
    Text = '1500'
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 78
    Width = 398
    Height = 19
    Panels = <
      item
        Text = 'Offline'
        Width = 50
      end>
  end
  object Log: TMemo
    Left = 101
    Top = 0
    Width = 297
    Height = 78
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    PopupMenu = PopupMenu1
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object ListBox: TListBox
    Left = 1000
    Top = 96
    Width = 345
    Height = 97
    ItemHeight = 13
    TabOrder = 4
  end
  object hexcb: TCheckBox
    Left = 392
    Top = 500
    Width = 113
    Height = 17
    Caption = 'hexcb'
    TabOrder = 5
  end
  object Button2: TButton
    Left = 80
    Top = 0
    Width = 25
    Height = 17
    Caption = 'Button2'
    TabOrder = 6
    Visible = False
    OnClick = Button2Click
  end
  object FreezeTimer: TTimer
    Interval = 250
    OnTimer = FreezeTimerTimer
    Left = 120
  end
  object UpdateTimer: TTimer
    Interval = 10000
    OnTimer = UpdateTimerTimer
    Left = 120
    Top = 40
  end
  object TimeOutTest: TTimer
    Interval = 30000
    OnTimer = TimeOutTestTimer
    Left = 160
    Top = 16
  end
  object Progressbartimer: TTimer
    OnTimer = ProgressbartimerTimer
    Left = 240
    Top = 8
  end
  object IdTCPServer1: TIdTCPServer
    Bindings = <>
    CommandHandlers = <>
    DefaultPort = 0
    Greeting.NumericCode = 0
    MaxConnectionReply.NumericCode = 0
    MaxConnections = 1
    OnExecute = IdTCPServer1Execute
    ReplyExceptionCode = 0
    ReplyTexts = <>
    ReplyUnknownCommand.NumericCode = 0
    Left = 320
    Top = 32
  end
  object PopupMenu1: TPopupMenu
    Left = 192
    Top = 24
    object Savetofile1: TMenuItem
      Caption = 'Save to file'
      OnClick = Savetofile1Click
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'Text files (*.txt)|*.txt'
    Left = 232
    Top = 40
  end
end
