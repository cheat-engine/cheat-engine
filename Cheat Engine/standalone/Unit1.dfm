object Form1: TForm1
  Left = 199
  Top = 234
  Width = 227
  Height = 243
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Timer1: TTimer
    Interval = 250
    OnTimer = Timer1Timer
    Left = 168
    Top = 160
  end
  object Timer2: TTimer
    Interval = 1
    OnTimer = Timer2Timer
    Left = 16
    Top = 32
  end
end
