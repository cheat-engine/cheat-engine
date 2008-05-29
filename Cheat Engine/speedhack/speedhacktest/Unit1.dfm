object Form1: TForm1
  Left = 192
  Top = 114
  Width = 475
  Height = 202
  Caption = 'speedhack test'
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
    Left = 0
    Top = 32
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 0
    Top = 48
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 0
    Top = 64
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
  end
end
