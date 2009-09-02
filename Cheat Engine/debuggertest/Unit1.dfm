object Form1: TForm1
  Left = 302
  Top = 330
  BorderStyle = bsSingle
  Caption = 'Form1'
  ClientHeight = 215
  ClientWidth = 435
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
  object Label1: TLabel
    Left = 96
    Top = 53
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 97
    Top = 93
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 200
    Top = 40
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 200
    Top = 56
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Label5: TLabel
    Left = 200
    Top = 72
    Width = 32
    Height = 13
    Caption = 'Label5'
  end
  object Label6: TLabel
    Left = 200
    Top = 88
    Width = 32
    Height = 13
    Caption = 'Label6'
  end
  object Label7: TLabel
    Left = 200
    Top = 104
    Width = 32
    Height = 13
    Caption = 'Label7'
  end
  object Label8: TLabel
    Left = 200
    Top = 120
    Width = 32
    Height = 13
    Caption = 'Label8'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 137
    Height = 25
    Caption = 'Test expected BP'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 48
    Width = 81
    Height = 25
    Caption = 'Change health'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 88
    Width = 81
    Height = 25
    Caption = 'Execute code'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 200
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Get DR'#39's'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 176
    Width = 121
    Height = 25
    Caption = 'Make non executable'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 216
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Crash'
    TabOrder = 5
    OnClick = Button6Click
  end
end
