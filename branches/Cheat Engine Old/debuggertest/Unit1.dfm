object Form1: TForm1
  Left = 302
  Top = 330
  BorderStyle = bsSingle
  Caption = 'Form1'
  ClientHeight = 287
  ClientWidth = 851
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 118
    Top = 75
    Width = 41
    Height = 16
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 119
    Top = 144
    Width = 41
    Height = 16
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 246
    Top = 49
    Width = 41
    Height = 16
    Caption = 'Label3'
  end
  object Label4: TLabel
    Left = 246
    Top = 69
    Width = 41
    Height = 16
    Caption = 'Label4'
  end
  object Label5: TLabel
    Left = 246
    Top = 89
    Width = 41
    Height = 16
    Caption = 'Label5'
  end
  object Label6: TLabel
    Left = 246
    Top = 108
    Width = 41
    Height = 16
    Caption = 'Label6'
  end
  object Label7: TLabel
    Left = 246
    Top = 128
    Width = 41
    Height = 16
    Caption = 'Label7'
  end
  object Label8: TLabel
    Left = 246
    Top = 148
    Width = 41
    Height = 16
    Caption = 'Label8'
  end
  object Label9: TLabel
    Left = 10
    Top = 187
    Width = 41
    Height = 16
    Caption = 'Label9'
  end
  object Button1: TButton
    Left = 10
    Top = 10
    Width = 168
    Height = 31
    Caption = 'Test expected BP'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 10
    Top = 59
    Width = 100
    Height = 21
    Caption = 'Change health'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 10
    Top = 138
    Width = 100
    Height = 31
    Caption = 'Execute code'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 246
    Top = 10
    Width = 92
    Height = 31
    Caption = 'Get DR'#39's'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 10
    Top = 217
    Width = 149
    Height = 30
    Caption = 'Make non executable'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 266
    Top = 217
    Width = 92
    Height = 30
    Caption = 'Crash'
    TabOrder = 5
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 10
    Top = 89
    Width = 100
    Height = 21
    Caption = 'With Thread'
    TabOrder = 6
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 264
    Top = 184
    Width = 105
    Height = 25
    Caption = 'Integrity check'
    TabOrder = 7
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 376
    Top = 8
    Width = 169
    Height = 25
    Caption = 'Create thread longterm'
    TabOrder = 8
    OnClick = Button9Click
  end
end
