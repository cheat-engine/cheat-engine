object Form5: TForm5
  Left = 263
  Top = 843
  BorderStyle = bsDialog
  Caption = 'Step 4'
  ClientHeight = 249
  ClientWidth = 571
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 40
    Top = 232
    Width = 18
    Height = 13
    Caption = '100'
  end
  object Label2: TLabel
    Left = 0
    Top = 232
    Width = 29
    Height = 13
    Caption = 'Ammo'
  end
  object Label3: TLabel
    Left = 0
    Top = 205
    Width = 34
    Height = 13
    Caption = 'Health:'
  end
  object Label4: TLabel
    Left = 40
    Top = 205
    Width = 18
    Height = 13
    Caption = '100'
  end
  object Label5: TLabel
    Left = 120
    Top = 205
    Width = 26
    Height = 13
    Caption = '(float)'
  end
  object Label6: TLabel
    Left = 120
    Top = 232
    Width = 38
    Height = 13
    Caption = '(double)'
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 571
    Height = 185
    Align = alTop
    Color = clBtnFace
    Lines.Strings = (
      
        'In the previous tutorial we used bytes to scan, but some games s' +
        'tore information in so called '#39'floating point'#39' notations. '
      
        '(propably to prevent simple memory scanners from finding it the ' +
        'easy way)'
      
        'a floating point is a value with some digits behind the point. (' +
        'like 5.12 or 11321.1)'
      ''
      
        'Below you see your health and ammo. Both are stored as Floating ' +
        'point notations, but health is stored as a float and '
      'ammo is stored as a double.'
      
        'Click on hit me to lose some health, and on shoot to decrease yo' +
        'ur ammo with 0.5'
      ''
      'You have to set BOTH values to 5000 or higher to proceed.'
      ''
      
        'Exact value scan will work fine here, but you may want to experi' +
        'ment with other types too.'
      ''
      ''
      ''
      ''
      ''
      ''
      ''
      ''
      ''
      ''
      ''
      ''
      ''
      ''
      'Hint: It is recommended to disable "Fast Scan" for type double')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button2: TButton
    Left = 248
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Next'
    Enabled = False
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button1: TButton
    Left = 72
    Top = 230
    Width = 41
    Height = 17
    Caption = 'Fire'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 72
    Top = 203
    Width = 41
    Height = 17
    Caption = 'Hit me'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Timer1: TTimer
    Interval = 250
    OnTimer = Timer1Timer
    Left = 432
    Top = 200
  end
end
