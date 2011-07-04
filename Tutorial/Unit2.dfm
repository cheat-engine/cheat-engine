object Form2: TForm2
  Left = 593
  Top = 376
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Step 2'
  ClientHeight = 309
  ClientWidth = 702
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 49
    Top = 286
    Width = 21
    Height = 16
    Caption = '100'
  end
  object Label2: TLabel
    Left = 0
    Top = 286
    Width = 42
    Height = 16
    Caption = 'Health:'
  end
  object SpeedButton1: TSpeedButton
    Left = 670
    Top = 286
    Width = 30
    Height = 20
    Caption = 'Skip'
    Flat = True
    OnClick = SpeedButton1Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 702
    Height = 228
    Align = alTop
    Color = clBtnFace
    Lines.Strings = (
      
        'Now that you have opened the tutorial with Cheat Engine lets get' +
        ' on with the next step.'
      ''
      'You see at the bottom of this window the text Health: xxx'
      'Each time you click '#39'Hit me'#39'  your health gets decreased.'
      ''
      
        'To get to the next step you have to find this value and change i' +
        't to 1000'
      ''
      
        'To find the value there are different ways, but I'#39'll tell you ab' +
        'out the easiest, '#39'Exact Value'#39':'
      
        'First make sure value type is set to at least 2 bytes or 4 bytes' +
        ', 1 byte will also work, but you'#39'll run into a (easy to fix) '
      
        'problem when you'#39've found the address and want to change it. The' +
        ' 8-byte may perhaps works if the '
      'bytes after the address are 0, but I wouldn'#39't take the bet.'
      
        'Single, double, and the other scans just don'#39't work, because the' +
        'y store the value in a different way.'
      ''
      
        'When the value type is set correctly, make sure the scantype is ' +
        'set to '#39'Exact Value'#39
      
        'Then fill in the number your health is in the value box. And cli' +
        'ck '#39'First Scan'#39
      
        'After a while (if you have a extremely slow pc) the scan is done' +
        ' and the results are shown in the list on the '
      'left'
      ''
      
        'If you find more than 1 address and you don'#39't know for sure whic' +
        'h address it is, click '#39'Hit me'#39', fill in the new '
      'health value into the value box, and click '#39'Next Scan'#39
      
        'repeat this until you'#39're sure you'#39've found it. (that includes th' +
        'at there'#39's only 1 address in the list.....)'
      ''
      
        'Now double click the address in the list on the left. This makes' +
        ' the address pop-up in the list at the bottom, '
      'showing you the current value.'
      
        'Double click the value, (or select it and press enter), and chan' +
        'ge the value to 1000.'
      ''
      
        'If everything went ok the next button should become enabled, and' +
        ' you'#39're ready for the next step.'
      ''
      ''
      'Note:'
      
        'If you did anything wrong while scanning, click "New Scan" and r' +
        'epeat the scanning again.'
      'Also, try playing around with the value and click '#39'hit me'#39)
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 305
    Top = 236
    Width = 93
    Height = 31
    Caption = 'Next'
    Enabled = False
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 79
    Top = 283
    Width = 50
    Height = 21
    Caption = 'Hit me'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Timer1: TTimer
    Interval = 250
    OnTimer = Timer1Timer
    Left = 136
    Top = 176
  end
end
