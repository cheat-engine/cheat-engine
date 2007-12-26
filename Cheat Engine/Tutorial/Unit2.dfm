object Form2: TForm2
  Left = 593
  Top = 376
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Step 2'
  ClientHeight = 251
  ClientWidth = 570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
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
    Width = 34
    Height = 13
    Caption = 'Health:'
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 570
    Height = 185
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
        ', 1 byte will also work, but you'#39'll run into a '
      
        '(easy to fix) problem when you'#39've found the address and want to ' +
        'change it. The 8-byte may perhaps works if the '
      'bytes '
      'after the address are 0, but I wouldn'#39't take the bet.'
      
        'Single, double, and the other scans just dont work, because they' +
        ' store the value in a different way.'
      ''
      
        'When the value type is set correctly, make sure the scantype is ' +
        'set to '#39'Exact Value'#39
      
        'Then fill in the number your health is in the value box. And cli' +
        'ck '#39'First Scan'#39
      
        'After a while (if you have a extremly slow pc) the scan is done ' +
        'and the results are shown in the list on the '
      'left'
      ''
      
        'If you find more than 1 address and you dont know for sure wich ' +
        'address it is, click '#39'Hit me'#39', fill in the new '
      'health value into the value box, and click '#39'Next Scan'#39
      
        'repeat this untill you'#39're sure you'#39've found it. (that includes t' +
        'hat there'#39's only 1 address in the list.....)'
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
      'Also, try playing arround with the value and click '#39'hit me'#39)
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 248
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Next'
    Enabled = False
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 64
    Top = 230
    Width = 41
    Height = 17
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
