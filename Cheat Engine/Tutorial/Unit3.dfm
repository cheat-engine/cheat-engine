object Form3: TForm3
  Left = 274
  Top = 559
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Step 3'
  ClientHeight = 250
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
    Left = 274
    Top = 216
    Width = 22
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Loss'
    Visible = False
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 571
    Height = 185
    Align = alTop
    Color = clBtnFace
    Lines.Strings = (
      
        'Ok, seeing that you'#39've figured out how to find a value using exa' +
        'ct value let'#39's move on to the next step.'
      ''
      
        'In the previous test we knew the initial value so we could do a ' +
        'exact value, but now we have a statusbar where '
      'we dont know the starting value.'
      
        'We only know that the value is between 0 and 500. And each time ' +
        'you click '#39'hit me'#39' you lose some health. The '
      'ammount you lose each time is shown above the statusbar.'
      ''
      
        'Again there are several different ways to find the value. (like ' +
        'doing a decreased value by... scan), but I'#39'll only '
      
        'explain the easiest. "Unknown initial value", and decreased valu' +
        'e.'
      
        'Because you dont know the value it is right now, a exact value w' +
        'ont do any good, so choose as scantype '
      
        #39'Unknown initial value'#39', again, the value type is 4-bytes. (most' +
        ' windows apps use 4-bytes)'
      'click first scan and wait till it'#39's done.'
      ''
      
        'When it is done click '#39'hit me'#39'. You'#39'll lose some of your health.' +
        ' (the ammount you lost shows for a few seconds and '
      'then disapears, but you dont need that)'
      
        'Now go to Cheat Engine, and choose '#39'Decreased Value'#39' and click '#39 +
        'Next Scan'#39
      
        'When that scan is done, click hit me again, and repeat the above' +
        ' till you only find a few. '
      ''
      
        'We know the value is between 0 and 500, so pick the one that is ' +
        'most likely the address we need, and add it to '
      'the list.'
      'Now change the health to 5000, to proceed to the next step.')
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
    Left = 0
    Top = 208
    Width = 41
    Height = 17
    Caption = 'Hit me'
    TabOrder = 2
    OnClick = Button2Click
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 232
    Width = 569
    Height = 16
    TabOrder = 3
  end
  object Timer1: TTimer
    Interval = 250
    OnTimer = Timer1Timer
    Left = 8
    Top = 8
  end
  object Timer2: TTimer
    OnTimer = Timer2Timer
    Left = 328
    Top = 216
  end
end
