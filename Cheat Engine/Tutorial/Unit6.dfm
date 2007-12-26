object Form6: TForm6
  Left = 210
  Top = 195
  BorderStyle = bsDialog
  Caption = 'Step 5'
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
    Left = 8
    Top = 216
    Width = 18
    Height = 13
    Caption = '100'
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 571
    Height = 185
    Align = alTop
    Color = clBtnFace
    Lines.Strings = (
      
        'Sometimes the location something is stored at changes when you r' +
        'estart the game, or even while you'#39're playing.. In '
      'that case you can use 2 things to still make a table that '
      'works.'
      
        'In this step I'#39'll try to descibe how to use the Code Finder func' +
        'tion.'
      ''
      
        'The value down here will be at a different location each time yo' +
        'u start the tutorial, so a normal entry in the address '
      'list wouldn'#39't work.'
      
        'First try to find the address. (you'#39've got to this point so I as' +
        'sume you know how to)'
      
        'When you'#39've found the address, right-click the address in Cheat ' +
        'Engine and choose "Find out what writes to this '
      'address". A window will pop up with an empty list.'
      
        'Then click on the Change value button in this tutorial, and go b' +
        'ack to Cheat Engine. If evrything went right there '
      'should be an address with assembler code there now.'
      
        'Click it and choose the replace option to replace it with code t' +
        'hat does nothing. That will also add the code address '
      
        'to the code list in the advanced options window. (Wich gets save' +
        'd if you save your table)'
      ''
      
        'Click on stop, so the game will start running normal again, and ' +
        'close to close the window.'
      
        'Now, click on Change value, and if everything went right the Nex' +
        't button should become clickable.'
      ''
      
        'Note: When you'#39're freezing the address with a high enough speed ' +
        'it may happen that next becomes visible anyhow')
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
    Left = 8
    Top = 228
    Width = 81
    Height = 19
    Caption = 'Change value'
    TabOrder = 2
    OnClick = Button1Click
  end
end
