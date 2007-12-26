object Form8: TForm8
  Left = 570
  Top = 503
  BorderStyle = bsDialog
  Caption = 'Step 7'
  ClientHeight = 250
  ClientWidth = 571
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
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
    Width = 571
    Height = 185
    Align = alTop
    Color = clBtnFace
    Lines.Strings = (
      
        'Code injection is a technique where one injects a piece of code ' +
        'into the target process, and then reroute the '
      'execution of code to go through your own written code'
      ''
      
        'In this tutorial you'#39'll have a health value and a button that wi' +
        'll decrease your health with 1 each time you click it.'
      
        'Your task is to use code injection to increase the value of your' +
        ' health with 2 every time it is clicked'
      ''
      'Start with finding the address and then find what writes to it.'
      
        'then when you'#39've found the code that decreases it browse to that' +
        ' address in the disassembler, and open the auto '
      'assembler window (ctrl+a)'
      
        'There click on template and then code injection, and give it the' +
        ' address that decreases health (If it isn'#39't already filled '
      'in correctly)'
      
        'That'#39'll generate a basic auto assembler injection framework you ' +
        'can use for your code.'
      ''
      
        'Notice the alloc, that'#39'll allocate a block of memory for your co' +
        'de cave, in the past, in the pre windows 2000 systems, '
      
        'people had to find code caves in the memory(regiosn of memory un' +
        'usaed by the game), but thats luckely a thing f '
      
        'the past since windows 2000, and will these days cause errors wh' +
        'en trying to be used, due to SP2 of XP and the NX '
      'bit of new CPU'#39's'
      ''
      
        'Also notice the line newmem: and originalcode: and the text "Pla' +
        'ce your code here"'
      
        'As you guessed it, write your code here that will increase the  ' +
        'health with 2.'
      
        'a usefull assembnler instruction in this case is the "ADD instru' +
        'ction"'
      'here are a few examples:'
      '"ADD [00901234],9" to increase the address at 00901234 with 9'
      
        '"ADD [ESP+4],9" to increase the address pointed to by ESP+4 with' +
        ' 9'
      
        'In this case, you'#39'll have to use the same thing between the brac' +
        'kets as the original code has that decreases your '
      'health'
      ''
      'Notice:'
      
        'It is recommended to delete the line that decreases your health ' +
        'from the original code section, else you'#39'll have to '
      
        'increase your health with 3 (you increase with 3, the original c' +
        'ode decreases with 1, so the end result is increase '
      
        'with 2), which might become confusing. But it'#39's all up to you an' +
        'd your programming.'
      ''
      'Notice 2:'
      
        'In some games the original code can exist out of multiple instru' +
        'ctions, and sometimes, not always, it might happen '
      
        'that a code at another place jumps into your jump instruction en' +
        'd will then cause unknown behaviour. If that '
      
        'happens, you should usually look near that instruction and see t' +
        'he jumps and fix it, or perhaps even choose to use a '
      
        'different address to do the code injection from. As long as you'#39 +
        're able to figure out the address to change from inside '
      'your injected code.')
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
    Left = 64
    Top = 230
    Width = 41
    Height = 17
    Caption = 'Hit me'
    TabOrder = 2
    OnClick = Button1Click
  end
end
