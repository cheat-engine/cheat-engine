object Form9: TForm9
  Left = 437
  Top = 446
  BorderStyle = bsDialog
  Caption = 'Step 8'
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
    Left = 8
    Top = 216
    Width = 18
    Height = 13
    Caption = '100'
  end
  object Label2: TLabel
    Left = 96
    Top = 215
    Width = 6
    Height = 13
    Caption = '3'
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
      'This step will explain how to use multi-level pointers.'
      
        'In step 6 you had a simple level-1 pointer, with the first addre' +
        'ss found already being the real base address.'
      
        'This step however is a level-4 pointer. It has a pointer to a po' +
        'inter to a pointer to a pointer to a pointer to the health.'
      ''
      
        'You basicly do the same as in step 6. Find out what accesses the' +
        ' value, look at the instruction and what probably is '
      
        'the base pointer value, and what is the offset, and already fill' +
        ' that in or write it down. But in this case the address '
      
        'you'#39'll find will also be a pointer. You just have to find out th' +
        'e pointer to that pointer exactly the same way as you did '
      
        'with the value. Find out what accesses that address you found, l' +
        'ook at the assembler instruction, note the probable '
      'instruction and offset, and use that.'
      
        'and continue till you can'#39't get any further (usually when the ba' +
        'se address is a static address, shown up as green)'
      ''
      'Click Change Value to let the tutorial access the health.'
      
        'If you think you'#39've found the pointer path click Change Register' +
        '. The pointers and value will then change and you'#39'll '
      'have 3 seconds to freeze the address to 5000'
      ''
      
        'Extra: This problem can also be solved using a auto assembler sc' +
        'ript, or using the pointer scanner'
      
        'Extra2: In some situations (like this) it is recommended to chan' +
        'ge ce'#39's codefinder settings to Access violations when '
      
        'encountering instructions like mov eax,[eax] since debugregister' +
        's show it AFTER it was changed, making it hard to '
      'find out the the value of the pointer'
      ''
      ''
      ''
      ''
      ''
      
        'Extra3: If you'#39're still reading. You might notice that when look' +
        'ing at the assembler instructions that the pointer is '
      
        'being read and filled out in the same codeblock. This doesn'#39't al' +
        'ways happen, but can be really usefull in finding a '
      'pointer when debuggng is troublesome')
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
  object Button3: TButton
    Left = 93
    Top = 228
    Width = 81
    Height = 19
    Caption = 'Change pointer'
    TabOrder = 3
    OnClick = Button3Click
  end
end
