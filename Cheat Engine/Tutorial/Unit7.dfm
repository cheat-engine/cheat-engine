object Form7: TForm7
  Left = 539
  Top = 241
  BorderStyle = bsDialog
  Caption = 'Step 6'
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
      
        'In the previous step I explained how to use the Code finder to h' +
        'andle changing locations. But that method alone '
      
        'makes it difficult to find the address to set the values you wan' +
        't.'
      'Thats why there are pointers:'
      ''
      
        'At the bottom you'#39'll find 2 buttons. One will change the value, ' +
        'and the other changes the value AND the location of '
      'the value.'
      
        'For this step you dont really need to know assembler, but it hel' +
        'ps a lot if you do.'
      ''
      
        'First find the address of the value. When you'#39've found it use th' +
        'e function to find out what writes to that address.'
      
        'Change the value again, and a item will show in the list. Double' +
        ' click that item. (or select and click on more info) and '
      
        'a new window will open with detailed information on what happene' +
        'd when the instruction ran.'
      
        'If the assembler instruction doesnt have anything between a '#39'['#39' ' +
        'and '#39']'#39' then use another item in the list.'
      
        'If it does it will say what it think will be the value of the po' +
        'inter you need.'
      
        'Go back to the main cheat engine window (you can keep this extra' +
        ' info window open if you want, but if you close it, '
      
        'remember what is between the [ and ] ) and do a 4 byte scan in h' +
        'exadecimal for the value the extra info told you.'
      
        'When done scanning it may return 1 or a few hundred addresses. M' +
        'ost of the time the address you need will be the '
      
        'smallest one. Now click on manually add and select the pointer c' +
        'heckbox.'
      ''
      
        'The window will change and allows you to type in the address of ' +
        'a pointer and a offset.'
      'Fill in as address the address you just found.'
      
        'If the assembler instruction has a calculation (e.g: [esi+12]) a' +
        't the end then type the value in thats at the end. else '
      
        'leave it 0. If it was a more complicated instruction look at the' +
        ' calculation.'
      ''
      'example of a more complicated instruction:'
      '[EAX*2+EDX+00000310] eax=4C and edx=00801234.'
      
        'In this case EDX would be the value the pointer has, and EAX*2+0' +
        '0000310 the offset, so the offset you'#39'd fill in '
      
        'would be 2*4C+00000310=3A8.  (this is all in hex, use cal.exe fr' +
        'om windows in scientific mode to calculate)'
      ''
      
        'Back to the tutorial, click OK and the address will be added, If' +
        ' all went right the address will show P->xxxxxxx, with '
      
        'xxxxxxx being the address of the value you found. If thats not r' +
        'ight, you'#39've done something wrong.'
      
        'Now, change the value using the pointer you added in 5000 and fr' +
        'eeze it. Then click Change pointer, and if all went '
      'right the next button will become visible.'
      ''
      ''
      'extra:'
      
        'And you could also use the pointer scanner to find the pointer t' +
        'o this address')
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
