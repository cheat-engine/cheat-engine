object frmModifyRegisters: TfrmModifyRegisters
  Left = 829
  Top = 474
  Width = 299
  Height = 300
  Caption = 'Modify register(s) at xxxxxxxx'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 21
    Height = 13
    Caption = 'EAX'
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 21
    Height = 13
    Caption = 'EBX'
  end
  object Label3: TLabel
    Left = 8
    Top = 56
    Width = 21
    Height = 13
    Caption = 'ECX'
  end
  object Label4: TLabel
    Left = 8
    Top = 80
    Width = 22
    Height = 13
    Caption = 'EDX'
  end
  object Label5: TLabel
    Left = 8
    Top = 104
    Width = 17
    Height = 13
    Caption = 'ESI'
  end
  object Label6: TLabel
    Left = 8
    Top = 128
    Width = 18
    Height = 13
    Caption = 'EDI'
  end
  object Label7: TLabel
    Left = 8
    Top = 152
    Width = 21
    Height = 13
    Caption = 'EBP'
  end
  object Label8: TLabel
    Left = 8
    Top = 176
    Width = 21
    Height = 13
    Caption = 'ESP'
  end
  object Label9: TLabel
    Left = 8
    Top = 200
    Width = 17
    Height = 13
    Caption = 'EIP'
  end
  object Label10: TLabel
    Left = 232
    Top = 24
    Width = 13
    Height = 13
    Caption = 'CF'
  end
  object Label11: TLabel
    Left = 232
    Top = 48
    Width = 13
    Height = 13
    Caption = 'PF'
  end
  object Label12: TLabel
    Left = 232
    Top = 72
    Width = 13
    Height = 13
    Caption = 'AF'
  end
  object Label13: TLabel
    Left = 232
    Top = 96
    Width = 13
    Height = 13
    Caption = 'ZF'
  end
  object Label14: TLabel
    Left = 232
    Top = 120
    Width = 13
    Height = 13
    Caption = 'SF'
  end
  object Label15: TLabel
    Left = 232
    Top = 144
    Width = 14
    Height = 13
    Caption = 'OF'
  end
  object Label16: TLabel
    Left = 248
    Top = 8
    Width = 25
    Height = 13
    Caption = 'Flags'
  end
  object CheckBox1: TCheckBox
    Left = 32
    Top = 6
    Width = 17
    Height = 17
    TabOrder = 0
    OnClick = CheckBox1Click
  end
  object Edit1: TEdit
    Left = 48
    Top = 4
    Width = 121
    Height = 21
    Enabled = False
    TabOrder = 1
  end
  object CheckBox2: TCheckBox
    Left = 32
    Top = 30
    Width = 17
    Height = 17
    TabOrder = 2
    OnClick = CheckBox2Click
  end
  object Edit2: TEdit
    Left = 48
    Top = 28
    Width = 121
    Height = 21
    Enabled = False
    TabOrder = 3
  end
  object CheckBox3: TCheckBox
    Left = 32
    Top = 54
    Width = 17
    Height = 17
    TabOrder = 4
    OnClick = CheckBox3Click
  end
  object Edit3: TEdit
    Left = 48
    Top = 52
    Width = 121
    Height = 21
    Enabled = False
    TabOrder = 5
  end
  object CheckBox4: TCheckBox
    Left = 32
    Top = 78
    Width = 17
    Height = 17
    TabOrder = 6
    OnClick = CheckBox4Click
  end
  object Edit4: TEdit
    Left = 48
    Top = 76
    Width = 121
    Height = 21
    Enabled = False
    TabOrder = 7
  end
  object CheckBox5: TCheckBox
    Left = 32
    Top = 104
    Width = 17
    Height = 15
    TabOrder = 8
    OnClick = CheckBox5Click
  end
  object Edit5: TEdit
    Left = 48
    Top = 100
    Width = 121
    Height = 21
    Enabled = False
    TabOrder = 9
  end
  object CheckBox6: TCheckBox
    Left = 32
    Top = 126
    Width = 17
    Height = 17
    TabOrder = 10
    OnClick = CheckBox6Click
  end
  object Edit6: TEdit
    Left = 48
    Top = 124
    Width = 121
    Height = 21
    Enabled = False
    TabOrder = 11
  end
  object CheckBox7: TCheckBox
    Left = 32
    Top = 150
    Width = 17
    Height = 17
    TabOrder = 12
    OnClick = CheckBox7Click
  end
  object Edit7: TEdit
    Left = 48
    Top = 148
    Width = 121
    Height = 21
    Enabled = False
    TabOrder = 13
  end
  object CheckBox8: TCheckBox
    Left = 32
    Top = 174
    Width = 17
    Height = 17
    TabOrder = 14
    OnClick = CheckBox8Click
  end
  object Edit8: TEdit
    Left = 48
    Top = 172
    Width = 121
    Height = 21
    Enabled = False
    TabOrder = 15
  end
  object CheckBox9: TCheckBox
    Left = 32
    Top = 198
    Width = 17
    Height = 17
    TabOrder = 16
    OnClick = CheckBox9Click
  end
  object Edit9: TEdit
    Left = 48
    Top = 196
    Width = 121
    Height = 21
    Enabled = False
    TabOrder = 17
  end
  object CheckBox10: TCheckBox
    Left = 248
    Top = 22
    Width = 17
    Height = 17
    TabOrder = 18
    OnClick = CheckBox10Click
  end
  object CheckBox11: TCheckBox
    Left = 248
    Top = 46
    Width = 17
    Height = 17
    TabOrder = 19
    OnClick = CheckBox11Click
  end
  object CheckBox12: TCheckBox
    Left = 248
    Top = 70
    Width = 17
    Height = 17
    TabOrder = 20
    OnClick = CheckBox12Click
  end
  object CheckBox13: TCheckBox
    Left = 248
    Top = 94
    Width = 17
    Height = 17
    TabOrder = 21
    OnClick = CheckBox13Click
  end
  object CheckBox14: TCheckBox
    Left = 248
    Top = 118
    Width = 17
    Height = 17
    TabOrder = 22
    OnClick = CheckBox14Click
  end
  object CheckBox15: TCheckBox
    Left = 248
    Top = 142
    Width = 17
    Height = 17
    TabOrder = 23
    OnClick = CheckBox15Click
  end
  object CheckBox16: TCheckBox
    Left = 264
    Top = 22
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 24
  end
  object CheckBox17: TCheckBox
    Left = 264
    Top = 46
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 25
  end
  object CheckBox18: TCheckBox
    Left = 264
    Top = 70
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 26
  end
  object CheckBox19: TCheckBox
    Left = 264
    Top = 94
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 27
  end
  object CheckBox20: TCheckBox
    Left = 264
    Top = 118
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 28
  end
  object CheckBox21: TCheckBox
    Left = 264
    Top = 142
    Width = 17
    Height = 17
    Enabled = False
    TabOrder = 29
  end
  object Button1: TButton
    Left = 80
    Top = 232
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 30
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 160
    Top = 232
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 31
  end
end
