object frmSavedisassembly: TfrmSavedisassembly
  Left = 629
  Top = 393
  BorderStyle = bsDialog
  Caption = 'Save disassembled output'
  ClientHeight = 136
  ClientWidth = 246
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 40
    Top = 32
    Width = 23
    Height = 13
    Caption = 'From'
  end
  object Label2: TLabel
    Left = 48
    Top = 56
    Width = 13
    Height = 13
    Caption = 'To'
  end
  object Button1: TButton
    Left = 88
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Save'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 72
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object Edit2: TEdit
    Left = 72
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 8
    Width = 57
    Height = 17
    Caption = 'Address'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object CheckBox2: TCheckBox
    Left = 96
    Top = 8
    Width = 49
    Height = 17
    Caption = 'bytes'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object CheckBox3: TCheckBox
    Left = 176
    Top = 8
    Width = 57
    Height = 17
    Caption = 'opcode'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 119
    Width = 246
    Height = 17
    Align = alBottom
    TabOrder = 6
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'textfiles (*.txt)|*.txt'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Top = 32
  end
end
