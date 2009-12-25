object frmSavedisassembly: TfrmSavedisassembly
  Left = 629
  Top = 393
  BorderStyle = bsDialog
  Caption = 'Save disassembled output'
  ClientHeight = 167
  ClientWidth = 303
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 49
    Top = 39
    Width = 31
    Height = 16
    Caption = 'From'
  end
  object Label2: TLabel
    Left = 59
    Top = 69
    Width = 17
    Height = 16
    Caption = 'To'
  end
  object Button1: TButton
    Left = 108
    Top = 108
    Width = 93
    Height = 31
    Caption = 'Save'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 89
    Top = 39
    Width = 149
    Height = 21
    TabOrder = 1
  end
  object Edit2: TEdit
    Left = 89
    Top = 69
    Width = 149
    Height = 21
    TabOrder = 2
  end
  object CheckBox1: TCheckBox
    Left = 10
    Top = 10
    Width = 70
    Height = 21
    Caption = 'Address'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object CheckBox2: TCheckBox
    Left = 118
    Top = 10
    Width = 60
    Height = 21
    Caption = 'bytes'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object CheckBox3: TCheckBox
    Left = 217
    Top = 10
    Width = 70
    Height = 21
    Caption = 'opcode'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 146
    Width = 303
    Height = 21
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
