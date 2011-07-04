object frmFilter: TfrmFilter
  Left = 191
  Top = 128
  Width = 361
  Height = 201
  Caption = 'Filter'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 353
    Height = 39
    Align = alTop
    Caption = 
      'This is just an example plugin for now. Perhaps Dark Byte or som' +
      'eone else migth add more advanced features here someday, or just' +
      ' a completly different implementation. (e.g in vb...)'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 0
    Top = 48
    Width = 40
    Height = 13
    Caption = 'Replace'
  end
  object Label3: TLabel
    Left = 0
    Top = 88
    Width = 19
    Height = 13
    Caption = 'with'
  end
  object Edit1: TEdit
    Left = 8
    Top = 64
    Width = 337
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 8
    Top = 104
    Width = 337
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 8
    Top = 136
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
  object CheckBox1: TCheckBox
    Left = 112
    Top = 136
    Width = 65
    Height = 17
    Caption = 'On Send'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object CheckBox2: TCheckBox
    Left = 192
    Top = 136
    Width = 81
    Height = 17
    Caption = 'On Receive'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
end
