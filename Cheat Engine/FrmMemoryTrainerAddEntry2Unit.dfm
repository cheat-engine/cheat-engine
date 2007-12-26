object FrmMemoryTrainerAddEntry2: TFrmMemoryTrainerAddEntry2
  Left = 929
  Top = 273
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Trainer maker: Add addresses'
  ClientHeight = 262
  ClientWidth = 281
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 0
    Width = 211
    Height = 13
    Caption = 'Specify  what has to happen when activated'
  end
  object Label2: TLabel
    Left = 8
    Top = 76
    Width = 27
    Height = 13
    Caption = 'Value'
  end
  object Recordlist: TListBox
    Left = 8
    Top = 114
    Width = 265
    Height = 113
    ItemHeight = 13
    TabOrder = 0
    OnClick = RecordlistClick
    OnDblClick = RecordlistDblClick
  end
  object RadioButton1: TRadioButton
    Tag = 1
    Left = 8
    Top = 16
    Width = 241
    Height = 17
    Caption = 'Set a value and freeze/unfreeze the address'
    Checked = True
    TabOrder = 1
    TabStop = True
    OnClick = RadioButtonClick
  end
  object RadioButton2: TRadioButton
    Tag = 2
    Left = 8
    Top = 32
    Width = 97
    Height = 17
    Caption = 'Only set a value'
    TabOrder = 2
    OnClick = RadioButtonClick
  end
  object RadioButton3: TRadioButton
    Tag = 3
    Left = 8
    Top = 48
    Width = 177
    Height = 17
    Caption = 'Only freeze/unfreeze the address'
    TabOrder = 3
    OnClick = RadioButtonClick
  end
  object EditValue: TEdit
    Left = 40
    Top = 72
    Width = 57
    Height = 21
    TabOrder = 4
  end
  object btnAdd: TButton
    Left = 55
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Add'
    Default = True
    Enabled = False
    TabOrder = 5
    OnClick = btnAddClick
  end
  object Button2: TButton
    Left = 151
    Top = 232
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object CheckBox1: TCheckBox
    Left = 104
    Top = 74
    Width = 169
    Height = 17
    Caption = 'Allow user to change this value'
    TabOrder = 7
  end
  object FreezePanel: TPanel
    Left = 8
    Top = 96
    Width = 265
    Height = 17
    BevelOuter = bvNone
    TabOrder = 8
    object NormalFreeze: TRadioButton
      Left = 0
      Top = 0
      Width = 89
      Height = 17
      Caption = 'Normal freeze'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object DecreaseFreeze: TRadioButton
      Left = 176
      Top = 0
      Width = 94
      Height = 17
      Caption = 'Allow decrease'
      TabOrder = 1
    end
    object IncreaseFreeze: TRadioButton
      Left = 87
      Top = 0
      Width = 90
      Height = 17
      Caption = 'Allow increase'
      TabOrder = 2
    end
  end
end
