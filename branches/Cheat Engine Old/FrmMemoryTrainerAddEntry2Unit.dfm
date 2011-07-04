object FrmMemoryTrainerAddEntry2: TFrmMemoryTrainerAddEntry2
  Left = 932
  Top = 273
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Trainer maker: Add addresses'
  ClientHeight = 308
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 10
    Top = 0
    Width = 258
    Height = 16
    Caption = 'Specify  what has to happen when activated'
  end
  object Label2: TLabel
    Left = 10
    Top = 82
    Width = 35
    Height = 16
    Caption = 'Value'
  end
  object Recordlist: TListBox
    Left = 8
    Top = 124
    Width = 337
    Height = 139
    ItemHeight = 16
    TabOrder = 0
    OnClick = RecordlistClick
    OnDblClick = RecordlistDblClick
  end
  object RadioButton1: TRadioButton
    Tag = 1
    Left = 10
    Top = 20
    Width = 296
    Height = 21
    Caption = 'Set a value and freeze/unfreeze the address'
    Checked = True
    TabOrder = 1
    TabStop = True
    OnClick = RadioButtonClick
  end
  object RadioButton2: TRadioButton
    Tag = 2
    Left = 10
    Top = 39
    Width = 119
    Height = 21
    Caption = 'Only set a value'
    TabOrder = 2
    OnClick = RadioButtonClick
  end
  object RadioButton3: TRadioButton
    Tag = 3
    Left = 10
    Top = 59
    Width = 218
    Height = 21
    Caption = 'Only freeze/unfreeze the address'
    TabOrder = 3
    OnClick = RadioButtonClick
  end
  object EditValue: TEdit
    Left = 49
    Top = 81
    Width = 70
    Height = 21
    TabOrder = 4
  end
  object btnAdd: TButton
    Left = 68
    Top = 270
    Width = 92
    Height = 30
    Caption = 'Add'
    Default = True
    Enabled = False
    TabOrder = 5
    OnClick = btnAddClick
  end
  object Button2: TButton
    Left = 186
    Top = 270
    Width = 92
    Height = 30
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object CheckBox1: TCheckBox
    Left = 128
    Top = 83
    Width = 208
    Height = 21
    Caption = 'Allow user to change this value'
    TabOrder = 7
  end
  object FreezePanel: TPanel
    Left = 10
    Top = 102
    Width = 335
    Height = 21
    BevelOuter = bvNone
    TabOrder = 8
    object NormalFreeze: TRadioButton
      Left = 0
      Top = 0
      Width = 110
      Height = 21
      Caption = 'Normal freeze'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object DecreaseFreeze: TRadioButton
      Left = 217
      Top = 0
      Width = 115
      Height = 21
      Caption = 'Allow decrease'
      TabOrder = 1
    end
    object IncreaseFreeze: TRadioButton
      Left = 107
      Top = 0
      Width = 111
      Height = 21
      Caption = 'Allow increase'
      TabOrder = 2
    end
  end
end
