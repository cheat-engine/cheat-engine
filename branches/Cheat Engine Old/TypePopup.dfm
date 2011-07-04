object TypeForm: TTypeForm
  Left = 939
  Top = 157
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Type'
  ClientHeight = 98
  ClientWidth = 489
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 10
    Top = 0
    Width = 118
    Height = 16
    Caption = 'Select the new type:'
  end
  object lengthlabel: TLabel
    Left = 217
    Top = -1
    Width = 40
    Height = 16
    Caption = 'Length'
  end
  object VarType: TComboBox
    Left = 10
    Top = 20
    Width = 198
    Height = 24
    Style = csDropDownList
    DropDownCount = 9
    ItemHeight = 16
    TabOrder = 0
    OnChange = VarTypeChange
    Items.Strings = (
      'Binary'
      'Byte'
      '2 Bytes'
      '4 Bytes'
      '8 Bytes'
      'Float'
      'Double'
      'Text'
      'Array of Bytes')
  end
  object Button1: TButton
    Left = 15
    Top = 59
    Width = 92
    Height = 31
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 116
    Top = 59
    Width = 92
    Height = 31
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
  object LengthPanel: TPanel
    Left = 213
    Top = 15
    Width = 143
    Height = 33
    BevelOuter = bvNone
    TabOrder = 3
    object Edit1: TEdit
      Left = 5
      Top = 4
      Width = 45
      Height = 21
      TabOrder = 0
      Text = '10'
    end
    object cbunicode: TCheckBox
      Left = 59
      Top = 6
      Width = 84
      Height = 21
      Caption = 'Unicode'
      TabOrder = 1
    end
  end
  object BitPanel: TPanel
    Left = 226
    Top = 44
    Width = 252
    Height = 36
    BevelOuter = bvNone
    TabOrder = 4
    object Label4: TLabel
      Left = 4
      Top = 0
      Width = 7
      Height = 16
      Caption = '0'
    end
    object Label5: TLabel
      Left = 25
      Top = 0
      Width = 7
      Height = 16
      Caption = '1'
    end
    object Label6: TLabel
      Left = 43
      Top = 0
      Width = 7
      Height = 16
      Caption = '2'
    end
    object Label7: TLabel
      Left = 64
      Top = 0
      Width = 7
      Height = 16
      Caption = '3'
    end
    object Label8: TLabel
      Left = 84
      Top = 0
      Width = 7
      Height = 16
      Caption = '4'
    end
    object Label9: TLabel
      Left = 103
      Top = 0
      Width = 7
      Height = 16
      Caption = '5'
    end
    object Label10: TLabel
      Left = 122
      Top = 0
      Width = 7
      Height = 16
      Caption = '6'
    end
    object Label11: TLabel
      Left = 143
      Top = 0
      Width = 7
      Height = 16
      Caption = '7'
    end
    object Label2: TLabel
      Left = 162
      Top = 14
      Width = 40
      Height = 16
      Caption = 'Length'
      Visible = False
    end
    object RadioButton1: TRadioButton
      Left = 0
      Top = 15
      Width = 21
      Height = 21
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButton2: TRadioButton
      Left = 20
      Top = 15
      Width = 21
      Height = 21
      TabOrder = 1
    end
    object RadioButton3: TRadioButton
      Left = 39
      Top = 15
      Width = 21
      Height = 21
      TabOrder = 2
    end
    object RadioButton4: TRadioButton
      Left = 59
      Top = 15
      Width = 21
      Height = 21
      TabOrder = 3
    end
    object RadioButton5: TRadioButton
      Left = 79
      Top = 15
      Width = 21
      Height = 21
      TabOrder = 4
    end
    object RadioButton6: TRadioButton
      Left = 98
      Top = 15
      Width = 21
      Height = 21
      TabOrder = 5
    end
    object RadioButton7: TRadioButton
      Left = 118
      Top = 15
      Width = 21
      Height = 21
      TabOrder = 6
    end
    object RadioButton8: TRadioButton
      Left = 138
      Top = 15
      Width = 21
      Height = 21
      TabOrder = 7
    end
    object Edit2: TEdit
      Left = 207
      Top = 10
      Width = 40
      Height = 21
      TabOrder = 8
      Text = '1'
      Visible = False
    end
  end
end
