object TypeForm: TTypeForm
  Left = 1069
  Top = 196
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Type'
  ClientHeight = 80
  ClientWidth = 174
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 0
    Width = 97
    Height = 13
    Caption = 'Select the new type:'
  end
  object lengthlabel: TLabel
    Left = 176
    Top = -1
    Width = 33
    Height = 13
    Caption = 'Length'
  end
  object VarType: TComboBox
    Left = 8
    Top = 16
    Width = 161
    Height = 21
    Style = csDropDownList
    DropDownCount = 9
    ItemHeight = 13
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
    Left = 12
    Top = 48
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 94
    Top = 48
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
  object LengthPanel: TPanel
    Left = 173
    Top = 12
    Width = 116
    Height = 27
    BevelOuter = bvNone
    TabOrder = 3
    object Edit1: TEdit
      Left = 4
      Top = 3
      Width = 37
      Height = 21
      TabOrder = 0
      Text = '10'
    end
    object cbunicode: TCheckBox
      Left = 48
      Top = 5
      Width = 68
      Height = 17
      Caption = 'Unicode'
      TabOrder = 1
    end
  end
  object BitPanel: TPanel
    Left = 184
    Top = 36
    Width = 204
    Height = 29
    BevelOuter = bvNone
    TabOrder = 4
    object Label4: TLabel
      Left = 3
      Top = 0
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label5: TLabel
      Left = 20
      Top = 0
      Width = 6
      Height = 13
      Caption = '1'
    end
    object Label6: TLabel
      Left = 35
      Top = 0
      Width = 6
      Height = 13
      Caption = '2'
    end
    object Label7: TLabel
      Left = 52
      Top = 0
      Width = 6
      Height = 13
      Caption = '3'
    end
    object Label8: TLabel
      Left = 68
      Top = 0
      Width = 6
      Height = 13
      Caption = '4'
    end
    object Label9: TLabel
      Left = 84
      Top = 0
      Width = 6
      Height = 13
      Caption = '5'
    end
    object Label10: TLabel
      Left = 99
      Top = 0
      Width = 6
      Height = 13
      Caption = '6'
    end
    object Label11: TLabel
      Left = 116
      Top = 0
      Width = 6
      Height = 13
      Caption = '7'
    end
    object Label2: TLabel
      Left = 132
      Top = 11
      Width = 33
      Height = 13
      Caption = 'Length'
      Visible = False
    end
    object RadioButton1: TRadioButton
      Left = 0
      Top = 12
      Width = 17
      Height = 17
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButton2: TRadioButton
      Left = 16
      Top = 12
      Width = 17
      Height = 17
      TabOrder = 1
    end
    object RadioButton3: TRadioButton
      Left = 32
      Top = 12
      Width = 17
      Height = 17
      TabOrder = 2
    end
    object RadioButton4: TRadioButton
      Left = 48
      Top = 12
      Width = 17
      Height = 17
      TabOrder = 3
    end
    object RadioButton5: TRadioButton
      Left = 64
      Top = 12
      Width = 17
      Height = 17
      TabOrder = 4
    end
    object RadioButton6: TRadioButton
      Left = 80
      Top = 12
      Width = 17
      Height = 17
      TabOrder = 5
    end
    object RadioButton7: TRadioButton
      Left = 96
      Top = 12
      Width = 17
      Height = 17
      TabOrder = 6
    end
    object RadioButton8: TRadioButton
      Left = 112
      Top = 12
      Width = 17
      Height = 17
      TabOrder = 7
    end
    object Edit2: TEdit
      Left = 168
      Top = 8
      Width = 33
      Height = 21
      TabOrder = 8
      Text = '1'
      Visible = False
    end
  end
end
