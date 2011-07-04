object AddForm: TAddForm
  Left = 839
  Top = 729
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Add address'
  ClientHeight = 167
  ClientWidth = 545
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    545
    167)
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 28
    Top = 10
    Width = 51
    Height = 16
    Caption = 'Address'
  end
  object Label2: TLabel
    Left = 47
    Top = 68
    Width = 32
    Height = 16
    Caption = 'Type'
  end
  object Label3: TLabel
    Left = 10
    Top = 39
    Width = 68
    Height = 16
    Caption = 'Description'
  end
  object ValuePanel: TPanel
    Left = 209
    Top = 62
    Width = 285
    Height = 33
    BevelOuter = bvNone
    TabOrder = 6
    Visible = False
    object Label12: TLabel
      Left = 0
      Top = 5
      Width = 110
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      BiDiMode = bdLeftToRight
      ParentBiDiMode = False
    end
    object Edit1: TEdit
      Left = 118
      Top = 2
      Width = 51
      Height = 24
      PopupMenu = MainForm.emptypopup
      TabOrder = 0
      Text = '1'
    end
    object cbUnicode: TCheckBox
      Left = 177
      Top = 5
      Width = 93
      Height = 21
      Caption = 'Unicode'
      TabOrder = 1
    end
  end
  object VarType: TComboBox
    Left = 79
    Top = 63
    Width = 129
    Height = 24
    Style = csDropDownList
    DropDownCount = 9
    ItemHeight = 16
    TabOrder = 2
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
      'Array of Byte')
  end
  object Button1: TButton
    Left = 10
    Top = 124
    Width = 92
    Height = 31
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 118
    Top = 124
    Width = 92
    Height = 31
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Description: TEdit
    Left = 79
    Top = 34
    Width = 464
    Height = 24
    MaxLength = 50
    TabOrder = 1
    Text = 'No Description!'
  end
  object BitPanel: TPanel
    Left = 208
    Top = 62
    Width = 316
    Height = 33
    BevelOuter = bvNone
    TabOrder = 5
    Visible = False
    object Label4: TLabel
      Left = 63
      Top = 0
      Width = 7
      Height = 16
      Caption = '0'
    end
    object Label5: TLabel
      Left = 84
      Top = 0
      Width = 7
      Height = 16
      Caption = '1'
    end
    object Label6: TLabel
      Left = 102
      Top = 0
      Width = 7
      Height = 16
      Caption = '2'
    end
    object Label7: TLabel
      Left = 123
      Top = 0
      Width = 7
      Height = 16
      Caption = '3'
    end
    object Label8: TLabel
      Left = 143
      Top = 0
      Width = 7
      Height = 16
      Caption = '4'
    end
    object Label9: TLabel
      Left = 162
      Top = 0
      Width = 7
      Height = 16
      Caption = '5'
    end
    object Label10: TLabel
      Left = 181
      Top = 0
      Width = 7
      Height = 16
      Caption = '6'
    end
    object Label11: TLabel
      Left = 202
      Top = 0
      Width = 7
      Height = 16
      Caption = '7'
    end
    object Label13: TLabel
      Left = 10
      Top = 10
      Width = 44
      Height = 16
      Caption = 'Startbit:'
    end
    object Label14: TLabel
      Left = 217
      Top = 7
      Width = 46
      Height = 16
      Caption = 'nrofbits:'
    end
    object RadioButton1: TRadioButton
      Left = 59
      Top = 15
      Width = 21
      Height = 21
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButton2: TRadioButton
      Left = 79
      Top = 15
      Width = 21
      Height = 21
      TabOrder = 1
    end
    object RadioButton3: TRadioButton
      Left = 98
      Top = 15
      Width = 21
      Height = 21
      TabOrder = 2
    end
    object RadioButton4: TRadioButton
      Left = 118
      Top = 15
      Width = 21
      Height = 21
      TabOrder = 3
    end
    object RadioButton5: TRadioButton
      Left = 138
      Top = 15
      Width = 21
      Height = 21
      TabOrder = 4
    end
    object RadioButton6: TRadioButton
      Left = 158
      Top = 15
      Width = 20
      Height = 21
      TabOrder = 5
    end
    object RadioButton7: TRadioButton
      Left = 177
      Top = 15
      Width = 21
      Height = 21
      TabOrder = 6
    end
    object RadioButton8: TRadioButton
      Left = 197
      Top = 15
      Width = 21
      Height = 21
      TabOrder = 7
    end
    object Edit2: TEdit
      Left = 266
      Top = 5
      Width = 45
      Height = 24
      TabOrder = 8
      Text = '1'
    end
  end
  object NewAddress: TEdit
    Left = 79
    Top = 5
    Width = 129
    Height = 24
    TabOrder = 0
    Text = '00400000'
    OnKeyPress = NewAddressKeyPress
  end
  object cbPointer: TCheckBox
    Left = 23
    Top = 98
    Width = 71
    Height = 21
    Alignment = taLeftJustify
    BiDiMode = bdLeftToRight
    Caption = 'Pointer'
    ParentBiDiMode = False
    TabOrder = 7
    OnClick = cbPointerClick
  end
  object Button3: TButton
    Left = 10
    Top = 126
    Width = 90
    Height = 25
    Caption = 'Add pointer'
    TabOrder = 8
    Visible = False
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 108
    Top = 126
    Width = 90
    Height = 25
    Caption = 'Remove'
    TabOrder = 9
    Visible = False
    OnClick = Button4Click
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 240
    Top = 88
  end
end
