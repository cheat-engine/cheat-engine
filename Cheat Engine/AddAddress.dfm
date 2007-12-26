object AddForm: TAddForm
  Left = 698
  Top = 634
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Add address'
  ClientHeight = 136
  ClientWidth = 443
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    443
    136)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 23
    Top = 8
    Width = 38
    Height = 13
    Caption = 'Address'
  end
  object Label2: TLabel
    Left = 38
    Top = 55
    Width = 24
    Height = 13
    Caption = 'Type'
  end
  object Label3: TLabel
    Left = 8
    Top = 32
    Width = 53
    Height = 13
    Caption = 'Description'
  end
  object ValuePanel: TPanel
    Left = 170
    Top = 50
    Width = 231
    Height = 27
    BevelOuter = bvNone
    TabOrder = 6
    Visible = False
    object Label12: TLabel
      Left = 0
      Top = 4
      Width = 89
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      BiDiMode = bdLeftToRight
      ParentBiDiMode = False
    end
    object Edit1: TEdit
      Left = 96
      Top = 2
      Width = 41
      Height = 21
      PopupMenu = MainForm.emptypopup
      TabOrder = 0
      Text = '1'
    end
    object cbUnicode: TCheckBox
      Left = 144
      Top = 4
      Width = 75
      Height = 17
      Caption = 'Unicode'
      TabOrder = 1
    end
  end
  object VarType: TComboBox
    Left = 64
    Top = 51
    Width = 105
    Height = 21
    Style = csDropDownList
    DropDownCount = 9
    ItemHeight = 13
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
    Left = 8
    Top = 101
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 96
    Top = 101
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Description: TEdit
    Left = 64
    Top = 28
    Width = 377
    Height = 21
    MaxLength = 50
    TabOrder = 1
    Text = 'No Description!'
  end
  object BitPanel: TPanel
    Left = 169
    Top = 50
    Width = 257
    Height = 27
    BevelOuter = bvNone
    TabOrder = 5
    Visible = False
    object Label4: TLabel
      Left = 51
      Top = 0
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label5: TLabel
      Left = 68
      Top = 0
      Width = 6
      Height = 13
      Caption = '1'
    end
    object Label6: TLabel
      Left = 83
      Top = 0
      Width = 6
      Height = 13
      Caption = '2'
    end
    object Label7: TLabel
      Left = 100
      Top = 0
      Width = 6
      Height = 13
      Caption = '3'
    end
    object Label8: TLabel
      Left = 116
      Top = 0
      Width = 6
      Height = 13
      Caption = '4'
    end
    object Label9: TLabel
      Left = 132
      Top = 0
      Width = 6
      Height = 13
      Caption = '5'
    end
    object Label10: TLabel
      Left = 147
      Top = 0
      Width = 6
      Height = 13
      Caption = '6'
    end
    object Label11: TLabel
      Left = 164
      Top = 0
      Width = 6
      Height = 13
      Caption = '7'
    end
    object Label13: TLabel
      Left = 8
      Top = 8
      Width = 36
      Height = 13
      Caption = 'Startbit:'
    end
    object Label14: TLabel
      Left = 176
      Top = 6
      Width = 37
      Height = 13
      Caption = 'nrofbits:'
    end
    object RadioButton1: TRadioButton
      Left = 48
      Top = 12
      Width = 17
      Height = 17
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButton2: TRadioButton
      Left = 64
      Top = 12
      Width = 17
      Height = 17
      TabOrder = 1
    end
    object RadioButton3: TRadioButton
      Left = 80
      Top = 12
      Width = 17
      Height = 17
      TabOrder = 2
    end
    object RadioButton4: TRadioButton
      Left = 96
      Top = 12
      Width = 17
      Height = 17
      TabOrder = 3
    end
    object RadioButton5: TRadioButton
      Left = 112
      Top = 12
      Width = 17
      Height = 17
      TabOrder = 4
    end
    object RadioButton6: TRadioButton
      Left = 128
      Top = 12
      Width = 17
      Height = 17
      TabOrder = 5
    end
    object RadioButton7: TRadioButton
      Left = 144
      Top = 12
      Width = 17
      Height = 17
      TabOrder = 6
    end
    object RadioButton8: TRadioButton
      Left = 160
      Top = 12
      Width = 17
      Height = 17
      TabOrder = 7
    end
    object Edit2: TEdit
      Left = 216
      Top = 4
      Width = 37
      Height = 21
      TabOrder = 8
      Text = '1'
    end
  end
  object NewAddress: TEdit
    Left = 64
    Top = 4
    Width = 105
    Height = 21
    TabOrder = 0
    Text = '00400000'
    OnKeyPress = NewAddressKeyPress
  end
  object cbPointer: TCheckBox
    Left = 19
    Top = 80
    Width = 57
    Height = 17
    Alignment = taLeftJustify
    BiDiMode = bdLeftToRight
    Caption = 'Pointer'
    ParentBiDiMode = False
    TabOrder = 7
    OnClick = cbPointerClick
  end
  object Button3: TButton
    Left = 8
    Top = 102
    Width = 73
    Height = 21
    Caption = 'Add pointer'
    TabOrder = 8
    Visible = False
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 88
    Top = 102
    Width = 73
    Height = 21
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
