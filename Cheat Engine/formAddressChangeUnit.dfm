object formAddressChange: TformAddressChange
  Left = 764
  Top = 476
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Change address'
  ClientHeight = 123
  ClientWidth = 346
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    346
    123)
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 10
    Top = 10
    Width = 132
    Height = 16
    Caption = 'Give the new address:'
  end
  object editAddress: TEdit
    Left = 10
    Top = 30
    Width = 168
    Height = 21
    TabOrder = 0
    OnKeyPress = editAddressKeyPress
  end
  object Button1: TButton
    Left = 10
    Top = 89
    Width = 80
    Height = 30
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 98
    Top = 89
    Width = 80
    Height = 30
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object cbPointer: TCheckBox
    Left = 10
    Top = 59
    Width = 70
    Height = 21
    Caption = 'Pointer'
    TabOrder = 3
    OnClick = cbPointerClick
  end
  object BitPanel: TPanel
    Left = 187
    Top = 0
    Width = 159
    Height = 54
    BevelOuter = bvNone
    TabOrder = 4
    Visible = False
    object Label4: TLabel
      Left = 4
      Top = 20
      Width = 7
      Height = 16
      Caption = '0'
    end
    object Label5: TLabel
      Left = 25
      Top = 20
      Width = 7
      Height = 16
      Caption = '1'
    end
    object Label6: TLabel
      Left = 43
      Top = 20
      Width = 7
      Height = 16
      Caption = '2'
    end
    object Label7: TLabel
      Left = 64
      Top = 20
      Width = 7
      Height = 16
      Caption = '3'
    end
    object Label8: TLabel
      Left = 84
      Top = 20
      Width = 7
      Height = 16
      Caption = '4'
    end
    object Label9: TLabel
      Left = 103
      Top = 20
      Width = 7
      Height = 16
      Caption = '5'
    end
    object Label10: TLabel
      Left = 122
      Top = 20
      Width = 7
      Height = 16
      Caption = '6'
    end
    object Label11: TLabel
      Left = 143
      Top = 20
      Width = 7
      Height = 16
      Caption = '7'
    end
    object Label2: TLabel
      Left = 59
      Top = 0
      Width = 44
      Height = 16
      Caption = 'Startbit:'
    end
    object RadioButton1: TRadioButton
      Left = 0
      Top = 34
      Width = 21
      Height = 21
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButton2: TRadioButton
      Left = 20
      Top = 34
      Width = 21
      Height = 21
      TabOrder = 1
    end
    object RadioButton3: TRadioButton
      Left = 39
      Top = 34
      Width = 21
      Height = 21
      TabOrder = 2
    end
    object RadioButton4: TRadioButton
      Left = 59
      Top = 34
      Width = 21
      Height = 21
      TabOrder = 3
    end
    object RadioButton5: TRadioButton
      Left = 79
      Top = 34
      Width = 21
      Height = 21
      TabOrder = 4
    end
    object RadioButton6: TRadioButton
      Left = 98
      Top = 34
      Width = 21
      Height = 21
      TabOrder = 5
    end
    object RadioButton7: TRadioButton
      Left = 118
      Top = 34
      Width = 21
      Height = 21
      TabOrder = 6
    end
    object RadioButton8: TRadioButton
      Left = 138
      Top = 34
      Width = 21
      Height = 21
      TabOrder = 7
    end
  end
  object Button3: TButton
    Left = 10
    Top = 86
    Width = 90
    Height = 26
    Caption = 'Add pointer'
    TabOrder = 5
    Visible = False
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 108
    Top = 86
    Width = 90
    Height = 26
    Caption = 'Remove'
    TabOrder = 6
    Visible = False
    OnClick = Button4Click
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 184
    Top = 56
  end
end
