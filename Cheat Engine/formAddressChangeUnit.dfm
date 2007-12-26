object formAddressChange: TformAddressChange
  Left = 764
  Top = 476
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Change address'
  ClientHeight = 100
  ClientWidth = 281
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    281
    100)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 106
    Height = 13
    Caption = 'Give the new address:'
  end
  object editAddress: TEdit
    Left = 8
    Top = 24
    Width = 137
    Height = 21
    TabOrder = 0
    OnKeyPress = editAddressKeyPress
  end
  object Button1: TButton
    Left = 8
    Top = 72
    Width = 65
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 80
    Top = 72
    Width = 65
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object cbPointer: TCheckBox
    Left = 8
    Top = 48
    Width = 57
    Height = 17
    Caption = 'Pointer'
    TabOrder = 3
    OnClick = cbPointerClick
  end
  object BitPanel: TPanel
    Left = 152
    Top = 0
    Width = 129
    Height = 44
    BevelOuter = bvNone
    TabOrder = 4
    Visible = False
    object Label4: TLabel
      Left = 3
      Top = 16
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label5: TLabel
      Left = 20
      Top = 16
      Width = 6
      Height = 13
      Caption = '1'
    end
    object Label6: TLabel
      Left = 35
      Top = 16
      Width = 6
      Height = 13
      Caption = '2'
    end
    object Label7: TLabel
      Left = 52
      Top = 16
      Width = 6
      Height = 13
      Caption = '3'
    end
    object Label8: TLabel
      Left = 68
      Top = 16
      Width = 6
      Height = 13
      Caption = '4'
    end
    object Label9: TLabel
      Left = 84
      Top = 16
      Width = 6
      Height = 13
      Caption = '5'
    end
    object Label10: TLabel
      Left = 99
      Top = 16
      Width = 6
      Height = 13
      Caption = '6'
    end
    object Label11: TLabel
      Left = 116
      Top = 16
      Width = 6
      Height = 13
      Caption = '7'
    end
    object Label2: TLabel
      Left = 48
      Top = 0
      Width = 36
      Height = 13
      Caption = 'Startbit:'
    end
    object RadioButton1: TRadioButton
      Left = 0
      Top = 28
      Width = 17
      Height = 17
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButton2: TRadioButton
      Left = 16
      Top = 28
      Width = 17
      Height = 17
      TabOrder = 1
    end
    object RadioButton3: TRadioButton
      Left = 32
      Top = 28
      Width = 17
      Height = 17
      TabOrder = 2
    end
    object RadioButton4: TRadioButton
      Left = 48
      Top = 28
      Width = 17
      Height = 17
      TabOrder = 3
    end
    object RadioButton5: TRadioButton
      Left = 64
      Top = 28
      Width = 17
      Height = 17
      TabOrder = 4
    end
    object RadioButton6: TRadioButton
      Left = 80
      Top = 28
      Width = 17
      Height = 17
      TabOrder = 5
    end
    object RadioButton7: TRadioButton
      Left = 96
      Top = 28
      Width = 17
      Height = 17
      TabOrder = 6
    end
    object RadioButton8: TRadioButton
      Left = 112
      Top = 28
      Width = 17
      Height = 17
      TabOrder = 7
    end
  end
  object Button3: TButton
    Left = 8
    Top = 70
    Width = 73
    Height = 21
    Caption = 'Add pointer'
    TabOrder = 5
    Visible = False
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 88
    Top = 70
    Width = 73
    Height = 21
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
