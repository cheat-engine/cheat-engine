object StandAlone: TStandAlone
  Left = 788
  Top = 791
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Stand Alone game trainer builder:Filename.exe'
  ClientHeight = 125
  ClientWidth = 305
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
  object Button1: TButton
    Left = 128
    Top = 96
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 224
    Top = 96
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object RadioButton3: TRadioButton
    Left = 8
    Top = 0
    Width = 233
    Height = 17
    Caption = 'Open existing trainer made by Cheat Engine'
    TabOrder = 2
    OnClick = RadioButton4Click
  end
  object RadioButton4: TRadioButton
    Left = 8
    Top = 16
    Width = 145
    Height = 17
    Caption = 'Create a new trainer that'
    Checked = True
    TabOrder = 3
    TabStop = True
    OnClick = RadioButton4Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 40
    Width = 289
    Height = 49
    TabOrder = 4
    object RadioButton2: TRadioButton
      Left = 8
      Top = 8
      Width = 273
      Height = 17
      Caption = 'will run during the game/application, and modify data'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButton1: TRadioButton
      Left = 8
      Top = 24
      Width = 113
      Height = 17
      Caption = 'will patch some files'
      TabOrder = 1
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'exe'
    Filter = 'Exe files|*.exe'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 248
    Top = 8
  end
end
