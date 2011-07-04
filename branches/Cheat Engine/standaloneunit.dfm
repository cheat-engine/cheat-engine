object StandAlone: TStandAlone
  Left = 640
  Top = 495
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Stand Alone game trainer builder:Filename.exe'
  ClientHeight = 154
  ClientWidth = 375
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 16
  object Button1: TButton
    Left = 158
    Top = 118
    Width = 92
    Height = 31
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 276
    Top = 118
    Width = 92
    Height = 31
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object RadioButton3: TRadioButton
    Left = 10
    Top = 0
    Width = 287
    Height = 21
    Caption = 'Open existing trainer made by Cheat Engine'
    TabOrder = 2
    OnClick = RadioButton4Click
  end
  object RadioButton4: TRadioButton
    Left = 10
    Top = 20
    Width = 178
    Height = 21
    Caption = 'Create a new trainer that'
    Checked = True
    TabOrder = 3
    TabStop = True
    OnClick = RadioButton4Click
  end
  object GroupBox1: TGroupBox
    Left = 10
    Top = 49
    Width = 356
    Height = 61
    TabOrder = 4
    object RadioButton2: TRadioButton
      Left = 10
      Top = 10
      Width = 336
      Height = 21
      Caption = 'will run during the game/application, and modify data'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbPatcher: TRadioButton
      Left = 10
      Top = 30
      Width = 139
      Height = 20
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
