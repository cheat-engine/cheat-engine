object frmPasteTableentry: TfrmPasteTableentry
  Left = 721
  Top = 435
  BorderStyle = bsSingle
  Caption = 'Paste table entries'
  ClientHeight = 139
  ClientWidth = 294
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
  object GroupBox1: TGroupBox
    Left = 8
    Top = 1
    Width = 137
    Height = 105
    Caption = 'Description'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 56
      Height = 13
      Caption = 'Text to find:'
    end
    object Label2: TLabel
      Left = 8
      Top = 56
      Width = 65
      Height = 13
      Caption = 'Replace with:'
    end
    object edtFind: TEdit
      Left = 8
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object edtReplace: TEdit
      Left = 8
      Top = 72
      Width = 121
      Height = 21
      TabOrder = 1
    end
  end
  object GroupBox2: TGroupBox
    Left = 152
    Top = 1
    Width = 137
    Height = 105
    Caption = 'Address'
    TabOrder = 1
    object Label3: TLabel
      Left = 8
      Top = 16
      Width = 89
      Height = 13
      Caption = 'Adjust address by: '
    end
    object edtOffset: TEdit
      Left = 8
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 0
      Text = '0'
    end
  end
  object Button1: TButton
    Left = 69
    Top = 111
    Width = 75
    Height = 25
    Caption = 'Paste'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object Button2: TButton
    Left = 152
    Top = 111
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
