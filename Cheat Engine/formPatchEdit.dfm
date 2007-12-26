object frmPatchEdit: TfrmPatchEdit
  Left = 272
  Top = 99
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'frmPatchEdit'
  ClientHeight = 115
  ClientWidth = 171
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 40
    Width = 28
    Height = 13
    Caption = 'Offset'
  end
  object Label2: TLabel
    Left = 128
    Top = 40
    Width = 33
    Height = 13
    Caption = 'Length'
  end
  object Label3: TLabel
    Left = 0
    Top = 0
    Width = 42
    Height = 13
    Caption = 'Filename'
  end
  object editOffset: TEdit
    Left = 0
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'editOffset'
    OnKeyPress = editOffsetKeyPress
  end
  object editLength: TEdit
    Left = 128
    Top = 56
    Width = 41
    Height = 21
    TabOrder = 2
    Text = 'editLength'
  end
  object editFilename: TEdit
    Left = 0
    Top = 16
    Width = 169
    Height = 21
    TabOrder = 0
    Text = 'editFilename'
  end
  object Button1: TButton
    Left = 0
    Top = 88
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 96
    Top = 88
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
