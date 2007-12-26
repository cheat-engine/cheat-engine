object frmAPIhookTemplateSettings: TfrmAPIhookTemplateSettings
  Left = 1197
  Top = 494
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Api hook template'
  ClientHeight = 191
  ClientWidth = 266
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
  object Label1: TLabel
    Left = 8
    Top = 0
    Width = 95
    Height = 13
    Caption = 'Original api address:'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 82
    Height = 13
    Caption = 'New api address:'
  end
  object Label3: TLabel
    Left = 8
    Top = 80
    Width = 224
    Height = 13
    Caption = 'Address where to store the new original api call:'
  end
  object Label4: TLabel
    Left = 8
    Top = 96
    Width = 245
    Height = 13
    Caption = '(optional, in case your dll uses a variable for the call)'
  end
  object Label6: TLabel
    Left = 136
    Top = 16
    Width = 3
    Height = 13
  end
  object Label5: TLabel
    Left = 8
    Top = 176
    Width = 252
    Height = 13
    Caption = 'Note: The addresses can also be exportnames of dll'#39's'
    WordWrap = True
  end
  object Edit1: TEdit
    Left = 8
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 8
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object Edit3: TEdit
    Left = 8
    Top = 112
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object Button1: TButton
    Left = 53
    Top = 144
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object Button2: TButton
    Left = 139
    Top = 144
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
