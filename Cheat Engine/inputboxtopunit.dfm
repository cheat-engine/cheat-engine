object InputboxTop: TInputboxTop
  Left = 624
  Top = 291
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 66
  ClientWidth = 283
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 283
    Height = 13
    Align = alTop
    Alignment = taCenter
    Caption = 'Label1'
  end
  object Edit1: TEdit
    Left = 5
    Top = 16
    Width = 273
    Height = 21
    TabOrder = 0
    Text = 'edit1'
  end
  object Button1: TButton
    Left = 64
    Top = 40
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 144
    Top = 40
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
