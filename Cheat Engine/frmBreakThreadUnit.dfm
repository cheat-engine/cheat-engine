object frmbreakthread: Tfrmbreakthread
  Left = 371
  Top = 214
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'frmbreakthread'
  ClientHeight = 166
  ClientWidth = 357
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 5
    Top = 0
    Width = 345
    Height = 13
    Caption = 
      'This process has more than 1 thread. Select the thread you wish ' +
      'to break'
  end
  object Threadlistbox: TListBox
    Left = 5
    Top = 16
    Width = 347
    Height = 113
    ItemHeight = 13
    TabOrder = 0
  end
  object Button1: TButton
    Left = 96
    Top = 136
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 192
    Top = 136
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
