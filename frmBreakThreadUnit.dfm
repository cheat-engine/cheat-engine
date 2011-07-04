object frmbreakthread: Tfrmbreakthread
  Left = 371
  Top = 214
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Break Thread'
  ClientHeight = 204
  ClientWidth = 439
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 6
    Top = 0
    Width = 428
    Height = 16
    Caption = 
      'This process has more than 1 thread. Select the thread you wish ' +
      'to break'
  end
  object Threadlistbox: TListBox
    Left = 6
    Top = 20
    Width = 427
    Height = 139
    ItemHeight = 16
    TabOrder = 0
    OnDblClick = ThreadlistboxDblClick
  end
  object Button1: TButton
    Left = 118
    Top = 167
    Width = 92
    Height = 31
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 236
    Top = 167
    Width = 93
    Height = 31
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
