object frmProcessInfo: TfrmProcessInfo
  Left = 432
  Top = 107
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Process/System Info'
  ClientHeight = 257
  ClientWidth = 206
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 63
    Top = 56
    Width = 24
    Height = 13
    Caption = 'CR3:'
  end
  object Label2: TLabel
    Left = 63
    Top = 72
    Width = 24
    Height = 13
    Caption = 'CR4:'
  end
  object Label3: TLabel
    Left = 32
    Top = 8
    Width = 55
    Height = 13
    Caption = 'PEProcess:'
  end
  object Label4: TLabel
    Left = 66
    Top = 136
    Width = 21
    Height = 13
    Caption = 'IDT:'
  end
  object lblcr3: TLabel
    Left = 96
    Top = 56
    Width = 25
    Height = 13
    Caption = 'lblcr3'
  end
  object lblcr4: TLabel
    Left = 96
    Top = 72
    Width = 25
    Height = 13
    Caption = 'lblcr4'
  end
  object lblPEPROCESS: TLabel
    Left = 96
    Top = 8
    Width = 75
    Height = 13
    Caption = 'lblPEPROCESS'
    OnDblClick = lblclick
  end
  object Label8: TLabel
    Left = 24
    Top = 24
    Width = 63
    Height = 13
    Caption = 'Valid Handle:'
  end
  object lblisvalid: TLabel
    Left = 96
    Top = 24
    Width = 39
    Height = 13
    Caption = 'lblisvalid'
  end
  object Label5: TLabel
    Left = 62
    Top = 88
    Width = 25
    Height = 13
    Caption = 'SDT:'
  end
  object lblSdt: TLabel
    Left = 96
    Top = 88
    Width = 26
    Height = 13
    Caption = 'lblSdt'
    OnDblClick = lblclick
  end
  object Label6: TLabel
    Left = 55
    Top = 104
    Width = 32
    Height = 13
    Caption = 'SSDT:'
  end
  object lblSsdt: TLabel
    Left = 96
    Top = 104
    Width = 31
    Height = 13
    Caption = 'lblSsdt'
    OnDblClick = lblclick
  end
  object Label7: TLabel
    Left = 61
    Top = 120
    Width = 26
    Height = 13
    Caption = 'GDT:'
  end
  object lblGdt: TLabel
    Left = 96
    Top = 120
    Width = 27
    Height = 13
    Caption = 'lblGdt'
    OnDblClick = lblclick
  end
  object Label9: TLabel
    Left = 63
    Top = 40
    Width = 24
    Height = 13
    Caption = 'CR0:'
  end
  object lblcr0: TLabel
    Left = 96
    Top = 40
    Width = 25
    Height = 13
    Caption = 'lblcr0'
  end
  object lbidt: TListBox
    Left = 96
    Top = 136
    Width = 81
    Height = 57
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = lbidtDblClick
  end
  object Button1: TButton
    Left = 72
    Top = 224
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 128
    Top = 200
    Width = 75
    Height = 17
    Caption = 'Threads>>'
    TabOrder = 2
    OnClick = Button2Click
  end
end
