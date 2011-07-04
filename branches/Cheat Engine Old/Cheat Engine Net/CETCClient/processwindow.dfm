object ProcesWindow: TProcesWindow
  Left = 246
  Top = 130
  Width = 191
  Height = 322
  Caption = 'Proces Window'
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
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 183
    Height = 224
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = ListBox1DblClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 224
    Width = 183
    Height = 64
    Align = alBottom
    BevelInner = bvLowered
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 0
      OnClick = Button1Click
    end
    object btnProcessList: TButton
      Left = 40
      Top = 44
      Width = 49
      Height = 17
      Caption = 'Process'
      Enabled = False
      TabOrder = 1
      OnClick = btnProcessListClick
    end
    object btnWindowList: TButton
      Left = 88
      Top = 44
      Width = 49
      Height = 17
      Caption = 'Window'
      Enabled = False
      TabOrder = 2
      OnClick = btnWindowListClick
    end
    object Button4: TButton
      Left = 96
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      Enabled = False
      ModalResult = 2
      TabOrder = 3
    end
  end
end
