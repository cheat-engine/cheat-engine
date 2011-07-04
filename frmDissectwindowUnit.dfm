object frmdissectWindow: TfrmdissectWindow
  Left = 350
  Top = 361
  Width = 303
  Height = 296
  Caption = 'Dissect Windows'
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
  object TreeView1: TTreeView
    Left = 0
    Top = 0
    Width = 295
    Height = 177
    Align = alClient
    HideSelection = False
    Indent = 19
    ReadOnly = True
    TabOrder = 0
  end
  object Button1: TButton
    Left = 296
    Top = 224
    Width = 75
    Height = 25
    Caption = 'More Info'
    TabOrder = 1
    Visible = False
    OnClick = Button1Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 177
    Width = 295
    Height = 85
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      295
      85)
    object Button2: TButton
      Left = 8
      Top = 8
      Width = 89
      Height = 17
      Caption = 'Toggle visible'
      TabOrder = 0
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 104
      Top = 8
      Width = 89
      Height = 17
      Caption = 'Close window'
      TabOrder = 1
      OnClick = Button3Click
    end
    object Button5: TButton
      Left = 112
      Top = 56
      Width = 75
      Height = 25
      Anchors = [akTop]
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 2
    end
    object Button4: TButton
      Left = 86
      Top = 32
      Width = 129
      Height = 17
      Anchors = [akTop]
      Caption = 'Capture Timer Messages'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button6: TButton
      Left = 200
      Top = 8
      Width = 89
      Height = 17
      Caption = 'Change text'
      TabOrder = 4
      OnClick = Button6Click
    end
  end
end
