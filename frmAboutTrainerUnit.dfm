object frmAboutTrainer: TfrmAboutTrainer
  Left = 451
  Top = 233
  Width = 341
  Height = 214
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    333
    180)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 128
    Top = 151
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Button1'
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 333
    Height = 144
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'About trainer'
    TabOrder = 1
    object Label1: TLabel
      Left = 2
      Top = 15
      Width = 329
      Height = 13
      Align = alTop
      Caption = 'This box will hold the text for the about box.'
      WordWrap = True
    end
  end
end
