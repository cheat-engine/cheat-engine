object Style3: TStyle3
  Left = 497
  Top = 174
  BorderStyle = bsNone
  Caption = 'Style3'
  ClientHeight = 281
  ClientWidth = 281
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 280
    Height = 280
  end
  object Label1: TLabel
    Left = 8
    Top = 216
    Width = 35
    Height = 13
    Caption = 'Label1t'
    Transparent = True
    OnMouseMove = Image3MouseMove
  end
  object Image3: TImage
    Left = 0
    Top = 0
    Width = 281
    Height = 249
    OnMouseMove = Image3MouseMove
  end
  object Image4: TImage
    Left = 0
    Top = 248
    Width = 249
    Height = 33
    OnMouseMove = Image3MouseMove
  end
  object Image6: TImage
    Left = 272
    Top = 248
    Width = 9
    Height = 25
    OnMouseMove = Image3MouseMove
  end
  object Image2: TImage
    Left = 248
    Top = 272
    Width = 33
    Height = 9
    OnMouseMove = Image3MouseMove
  end
  object Image5: TImage
    Left = 248
    Top = 248
    Width = 25
    Height = 25
    Cursor = crHandPoint
    OnClick = Image5Click
  end
  object CheckListBox1: TCheckListBox
    Left = 40
    Top = 16
    Width = 201
    Height = 193
    OnClickCheck = CheckListBox1ClickCheck
    ItemHeight = 13
    TabOrder = 0
  end
end
