object Comments: TComments
  Left = 203
  Top = 119
  Width = 300
  Height = 240
  HelpContext = 11
  BorderIcons = [biMaximize]
  Caption = 'Comments'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    292
    206)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 292
    Height = 172
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 112
    Top = 177
    Width = 68
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'OK'
    TabOrder = 1
    OnClick = Button1Click
  end
end
