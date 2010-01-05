object Comments: TComments
  Left = 203
  Top = 119
  Width = 470
  Height = 378
  HelpContext = 11
  BorderIcons = [biMaximize]
  Caption = 'Comments'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 16
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 452
    Height = 292
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 292
    Width = 452
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = Panel1Resize
    object Button1: TButton
      Left = 11
      Top = 6
      Width = 412
      Height = 31
      Caption = 'OK'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
end
