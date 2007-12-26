object tlg: Ttlg
  Left = 198
  Top = 114
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'TLG:The Lame Game'
  ClientHeight = 348
  ClientWidth = 483
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 16
    Width = 25
    Height = 41
    Picture.Data = {
      07544269746D6170A2000000424DA2000000000000003E000000280000000D00
      0000190000000100010000000000640000000000000000000000020000000000
      000000000000FFFFFF00DF780000CF780000E6780000F4F80000F1F80000FBF8
      0000FBF80000FBF80000FBF80000FBF80000F1F80000CA780000BB9800007BE0
      0000F1F80000CE780000B1B80000AEB800006ED800007BD800007FD80000ADB8
      0000BFB80000CE780000F1F80000}
    Stretch = True
    Transparent = True
    Visible = False
    OnMouseDown = Image1MouseDown
  end
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 31
    Height = 13
    Caption = 'Score:'
  end
  object Label2: TLabel
    Left = 32
    Top = 0
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Timer1: TTimer
    Interval = 3000
    OnTimer = Timer1Timer
    Left = 136
    Top = 48
  end
end
