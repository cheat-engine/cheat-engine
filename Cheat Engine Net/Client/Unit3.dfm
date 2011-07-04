object Waitform: TWaitform
  Left = 480
  Top = 219
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Please wait...'
  ClientHeight = 63
  ClientWidth = 115
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Animate1: TAnimate
    Left = 0
    Top = 0
    Width = 115
    Height = 63
    Align = alClient
    CommonAVI = aviFindComputer
    StopFrame = 8
  end
  object Timer1: TTimer
    Left = 24
    Top = 24
  end
end
