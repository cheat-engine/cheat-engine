object frmTrainerDesigner: TfrmTrainerDesigner
  Left = 361
  Top = 566
  Width = 618
  Height = 415
  Caption = 'Trainer Designer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  HelpFile = '14'
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object PaintBox1: TPaintBox
    Left = 0
    Top = 0
    Width = 600
    Height = 370
    Align = alClient
    OnMouseDown = PaintBox1MouseDown
    OnMouseMove = PaintBox1MouseMove
    OnMouseUp = PaintBox1MouseUp
  end
  object PopupMenu1: TPopupMenu
    Left = 72
    Top = 32
    object Bringtofront1: TMenuItem
      Caption = 'Bring to front'
      OnClick = Bringtofront1Click
    end
    object Sendtoback1: TMenuItem
      Caption = 'Send to back'
      OnClick = Sendtoback1Click
    end
  end
end
