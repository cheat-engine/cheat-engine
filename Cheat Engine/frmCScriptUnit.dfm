object frmCScript: TfrmCScript
  Left = 215
  Top = 219
  Width = 512
  Height = 389
  Caption = 'C Script console'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 311
    Width = 504
    Height = 24
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = Panel1Resize
    object edtCommand: TEdit
      Left = 0
      Top = 0
      Width = 497
      Height = 21
      TabOrder = 0
      OnKeyDown = edtCommandKeyDown
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 504
    Height = 311
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object MainMenu1: TMainMenu
    Left = 128
    Top = 40
    object File1: TMenuItem
      Caption = 'File'
      object Savelog1: TMenuItem
        Caption = 'Save log'
      end
      object Loadandparsescript1: TMenuItem
        Caption = 'Load and parse script'
        ShortCut = 16463
      end
      object Saveinput1: TMenuItem
        Caption = 'Save all input'
        ShortCut = 16467
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object Verbose1: TMenuItem
        Caption = 'Verbose'
        Checked = True
      end
      object Clearallpreviouscommands1: TMenuItem
        Caption = 'Clear all previous commands'
      end
    end
  end
end
