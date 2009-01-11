object frmEventLog: TfrmEventLog
  Left = 511
  Top = 111
  Width = 561
  Height = 289
  Caption = 'Debug event log'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 553
    Height = 214
    Align = alClient
    Columns = <
      item
        Caption = 'Timestamp'
        Width = 80
      end
      item
        Caption = 'PID'
      end
      item
        Caption = 'TID'
      end
      item
        Caption = 'Event'
        Width = 200
      end
      item
        AutoSize = True
        Caption = 'Details'
      end>
    RowSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 0
    ViewStyle = vsReport
  end
  object Panel1: TPanel
    Left = 0
    Top = 214
    Width = 553
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Button2: TButton
      Left = 88
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Stop logging'
      Enabled = False
      TabOrder = 0
      OnClick = Button2Click
    end
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Start logging'
      Default = True
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 80
    Top = 64
    object Clear1: TMenuItem
      Caption = 'Clear'
      OnClick = Clear1Click
    end
  end
end
