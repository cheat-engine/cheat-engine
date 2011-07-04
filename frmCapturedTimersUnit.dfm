object frmCapturedTimers: TfrmCapturedTimers
  Left = 489
  Top = 140
  Width = 507
  Height = 179
  Caption = 'frmCapturedTimers'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  DesignSize = (
    499
    152)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 499
    Height = 13
    Align = alTop
    Caption = 'The following WM_TIMER messages have been received'
  end
  object Button1: TButton
    Left = 210
    Top = 123
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = 'Close'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListView1: TListView
    Left = 0
    Top = 13
    Width = 499
    Height = 106
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Window Handle'
        Width = 110
      end
      item
        Caption = 'Window Title'
        Width = 120
      end
      item
        Caption = 'Window Class'
        Width = 90
      end
      item
        Caption = 'Timer ID'
        Width = 55
      end
      item
        Caption = 'TimerProc'
        Width = 70
      end
      item
        Caption = 'Count'
      end>
    HideSelection = False
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
  end
  object Edit1: TEdit
    Left = 0
    Top = 128
    Width = 65
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    Text = '10'
  end
  object Button2: TButton
    Left = 70
    Top = 130
    Width = 121
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Send timer messages'
    TabOrder = 3
    OnClick = Button2Click
  end
end
