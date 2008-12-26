object frmChangedAddresses: TfrmChangedAddresses
  Left = 942
  Top = 241
  Width = 255
  Height = 280
  Caption = 'Changed Addresses'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblInfo: TLabel
    Left = 0
    Top = 0
    Width = 247
    Height = 26
    Align = alTop
    Caption = 
      'The following addresses have been changed by the code you select' +
      'ed'
    WordWrap = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 205
    Width = 247
    Height = 41
    Align = alBottom
    TabOrder = 0
    object OKButton: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Stop'
      Default = True
      TabOrder = 0
      OnClick = OKButtonClick
    end
    object cbDisplayType: TComboBox
      Left = 96
      Top = 8
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 2
      TabOrder = 1
      Text = '4 Bytes'
      Items.Strings = (
        'Byte'
        '2 Bytes'
        '4 Bytes'
        'Single'
        'Double')
    end
  end
  object Changedlist: TListView
    Left = 0
    Top = 26
    Width = 247
    Height = 179
    Align = alClient
    Columns = <
      item
        Caption = 'Address'
        Width = 100
      end
      item
        AutoSize = True
        Caption = 'Value'
      end>
    RowSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = ChangedlistDblClick
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 56
    Top = 64
  end
  object PopupMenu1: TPopupMenu
    Left = 40
    Top = 104
    object Showregisterstates1: TMenuItem
      Caption = 'Show register states'
      OnClick = Showregisterstates1Click
    end
  end
end
