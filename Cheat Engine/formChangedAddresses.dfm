object frmChangedAddresses: TfrmChangedAddresses
  Left = 942
  Top = 241
  Width = 349
  Height = 293
  Caption = 'Changed Addresses'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object lblInfo: TLabel
    Left = 0
    Top = 0
    Width = 331
    Height = 32
    Align = alTop
    Caption = 
      'The following addresses have been changed by the code you select' +
      'ed'
    WordWrap = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 197
    Width = 331
    Height = 51
    Align = alBottom
    TabOrder = 0
    object OKButton: TButton
      Left = 10
      Top = 10
      Width = 92
      Height = 31
      Caption = 'Stop'
      Default = True
      TabOrder = 0
      OnClick = OKButtonClick
    end
    object cbDisplayType: TComboBox
      Left = 118
      Top = 10
      Width = 179
      Height = 24
      Style = csDropDownList
      ItemHeight = 16
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
    Top = 32
    Width = 331
    Height = 165
    Align = alClient
    Columns = <
      item
        Caption = 'Address'
        Width = 123
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
    object Browsethismemoryregion1: TMenuItem
      Caption = 'Browse this memory region'
      OnClick = Browsethismemoryregion1Click
    end
  end
end
