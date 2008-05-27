object frmSymbolhandler: TfrmSymbolhandler
  Left = 550
  Top = 342
  Width = 389
  Height = 282
  Caption = 'Symbol config'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 244
    Top = 0
    Width = 137
    Height = 248
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object Label3: TLabel
      Left = 26
      Top = 8
      Width = 95
      Height = 13
      Caption = 'Unique symbolname'
    end
    object Label2: TLabel
      Left = 51
      Top = 48
      Width = 38
      Height = 13
      Caption = 'Address'
    end
    object edtSymbolname: TEdit
      Left = 8
      Top = 24
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object edtAddress: TEdit
      Left = 8
      Top = 64
      Width = 121
      Height = 21
      TabOrder = 1
    end
    object Button1: TButton
      Left = 30
      Top = 88
      Width = 75
      Height = 25
      Caption = 'Add symbol'
      TabOrder = 2
      OnClick = Button1Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 244
    Height = 248
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 244
      Height = 13
      Align = alTop
      Caption = 'Userdefined symbols:'
    end
    object ListView1: TListView
      Left = 0
      Top = 13
      Width = 244
      Height = 235
      Align = alClient
      Columns = <
        item
          Caption = 'Symbolname'
          Width = 100
        end
        item
          Caption = 'Address'
          Width = 80
        end
        item
          Caption = 'AllocSize'
          Width = 60
        end>
      ReadOnly = True
      RowSelect = True
      PopupMenu = PopupMenu1
      TabOrder = 0
      ViewStyle = vsReport
      OnClick = ListView1Click
      OnDblClick = Delete1Click
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 72
    Top = 80
    object Delete1: TMenuItem
      Caption = 'Delete'
      Default = True
      OnClick = Delete1Click
    end
  end
end
