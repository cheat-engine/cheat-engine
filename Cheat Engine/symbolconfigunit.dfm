object frmSymbolhandler: TfrmSymbolhandler
  Left = 869
  Top = 258
  Width = 489
  Height = 338
  Caption = 'Symbol config'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 302
    Top = 0
    Width = 169
    Height = 293
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object Label3: TLabel
      Left = 32
      Top = 10
      Width = 124
      Height = 16
      Caption = 'Unique symbolname'
    end
    object Label2: TLabel
      Left = 63
      Top = 59
      Width = 51
      Height = 16
      Caption = 'Address'
    end
    object edtSymbolname: TEdit
      Left = 10
      Top = 30
      Width = 149
      Height = 21
      TabOrder = 0
    end
    object edtAddress: TEdit
      Left = 10
      Top = 79
      Width = 149
      Height = 21
      TabOrder = 1
    end
    object Button1: TButton
      Left = 37
      Top = 108
      Width = 92
      Height = 31
      Caption = 'Add symbol'
      TabOrder = 2
      OnClick = Button1Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 302
    Height = 293
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 302
      Height = 16
      Align = alTop
      Caption = 'Userdefined symbols:'
    end
    object ListView1: TListView
      Left = 0
      Top = 16
      Width = 302
      Height = 277
      Align = alClient
      Columns = <
        item
          Caption = 'Symbolname'
          Width = 123
        end
        item
          Caption = 'Address'
          Width = 98
        end
        item
          Caption = 'AllocSize'
          Width = 74
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
