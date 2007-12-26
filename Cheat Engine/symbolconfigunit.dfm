object frmSymbolhandler: TfrmSymbolhandler
  Left = 550
  Top = 342
  Width = 339
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
  object Label1: TLabel
    Left = 8
    Top = 0
    Width = 100
    Height = 13
    Caption = 'Userdefined symbols:'
  end
  object Label3: TLabel
    Left = 200
    Top = 8
    Width = 95
    Height = 13
    Caption = 'Unique symbolname'
  end
  object Label2: TLabel
    Left = 200
    Top = 48
    Width = 38
    Height = 13
    Caption = 'Address'
  end
  object Button1: TButton
    Left = 200
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Add symbol'
    TabOrder = 0
    OnClick = Button1Click
  end
  object edtSymbolname: TEdit
    Left = 200
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object edtAddress: TEdit
    Left = 200
    Top = 64
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object ListView1: TListView
    Left = 8
    Top = 16
    Width = 185
    Height = 225
    Columns = <
      item
        Caption = 'Symbolname'
        Width = 100
      end
      item
        Caption = 'Address'
        Width = 80
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 3
    ViewStyle = vsReport
    OnClick = ListView1Click
    OnDblClick = ListView1DblClick
  end
end
