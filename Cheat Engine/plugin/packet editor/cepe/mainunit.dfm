object mainform: Tmainform
  Left = 333
  Top = 434
  Width = 676
  Height = 391
  Caption = 'Crappy Packet Editor Doh!'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 161
    Width = 668
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object Panel1: TPanel
    Left = 0
    Top = 164
    Width = 668
    Height = 132
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 0
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 666
      Height = 41
      Align = alTop
      BevelInner = bvSpace
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 0
        Width = 34
        Height = 13
        Caption = 'Socket'
      end
      object Label2: TLabel
        Left = 88
        Top = 0
        Width = 20
        Height = 13
        Caption = 'Size'
      end
      object edtSocket: TEdit
        Left = 8
        Top = 16
        Width = 65
        Height = 21
        TabOrder = 0
        Text = '-1'
      end
      object edtSize: TEdit
        Left = 88
        Top = 16
        Width = 57
        Height = 21
        TabOrder = 1
        Text = '0'
      end
      object Button3: TButton
        Left = 152
        Top = 18
        Width = 25
        Height = 17
        Caption = 'set'
        TabOrder = 2
        OnClick = Button3Click
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 296
    Width = 668
    Height = 41
    Align = alBottom
    TabOrder = 1
    object Label3: TLabel
      Left = 130
      Top = 14
      Width = 34
      Height = 13
      Caption = 'Time(s)'
    end
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Send'
      TabOrder = 0
      OnClick = Button1Click
    end
    object edtTimes: TEdit
      Left = 88
      Top = 10
      Width = 33
      Height = 21
      TabOrder = 1
      Text = '1'
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 668
    Height = 161
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object ListView1: TListView
      Left = 0
      Top = 0
      Width = 668
      Height = 161
      Align = alClient
      Columns = <
        item
          Caption = 'S/R'
        end
        item
          Caption = 'Socket'
        end
        item
          Caption = 'Length'
        end
        item
          Caption = 'From'
          Width = 150
        end
        item
          Caption = 'To'
          Width = 150
        end
        item
          Caption = 'Type'
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = ListView1Change
    end
  end
  object MainMenu1: TMainMenu
    Left = 152
    Top = 32
    object File1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Caption = 'Open'
      end
      object Save1: TMenuItem
        Caption = 'Save'
      end
    end
    object Filters1: TMenuItem
      Caption = 'Filters'
      object Newfilter1: TMenuItem
        Caption = 'New filter'
      end
      object N1: TMenuItem
        Caption = '-'
      end
    end
  end
end
