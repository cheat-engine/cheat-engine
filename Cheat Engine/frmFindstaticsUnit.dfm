object frmFindStatics: TfrmFindStatics
  Left = 631
  Top = 375
  BorderStyle = bsToolWindow
  Caption = 'Find static addresses'
  ClientHeight = 320
  ClientWidth = 351
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 303
    Width = 351
    Height = 17
    Align = alBottom
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 270
    Top = 0
    Width = 81
    Height = 284
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 40
      Width = 23
      Height = 13
      Caption = 'From'
    end
    object Label2: TLabel
      Left = 8
      Top = 80
      Width = 16
      Height = 13
      Caption = 'To:'
    end
    object Label4: TLabel
      Left = 3
      Top = 136
      Width = 73
      Height = 13
      Caption = 'Filter addresses'
    end
    object Label5: TLabel
      Left = 8
      Top = 152
      Width = 23
      Height = 13
      Caption = 'From'
    end
    object Label6: TLabel
      Left = 8
      Top = 192
      Width = 16
      Height = 13
      Caption = 'To:'
    end
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 65
      Height = 25
      Caption = 'Stopping...'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Edit1: TEdit
      Left = 8
      Top = 56
      Width = 65
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 8
      TabOrder = 1
      Text = '00401000'
    end
    object Edit2: TEdit
      Left = 8
      Top = 96
      Width = 65
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 8
      TabOrder = 2
      Text = '00700000'
    end
    object Edit3: TEdit
      Left = 8
      Top = 168
      Width = 65
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 8
      TabOrder = 3
      Text = '00400000'
    end
    object Edit4: TEdit
      Left = 8
      Top = 208
      Width = 65
      Height = 21
      CharCase = ecUpperCase
      MaxLength = 8
      TabOrder = 4
      Text = '7FFFFFFF'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 284
    Width = 351
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      351
      19)
    object CheckBox1: TCheckBox
      Left = 0
      Top = 0
      Width = 351
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Only disassemble and check executable code'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 270
    Height = 284
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    object ListView1: TListView
      Left = 0
      Top = 0
      Width = 270
      Height = 284
      Align = alClient
      Columns = <
        item
          Caption = 'Address'
          Width = 80
        end
        item
          Alignment = taCenter
          Caption = 'Pointer?'
          Width = 85
        end
        item
          Caption = 'Reference count'
          Width = 100
        end>
      ReadOnly = True
      RowSelect = True
      SortType = stText
      TabOrder = 0
      ViewStyle = vsReport
      OnColumnClick = ListView1ColumnClick
      OnDblClick = ListView1DblClick
    end
  end
end
