object frmPEInfo: TfrmPEInfo
  Left = 546
  Top = 609
  Width = 485
  Height = 318
  Caption = 'Portable Executable (PE) Info'
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
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    Left = 144
    Top = 0
    Width = 333
    Height = 284
    Align = alClient
    Caption = 'Info'
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 2
      Top = 15
      Width = 329
      Height = 267
      ActivePage = TabSheet1
      Align = alClient
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'All'
        object PEItv: TTreeView
          Left = 0
          Top = 0
          Width = 321
          Height = 239
          Align = alClient
          Indent = 19
          TabOrder = 0
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Imports'
        ImageIndex = 1
        object lbImports: TListBox
          Left = 0
          Top = 0
          Width = 321
          Height = 239
          Align = alClient
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Exports'
        ImageIndex = 2
        object lbExports: TListBox
          Left = 0
          Top = 0
          Width = 321
          Height = 239
          Align = alClient
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'Base Relocations'
        ImageIndex = 3
        object lbBaseReloc: TListBox
          Left = 0
          Top = 0
          Width = 321
          Height = 239
          Align = alClient
          ItemHeight = 13
          TabOrder = 0
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 144
    Height = 284
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object LoadButton: TSpeedButton
      Left = 48
      Top = 163
      Width = 25
      Height = 25
      Glyph.Data = {
        D6020000424DD6020000000000003600000028000000100000000E0000000100
        180000000000A0020000C40E0000C40E00000000000000000000C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C000000000000000000000000000000000000000000000
        0000000000000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000000000
        008484008484008484008484008484008484008484008484008484000000C0C0
        C0C0C0C0C0C0C0C0C0C000000000FFFF00000000848400848400848400848400
        8484008484008484008484008484000000C0C0C0C0C0C0C0C0C0000000FFFFFF
        00FFFF0000000084840084840084840084840084840084840084840084840084
        84000000C0C0C0C0C0C000000000FFFFFFFFFF00FFFF00000000848400848400
        8484008484008484008484008484008484008484000000C0C0C0000000FFFFFF
        00FFFFFFFFFF00FFFF0000000000000000000000000000000000000000000000
        0000000000000000000000000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00
        FFFFFFFFFF00FFFF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000FFFFFF
        00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF000000C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C000000000FFFFFFFFFF00FFFF00000000000000000000
        0000000000000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000
        000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000
        00000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000000000C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000C0C0C0C0C0C0C0C0C00000
        00C0C0C0000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0000000000000000000C0C0C0C0C0C0C0C0C0C0C0C0}
      ParentShowHint = False
      ShowHint = True
      OnClick = LoadButtonClick
    end
    object Label1: TLabel
      Left = 8
      Top = 192
      Width = 69
      Height = 13
      Caption = 'No file opened'
    end
    object Label2: TLabel
      Left = 8
      Top = 208
      Width = 37
      Height = 13
      Caption = 'address'
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 16
      Width = 131
      Height = 145
      Caption = 'MZ-Start'
      TabOrder = 0
      object edtAddress: TEdit
        Left = 8
        Top = 16
        Width = 87
        Height = 21
        TabOrder = 0
      end
      object modulelist: TListBox
        Left = 8
        Top = 40
        Width = 115
        Height = 97
        ItemHeight = 13
        TabOrder = 1
        OnClick = modulelistClick
      end
      object Button1: TButton
        Left = 98
        Top = 16
        Width = 25
        Height = 17
        Caption = 'Info'
        TabOrder = 2
        OnClick = Button1Click
      end
    end
    object RadioButton1: TRadioButton
      Left = 8
      Top = 168
      Width = 39
      Height = 17
      Caption = 'File'
      TabOrder = 1
    end
    object RadioButton2: TRadioButton
      Left = 8
      Top = 0
      Width = 113
      Height = 17
      Caption = 'Memory'
      Checked = True
      TabOrder = 2
      TabStop = True
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Executables|*.exe;*.sys;*.dll'
    Options = [ofFileMustExist, ofEnableSizing]
    Left = 104
    Top = 168
  end
end
