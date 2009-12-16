object frmPEInfo: TfrmPEInfo
  Left = 546
  Top = 609
  Width = 485
  Height = 318
  Caption = 'Portable Executable (PE) Info'
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
  object GroupBox2: TGroupBox
    Left = 177
    Top = 0
    Width = 290
    Height = 273
    Align = alClient
    Caption = 'Info'
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 2
      Top = 18
      Width = 286
      Height = 253
      ActivePage = TabSheet1
      Align = alClient
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'All'
        object PEItv: TTreeView
          Left = 0
          Top = 0
          Width = 278
          Height = 222
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
          Width = 395
          Height = 294
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
          Width = 395
          Height = 294
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
          Width = 395
          Height = 294
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
    Width = 177
    Height = 273
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object LoadButton: TSpeedButton
      Left = 59
      Top = 201
      Width = 31
      Height = 30
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
      Left = 10
      Top = 236
      Width = 88
      Height = 16
      Caption = 'No file opened'
    end
    object Label2: TLabel
      Left = 10
      Top = 256
      Width = 50
      Height = 16
      Caption = 'address'
    end
    object GroupBox1: TGroupBox
      Left = 10
      Top = 20
      Width = 161
      Height = 178
      Caption = 'MZ-Start'
      TabOrder = 0
      object edtAddress: TEdit
        Left = 10
        Top = 20
        Width = 107
        Height = 21
        TabOrder = 0
      end
      object modulelist: TListBox
        Left = 10
        Top = 49
        Width = 141
        Height = 120
        ItemHeight = 16
        TabOrder = 1
        OnClick = modulelistClick
      end
      object Button1: TButton
        Left = 121
        Top = 20
        Width = 30
        Height = 21
        Caption = 'Info'
        TabOrder = 2
        OnClick = Button1Click
      end
    end
    object RadioButton1: TRadioButton
      Left = 10
      Top = 207
      Width = 48
      Height = 21
      Caption = 'File'
      TabOrder = 1
    end
    object RadioButton2: TRadioButton
      Left = 10
      Top = 0
      Width = 139
      Height = 21
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
