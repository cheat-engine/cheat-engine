object AdvancedOptions: TAdvancedOptions
  Left = 772
  Top = 158
  Width = 415
  Height = 297
  Anchors = []
  Caption = 'Advanced Options'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 210
    Width = 397
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = Panel1Resize
    DesignSize = (
      397
      42)
    object Button1: TButton
      Left = 205
      Top = 6
      Width = 92
      Height = 31
      Anchors = [akBottom]
      Caption = 'OK'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button4: TButton
      Left = 0
      Top = 21
      Width = 100
      Height = 21
      Anchors = [akLeft, akBottom]
      Caption = 'Direct X-Mess'
      TabOrder = 1
      OnClick = Button4Click
    end
    object Button2: TButton
      Left = 392
      Top = 21
      Width = 100
      Height = 21
      Anchors = [akRight, akBottom]
      Caption = 'OpenGL Mess'
      TabOrder = 2
      Visible = False
      OnClick = Button2Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 397
    Height = 38
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = Panel2Resize
    object Pausebutton: TSpeedButton
      Left = 42
      Top = 4
      Width = 31
      Height = 30
      Hint = 'Pause the game'
      AllowAllUp = True
      GroupIndex = 1
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000C40E0000C40E00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333333333333333333333333333333333333FFFFFFFFFFFFF3300000000000
        003337777777777777F330F777777777703337F33333333337F330F333333333
        703337F33333333337F330F333333333703337F333FF3FF337F330F330030033
        703337F3377F77F337F330F330030033703337F3377F77F337F330F330030033
        703337F3377F77F337F330F330030033703337F3377F77F337F330F330030033
        703337F33773773337F330F333333333703337F33333333337F330F333333333
        703337F33333333337F330FFFFFFFFFFF03337FFFFFFFFFFF7F3300000000000
        0033377777777777773333333333333333333333333333333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = PausebuttonClick
      OnMouseMove = PausebuttonMouseMove
    end
    object SaveButton: TSpeedButton
      Left = 2
      Top = 4
      Width = 31
      Height = 30
      Hint = 'Create a standalone trainer'
      Glyph.Data = {
        CA020000424DCA0200000000000036000000280000000E0000000F0000000100
        18000000000094020000C40E0000C40E00000000000000000000C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C00000C0C0C00000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000084840084840000000000
        00000000000000000000000000C6C6C6C6C6C600000000848400000000000000
        00008484008484000000000000000000000000000000000000C6C6C6C6C6C600
        0000008484000000000000000000848400848400000000000000000000000000
        0000000000C6C6C6C6C6C6000000008484000000000000000000848400848400
        0000000000000000000000000000000000000000000000000000008484000000
        0000000000008484008484008484008484008484008484008484008484008484
        0084840084840084840000000000000000008484008484000000000000000000
        0000000000000000000000000000000084840084840000000000000000008484
        000000C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C60000000084
        840000000000000000008484000000C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6
        C6C6C6C6C6C6C60000000084840000000000000000008484000000C6C6C6C6C6
        C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C600000000848400000000000000
        00008484000000C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C600
        00000084840000000000000000008484000000C6C6C6C6C6C6C6C6C6C6C6C6C6
        C6C6C6C6C6C6C6C6C6C6C60000000000000000000000000000008484000000C6
        C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6000000C6C6C6000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000}
      ParentShowHint = False
      ShowHint = True
      OnClick = SaveButtonClick
    end
    object Label1: TLabel
      Left = 166
      Top = 20
      Width = 55
      Height = 16
      Caption = 'Code list:'
    end
  end
  object Codelist2: TListView
    Left = 0
    Top = 38
    Width = 397
    Height = 172
    Align = alClient
    Columns = <
      item
        Caption = 'Address'
        MinWidth = 10
        Width = 120
      end
      item
        AutoSize = True
        Caption = 'Name'
        MinWidth = 10
      end>
    ColumnClick = False
    HideSelection = False
    MultiSelect = True
    RowSelect = True
    PopupMenu = PopupMenu2
    TabOrder = 2
    ViewStyle = vsReport
    OnDblClick = Codelist2DblClick
  end
  object PopupMenu2: TPopupMenu
    OnPopup = PopupMenu2Popup
    Left = 112
    Top = 104
    object Openthedisassemblerhere1: TMenuItem
      Caption = 'Open the disassembler at this location'
      Default = True
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object CC1: TMenuItem
      Caption = 'Replace with code that does nothing'
      OnClick = CC1Click
    end
    object CC2: TMenuItem
      Caption = 'Restore with original code'
      OnClick = CC2Click
    end
    object Findthiscodeinsideabinaryfile1: TMenuItem
      Caption = 'Find this code inside a file'
      OnClick = Findthiscodeinsideabinaryfile1Click
    end
    object Findoutwhatthiscodechanges1: TMenuItem
      Caption = 'Find out what addresses this code writes to'
      Visible = False
      OnClick = Findoutwhatthiscodechanges1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Rename1: TMenuItem
      Caption = 'Rename'
      OnClick = Rename1Click
    end
    object Remove1: TMenuItem
      Caption = 'Remove from list'
      ShortCut = 46
      OnClick = Remove1Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Replaceall1: TMenuItem
      Caption = 'Replace all'
      OnClick = Replaceall1Click
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'exe'
    Filter = 'Exefile (*.exe)|*.exe'
    FilterIndex = 0
    Options = [ofFileMustExist, ofEnableSizing]
    Title = 'Select the file you want to search'
    Left = 24
    Top = 88
  end
  object SaveDialog1: TSaveDialog
    Filter = 'Standalone trainer (*.exe)|*.exe'
    Left = 88
    Top = 184
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 104
    Top = 8
  end
end
