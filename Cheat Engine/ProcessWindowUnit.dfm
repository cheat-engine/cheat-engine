object ProcessWindow: TProcessWindow
  Left = 1009
  Top = 120
  Width = 248
  Height = 355
  HelpContext = 4
  BorderIcons = [biSystemMenu]
  Caption = 'Process List'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object ProcessList: TListBox
    Left = 0
    Top = 0
    Width = 240
    Height = 152
    Align = alClient
    ItemHeight = 13
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnClick = ProcessListClick
    OnDblClick = OKButtonClick
    OnKeyPress = ProcessListKeyPress
  end
  object Panel1: TPanel
    Left = 0
    Top = 152
    Width = 240
    Height = 169
    Align = alBottom
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 1
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 238
      Height = 167
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object Button6: TButton
        Left = 164
        Top = 120
        Width = 76
        Height = 17
        Caption = 'Process watch'
        TabOrder = 0
        OnClick = Button6Click
      end
      object Button2: TButton
        Left = 83
        Top = 120
        Width = 76
        Height = 17
        Caption = 'Window List'
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button1: TButton
        Left = 7
        Top = 120
        Width = 76
        Height = 17
        Caption = 'Process List'
        TabOrder = 2
        OnClick = Button1Click
      end
      object Button5: TButton
        Left = 32
        Top = 88
        Width = 177
        Height = 17
        Caption = 'Open file'
        TabOrder = 3
        OnClick = Button5Click
      end
      object Button3: TButton
        Left = 32
        Top = 64
        Width = 177
        Height = 17
        Caption = 'Create process'
        TabOrder = 4
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 32
        Top = 40
        Width = 177
        Height = 17
        Caption = 'Attach to process'
        TabOrder = 5
        OnClick = Button4Click
      end
      object CancelButton: TButton
        Left = 134
        Top = 8
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        TabOrder = 6
        OnClick = CancelButtonClick
      end
      object OKButton: TButton
        Left = 32
        Top = 8
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        TabOrder = 7
        OnClick = OKButtonClick
      end
      object btnProcessListLong: TButton
        Left = 56
        Top = 145
        Width = 129
        Height = 18
        Caption = 'Process List(long)'
        TabOrder = 8
        OnClick = btnProcessListLongClick
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'EXE'
    Filter = 'EXE files|*.exe'
  end
  object OpenDialog2: TOpenDialog
    Filter = 'All files (*.*)|*.*'
    Left = 224
  end
  object PopupMenu1: TPopupMenu
    Left = 112
    Top = 48
    object InputPIDmanually1: TMenuItem
      Caption = 'Input PID manually'
      ShortCut = 16464
      OnClick = InputPIDmanually1Click
    end
    object Filter1: TMenuItem
      Caption = 'Filter'
      ShortCut = 16454
      OnClick = Filter1Click
    end
  end
end
