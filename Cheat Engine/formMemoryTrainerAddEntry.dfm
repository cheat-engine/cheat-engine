object FrmMemoryTrainerAddEntry: TFrmMemoryTrainerAddEntry
  Left = 468
  Top = 331
  Width = 455
  Height = 310
  Caption = 'Trainer maker: Add records'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 437
    Height = 186
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Code'
      object Label2: TLabel
        Left = 0
        Top = 0
        Width = 429
        Height = 28
        Align = alTop
        Caption = 
          'Select the address(es) from below that you want to get replaced ' +
          'with code that does nothing'
        WordWrap = True
      end
      object ListBox1: TListBox
        Left = 0
        Top = 28
        Width = 429
        Height = 130
        Align = alClient
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Addresses'
      ImageIndex = 1
      DesignSize = (
        429
        158)
      object Label5: TLabel
        Left = 0
        Top = 145
        Width = 429
        Height = 13
        Hint = 
          'Dynamic memory allocation is when a game doesnt store it'#39's stats' +
          ' at the same spot in memory'
        Align = alBottom
        Caption = 
          'Hint:Make use of pointers when the game uses dynamic memory allo' +
          'cation'
        ParentShowHint = False
        ShowHint = True
      end
      object btnDelete: TButton
        Left = 96
        Top = 113
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Delete'
        Enabled = False
        TabOrder = 0
        OnClick = btnDeleteClick
      end
      object Button3: TButton
        Left = 8
        Top = 113
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Add'
        TabOrder = 1
        OnClick = Button3Click
      end
      object Listview: TListView
        Left = 0
        Top = 0
        Width = 429
        Height = 109
        Align = alTop
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Description'
            Width = 100
          end
          item
            Alignment = taCenter
            Caption = 'Frozen'
          end
          item
            Alignment = taCenter
            Caption = 'Value'
          end
          item
            Caption = 'User Input'
            Width = 70
          end>
        HideSelection = False
        RowSelect = True
        TabOrder = 2
        ViewStyle = vsReport
        OnClick = ListviewClick
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 186
    Width = 437
    Height = 79
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Panel2: TPanel
      Left = 232
      Top = 41
      Width = 205
      Height = 38
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object Button2: TButton
        Left = 118
        Top = 8
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 0
      end
      object Button1: TButton
        Left = 38
        Top = 8
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        TabOrder = 1
        OnClick = Button1Click
      end
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 437
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Label3: TLabel
        Left = 8
        Top = 2
        Width = 53
        Height = 13
        Caption = 'Description'
      end
      object Label4: TLabel
        Left = 257
        Top = 2
        Width = 37
        Height = 13
        Caption = 'Hotkey:'
      end
      object editDescription: TEdit
        Left = 8
        Top = 18
        Width = 241
        Height = 21
        TabOrder = 0
      end
      object editHotkey: TEdit
        Left = 257
        Top = 18
        Width = 121
        Height = 21
        TabOrder = 1
        OnKeyDown = editHotkeyKeyDown
        OnKeyPress = editHotkeyKeyPress
      end
      object Button4: TButton
        Left = 376
        Top = 20
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 2
        OnClick = Button4Click
      end
    end
  end
end
