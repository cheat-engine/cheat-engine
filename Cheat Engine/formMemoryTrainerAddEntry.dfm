object FrmMemoryTrainerAddEntry: TFrmMemoryTrainerAddEntry
  Left = 373
  Top = 499
  Width = 393
  Height = 335
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
  DesignSize = (
    385
    301)
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 62
    Top = 232
    Width = 31
    Height = 13
    Anchors = [akBottom]
    Caption = 'Effect:'
  end
  object Label4: TLabel
    Left = 200
    Top = 232
    Width = 37
    Height = 13
    Anchors = [akBottom]
    Caption = 'Hotkey:'
  end
  object Button1: TButton
    Left = 110
    Top = 275
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 200
    Top = 275
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object editDescription: TEdit
    Left = 62
    Top = 248
    Width = 121
    Height = 21
    Anchors = [akBottom]
    TabOrder = 2
  end
  object editHotkey: TEdit
    Left = 200
    Top = 248
    Width = 121
    Height = 21
    Anchors = [akBottom]
    TabOrder = 3
    OnKeyDown = editHotkeyKeyDown
    OnKeyPress = editHotkeyKeyPress
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 385
    Height = 230
    ActivePage = TabSheet2
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
    object TabSheet1: TTabSheet
      Caption = 'Code'
      object Label2: TLabel
        Left = 0
        Top = 0
        Width = 373
        Height = 26
        Caption = 
          'Select the address(es) from below that you want to get replaced ' +
          'with code that does nothing'
        WordWrap = True
      end
      object ListBox1: TListBox
        Left = 0
        Top = 32
        Width = 377
        Height = 170
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Addresses'
      ImageIndex = 1
      DesignSize = (
        377
        202)
      object Label5: TLabel
        Left = 0
        Top = 189
        Width = 377
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
        Top = 157
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
        Top = 157
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
        Width = 377
        Height = 153
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
  object Button4: TButton
    Left = 328
    Top = 248
    Width = 49
    Height = 17
    Caption = 'Clear'
    TabOrder = 5
    OnClick = Button4Click
  end
end
