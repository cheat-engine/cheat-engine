object frmSaveMemoryRegion: TfrmSaveMemoryRegion
  Left = 531
  Top = 369
  Width = 270
  Height = 278
  BorderIcons = [biSystemMenu]
  Caption = 'Save memoryregion'
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
  PixelsPerInch = 120
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 252
    Height = 185
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 252
      Height = 13
      Align = alTop
      Caption = 'Add the region(s) of memory you want to save'
      WordWrap = True
    end
    object lbRegions: TListBox
      Left = 0
      Top = 13
      Width = 145
      Height = 172
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnDblClick = lbRegionsDblClick
    end
    object Panel4: TPanel
      Left = 145
      Top = 13
      Width = 107
      Height = 172
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object Label2: TLabel
        Left = 8
        Top = 2
        Width = 23
        Height = 13
        Caption = 'From'
      end
      object Label3: TLabel
        Left = 8
        Top = 43
        Width = 13
        Height = 13
        Caption = 'To'
      end
      object editFrom: TEdit
        Left = 7
        Top = 16
        Width = 90
        Height = 21
        MaxLength = 8
        TabOrder = 0
      end
      object editTo: TEdit
        Left = 7
        Top = 56
        Width = 89
        Height = 21
        MaxLength = 8
        TabOrder = 1
      end
      object Button3: TButton
        Left = 13
        Top = 80
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 2
        OnClick = Button3Click
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 185
    Width = 252
    Height = 48
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      252
      48)
    object DontInclude: TCheckBox
      Left = 3
      Top = 31
      Width = 262
      Height = 17
      Caption = 'Don'#39't include Cheat Engine header in file'
      TabOrder = 0
      OnClick = DontIncludeClick
    end
    object Panel3: TPanel
      Left = 47
      Top = 0
      Width = 169
      Height = 33
      Anchors = [akTop]
      BevelOuter = bvNone
      TabOrder = 1
      object Button1: TButton
        Left = 0
        Top = 3
        Width = 75
        Height = 25
        Caption = 'Save'
        Default = True
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 86
        Top = 3
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'CEM'
    Filter = 'Cheat Engine Memory file(*.CEM)|*.CEM|All files(*.*)|*.*'
    FilterIndex = 0
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 8
    Top = 16
  end
end
