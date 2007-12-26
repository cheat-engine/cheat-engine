object frmSaveMemoryRegion: TfrmSaveMemoryRegion
  Left = 799
  Top = 823
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Save memoryregion'
  ClientHeight = 178
  ClientWidth = 276
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 2
    Width = 215
    Height = 13
    Caption = 'Add the region(s) of memory you want to save'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 184
    Top = 18
    Width = 23
    Height = 13
    Caption = 'From'
  end
  object Label3: TLabel
    Left = 184
    Top = 59
    Width = 13
    Height = 13
    Caption = 'To'
  end
  object editFrom: TEdit
    Left = 184
    Top = 32
    Width = 89
    Height = 21
    MaxLength = 8
    TabOrder = 0
  end
  object Button1: TButton
    Left = 56
    Top = 132
    Width = 75
    Height = 25
    Caption = 'Save'
    Default = True
    TabOrder = 3
    OnClick = Button1Click
  end
  object editTo: TEdit
    Left = 184
    Top = 72
    Width = 89
    Height = 21
    MaxLength = 8
    TabOrder = 1
  end
  object Button2: TButton
    Left = 143
    Top = 132
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object DontInclude: TCheckBox
    Left = 3
    Top = 160
    Width = 214
    Height = 17
    Caption = 'Don'#39't include Cheat Engine header in file'
    TabOrder = 2
    OnClick = DontIncludeClick
  end
  object Button3: TButton
    Left = 184
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 5
    OnClick = Button3Click
  end
  object lbRegions: TListBox
    Left = 8
    Top = 16
    Width = 169
    Height = 105
    ItemHeight = 13
    TabOrder = 6
    OnDblClick = lbRegionsDblClick
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
