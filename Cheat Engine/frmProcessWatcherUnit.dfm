object frmProcessWatcher: TfrmProcessWatcher
  Left = 839
  Top = 265
  Width = 207
  Height = 279
  Anchors = [akTop, akRight]
  BorderIcons = [biSystemMenu]
  Caption = 'Process watcher'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object tvProcesslist: TTreeView
    Left = 0
    Top = 0
    Width = 199
    Height = 190
    Align = alClient
    Indent = 19
    PopupMenu = pmthreadid
    ReadOnly = True
    ShowRoot = False
    TabOrder = 0
    OnDblClick = tvProcesslistDblClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 190
    Width = 199
    Height = 55
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      199
      55)
    object btnOpen: TButton
      Left = 57
      Top = 8
      Width = 89
      Height = 17
      Anchors = [akTop]
      Caption = 'Open process'
      TabOrder = 0
      OnClick = btnOpenClick
    end
    object btnAttach: TButton
      Left = 57
      Top = 32
      Width = 89
      Height = 17
      Anchors = [akTop]
      Caption = 'Attach to process'
      TabOrder = 1
      OnClick = btnAttachClick
    end
  end
  object pmthreadid: TPopupMenu
    Left = 56
    Top = 48
    object ShowThreadIDs1: TMenuItem
      Caption = 'Show ThreadID'#39's'
      OnClick = ShowThreadIDs1Click
    end
  end
end
