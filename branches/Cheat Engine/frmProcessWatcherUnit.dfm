object frmProcessWatcher: TfrmProcessWatcher
  Left = 839
  Top = 265
  Width = 202
  Height = 279
  Anchors = [akTop, akRight]
  BorderIcons = [biSystemMenu]
  Caption = 'Process watcher'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object tvProcesslist: TTreeView
    Left = 0
    Top = 0
    Width = 184
    Height = 166
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
    Top = 166
    Width = 184
    Height = 68
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = Panel1Resize
    object btnOpen: TButton
      Left = 39
      Top = 10
      Width = 110
      Height = 21
      Caption = 'Open process'
      TabOrder = 0
      OnClick = btnOpenClick
    end
    object btnAttach: TButton
      Left = 39
      Top = 39
      Width = 110
      Height = 21
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
