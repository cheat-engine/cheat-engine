object frmHeaps: TfrmHeaps
  Left = 408
  Top = 186
  Width = 229
  Height = 316
  BorderIcons = [biSystemMenu]
  Caption = 'Current Heaplist'
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
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 241
    Width = 221
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      221
      41)
    object Button1: TButton
      Left = 72
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop]
      Caption = 'Close'
      ModalResult = 1
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 0
    Width = 221
    Height = 241
    Align = alClient
    HideSelection = False
    Indent = 19
    ParentShowHint = False
    ReadOnly = True
    ShowHint = False
    TabOrder = 1
    OnDblClick = TreeView1DblClick
    OnExpanding = TreeView1Expanding
    Items.Data = {
      030000001A0000000000000000000000FFFFFFFFFFFFFFFF0000000003000000
      01611B0000000000000000000000FFFFFFFFFFFFFFFF00000000000000000261
      311B0000000000000000000000FFFFFFFFFFFFFFFF0000000000000000026132
      1B0000000000000000000000FFFFFFFFFFFFFFFF00000000000000000261331A
      0000000000000000000000FFFFFFFFFFFFFFFF000000000000000001621A0000
      000000000000000000FFFFFFFFFFFFFFFF00000000000000000163}
  end
end
