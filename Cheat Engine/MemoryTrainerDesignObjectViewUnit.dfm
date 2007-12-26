object frmTrainerDesignObjectView: TfrmTrainerDesignObjectView
  Left = 804
  Top = 578
  Width = 189
  Height = 368
  HelpContext = 14
  BorderIcons = [biSystemMenu]
  Caption = 'Object Info'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ValueListEditor1: TValueListEditor
    Left = 0
    Top = 0
    Width = 181
    Height = 334
    Align = alClient
    Color = clBtnFace
    DisplayOptions = [doAutoColResize]
    Options = [goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goAlwaysShowEditor, goThumbTracking]
    Strings.Strings = (
      'a=b')
    TabOrder = 0
    OnEditButtonClick = ValueListEditor1EditButtonClick
    OnKeyPress = ValueListEditor1KeyPress
    OnStringsChange = ValueListEditor1StringsChange
    OnValidate = ValueListEditor1Validate
    ColWidths = (
      87
      88)
  end
  object ColorDialog1: TColorDialog
    Color = clLime
    CustomColors.Strings = (
      'ColorA=FFFFFFFF'
      'ColorB=FFFFFFFF'
      'ColorC=FFFFFFFF'
      'ColorD=FFFFFFFF'
      'ColorE=FFFFFFFF'
      'ColorF=FFFFFFFF'
      'ColorG=FFFFFFFF'
      'ColorH=FFFFFFFF'
      'ColorI=FFFFFFFF'
      'ColorJ=FFFFFFFF'
      'ColorK=FFFFFFFF'
      'ColorL=FFFFFFFF'
      'ColorM=FFFFFFFF'
      'ColorN=FFFFFFFF'
      'ColorO=FFFFFFFF'
      'ColorP=FFFFFFFF')
    Left = 24
    Top = 48
  end
  object OpenPictureDialog1: TOpenPictureDialog
    DefaultExt = 'bmp'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 72
    Top = 56
  end
  object OpenDialog1: TOpenDialog
    Left = 48
    Top = 96
  end
end
