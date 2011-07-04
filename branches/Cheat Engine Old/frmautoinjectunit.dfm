object frmAutoInject: TfrmAutoInject
  Left = 1024
  Top = 496
  Width = 431
  Height = 331
  HelpContext = 1089
  Caption = 'Auto assemble'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 236
    Width = 415
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = Panel1Resize
    object Button1: TButton
      Left = 174
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Execute'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object TabControl1: TTabControl
    Left = 0
    Top = 0
    Width = 415
    Height = 236
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 0
    OnChange = TabControl1Change
    OnContextPopup = TabControl1ContextPopup
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 72
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Caption = 'New tab'
        ShortCut = 16462
        OnClick = New1Click
      end
      object Load1: TMenuItem
        Caption = 'Open'
        ShortCut = 16463
        OnClick = Load1Click
      end
      object Save1: TMenuItem
        Caption = 'Save'
        ShortCut = 16467
        OnClick = Save1Click
      end
      object SaveAs1: TMenuItem
        Caption = 'Save As...'
        OnClick = SaveAs1Click
      end
      object Assigntocurrentcheattable1: TMenuItem
        Caption = 'Assign to current cheat table'
        OnClick = Assigntocurrentcheattable1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object Syntaxhighlighting1: TMenuItem
        Caption = 'Syntax highlighting'
        Checked = True
        OnClick = Syntaxhighlighting1Click
      end
      object AAPref1: TMenuItem
        Caption = 'Preferences'
        OnClick = AAPref1Click
      end
    end
    object emplate1: TMenuItem
      Caption = 'Template'
      object Codeinjection1: TMenuItem
        Caption = 'Code injection'
        OnClick = Codeinjection1Click
      end
      object APIHook1: TMenuItem
        Caption = 'API Hook'
        OnClick = APIHook1Click
      end
      object Coderelocation1: TMenuItem
        Caption = 'Code relocation'
        OnClick = Coderelocation1Click
      end
      object CheatTablecompliantcodee1: TMenuItem
        Caption = 'Cheat Table framework code'
        OnClick = CheatTablecompliantcodee1Click
      end
    end
    object Inject1: TMenuItem
      Caption = 'Inject'
      Visible = False
      object Injectincurrentprocess1: TMenuItem
        Caption = 'Inject into current process'
        OnClick = Injectincurrentprocess1Click
      end
      object Injectintocurrentprocessandexecute1: TMenuItem
        Caption = 'Inject into current process and execute'
        OnClick = Injectintocurrentprocessandexecute1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'CEA'
    Filter = 'Cheat Engine Assembly (*.CEA)|*.CEA|All Files (*.*)|*.*'
    Title = 'Open CE assembly file'
    Left = 8
    Top = 40
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'CEA'
    Filter = 'Cheat Engine Assembly (*.CEA)|*.CEA|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Title = 'Open CE assembly file'
    Left = 40
    Top = 40
  end
  object PopupMenu1: TPopupMenu
    Left = 8
    Top = 8
    object Cut1: TMenuItem
      Caption = 'Cu&t'
      ShortCut = 16472
      OnClick = Cut1Click
    end
    object Copy1: TMenuItem
      Caption = '&Copy'
      ShortCut = 16451
      OnClick = Copy1Click
    end
    object Paste1: TMenuItem
      Caption = '&Paste'
      ShortCut = 16470
      OnClick = Paste1Click
    end
    object Undo1: TMenuItem
      Caption = '&Undo'
      ShortCut = 16474
      OnClick = Undo1Click
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object Find1: TMenuItem
      Caption = '&Find...'
      ShortCut = 16454
      OnClick = Find1Click
    end
  end
  object closemenu: TPopupMenu
    Left = 40
    Top = 8
    object Close1: TMenuItem
      Caption = 'Close'
      OnClick = Close1Click
    end
  end
  object FindDialog1: TFindDialog
    Options = [frDown, frHideMatchCase, frHideWholeWord, frHideUpDown]
    OnFind = FindDialog1Find
    Left = 8
    Top = 104
  end
  object undotimer: TTimer
    Interval = 250
    Left = 40
    Top = 72
  end
end
