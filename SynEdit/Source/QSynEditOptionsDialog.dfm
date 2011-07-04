object fmEditorOptionsDialog: TfmEditorOptionsDialog
  Left = 580
  Top = 154
  Width = 369
  Height = 394
  VertScrollBar.Range = 387
  HorzScrollBar.Range = 361
  ActiveControl = PageControl1
  BorderStyle = fbsDialog
  Caption = 'Editor Options'
  Color = clButton
  Font.Color = clText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  Font.Weight = 40
  ParentFont = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  TextWidth = 6
  object PageControl1: TPageControl
    Left = 6
    Top = 8
    Width = 355
    Height = 345
    ActivePage = Display
    TabOrder = 0
    object Display: TTabSheet
      Caption = 'Display'
      ImageIndex = 1
      object gbRightEdge: TGroupBox
        Left = 8
        Top = 136
        Width = 159
        Height = 88
        Caption = 'Right Edge'
        TabOrder = 1
        object Label3: TLabel
          Left = 9
          Top = 56
          Width = 54
          Height = 13
          Caption = 'Edge color:'
        end
        object Label10: TLabel
          Left = 9
          Top = 26
          Width = 66
          Height = 13
          Caption = 'Edge Column:'
        end
        object pRightEdgeBack: TPanel
          Left = 80
          Top = 54
          Width = 52
          Height = 21
          BorderWidth = 1
          TabOrder = 1
          object pRightEdgeColor: TPanel
            Left = 2
            Top = 2
            Width = 38
            Height = 17
            Align = alClient
            BevelOuter = bvLowered
            Color = clGray
            TabOrder = 0
            OnClick = pRightEdgeColorClick
          end
          object btnRightEdge: TPanel
            Left = 40
            Top = 2
            Width = 10
            Height = 17
            Align = alRight
            BevelOuter = bvNone
            TabOrder = 1
            OnMouseDown = btnRightEdgeMouseDown
            object Image1: TImage
              Left = 3
              Top = 6
              Width = 5
              Height = 5
              Picture.Data = {
                07544269746D61708E000000424D8A0000000000000076000000280000000500
                000005000000010004000000000014000000C40E0000C40E0000100000000000
                0000000000000000800000800000008080008000000080008000808000008080
                8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
                FF00DDDDD000DD0DD000D000D00000000000DDDDD000}
              Transparent = True
              OnMouseDown = btnRightEdgeMouseDown
            end
          end
        end
        object eRightEdge: TEdit
          Left = 80
          Top = 23
          Width = 51
          Height = 21
          TabOrder = 0
          Text = '0'
        end
      end
      object gbGutter: TGroupBox
        Left = 8
        Top = 8
        Width = 330
        Height = 121
        Caption = 'Gutter'
        TabOrder = 0
        object Label1: TLabel
          Left = 176
          Top = 89
          Width = 58
          Height = 13
          Caption = 'Gutter color:'
        end
        object ckGutterAutosize: TCheckBox
          Left = 9
          Top = 37
          Width = 120
          Height = 17
          Caption = 'Autosize'
          TabOrder = 1
        end
        object ckGutterShowLineNumbers: TCheckBox
          Left = 9
          Top = 56
          Width = 120
          Height = 17
          Caption = 'Show line numbers'
          TabOrder = 2
        end
        object ckGutterShowLeaderZeros: TCheckBox
          Left = 9
          Top = 94
          Width = 120
          Height = 17
          Caption = 'Show leading zeros'
          TabOrder = 4
        end
        object ckGutterStartAtZero: TCheckBox
          Left = 9
          Top = 75
          Width = 120
          Height = 17
          Caption = 'Start at zero'
          TabOrder = 3
        end
        object ckGutterVisible: TCheckBox
          Left = 9
          Top = 18
          Width = 120
          Height = 17
          Caption = 'Visible'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object cbGutterFont: TCheckBox
          Left = 176
          Top = 18
          Width = 120
          Height = 17
          Caption = 'Use Gutter Font'
          TabOrder = 5
          OnClick = cbGutterFontClick
        end
        object btnGutterFont: TButton
          Left = 282
          Top = 13
          Width = 40
          Height = 25
          Caption = 'Font'
          TabOrder = 6
          OnClick = btnGutterFontClick
        end
        object pGutterBack: TPanel
          Left = 252
          Top = 85
          Width = 52
          Height = 21
          BorderWidth = 1
          TabOrder = 8
          object pGutterColor: TPanel
            Left = 2
            Top = 2
            Width = 38
            Height = 17
            Align = alClient
            BevelOuter = bvLowered
            Color = clGray
            TabOrder = 0
            OnClick = pGutterColorClick
          end
          object btnGutterColor: TPanel
            Left = 40
            Top = 2
            Width = 10
            Height = 17
            Align = alRight
            BevelOuter = bvNone
            TabOrder = 1
            OnMouseDown = btnGutterColorMouseDown
            object Image2: TImage
              Left = 3
              Top = 6
              Width = 5
              Height = 5
              Picture.Data = {
                07544269746D61708E000000424D8A0000000000000076000000280000000500
                000005000000010004000000000014000000C40E0000C40E0000100000000000
                0000000000000000800000800000008080008000000080008000808000008080
                8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
                FF00DDDDD000DD0DD000D000D00000000000DDDDD000}
              Transparent = True
              OnMouseDown = btnGutterColorMouseDown
            end
          end
        end
        object pnlGutterFontDisplay: TPanel
          Left = 176
          Top = 40
          Width = 145
          Height = 33
          BevelOuter = bvNone
          TabOrder = 7
          object lblGutterFont: TLabel
            Left = 19
            Top = 9
            Width = 72
            Height = 8
            Caption = 'Terminal 8pt'
            Font.Color = clText
            Font.Height = 11
            Font.Name = 'Terminal'
            Font.Pitch = fpVariable
            Font.Style = []
            Font.Weight = 40
            ParentFont = False
          end
        end
      end
      object gbBookmarks: TGroupBox
        Left = 8
        Top = 232
        Width = 159
        Height = 79
        Caption = 'Bookmarks'
        TabOrder = 3
        object ckBookmarkKeys: TCheckBox
          Left = 9
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Bookmark keys'
          TabOrder = 0
        end
        object ckBookmarkVisible: TCheckBox
          Left = 9
          Top = 48
          Width = 121
          Height = 17
          Caption = 'Bookmarks visible'
          TabOrder = 1
        end
      end
      object gbEditorFont: TGroupBox
        Left = 180
        Top = 232
        Width = 159
        Height = 79
        Caption = 'Editor Font'
        TabOrder = 4
        object btnFont: TButton
          Left = 64
          Top = 49
          Width = 84
          Height = 25
          Caption = 'Font'
          TabOrder = 0
          OnClick = btnFontClick
        end
        object Panel3: TPanel
          Left = 8
          Top = 19
          Width = 143
          Height = 30
          BevelOuter = bvNone
          TabOrder = 1
          object labFont: TLabel
            Left = 2
            Top = 1
            Width = 128
            Height = 16
            Caption = 'Courier New 10pt'
            Font.Color = clText
            Font.Height = 13
            Font.Name = 'Courier New'
            Font.Pitch = fpVariable
            Font.Style = []
            Font.Weight = 40
            ParentFont = False
          end
        end
      end
      object gbLineSpacing: TGroupBox
        Left = 180
        Top = 136
        Width = 159
        Height = 88
        Caption = 'Line spacing / Tab spacing'
        TabOrder = 2
        object Label8: TLabel
          Left = 9
          Top = 27
          Width = 55
          Height = 13
          Caption = 'Extra Lines:'
        end
        object Label9: TLabel
          Left = 9
          Top = 56
          Width = 53
          Height = 13
          Caption = 'Tab Width:'
        end
        object eLineSpacing: TEdit
          Left = 80
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 0
          Text = '0'
        end
        object eTabWidth: TEdit
          Left = 80
          Top = 53
          Width = 52
          Height = 21
          TabOrder = 1
          Text = '8'
        end
      end
    end
    object Options: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
      object gbOptions: TGroupBox
        Left = 8
        Top = 0
        Width = 330
        Height = 247
        Caption = 'Options'
        TabOrder = 0
        object ckAutoIndent: TCheckBox
          Left = 9
          Top = 15
          Width = 130
          Height = 17
          Hint = 
            'Will indent the caret on new lines with the same amount of leadi' +
            'ng white space as the preceding line'
          Caption = 'Auto indent'
          TabOrder = 0
        end
        object ckDragAndDropEditing: TCheckBox
          Left = 9
          Top = 34
          Width = 130
          Height = 17
          Hint = 
            'Allows you to select a block of text and drag it within the docu' +
            'ment to another location'
          Caption = 'Drag and drop editing'
          TabOrder = 1
        end
        object ckDragAndDropFiles: TCheckBox
          Left = 9
          Top = 53
          Width = 130
          Height = 17
          Hint = 'Allows the editor accept OLE file drops'
          Caption = 'Drag and drop files'
          TabOrder = 2
        end
        object ckHalfPageScroll: TCheckBox
          Left = 176
          Top = 15
          Width = 130
          Height = 17
          Hint = 
            'When scrolling with page-up and page-down commands, only scroll ' +
            'a half page at a time'
          Caption = 'Half page scroll'
          TabOrder = 11
        end
        object ckNoSelection: TCheckBox
          Left = 176
          Top = 224
          Width = 130
          Height = 17
          Hint = 'Disables selecting text'
          Caption = 'No selection'
          TabOrder = 21
        end
        object ckNoCaret: TCheckBox
          Left = 9
          Top = 224
          Width = 130
          Height = 17
          Hint = 'Makes it so the caret is never visible'
          Caption = 'No caret'
          TabOrder = 20
        end
        object ckScrollByOneLess: TCheckBox
          Left = 176
          Top = 34
          Width = 130
          Height = 17
          Hint = 'Forces scrolling to be one less'
          Caption = 'Scroll by one less'
          TabOrder = 12
        end
        object ckScrollPastEOF: TCheckBox
          Left = 176
          Top = 53
          Width = 130
          Height = 17
          Hint = 'Allows the cursor to go past the end of file marker'
          Caption = 'Scroll past end of file'
          TabOrder = 13
        end
        object ckScrollPastEOL: TCheckBox
          Left = 176
          Top = 72
          Width = 130
          Height = 17
          Hint = 
            'Allows the cursor to go past the last character into the white s' +
            'pace at the end of a line'
          Caption = 'Scroll past end of line'
          TabOrder = 14
        end
        object ckShowScrollHint: TCheckBox
          Left = 176
          Top = 91
          Width = 130
          Height = 17
          Hint = 
            'Shows a hint of the visible line numbers when scrolling vertical' +
            'ly'
          Caption = 'Show scroll hint'
          TabOrder = 15
        end
        object ckSmartTabs: TCheckBox
          Left = 9
          Top = 129
          Width = 130
          Height = 17
          Hint = 
            'When tabbing, the cursor will go to the next non-white space cha' +
            'racter of the previous line'
          Caption = 'Smart tabs'
          TabOrder = 6
        end
        object ckTabsToSpaces: TCheckBox
          Left = 176
          Top = 129
          Width = 130
          Height = 17
          Hint = 'Converts a tab character to the number of spaces in Tab Width'
          Caption = 'Tabs to spaces'
          TabOrder = 17
        end
        object ckTrimTrailingSpaces: TCheckBox
          Left = 176
          Top = 148
          Width = 130
          Height = 17
          Hint = 'Spaces at the end of lines will be trimmed and not saved'
          Caption = 'Trim trailing spaces'
          TabOrder = 18
        end
        object ckWantTabs: TCheckBox
          Left = 9
          Top = 110
          Width = 130
          Height = 17
          Hint = 
            'Let the editor accept tab characters instead of going to the nex' +
            't control'
          Caption = 'Want tabs'
          TabOrder = 5
        end
        object ckAltSetsColumnMode: TCheckBox
          Left = 9
          Top = 72
          Width = 130
          Height = 17
          Hint = 
            'Holding down the Alt Key will put the selection mode into column' +
            'ar format'
          Caption = 'Alt sets column mode'
          TabOrder = 3
        end
        object ckKeepCaretX: TCheckBox
          Left = 9
          Top = 91
          Width = 130
          Height = 17
          Hint = 
            'When moving through lines the X position will always stay the sa' +
            'me'
          Caption = 'Maintain caret column'
          TabOrder = 4
        end
        object ckScrollHintFollows: TCheckBox
          Left = 176
          Top = 110
          Width = 152
          Height = 17
          Hint = 'The scroll hint follows the mouse when scrolling vertically'
          Caption = 'Scroll hint follows mouse'
          TabOrder = 16
        end
        object ckGroupUndo: TCheckBox
          Left = 176
          Top = 167
          Width = 130
          Height = 17
          Hint = 
            'When undoing/redoing actions, handle all continous changes of th' +
            'e same kind in one call instead undoing/redoing each command sep' +
            'arately'
          Caption = 'Group undo'
          TabOrder = 19
        end
        object ckSmartTabDelete: TCheckBox
          Left = 9
          Top = 148
          Width = 130
          Height = 17
          Hint = 'similar to Smart Tabs, but when you delete characters'
          Caption = 'Smart tab delete'
          TabOrder = 7
        end
        object ckRightMouseMoves: TCheckBox
          Left = 9
          Top = 167
          Width = 146
          Height = 17
          Hint = 
            'When clicking with the right mouse for a popup menu, move the cu' +
            'rsor to that location'
          Caption = 'Right mouse moves cursor'
          TabOrder = 8
        end
        object ckEnhanceHomeKey: TCheckBox
          Left = 9
          Top = 186
          Width = 146
          Height = 17
          Hint = 'enhances home key positioning, similar to visual studio'
          Caption = 'Enhance Home Key'
          TabOrder = 9
        end
        object ckHideShowScrollbars: TCheckBox
          Left = 9
          Top = 205
          Width = 160
          Height = 17
          Hint = 
            'if enabled, then the scrollbars will only show when necessary.  ' +
            'If you have ScrollPastEOL, then it the horizontal bar will alway' +
            's be there (it uses MaxLength instead)'
          Caption = 'Hide scrollbars as necessary'
          TabOrder = 10
        end
        object ckDisableScrollArrows: TCheckBox
          Left = 176
          Top = 186
          Width = 130
          Height = 17
          Hint = 
            'Disables the scroll bar arrow buttons when you can'#39't scroll in t' +
            'hat direction any more'
          Caption = 'Disable scroll arrows'
          TabOrder = 22
        end
        object ckShowSpecialChars: TCheckBox
          Left = 176
          Top = 205
          Width = 130
          Height = 17
          Hint = 'Shows linebreaks, spaces and tabs using special symbols'
          Caption = 'Show special chars'
          TabOrder = 23
        end
      end
      object gbCaret: TGroupBox
        Left = 8
        Top = 249
        Width = 330
        Height = 62
        Caption = 'Caret'
        TabOrder = 1
        object Label2: TLabel
          Left = 16
          Top = 17
          Width = 56
          Height = 13
          Caption = 'Insert caret:'
        end
        object Label4: TLabel
          Left = 16
          Top = 41
          Width = 75
          Height = 13
          Caption = 'Overwrite caret:'
        end
        object cInsertCaret: TComboBox
          Left = 120
          Top = 13
          Width = 186
          Height = 21
          Style = csDropDownList
          ItemHeight = 15
          Items.Strings = (
            'Vertical Line'
            'Horizontal Line'
            'Half Block'
            'Block')
          TabOrder = 0
        end
        object cOverwriteCaret: TComboBox
          Left = 120
          Top = 37
          Width = 186
          Height = 21
          Style = csDropDownList
          ItemHeight = 15
          Items.Strings = (
            'Vertical Line'
            'Horizontal Line'
            'Half Block'
            'Block')
          TabOrder = 1
        end
      end
    end
    object Keystrokes: TTabSheet
      Caption = 'Keystrokes'
      ImageIndex = 2
      object btnAddKey: TButton
        Left = 96
        Top = 152
        Width = 75
        Height = 25
        Caption = '&Add'
        TabOrder = 2
        OnClick = btnAddKeyClick
      end
      object btnRemKey: TButton
        Left = 176
        Top = 152
        Width = 75
        Height = 25
        Caption = '&Remove'
        TabOrder = 3
        OnClick = btnRemKeyClick
      end
      object gbKeyStrokes: TGroupBox
        Left = 8
        Top = 192
        Width = 330
        Height = 119
        Caption = 'Keystroke Options'
        TabOrder = 4
        object Label5: TLabel
          Left = 16
          Top = 28
          Width = 50
          Height = 13
          Caption = 'Command:'
        end
        object Label6: TLabel
          Left = 16
          Top = 91
          Width = 50
          Height = 13
          Caption = 'Keystroke:'
        end
        object Label7: TLabel
          Left = 16
          Top = 59
          Width = 50
          Height = 13
          Caption = 'Keystroke:'
        end
        object cKeyCommand: TComboBox
          Left = 120
          Top = 23
          Width = 186
          Height = 21
          ItemHeight = 15
          TabOrder = 0
          OnExit = cKeyCommandExit
          OnKeyPress = cKeyCommandKeyPress
          OnKeyUp = cKeyCommandKeyUp
        end
      end
      object btnUpdateKey: TButton
        Left = 16
        Top = 152
        Width = 75
        Height = 25
        Caption = '&Update'
        TabOrder = 1
        OnClick = btnUpdateKeyClick
      end
      object pnlCommands: TPanel
        Left = 8
        Top = 13
        Width = 330
        Height = 132
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelInner = bvRaised
        BevelOuter = bvLowered
        Caption = 'pnlCommands'
        TabOrder = 0
        object KeyList: TListView
          Left = 2
          Top = 2
          Width = 326
          Height = 128
          Align = alClient
          BorderStyle = bsNone
          ColumnClick = False
          ColumnMove = False
          Columns = <
            item
              AllowClick = False
              Caption = 'Command'
              Tag = 0
              Width = 167
            end
            item
              AllowClick = False
              Caption = 'Keystroke'
              Tag = 0
              Width = 142
            end>
          RowSelect = True
          ReadOnly = True
          TabOrder = 0
          ViewStyle = vsReport
          OnChanging = KeyListChanging
          OnEditing = KeyListEditing
          OnSelectItem = KeyListSelectItem
        end
      end
    end
  end
  object btnOk: TButton
    Left = 200
    Top = 362
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 280
    Top = 362
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ColorDialog: TColorDialog
    Left = 8
    Top = 368
  end
  object ColorPopup: TPopupMenu
    Images = ImageList1
    Left = 40
    Top = 368
    object None1: TMenuItem
      Tag = -1
      Caption = 'None'
      OnClick = PopupMenuClick
    end
    object Scrollbar1: TMenuItem
      Caption = 'Scrollbar'
      OnClick = PopupMenuClick
    end
    object Background1: TMenuItem
      Tag = 1
      Caption = 'Background'
      OnClick = PopupMenuClick
    end
    object ActiveCaption1: TMenuItem
      Tag = 2
      Caption = 'Active Caption'
      OnClick = PopupMenuClick
    end
    object InactiveCaption1: TMenuItem
      Tag = 3
      Caption = 'Inactive Caption'
      OnClick = PopupMenuClick
    end
    object Menu1: TMenuItem
      Tag = 4
      Caption = 'Menu'
      OnClick = PopupMenuClick
    end
    object Window1: TMenuItem
      Tag = 5
      Caption = 'Window'
      OnClick = PopupMenuClick
    end
    object WindowFrame1: TMenuItem
      Tag = 6
      Caption = 'Window Frame'
      OnClick = PopupMenuClick
    end
    object MEnu2: TMenuItem
      Tag = 7
      Caption = 'Menu Text'
      OnClick = PopupMenuClick
    end
    object WindowText1: TMenuItem
      Tag = 8
      Caption = 'Window Text'
      OnClick = PopupMenuClick
    end
    object CaptionText1: TMenuItem
      Tag = 9
      Caption = 'Caption Text'
      OnClick = PopupMenuClick
    end
    object ActiveBorder1: TMenuItem
      Tag = 10
      Caption = 'Active Border'
      OnClick = PopupMenuClick
    end
    object InactiveBorder1: TMenuItem
      Tag = 11
      Caption = 'Inactive Border'
      OnClick = PopupMenuClick
    end
    object ApplicationWorkspace1: TMenuItem
      Tag = 12
      Caption = 'Application Workspace'
      OnClick = PopupMenuClick
    end
    object Highlight1: TMenuItem
      Tag = 13
      Caption = 'Highlight'
      OnClick = PopupMenuClick
    end
    object HighlightText1: TMenuItem
      Tag = 14
      Caption = 'Highlight Text'
      OnClick = PopupMenuClick
    end
    object ButtonFace1: TMenuItem
      Tag = 15
      Caption = 'Button Face'
      OnClick = PopupMenuClick
    end
    object ButtonShadow1: TMenuItem
      Tag = 16
      Caption = 'Button Shadow'
      OnClick = PopupMenuClick
    end
    object GrayText1: TMenuItem
      Tag = 17
      Caption = 'Gray Text'
      OnClick = PopupMenuClick
    end
    object ButtonText1: TMenuItem
      Tag = 18
      Caption = 'Button Text'
      OnClick = PopupMenuClick
    end
    object InactiveCaptionText1: TMenuItem
      Tag = 19
      Caption = 'Inactive Caption Text'
      OnClick = PopupMenuClick
    end
    object Highlight2: TMenuItem
      Tag = 20
      Caption = 'Highlight'
      OnClick = PopupMenuClick
    end
    object N3dDarkShadow1: TMenuItem
      Tag = 21
      Caption = '3D Dark Shadow'
      OnClick = PopupMenuClick
    end
    object N3DLight1: TMenuItem
      Tag = 22
      Caption = '3D Light'
      OnClick = PopupMenuClick
    end
    object InfoTipText1: TMenuItem
      Tag = 23
      Caption = 'Info Tip Text'
      OnClick = PopupMenuClick
    end
    object InfoTipBackground1: TMenuItem
      Tag = 24
      Caption = 'Info Tip Background'
      OnClick = PopupMenuClick
    end
  end
  object ImageList1: TImageList
    Left = 72
    Top = 368
  end
  object FontDialog: TFontDialog
    Font.Color = clText
    Font.Height = 11
    Font.Name = 'MS Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = []
    Font.Weight = 40
    Left = 104
    Top = 368
  end
end
