object SynEditKeystrokesEditorForm: TSynEditKeystrokesEditorForm
  Left = 345
  Top = 317
  Width = 390
  Height = 353
  ActiveControl = KeyCmdList
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Keystroke Editor'
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
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  TextWidth = 6
  object pnlBottom: TPanel
    Left = 8
    Top = 8
    Width = 365
    Height = 308
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object lnlInfo: TLabel
      Left = 5
      Top = 271
      Width = 229
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'NOTE: To have multiple keystrokes do the same'
    end
    object lnlInfo2: TLabel
      Left = 42
      Top = 287
      Width = 217
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'command, assign the command multiple times.'
    end
    object pnlCommands: TPanel
      Left = 16
      Top = 16
      Width = 246
      Height = 244
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvLowered
      BorderWidth = 4
      Caption = 'pnlCommands'
      TabOrder = 0
      object KeyCmdList: TListView
        Left = 6
        Top = 6
        Width = 234
        Height = 232
        Align = alClient
        BorderStyle = bsNone
        ColumnClick = False
        ColumnMove = False
        Columns = <
          item
            AllowClick = False
            Caption = 'Command'
            Tag = 0
            Width = 117
          end
          item
            AllowClick = False
            Caption = 'Keystroke'
            Tag = 0
            Width = 101
          end>
        TabOrder = 0
        ViewStyle = vsReport
        OnClick = KeyCmdListClick
        OnDblClick = btnEditClick
      end
    end
    object btnAdd: TButton
      Left = 276
      Top = 20
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Add'
      TabOrder = 1
      OnClick = btnAddClick
    end
    object btnEdit: TButton
      Left = 276
      Top = 52
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Edit'
      Enabled = False
      TabOrder = 2
      OnClick = btnEditClick
    end
    object btnDelete: TButton
      Left = 276
      Top = 84
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Delete'
      Enabled = False
      TabOrder = 3
      OnClick = btnDeleteClick
    end
    object btnClear: TButton
      Left = 276
      Top = 116
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'C&lear List'
      TabOrder = 4
      OnClick = btnClearClick
    end
    object btnReset: TButton
      Left = 276
      Top = 148
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Reset List'
      TabOrder = 5
      OnClick = btnResetClick
    end
    object btnOK: TButton
      Left = 276
      Top = 241
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      TabOrder = 6
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 276
      Top = 273
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      TabOrder = 7
      OnClick = btnCancelClick
    end
  end
end
