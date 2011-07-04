object frmExcludeHide: TfrmExcludeHide
  Left = 893
  Top = 158
  Width = 453
  Height = 398
  Caption = 'Show/Hide settings'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Label4: TLabel
    Left = 0
    Top = 0
    Width = 435
    Height = 41
    Align = alTop
    AutoSize = False
    Caption = 
      'Select the way cheat Engine hides/shows windows. (Will not work ' +
      'if a window that gets hidden or shown is not responding. E.g:Pau' +
      'sed)'
    WordWrap = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 41
    Width = 435
    Height = 312
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = Panel1Resize
    DesignSize = (
      435
      312)
    object Label1: TLabel
      Left = 0
      Top = 39
      Width = 430
      Height = 40
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Select the processes you want to exclude from being hidden. Doub' +
        'leclick the process to add it to the list. (Or remove it from th' +
        'e list)'
      Enabled = False
      WordWrap = True
    end
    object RadioButton1: TRadioButton
      Left = 0
      Top = -1
      Width = 434
      Height = 21
      Caption = 'Only hide/show the foreground window'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = RadioButton1Click
    end
    object RadioButton2: TRadioButton
      Left = 0
      Top = 19
      Width = 434
      Height = 21
      Caption = 'Hide/show ALL windows'
      TabOrder = 1
      OnClick = RadioButton1Click
    end
    object Panel2: TPanel
      Left = 0
      Top = 80
      Width = 214
      Height = 185
      BevelOuter = bvNone
      TabOrder = 2
      object Label2: TLabel
        Left = 0
        Top = 0
        Width = 214
        Height = 16
        Align = alTop
        Caption = 'Current process list'
        Enabled = False
      end
      object ListBox1: TListBox
        Left = 0
        Top = 16
        Width = 214
        Height = 169
        Align = alClient
        Enabled = False
        ItemHeight = 16
        TabOrder = 0
        OnDblClick = ListBox1DblClick
      end
    end
    object Panel3: TPanel
      Left = 219
      Top = 80
      Width = 214
      Height = 185
      BevelOuter = bvNone
      TabOrder = 3
      object Label3: TLabel
        Left = 0
        Top = 0
        Width = 214
        Height = 16
        Align = alTop
        Caption = 'List of processes that will not hide'
        Enabled = False
      end
      object ListBox2: TListBox
        Left = 0
        Top = 16
        Width = 214
        Height = 169
        Align = alClient
        Enabled = False
        ItemHeight = 16
        TabOrder = 0
        OnDblClick = ListBox2DblClick
      end
    end
    object Panel4: TPanel
      Left = 120
      Top = 272
      Width = 195
      Height = 41
      BevelOuter = bvNone
      TabOrder = 4
      object Button2: TButton
        Left = 101
        Top = 2
        Width = 92
        Height = 31
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 0
      end
      object Button1: TButton
        Left = 5
        Top = 2
        Width = 92
        Height = 31
        Caption = 'OK'
        Default = True
        TabOrder = 1
        OnClick = Button1Click
      end
    end
  end
end
