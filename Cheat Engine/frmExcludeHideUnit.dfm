object frmExcludeHide: TfrmExcludeHide
  Left = 915
  Top = 553
  Width = 365
  Height = 320
  Caption = 'Show/Hide settings'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 64
    Width = 353
    Height = 33
    AutoSize = False
    Caption = 
      'Select the processes you want to exclude from being hidden. Doub' +
      'leclick the process to add it to the list. (Or remove it from th' +
      'e list)'
    Enabled = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 45
    Top = 96
    Width = 89
    Height = 13
    Caption = 'Current process list'
    Enabled = False
  end
  object Label3: TLabel
    Left = 188
    Top = 96
    Width = 158
    Height = 13
    Caption = 'List of processes that will not hide'
    Enabled = False
  end
  object Label4: TLabel
    Left = 0
    Top = 0
    Width = 353
    Height = 33
    AutoSize = False
    Caption = 
      'Select the way cheat Engine hides/shows windows. (Will not work ' +
      'if a window that gets hidden or shown is not responding. E.g:Pau' +
      'sed)'
    WordWrap = True
  end
  object ListBox1: TListBox
    Left = 4
    Top = 112
    Width = 171
    Height = 137
    Enabled = False
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = ListBox1DblClick
  end
  object ListBox2: TListBox
    Left = 182
    Top = 112
    Width = 171
    Height = 137
    Enabled = False
    ItemHeight = 13
    TabOrder = 1
    OnDblClick = ListBox2DblClick
  end
  object Button1: TButton
    Left = 100
    Top = 256
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 182
    Top = 256
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object RadioButton1: TRadioButton
    Left = 0
    Top = 32
    Width = 353
    Height = 17
    Caption = 'Only hide/show the foreground window'
    Checked = True
    TabOrder = 4
    TabStop = True
    OnClick = RadioButton1Click
  end
  object RadioButton2: TRadioButton
    Left = 0
    Top = 48
    Width = 353
    Height = 17
    Caption = 'Hide/show ALL windows'
    TabOrder = 5
    OnClick = RadioButton1Click
  end
end
