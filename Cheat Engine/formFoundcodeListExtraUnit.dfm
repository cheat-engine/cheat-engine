object FormFoundCodeListExtra: TFormFoundCodeListExtra
  Left = 426
  Top = 132
  Width = 369
  Height = 289
  Caption = 'Extra info'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  DesignSize = (
    361
    255)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 48
    Height = 13
    Caption = 'Label1'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 16
    Top = 24
    Width = 48
    Height = 13
    Caption = 'Label2'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 16
    Top = 40
    Width = 48
    Height = 13
    Caption = 'Label3'
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 16
    Top = 56
    Width = 48
    Height = 13
    Caption = 'Label4'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 16
    Top = 72
    Width = 48
    Height = 13
    Caption = 'Label5'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 8
    Top = 96
    Width = 346
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Label6'
    WordWrap = True
  end
  object Label7: TLabel
    Left = 8
    Top = 144
    Width = 96
    Height = 13
    Caption = 'EAX=DDDDDDDD'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
  end
  object Label8: TLabel
    Left = 8
    Top = 160
    Width = 96
    Height = 13
    Caption = 'EBX=DDDDDDDD'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
  end
  object Label9: TLabel
    Left = 8
    Top = 176
    Width = 96
    Height = 13
    Caption = 'ECX=DDDDDDDD'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
  end
  object Label11: TLabel
    Left = 134
    Top = 144
    Width = 96
    Height = 13
    Anchors = [akTop]
    Caption = 'EDX=DDDDDDDD'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
  end
  object Label12: TLabel
    Left = 257
    Top = 160
    Width = 96
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'EBP=DDDDDDDD'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
  end
  object Label13: TLabel
    Left = 257
    Top = 144
    Width = 96
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'ESP=DDDDDDDD'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
  end
  object Label14: TLabel
    Left = 134
    Top = 176
    Width = 96
    Height = 13
    Anchors = [akTop]
    Caption = 'EDI=DDDDDDDD'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
  end
  object Label15: TLabel
    Left = 257
    Top = 176
    Width = 96
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'ESI=DDDDDDDD'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
  end
  object Label16: TLabel
    Left = 134
    Top = 160
    Width = 96
    Height = 13
    Anchors = [akTop]
    Caption = 'ESI=DDDDDDDD'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
  end
  object Label10: TLabel
    Left = 0
    Top = 39
    Width = 16
    Height = 13
    Caption = '>>'
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
  end
  object Label17: TLabel
    Left = 8
    Top = 112
    Width = 346
    Height = 33
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Label6'
    PopupMenu = pmCopy
    WordWrap = True
  end
  object Label18: TLabel
    Left = 8
    Top = 192
    Width = 353
    Height = 49
    AutoSize = False
    Caption = 
      'The registers shown here are AFTER the instruction has been exec' +
      'uted. To show them before the instruction is executed use Access' +
      ' Exceptions instead of Debug Registers'
    Visible = False
    WordWrap = True
  end
  object Button1: TButton
    Left = 141
    Top = 225
    Width = 75
    Height = 26
    Anchors = [akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = Button1Click
  end
  object pmCopy: TPopupMenu
    Left = 112
    Top = 112
    object Copyaddresstoclipboard1: TMenuItem
      Caption = 'Copy address to clipboard'
      OnClick = Copyaddresstoclipboard1Click
    end
  end
end
