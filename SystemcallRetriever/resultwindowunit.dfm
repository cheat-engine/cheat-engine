object resultwindow: Tresultwindow
  Left = 577
  Top = 289
  Width = 487
  Height = 354
  Caption = 'Results'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnResize = FormResize
  DesignSize = (
    469
    309)
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 58
    Top = 47
    Width = 194
    Height = 16
    Caption = 'NtUserBuildHwndListCallnumber'
  end
  object Label2: TLabel
    Left = 58
    Top = 76
    Width = 194
    Height = 16
    Caption = 'NtUserQueryWindowCallnumber'
  end
  object Label3: TLabel
    Left = 53
    Top = 106
    Width = 199
    Height = 16
    Caption = 'NtUserFindWindowExCallnumber'
  end
  object Label4: TLabel
    Left = 5
    Top = 135
    Width = 249
    Height = 16
    Caption = 'NtUserGetForegroundWindowCallnumber'
  end
  object Label5: TLabel
    Left = 158
    Top = 175
    Width = 88
    Height = 16
    Caption = 'activelinkoffset'
  end
  object Label6: TLabel
    Left = 135
    Top = 204
    Width = 115
    Height = 16
    Caption = 'processnameoffset'
  end
  object Label7: TLabel
    Left = 154
    Top = 234
    Width = 94
    Height = 16
    Caption = 'debugportoffset'
  end
  object Button1: TButton
    Left = 174
    Top = 265
    Width = 92
    Height = 31
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 255
    Top = 44
    Width = 210
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object Edit2: TEdit
    Left = 255
    Top = 74
    Width = 210
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object Edit3: TEdit
    Left = 255
    Top = 103
    Width = 210
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object Edit4: TEdit
    Left = 255
    Top = 133
    Width = 210
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
  end
  object Edit5: TEdit
    Left = 255
    Top = 172
    Width = 210
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
  end
  object Edit6: TEdit
    Left = 255
    Top = 202
    Width = 210
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
  end
  object Edit7: TEdit
    Left = 255
    Top = 231
    Width = 210
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 469
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 8
    object Label8: TLabel
      Left = 52
      Top = 10
      Width = 361
      Height = 16
      Caption = 'If you have no idea what the following means then just click ok'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
  end
end
