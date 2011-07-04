object frmStructuresAddElement: TfrmStructuresAddElement
  Left = 537
  Top = 281
  BorderStyle = bsDialog
  Caption = 'Add Element'
  ClientHeight = 186
  ClientWidth = 359
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 20
    Top = 40
    Width = 66
    Height = 16
    Caption = 'description'
  end
  object Label2: TLabel
    Left = 256
    Top = 91
    Width = 51
    Height = 16
    Caption = 'Bytesize'
  end
  object Label3: TLabel
    Left = 20
    Top = 0
    Width = 34
    Height = 16
    Caption = 'Offset'
  end
  object cbType: TComboBox
    Left = 20
    Top = 111
    Width = 227
    Height = 24
    Style = csDropDownList
    ItemHeight = 16
    TabOrder = 2
    OnChange = cbTypeChange
  end
  object cbPointerto: TCheckBox
    Left = 20
    Top = 81
    Width = 85
    Height = 21
    Caption = 'pointer to:'
    TabOrder = 1
  end
  object Button1: TButton
    Left = 20
    Top = 150
    Width = 92
    Height = 31
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object Button2: TButton
    Left = 138
    Top = 150
    Width = 92
    Height = 31
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object edtDescription: TEdit
    Left = 20
    Top = 56
    Width = 227
    Height = 21
    TabOrder = 0
    Text = 'undefined'
  end
  object edtByteSize: TEdit
    Left = 256
    Top = 111
    Width = 70
    Height = 21
    Enabled = False
    TabOrder = 3
    Text = '0'
    OnChange = edtByteSizeChange
  end
  object edtOffset: TEdit
    Left = 20
    Top = 16
    Width = 229
    Height = 24
    TabOrder = 6
    Text = '0'
    OnChange = edtOffsetChange
  end
end
