object frmOpenGL: TfrmOpenGL
  Left = 632
  Top = 534
  Width = 509
  Height = 456
  Caption = 'OpenGl Mess'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 369
    Width = 501
    Height = 53
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Label21: TLabel
      Left = 159
      Top = 8
      Width = 164
      Height = 13
      Caption = 'Key poll intervall.  (Smaller is faster)'
    end
    object Label22: TLabel
      Left = 260
      Top = 33
      Width = 16
      Height = 13
      Caption = 'ms.'
    end
    object Button6: TButton
      Left = 416
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Apply'
      TabOrder = 0
      OnClick = Button6Click
    end
    object editKeyPolling: TEdit
      Left = 191
      Top = 29
      Width = 65
      Height = 21
      TabOrder = 1
      Text = '100'
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 33
    Width = 501
    Height = 336
    HorzScrollBar.Increment = 43
    VertScrollBar.Increment = 36
    VertScrollBar.Position = 37
    VertScrollBar.Tracking = True
    Align = alClient
    TabOrder = 1
    object GroupBox1: TGroupBox
      Left = 8
      Top = -37
      Width = 465
      Height = 225
      Caption = 'Zoom Config'
      TabOrder = 0
      object Label2: TLabel
        Left = 14
        Top = 16
        Width = 56
        Height = 13
        Caption = 'Zoom 1 key'
      end
      object Label3: TLabel
        Left = 14
        Top = 40
        Width = 56
        Height = 13
        Caption = 'Zoom 2 key'
      end
      object Label4: TLabel
        Left = 14
        Top = 64
        Width = 56
        Height = 13
        Caption = 'Zoom 3 key'
      end
      object Label5: TLabel
        Left = 14
        Top = 88
        Width = 56
        Height = 13
        Caption = 'Zoom 4 key'
      end
      object Label6: TLabel
        Left = 14
        Top = 112
        Width = 56
        Height = 13
        Caption = 'Zoom 5 key'
      end
      object Label7: TLabel
        Left = 355
        Top = 16
        Width = 42
        Height = 13
        Caption = 'Zoom 1='
      end
      object Label8: TLabel
        Left = 438
        Top = 16
        Width = 5
        Height = 13
        Caption = 'x'
      end
      object Label9: TLabel
        Left = 355
        Top = 40
        Width = 42
        Height = 13
        Caption = 'Zoom 2='
      end
      object Label10: TLabel
        Left = 438
        Top = 40
        Width = 5
        Height = 13
        Caption = 'x'
      end
      object Label11: TLabel
        Left = 355
        Top = 64
        Width = 42
        Height = 13
        Caption = 'Zoom 3='
      end
      object Label12: TLabel
        Left = 438
        Top = 64
        Width = 5
        Height = 13
        Caption = 'x'
      end
      object Label13: TLabel
        Left = 355
        Top = 88
        Width = 42
        Height = 13
        Caption = 'Zoom 4='
      end
      object Label14: TLabel
        Left = 438
        Top = 88
        Width = 5
        Height = 13
        Caption = 'x'
      end
      object Label15: TLabel
        Left = 355
        Top = 112
        Width = 42
        Height = 13
        Caption = 'Zoom 5='
      end
      object Label16: TLabel
        Left = 438
        Top = 112
        Width = 5
        Height = 13
        Caption = 'x'
      end
      object Label17: TLabel
        Left = 12
        Top = 144
        Width = 58
        Height = 13
        Caption = 'Zoom in key'
      end
      object Label18: TLabel
        Left = 353
        Top = 158
        Width = 59
        Height = 13
        Caption = 'Zoom delta='
      end
      object Label19: TLabel
        Left = 455
        Top = 158
        Width = 5
        Height = 13
        Caption = 'x'
      end
      object Label20: TLabel
        Left = 5
        Top = 168
        Width = 65
        Height = 13
        Caption = 'Zoom out key'
      end
      object Label23: TLabel
        Left = 8
        Top = 193
        Width = 62
        Height = 13
        Caption = 'No zoom key'
      end
      object Zoom1: TEdit
        Left = 73
        Top = 14
        Width = 219
        Height = 21
        ReadOnly = True
        TabOrder = 0
      end
      object Zoom2: TEdit
        Left = 73
        Top = 38
        Width = 219
        Height = 21
        ReadOnly = True
        TabOrder = 3
      end
      object Zoom3: TEdit
        Left = 73
        Top = 62
        Width = 219
        Height = 21
        ReadOnly = True
        TabOrder = 6
      end
      object Zoom4: TEdit
        Left = 73
        Top = 86
        Width = 219
        Height = 21
        ReadOnly = True
        TabOrder = 9
      end
      object Zoom5: TEdit
        Left = 73
        Top = 110
        Width = 219
        Height = 21
        ReadOnly = True
        TabOrder = 12
      end
      object Button1: TButton
        Left = 299
        Top = 16
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 1
      end
      object Button2: TButton
        Left = 299
        Top = 40
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 4
      end
      object Button3: TButton
        Left = 299
        Top = 64
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 7
      end
      object Button4: TButton
        Left = 299
        Top = 88
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 10
      end
      object Button5: TButton
        Left = 299
        Top = 112
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 13
      end
      object Zoomlevel1: TEdit
        Left = 398
        Top = 13
        Width = 38
        Height = 21
        TabOrder = 2
        Text = '1.0'
      end
      object Zoomlevel2: TEdit
        Left = 398
        Top = 37
        Width = 38
        Height = 21
        TabOrder = 5
        Text = '2.0'
      end
      object Zoomlevel3: TEdit
        Left = 398
        Top = 61
        Width = 38
        Height = 21
        TabOrder = 8
        Text = '4.0'
      end
      object Zoomlevel4: TEdit
        Left = 398
        Top = 85
        Width = 38
        Height = 21
        TabOrder = 11
        Text = '8.0'
      end
      object Zoomlevel5: TEdit
        Left = 398
        Top = 109
        Width = 38
        Height = 21
        TabOrder = 14
        Text = '16.0'
      end
      object Zoomin: TEdit
        Left = 73
        Top = 142
        Width = 219
        Height = 21
        ReadOnly = True
        TabOrder = 15
      end
      object Button7: TButton
        Left = 299
        Top = 144
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 16
      end
      object zoomdelta: TEdit
        Left = 415
        Top = 155
        Width = 38
        Height = 21
        TabOrder = 19
        Text = '0.2'
      end
      object Zoomout: TEdit
        Left = 73
        Top = 166
        Width = 219
        Height = 21
        ReadOnly = True
        TabOrder = 17
      end
      object Button8: TButton
        Left = 299
        Top = 168
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 18
      end
      object nozoom: TEdit
        Left = 73
        Top = 190
        Width = 219
        Height = 21
        ReadOnly = True
        TabOrder = 20
      end
      object Button9: TButton
        Left = 299
        Top = 192
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 21
      end
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 190
      Width = 465
      Height = 142
      Caption = 'Visible settings'
      TabOrder = 1
      OnClick = GroupBox2Click
      object Label25: TLabel
        Left = 13
        Top = 18
        Width = 73
        Height = 13
        Caption = 'Toggle textures'
      end
      object Label27: TLabel
        Left = 22
        Top = 42
        Width = 64
        Height = 13
        Caption = 'Toggle Lights'
        WordWrap = True
      end
      object Label24: TLabel
        Left = 280
        Top = 264
        Width = 100
        Height = 13
        Caption = 'Transparency of logo'
        Visible = False
      end
      object Label33: TLabel
        Left = 421
        Top = 283
        Width = 14
        Height = 13
        Caption = '0%'
      end
      object Label26: TLabel
        Left = 4
        Top = 66
        Width = 82
        Height = 13
        Caption = 'Toggle Depthtest'
      end
      object Label28: TLabel
        Left = 32
        Top = 90
        Width = 54
        Height = 13
        Caption = 'Toggle Fog'
      end
      object textures: TEdit
        Left = 88
        Top = 16
        Width = 211
        Height = 21
        ReadOnly = True
        TabOrder = 0
        OnKeyDown = texturesKeyDown
      end
      object Button10: TButton
        Left = 304
        Top = 18
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 1
        OnClick = Button10Click
      end
      object Lighting: TEdit
        Left = 88
        Top = 40
        Width = 211
        Height = 21
        ReadOnly = True
        TabOrder = 2
        OnKeyDown = LightingKeyDown
      end
      object Button12: TButton
        Left = 304
        Top = 42
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 3
      end
      object CheckBox1: TCheckBox
        Left = 184
        Top = 232
        Width = 137
        Height = 17
        Caption = 'Show Cheat Engine logo'
        TabOrder = 4
        Visible = False
      end
      object CheckBox3: TCheckBox
        Left = 16
        Top = 232
        Width = 145
        Height = 17
        Caption = 'Place crosshair on screen'
        TabOrder = 5
        Visible = False
      end
      object RadioButton1: TRadioButton
        Left = 16
        Top = 248
        Width = 57
        Height = 17
        Caption = 'Center'
        TabOrder = 6
        Visible = False
      end
      object RadioButton2: TRadioButton
        Left = 16
        Top = 264
        Width = 81
        Height = 17
        Caption = 'Userdefined'
        TabOrder = 7
        Visible = False
      end
      object Edit6: TEdit
        Left = 16
        Top = 280
        Width = 33
        Height = 21
        TabOrder = 8
        Text = 'x'
        Visible = False
      end
      object Edit7: TEdit
        Left = 56
        Top = 280
        Width = 33
        Height = 21
        TabOrder = 9
        Text = 'y'
        Visible = False
      end
      object Edit1: TEdit
        Left = 183
        Top = 284
        Width = 33
        Height = 21
        TabOrder = 10
        Text = 'x'
        Visible = False
      end
      object Edit2: TEdit
        Left = 223
        Top = 284
        Width = 33
        Height = 21
        TabOrder = 11
        Text = 'y'
        Visible = False
      end
      object RadioButton3: TRadioButton
        Left = 183
        Top = 264
        Width = 81
        Height = 17
        Caption = 'Userdefined'
        TabOrder = 12
        Visible = False
      end
      object RadioButton4: TRadioButton
        Left = 183
        Top = 248
        Width = 57
        Height = 17
        Caption = 'Top left'
        TabOrder = 13
        Visible = False
      end
      object RadioButton5: TRadioButton
        Left = 247
        Top = 248
        Width = 73
        Height = 17
        Caption = 'Top Right'
        TabOrder = 14
        Visible = False
      end
      object RadioButton6: TRadioButton
        Left = 319
        Top = 248
        Width = 73
        Height = 17
        Caption = 'Bottom left'
        TabOrder = 15
        Visible = False
      end
      object RadioButton7: TRadioButton
        Left = 395
        Top = 248
        Width = 73
        Height = 17
        Caption = 'Bottom right'
        TabOrder = 16
        Visible = False
      end
      object TrackBar1: TTrackBar
        Left = 272
        Top = 280
        Width = 150
        Height = 25
        Max = 255
        TabOrder = 17
        Visible = False
      end
      object DepthTest: TEdit
        Left = 88
        Top = 64
        Width = 211
        Height = 21
        ReadOnly = True
        TabOrder = 18
        OnKeyDown = DepthTestKeyDown
      end
      object Button11: TButton
        Left = 304
        Top = 66
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 19
        OnClick = Button11Click
      end
      object fog: TEdit
        Left = 88
        Top = 88
        Width = 211
        Height = 21
        ReadOnly = True
        TabOrder = 20
        OnKeyDown = fogKeyDown
      end
      object Button13: TButton
        Left = 304
        Top = 90
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 21
        OnClick = Button13Click
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 501
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 0
      Top = 16
      Width = 154
      Height = 13
      Caption = 'Key combinations and other stuff'
    end
    object LoadButton: TSpeedButton
      Left = 444
      Top = 3
      Width = 25
      Height = 25
      Hint = 'Load'
      Glyph.Data = {
        D6020000424DD6020000000000003600000028000000100000000E0000000100
        180000000000A0020000C40E0000C40E00000000000000000000C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C000000000000000000000000000000000000000000000
        0000000000000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000000000
        008484008484008484008484008484008484008484008484008484000000C0C0
        C0C0C0C0C0C0C0C0C0C000000000FFFF00000000848400848400848400848400
        8484008484008484008484008484000000C0C0C0C0C0C0C0C0C0000000FFFFFF
        00FFFF0000000084840084840084840084840084840084840084840084840084
        84000000C0C0C0C0C0C000000000FFFFFFFFFF00FFFF00000000848400848400
        8484008484008484008484008484008484008484000000C0C0C0000000FFFFFF
        00FFFFFFFFFF00FFFF0000000000000000000000000000000000000000000000
        0000000000000000000000000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00
        FFFFFFFFFF00FFFF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000FFFFFF
        00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF000000C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C000000000FFFFFFFFFF00FFFF00000000000000000000
        0000000000000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000
        000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000
        00000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000000000C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000C0C0C0C0C0C0C0C0C00000
        00C0C0C0000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C0C0000000000000000000C0C0C0C0C0C0C0C0C0C0C0C0}
      ParentShowHint = False
      ShowHint = True
    end
    object SaveButton: TSpeedButton
      Left = 470
      Top = 3
      Width = 25
      Height = 25
      Hint = 'Save'
      Glyph.Data = {
        CA020000424DCA0200000000000036000000280000000E0000000F0000000100
        18000000000094020000C40E0000C40E00000000000000000000C0C0C0C0C0C0
        C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
        C0C0C0C00000C0C0C00000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000084840084840000000000
        00000000000000000000000000C6C6C6C6C6C600000000848400000000000000
        00008484008484000000000000000000000000000000000000C6C6C6C6C6C600
        0000008484000000000000000000848400848400000000000000000000000000
        0000000000C6C6C6C6C6C6000000008484000000000000000000848400848400
        0000000000000000000000000000000000000000000000000000008484000000
        0000000000008484008484008484008484008484008484008484008484008484
        0084840084840084840000000000000000008484008484000000000000000000
        0000000000000000000000000000000084840084840000000000000000008484
        000000C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C60000000084
        840000000000000000008484000000C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6
        C6C6C6C6C6C6C60000000084840000000000000000008484000000C6C6C6C6C6
        C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C600000000848400000000000000
        00008484000000C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C600
        00000084840000000000000000008484000000C6C6C6C6C6C6C6C6C6C6C6C6C6
        C6C6C6C6C6C6C6C6C6C6C60000000000000000000000000000008484000000C6
        C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6C6000000C6C6C6000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000}
      ParentShowHint = False
      ShowHint = True
    end
  end
end
