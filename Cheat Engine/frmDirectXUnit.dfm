object frmDirectX: TfrmDirectX
  Left = 548
  Top = 342
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'DirectX Settings'
  ClientHeight = 518
  ClientWidth = 615
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 41
    Width = 615
    Height = 412
    HorzScrollBar.Increment = 43
    VertScrollBar.Increment = 36
    VertScrollBar.Tracking = True
    Align = alClient
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 10
      Top = 0
      Width = 572
      Height = 277
      Caption = 'Zoom Config'
      TabOrder = 0
      object Label2: TLabel
        Left = 17
        Top = 20
        Width = 70
        Height = 16
        Caption = 'Zoom 1 key'
      end
      object Label3: TLabel
        Left = 17
        Top = 49
        Width = 70
        Height = 16
        Caption = 'Zoom 2 key'
      end
      object Label4: TLabel
        Left = 17
        Top = 79
        Width = 70
        Height = 16
        Caption = 'Zoom 3 key'
      end
      object Label5: TLabel
        Left = 17
        Top = 108
        Width = 70
        Height = 16
        Caption = 'Zoom 4 key'
      end
      object Label6: TLabel
        Left = 17
        Top = 138
        Width = 70
        Height = 16
        Caption = 'Zoom 5 key'
      end
      object Label7: TLabel
        Left = 437
        Top = 20
        Width = 52
        Height = 16
        Caption = 'Zoom 1='
      end
      object Label8: TLabel
        Left = 539
        Top = 20
        Width = 6
        Height = 16
        Caption = 'x'
      end
      object Label9: TLabel
        Left = 437
        Top = 49
        Width = 52
        Height = 16
        Caption = 'Zoom 2='
      end
      object Label10: TLabel
        Left = 539
        Top = 49
        Width = 6
        Height = 16
        Caption = 'x'
      end
      object Label11: TLabel
        Left = 437
        Top = 79
        Width = 52
        Height = 16
        Caption = 'Zoom 3='
      end
      object Label12: TLabel
        Left = 539
        Top = 79
        Width = 6
        Height = 16
        Caption = 'x'
      end
      object Label13: TLabel
        Left = 437
        Top = 108
        Width = 52
        Height = 16
        Caption = 'Zoom 4='
      end
      object Label14: TLabel
        Left = 539
        Top = 108
        Width = 6
        Height = 16
        Caption = 'x'
      end
      object Label15: TLabel
        Left = 437
        Top = 138
        Width = 52
        Height = 16
        Caption = 'Zoom 5='
      end
      object Label16: TLabel
        Left = 539
        Top = 138
        Width = 6
        Height = 16
        Caption = 'x'
      end
      object Label17: TLabel
        Left = 15
        Top = 177
        Width = 73
        Height = 16
        Caption = 'Zoom in key'
      end
      object Label18: TLabel
        Left = 434
        Top = 194
        Width = 75
        Height = 16
        Caption = 'Zoom delta='
      end
      object Label19: TLabel
        Left = 560
        Top = 194
        Width = 6
        Height = 16
        Caption = 'x'
      end
      object Label20: TLabel
        Left = 6
        Top = 207
        Width = 81
        Height = 16
        Caption = 'Zoom out key'
      end
      object Label23: TLabel
        Left = 10
        Top = 238
        Width = 79
        Height = 16
        Caption = 'No zoom key'
      end
      object Zoom1: TEdit
        Left = 90
        Top = 17
        Width = 269
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 0
        OnKeyDown = Zoom1KeyDown
      end
      object Zoom2: TEdit
        Left = 90
        Top = 47
        Width = 269
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 3
        OnKeyDown = Zoom2KeyDown
      end
      object Zoom3: TEdit
        Left = 90
        Top = 76
        Width = 269
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 6
        OnKeyDown = Zoom3KeyDown
      end
      object Zoom4: TEdit
        Left = 90
        Top = 106
        Width = 269
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 9
        OnKeyDown = Zoom4KeyDown
      end
      object Zoom5: TEdit
        Left = 90
        Top = 135
        Width = 269
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 12
        OnKeyDown = Zoom5KeyDown
      end
      object Button1: TButton
        Left = 368
        Top = 20
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 1
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 368
        Top = 49
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 4
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 368
        Top = 79
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 7
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 368
        Top = 108
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 10
        OnClick = Button4Click
      end
      object Button5: TButton
        Left = 368
        Top = 138
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 13
        OnClick = Button5Click
      end
      object Zoomlevel1: TEdit
        Left = 490
        Top = 16
        Width = 47
        Height = 24
        TabOrder = 2
        Text = '1.0'
      end
      object Zoomlevel2: TEdit
        Left = 490
        Top = 46
        Width = 47
        Height = 24
        TabOrder = 5
        Text = '2.0'
      end
      object Zoomlevel3: TEdit
        Left = 490
        Top = 75
        Width = 47
        Height = 24
        TabOrder = 8
        Text = '4.0'
      end
      object Zoomlevel4: TEdit
        Left = 490
        Top = 105
        Width = 47
        Height = 24
        TabOrder = 11
        Text = '8.0'
      end
      object Zoomlevel5: TEdit
        Left = 490
        Top = 134
        Width = 47
        Height = 24
        TabOrder = 14
        Text = '16.0'
      end
      object Zoomin: TEdit
        Left = 90
        Top = 175
        Width = 269
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 15
        OnKeyDown = ZoominKeyDown
      end
      object Button7: TButton
        Left = 368
        Top = 177
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 16
        OnClick = Button7Click
      end
      object zoomdelta: TEdit
        Left = 511
        Top = 191
        Width = 47
        Height = 24
        TabOrder = 19
        Text = '0.2'
      end
      object Zoomout: TEdit
        Left = 90
        Top = 204
        Width = 269
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 17
        OnKeyDown = ZoomoutKeyDown
      end
      object Button8: TButton
        Left = 368
        Top = 207
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 18
        OnClick = Button8Click
      end
      object nozoom: TEdit
        Left = 90
        Top = 234
        Width = 269
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 20
        OnKeyDown = nozoomKeyDown
      end
      object Button9: TButton
        Left = 368
        Top = 236
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 21
        OnClick = Button9Click
      end
    end
    object GroupBox2: TGroupBox
      Left = 10
      Top = 279
      Width = 572
      Height = 175
      Caption = 'Visible settings'
      TabOrder = 1
      object Label25: TLabel
        Left = 28
        Top = 22
        Width = 71
        Height = 16
        Caption = 'Toggle Fog'
      end
      object Label26: TLabel
        Left = 4
        Top = 54
        Width = 93
        Height = 16
        Caption = 'Toggle Z-Buffer'
      end
      object Label27: TLabel
        Left = 16
        Top = 81
        Width = 47
        Height = 32
        Caption = 'Toggle Lights'
        WordWrap = True
      end
      object Label24: TLabel
        Left = 345
        Top = 325
        Width = 128
        Height = 16
        Caption = 'Transparency of logo'
        Visible = False
      end
      object Label33: TLabel
        Left = 518
        Top = 348
        Width = 19
        Height = 16
        Caption = '0%'
      end
      object Label79: TLabel
        Left = 15
        Top = 138
        Width = 85
        Height = 16
        Caption = 'Toggle keylist'
      end
      object Label32: TLabel
        Left = 36
        Top = 111
        Width = 62
        Height = 16
        Caption = 'Wireframe'
      end
      object fog: TEdit
        Left = 98
        Top = 20
        Width = 260
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 0
        OnKeyDown = fogKeyDown
      end
      object Button10: TButton
        Left = 364
        Top = 22
        Width = 61
        Height = 21
        Caption = 'Clear'
        TabOrder = 1
        OnClick = Button10Click
      end
      object Button11: TButton
        Left = 364
        Top = 52
        Width = 61
        Height = 21
        Caption = 'Clear'
        TabOrder = 3
        OnClick = Button11Click
      end
      object zbuffer: TEdit
        Left = 98
        Top = 49
        Width = 260
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 2
        OnKeyDown = zbufferKeyDown
      end
      object Lighting: TEdit
        Left = 98
        Top = 79
        Width = 260
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 4
        OnKeyDown = LightingKeyDown
      end
      object Button12: TButton
        Left = 364
        Top = 81
        Width = 61
        Height = 21
        Caption = 'Clear'
        TabOrder = 5
        OnClick = Button12Click
      end
      object CheckBox1: TCheckBox
        Left = 226
        Top = 286
        Width = 169
        Height = 20
        Caption = 'Show Cheat Engine logo'
        TabOrder = 7
        Visible = False
      end
      object CheckBox3: TCheckBox
        Left = 20
        Top = 286
        Width = 178
        Height = 20
        Caption = 'Place crosshair on screen'
        TabOrder = 8
        Visible = False
      end
      object RadioButton1: TRadioButton
        Left = 20
        Top = 305
        Width = 70
        Height = 21
        Caption = 'Center'
        TabOrder = 9
        Visible = False
      end
      object RadioButton2: TRadioButton
        Left = 20
        Top = 325
        Width = 99
        Height = 21
        Caption = 'Userdefined'
        TabOrder = 10
        Visible = False
      end
      object Edit6: TEdit
        Left = 20
        Top = 345
        Width = 40
        Height = 24
        TabOrder = 11
        Text = 'x'
        Visible = False
      end
      object Edit7: TEdit
        Left = 69
        Top = 345
        Width = 41
        Height = 24
        TabOrder = 12
        Text = 'y'
        Visible = False
      end
      object Edit1: TEdit
        Left = 225
        Top = 350
        Width = 41
        Height = 24
        TabOrder = 13
        Text = 'x'
        Visible = False
      end
      object Edit2: TEdit
        Left = 274
        Top = 350
        Width = 41
        Height = 24
        TabOrder = 14
        Text = 'y'
        Visible = False
      end
      object RadioButton3: TRadioButton
        Left = 225
        Top = 325
        Width = 100
        Height = 21
        Caption = 'Userdefined'
        TabOrder = 15
        Visible = False
      end
      object RadioButton4: TRadioButton
        Left = 225
        Top = 305
        Width = 70
        Height = 21
        Caption = 'Top left'
        TabOrder = 16
        Visible = False
      end
      object RadioButton5: TRadioButton
        Left = 304
        Top = 305
        Width = 90
        Height = 21
        Caption = 'Top Right'
        TabOrder = 17
        Visible = False
      end
      object RadioButton6: TRadioButton
        Left = 393
        Top = 305
        Width = 89
        Height = 21
        Caption = 'Bottom left'
        TabOrder = 18
        Visible = False
      end
      object RadioButton7: TRadioButton
        Left = 486
        Top = 305
        Width = 90
        Height = 21
        Caption = 'Bottom right'
        TabOrder = 19
        Visible = False
      end
      object TrackBar1: TTrackBar
        Left = 335
        Top = 345
        Width = 184
        Height = 30
        Max = 255
        TabOrder = 20
        Visible = False
      end
      object Wireframe: TEdit
        Left = 98
        Top = 108
        Width = 260
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 6
        OnKeyDown = WireframeKeyDown
      end
      object Button32: TButton
        Left = 364
        Top = 111
        Width = 61
        Height = 21
        Caption = 'Clear'
        TabOrder = 21
        OnClick = Button32Click
      end
      object ShowKeyList: TEdit
        Left = 98
        Top = 138
        Width = 260
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 22
        OnKeyDown = ShowKeyListKeyDown
      end
      object Button33: TButton
        Left = 364
        Top = 140
        Width = 61
        Height = 21
        Caption = 'Clear'
        TabOrder = 23
        OnClick = Button33Click
      end
    end
    object GroupBox3: TGroupBox
      Left = 10
      Top = 534
      Width = 572
      Height = 990
      Caption = 'Aimhelper functions'
      TabOrder = 2
      object Label28: TLabel
        Left = 6
        Top = 229
        Width = 74
        Height = 16
        Caption = 'Autoaim key'
      end
      object Label34: TLabel
        Left = 10
        Top = 465
        Width = 101
        Height = 16
        Caption = 'Previous Texture'
      end
      object Label35: TLabel
        Left = 292
        Top = 465
        Width = 75
        Height = 16
        Caption = 'Next Texture'
      end
      object Label36: TLabel
        Left = 10
        Top = 514
        Width = 123
        Height = 16
        Caption = 'Lock/Unlock Texture'
      end
      object Label37: TLabel
        Left = 10
        Top = 581
        Width = 232
        Height = 16
        Caption = 'Increase X pos of aim on locked texture'
      end
      object Label38: TLabel
        Left = 292
        Top = 581
        Width = 240
        Height = 16
        Caption = 'Decrease X pos of aim on locked texture'
      end
      object Label39: TLabel
        Left = 10
        Top = 630
        Width = 233
        Height = 16
        Caption = 'Increase Y pos of aim on locked texture'
      end
      object Label40: TLabel
        Left = 292
        Top = 630
        Width = 241
        Height = 16
        Caption = 'Decrease Y pos of aim on locked texture'
      end
      object Label41: TLabel
        Left = 10
        Top = 679
        Width = 232
        Height = 16
        Caption = 'Increase Z pos of aim on locked texture'
      end
      object Label42: TLabel
        Left = 292
        Top = 679
        Width = 240
        Height = 16
        Caption = 'Decrease Z pos of aim on locked texture'
      end
      object Label43: TLabel
        Left = 384
        Top = 23
        Width = 50
        Height = 16
        Caption = 'Aimfile 1'
      end
      object Label46: TLabel
        Left = 6
        Top = 22
        Width = 118
        Height = 16
        Caption = 'Select aimfile 1 key:'
      end
      object Label44: TLabel
        Left = 384
        Top = 68
        Width = 50
        Height = 16
        Caption = 'Aimfile 2'
      end
      object Label45: TLabel
        Left = 6
        Top = 66
        Width = 118
        Height = 16
        Caption = 'Select aimfile 2 key:'
      end
      object Label47: TLabel
        Left = 384
        Top = 113
        Width = 50
        Height = 16
        Caption = 'Aimfile 3'
      end
      object Label48: TLabel
        Left = 6
        Top = 112
        Width = 118
        Height = 16
        Caption = 'Select aimfile 3 key:'
      end
      object Label49: TLabel
        Left = 9
        Top = 170
        Width = 128
        Height = 16
        Caption = 'Load selected aimfile'
      end
      object Label50: TLabel
        Left = 294
        Top = 170
        Width = 268
        Height = 16
        Caption = 'Save selected aimfile (calibration mode only)'
      end
      object Label51: TLabel
        Left = 10
        Top = 745
        Width = 129
        Height = 16
        Caption = 'Auto callibrate mouse'
      end
      object Label52: TLabel
        Left = 20
        Top = 811
        Width = 81
        Height = 16
        Caption = 'mousepos 1='
      end
      object Label53: TLabel
        Left = 148
        Top = 811
        Width = 43
        Height = 16
        Caption = 'pixel(s)'
      end
      object Label54: TLabel
        Left = 20
        Top = 870
        Width = 81
        Height = 16
        Caption = 'mousepos 5='
      end
      object Label55: TLabel
        Left = 148
        Top = 870
        Width = 43
        Height = 16
        Caption = 'pixel(s)'
      end
      object Label56: TLabel
        Left = 14
        Top = 900
        Width = 88
        Height = 16
        Caption = 'mousepos 10='
      end
      object Label57: TLabel
        Left = 148
        Top = 900
        Width = 43
        Height = 16
        Caption = 'pixel(s)'
      end
      object Label58: TLabel
        Left = 30
        Top = 791
        Width = 126
        Height = 16
        Caption = 'Horizontal movement'
      end
      object Label59: TLabel
        Left = 276
        Top = 811
        Width = 81
        Height = 16
        Caption = 'mousepos 1='
      end
      object Label60: TLabel
        Left = 404
        Top = 811
        Width = 43
        Height = 16
        Caption = 'pixel(s)'
      end
      object Label61: TLabel
        Left = 404
        Top = 870
        Width = 43
        Height = 16
        Caption = 'pixel(s)'
      end
      object Label62: TLabel
        Left = 276
        Top = 870
        Width = 81
        Height = 16
        Caption = 'mousepos 5='
      end
      object Label63: TLabel
        Left = 270
        Top = 900
        Width = 88
        Height = 16
        Caption = 'mousepos 10='
      end
      object Label64: TLabel
        Left = 404
        Top = 900
        Width = 43
        Height = 16
        Caption = 'pixel(s)'
      end
      object Label65: TLabel
        Left = 305
        Top = 791
        Width = 111
        Height = 16
        Caption = 'Vertical movement'
      end
      object Label66: TLabel
        Left = 20
        Top = 841
        Width = 81
        Height = 16
        Caption = 'mousepos 2='
      end
      object Label67: TLabel
        Left = 148
        Top = 841
        Width = 43
        Height = 16
        Caption = 'pixel(s)'
      end
      object Label68: TLabel
        Left = 276
        Top = 841
        Width = 81
        Height = 16
        Caption = 'mousepos 2='
      end
      object Label69: TLabel
        Left = 404
        Top = 841
        Width = 43
        Height = 16
        Caption = 'pixel(s)'
      end
      object Label70: TLabel
        Left = 14
        Top = 929
        Width = 88
        Height = 16
        Caption = 'mousepos 20='
      end
      object Label71: TLabel
        Left = 148
        Top = 929
        Width = 43
        Height = 16
        Caption = 'pixel(s)'
      end
      object Label72: TLabel
        Left = 270
        Top = 928
        Width = 88
        Height = 16
        Caption = 'mousepos 20='
      end
      object Label73: TLabel
        Left = 404
        Top = 928
        Width = 43
        Height = 16
        Caption = 'pixel(s)'
      end
      object Label74: TLabel
        Left = 14
        Top = 959
        Width = 88
        Height = 16
        Caption = 'mousepos 40='
      end
      object Label75: TLabel
        Left = 148
        Top = 959
        Width = 43
        Height = 16
        Caption = 'pixel(s)'
      end
      object Label76: TLabel
        Left = 270
        Top = 955
        Width = 88
        Height = 16
        Caption = 'mousepos 40='
      end
      object Label77: TLabel
        Left = 404
        Top = 955
        Width = 43
        Height = 16
        Caption = 'pixel(s)'
      end
      object Addresslabel: TLabel
        Left = 310
        Top = 288
        Width = 96
        Height = 16
        Caption = '                                '
      end
      object autoaimtoggle: TEdit
        Left = 7
        Top = 246
        Width = 201
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 13
        OnKeyDown = autoaimtoggleKeyDown
      end
      object Button13: TButton
        Left = 217
        Top = 249
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 14
        OnClick = Button13Click
      end
      object PreviousTexture: TEdit
        Left = 10
        Top = 482
        Width = 200
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 20
        OnKeyDown = PreviousTextureKeyDown
      end
      object Button16: TButton
        Left = 217
        Top = 485
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 21
        OnClick = Button16Click
      end
      object CheckBox2: TCheckBox
        Left = 10
        Top = 443
        Width = 139
        Height = 21
        Caption = 'Calibrate auto-aim'
        Checked = True
        State = cbChecked
        TabOrder = 19
        OnClick = CheckBox2Click
      end
      object Button17: TButton
        Left = 502
        Top = 485
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 51
        OnClick = Button17Click
      end
      object nexttexture: TEdit
        Left = 293
        Top = 482
        Width = 201
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 22
        OnKeyDown = nexttextureKeyDown
      end
      object Button18: TButton
        Left = 217
        Top = 532
        Width = 60
        Height = 23
        Caption = 'Clear'
        TabOrder = 24
        OnClick = Button18Click
      end
      object locktexture: TEdit
        Left = 10
        Top = 532
        Width = 200
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 23
        OnKeyDown = locktextureKeyDown
      end
      object IncreaseX: TEdit
        Left = 10
        Top = 598
        Width = 200
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 25
        OnKeyDown = IncreaseXKeyDown
      end
      object Button19: TButton
        Left = 217
        Top = 601
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 26
        OnClick = Button19Click
      end
      object DecreaseX: TEdit
        Left = 293
        Top = 598
        Width = 201
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 27
        OnKeyDown = DecreaseXKeyDown
      end
      object Button20: TButton
        Left = 502
        Top = 601
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 28
        OnClick = Button20Click
      end
      object IncreaseY: TEdit
        Left = 10
        Top = 647
        Width = 200
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 29
        OnKeyDown = IncreaseYKeyDown
      end
      object Button21: TButton
        Left = 217
        Top = 650
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 30
        OnClick = Button21Click
      end
      object DecreaseY: TEdit
        Left = 293
        Top = 647
        Width = 201
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 31
        OnKeyDown = DecreaseYKeyDown
      end
      object Button22: TButton
        Left = 502
        Top = 650
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 32
        OnClick = Button22Click
      end
      object IncreaseZ: TEdit
        Left = 10
        Top = 697
        Width = 200
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 33
        OnKeyDown = IncreaseZKeyDown
      end
      object Button23: TButton
        Left = 217
        Top = 699
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 34
        OnClick = Button23Click
      end
      object DecreaseZ: TEdit
        Left = 293
        Top = 697
        Width = 201
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 35
        OnKeyDown = DecreaseZKeyDown
      end
      object Button24: TButton
        Left = 502
        Top = 699
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 36
        OnClick = Button24Click
      end
      object aimfile1: TEdit
        Left = 384
        Top = 39
        Width = 178
        Height = 24
        TabOrder = 2
      end
      object setaimsetting1: TEdit
        Left = 7
        Top = 39
        Width = 290
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 0
        OnKeyDown = setaimsetting1KeyDown
      end
      object Button25: TButton
        Left = 305
        Top = 42
        Width = 61
        Height = 21
        Caption = 'Clear'
        TabOrder = 1
        OnClick = Button25Click
      end
      object aimfile2: TEdit
        Left = 384
        Top = 84
        Width = 178
        Height = 24
        TabOrder = 5
      end
      object setaimsetting2: TEdit
        Left = 7
        Top = 84
        Width = 290
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 3
        OnKeyDown = setaimsetting2KeyDown
      end
      object Button26: TButton
        Left = 305
        Top = 86
        Width = 61
        Height = 21
        Caption = 'Clear'
        TabOrder = 4
        OnClick = Button26Click
      end
      object aimfile3: TEdit
        Left = 384
        Top = 129
        Width = 178
        Height = 24
        TabOrder = 8
      end
      object setaimsetting3: TEdit
        Left = 7
        Top = 129
        Width = 290
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 6
        OnKeyDown = setaimsetting3KeyDown
      end
      object Button27: TButton
        Left = 305
        Top = 132
        Width = 61
        Height = 21
        Caption = 'Clear'
        TabOrder = 7
        OnClick = Button27Click
      end
      object loadaimsettingsfile: TEdit
        Left = 7
        Top = 187
        Width = 201
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 9
        OnKeyDown = loadaimsettingsfileKeyDown
      end
      object Button28: TButton
        Left = 217
        Top = 190
        Width = 60
        Height = 20
        Caption = 'Clear'
        TabOrder = 10
        OnClick = Button28Click
      end
      object Saveaimsettingsfile: TEdit
        Left = 293
        Top = 187
        Width = 201
        Height = 24
        ReadOnly = True
        TabOrder = 11
        OnKeyDown = SaveaimsettingsfileKeyDown
      end
      object Button29: TButton
        Left = 502
        Top = 190
        Width = 60
        Height = 20
        Caption = 'Clear'
        TabOrder = 12
        OnClick = Button29Click
      end
      object callibrationkey: TEdit
        Left = 10
        Top = 762
        Width = 200
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 37
        OnKeyDown = callibrationkeyKeyDown
      end
      object Button30: TButton
        Left = 217
        Top = 764
        Width = 60
        Height = 21
        Caption = 'Clear'
        TabOrder = 38
        OnClick = Button30Click
      end
      object mousecallibrationhorizontal1point: TEdit
        Left = 98
        Top = 807
        Width = 41
        Height = 24
        TabOrder = 39
        Text = '1.0'
      end
      object mousecallibrationhorizontal5point: TEdit
        Left = 98
        Top = 866
        Width = 41
        Height = 24
        TabOrder = 41
        Text = '5.0'
      end
      object mousecallibrationhorizontal10point: TEdit
        Left = 98
        Top = 896
        Width = 41
        Height = 24
        TabOrder = 42
        Text = '10.0'
      end
      object mousecallibrationvertical1point: TEdit
        Left = 354
        Top = 807
        Width = 41
        Height = 24
        TabOrder = 45
        Text = '1.0'
      end
      object mousecallibrationvertical5point: TEdit
        Left = 354
        Top = 866
        Width = 41
        Height = 24
        TabOrder = 47
        Text = '5.0'
      end
      object mousecallibrationvertical10point: TEdit
        Left = 354
        Top = 896
        Width = 41
        Height = 24
        TabOrder = 48
        Text = '10'
      end
      object mousecallibrationhorizontal2point: TEdit
        Left = 98
        Top = 837
        Width = 41
        Height = 24
        TabOrder = 40
        Text = '2.0'
      end
      object mousecallibrationvertical2point: TEdit
        Left = 354
        Top = 837
        Width = 41
        Height = 24
        TabOrder = 46
        Text = '2.0'
      end
      object mousecallibrationhorizontal20point: TEdit
        Left = 98
        Top = 926
        Width = 41
        Height = 24
        TabOrder = 43
        Text = '20.0'
      end
      object mousecallibrationvertical20point: TEdit
        Left = 354
        Top = 924
        Width = 41
        Height = 24
        TabOrder = 49
        Text = '20.0'
      end
      object mousecallibrationhorizontal40point: TEdit
        Left = 98
        Top = 955
        Width = 41
        Height = 24
        TabOrder = 44
        Text = '40.0'
      end
      object mousecallibrationvertical40point: TEdit
        Left = 354
        Top = 951
        Width = 41
        Height = 24
        TabOrder = 50
        Text = '40.0'
      end
      object Panel3: TPanel
        Left = 279
        Top = 246
        Width = 287
        Height = 21
        BevelOuter = bvNone
        TabOrder = 52
        object rbtoggleoneff: TRadioButton
          Left = 10
          Top = 0
          Width = 109
          Height = 21
          Caption = 'Toggle on/off'
          TabOrder = 0
          OnClick = rbtoggleoneffClick
        end
        object rbKeepDown: TRadioButton
          Left = 128
          Top = 0
          Width = 110
          Height = 21
          Caption = 'Keep pressed'
          Checked = True
          TabOrder = 1
          TabStop = True
          OnClick = rbtoggleoneffClick
        end
      end
      object autoshoot: TCheckBox
        Left = 409
        Top = 266
        Width = 119
        Height = 21
        Caption = 'Auto shoot'
        TabOrder = 15
      end
      object Panel4: TPanel
        Left = 2
        Top = 329
        Width = 484
        Height = 109
        BevelOuter = bvNone
        TabOrder = 53
        object Label29: TLabel
          Left = 6
          Top = 5
          Width = 78
          Height = 16
          Caption = 'Increase Lag'
        end
        object Label78: TLabel
          Left = 279
          Top = 5
          Width = 61
          Height = 16
          Caption = 'Defaultlag'
        end
        object Label30: TLabel
          Left = 6
          Top = 54
          Width = 86
          Height = 16
          Caption = 'Decrease Lag'
        end
        object Label31: TLabel
          Left = 279
          Top = 54
          Width = 58
          Height = 16
          Caption = 'Lag Delta'
        end
        object increaselag: TEdit
          Left = 7
          Top = 25
          Width = 201
          Height = 24
          PopupMenu = Mousekeymenu
          ReadOnly = True
          TabOrder = 0
          OnKeyDown = increaselagKeyDown
        end
        object Button14: TButton
          Left = 217
          Top = 28
          Width = 60
          Height = 21
          Caption = 'Clear'
          TabOrder = 1
          OnClick = Button14Click
        end
        object lag: TEdit
          Left = 284
          Top = 25
          Width = 47
          Height = 24
          TabOrder = 2
          Text = '0'
        end
        object decreaselag: TEdit
          Left = 7
          Top = 74
          Width = 201
          Height = 24
          PopupMenu = Mousekeymenu
          ReadOnly = True
          TabOrder = 3
          OnKeyDown = decreaselagKeyDown
        end
        object Button15: TButton
          Left = 217
          Top = 78
          Width = 60
          Height = 20
          Caption = 'Clear'
          TabOrder = 4
          OnClick = Button15Click
        end
        object lagdelta: TEdit
          Left = 284
          Top = 74
          Width = 47
          Height = 24
          TabOrder = 5
          Text = '10'
        end
      end
      object getlagfrommemory: TCheckBox
        Left = 10
        Top = 286
        Width = 198
        Height = 20
        Caption = 'Get lag from memory address'
        TabOrder = 16
        OnClick = getlagfrommemoryClick
      end
      object Button31: TButton
        Left = 207
        Top = 286
        Width = 99
        Height = 20
        Caption = 'Select address'
        Enabled = False
        TabOrder = 17
        OnClick = Button31Click
      end
      object UseFpsLag: TCheckBox
        Left = 10
        Top = 305
        Width = 168
        Height = 21
        Caption = 'Use lag caused by fps'
        TabOrder = 18
      end
    end
    object GroupBox4: TGroupBox
      Left = 10
      Top = 464
      Width = 572
      Height = 60
      Caption = 'Textures'
      TabOrder = 3
      object Label80: TLabel
        Left = 5
        Top = 17
        Width = 63
        Height = 64
        Caption = 'Save all loaded textures to disk'
        WordWrap = True
      end
      object SaveAllTextures: TEdit
        Left = 98
        Top = 20
        Width = 260
        Height = 24
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 0
        OnKeyDown = SaveAllTexturesKeyDown
      end
      object Button34: TButton
        Left = 364
        Top = 22
        Width = 61
        Height = 21
        Caption = 'Clear'
        TabOrder = 1
        OnClick = Button34Click
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 615
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 0
      Top = 20
      Width = 191
      Height = 16
      Caption = 'Key combinations and other stuff'
    end
    object LoadButton: TSpeedButton
      Left = 546
      Top = 4
      Width = 31
      Height = 30
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
      OnClick = LoadButtonClick
    end
    object SaveButton: TSpeedButton
      Left = 578
      Top = 4
      Width = 31
      Height = 30
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
      OnClick = SaveButtonClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 453
    Width = 615
    Height = 65
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Label21: TLabel
      Left = 196
      Top = 10
      Width = 209
      Height = 16
      Caption = 'Key poll intervall.  (Smaller is faster)'
    end
    object Label22: TLabel
      Left = 320
      Top = 41
      Width = 21
      Height = 16
      Caption = 'ms.'
    end
    object Button6: TButton
      Left = 512
      Top = 20
      Width = 92
      Height = 30
      Caption = 'Apply'
      TabOrder = 0
      OnClick = Button6Click
    end
    object editKeyPolling: TEdit
      Left = 235
      Top = 36
      Width = 80
      Height = 24
      TabOrder = 1
      Text = '100'
    end
    object Button35: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Launch'
      TabOrder = 2
      OnClick = Button35Click
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'ctx'
    Filter = 'ctx files (*.ctx)|*.ctx'
    Left = 376
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ctx'
    Filter = 'ctx files (*.ctx)|*.ctx'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 336
  end
  object Mousekeymenu: TPopupMenu
    Left = 400
    Top = 257
    object LeftMouse1: TMenuItem
      Caption = 'Left Mouse'
      OnClick = LeftMouse1Click
    end
    object CenterMouse1: TMenuItem
      Tag = 1
      Caption = 'Center Mouse'
      OnClick = LeftMouse1Click
    end
    object RightMouse1: TMenuItem
      Tag = 2
      Caption = 'Right Mouse'
      OnClick = LeftMouse1Click
    end
  end
end
