object frmDirectX: TfrmDirectX
  Left = 235
  Top = 323
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'DirectX Settings'
  ClientHeight = 421
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 33
    Width = 500
    Height = 335
    HorzScrollBar.Increment = 43
    VertScrollBar.Increment = 36
    VertScrollBar.Position = 167
    VertScrollBar.Tracking = True
    Align = alClient
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 8
      Top = -167
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
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 0
        OnKeyDown = Zoom1KeyDown
      end
      object Zoom2: TEdit
        Left = 73
        Top = 38
        Width = 219
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 3
        OnKeyDown = Zoom2KeyDown
      end
      object Zoom3: TEdit
        Left = 73
        Top = 62
        Width = 219
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 6
        OnKeyDown = Zoom3KeyDown
      end
      object Zoom4: TEdit
        Left = 73
        Top = 86
        Width = 219
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 9
        OnKeyDown = Zoom4KeyDown
      end
      object Zoom5: TEdit
        Left = 73
        Top = 110
        Width = 219
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 12
        OnKeyDown = Zoom5KeyDown
      end
      object Button1: TButton
        Left = 299
        Top = 16
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 1
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 299
        Top = 40
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 4
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 299
        Top = 64
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 7
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 299
        Top = 88
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 10
        OnClick = Button4Click
      end
      object Button5: TButton
        Left = 299
        Top = 112
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 13
        OnClick = Button5Click
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
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 15
        OnKeyDown = ZoominKeyDown
      end
      object Button7: TButton
        Left = 299
        Top = 144
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 16
        OnClick = Button7Click
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
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 17
        OnKeyDown = ZoomoutKeyDown
      end
      object Button8: TButton
        Left = 299
        Top = 168
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 18
        OnClick = Button8Click
      end
      object nozoom: TEdit
        Left = 73
        Top = 190
        Width = 219
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 20
        OnKeyDown = nozoomKeyDown
      end
      object Button9: TButton
        Left = 299
        Top = 192
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 21
        OnClick = Button9Click
      end
    end
    object GroupBox2: TGroupBox
      Left = 8
      Top = 60
      Width = 465
      Height = 142
      Caption = 'Visible settings'
      TabOrder = 1
      object Label25: TLabel
        Left = 23
        Top = 18
        Width = 54
        Height = 13
        Caption = 'Toggle Fog'
      end
      object Label26: TLabel
        Left = 3
        Top = 44
        Width = 74
        Height = 13
        Caption = 'Toggle Z-Buffer'
      end
      object Label27: TLabel
        Left = 13
        Top = 66
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
      object Label79: TLabel
        Left = 12
        Top = 112
        Width = 65
        Height = 13
        Caption = 'Toggle keylist'
      end
      object Label32: TLabel
        Left = 29
        Top = 90
        Width = 48
        Height = 13
        Caption = 'Wireframe'
      end
      object fog: TEdit
        Left = 80
        Top = 16
        Width = 211
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 0
        OnKeyDown = fogKeyDown
      end
      object Button10: TButton
        Left = 296
        Top = 18
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 1
        OnClick = Button10Click
      end
      object Button11: TButton
        Left = 296
        Top = 42
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 3
        OnClick = Button11Click
      end
      object zbuffer: TEdit
        Left = 80
        Top = 40
        Width = 211
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 2
        OnKeyDown = zbufferKeyDown
      end
      object Lighting: TEdit
        Left = 80
        Top = 64
        Width = 211
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 4
        OnKeyDown = LightingKeyDown
      end
      object Button12: TButton
        Left = 296
        Top = 66
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 5
        OnClick = Button12Click
      end
      object CheckBox1: TCheckBox
        Left = 184
        Top = 232
        Width = 137
        Height = 17
        Caption = 'Show Cheat Engine logo'
        TabOrder = 7
        Visible = False
      end
      object CheckBox3: TCheckBox
        Left = 16
        Top = 232
        Width = 145
        Height = 17
        Caption = 'Place crosshair on screen'
        TabOrder = 8
        Visible = False
      end
      object RadioButton1: TRadioButton
        Left = 16
        Top = 248
        Width = 57
        Height = 17
        Caption = 'Center'
        TabOrder = 9
        Visible = False
      end
      object RadioButton2: TRadioButton
        Left = 16
        Top = 264
        Width = 81
        Height = 17
        Caption = 'Userdefined'
        TabOrder = 10
        Visible = False
      end
      object Edit6: TEdit
        Left = 16
        Top = 280
        Width = 33
        Height = 21
        TabOrder = 11
        Text = 'x'
        Visible = False
      end
      object Edit7: TEdit
        Left = 56
        Top = 280
        Width = 33
        Height = 21
        TabOrder = 12
        Text = 'y'
        Visible = False
      end
      object Edit1: TEdit
        Left = 183
        Top = 284
        Width = 33
        Height = 21
        TabOrder = 13
        Text = 'x'
        Visible = False
      end
      object Edit2: TEdit
        Left = 223
        Top = 284
        Width = 33
        Height = 21
        TabOrder = 14
        Text = 'y'
        Visible = False
      end
      object RadioButton3: TRadioButton
        Left = 183
        Top = 264
        Width = 81
        Height = 17
        Caption = 'Userdefined'
        TabOrder = 15
        Visible = False
      end
      object RadioButton4: TRadioButton
        Left = 183
        Top = 248
        Width = 57
        Height = 17
        Caption = 'Top left'
        TabOrder = 16
        Visible = False
      end
      object RadioButton5: TRadioButton
        Left = 247
        Top = 248
        Width = 73
        Height = 17
        Caption = 'Top Right'
        TabOrder = 17
        Visible = False
      end
      object RadioButton6: TRadioButton
        Left = 319
        Top = 248
        Width = 73
        Height = 17
        Caption = 'Bottom left'
        TabOrder = 18
        Visible = False
      end
      object RadioButton7: TRadioButton
        Left = 395
        Top = 248
        Width = 73
        Height = 17
        Caption = 'Bottom right'
        TabOrder = 19
        Visible = False
      end
      object TrackBar1: TTrackBar
        Left = 272
        Top = 280
        Width = 150
        Height = 25
        Max = 255
        TabOrder = 20
        Visible = False
      end
      object Wireframe: TEdit
        Left = 80
        Top = 88
        Width = 211
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 6
        OnKeyDown = WireframeKeyDown
      end
      object Button32: TButton
        Left = 296
        Top = 90
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 21
        OnClick = Button32Click
      end
      object ShowKeyList: TEdit
        Left = 80
        Top = 112
        Width = 211
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 22
        OnKeyDown = ShowKeyListKeyDown
      end
      object Button33: TButton
        Left = 296
        Top = 114
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 23
        OnClick = Button33Click
      end
    end
    object GroupBox3: TGroupBox
      Left = 8
      Top = 267
      Width = 465
      Height = 804
      Caption = 'Aimhelper functions'
      TabOrder = 2
      object Label28: TLabel
        Left = 5
        Top = 186
        Width = 58
        Height = 13
        Caption = 'Autoaim key'
      end
      object Label34: TLabel
        Left = 8
        Top = 378
        Width = 80
        Height = 13
        Caption = 'Previous Texture'
      end
      object Label35: TLabel
        Left = 237
        Top = 378
        Width = 61
        Height = 13
        Caption = 'Next Texture'
      end
      object Label36: TLabel
        Left = 8
        Top = 418
        Width = 102
        Height = 13
        Caption = 'Lock/Unlock Texture'
      end
      object Label37: TLabel
        Left = 8
        Top = 472
        Width = 187
        Height = 13
        Caption = 'Increase X pos of aim on locked texture'
      end
      object Label38: TLabel
        Left = 237
        Top = 472
        Width = 192
        Height = 13
        Caption = 'Decrease X pos of aim on locked texture'
      end
      object Label39: TLabel
        Left = 8
        Top = 512
        Width = 187
        Height = 13
        Caption = 'Increase Y pos of aim on locked texture'
      end
      object Label40: TLabel
        Left = 237
        Top = 512
        Width = 192
        Height = 13
        Caption = 'Decrease Y pos of aim on locked texture'
      end
      object Label41: TLabel
        Left = 8
        Top = 552
        Width = 187
        Height = 13
        Caption = 'Increase Z pos of aim on locked texture'
      end
      object Label42: TLabel
        Left = 237
        Top = 552
        Width = 192
        Height = 13
        Caption = 'Decrease Z pos of aim on locked texture'
      end
      object Label43: TLabel
        Left = 312
        Top = 19
        Width = 39
        Height = 13
        Caption = 'Aimfile 1'
      end
      object Label46: TLabel
        Left = 5
        Top = 18
        Width = 94
        Height = 13
        Caption = 'Select aimfile 1 key:'
      end
      object Label44: TLabel
        Left = 312
        Top = 55
        Width = 39
        Height = 13
        Caption = 'Aimfile 2'
      end
      object Label45: TLabel
        Left = 5
        Top = 54
        Width = 94
        Height = 13
        Caption = 'Select aimfile 2 key:'
      end
      object Label47: TLabel
        Left = 312
        Top = 92
        Width = 39
        Height = 13
        Caption = 'Aimfile 3'
      end
      object Label48: TLabel
        Left = 5
        Top = 91
        Width = 94
        Height = 13
        Caption = 'Select aimfile 3 key:'
      end
      object Label49: TLabel
        Left = 7
        Top = 138
        Width = 99
        Height = 13
        Caption = 'Load selected aimfile'
      end
      object Label50: TLabel
        Left = 239
        Top = 138
        Width = 208
        Height = 13
        Caption = 'Save selected aimfile (calibration mode only)'
      end
      object Label51: TLabel
        Left = 8
        Top = 605
        Width = 101
        Height = 13
        Caption = 'Auto callibrate mouse'
      end
      object Label52: TLabel
        Left = 16
        Top = 659
        Width = 63
        Height = 13
        Caption = 'mousepos 1='
      end
      object Label53: TLabel
        Left = 120
        Top = 659
        Width = 32
        Height = 13
        Caption = 'pixel(s)'
      end
      object Label54: TLabel
        Left = 16
        Top = 707
        Width = 63
        Height = 13
        Caption = 'mousepos 5='
      end
      object Label55: TLabel
        Left = 120
        Top = 707
        Width = 32
        Height = 13
        Caption = 'pixel(s)'
      end
      object Label56: TLabel
        Left = 11
        Top = 731
        Width = 69
        Height = 13
        Caption = 'mousepos 10='
      end
      object Label57: TLabel
        Left = 120
        Top = 731
        Width = 32
        Height = 13
        Caption = 'pixel(s)'
      end
      object Label58: TLabel
        Left = 24
        Top = 643
        Width = 99
        Height = 13
        Caption = 'Horizontal movement'
      end
      object Label59: TLabel
        Left = 224
        Top = 659
        Width = 63
        Height = 13
        Caption = 'mousepos 1='
      end
      object Label60: TLabel
        Left = 328
        Top = 659
        Width = 32
        Height = 13
        Caption = 'pixel(s)'
      end
      object Label61: TLabel
        Left = 328
        Top = 707
        Width = 32
        Height = 13
        Caption = 'pixel(s)'
      end
      object Label62: TLabel
        Left = 224
        Top = 707
        Width = 63
        Height = 13
        Caption = 'mousepos 5='
      end
      object Label63: TLabel
        Left = 219
        Top = 731
        Width = 69
        Height = 13
        Caption = 'mousepos 10='
      end
      object Label64: TLabel
        Left = 328
        Top = 731
        Width = 32
        Height = 13
        Caption = 'pixel(s)'
      end
      object Label65: TLabel
        Left = 248
        Top = 643
        Width = 87
        Height = 13
        Caption = 'Vertical movement'
      end
      object Label66: TLabel
        Left = 16
        Top = 683
        Width = 63
        Height = 13
        Caption = 'mousepos 2='
      end
      object Label67: TLabel
        Left = 120
        Top = 683
        Width = 32
        Height = 13
        Caption = 'pixel(s)'
      end
      object Label68: TLabel
        Left = 224
        Top = 683
        Width = 63
        Height = 13
        Caption = 'mousepos 2='
      end
      object Label69: TLabel
        Left = 328
        Top = 683
        Width = 32
        Height = 13
        Caption = 'pixel(s)'
      end
      object Label70: TLabel
        Left = 11
        Top = 755
        Width = 69
        Height = 13
        Caption = 'mousepos 20='
      end
      object Label71: TLabel
        Left = 120
        Top = 755
        Width = 32
        Height = 13
        Caption = 'pixel(s)'
      end
      object Label72: TLabel
        Left = 219
        Top = 754
        Width = 69
        Height = 13
        Caption = 'mousepos 20='
      end
      object Label73: TLabel
        Left = 328
        Top = 754
        Width = 32
        Height = 13
        Caption = 'pixel(s)'
      end
      object Label74: TLabel
        Left = 11
        Top = 779
        Width = 69
        Height = 13
        Caption = 'mousepos 40='
      end
      object Label75: TLabel
        Left = 120
        Top = 779
        Width = 32
        Height = 13
        Caption = 'pixel(s)'
      end
      object Label76: TLabel
        Left = 219
        Top = 776
        Width = 69
        Height = 13
        Caption = 'mousepos 40='
      end
      object Label77: TLabel
        Left = 328
        Top = 776
        Width = 32
        Height = 13
        Caption = 'pixel(s)'
      end
      object Addresslabel: TLabel
        Left = 252
        Top = 234
        Width = 96
        Height = 13
        Caption = '                                '
      end
      object autoaimtoggle: TEdit
        Left = 6
        Top = 200
        Width = 163
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 13
        OnKeyDown = autoaimtoggleKeyDown
      end
      object Button13: TButton
        Left = 176
        Top = 202
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 14
        OnClick = Button13Click
      end
      object PreviousTexture: TEdit
        Left = 8
        Top = 392
        Width = 163
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 20
        OnKeyDown = PreviousTextureKeyDown
      end
      object Button16: TButton
        Left = 176
        Top = 394
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 21
        OnClick = Button16Click
      end
      object CheckBox2: TCheckBox
        Left = 8
        Top = 360
        Width = 113
        Height = 17
        Caption = 'Calibrate auto-aim'
        Checked = True
        State = cbChecked
        TabOrder = 19
        OnClick = CheckBox2Click
      end
      object Button17: TButton
        Left = 408
        Top = 394
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 51
        OnClick = Button17Click
      end
      object nexttexture: TEdit
        Left = 238
        Top = 392
        Width = 163
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 22
        OnKeyDown = nexttextureKeyDown
      end
      object Button18: TButton
        Left = 176
        Top = 432
        Width = 49
        Height = 19
        Caption = 'Clear'
        TabOrder = 24
        OnClick = Button18Click
      end
      object locktexture: TEdit
        Left = 8
        Top = 432
        Width = 163
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 23
        OnKeyDown = locktextureKeyDown
      end
      object IncreaseX: TEdit
        Left = 8
        Top = 486
        Width = 163
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 25
        OnKeyDown = IncreaseXKeyDown
      end
      object Button19: TButton
        Left = 176
        Top = 488
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 26
        OnClick = Button19Click
      end
      object DecreaseX: TEdit
        Left = 238
        Top = 486
        Width = 163
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 27
        OnKeyDown = DecreaseXKeyDown
      end
      object Button20: TButton
        Left = 408
        Top = 488
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 28
        OnClick = Button20Click
      end
      object IncreaseY: TEdit
        Left = 8
        Top = 526
        Width = 163
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 29
        OnKeyDown = IncreaseYKeyDown
      end
      object Button21: TButton
        Left = 176
        Top = 528
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 30
        OnClick = Button21Click
      end
      object DecreaseY: TEdit
        Left = 238
        Top = 526
        Width = 163
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 31
        OnKeyDown = DecreaseYKeyDown
      end
      object Button22: TButton
        Left = 408
        Top = 528
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 32
        OnClick = Button22Click
      end
      object IncreaseZ: TEdit
        Left = 8
        Top = 566
        Width = 163
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 33
        OnKeyDown = IncreaseZKeyDown
      end
      object Button23: TButton
        Left = 176
        Top = 568
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 34
        OnClick = Button23Click
      end
      object DecreaseZ: TEdit
        Left = 238
        Top = 566
        Width = 163
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 35
        OnKeyDown = DecreaseZKeyDown
      end
      object Button24: TButton
        Left = 408
        Top = 568
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 36
        OnClick = Button24Click
      end
      object aimfile1: TEdit
        Left = 312
        Top = 32
        Width = 145
        Height = 21
        TabOrder = 2
      end
      object setaimsetting1: TEdit
        Left = 6
        Top = 32
        Width = 235
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 0
        OnKeyDown = setaimsetting1KeyDown
      end
      object Button25: TButton
        Left = 248
        Top = 34
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 1
        OnClick = Button25Click
      end
      object aimfile2: TEdit
        Left = 312
        Top = 68
        Width = 145
        Height = 21
        TabOrder = 5
      end
      object setaimsetting2: TEdit
        Left = 6
        Top = 68
        Width = 235
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 3
        OnKeyDown = setaimsetting2KeyDown
      end
      object Button26: TButton
        Left = 248
        Top = 70
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 4
        OnClick = Button26Click
      end
      object aimfile3: TEdit
        Left = 312
        Top = 105
        Width = 145
        Height = 21
        TabOrder = 8
      end
      object setaimsetting3: TEdit
        Left = 6
        Top = 105
        Width = 235
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 6
        OnKeyDown = setaimsetting3KeyDown
      end
      object Button27: TButton
        Left = 248
        Top = 107
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 7
        OnClick = Button27Click
      end
      object loadaimsettingsfile: TEdit
        Left = 6
        Top = 152
        Width = 163
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 9
        OnKeyDown = loadaimsettingsfileKeyDown
      end
      object Button28: TButton
        Left = 176
        Top = 154
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 10
        OnClick = Button28Click
      end
      object Saveaimsettingsfile: TEdit
        Left = 238
        Top = 152
        Width = 163
        Height = 21
        ReadOnly = True
        TabOrder = 11
        OnKeyDown = SaveaimsettingsfileKeyDown
      end
      object Button29: TButton
        Left = 408
        Top = 154
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 12
        OnClick = Button29Click
      end
      object callibrationkey: TEdit
        Left = 8
        Top = 619
        Width = 163
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 37
        OnKeyDown = callibrationkeyKeyDown
      end
      object Button30: TButton
        Left = 176
        Top = 621
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 38
        OnClick = Button30Click
      end
      object mousecallibrationhorizontal1point: TEdit
        Left = 80
        Top = 656
        Width = 33
        Height = 21
        TabOrder = 39
        Text = '1.0'
      end
      object mousecallibrationhorizontal5point: TEdit
        Left = 80
        Top = 704
        Width = 33
        Height = 21
        TabOrder = 41
        Text = '5.0'
      end
      object mousecallibrationhorizontal10point: TEdit
        Left = 80
        Top = 728
        Width = 33
        Height = 21
        TabOrder = 42
        Text = '10.0'
      end
      object mousecallibrationvertical1point: TEdit
        Left = 288
        Top = 656
        Width = 33
        Height = 21
        TabOrder = 45
        Text = '1.0'
      end
      object mousecallibrationvertical5point: TEdit
        Left = 288
        Top = 704
        Width = 33
        Height = 21
        TabOrder = 47
        Text = '5.0'
      end
      object mousecallibrationvertical10point: TEdit
        Left = 288
        Top = 728
        Width = 33
        Height = 21
        TabOrder = 48
        Text = '10'
      end
      object mousecallibrationhorizontal2point: TEdit
        Left = 80
        Top = 680
        Width = 33
        Height = 21
        TabOrder = 40
        Text = '2.0'
      end
      object mousecallibrationvertical2point: TEdit
        Left = 288
        Top = 680
        Width = 33
        Height = 21
        TabOrder = 46
        Text = '2.0'
      end
      object mousecallibrationhorizontal20point: TEdit
        Left = 80
        Top = 752
        Width = 33
        Height = 21
        TabOrder = 43
        Text = '20.0'
      end
      object mousecallibrationvertical20point: TEdit
        Left = 288
        Top = 751
        Width = 33
        Height = 21
        TabOrder = 49
        Text = '20.0'
      end
      object mousecallibrationhorizontal40point: TEdit
        Left = 80
        Top = 776
        Width = 33
        Height = 21
        TabOrder = 44
        Text = '40.0'
      end
      object mousecallibrationvertical40point: TEdit
        Left = 288
        Top = 773
        Width = 33
        Height = 21
        TabOrder = 50
        Text = '40.0'
      end
      object Panel3: TPanel
        Left = 227
        Top = 200
        Width = 233
        Height = 17
        BevelOuter = bvNone
        TabOrder = 52
        object rbtoggleoneff: TRadioButton
          Left = 8
          Top = 0
          Width = 89
          Height = 17
          Caption = 'Toggle on/off'
          TabOrder = 0
          OnClick = rbtoggleoneffClick
        end
        object rbKeepDown: TRadioButton
          Left = 104
          Top = 0
          Width = 89
          Height = 17
          Caption = 'Keep pressed'
          Checked = True
          TabOrder = 1
          TabStop = True
          OnClick = rbtoggleoneffClick
        end
      end
      object autoshoot: TCheckBox
        Left = 332
        Top = 216
        Width = 97
        Height = 17
        Caption = 'Auto shoot'
        TabOrder = 15
      end
      object Panel4: TPanel
        Left = 2
        Top = 267
        Width = 393
        Height = 89
        BevelOuter = bvNone
        TabOrder = 53
        object Label29: TLabel
          Left = 5
          Top = 4
          Width = 62
          Height = 13
          Caption = 'Increase Lag'
        end
        object Label78: TLabel
          Left = 227
          Top = 4
          Width = 48
          Height = 13
          Caption = 'Defaultlag'
        end
        object Label30: TLabel
          Left = 5
          Top = 44
          Width = 67
          Height = 13
          Caption = 'Decrease Lag'
        end
        object Label31: TLabel
          Left = 227
          Top = 44
          Width = 46
          Height = 13
          Caption = 'Lag Delta'
        end
        object increaselag: TEdit
          Left = 6
          Top = 20
          Width = 163
          Height = 21
          PopupMenu = Mousekeymenu
          ReadOnly = True
          TabOrder = 0
          OnKeyDown = increaselagKeyDown
        end
        object Button14: TButton
          Left = 176
          Top = 23
          Width = 49
          Height = 17
          Caption = 'Clear'
          TabOrder = 1
          OnClick = Button14Click
        end
        object lag: TEdit
          Left = 231
          Top = 20
          Width = 38
          Height = 21
          TabOrder = 2
          Text = '0'
        end
        object decreaselag: TEdit
          Left = 6
          Top = 60
          Width = 163
          Height = 21
          PopupMenu = Mousekeymenu
          ReadOnly = True
          TabOrder = 3
          OnKeyDown = decreaselagKeyDown
        end
        object Button15: TButton
          Left = 176
          Top = 63
          Width = 49
          Height = 17
          Caption = 'Clear'
          TabOrder = 4
          OnClick = Button15Click
        end
        object lagdelta: TEdit
          Left = 231
          Top = 60
          Width = 38
          Height = 21
          TabOrder = 5
          Text = '10'
        end
      end
      object getlagfrommemory: TCheckBox
        Left = 8
        Top = 232
        Width = 161
        Height = 17
        Caption = 'Get lag from memory address'
        TabOrder = 16
        OnClick = getlagfrommemoryClick
      end
      object Button31: TButton
        Left = 168
        Top = 232
        Width = 81
        Height = 17
        Caption = 'Select address'
        Enabled = False
        TabOrder = 17
        OnClick = Button31Click
      end
      object UseFpsLag: TCheckBox
        Left = 8
        Top = 248
        Width = 137
        Height = 17
        Caption = 'Use lag caused by fps'
        TabOrder = 18
      end
    end
    object GroupBox4: TGroupBox
      Left = 8
      Top = 210
      Width = 465
      Height = 49
      Caption = 'Textures'
      TabOrder = 3
      object Label80: TLabel
        Left = 4
        Top = 14
        Width = 76
        Height = 26
        Caption = 'Save all loaded textures to disk'
        WordWrap = True
      end
      object SaveAllTextures: TEdit
        Left = 80
        Top = 16
        Width = 211
        Height = 21
        PopupMenu = Mousekeymenu
        ReadOnly = True
        TabOrder = 0
        OnKeyDown = SaveAllTexturesKeyDown
      end
      object Button34: TButton
        Left = 296
        Top = 18
        Width = 49
        Height = 17
        Caption = 'Clear'
        TabOrder = 1
        OnClick = Button34Click
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 500
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
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
      OnClick = LoadButtonClick
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
      OnClick = SaveButtonClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 368
    Width = 500
    Height = 53
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
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
