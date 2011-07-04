object MemoryBrowser: TMemoryBrowser
  Left = 331
  Top = 409
  Width = 646
  Height = 501
  BorderIcons = [biSystemMenu]
  Caption = 'Memory Browser'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 497
    Top = 0
    Width = 4
    Height = 474
    Cursor = crHSplit
    Align = alRight
  end
  object GroupBox1: TGroupBox
    Left = 501
    Top = 0
    Width = 137
    Height = 474
    Align = alRight
    Anchors = [akLeft, akTop, akRight, akBottom]
    BiDiMode = bdLeftToRight
    Caption = 'Registers'
    ParentBiDiMode = False
    TabOrder = 0
    object Label271: TLabel
      Left = 16
      Top = 16
      Width = 21
      Height = 13
      Caption = 'EAX'
    end
    object Label289: TLabel
      Left = 16
      Top = 32
      Width = 21
      Height = 13
      Caption = 'EBX'
    end
    object Label1: TLabel
      Left = 16
      Top = 48
      Width = 21
      Height = 13
      Caption = 'ECX'
    end
    object Label2: TLabel
      Left = 16
      Top = 64
      Width = 22
      Height = 13
      Caption = 'EDX'
    end
    object Label3: TLabel
      Left = 16
      Top = 80
      Width = 17
      Height = 13
      Caption = 'ESI'
    end
    object Label4: TLabel
      Left = 16
      Top = 96
      Width = 18
      Height = 13
      Caption = 'EDI'
    end
    object Label6: TLabel
      Left = 16
      Top = 128
      Width = 21
      Height = 13
      Caption = 'ESP'
    end
    object Label5: TLabel
      Left = 16
      Top = 112
      Width = 21
      Height = 13
      Caption = 'EBP'
    end
    object Label7: TLabel
      Left = 16
      Top = 144
      Width = 17
      Height = 13
      Caption = 'EIP'
    end
    object Label9: TLabel
      Left = 16
      Top = 160
      Width = 14
      Height = 13
      Caption = 'CS'
    end
    object Label10: TLabel
      Left = 16
      Top = 176
      Width = 15
      Height = 13
      Caption = 'DS'
    end
    object Label11: TLabel
      Left = 16
      Top = 192
      Width = 14
      Height = 13
      Caption = 'SS'
    end
    object Label12: TLabel
      Left = 16
      Top = 208
      Width = 14
      Height = 13
      Caption = 'ES'
    end
    object Label13: TLabel
      Left = 16
      Top = 224
      Width = 13
      Height = 13
      Caption = 'FS'
    end
    object Label14: TLabel
      Left = 16
      Top = 240
      Width = 15
      Height = 13
      Caption = 'GS'
    end
    object Label16: TLabel
      Left = 16
      Top = 272
      Width = 13
      Height = 13
      Caption = 'CF'
    end
    object Label17: TLabel
      Left = 16
      Top = 288
      Width = 13
      Height = 13
      Caption = 'PF'
    end
    object Label18: TLabel
      Left = 16
      Top = 304
      Width = 13
      Height = 13
      Caption = 'AF'
    end
    object Label19: TLabel
      Left = 16
      Top = 320
      Width = 13
      Height = 13
      Caption = 'ZF'
    end
    object Label20: TLabel
      Left = 16
      Top = 336
      Width = 13
      Height = 13
      Caption = 'SF'
    end
    object Label21: TLabel
      Left = 16
      Top = 352
      Width = 14
      Height = 13
      Caption = 'OF'
    end
    object Label22: TLabel
      Left = 48
      Top = 16
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label23: TLabel
      Left = 48
      Top = 32
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label24: TLabel
      Left = 48
      Top = 48
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label25: TLabel
      Left = 48
      Top = 64
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label26: TLabel
      Left = 48
      Top = 80
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label27: TLabel
      Left = 48
      Top = 96
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label28: TLabel
      Left = 48
      Top = 112
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label29: TLabel
      Left = 48
      Top = 128
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label30: TLabel
      Left = 48
      Top = 144
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label32: TLabel
      Left = 48
      Top = 160
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label33: TLabel
      Left = 48
      Top = 176
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label34: TLabel
      Left = 48
      Top = 192
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label35: TLabel
      Left = 48
      Top = 208
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label36: TLabel
      Left = 48
      Top = 224
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label37: TLabel
      Left = 48
      Top = 240
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label38: TLabel
      Left = 48
      Top = 272
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label39: TLabel
      Left = 48
      Top = 288
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label40: TLabel
      Left = 48
      Top = 304
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label41: TLabel
      Left = 48
      Top = 320
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label42: TLabel
      Left = 48
      Top = 336
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
    object Label43: TLabel
      Left = 48
      Top = 352
      Width = 38
      Height = 13
      Caption = 'Label22'
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 497
    Height = 474
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 0
      Top = 289
      Width = 497
      Height = 4
      Cursor = crVSplit
      Align = alTop
      OnMoved = Splitter2Moved
    end
    object GroupBox3: TGroupBox
      Left = 0
      Top = 293
      Width = 497
      Height = 181
      Align = alClient
      Caption = 'GroupBox3'
      PopupMenu = memorypopup
      TabOrder = 0
      object AddressLabel0: TLabel
        Tag = 0
        Left = 5
        Top = 20
        Width = 36
        Height = 8
        Caption = 'Label8'
        Font.Charset = OEM_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Terminal'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object AddressLabel1: TLabel
        Tag = 1;
        Left = 5
        Top = 35
        Width = 36
        Height = 8
        Caption = 'Label8'
        Font.Charset = OEM_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Terminal'
        Font.Style = [fsBold]
        ParentFont = False
      end
//blaat


      object FControl2: TEdit
        Left = 48
        Top = 120
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'FControl2'
        Visible = False
      end
      object SpinButton1: TSpinButton
        Left = 475
        Top = 15
        Width = 20
        Height = 164
        Align = alRight
        DownGlyph.Data = {
          BA000000424DBA00000000000000420000002800000009000000060000000100
          1000030000007800000000000000000000000000000000000000007C0000E003
          00001F0000000042004200420042004200420042004200420000004200420042
          0042000000420042004200420000004200420042000000000000004200420042
          0000004200420000000000000000000000420042000000420000000000000000
          000000000000004200000042004200420042004200420042004200420000}
        TabOrder = 1
        UpGlyph.Data = {
          BA000000424DBA00000000000000420000002800000009000000060000000100
          1000030000007800000000000000000000000000000000000000007C0000E003
          00001F0000000042004200420042004200420042004200420000004200000000
          0000000000000000000000420000004200420000000000000000000000420042
          0000004200420042000000000000004200420042000000420042004200420000
          004200420042004200000042004200420042004200420042004200420000}
        OnDownClick = SpinButton1DownClick
        OnUpClick = SpinButton1UpClick
      end
    end
    object GroupBox2: TGroupBox
      Left = 0
      Top = 0
      Width = 497
      Height = 289
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Debugger'
      TabOrder = 1
      object Button2: TButton
        Left = 8
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Continue'
        TabOrder = 0
        OnClick = Button2Click
      end
      object Button4: TButton
        Left = 88
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Step'
        TabOrder = 1
        OnClick = Button4Click
      end
      object GroupBox4: TGroupBox
        Left = 8
        Top = 48
        Width = 425
        Height = 241
        Caption = 'GroupBox4'
        TabOrder = 2
        object ListBox1: TListBox
          Left = 2
          Top = 15
          Width = 421
          Height = 194
          Align = alTop
          ItemHeight = 13
          TabOrder = 0
        end
        object FControl1: TEdit
          Left = 304
          Top = 184
          Width = 121
          Height = 21
          TabOrder = 1
          Text = 'FControl1'
        end
        object Memo1: TMemo
          Left = 2
          Top = 209
          Width = 421
          Height = 30
          Align = alClient
          Lines.Strings = (
            'Memo1')
          TabOrder = 2
        end
      end
      object Button1: TButton
        Left = 200
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Button1'
        TabOrder = 3
        OnClick = Button1Click
      end
      object Edit1: TEdit
        Left = 288
        Top = 24
        Width = 25
        Height = 21
        TabOrder = 4
        Text = '81'
      end
      object Edit2: TEdit
        Left = 312
        Top = 24
        Width = 25
        Height = 21
        TabOrder = 5
        Text = '10'
      end
      object Edit3: TEdit
        Left = 336
        Top = 24
        Width = 25
        Height = 21
        TabOrder = 6
        Text = '1'
      end
      object Edit4: TEdit
        Left = 360
        Top = 24
        Width = 25
        Height = 21
        TabOrder = 7
        Text = '2'
      end
      object Edit5: TEdit
        Left = 384
        Top = 24
        Width = 25
        Height = 21
        TabOrder = 8
        Text = '3'
      end
      object Edit6: TEdit
        Left = 408
        Top = 24
        Width = 25
        Height = 21
        TabOrder = 9
        Text = '4'
      end
      object Button3: TButton
        Left = 440
        Top = 48
        Width = 41
        Height = 41
        Caption = 'Button3'
        TabOrder = 10
        OnClick = Button3Click
      end
      object Button5: TButton
        Left = 448
        Top = 120
        Width = 41
        Height = 33
        Caption = 'Button5'
        TabOrder = 11
        OnClick = Button5Click
      end
    end
  end
  object memorypopup: TPopupMenu
    Left = 360
    Top = 392
    object Goto1: TMenuItem
      Caption = 'Goto address'
      OnClick = Goto1Click
    end
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 16
    Top = 416
  end
end
