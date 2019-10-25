object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'MMDB Reader Sample'
  ClientHeight = 520
  ClientWidth = 776
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  DesignSize = (
    776
    520)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 151
    Top = 18
    Width = 56
    Height = 13
    Caption = 'IP Address:'
  end
  object Memo1: TMemo
    Left = 16
    Top = 56
    Width = 745
    Height = 441
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Button1: TButton
    Left = 16
    Top = 13
    Width = 113
    Height = 25
    Caption = 'Open MMDB'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 221
    Top = 15
    Width = 76
    Height = 21
    TabOrder = 2
    Text = '8.8.8.8'
  end
  object Button2: TButton
    Left = 303
    Top = 13
    Width = 122
    Height = 25
    Caption = 'Find IP Address'
    Enabled = False
    TabOrder = 3
    OnClick = Button2Click
  end
  object OpenDialog1: TOpenDialog
    Filter = 'MMDB files (*.mmdb)|*.mmdb|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 48
    Top = 72
  end
end
