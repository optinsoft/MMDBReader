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
  object Label2: TLabel
    Left = 16
    Top = 51
    Width = 81
    Height = 13
    Caption = 'Display IP count:'
  end
  object Label3: TLabel
    Left = 558
    Top = 18
    Width = 54
    Height = 13
    Caption = 'Exporting..'
    Visible = False
  end
  object Memo1: TMemo
    Left = 16
    Top = 75
    Width = 745
    Height = 422
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
    Left = 213
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
  object Edit2: TEdit
    Left = 103
    Top = 48
    Width = 57
    Height = 21
    TabOrder = 4
    Text = '1000'
  end
  object CheckBox1: TCheckBox
    Left = 176
    Top = 50
    Width = 73
    Height = 17
    Caption = 'IPv4 only'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object Button3: TButton
    Left = 439
    Top = 13
    Width = 105
    Height = 25
    Caption = 'Export to CSV...'
    Enabled = False
    TabOrder = 6
    OnClick = Button3Click
  end
  object OpenDialog1: TOpenDialog
    Filter = 'MMDB files (*.mmdb)|*.mmdb|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 48
    Top = 144
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.CSV'
    Filter = 
      'CSV files (*.CSV)|*.CSV|TXT files (*.TXT)|*.TXT|All files (*.*)|' +
      '*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 128
    Top = 144
  end
end
