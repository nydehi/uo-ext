object FMain: TFMain
  Left = 555
  Top = 160
  Width = 320
  Height = 150
  BorderIcons = [biSystemMenu]
  Caption = #1047#1072#1075#1088#1091#1079#1095#1080#1082
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    304
    112)
  PixelsPerInch = 96
  TextHeight = 13
  object LUOPath: TLabel
    Left = 8
    Top = 8
    Width = 46
    Height = 13
    Caption = 'Client.exe'
  end
  object LDLLName: TLabel
    Left = 8
    Top = 72
    Width = 48
    Height = 13
    Caption = #1048#1084#1103' DLL:'
  end
  object BRun: TButton
    Left = 200
    Top = 80
    Width = 88
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1047#1072#1087#1091#1089#1090#1080#1090#1100' '#1059#1054
    TabOrder = 0
    OnClick = BRunClick
  end
  object BPathSelect: TButton
    Left = 272
    Top = 24
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = BPathSelectClick
  end
  object CBCloseAfter: TCheckBox
    Left = 8
    Top = 56
    Width = 281
    Height = 15
    Anchors = [akLeft, akTop, akRight]
    Caption = #1047#1072#1082#1088#1099#1090#1100' '#1087#1086#1089#1083#1077' '#1079#1072#1087#1091#1089#1082#1072
    TabOrder = 2
    OnClick = NeedSaveOptions
  end
  object EUOPath: TEdit
    Left = 8
    Top = 24
    Width = 257
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    OnChange = NeedSaveOptions
  end
  object EDLLName: TEdit
    Left = 8
    Top = 88
    Width = 185
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    Text = 'ProtocolExtender.dll'
  end
  object OD: TOpenDialog
    Filter = #1042#1099#1087#1086#1083#1085#1103#1077#1084#1099#1077' '#1092#1072#1081#1083#1099' (*.exe)|*.exe|'#1042#1089#1077' '#1092#1072#1081#1083#1099' (*.*)|*.*'
    Title = #1042#1099#1073#1077#1088#1080#1090#1077' Client.exe '#1080#1079' '#1087#1072#1087#1082#1080' '#1089' '#1082#1083#1080#1077#1085#1090#1086#1084' Ultima Online'
    Left = 16
    Top = 8
  end
end
