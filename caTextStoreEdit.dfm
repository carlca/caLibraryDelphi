object caEditTextForm: TcaEditTextForm
  Left = 351
  Top = 258
  Width = 389
  Height = 314
  BorderStyle = bsSizeToolWin
  BorderWidth = 2
  Caption = 'Edit Text List'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ListEditor: TValueListEditor
    Left = 0
    Top = 23
    Width = 377
    Height = 262
    Align = alClient
    BorderStyle = bsNone
    KeyOptions = [keyEdit, keyAdd, keyDelete, keyUnique]
    TabOrder = 0
    TitleCaptions.Strings = (
      'Name'
      'Value')
    ColWidths = (
      150
      225)
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 377
    Height = 23
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object OKButton: TcaXPButton
      Left = 0
      Top = 0
      Width = 57
      Height = 20
      Alignment = taCenter
      AutoSizeMargin = 5
      Caption = 'OK'
      DisabledTextColor = clBtnShadow
      EnabledTextColor = clWindowText
      FrameColor = clMedGray
      MouseOverFrameColor = clHighlight
      OnClick = OKButtonClick
    end
    object CancelButton: TcaXPButton
      Left = 60
      Top = 0
      Width = 57
      Height = 20
      Alignment = taCenter
      AutoSizeMargin = 5
      Caption = 'Cancel'
      DisabledTextColor = clBtnShadow
      EnabledTextColor = clWindowText
      FrameColor = clMedGray
      MouseOverFrameColor = clHighlight
      OnClick = CancelButtonClick
    end
    object ClipboardButton: TcaXPButton
      Left = 120
      Top = 0
      Width = 136
      Height = 20
      Alignment = taCenter
      AutoSizeMargin = 5
      Caption = 'Copy names to clipboard'
      DisabledTextColor = clBtnShadow
      EnabledTextColor = clWindowText
      FrameColor = clMedGray
      MouseOverFrameColor = clHighlight
      OnClick = ClipboardButtonClick
    end
  end
end
