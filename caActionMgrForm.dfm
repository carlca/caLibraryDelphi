object caActionManagerForm: TcaActionManagerForm
  Left = 318
  Top = 249
  Width = 707
  Height = 354
  BorderStyle = bsSizeToolWin
  BorderWidth = 2
  Caption = 'Action Manager'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LeftSplitter: TSplitter
    Left = 237
    Top = 28
    Width = 6
    Height = 295
    Cursor = crHSplit
  end
  object RightSplitter: TSplitter
    Left = 428
    Top = 28
    Width = 6
    Height = 295
    Cursor = crHSplit
  end
  object ControlsPanel: TPanel
    Left = 0
    Top = 28
    Width = 237
    Height = 295
    Align = alLeft
    Caption = 'ControlsPanel'
    TabOrder = 0
    object ControlsLabel: TLabel
      Left = 1
      Top = 1
      Width = 235
      Height = 18
      Align = alTop
      AutoSize = False
      Caption = ' Controls'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object ControlsList: TListBox
      Left = 1
      Top = 19
      Width = 235
      Height = 275
      Align = alClient
      BorderStyle = bsNone
      ItemHeight = 13
      MultiSelect = True
      Sorted = True
      TabOrder = 0
    end
  end
  object LinksPanel: TPanel
    Left = 434
    Top = 28
    Width = 261
    Height = 295
    Align = alClient
    Caption = 'Panel1'
    TabOrder = 1
    object LinksLabel: TLabel
      Left = 1
      Top = 1
      Width = 259
      Height = 18
      Align = alTop
      AutoSize = False
      Caption = ' Links'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object LinksList: TListBox
      Left = 1
      Top = 19
      Width = 259
      Height = 275
      Align = alClient
      BorderStyle = bsNone
      ItemHeight = 13
      MultiSelect = True
      Sorted = True
      TabOrder = 0
    end
  end
  object ActionPanel: TPanel
    Left = 243
    Top = 28
    Width = 185
    Height = 295
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 2
    object ActionsLabel: TLabel
      Left = 1
      Top = 1
      Width = 183
      Height = 18
      Align = alTop
      AutoSize = False
      Caption = ' Actions'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object ActionsList: TListBox
      Left = 1
      Top = 19
      Width = 183
      Height = 275
      Align = alClient
      BorderStyle = bsNone
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
    end
  end
  object ToolbarPanel: TPanel
    Left = 0
    Top = 0
    Width = 695
    Height = 28
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object CloseButton: TSpeedButton
      Left = 3
      Top = 2
      Width = 63
      Height = 22
      Caption = 'Close'
      Flat = True
      OnClick = CloseButtonClick
    end
    object LinkButton: TSpeedButton
      Left = 303
      Top = 2
      Width = 63
      Height = 22
      Caption = 'Link'
      Flat = True
      OnClick = LinkButtonClick
    end
    object UnlinkButton: TSpeedButton
      Left = 371
      Top = 2
      Width = 63
      Height = 22
      Caption = 'Unlink'
      Flat = True
      OnClick = UnlinkButtonClick
    end
    object CreateActionsButton: TSpeedButton
      Left = 213
      Top = 2
      Width = 85
      Height = 22
      Caption = 'Create Actions'
      Flat = True
      OnClick = CreateActionsButtonClick
    end
  end
end
