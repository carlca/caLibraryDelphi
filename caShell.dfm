object caSelectFolderForm: TcaSelectFolderForm
  Left = 400
  Top = 279
  Width = 500
  Height = 300
  BorderStyle = bsSizeToolWin
  BorderWidth = 2
  Caption = 'Select Folder'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object ShellTree: TShellTreeView
    Left = 0
    Top = 38
    Width = 488
    Height = 201
    ObjectTypes = [otFolders]
    Root = 'rfDesktop'
    UseShellImages = True
    Align = alClient
    AutoRefresh = False
    BorderStyle = bsNone
    Indent = 19
    ParentColor = False
    RightClickSelect = True
    ShowRoot = False
    TabOrder = 0
  end
  object ButtonPanel: TPanel
    Left = 0
    Top = 239
    Width = 488
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object OKButton: TButton
      Left = 104
      Top = 5
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object CancelButton: TButton
      Left = 184
      Top = 5
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object ToolbarPanel: TPanel
    Left = 0
    Top = 0
    Width = 488
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 2
    object DesktopButton: TcaSpeedButton
      Left = 3
      Top = 3
      Width = 100
      Height = 24
      Align = alLeft
      AllowAllUp = False
      AutoSizeMargin = 5
      Caption = 'Desktop'
      Color = clBtnFace
      Color3DLight = cl3DLight
      ColorBtnHighlight = clBtnHighlight
      ColorBtnShadow = clBtnShadow
      ColorWindowFrame = clWindowFrame
      DisableDown = False
      Down = False
      DownFont.Charset = DEFAULT_CHARSET
      DownFont.Color = clWindowText
      DownFont.Height = -11
      DownFont.Name = 'MS Sans Serif'
      DownFont.Style = []
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00000000000000
        000066667777777666606E6FF8F888886E6066FF0000008886606FFF7FFFF088
        88608FFF7FFFF08888708FFF7FFFF0F888708FFF7F0FF08F88708FFF7F7300F8
        F87068FF7F3B308F8860668F77738308F6606668FFFF3B306E606E668FFFF383
        066066666888883B308088888888888381888888888888889988}
      GlyphOffsetWhenDown = True
      GroupIndex = 1
      Layout = laGlyphLeftCentered
      MouseOverFont.Charset = DEFAULT_CHARSET
      MouseOverFont.Color = clWindowText
      MouseOverFont.Height = -11
      MouseOverFont.Name = 'MS Sans Serif'
      MouseOverFont.Style = []
      MouseOverStyle = bsFlat
      OnClick = ShellSelectClick
      Spacing = 12
      Style = bsFlat
      SyncDownFont = True
      SyncMouseOverFont = True
      TextOffsetWhenDown = True
      TextStyle = tsNormal
      TransparentGlyph = True
      XOffset = 0
      YOffset = 0
    end
    object MyComputerButton: TcaSpeedButton
      Left = 103
      Top = 3
      Width = 100
      Height = 24
      Align = alLeft
      AllowAllUp = False
      AutoSizeMargin = 5
      Caption = 'My Computer'
      Color = clBtnFace
      Color3DLight = cl3DLight
      ColorBtnHighlight = clBtnHighlight
      ColorBtnShadow = clBtnShadow
      ColorWindowFrame = clWindowFrame
      DisableDown = False
      Down = False
      DownFont.Charset = DEFAULT_CHARSET
      DownFont.Color = clWindowText
      DownFont.Height = -11
      DownFont.Name = 'MS Sans Serif'
      DownFont.Style = []
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00883000000000
        000080708888F8F888800770888FFF8833800738888F8F8F8887008871038810
        07880800000000000008800888FFFFFF8808008FE66666666F08088FEEEEEEEE
        CF78088F7E8EEEEE6F78088F7E88EEEE6F78088F7888E8EE6878088F7E888EEE
        CE7808FF870000004F780FFFFFFFFFFFF8788788888888887788}
      GlyphOffsetWhenDown = True
      GroupIndex = 1
      Layout = laGlyphLeftCentered
      MouseOverFont.Charset = DEFAULT_CHARSET
      MouseOverFont.Color = clWindowText
      MouseOverFont.Height = -11
      MouseOverFont.Name = 'MS Sans Serif'
      MouseOverFont.Style = []
      MouseOverStyle = bsFlat
      OnClick = ShellSelectClick
      Spacing = 4
      Style = bsFlat
      SyncDownFont = True
      SyncMouseOverFont = True
      TextOffsetWhenDown = True
      TextStyle = tsNormal
      TransparentGlyph = True
      XOffset = 0
      YOffset = 0
    end
    object NetworkButton: TcaSpeedButton
      Left = 203
      Top = 3
      Width = 100
      Height = 24
      Align = alLeft
      AllowAllUp = False
      AutoSizeMargin = 5
      Caption = 'Network'
      Color = clBtnFace
      Color3DLight = cl3DLight
      ColorBtnHighlight = clBtnHighlight
      ColorBtnShadow = clBtnShadow
      ColorWindowFrame = clWindowFrame
      DisableDown = False
      Down = False
      DownFont.Charset = DEFAULT_CHARSET
      DownFont.Color = clWindowText
      DownFont.Height = -11
      DownFont.Name = 'MS Sans Serif'
      DownFont.Style = []
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00804444444448
        88880077777777488888078F8F88F840000008000000008E8E8E808FFFFF8088
        80E807F7EEEEF088088808FE8E86F088E88808FE88E6F044444408F77777F078
        787708FFFFFF808FFFF7C777777707000007CFE2CEEC0848EE478C2A22EE0848
        8E4782F8A222087777478822FA22278888878888222888888888}
      GlyphOffsetWhenDown = True
      GroupIndex = 1
      Layout = laGlyphLeftCentered
      MouseOverFont.Charset = DEFAULT_CHARSET
      MouseOverFont.Color = clWindowText
      MouseOverFont.Height = -11
      MouseOverFont.Name = 'MS Sans Serif'
      MouseOverFont.Style = []
      MouseOverStyle = bsFlat
      OnClick = ShellSelectClick
      Spacing = 12
      Style = bsFlat
      SyncDownFont = True
      SyncMouseOverFont = True
      TextOffsetWhenDown = True
      TextStyle = tsNormal
      TransparentGlyph = True
      XOffset = 0
      YOffset = 0
    end
  end
  object UpperSpacer: TcaPanel
    Left = 0
    Top = 30
    Width = 488
    Height = 4
    Frame.FocusedSides = [sdLeft, sdTop, sdRight, sdBottom]
    Frame.FocusedStyle = fsRaisedPanel
    Frame.FocusedLineColor = clBtnShadow
    Frame.LineColor = clBtnShadow
    Frame.Sides = [sdBottom]
    Frame.Style = fsLowered
    Frame.SyncSides = False
    Frame.SyncStyle = False
    SubClassForm = False
    Transparent = False
    Align = alTop
    DoubleBuffered = False
    TabOrder = 3
  end
  object LowerSpacer: TcaPanel
    Left = 0
    Top = 34
    Width = 488
    Height = 4
    Frame.FocusedSides = [sdLeft, sdTop, sdRight, sdBottom]
    Frame.FocusedStyle = fsRaisedPanel
    Frame.FocusedLineColor = clBtnShadow
    Frame.LineColor = clBtnShadow
    Frame.Sides = []
    Frame.Style = fsRaisedPanel
    Frame.SyncSides = False
    Frame.SyncStyle = False
    SubClassForm = False
    Transparent = False
    Align = alTop
    DoubleBuffered = False
    TabOrder = 4
  end
end
