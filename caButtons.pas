{ This file is part of the caLibrary (for Delphi 7) package

  Copyright (C) 1999-2017 - Carl Caulkett - carl.caulkett@gmail.com

  MODIFIED LGPL Licence - this is the same licence as that used by the Free Pascal Compiler (FPC)
  A copy of the full licence can be found in the file Licence.md in the same folder as this file.

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version
  with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this library with independent
  modules to produce an executable, regardless of the license terms of these independent modules, and to copy and distribute the
  resulting executable under terms of your choice, provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a module which is not derived from or based on this
  library. If you modify this library, you may extend this exception to your version of the library, but you are not obligated
  to do so. If you do not wish to do so, delete this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to the Free
  Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}


unit caButtons;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  Classes,
  Messages,
  Controls,
  Sysutils,
  Graphics,
  Math,

  // ca units 
  caLog,
  caUtils,
  caTypes,
  caConsts,
  caClasses,
  caControls,
  caTimer,
  caGraphics;

type

  TcaDrawButtonEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect; IsDown: Boolean) of object;

  //---------------------------------------------------------------------------
  // TcaCustomButtonProperties                                                 
  //---------------------------------------------------------------------------

  TcaCustomButtonProperties = class(TcaNotifyProperties)
  private
    // Property fields 
    FColor: TColor;
    FColor3DLight: TColor;
    FColorBtnHighlight: TColor;
    FColorBtnShadow: TColor;
    FColorWindowFrame: TColor;
    FFont: TFont;
    FHeight: Integer;
    FLeft: Integer;
    FStyle: TcaButtonStyle;
    FTop: Integer;
    FWidth: Integer;
    // Property methods 
    function GetBoundsRect: TRect;
    procedure SetBoundsRect(const Value: TRect);
    procedure SetColor(const Value: TColor);
    procedure SetColor3DLight(const Value: TColor);
    procedure SetColorBtnHighlight(const Value: TColor);
    procedure SetColorBtnShadow(const Value: TColor);
    procedure SetColorWindowFrame(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetHeight(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetStyle(const Value: TcaButtonStyle);
    procedure SetTop(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
    // Protected property methods 
    procedure SetButtonStyleProperty(var AProperty: TcaButtonStyle; AValue: TcaButtonStyle);
    procedure SetColorProperty(var AProperty: TColor; AValue: Integer);
    // Protected virtual methods 
    procedure Initialize; override;
    // Protected properties 
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property Color: TColor read FColor write SetColor;
    property Color3DLight: TColor read FColor3DLight write SetColor3DLight;
    property ColorBtnHighlight: TColor read FColorBtnHighlight write SetColorBtnHighlight;
    property ColorBtnShadow: TColor read FColorBtnShadow write SetColorBtnShadow;
    property ColorWindowFrame: TColor read FColorWindowFrame write SetColorWindowFrame;
    property Font: TFont read FFont write SetFont;
    property Style: TcaButtonStyle read FStyle write SetStyle;
    property Height: Integer read FHeight write SetHeight;
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
  public
    // Create/Destroy 
    constructor Create(ANotifyMethod: TcaProcedure); override;
    destructor Destroy; override;
    // Public methods 
    procedure Assign(Source: TPersistent); override;
  end;

  //---------------------------------------------------------------------------
  // TcaButtonProperties                                                       
  //---------------------------------------------------------------------------

  TcaButtonProperties = class(TcaCustomButtonProperties)
  published
    // Promoted properties 
    property Color;
    property Color3DLight;
    property ColorBtnHighlight;
    property ColorBtnShadow;
    property ColorWindowFrame;
    property Font;
    property Style;
    property Height;
    property Left;
    property Top;
    property Width;
  end;

  //---------------------------------------------------------------------------
  // TcaButtonFace                                                             
  //---------------------------------------------------------------------------

  TcaButtonFace = class(TObject)
  private
    // Private fields 
    FButtonHeight: Integer;
    FButtonLayout: TcaButtonLayout;
    FButtonWidth: Integer;
    FCanvas: TCanvas;
    FCaption: TCaption;
    FCaptionLeft: Integer;
    FCaptionTop: Integer;
    FGlyphHeight: Integer;
    FGlyphLeft: Integer;
    FGlyphTop: Integer;
    FGlyphWidth: Integer;
    FHeight: Integer;
    FLeft: Integer;
    FSpacing: Integer;
    FTop: Integer;
    FWidth: Integer;
    // Property fields 
    FButton: TControl;
    FCaptionRect: TRect;
    FDown: Boolean;
    FGlyph: TBitmap;
    FGlyphRect: TRect;
    // Property methods 
    procedure SetButton(const Value: TControl);
    procedure SetDown(const Value: Boolean);
    // Private methods 
    procedure GlyphBottomLayout;
    procedure GlyphLeftCenteredLayout;
    procedure GlyphLeftLayout;
    procedure GlyphRightLayout;
    procedure GlyphTopLayout;
    procedure HorizontalLayout;
    procedure TextLeftLayout;
    procedure TextRightLayout;
    procedure UpdateBounds;
    procedure UpdateCaptionRect;
    procedure UpdateGlyphRect;
    procedure VerticalLayout;
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
    // Properties 
    property Button: TControl read FButton write SetButton;
    property CaptionRect: TRect read FCaptionRect;
    property Down: Boolean read FDown write SetDown;
    property Glyph: TBitmap read FGlyph;
    property GlyphRect: TRect read FGlyphRect;
  end;

  //---------------------------------------------------------------------------
  // TcaButtonEdges                                                            
  //---------------------------------------------------------------------------

  TcaButtonEdges = class(TObject)
  private
    // Property fields 
    FCanvas: TCanvas;
    FColor3DLight: TColor;
    FColorBtnColor: TColor;
    FColorBtnHighlight: TColor;
    FColorBtnShadow: TColor;
    FColorWindowFrame: TColor;
    FDown: Boolean;
    FEnabled: Boolean;
    FIsRunTime: Boolean;
    FMouseOver: Boolean;
    FMouseOverStyle: TcaButtonStyle;
    FRect: TcaRect;
    FStyle: TcaButtonStyle;
    FUseMouseOver: Boolean;
    // Private methods 
    function XY(AX, AY: Integer): TPoint;
    procedure DrawFlatEdge;
    procedure DrawNoEdge;
    procedure DrawNormalEdge;
    procedure DrawPoly(AColor: TColor; Points: array of TPoint);
    procedure DrawThinEdge;
    procedure UpdateCoords(var L0, L1, T0, T1, R0, R1, R2, B0, B1, B2: Integer);
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
    // Public methods 
    procedure DrawEdges;
    procedure SetBounds(const ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetRect(const ALeft, ATop, ARight, ABottom: Integer);
    // Properties 
    property Canvas: TCanvas read FCanvas write FCanvas;
    property Color3DLight: TColor read FColor3DLight write FColor3DLight;
    property ColorBtnColor: TColor read FColorBtnColor write FColorBtnColor;
    property ColorBtnHighlight: TColor read FColorBtnHighlight write FColorBtnHighlight;
    property ColorBtnShadow: TColor read FColorBtnShadow write FColorBtnShadow;
    property ColorWindowFrame: TColor read FColorWindowFrame write FColorWindowFrame;
    property Down: Boolean read FDown write FDown;
    property Enabled: Boolean read FEnabled write FEnabled;
    property IsRunTime: Boolean read FIsRunTime write FIsRunTime;
    property MouseOver: Boolean read FMouseOver write FMouseOver;
    property MouseOverStyle: TcaButtonStyle read FMouseOverStyle write FMouseOverStyle;
    property Rect: TcaRect read FRect;
    property Style: TcaButtonStyle read FStyle write FStyle;
    property UseMouseOver: Boolean read FUseMouseOver write FUseMouseOver;
  end;

  //---------------------------------------------------------------------------
  // TcaCustomSpeedButton                                                      
  //---------------------------------------------------------------------------

  TcaCustomSpeedButton = class(TcaGraphicControl)
  private
    // Property fields 
    FAllowAllUp: Boolean;
    FButton: TMouseButton;
    FColor3DLight: TColor;
    FColorBtnHighlight: TColor;
    FColorBtnShadow: TColor;
    FColorWindowFrame: TColor;
    FDisableDown: Boolean;
    FDown: Boolean;
    FDownFont: TFont;
    FGlyph: TBitmap;
    FGlyphOffsetWhenDown: Boolean;
    FGroupIndex: Integer;
    FLayout: TcaButtonLayout;
    FMouseIsDown: Boolean;
    FMouseOver: Boolean;
    FMouseOverFont: TFont;
    FMouseOverStyle: TcaButtonStyle;
    FNumGlyphs: TcaNumGlyphs;
    FOnDrawButton: TcaDrawButtonEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnTextChanged: TNotifyEvent;
    FProperties: TcaCustomButtonProperties;
    FShift: TShiftState;
    FShiftState: TShiftState;
    FSpacing: Integer;
    FState: TcaButtonState;
    FStyle: TcaButtonStyle;
    FSyncDownFont: Boolean;
    FSyncMouseOverFont: Boolean;
    FTextOffsetWhenDown: Boolean;
    FTextStyle: TcaTextStyle;
    FTransparentGlyph: Boolean;
    FUseMouseOver: Boolean;
    FXOffset: Integer;
    FXPos: Integer;
    FYOffset: Integer;
    FYPos: Integer;
    // Property methods 
    procedure SetAllowAllUp(const Value: Boolean);
    procedure SetColor3DLight(const Value: TColor);
    procedure SetColorBtnHighlight(const Value: TColor);
    procedure SetColorBtnShadow(const Value: TColor);
    procedure SetColorWindowFrame(const Value: TColor);
    procedure SetDisableDown(const Value: Boolean);
    procedure SetDown(const Value: Boolean);
    procedure SetDownFont(const Value: TFont);
    procedure SetGlyph(const Value: TBitmap);
    procedure SetGroupIndex(const Value: Integer);
    procedure SetLayout(const Value: TcaButtonLayout);
    procedure SetMouseOverFont(const Value: TFont);
    procedure SetNumGlyphs(const Value: TcaNumGlyphs);
    procedure SetProperties(const Value: TcaCustomButtonProperties);
    procedure SetSpacing(const Value: Integer);
    procedure SetStyle(const Value: TcaButtonStyle);
    procedure SetTextStyle(const Value: TcaTextStyle);
    procedure SetTransparentGlyph(const Value: Boolean);
    procedure SetXOffset(const Value: Integer);
    procedure SetYOffset(const Value: Integer);
    // Private methods 
    function GetButtonColor: TColor;
    procedure NotifyGroupButtons;
    procedure UpdateProperties;
    procedure UpdateFromProperties;
    // Component message handlers 
    procedure CAMButtonPressed(var Message: TMessage); message CAM_BUTTONPRESSED;
    procedure CAMGlyphListChanged(var Message: TMessage); message CAM_GLYPHLISTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
  protected
    // Virtual protected methods 
    function CanClick: Boolean; virtual;
    procedure DoDrawButton(ACanvas: TCanvas; ARect: TRect; IsDown: Boolean); virtual;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure DoTextChanged; virtual;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure BufferedPaint(C: TCanvas; R: TRect); override;
    // Static protected methods 
    procedure Erase;
    // Properties 
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp;
    property Color3DLight: TColor read FColor3DLight write SetColor3DLight;
    property ColorBtnHighlight: TColor read FColorBtnHighlight write SetColorBtnHighlight;
    property ColorBtnShadow: TColor read FColorBtnShadow write SetColorBtnShadow;
    property ColorWindowFrame: TColor read FColorWindowFrame write SetColorWindowFrame;
    property DisableDown: Boolean read FDisableDown write SetDisableDown;
    property Down: Boolean read FDown write SetDown;
    property DownFont: TFont read FDownFont write SetDownFont;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphOffsetWhenDown: Boolean read FGlyphOffsetWhenDown write FGlyphOffsetWhenDown;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex;
    property Layout: TcaButtonLayout read FLayout write SetLayout;
    property MouseIsDown: Boolean read FMouseIsDown write FMouseIsDown;
    property MouseOverFont: TFont read FMouseOverFont write SetMouseOverFont;
    property MouseOverStyle: TcaButtonStyle read FMouseOverStyle write FMouseOverStyle;
    property NumGlyphs: TcaNumGlyphs read FNumGlyphs write SetNumGlyphs default 1;
    property OnDrawButton: TcaDrawButtonEvent read FOnDrawButton write FOnDrawButton;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnTextChanged: TNotifyEvent read FOnTextChanged write FOnTextChanged;
    property Properties: TcaCustomButtonProperties read FProperties write SetProperties;
    property ShiftState: TShiftState read FShiftState write FShiftState;
    property Spacing: Integer read FSpacing write SetSpacing;
    property State: TcaButtonState read FState write FState;
    property Style: TcaButtonStyle read FStyle write SetStyle;
    property SyncDownFont: Boolean read FSyncDownFont write FSyncDownFont;
    property SyncMouseOverFont: Boolean read FSyncMouseOverFont write FSyncMouseOverFont;
    property TextOffsetWhenDown: Boolean read FTextOffsetWhenDown write FTextOffsetWhenDown;
    property TextStyle: TcaTextStyle read FTextStyle write SetTextStyle;
    property TransparentGlyph: Boolean read FTransparentGlyph write SetTransparentGlyph;
    property UseMouseOver: Boolean read FUseMouseOver write FUseMouseOver;
    property XOffset: Integer read FXOffset write SetXOffset;
    property YOffset: Integer read FYOffset write SetYOffset;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
    procedure Click; override;
  end;

  //---------------------------------------------------------------------------
  // TcaSpeedButton                                                            
  //---------------------------------------------------------------------------

  TcaSpeedButton = class(TcaCustomSpeedButton)
  public
    // Public properties 
    property OnTextChanged;
    property Properties;
  published
    // Promoted properties 
    property Action;
    property Align;
    property AllowAllUp;
    property Anchors;
    property AutoSize;
    property AutoSizeMargin;
    property BiDiMode;
    property Caption;
    property Color;
    property Color3DLight;
    property ColorBtnHighlight;
    property ColorBtnShadow;
    property ColorWindowFrame;
    property Constraints;
    property DisableDown;
    property Down;
    property DownFont;
    property DragKind;
    property Enabled;
    property Font;
    property Glyph;
    property GlyphOffsetWhenDown;
    property GroupIndex;
    property Layout;
    property MouseOverFont;
    property MouseOverStyle;
    property NumGlyphs;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDrawButton;
    property OnEndDock;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Spacing;
    property Style;
    property SyncDownFont;
    property SyncMouseOverFont;
    property TextOffsetWhenDown;
    property TextStyle;
    property TransparentGlyph;
    property Visible;
    property XOffset;
    property YOffset;
  end;

  //----------------------------------------------------------------------------
  // TcaDesignTimeButton                                                        
  //----------------------------------------------------------------------------

  TcaDesignTimeButton = class(TcaSpeedButton)
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
  end;

  //----------------------------------------------------------------------------
  // TcaRepeatSpeedButton                                                       
  //----------------------------------------------------------------------------

  TcaRepeatSpeedButton = class(TcaSpeedButton)
  private
    // Private fields 
    FTimerClicked: Boolean;
    FRepeatTimer: TcaTimer;
    // Property fields 
    FDelay: Integer;
    FInterval: Integer;
    FAutoRepeat: Boolean;
    // Event handlers 
    procedure TimerExpired(Sender: TObject);
  protected
    // Virtual protected methods 
    function CanClick: Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    // Create/Destroy 
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    // Published properties 
    property Delay: Integer read FDelay write FDelay;
    property Interval: Integer read FInterval write FInterval;
    property AutoRepeat: Boolean read FAutoRepeat write FAutoRepeat;
  end;

implementation

{$IFDEF D7_UP}
uses Types;
{$ENDIF}

  //---------------------------------------------------------------------------
  // TcaCustomButtonProperties                                                 
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaCustomButtonProperties.Create(ANotifyMethod: TcaProcedure);
begin
  inherited;
  FFont := TFont.Create;
end;

destructor TcaCustomButtonProperties.Destroy;
begin
  FFont.Free;
  inherited;
end;

  // Public methods 

procedure TcaCustomButtonProperties.Assign(Source: TPersistent);
var
  SourceProps: TcaCustomButtonProperties;
begin
  if Source is TcaCustomButtonProperties then
    begin
      SourceProps := TcaCustomButtonProperties(Source);
      FColor := SourceProps.Color;
      FColor3DLight := SourceProps.Color3DLight;
      FColorBtnHighlight := SourceProps.ColorBtnHighlight;
      FColorBtnShadow := SourceProps.ColorBtnShadow;
      FColorWindowFrame := SourceProps.ColorWindowFrame;
      FFont.Assign(SourceProps.Font);
      FStyle := SourceProps.Style;
    end
  else
    inherited;
end;

  // Protected virtual methods 

procedure TcaCustomButtonProperties.Initialize;
begin
  FColor := clBtnFace;
  FColor3DLight := cl3DLight;
  FColorBtnHighlight := clBtnHighlight;
  FColorBtnShadow := clBtnShadow;
  FColorWindowFrame := clWindowFrame;
end;

  // Protected property methods 

procedure TcaCustomButtonProperties.SetButtonStyleProperty(var AProperty: TcaButtonStyle; AValue: TcaButtonStyle);
begin
  if AValue <> AProperty then
    begin
      AProperty := AValue;
      Changed;
    end;
end;

procedure TcaCustomButtonProperties.SetColorProperty(var AProperty: TColor; AValue: Integer);
begin
  if AValue <> AProperty then
    begin
      AProperty := AValue;
      Changed;
    end;
end;

  // Property methods 

function TcaCustomButtonProperties.GetBoundsRect: TRect;
begin
  Result.Left := FLeft;
  Result.Right := FLeft + FWidth;
  Result.Top := FTop;
  Result.Bottom := FTop + FHeight;
end;

procedure TcaCustomButtonProperties.SetBoundsRect(const Value: TRect);
begin
  FLeft := Value.Left;
  FWidth := Value.Right - FLeft;
  FTop := Value.Top;
  FHeight := Value.Bottom - FTop;
end;

procedure TcaCustomButtonProperties.SetColor(const Value: TColor);
begin
  SetColorProperty(FColor, Value);
end;

procedure TcaCustomButtonProperties.SetColor3DLight(const Value: TColor);
begin
  SetColorProperty(FColor3DLight, Value);
end;

procedure TcaCustomButtonProperties.SetColorBtnHighlight(const Value: TColor);
begin
  SetColorProperty(FColorBtnHighlight, Value);
end;

procedure TcaCustomButtonProperties.SetColorBtnShadow(const Value: TColor);
begin
  SetColorProperty(FColorBtnShadow, Value);
end;

procedure TcaCustomButtonProperties.SetColorWindowFrame(const Value: TColor);
begin
  SetColorProperty(FColorWindowFrame, Value);
end;

procedure TcaCustomButtonProperties.SetFont(const Value: TFont);
begin
  if not Utils.FontsEqual(FFont, Value) then
    begin
      FFont.Assign(Value);
      Changed;
    end;
end;

procedure TcaCustomButtonProperties.SetHeight(const Value: Integer);
begin
  SetIntegerProperty(FHeight, Value);
end;

procedure TcaCustomButtonProperties.SetLeft(const Value: Integer);
begin
  SetIntegerProperty(FLeft, Value);
end;

procedure TcaCustomButtonProperties.SetStyle(const Value: TcaButtonStyle);
begin
  SetButtonStyleProperty(FStyle, Value);
end;

procedure TcaCustomButtonProperties.SetTop(const Value: Integer);
begin
  SetIntegerProperty(FTop, Value);
end;

procedure TcaCustomButtonProperties.SetWidth(const Value: Integer);
begin
  SetIntegerProperty(FWidth, Value);
end;

  //----------------------------------------------------------------------------
  // TcaButtonFace                                                              
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaButtonFace.Create;
begin
  inherited;
  FGlyph := TBitmap.Create;
end;

destructor TcaButtonFace.Destroy;
begin
  FGlyph.Free;
  inherited;
end;

  // Private methods 

procedure TcaButtonFace.GlyphBottomLayout;
begin
  VerticalLayout;
  FCaptionLeft := (FButtonWidth div 2) - (FCanvas.TextWidth(FCaption) div 2);
  FCaptionTop := FTop;
  FGlyphLeft := (FButtonWidth div 2) - (FGlyphWidth div 2);
  FGlyphTop := FTop + FCanvas.TextHeight(FCaption) + FSpacing;
end;

procedure TcaButtonFace.GlyphLeftCenteredLayout;
begin
  HorizontalLayout;
  FGlyphLeft := FLeft;
  FGlyphTop := (FButtonHeight div 2) - (FGlyphHeight div 2);
  FCaptionLeft := FLeft + FGlyphWidth + FSpacing;
  FCaptionTop := (FButtonHeight div 2) - (FCanvas.TextHeight(FCaption) div 2);
end;

procedure TcaButtonFace.GlyphLeftLayout;
begin
  HorizontalLayout;
  FGlyphLeft := FSpacing;
  FGlyphTop := (FButtonHeight div 2) - (FGlyphHeight div 2);
  FCaptionLeft := FSpacing + FGlyphWidth + FSpacing;
  FCaptionTop := (FButtonHeight div 2) - (FCanvas.TextHeight(FCaption) div 2);
end;

procedure TcaButtonFace.GlyphRightLayout;
begin
  HorizontalLayout;
  FCaptionLeft := FLeft;
  FCaptionTop := (FButtonHeight div 2) - (FCanvas.TextHeight(FCaption) div 2);
  FGlyphLeft := FLeft + FCanvas.TextWidth(FCaption) + FSpacing;
  FGlyphTop := (FButtonHeight div 2) - (FGlyphHeight div 2);
end;

procedure TcaButtonFace.GlyphTopLayout;
begin
  VerticalLayout;
  FGlyphLeft := (FButtonWidth div 2) - (FGlyphWidth div 2);
  FGlyphTop := FTop;
  FCaptionLeft := (FButtonWidth div 2) - (FCanvas.TextWidth(FCaption) div 2);
  FCaptionTop := FTop + FGlyphHeight + FSpacing;
end;

procedure TcaButtonFace.HorizontalLayout;
begin
  FWidth := FGlyphWidth + FSpacing + FCanvas.TextWidth(FCaption);
  FHeight := Max(FGlyphHeight, FCanvas.TextHeight(FCaption));
  FLeft := (FButtonWidth div 2) - (FWidth div 2);
  FTop := (FButtonHeight div 2) - (FHeight div 2);
end;

procedure TcaButtonFace.TextLeftLayout;
begin
  HorizontalLayout;
  FCaptionLeft := 4;
  FCaptionTop := (FButtonHeight div 2) - (FCanvas.TextHeight(FCaption) div 2);
  FGlyphLeft := -FGlyphWidth * 2;
  FGlyphTop := -FGlyphHeight * 2;
end;

procedure TcaButtonFace.TextRightLayout;
begin
  HorizontalLayout;
  FCaptionLeft := FButton.Width - (FCanvas.TextWidth(FCaption) div 2) - 4;
  FCaptionTop := (FButtonHeight div 2) - (FCanvas.TextHeight(FCaption) div 2);
  FGlyphLeft := -FGlyphWidth * 2;
  FGlyphTop := -FGlyphHeight * 2;
end;

procedure TcaButtonFace.UpdateBounds;
var
  TextOffset: Boolean;
  GlyphOffset: Boolean;
begin
  case
    FButtonLayout of
      laGlyphLeft:          GlyphLeftLayout;
      laGlyphLeftCentered:  GlyphLeftCenteredLayout;
      laGlyphRight:         GlyphRightLayout;
      laGlyphTop:           GlyphTopLayout;
      laGlyphBottom:        GlyphBottomLayout;
      laTextLeft:           TextLeftLayout;
      laTextRight:          TextRightLayout;
    end;
  UpdateCaptionRect;
  UpdateGlyphRect;
  if FDown then
    begin
      TextOffset := True;
      GlyphOffset := True;
      if FButton is TcaSpeedButton then
        begin
          TextOffset := TcaSpeedButton(FButton).TextOffsetWhenDown;
          GlyphOffset := TcaSpeedButton(FButton).GlyphOffsetWhenDown;
        end;
      if TextOffset then
        OffsetRect(FCaptionRect, 1, 1);
      if GlyphOffset then
        OffsetRect(FGlyphRect, 1, 1);
    end;
end;

procedure TcaButtonFace.UpdateCaptionRect;
begin
  FCaptionRect.Left := FCaptionLeft;
  FCaptionRect.Right := FCaptionLeft + FCanvas.TextWidth(FCaption);
  FCaptionRect.Top := FCaptionTop;
  FCaptionRect.Bottom := FCaptionTop + FCanvas.TextHeight(FCaption);
end;

procedure TcaButtonFace.UpdateGlyphRect;
begin
  FGlyphRect.Left := FGlyphLeft;
  FGlyphRect.Right := FGlyphLeft + FGlyphWidth;
  FGlyphRect.Top := FGlyphTop;
  FGlyphRect.Bottom := FGlyphTop + FGlyphHeight;
end;

procedure TcaButtonFace.VerticalLayout;
begin
  FWidth := Max(FGlyphWidth, FCanvas.TextWidth(FCaption));
  FHeight := FGlyphHeight + FSpacing + FCanvas.TextHeight(FCaption);
  FLeft := (FButtonWidth div 2) - (FWidth div 2);
  FTop := (FButtonHeight div 2) - (FHeight div 2);
end;

  // Property methods 

procedure TcaButtonFace.SetButton(const Value: TControl);
var
  OriginalGlyph: TBitmap;
  NumGlyphs: TcaNumGlyphs;
  SplitWidth: Integer;
  SplitHeight: Integer;
  SplitXPos: Integer;
  GlyphDC: HDC;
  SplitDC: HDC;
begin
  FButton := Value;
  if FButton <> nil then
    begin
      FButtonWidth := FButton.Width;
      FButtonHeight := FButton.Height;
      if FButton is TcaSpeedButton then
        begin
          FButtonLayout := TcaSpeedButton(FButton).Layout;
          FCaption := TcaSpeedButton(FButton).Caption;
          FSpacing := TcaSpeedButton(FButton).Spacing;
          // Deal with enabled and disabled glyph images 
          OriginalGlyph := TcaSpeedButton(FButton).Glyph;
          if OriginalGlyph <> nil then
            begin
              NumGlyphs := TcaSpeedButton(FButton).NumGlyphs;
              SplitWidth := OriginalGlyph.Width div NumGlyphs;
              SplitHeight := OriginalGlyph.Height;
              FGlyph.Width := SplitWidth;
              FGlyph.Height := SplitHeight;
              if FButton.Enabled then
                SplitXPos := 0
              else
                SplitXPos := SplitWidth * (NumGlyphs - 1);
              GlyphDC := OriginalGlyph.Canvas.Handle;
              SplitDC := FGlyph.Canvas.Handle;
              BitBlt(SplitDC, 0, 0, SplitWidth, SplitHeight, GlyphDC, SplitXPos, 0, SRCCOPY);
              FGlyphWidth := FGlyph.Width;
              FGlyphHeight := FGlyph.Height;
            end;
        end;
      FCanvas := TcaSpeedButton(FButton).OffScreenCanvas;
    end;
  UpdateBounds;
end;

procedure TcaButtonFace.SetDown(const Value: Boolean);
begin
  if Value <> FDown then
    begin
      FDown := Value;
      UpdateBounds;
    end;
end;

  //---------------------------------------------------------------------------
  // TcaButtonEdges                                                            
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaButtonEdges.Create;
begin
  inherited;
  FRect := TcaRect.Create;
end;

destructor TcaButtonEdges.Destroy;
begin
  FRect.Free;
  inherited;
end;

procedure TcaButtonEdges.DrawEdges;
begin
  if FCanvas <> nil then
    begin
      if FUseMouseOver and FMouseOver then
        begin
          case FMouseOverStyle of
            bsDefault:  DrawNormalEdge;
            bsThin:     DrawThinEdge;
            bsFlat:     DrawFlatEdge;
            bsNoEdge:   DrawNoEdge;
          end;
        end
      else
        begin
          case FStyle of
            bsDefault:  DrawNormalEdge;
            bsThin:     DrawThinEdge;
            bsFlat:     DrawFlatEdge;
            bsNoEdge:   DrawNoEdge;
          end;
        end;
    end;
end;

procedure TcaButtonEdges.DrawFlatEdge;
var
  cl1, cl2: TColor;
  L0, L1, T0, T1, R0, R1, R2, B0, B1, B2: Integer;
begin
  UpdateCoords(L0, L1, T0, T1, R0, R1, R2, B0, B1, B2);
  if FEnabled then
    begin
      if FDown then
        begin
          cl1 := FColorBtnShadow;
          cl2 := FColorBtnHighlight;
        end
      else
        begin
          if (FUseMouseOver and FMouseOver) or (not FIsRunTime) then
            begin
              cl1 := FColorBtnHighlight;
              cl2 := FColorBtnShadow;
            end
          else
            begin
              cl1 := FColorBtnColor;
              cl2 := FColorBtnColor;
            end;
        end;
    end
  else
    begin
      cl1 := FColorBtnColor;
      cl2 := FColorBtnColor;
    end;
  // Bottom/Left -> Top/Left -> Top/Right 
  DrawPoly(cl1, [XY(L0, B1), XY(L0, T0), XY(R0, T0)]);
  // Bottom/Left -> Bottom/Right -> Top/Right 
  DrawPoly(cl2, [XY(L0, B0), XY(R1, B0), XY(R1, T0)]);
end;

procedure TcaButtonEdges.DrawNoEdge;
begin
end;

procedure TcaButtonEdges.DrawNormalEdge;
var
  cl1, cl2, cl3, cl4: TColor;
  L0, L1, T0, T1, R0, R1, R2, B0, B1, B2: Integer;
begin
  UpdateCoords(L0, L1, T0, T1, R0, R1, R2, B0, B1, B2);
  if FDown then
    begin
      cl1 := FColorWindowFrame;
      cl2 := FColorBtnShadow;
      cl3 := FColor3DLight;
      cl4 := FColorBtnHighlight;
    end
  else
    begin
      cl1 := FColorBtnHighlight;
      cl2 := FColor3DLight;
      cl3 := FColorBtnShadow;
      cl4 := FColorWindowFrame;
    end;
  // Outer - Bottom/Left -> Top/Left -> Top/Right 
  DrawPoly(cl1, [XY(L0, B1), XY(L0, T0), XY(R0, T0)]);
  // Inner - Bottom/Left -> Top/Left -> Top/Right 
  DrawPoly(cl2, [XY(L1, B2), XY(L1, T1), XY(R1, T1)]);
  // Inner - Bottom/Left -> Bottom/Right -> Top/Right 
  DrawPoly(cl3, [XY(L1, B1), XY(R2, B1), XY(R2, T1)]);
  // Outer - Bottom/Left -> Bottom/Right -> Top/Right 
  DrawPoly(cl4, [XY(L0, B0), XY(R1, B0), XY(R1, T0)]);
end;

procedure TcaButtonEdges.DrawPoly(AColor: TColor; Points: array of TPoint);
begin
  FCanvas.Pen.Color := AColor;
  FCanvas.PolyLine(Points);
end;

procedure TcaButtonEdges.DrawThinEdge;
var
  cl1, cl2: TColor;
  L0, L1, T0, T1, R0, R1, R2, B0, B1, B2: Integer;
begin
  UpdateCoords(L0, L1, T0, T1, R0, R1, R2, B0, B1, B2);
  if FDown then
    begin
      cl1 := FColorBtnShadow;
      cl2 := FColorBtnHighlight;
    end
  else
    begin
      cl1 := FColorBtnHighlight;
      cl2 := FColorBtnShadow;
    end;
  // Bottom/Left -> Top/Left -> Top/Right 
  DrawPoly(cl1, [XY(L0, B1), XY(L0, T0), XY(R0, T0)]);
  // Bottom/Left -> Bottom/Right -> Top/Right 
  DrawPoly(cl2, [XY(L0, B0), XY(R1, B0), XY(R1, T0)]);
end;

procedure TcaButtonEdges.SetBounds(const ALeft, ATop, AWidth, AHeight: Integer);
begin
  FRect.SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TcaButtonEdges.SetRect(const ALeft, ATop, ARight, ABottom: Integer);
begin
  FRect.SetRect(ALeft, ATop, ARight, ABottom);
end;

procedure TcaButtonEdges.UpdateCoords(var L0, L1, T0, T1, R0, R1, R2, B0, B1, B2: Integer);
begin
  L0 := FRect.Left;
  L1 := L0 + 1;
  T0 := FRect.Top;
  T1 := T0 + 1;
  R0 := L0 + FRect.Width;
  R1 := R0 - 1;
  R2 := R0 - 2;
  B0 := T0 + FRect.Height - 1;
  B1 := B0 - 1;
  B2 := B0 - 2;
end;

function TcaButtonEdges.XY(AX, AY: Integer): TPoint;
begin
  Result := Point(AX, AY);
end;

  //---------------------------------------------------------------------------
  // TcaCustomSpeedButton                                                      
  //---------------------------------------------------------------------------

constructor TcaCustomSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
  FProperties := TcaCustomButtonProperties.Create(nil);
  SetBounds(0, 0, 25, 25);
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];
  ParentFont := True;
  Color := clBtnFace;
  FColor3DLight := cl3DLight;
  FColorBtnHighlight := clBtnHighlight;
  FColorBtnShadow := clBtnShadow;
  FColorWindowFrame := clWindowFrame;
  FGlyph := TBitmap.Create;
  FDownFont := TFont.Create;
  FDownFont.Assign(Font);
  FMouseOverFont := TFont.Create;
  FMouseOverFont.Assign(Font);
  FNumGlyphs := 1;
  FMouseOverStyle := bsThin;
  FSyncDownFont := True;
  FSyncMouseOverFont := True;
  FGlyphOffsetWhenDown := True;
  FTextOffsetWhenDown := True;
  FTransparentGlyph := True;
  FUseMouseOver := True;
end;

destructor TcaCustomSpeedButton.Destroy;
begin
  FDownFont.Free;
  FMouseOverFont.Free;
  FGlyph.Free;
  FProperties.Free;
  inherited;
end;

procedure TcaCustomSpeedButton.CMTextChanged(var Message: TMessage);
begin
  inherited;
  RequestPaint;
  DoTextChanged;
end;

procedure TcaCustomSpeedButton.SetStyle(const Value: TcaButtonStyle);
begin
  if Value <> FStyle then
    begin
      FStyle := Value;
      FMouseOverStyle := FStyle;
      RequestPaint;
    end;
end;

procedure TcaCustomSpeedButton.SetTextStyle(const Value: TcaTextStyle);
begin
  if Value <> FTextStyle then
    begin
      FTextStyle := Value;
      RequestPaint;
    end;
end;

procedure TcaCustomSpeedButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseOver := True;
  DoMouseEnter;
end;

procedure TcaCustomSpeedButton.DoMouseEnter;
begin
  if FStyle <> FMouseOverStyle then RequestPaint;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TcaCustomSpeedButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseOver := False;
  DoMouseLeave;
end;

procedure TcaCustomSpeedButton.DoDrawButton(ACanvas: TCanvas; ARect: TRect; IsDown: Boolean);
begin
  if Assigned(FOnDrawButton) then FOnDrawButton(Self, ACanvas, ARect, IsDown);
end;

procedure TcaCustomSpeedButton.DoMouseLeave;
begin
  if FMouseIsDown then MouseUp(FButton, FShift, FXPos, FYPos);
  if FStyle <> FMouseOverStyle then RequestPaint;
  NotifyGroupButtons;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TcaCustomSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FButton := Button;
  FMouseIsDown := True;
  FShiftState := Shift;
  if not FDown then FState := bsDown;
  RequestPaint;
end;

procedure TcaCustomSpeedButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  FShiftState := Shift;
  FXPos := X;
  FYPos := Y;
end;

procedure TcaCustomSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FMouseIsDown then
    begin
      FMouseIsDown := False;
      FShiftState := Shift;
      DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
      if FGroupIndex = 0 then
        begin
          FState := bsUp;
          if DoClick and not (FState in [bsExclusive, bsDown]) then
            RequestPaint;
        end
      else
        if DoClick then
          begin
            SetDown(not FDown);
            if FDown then RequestPaint;
          end
        else
          begin
            if FDown then FState := bsExclusive;
            RequestPaint;
          end;
      if DoClick and (CanClick) then Click;
    end;
end;

procedure TcaCustomSpeedButton.Click;
begin
  inherited;
end;

procedure TcaCustomSpeedButton.BufferedPaint(C: TCanvas; R: TRect);
var
  Offset: Integer;
  TempColor: TColor;
  CaptionStr: String;
  TextBounds: TRect;
  ButtonFace: TcaButtonFace;
  ButtonEdges: TcaButtonEdges;
  IsDown: Boolean;
  CompState: IcaComponentState;

  procedure DrawCaption;
  begin
    case
      FTextStyle of
        tsNormal:   Offset := 0;
        tsRaised:   Offset := -1;
        tsLowered:  Offset := 1;
      end;
    if not Enabled then Offset := 1;
    TextBounds := ButtonFace.CaptionRect;
    OffsetRect(TextBounds, FXOffset, FYOffset);
    C.Brush.Style := bsClear;
    CaptionStr := Caption;
    if Visible then
      if (FTextStyle <> tsNormal) or (not Enabled) then
        begin
          TempColor := C.Font.Color;
          C.Font.Color := clBtnHighlight;
          OffsetRect(TextBounds, Offset, Offset);
          DrawText(C.Handle, PChar(CaptionStr), Length(CaptionStr), TextBounds, 0);
          OffsetRect(TextBounds, -Offset, -Offset);
          C.Font.Color := TempColor;
        end;
    if (not Enabled) and Visible then C.Font.Color := clBtnShadow;
    if not Visible then C.Font.Color := clWhite;
    DrawText(C.Handle, PChar(CaptionStr), Length(CaptionStr), TextBounds, 0);
  end;

  procedure DrawGlyph;
  var
    DrawBitmap: TBitmap;
    TransparentColor: TColor;
    W: Integer;
    H: Integer;
    R: TRect;
  begin
    DrawBitmap := TBitmap.Create;
    try
      W := ButtonFace.Glyph.Width;
      H := ButtonFace.Glyph.Height;
      DrawBitmap.Width := W;
      DrawBitmap.Height := H;
      DrawBitmap.Canvas.Brush.Color := GetButtonColor;
      TransparentColor := ButtonFace.Glyph.Canvas.Pixels[0, H - 1];
      R := Utils.GetBitmapRect(DrawBitmap);
      if FTransparentGlyph then
        DrawBitmap.Canvas.BrushCopy(R, ButtonFace.Glyph, R, TransparentColor)
      else
        DrawBitmap.Canvas.CopyRect(R, ButtonFace.Glyph.Canvas, R);
      C.Draw(ButtonFace.GlyphRect.Left + FXOffset, ButtonFace.GlyphRect.Top + FYOffset, DrawBitmap);
    finally
      DrawBitmap.Free;
    end;
  end;

begin
  inherited;
  CompState := TcaComponentState.Create; // CompState: IcaComponentState
  CompState.Component := Self;
  if not CompState.IsDestroying then
    begin
      // if AutoSize then AdjustSize; 
      Erase;
      IsDown := (FMouseIsDown or Down) and (not FDisableDown);
      if IsDown then
        C.Font.Assign(FDownFont)
      else
        begin
          if (FUseMouseOver and FMouseOver) then
            C.Font.Assign(FMouseOverFont)
          else
            C.Font.Assign(Font);
        end;
      // ButtonEdges 
      ButtonEdges := Auto(TcaButtonEdges.Create).Instance;
      ButtonEdges.Canvas := C;
      ButtonEdges.Color3DLight := FColor3DLight;
      ButtonEdges.ColorBtnColor := Color;
      ButtonEdges.ColorBtnHighlight := FColorBtnHighlight;
      ButtonEdges.ColorBtnShadow := FColorBtnShadow;
      ButtonEdges.ColorWindowFrame := FColorWindowFrame;
      ButtonEdges.SetBounds(0, 0, Width, Height);
      ButtonEdges.Down := IsDown;
      ButtonEdges.Enabled := Enabled;
      ButtonEdges.UseMouseOver := FUseMouseOver;
      ButtonEdges.MouseOver := FMouseOver;
      ButtonEdges.MouseOverStyle := FMouseOverStyle;
      ButtonEdges.Style := FStyle;
      ButtonEdges.IsRunTime := CompState.IsRunTime;
      ButtonEdges.DrawEdges;
      // ButtonFace 
      ButtonFace := Auto(TcaButtonFace.Create).Instance;
      ButtonFace.Button := Self;
      ButtonFace.Down := IsDown;
      DrawCaption;
      if FGlyph <> nil then DrawGlyph;
      DoDrawButton(C, Rect(0, 0, Width, Height), IsDown);
    end;
end;

procedure TcaCustomSpeedButton.Erase;
var
  C: TCanvas;
begin
  C := OffScreenCanvas;
  C.Brush.Color := GetButtonColor;
  C.Brush.Style := bsSolid;
  FillRect(C.Handle, Rect(0, 0, Width, Height), C.Brush.Handle);
end;

procedure TcaCustomSpeedButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

procedure TcaCustomSpeedButton.SetAllowAllUp(const Value: Boolean);
begin
  if Value <> FAllowAllUp then
    begin
      FAllowAllUp := Value;
      if not (csLoading in ComponentState) then
        NotifyGroupButtons;
    end;
end;

procedure TcaCustomSpeedButton.SetDown(const Value: Boolean);
begin
  if FGroupIndex = 0 then
    FDown := False
  else
    if Value <> FDown then
      begin
        if not (FDown and (not FAllowAllUp)) then
          begin
            FDown := Value;
            if FDown then
              begin
                if FState = bsUp then RequestPaint;
                FState := bsExclusive
              end
            else
              begin
                FState := bsUp;
                RequestPaint;
              end;
            if FDown then NotifyGroupButtons;
          end;
      end;
end;

procedure TcaCustomSpeedButton.SetGroupIndex(const Value: Integer);
begin
  if Value <> FGroupIndex then
    begin
      FGroupIndex := Value;
      NotifyGroupButtons;
    end;
end;

procedure TcaCustomSpeedButton.NotifyGroupButtons;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
    begin
      Msg.Msg := CAM_BUTTONPRESSED;
      Msg.WParam := FGroupIndex;
      Msg.LParam := LongInt(Self);
      Msg.Result := 0;
      Parent.Broadcast(Msg);
    end;
end;

procedure TcaCustomSpeedButton.UpdateProperties;
begin
  FProperties.Color := Color;
  FProperties.Color3DLight := FColor3DLight;
  FProperties.ColorBtnHighlight := FColorBtnHighlight;
  FProperties.ColorBtnShadow := FColorBtnShadow;
  FProperties.ColorWindowFrame := FColorWindowFrame;
  FProperties.Font.Assign(Font);
  FProperties.Style := FStyle;
end;

procedure TcaCustomSpeedButton.UpdateFromProperties;
begin
  Color := FProperties.Color;
  FColor3DLight := FProperties.Color3DLight;
  FColorBtnHighlight := FProperties.ColorBtnHighlight;
  FColorBtnShadow := FProperties.ColorBtnShadow;
  FColorWindowFrame := FProperties.ColorWindowFrame;
  Font.Assign(FProperties.Font);
  FStyle := FProperties.Style;
  RequestPaint;
end;

procedure TcaCustomSpeedButton.CAMButtonPressed(var Message: TMessage);
var
  Sender: TcaCustomSpeedButton;
begin
  if Message.WParam = FGroupIndex then
    begin
      Sender := TcaCustomSpeedButton(Message.LParam);
      if Sender <> Self then
        begin
          if Sender.Down and FDown then
            begin
              FDown := False;
              FState := bsUp;
              RequestPaint;
            end;
          FAllowAllUp := Sender.AllowAllUp;
        end;
    end;
end;

procedure TcaCustomSpeedButton.CAMGlyphListChanged(var Message: TMessage);
begin
  RequestPaint;
end;

procedure TcaCustomSpeedButton.SetLayout(const Value: TcaButtonLayout);
begin
  if Value <> FLayout then
    begin
      FLayout := Value;
      RequestPaint;
    end;
end;

procedure TcaCustomSpeedButton.SetProperties(const Value: TcaCustomButtonProperties);
begin
  FProperties.Assign(Value);
  UpdateFromProperties;
end;

procedure TcaCustomSpeedButton.SetSpacing(const Value: Integer);
begin
  if Value <> FSpacing then
    begin
      FSpacing := Value;
      RequestPaint;
    end;
end;

procedure TcaCustomSpeedButton.CMEnabledChanged(var Message: TMessage);
begin
  RequestPaint;
end;

procedure TcaCustomSpeedButton.CMFontChanged(var Message: TMessage);
begin
  if FSyncDownFont then FDownFont.Assign(Font);
  if FSyncMouseOverFont then FMouseOverFont.Assign(Font);
end;

procedure TcaCustomSpeedButton.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  RequestPaint;
end;

function TcaCustomSpeedButton.CanClick: Boolean;
begin
  Result := True;
end;

procedure TcaCustomSpeedButton.DoTextChanged;
begin
  if Assigned(FOnTextChanged) then FOnTextChanged(Self);
end;

function TcaCustomSpeedButton.GetButtonColor: TColor;
begin
  if (FMouseIsDown or Down) and (not FDisableDown) and (FGroupIndex <> 0) then
    Result := clWhite
  else
    Result := Color;
end;

procedure TcaCustomSpeedButton.Loaded;
begin
  inherited;
  UpdateProperties;
  RequestPaint;
end;

procedure TcaCustomSpeedButton.SetColor3DLight(const Value: TColor);
begin
  if Value <> FColor3DLight then
    begin
      FColor3DLight := Value;
      RequestPaint;
    end;
end;

procedure TcaCustomSpeedButton.SetColorBtnHighlight(const Value: TColor);
begin
  if Value <> FColorBtnHighlight then
    begin
      FColorBtnHighlight := Value;
      RequestPaint;
    end;
end;

procedure TcaCustomSpeedButton.SetColorBtnShadow(const Value: TColor);
begin
  if Value <> FColorBtnShadow then
    begin
      FColorBtnShadow := Value;
      RequestPaint;
    end;
end;

procedure TcaCustomSpeedButton.SetColorWindowFrame(const Value: TColor);
begin
  if Value <> FColorWindowFrame then
    begin
      FColorWindowFrame := Value;
      RequestPaint;
    end;
end;

procedure TcaCustomSpeedButton.SetDisableDown(const Value: Boolean);
begin
  if Value <> FDisableDown then
    begin
      FDisableDown := Value;
      RequestPaint;
    end;
end;

procedure TcaCustomSpeedButton.SetXOffset(const Value: Integer);
begin
  if Value <> FXOffset then
    begin
      FXOffset := Value;
      RequestPaint;
    end;
end;

procedure TcaCustomSpeedButton.SetYOffset(const Value: Integer);
begin
  if Value <> FYOffset then
    begin
      FYOffset := Value;
      RequestPaint;
    end;
end;

procedure TcaCustomSpeedButton.SetDownFont(const Value: TFont);
var
  CompState: IcaComponentState;
begin
  FDownFont.Assign(Value);
  CompState := TcaComponentState.Create; // CompState: IcaComponentState
  CompState.Component := Self;
  if not CompState.IsLoading then Invalidate;
end;

procedure TcaCustomSpeedButton.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
  RequestPaint;
end;

procedure TcaCustomSpeedButton.SetNumGlyphs(const Value: TcaNumGlyphs);
begin
  FNumGlyphs := Value;
  RequestPaint;
end;

procedure TcaCustomSpeedButton.SetMouseOverFont(const Value: TFont);
begin
  FMouseOverFont.Assign(Value);
end;

procedure TcaCustomSpeedButton.SetTransparentGlyph(const Value: Boolean);
begin
  if Value <> FTransparentGlyph then
    begin
      FTransparentGlyph := Value;
      RequestPaint;
    end;
end;

  //----------------------------------------------------------------------------
  // TcaDesignTimeButton                                                        
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaDesignTimeButton.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csDesignInteractive];
end;

  //----------------------------------------------------------------------------
  // TcaRepeatSpeedButton                                                       
  //----------------------------------------------------------------------------

constructor TcaRepeatSpeedButton.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FDelay := 500;
  FInterval := 200;
end;

function TcaRepeatSpeedButton.CanClick: Boolean;
begin
  Result := not FTimerClicked;
end;

destructor TcaRepeatSpeedButton.Destroy;
begin
  FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TcaRepeatSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);
  if FAutoRepeat then
    begin
      if FRepeatTimer = nil then FRepeatTimer := TcaTimer.Create(Self);
      FTimerClicked := False;
      FRepeatTimer.OnTimer := TimerExpired;
      FRepeatTimer.Interval := FDelay;
      FRepeatTimer.Enabled  := True;
    end
  else
    begin
      FRepeatTimer.Free;
      FRepeatTimer := nil;
    end;
end;

procedure TcaRepeatSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FRepeatTimer <> nil then FRepeatTimer.Enabled  := False;
  inherited MouseUp (Button, Shift, X, Y);
end;

procedure TcaRepeatSpeedButton.TimerExpired(Sender: TObject);
var
  IntervalFactor: Integer;
begin
  if Enabled then
    begin
      IntervalFactor := 1;
      if ssAlt in FShiftState then IntervalFactor := 3;
      FRepeatTimer.Interval := FInterval * IntervalFactor;
      if (State = bsDown) and MouseCapture then
        try
          FTimerClicked := True;
          Click;
        except
          FRepeatTimer.Enabled := False;
          raise;
        end;
    end
  else
    FRepeatTimer.Enabled := False;
end;

end.


