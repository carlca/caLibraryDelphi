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


unit caSizeBar;

{$INCLUDE ca.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  Math,
  caClasses,
  caTypes,
  caControls,
  caUtils,
  caButtons;

type

 //----------------------------------------------------------------------------
 // IcaSizeBarButton
 //----------------------------------------------------------------------------

  IcaSizeBarButton = interface
  ['{63C4E1E4-E87D-48E6-A9E6-8FFC33BB0A52}']
    // Property methods
    function GetArrow: TcaArrowType;
    function GetArrowColor: TColor;
    function GetDotDarkColor: TColor;
    function GetDotLightColor: TColor;
    procedure SetArrow(const Value: TcaArrowType);
    procedure SetArrowColor(const Value: TColor);
    procedure SetDotDarkColor(const Value: TColor);
    procedure SetDotLightColor(const Value: TColor);
    // Properties
    property Arrow: TcaArrowType read GetArrow write SetArrow;
    property ArrowColor: TColor read GetArrowColor write SetArrowColor;
    property DotDarkColor: TColor read GetDotDarkColor write SetDotDarkColor;
    property DotLightColor: TColor read GetDotLightColor write SetDotLightColor;
  end;

 //----------------------------------------------------------------------------
 // TcaSizeBarButton
 //----------------------------------------------------------------------------

  TcaSizeBarButton = class(TcaSpeedButton, IcaSizeBarButton)
  private
    FArrow: TcaArrowType;
    FArrowColor: TColor;
    FDotDarkColor: TColor;
    FDotLightColor: TColor;
    function GetArrow: TcaArrowType;
    function GetArrowColor: TColor;
    function GetDotDarkColor: TColor;
    function GetDotLightColor: TColor;
    procedure SetArrow(const Value: TcaArrowType);
    procedure SetArrowColor(const Value: TColor);
    procedure SetDotDarkColor(const Value: TColor);
    procedure SetDotLightColor(const Value: TColor);
  protected
    procedure Paint; override;
  public
  	constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Arrow: TcaArrowType read GetArrow write SetArrow;
    property ArrowColor: TColor read GetArrowColor write SetArrowColor;
    property DotDarkColor: TColor read GetDotDarkColor write SetDotDarkColor;
    property DotLightColor: TColor read GetDotLightColor write SetDotLightColor;
  end;

 //----------------------------------------------------------------------------
 // IcaSizeBar
 //----------------------------------------------------------------------------

  IcaSizeBar = interface
  ['{D66A2DBF-0E32-4A16-A25D-F283849BD86E}']
    // Property methods
    function GetBigAndSmall: TcaSizeBarArrows;
    function GetButtonGapBottom: Integer;
    function GetButtonGapLeft: Integer;
    function GetButtonGapRight: Integer;
    function GetButtonGapTop: Integer;
    function GetButtonPosition: TcaSizeBarBtnPosition;
    function GetButtonStyle: TcaButtonStyle;
    function GetOnStateChange: TNotifyEvent;
    function GetState: TcaSizeBarState;
    procedure SetBigAndSmall(const Value: TcaSizeBarArrows);
    procedure SetButtonGapBottom(const Value: Integer);
    procedure SetButtonGapLeft(const Value: Integer);
    procedure SetButtonGapRight(const Value: Integer);
    procedure SetButtonGapTop(const Value: Integer);
    procedure SetButtonPosition(const Value: TcaSizeBarBtnPosition);
    procedure SetButtonStyle(const Value: TcaButtonStyle);
    procedure SetOnStateChange(const Value: TNotifyEvent);
    procedure SetState(const Value: TcaSizeBarState);
    // Properties
    property BigAndSmall: TcaSizeBarArrows read GetBigAndSmall write SetBigAndSmall;
    property ButtonGapBottom: Integer read GetButtonGapBottom write SetButtonGapBottom;
    property ButtonGapLeft: Integer read GetButtonGapLeft write SetButtonGapLeft;
    property ButtonGapRight: Integer read GetButtonGapRight write SetButtonGapRight;
    property ButtonGapTop: Integer read GetButtonGapTop write SetButtonGapTop;
    property ButtonPosition: TcaSizeBarBtnPosition read GetButtonPosition write SetButtonPosition;
    property ButtonStyle: TcaButtonStyle read GetButtonStyle write SetButtonStyle;
    property OnStateChange: TNotifyEvent read GetOnStateChange write SetOnStateChange;
    property State: TcaSizeBarState read GetState write SetState;
  end;

 //----------------------------------------------------------------------------
 // TcaCustomSizeBar
 //----------------------------------------------------------------------------

  TcaCustomSizeBar = class(TcaCustomPanel, IcaSizeBar)
  private
    FBigAndSmall: TcaSizeBarArrows;
    FButtonGapBottom: Integer;
    FButtonGapLeft: Integer;
    FButtonGapRight: Integer;
    FButtonGapTop: Integer;
    FButtonPosition: TcaSizeBarBtnPosition;
    FButton: TcaSizeBarButton;
    FKind: TcaSizeBarKind;
    FOnStateChange: TNotifyEvent;
    FState: TcaSizeBarState;
    // Property methods
    function GetBigAndSmall: TcaSizeBarArrows;
    function GetButtonGapBottom: Integer;
    function GetButtonGapLeft: Integer;
    function GetButtonGapRight: Integer;
    function GetButtonGapTop: Integer;
    function GetButtonPosition: TcaSizeBarBtnPosition;
    function GetButtonStyle: TcaButtonStyle;
    function GetOnStateChange: TNotifyEvent;
    function GetState: TcaSizeBarState;
    procedure SetBigAndSmall(const Value: TcaSizeBarArrows);
    procedure SetButtonGapBottom(const Value: Integer);
    procedure SetButtonGapLeft(const Value: Integer);
    procedure SetButtonGapRight(const Value: Integer);
    procedure SetButtonGapTop(const Value: Integer);
    procedure SetButtonPosition(const Value: TcaSizeBarBtnPosition);
    procedure SetButtonStyle(const Value: TcaButtonStyle);
    procedure SetOnStateChange(const Value: TNotifyEvent);
    procedure SetState(const Value: TcaSizeBarState);
    // Private methods
		procedure ButtonClickEvent(Sender: TObject);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
		procedure UpdateButtonArrows;
		procedure UpdateButtonPosition;
  protected
    procedure CreateWnd; override;
    procedure Resize; override;
    property BigAndSmall: TcaSizeBarArrows read GetBigAndSmall write SetBigAndSmall;
    property ButtonGapBottom: Integer read GetButtonGapBottom write SetButtonGapBottom;
    property ButtonGapLeft: Integer read GetButtonGapLeft write SetButtonGapLeft;
    property ButtonGapRight: Integer read GetButtonGapRight write SetButtonGapRight;
    property ButtonGapTop: Integer read GetButtonGapTop write SetButtonGapTop;
    property ButtonPosition: TcaSizeBarBtnPosition read GetButtonPosition write SetButtonPosition;
    property ButtonStyle: TcaButtonStyle read GetButtonStyle write SetButtonStyle;
    property OnStateChange: TNotifyEvent read GetOnStateChange write SetOnStateChange;
    property State: TcaSizeBarState read GetState write SetState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

 //----------------------------------------------------------------------------
 // TcaSizeBarProperties
 //----------------------------------------------------------------------------

  TcaSizeBarProperties = class(TcaInterfacedPersistent, IcaSizeBar)
  private
    FSizeBar: TcaCustomSizeBar;
    function GetBigAndSmall: TcaSizeBarArrows;
    function GetButtonGapBottom: Integer;
    function GetButtonGapLeft: Integer;
    function GetButtonGapRight: Integer;
    function GetButtonGapTop: Integer;
    function GetButtonPosition: TcaSizeBarBtnPosition;
    function GetButtonStyle: TcaButtonStyle;
    function GetOnStateChange: TNotifyEvent;
    function GetState: TcaSizeBarState;
    procedure SetBigAndSmall(const Value: TcaSizeBarArrows);
    procedure SetButtonGapBottom(const Value: Integer);
    procedure SetButtonGapLeft(const Value: Integer);
    procedure SetButtonGapRight(const Value: Integer);
    procedure SetButtonGapTop(const Value: Integer);
    procedure SetButtonPosition(const Value: TcaSizeBarBtnPosition);
    procedure SetButtonStyle(const Value: TcaButtonStyle);
    procedure SetOnStateChange(const Value: TNotifyEvent);
    procedure SetState(const Value: TcaSizeBarState);
    function GetWidth: Integer;
    procedure SetWidth(const Value: Integer);
  public
    procedure Assign(Source: TPersistent); override;
    property BigAndSmall: TcaSizeBarArrows read GetBigAndSmall write SetBigAndSmall;
    property OnStateChange: TNotifyEvent read GetOnStateChange write SetOnStateChange;
    property SizeBar: TcaCustomSizeBar read FSizeBar write FSizeBar;
    property State: TcaSizeBarState read GetState write SetState;
  published
    property ButtonGapBottom: Integer read GetButtonGapBottom write SetButtonGapBottom;
    property ButtonGapLeft: Integer read GetButtonGapLeft write SetButtonGapLeft;
    property ButtonGapRight: Integer read GetButtonGapRight write SetButtonGapRight;
    property ButtonGapTop: Integer read GetButtonGapTop write SetButtonGapTop;
    property ButtonPosition: TcaSizeBarBtnPosition read GetButtonPosition write SetButtonPosition;
    property ButtonStyle: TcaButtonStyle read GetButtonStyle write SetButtonStyle;
    property Width: Integer read GetWidth write SetWidth;
  end;

 //----------------------------------------------------------------------------
 // TcaSizeBar
 //----------------------------------------------------------------------------

  TcaSizeBar = class(TcaCustomSizeBar)
  public
    // TCustomPanel
    property DockManager;
  published
    // IcaSizeBar
    property BigAndSmall;
    property ButtonGapBottom;
    property ButtonGapLeft;
    property ButtonGapRight;
    property ButtonGapTop;
    property ButtonPosition;
    property ButtonStyle;
    property OnStateChange;
    property State;
    // TCustomPanel
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderWidth;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Frame;
    property FullRepaint;
    property Font;
    property Locked;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

 //----------------------------------------------------------------------------
 // TcaSizeBarButton
 //----------------------------------------------------------------------------

constructor TcaSizeBarButton.Create(AOwner: TComponent);
begin
  inherited;
  FArrowColor := clBlack;
  FDotLightColor := clWhite;
  FDotDarkColor := clBlack;
end;

destructor TcaSizeBarButton.Destroy;
begin
  inherited;
end;

function TcaSizeBarButton.GetArrow: TcaArrowType;
begin
  Result := FArrow;
end;

function TcaSizeBarButton.GetArrowColor: TColor;
begin
  Result := FArrowColor;
end;

function TcaSizeBarButton.GetDotDarkColor: TColor;
begin
  Result := FDotDarkColor;
end;

function TcaSizeBarButton.GetDotLightColor: TColor;
begin
  Result := FDotLightColor;
end;

procedure TcaSizeBarButton.SetArrow(const Value: TcaArrowType);
begin
  if Value <> FArrow then
    begin
      FArrow := Value;
      Invalidate;
    end;
end;

procedure TcaSizeBarButton.SetArrowColor(const Value: TColor);
begin
  if Value <> FArrowColor then
    begin
      FArrowColor := Value;
      Invalidate;
    end;
end;

procedure TcaSizeBarButton.SetDotDarkColor(const Value: TColor);
begin
  if Value <> FDotDarkColor then
    begin
      FDotDarkColor := Value;
      Invalidate;
    end;
end;

procedure TcaSizeBarButton.SetDotLightColor(const Value: TColor);
begin
  if Value <> FDotLightColor then
    begin
      FDotLightColor := Value;
      Invalidate;
    end;
end;

procedure TcaSizeBarButton.Paint;
var
  C: TCanvas;
  BtnLength: Integer;

  procedure DrawDots(const AFrom, ATo, AOffset, AStep: Integer; AColor: TColor);
  var
    X, Y, O: Integer;
  begin
    O := Ord(MouseIsDown);
    if FArrow in [atLeft, atRight] then
      begin
        Y := AFrom;
        X := (Width div 2) + AOffset;
        while Y <= ATo do
          begin
            C.Pixels[X + O, Y + O] := AColor;
            Inc(Y, AStep);
          end;
      end
    else
      begin
        X := AFrom;
        Y := (Height div 2) + AOffset + 1;
        while X <= ATo do
          begin
            C.Pixels[X + O, Y + O] := AColor;
            Inc(X, AStep);
          end;
      end;
  end;

  procedure DrawLightDots(const AFrom, ATo, AOffset, AStep: Integer);
  begin
    DrawDots(AFrom, ATo, AOffset, AStep, FDotLightColor);
  end;

  procedure DrawDarkDots(const AFrom, ATo, AOffset, AStep: Integer);
  begin
    DrawDots(AFrom, ATo, AOffset, AStep, FDotDarkColor);
  end;

  type
    TcaArrowShape = array[0..14] of Byte;

  procedure DrawOneArrow(ALeft, ATop: Integer; AShape: TcaArrowShape);
  var
    P, X, Y, O: Integer;
  begin
    O := Ord(MouseIsDown);
    for P := 0 to 14 do
      begin
        if FArrow in [atLeft, atRight] then
          begin
            X := P mod 3;
            Y := P div 3;
          end
        else
          begin
            X := P mod 5;
            Y := (P div 5) + 2;
          end;
        if AShape[P] = 1 then C.Pixels[ALeft + X + O, ATop + Y + O] := FArrowColor;
      end;
  end;

  procedure DrawArrows;
  const
    LeftArrow: TcaArrowShape  = (0, 0, 1,
    														 0, 1, 1,
                                 1, 1, 1,
                                 0, 1, 1,
                                 0, 0, 1);

    RightArrow: TcaArrowShape = (1, 0, 0,
    														 1, 1, 0,
                                 1, 1, 1,
                                 1, 1, 0,
                                 1, 0, 0);

    UpArrow: TcaArrowShape =    (0, 0, 1, 0, 0,
    														 0, 1, 1, 1, 0,
                                 1, 1, 1, 1, 1);

    DownArrow: TcaArrowShape =  (1, 1, 1, 1, 1,
    														 0, 1, 1, 1, 0,
                                 0, 0, 1, 0, 0);

  begin
    case
      FArrow of
        atLeft:   begin
                    DrawOneArrow((Width div 2) - 2, 6, LeftArrow);
                    DrawOneArrow((Width div 2) - 2, Height - 10, LeftArrow);
                  end;
        atRight:  begin
                    DrawOneArrow((Width div 2) - 1, 6, RightArrow);
                    DrawOneArrow((Width div 2) - 1, Height - 10, RightArrow);
                  end;
        atUp:     begin
                    DrawOneArrow(6, (Height div 2) - 3, UpArrow);
                    DrawOneArrow(Width - 11, (Height div 2) - 3, UpArrow);
                  end;
        atDown:   begin
                    DrawOneArrow(6, (Height div 2) - 3, DownArrow);
                    DrawOneArrow(Width - 11, (Height div 2) - 3, DownArrow);
                  end;
      end;
  end;

begin
	inherited;
  C := OffScreenCanvas;
  if FArrow in [atLeft, atRight] then
    begin
      BtnLength := Height;
		  DrawLightDots(15, BtnLength - 15, -1, 3);
		  DrawDarkDots(16, BtnLength - 14, 0, 3);
    end
  else
    begin
      BtnLength := Width;
		  DrawLightDots(15, BtnLength - 17, -2, 3);
		  DrawDarkDots(16, BtnLength - 16, -1, 3);
    end;
  DrawArrows;
  UpdateOnScreenBitmap;
end;

 //----------------------------------------------------------------------------
 // TcaCustomSizeBar
 //----------------------------------------------------------------------------

constructor TcaCustomSizeBar.Create(AOwner: TComponent);
begin
  inherited;
  FButton := TcaSizeBarButton.Create(Self);
  FButton.Color := clBtnFace;
  FButton.Style := bsNoEdge;
  FButton.OnClick := ButtonClickEvent;
  FButton.Font.Name := 'Marlett';
  FButton.Font.Size := 10;
  FButton.Font.Color := clNavy;
  FButton.TextStyle := tsNormal;
  ControlStyle := ControlStyle - [csSetCaption];
  Width := 12;
  Height := 160;
  FKind := sbVertical;
  FState := bsBig;
  UpdateButtonArrows;
end;

destructor TcaCustomSizeBar.Destroy;
begin
  inherited;
end;

procedure TcaCustomSizeBar.CreateWnd;
begin
  inherited;
  UpdateButtonPosition;
end;

procedure TcaCustomSizeBar.ButtonClickEvent(Sender: TObject);
begin
  if FState = bsBig then FState := bsSmall else FState := bsBig;
  UpdateButtonArrows;
  if Assigned(FOnStateChange) then FOnStateChange(Self);
end;

procedure TcaCustomSizeBar.CMEnabledChanged(var Message: TMessage); 
begin
  inherited;
  FButton.Enabled := Enabled;
end;

function TcaCustomSizeBar.GetButtonPosition: TcaSizeBarBtnPosition;
begin
  Result := FButtonPosition;
end;

function TcaCustomSizeBar.GetBigAndSmall: TcaSizeBarArrows;
begin
  Result := FBigAndSmall;
end;

function TcaCustomSizeBar.GetButtonGapLeft: Integer;
begin
  Result := FButtonGapLeft;
end;

function TcaCustomSizeBar.GetButtonGapBottom: Integer;
begin
  Result := FButtonGapBottom;
end;

function TcaCustomSizeBar.GetButtonStyle: TcaButtonStyle;
begin
	Result := FButton.Style;
end;

function TcaCustomSizeBar.GetState: TcaSizeBarState;
begin
  Result := FState;
end;

function TcaCustomSizeBar.GetOnStateChange: TNotifyEvent;
begin
  Result := FOnStateChange;
end;

procedure TcaCustomSizeBar.Resize;
begin
  inherited;
  UpdateButtonPosition;
end;

procedure TcaCustomSizeBar.SetBigAndSmall(const Value: TcaSizeBarArrows);
begin
  if Value <> FBigAndSmall then
    begin
      FBigAndSmall := Value;
      UpdateButtonArrows;
    end;
end;

procedure TcaCustomSizeBar.SetState(const Value: TcaSizeBarState);
begin
  if Value <> FState then
    begin
      FState := Value;
      UpdateButtonArrows;
    end;
end;

procedure TcaCustomSizeBar.UpdateButtonArrows;
var
  Arrow: TcaArrowType;
begin
  Arrow := Low(TcaArrowType);
  case
    FBigAndSmall of
      baLeftRight:  if FState = bsBig then Arrow := atLeft else Arrow := atRight;
      baRightLeft:  if FState = bsBig then Arrow := atRight else Arrow := atLeft;
      baUpDown:     if FState = bsBig then Arrow := atUp else Arrow := atDown;
      baDownUp:     if FState = bsBig then Arrow := atDown else Arrow := atUp;
    end;
  FButton.Arrow := Arrow;
end;

procedure TcaCustomSizeBar.UpdateButtonPosition;
var
  A3d: Integer;
  P: Byte;
begin
  FButton.Parent := Self;
  P := Ord(FButtonPosition);
  if Width > Height then
    begin
      A3d := Width div 3;
      FButton.SetBounds(Max(1, A3d * P - (4 * P)) + FButtonGapLeft,    // Left
                        1 + FButtonGapTop,                             // Top
                        A3d + 13 - (3 * P) + 1 - FButtonGapRight,      // Width
                        Height - 3 - FButtonGapBottom - FButtonGapTop);   // Height
    end
  else
    begin
      A3d := Height div 3;
      FButton.SetBounds(1 + FButtonGapLeft,                            // Left
                        Max(1, A3d * P - (4 * P)) + FButtonGapTop,     // Top
                        Width - 2 - FButtonGapRight - FButtonGapLeft,     // Width
                        A3d + 13 - (3 * P) + 1 - FButtonGapBottom);    // Height
    end;
  UpdateButtonArrows;
end;

procedure TcaCustomSizeBar.SetButtonPosition(const Value: TcaSizeBarBtnPosition);
begin
  if Value <> FButtonPosition then
    begin
      FButtonPosition := Value;
      UpdateButtonPosition;
    end;
end;

procedure TcaCustomSizeBar.SetButtonStyle(const Value: TcaButtonStyle);
begin
  FButton.Style := Value;
end;

procedure TcaCustomSizeBar.SetOnStateChange(const Value: TNotifyEvent);
begin
  FOnStateChange := Value;
end;

function TcaCustomSizeBar.GetButtonGapRight: Integer;
begin
  Result := FButtonGapRight;
end;

function TcaCustomSizeBar.GetButtonGapTop: Integer;
begin
  Result := FButtonGapTop;
end;

procedure TcaCustomSizeBar.SetButtonGapBottom(const Value: Integer);
begin
  if Value <> FButtonGapBottom then
    begin
      FButtonGapBottom := Value;
      UpdateButtonPosition;
    end;
end;

procedure TcaCustomSizeBar.SetButtonGapLeft(const Value: Integer);
begin
  if Value <> FButtonGapLeft then
    begin
      FButtonGapLeft := Value;
      UpdateButtonPosition;
    end;
end;

procedure TcaCustomSizeBar.SetButtonGapRight(const Value: Integer);
begin
  if Value <> FButtonGapRight then
    begin
      FButtonGapRight := Value;
      UpdateButtonPosition;
    end;
end;

procedure TcaCustomSizeBar.SetButtonGapTop(const Value: Integer);
begin
  if Value <> FButtonGapTop then
    begin
      FButtonGapTop := Value;
      UpdateButtonPosition;
    end;
end;

 //----------------------------------------------------------------------------
 // TcaSizeBarProperties
 //----------------------------------------------------------------------------

procedure TcaSizeBarProperties.Assign(Source: TPersistent);
var
  SourceSizeBarProps: TcaSizeBarProperties;
begin
  if Source is TcaSizeBarProperties then
    begin
      SourceSizeBarProps := TcaSizeBarProperties(Source);
      BigAndSmall := SourceSizeBarProps.BigAndSmall;
      ButtonGapBottom := SourceSizeBarProps.ButtonGapBottom;
      ButtonGapLeft := SourceSizeBarProps.ButtonGapLeft;
      ButtonGapRight := SourceSizeBarProps.GetButtonGapRight;
      ButtonGapTop := SourceSizeBarProps.ButtonGapTop;
      ButtonPosition := SourceSizeBarProps.ButtonPosition;
      ButtonStyle := SourceSizeBarProps.ButtonStyle;
      OnStateChange := SourceSizeBarProps.OnStateChange;
      State := SourceSizeBarProps.State;
    end;
  inherited;
end;
 
function TcaSizeBarProperties.GetBigAndSmall: TcaSizeBarArrows;
begin
  Result := FSizeBar.BigAndSmall;
end;

function TcaSizeBarProperties.GetButtonGapBottom: Integer;
begin
  Result := FSizeBar.ButtonGapBottom;
end;

function TcaSizeBarProperties.GetButtonGapLeft: Integer;
begin
  Result := FSizeBar.ButtonGapLeft;
end;

function TcaSizeBarProperties.GetButtonGapRight: Integer;
begin
  Result := FSizeBar.ButtonGapRight;
end;

function TcaSizeBarProperties.GetButtonGapTop: Integer;
begin
  Result := FSizeBar.ButtonGapTop;
end;

function TcaSizeBarProperties.GetButtonPosition: TcaSizeBarBtnPosition;
begin
  Result := FSizeBar.ButtonPosition;
end;

function TcaSizeBarProperties.GetButtonStyle: TcaButtonStyle;
begin
  Result := FSizeBar.ButtonStyle;
end;

function TcaSizeBarProperties.GetOnStateChange: TNotifyEvent;
begin
  Result := FSizeBar.OnStateChange;
end;

function TcaSizeBarProperties.GetState: TcaSizeBarState;
begin
  Result := FSizeBar.State;
end;

function TcaSizeBarProperties.GetWidth: Integer;
begin
  Result := FSizeBar.Width;
end;

procedure TcaSizeBarProperties.SetBigAndSmall(const Value: TcaSizeBarArrows);
begin
  FSizeBar.BigAndSmall := Value;
end;

procedure TcaSizeBarProperties.SetButtonGapBottom(const Value: Integer);
begin
  FSizeBar.ButtonGapBottom := Value;
end;

procedure TcaSizeBarProperties.SetButtonGapLeft(const Value: Integer);
begin
  FSizeBar.ButtonGapLeft := Value;
end;

procedure TcaSizeBarProperties.SetButtonGapRight(const Value: Integer);
begin
  FSizeBar.ButtonGapRight := Value;
end;

procedure TcaSizeBarProperties.SetButtonGapTop(const Value: Integer);
begin
  FSizeBar.ButtonGapTop := Value;
end;

procedure TcaSizeBarProperties.SetButtonPosition(const Value: TcaSizeBarBtnPosition);
begin
  FSizeBar.ButtonPosition := Value;
end;

procedure TcaSizeBarProperties.SetButtonStyle(const Value: TcaButtonStyle);
begin
  FSizeBar.ButtonStyle := Value;
end;

procedure TcaSizeBarProperties.SetOnStateChange(const Value: TNotifyEvent);
begin
  FSizeBar.OnStateChange := Value;
end;

procedure TcaSizeBarProperties.SetState(const Value: TcaSizeBarState);
begin
  FSizeBar.State := Value;
end;

procedure TcaSizeBarProperties.SetWidth(const Value: Integer);
begin
  FSizeBar.Width := Value;
end;

end.
