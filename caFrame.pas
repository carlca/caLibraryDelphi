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


unit caFrame;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  Classes,
  Sysutils,
  Graphics,
  Controls,

  // ca units 
  caTypes,
  caClasses,
  caUtils;

type

  //---------------------------------------------------------------------------
  // IcaFrame                                                                  
  //---------------------------------------------------------------------------

  IcaFrame = interface
  ['{78D1006D-2B2A-4A73-AC33-95AC59642B07}']
    function GetBottomWidth: Integer;
    function GetCanvas: TCanvas;
    function GetControl: TWinControl;
    function GetFocused: Boolean;
    function GetFocusedLineColor: TColor;
    function GetFocusedSides: TcaSides;
    function GetFocusedStyle: TcaFrameStyle;
    function GetLeftWidth: Integer;
    function GetLineColor: TColor;
    function GetRightWidth: Integer;
    function GetSides: TcaSides;
    function GetStyle: TcaFrameStyle;
    function GetSyncLineColor: Boolean;
    function GetSyncSides: Boolean;
    function GetSyncStyle: Boolean;
    function GetTopWidth: Integer;
    function ReadyToPaint: Boolean;
    procedure AdjustOffsets(const ALeft, ATop, ARight, ABottom: Integer);
    procedure SetControl(const Value: TWinControl);
    procedure SetFocused(const Value: Boolean);
    procedure SetFocusedLineColor(const Value: TColor);
    procedure SetFocusedSides(const Value: TcaSides);
    procedure SetFocusedStyle(const Value: TcaFrameStyle);
    procedure SetLineColor(const Value: TColor);
    procedure SetSides(const Value: TcaSides);
    procedure SetStyle(const Value: TcaFrameStyle);
    procedure SetSyncLineColor(const Value: Boolean);
    procedure SetSyncSides(const Value: Boolean);
    procedure SetSyncStyle(const Value: Boolean);
    procedure Update;
    property BottomWidth: Integer read GetBottomWidth;
    property Canvas: TCanvas read GetCanvas;
    property Control: TWinControl read GetControl write SetControl;
    property Focused: Boolean read GetFocused write SetFocused;
    property FocusedLineColor: TColor read GetFocusedLineColor write SetFocusedLineColor;
    property FocusedSides: TcaSides read GetFocusedSides write SetFocusedSides;
    property FocusedStyle: TcaFrameStyle read GetFocusedStyle write SetFocusedStyle;
    property LeftWidth: Integer read GetLeftWidth;
    property LineColor: TColor read GetLineColor write SetLineColor;
    property RightWidth: Integer read GetRightWidth;
    property Sides: TcaSides read GetSides write SetSides;
    property Style: TcaFrameStyle read GetStyle write SetStyle;
    property SyncLineColor: Boolean read GetSyncLineColor write SetSyncLineColor;
    property SyncSides: Boolean read GetSyncSides write SetSyncSides;
    property SyncStyle: Boolean read GetSyncStyle write SetSyncStyle;
    property TopWidth: Integer read GetTopWidth;
  end;

  //---------------------------------------------------------------------------
  // TcaFrame                                                                  
  //---------------------------------------------------------------------------

  TcaFrame = class(TInterfacedObject, IcaFrame)
  private
    FBottomOffset: Integer;
    FBottomWidth: Integer;
    FCanvas: TCanvas;
    FControl: TWinControl;
    FFocused: Boolean;
    FFocusedLineColor: TColor;
    FFocusedSides: TcaSides;
    FFocusedStyle: TcaFrameStyle;
    FLeftOffset: Integer;
    FLeftWidth: Integer;
    FLineColor: TColor;
    FPaintLineColor: TColor;
    FPaintSides: TcaSides;
    FPaintStyle: TcaFrameStyle;
    FRightOffset: Integer;
    FRightWidth: Integer;
    FSides: TcaSides;
    FStyle: TcaFrameStyle;
    FSyncLineColor: Boolean;
    FSyncSides: Boolean;
    FSyncStyle: Boolean;
    FTopOffset: Integer;
    FTopWidth: Integer;
    function GetBottomWidth: Integer;
    function GetLeftWidth: Integer;
    function GetRightWidth: Integer;
    function GetTopWidth: Integer;
    function GetCanvas: TCanvas;
    function GetControl: TWinControl;
    function GetFocused: Boolean;
    function GetFocusedLineColor: TColor;
    function GetFocusedSides: TcaSides;
    function GetFocusedStyle: TcaFrameStyle;
    function GetLineColor: TColor;
    function GetSides: TcaSides;
    function GetStyle: TcaFrameStyle;
    function GetSyncLineColor: Boolean;
    function GetSyncSides: Boolean;
    function GetSyncStyle: Boolean;
    function ReadyToPaint: Boolean;
    procedure AdjustOffsets(const ALeft, ATop, ARight, ABottom: Integer);
    procedure PaintLine(const FrameWidth: Integer; Colors: array of TColor);
    procedure PaintLoweredLine;
    procedure PaintLoweredPanelLine;
    procedure PaintRaisedLine;
    procedure PaintRaisedPanelLine;
    procedure PaintSingleLine;
    procedure SetControl(const Value: TWinControl);
    procedure SetFocused(const Value: Boolean);
    procedure SetFocusedLineColor(const Value: TColor);
    procedure SetFocusedSides(const Value: TcaSides);
    procedure SetFocusedStyle(const Value: TcaFrameStyle);
    procedure SetLineColor(const Value: TColor);
    procedure SetSides(const Value: TcaSides);
    procedure SetStyle(const Value: TcaFrameStyle);
    procedure SetSyncLineColor(const Value: Boolean);
    procedure SetSyncSides(const Value: Boolean);
    procedure SetSyncStyle(const Value: Boolean);
  protected
    procedure Paint; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Update;
    property BottomWidth: Integer read GetBottomWidth;
    property FocusedSides: TcaSides read GetFocusedSides write SetFocusedSides;
    property FocusedStyle: TcaFrameStyle read GetFocusedStyle write SetFocusedStyle;
    property LeftWidth: Integer read GetLeftWidth;
    property LineColor: TColor read GetLineColor write SetLineColor;
    property RightWidth: Integer read GetRightWidth;
    property Sides: TcaSides read GetSides write SetSides;
    property Style: TcaFrameStyle read GetStyle write SetStyle;
    property SyncLineColor: Boolean read GetSyncLineColor write SetSyncLineColor;
    property SyncSides: Boolean read GetSyncSides write SetSyncSides;
    property SyncStyle: Boolean read GetSyncStyle write SetSyncStyle;
    property TopWidth: Integer read GetTopWidth;
  end;

  //---------------------------------------------------------------------------
  // IcaFrameProperties                                                        
  //---------------------------------------------------------------------------

  IcaFrameProperties = interface
  ['{81BC414C-C441-4DD9-9458-DB143A399D32}']
    // Property methods
    function GetFocusedLineColor: TColor;
    function GetFocusedSides: TcaSides;
    function GetFocusedStyle: TcaFrameStyle;
    function GetFrame: IcaFrame;
    function GetLineColor: TColor;
    function GetSides: TcaSides;
    function GetStyle: TcaFrameStyle;
    function GetSyncSides: Boolean;
    function GetSyncStyle: Boolean;
    procedure SetFocusedLineColor(const Value: TColor);
    procedure SetFocusedSides(const Value: TcaSides);
    procedure SetFocusedStyle(const Value: TcaFrameStyle);
    procedure SetFrame(const Value: IcaFrame);
    procedure SetLineColor(const Value: TColor);
    procedure SetSides(const Value: TcaSides);
    procedure SetStyle(const Value: TcaFrameStyle);
    procedure SetSyncSides(const Value: Boolean);
    procedure SetSyncStyle(const Value: Boolean);
    // Properties
    property FocusedSides: TcaSides read GetFocusedSides write SetFocusedSides;
    property FocusedStyle: TcaFrameStyle read GetFocusedStyle write SetFocusedStyle;
    property FocusedLineColor: TColor read GetFocusedLineColor write SetFocusedLineColor;
    property Frame: IcaFrame read GetFrame write SetFrame;
    property LineColor: TColor read GetLineColor write SetLineColor;
    property Sides: TcaSides read GetSides write SetSides;
    property Style: TcaFrameStyle read GetStyle write SetStyle;
    property SyncSides: Boolean read GetSyncSides write SetSyncSides;
    property SyncStyle: Boolean read GetSyncStyle write SetSyncStyle;
  end;

  //---------------------------------------------------------------------------
  // TcaFrameProperties                                                        
  //---------------------------------------------------------------------------

  TcaFrameProperties = class(TcaInterfacedPersistent, IcaFrameProperties)
  private
    FFrame: IcaFrame;
    function GetFocusedSides: TcaSides;
    function GetFocusedStyle: TcaFrameStyle;
    function GetFrame: IcaFrame;
    function GetLineColor: TColor;
    function GetSides: TcaSides;
    function GetStyle: TcaFrameStyle;
    function GetSyncSides: Boolean;
    function GetSyncStyle: Boolean;
    function GetFocusedLineColor: TColor;
    procedure SetFocusedLineColor(const Value: TColor);
    procedure SetFocusedSides(const Value: TcaSides);
    procedure SetFocusedStyle(const Value: TcaFrameStyle);
    procedure SetFrame(const Value: IcaFrame);
    procedure SetLineColor(const Value: TColor);
    procedure SetSides(const Value: TcaSides);
    procedure SetStyle(const Value: TcaFrameStyle);
    procedure SetSyncSides(const Value: Boolean);
    procedure SetSyncStyle(const Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property FocusedSides: TcaSides read GetFocusedSides write SetFocusedSides;
    property FocusedStyle: TcaFrameStyle read GetFocusedStyle write SetFocusedStyle;
    property FocusedLineColor: TColor read GetFocusedLineColor write SetFocusedLineColor;
    property Frame: IcaFrame read GetFrame write SetFrame;
    property LineColor: TColor read GetLineColor write SetLineColor;
    property Sides: TcaSides read GetSides write SetSides;
    property Style: TcaFrameStyle read GetStyle write SetStyle;
    property SyncSides: Boolean read GetSyncSides write SetSyncSides;
    property SyncStyle: Boolean read GetSyncStyle write SetSyncStyle;
  end;

implementation

uses
  caControls;

  //---------------------------------------------------------------------------
  // Cracker classes                                                           
  //---------------------------------------------------------------------------

type

  TcaCustomPanelEx = class(TcaCustomPanel);

  //---------------------------------------------------------------------------
  // TcaFrame                                                                  
  //---------------------------------------------------------------------------

constructor TcaFrame.Create;
begin
  inherited;
end;

destructor TcaFrame.Destroy;
begin
  if not (FControl is TcaCustomPanel) then
    FCanvas.Free;
  inherited;
end;

procedure TcaFrame.AdjustOffsets(const ALeft, ATop, ARight, ABottom: Integer);
begin
  FLeftOffset := ALeft;
  FTopOffset := ATop;
  FRightOffset := ARight;
  FBottomOffset := ABottom;
end;

function TcaFrame.GetBottomWidth: Integer;
begin
  Result := FBottomWidth;
end;

function TcaFrame.GetCanvas: TCanvas;
begin
  Result := FCanvas;
end;

function TcaFrame.GetControl: TWinControl;
begin
  Result := FControl;
end;

function TcaFrame.GetFocused: Boolean;
begin
  Result := FFocused;
end;

function TcaFrame.GetFocusedLineColor: TColor;
begin
  Result := FFocusedLineColor;
end;

function TcaFrame.GetFocusedSides: TcaSides;
begin
  Result := FFocusedSides;
end;

function TcaFrame.GetFocusedStyle: TcaFrameStyle;
begin
  Result := FFocusedStyle;
end;

function TcaFrame.GetLeftWidth: Integer;
begin
  Result := FLeftWidth;
end;

function TcaFrame.GetSides: TcaSides;
begin
  Result := FSides;
end;

function TcaFrame.GetStyle: TcaFrameStyle;
begin
  Result := FStyle;
end;

function TcaFrame.GetLineColor: TColor;
begin
  Result := FLineColor;
end;

function TcaFrame.GetRightWidth: Integer;
begin
  Result := FRightWidth;
end;

function TcaFrame.GetSyncLineColor: Boolean;
begin
  Result := FSyncLineColor;
end;

function TcaFrame.GetSyncSides: Boolean;
begin
  Result := FSyncSides;
end;

function TcaFrame.GetSyncStyle: Boolean;
begin
  Result := FSyncStyle;
end;

function TcaFrame.GetTopWidth: Integer;
begin
  Result := FTopWidth;
end;

procedure TcaFrame.SetControl(const Value: TWinControl);
begin
  FControl := Value;
  if FControl is TcaCustomPanel then
    FCanvas := TcaCustomPanelEx(FControl).GetOffScreenCanvas
  else
    begin
      FCanvas := TControlCanvas.Create;
      TControlCanvas(FCanvas).Control := FControl;
    end;
end;

procedure TcaFrame.SetFocused(const Value: Boolean);
begin
  FFocused := Value;
end;

procedure TcaFrame.SetFocusedLineColor(const Value: TColor);
begin
  FFocusedLineColor := Value;
  FControl.Invalidate;
end;

procedure TcaFrame.SetFocusedSides(const Value: TcaSides);
begin
  FFocusedSides := Value;
  FControl.Invalidate;
end;

procedure TcaFrame.SetFocusedStyle(const Value: TcaFrameStyle);
begin
  FFocusedStyle := Value;
  FControl.Invalidate;
end;

procedure TcaFrame.SetSides(const Value: TcaSides);
begin
  FSides := Value;
  FControl.Invalidate;
end;

procedure TcaFrame.SetStyle(const Value: TcaFrameStyle);
begin
  FStyle := Value;
  FControl.Invalidate;
end;

procedure TcaFrame.SetLineColor(const Value: TColor);
begin
  FLineColor := Value;
  FControl.Invalidate;
end;

procedure TcaFrame.SetSyncLineColor(const Value: Boolean);
begin
  FSyncLineColor := Value;
end;

procedure TcaFrame.SetSyncSides(const Value: Boolean);
begin
  FSyncSides := Value;
  FControl.Invalidate;
end;

procedure TcaFrame.SetSyncStyle(const Value: Boolean);
begin
  FSyncStyle := Value;
  FControl.Invalidate;
end;

procedure TcaFrame.Update;
begin
  if ReadyToPaint then Paint;
end;

procedure TcaFrame.Paint;
begin
  if FFocused then
    begin
      FPaintLineColor := FFocusedLineColor;
      FPaintSides := FFocusedSides;
      FPaintStyle := FFocusedStyle;
    end
  else
    begin
      FPaintLineColor := FLineColor;
      FPaintSides := FSides;
      FPaintStyle := FStyle;
    end;
  case FPaintStyle of
    fsLowered:      PaintLoweredLine;
    fsRaised:       PaintRaisedLine;
    fsLoweredPanel: PaintLoweredPanelLine;
    fsRaisedPanel:  PaintRaisedPanelLine;
    fsLine:         PaintSingleLine;
  end;
end;

procedure TcaFrame.PaintLine(const FrameWidth: Integer; Colors: array of TColor);
var
  C: TCanvas;
  R: TRect;
  Offset: Integer;
  L, T, W, H: Integer;

  procedure PaintLeft;
  begin
    C.MoveTo(Offset, Offset);
    C.LineTo(Offset, H - Offset);
    FLeftWidth := FrameWidth;
  end;

  procedure PaintRight;
  begin
    C.MoveTo(W - Offset, Offset);
    C.LineTo(W - Offset, H - Offset);
    FRightWidth := FrameWidth;
  end;

  procedure PaintTop;
  begin
    C.MoveTo(Offset, Offset);
    C.LineTo(W - Offset, Offset);
    FTopWidth := FrameWidth;
  end;

  procedure PaintBottom;
  begin
    C.MoveTo(Offset, H - Offset);
    C.LineTo(W - Offset + 1, H - Offset);
    FBottomWidth := FrameWidth;    
  end;

  function BottomRightIntersect: Boolean;
  begin
    Result := ([sdRight, sdBottom] * FPaintSides <> []);
  end;

  procedure DrawBottomRightPixel;
  var
    PixelColor: TColor;
  begin
    PixelColor := clNone;
    case
      FPaintStyle of
        fsLowered:      PixelColor := clBtnHighlight;
        fsRaised:       PixelColor := clBtnShadow;
        fsLoweredPanel: PixelColor := clBtnHighlight;
        fsRaisedPanel:  PixelColor := clBtnShadow;
        fsLine:         PixelColor := Colors[0];
      end;
    C.Pixels[W + 1 - Offset, H + 1 - Offset] := PixelColor;
  end;

begin
  C := FCanvas;
  C.Pen.Style := psSolid;
  R := FControl.BoundsRect;
  R.Right := R.Left + FControl.ClientRect.Right;
  R.Bottom := R.Top + FControl.ClientRect.Bottom;
  Utils.AdjustRect(R, FLeftOffset, FTopOffset, FRightOffset, FBottomOffset);
  Utils.DecodeRect(R, L, T, W, H);
  FLeftWidth := 0;
  FTopWidth := 0;
  FRightWidth := 0;
  FBottomWidth := 0;
  for Offset := 0 to FrameWidth - 1 do
    begin
      C.Pen.Color := Colors[Offset];
    	if sdLeft in FPaintSides then PaintLeft;
    	if sdTop in FPaintSides then PaintTop;
      C.Pen.Color := Colors[1 - Offset];
    	if sdRight in FPaintSides then PaintRight;
    	if sdBottom in FPaintSides then PaintBottom;
    end;
  if BottomRightIntersect and (FrameWidth = 1) then
    DrawBottomRightPixel;
end;

procedure TcaFrame.PaintSingleLine;
begin
  PaintLine(1, [FPaintLineColor, FPaintLineColor]);
end;

procedure TcaFrame.PaintRaisedPanelLine;
begin
  PaintLine(1, [clBtnHighlight, clBtnShadow]);
end;

procedure TcaFrame.PaintRaisedLine;
begin
  PaintLine(2, [clBtnHighlight, clBtnShadow]);
end;

procedure TcaFrame.PaintLoweredPanelLine;
begin
  PaintLine(1, [clBtnShadow, clBtnHighlight]);
end;

procedure TcaFrame.PaintLoweredLine;
begin
  PaintLine(2, [clBtnShadow, clBtnHighlight]);
end;

function TcaFrame.ReadyToPaint: Boolean;
var
  CompState: IcaComponentState;
begin
  Result := False;
  if FControl <> nil then
    begin
      CompState := TcaComponentState.Create;
      CompState.Component := FControl;
      if not CompState.IsDestroying then
        begin
          if FControl.Visible or CompState.IsDesigning then
            Result := True;
        end;
    end;
end;

  //---------------------------------------------------------------------------
  // TcaFrameProperties                                                        
  //---------------------------------------------------------------------------

procedure TcaFrameProperties.Assign(Source: TPersistent);
var
  SourceFrameProps: TcaFrameProperties;
begin
  if Source is TcaFrameProperties then
    begin
      SourceFrameProps := TcaFrameProperties(Source);
      FocusedLineColor := SourceFrameProps.FocusedLineColor;
      FocusedSides := SourceFrameProps.FocusedSides;
      FocusedStyle := SourceFrameProps.FocusedStyle;
      LineColor := SourceFrameProps.LineColor;
      Sides := SourceFrameProps.Sides;
      Style := SourceFrameProps.Style;
      SyncSides := SourceFrameProps.SyncSides;
      SyncStyle := SourceFrameProps.SyncStyle;
    end;
  inherited;
end;

function TcaFrameProperties.GetFocusedLineColor: TColor;
begin
  Result := FFrame.FocusedLineColor;
end;

function TcaFrameProperties.GetFocusedSides: TcaSides;
begin
  Result := FFrame.FocusedSides;
end;

function TcaFrameProperties.GetFocusedStyle: TcaFrameStyle;
begin
  Result := FFrame.FocusedStyle;
end;

function TcaFrameProperties.GetFrame: IcaFrame;
begin
  Result := FFrame;
end;

function TcaFrameProperties.GetLineColor: TColor;
begin
  Result := FFrame.LineColor;
end;

function TcaFrameProperties.GetSides: TcaSides;
begin
  Result := FFrame.Sides;
end;

function TcaFrameProperties.GetStyle: TcaFrameStyle;
begin
  Result := FFrame.Style;
end;

function TcaFrameProperties.GetSyncSides: Boolean;
begin
  Result := FFrame.SyncSides;
end;

function TcaFrameProperties.GetSyncStyle: Boolean;
begin
  Result := FFrame.SyncStyle;
end;

procedure TcaFrameProperties.SetFocusedLineColor(const Value: TColor);
begin
  FFrame.FocusedLineColor := Value;
end;

procedure TcaFrameProperties.SetFocusedSides(const Value: TcaSides);
begin
  FFrame.FocusedSides := Value;
end;

procedure TcaFrameProperties.SetFocusedStyle(const Value: TcaFrameStyle);
begin
  FFrame.FocusedStyle := Value;
end;

procedure TcaFrameProperties.SetFrame(const Value: IcaFrame);
begin
  FFrame := Value;
end;

procedure TcaFrameProperties.SetLineColor(const Value: TColor);
begin
  FFrame.LineColor := Value;
end;

procedure TcaFrameProperties.SetSides(const Value: TcaSides);
begin
  FFrame.Sides := Value;
end;

procedure TcaFrameProperties.SetStyle(const Value: TcaFrameStyle);
begin
  FFrame.Style := Value;
end;

procedure TcaFrameProperties.SetSyncSides(const Value: Boolean);
begin
  FFrame.SyncSides := Value;
end;

procedure TcaFrameProperties.SetSyncStyle(const Value: Boolean);
begin
  FFrame.SyncStyle := Value;
end;

end.
