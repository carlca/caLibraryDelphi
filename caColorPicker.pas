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


unit caColorPicker;

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
  ExtCtrls,
  Math,
  Dialogs,

  // ca units
  caTypes,
  caConsts,
  caClasses,
  caControls,
  caGraphics,
  caButtons;

type

 //---------------------------------------------------------------------------
 // IcaColorPicker
 //---------------------------------------------------------------------------

  IcaColorPicker = interface
  ['{3237C399-982E-4882-948B-AEAA88335577}']
    // Property methods
    function GetOnSelectedColorChanged: TNotifyEvent;
    function GetSelectedColor: TColor;
    procedure SetOnSelectedColorChanged(const Value: TNotifyEvent);
    procedure SetSelectedColor(const Value: TColor);
    // Properties
    property OnSelectedColorChanged: TNotifyEvent read GetOnSelectedColorChanged write SetOnSelectedColorChanged;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
  end;

 //---------------------------------------------------------------------------
 // TcaCustomColorPicker
 //---------------------------------------------------------------------------

  TcaCustomColorPicker = class(TcaCustomSpeedButton, IcaColorPicker)
  private
    FColors: TcaColors;
    FOnSelectedColorChanged: TNotifyEvent;
    FSelectedColor: TColor;
    // Property methods
    function GetOnSelectedColorChanged: TNotifyEvent;
    function GetSelectedColor: TColor;
    procedure SetOnSelectedColorChanged(const Value: TNotifyEvent);
    procedure SetSelectedColor(const Value: TColor);
    // Private methods
    procedure ChooseColor;
    procedure DrawSelectedColor;
  protected
    procedure DoSelectedColorChanged; virtual;
    procedure Paint; override;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
    property OnSelectedColorChanged: TNotifyEvent read GetOnSelectedColorChanged write SetOnSelectedColorChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  end;

 //---------------------------------------------------------------------------
 // TcaColorPicker
 //---------------------------------------------------------------------------

  TcaColorPicker = class(TcaCustomColorPicker)
  public
    property OnTextChanged;
  published
    property Action;
    property Align;
    property AllowAllUp;
    property Anchors;
    property BiDiMode;
    property Color;
    property Color3DLight;
    property ColorBtnHighlight;
    property ColorBtnShadow;
    property ColorWindowFrame;
    property Constraints;
    property DisableDown;
    property Down;
    property DragKind;
    property Enabled;
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
    property OnSelectedColorChanged;
    property OnStartDock;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property SelectedColor;
    property ShowHint;
    property Style;
    property Visible;
  end;

implementation

 //----------------------------------------------------------------------------
 // TcaCustomColorPicker
 //----------------------------------------------------------------------------

constructor TcaCustomColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  FColors := TcaColors.Create;
  FColors.AddGretagColors;
end;

destructor TcaCustomColorPicker.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TcaCustomColorPicker.ChooseColor;
var
  Dlg: TColorDialog;
begin
  Dlg := TColorDialog.Create(nil);
  try
    Dlg.Color := FSelectedColor;
    Dlg.Options := [cdFullOpen, cdSolidColor, cdAnyColor];
    Dlg.CustomColors.Add('ColorA=' + IntToHex(ColorToRGB(clBtnFace), 6));
    Dlg.CustomColors.Add('ColorB=' + IntToHex($E0E0E0, 6));
    if Dlg.Execute then
      begin
        FSelectedColor := Dlg.Color;
        Invalidate;
        DoSelectedColorChanged;
      end;
  finally
    Dlg.Free;
  end;
end;

procedure TcaCustomColorPicker.Click;
begin
  inherited;
  ChooseColor;
end;

procedure TcaCustomColorPicker.DoSelectedColorChanged;
begin
  if Assigned(FOnSelectedColorChanged) then
    FOnSelectedColorChanged(Self);    
end;

procedure TcaCustomColorPicker.DrawSelectedColor;
var
  C: TCanvas;
  R: IcaRect;
begin
  C := OffScreenCanvas;
  C.Brush.Color := FSelectedColor;
  C.Brush.Style := bsSolid;
  R := TcaRect.Create(Rect(0, 0, Width, Height));
  R.Adjust(5, 5, -6, -6);
  if MouseIsDown then R.Adjust(1, 1, 1, 1);
  C.FillRect(R.Rect);
  C.Brush.Color := clBtnShadow;
  C.FrameRect(R.Rect);
  UpdateOnScreenBitmap;
end;

function TcaCustomColorPicker.GetOnSelectedColorChanged: TNotifyEvent;
begin
  Result := FOnSelectedColorChanged;
end;

function TcaCustomColorPicker.GetSelectedColor: TColor;
begin
  Result := FSelectedColor;
end;

procedure TcaCustomColorPicker.Paint;
begin
  inherited;
  DrawSelectedColor;
end;

procedure TcaCustomColorPicker.SetOnSelectedColorChanged(const Value: TNotifyEvent);
begin
  FOnSelectedColorChanged := Value;
end;

procedure TcaCustomColorPicker.SetSelectedColor(const Value: TColor);
begin
  if Value <> FSelectedColor then
    begin
      FSelectedColor := Value;
      Invalidate;
    end;
end;

end.
