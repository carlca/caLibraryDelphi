unit caCheckbox;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls;

type

 //---------------------------------------------------------------------------
 // TcaCheckBox
 //---------------------------------------------------------------------------

  TcaCheckBox = class(TCustomControl)
  private
    FCheckedGlyph: TBitmap;
    FUncheckedGlyph: TBitmap;
    FGrayedGlyph: TBitmap;
    FAlignment: TLeftRight;
    FState: TCheckBoxState;
    FAllowGrayed: Boolean;
    FCheckedNumGlyphs: Integer;
    FGrayedNumGlyphs: Integer;
    FUncheckedNumGlyphs: Integer;
    FShowFocusRect: Boolean;
    FGrayDisabledText: Boolean;
    // Property methods
    function GetChecked: Boolean;
    procedure SetCheckedNumGlyphs(Value: Integer);
    procedure SetGrayedNumGlyphs(Value: Integer);
    procedure SetUncheckedNumGlyphs(Value: Integer);
    procedure SetCheckedGlyph(Value: TBitmap);
    procedure SetGrayedGlyph(Value: TBitmap);
    procedure SetUncheckedGlyph(Value: TBitmap);
    procedure SetState(Value: TCheckBoxState);
    procedure SetAlignment(Value: TLeftRight);
    procedure SetChecked(Value: Boolean);
    procedure SetShowFocusRect(Value: Boolean);
    procedure SetGrayDisabledText(Value: Boolean);
    // Private methods
    procedure CreateObjects;
    procedure FreeObjects;
    procedure InitializeInheritedProperties;
    procedure InitializeSpecificProperties;
    // Message handlers
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
  protected
    procedure Paint; override;
    procedure Click; override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Toggle; virtual;
  published
    // Promoted inherited properties
    property Action;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
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
    property UseDockManager;
    // TcaCheckBox specific properties
    property Alignment: TLeftRight read FAlignment write SetAlignment default taRightJustify;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property Checked: Boolean read GetChecked write SetChecked default False;
    property CheckedGlyph: TBitmap read FCheckedGlyph write SetCheckedGlyph;
    property CheckedNumGlyphs: Integer read FCheckedNumGlyphs write SetCheckedNumGlyphs default 1;
    property GrayDisabledText: Boolean read FGrayDisabledText write SetGrayDisabledText;
    property GrayedGlyph: TBitmap read FGrayedGlyph write SetGrayedGlyph;
    property GrayedNumGlyphs: Integer read FGrayedNumGlyphs write SetGrayedNumGlyphs default 1;
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect;
    property State: TCheckBoxState read FState write SetState default cbUnchecked;
    property UncheckedGlyph: TBitmap read FUncheckedGlyph write SetUncheckedGlyph;
    property UncheckedNumGlyphs: Integer read FUncheckedNumGlyphs write SetUncheckedNumGlyphs default 1;
    // Event properties
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnResize;
    property OnStartDock;
    property OnUnDock;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation

{$R CACHECK.RES}

constructor TcaCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateObjects;
  InitializeInheritedProperties;
  InitializeSpecificProperties;
end;

destructor TcaCheckBox.Destroy;
begin
  FreeObjects;
  inherited Destroy;
end;

 // Public methods

procedure TcaCheckBox.Toggle;
begin
  case State of
    cbUnchecked:
      if AllowGrayed then
        State := cbGrayed
      else
        State := cbChecked;
    cbChecked: State := cbUnchecked;
    cbGrayed: State := cbChecked;
  end;
end;

 // Protected methods

procedure TcaCheckBox.Paint;
const
  DrawOptions = DT_LEFT or DT_VCENTER or DT_SINGLELINE;
var
  Bitmap: TBitmap;
  BitRect: TRect;
  C: TCanvas;
  FocusRect: TRect;
  GlyphWidth: Integer;
  HOffset: Integer;
  NumGlyphs: Integer;
  OldColor: TColor;
  Rect: TRect;
  VOffset: Integer;

 {} procedure EraseClientArea;
 {} begin
 {}   Rect := ClientRect;
 {}   C.Brush.Color := Self.Color;
 {}   C.Brush.Style := bsSolid;
 {}   C.FillRect(Rect);
 {} end;

 {} procedure PrepareBrush;
 {} begin
 {}   C.Brush.Style := bsClear;
 {} end;

 {} procedure SelectGlyph;
 {} begin
 {}   case State of
 {}     cbChecked:
 {}       begin
 {}         Bitmap := FCheckedGlyph;
 {}         NumGlyphs := FCheckedNumGlyphs;
 {}       end;
 {}     cbUnChecked:
 {}       begin
 {}         Bitmap := FUncheckedGlyph;
 {}         NumGlyphs := FUncheckedNumGlyphs;
 {}       end;
 {}   else
 {}     begin
 {}       Bitmap := FGrayedGlyph;
 {}       NumGlyphs := FGrayedNumGlyphs;
 {}     end;
 {}   end;
 {}   GlyphWidth := Bitmap.Width div NumGlyphs;
 {} end;

 {} procedure DrawCaptionText;
 {} begin
 {}   Rect := ClientRect;
 {}   if Alignment = taLeftJustify then
 {}     HOffset := 1
 {}   else
 {}     HOffset := GlyphWidth + 4;
 {}   Rect.Left := HOffset;
 {}   Rect.Right := Rect.Left + C.TextWidth(Caption);
 {}   // Subtract out the width of the underscoring character
 {}   if Pos('&', Caption) > 0 then Rect.Right := Rect.Right - C.TextWidth('&');
 {}   VOffset := (ClientHeight - C.TextHeight(Caption)) div 2 - 2;
 {}   Rect.Top := VOffset;
 {}   Rect.Bottom := Rect.Top + C.TextHeight(Caption) + 3;
 {}   IntersectRect(FocusRect, Rect, ClientRect);
 {}   OldColor := C.Font.Color;
 {}   // DrawText is used because it handles
 {}   // the underscored accelerator key
 {}   if Enabled or (not FGrayDisabledText) then
 {}     DrawText(Canvas.Handle, PChar(Caption), Length(Caption), FocusRect, DrawOptions)
 {}   else
 {}     begin
 {}       if Ctl3D then
 {}         begin
 {}           // This draws disabled text in 3D
 {}           OffsetRect(FocusRect, 1, 1);
 {}           C.Font.Color := clBtnHighlight;
 {}           DrawText(Canvas.Handle, PChar(Caption), Length(Caption), FocusRect, DrawOptions);
 {}           OffsetRect(FocusRect, -1, -1);
 {}         end;
 {}       // This draws disabled text like SQL6
 {}       C.Font.Color := clGrayText;
 {}       DrawText(Canvas.Handle, PChar(Caption), Length(Caption), FocusRect, DrawOptions);
 {}     end;
 {}   C.Font.Color := OldColor;
 {} end;

 {} procedure ResetBrush;
 {} begin
 {}   C.Brush.Color := Self.Color;
 {}   C.Brush.Style := bsSolid;
 {} end;

 {} procedure DrawFocusRect;
 {} begin
 {}   if Focused and FShowFocusRect then C.DrawFocusRect(FocusRect);
 {} end;

 {} procedure DrawBitmap;
 {} begin
 {}   if Alignment = taLeftJustify then
 {}     HOffset := ClientWidth - GlyphWidth
 {}   else
 {}     HOffset := 0;
 {}   VOffset := (ClientHeight - Bitmap.Height) div 2;
 {}   // Figure out where to draw the image
 {}   Rect.Top := VOffset;
 {}   Rect.Bottom := Bitmap.Height + VOffset;
 {}   Rect.Left := HOffset;
 {}   Rect.Right := GlyphWidth + HOffset;
 {}   // Choose the correct bitmap
 {}   // If we're disabled choose the second bitmap
 {}   if not Enabled and (NumGlyphs = 2) then
 {}     BitRect.Left := GlyphWidth
 {}   else
 {}     BitRect.Left := 0;
 {}   BitRect.Right := BitRect.Left + GlyphWidth;
 {}   BitRect.Top := 0;
 {}   BitRect.Bottom := Bitmap.Height;
 {}   // Draw the image
 {}   C.BrushCopy(Rect, Bitmap, BitRect, Bitmap.TransparentColor);
 {} end;

 // caCheckBox

begin
  C := Canvas;
  EraseClientArea;
  PrepareBrush;
  SelectGlyph;
  DrawCaptionText;
  ResetBrush;
  DrawFocusRect;
  DrawBitmap;
end;

procedure TcaCheckBox.Click;
begin
  Toggle;
  inherited Click;
  if Showing and CanFocus then SetFocus;
end;

procedure TcaCheckBox.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if Key = ' ' then Click;
end;

 // Private methods

procedure TcaCheckBox.CreateObjects;
begin
  FCheckedGlyph := TBitmap.Create;
  FUncheckedGlyph := TBitmap.Create;
  FGrayedGlyph := TBitmap.Create;
end;

procedure TcaCheckBox.FreeObjects;
begin
  FGrayedGlyph.Free;
  FUncheckedGlyph.Free;
  FCheckedGlyph.Free;
end;

procedure TcaCheckBox.InitializeInheritedProperties;
begin
  ControlStyle := [csSetCaption, csClickEvents];
  Alignment := taRightJustify;
  Checked := False;
  Width := 97;
  Height := 17;
  TabStop := True;
  ParentCtl3D := True;
  AllowGrayed := False;
end;

procedure TcaCheckBox.InitializeSpecificProperties;
begin
  SetCheckedGlyph(nil);
  SetGrayedGlyph(nil);
  SetUncheckedGlyph(nil);
  SetCheckedNumGlyphs(1);
  SetGrayedNumGlyphs(1);
  SetUncheckedNumGlyphs(1);
end;

 // Message handlers

procedure TcaCheckBox.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TcaCheckBox.CMDialogChar(var Msg: TCMDialogChar);
begin
  with Msg do
    if IsAccel(CharCode, Caption) and Enabled and CanFocus then
      begin
        SetFocus;
        if Focused then Toggle;
        Result := 1;
      end
    else
      inherited;
end;

procedure TcaCheckBox.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Canvas.Font := Font;
  Invalidate;
end;

procedure TcaCheckBox.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TcaCheckBox.CMSysColorChange(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TcaCheckBox.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
  Realign;
end;

procedure TcaCheckBox.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TcaCheckBox.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Invalidate;
end;

 // Property methods

procedure TcaCheckBox.SetAlignment(Value: TLeftRight);
begin
  if Value <> FAlignment then
    begin
      FAlignment := Value;
      Invalidate;
    end;
end;

procedure TcaCheckBox.SetState(Value: TCheckBoxState);
begin
  if FState <> Value then
    begin
      FState := Value;
      Invalidate;
    end;
end;

function TcaCheckBox.GetChecked: Boolean;
begin
  Result := (State = cbChecked);
end;

procedure TcaCheckBox.SetChecked(Value: Boolean);
begin
  if Value then
    State := cbChecked
  else
    State := cbUnchecked;
end;

procedure TcaCheckBox.SetShowFocusRect(Value: Boolean);
begin
  if Value <> FShowFocusRect then
    begin
      FShowFocusRect := Value;
      Invalidate;
    end;
end;

procedure TcaCheckBox.SetGrayDisabledText(Value: Boolean);
begin
  if Value <> FGrayDisabledText then
    begin
      FGrayDisabledText := Value;
      Invalidate;
    end;
end;

procedure TcaCheckBox.SetCheckedGlyph(Value: TBitmap);
begin
  if Value = nil then
    FCheckedGlyph.LoadFromResourceName(HInstance, 'CHECKED')
  else
    FCheckedGlyph.Assign(Value);
  if (FCheckedGlyph.Width mod FCheckedGlyph.Height) = 0 then
    CheckedNumGlyphs := FCheckedGlyph.Width div FCheckedGlyph.Height;
  Invalidate;
end;

procedure TcaCheckBox.SetGrayedGlyph(Value: TBitmap);
begin
  if Value = nil then
    FGrayedGlyph.LoadFromResourceName(HInstance, 'GRAYED')
  else
    FGrayedGlyph.Assign(Value);
  if (FGrayedGlyph.Width mod FGrayedGlyph.Height) = 0 then
    GrayedNumGlyphs := FGrayedGlyph.Width div FGrayedGlyph.Height;
  Invalidate;
end;

procedure TcaCheckBox.SetUncheckedGlyph(Value: TBitmap);
begin
  if Value = nil then
    FUncheckedGlyph.LoadFromResourceName(HInstance, 'UNCHECKED')
  else
    FUncheckedGlyph.Assign(Value);
  if (FUncheckedGlyph.Width mod FUncheckedGlyph.Height) = 0 then
    UncheckedNumGlyphs := FUncheckedGlyph.Width div FUncheckedGlyph.Height;
  Invalidate;
end;

procedure TcaCheckBox.SetCheckedNumGlyphs(Value: Integer);
begin
  if Value < 1 then FCheckedNumGlyphs := 1
  else if Value > 2 then FCheckedNumGlyphs := 2
  else FCheckedNumGlyphs := Value;
  Invalidate;
end;

procedure TcaCheckBox.SetGrayedNumGlyphs(Value: Integer);
begin
  if Value < 1 then FGrayedNumGlyphs := 1
  else if Value > 2 then FGrayedNumGlyphs := 2
  else FGrayedNumGlyphs := Value;
  Invalidate;
end;

procedure TcaCheckBox.SetUncheckedNumGlyphs(Value: Integer);
begin
  if Value < 1 then FUncheckedNumGlyphs := 1
  else if Value > 2 then FUncheckedNumGlyphs := 2
  else FUncheckedNumGlyphs := Value;
  Invalidate;
end;

end.

