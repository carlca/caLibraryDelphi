unit caPageControl;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Windows,
  SysUtils,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  StdCtrls,
  ExtCtrls,
  ComCtrls;

type

  TcaPageControlMode = (moStandard, moNotebook);

  //---------------------------------------------------------------------------
  // TcaPageControl
  //---------------------------------------------------------------------------

  TcaPageControl = class(TPageControl)
  private
    // Property fields
    FMode: TcaPageControlMode;
    FHotSelected: Boolean;
    FSelectedTabColor: TColor;
    // Property methods
    function GetMode: TcaPageControlMode;
    procedure SetHotSelected(const Value: Boolean);
    procedure SetMode(Value: TcaPageControlMode);
    procedure SetSelectedTabColor(const Value: TColor);
    // Private methods
    procedure SetStandardMode;
    procedure SetNotebookMode;
    procedure UpdateMode;
  protected
    // Protected methods
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure CreateWnd; override;
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); override;
  public
    // Create/Destroy
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // Properties
    property HotSelected: Boolean read FHotSelected write SetHotSelected;
    property Mode: TcaPageControlMode read GetMode write SetMode;
    property SelectedTabColor: TColor read FSelectedTabColor write SetSelectedTabColor;
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaPageControl
  //---------------------------------------------------------------------------

  // Create/Destroy

constructor TcaPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectedTabColor := clBtnFace;
end;

destructor TcaPageControl.Destroy;
begin
  inherited Destroy;
end;

  // Protected methods

procedure TcaPageControl.AdjustClientRect(var Rect: TRect);
begin
  case FMode of
    moStandard:   inherited;
    moNotebook:   Rect := ClientRect;
  end;
end;

procedure TcaPageControl.CreateWnd;
begin
  inherited;
  UpdateMode;
end;

procedure TcaPageControl.DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  ARect: TRect;
  Offset: Integer;
begin
  if not Pages[TabIndex].TabVisible then
    inherited
  else
    begin
      if Active and (FSelectedTabColor <> Color) then
        begin
          Canvas.Brush.Color := FSelectedTabColor;
          Canvas.Brush.Style := bsSolid;
          Canvas.FillRect(Rect);
        end;
      if FHotSelected and (FMode = moStandard) then
        begin
          Canvas.Font.Assign(Font);
          if Active then
            begin
              Canvas.Font.Color := clBlue;
              Offset := 8;
            end
          else
            Offset := 4;
          Canvas.TextOut(Rect.Left + Offset, Rect.Top + 3, Pages[TabIndex].Caption);
          if Active then
            begin
              Canvas.Brush.Color := clBtnFace;
              Canvas.Brush.Style := bsSolid;
              ARect := Rect;
              ARect.Top := ARect.Bottom - 2;
              Canvas.FillRect(ARect);
            end;
        end
      else
        inherited;
    end;
end;

  // Private methods

procedure TcaPageControl.SetNotebookMode;
var
  Index: Integer;
begin
  Style := tsButtons;
  for Index := 0 to PageCount - 1 do
    Pages[Index].TabVisible := False;
end;

procedure TcaPageControl.SetStandardMode;
var
  Index: Integer;
begin
  Style := tsTabs;
  for Index := 0 to PageCount - 1 do
    Pages[Index].TabVisible := True;
end;

procedure TcaPageControl.UpdateMode;
begin
  case FMode of
    moStandard:   SetStandardMode;
    moNotebook:   SetNotebookMode;
  end;
end;

  // Property methods

function TcaPageControl.GetMode: TcaPageControlMode;
begin
  Result := FMode;
end;

procedure TcaPageControl.SetMode(Value: TcaPageControlMode);
begin
  if Value <> FMode then
    begin
      FMode := Value;
      UpdateMode;
    end;
end;

procedure TcaPageControl.SetHotSelected(const Value: Boolean);
begin
  if Value <> FHotSelected then
    begin
      FHotSelected := Value;
      Invalidate;
    end;
end;

procedure TcaPageControl.SetSelectedTabColor(const Value: TColor);
begin
  if Value <> FSelectedTabColor then
    begin
      FSelectedTabColor := Value;
      Invalidate;
    end;
end;

end.
