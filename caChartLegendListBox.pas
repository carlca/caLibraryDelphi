unit caChartLegendListBox;

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
  StdCtrls,
  CheckLst;

type

  //----------------------------------------------------------------------------
  // TcaChartLegendListBox                                                      
  //----------------------------------------------------------------------------

  TcaChartLegendListBox = class(TCheckListBox)
  private
    // Private fields 
    FLineColors: TList;
    // Property methods 
    function GetCurrentText: String;
    function GetLineColor(Index: Integer): TColor;
    procedure SetLineColor(Index: Integer; const Value: TColor);
    // Private methods 
    procedure CheckLineColorsList;
  protected
    // Protected methods 
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure DblClick; override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
    procedure CheckSelectedItems;
    procedure SelectAllItems;
    procedure UnCheckSelectedItems;
    procedure UnSelectAllItems;
    procedure UnCheckNonHighlightedItems;
    procedure UnHighlightAll;
    procedure SingleSelect;
    // Properties 
    property CurrentText: String read GetCurrentText;
    property LineColor[Index: Integer]: TColor read GetLineColor write SetLineColor;
  end;

implementation

  //----------------------------------------------------------------------------
  // TcaChartLegendListBox                                                      
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaChartLegendListBox.Create(Owner: TComponent);
begin
  inherited;
  Style := lbOwnerDrawFixed;
  ItemHeight := 15;
  MultiSelect := True;
  FLineColors := TList.Create;
end;

destructor TcaChartLegendListBox.Destroy;
begin
  FLineColors.Free;
  inherited;
end;

  // Public methods 

procedure TcaChartLegendListBox.CheckSelectedItems;
var
  Index: Integer;
begin
  for Index := 0 to Items.Count - 1 do
    if ItemEnabled[Index] and Selected[Index] then
      Checked[Index] := True;
end;

procedure TcaChartLegendListBox.SelectAllItems;
var
  Index: Integer;
begin
  for Index := 0 to Items.Count - 1 do
    Selected[Index] := True;
end;

procedure TcaChartLegendListBox.UnCheckSelectedItems;
var
  Index: Integer;
begin
  for Index := 0 to Items.Count - 1 do
    if ItemEnabled[Index] and Selected[Index] then
      Checked[Index] := False;
end;

procedure TcaChartLegendListBox.UnSelectAllItems;
var
  Index: Integer;
begin
  for Index := 0 to Items.Count - 1 do
    Selected[Index] := False;
end;

  // Protected methods 

procedure TcaChartLegendListBox.DblClick;
begin
  if (ItemIndex >= 0) and ItemEnabled[ItemIndex] then
    begin
      Checked[ItemIndex] := not Checked[ItemIndex];
      ClickCheck;
    end;
end;

procedure TcaChartLegendListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  H, X0, X1, Y: Integer;
const
  Offset = 20;
begin
  inherited;
  H := Rect.Bottom - Rect.Top;
  Y := Rect.Top + H div 2;
  X0 := Rect.Left + 2;
  X1 := X0 + Offset;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Classes.Rect(Rect.Left, Rect.Top, X1, Rect.Bottom));
  if ItemEnabled[Index] then
    begin
      Canvas.Pen.Color := GetLineColor(Index);
      Canvas.MoveTo(X0, Y);
      Canvas.LineTo(X1 - 2, Y);
      Canvas.MoveTo(X0, Y + 1);
      Canvas.LineTo(X1 - 2, Y + 1);
      Canvas.MoveTo(X0, Y + 2);
      Canvas.LineTo(X1 - 2, Y + 2);
    end;
  Rect.Left := X1;
  if odSelected in State then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText;
    end
  else
    begin
      Canvas.Brush.Color := Color;
      if ItemEnabled[Index] then
        Canvas.Font.Color := Font.Color
      else
        Canvas.Font.Color := clBtnShadow;
    end;
  Canvas.FillRect(Rect);
  Canvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index]);
end;

  // Private methods 

procedure TcaChartLegendListBox.CheckLineColorsList;
begin
  while FLineColors.Count < Items.Count do
    FLineColors.Add(nil);
  while FLineColors.Count > Items.Count do
    FLineColors.Delete(Pred(FLineColors.Count));
end;

  // Property methods 

function TcaChartLegendListBox.GetCurrentText: String;
begin
  Result := '';
  if ItemIndex >= 0 then Result := Items[ItemIndex];
end;

function TcaChartLegendListBox.GetLineColor(Index: Integer): TColor;
begin
  CheckLineColorsList;
  Result := clNone;
  if (Index >= 0) and (Index < FLineColors.Count) then
    Result := TColor(FLineColors[Index]);
end;

procedure TcaChartLegendListBox.SetLineColor(Index: Integer; const Value: TColor);
begin
  CheckLineColorsList;
  if (Index >= 0) and (Index < FLineColors.Count) then
    begin
      FLineColors[Index] := Pointer(Value);
      Invalidate;
    end;
end;

procedure TcaChartLegendListBox.SingleSelect;
var
  Index: Integer;
  Found: Boolean;
begin
  Found := False;
  for Index := 0 to Items.Count - 1 do
    if ItemEnabled[Index] and Selected[Index] then
      if Found = True then
        Selected[Index] := False
      else
        Found := True;  
end;

procedure TcaChartLegendListBox.UnCheckNonHighlightedItems;
var
  Index: Integer;
begin
  for Index := 0 to Items.Count - 1 do
    if (ItemEnabled[Index]) and (Selected[Index] = False) then
      Checked[Index] := False;
end;

procedure TcaChartLegendListBox.UnHighlightAll;
var
  Index: Integer;
begin
  for Index := 0 to Items.Count - 1 do
    if (ItemEnabled[Index]) then
      Selected[Index] := False;
end;

end.


