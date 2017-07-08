unit caFormScaler;

interface

uses

  // standard Delphi units 

  Windows,
  Classes,
  Forms,
  Controls,
  Math,

  // ca units 

  caLog,
  caClasses;

type

  //---------------------------------------------------------------------------
  // TcaFormScaler                                                             
  //---------------------------------------------------------------------------

  TcaFormScaler = class(TComponent)
  private
    // property fields 
    FHeightOffset: Integer;
    FWidthOffset: Integer;
    FBottomGap: Integer;
    FRightGap: Integer;
    // property methods 
    procedure SetBottomGap(const Value: Integer);
    procedure SetRightGap(const Value: Integer);
    // private methods 
    procedure CheckUpdate(AOwner: TComponent);
    procedure Update(AOwnerForm: TCustomForm);
  protected
    // protected methods 
    procedure Loaded; override;
  public
    // lifetime 
    constructor Create(AOwner: TComponent); override;
  published
    // published properties 
    property BottomGap: Integer read FBottomGap write SetBottomGap default 8;
    property RightGap: Integer read FRightGap write SetRightGap default 8;
  end;

implementation

uses Types;

  //---------------------------------------------------------------------------
  // TcaFormScaler                                                             
  //---------------------------------------------------------------------------

  // lifetime 

constructor TcaFormScaler.Create(AOwner: TComponent);
begin
  inherited;
  FWidthOffset := 8;
  FHeightOffset := 9;
  FBottomGap := 8;
  FRightGap := 8;
end;

  // protected methods 

procedure TcaFormScaler.Loaded;
begin
  inherited;
  CheckUpdate(Owner);
end;

  // private methods 

procedure TcaFormScaler.CheckUpdate(AOwner: TComponent);
begin
  if Assigned(AOwner) and (not (csDesigning in AOwner.ComponentState)) and (AOwner is TCustomForm) then
    Update(TCustomForm(AOwner));
end;

type
  TCustomFormEx = class(TCustomForm);

procedure TcaFormScaler.Update(AOwnerForm: TCustomForm);
var
  CaptionHeight: Integer;
  Control: TControl;
  Controls: TList;
  Index: Integer;
  MaxRight: Integer;
  MaxBottom: Integer;

  function Bottom(AOwnerForm: TCustomForm; AControl: TControl): Integer;
  var
    Control: TControl;
  begin
    Result := AControl.Top + AControl.Height - 1;
    Control := AControl.Parent;
    while Assigned(Control) and (not (Control = AOwnerForm)) do
      begin
        Result := Result + Control.Top;
        Control := Control.Parent;
      end;
  end;

  function Right(AOwnerForm: TCustomForm; AControl: TControl): Integer;
  var
    Control: TControl;
  begin
    Result := AControl.Left + AControl.Width - 1;
    Control := AControl.Parent;
    while Assigned(Control) and (not (Control = AOwnerForm)) do
      begin
        Result := Result + Control.Left;
        Control := Control.Parent;
      end;
  end;

begin
  // gather controls 
  Controls := Auto(TList.Create).Instance;
  for Index := 0 to Pred(AOwnerForm.ComponentCount) do
    if AOwnerForm.Components[Index] is TControl then
      Controls.Add(AOwnerForm.Components[Index]);
  // calculate rightmost and bottommost control locations 
  MaxRight := 0;
  MaxBottom := 0;
  for Index := 0 to Pred(Controls.Count) do
    begin
      Control := TControl(Controls[Index]);
      MaxRight := Max(MaxRight, Right(AOwnerForm, Control));
      MaxBottom := Max(MaxBottom, Bottom(AOwnerForm, Control));
    end;
  // resize form to exceed max locations by gap amounts 
  TCustomFormEx(AOwnerForm).Width := MaxRight + FRightGap + FWidthOffset;
  CaptionHeight := GetSystemMetrics(SM_CYCAPTION);
  TCustomFormEx(AOwnerForm).Height := MaxBottom + FBottomGap + CaptionHeight + FHeightOffset;
end;

  // property methods 

procedure TcaFormScaler.SetBottomGap(const Value: Integer);
begin
  if (Value <> FBottomGap) and (Value >= 0) then
    begin
      FBottomGap := Value;
      CheckUpdate(Owner);
    end;
end;

procedure TcaFormScaler.SetRightGap(const Value: Integer);
begin
  if (Value <> FRightGap) and (Value >= 0) then
    begin
      FRightGap := Value;
      CheckUpdate(Owner);
    end;
end;

end.
