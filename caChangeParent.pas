unit caChangeParent;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units

  Windows,
  Classes,
  Controls;

type

  //---------------------------------------------------------------------------
  // TcaChangeParent
  //---------------------------------------------------------------------------

  TcaChangeParent = class(TComponent)
  private
    // Property fields
    FActivate: Boolean;
    FBringToFront: Boolean;
    FControlToChange: TControl;
    FNewParent: TWinControl;
    FNewParentIsForm: Boolean;
    FSendToBack: Boolean;
    // Property methods
    procedure SetActivate(const Value: Boolean);
    procedure SetBringToFront(const Value: Boolean);
    procedure SetNewParentIsForm(const Value: Boolean);
    procedure SetSendToBack(const Value: Boolean);
  protected
    // Protected methods
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    // Properties
    property Activate: Boolean read FActivate write SetActivate;
    property BringToFront: Boolean read FBringToFront write SetBringToFront;
    property ControlToChange: TControl read FControlToChange write FControlToChange;
    property NewParent: TWinControl read FNewParent write FNewParent;
    property NewParentIsForm: Boolean read FNewParentIsForm write SetNewParentIsForm;
    property SendToBack: Boolean read FSendToBack write SetSendToBack;
  end;

implementation

  //---------------------------------------------------------------------------
  // Protected methods
  //---------------------------------------------------------------------------

procedure TcaChangeParent.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
    begin
      if (AComponent = FControlToChange) then
        FControlToChange := nil;
      if (AComponent = FNewParent) then
        FControlToChange := nil;
    end;
end;

  //---------------------------------------------------------------------------
  // Property methods
  //---------------------------------------------------------------------------

procedure TcaChangeParent.SetActivate(const Value: Boolean);
begin
  if Value and (FControlToChange <> nil) and (FNewParent <> nil) then
    begin
      FNewParent.HandleNeeded;
      FControlToChange.Parent := FNewParent;
      FControlToChange := nil;
      FNewParent := nil;
      FNewParentIsForm := False;
    end;
end;

procedure TcaChangeParent.SetBringToFront(const Value: Boolean);
begin
  if Value and (FControlToChange <> nil) then
    FControlToChange.BringToFront;
end;

procedure TcaChangeParent.SetNewParentIsForm(const Value: Boolean);
begin
  if Value <> FNewParentIsForm then
    begin
      FNewParentIsForm := Value;
      if FNewParentIsForm and Assigned(Owner) and (Owner is TWinControl) then
        FNewParent := TWinControl(Owner);
    end;
end;

procedure TcaChangeParent.SetSendToBack(const Value: Boolean);
begin
  if Value and (FControlToChange <> nil) then
    FControlToChange.SendToBack;
end;

end.
