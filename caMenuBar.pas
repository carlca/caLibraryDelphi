unit caMenuBar;

{$INCLUDE ca.inc}

interface

uses

  Windows,
  Messages,
  Classes,
  Controls,
  ComCtrls,
  Menus;

type

 //---------------------------------------------------------------------------
 // TcaMenuBar
 //---------------------------------------------------------------------------

  TcaMenuBar = class(TToolBar)
  private
    FMenu: TMainMenu;
    function HandleAppKeyDown(var Message: TWMKey): Boolean;
    procedure CMDialogKey(var Message: TWMKey); message CM_DIALOGKEY;
    procedure SetMenu(const Value: TMainMenu);
    procedure UpdateMenuItems;
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property EdgeBorders default [];
    property Flat default True;
    property Menu: TMainMenu read FMenu write SetMenu;
    property ShowCaptions default True;
  end;

implementation

uses
  SysUtils,
  Forms;

 //---------------------------------------------------------------------------
 // TcaMenuBar
 //---------------------------------------------------------------------------

procedure TcaMenuBar.CMDialogKey(var Message: TWMKey);
begin
  if not HandleAppKeyDown(Message) then Inherited;
end;

constructor TcaMenuBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Flat := True;
  ShowCaptions := True;
  EdgeBorders := [];
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks, csMenuEvents, csSetCaption];
end;

procedure TcaMenuBar.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

function TcaMenuBar.HandleAppKeyDown(var Message: TWMKey): Boolean;
begin
  if Assigned(Menu) and Menu.IsShortCut(Message) then
    begin
      Message.Result := 1;
      Result := True
    end
  else
    Result := False;
end;

procedure TcaMenuBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FMenu) and (Operation = opRemove) then SetMenu(nil);
end;

procedure TcaMenuBar.SetMenu(const Value: TMainMenu);
begin
  if Value <> FMenu then
    begin
      FMenu := Value;
      if FMenu <> nil then UpdateMenuItems;
    end;
end;

procedure TcaMenuBar.UpdateMenuItems;
var
  Index: Integer;
  Button: TToolButton;
begin
  if ButtonCount > 0 then
    for Index := Pred(ButtonCount) downto 0 do
      Buttons[Index].Free;
  if not Assigned(FMenu) then exit;
  for Index := ButtonCount to Pred(FMenu.Items.Count) do
    begin
      Button := TToolButton.Create(Self);
      try
        Button.AutoSize := True;
        Button.Grouped := True;
        Button.Parent := Self;
        Buttons[Index].MenuItem := FMenu.Items[Index];
      except
        Button.Free;
        raise;
      end;
    end;
  for Index := 0 to Pred(FMenu.Items.Count) do
    Buttons[Index].MenuItem := FMenu.Items[Index];
end;

end.


