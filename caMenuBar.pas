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


