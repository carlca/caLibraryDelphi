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
