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


unit caRadioButton;

interface

uses

  // standard Delphi units...
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  StdCtrls,
  Forms;

type

  //---------------------------------------------------------------------------
  // TcaRadioButton
  //---------------------------------------------------------------------------

  TcaRadioButton = class(TRadioButton)
  private
    // private members...
    FLabel: TLabel;
    // private methods...
    procedure CreateLabel;
    procedure UpdateLabel;
    procedure UpdateRegion;
    // event handlers...
    procedure LabelClickEventHandler(Sender: TObject);
  protected
    // overridden protected methods...
    procedure CreateWnd; override;
  public
    // lifetime...
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

//---------------------------------------------------------------------------
// TcaRadioButton
//---------------------------------------------------------------------------

// lifetime...

constructor TcaRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateLabel;
end;

destructor TcaRadioButton.Destroy;
begin
  FreeAndNil(FLabel);
  inherited;
end;

// overridden protected methods...

procedure TcaRadioButton.CreateWnd;
begin
  inherited CreateWnd;
  if not (csDesigning in ComponentState) then
    begin
      UpdateLabel;
      UpdateRegion;
    end;
end;

// private methods...

procedure TcaRadioButton.CreateLabel;
begin
  FLabel := TLabel.Create(Owner);
  FLabel.Transparent := True;
  FLabel.Caption := '';
  FLabel.OnClick := LabelClickEventHandler;
end;

procedure TcaRadioButton.UpdateLabel;
begin
  if FLabel.Caption = '' then
    begin
      FLabel.Caption := Caption;
      Caption := '';
      FLabel.Parent := Parent;
      FLabel.Top := Top + 1;
      FLabel.Left := Left + 20;
      FLabel.Visible := True;
    end;
end;

procedure TcaRadioButton.UpdateRegion;
var
  Region: HRGN;
  HalfHeight: Integer;
begin
  HalfHeight := (Height + 1) div 2;
  Region := CreateEllipticRgn(-1, HalfHeight - 7, 16, HalfHeight + 7);
  SetWindowRgn(Handle, Region, True);
end;

// event handlers...

procedure TcaRadioButton.LabelClickEventHandler(Sender: TObject);
begin
  Checked := True;
end;

end.

