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


unit caCompPalette;

{$INCLUDE ca.inc}

interface

uses
  Classes, SysUtils, Forms, ComCtrls;

type

  TcaCompPalette = class(TComponent)
  private
    FStyle: TTabStyle;
    FTabIndex: Integer;
    function FindDockedPalette: TTabControl;
    function FindFloatingPalette: TTabControl;
    function FindPalette: TTabControl;
    procedure SetStyle(const Value: TTabStyle);
    procedure SetTabIndex(const Value: Integer);
    procedure UpdateComponentPaletteStyle;
    procedure UpdateComponentPaletteTabIndex;
  published
    property Style: TTabStyle read FStyle write SetStyle;
    property TabIndex: Integer read FTabIndex write SetTabIndex;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TcaCompPalette]);
end;

{ TcaCompPalette }

function TcaCompPalette.FindDockedPalette: TTabControl;
var
  Index: Integer;
  MainForm: TForm;
begin
  Result := nil;
  MainForm := Application.MainForm;
  for Index := 0 to MainForm.ComponentCount - 1 do
    begin
      if MainForm.Components[Index].ClassName = 'TComponentPaletteTabControl' then
        begin
          Result := TTabControl(MainForm.Components[Index]);
          Break;
        end;
    end;
end;

function TcaCompPalette.FindFloatingPalette: TTabControl;
var
  Index: Integer;
  PaletteForm: TForm;
begin
  Result := nil;
  PaletteForm := nil;
  for Index := 0 to Screen.FormCount - 1 do
    begin
      if Screen.Forms[Index].Caption = 'Component Palette' then
        begin
          PaletteForm := Screen.Forms[Index];
          Break;
        end;
    end;
  if PaletteForm <> nil then
    begin
      for Index := 0 to PaletteForm.ComponentCount - 1 do
        begin
          if PaletteForm.Components[Index] is TTabControl then
            begin
              Result := TTabControl(PaletteForm.Components[Index]);
              Break;
            end;
        end;
    end;
end;

function TcaCompPalette.FindPalette: TTabControl;
begin
  Result := nil;
  if csDesigning in ComponentState then
    begin
      Result := FindFloatingPalette;
      if Result = nil then
        Result := FindDockedPalette;
    end;
end;

procedure TcaCompPalette.UpdateComponentPaletteStyle;
var
  Palette: TTabControl;
begin
  Palette := FindPalette;
  if Palette <> nil then
    Palette.Style := FStyle;
end;

procedure TcaCompPalette.UpdateComponentPaletteTabIndex;
var
  Palette: TTabControl;
begin
  Palette := FindPalette;
  if Palette <> nil then
    Palette.TabIndex := FTabIndex;
end;

procedure TcaCompPalette.SetStyle(const Value: TTabStyle);
begin
  if Value <> FStyle then
    begin
      FStyle := Value;
      UpdateComponentPaletteStyle;
    end;
end;

procedure TcaCompPalette.SetTabIndex(const Value: Integer);
begin
  if Value <> FTabIndex then
    begin
      FTabIndex := Value;
      UpdateComponentPaletteTabIndex;
    end;
end;

end.
