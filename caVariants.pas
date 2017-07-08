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


unit caVariants;

{$INCLUDE ca.inc}

interface

uses

  Windows,
  SysUtils,
  Classes;

type

 //---------------------------------------------------------------------------
 // TcaVarArray
 //---------------------------------------------------------------------------

  TcaVarArray = class(TObject)
  private
    // Private fields
    FArray: Variant;
    FCapacity: Integer;
    FCount: Integer;
    // Private methods
    procedure Grow;
    procedure ReDim;
    // Property methods
    function GetItem(Index: Integer): Variant;
    procedure SetItem(Index: Integer; const Value: Variant);
  public
    constructor Create;
    // Public methods
    function Add(AItem: Variant): Integer;
    // Properties
    property Count: Integer read FCount;
    property Items[Index: Integer]: Variant read GetItem write SetItem; default;
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaVarArray
 //---------------------------------------------------------------------------

constructor TcaVarArray.Create;
begin
  inherited;
  FCapacity := 100;
  FArray := VarArrayCreate([0, 0], varVariant);
  ReDim;
end;

 // Public methods

function TcaVarArray.Add(AItem: Variant): Integer;
begin
  Inc(FCount);
  if FCount = FCapacity then Grow;
  Result := FCount - 1;
  FArray[Result] := AItem;
end;

 // Private methods

procedure TcaVarArray.Grow;
begin
  Inc(FCapacity, 100);
  ReDim;
end;

procedure TcaVarArray.ReDim;
begin
  VarArrayRedim(FArray, FCapacity);
end;

 // Property methods

function TcaVarArray.GetItem(Index: Integer): Variant;
begin
  Result := FArray[Index];
end;

procedure TcaVarArray.SetItem(Index: Integer; const Value: Variant);
begin
  FArray[Index] := Value;
end;

end.
