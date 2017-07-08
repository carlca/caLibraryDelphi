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


unit caMath;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Classes,
  Sysutils,

  // ca units
  caClasses;

type

 //---------------------------------------------------------------------------
 // IcaValidNum
 //---------------------------------------------------------------------------

  IcaValidNumType = (vtInteger, vtFloat);

  IcaValidNum = interface
  ['{EF66F544-E5E5-438F-9C4B-5140E94AB183}']
    function GetIsValid: Boolean;
    function GetNumType: IcaValidNumType;
    function GetNumString: String;
    procedure SetNumType(const Value: IcaValidNumType);
    procedure SetNumString(const Value: String);
    property IsValid: Boolean read GetIsValid;
    property NumType: IcaValidNumType read GetNumType write SetNumType;
    property NumString: String read GetNumString write SetNumString;
  end;

 //---------------------------------------------------------------------------
 // TcaValidNum
 //---------------------------------------------------------------------------

  TcaValidNum = class(TInterfacedObject, IcaValidNum)
  private
    FIsValid: Boolean;
    FNumString: String;
    FNumType: IcaValidNumType;
    FValidChars: set of Char;
    FValidated: Boolean;
    function GetIsValid: Boolean;
    function GetNumString: String;
    function GetNumType: IcaValidNumType;
    procedure SetNumString(const Value: String);
    procedure SetNumType(const Value: IcaValidNumType);
    procedure UpdateValidChars;
  public
    constructor Create(ANumType: IcaValidNumType); 
    property IsValid: Boolean read GetIsValid;
    property NumType: IcaValidNumType read GetNumType write SetNumType;
    property NumString: String read GetNumString write SetNumString;
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaValidNum
 //---------------------------------------------------------------------------

constructor TcaValidNum.Create(ANumType: IcaValidNumType);
begin
  inherited Create;
  SetNumType(ANumType);
end;

function TcaValidNum.GetIsValid: Boolean;
var
  Index: Integer;
begin
  if FValidated then
    Result := FIsValid
  else
    begin
      Result := True;
      for Index := 1 to Length(FNumString) do
        if not (FNumString[Index] in FValidChars) then
          begin
            FIsValid := False;
            Result := FIsValid;
            FValidated := True;
            Break;
          end;
    end;
end;

function TcaValidNum.GetNumType: IcaValidNumType;
begin
  Result := FNumType;
end;

function TcaValidNum.GetNumString: String;
begin
  Result := FNumString;
end;

procedure TcaValidNum.SetNumType(const Value: IcaValidNumType);
begin
  if Value <> FNumType then
    begin
      FNumType := Value;
      UpdateValidChars;
    end;
end;

procedure TcaValidNum.SetNumString(const Value: String);
begin
  if Value <> FNumString then
    begin
      FNumString := Value;
      FValidated := False;
    end;
end;

procedure TcaValidNum.UpdateValidChars;
begin
  case FNumType of
    vtInteger:  FValidChars := ['+', '-', '0'..'9'];
    vtFloat:    FValidChars := [DecimalSeparator, '+', '-', '0'..'9', 'E', 'e'];
  end;
end;

end.
