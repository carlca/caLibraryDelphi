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


unit caStackObjects;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  SysUtils,
  Classes,

  // ca units
  caClasses,
  caUtils;

type

 //---------------------------------------------------------------------------
 // TcaStackInteger
 //---------------------------------------------------------------------------

  TcaStackInteger = class
  private
    // Property fields
    FIndex: Integer;
    FValue: Integer;
    // Property methods
    function GetAsString: String;
    procedure SetAsString(const Value: String);
  protected
    // Protected virtual methods
    procedure DoCreate(AValue : Integer ); virtual;
    procedure DoDestroy; virtual;
  public
    constructor Create(AValue: Integer);
    destructor Destroy; override;
    // Public class methods
    class function CreateObject(AValue: Integer): TcaStackInteger;
    class procedure FreeObject;
    // Properties
    property AsString: String read GetAsString write SetAsString;
    property Index: Integer read FIndex;
    property Value: Integer read FValue write FValue;
  end;

  TcaStackIntegerRec = record
    _VMT: TClass;
    _FValue : Integer;
  end;

var
  caStackIntegerRec: TcaStackIntegerRec;

implementation

 //---------------------------------------------------------------------------
 // TcaStackInteger
 //---------------------------------------------------------------------------

constructor TcaStackInteger.Create(AValue: Integer);
begin
  DoCreate(AValue);
end;

destructor TcaStackInteger.Destroy;
begin
  DoDestroy;
end;

 // Public class methods

class function TcaStackInteger.CreateObject(AValue: Integer): TcaStackInteger;
begin
  InitInstance(@caStackIntegerRec);
  Result := TcaStackInteger(@caStackIntegerRec);
  try
    Result.DoCreate(AValue);
    Result.AfterConstruction;
  except
    Result.DoDestroy;
    Result.CleanupInstance;
    raise;
  end;
end;

class procedure TcaStackInteger.FreeObject;
var
  Obj: TcaStackInteger;
begin
  Obj := TcaStackInteger(@caStackIntegerRec);
  Obj.BeforeDestruction;
  Obj.DoDestroy;
  Obj.CleanupInstance;
end;

 // Protected virtual methods

procedure TcaStackInteger.DoCreate(AValue: Integer);
begin
  FValue := AValue;
end;

procedure TcaStackInteger.DoDestroy;
begin
  // Nada
end;

 // Property methods

function TcaStackInteger.GetAsString: String;
begin
  Result := Utils.IntegerToString(FValue, '');
end;

procedure TcaStackInteger.SetAsString(const Value: String);
begin
  FValue := Utils.StringToInteger(Value);
end;

end.
