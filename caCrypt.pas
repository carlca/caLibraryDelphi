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


unit caCrypt;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Classes,
  SysUtils;

type

 //---------------------------------------------------------------------------
 // IcaCrypt
 //---------------------------------------------------------------------------

  IcaCrypt = interface
  ['{01BB5DD9-AD0F-4451-A98F-0A23DF500172}']
    // Public methods
    function Encrypt(const S: String; const StartKey: Word): String;
    function Decrypt(const S: String; const StartKey: Word): String;
  end;

 //---------------------------------------------------------------------------
 // TcaCrypt
 //---------------------------------------------------------------------------

  TcaCrypt = class(TInterfacedObject, IcaCrypt)
  private
    FKey1: Word;
    FKey2: Word;
  public
    constructor Create(const AKey1, AKey2: Word);
    // Public methods
    function Encrypt(const S: String; const StartKey: Word): String;
    function Decrypt(const S: String; const StartKey: Word): String;
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaCrypt
 //---------------------------------------------------------------------------

constructor TcaCrypt.Create(const AKey1, AKey2: Word);
begin
  inherited Create;
  FKey1 := AKey1;
  FKey2 := AKey2;
end;

function TcaCrypt.Decrypt(const S: String; const StartKey: Word): String;
var
  B: Byte;
  Index: Integer;
  Key: Word;
begin
  B := 0;
  Key := StartKey;
  Result := '';
  for Index := 1 to Length(S) div 2 do
    begin
      try
        B :=  StrToInt('$' + Copy(S, 2 * Index - 1, 2));
      except
        on EConvertError do B := 0
      end;
      Result := Result + Char(B xor (Key shr 8));
      Key := (B + Key) * FKey1 + FKey2;
    end
end;

function TcaCrypt.Encrypt(const S: String; const StartKey: Word): String;
var
  B: Byte;
  Index: Integer;
  Key: Word;
begin
  Key := StartKey;
  Result := '';
  for Index := 1 to Length(S) do
    begin
      B := Byte(S[Index]) xor (Key shr 8);
      Key := (B + Key) * FKey1 + FKey2;
      Result := Result + IntToHex(B, 2)
    end
end;

end.
