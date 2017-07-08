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


unit caMemory;

{$INCLUDE ca.inc}

interface

uses
  Windows, TypInfo, SysUtils, Classes, Forms;

function MemListAsXML: String;
function MemObject(Index: Integer): TObject;
function MemObjectCount: Integer;

implementation

var
  ObjList: array[0..65535] of Pointer;
  FirstFree: Integer = -1;

function GetClassAttributes(PItem: Pointer; var AObjAddr, AObjClass, AObjName, AObjSize, AObjUnit: ShortString): Boolean;
var
  IsClass: Boolean;
  IsOwnedComponent: Boolean;
  Item: TObject;
  PropInfo: PPropInfo;
  TypeData: PTypeData;
begin
  Item := TObject(PItem);
  try
    IsClass := PTypeInfo(Item.ClassInfo).Kind = tkClass;
    if IsClass then
      begin
        TypeData := GetTypeData(PTypeInfo(Item.ClassInfo));
        IsOwnedComponent := False;
        if Item is TComponent then
          IsOwnedComponent := TComponent(Item).Owner <> nil;
        if not IsOwnedComponent then
          begin
            PropInfo := GetPropInfo(PTypeInfo(Item.ClassInfo), 'Name');
            AObjAddr := IntToHex(Cardinal(PItem), 8);
            AObjName := 'NoName';
            if PropInfo <> nil then
              AObjName := GetStrProp(Item, PropInfo);
            AObjClass := PTypeInfo(Item.ClassInfo).Name;
            if AObjClass = 'TFont' then
              AObjName := 'NoName';
            AObjSize := IntToStr(TypeData.ClassType.InstanceSize);
            AObjUnit := TypeData.UnitName;
          end
        else
          IsClass := False;
      end;
  except
    IsClass := False;
  end;
  Result := IsClass;
end;

function MemListAsXML: String;
const
  CRLF = #13#10;
var
  Index: Integer;
  ObjAddr, ObjClass, ObjName, ObjSize, ObjUnit: ShortString;
begin
  Result := '<MemList>' + CRLF;
  for Index := 0 to FirstFree - 1 do
    begin
      if GetClassAttributes(ObjList[Index], ObjAddr, ObjClass, ObjName, ObjSize, ObjUnit) then
        begin
          Result := Result + '  <Object Name="' + ObjName + '" Class="' + ObjClass + '">' + CRLF;
          Result := Result + '    <Address>' + ObjAddr + '</Address>' + CRLF;
          Result := Result + '    <Size>' + ObjSize + '</Size>' + CRLF;
          Result := Result + '    <Unit>' + ObjUnit + '</Unit>' + CRLF;
          Result := Result + '  </Object>' + CRLF;
        end;
    end;
  Result := Result + '</MemList>' + CRLF;
end;

procedure WriteMemListToLogFile;
var
  LogFileName: ShortString;
  LogFile: TextFile;
begin
  LogFileName := ExtractFilePath(ParamStr(0));
  AssignFile(LogFile, LogFileName + 'MemList.xml');
  try
    Rewrite(LogFile);
    Write(LogFile, MemListAsXML);
  finally
    CloseFile(LogFile);
  end;
end;

function MemObject(Index: Integer): TObject;
begin
  Result := nil;
  if Index < FirstFree then
    Result := TObject(ObjList[Index]);
end;

function MemObjectCount: Integer;
begin
  Result := FirstFree;
end;

procedure AddPointer(P: Pointer);
begin
  if FirstFree >= Length(ObjList) then
    MessageBox(0, 'ObjectList is full', 'caMemory', MB_OK)
  else
    begin
      Inc(FirstFree);
      ObjList[FirstFree] := P;
    end;
end;

procedure DeletePointer(P: Pointer);
var
  Index: Integer;
begin
  for Index := 0 to FirstFree - 1 do
    begin
      if ObjList[Index] = P then
        begin
          Dec(FirstFree);
          Move(ObjList[Index + 1], ObjList[Index], (FirstFree - Index) * SizeOf(Pointer));
          Break;
        end;
    end;
end;

var
  DefaultMemoryManager: TMemoryManager;

function EnhGetMem(Size: Integer): Pointer;
begin
  Result := DefaultMemoryManager.GetMem(Size);
  AddPointer(Result);
end;

function EnhFreeMem(P: Pointer): Integer;
begin
  Result := DefaultMemoryManager.FreeMem(P);
  DeletePointer(P);
end;

function EnhReallocMem(P: Pointer; Size: Integer): Pointer;
begin
  Result := DefaultMemoryManager.ReallocMem(P, Size);
  DeletePointer(P);
  AddPointer(Result);
end;

var
  EnhancedMemoryManager: TMemoryManager =
   (GetMem: EnhGetMem;
    FreeMem: EnhFreeMem;
    ReallocMem: EnhReallocMem);

procedure InitializeMemoryManager;
begin
  GetMemoryManager(DefaultMemoryManager);
  SetMemoryManager(EnhancedMemoryManager);
end;

procedure FinalizeMemoryManager;
begin
  SetMemoryManager(DefaultMemoryManager);
  WriteMemListToLogFile;
end;

initialization
  InitializeMemoryManager;

finalization
  FinalizeMemoryManager;

end.
