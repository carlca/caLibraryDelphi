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


unit caExcel;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Windows,
  SysUtils,
  Classes,
  Graphics,

  // ca units
  caTypes,
  caConsts,
  caClasses,
  caUtils;

type

 //---------------------------------------------------------------------------
 // IcaExcelFactory
 //---------------------------------------------------------------------------

  IcaExcelFactory = interface
  ['{0082C127-E383-4A92-8CA3-8529C14BBC78}']
    procedure AddHeaderColumn(const AColumnName: String);
    procedure AddRowColumn(const AColumnName, AColumnData: String);
    procedure SaveXLSToFile(const AFileName: String);
    procedure SaveXLSToStream(const AStream: TStream);
    procedure WriteHeader;
    procedure WriteRow;
  end;

 //---------------------------------------------------------------------------
 // TcaExcelFactory
 //---------------------------------------------------------------------------

  TcaExcelFactory = class(TcaStringList, IcaExcelFactory)
  private
    FHeaderColumns: IcaStringList;
    FRowColumns: IcaStringList;
    function Separator: Char;
    function Quote: Char;
  public
    constructor Create;
    procedure AddHeaderColumn(const AColumnName: String);
    procedure AddRowColumn(const AColumnName, AColumnData: String);
    procedure SaveXLSToFile(const AFileName: String);
    procedure SaveXLSToStream(const AStream: TStream);
    procedure WriteHeader;
    procedure WriteRow;
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaExcelFactory
 //---------------------------------------------------------------------------

constructor TcaExcelFactory.Create;
begin
  inherited;
  FHeaderColumns := TcaStringList.Create;
  FRowColumns := TcaStringList.Create;
end;

procedure TcaExcelFactory.AddHeaderColumn(const AColumnName: String);
begin
  if FHeaderColumns.IndexOf(AColumnName) = -1 then
    FHeaderColumns.Add(AColumnName);
end;

procedure TcaExcelFactory.AddRowColumn(const AColumnName, AColumnData: String);
var
  ColumnIndex: Integer;
begin
  ColumnIndex := FHeaderColumns.IndexOf(AColumnName);
  if ColumnIndex = -1 then
    raise EcaException.Create('Column does not exist');
  FRowColumns.Values[IntToStr(ColumnIndex)] := AColumnData;
end;

function TcaExcelFactory.Separator: Char;
begin
  Result := #9;
end;

function TcaExcelFactory.Quote: Char;
begin
  Result := '"';
end;

procedure TcaExcelFactory.SaveXLSToFile(const AFileName: String);
begin
  SaveToFile(AFileName);
end;

procedure TcaExcelFactory.SaveXLSToStream(const AStream: TStream);
begin
  SaveToStream(AStream);
end;

procedure TcaExcelFactory.WriteHeader;
var
  Index: Integer;
  Header: String;
begin
  Header := '';
  for Index := 0 to FHeaderColumns.Count - 1 do
    Header := Header + Quote + FHeaderColumns[Index] + Quote + Separator;
  Utils.DeleteFromEnd(Header, 1);
  Add(Header);
end;

procedure TcaExcelFactory.WriteRow;
var
  Index: Integer;
  Row: String;
  RowColumn: String;
begin
  Row := '';
  for Index := 0 to FHeaderColumns.Count - 1 do
    begin
      RowColumn := FRowColumns.Values[IntToStr(Index)];
      if not Utils.IsNumeric(RowColumn) then
        RowColumn := Quote + RowColumn + Quote;
      Row := Row + RowColumn + Separator;
    end;
  Utils.DeleteFromEnd(Row, 1);
  Add(Row);
  FRowColumns.Clear;
end;

end.
