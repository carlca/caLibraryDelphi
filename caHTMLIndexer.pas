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


unit caHTMLIndexer;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Windows,
  SysUtils,
  Classes,
  Contnrs,

  // ca units
  caClasses,
  caUtils,
  caHTMLParser;

type

 //---------------------------------------------------------------------------
 // TcaHTMLIndexWordList
 //---------------------------------------------------------------------------

  TcaHTMLIndexWordReference = class;

  TcaHTMLIndexWordList = class(TObject)
  private
    // Property fields
    FList: TObjectList;
    // Property methods
    function GetCount: Integer;
    function GetItem(Index: Integer): TcaHTMLIndexWordReference;
  public
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcaHTMLIndexWordReference read GetItem;
  end;

 //---------------------------------------------------------------------------
 // TcaHTMLIndexWordReference
 //---------------------------------------------------------------------------

  TcaHTMLIndexWordReference = class(TObject)
  private
    // Property fields
    FColumn: Integer;
    FFileName: string;
    FLineNumber: Integer;
  public
    // Properties
    property Column: Integer read FColumn write FColumn;
    property FileName: string read FFileName write FFileName;
    property LineNumber: Integer read FLineNumber write FLineNumber;
  end;

 //---------------------------------------------------------------------------
 // IcaHTMLIndexer
 //---------------------------------------------------------------------------

  IcaHTMLIndexer = interface
  ['{4901F594-1A0E-4150-8156-573AD2A83084}']
    // Property methods
    function GetFileList: TStrings;
    function GetFolder: string;
    procedure setFileList(const Value: TStrings);
    procedure SetFolder(const Value: string);
    // Properties
    property FileList: TStrings read GetFileList write setFileList;
    property Folder: string read GetFolder write SetFolder;
  end;

 //---------------------------------------------------------------------------
 // TcaHTMLIndexer
 //---------------------------------------------------------------------------

  TcaHTMLIndexer = class(TcaInterfacedPersistent, IcaHTMLIndexer)
  private
    FFileList: TStrings;
    FFolder: string;
  protected
    // Property methods
    function GetFileList: TStrings;
    function GetFolder: string;
    procedure SetFileList(const Value: TStrings);
    procedure SetFolder(const Value: string);
    // Private methods
    procedure UpdateFileList;
  public
    // Public methods
    // function Add:
    // Properties
    property FileList: TStrings read GetFileList write setFileList;
    property Folder: string read GetFolder write SetFolder;
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaHTMLIndexWordList
 //---------------------------------------------------------------------------

function TcaHTMLIndexWordList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcaHTMLIndexWordList.GetItem(Index: Integer): TcaHTMLIndexWordReference;
begin
  Result := TcaHTMLIndexWordReference(FList[Index]);
end;

 //---------------------------------------------------------------------------
 // TcaHTMLIndexer
 //---------------------------------------------------------------------------

 // Private methods

procedure TcaHTMLIndexer.UpdateFileList;
begin
  Utils.GetFileList(FFolder, '*.htm*', FFileList);
end;

 // Property methods

function TcaHTMLIndexer.GetFileList: TStrings;
begin
  Result := FFileList;
end;

function TcaHTMLIndexer.GetFolder: string;
begin
  Result := FFolder;
end;

procedure TcaHTMLIndexer.SetFileList(const Value: TStrings);
begin
  FFileList.Assign(Value);
end;

procedure TcaHTMLIndexer.SetFolder(const Value: string);
begin
  FFolder := Value;
  UpdateFileList;
end;

end.
