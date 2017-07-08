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


unit caListView;

interface

uses

  // Standard Delphi units 
  Windows,
  SysUtils,
  Classes,
  Controls,
  ComCtrls;

type

  //---------------------------------------------------------------------------
  // TcaListView                                                               
  //---------------------------------------------------------------------------

  TcaListView = class(TListView)
  private
    // Property fields 
    FAutoResizeColumns: Boolean;
    FColumnCount: Integer;
    // Property methods 
    function GetColumnCount: Integer;
    procedure SetAutoResizeColumns(const Value: Boolean);
    procedure SetColumnCount(const Value: Integer);
    // Private methods 
    procedure ResizeColumns;
    procedure UpdateColumns;
  protected
    // Protected methods 
    procedure CreateWnd; override;
    procedure Resize; override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
  published
    // Published properties 
    property AutoResizeColumns: Boolean read FAutoResizeColumns write SetAutoResizeColumns;

    property ColumnCount: Integer read GetColumnCount write SetColumnCount;
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaListView                                                               
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaListView.Create(AOwner: TComponent);
begin
  inherited;
  ViewStyle := vsReport;
end;

  // Protected methods 

procedure TcaListView.CreateWnd;
begin
  inherited;
  UpdateColumns;
end;

procedure TcaListView.Resize;
begin
  inherited;
  if FAutoResizeColumns then ResizeColumns;
end;

  // Private methods 

procedure TcaListView.ResizeColumns;
var
  AvailWidth: Integer;
  Index: Integer;
begin
  inherited;
  AvailWidth := ClientWidth - GetSystemMetrics(SM_CXVSCROLL);
  for Index := 0 to Pred(Columns.Count) do
    Columns[Index].Width := AvailWidth div Columns.Count;
end;

procedure TcaListView.UpdateColumns;
var
  Index: Integer;
begin
  Columns.Clear;
  for Index := 0 to Pred(FColumnCount) do
    Columns.Add;
end;

  // Properties methods 

function TcaListView.GetColumnCount: Integer;
begin
  Result := Columns.Count;
end;

procedure TcaListView.SetAutoResizeColumns(const Value: Boolean);
begin
  if Value <> FAutoResizeColumns then
    begin
      FAutoResizeColumns := Value;
      Resize;
    end;
end;

procedure TcaListView.SetColumnCount(const Value: Integer);
begin
  if Value <> FColumnCount then
    begin
      FColumnCount := Value;
      if HandleAllocated then
        UpdateColumns;
    end;
end;

end.
