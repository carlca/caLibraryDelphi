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


unit caTranslateEdit;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  Messages,
  SysUtils,
  {$IFDEF D7_UP}
  Variants,
  {$ENDIF}
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Grids,
  StdCtrls,
  {$IFDEF D7_UP}
  ValEdit,
  {$ENDIF}
  caLog;

type

  {$IFDEF D5}

  //---------------------------------------------------------------------------
  // TcaEditTextForm                                                           
  //---------------------------------------------------------------------------

  TValueListEditor = class(TMemo)
  private
    // Property methods 
    function GetStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
  public
    // Properties 
    property Strings: TStrings read GetStrings write SetStrings;
  end;

  {$ENDIF}

  TcaEditTextForm = class(TForm)
  private
    // Private fields 
    FListEditor: TValueListEditor;
    // Property methods 
    function GetStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    // Properties 
    property Strings: TStrings read GetStrings write SetStrings;
  end;

var
  caEditTextForm: TcaEditTextForm;

implementation

{$R *.dfm}

  {$IFDEF D5}

  //---------------------------------------------------------------------------
  // TcaEditTextForm                                                           
  //---------------------------------------------------------------------------

function TValueListEditor.GetStrings: TStrings;
begin
  Result := inherited Lines;
end;

procedure TValueListEditor.SetStrings(const Value: TStrings);
begin
  inherited Lines := Value;
end;

  {$ENDIF}

  //---------------------------------------------------------------------------
  // TcaEditTextForm                                                           
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaEditTextForm.Create(AOwner: TComponent);
begin
  inherited;
  FListEditor := TValueListEditor.Create(Self);
  FListEditor.Align := alClient;
  FListEditor.Parent := Self;
end;

  // Property methods 

function TcaEditTextForm.GetStrings: TStrings;
begin
  Result := FListEditor.Strings;
end;

procedure TcaEditTextForm.SetStrings(const Value: TStrings);
begin
  FListEditor.Strings := Value;
end;

end.
