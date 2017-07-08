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


unit caTextStoreDsgn;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Classes,
  Sysutils,
  Controls,

  {$IFDEF D7_UP}
  DesignIntf,
  DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}

  // ca units 
  caLog,
  caForms,
  caTextStore;

type

  //---------------------------------------------------------------------------
  // TcaTextStoreComponentEditor                                               
  //---------------------------------------------------------------------------

  TcaTextStoreComponentEditor = class(TComponentEditor)
  public
    // Overridden methods 
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  caTextStoreEdit;

  //---------------------------------------------------------------------------
  // TcaTextStoreComponentEditor                                               
  //---------------------------------------------------------------------------

procedure TcaTextStoreComponentEditor.ExecuteVerb(Index: Integer);
var
  Editor: TcaEditTextForm;
  TextStore: TcaTextStore;
  OldStrings: string;
begin
  TextStore := TcaTextStore(Component);
  Editor := TcaEditTextForm.Create(nil);
  try
    OldStrings := TextStore.Strings.Text;
    Editor.Strings := TextStore.Strings;
    if Editor.ShowModal = mrOK then
      begin
        TextStore.Strings := Editor.Strings;
        if TextStore.Strings.Text <> OldStrings then
          Designer.Modified;
      end;
    Log.Send('Editor.Strings', Editor.Strings);
    Log.Send('OldStrings', OldStrings);
  finally
    Editor.Free;
  end;    
end;

function TcaTextStoreComponentEditor.GetVerb(Index: Integer): String;
begin
  Result := 'Edit...';
end;

function TcaTextStoreComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
