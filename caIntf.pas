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


unit caIntf;

interface

uses

  // Standard Delphi units 
  Classes,
  Sysutils,
  // sIntfInfo,
  // Invoker,
  TypInfo,

  // ca units 
  caClasses,
  caLog;

type

  //----------------------------------------------------------------------------
  // TcaIntfMethod                                                              
  //----------------------------------------------------------------------------

  TcaIntfMethod = class(TObject)
  private
  protected
  public
    // constructor Create(const AMetaData: TIntfMetaData);
  end;

  //----------------------------------------------------------------------------
  // IcaIntfInfo                                                                
  //----------------------------------------------------------------------------

  IcaIntfInfo = interface
  ['{9DB26BE5-1EFA-4CDD-944E-715A7AFDF1F9}']
    // Interface methods 
    procedure GetMethodsAsStrings(AStrings: TStrings);
  end;

  //----------------------------------------------------------------------------
  // TcaIntfInfo                                                                
  //----------------------------------------------------------------------------

  TcaIntfInfo = class(TInterfacedObject, IcaIntfInfo)
  private
    // Private fields 
//    FIntf: IUnknown;
//    FMetaData: TIntfMetaData;
  protected
  public
    // Create/Destroy 
    // constructor Create(const AIntf: IUnknown; const ATypeInfo: PTypeInfo);
    // Interface methods - IcaIntfInfo 
    procedure GetMethodsAsStrings(AStrings: TStrings);
  end;

implementation

  //----------------------------------------------------------------------------
  // TcaIntfMethod                                                              
  //----------------------------------------------------------------------------

//  constructor TcaIntfMethod.Create(const AMetaData: TIntfMetaData);
//  begin
//
//  end;

  // Create/Destroy 

//  constructor TcaIntfInfo.Create(const AIntf: IUnknown; const ATypeInfo: PTypeInfo);
//  begin
//    inherited Create;
//    FIntf := AIntf;
//    GetIntfMetaData(ATypeInfo, FMetaData);
//  end;

  // Public methods 

procedure TcaIntfInfo.GetMethodsAsStrings(AStrings: TStrings);
//var
//  Index: Integer;
//  Method: TIntfMethEntry;
//  Methods: TIntfMethEntryArray;
begin
//  AStrings.Clear;
//  Methods := FMetaData.MDA;
//  for Index := Low(Methods) to High(Methods) do
//    begin
//      Method := Methods[Index];
//      AStrings.Add(Method.Name);
//    end;
end;

end.
