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


unit caRtti;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  SysUtils,
  Classes,
  Contnrs,
  TypInfo,

  // ca Utils 
  caUtils;

type

  //----------------------------------------------------------------------------
  // TcaRttiItem                                                                
  //----------------------------------------------------------------------------

  IcaRttiList = interface;

  TcaRttiItem = class(TObject)
  private
    // Private methods 
    FCollectionItems: TList;
    FIndex: Integer;
    FInstance: TObject;
    FParentItem: TcaRttiItem;
    FPropClassType: TClass;
    FPropInfo: PPropInfo;
    FPropName: String;
    FPropSize: Integer;
    FPropType: TTypeKind;
    FSubItems: IcaRttiList;
    // Property methods 
    function GetCollectionItemCount: Integer;
    function GetCollectionItem(Index: Integer): TCollectionItem;
    function GetLevel: Integer;
    function GetPropValue: Variant;
    function GetPropValueAsString: string;
  public
    // Create/Destroy 
    constructor Create(AInstance: TObject);
    destructor Destroy; override;
    // Public methods 
    procedure AddCollectionItem(ACollectionItem: TCollectionItem);
    procedure Copy(ASource: TcaRttiItem);
    procedure CreateSubItems(const AInstance: TObject; ARecurse: Boolean = False; AParentItem: TcaRttiItem = nil);
    // Public properties 
    property CollectionItemCount: Integer read GetCollectionItemCount;
    property CollectionItems[Index: Integer]: TCollectionItem read GetCollectionItem;
    property Index: Integer read FIndex write FIndex;
    property Instance: TObject read FInstance;
    property Level: Integer read GetLevel;
    property ParentItem: TcaRttiItem read FParentItem write FParentItem;
    property PropClassType: TClass read FPropClassType write FPropClassType;
    property PropInfo: PPropInfo read FPropInfo write FPropInfo;
    property PropName: String read FPropName write FPropName;
    property PropSize: Integer read FPropSize write FPropSize;
    property PropType: TTypeKind read FPropType write FPropType;
    property PropValue: Variant read GetPropValue;
    property PropValueAsString: string read GetPropValueAsString;
    property SubItems: IcaRttiList read FSubItems;
  end;

  //----------------------------------------------------------------------------
  // IcaRttiList                                                                
  //----------------------------------------------------------------------------

  IcaRttiList = interface
  ['{2FBAD7A9-0ED8-49BD-8FF1-537C0E89814E}']
    // Property methods 
    function GetCount: Integer;
    function GetItem(Index: Integer): TcaRttiItem;
    // Public methods 
    procedure AddToStrings(AStrings: TStrings; AUseIndent: Boolean);
    // Public properties 
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcaRttiItem read GetItem; default;
  end;

  //----------------------------------------------------------------------------
  // TcaRttiList                                                                
  //----------------------------------------------------------------------------

  TcaRttiList = class(TInterfacedObject, IcaRttiList)
  private
    // Private fields 
    FInstance: TObject;
    FList: TObjectList;
    FParentItem: TcaRttiItem;
    FRecurse: Boolean;
    // Property methods 
    function GetCount: Integer;
    function GetItem(Index: Integer): TcaRttiItem;
    // Private methods 
    procedure Update;
  public
    // Create/Destroy 
    constructor Create(const AInstance: TObject; ARecurse: Boolean = False; AParentItem: TcaRttiItem = nil);
    destructor Destroy; override;
    // Public methods 
    procedure AddToStrings(AStrings: TStrings; AUseIndent: Boolean);
    // Public properties 
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcaRttiItem read GetItem; default;
  end;

implementation

  //----------------------------------------------------------------------------
  // TcaRttiItem                                                                
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaRttiItem.Create(AInstance: TObject);
begin
  inherited Create;
  FInstance := AInstance;
  FCollectionItems := TList.Create;
end;

destructor TcaRttiItem.Destroy;
begin
  FCollectionItems.Free;
  inherited;
end;

  // Public methods 

procedure TcaRttiItem.AddCollectionItem(ACollectionItem: TCollectionItem);
begin
  FCollectionItems.Add(ACollectionItem);
end;

procedure TcaRttiItem.Copy(ASource: TcaRttiItem);
begin
  FIndex := ASource.Index;
  FInstance := ASource.Instance;
  FPropClassType := ASource.PropClassType;
  FPropInfo := ASource.PropInfo;
  FPropName := ASource.PropName;
  FPropSize := ASource.PropSize;
  FPropType := ASource.PropType;
end;

procedure TcaRttiItem.CreateSubItems(const AInstance: TObject; ARecurse: Boolean = False; AParentItem: TcaRttiItem = nil);
begin
  FSubItems := TcaRttiList.Create(AInstance, ARecurse, AParentItem);
end;

 // Property methods 

function TcaRttiItem.GetCollectionItemCount: Integer;
begin
  Result := FCollectionItems.Count;
end;

function TcaRttiItem.GetCollectionItem(Index: Integer): TCollectionItem;
begin
  Result := TCollectionItem(FCollectionItems[Index]);
end;

function TcaRttiItem.GetLevel: Integer;
var
  Parent: TcaRttiItem;
begin
  Result := 0;
  Parent := FParentItem;
  while Assigned(Parent) do
    begin
      Inc(Result);
      Parent := Parent.ParentItem;
    end;
end;

function TcaRttiItem.GetPropValue: Variant;
begin
  Result := TypInfo.GetPropValue(FInstance, FPropName, False);
end;

function TcaRttiItem.GetPropValueAsString: string;
begin
  Result := TypInfo.GetPropValue(FInstance, FPropName, True);
end;

  //---------------------------------------------------------------------------
  // TcaRttiList                                                               
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaRttiList.Create(const AInstance: TObject; ARecurse: Boolean = False; AParentItem: TcaRttiItem = nil);
begin
  inherited Create;
  FInstance := AInstance;
  FRecurse := ARecurse;
  FParentItem := AParentItem;
  FList := TObjectList.Create(True);
  Update;
end;

destructor TcaRttiList.Destroy;
begin
  FList.Free;
  inherited;
end;

  // Public methods 

procedure TcaRttiList.AddToStrings(AStrings: TStrings; AUseIndent: Boolean);
var
  CollItem: TCollectionItem;
  CollIndex: Integer;
  Index: Integer;
  Item: TcaRttiItem;
  RttiList: IcaRttiList;
  PadStr: string;
begin
  for Index := 0 to Pred(GetCount) do
    begin
      Item := GetItem(Index);
      PadStr := Utils.BuildString(#32, 4 * Item.Level * Ord(AUseIndent));
      if Item.SubItems <> nil then
        begin
          AStrings.Add(Format('%s%s: %s', [PadStr, Item.PropName, Item.PropClassType.ClassName]));
          Item.SubItems.AddToStrings(AStrings, True);
        end
      else
        begin
          if Item.CollectionItemCount > 0 then
            begin
              AStrings.Add(Format('%s%s: %s', [PadStr, Item.PropName, Item.PropClassType.ClassName]));
              for CollIndex := 0 to Pred(Item.CollectionItemCount) do
                begin
                  CollItem := Item.CollectionItems[CollIndex];
                  RttiList := TcaRttiList.Create(CollItem, True, Item);
                  RttiList.AddToStrings(AStrings, AUseIndent);
                  AStrings.Add('');
                end;
            end
          else
            AStrings.Add(Format('%s%s = %s', [PadStr, Item.PropName, Item.PropValueAsString]));
        end;
    end;
end;

 // Private methods 

procedure TcaRttiList.Update;
var
  CollIndex: Integer;
  PropIndex: Integer;
  Item: TcaRttiItem;
  PropInfo: PPropInfo;
  PropList: PPropList;
  PropName: String;
  PropType: TTypeKind;
  PropTypeData: PTypeData;
  PropTypeInfo: TTypeInfo;
  TypeData: PTypeData;
  TypeInfo: PTypeInfo;
  PropObj: TPersistent;
  CollObj: TCollection;
  CollItem: TCollectionItem;
begin
  FList.Clear;
  TypeInfo := FInstance.ClassInfo;
  TypeData := GetTypeData(TypeInfo);
  if TypeData.PropCount <> 0 then
    begin
      GetMem(PropList, SizeOf(PPropInfo) * TypeData.PropCount);
      try
        GetPropInfos(TypeInfo, PropList);
        for PropIndex := 0 to TypeData.PropCount - 1 do
          begin
            // Get RTTI for each published object property 
            PropInfo := PropList[PropIndex];
            PropName := PropInfo^.Name;
            PropType := PropInfo^.PropType^.Kind;
            PropTypeInfo := PropList[PropIndex]^.PropType^^;
            PropTypeData := GetTypeData(@PropTypeInfo);
            // Create the RTTI schema and add to list 
            Item := TcaRttiItem.Create(FInstance);
            Item.ParentItem := FParentItem;
            FList.Add(Item);
            // Set RTTI schema properties 
            Item.Index := PropIndex;
            Item.PropInfo := PropInfo;
            Item.PropType := PropType;
            Item.PropName := PropName;
            Item.PropClassType := PropTypeData^.ClassType;
            case PropType of
              tkChar:     Item.PropSize := 1;
              tkString:   Item.PropSize := PropTypeData^.MaxLength;
            else
              Item.PropSize := 0;
            end;

            if FRecurse and (PropType = tkClass) then
              begin
                if Item.PropClassType.InheritsFrom(TPersistent) and
                (not Item.PropClassType.InheritsFrom(TComponent)) then
                  begin
                    if Item.PropClassType.InheritsFrom(TCollection) then
                      begin
                        CollObj := TCollection(TypInfo.GetObjectProp(FInstance, PropName));
                        for CollIndex := 0 to Pred(CollObj.Count) do
                          begin
                            CollItem := CollObj.Items[CollIndex];
                            Item.AddCollectionItem(CollItem);
                          end;
                      end
                    else
                      begin
                        PropObj := TPersistent(TypInfo.GetObjectProp(FInstance, PropName));
                        if Assigned(PropObj) then
                          Item.CreateSubItems(PropObj, FRecurse, Item);
                      end;
                  end;
              end;
          end;
      finally
        FreeMem(PropList);
      end;
    end;
end;

 // Property methods 

function TcaRttiList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcaRttiList.GetItem(Index: Integer): TcaRttiItem;
begin
  Result := TcaRttiItem(FList[Index]);
end;

end.
