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


unit caNetwork;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ScktComp,
  Winsock,

  // ca units 
  caUtils,
  caNodes;

type

  TcaNetNodeType = (ntDomain, ntGeneric, ntServer, ntShare, ntGroup, ntNetwork,
                     ntRoot, ntShareAdmin, ntDirectory, ntTree, ntNDSContainer);

  TcaNetNodeTypes = set of TcaNetNodeType;

  TcaAddNetNodeEvent = procedure(Sender: TObject; const AResourceName, AIPAddress: String;
    ANodeType: TcaNetNodeType; var Accept: Boolean) of object;

  TcaNetNode = class;

  //---------------------------------------------------------------------------
  // TcaNetNode                                                                
  //---------------------------------------------------------------------------

  TcaNetNode = class(TcaNode)
  private
    FIPAddress: String;
    FNodeName: String;
    FNodeType: TcaNetNodeType;
    // New property methods 
    function GetIPAddress: String;
    function GetNodeName: String;
    function GetNodeType: TcaNetNodeType;
    procedure SetIPAddress(const Value: String);
    procedure SetNodeName(const Value: String);
    procedure SetNodeType(const Value: TcaNetNodeType);
    // Replacement property methods 
    function GetFirstSub: TcaNetNode;
    function GetLastSub: TcaNetNode;
    function GetNext: TcaNetNode;
    function GetNextIso: TcaNetNode;
    function GetParent: TcaNetNode;
    function GetPrior: TcaNetNode;
    function GetPriorIso: TcaNetNode;
    function GetRoot: TcaNetNode;
    procedure SetFirstSub(const Value: TcaNetNode);
    procedure SetLastSub(const Value: TcaNetNode);
    procedure SetNextIso(const Value: TcaNetNode);
    procedure SetParent(const Value: TcaNetNode);
    procedure SetPriorIso(const Value: TcaNetNode);
  public
    // Replacement properties 
    property FirstSub: TcaNetNode read GetFirstSub write SetFirstSub;
    property LastSub: TcaNetNode read GetLastSub write SetLastSub;
    property Next: TcaNetNode read GetNext;
    property NextIso: TcaNetNode read GetNextIso write SetNextIso;
    property Parent: TcaNetNode read GetParent write SetParent;
    property Prior: TcaNetNode read GetPrior;
    property PriorIso: TcaNetNode read GetPriorIso write SetPriorIso;
    property Root: TcaNetNode read GetRoot;
    // New properties 
    property IPAddress: String read GetIPAddress write SetIPAddress;
    property NodeName: String read GetNodeName write SetNodeName;
    property NodeType: TcaNetNodeType read GetNodeType write SetNodeType;
  end;

  //---------------------------------------------------------------------------
  // TcaNetNodeList                                                            
  //---------------------------------------------------------------------------

  TcaNetNodeList = class(TcaNodeList)
  private
    function GetNode(Index: Integer): TcaNetNode;
    procedure SetNode(Index: Integer; const Value: TcaNetNode);
  public
    property Items[Index: Integer]: TcaNetNode read GetNode write SetNode; default;
  end;

  //---------------------------------------------------------------------------
  // IcaNetNodes                                                               
  //---------------------------------------------------------------------------

  IcaNetNodes = interface
  ['{B6935496-402C-4D85-9880-590FA3646E60}']
    // Property methods 
    function GetLevelCount: Integer;
    function GetLoadNode: TcaNetNode;
    function GetNodeCount: Integer;
    function GetRoot(Index: Integer): TcaNetNode;
    function GetRootCount: Integer;
    function GetSaveNode: TcaNetNode;
    function GetVisibleNodeCount: Integer;
    procedure SetLoadNode(const Value: TcaNetNode);
    procedure SetSaveNode(const Value: TcaNetNode);
    // Public methods 
    function AddIso(APriorIso: TcaNetNode; AText: string): TcaNetNode;
    function AddRoot(AText: string): TcaNetNode;
    function AddSub(AParent: TcaNetNode; AText: string): TcaNetNode;
    function AddSubAndCopy(AParent: TcaNetNode; AText: string): TcaNetNode;
    function IsBottomLevel(ANode: TcaNetNode): Boolean;
    procedure BeginUpdate;
    procedure Clear;
    procedure Delete(Node: TcaNetNode);
    procedure EndUpdate;
    procedure FullCollapse;
    procedure FullExpand;
    procedure GetAllNodes(ANodeList: TcaNetNodeList);
    procedure GetNodesForLevel(ANodeList: TcaNetNodeList; ALevel: Integer);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    // Properties 
    property LevelCount: Integer read GetLevelCount;
    property LoadNode: TcaNetNode read GetLoadNode write SetLoadNode;
    property NodeCount: Integer read GetNodeCount;
    property RootCount: Integer read GetRootCount;
    property Roots[Index: Integer]: TcaNetNode read GetRoot;
    property SaveNode: TcaNetNode read GetSaveNode write SetSaveNode;
    property VisibleNodeCount: Integer read GetVisibleNodeCount;
  end;

  //---------------------------------------------------------------------------
  // TcaNetNodes                                                               
  //---------------------------------------------------------------------------

  TcaNetNodes = class(TcaNodes, IcaNetNodes)
  private
    // Property methods 
    function GetLevelCount: Integer;
    function GetLoadNode: TcaNetNode;
    function GetNodeCount: Integer;
    function GetRoot(Index: Integer): TcaNetNode;
    function GetRootCount: Integer;
    function GetSaveNode: TcaNetNode;
    function GetVisibleNodeCount: Integer;
    procedure SetLoadNode(const Value: TcaNetNode);
    procedure SetSaveNode(const Value: TcaNetNode);
  protected
    function CreateNode: TcaNode; override;
  public
    // Public methods 
    function AddIso(APriorIso: TcaNetNode; AText: string): TcaNetNode;
    function AddRoot(AText: string): TcaNetNode;
    function AddSub(AParent: TcaNetNode; AText: string): TcaNetNode;
    function AddSubAndCopy(AParent: TcaNetNode; AText: string): TcaNetNode;
    function IsBottomLevel(ANode: TcaNetNode): Boolean;
    procedure Delete(Node: TcaNetNode);
    procedure GetAllNodes(ANodeList: TcaNetNodeList);
    procedure GetNodesForLevel(ANodeList: TcaNetNodeList; ALevel: Integer);
    // Properties 
    property LevelCount: Integer read GetLevelCount;
    property LoadNode: TcaNetNode read GetLoadNode write SetLoadNode;
    property NodeCount: Integer read GetNodeCount;
    property RootCount: Integer read GetRootCount;
    property Roots[Index: Integer]: TcaNetNode read GetRoot;
    property SaveNode: TcaNetNode read GetSaveNode write SetSaveNode;
    property VisibleNodeCount: Integer read GetVisibleNodeCount;
  end;

  //---------------------------------------------------------------------------
  // IcaNetResources                                                           
  //---------------------------------------------------------------------------

  IcaNetResources = interface
  ['{80128C15-5D7A-4B8E-A563-84E42B81A826}']
    // Property methods 
    function GetExcludeNodeTypes: TcaNetNodeTypes;
    function GetNodes: TcaNetNodes;
    procedure SetExcludeNodeTypes(const Value: TcaNetNodeTypes);
    // Public methods 
    procedure Update;
    // Properties 
    property ExcludeNodeTypes: TcaNetNodeTypes read GetExcludeNodeTypes write SetExcludeNodeTypes;
    property Nodes: TcaNetNodes read GetNodes;
  end;

  //---------------------------------------------------------------------------
  // TcaNetResources                                                           
  //---------------------------------------------------------------------------

  TcaNetResources = class(TInterfacedObject, IcaNetResources)
  private
    // Property fields 
    FExcludeNodeTypes: TcaNetNodeTypes;
    FNodes: TcaNetNodes;
    FOnAddNetNode: TcaAddNetNodeEvent;
    // Private fields 
    FSocket: TClientSocket;
    // Property methods 
    function GetExcludeNodeTypes: TcaNetNodeTypes;
    function GetNodes: TcaNetNodes;
    function GetOnAddNetNode: TcaAddNetNodeEvent;
    procedure SetExcludeNodeTypes(const Value: TcaNetNodeTypes);
    procedure SetOnAddNetNode(const Value: TcaAddNetNodeEvent);
    // Private methods 
    function HostNameToIPAddress(const AHostName: String): String;
    procedure EnumerateNetResources;
    procedure EnumerateNetNode(ANetNode: PNetResource; ANode: TcaNetNode);
  protected
    procedure DoAddNetNode(const AResourceName, AIPAddress: String; ANodeType: TcaNetNodeType; var Accept: Boolean); virtual;
  public
    constructor Create(AUpdateImmediately: Boolean = True);
    destructor Destroy; override;
    // Public methods 
    procedure Update;
    // Properties 
    property ExcludeNodeTypes: TcaNetNodeTypes read GetExcludeNodeTypes write SetExcludeNodeTypes;
    property Nodes: TcaNetNodes read GetNodes;
    property OnAddNetNode: TcaAddNetNodeEvent read GetOnAddNetNode write SetOnAddNetNode;
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaNetNode                                                                
  //---------------------------------------------------------------------------

  // New property methods 

function TcaNetNode.GetIPAddress: String;
begin
  Result := FIPAddress;
end;

function TcaNetNode.GetNodeName: String;
begin
  Result := FNodeName;
end;

function TcaNetNode.GetNodeType: TcaNetNodeType;
begin
  Result := FNodeType;
end;

procedure TcaNetNode.SetIPAddress(const Value: String);
begin
  FIPAddress := Value;
end;

procedure TcaNetNode.SetNodeName(const Value: String);
begin
  FNodeName := Value;
end;

procedure TcaNetNode.SetNodeType(const Value: TcaNetNodeType);
begin
  FNodeType := Value;
end;

  // Replacement property methods 

function TcaNetNode.GetFirstSub: TcaNetNode;
begin
  Result := TcaNetNode(inherited FirstSub);
end;

function TcaNetNode.GetLastSub: TcaNetNode;
begin
  Result := TcaNetNode(inherited LastSub);
end;

function TcaNetNode.GetNext: TcaNetNode;
begin
  Result := TcaNetNode(inherited Next);
end;

function TcaNetNode.GetNextIso: TcaNetNode;
begin
  Result := TcaNetNode(inherited NextIso);
end;

function TcaNetNode.GetParent: TcaNetNode;
begin
  Result := TcaNetNode(inherited Parent);
end;

function TcaNetNode.GetPrior: TcaNetNode;
begin
  Result := TcaNetNode(inherited Prior);
end;

function TcaNetNode.GetPriorIso: TcaNetNode;
begin
  Result := TcaNetNode(inherited PriorIso);
end;

function TcaNetNode.GetRoot: TcaNetNode;
begin
  Result := TcaNetNode(inherited Root);
end;

procedure TcaNetNode.SetFirstSub(const Value: TcaNetNode);
begin
  inherited FirstSub := Value;
end;

procedure TcaNetNode.SetLastSub(const Value: TcaNetNode);
begin
  inherited LastSub := Value;
end;

procedure TcaNetNode.SetNextIso(const Value: TcaNetNode);
begin
  inherited NextIso := Value;
end;

procedure TcaNetNode.SetParent(const Value: TcaNetNode);
begin
  inherited Parent := Value;
end;

procedure TcaNetNode.SetPriorIso(const Value: TcaNetNode);
begin
  inherited PriorIso := Value;
end;

  //---------------------------------------------------------------------------
  // TcaNetNodeList                                                            
  //---------------------------------------------------------------------------

function TcaNetNodeList.GetNode(Index: Integer): TcaNetNode;
begin
  Result := TcaNetNode(inherited Items[Index]);
end;

procedure TcaNetNodeList.SetNode(Index: Integer; const Value: TcaNetNode);
begin
  inherited Items[Index] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaNetNodes                                                               
  //---------------------------------------------------------------------------

  // Public methods 

function TcaNetNodes.AddIso(APriorIso: TcaNetNode; AText: string): TcaNetNode;
begin
  Result := TcaNetNode(inherited AddIso(APriorIso, AText));
end;

function TcaNetNodes.AddRoot(AText: string): TcaNetNode;
begin
  Result := TcaNetNode(inherited AddRoot(AText));
end;

function TcaNetNodes.AddSub(AParent: TcaNetNode; AText: string): TcaNetNode;
begin
  Result := TcaNetNode(inherited AddSub(AParent, AText));
end;

function TcaNetNodes.AddSubAndCopy(AParent: TcaNetNode; AText: string): TcaNetNode;
begin
  Result := TcaNetNode(inherited AddSubAndCopy(AParent, AText));
end;

function TcaNetNodes.IsBottomLevel(ANode: TcaNetNode): Boolean;
begin
  Result := inherited IsBottomLevel(ANode);
end;

procedure TcaNetNodes.Delete(Node: TcaNetNode);
begin
  inherited Delete(Node);
end;

procedure TcaNetNodes.GetAllNodes(ANodeList: TcaNetNodeList);
begin
  inherited GetAllNodes(ANodeList);
end;

procedure TcaNetNodes.GetNodesForLevel(ANodeList: TcaNetNodeList; ALevel: Integer);
begin
  inherited GetNodesForLevel(ANodeList, ALevel);
end;

  // Protected methods 

function TcaNetNodes.CreateNode: TcaNode;
begin
  Result := TcaNetNode.Create(nil);
end;

  // Property methods 

function TcaNetNodes.GetLevelCount: Integer;
begin
  Result := inherited LevelCount;
end;

function TcaNetNodes.GetLoadNode: TcaNetNode;
begin
  Result := TcaNetNode(inherited LoadNode);
end;

function TcaNetNodes.GetNodeCount: Integer;
begin
  Result := inherited NodeCount;
end;

function TcaNetNodes.GetRoot(Index: Integer): TcaNetNode;
begin
  Result := TcaNetNode(inherited Roots[Index]);
end;

function TcaNetNodes.GetRootCount: Integer;
begin
  Result := inherited RootCount;
end;

function TcaNetNodes.GetSaveNode: TcaNetNode;
begin
  Result := TcaNetNode(inherited SaveNode);
end;

function TcaNetNodes.GetVisibleNodeCount: Integer;
begin
  Result := inherited VisibleNodeCount;
end;

procedure TcaNetNodes.SetLoadNode(const Value: TcaNetNode);
begin
  inherited LoadNode := Value;
end;

procedure TcaNetNodes.SetSaveNode(const Value: TcaNetNode);
begin
  inherited SaveNode := Value;
end;

  //---------------------------------------------------------------------------
  // TcaNetResources                                                           
  //---------------------------------------------------------------------------

constructor TcaNetResources.Create(AUpdateImmediately: Boolean = True);
begin
  inherited Create;
  FNodes := TcaNetNodes.Create;
  FSocket := TClientSocket.Create(nil);
  if AUpdateImmediately then Update;
end;

destructor TcaNetResources.Destroy;
begin
  FNodes.Free;
  FSocket.Free;
  inherited;
end;

  // Protected methods 

procedure TcaNetResources.DoAddNetNode(const AResourceName, AIPAddress: String; ANodeType: TcaNetNodeType; var Accept: Boolean);
begin
  if Assigned(FOnAddNetNode) then
    FOnAddNetNode(Self, AResourceName, AIPAddress, ANodeType, Accept);
end;

  // Public methods 

procedure TcaNetResources.Update;
begin
  EnumerateNetResources;
end;

  // Private methods 

function TcaNetResources.HostNameToIPAddress(const AHostName: String): String;
var
  IP: SunB;
  HostName: String;
begin
  HostName := AHostName;
  Utils.Replace(HostName, '\\', '');
  IP := FSocket.Socket.LookupName(HostName).S_un_b;
  Result := Format('%d.%d.%d.%d', [Ord(IP.s_b1), Ord(IP.s_b2), Ord(IP.s_b3), Ord(IP.s_b4)]);
end;

procedure TcaNetResources.EnumerateNetNode(ANetNode: PNetResource; ANode: TcaNetNode);
var
  Accept: Boolean;
  Buffer: PNetResource;
  BufferSize: DWORD;
  ChildNode: TcaNetNode;
  Count: DWORD;
  EnumHandle: THandle;
  ErrorCode: Integer;
  Included: Boolean;
  IPAddress: String;
  NetResource: PNetResource;
  NewNodeType: TcaNetNodeType;
  ResourceName: String;
const
  AllItems = $FFFFFFFF;
begin
  ErrorCode := WNetOpenEnum(RESOURCE_GLOBALNET, RESOURCETYPE_ANY, 0, ANetNode, EnumHandle);
  try
    if ErrorCode = NO_ERROR then
      begin
        BufferSize := 1;
        GetMem(Buffer, BufferSize);
        try
          repeat
            Count := AllItems;
            ErrorCode := WNetEnumResource(EnumHandle, Count, Buffer, BufferSize);
            if ErrorCode = ERROR_MORE_DATA then
              begin
                FreeMem(Buffer);
                GetMem(Buffer, BufferSize);
                Count := AllItems;
                ErrorCode := WNetEnumResource(EnumHandle, Count, Buffer, BufferSize);
              end;
            if ErrorCode <> ERROR_NO_MORE_ITEMS then
              begin
                NetResource := Buffer;
                while Count > 0 do
                  begin
                    // Is the node type in the requested set 
                    NewNodeType := TcaNetNodeType(NetResource.dwDisplayType);
                    Included := not (NewNodeType in FExcludeNodeTypes);
                    // Set up an event call 
                    ResourceName := String(NetResource.lpRemoteName);
                    IPAddress := '';
                    if NewNodeType = ntServer then
                      IPAddress := HostNameToIPAddress(ResourceName);
                    Accept := True;
                    DoAddNetNode(ResourceName, IPAddress, NewNodeType, Accept);
                    if Included and Accept then
                      begin
                        if ANode = nil then
                          ChildNode := FNodes.AddRoot('')
                        else
                          ChildNode := FNodes.AddSub(ANode, '');
                        ChildNode.NodeName := ResourceName;
                        ChildNode.NodeType := NewNodeType;
                        if ChildNode.NodeType = ntServer then
                          ChildNode.IPAddress := HostNameToIPAddress(ChildNode.NodeName);
                        EnumerateNetNode(NetResource, ChildNode);
                      end;
                    Inc(NetResource);
                    Dec(Count);
                  end;
              end;
          until ErrorCode = ERROR_NO_MORE_ITEMS;
        finally
          FreeMem(Buffer);
        end;
      end;
  finally
    WNetCloseEnum(EnumHandle);
  end;
end;

procedure TcaNetResources.EnumerateNetResources;
begin
  EnumerateNetNode(nil, nil);
end;

 // Property methods 

function TcaNetResources.GetExcludeNodeTypes: TcaNetNodeTypes;
begin
  Result := FExcludeNodeTypes;
end;

function TcaNetResources.GetNodes: TcaNetNodes;
begin
  Result := FNodes;
end;

function TcaNetResources.GetOnAddNetNode: TcaAddNetNodeEvent;
begin
  Result := FOnAddNetNode;
end;

procedure TcaNetResources.SetExcludeNodeTypes(const Value: TcaNetNodeTypes);
begin
  FExcludeNodeTypes := Value;
end;

procedure TcaNetResources.SetOnAddNetNode(const Value: TcaAddNetNodeEvent);
begin
  FOnAddNetNode := Value;
end;

end.
