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


unit caForms;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Windows,
  Messages,
  Classes,
  SysUtils,
  Controls,
  Forms,
  Contnrs,
  Dialogs,
  TypInfo,

  // ca units
  caUtils,
  caClasses,
  caConsts,
  caLog,
  caNodes;

type

  TcaAddComponentEvent = procedure(Sender: TObject; AComponent: TComponent; var AAccept: Boolean) of object;

  TcaControlStateEvent = procedure(Sender: TObject; AControl: TControl; var AAccept: Boolean) of object;

  TcaControlStateCancelProc = procedure of object;

  //---------------------------------------------------------------------------
  // IcaCursor                                                                 
  //---------------------------------------------------------------------------

  IcaCursor = interface
  ['{A391B7DF-0355-4ABB-92AF-5A12DEBC29BB}']
  end;

  //---------------------------------------------------------------------------
  // TcaCursor                                                                 
  //---------------------------------------------------------------------------

  TcaCursor = class(TInterfacedObject, IcaCursor)
  private
    FOldCursor: TCursor;
  public
    constructor Create(AShape: TCursor);
    destructor Destroy; override;
  end;

  //---------------------------------------------------------------------------
  // IcaOwnedComponents                                                        
  //---------------------------------------------------------------------------

  IcaOwnedComponents = interface
  ['{6689CA56-7CC7-43C1-BDFE-6F9C0BD2E8D0}']
    // Property methods 
    function GetComponent(Index: Integer): TComponent;
    function GetCount: Integer;
    // Event property methods 
    function GetOnAddComponent: TcaAddComponentEvent;
    procedure SetOnAddComponent(const Value: TcaAddComponentEvent);
    // Properties 
    property Components[Index: Integer]: TComponent read GetComponent;
    property Count: Integer read GetCount;
    // Event properties 
    property OnAddComponent: TcaAddComponentEvent read GetOnAddComponent write SetOnAddComponent;
  end;

  //---------------------------------------------------------------------------
  // TcaOwnedComponents                                                        
  //---------------------------------------------------------------------------

  TcaOwnedComponents = class(TInterfacedObject, IcaOwnedComponents)
  private
    // Private fields 
    FComponents: TList;
    FOnAddComponent: TcaAddComponentEvent;
    FRootComponent: TComponent;
    // Property methods 
    function GetComponent(Index: Integer): TComponent;
    function GetCount: Integer;
    // Event property methods 
    function GetOnAddComponent: TcaAddComponentEvent;
    procedure SetOnAddComponent(const Value: TcaAddComponentEvent);
    // Private methods 
    procedure AddComponents;
    procedure AddComponents_Recursed(AComponent: TComponent);
  protected
    // Protected methods 
    procedure DoAddComponent(AComponent: TComponent; var AAccept: Boolean); virtual;
  public
    // Create/Destroy 
    constructor Create(ARootComponent: TComponent);
    destructor Destroy; override;
  end;

  //---------------------------------------------------------------------------
  // IcaParentedControls                                                       
  //---------------------------------------------------------------------------

  IcaParentedControls = interface
  ['{E60D9E67-A9E8-437F-8864-5895FCFB54CC}']
    // Property methods 
    function GetControl(Index: Integer): TControl;
    function GetCount: Integer;
    // Properties 
    property Controls[Index: Integer]: TControl read GetControl;
    property Count: Integer read GetCount;
  end;

  //---------------------------------------------------------------------------
  // TcaParentedControls                                                       
  //---------------------------------------------------------------------------

  TcaParentedControls = class(TInterfacedObject, IcaParentedControls)
  private
    // Private fields 
    FControls: TList;
    FRootControl: TWinControl;
    // Property methods 
    function GetControl(Index: Integer): TControl;
    function GetCount: Integer;
    // Private methods 
    procedure AddControls;
    procedure AddControls_Recursed(AControl: TWinControl);
  public
    // Create/Destroy 
    constructor Create(ARootControl: TWinControl);
    destructor Destroy; override;
  end;

  //---------------------------------------------------------------------------
  // TcaControlStateItem                                                       
  //---------------------------------------------------------------------------

  TcaControlStateItem = class(TObject)
  private
    // Private fields 
    FControl: TControl;
    FDefaultCursor: TCursor;
    FDefaultEnabled: Boolean;
    FDefaultVisible: Boolean;
  public
    // Public methods 
    procedure SaveDefaults;
    procedure RestoreDefaults;
    // Properties 
    property Control: TControl read FControl write FControl;
    property DefaultCursor: TCursor read FDefaultCursor write FDefaultCursor;
    property DefaultEnabled: Boolean read FDefaultEnabled write FDefaultEnabled;
    property DefaultVisible: Boolean read FDefaultVisible write FDefaultVisible;
  end;

  //----------------------------------------------------------------------------
  // TcaControlStateList                                                        
  //----------------------------------------------------------------------------

  TcaControlStateList = class(TObject)
  private
    // Private fields 
    FList: TObjectList;
    // Property methods 
    function GetCount: Integer;
    function GetItem(Index: Integer): TcaControlStateItem;
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
    // Public methods 
    function Add: TcaControlStateItem;
    function IndexOf(AItem: TcaControlStateItem): Integer;
    procedure Clear;
    procedure RemoveControl(AControl: TControl);
    // Properties 
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcaControlStateItem read GetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaControlState                                                           
  //---------------------------------------------------------------------------

  IcaControlState = interface
  ['{E819573A-E509-416E-B714-8DA6DC518A91}']
    // Property methods
    function GetCursor: TCursor;
    function GetEnabled: Boolean;
    function GetVisible: Boolean;
    procedure SetCursor(const Value: TCursor);
    procedure SetEnabled(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    // Event property methods 
    function GetOnAddControl: TcaControlStateEvent;
    function GetOnSetCursor: TcaControlStateEvent;
    function GetOnSetEnabled: TcaControlStateEvent;
    function GetOnSetVisible: TcaControlStateEvent;
    procedure SetOnAddControl(const Value: TcaControlStateEvent);
    procedure SetOnSetCursor(const Value: TcaControlStateEvent);
    procedure SetOnSetEnabled(const Value: TcaControlStateEvent);
    procedure SetOnSetVisible(const Value: TcaControlStateEvent);
    // Interface methods 
    procedure AddAllControls;
    procedure AddControl(AControl: TControl);
    procedure BeginLongProcess(ACancelControl: TControl; ACancelProc: TcaControlStateCancelProc);
    procedure RemoveControl(AControl: TControl);
    // Properties 
    property Cursor: TCursor read GetCursor write SetCursor;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Visible: Boolean read GetVisible write SetVisible;
    // Event properties 
    property OnAddControl: TcaControlStateEvent read GetOnAddControl write SetOnAddControl;
    property OnSetCursor: TcaControlStateEvent read GetOnSetCursor write SetOnSetCursor;
    property OnSetEnabled: TcaControlStateEvent read GetOnSetEnabled write SetOnSetEnabled;
    property OnSetVisible: TcaControlStateEvent read GetOnSetVisible write SetOnSetVisible;
  end;

  //---------------------------------------------------------------------------
  // TcaControlState                                                           
  //---------------------------------------------------------------------------

  TcaControlStateProperty = (spCursor, spEnabled, spVisible);

  TcaControlState = class(TInterfacedObject, IcaControlState)
  private
    // Private fields 
    FCancelControl: TControl;
    FCancelProc: TcaControlStateCancelProc;
    FControls: TcaControlStateList;
    FDefaultAppMessageEvent: TMessageEvent;
    FForm: TCustomForm;
    // Property fields 
    FCursor: TCursor;
    FEnabled: Boolean;
    FInLongProcess: Boolean;
    FVisible: Boolean;
    // Event property fields 
    FOnAddControl: TcaControlStateEvent;
    FOnSetCursor: TcaControlStateEvent;
    FOnSetEnabled: TcaControlStateEvent;
    FOnSetVisible: TcaControlStateEvent;
    // Property methods
    function GetCursor: TCursor;
    function GetEnabled: Boolean;
    function GetVisible: Boolean;
    procedure SetCursor(const Value: TCursor);
    procedure SetEnabled(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    // Event property methods 
    function GetOnAddControl: TcaControlStateEvent;
    function GetOnSetCursor: TcaControlStateEvent;
    function GetOnSetEnabled: TcaControlStateEvent;
    function GetOnSetVisible: TcaControlStateEvent;
    procedure SetOnAddControl(const Value: TcaControlStateEvent);
    procedure SetOnSetCursor(const Value: TcaControlStateEvent);
    procedure SetOnSetEnabled(const Value: TcaControlStateEvent);
    procedure SetOnSetVisible(const Value: TcaControlStateEvent);
    // Private methods 
    procedure Update(AProperty: TcaControlStateProperty);
    procedure EndLongProcess;
    procedure RestoreDefaults;
    // Event handlers 
    procedure ApplicationMessageEvent(var Msg: tagMSG; var Handled: Boolean);
    procedure CancelControlOnClick(Sender: TObject);
  protected
    // Protected methods 
    procedure DoAddControl(AControl: TControl; var AAccept: Boolean); virtual;
    procedure DoSetCursor(AControl: TControl; var AAccept: Boolean); virtual;
    procedure DoSetEnabled(AControl: TControl; var AAccept: Boolean); virtual;
    procedure DoSetVisible(AControl: TControl; var AAccept: Boolean); virtual;
  public
    // Create/Destroy 
    constructor Create(AForm: TCustomForm);
    destructor Destroy; override;
    // Interface methods - IcaControlState 
    procedure AddAllControls;
    procedure AddControl(AControl: TControl);
    procedure BeginLongProcess(ACancelControl: TControl; ACancelProc: TcaControlStateCancelProc);
    procedure RemoveControl(AControl: TControl);
  end;

  //---------------------------------------------------------------------------
  // IcaFormUtils                                                              
  //---------------------------------------------------------------------------

  IcaFormUtils = interface
  ['{0F69BEB5-5D93-4024-B81D-BD810794800F}']
    // Interface methods 
    function FormLeft(AControl: TControl): Integer;
    function FormTop(AControl: TControl): Integer;
    function FormTopLeft(AControl: TControl): TPoint;
    function HourGlass: IcaCursor;
    function SQLWait: IcaCursor;
    procedure SetStayOnTopState(AForm: TForm; IsOnTop: Boolean);
  end;

  //---------------------------------------------------------------------------
  // TcaFormUtils                                                              
  //---------------------------------------------------------------------------

  TcaFormUtils = class(TInterfacedObject, IcaFormUtils)
  public
    // Public methods
    function FormLeft(AControl: TControl): Integer;
    function FormTop(AControl: TControl): Integer;
    function FormTopLeft(AControl: TControl): TPoint;
    function HourGlass: IcaCursor;
    function SQLWait: IcaCursor;
    procedure SetStayOnTopState(AForm: TForm; IsOnTop: Boolean);
  end;

  //---------------------------------------------------------------------------
  // CoFormUtilsFactory                                                        
  //---------------------------------------------------------------------------

  CoFormUtilsFactory = class
  public
    class function Instance: IcaFormUtils;
  end;

  //---------------------------------------------------------------------------
  // TcaDFMNode                                                                
  //---------------------------------------------------------------------------

  TcaDFMObjectType = (otObject, otInherited, otInline, otEmptyCollection,
                      otCollection, otCollectionItem, otCollectionEnd,
                      otBinaryOneLine, otBinary, otBinaryLine, otBinaryEnd,
                      otList, otListItem, otListEnd,
                      otEmptyList, otEnd, otProperty, otUnknown);

  TcaDFMPropValueType = (vtSymbol, vtString, vtInteger, vtFloat);

  TcaDFMNode = class(TcaNode)
  private
    // Private fields 
    FObjectClass: string;
    FObjectName: string;
    FObjectType: TcaDFMObjectType;
    FPropName: string;
    FPropValue: string;
    FPropValueType: TcaDFMPropValueType;
  public
    // Properties 
    property ObjectClass: string read FObjectClass write FObjectClass;
    property ObjectName: string read FObjectName write FObjectName;
    property ObjectType: TcaDFMObjectType read FObjectType write FObjectType;
    property PropName: string read FPropName write FPropName;
    property PropValue: string read FPropValue write FPropValue;
    property PropValueType: TcaDFMPropValueType read FPropValueType write FPropValueType;
  end;

  //---------------------------------------------------------------------------
  // IcaDFMNodes                                                               
  //---------------------------------------------------------------------------

  IcaDFMNodes = interface(IcaNodes)
  ['{C34D2AB5-B71C-42C5-9FD6-94849D0B0751}']
  end;

  //---------------------------------------------------------------------------
  // TcaDFMNodes                                                               
  //---------------------------------------------------------------------------

  TcaDFMNodes = class(TcaNodes, IcaDFMNodes)
  private
  protected
    // Protected methods 
    function CreateNode: TcaNode; override;
  public
  end;

  //---------------------------------------------------------------------------
  // IcaDFMFile                                                                
  //---------------------------------------------------------------------------

  TcaDFMTranslateStringEvent = procedure(Sender: TObject; const APropPath: string; var APropValue: string) of object;

  IcaDFMFile = interface
  ['{F061FF1E-C781-4B5B-A196-2970050EE748}']
    // Property methods 
    function GetFormClass: TFormClass;
    function GetNodes: TcaDFMNodes;
    function GetPropList: TStrings;
    function GetText: TStrings;
    procedure SetFormClass(const Value: TFormClass);
    // Event property methods 
    function GetOnTranslateString: TcaDFMTranslateStringEvent;
    procedure SetOnTranslateString(const Value: TcaDFMTranslateStringEvent);
    // Interface methods 
    function CreateForm(AOwner: TComponent): TForm;
    procedure Translate;
    // Properties 
    property FormClass: TFormClass read GetFormClass write SetFormClass;
    property Nodes: TcaDFMNodes read GetNodes;
    property PropList: TStrings read GetPropList;
    property Text: TStrings read GetText;
    // Event properties 
    property OnTranslateString: TcaDFMTranslateStringEvent read GetOnTranslateString write SetOnTranslateString;
  end;

  //---------------------------------------------------------------------------
  // TcaDFMTempOwner                                                           
  //---------------------------------------------------------------------------

  TcaDFMTempOwner = class(TComponent);

  //---------------------------------------------------------------------------
  // TcaDFMFile                                                                
  //---------------------------------------------------------------------------

  TcaDFMProcessNodeProc = procedure(ANode: TcaDFMNode; const APropPrefix: string) of object;

  TcaDFMFile = class(TComponent, IcaDFMFile)
  private
    // Private fields 
    FBinary: TMemoryStream;
    FFormClass: TFormClass;
    FNodes: TcaDFMNodes;
    FText: TStrings;
    FPropList: TStrings;
    // Event property fields 
    FOnTranslateString: TcaDFMTranslateStringEvent;
    // Private methods 
    procedure CreateObjects;
    procedure FreeObjects;
    procedure RebuildTextFromNodes;
    procedure TranslateProc(ANode: TcaDFMNode; const APropPrefix: string);
    procedure TraverseNodes(AProc: TcaDFMProcessNodeProc);
    procedure Update;
    procedure UpdateBinary;
    procedure UpdateNodes;
    procedure UpdatePropList;
    procedure UpdatePropListProc(ANode: TcaDFMNode; const APropPrefix: string);
    procedure UpdateText;
  protected
    // Property methods 
    function GetFormClass: TFormClass;
    function GetNodes: TcaDFMNodes;
    function GetPropList: TStrings;
    function GetText: TStrings;
    procedure SetFormClass(const Value: TFormClass);
    // Protected event triggers 
    procedure DoTranslateString(const APropPath: string; var APropValue: string); virtual;
    // Event property methods 
    function GetOnTranslateString: TcaDFMTranslateStringEvent;
    procedure SetOnTranslateString(const Value: TcaDFMTranslateStringEvent);
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    // Interface methods - IcaDFMFile 
    function CreateForm(AOwner: TComponent): TForm;
    procedure Translate;
    // Properties 
    property Binary: TMemoryStream read FBinary;
    property FormClass: TFormClass read GetFormClass write SetFormClass;
    property Nodes: TcaDFMNodes read GetNodes;
    property PropList: TStrings read GetPropList;
    property Text: TStrings read GetText;
  published
    // Event properties 
    property OnTranslateString: TcaDFMTranslateStringEvent read GetOnTranslateString write SetOnTranslateString;
  end;

  //---------------------------------------------------------------------------
  // TcaDFMFiler                                                               
  //---------------------------------------------------------------------------

  TcaDFMFiler = class(TInterfacedObject)
  private
    // Private fields 
    FNodes: TcaDFMNodes;
    FText: TStrings;
  protected
    // Protected properties 
    property Nodes: TcaDFMNodes read FNodes;
    property Text: TStrings read FText;
  public
    // Create/Destroy 
    constructor Create(AText: TStrings; ANodes: TcaDFMNodes);
    destructor Destroy; override;
  end;

  //---------------------------------------------------------------------------
  // IcaDFMReader                                                              
  //---------------------------------------------------------------------------

  IcaDFMReader = interface
  ['{5001EDDC-5C23-49FF-962F-9D6B7086A4FD}']
    // Interface methods 
    procedure Read;
  end;

  //---------------------------------------------------------------------------
  // TcaDFMReader                                                              
  //---------------------------------------------------------------------------

  TcaDFMReader = class(TcaDFMFiler, IcaDFMReader)
  private
    // Private fields 
    FObjectNode: TcaDFMNode;
    // Private methods 
    function GetObjectLevel(const ALine: string): Integer;
    function GetObjectType(const ALine: string): TcaDFMObjectType;
    function IsBinaryLine(const ALine: string): Boolean;
    procedure ProcessBinary(const ALine: string; ALevel: Integer);
    procedure ProcessBinaryLine(const ALine: string; ALevel: Integer; IsEndItem: Boolean = False);
    procedure ProcessBinaryEnd(const ALine: string; ALevel: Integer);
    procedure ProcessBinaryOneLine(const ALine: string; ALevel: Integer);
    procedure ProcessCollection(const ALine: string; ALevel: Integer);
    procedure ProcessCollectionItem(const ALine: string; ALevel: Integer);
    procedure ProcessCollectionEnd(const ALine: string; ALevel: Integer);
    procedure ProcessEmptyCollection(const ALine: string; ALevel: Integer);
    procedure ProcessEmptyList(const ALine: string; ALevel: Integer);
    procedure ProcessEnd;
    procedure ProcessObject(const ALine: string; AObjectType: TcaDFMObjectType; ALevel: Integer);
    procedure ProcessProperty(const ALine: string);
    procedure ProcessList(const ALine: string; ALevel: Integer);
    procedure ProcessListEnd(const ALine: string; ALevel: Integer);
    procedure ProcessListItem(const ALine: string; ALevel: Integer; IsEndItem: Boolean = False);
  protected
    // Interface methods - IcaDFMReader 
    procedure Read;
  end;

  //---------------------------------------------------------------------------
  // IcaDFMWriter                                                              
  //---------------------------------------------------------------------------

  IcaDFMWriter = interface
  ['{03180327-CA07-42FE-ADE7-F8353AF933A3}']
    // Interface methods 
    procedure Write;
  end;

  //---------------------------------------------------------------------------
  // TcaDFMWriter                                                              
  //---------------------------------------------------------------------------

  TcaDFMWriter = class(TcaDFMFiler, IcaDFMWriter)
  private
    // Private methods 
    function GetObjectTypeString(AObjectType: TcaDFMObjectType): string;
    procedure WriteBinary(ANode: TcaDFMNode);
    procedure WriteBinaryLine(ANode: TcaDFMNode);
    procedure WriteBinaryEnd(ANode: TcaDFMNode);
    procedure WriteCollection(ANode: TcaDFMNode);
    procedure WriteCollectionItem(ANode: TcaDFMNode);
    procedure WriteEnd(ANode: TcaDFMNode);
    procedure WriteEnds(const AStack: IcaObjectStack; ANode: TcaDFMNode);
    procedure WriteObject(ANode: TcaDFMNode);
    procedure WriteProperty(ANode: TcaDFMNode);
    procedure WriteRemainingEnds(const AStack: IcaObjectStack);
    procedure WriteList(ANode: TcaDFMNode);
    procedure WriteListEnd(ANode: TcaDFMNode);
    procedure WriteListItem(ANode: TcaDFMNode);
    procedure WriteText(ALevel: Integer; const AText: string);
  protected
    // Interface methods - IcaDFMWriter 
    procedure Write;
  end;

var
  FormUtils: IcaFormUtils;

implementation

  //---------------------------------------------------------------------------
  // TcaCursor                                                                 
  //---------------------------------------------------------------------------

constructor TcaCursor.Create(AShape: TCursor);
begin
  inherited Create;
  FOldCursor := Screen.Cursor;
  Screen.Cursor := AShape;
end;

destructor TcaCursor.Destroy;
begin
  Screen.Cursor := FOldCursor;
  inherited;
end;

  //---------------------------------------------------------------------------
  // TcaOwnedComponents                                                        
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaOwnedComponents.Create(ARootComponent: TComponent);
begin
  inherited Create;
  FComponents := TList.Create;
  FRootComponent := ARootComponent;
  AddComponents;
end;

destructor TcaOwnedComponents.Destroy;
begin
  FComponents.Free;
  inherited;
end;

  // Protected methods 

procedure TcaOwnedComponents.DoAddComponent(AComponent: TComponent; var AAccept: Boolean);
begin
  if Assigned(FOnAddComponent) then FOnAddComponent(Self, AComponent, AAccept);    
end;

  // Private methods 

procedure TcaOwnedComponents.AddComponents;
begin
  FComponents.Clear;
  AddComponents_Recursed(FRootComponent);  
end;

procedure TcaOwnedComponents.AddComponents_Recursed(AComponent: TComponent);
var
  Accept: Boolean;
  Comp: TComponent;
  Index: Integer;
begin
  for Index := 0 to Pred(AComponent.ComponentCount) do
    begin
      Comp := AComponent.Components[Index];
      Accept := True;
      DoAddComponent(Comp, Accept);
      if Accept then
        begin
          FComponents.Add(Comp);
          AddComponents_Recursed(Comp);
        end;
    end;
end;

  // Property methods 

function TcaOwnedComponents.GetComponent(Index: Integer): TComponent;
begin
  Result := TComponent(FComponents[Index]);
end;

function TcaOwnedComponents.GetCount: Integer;
begin
  Result := FComponents.Count;
end;

  // Event property methods 

function TcaOwnedComponents.GetOnAddComponent: TcaAddComponentEvent;
begin
  Result := FOnAddComponent;
end;

procedure TcaOwnedComponents.SetOnAddComponent(const Value: TcaAddComponentEvent);
begin
  FOnAddComponent := Value;
  if Assigned(FOnAddComponent) then
    AddComponents;
end;

  //---------------------------------------------------------------------------
  // TcaParentedControls                                                       
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaParentedControls.Create(ARootControl: TWinControl);
begin
  inherited Create;
  FControls := TList.Create;
  FRootControl := ARootControl;
  AddControls;
end;

destructor TcaParentedControls.Destroy;
begin
  FControls.Free;
  inherited;
end;

  // Private methods 

procedure TcaParentedControls.AddControls;
begin
  FControls.Clear;
  AddControls_Recursed(FRootControl);
end;

procedure TcaParentedControls.AddControls_Recursed(AControl: TWinControl);
var
  Control: TControl;
  Index: Integer;
begin
  for Index := 0 to Pred(AControl.ControlCount) do
    begin
      Control := AControl.Controls[Index];
      FControls.Add(Control);
      if Control is TWinControl then
        AddControls_Recursed(TWinControl(Control));
    end;
end;

  // Property methods 

function TcaParentedControls.GetControl(Index: Integer): TControl;
begin
  Result := TControl(FControls[Index]);
end;

function TcaParentedControls.GetCount: Integer;
begin
  Result := FControls.Count;
end;

  //---------------------------------------------------------------------------
  // TcaControlStateItem                                                       
  //---------------------------------------------------------------------------

  // Public methods 

procedure TcaControlStateItem.RestoreDefaults;
begin
  FControl.Cursor := FDefaultCursor;
  FControl.Enabled := FDefaultEnabled;
  FControl.Visible := FDefaultVisible;
end;

procedure TcaControlStateItem.SaveDefaults;
begin
  FDefaultCursor := FControl.Cursor;
  FDefaultEnabled := FControl.Enabled;
  FDefaultVisible := FControl.Visible;
end;

  //---------------------------------------------------------------------------
  // TcaControlStateList                                                       
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaControlStateList.Create;
begin
  inherited;
  FList := TObjectList.Create(True);
end;

destructor TcaControlStateList.Destroy;
begin
  FList.Free;
  inherited;
end;

  // Public methods 

function TcaControlStateList.Add: TcaControlStateItem;
begin
  Result := TcaControlStateItem.Create;
  FList.Add(Result);
end;

function TcaControlStateList.IndexOf(AItem: TcaControlStateItem): Integer;
begin
  Result := FList.IndexOf(AItem);
end;

procedure TcaControlStateList.Clear;
begin
  FList.Clear;
end;

procedure TcaControlStateList.RemoveControl(AControl: TControl);
var
  ControlState: TcaControlStateItem;
  Index: Integer;
begin
  ControlState := nil;
  for Index := 0 to Pred(GetCount) do
    begin
      if GetItem(Index).Control = AControl then
        begin
          ControlState := GetItem(Index);
          Break;
        end;
    end;
  if Assigned(ControlState) then
    FList.Remove(ControlState);
end;

  // Property methods 

function TcaControlStateList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcaControlStateList.GetItem(Index: Integer): TcaControlStateItem;
begin
  Result := TcaControlStateItem(FList[Index]);
end;

  //---------------------------------------------------------------------------
  // TcaControlState                                                           
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaControlState.Create(AForm: TCustomForm);
begin
  inherited Create;
  FForm := AForm;
  FControls := TcaControlStateList.Create;
end;

destructor TcaControlState.Destroy;
begin
  if FInLongProcess then EndLongProcess;
  FControls.Free;
  inherited;
end;

  // Interface methods - IcaControlState 

procedure TcaControlState.AddAllControls;
var
  Index: Integer;
  ParentedControls: IcaParentedControls;
begin
  ParentedControls := TcaParentedControls.Create(FForm);
  for Index := 0 to Pred(ParentedControls.Count) do
    AddControl(ParentedControls.Controls[Index]);
  AddControl(FForm);
end;

procedure TcaControlState.AddControl(AControl: TControl);
var
  Accept: Boolean;
  ControlState: TcaControlStateItem;
begin
  Accept := True;
  DoAddControl(AControl, Accept);
  if Accept then
    begin
      ControlState := FControls.Add;
      ControlState.Control := AControl;
      ControlState.SaveDefaults;
    end;
end;

type
  TControlEx = class(TControl);

procedure TcaControlState.BeginLongProcess(ACancelControl: TControl; ACancelProc: TcaControlStateCancelProc);
begin
  FCancelControl := ACancelControl;
  FCancelProc := ACancelProc;
  TControlEx(FCancelControl).OnClick := CancelControlOnClick;
  AddAllControls;
  RemoveControl(FCancelControl);
  SetCursor(crHourglass);
  FDefaultAppMessageEvent := Application.OnMessage;
  Application.OnMessage := ApplicationMessageEvent;
  FInLongProcess := True;
end;

procedure TcaControlState.EndLongProcess;
begin
  RestoreDefaults;
  Application.OnMessage := FDefaultAppMessageEvent;
  FInLongProcess := False;
end;

procedure TcaControlState.RemoveControl(AControl: TControl);
begin
  FControls.RemoveControl(AControl);
end;

  // Private methods 

procedure TcaControlState.Update(AProperty: TcaControlStateProperty);
var
  Accept: Boolean;
  ControlState: TcaControlStateItem;
  Index: Integer;

  function IsParentOfCancelControl(AControl: TControl): Boolean;
  var
    Parent: TWinControl;
  begin
    Result := False;
    Parent := FCancelControl.Parent;
    while Assigned(Parent) do
      begin
        if Parent = AControl then
          begin
            Result := True;
            Break;
          end;
        Parent := Parent.Parent;
      end;
  end;

begin
  for Index := 0 to Pred(FControls.Count) do
    begin
      ControlState := FControls[Index];
      Accept := True;
      case AProperty of
        spCursor:
          begin
            DoSetCursor(ControlState.Control, Accept);
            if Accept then
              if not IsParentOfCancelControl(ControlState.Control) then
                ControlState.Control.Cursor := FCursor;
          end;
        spEnabled:
          begin
            DoSetEnabled(ControlState.Control, Accept);
            if Accept then
              if not IsParentOfCancelControl(ControlState.Control) then
              //if ControlState.Control <> FForm then
                ControlState.Control.Enabled := FEnabled;
          end;
        spVisible:
          begin
            DoSetVisible(ControlState.Control, Accept);
            if Accept then
              ControlState.Control.Visible := FVisible;
          end;
      end;
    end;
end;

procedure TcaControlState.RestoreDefaults;
var
  ControlState: TcaControlStateItem;
  Index: Integer;
begin
  for Index := 0 to Pred(FControls.Count) do
    begin
      ControlState := FControls[Index];
      ControlState.RestoreDefaults;
    end;
end;

  // Protected methods 

procedure TcaControlState.DoAddControl(AControl: TControl; var AAccept: Boolean);
begin
  if Assigned(FOnAddControl) then
    FOnAddControl(Self, AControl, AAccept);
end;

procedure TcaControlState.DoSetCursor(AControl: TControl; var AAccept: Boolean);
begin
  if Assigned(FOnSetCursor) then
    FOnSetCursor(Self, AControl, AAccept);    
end;

procedure TcaControlState.DoSetEnabled(AControl: TControl; var AAccept: Boolean);
begin
  if Assigned(FOnSetEnabled) then
    FOnSetEnabled(Self, AControl, AAccept);
end;

procedure TcaControlState.DoSetVisible(AControl: TControl; var AAccept: Boolean);
begin
  if Assigned(FOnSetVisible) then
    FOnSetVisible(Self, AControl, AAccept);
end;

  // Event handlers 

procedure TcaControlState.ApplicationMessageEvent(var Msg: tagMSG; var Handled: Boolean);
var
  Control: TWinControl;
begin
  Control := FindControl(Msg.hwnd);
  if Assigned(Control) and Assigned(FCancelControl) and (Control <> FCancelControl.Parent)
  and (Msg.Message >= WM_MOUSEFIRST) and (Msg.Message <= WM_MOUSELAST) then
    Handled := True;
end;

procedure TcaControlState.CancelControlOnClick(Sender: TObject);
begin
  EndLongProcess;
  FCancelProc;
end;

  // Property methods 

function TcaControlState.GetCursor: TCursor;
begin
  Result := FCursor;
end;

function TcaControlState.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TcaControlState.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TcaControlState.SetCursor(const Value: TCursor);
begin
  FCursor := Value;
  Update(spCursor);
end;

procedure TcaControlState.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  Update(spEnabled);
end;

procedure TcaControlState.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  Update(spVisible);
end;

  // Event property methods 

function TcaControlState.GetOnAddControl: TcaControlStateEvent;
begin
  Result := FOnAddControl;
end;

function TcaControlState.GetOnSetCursor: TcaControlStateEvent;
begin
  Result := FOnSetCursor;
end;

function TcaControlState.GetOnSetEnabled: TcaControlStateEvent;
begin
  Result := FOnSetEnabled;
end;

function TcaControlState.GetOnSetVisible: TcaControlStateEvent;
begin
  Result := FOnSetVisible;
end;

procedure TcaControlState.SetOnAddControl(const Value: TcaControlStateEvent);
begin
  FOnAddControl := Value;
end;

procedure TcaControlState.SetOnSetCursor(const Value: TcaControlStateEvent);
begin
  FOnSetCursor := Value;
end;

procedure TcaControlState.SetOnSetEnabled(const Value: TcaControlStateEvent);
begin
  FOnSetEnabled := Value;
end;

procedure TcaControlState.SetOnSetVisible(const Value: TcaControlStateEvent);
begin
  FOnSetVisible := Value;
end;

  //---------------------------------------------------------------------------
  // TcaFormUtils                                                              
  //---------------------------------------------------------------------------

function TcaFormUtils.FormLeft(AControl: TControl): Integer;
begin
  Result := FormTopLeft(AControl).x;
end;

function TcaFormUtils.FormTop(AControl: TControl): Integer;
begin
  Result := FormTopLeft(AControl).y;
end;

function TcaFormUtils.FormTopLeft(AControl: TControl): TPoint;
var
  OwnerForm: TCustomForm;
  Parent: TWinControl;
begin
  Result.x := -1;
  Result.y := -1;
  if (AControl.Owner <> nil) and (AControl.Owner is TCustomForm) then
    begin
      Result.x := AControl.Left;
      Result.y := AControl.Top;
      OwnerForm := TCustomForm(AControl.Owner);
      Parent := AControl.Parent;
      while (Parent <> nil) and (Parent <> OwnerForm) do
        begin
          Result.x := Result.x + Parent.Left;
          Result.y := Result.y + Parent.Top;
          Parent := Parent.Parent;
        end;
    end;
end;

function TcaFormUtils.HourGlass: IcaCursor;
begin
  Result := TcaCursor.Create(crHourGlass);
end;

function TcaFormUtils.SQLWait: IcaCursor;
begin
  Result := TcaCursor.Create(crSQLWait);
end;

procedure TcaFormUtils.SetStayOnTopState(AForm: TForm; IsOnTop: Boolean);
const
  TopParams: array[Boolean] of HWND = (HWND_NOTOPMOST, HWND_TOPMOST);
begin
  Application.NormalizeAllTopMosts;
  SetWindowPos(AForm.Handle, TopParams[IsOnTop], 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE);
end;

  //---------------------------------------------------------------------------
  // TcaDFMFile                                                                
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaDFMFile.Create(AOwner: TComponent);
begin
  inherited;
  CreateObjects;
end;

destructor TcaDFMFile.Destroy;
begin
  FreeObjects;
  inherited;
end;

  // Interface methods - IcaDFMFile 

function TcaDFMFile.CreateForm(AOwner: TComponent): TForm;
begin
  Result := FFormClass.CreateNew(AOwner);
  FBinary.ReadComponent(Result);
end;

procedure TcaDFMFile.Translate;
begin
  TraverseNodes(TranslateProc);
  RebuildTextFromNodes;
  UpdateBinary;
end;

  // Protected event triggers 

procedure TcaDFMFile.DoTranslateString(const APropPath: string; var APropValue: string);
begin
  if Assigned(FOnTranslateString) then
    FOnTranslateString(Self, APropPath, APropValue);    
end;

  // Private methods 

procedure TcaDFMFile.CreateObjects;
begin
  FBinary := TMemoryStream.Create;
  FNodes := TcaDFMNodes.Create;
  FPropList := TStringList.Create;
  FText := TStringList.Create;
end;

procedure TcaDFMFile.FreeObjects;
begin
  FNodes.Free;
  FPropList.Free;
  FText.Free;
  FBinary.Free;
end;

procedure TcaDFMFile.RebuildTextFromNodes;
var
  Writer: IcaDFMWriter;
begin
  Writer := TcaDFMWriter.Create(FText, FNodes);
  Writer.Write;
end;

procedure TcaDFMFile.TranslateProc(ANode: TcaDFMNode; const APropPrefix: string);
var
  PropValue: string;
  PropPath: string;

  function GetStringContent(const APropValue: string): string;
  var
    Index: Integer;
    InQuotes: Boolean;
    AsciiStr: string;
    AsciiCode: Byte;
  begin
    Result := '';
    InQuotes := False;
    Index := 1;
    while Index <= Length(APropValue) do
      begin
        if APropValue[Index] = cSingleQuote then
          begin
            InQuotes := not InQuotes;
            Inc(Index);
          end
        else
          begin
            if InQuotes then
              begin
                Result := Result + APropValue[Index];
                Inc(Index);
              end
            else
              begin
                if APropValue[Index] = cHash then
                  begin
                    Inc(Index);
                    AsciiStr := '';
                    while (APropValue[Index] in ['0'..'9'])
                    and (Length(AsciiStr) < 3)
                    and (Index <= Length(APropValue)) do
                      begin
                        AsciiStr := AsciiStr + APropValue[Index];
                        Inc(Index);
                      end;
                    if AsciiStr <> '' then
                      begin
                        AsciiCode := StrToInt(AsciiStr);
                        if AsciiCode = 39 then
                          Result := Result + #39#39
                        else
                          Result := Result + Chr(AsciiCode);
                      end;
                  end
                else
                  Inc(Index);
              end;
          end;
      end;
  end;

begin
  if ANode.ObjectType = otProperty then
    begin
      PropValue := GetStringContent(ANode.PropValue);
      if (PropValue <> '')
      and (ANode.PropName <> cName)
      and (Utils.RightStr(ANode.PropName, 9) <> cFontDotName) then
        begin
          PropPath := APropPrefix + ANode.PropName;
          DoTranslateString(PropPath, PropValue);
          ANode.PropValue := cSingleQuote + PropValue + cSingleQuote;
          ANode.Text := ANode.PropValue;
        end;
    end;
end;

procedure TcaDFMFile.TraverseNodes(AProc: TcaDFMProcessNodeProc);
var
  Node: TcaDFMNode;
  ParentNode: TcaDFMNode;
  PropPrefix: string;
begin
  Node := TcaDFMNode(FNodes.Roots[0]);
  while Assigned(Node) do
    begin
      if Node.PropName <> '' then
        begin
          PropPrefix := '';
          ParentNode := TcaDFMNode(Node.Parent);
          while Assigned(ParentNode) do
            begin
              PropPrefix := ParentNode.ObjectName + '.' + PropPrefix;
              ParentNode := TcaDFMNode(ParentNode.Parent);
            end;
          AProc(Node, PropPrefix);
        end;
      Node := TcaDFMNode(Node.Next);
    end;
end;

procedure TcaDFMFile.Update;
begin
  UpdateText;
  UpdateNodes;
  UpdatePropList;
  UpdateBinary;
end;

procedure TcaDFMFile.UpdateNodes;
var
  Reader: IcaDFMReader;
begin
  Reader := TcaDFMReader.Create(FText, FNodes);
  Reader.Read;
end;

procedure TcaDFMFile.UpdateBinary;
var
  TxtStream: TMemoryStream;
begin
  TxtStream := Auto(TMemoryStream.Create).Instance;
  FText.SaveToStream(TxtStream);
  TxtStream.Position := 0;
  ObjectTextToBinary(TxtStream, FBinary);
  FBinary.Position := 0;
end;

procedure TcaDFMFile.UpdatePropList;
begin
  FPropList.Clear;
  TraverseNodes(UpdatePropListProc);
end;

procedure TcaDFMFile.UpdatePropListProc(ANode: TcaDFMNode; const APropPrefix: string);
begin
  FPropList.Add(APropPrefix + ANode.PropName + ' = ' + ANode.PropValue);
end;

procedure TcaDFMFile.UpdateText;
var
  Form: TForm;
  BinStream: TMemoryStream;
  TxtStream: TMemoryStream;
  TempOwner: TcaDFMTempOwner;
begin
  TempOwner := Auto(TcaDFMTempOwner.Create(nil)).Instance;
  Form := Auto(FFormClass.Create(TempOwner)).Instance;
  BinStream := Auto(TMemoryStream.Create).Instance;
  TxtStream := Auto(TMemoryStream.Create).Instance;
  BinStream.WriteComponent(Form);
  BinStream.Position := 0;
  ObjectBinaryToText(BinStream, TxtStream);
  TxtStream.Position := 0;
  // TxtStream.SaveToFile(Utils.AppPath + 'Modules\nnuNinahMain.txt');
  FText.LoadFromStream(TxtStream);
  // FText.LoadFromFile(Utils.AppPath + 'Modules\nnuNinahMain.txt');
end;

  // Property methods 

function TcaDFMFile.GetFormClass: TFormClass;
begin
  Result := FFormClass;
end;

function TcaDFMFile.GetNodes: TcaDFMNodes;
begin
  Result := FNodes;
end;

function TcaDFMFile.GetPropList: TStrings;
begin
  Result := FPropList;
end;

function TcaDFMFile.GetText: TStrings;
begin
  Result := FText;
end;

procedure TcaDFMFile.SetFormClass(const Value: TFormClass);
begin
  FFormClass := Value;
  Update;
end;

  // Event property methods 

function TcaDFMFile.GetOnTranslateString: TcaDFMTranslateStringEvent;
begin
  Result := FOnTranslateString;
end;

procedure TcaDFMFile.SetOnTranslateString(const Value: TcaDFMTranslateStringEvent);
begin
  FOnTranslateString := Value;
end;

  //---------------------------------------------------------------------------
  // TcaDFMNodes                                                               
  //---------------------------------------------------------------------------

  // Protected methods 

function TcaDFMNodes.CreateNode: TcaNode;
begin
  Result := TcaDFMNode.Create(nil);
end;

  //---------------------------------------------------------------------------
  // TcaDFMFiler                                                               
  //---------------------------------------------------------------------------

constructor TcaDFMFiler.Create(AText: TStrings; ANodes: TcaDFMNodes);
begin
  inherited Create;
  FText := AText;
  FNodes := ANodes;
end;

destructor TcaDFMFiler.Destroy;
begin
  inherited;
end;

  //---------------------------------------------------------------------------
  // TcaDFMReader                                                              
  //---------------------------------------------------------------------------

  // Interface methods - IcaDFMReader 

procedure TcaDFMReader.Read;
var
  Index: Integer;
  Level: Integer;
  Line: string;
  ObjectType: TcaDFMObjectType;
begin
  Nodes.Clear;
  for Index := 0 to Pred(Text.Count) do
    begin
      Line := Text[Index];
      Level := GetObjectLevel(Line);
      Line := Trim(Line);
      ObjectType := GetObjectType(Line);
      case ObjectType of
        otObject,
        otInherited,
        otInline:           ProcessObject(Line, ObjectType, Level);
        otEmptyCollection:  ProcessEmptyCollection(Line, Level);  
        otCollection:       ProcessCollection(Line, Level);
        otCollectionItem:   ProcessCollectionItem(Line, Level);
        otCollectionEnd:    ProcessCollectionEnd(Line, Level);
        otBinaryOneLine:    ProcessBinaryOneLine(Line, Level);
        otBinary:           ProcessBinary(Line, Level);
        otBinaryLine:       ProcessBinaryLine(Line, Level);
        otBinaryEnd:        ProcessBinaryEnd(Line, Level);
        otList:             ProcessList(Line, Level);
        otListItem:         ProcessListItem(Line, Level);
        otListEnd:          ProcessListEnd(Line, Level);
        otEmptyList:        ProcessEmptyList(Line, Level);
        otEnd:              ProcessEnd;
        otProperty:         ProcessProperty(Line);
      end;  
    end;
end;

  // Private methods 

function TcaDFMReader.GetObjectLevel(const ALine: string): Integer;
var
  Line: string;
begin
  Result := 0;
  Line := ALine;
  while (Line <> '') and (Line[1] = #32) do
    begin
      Inc(Result);
      Utils.DeleteFromStart(Line, 2);
    end;
end;

function TcaDFMReader.GetObjectType(const ALine: string): TcaDFMObjectType;
begin
  if Pos('object', ALine) = 1 then
    Result := otObject else
  if Pos('inherited', ALine) = 1 then
    Result := otInherited else
  if Pos('= <>', ALine) > 0 then
    Result := otEmptyCollection else
  if Pos('= <', ALine) > 0 then
    Result := otCollection else
  if Pos('= ()', ALine) > 0 then
    Result := otEmptyList else
  if Pos('= (', ALine) > 0 then
    Result := otList else
  if Utils.RightStr(ALine, 1) = ')' then
    Result := otListEnd else
  if (Pos('= {', ALine) > 0) and (Utils.RightStr(ALine, 1) = '}') then
    Result := otBinaryOneLine else
  if Pos('= {', ALine) > 0 then
    Result := otBinary else
  if IsBinaryLine(ALine) then
    Result := otBinaryLine else
  if Utils.RightStr(ALine, 1) = '}' then
    Result := otBinaryEnd else
  if Pos('item', ALine) = 1 then
    Result := otCollectionItem else
  if Pos('end>', ALine) = 1 then
    Result := otCollectionEnd else
  if Pos('end', ALine) = 1 then
    Result := otEnd else
  if Pos('inline', ALine) = 1 then
    Result := otInline else
  if Pos(' = ', ALine) > 0 then
    Result := otProperty
  else
    Result := otListItem;
end;

function TcaDFMReader.IsBinaryLine(const ALine: string): Boolean;
var
  Index: Integer;
begin
  Result := False;
  if (Length(ALine) = 64) and (Pos(#32, ALine) = 0) then
    begin
      Result := True;
      for Index := 1 to 64 do
        begin
          if not (ALine[Index] in ['A'..'Z', 'a'..'z', '0'..'9']) then
            begin
              Result := False;
              Break;
            end;
        end
    end;
end;

procedure TcaDFMReader.ProcessBinary(const ALine: string; ALevel: Integer);
var
  S: string;
begin
  S := ALine;
  Utils.DeleteFromChar(S, #32, True);
  FObjectNode := TcaDFMNode(Nodes.AddSub(FObjectNode, S));
  FObjectNode.Level := ALevel;
  FObjectNode.ObjectType := otBinary;
  FObjectNode.ObjectName := S;
end;

procedure TcaDFMReader.ProcessBinaryLine(const ALine: string; ALevel: Integer; IsEndItem: Boolean);
var
  StringsNode: TcaDFMNode;
  StringsValue: string;
begin
  StringsValue := ALine;
  StringsNode := TcaDFMNode(Nodes.AddSub(FObjectNode, StringsValue));
  StringsNode.PropName := '';
  StringsNode.PropValue := StringsValue;
  if IsEndItem then
    StringsNode.ObjectType := otBinaryEnd
  else
    StringsNode.ObjectType := otBinaryLine;
end;

procedure TcaDFMReader.ProcessBinaryEnd(const ALine: string; ALevel: Integer);
var
  StringsValue: string;
begin
  StringsValue := ALine;
  Utils.DeleteFromEnd(StringsValue, 1);
  ProcessBinaryLine(StringsValue, ALevel, True);
  ProcessEnd;
end;

procedure TcaDFMReader.ProcessBinaryOneLine(const ALine: string; ALevel: Integer);
begin
  ProcessProperty(ALine);
end;

procedure TcaDFMReader.ProcessCollection(const ALine: string; ALevel: Integer);
var
  S: string;
begin
  S := ALine;
  Utils.DeleteFromChar(S, #32, True);
  FObjectNode := TcaDFMNode(Nodes.AddSub(FObjectNode, S));
  FObjectNode.Level := ALevel;
  FObjectNode.ObjectType := otCollection;
  FObjectNode.ObjectName := S;
end;

procedure TcaDFMReader.ProcessCollectionItem(const ALine: string; ALevel: Integer);
begin
  ProcessObject(ALine, otCollectionItem, ALevel);
  FObjectNode.ObjectName := 'item';
  FObjectNode.Text := FObjectNode.ObjectName;
  FObjectNode.ObjectClass := '';
end;

procedure TcaDFMReader.ProcessCollectionEnd(const ALine: string; ALevel: Integer);
begin
  ProcessEnd;
  ProcessEnd;
end;

procedure TcaDFMReader.ProcessEmptyCollection(const ALine: string; ALevel: Integer);
begin
  ProcessProperty(ALine);  
end;

procedure TcaDFMReader.ProcessEmptyList(const ALine: string; ALevel: Integer);
begin
  ProcessProperty(ALine);
end;

procedure TcaDFMReader.ProcessEnd;
begin
  FObjectNode := TcaDFMNode(FObjectNode.Parent);
end;

procedure TcaDFMReader.ProcessObject(const ALine: string; AObjectType: TcaDFMObjectType; ALevel: Integer);
var
  ObjectClass: string;
  S: string;
begin
  S := ALine;
  Utils.DeleteUntilChar(S, #32, True);
  ObjectClass := S;
  Utils.DeleteUntilChar(ObjectClass, #32, True);
  Utils.DeleteFromChar(S, ':', True);
  while Assigned(FObjectNode) and (FObjectNode.Level >= ALevel) do
    FObjectNode := TcaDFMNode(FObjectNode.Parent);
  if Nodes.RootCount = 0 then
    FObjectNode := TcaDFMNode(Nodes.AddRoot(S))
  else
    FObjectNode := TcaDFMNode(Nodes.AddSub(FObjectNode, S));
  FObjectNode.Level := ALevel;
  FObjectNode.ObjectName := S;
  FObjectNode.ObjectType := AObjectType;
  FObjectNode.ObjectClass := ObjectClass;
end;

procedure TcaDFMReader.ProcessProperty(const ALine: string);
var
  PropName: string;
  PropNode: TcaDFMNode;
  PropValue: string;
  S: string;
begin
  S := ALine;
  Utils.DeleteFromChar(S, #32, True);
  PropName := S;
  S := ALine;
  Utils.DeleteUntilChar(S, #32, True);
  Utils.DeleteUntilChar(S, #32, True);
  PropValue := S;
  PropNode := TcaDFMNode(Nodes.AddSub(FObjectNode, Format('%s = %s', [PropName, PropValue])));
  PropNode.PropName := PropName;
  PropNode.PropValue := PropValue;
  PropNode.ObjectType := otProperty;
end;

procedure TcaDFMReader.ProcessList(const ALine: string; ALevel: Integer);
var
  S: string;
begin
  S := ALine;
  Utils.DeleteFromChar(S, #32, True);
  FObjectNode := TcaDFMNode(Nodes.AddSub(FObjectNode, S));
  FObjectNode.Level := ALevel;
  FObjectNode.ObjectType := otList;
  FObjectNode.ObjectName := S;
end;

procedure TcaDFMReader.ProcessListEnd(const ALine: string; ALevel: Integer);
var
  StringsValue: string;
begin
  StringsValue := ALine;
  Utils.DeleteFromEnd(StringsValue, 1);
  ProcessListItem(StringsValue, ALevel, True);
  ProcessEnd;
end;

procedure TcaDFMReader.ProcessListItem(const ALine: string; ALevel: Integer; IsEndItem: Boolean = False);
var
  StringsNode: TcaDFMNode;
  StringsValue: string;
begin
  StringsValue := ALine;
  StringsNode := TcaDFMNode(Nodes.AddSub(FObjectNode, StringsValue));
  StringsNode.PropName := '';
  StringsNode.PropValue := StringsValue;
  if IsEndItem then
    StringsNode.ObjectType := otListEnd
  else
    StringsNode.ObjectType := otListItem;
end;

  //---------------------------------------------------------------------------
  // TcaDFMWriter                                                              
  //---------------------------------------------------------------------------

  // Interface methods - IcaDFMWriter 

procedure TcaDFMWriter.Write;
var
  Node: TcaDFMNode;
  ObjectType: TcaDFMObjectType;
  Stack: IcaObjectStack;
begin
  Text.Clear;
  Stack := TcaObjectList.Create;
  Node := TcaDFMNode(Nodes.Roots[0]);
  while Assigned(Node) do
    begin
      ObjectType := Node.ObjectType;
      WriteEnds(Stack, Node);
      case ObjectType of
        otObject,
        otInherited,
        otInline:           WriteObject(Node);
        otCollection:       WriteCollection(Node);
        otCollectionItem:   WriteCollectionItem(Node);
        otBinary:           WriteBinary(Node);
        otBinaryLine:       WriteBinaryLine(Node);
        otBinaryEnd:        WriteBinaryEnd(Node);
        otList:             WriteList(Node);
        otListItem:         WriteListItem(Node);
        otListEnd:          WriteListEnd(Node);
        otProperty:         WriteProperty(Node);
      end;
      Node := TcaDFMNode(Node.Next);
    end;
  WriteRemainingEnds(Stack);
end;

  // Private methods 

function TcaDFMWriter.GetObjectTypeString(AObjectType: TcaDFMObjectType): string;
begin
  case AObjectType of
    otObject:     Result := 'object';
    otInherited:  Result := 'inherited';
    otInline:     Result := 'inline';
  else
    Result := '';
  end;
end;

procedure TcaDFMWriter.WriteBinary(ANode: TcaDFMNode);
var
  NodeText: string;
begin
  NodeText := Format('%s = {', [ANode.ObjectName]);
  WriteText(ANode.Level, NodeText);
end;

procedure TcaDFMWriter.WriteBinaryLine(ANode: TcaDFMNode);
begin
  WriteText(ANode.Level, ANode.PropValue);
end;

procedure TcaDFMWriter.WriteBinaryEnd(ANode: TcaDFMNode);
begin
  WriteText(ANode.Level, ANode.PropValue + '}');
end;

procedure TcaDFMWriter.WriteCollection(ANode: TcaDFMNode);
var
  CollName: string;
  NodeText: string;
begin
  CollName := ANode.ObjectName;
  Utils.DeleteFromChar(CollName, '[', True);
  NodeText := Format('%s = <', [CollName]);
  WriteText(ANode.Level, NodeText);
end;

procedure TcaDFMWriter.WriteCollectionItem(ANode: TcaDFMNode);
begin
  WriteText(ANode.Level, 'item');
end;

procedure TcaDFMWriter.WriteEnd(ANode: TcaDFMNode);
var
  EndStr: string;
begin
  EndStr := 'end';
  if (ANode.Parent <> nil)
  and (TcaDFMNode(ANode.Parent).ObjectType = otCollection)
  and (ANode.Parent.LastSub = ANode) then
    EndStr := EndStr + '>';
  WriteText(ANode.Level, EndStr);
end;

procedure TcaDFMWriter.WriteEnds(const AStack: IcaObjectStack; ANode: TcaDFMNode);
begin
  if ANode.ObjectType in [otObject, otInherited, otInline, otCollectionItem, otProperty, otList, otCollection, otBinary] then
    begin
      while AStack.HasItems and (TcaDFMNode(AStack.Peek).Level >= ANode.Level) do
        WriteEnd(TcaDFMNode(AStack.Pop));
      if not (ANode.ObjectType in [otProperty, otList, otCollection, otBinary]) then
        AStack.Push(ANode);
    end;
end;

procedure TcaDFMWriter.WriteObject(ANode: TcaDFMNode);
var
  NodeText: string;
  ObjectTypeStr: string;
begin
  ObjectTypeStr := GetObjectTypeString(ANode.ObjectType);
  NodeText := Format('%s %s: %s', [ObjectTypeStr, ANode.ObjectName, ANode.ObjectClass]);
  WriteText(ANode.Level, NodeText);
end;

procedure TcaDFMWriter.WriteProperty(ANode: TcaDFMNode);
var
  PropText: string;
begin
  PropText := Format('%s = %s', [ANode.PropName, ANode.PropValue]);
  WriteText(ANode.Level, PropText);
end;

procedure TcaDFMWriter.WriteRemainingEnds(const AStack: IcaObjectStack);
begin
  while AStack.HasItems do
    WriteEnd(TcaDFMNode(AStack.Pop));
end;

procedure TcaDFMWriter.WriteList(ANode: TcaDFMNode);
var
  NodeText: string;
begin
  NodeText := Format('%s = (', [ANode.ObjectName]);
  WriteText(ANode.Level, NodeText);
end;

procedure TcaDFMWriter.WriteListEnd(ANode: TcaDFMNode);
begin
  WriteText(ANode.Level, ANode.PropValue + ')');
end;

procedure TcaDFMWriter.WriteListItem(ANode: TcaDFMNode);
begin
  WriteText(ANode.Level, ANode.PropValue);
end;

procedure TcaDFMWriter.WriteText(ALevel: Integer; const AText: string);
var
  Indent: string;
begin
  Indent := Utils.BuildString(#32, ALevel * 2);
  Text.Add(Indent + AText);
end;

  //---------------------------------------------------------------------------
  // CoFormUtilsFactory                                                        
  //---------------------------------------------------------------------------

class function CoFormUtilsFactory.Instance: IcaFormUtils;
const
  FInstance: IcaFormUtils = nil;
begin
  if not Assigned(FInstance) then
    FInstance := TcaFormUtils.Create;
  Result := FInstance;
end;

  //---------------------------------------------------------------------------
  // Initialization                                                            
  //---------------------------------------------------------------------------

initialization
  FormUtils := CoFormUtilsFactory.Instance;

end.
