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



unit caDirListView;

interface

uses

  // Standard Delphi units 
  SysUtils,
  Classes,
  Controls,
  ComCtrls,

  // ca units 
  caUtils,
  caDir;

type

  //----------------------------------------------------------------------------
  // TcaDirListView                                                             
  //----------------------------------------------------------------------------

  TcaDirListView = class(TCustomListView)
  private
    // Private fields 
    FDir: TcaDir;
    // Property fields 
    FAutoScan: Boolean;
    FExclude: string;
    FFileSpec: string;
    FFolder: string;
    // Event property fields 
    FOnAddNode: TcaDirAddNodeEvent;
    // Property methods 
    function GetFileNodes: TcaDirNodes;
    procedure SetExclude(const Value: string);
    procedure SetFileSpec(const Value: string);
    procedure SetFolder(const Value: string);
    // Private methods 
    procedure AddColumn(const ACaption: string);
    procedure AddNodeEvent(Sender: TObject; AItem: TcaDirNode; var AAccept: Boolean);
    procedure UpdateDir;
  protected
    // Protected methods 
    function OwnerDataFetch(Item: TListItem; Request: TItemRequest): Boolean; override;
    procedure ColClick(Column: TListColumn); override;
    procedure DoAddNode(AItem: TcaDirNode; var AAccept: Boolean); virtual;
    procedure Resize; override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
    procedure Scan;
    // Public properties 
    property FileNodes: TcaDirNodes read GetFileNodes;
  published
    // Published properties 
    property AutoScan: Boolean read FAutoScan write FAutoScan;
    property Exclude: string read FExclude write SetExclude;
    property FileSpec: string read FFileSpec write SetFileSpec;
    property Folder: string read FFolder write SetFolder;
    // Published event properties 
    property OnAddNode: TcaDirAddNodeEvent read FOnAddNode write FOnAddNode;
    // Promoted properties 
    property Action;
    property Align;
    property AllocBy;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color;
//    property Columns;
    property ColumnClick;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FlatScrollBars;
    property FullDrag;
    property GridLines;
    property HideSelection;
    property HotTrack;
    property HotTrackStyles;
    property HoverTime;
    property IconOptions;
    property Items;
    property LargeImages;
    property MultiSelect;
//    property OwnerData;
    property OwnerDraw;
    property ReadOnly default False;
    property RowSelect;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowWorkAreas;
    property ShowHint;
    property SmallImages;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
//    property ViewStyle;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnCompare;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSubItemImage;
    property OnDragDrop;
    property OnDragOver;
    property OnInfoTip;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

  //----------------------------------------------------------------------------
  // TcaDirListView                                                             
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaDirListView.Create(AOwner: TComponent);
begin
  inherited;
  OwnerData := True;
  ViewStyle := vsReport;
  AddColumn('Folder');
  AddColumn('Filename');
  AddColumn('Last Changed on');
  FDir := TcaDir.Create;
  FDir.OnAddNode := AddNodeEvent;
end;

destructor TcaDirListView.Destroy;
begin
  FDir.Free;
  inherited;
end;

  // Public methods 

procedure TcaDirListView.Scan;
begin
  Items.BeginUpdate;
  try
    FDir.Scan;
    FDir.SortByLastWriteTime;
    Items.Count := FDir.Count;
  finally
    Items.EndUpdate;
  end;
end;

  // Protected methods 

function TcaDirListView.OwnerDataFetch(Item: TListItem; Request: TItemRequest): Boolean;
var
  DirItem: TcaDirNode;
begin
  DirItem := FDir[Item.Index];
  Item.Caption := DirItem.Folder;
  Item.SubItems.Add(DirItem.Filename);
  Item.SubItems.Add(FormatDateTime('', DirItem.LastWriteTime));
  Item.Data := DirItem;
  Result := True;
end;

procedure TcaDirListView.ColClick(Column: TListColumn);
begin
  case Column.Index of
    0:  FDir.SortByFolder;
    1:  FDir.SortByFilename;
    2:  FDir.SortByLastWriteTime;
  end;
  Invalidate;
end;

procedure TcaDirListView.DoAddNode(AItem: TcaDirNode; var AAccept: Boolean);
begin
  if Assigned(FOnAddNode) then FOnAddNode(Self, AItem, AAccept);
end;

procedure TcaDirListView.Resize;
var
  AvailWidth: Integer;
begin
  inherited;
  Column[2].Width := 150;
  AvailWidth := ClientWidth - Column[2].Width;
  Column[0].Width := AvailWidth div 2;
  Column[1].Width := AvailWidth div 2;
end;

  // Private methods 

procedure TcaDirListView.AddColumn(const ACaption: string);
begin
  Columns.Add.Caption := ACaption;
end;

procedure TcaDirListView.AddNodeEvent(Sender: TObject; AItem: TcaDirNode; var AAccept: Boolean);
begin
  DoAddNode(AItem, AAccept);
end;

procedure TcaDirListView.UpdateDir;
begin
  FDir.Folder := FFolder;
  FDir.FileSpec := FFileSpec;
  FDir.Exclude := FExclude;
  if not (csLoading in ComponentState) and FAutoScan then Scan;
end;

  // Property methods 

function TcaDirListView.GetFileNodes: TcaDirNodes;
begin
  Result := FDir.Nodes;
end;

procedure TcaDirListView.SetExclude(const Value: string);
begin
  if Value <> FExclude then
    begin
      FExclude := Value;
      UpdateDir;
    end;
end;

procedure TcaDirListView.SetFileSpec(const Value: string);
begin
  if Value <> FFileSpec then
    begin
      FFileSpec := Value;
      UpdateDir;
    end;
end;

procedure TcaDirListView.SetFolder(const Value: string);
begin
  if Value <> FFolder then
    begin
      FFolder := Value;
      UpdateDir;
    end;
end;

end.
