unit caTreeView;

{$INCLUDE ca.inc}

interface

uses

  // standard Delphi units...
  SysUtils,
  Classes,
  Windows,
  Messages,
  Graphics,
  Controls,
  ComCtrls,
  CommCtrl,

  // ca units...
  caClasses,
  caUtils,
  caLog,
  caNodes;

type

  //---------------------------------------------------------------------------
  // TcaTreeNode
  //---------------------------------------------------------------------------

  TcaTreeNode = class(TTreeNode)
  private
    // private members...
    FBold: Boolean;
    FPartChecked: Boolean;
    // property methods...
    function GetChecked: Boolean;
    procedure SetBold(const Value: Boolean);
    procedure SetChecked(const Value: Boolean);
    procedure SetPartChecked(const Value: Boolean);
  public
    // public properties...
    property Bold: Boolean read FBold write SetBold;
    property Checked: Boolean read GetChecked write SetChecked;
    property PartChecked: Boolean read FPartChecked write SetPartChecked;
  end;

  //---------------------------------------------------------------------------
  // TcaTreeView
  //---------------------------------------------------------------------------

  TcaTreeViewAddNodeEvent = procedure(Sender: TObject; ANode: TcaNode; ATreeNode: TcaTreeNode) of object;

  TcaTreeViewCheckBoxClickEvent = procedure(Sender: TObject; ATreeNode: TcaTreeNode) of object;

  TcaTreeView = class(TTreeView)
  private
    // private members...
    FNodes: IcaNodes;
    FUpdating: Boolean;
    // property fields...
    FUseCheckBoxes: Boolean;
    FUseLinkedChecking: Boolean;
    // event property fields...
    FOnAddNode: TcaTreeViewAddNodeEvent;
    FOnCheckBoxClicked: TcaTreeViewCheckBoxClickEvent;
    // private methods...
    // function GetNodeFromItem(const Item: TTVItem): TTreeNode;
    procedure AddTreeNode(ANode: TcaNode; ATreeNode: TcaTreeNode);
    procedure UpdateChildrenCheckState(ATreeNode: TcaTreeNode);
    procedure UpdateParentsCheckState(ATreeNode: TcaTreeNode);
    procedure UpdateNodes;
    // property methods...
    function GetItem(Index: Integer): TcaTreeNode;
    procedure SetUseCheckBoxes(const Value: Boolean);
    procedure SetUseLinkedChecking(const Value: Boolean);
    // component notification message handlers...
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    // windows message handlers...
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    // event handler - THIS IS A HACK because there seems to be no way to
    //                 use the internal virtual methods...
    procedure AdvancedCustomDrawItemEvent(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
  protected
    // protected virtual methods...
    function CreateNode: TTreeNode; override;
//     function CustomDrawItem(Node: TTreeNode; State: TCustomDrawState;
//       Stage: TCustomDrawStage; var PaintImages: Boolean): Boolean; override;
    function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean; override;
    procedure Added(Node: TTreeNode); {$IFDEF D7_UP}override{$ELSE}virtual{$ENDIF};
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoAddNode(ANode: TcaNode; ATreeNode: TcaTreeNode); virtual;
    procedure DoCheckBoxClicked; virtual;
    // protected static methods...
    function GetNodeCheckedState(ATreeNode: TcaTreeNode): Boolean;
    procedure RepaintNode(ATreeNode: TcaTreeNode);
    procedure SetNodeBoldState(ATreeNode: TcaTreeNode; IsBold: Boolean);
    procedure SetNodeCheckedState(ATreeNode: TcaTreeNode; IsChecked: Boolean);
    procedure UpdateNodeCheckLinkStates(ATreeNode: TcaTreeNode);
  public
    // lifetime...
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // public methods...
    procedure SetNodes(const Value: IcaNodes);
    // public properties...
    property Item[Index: Integer]: TcaTreeNode read GetItem;
    property Items;
  published
    // published properties...
    property UseCheckBoxes: Boolean read FUseCheckBoxes write SetUseCheckBoxes;
    property UseLinkedChecking: Boolean read FUseLinkedChecking write SetUseLinkedChecking;
    // event properties...
    property OnAddNode: TcaTreeViewAddNodeEvent read FOnAddNode write FOnAddNode;
    property OnCheckBoxClicked: TcaTreeViewCheckBoxClickEvent read FOnCheckBoxClicked write FOnCheckBoxClicked;
  end;

  //---------------------------------------------------------------------------
  // TcaTreeNodesProc
  //---------------------------------------------------------------------------

  TcaTreeNodesProc = procedure(ANodes: TTreeNodes) of object;

implementation

//---------------------------------------------------------------------------
// TcaTreeNode;
//---------------------------------------------------------------------------

function TcaTreeNode.GetChecked: Boolean;
begin
  Result := TcaTreeView(TreeView).GetNodeCheckedState(Self);
end;

procedure TcaTreeNode.SetBold(const Value: Boolean);
begin
  if Value <> FBold then
    begin
      FBold := Value;
      TcaTreeView(TreeView).SetNodeBoldState(Self, FBold);
    end;
end;

procedure TcaTreeNode.SetChecked(const Value: Boolean);
begin
  if Value <> GetChecked then
    begin
      TcaTreeView(TreeView).SetNodeCheckedState(Self, Value);
      SetPartChecked(False);
      TcaTreeView(TreeView).UpdateNodeCheckLinkStates(Self);
    end;
end;

procedure TcaTreeNode.SetPartChecked(const Value: Boolean);
begin
  if Value <> FPartChecked then
    begin
      FPartChecked := Value;
      TcaTreeView(TreeView).RepaintNode(Self);
    end;
end;

//---------------------------------------------------------------------------
// TcaTreeView                                                               
//---------------------------------------------------------------------------

// lifetime...

constructor TcaTreeView.Create(AOwner: TComponent);
begin
  inherited;
  // HACK - there seems to be no way to use the internal virtual methods...
  Self.OnAdvancedCustomDrawItem := AdvancedCustomDrawItemEvent;
end;

destructor TcaTreeView.Destroy;
begin
  inherited;
end;

// public methods...

procedure TcaTreeView.SetNodes(const Value: IcaNodes);
begin
  FNodes := Value;
  UpdateNodes;
end;

// protected virtual methods...

function TcaTreeView.CreateNode: TTreeNode;
begin
  Result := TcaTreeNode.Create(Items);
end;

// This is the virtual method that should be fired before the AdvancedCustomDrawItem
// event is fired. It isn't...
//
// function TcaTreeView.CustomDrawItem(Node: TTreeNode; State: TCustomDrawState;
//   Stage: TCustomDrawStage; var PaintImages: Boolean): Boolean;
// var
//   R: TRect;
// begin
//   Result := False;
//   if (Stage = cdPostPaint) and TcaTreeNode(Node).PartChecked then
//     begin
//       R := Node.DisplayRect(False);
//       R.Right := R.Left + 10;
//       Canvas.Brush.Color := clGray;
//       Canvas.FillRect(R);
//       Result := True;
//     end;
// end;

function TcaTreeView.IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean;
begin
  Result := inherited IsCustomDrawn(Target, Stage);
//  if (Stage = cdPostPaint) and ((Target = dtItem) or (Target = dtControl)) then
//    Result := True; 
end;

procedure TcaTreeView.Added(Node: TTreeNode);
var
  ANode: TcaNode;
  ParentNode: TcaNode;
  ParentTreeNode: TTreeNode;
begin
  inherited;
  if Assigned(FNodes) then
    begin
      ANode := nil;
      if not FUpdating then
        begin
          ParentTreeNode := Node.Parent;
          if Assigned(ParentTreeNode) then
            begin
              ParentNode := TcaNode(ParentTreeNode.Data);
              if Assigned(ParentNode) then
                ANode := FNodes.AddSub(ParentNode, Node.Text);
            end
          else
            ANode := FNodes.AddRoot(Node.Text);
          if Assigned(ANode) then
            Node.Data := ANode;
        end;
    end;
end;

procedure TcaTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if FUseCheckBoxes then
    Params.Style := Params.Style or TVS_CHECKBOXES;
end;

procedure TcaTreeView.DoAddNode(ANode: TcaNode; ATreeNode: TcaTreeNode);
begin
  if Assigned(FOnAddNode) then FOnAddNode(Self, ANode, ATreeNode);
end;

procedure TcaTreeView.DoCheckBoxClicked;
begin
  if Assigned(FOnCheckBoxClicked) then
    FOnCheckBoxClicked(Self, TcaTreeNode(Selected));
end;

// component notification message handlers...

procedure TcaTreeView.CNNotify(var Message: TWMNotify);
// var
//   tvi: TTVItem;
//   Node: TTreeNode;
//   DrawMsg: PNMCustomDraw;
//   PaintImages: Boolean;
begin
  inherited;
//   if Message.NMHdr^.code = NM_CUSTOMDRAW then
//     begin
//       DrawMsg := PNMCustomDraw(Message.NMHdr);
//       if (DrawMsg^.dwDrawStage or CDDS_ITEM) = CDDS_ITEMPOSTPAINT then
//         begin
//           FillChar(tvi, SizeOf(tvi), 0);
//           tvi.hItem := HTREEITEM(DrawMsg^.dwItemSpec);
//           Node := GetNodeFromItem(tvi);
//           try
//             Canvas.Handle := DrawMsg^.hdc;
//             Canvas.Font := Font;
//             Canvas.Brush := Brush;
//             if Assigned(Node) and IsCustomDrawn(dtItem, cdPostPaint) then
//               CustomDrawItem(Node, TCustomDrawState(Word(DrawMsg^.uItemState)), cdPostPaint, PaintImages);
//           finally
//             Canvas.Handle := 0;
//           end;
//         end;
//     end;
end;

// windows message handlers...

procedure TcaTreeView.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
  if Message.CharCode = $20 then
    begin
      UpdateNodeCheckLinkStates(TcaTreeNode(Selected));
      DoCheckBoxClicked;
      Message.Result := 0;
    end;
end;

procedure TcaTreeView.WMLButtonDown(var Message: TWMLButtonDown);
var
  Node: TcaTreeNode;
begin
  inherited;
  if htOnStateIcon in GetHitTestInfoAt(Message.XPos, Message.YPos) then
    begin
      Node := TcaTreeNode(GetNodeAt(Message.XPos, Message.YPos));
      if Assigned(Node) then
        begin
          Node.Focused := True;
          Node.Selected := True;
          if Node.Checked then
            Node.PartChecked := False;
        end;
      if FUseLinkedChecking then
        UpdateNodeCheckLinkStates(TcaTreeNode(Selected));
      DoCheckBoxClicked;
    end;
end;

// event handler - THIS IS A HACK because there seems to be no way to
//                 use the internal virtual methods...

procedure TcaTreeView.AdvancedCustomDrawItemEvent(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  R: TRect;
begin
  if (Stage = cdPostPaint) and (not TcaTreeNode(Node).Checked) then
    begin
      R := Node.DisplayRect(False);
      R.Left := R.Left + (Node.Level * Indent) + 26;
      R.Right := R.Left + 9;
      R.Top := R.Top + 4;
      R.Bottom := R.Top + 9;
      if TcaTreeNode(Node).PartChecked then
        Canvas.Brush.Color := clGray
      else
        Canvas.Brush.Color := clWhite;
      Canvas.FillRect(R);
      DefaultDraw := True;
    end;
end;

// protected static methods...

function TcaTreeView.GetNodeCheckedState(ATreeNode: TcaTreeNode): Boolean;
var
  tvi: TTVItem;
begin
  Result := False;
  if Assigned(ATreeNode) then
    begin
      FillChar(tvi, SizeOf(tvi), 0);
      tvi.hItem := ATreeNode.ItemID;
      tvi.mask := TVIF_HANDLE or TVIF_STATE;
      tvi.stateMask := TVIS_STATEIMAGEMASK;
      TreeView_GetItem(ATreeNode.Handle, tvi);
      Result := Pred(tvi.state shr 12) > 0;
    end;
end;

procedure TcaTreeView.RepaintNode(ATreeNode: TcaTreeNode);
var
  R: TRect;
begin
  while (ATreeNode <> nil) and not ATreeNode.IsVisible do
    ATreeNode := TcaTreeNode(ATreeNode.Parent);
  if ATreeNode <> nil then
    begin
      R := ATreeNode.DisplayRect(False);
      InvalidateRect(Handle, @R, True);
    end;
end;

procedure TcaTreeView.SetNodeBoldState(ATreeNode: TcaTreeNode; IsBold: Boolean);
var
  tvi: TTVItem;
begin
  if Assigned(ATreeNode) then
    begin
      FillChar(tvi, SizeOf(tvi), 0);
      tvi.hItem := ATreeNode.ItemID;
      tvi.mask := TVIF_STATE;
      tvi.stateMask := TVIS_BOLD;
      tvi.state := TVIS_BOLD * Ord(IsBold);
      TreeView_SetItem(ATreeNode.Handle, tvi);
    end;
end;

procedure TcaTreeView.SetNodeCheckedState(ATreeNode: TcaTreeNode; IsChecked: Boolean);
var
  tvi: TTVItem;
const
  StateIndexes: array[Boolean] of Integer = (1, 2);
begin
  if Assigned(ATreeNode) then
    begin
      FillChar(tvi, SizeOf(tvi), 0);
      tvi.hItem := ATreeNode.ItemID;
      tvi.mask := TVIF_HANDLE or TVIF_STATE;
      tvi.stateMask := TVIS_STATEIMAGEMASK;
      tvi.state := IndexToStateImageMask(StateIndexes[IsChecked]);
      TreeView_SetItem(ATreeNode.Handle, tvi);
    end;
end;

procedure TcaTreeView.UpdateNodeCheckLinkStates(ATreeNode: TcaTreeNode);
begin
  Items.BeginUpdate;
  try
    if Assigned(ATreeNode) then
      begin
        UpdateChildrenCheckState(ATreeNode);
        UpdateParentsCheckState(ATreeNode);
      end;
  finally
    Items.EndUpdate;
  end;
end;

// private methods...

// function TcaTreeView.GetNodeFromItem(const Item: TTVItem): TTreeNode;
// begin
//   Result := nil;
//   if Items <> nil then
//     begin
//       if (Item.state and TVIF_PARAM) <> 0 then
//         Result := Pointer(Item.lParam)
//       else
//         Result := Items.GetNode(Item.hItem);
//     end;
// end;

procedure TcaTreeView.AddTreeNode(ANode: TcaNode; ATreeNode: TcaTreeNode);
var
  TreeNode: TTreeNode;
  SubNode: TcaNode;
begin
  if not Assigned(ANode) then
    if FNodes.RootCount > 0 then
      ANode := FNodes.Roots[0];
  if Assigned(ANode) then
    begin
      TreeNode := Items.AddChild(ATreeNode, ANode.Text);
      TreeNode.Data := ANode;
      TreeNode.ImageIndex := 0;
      TreeNode.SelectedIndex := 0;
      DoAddNode(ANode, TcaTreeNode(TreeNode));
      SubNode := ANode.FirstSub;
      while Assigned(SubNode) do
        begin
          AddTreeNode(SubNode, TcaTreeNode(TreeNode));
          SubNode := SubNode.NextIso;
        end;
    end;
end;

procedure TcaTreeView.UpdateChildrenCheckState(ATreeNode: TcaTreeNode);
var
  SubTreeNode: TcaTreeNode;
begin
  SubTreeNode := TcaTreeNode(ATreeNode.getFirstChild);
  while Assigned(SubTreeNode) and SubTreeNode.HasAsParent(ATreeNode) do
    begin
      SubTreeNode.Checked := ATreeNode.Checked;
      SubTreeNode := TcaTreeNode(SubTreeNode.GetNext);
    end;
end;

procedure TcaTreeView.UpdateParentsCheckState(ATreeNode: TcaTreeNode);
var
  ParentTreeNode: TcaTreeNode;
  SubTreeNode: TcaTreeNode;
  ChildCount: Integer;
  CheckedCount: Integer;
  PartCheckedCount: Integer;
begin
  ParentTreeNode := TcaTreeNode(ATreeNode.Parent);
  while Assigned(ParentTreeNode) do
    begin
      ChildCount := 0;
      CheckedCount := 0;
      PartCheckedCount := 0;
      SubTreeNode := TcaTreeNode(ParentTreeNode.getFirstChild);
      while Assigned(SubTreeNode) do
        begin
          if SubTreeNode.Parent = ParentTreeNode then
            begin
              Inc(ChildCount);
              if SubTreeNode.Checked then
                Inc(CheckedCount);
              if SubTreeNode.PartChecked then
                Inc(PartCheckedCount);
            end;
          SubTreeNode := TcaTreeNode(ParentTreeNode.GetNextChild(SubTreeNode));
        end;
      if ChildCount = 0 then
        begin
          ParentTreeNode.Checked := False;
          ParentTreeNode.PartChecked := False;
        end
      else
        begin
          if CheckedCount = ChildCount then
            begin
              ParentTreeNode.Checked := True;
              ParentTreeNode.PartChecked := False;
            end
          else
            begin
              if (CheckedCount > 0) or (PartCheckedCount > 0) then
                begin
                  ParentTreeNode.Checked := False;
                  ParentTreeNode.PartChecked := True;
                end
              else
                begin
                  ParentTreeNode.Checked := False;
                  ParentTreeNode.PartChecked := False;
                end;
            end;
        end;
      ParentTreeNode := TcaTreeNode(ParentTreeNode.Parent);
    end;  
end;

procedure TcaTreeView.UpdateNodes;
var
  Index: Integer;
  RootNode: TcaNode;
begin
  if Assigned(FNodes) then
    begin
      Items.BeginUpdate;
      FUpdating := True;
      try
        Items.Clear;
        for Index := 0 to Pred(FNodes.RootCount) do
          begin
            RootNode := FNodes.Roots[Index];
            AddTreeNode(RootNode, nil);
          end;
      finally
        Items.EndUpdate;
        FUpdating := False;
      end;
    end;
end;

// property methods...

function TcaTreeView.GetItem(Index: Integer): TcaTreeNode;
begin
  Result := TcaTreeNode(Items[Index]);
end;

procedure TcaTreeView.SetUseCheckBoxes(const Value: Boolean);
begin
  if Value <> FUseCheckBoxes then
    begin
      FUseCheckBoxes := Value;
      // need to force the CreateParams call...
      if not (csDesigning in ComponentState) then
        RecreateWnd;
    end;  
end;

procedure TcaTreeView.SetUseLinkedChecking(const Value: Boolean);
begin
  if Value <> FUseLinkedChecking then
    begin
      FUseLinkedChecking := Value;
      if FUseLinkedChecking and (not (csDesigning in ComponentState)) then
        UpdateNodeCheckLinkStates(TcaTreeNode(Selected));
    end;  
end;

end.
