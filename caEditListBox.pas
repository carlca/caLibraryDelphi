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


unit caEditListBox;

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
  StdCtrls,
  Math,

  // ca units 
  caClasses,
  caConsts,
  caLog;

type

  TcaEditListBoxAddItemEvent = procedure(Sender: TObject; const AText: string; var AObject: TObject) of object;

  TcaEditListBoxEditItemEvent = procedure(Sender: TObject; const AText: string; AObject: TObject) of object;

  TcaEditListBoxDeleteItemEvent = procedure(Sender: TObject; const AText: string; AObject: TObject) of object;

  TcaEditListBoxItemIndexChangedEvent = procedure(Sender: TObject; AItemIndex: Integer;
    const AText: string; AObject: TObject) of object;

  TcaEditListBox = class;

  //---------------------------------------------------------------------------
  // TcaEditListDragImage                                                      
  //---------------------------------------------------------------------------

  TcaEditListDragImage = class(TCustomControl)
  private
    // Private fields 
    FColor: TColor;
    // Property fields 
    FText: string;
    // Property methods 
    procedure SetText(const Value: string);
  public
    // Public methods 
    procedure Paint; override;
  published
    // Published properties 
    property Color: TColor read FColor write FColor;
    property Text: string read FText write SetText;
  end;

  //---------------------------------------------------------------------------
  // TcaEditListBoxInplaceEditor                                               
  //---------------------------------------------------------------------------

  TcaEditListBoxInplaceEditor = class(TEdit)
  private
    // Private fields 
    FListbox: TcaEditListBox;
  protected
    // Protected methods 
    procedure KeyPress(var Key: Char); override;
  public
    // Public properties 
    property Listbox: TcaEditListBox read FListbox write FListbox;
  end;

  //---------------------------------------------------------------------------
  // TcaEditListBox                                                            
  //---------------------------------------------------------------------------

  TcaEditListBox = class(TListbox)
  private
    // Private fields 
    FAllowDeleteKey: Boolean;
    FAllowInsertAtEnd: Boolean;
    FDownIndex: Integer;
    FDragging: Boolean;
    FDragImage: TcaEditListDragImage;
    FItems: TStrings;
    FInplaceEdit: TcaEditListBoxInplaceEditor;
    FIsEditing: Boolean;
    FMouseIsDown: Boolean;
    // Event property fields 
    FOnAddItem: TcaEditListBoxAddItemEvent;
    FOnDeleteItem: TcaEditListBoxDeleteItemEvent;
    FOnEditItem: TcaEditListBoxEditItemEvent;
    FOnItemIndexChanged: TcaEditListBoxItemIndexChangedEvent;
    // Property methods 
    function GetDragColor: TColor;
    function GetItems: TStrings;
    procedure SetDragColor(const Value: TColor);
    procedure SetItems(const Value: TStrings);
    // Private methods 
    procedure CreateDragImage;
    procedure CreateInplaceEditor;
    procedure CreateItems;
    procedure EndEditing(SaveNeeded: Boolean);
    procedure InitializeDragList(const S: string);
    procedure StartEditing;
    procedure SyncItems;
    // Event handlers 
    procedure InplaceEditExitEvent(Sender: TObject);
    procedure ItemChangedEvent(Sender: TObject);
  protected
    // Protected methods 
    procedure Click; override;
    procedure DblClick; override;
    procedure DoAddItem(Index: Integer); virtual;
    procedure DoDeleteItem(Index: Integer); virtual;
    procedure DoEditItem(Index: Integer); virtual;
    procedure DoItemIndexChanged; virtual;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    // Create/Destroy 
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    // public methods 
    procedure DeleteSelectedItems;
    procedure EditCurrentItem;
  published
    // Published properties 
    property AllowDeleteKey: Boolean read FAllowDeleteKey write FAllowDeleteKey;
    property AllowInsertAtEnd: Boolean read FAllowInsertAtEnd write FAllowInsertAtEnd;
    property DragColor: TColor read GetDragColor write SetDragColor;
    property Items: TStrings read GetItems write SetItems;
    // Published event properties 
    property OnAddItem: TcaEditListBoxAddItemEvent read FOnAddItem write FOnAddItem;
    property OnDeleteItem: TcaEditListBoxDeleteItemEvent read FOnDeleteItem write FOnDeleteItem;
    property OnEditItem: TcaEditListBoxEditItemEvent read FOnEditItem write FOnEditItem;
    property OnItemIndexChanged: TcaEditListBoxItemIndexChangedEvent read FOnItemIndexChanged write FOnItemIndexChanged;
  end;

implementation

uses Types;

const
  cNewItem      = '';
  
  //```````````````````````````````````````````````````````````````````````````
  // TcaEditListBox                                                            
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaEditListBox.Create(AOwner: TComponent);
begin
  inherited;
  CreateItems;
  CreateInplaceEditor;
  CreateDragImage;
  FDragging := False;
  FMouseIsDown := False;
  FAllowDeleteKey := True;
  FAllowInsertAtEnd := True;
end;

destructor TcaEditListBox.Destroy;
begin
  FInplaceEdit.Free;
  FItems.Free;
  inherited;
end;

  // public methods 

procedure TcaEditListBox.EditCurrentItem;
begin
  if ItemIndex <> -1 then
    StartEditing;
end;

procedure TcaEditListBox.DeleteSelectedItems;
var
  FirstSelected: Integer;
  Index: Integer;
  RemainingList: TStrings;
begin
  RemainingList := Auto(TStringList.Create).Instance;
  FirstSelected := -1;
  for Index := 0 to Pred(Items.Count) do
    begin
      if Selected[Index] then
        begin
          if FirstSelected = -1 then
            FirstSelected := Index;
        end
      else
        RemainingList.AddObject(Items[Index], Items.Objects[Index]);
    end;
  Items.Assign(RemainingList);
  Index := Min(Pred(Items.Count), FirstSelected);
  if Index >= 0 then
    begin
      ItemIndex := ItemIndex;
      Selected[Index] := True;
    end;
end;

  // Protected methods 

procedure TcaEditListBox.Click;
var
  Index1: Integer;
  Index2: Integer;
begin
  if FDragging then
    begin
      Index1 := FDownIndex;
      Index2 := ItemIndex;
      if (Index1 <> -1) and (Index2 <> -1) then
        FItems.Move(Index1, Index2);
      ItemIndex := Index2;
    end;
  FDragging := False;
  FMouseIsDown := False;
  FDragImage.Visible := False;
  inherited;
end;

procedure TcaEditListBox.DblClick;
begin
  FDragImage.Visible := False;
  FDragging := False;
  FMouseIsDown := False;
  if Assigned(OnDblClick) then
    OnDblClick(Self)
  else
    begin
      if ItemIndex <> -1 then
        StartEditing
    end;
end;

procedure TcaEditListBox.DoAddItem(Index: Integer);
var
  AObject: TObject;
begin
  if Assigned(FOnAddItem) then
    begin
      AObject := nil;
      FOnAddItem(Self, FItems[Index], AObject);
      FItems.Objects[Index] := AObject;
    end;
end;

procedure TcaEditListBox.DoDeleteItem(Index: Integer);
begin
  if Assigned(FOnDeleteItem) then
    FOnDeleteItem(Self, FItems[Index], FItems.Objects[Index]);
end;

procedure TcaEditListBox.DoEditItem(Index: Integer);
begin
  if Assigned(FOnEditItem) then
    FOnEditItem(Self, FItems[Index], FItems.Objects[Index]);
end;

procedure TcaEditListBox.DoItemIndexChanged;
var
  AItemIndex: Integer;
  AObject: TObject;
  AText: string;
begin
  if (ItemIndex >= 0) and (ItemIndex <= FItems.Count) and Assigned(FOnItemIndexChanged) then
    begin
      AItemIndex := -1;
      AText := '';
      AObject := nil;
      if ItemIndex < FItems.Count then
        begin
          AItemIndex := ItemIndex;
          AText := FItems[ItemIndex];
          AObject := FItems.Objects[ItemIndex];
        end;
      FOnItemIndexChanged(Self, AItemIndex, AText, AObject);
    end;
end;

procedure TcaEditListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  GapRect: TRect;
begin
  inherited;
  if FIsEditing then
    begin
      GapRect := Rect;
      GapRect.Right := GapRect.Left + 2;
      Canvas.Brush.Color := FInplaceEdit.Color;
      Canvas.FillRect(GapRect);
      GapRect := Rect;
      GapRect.Bottom := GapRect.Top + 1;
      Canvas.FillRect(GapRect);
    end;
end;

procedure TcaEditListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_RETURN, VK_F2] then
    begin
      if ItemIndex <> -1 then
        StartEditing
      else
        Key := 0;
    end;
  inherited;
end;

procedure TcaEditListBox.KeyUp(var Key: Word; Shift: TShiftState);
var
  Index: Integer;
begin
  case Key of
    VK_DELETE:
      begin
        if FAllowDeleteKey then
          begin
            Index := ItemIndex;
            if Index >= 0 then
              begin
                if Index < FItems.Count then
                  begin
                    DoDeleteItem(Index);
                    FItems.Delete(Index);
                    if FItems.Count > 0 then
                      begin
                        if Index > FItems.Count then
                          ItemIndex := Pred(FItems.Count)
                        else
                          ItemIndex := Index;
                      end;
                  end;
              end;
          end;
      end;
  else
    begin
      if ssCtrl in Shift then
        ShowMessage('Yo')
      else
        DoItemIndexChanged;
    end;
  end;
  inherited;
end;

procedure TcaEditListBox.Loaded;
begin
  inherited;
  SyncItems;
end;

procedure TcaEditListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseIsDown := True;
  FDragging := ssCtrl in Shift;
  FDownIndex := ItemIndex;
  if FDragging then
    begin
      InitializeDragList(FItems[FDownIndex]);
      FDragImage.Left := X;
      FDragImage.Top := Y;
      FDragImage.BringToFront;
      FDragImage.Visible := True;
    end;
end;

procedure TcaEditListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FDragging then
    begin
      FDragImage.Left := X;
      FDragImage.Top := Y;
      FDragImage.Invalidate;
    end;
end;

procedure TcaEditListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited;
  DoItemIndexChanged;
end;

  // Private methods 

procedure TcaEditListBox.CreateDragImage;
begin
  FDragImage := TcaEditListDragImage.Create(Self);
  FDragImage.Parent := Self;
  FDragImage.Color := clYellow;
end;

procedure TcaEditListBox.CreateInplaceEditor;
begin
  FInplaceEdit := TcaEditListBoxInplaceEditor.Create(Self);
  if not (csDesigning in ComponentState) then
    FInplaceEdit.Parent := Self;
  FInplaceEdit.Hide;
  FInplaceEdit.Listbox := Self;
  FInplaceEdit.Ctl3D := False;
  FInplaceEdit.BorderStyle := bsNone;
  FInplaceEdit.OnExit := InplaceEditExitEvent; 
end;

procedure TcaEditListBox.CreateItems;
begin
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemChangedEvent;
end;

procedure TcaEditListBox.EndEditing(SaveNeeded: Boolean);
var
  Index: Integer;
begin
  FIsEditing := False;
  FInplaceEdit.Hide;
  if SaveNeeded then
    begin
      if FInplaceEdit.Text <> '' then
        begin
          Index := ItemIndex;
          if Index < FItems.Count then
            begin
              FItems[Index] := FInplaceEdit.Text;
              DoEditItem(Index);
              ItemIndex := Index;
            end
          else
            begin
              FItems.Add(FInplaceEdit.Text);
              DoAddItem(Index);
              ItemIndex := Succ(Index);
            end;
        end;
    end;
  SetFocus;
end;

procedure TcaEditListBox.InitializeDragList(const S: string);
var
  ASize: TSize;
begin
  ASize := FDragImage.Canvas.TextExtent(S);
  FDragImage.Width := Asize.cx + 2;
  FDragImage.Height := ASize.cy + 2;
  FDragImage.Text := S;
  FDragImage.Canvas.TextOut(1, 1, S);
end;

procedure TcaEditListBox.StartEditing;
var
  SelLength: Integer;
begin
  FInplaceEdit.BoundsRect := ItemRect(ItemIndex);
  FInplaceEdit.Left := FInplaceEdit.Left + 2;
  FInplaceEdit.Top := FInplaceEdit.Top + 1;
  if ItemIndex < FItems.Count then
    FInplaceEdit.Text := FItems[ItemIndex]
  else
    FInplaceEdit.Text := '';
  FInplaceEdit.Font.Assign(Font);
  FInplaceEdit.Show;
  FInplaceEdit.BringToFront;
  FInplaceEdit.SetFocus;
  FIsEditing := True;
  SelLength := 0;
  if Length(FInplaceEdit.Text) > 0 then
    SelLength := Length(FInplaceEdit.Text);
  FInplaceEdit.SetSelLength(SelLength);
  Invalidate;
end;

procedure TcaEditListBox.SyncItems;
begin
  inherited Items := FItems;
  if FAllowInsertAtEnd then
    inherited Items.Add('');
end;

  // Event handlers 

procedure TcaEditListBox.InplaceEditExitEvent(Sender: TObject);
begin
  if FIsEditing then
    begin
      EndEditing(True);
      Invalidate;
    end;
end;

procedure TcaEditListBox.ItemChangedEvent(Sender: TObject);
begin
  SyncItems;
end;

  // Property methods 

function TcaEditListBox.GetDragColor: TColor;
begin
  Result := FDragImage.Color;
end;

function TcaEditListBox.GetItems: TStrings;
begin
  Result := FItems;
end;

procedure TcaEditListBox.SetDragColor(const Value: TColor);
begin
  FDragImage.Color := Value;
end;

procedure TcaEditListBox.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
  inherited Items := FItems;
end;

  //---------------------------------------------------------------------------
  // TcaEditListBoxInplaceEditor                                               
  //---------------------------------------------------------------------------

procedure TcaEditListBoxInplaceEditor.KeyPress(var Key: Char);
begin
  if Key = #13 then
    begin
      Key := #0;
      FListbox.EndEditing(True);
    end
  else
    begin
      if Key = #27 then
        FListbox.EndEditing(False);
    end;
  inherited;
end;

  //---------------------------------------------------------------------------
  // TcaEditListDragImage                                                      
  //---------------------------------------------------------------------------

  // Public methods 

procedure TcaEditListDragImage.Paint;
begin
  inherited;
  Canvas.Brush.Color := FColor;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  Canvas.Brush.Style := bsClear;
  Canvas.TextOut(1, 1, FText);
end;

  // Property methods 

procedure TcaEditListDragImage.SetText(const Value: string);
begin
  FText := Value;
end;

end.

