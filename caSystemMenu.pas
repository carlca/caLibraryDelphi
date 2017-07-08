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


unit caSystemMenu;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Windows,
  SysUtils,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  StdCtrls,
  ExtCtrls,

  // ca units
  caUtils;


type

 //---------------------------------------------------------------------------
 // Enumerated types
 //---------------------------------------------------------------------------

  TcaSystemMenuItemType = (smRestore, smMove, smSize, smMinimize, smMaximize, smSeparator, smClose);

 //---------------------------------------------------------------------------
 // TcaSystemMenu
 //---------------------------------------------------------------------------

  TcaSystemMenu = class(TImage)
  private
    FPopupMenu: TPopupMenu;
    FSysBlankBitmap: TBitmap;
    FSysCloseBitmap: TBitmap;
    FSysMinimizeBitmap: TBitmap;
    FSysMaximizeBitmap: TBitmap;
    FSysRestoreBitmap: TBitmap;
    // Private methods
    procedure AddMenuItem(const ACaption: String; ABitmap: TBitmap; AItemType: TcaSystemMenuItemType; AShortCut: TShortCut);
    procedure LoadBitmaps;
    procedure BuildSystemMenu;
    procedure CreateObjects;
    procedure FreeObjects;
    // Event handlers
    procedure MenuItemClickEvent(Sender: TObject);
  protected
    procedure DblClick; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

implementation

{$R CASYSTEMMENU.RES}

 //---------------------------------------------------------------------------
 // TcaSystemMenu
 //---------------------------------------------------------------------------

 // Constructor / destructor

constructor TcaSystemMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateObjects;
  LoadBitmaps;
  BuildSystemMenu;
end;

destructor TcaSystemMenu.Destroy;
begin
  FreeObjects;
  inherited Destroy;
end;

 // Private methods

procedure TcaSystemMenu.CreateObjects;
begin
  FPopupMenu := TPopupMenu.Create(nil);
  PopupMenu := FPopupMenu;
  FSysBlankBitmap := TBitmap.Create;
  FSysCloseBitmap := TBitmap.Create;
  FSysMaximizeBitmap := TBitmap.Create;
  FSysMinimizeBitmap := TBitmap.Create;
  FSysRestoreBitmap := TBitmap.Create;
end;

procedure TcaSystemMenu.FreeObjects;
begin
  FPopupMenu.Free;
  FSysBlankBitmap.Free;
  FSysCloseBitmap.Free;
  FSysMaximizeBitmap.Free;
  FSysMinimizeBitmap.Free;
  FSysRestoreBitmap.Free;
end;

procedure TcaSystemMenu.LoadBitmaps;
begin
  FSysBlankBitmap.LoadFromResourceName(HInstance, 'SYSBLANK');
  FSysCloseBitmap.LoadFromResourceName(HInstance, 'SYSCLOSE');
  FSysMaximizeBitmap.LoadFromResourceName(HInstance, 'SYSMAXIMIZE');
  FSysMinimizeBitmap.LoadFromResourceName(HInstance, 'SYSMINIMIZE');
  FSysRestoreBitmap.LoadFromResourceName(HInstance, 'SYSRESTORE');
end;

procedure TcaSystemMenu.AddMenuItem(const ACaption: String; ABitmap: TBitmap; AItemType: TcaSystemMenuItemType; AShortCut: TShortCut);
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(Owner);
  MenuItem.Caption := ACaption;
  ABitmap.Transparent := True;
  MenuItem.Bitmap := ABitmap;
  MenuItem.Tag := Ord(AItemType);
  MenuItem.OnClick := MenuItemClickEvent;
  MenuItem.ShortCut := AShortCut;
  FPopupMenu.Items.Add(MenuItem);
end;

procedure TcaSystemMenu.BuildSystemMenu;
begin
  AddMenuItem('Restore', FSysRestoreBitmap, smRestore, 0);
  AddMenuItem('Move', FSysBlankBitmap, smMove, 0);
  AddMenuItem('Size', FSysBlankBitmap, smSize, 0);
  AddMenuItem('Minimize', FSysMinimizeBitmap, smMinimize, 0);
  AddMenuItem('Maximize', FSysMaximizeBitmap, smMaximize, 0);
  AddMenuItem('-', FSysBlankBitmap, smSeparator, 0);
  AddMenuItem('Close', FSysCloseBitmap, smClose, ShortCut(VK_F4, [ssAlt]));
end;

procedure TcaSystemMenu.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  XPos: Integer;
  YPos: Integer;
begin
  inherited;
  XPos := ClientOrigin.X + X;
  YPos := ClientOrigin.Y + Y;
  if Button = mbLeft then FPopupMenu.Popup(XPos, YPos);
end;


procedure TcaSystemMenu.DblClick;
var
  Form: TCustomForm;
begin
  inherited;
  Form := GetParentForm(Self);
  SendMessage(Form.Handle, WM_CLOSE, 0, 0);
end;

 // Event handlers

procedure TcaSystemMenu.MenuItemClickEvent(Sender: TObject);
var
  MenuItem: TMenuItem;
  ItemType: TcaSystemMenuItemType;
  Form: TCustomForm;
begin
  MenuItem := TMenuItem(Sender);
  ItemType := TcaSystemMenuItemType(MenuItem.Tag);
  Form := GetParentForm(Self);
  case ItemType of
    smRestore:    Utils.AppRestore(Form.Handle);
    smMove:       Utils.AppMove(Form.Handle);
    smSize:       Utils.AppSize(Form.Handle);
    smMinimize:   Utils.AppMinimize(Form.Handle);
    smMaximize:   Utils.AppMaximize(Form.Handle);
    smSeparator:  Pass;
    smClose:      Utils.AppClose(Form.Handle);
  end;
end;

end.
