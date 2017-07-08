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
