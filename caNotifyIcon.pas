unit caNotifyIcon;

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
  ShellAPI,
  Menus,
  CommCtrl,
  MMSystem;

type

  //---------------------------------------------------------------------------
  // Exceptions
  //---------------------------------------------------------------------------

  EcaNotifyIconError = class(Exception);

  //---------------------------------------------------------------------------
  // TcaNotifyIcon
  //---------------------------------------------------------------------------

  TcaNotifyIcon = class(TComponent)
  protected
    // Property fields
    FActive: Boolean;
    FAnimated: Boolean;
    FAnimateDelay: Integer;
    FAnimatePriority: TThreadPriority;
    FHint: string;
    FIconIndex: Integer;
    FImageList: TImageList;
    FMinFormToTray: Boolean;
    FPopupMenu: TPopupMenu;
    // Event fields
    FOnDblClick: TNotifyEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    // New fields
    IconThread: TThread;
    NewFormProc: Pointer;
    OldFormProc: TFarProc;
    Wnd: HWnd;
    // Write methods
    procedure SetActive(Value: Boolean);
    procedure SetAnimated(Value: Boolean);
    procedure SetAnimateDelay(Value: Integer);
    procedure SetAnimatePriority(Value: TThreadPriority);
    procedure SetHint(Value: string);
    procedure SetIconIndex(Value: Integer);
    procedure SetImageList(Value: TImageList);
    procedure SetMinFormToTray(Value: Boolean);
    procedure SetPopupMenu(Value: TPopupMenu);
    // Overriden methods
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // New methods
    function GetTrayNotifyRect: TRect;
    procedure HookWndProc(var Msg: TMessage);
    procedure Modify(IconProps: Boolean);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState);
    procedure NotifyIconProc(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Minimize;
    procedure Restore;
  published
    property Active: Boolean read FActive write SetActive;
    property Animated: Boolean read FAnimated write SetAnimated;
    property AnimateDelay: Integer read FAnimateDelay write SetAnimateDelay default 50;
    property AnimatePriority: TThreadPriority read FAnimatePriority write SetAnimatePriority default tpNormal;
    property Hint: string read FHint write SetHint;
    property IconIndex: Integer read FIconIndex write SetIconIndex;
    property ImageList: TImageList read FImageList write SetImageList;
    property MinFormToTray: Boolean read FMinFormToTray write SetMinFormToTray;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
  end;

implementation

const
  CM_NOTIFYICON = WM_USER + 100;

  //---------------------------------------------------------------------------
  // TcaNotifyIconThread
  //---------------------------------------------------------------------------

type
  TcaNotifyIconThread = class(TThread)
  protected
    // Animate options
    Animated: Boolean;
    Delay: Integer;
    // Icon properties
    Hint: string;
    IconIndex: Integer;
    ImageList: HImageList;
    Wnd: HWnd;
    // Modify flag
    Modified: Boolean;
    procedure Execute; override;
  public
    constructor Create(AWnd: HWnd);
    procedure SetAnimationOptions(AAnimated: Boolean; ADelay: Integer);
    procedure SetIconProps(const AHint: string; AIconIndex: Integer;
      AImageList: HImageList);
  end;

constructor TcaNotifyIconThread.Create(AWnd: HWnd);
begin
  Wnd := AWnd;
  inherited Create(False);
end;

procedure TcaNotifyIconThread.SetAnimationOptions(AAnimated: Boolean; ADelay: Integer);
begin
  Animated := AAnimated;
  if Animated then
    Delay := ADelay
  else
    Delay := 50;
end;

procedure TcaNotifyIconThread.SetIconProps(const AHint: string; AIconIndex: Integer;
  AImageList: HImageList);
begin
  Hint := AHint;
  IconIndex := AIconIndex;
  ImageList := AImageList;
  Modified := True;
end;

procedure TcaNotifyIconThread.Execute;
var
  IconData: TNotifyIconData;
  AniIconIndex: Integer;
begin
  AniIconIndex := 0;
  with IconData do
    begin
      cbSize := SizeOf(IconData);
      Wnd := Self.Wnd;
      uID := 0;
      uFlags := NIF_MESSAGE or NIF_TIP or NIF_ICON;
      uCallbackMessage := CM_NOTIFYICON;
      hIcon := 0;
      szTip := '';
    end;
  Shell_NotifyIcon(NIM_ADD, @IconData);
  repeat
    if Modified then
      with IconData do
        begin
          StrLCopy(szTip, PChar(Hint), SizeOf(szTip) - 1);
          if not Animated then
            begin
              DestroyIcon(hIcon);
              hIcon := ImageList_GetIcon(ImageList, IconIndex, ILD_TRANSPARENT);
              Shell_NotifyIcon(NIM_MODIFY, @IconData);
            end;
          Modified := False;
        end;
    if Animated then
      with IconData do
        begin
          DestroyIcon(hIcon);
          hIcon := ImageList_GetIcon(ImageList, AniIconIndex, ILD_TRANSPARENT);
          Shell_NotifyIcon(NIM_MODIFY, @IconData);
          Inc(AniIconIndex);
          if (AniIconIndex >= ImageList_GetImageCount(ImageList)) then
            AniIconIndex := 0;
        end;
    Sleep(Delay);
  until Terminated;
  Shell_NotifyIcon(NIM_DELETE, @IconData);
end;

//---------------------------------------------------------------------------
// TcaNotifyIcon
//---------------------------------------------------------------------------

constructor TcaNotifyIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnimateDelay := 50;
  FAnimatePriority := tpNormal;
  Wnd := AllocateHWnd(NotifyIconProc);
end;

procedure TcaNotifyIcon.Loaded;
begin
  inherited Loaded;
  Modify(True);
end;

destructor TcaNotifyIcon.Destroy;
begin
  SetActive(False);
  if Assigned(Owner) and Assigned(OldFormProc) then
    begin
      SetWindowLong((Owner as TForm).Handle, GWL_WNDPROC, LongInt(OldFormProc));
      if Assigned(NewFormProc) then
        FreeObjectInstance(NewFormProc);
    end;
  DeallocateHWnd(Wnd);
  inherited Destroy;
end;

function TcaNotifyIcon.GetTrayNotifyRect: TRect;
var
  ShellTrayWnd, TrayNotifyWnd: HWnd;
begin
  ShellTrayWnd := FindWindow('Shell_TrayWnd', '');
  TrayNotifyWnd := FindWindowEx(ShellTrayWnd, 0, 'TrayNotifyWnd', '');
  GetWindowRect(TrayNotifyWnd, Result);
end;

procedure TcaNotifyIcon.Minimize;
begin
  with (Owner as TForm) do
    DrawAnimatedRects(Handle, IDANI_CAPTION, BoundsRect, GetTrayNotifyRect);
  if (Owner = Application.MainForm) then
    begin
      Application.Minimize;
      ShowWindow(Application.Handle, SW_HIDE);
    end
  else
    ShowWindow((Owner as TForm).Handle, SW_HIDE);
  Active := True;
end;

procedure TcaNotifyIcon.Restore;
begin
  with (Owner as TForm) do
    DrawAnimatedRects(Handle, IDANI_CAPTION, GetTrayNotifyRect, BoundsRect);
  if (Owner = Application.MainForm) then
    begin
      ShowWindow(Application.Handle, SW_SHOW);
      Application.Restore;
      (Owner as TForm).Visible := True;
    end
  else
    ShowWindow((Owner as TForm).Handle, SW_SHOW);
  (Owner as TForm).Update;
  Active := False;
end;

procedure TcaNotifyIcon.HookWndProc(var Msg: TMessage);

  procedure InheritedProc;
  begin
    with Msg do
      Result := CallWindowProc(OldFormProc, (Owner as TForm).Handle, Msg, wParam, lParam);
  end;

begin
  with Msg do
    case Msg of
      WM_SYSCOMMAND:
        if (wParam = SC_MINIMIZE) then
          Minimize
        else
          InheritedProc;
    else
      InheritedProc;
    end;
end;

procedure TcaNotifyIcon.Modify(IconProps: Boolean);
var
  ImageListHandle: HImageList;
begin
  if FActive then
    begin
      if not Assigned(IconThread) then
        IconThread := TcaNotifyIconThread.Create(Wnd);
      TcaNotifyIconThread(IconThread).SetAnimationOptions(FAnimated,
        FAnimateDelay);
      if IconProps then
        begin
          if Assigned(FImageList) then
            ImageListHandle := FimageList.Handle
          else
            ImageListHandle := 0;
          TcaNotifyIconThread(IconThread).SetIconProps(FHint, FIconIndex,
            ImageListHandle);
        end;
      IconThread.Priority := FAnimatePriority;
    end
  else if Assigned(IconThread) then
    begin
      IconThread.Free;
      IconThread := nil;
    end;
end;

procedure TcaNotifyIcon.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(Acomponent, Operation);
  if (Operation = opRemove) then
    begin
      if (AComponent = FImageList) then
        ImageList := nil
      else if (AComponent = FPopupMenu) then
        PopupMenu := nil;
    end;
end;

procedure TcaNotifyIcon.SetActive(Value: Boolean);
begin
  if (Value <> FActive) then
    begin
      FActive := Value;
      Modify(True);
    end;
end;

procedure TcaNotifyIcon.SetAnimated(Value: Boolean);
begin
  if (Value <> FAnimated) then
    begin
      FAnimated := Value;
      Modify(False);
    end;
end;

procedure TcaNotifyIcon.SetAnimateDelay(Value: Integer);
begin
  if (Value <> AnimateDelay) then
    begin
      FAnimateDelay := Value;
      Modify(False);
    end;
end;

procedure TcaNotifyIcon.SetAnimatePriority(Value: TThreadPriority);
begin
  if (Value <> FAnimatePriority) then
    begin
      FAnimatePriority := Value;
      Modify(False);
    end;
end;

procedure TcaNotifyIcon.SetHint(Value: string);
begin
  if (Value <> FHint) then
    begin
      FHint := Value;
      Modify(True);
    end;
end;

procedure TcaNotifyIcon.SetIconIndex(Value: Integer);
begin
  if (Value <> FIconIndex) then
    begin
      FIconIndex := Value;
      Modify(True);
    end;
end;

procedure TcaNotifyIcon.SetImageList(Value: TImageList);
begin
  if (Value <> FImageList) then
    begin
      FImageList := Value;
      if Assigned(Value) then Value.FreeNotification(Self);
      Modify(True);
    end;
end;

procedure TcaNotifyIcon.SetMinFormToTray(Value: Boolean);
begin
  if (Value <> FMinFormToTray) then
    begin
      FMinFormToTray := Value;
      if FMinFormToTray then
        begin
          if not (Owner is TForm) then
            raise EcaNotifyIconError.Create('Owner must be a form');
          OldFormProc := TFarProc(GetWindowLong((Owner as TForm).Handle, GWL_WNDPROC));
          NewFormProc := MakeObjectInstance(HookWndProc);
          SetWindowLong((Owner as TForm).Handle, GWL_WNDPROC, LongInt(NewFormProc));
        end
      else if Assigned(OldFormProc) then
        begin
          SetWindowLong((Owner as TForm).Handle, GWL_WNDPROC, LongInt(OldFormProc));
          OldFormProc := nil;
          if Assigned(NewFormProc) then
            FreeObjectInstance(NewFormProc);
          NewFormProc := nil;
        end;
    end;
end;

procedure TcaNotifyIcon.SetPopupMenu(Value: TPopupMenu);
begin
  if (Value <> FPopupMenu) then
    begin
      FPopupMenu := Value;
      if Assigned(Value) then Value.FreeNotification(Self);
    end;
end;

procedure TcaNotifyIcon.MouseDown(Button: TMouseButton; Shift: TShiftState);
var
  P: TPoint;
begin
  GetCursorPos(P);
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, P.X, P.Y);
end;

procedure TcaNotifyIcon.MouseUp(Button: TMouseButton; Shift: TShiftState);
var
  P: TPoint;
begin
  GetCursorPos(P);
  if Assigned(FOnMouseUp) then FOnMouseUp(Self, Button, Shift, P.X, P.Y);
  if Assigned(FPopupMenu) then
    begin
      SetForegroundWindow(Wnd);
      FPopupMenu.PopupComponent := Self;
      FPopupMenu.Popup(P.X, P.Y);
      SetForegroundWindow(Application.Handle);
    end
  else
    if FMinFormToTray then Restore;
end;

procedure TcaNotifyIcon.NotifyIconProc(var Msg: TMessage);
var
  Shift: TShiftState;
  Down: boolean;
  Button: TMouseButton;
begin
  if (Msg.Msg = CM_NOTIFYICON) then
    begin
      case Msg.lParam of
        WM_LBUTTONDOWN:
          begin
            Button := mbLeft; Down := True; end;
        WM_LBUTTONUP:
          begin
            Button := mbLeft; Down := False; end;
        WM_MBUTTONDOWN:
          begin
            Button := mbMiddle; Down := True; end;
        WM_MBUTTONUP:
          begin
            Button := mbMiddle; Down := False; end;
        WM_RBUTTONDOWN:
          begin
            Button := mbRight; Down := True; end;
        WM_RBUTTONUP:
          begin
            Button := mbRight; Down := False; end;
        WM_LBUTTONDBLCLK:
          begin
            if Assigned(FOnDblClick) then FOnDblClick(Self); Exit;
          end;
      else
        Exit;
      end;
      Shift := KeysToShiftState(Msg.WParam);
      if Down then
        MouseDown(Button, Shift)
      else
        MouseUp(Button, Shift);
    end
  else
    with Msg do
      Result := DefWindowProc(Wnd, Msg, WParam, LParam);
end;

end.

