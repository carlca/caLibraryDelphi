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


unit caFormHook;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Classes,
  Windows,
  Messages,
  Sysutils,
  SyncObjs,
  Controls,
  Forms,
  ShellAPI,
  Graphics,
  Menus,

  // ca utils 
  caTypes,
  caUtils;

type

  TcaReceiveMessageEvent = procedure(Sender: TObject; Msg: TMessage; var Handled: Boolean) of object;

  //----------------------------------------------------------------------------
  // TcaFormHook                                                                
  //----------------------------------------------------------------------------

  TcaFormHook = class(TComponent)
  private
    // Private fields 
    FHandle: HWND;
    FNewFormProc: Pointer;
    FOldFormProc: TFarProc;
    // Event property fields 
    FOnFormReceiveMessage: TcaReceiveMessageEvent;
    FOnReceiveMessage: TcaReceiveMessageEvent;
    // Private methods 
    procedure CreateOwnerFormHook;
    procedure CreateWindowHandle;
    procedure FreeOwnerFormHook;
    procedure FreeWindowHandle;
    procedure FormWndProc(var Msg: TMessage);
    procedure WndProc(var Msg: TMessage);
  protected
    // Protected static methods 
    function OwnerForm: TForm;
    function OwnerFormExists: Boolean;
    function OwnerFormHandle: HWND;
    // Protected methods 
    procedure DoFormReceiveMessage(Msg: TMessage; var Handled: Boolean); virtual;
    procedure DoReceiveMessage(Msg: TMessage; var Handled: Boolean); virtual;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
    procedure Hook;
    procedure Unhook;
    // Properties 
    property Handle: HWND read FHandle;
  published
    // Event properties 
    property OnFormReceiveMessage: TcaReceiveMessageEvent read FOnFormReceiveMessage write FOnFormReceiveMessage;
    property OnReceiveMessage: TcaReceiveMessageEvent read FOnFormReceiveMessage write FOnFormReceiveMessage;
  end;

implementation

  //----------------------------------------------------------------------------
  // TcaFormHook                                                                
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaFormHook.Create(AOwner: TComponent);
begin
  inherited;
  Hook;
end;

destructor TcaFormHook.Destroy;
begin
  Unhook;
  inherited;
end;

  // Public methods 

procedure TcaFormHook.Hook;
begin
  CreateOwnerFormHook;
  CreateWindowHandle;
end;

procedure TcaFormHook.UnHook;
begin
  FreeWindowHandle;
  FreeOwnerFormHook;
end;

  // Protected static methods 

function TcaFormHook.OwnerForm: TForm;
begin
  Result := TForm(Owner);
end;

function TcaFormHook.OwnerFormExists: Boolean;
begin
  Result := Owner <> nil;
end;

function TcaFormHook.OwnerFormHandle: HWND;
begin
  Result := OwnerForm.Handle;
end;

  // Protected methods 

procedure TcaFormHook.DoFormReceiveMessage(Msg: TMessage; var Handled: Boolean);
begin
  if Assigned(FOnFormReceiveMessage) then
    FOnFormReceiveMessage(Self, Msg, Handled);
end;

procedure TcaFormHook.DoReceiveMessage(Msg: TMessage; var Handled: Boolean);
begin
  if Assigned(FOnReceiveMessage) then
    FOnReceiveMessage(Self, Msg, Handled);
end;

  // Private methods 

procedure TcaFormHook.CreateOwnerFormHook;
begin
  FOldFormProc := TFarProc(GetWindowLong(OwnerFormHandle, GWL_WNDPROC));
  FNewFormProc := {$IFDEF D5}Forms{$ELSE}Classes{$ENDIF}.MakeObjectInstance(FormWndProc);
  SetWindowLong(OwnerFormHandle, GWL_WNDPROC, LongInt(FNewFormProc));
end;

procedure TcaFormHook.CreateWindowHandle;
begin
  FHandle := {$IFDEF D5}Forms{$ELSE}Classes{$ENDIF}.AllocateHWnd(WndProc);
end;

procedure TcaFormHook.FreeOwnerFormHook;
begin
  if Assigned(Owner) and Assigned(FOldFormProc) then
    begin
      SetWindowLong(OwnerFormHandle, GWL_WNDPROC, LongInt(FOldFormProc));
      if Assigned(FNewFormProc) then {$IFDEF D5}Forms{$ELSE}Classes{$ENDIF}.FreeObjectInstance(FNewFormProc);
    end;
end;

procedure TcaFormHook.FreeWindowHandle;
begin
  {$IFDEF D5}Forms{$ELSE}Classes{$ENDIF}.DeallocateHWnd(FHandle);
end;

procedure TcaFormHook.FormWndProc(var Msg: TMessage);

  procedure InheritedProc;
  begin
    Msg.Result := CallWindowProc(FOldFormProc, OwnerFormHandle, Msg.Msg, Msg.wParam, Msg.lParam);
  end;

var
  Handled: Boolean;
begin
  Handled := False;
  DoFormReceiveMessage(Msg, Handled);
  if not Handled then InheritedProc;
end;

procedure TcaFormHook.WndProc(var Msg: TMessage);
var
  Handled: Boolean;
begin
  Handled := False;
  DoReceiveMessage(Msg, Handled);
  if not Handled then
    Msg.Result := DefWindowProc(Handle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

end.
