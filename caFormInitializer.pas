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


unit caFormInitializer;

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
  ActnList,

  // ca units 
  caUtils,
  caClasses,
  caLog,
  caMessages,
  caFormHook;

const
  WM_AFTER_SHOW = WM_USER + 1;

type

  //----------------------------------------------------------------------------
  // TcaFormInitializer                                                         
  //----------------------------------------------------------------------------

  TcaFormInitializer = class(TcaFormHook)
  private
    // Property fields 
    FAction: TAction;
    FDestroyDetected: Boolean;
    FHandledNCDestroy: Boolean;
    FHandledShowWindow: Boolean;
    FMessages: IcaMessages;
    FOnFormIsShowing: TNotifyEvent;
    // Property methods 
    function GetOnFormIsShowing: TNotifyEvent;
    // Private methods 
    procedure PostAfterShowMessage(AHandle: HWND);
  protected
    // Virtual protected methods 
    procedure DoFormIsShowing; virtual;
    procedure DoFormReceiveMessage(Msg: TMessage; var Handled: Boolean); override;
    procedure DoReceiveMessage(Msg: TMessage; var Handled: Boolean); override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
  published
    // Published properties 
    property Action: TAction read FAction write FAction;
    property OnFormIsShowing: TNotifyEvent read GetOnFormIsShowing write FOnFormIsShowing;
  end;

implementation

  //----------------------------------------------------------------------------
  // TcaFormInitializer                                                         
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaFormInitializer.Create(AOwner: TComponent);
begin
  inherited;
  FMessages := TcaMessages.Create;
end;

  // Virtual protected methods 

procedure TcaFormInitializer.DoFormIsShowing;
begin
  if Assigned(FAction) then
    begin
      FAction.Enabled := True;
      FAction.Execute;
    end
  else
    begin
      if Assigned(FOnFormIsShowing) then
        FOnFormIsShowing(Self);
    end;
end;

procedure TcaFormInitializer.DoFormReceiveMessage(Msg: TMessage; var Handled: Boolean);
begin
  inherited;
  case Msg.Msg of
    WM_SHOWWINDOW:
      if (not FHandledShowWindow) and Boolean(Msg.WParam) then
        begin
          PostAfterShowMessage(OwnerFormHandle);
          Handled := True;
          FHandledShowWindow := True;
        end;
    WM_DESTROY:
      FDestroyDetected := True;
    WM_NCDESTROY:
      begin
        if FDestroyDetected and (not FHandledNCDestroy) then 
          begin
            PostAfterShowMessage(Handle);
            Handled := True;
            FHandledNCDestroy := True;
          end;
      end;
    WM_AFTER_SHOW:
      begin
        DoFormIsShowing;
        Handled := True;
      end;
  end;
end;

procedure TcaFormInitializer.DoReceiveMessage(Msg: TMessage; var Handled: Boolean);
begin
  if Msg.Msg = WM_AFTER_SHOW then
    begin
      DoFormIsShowing;
      Handled := True;
    end;
end;

  // Private methods 

procedure TcaFormInitializer.PostAfterShowMessage(AHandle: HWND);
begin
  PostMessage(AHandle, WM_AFTER_SHOW, 0, 0);
end;

  // Property methods 

function TcaFormInitializer.GetOnFormIsShowing: TNotifyEvent;
begin
  if Assigned(FOnFormIsShowing) then
    Result := FOnFormIsShowing
  else
    begin
      if Assigned(Action) then
        Result := Action.OnExecute;
    end;
end;

end.
