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


unit caTransparentWnd;

{$INCLUDE ca.inc}

interface

uses
  Windows, Messages, Classes, Controls, Forms;

type
  TcaTransparentWnd = class(TComponent)
  private
    { Private declarations }
  protected
    _percent : shortint;
    { Protected declarations }
  public
    procedure SetTransparent(percent : shortint); overload;
    procedure SetTransparentHWND(hwnd: longint; percent : shortint);
    procedure SetTransparent; overload;
    procedure SetOpaqueHWND(hwnd : longint);
    procedure SetOpaque;
    { Public declarations }
  published
    property Percent: shortint read _percent write _percent default 0;
    { Published declarations }
  end;

implementation

const LWA_ALPHA = $2;
const GWL_EXSTYLE = (-20);
const WS_EX_LAYERED = $80000;
const WS_EX_TRANSPARENT = $20;

procedure TcaTransparentWnd.SetOpaqueHWND(hwnd: longint);
var
  old: longint;
begin
  old := GetWindowLongA(hwnd,GWL_EXSTYLE);
  SetWindowLongA(hwnd, GWL_EXSTYLE, old and ((not 0)-WS_EX_LAYERED) );
end;

procedure TcaTransparentWnd.SetOpaque;
begin
  Self.SetOpaqueHWND((Self.Owner as TForm).Handle);
end;

procedure TcaTransparentWnd.SetTransparent;
begin
  Self.SetTransparentHWND((Self.Owner as TForm).Handle,100-Self._percent);
end;

procedure TcaTransparentWnd.SetTransparentHWND(hwnd: longint; percent : shortint);
var
  SetLayeredWindowAttributes: function (hwnd: LongInt; crKey: byte; bAlpha: byte; dwFlags: LongInt): LongInt; stdcall;

  old: longint;
  User32: Cardinal;
begin
  User32 := LoadLibrary('USER32');
  if User32 <> 0 then
  try
    SetLayeredWindowAttributes := GetProcAddress(User32, 'SetLayeredWindowAttributes');
    if @SetLayeredWindowAttributes <> nil then
    begin
      old := GetWindowLongA(hwnd,GWL_EXSTYLE);
      SetWindowLongA(hwnd,GWL_EXSTYLE,old or WS_EX_LAYERED);
      SetLayeredWindowAttributes(hwnd, 0, (255 * percent) DIV 100, LWA_ALPHA);
    end;
  finally
    FreeLibrary(User32);
  end;
end;

procedure TcaTransparentWnd.SetTransparent(percent: shortint);
begin
  Self.SetTransparentHWND((Self.Owner as TForm).Handle,100 - percent);
end;

end.
