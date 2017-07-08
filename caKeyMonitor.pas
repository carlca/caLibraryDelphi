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


unit caKeyMonitor;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  Messages,
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  Dialogs,

  // caCollection 
  caClasses,
  caConsts;

type

  TcaFoundKeyEvent = procedure(Sender: TObject; AKey: Word) of object;

  //---------------------------------------------------------------------------
  // IcaKeyMonitor                                                             
  //---------------------------------------------------------------------------

  IcaKeyMonitor = interface
  ['{C3E5F891-927E-4549-AA2B-FE91F2FF04BA}']
    // property methods 
    function GetActive: Boolean;
    function GetKey(Index: Integer): Word;
    function GetKeyCount: Integer;
    function GetOnFoundKey: TcaFoundKeyEvent;
    function GetOnKeyDown: TKeyEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetOnFoundKey(const Value: TcaFoundKeyEvent);
    procedure SetOnKeyDown(const Value: TKeyEvent);
    // interface methods 
    procedure AddKey(AKey: Word);
    procedure ClearKeys;
    // properties 
    property Active: Boolean read GetActive write SetActive;
    property KeyCount: Integer read GetKeyCount;
    property Keys[Index: Integer]: Word read GetKey;
    property OnFoundKey: TcaFoundKeyEvent read GetOnFoundKey write SetOnFoundKey;
    property OnKeyDown: TKeyEvent read GetOnKeyDown write SetOnKeyDown;
  end;

  //---------------------------------------------------------------------------
  // TcaKeyMonitor                                                             
  //---------------------------------------------------------------------------

  TcaKeyMonitor = class(TComponent, IcaKeyMonitor)
  private
    FActive: Boolean;
    FKeys: TList;
    FOnFoundKey: TcaFoundKeyEvent;
    FOnKeyDown: TKeyEvent;
    FOriginalHandler: TMessageEvent;
    // property methods 
    function GetActive: Boolean;
    function GetKey(Index: Integer): Word;
    function GetKeyCount: Integer;
    function GetOnFoundKey: TcaFoundKeyEvent;
    function GetOnKeyDown: TKeyEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetOnFoundKey(const Value: TcaFoundKeyEvent);
    procedure SetOnKeyDown(const Value: TKeyEvent);
    // private methods 
    function CheckKey(AKey: Word): Boolean;
    function IsRunTime: Boolean;
    procedure MessageHandler(var Msg: TMsg; var Handled: Boolean);
  protected
    // event triggers 
    procedure DoFoundKey(AKey: Word); virtual;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); virtual;
  public
    // lifetime 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // interface methods - IcaKeyMonitor 
    procedure AddKey(AKey: Word);
    procedure ClearKeys;
    // interface properties - IcaKeyMonitor 
    property Active: Boolean read GetActive write SetActive;
    property KeyCount: Integer read GetKeyCount;
    property Keys[Index: Integer]: Word read GetKey;
  published
    // public properties 
    property OnFoundKey: TcaFoundKeyEvent read GetOnFoundKey write SetOnFoundKey;
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaKeyMonitor                                                             
  //---------------------------------------------------------------------------

  // lifetime 

constructor TcaKeyMonitor.Create(AOwner: TComponent);
begin
  inherited;
  FKeys := TList.Create;
  FOriginalHandler := Application.OnMessage;
end;

destructor TcaKeyMonitor.Destroy;
begin
  Application.OnMessage := FOriginalHandler;
  FKeys.Free;
  inherited;
end;

  // interface methods - IcaKeyMonitor 

procedure TcaKeyMonitor.AddKey(AKey: Word);
begin
  FKeys.Add(Pointer(AKey));
end;

procedure TcaKeyMonitor.ClearKeys;
begin
  FKeys.Clear;
end;

  // event triggers 

procedure TcaKeyMonitor.DoFoundKey(AKey: Word);
begin
  if Assigned(FOnFoundKey) then FOnFoundKey(Self, AKey);
end;

procedure TcaKeyMonitor.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then FOnKeyDown(Self, Key, Shift);
end;

  // private methods 

function TcaKeyMonitor.CheckKey(AKey: Word): Boolean;
var
  Index: Integer;
  FoundKey: Word;
begin
  Result := False;
  FoundKey := VK_NONE;
  for Index := 0 to FKeys.Count - 1 do
    begin
      if AKey = Integer(FKeys[Index]) then
        begin
          FoundKey := AKey;
          Break;
        end;
    end;
  if FoundKey <> VK_NONE then
    begin
      DoFoundKey(FoundKey);
      Result := True;
    end;
end;

function TcaKeyMonitor.IsRunTime: Boolean;
begin
  Result := Pos('delphi32', LowerCase(Application.ExeName)) = 0;
end;

procedure TcaKeyMonitor.MessageHandler(var Msg: TMsg; var Handled: Boolean);
var
  Key: Word;
  Shift: TShiftState;
begin
  if Msg.Message = WM_KEYDOWN then
    begin
      Handled := CheckKey(Msg.wParam);
      Shift := KeyDataToShiftState(Msg.lParam);
      Key := Msg.wParam;
      DoKeyDown(Key, Shift);
      Msg.wParam := Key;
    end;
end;

  // property methods 

function TcaKeyMonitor.GetKey(Index: Integer): Word;
begin
  Result := Integer(FKeys[Index]);
end;

function TcaKeyMonitor.GetKeyCount: Integer;
begin
  Result := FKeys.Count;
end;

function TcaKeyMonitor.GetOnFoundKey: TcaFoundKeyEvent;
begin
  Result := FOnFoundKey;
end;

procedure TcaKeyMonitor.SetOnFoundKey(const Value: TcaFoundKeyEvent);
begin
  FOnFoundKey := Value;
end;

function TcaKeyMonitor.GetActive: Boolean;
begin
  Result := FActive;
end;

function TcaKeyMonitor.GetOnKeyDown: TKeyEvent;
begin
  Result := FOnKeyDown;
end;

procedure TcaKeyMonitor.SetActive(const Value: Boolean);
begin
  if IsRunTime then
    begin
      FActive := Value;
      if FActive then
        Application.OnMessage := MessageHandler
      else
        Application.OnMessage := nil;
    end;
end;

procedure TcaKeyMonitor.SetOnKeyDown(const Value: TKeyEvent);
begin
  FOnKeyDown := Value;
end;

end.
