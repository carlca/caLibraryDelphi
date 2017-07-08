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


unit caTimer;

{$INCLUDE ca.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  MMSystem,

  caTypes;

type
  TcaTimer = class;

  //----------------------------------------------------------------------------
  // TcaTimerThread                                                             
  //----------------------------------------------------------------------------

  TcaTimerThread = class(TThread)
  private
    FTimer: TcaTimer;
  protected
    procedure DoExecute;
  public
    constructor CreateTimerThread(Timer: TcaTimer);
    procedure Execute; override;
  end;

  //----------------------------------------------------------------------------
  // IcaTimer                                                                   
  //----------------------------------------------------------------------------

  IcaTimer = interface
  ['{0BB08B16-3CDF-4211-BC6E-D08B563F9F09}']
    function GetEnabled: Boolean;
    function GetInterval: LongWord;
    function GetOnTimer: TNotifyEvent;
    function GetPriority: TThreadPriority;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: LongWord);
    procedure SetOnTimer(const Value: TNotifyEvent);
    procedure SetPriority(const Value: TThreadPriority);
    procedure Start;
    procedure Stop;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Interval: LongWord read GetInterval write SetInterval;
    property OnTimer: TNotifyEvent read GetOnTimer write SetOnTimer;
    property Priority: TThreadPriority read GetPriority write SetPriority;
  end;

  //----------------------------------------------------------------------------
  // TcaTimer                                                                   
  //----------------------------------------------------------------------------

  TcaTimer = class(TComponent, IcaTimer)
  private
    FInterval: LongWord;
    FPriority: TThreadPriority;
    FOnTimer: TNotifyEvent;
    FContinue: Boolean;
    FRunning: Boolean;
    FEnabled: Boolean;
    function GetEnabled: Boolean;
    function GetInterval: LongWord;
    function GetOnTimer: TNotifyEvent;
    function GetPriority: TThreadPriority;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: LongWord);
    procedure SetOnTimer(const Value: TNotifyEvent);
    procedure SetPriority(const Value: TThreadPriority);
  protected
    property Continue: Boolean read FContinue write FContinue;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  published
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Interval: LongWord read GetInterval write SetInterval;
    property OnTimer: TNotifyEvent read GetOnTimer write SetOnTimer;
    property Priority: TThreadPriority read GetPriority write SetPriority;
  end;

implementation

  //----------------------------------------------------------------------------
  // TcaTimerThread                                                             
  //----------------------------------------------------------------------------

constructor TcaTimerThread.CreateTimerThread(Timer: TcaTimer);
begin
  inherited Create(True);
  FTimer := Timer;
  FreeOnTerminate := True;
end;

procedure TcaTimerThread.Execute;
var
  SleepTime, Last: LongWord;
begin
  while FTimer.Continue do
    begin
      Last := timeGetTime;
      Synchronize(DoExecute);
      SleepTime := FTimer.Interval - (timeGetTime - Last);
      if SleepTime < 10 then SleepTime := 10;
      Sleep(SleepTime);
    end;
end;

procedure TcaTimerThread.DoExecute;
begin
  if Assigned(FTimer.OnTimer) then FTimer.OnTimer(FTimer);
end;

  //----------------------------------------------------------------------------
  // TcaTimer                                                                   
  //----------------------------------------------------------------------------

constructor TcaTimer.Create(Owner: TComponent);
begin
  inherited;
  FPriority := tpNormal;
end;

destructor TcaTimer.Destroy;
begin
  Stop;
  inherited;
end;

function TcaTimer.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TcaTimer.GetInterval: LongWord;
begin
  Result := FInterval;
end;

function TcaTimer.GetOnTimer: TNotifyEvent;
begin
  Result := FOnTimer;
end;

function TcaTimer.GetPriority: TThreadPriority;
begin
  Result := FPriority;
end;

procedure TcaTimer.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
    begin
      FEnabled := Value;
      if FEnabled then
        Start
      else
        Stop;
    end;
end;

procedure TcaTimer.SetInterval(const Value: LongWord);
begin
  FInterval := Value;
end;

procedure TcaTimer.SetOnTimer(const Value: TNotifyEvent);
begin
  FOnTimer := Value;
end;

procedure TcaTimer.SetPriority(const Value: TThreadPriority);
begin
  FPriority := Value;
end;

procedure TcaTimer.Start;
var
  TimerThread: TcaTimerThread;
begin
  if not FRunning then
    begin
      FContinue := True;
      if not (csDesigning in ComponentState) then
        begin
          TimerThread := TcaTimerThread.CreateTimerThread(Self);
          TimerThread.Priority := FPriority;
          TimerThread.Resume;
        end;
      FRunning := True;
    end;
end;

procedure TcaTimer.Stop;
begin
  FContinue := False;
  FRunning := False;
end;

end.
