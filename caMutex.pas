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


unit caMutex;

{$INCLUDE ca.inc}

interface

uses

  // Standard delphi units 
  Windows,
  SysUtils,
  Classes,

  // ca units 
  caClasses,
  caUtils;

type

  TcaMutexActivation = (maOpen, maCreate);

  TcaMutexState = (msClosed, msOpenNew, msOpenExisting);

  TcaWaitResult = (wrAbandoned, wrSignaledObject, wrTimedOut, wrFailed);

  //```````````````````````````````````````````````````````````````````````````
  // IcaMutex                                                                  
  //```````````````````````````````````````````````````````````````````````````

  IcaMutex = interface
  ['{D59BF1B3-772E-4EC2-A7EE-D1FDDCD7F3F3}']
    // Property methods 
    function GetHandle: THandle;
    function GetIsOpen: Boolean;
    function GetName: string;
    function GetState: TcaMutexState;
    // Interface methods 
    function Wait(ATimeOut: DWORD = 0): TcaWaitResult;
    procedure Activate(AActivationType: TcaMutexActivation);
    procedure Open;
    procedure Close;
    // Interface properties 
    property Handle: THandle read GetHandle;
    property IsOpen: Boolean read GetIsOpen;
    property Name: string read GetName;
    property State: TcaMutexState read GetState;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaMutex                                                                  
  //```````````````````````````````````````````````````````````````````````````

  TcaMutex = class(TInterfacedObject)
  private
    // Property fields 
    FHandle: THandle;
    FName: string;
    FState: TcaMutexState;
    // Property methods 
    function GetHandle: THandle;
    function GetIsOpen: Boolean;
    function GetName: string;
    function GetState: TcaMutexState;
  public
    // Create/Destroy 
    constructor Create(const AName: string = '');
    destructor Destroy; override;
    // Interface methods 
    function Wait(ATimeOut: DWORD = INFINITE): TcaWaitResult;
    procedure Open;
    procedure Close;
    // Interface properties 
    property Handle: THandle read GetHandle;
    property IsOpen: Boolean read GetIsOpen;
    property Name: string read GetName;
    property State: TcaMutexState read GetState;
  end;

implementation

  //```````````````````````````````````````````````````````````````````````````
  // TcaMutex                                                                  
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaMutex.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

destructor TcaMutex.Destroy;
begin
  if GetIsOpen then Close;
  inherited;
end;

  // Interface methods 

function TcaMutex.Wait(ATimeOut: DWORD = INFINITE): TcaWaitResult;
var
  WaitResult: DWORD;
begin
  WaitResult := WaitForSingleObject(FHandle, ATimeOut);
  case WaitResult of
    WAIT_OBJECT_0:    Result := wrSignaledObject;
    WAIT_ABANDONED:   Result := wrAbandoned;
    WAIT_TIMEOUT:     Result := wrTimedOut;
    WAIT_FAILED:      Result := wrFailed;
  else
    Result := wrFailed;
  end;
end;

procedure TcaMutex.Close;
begin
  ReleaseMutex(FHandle);
  CloseHandle(FHandle);
  FHandle := 0;
  FState := msClosed;
end;

procedure TcaMutex.Open;
var
  Err: DWORD;
begin
  if FState = msClosed then
    begin
      FHandle := CreateMutex(nil, False, PChar(FName));
      Err := GetLastError;
      if Err = ERROR_ALREADY_EXISTS then
        begin
          FHandle := OpenMutex(MUTEX_ALL_ACCESS, True, PChar(FName));
          FState := msOpenExisting;
        end
      else
        begin
          if Err = ERROR_SUCCESS then
            FState := msOpenNew
          else
            FState := msClosed;
        end;
    end;
end;

  // Property methods 

function TcaMutex.GetHandle: THandle;
begin
  Result := FHandle;
end;

function TcaMutex.GetName: string;
begin
  Result := FName;
end;

function TcaMutex.GetIsOpen: Boolean;
begin
  Result := FState <> msClosed;
end;

function TcaMutex.GetState: TcaMutexState;
begin
  Result := FState;
end;

end.
