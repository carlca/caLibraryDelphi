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


unit caServices;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  SysUtils,
  Classes,
  Contnrs,
  WinSvc;

type

  //---------------------------------------------------------------------------
  // TcaService                                                                
  //---------------------------------------------------------------------------

  TcaServiceStatus = (ssUndefined, ssStopped, ssStartPending, ssStopPending,
                      ssRunning, ssContinuePending, ssPausePending, ssPaused);

  TcaUpdateCallback = procedure of object;

  TcaService = class(TObject)
  private
    // private members...
    FMachineName: string;
    FServiceName: string;
    FDisplayName: string;
    FServiceStatus: TServiceStatus;
    FUpdateCallback: TcaUpdateCallback;
    // property methods...
    function GetServiceStatus: TcaServiceStatus;
  public
    // lifetime...
    constructor Create(const AMachineName, AServiceName, ADisplayName: string; AServiceStatus: TServiceStatus);
    // public methods...
    function Start: Boolean;
    function Stop: Boolean;
    // public properties...
    property ServiceName: string read FServiceName;
    property DisplayName: string read FDisplayName;
    property ServiceStatus: TcaServiceStatus read GetServiceStatus;
    property UpdateCallback: TcaUpdateCallback read FUpdateCallback write FUpdateCallback;
  end;

  //---------------------------------------------------------------------------
  // IcaServiceList                                                                
  //---------------------------------------------------------------------------

  IcaServiceList = interface
  ['{0F9F855C-A80B-40AB-A114-173FC2581EA1}']
    // property methods...
    function GetIgnoreInactive: Boolean;
    function GetMachine: string;
    function GetService(Index: Integer): TcaService;
    function GetServiceCount: Integer;
    function GetServiceType: DWORD;
    procedure SetIgnoreInactive(const Value: Boolean);
    procedure SetMachine(const Value: string);
    procedure SetServiceType(Value: DWORD);
    // interface methods - IcaServiceList...
    function FindServiceName(const AServiceName: string; AllowPartialMatch: Boolean): TcaService;
    function FindDisplayName(const AServiceName: string; AllowPartialMatch: Boolean): TcaService;
    procedure GetServiceNamesAsStrings(AList: TStrings);
    procedure SortByDisplayName;
    procedure Update;
    // interface properties - IcaServiceList...
    property IgnoreInactive: Boolean read GetIgnoreInactive write SetIgnoreInactive;
    property Machine: string read GetMachine write SetMachine;
    property ServiceCount: Integer read GetServiceCount;
    property Services[Index: Integer]: TcaService read GetService; default;
    property ServiceType: DWORD read GetServiceType write SetServiceType;
  end;

  //---------------------------------------------------------------------------
  // TcaServiceList                                                                
  //---------------------------------------------------------------------------

  TcaServiceList = class(TInterfacedObject, IcaServiceList)
  private
    // Private fields 
    FList: TObjectList;
    // property fields...
    FIgnoreInactive: Boolean;
    FMachineName: string;
    FServiceType: DWORD;
    // private methods...
    function FindService(const ASearchName: string; AllowPartialMatch, UseServiceName: Boolean): TcaService;
    procedure BuildList;
    procedure Initialize;
    // property methods...
    function GetIgnoreInactive: Boolean;
    function GetMachine: string;
    function GetService(Index: Integer): TcaService;
    function GetServiceCount: Integer;
    function GetServiceType: DWORD;
    procedure SetIgnoreInactive(const Value: Boolean);
    procedure SetMachine(const Value: string);
    procedure SetServiceType(Value: DWORD);
  protected
    // interface methods - IcaServiceList...
    function FindServiceName(const AServiceName: string; AllowPartialMatch: Boolean): TcaService;
    function FindDisplayName(const AServiceName: string; AllowPartialMatch: Boolean): TcaService;
    procedure GetServiceNamesAsStrings(AList: TStrings);
    procedure SortByDisplayName;
    procedure Update;
  public
    // lifetime...
    constructor Create; overload;
    constructor Create(const AMachineName: string); overload;
    destructor Destroy; override;
  end;

implementation

//---------------------------------------------------------------------------
// TcaService
//---------------------------------------------------------------------------

// lifetime...

constructor TcaService.Create(const AMachineName, AServiceName, ADisplayName: string; AServiceStatus: TServiceStatus);
begin
  inherited Create;
  FMachineName := AMachineName;
  FServiceName := AServiceName;
  FDisplayName := ADisplayName;
  FServiceStatus := AServiceStatus;
end;

// public methods...

function TcaService.Start: Boolean;
var
  ServiceControlManager: SC_HANDLE;
  ServiceHandle: SC_HANDLE;
  Flags: Cardinal;
  ArgVectors: PChar;
  ServStatus: TServiceStatus;
  CheckPoint: DWORD;
  Started: Boolean;
begin
  ServStatus.dwCurrentState := Cardinal(-1);
  Started := False;
  ServiceControlManager := OpenSCManager(PChar(FMachineName), nil, SC_MANAGER_ALL_ACCESS);
  if ServiceControlManager <> 0 then
    begin
      Flags := SERVICE_START or SERVICE_QUERY_STATUS;
      ServiceHandle := OpenService(ServiceControlManager, PChar(FServiceName), Flags);
      if ServiceHandle <> 0 then
        begin
          ArgVectors := nil;
          if StartService(ServiceHandle, 0, ArgVectors) then
            begin
              Started := True;
              if QueryServiceStatus(ServiceHandle, ServStatus) then
                begin
                  while (ServStatus.dwCurrentState <> SERVICE_RUNNING) and
                        (ServStatus.dwCurrentState <> SERVICE_STOPPED) do
                    begin
                      CheckPoint := ServStatus.dwCheckPoint;
                      Sleep(ServStatus.dwWaitHint);
                      if not QueryServiceStatus(ServiceHandle, ServStatus) then
                        // couldn't check status...
                        Break;
                      if ServStatus.dwCheckPoint < CheckPoint then
                        // avoid infinite loop...
                        Break;
                    end;
                end;
            end;
          CloseServiceHandle(ServiceHandle);
        end;
      CloseServiceHandle(ServiceControlManager);
    end;
  Result := (ServStatus.dwCurrentState = SERVICE_RUNNING) or
            ((ServStatus.dwCurrentState = SERVICE_STOPPED) and Started); 
  if Assigned(FUpdateCallback) then
    FUpdateCallback;
end;

function TcaService.Stop: Boolean;
var
  ServiceControlManager: SC_HANDLE;
  ServiceHandle: SC_HANDLE;
  Flags: Cardinal;
  ServStatus: TServiceStatus;
  CheckPoint: DWORD;
begin
  ServStatus.dwCurrentState := Cardinal(-1);
  ServiceControlManager := OpenSCManager(PChar(FMachineName), nil, SC_MANAGER_ALL_ACCESS);
  if ServiceControlManager <> 0 then
    begin
      Flags := SERVICE_STOP or SERVICE_QUERY_STATUS;
      ServiceHandle := OpenService(ServiceControlManager, PChar(FServiceName), Flags);
      if ServiceHandle <> 0 then
        begin
          if ControlService(ServiceHandle, SERVICE_CONTROL_STOP, ServStatus) then
            begin
              if QueryServiceStatus(ServiceHandle, ServStatus) then
                begin
                  while ServStatus.dwCurrentState <> SERVICE_STOPPED do
                    begin
                      CheckPoint := ServStatus.dwCheckPoint;
                      Sleep(ServStatus.dwWaitHint);
                      if not QueryServiceStatus(ServiceHandle, ServStatus) then
                        // couldn't check status...
                        Break;
                      if ServStatus.dwCheckPoint < CheckPoint then
                        // avoid infinite loop...
                        Break;
                    end;
                end;
            end;
          CloseServiceHandle(ServiceHandle);
        end;
      CloseServiceHandle(ServiceControlManager);
    end;
  Result := ServStatus.dwCurrentState = SERVICE_STOPPED;
  if Assigned(FUpdateCallback) then
    FUpdateCallback;
end;

// property methods...

function TcaService.GetServiceStatus: TcaServiceStatus;
begin
  case FServiceStatus.dwCurrentState of
    SERVICE_STOPPED:            Result := ssStopped;
    SERVICE_START_PENDING:      Result := ssStartPending;
    SERVICE_STOP_PENDING:       Result := ssStopPending;
    SERVICE_RUNNING:            Result := ssRunning;
    SERVICE_CONTINUE_PENDING:   Result := ssContinuePending;
    SERVICE_PAUSE_PENDING:      Result := ssPausePending;
    SERVICE_PAUSED:             Result := ssPaused;
  else
    Result := ssUndefined;
  end;
end;

{
  SERVICE_STOPPED                = $00000001;
  SERVICE_START_PENDING          = $00000002;
  SERVICE_STOP_PENDING           = $00000003;
  SERVICE_RUNNING                = $00000004;
  SERVICE_CONTINUE_PENDING       = $00000005;
  SERVICE_PAUSE_PENDING          = $00000006;
  SERVICE_PAUSED                 = $00000007;
  }

//---------------------------------------------------------------------------
// TcaServiceList                                                                
//---------------------------------------------------------------------------

// lifetime...

constructor TcaServiceList.Create;
begin
  inherited;
  Initialize;
  Update;
end;

constructor TcaServiceList.Create(const AMachineName: string);
begin
  inherited Create;
  FMachineName := AMachineName;
  Initialize;
  Update;
end;

destructor TcaServiceList.Destroy;
begin
  FList.Free;
  inherited;
end;

// interface methods - IcaServiceList...

function TcaServiceList.FindDisplayName(const AServiceName: string; AllowPartialMatch: Boolean): TcaService;
begin
  Result := FindService(AServiceName, AllowPartialMatch, False);
end;

function TcaServiceList.FindServiceName(const AServiceName: string; AllowPartialMatch: Boolean): TcaService;
begin
  Result := FindService(AServiceName, AllowPartialMatch, True);
end;

procedure TcaServiceList.GetServiceNamesAsStrings(AList: TStrings);
var
  Index: Integer;
  Service: TcaService;
  ListEntry: String;
begin
  AList.Clear;
  AList.BeginUpdate;
  try
    for Index := 0 to Pred(GetServiceCount) do
      begin
        Service := GetService(Index);
        ListEntry := Service.ServiceName;
        AList.Add(ListEntry);
      end;
  finally
    AList.EndUpdate;
  end;
end;

function CompareDisplayNames(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TcaService(Item1).DisplayName, TcaService(Item2).DisplayName);
end;

procedure TcaServiceList.SortByDisplayName;
begin
  FList.Sort(@CompareDisplayNames);
end;

procedure TcaServiceList.Update;
begin
  BuildList;
end;

// private methods...

function TcaServiceList.FindService(const ASearchName: string; AllowPartialMatch, UseServiceName: Boolean): TcaService;
var
  Ptr: Pointer;
  Service: TcaService;
  Name: string;
  Matched: Boolean;
begin
  Result := nil;
  for Ptr in FList do
    begin
      Service := TcaService(Ptr);
      if UseServiceName then
        Name := Service.ServiceName
      else
        Name := Service.DisplayName;
      if AllowPartialMatch then
        Matched := Pos(AnsiLowerCase(ASearchName), AnsiLowerCase(Name)) > 0
      else
        Matched := AnsiLowerCase(ASearchName) = AnsiLowerCase(Name);
      if Matched then
        begin
          Result := Service;
          Break;
        end;
    end;
end;

procedure TcaServiceList.BuildList;
const
  MAX_SERVICE_COUNT = 4096;
type
  TServiceArray = array[0..Pred(MAX_SERVICE_COUNT)] of TEnumServiceStatus;
  PServiceArray = ^TServiceArray;
var
  Index: Integer;
  ServiceControlManager: SC_HANDLE;
  ServiceState: DWORD;
  BytesNeeded: DWORD;
  ServiceCount: DWORD;
  ResumeHandle : DWord;
  ServiceArray : PServiceArray;
  Service: TcaService;
begin
  FList.Clear;
  if FIgnoreInactive then
    ServiceState := SERVICE_ACTIVE
  else
    ServiceState := SERVICE_STATE_ALL;
  // connect to the service control manager...
  ServiceControlManager := OpenSCManager(PChar(FMachineName), nil, SC_MANAGER_ALL_ACCESS);
  // if successful...
  if ServiceControlManager <> 0 then
    begin
      ResumeHandle := 0;
      New(ServiceArray);
      EnumServicesStatus(ServiceControlManager,
                         FServiceType,
                         ServiceState,
                         ServiceArray^[0],
                         SizeOf(ServiceArray^),
                         BytesNeeded,
                         ServiceCount,
                         ResumeHandle );
      for Index := 0 to Pred(ServiceCount) do
        begin
          Service := TcaService.Create(FMachineName,
                                       ServiceArray^[Index].lpServiceName,
                                       ServiceArray^[Index].lpDisplayName,
                                       ServiceArray^[Index].ServiceStatus);
          Service.UpdateCallback := Update;
          FList.Add(Service);
        end;
      Dispose(ServiceArray);
      CloseServiceHandle(ServiceControlManager);
    end;
end;

procedure TcaServiceList.Initialize;
begin
  FList := TObjectList.Create(True);
  FServiceType := SERVICE_TYPE_ALL;
end;

  // property methods...

function TcaServiceList.GetIgnoreInactive: Boolean;
begin
  Result := FIgnoreInactive;
end;

function TcaServiceList.GetMachine: string;
begin
  Result := FMachineName;
end;

function TcaServiceList.GetService(Index: Integer): TcaService;
begin
  Result := TcaService(FList[Index]);
end;

function TcaServiceList.GetServiceCount: Integer;
begin
  Result := FList.Count;
end;

function TcaServiceList.GetServiceType: DWORD;
begin
  Result := FServiceType;
end;

procedure TcaServiceList.SetIgnoreInactive(const Value: Boolean);
begin
  if Value <> FIgnoreInactive then
    begin
      FIgnoreInactive := Value;
      Update;
    end;
end;

procedure TcaServiceList.SetMachine(const Value: string);
begin
  if Value <> FMachineName then
    begin
      FMachineName := Value;
      Update;
    end;
end;

procedure TcaServiceList.SetServiceType(Value: DWORD);
begin
  if Value <> FServiceType then
    begin
      FServiceType := Value;
      Update;
    end;
end;

end.
