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


unit caNetAPI;

interface

uses

  // Standard Delphi units 
  Windows,
  Classes,
  Sysutils,
  Contnrs;

type

  TcaNetworkServerType = (stWorkStation, stServer, stSQLServer, stDomainController, stDomainBackup, stTimeSource,
                          stAFP, stNovell, stDomainMember, stPrintQueue, stDialIn, stNix, stNT, stWFW, stServerMFPN,
                          stServerNT, stPotentialBrowser, stBackupBrowser, stMasterBrowser, stDomainMaster, stServerOSF,
                          stServerVMS, stWindows, stDFS, stClusterNT, stDCE, stAlternateXPort, stLocalListOnly,
                          stDomainEnum, stAll, stUnknown);

  TcaNetworkServerTypes = set of TcaNetworkServerType;

  TcaNetworkPlatformID = (npDOS, npOS2, npNT, npOSF, npVMS, npUnknown);

  //```````````````````````````````````````````````````````````````````````````
  // TcaNetworkServer                                                          
  //```````````````````````````````````````````````````````````````````````````

  TcaNetworkServer = class(TObject)
  private
    // Private methods 
    FMajorVersion: Integer;
    FMinorVersion: Integer;
    FName: string;
    FPlatformID: TcaNetworkPlatformID;
    FComment: string;
    FServerTypes: TcaNetworkServerTypes;
  public
    // Properties 
    property Comment: string read FComment write FComment;
    property MajorVersion: Integer read FMajorVersion write FMajorVersion;
    property MinorVersion: Integer read FMinorVersion write FMinorVersion;
    property Name: string read FName write FName;
    property PlatformID: TcaNetworkPlatformID read FPlatformID write FPlatformID;
    property ServerTypes: TcaNetworkServerTypes read FServerTypes write FServerTypes;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // IcaNetworkServers                                                         
  //```````````````````````````````````````````````````````````````````````````

  IcaNetworkServers = interface
  ['{5136E80F-76FB-4CD9-A7D2-FAA8150875F3}']
    // Property methods 
    function GetCount: Integer;
    function GetItem(Index: Integer): TcaNetworkServer;
    // Interface properties 
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcaNetworkServer read GetItem; default;
    // Interface methods 
    procedure Clear;
    procedure GetServerNames(AServerNames: TStrings);
    procedure Update(const ADomainName: string; AServerTypes: TcaNetworkServerTypes);
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaNetworkServers                                                         
  //```````````````````````````````````````````````````````````````````````````

  TcaNetworkServers = class(TInterfacedObject, IcaNetworkServers)
  private
    // Private fields 
    FList: TObjectList;
    // Property methods 
    function GetCount: Integer;
    function GetItem(Index: Integer): TcaNetworkServer;
    // Private methods 
    function GetNetAPIServerTypes(AServerTypes: TcaNetworkServerTypes): DWORD;
    function GetPlatformID(ANetAPIPlatformID: DWORD): TcaNetworkPlatformID;
    function GetServerTypes(ANetAPIServerTypes: DWORD): TcaNetworkServerTypes;
  protected
    // Interface methods - IcaNetworkServers 
    procedure Clear;
    procedure GetServerNames(AServerNames: TStrings);
    procedure Update(const ADomainName: string; AServerTypes: TcaNetworkServerTypes);
    // Virtual methods 
    function CreateNetworkServer: TcaNetworkServer; virtual;
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
  end;

implementation

const
  cCannotEnumerateServers   = 'Cannot enumerate servers';

type

  //```````````````````````````````````````````````````````````````````````````
  // TServerInfo101                                                            
  //```````````````````````````````````````````````````````````````````````````

  PServerInfo101 = ^TServerInfo101;
  TServerInfo101 = record
    platform_id: DWORD;
    name: PWideChar;
    version_major: DWORD;
    version_minor: DWORD;
    server_type: DWORD;
    comment: PWideChar;
  end;

const

  NERR_SUCCESS              = 0;
  MAX_PREFERRED_LENGTH      = DWORD(-1);

  PLATFORM_ID_DOS           = 300;
  PLATFORM_ID_NT            = 500;
  PLATFORM_ID_OS2           = 400;
  PLATFORM_ID_OSF           = 600;
  PLATFORM_ID_VMS           = 700;

  SV_TYPE_WORKSTATION       = $00000001;
  SV_TYPE_SERVER            = $00000002;
  SV_TYPE_SQLSERVER         = $00000004;
  SV_TYPE_DOMAIN_CTRL       = $00000008;
  SV_TYPE_DOMAIN_BAKCTRL    = $00000010;
  SV_TYPE_TIME_SOURCE       = $00000020;
  SV_TYPE_AFP               = $00000040;
  SV_TYPE_NOVELL            = $00000080;
  SV_TYPE_DOMAIN_MEMBER     = $00000100;
  SV_TYPE_PRINTQ_SERVER     = $00000200;
  SV_TYPE_DIALIN_SERVER     = $00000400;
  SV_TYPE_XENIX_SERVER      = $00000800;
  SV_TYPE_SERVER_UNIX       = SV_TYPE_XENIX_SERVER;
  SV_TYPE_NT                = $00001000;
  SV_TYPE_WFW               = $00002000;
  SV_TYPE_SERVER_MFPN       = $00004000;
  SV_TYPE_SERVER_NT         = $00008000;
  SV_TYPE_POTENTIAL_BROWSER = $00010000;
  SV_TYPE_BACKUP_BROWSER    = $00020000;
  SV_TYPE_MASTER_BROWSER    = $00040000;
  SV_TYPE_DOMAIN_MASTER     = $00080000;
  SV_TYPE_SERVER_OSF        = $00100000;
  SV_TYPE_SERVER_VMS        = $00200000;
  SV_TYPE_WINDOWS           = $00400000; // Windows95 and above                                     
  SV_TYPE_DFS               = $00800000; // Root of a DFS tree                                      
  SV_TYPE_CLUSTER_NT        = $01000000; // NT Cluster                                              
  SV_TYPE_DCE               = $10000000; // IBM DSS (Directory and Security Services) or equivalent 
  SV_TYPE_ALTERNATE_XPORT   = $20000000; // Return list for alternate transport                     
  SV_TYPE_LOCAL_LIST_ONLY   = $40000000; // Return local list only                                  
  SV_TYPE_DOMAIN_ENUM       = $80000000; //                                                         
  SV_TYPE_ALL               = $FFFFFFFF; // Handy for NetServerEnum2                                

  //```````````````````````````````````````````````````````````````````````````
  // NetAPI functions                                                          
  //```````````````````````````````````````````````````````````````````````````

function NetServerEnum(const ServerName: PWideString; level: DWORD; var Buffer: pointer; PrefMaxLen: DWORD;
  var EntriesRead: DWORD; var TotalEntries: DWORD; ServerType: DWORD; const Domain: PWideChar;
  var ResumeHandle: DWORD): DWORD; stdcall; external 'netapi32.dll';

function NetApiBufferFree(Buffer: pointer): DWORD; stdcall; external 'netapi32.dll';

  //```````````````````````````````````````````````````````````````````````````
  // TcaNetworkServers                                                         
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaNetworkServers.Create;
begin
  inherited;
  FList := TObjectList.Create(True);
end;

destructor TcaNetworkServers.Destroy;
begin
  FList.Free;
  inherited;
end;

  // Interface methods - IcaNetworkServers 

procedure TcaNetworkServers.Clear;
begin
  FList.Clear;
end;

procedure TcaNetworkServers.GetServerNames(AServerNames: TStrings);
var
  Index: Integer;
begin
  AServerNames.Clear;
  for Index := 0 to Pred(GetCount) do
    AServerNames.Add(GetItem(Index).Name);
end;

procedure TcaNetworkServers.Update(const ADomainName: string; AServerTypes: TcaNetworkServerTypes);
var
  Buffer: Pointer;
  DomainUnicode: array[0..100] of WideChar;
  EntriesRead: DWORD;
  ErrCode: DWORD;
  Index: DWORD;
  PDomainUnicode: PWideChar;
  ResumeHandle: DWORD;
  Server: TcaNetworkServer;
  ServerInfo: PServerInfo101;
  ServerTypes: DWORD;
  TotalEntries: DWORD;
begin
  Clear;
  ResumeHandle := 0;
  if (ADomainName = '') then
    PDomainUnicode := nil
  else
    begin
      StringToWideChar(ADomainName, DomainUnicode, SizeOf(DomainUnicode));
      PDomainUnicode := DomainUnicode;
    end;
  ServerTypes := GetNetAPIServerTypes(AServerTypes);
  errCode := NetServerEnum(nil,
                           101,
                           Buffer,
                           MAX_PREFERRED_LENGTH,
                           EntriesRead,
                           TotalEntries,
                           ServerTypes,
                           PDomainUnicode,
                           ResumeHandle);
  if (errCode <> NERR_SUCCESS) then
    raise Exception.Create(cCannotEnumerateServers);
  try
    ServerInfo := Buffer;
    for Index := 1 to EntriesRead do
      begin
        Server := CreateNetworkServer;
        Server.PlatformID := GetPlatformID(ServerInfo^.platform_id);
        Server.Name := ServerInfo^.name;
        Server.ServerTypes := GetServerTypes(ServerInfo^.server_type);
        Server.MajorVersion := ServerInfo^.version_major;
        Server.MinorVersion := ServerInfo^.version_minor;
        Server.Comment := ServerInfo^.comment;
        FList.Add(Server);
        Inc(ServerInfo);
      end;
  finally
    NetApiBufferFree(Buffer);
  end;
end;

  // Virtual methods 

function TcaNetworkServers.CreateNetworkServer: TcaNetworkServer;
begin
  Result := TcaNetworkServer.Create;
end;

  // Private methods 

function TcaNetworkServers.GetPlatformID(ANetAPIPlatformID: DWORD): TcaNetworkPlatformID;
begin
  case ANetAPIPlatformID of
    PLATFORM_ID_DOS:  Result := npDOS;
    PLATFORM_ID_NT:   Result := npNT;
    PLATFORM_ID_OS2:  Result := npOS2;
    PLATFORM_ID_OSF:  Result := npOSF;
    PLATFORM_ID_VMS:  Result := npVMS;
  else
    Result := npUnknown;
  end;
end;

function TcaNetworkServers.GetNetAPIServerTypes(AServerTypes: TcaNetworkServerTypes): DWORD;
begin
  Result := 0;
  if stWorkStation in AServerTypes then Result := Result or SV_TYPE_WORKSTATION;
  if stServer in AServerTypes then Result := Result or SV_TYPE_SERVER;
  if stSQLServer in AServerTypes then Result := Result or SV_TYPE_SQLSERVER;
  if stDomainController in AServerTypes then Result := Result or SV_TYPE_DOMAIN_CTRL;
  if stDomainBackup in AServerTypes then Result := Result or SV_TYPE_DOMAIN_BAKCTRL;
  if stTimeSource in AServerTypes then Result := Result or SV_TYPE_TIME_SOURCE;
  if stAFP in AServerTypes then Result := Result or SV_TYPE_AFP;
  if stNovell in AServerTypes then Result := Result or SV_TYPE_NOVELL;
  if stDomainMember in AServerTypes then Result := Result or SV_TYPE_DOMAIN_MEMBER;
  if stPrintQueue in AServerTypes then Result := Result or SV_TYPE_PRINTQ_SERVER;
  if stDialIn in AServerTypes then Result := Result or SV_TYPE_DIALIN_SERVER;
  if stNix in AServerTypes then Result := Result or SV_TYPE_SERVER_UNIX;
  if stNT in AServerTypes then Result := Result or SV_TYPE_NT;
  if stWFW in AServerTypes then Result := Result or SV_TYPE_WFW;
  if stServerMFPN in AServerTypes then Result := Result or SV_TYPE_SERVER_MFPN;
  if stServerNT in AServerTypes then Result := Result or SV_TYPE_SERVER_NT;
  if stPotentialBrowser in AServerTypes then Result := Result or SV_TYPE_POTENTIAL_BROWSER;
  if stBackupBrowser in AServerTypes then Result := Result or SV_TYPE_BACKUP_BROWSER;
  if stMasterBrowser in AServerTypes then Result := Result or SV_TYPE_MASTER_BROWSER;
  if stDomainMaster in AServerTypes then Result := Result or SV_TYPE_DOMAIN_MASTER;
  if stServerOSF in AServerTypes then Result := Result or SV_TYPE_SERVER_OSF;
  if stServerVMS in AServerTypes then Result := Result or SV_TYPE_SERVER_VMS;
  if stWindows in AServerTypes then Result := Result or SV_TYPE_WINDOWS;
  if stDFS in AServerTypes then Result := Result or SV_TYPE_DFS;
  if stClusterNT in AServerTypes then Result := Result or SV_TYPE_CLUSTER_NT;
  if stDCE in AServerTypes then Result := Result or SV_TYPE_DCE;
  if stAlternateXPort in AServerTypes then Result := Result or SV_TYPE_ALTERNATE_XPORT;
  if stLocalListOnly in AServerTypes then Result := Result or SV_TYPE_LOCAL_LIST_ONLY;
  if stDomainEnum in AServerTypes then Result := Result or SV_TYPE_DOMAIN_ENUM;
  if stAll in AServerTypes then Result := Result or SV_TYPE_ALL;
end;

function TcaNetworkServers.GetServerTypes(ANetAPIServerTypes: DWORD): TcaNetworkServerTypes;
begin
  Result := [];
  if (ANetAPIServerTypes and SV_TYPE_WORKSTATION) <> 0 then Include(Result, stWorkStation);
  if (ANetAPIServerTypes and SV_TYPE_SERVER) <> 0 then Include(Result, stServer);
  if (ANetAPIServerTypes and SV_TYPE_SQLSERVER) <> 0 then Include(Result, stSQLServer);
  if (ANetAPIServerTypes and SV_TYPE_DOMAIN_CTRL) <> 0 then Include(Result, stDomainController);
  if (ANetAPIServerTypes and SV_TYPE_DOMAIN_BAKCTRL) <> 0 then Include(Result, stDomainBackup);
  if (ANetAPIServerTypes and SV_TYPE_TIME_SOURCE) <> 0 then Include(Result, stTimeSource);
  if (ANetAPIServerTypes and SV_TYPE_AFP) <> 0 then Include(Result, stAFP);
  if (ANetAPIServerTypes and SV_TYPE_NOVELL) <> 0 then Include(Result, stNovell);
  if (ANetAPIServerTypes and SV_TYPE_DOMAIN_MEMBER) <> 0 then Include(Result, stDomainMember);
  if (ANetAPIServerTypes and SV_TYPE_PRINTQ_SERVER) <> 0 then Include(Result, stPrintQueue);
  if (ANetAPIServerTypes and SV_TYPE_DIALIN_SERVER) <> 0 then Include(Result, stDialIn);
  if (ANetAPIServerTypes and SV_TYPE_SERVER_UNIX) <> 0 then Include(Result, stNix);
  if (ANetAPIServerTypes and SV_TYPE_NT) <> 0 then Include(Result, stNT);
  if (ANetAPIServerTypes and SV_TYPE_WFW) <> 0 then Include(Result, stWFW);
  if (ANetAPIServerTypes and SV_TYPE_SERVER_MFPN) <> 0 then Include(Result, stServerMFPN);
  if (ANetAPIServerTypes and SV_TYPE_SERVER_NT) <> 0 then Include(Result, stServerNT);
  if (ANetAPIServerTypes and SV_TYPE_POTENTIAL_BROWSER) <> 0 then Include(Result, stPotentialBrowser);
  if (ANetAPIServerTypes and SV_TYPE_BACKUP_BROWSER) <> 0 then Include(Result, stBackupBrowser);
  if (ANetAPIServerTypes and SV_TYPE_MASTER_BROWSER) <> 0 then Include(Result, stMasterBrowser);
  if (ANetAPIServerTypes and SV_TYPE_DOMAIN_MASTER) <> 0 then Include(Result, stDomainMaster);
  if (ANetAPIServerTypes and SV_TYPE_SERVER_OSF) <> 0 then Include(Result, stServerOSF);
  if (ANetAPIServerTypes and SV_TYPE_SERVER_VMS) <> 0 then Include(Result, stServerVMS);
  if (ANetAPIServerTypes and SV_TYPE_WINDOWS) <> 0 then Include(Result, stWindows);
  if (ANetAPIServerTypes and SV_TYPE_DFS) <> 0 then Include(Result, stDFS);
  if (ANetAPIServerTypes and SV_TYPE_CLUSTER_NT) <> 0 then Include(Result, stClusterNT);
  if (ANetAPIServerTypes and SV_TYPE_DCE) <> 0 then Include(Result, stDCE);
  if (ANetAPIServerTypes and SV_TYPE_ALTERNATE_XPORT) <> 0 then Include(Result, stAlternateXPort);
  if (ANetAPIServerTypes and SV_TYPE_LOCAL_LIST_ONLY) <> 0 then Include(Result, stLocalListOnly);
  if (ANetAPIServerTypes and SV_TYPE_DOMAIN_ENUM) <> 0 then Include(Result, stDomainEnum);
  if (ANetAPIServerTypes and SV_TYPE_ALL) <> 0 then Include(Result, stAll);
end;

  // Property methods 

function TcaNetworkServers.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcaNetworkServers.GetItem(Index: Integer): TcaNetworkServer;
begin
  Result := TcaNetworkServer(FList[Index]);
end;

end.
