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


unit caIni;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Classes,
  SysUtils,
  FileCtrl,

  // ca Units 
  caLog,
  caCrypt,
  caClasses,
  caTypes,
  caUtils;

const
  cStartKey = 19423;

type

  //---------------------------------------------------------------------------
  // IcaIni                                                                    
  //---------------------------------------------------------------------------

  IcaIni = interface
  ['{A6118E1F-E626-45A5-AD4F-5A398AF1C816}']
    // Property methods 
    function GetAppName: string;
    function GetEncrypted: Boolean;
    function GetIniFile: string;
    function GetIsEmpty: Boolean;
    function GetName: String;
    function GetReadEncrypted: Boolean;
    function GetWriteEncrypted: Boolean;
    procedure SetAppName(const Value: string);
    procedure SetEncrypted(const Value: Boolean);
    procedure SetIniFile(const Value: string);
    procedure SetName(const Value: String);
    procedure SetReadEncrypted(const Value: Boolean);
    procedure SetWriteEncrypted(const Value: Boolean);
    // Interface methods 
    function DeleteKey(const Key: String): Boolean;
    procedure GetStrings(AStrings: TStrings);
    // Value property methods 
    function GetBoolean(const Key: String): Boolean;
    function GetFloat(const Key: String): Extended;
    function GetInteger(const Key: String): Integer;
    function GetString(const Key: String): String;
    procedure SetBoolean(const Key: String; const Value: Boolean);
    procedure SetFloat(const Key: String; const Value: Extended);
    procedure SetInteger(const Key: String; const Value: Integer);
    procedure SetString(const Key, Value: String);
    // Properties 
    property AppName: string read GetAppName write SetAppName;
    property IniFile: string read GetIniFile write SetIniFile;
    property IsEmpty: Boolean read GetIsEmpty;
    property Encrypted: Boolean read GetEncrypted write SetEncrypted;
    property Name: String read GetName write SetName;
    property ReadEncrypted: Boolean read GetReadEncrypted write SetReadEncrypted;
    property WriteEncrypted: Boolean read GetWriteEncrypted write SetWriteEncrypted;
    // Value properties 
    property Booleans[const Key: String]: Boolean read GetBoolean write SetBoolean;
    property Floats[const Key: String]: Extended read GetFloat write SetFloat;
    property Integers[const Key: String]: Integer read GetInteger write SetInteger;
    property Strings[const Key: String]: String read GetString write SetString;
  end;

  //---------------------------------------------------------------------------
  // TcaIni                                                                    
  //---------------------------------------------------------------------------

  TcaIni = class(TcaInterfacedPersistent, IcaIni)
  private
    // Private fields 
    FAppName: string;
    FCrypt: IcaCrypt;
    FEncrypted: Boolean;
    FIniFile: String;
    FList: TStringList;
    FName: String;
    FReadEncrypted: Boolean;
    FWriteEncrypted: Boolean;
    FUseWindowsFolder: Boolean;
    // Value property methods 
    function GetBoolean(const Key: String): Boolean;
    function GetFloat(const Key: String): Extended;
    function GetInteger(const Key: String): Integer;
    function GetString(const Key: String): String;
    procedure SetBoolean(const Key: String; const Value: Boolean);
    procedure SetFloat(const Key: String; const Value: Extended);
    procedure SetInteger(const Key: String; const Value: Integer);
    procedure SetString(const Key, Value: String);
    // Private methods 
    function CheckEncrypt(const AValue: String): String;
    function CheckDecrypt(const AValue: String): String;
    procedure Load;
    procedure Save;
    procedure UpdateIniPath;
  protected
    // Protected property methods 
    function GetAppName: string;
    function GetEncrypted: Boolean;
    function GetIniFile: string;
    function GetIsEmpty: Boolean;
    function GetName: String;
    function GetReadEncrypted: Boolean;
    function GetWriteEncrypted: Boolean;
    procedure SetAppName(const Value: string);
    procedure SetEncrypted(const Value: Boolean);
    procedure SetIniFile(const Value: string);
    procedure SetName(const Value: String);
    procedure SetReadEncrypted(const Value: Boolean);
    procedure SetWriteEncrypted(const Value: Boolean);
    // Protected methods
    procedure Initialize; virtual;
    // Interface methods 
    function DeleteKey(const Key: String): Boolean;
    procedure GetStrings(AStrings: TStrings);
  public
    constructor Create; overload; virtual;
    constructor Create(AEncrypted: Boolean); overload; virtual;
    constructor Create(const AName: String; AEncrypted: Boolean); overload; virtual;
    constructor Create(const AName: String; AEncrypted: Boolean; AUseWindowsFolder: Boolean); overload; virtual;
    constructor Create(const AName: String); overload; virtual;
    destructor Destroy; override;
    // Properties 
    property AppName: string read GetAppName write SetAppName;
    property IniFile: string read GetIniFile write SetIniFile;
    property IsEmpty: Boolean read GetIsEmpty;
    property Encrypted: Boolean read GetEncrypted write SetEncrypted;
    property Name: String read GetName write SetName;
    property ReadEncrypted: Boolean read GetReadEncrypted write SetReadEncrypted;
    property WriteEncrypted: Boolean read GetWriteEncrypted write SetWriteEncrypted;
    // Value properties 
    property Booleans[const Key: String]: Boolean read GetBoolean write SetBoolean;
    property Floats[const Key: String]: Extended read GetFloat write SetFloat;
    property Integers[const Key: String]: Integer read GetInteger write SetInteger;
    property Strings[const Key: String]: String read GetString write SetString;
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaIni                                                                    
  //---------------------------------------------------------------------------

constructor TcaIni.Create;
begin
  inherited;
  Initialize;
end;

constructor TcaIni.Create(AEncrypted: Boolean);
begin
  inherited Create;
  FEncrypted := AEncrypted;
  Initialize;
end;

constructor TcaIni.Create(const AName: String; AEncrypted: Boolean);
begin
  inherited Create;
  FName := AName;
  FEncrypted := AEncrypted;
  Initialize;
end;

constructor TcaIni.Create(const AName: String; AEncrypted: Boolean; AUseWindowsFolder: Boolean);
begin
  inherited Create;
  FName := AName;
  FEncrypted := AEncrypted;
  FUseWindowsFolder := AUseWindowsFolder;
  Initialize;
end;

constructor TcaIni.Create(const AName: String);
begin
  inherited Create;
  FName := AName;
  Initialize;
end;

procedure TcaIni.Initialize;
begin
  FAppName := Utils.AppName;
  FCrypt := TcaCrypt.Create(57233, 23190);
  FList := TStringList.Create;
  Load;
end;

destructor TcaIni.Destroy;
begin
  try Save; except end;
  FList.Free;
  inherited;
end;

  // Interface methods 

function TcaIni.DeleteKey(const Key: string): Boolean;
var
  Index: Integer;
begin
  Result := False;
  Index := FList.IndexOfName(Key);
  if Index >= 0 then
    begin
      FList.Delete(Index);
      Result := True;
    end;
end;

procedure TcaIni.GetStrings(AStrings: TStrings);
begin
  AStrings.Assign(FList);
end;

  // Private methods 

function TcaIni.CheckDecrypt(const AValue: String): String;
begin
  if FEncrypted or FReadEncrypted then
    Result := FCrypt.Decrypt(AValue, cStartKey)
  else
    Result := AValue;
end;

function TcaIni.CheckEncrypt(const AValue: String): String;
begin
  if FEncrypted or FWriteEncrypted then
    Result := FCrypt.Encrypt(AValue, cStartKey)
  else
    Result := AValue;
end;

procedure TcaIni.Load;
begin
  if FIniFile = '' then UpdateIniPath;
  if FileExists(FIniFile) then FList.LoadFromFile(FIniFile);
end;

procedure TcaIni.Save;
begin
  if FIniFile <> '' then FList.SaveToFile(FIniFile);
end;

procedure TcaIni.UpdateIniPath;
var
  AppName: string;  
  Index: Integer;
  IniName: String;
  IniPath: String;
begin
  IniName := FName;
  if IniName = '' then
    begin
      IniName := ClassName;
      Utils.DeleteFromStart(IniName, 1);
      Index := 0;
      while Index <= Length(IniName) do
        begin
          if IniName[Index] in ['A'..'Z'] then Break;
          Inc(Index);
        end;
      if Index < Length(IniName) then
        Utils.DeleteFromStart(IniName, Index - 1);
    end;
  AppName := FAppName;
  Utils.DeleteFromChar(AppName, '.', False);
  if FUseWindowsFolder then
    begin
      IniPath := Utils.GetWindowsFolder;
      FIniFile := IniPath + IniName + '.ini';
    end
  else
    begin
      IniPath := Utils.AppPath + 'Config\';
      if not DirectoryExists(IniPath) then CreateDir(IniPath);
      if IniName = 'Ini' then IniName := '';
      FIniFile := StringReplace(IniPath + AppName + IniName + '.ini', '..', '.', [rfReplaceAll]);
    end;
end;

  // Property methods 

function TcaIni.GetAppName: string;
begin
  Result := FAppName;
end;

function TcaIni.GetEncrypted: Boolean;
begin
  Result := FEncrypted;
end;

function TcaIni.GetName: String;
begin
  Result := FName;
end;

function TcaIni.GetReadEncrypted: Boolean;
begin
  Result := FReadEncrypted;
end;

function TcaIni.GetWriteEncrypted: Boolean;
begin
  Result := FWriteEncrypted;
end;

procedure TcaIni.SetAppName(const Value: string);
begin
  Save;
  FAppName := Value;
  FIniFile := '';
  Load;
end;

procedure TcaIni.SetEncrypted(const Value: Boolean);
begin
  FEncrypted := Value;
  FReadEncrypted := False;
  FWriteEncrypted := False;
end;

procedure TcaIni.SetIniFile(const Value: string);
begin
  FIniFile := Value;
end;

procedure TcaIni.SetName(const Value: String);
begin
  Save;
  FName := Value;
  FIniFile := '';
  Load;
end;

procedure TcaIni.SetReadEncrypted(const Value: Boolean);
begin
  FReadEncrypted := Value;
  FEncrypted := False;
end;

procedure TcaIni.SetWriteEncrypted(const Value: Boolean);
begin
  FWriteEncrypted := Value;
  FEncrypted := False;
end;

  // Property methods 

function TcaIni.GetBoolean(const Key: String): Boolean;
begin
  Result := Utils.StringToBoolean(CheckDecrypt(FList.Values[Key]));
end;

function TcaIni.GetIniFile: string;
begin
  Result := FIniFile;
end;

function TcaIni.GetIsEmpty: Boolean;
begin
  Result := FList.Count = 0;
end;

function TcaIni.GetFloat(const Key: String): Extended;
begin
  Result := Utils.StringToExtended(CheckDecrypt(FList.Values[Key]));
end;

function TcaIni.GetInteger(const Key: String): Integer;
begin
  Result := Utils.StringToInteger(CheckDecrypt(FList.Values[Key]));
end;

function TcaIni.GetString(const Key: String): String;
begin
  Result := CheckDecrypt(FList.Values[Key]);
end;

procedure TcaIni.SetBoolean(const Key: String; const Value: Boolean);
begin
  FList.Values[Key] := CheckEncrypt(Utils.BooleanToString(Value));
end;

procedure TcaIni.SetFloat(const Key: String; const Value: Extended);
begin
  FList.Values[Key] := CheckEncrypt(Utils.ExtendedToString(Value, ''));
end;

procedure TcaIni.SetInteger(const Key: String; const Value: Integer);
begin
  FList.Values[Key] := CheckEncrypt(Utils.IntegerToString(Value, ''));
end;

procedure TcaIni.SetString(const Key, Value: String);
begin
  FList.Values[Key] := CheckEncrypt(Value);
end;

end.



