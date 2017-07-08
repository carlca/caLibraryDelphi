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


unit caAppConfig;

interface

uses

  // standard Delphi units...
  Windows,
  Classes,
  SysUtils,
  TypInfo,
  ActiveX,
  ShlObj,

  // ca units...
  caXml,
  caRtti,
  caClasses,
  caUtils;

type

  //---------------------------------------------------------------------------
  // IcaAppConfig
  //---------------------------------------------------------------------------

  IcaAppConfig = interface
  ['{93FC81B2-B238-4622-8836-01F580000F0D}']
  end;

  //---------------------------------------------------------------------------
  // TcaAppConfig
  //---------------------------------------------------------------------------

  TcaAppConfig = class(TInterfacedObject, IcaAppConfig)
  private
    // private members...
    FPath: string;
    FXmlReader: IcaXmlReader;
    FValues: TStrings;
    // private methods...
    function AllUsersAppDataPath: WideString;
    function GetSpecialFolder(APathID: Integer): WideString;
    function PathJoin(const APaths: array of WideString; ADeleteFinalDelimiter: Boolean): WideString; 
    procedure BuildPath;
    procedure CreateObjects;
    procedure CreateDefaultXml;
    procedure LoadXml;
    // xml events...
    procedure XmlTagEvent(Sender: TObject; const ATag, AAttributes: string; ALevel: Integer);
  public
    // lifetime...
    constructor Create;
    destructor Destroy; override;
    // public methods...
    procedure Save;
  end;

implementation

//---------------------------------------------------------------------------
// TcaAppConfig
//---------------------------------------------------------------------------

// lifetime...

constructor TcaAppConfig.Create;
begin
  inherited;
  BuildPath;
  CreateObjects;
  if not FileExists(FPath) then
    CreateDefaultXml;
  LoadXml;
end;

destructor TcaAppConfig.Destroy;
begin
  Save;
  FXmlReader := nil;
  FValues.Free;
  inherited;
end;

// public methods...

procedure TcaAppConfig.Save;
begin
  CreateDefaultXml;
end;

// private methods...

function TcaAppConfig.AllUsersAppDataPath: WideString;
begin
  Result := GetSpecialFolder(CSIDL_COMMON_APPDATA);
end;

function TcaAppConfig.GetSpecialFolder(APathID: Integer): WideString;
var
  pidl: PItemIDList;
  Path: array[0..MAX_PATH] of WideChar;
begin
  Result := '';
  if Succeeded(SHGetSpecialFolderLocation(0, APathID, pidl)) then
    if SHGetPathFromIDListW(pidl, Path) then
      Result := Path;
end;

function TcaAppConfig.PathJoin(const APaths: array of WideString; ADeleteFinalDelimiter: Boolean): WideString;
var
  Path: string;
  TempPath: string;
  ResultPath: string;
begin
  ResultPath := '';
  for Path in APaths do
    begin
      TempPath := Path;
      if Length(TempPath) > 0 then
        begin
          if TempPath[1] = PathDelim then
            Delete(TempPath, 1, 1);
          TempPath := IncludeTrailingPathDelimiter(TempPath);
          ResultPath := ResultPath + TempPath;
          ResultPath := IncludeTrailingPathDelimiter(ResultPath);
        end;
    end;
  if ADeleteFinalDelimiter and IsPathDelimiter(ResultPath, Length(ResultPath)) then
    Delete(ResultPath, Length(ResultPath), 1);
  Result := ResultPath;
end;

procedure TcaAppConfig.BuildPath;
var
  Path: string;
begin
  Path := PathJoin([AllUsersAppDataPath, 'Inspiration Matters', 'Inspired Signage', 'SFXBuilder'], False);
  if not DirectoryExists(Path) then
    CreateDir(Path);
  FPath := PathJoin([Path, Utils.AppName + '.config'], True);
end;

procedure TcaAppConfig.CreateObjects;
begin
  FXmlReader := TcaXmlReader.Create;
  FValues := TStringList.Create;
end;

procedure TcaAppConfig.CreateDefaultXml;
var
  XmlBuilder: IcaXmlBuilder;
  RttiList: IcaRttiList;
  Index: Integer;
begin
  XmlBuilder := TcaXmlBuilder.CreateUtf8;
  XmlBuilder.AddTag('configuration');
  XmlBuilder.AddTag('appSettings');
  RttiList := TcaRttiList.Create(Self);
  for Index := 0 to Pred(RttiList.Count) do
    begin
      XmlBuilder.AddTag('add', Format('key="%s" value="%s"', [RttiList[Index].PropName, RttiList[Index].PropValueAsString]));
      XmlBuilder.EndTag;
    end;
  XmlBuilder.EndTag;
  XmlBuilder.EndTag;
  XmlBuilder.SaveXmlToFile(FPath);
end;

procedure TcaAppConfig.LoadXml;
begin
  FXmlReader.OnTag := XmlTagEvent;
  FXmlReader.LoadFromXml(FPath);
  FXmlReader.Parse;
end;

// xml events...

procedure TcaAppConfig.XmlTagEvent(Sender: TObject; const ATag, AAttributes: string; ALevel: Integer);
var
  Attributes: IcaXmlAttributes;
  Key: string;
  Value: Variant;
begin
  Attributes := TcaXmlAttributes.Create(AAttributes);
  if ATag = 'add' then
    begin
      Key := Attributes['key'];
      Value := Attributes['value'];
      FValues.Values[Key] := Value;
      if IsPublishedProp(Self, Key) then
        SetPropValue(Self, Key, Value);
    end;
end;

end.
