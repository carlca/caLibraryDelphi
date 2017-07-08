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


unit caProfiler;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  SysUtils,
  Classes,
  Math,

  // ca units
  caClasses,
  caUtils,
  caConsts,
  caLog;

type

  EcaProfilerException = class(EcaException);

  TcaMethodType = (mtEnter, mtExit);

 //---------------------------------------------------------------------------
 // IcaProfiler
 //---------------------------------------------------------------------------

  IcaProfiler = interface
  ['{62E0B12A-FF12-426D-BC1F-F18832893E0F}']
    // Property methods
    function GetActive: Boolean;
    function GetShowMethodTrace: Boolean;
    function GetTrackMethodUsage: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetShowMethodTrace(const Value: Boolean);
    procedure SetTrackMethodUsage(const Value: Boolean);
    // Public methods
    procedure AddToMethodSkipList(const AMethodName: String);
    procedure ClearMethodSkipList;
    procedure EnterMethod(const AMethodName: String; const AStringParam: String = ''; ANumParam: Double = cMinDouble);
    procedure ExitMethod(const AMethodName: String; const AStringParam: String = ''; ANumParam: Double = cMinDouble);
    procedure ShowMethodUsage;
    // Public properties
    property Active: Boolean read GetActive write SetActive;
    property ShowMethodTrace: Boolean read GetShowMethodTrace write SetShowMethodTrace;
    property TrackMethodUsage: Boolean read GetTrackMethodUsage write SetTrackMethodUsage;
  end;

 //---------------------------------------------------------------------------
 // TcaProfiler
 //---------------------------------------------------------------------------

  TcaProfiler = class(TInterfacedObject, IcaProfiler)
  private
    // Property fields
    FActive: Boolean;
    FShowMethodTrace: Boolean;
    FTrackMethodUsage: Boolean;
    // Private fields
    FMethodList: IcaStringList;
    FMethodSkipList: IcaStringList;
    FStack: IcaStringStack;
    // Property methods
    function GetActive: Boolean;
    function GetShowMethodTrace: Boolean;
    function GetTrackMethodUsage: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetShowMethodTrace(const Value: Boolean);
    procedure SetTrackMethodUsage(const Value: Boolean);
    // Private methods
    function Indent(AValue: Integer): String;
    procedure CreateInterfaceObjects;
    procedure LogMethod(const AMethodName, AStringParam: String; ANumParam: Double;
      AMethodType: TcaMethodType; ALevel: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    // Public methods
    procedure AddToMethodSkipList(const AMethodName: String);
    procedure ClearMethodSkipList;
    procedure EnterMethod(const AMethodName: String; const AStringParam: String = ''; ANumParam: Double = cMinDouble);
    procedure ExitMethod(const AMethodName: String; const AStringParam: String = ''; ANumParam: Double = cMinDouble);
    procedure ShowMethodUsage;
    // Public properties
    property Active: Boolean read GetActive write SetActive;
    property ShowMethodTrace: Boolean read GetShowMethodTrace write SetShowMethodTrace;
    property TrackMethodUsage: Boolean read GetTrackMethodUsage write SetTrackMethodUsage;
  end;

 //---------------------------------------------------------------------------
 // CProfilerFactory
 //---------------------------------------------------------------------------

  CProfilerFactory = class
  public
    class function Instance: IcaProfiler;
  end;

var
  Profiler: IcaProfiler;

implementation

 //---------------------------------------------------------------------------
 // TcaProfiler
 //---------------------------------------------------------------------------

constructor TcaProfiler.Create;
begin
  inherited;
  CreateInterfaceObjects;
  FActive := True;
  FShowMethodTrace := True;
end;

destructor TcaProfiler.Destroy;
begin
  inherited;
end;

 // Public methods

procedure TcaProfiler.AddToMethodSkipList(const AMethodName: String);
begin
  FMethodSkipList.Add(AMethodName);
end;

procedure TcaProfiler.ClearMethodSkipList;
begin
  FMethodSkipList.Clear;
end;

procedure TcaProfiler.EnterMethod(const AMethodName: String; const AStringParam: String = ''; ANumParam: Double = cMinDouble);
var
  Level: Integer;
begin
  if FActive then
    begin
      if not FMethodSkipList.Contains(AMethodName) then
        begin
          if FShowMethodTrace then
            begin
              Level := FStack.Push(AMethodName) + 1;
              LogMethod(AMethodName, AStringParam, ANumParam, mtEnter, Level);
            end;
          if FTrackMethodUsage then
            FMethodList.Add(AMethodName);
        end;
    end;
end;

procedure TcaProfiler.ExitMethod(const AMethodName: String; const AStringParam: String = ''; ANumParam: Double = cMinDouble);
var
  Level: Integer;
begin
  if FActive then
    begin
      if not FMethodSkipList.Contains(AMethodName) then
        begin
          if FShowMethodTrace then
            begin
              if FStack.Peek <> AMethodName then
                raise EcaProfilerException.Create('Exits do not match Enters');
              Level := FStack.Size;
              LogMethod(AMethodName, AStringParam, ANumParam, mtExit, Level);
              FStack.Pop;
            end;
          if FTrackMethodUsage then
            FMethodList.Add(AMethodName);
        end;
    end;
end;

procedure TcaProfiler.ShowMethodUsage;
var
  Index: Integer;
  MethodIndex: Integer;
  MethodName: String;
  MethodUsage: IcaStringList;
  MethodCount: Integer;
begin
  MethodUsage := TcaStringList.Create;
  // Build list
  for Index := 0 to FMethodList.High do
    begin
      MethodName := FMethodList[Index];
      MethodIndex := MethodUsage.IndexOf(MethodName);
      if MethodIndex >= 0 then
        begin
          MethodCount := Integer(MethodUsage.Objects[MethodIndex]);
          Inc(MethodCount);
          MethodUsage.Objects[MethodIndex] := Pointer(MethodCount);
        end
      else
        MethodUsage.AddObject(MethodName, Pointer(1));
    end;
  // Send list to log
  for Index := 0 to MethodUsage.High do
    Log.Send(MethodUsage[Index] + '  [ ' + IntToStr(Integer(MethodUsage.Objects[Index])) + ' ]');
end;

 // Private methods

function TcaProfiler.Indent(AValue: Integer): String;
begin
  Result := Utils.BuildString(#32, AValue * 4);
end;

procedure TcaProfiler.CreateInterfaceObjects;
begin
  FMethodList := TcaStringList.Create;
  FMethodSkipList := TcaStringList.Create;
  FStack := TcaStringList.Create;
end;

procedure TcaProfiler.LogMethod(const AMethodName, AStringParam: String; ANumParam: Double;
  AMethodType: TcaMethodType; ALevel: Integer);
var
  Arrows: String;
  TraceStr: String;
begin
  case AMethodType of
    mtEnter:  Arrows := '>> ';
    mtExit:   Arrows := '<< ';
  end;

  TraceStr := Indent(ALevel - 1) + Arrows + AMethodName;
  if AStringParam <> '' then
    TraceStr := TraceStr + ' - ' + AStringParam + ' - ';
  if ANumParam > cMinDouble Then
    Log.Send(TraceStr, ANumParam)
  else
    Log.Send(TraceStr);

end;

 // Property methods

function TcaProfiler.GetActive: Boolean;
begin
  Result := FActive;
end;

function TcaProfiler.GetShowMethodTrace: Boolean;
begin
  Result := FShowMethodTrace;
end;

function TcaProfiler.GetTrackMethodUsage: Boolean;
begin
  Result := FTrackMethodUsage;
end;

procedure TcaProfiler.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TcaProfiler.SetShowMethodTrace(const Value: Boolean);
begin
  FShowMethodTrace := Value;
end;

procedure TcaProfiler.SetTrackMethodUsage(const Value: Boolean);
begin
  FTrackMethodUsage := Value;
end;

 //---------------------------------------------------------------------------
 // CProfilerFactory
 //---------------------------------------------------------------------------

class function CProfilerFactory.Instance: IcaProfiler;
const
  FInstance: IcaProfiler = nil;
begin
  if not Assigned(FInstance) then
    FInstance := TcaProfiler.Create;
  Result := FInstance;
end;

initialization
  Profiler := CProfilerFactory.Instance;

end.
