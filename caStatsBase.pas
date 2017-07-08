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


unit caStatsBase;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Classes,
  Sysutils,
  Math,
  Contnrs,

  // ca units 
  caClasses,
  caTypes,
  caUtils,
  caVMT,
  caConsts,
  caMtxVec;

type

  EcaStatsException = class(Exception);

  TcaStatsFloat = Extended;

  TcaStatsFloatFunc = function: TcaStatsFloat of object;

  //----------------------------------------------------------------------------
  // TcaStatsFloatFunctionEnumerator                                            
  //----------------------------------------------------------------------------

  TcaStatsFloatFunctionEnumerator = class(TObject)
  private
    // Private fields 
    FInstance: TObject;
  public
    // Create/Destroy 
    constructor Create(AInstance: TObject);
    // Public methods 
    function AsString: string;
  end;

  //----------------------------------------------------------------------------
  // IcaStatsAsString                                                           
  //----------------------------------------------------------------------------

  IcaStatsAsString = interface
  ['{E73BF2C5-BA59-4BC7-8706-870F75BAFE32}']
    // Interface methods 
    function AsString: string;
  end;

  //----------------------------------------------------------------------------
  // IcaStatsVector                                                             
  //----------------------------------------------------------------------------

  IcaStatsVector = interface
  ['{1A7EC23A-AF22-4C9A-869B-3E69DA45EE08}']
    // Interface methods 
    function Avg: TcaStatsFloat;
    function DegFree: Integer;
    function HasMore: Boolean;
    function HasValues: Boolean;
    function IndexOfFirstNonZero: Integer;
    function IndexOfLastNonZero: Integer;
    function IsEmpty: Boolean;
    function Max: TcaStatsFloat;
    function Min: TcaStatsFloat;
    function N: Integer;
    function Next: TcaStatsFloat;
    function Sum: TcaStatsFloat;
    function SumOfSquares: TcaStatsFloat;
    function SumSqrDev: TcaStatsFloat;
    function Variance: TcaStatsFloat;
    function X(Index: Integer): TcaStatsFloat;
    procedure Add(AValue: TcaStatsFloat);
    procedure Reset; overload;
    procedure Reset(var AVariable: TcaStatsFloat; AValue: TcaStatsFloat = 0); overload;
    // Properties 
  end;

  //----------------------------------------------------------------------------
  // TcaStatsVector                                                             
  //----------------------------------------------------------------------------

  TcaStatsVector = class(TcaDblVec, IcaStatsVector, IcaStatsAsString)
  private
    // Private fields 
    FIteratorIndex: Integer;
    FMathUtils: IcaMathUtils;
  protected
    // Protected methods 
    procedure DoAdd(AValue: TcaStatsFloat); virtual;
  public
    // Create/Destroy 
    constructor Create(ACount: Integer = 0); overload;
    constructor Create(ADblVec: IcaDblVec); overload;
    destructor Destroy; override;
    // Interface methods - IcaStatsVector 
    function DegFree: Integer;
    function HasMore: Boolean;
    function HasValues: Boolean;
    function IndexOfFirstNonZero: Integer;
    function IndexOfLastNonZero: Integer;
    function IsEmpty: Boolean;
    function Max: TcaStatsFloat;
    function Min: TcaStatsFloat;
    function N: Integer;
    function X(Index: Integer): TcaStatsFloat;
    procedure Add(AValue: TcaStatsFloat);
    procedure Reset(var AVariable: TcaStatsFloat; AValue: TcaStatsFloat = 0); overload;
    procedure Reset; overload;
    // Interface methods - IcaStatsAsString 
    function AsString: string;
  published
    // Interface methods - IcaStatsVector 
    function Avg: TcaStatsFloat;
    function Next: TcaStatsFloat;
    function StdDev: TcaStatsFloat;
    function Sum: TcaStatsFloat;
    function SumOfSquares: TcaStatsFloat;
    function SumSqrDev: TcaStatsFloat;
    function Variance: TcaStatsFloat;
  end;

implementation


  //----------------------------------------------------------------------------
  // TcaStatsFloatFunctionEnumerator                                            
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaStatsFloatFunctionEnumerator.Create(AInstance: TObject);
begin
  inherited Create;
  FInstance := AInstance;
end;

  // Public methods 

function TcaStatsFloatFunctionEnumerator.AsString: string;
var
  Fn: TcaStatsFloatFunc;
  Index: Integer;
  Method: TcaVmtMethod;
  Strings: TStringList;
  VMT: IcaVmt;
begin
  Strings := Auto(TStringList.Create).Instance;
  VMT := TcaVmt.Create(FInstance.ClassType);
  for Index := 0 to Pred(VMT.MethodCount) do
    begin
      Method := VMT.Methods[Index];
      TMethod(Fn).Code := FInstance.MethodAddress(Method.Name);
      TMethod(Fn).Data := FInstance;
      Strings.Add(Method.Name + cTab + FloatToStr(Fn));
    end;
  Result := Strings.Text;
end;

  //----------------------------------------------------------------------------
  // TcaStatsVector                                                             
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaStatsVector.Create(ACount: Integer = 0);
begin
  inherited Create;
  FMathUtils := Utils as IcaMathUtils;
end;

constructor TcaStatsVector.Create(ADblVec: IcaDblVec);
var
  Index: Integer;
begin
  inherited Create;
  FMathUtils := Utils as IcaMathUtils;
  for Index := 0 to Pred(ADblVec.Count) do
    Add(ADblVec[Index]);
end;

destructor TcaStatsVector.Destroy;
begin
  FMathUtils := nil;
  inherited;
end;

  // Interface methods - IcaXYVector 

function TcaStatsVector.Avg: TcaStatsFloat;
begin
  Result := Sum / N;
end;

function TcaStatsVector.DegFree: Integer;
begin
  Result := N - 2;
end;

function TcaStatsVector.HasMore: Boolean;
begin
  Result := FIteratorIndex < N;
end;

function TcaStatsVector.HasValues: Boolean;
begin
  Result := inherited Count > 0;
end;

function TcaStatsVector.IndexOfFirstNonZero: Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Pred(N) do
    begin
      if FMathUtils.IsN0(X(Index)) then
        begin
          Result := Index;
          Break;
        end;
    end
end;

function TcaStatsVector.IndexOfLastNonZero: Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := Pred(N) downto 0 do
    begin
      if FMathUtils.IsN0(X(Index)) then
        begin
          Result := Index;
          Break;
        end;
    end
end;

function TcaStatsVector.IsEmpty: Boolean;
begin
  Result := not HasValues;
end;

function TcaStatsVector.Max: TcaStatsFloat;
begin
  N;
  Reset(Result, -MaxDouble);
  while HasMore do
    Result := Math.Max(Result, Next);
  Reset;
end;

function TcaStatsVector.Min: TcaStatsFloat;
begin
  N;
  Reset(Result, MaxDouble);
  while HasMore do
    Result := Math.Min(Result, Next);
  Reset;
end;

function TcaStatsVector.N: Integer;
begin
  Result := inherited Count;
  if Result = 0 then
    raise EcaStatsException.Create('TcaStatsVector.N = 0');
end;

function TcaStatsVector.Next: TcaStatsFloat;
begin
  Result := X(FIteratorIndex);
  Inc(FIteratorIndex);
end;

function TcaStatsVector.Sum: TcaStatsFloat;
begin
  Reset(Result);
  while HasMore do
    Result := Result + Next;
  Reset;
end;

function TcaStatsVector.StdDev: TcaStatsFloat;
begin
  Result := Sqrt(Variance);
end;

function TcaStatsVector.SumOfSquares: TcaStatsFloat;
begin
  Reset(Result);
  while HasMore do
    Result := Result + Sqr(Next);
  Reset;
end;

function TcaStatsVector.SumSqrDev: TcaStatsFloat;  // EXCEL.DEVSQ(X) 
var
  Average: TcaStatsFloat;
begin
  Reset(Result);
  Average := Avg;
  while HasMore do
    Result := Result + Sqr(Next - Average);
end;

function TcaStatsVector.Variance: TcaStatsFloat;
begin
  Result := 0;
  if N > 2 then
    Result := SumSqrDev / (N - 1);
end;

function TcaStatsVector.X(Index: Integer): TcaStatsFloat;
begin
  Result := inherited Items[Index];
end;

procedure TcaStatsVector.Add(AValue: TcaStatsFloat);
begin
  inherited Add(AValue);
  DoAdd(AValue);
end;

procedure TcaStatsVector.Reset;
begin
  FIteratorIndex := 0;
end;

procedure TcaStatsVector.Reset(var AVariable: TcaStatsFloat; AValue: TcaStatsFloat = 0);
begin
  Reset;
  AVariable := AValue;
end;

  // Interface methods - IcaStatsAsString 

function TcaStatsVector.AsString: string;
var
  FunctionEnumerator: TcaStatsFloatFunctionEnumerator;
begin
  FunctionEnumerator := Auto(TcaStatsFloatFunctionEnumerator.Create(Self)).Instance;
  Result := FunctionEnumerator.AsString;
end;

  // Protected methods 

procedure TcaStatsVector.DoAdd(AValue: TcaStatsFloat);
begin
  // Virtual 
end;

end.
