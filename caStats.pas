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


unit caStats;

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
  caLog,
  caStatsBase;

type

  {$IFDEF D5}
  IInvokable = IUnknown;
  {$ENDIF}

  //----------------------------------------------------------------------------
  // IcaRegression                                                              
  //----------------------------------------------------------------------------

  IcaRegression = interface(IInvokable)
  ['{6B636796-F4B8-4F19-9BB2-9EFF853BCA9D}']
    // Interface methods 
    function Correlation: TcaStatsFloat;
    function CoVariance: TcaStatsFloat;
    function N: Integer;
    function ResidualSumSqr: TcaStatsFloat;
    function RSquared: TcaStatsFloat;
    function S2: TcaStatsFloat;
    function Slope: TcaStatsFloat;
    function StdErrSlope: TcaStatsFloat; 
    function StdErrY: TcaStatsFloat;
    function STEYX: TcaStatsFloat;
    function SumOfProducts: TcaStatsFloat;
    function Sxx: TcaStatsFloat;
    function Sxy: TcaStatsFloat;
    function Syy: TcaStatsFloat;
    function T_Ratio: TcaStatsFloat;
    function X: TcaStatsVector;
    function Y: TcaStatsVector;
    procedure AddXY(AX, AY: Double);
    procedure Clear;
  end;

  //----------------------------------------------------------------------------
  // TcaRegression                                                              
  //----------------------------------------------------------------------------

  {$M+}

  TcaRegression = class(TInterfacedObject, IcaRegression, IcaStatsAsString)
  private
    // Private fields 
    FMathUtils: IcaMathUtils;
    FX: TcaStatsVector;
    FY: TcaStatsVector;
    // Private methods 
    function CheckReset: Boolean;
    function Divide(ANumerator, ADenominator: TcaStatsFloat): TcaStatsFloat;
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
    // Abstract virtual methods 
    function GetS2: TcaStatsFloat; virtual; abstract;
    function GetSlope: TcaStatsFloat; virtual; abstract;
    function GetStdErrSlope: TcaStatsFloat; virtual; abstract;
    function GetYIntercept: TcaStatsFloat; virtual; abstract;
    // Interface methods - IcaRegression 
    function X: TcaStatsVector;
    function Y: TcaStatsVector;
    procedure AddXY(AX, AY: Double);
    procedure Clear;
    // Interface methods - IcaStatsAsString 
    function AsString: string;
  published
    // Interface methods - IcaRegression 
    function Correlation: TcaStatsFloat;
    function CoVariance: TcaStatsFloat;
    function YIntercept: TcaStatsFloat;
    function N: Integer;
    function ResidualSumSqr: TcaStatsFloat;
    function RSquared: TcaStatsFloat;
    function S2: TcaStatsFloat;
    function Slope: TcaStatsFloat;
    function StdErrSlope: TcaStatsFloat;
    function StdErrY: TcaStatsFloat;
    function STEYX: TcaStatsFloat;
    function SumOfProducts: TcaStatsFloat;
    function Sxx: TcaStatsFloat;
    function Sxy: TcaStatsFloat;
    function Syy: TcaStatsFloat;
    function T_Ratio: TcaStatsFloat;
  end;

  {$M-}

  //----------------------------------------------------------------------------
  // TcaForcedOriginRegression                                                  
  //----------------------------------------------------------------------------

  TcaForcedOriginRegression = class(TcaRegression)
  public
    function GetS2: TcaStatsFloat; override;
    function GetSlope: TcaStatsFloat; override;
    function GetStdErrSlope: TcaStatsFloat; override;
    function GetYIntercept: TcaStatsFloat; override;
  end;

  //----------------------------------------------------------------------------
  // TcaInterceptRegression                                                     
  //----------------------------------------------------------------------------

  TcaInterceptRegression = class(TcaRegression)
  public
    function GetS2: TcaStatsFloat; override;
    function GetSlope: TcaStatsFloat; override;
    function GetStdErrSlope: TcaStatsFloat; override;
    function GetYIntercept: TcaStatsFloat; override;
  end;

implementation

  //----------------------------------------------------------------------------
  // TcaRegression                                                              
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaRegression.Create;
begin
  inherited Create;
  FMathUtils := Utils as IcaMathUtils;
  FX := TcaStatsVector.Create;
  FY := TcaStatsVector.Create;
end;

destructor TcaRegression.Destroy;
begin
  FX.Free;
  FY.Free;
  FMathUtils := nil;
  inherited;
end;

  // Interface methods - IcaRegression 

function TcaRegression.Correlation: TcaStatsFloat;   // EXCEL.CORREL(X, Y) 
begin
  Result := CoVariance / Sqrt(FX.Variance * FY.Variance) * N;
end;

function TcaRegression.CoVariance: TcaStatsFloat;    // EXCEL.COVAR(X, Y) 
begin
  Result := Sxy / N;
end;

function TcaRegression.N: Integer;
begin
  Result := Min(FX.N, FY.N);
end;

function TcaRegression.ResidualSumSqr: TcaStatsFloat;
begin
  Result := (1 - RSquared) * FY.SumSqrDev;
end;

function TcaRegression.RSquared: TcaStatsFloat;
begin
  Result := Sqr(Correlation);
end;

function TcaRegression.S2: TcaStatsFloat;
begin
  Result := GetS2;
end;

function TcaRegression.Slope: TcaStatsFloat;
begin
  Result := GetSlope;
end;

function TcaRegression.StdErrSlope: TcaStatsFloat;
begin
  Result := GetStdErrSlope;
end;

function TcaRegression.StdErrY: TcaStatsFloat;
begin
  Result := Sqrt(S2);
end;

function TcaRegression.STEYX: TcaStatsFloat;     // EXCEL.STEYX(X, Y) 
begin
  Result := StdErrY;
end;

function TcaRegression.SumOfProducts: TcaStatsFloat;
begin
  Result := 0;
  if CheckReset then
    while FX.HasMore and FY.HasMore do
      Result := Result + (FX.Next * FY.Next);
end;

function TcaRegression.Sxx: TcaStatsFloat;
begin
  Result := FX.SumOfSquares - (Sqr(FX.Sum) / N);
end;

function TcaRegression.Sxy: TcaStatsFloat;
begin
  Result := SumOfProducts - FX.Sum * FY.Sum / N;
end;

function TcaRegression.Syy: TcaStatsFloat;
begin
  Result := FY.SumOfSquares - (Sqr(FY.Sum) / N);
end;

function TcaRegression.T_Ratio: TcaStatsFloat;
begin
  Result := Divide(Slope, StdErrSlope);
end;

function TcaRegression.X: TcaStatsVector;
begin
  Result := FX;
end;

function TcaRegression.Y: TcaStatsVector;
begin
  Result := FY;
end;

function TcaRegression.YIntercept: TcaStatsFloat;
begin
  Result := GetYIntercept;
end;

procedure TcaRegression.AddXY(AX, AY: Double);
begin
  FX.Add(AX);
  FY.Add(AY);
end;

procedure TcaRegression.Clear;
begin
  FX.Clear;
  FY.Clear;
end;

  // Interface methods - IcaStatsAsString 

function TcaRegression.AsString: string;
var
  FunctionEnumerator: TcaStatsFloatFunctionEnumerator;
begin
  FunctionEnumerator := Auto(TcaStatsFloatFunctionEnumerator.Create(Self)).Instance;
  Result := FunctionEnumerator.AsString;
end;

  // Private methods 

function TcaRegression.CheckReset: Boolean;
begin
  FX.Reset;
  FY.Reset;
  Result := FX.N = FY.N;
end;

function TcaRegression.Divide(ANumerator, ADenominator: TcaStatsFloat): TcaStatsFloat;
begin
  Result := FMathUtils.FloatDiv(ANumerator, ADenominator, 0);
end;

  //----------------------------------------------------------------------------
  // TcaForcedOriginRegression                                                  
  //----------------------------------------------------------------------------

function TcaForcedOriginRegression.GetS2: TcaStatsFloat;
begin
  Result := (FY.SumOfSquares - Sqr(SumOfProducts) / FX.SumOfSquares) / (N - 1);
end;

function TcaForcedOriginRegression.GetSlope: TcaStatsFloat;
begin
  Result := Divide(SumOfProducts, FX.SumOfSquares)
end;

function TcaForcedOriginRegression.GetStdErrSlope: TcaStatsFloat;
begin
  Result := Sqrt(Divide(S2, FX.SumOfSquares));
end;

function TcaForcedOriginRegression.GetYIntercept: TcaStatsFloat;
begin
  Result := 0;
end;

  //----------------------------------------------------------------------------
  // TcaInterceptRegression                                                     
  //----------------------------------------------------------------------------

function TcaInterceptRegression.GetS2: TcaStatsFloat;
begin
  Result := (Syy - (Sqr(Sxy) / Sxx)) / (N - 2);
end;

function TcaInterceptRegression.GetSlope: TcaStatsFloat;
begin
  Result := Divide(Sxy, Sxx);
end;

function TcaInterceptRegression.GetStdErrSlope: TcaStatsFloat;
begin
  Result := Sqrt(Divide(S2, Sxx));
end;

function TcaInterceptRegression.GetYIntercept: TcaStatsFloat;
begin
  Result := FY.Avg - Slope * FX.Avg;
end;

end.
