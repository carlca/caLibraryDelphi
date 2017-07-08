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


unit caChiSquared;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Classes,
  SysUtils,

  // ca units
  caClasses,
  caUtils;

type

  TcaDegreesOfFreedom = 1..100;

 //---------------------------------------------------------------------------
 // IcaChiSquared
 //---------------------------------------------------------------------------

  IcaChiSquared = interface
  ['{19B9EED0-E500-4148-AC4C-42C853B23444}']
    // Property methods
    function GetDegreesOfFreedom: TcaDegreesOfFreedom;
    function GetScore: Double;
    function GetSucceeded: Boolean;
    procedure SetDegreesOfFreedom(const Value: TcaDegreesOfFreedom);
    procedure SetScore(const Value: Double);
    // Properties
    property DegreesOfFreedom: TcaDegreesOfFreedom read GetDegreesOfFreedom write SetDegreesOfFreedom;
    property Score: Double read GetScore write SetScore;
    property Succeeded: Boolean read GetSucceeded;
  end;

 //---------------------------------------------------------------------------
 // TcaChiSquared
 //---------------------------------------------------------------------------

  TcaChiSquared = class(TcaInterfacedPersistent, IcaChiSquared)
  private
    FDegreesOfFreedom: TcaDegreesOfFreedom;
    FScore: Double;
    // Property methods
    function GetDegreesOfFreedom: TcaDegreesOfFreedom;
    function GetScore: Double;
    function GetSucceeded: Boolean;
    procedure SetDegreesOfFreedom(const Value: TcaDegreesOfFreedom);
    procedure SetScore(const Value: Double);
  public
    constructor Create(ADegreesOfFreedom: TcaDegreesOfFreedom; AScore: Double);
    // Properties
    property DegreesOfFreedom: TcaDegreesOfFreedom read GetDegreesOfFreedom write SetDegreesOfFreedom;
    property Score: Double read GetScore write SetScore;
    property Succeeded: Boolean read GetSucceeded;
  end;

implementation

const
  ChiTable5 : array [1..100] of double = (
        0.00393,   0.10260,   0.35184,   0.71069,   1.14548,
        1.63550,   2.16742,   2.73267,   3.32501,   3.94043,
        4.57483,   5.22583,   5.89182,   6.57062,   7.26105,
        7.96191,   8.67172,   9.39056,  10.11694,  10.85083,
       11.59140,  12.33807,  13.09052,  13.84863,  14.61182,
       15.37878,  16.15155,  16.92749,  17.70816,  18.49274,
       19.28040,  20.07227,  20.86670,  21.66400,  22.46460,
       23.26904,  24.07462,  24.88416,  25.69492,  26.50879,
       27.32541,  28.14441,  28.96411,  29.78809,  30.61203,
       31.43970,  32.26804,  33.09814,  33.92975,  34.76410,
       35.59946,  36.43713,  37.27533,  38.11624,  38.95805,
       39.80054,  40.64520,  41.49191,  42.33963,  43.18726,
       44.03735,  44.88983,  45.74075,  46.59570,  47.44873,
       48.30560,  49.16223,  50.02051,  50.87929,  51.73950,
       52.59998,  53.46167,  54.32449,  55.18835,  56.05316,
       56.91998,  57.78642,  58.65472,  59.52243,  60.39063,
       61.26169,  62.13312,  63.00482,  63.87671,  64.74869,
       65.62329,  66.49786,  67.37231,  68.24927,  69.12598,
       70.00374,  70.88110,  71.75940,  72.64001,  73.52005,
       74.39941,  75.28098,  76.16473,  77.04616,  77.92969);

  ChiTable95 : array [1..100] of double = (
        3.84155,   5.99121,   7.81494,   9.48730,  11.07025,
       12.59125,  14.06708,  15.50781,  16.91895,  18.30750,
       19.67499,  21.02600,  22.36160,  23.68439,  24.99619,
       26.29639,  27.58713,  28.86932,  30.14412,  31.41022,
       32.66991,  33.92517,  35.17242,  36.41418,  37.65297,
       38.88535,  40.11314,  41.33713,  42.55791,  43.77365,
       44.98444,  46.19385,  47.40074,  48.60237,  49.80087,
       50.99854,  52.19215,  53.38387,  54.57298,  55.75867,
       56.94305,  58.12363,  59.30412,  60.48187,  61.65733,
       62.82921,  64.00143,  65.17090,  66.33797,  67.50488,
       68.66821,  69.83215,  70.99312,  72.15340,  73.31123,
       74.46899,  75.62485,  76.77902,  77.93175,  79.08096,
       80.23144,  81.38115,  82.52792,  83.67432,  84.82056,
       85.96436,  87.10838,  88.25027,  89.39014,  90.53078,
       91.66969,  92.80701,  93.94562,  95.08011,  96.21620,
       97.35123,  98.48530,  99.61555, 100.74802, 101.87988,
      103.00816, 104.13910, 105.26661, 106.39389, 107.52106,
      108.64822, 109.77219, 110.89798, 112.02068, 113.14545,
      114.26723, 115.38956, 116.51253, 117.63268, 118.75181,
      119.87183, 120.99098, 122.10747, 123.22506, 124.34196);

 //---------------------------------------------------------------------------
 // TcaChiSquared
 //---------------------------------------------------------------------------

constructor TcaChiSquared.Create(ADegreesOfFreedom: TcaDegreesOfFreedom; AScore: Double);
begin
  inherited Create;
  FDegreesOfFreedom := ADegreesOfFreedom;
  FScore := AScore;
end;

 // Property methods

function TcaChiSquared.GetDegreesOfFreedom: TcaDegreesOfFreedom;
begin
  Result := FDegreesOfFreedom;
end;

function TcaChiSquared.GetScore: Double;
begin
  Result := FScore;
end;

function TcaChiSquared.GetSucceeded: Boolean;
var
  Chi5Succeeded: Boolean;
  Chi95Succeeded: Boolean;
begin
  Chi5Succeeded := ChiTable5[FDegreesOfFreedom] <= FScore;
  Chi95Succeeded := FScore <= ChiTable95[FDegreesOfFreedom];
  Result := Chi5Succeeded and Chi95Succeeded;
end;

procedure TcaChiSquared.SetDegreesOfFreedom(const Value: TcaDegreesOfFreedom);
begin
  FDegreesOfFreedom := Value;
end;

procedure TcaChiSquared.SetScore(const Value: Double);
begin
  FScore := Value;
end;

end.
