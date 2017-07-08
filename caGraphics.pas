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


unit caGraphics;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  Classes,
  Messages,
  Controls,
  Sysutils,
  Graphics,
  ExtCtrls,
  Math,

  // ca units 
  caTypes,
  caConsts,
  caClasses,
  caUtils,
  caMatrix;

type

  EcaColorError = class(EcaException);

  //---------------------------------------------------------------------------
  // IcaRect                                                                   
  //---------------------------------------------------------------------------

  IcaRect = interface
  ['{5A5B3CD1-EFC6-4A1C-96FA-2BEC66CF9B52}']
    function GetBottom: Integer;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetRect: TRect;
    function GetRight: Integer;
    function GetTop: Integer;
    function GetWidth: Integer;
    procedure SetBottom(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetRight(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    // Public
    procedure Adjust(const dL, dT, dR, dB: Integer);
    procedure SetBounds(const ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetRect(const ALeft, ATop, ARight, ABottom: Integer);
    procedure SetLeftAndWidth(const ALeft, AWidth: Integer);
    procedure SetTopAndHeight(const ATop, AHeight: Integer);
    // Properties
    property Bottom: Integer read GetBottom write SetBottom;
    property Height: Integer read GetHeight write SetHeight;
    property Left: Integer read GetLeft write SetLeft;
    property Rect: TRect read GetRect;
    property Right: Integer read GetRight write SetRight;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
  end;

  //---------------------------------------------------------------------------
  // TcaRect                                                                   
  //---------------------------------------------------------------------------

  TcaRect = class(TcaInterfacedPersistent, IcaRect)
  private
    FRect: TRect;
    function GetBottom: Integer;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetRect: TRect;
    function GetRight: Integer;
    function GetTop: Integer;
    function GetWidth: Integer;
    procedure SetBottom(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetRight(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  public
    constructor Create(aRect: TRect); overload;
    constructor Create(const aLeft, aTop, aWidth, aHeight: Integer); overload;
    // Public methods
    procedure Adjust(const dL, dT, dR, dB: Integer);
    procedure Assign(Source: TPersistent); override;
    procedure SetBounds(const aLeft, aTop, aWidth, aHeight: Integer);
    procedure SetLeftAndWidth(const ALeft, AWidth: Integer);
    procedure SetRect(const ALeft, ATop, ARight, ABottom: Integer);
    procedure SetTopAndHeight(const ATop, AHeight: Integer);
    // Properties
    property Rect: TRect read GetRect;
  published
    // Published Properties
    property Bottom: Integer read GetBottom write SetBottom;
    property Height: Integer read GetHeight write SetHeight;
    property Left: Integer read GetLeft write SetLeft;
    property Right: Integer read GetRight write SetRight;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
  end;

  //---------------------------------------------------------------------------
  // TcaBitmap                                                                 
  //---------------------------------------------------------------------------

  TcaBitmap = class(TBitmap)
  public
    function BoundsRect: TRect;
  end;

  //----------------------------------------------------------------------------
  // TcaRGB                                                                     
  //----------------------------------------------------------------------------

  TcaRGB = packed record
    Blue: Byte;
    Green: Byte;
    Red: Byte;
  end;

  //----------------------------------------------------------------------------
  // TcaHSV                                                                     
  //----------------------------------------------------------------------------

  TcaHSV = packed record
    Hue: Single;
    Saturation: Single;
    Value: Single;
  end;

  //----------------------------------------------------------------------------
  // TcaHLS                                                                     
  //----------------------------------------------------------------------------

  TcaHLS = packed record
    Hue: Single;
    Lightness: Single;
    Saturation: Single;
  end;

  //---------------------------------------------------------------------------
  // IcaColorUtils                                                             
  //---------------------------------------------------------------------------

  IcaColorUtils = interface
  ['{D0678195-78A2-43B6-BF5B-998BC0CF105E}']
    // Color conversion methods
    function ColorToRGB(AColor: TColor): TcaRGB;
    function ColorToString(AColor: TColor): String;
    function GetFlatToolbarsColor: COLORREF;
    function GetFlatToolbarsDownedColor: COLORREF;
    function GetFlatToolbarsDownedSelectedColor: COLORREF;
    function GetFlatToolbarsSelectedColor: COLORREF;
    function GetRealColor(AColor: COLORREF): COLORREF;
    function HLSToRGB(const AHLS: TcaHLS): TcaRGB;
    function HSVToRGB(const AHSV: TcaHSV): TcaRGB;
    function MakeDarker(AColor: TColor; ADelta: Integer): TColor;
    function MakeLighter(AColor: TColor; ADelta: Integer): TColor;
    function OffsetColor(BaseColor: COLORREF; DeltaR, DeltaG, DeltaB: Integer): COLORREF;
    function RGBToColor(const ARGB: TcaRGB): TColor; overload;
    function RGBToColor(const R, G, B: Integer): TColor; overload;
    function RGBToHLS(const ARGB: TcaRGB): TcaHLS; overload;
    function RGBToHLS(const R, G, B: Integer): TcaHLS; overload;
    function RGBToHSV(const ARGB: TcaRGB): TcaHSV; overload;
    function RGBToHSV(const R, G, B: Integer): TcaHSV; overload;
    function RGBToIntensity(const ARGB: TcaRGB): Integer; overload;
    function RGBToIntensity(const R, G, B: Integer): Integer; overload;
    function RGBToLightness(const ARGB: TcaRGB): Integer; overload;
    function RGBToLightness(const R, G, B: Integer): Integer; overload;
    function RGBToYIC(const ARGB: TcaRGB): Integer; overload;
    function RGBToYIC(const R, G, B: Integer): Integer; overload;
    // Bitmap utils 
    procedure CreateDisabledBitmap(Source, Destination: TBitmap);
  end;

  //---------------------------------------------------------------------------
  // TcaColorUtils                                                             
  //---------------------------------------------------------------------------

  TcaColorUtils = class(TInterfacedObject, IcaColorUtils)
  private
    // Private methods 
    function ChangeHSVValue(AColor: TColor; ADelta: Integer): TColor;
  public
    // Color conversion methods
    function ColorToRGB(AColor: TColor): TcaRGB;
    function ColorToString(AColor: TColor): String;
    function GetFlatToolbarsColor: COLORREF;
    function GetFlatToolbarsDownedColor: COLORREF;
    function GetFlatToolbarsDownedSelectedColor: COLORREF;
    function GetFlatToolbarsSelectedColor: COLORREF;
    function GetRealColor(AColor: COLORREF): COLORREF;
    function HLSToRGB(const AHLS: TcaHLS): TcaRGB;
    function HSVToRGB(const AHSV: TcaHSV): TcaRGB;
    function MakeDarker(AColor: TColor; ADelta: Integer): TColor;
    function MakeLighter(AColor: TColor; ADelta: Integer): TColor;
    function OffsetColor(BaseColor: COLORREF; DeltaR, DeltaG, DeltaB: Integer): COLORREF;
    function RGBToColor(const ARGB: TcaRGB): TColor; overload;
    function RGBToColor(const R, G, B: Integer): TColor; overload;
    function RGBToHLS(const ARGB: TcaRGB): TcaHLS; overload;
    function RGBToHLS(const R, G, B: Integer): TcaHLS; overload;
    function RGBToHSV(const ARGB: TcaRGB): TcaHSV; overload;
    function RGBToHSV(const R, G, B: Integer): TcaHSV; overload;
    function RGBToIntensity(const ARGB: TcaRGB): Integer; overload;
    function RGBToIntensity(const R, G, B: Integer): Integer; overload;
    function RGBToLightness(const ARGB: TcaRGB): Integer; overload;
    function RGBToLightness(const R, G, B: Integer): Integer; overload;
    function RGBToYIC(const ARGB: TcaRGB): Integer; overload;
    function RGBToYIC(const R, G, B: Integer): Integer; overload;
    // Bitmap utils 
    procedure CreateDisabledBitmap(Source, Destination: TBitmap);
  end;

  //----------------------------------------------------------------------------
  // IcaCustomColors                                                            
  //----------------------------------------------------------------------------

  IcaCustomColors = interface
  ['{340F8B53-2F14-4790-A269-BC97522985D9}']
    // Property methods
    function GetColorHexNums(ARow: Integer): String;
    function GetColorNames(ARow: Integer): String;
    function GetColors(ARow: Integer): TColor;
    function GetCount: Integer;
    function GetHSV(ARow: Integer): TcaHSV;
    function GetIntensity(ARow: Integer): Integer;
    function GetLightness(ARow: Integer): Integer;
    function GetMatrix: TcaMatrix;
    function GetRGB(ARow: Integer): TcaRGB;
    function GetRowCount: Integer;
    function GetYIQ(ARow: Integer): Integer;
    // Public methods
    function GetDefaultColor(Index, MaxColors: Integer): TColor;
    procedure Initialize;
    // Properties
    property ColorHexNums[ARow: Integer]: String read GetColorHexNums;
    property ColorNames[ARow: Integer]: String read GetColorNames;
    property Colors[ARow: Integer]: TColor read GetColors; default;
    property Count: Integer read GetCount;
    property HSV[ARow: Integer]: TcaHSV read GetHSV;
    property Intensity[ARow: Integer]: Integer read GetIntensity;
    property Lightness[ARow: Integer]: Integer read GetLightness;
    property Matrix: TcaMatrix read GetMatrix;
    property RGB[ARow: Integer]: TcaRGB read GetRGB;
    property RowCount: Integer read GetRowCount;
    property YIQ[ARow: Integer]: Integer read GetYIQ;
  end;

  //----------------------------------------------------------------------------
  // TcaCustomColors                                                            
  //----------------------------------------------------------------------------

  TcaCustomColors = class(TInterfacedObject, IcaCustomColors)
  private
    FMatrix: TcaMatrix;
    function GetColorHexNums(ARow: Integer): String;
    function GetColorNames(ARow: Integer): String;
    function GetColors(ARow: Integer): TColor;
    function GetCount: Integer;
    function GetHSV(ARow: Integer): TcaHSV;
    function GetIntensity(ARow: Integer): Integer;
    function GetLightness(ARow: Integer): Integer;
    function GetMatrix: TcaMatrix;
    function GetRGB(ARow: Integer): TcaRGB;
    function GetRowCount: Integer;
    function GetYIQ(ARow: Integer): Integer;
  protected
    procedure AddColorRow(const AColor: TColor; const AColorName: String);
    procedure AddColorConstants; virtual;
    procedure BuildExtraColorData;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDefaultColor(Index, MaxColors: Integer): TColor;
    procedure Initialize;
    property ColorHexNums[ARow: Integer]: String read GetColorHexNums;
    property ColorNames[ARow: Integer]: String read GetColorNames;
    property Colors[ARow: Integer]: TColor read GetColors; default;
    property Count: Integer read GetCount;
    property HSV[ARow: Integer]: TcaHSV read GetHSV;
    property Intensity[ARow: Integer]: Integer read GetIntensity;
    property Lightness[ARow: Integer]: Integer read GetLightness;
    property Matrix: TcaMatrix read GetMatrix;
    property RGB[ARow: Integer]: TcaRGB read GetRGB;
    property RowCount: Integer read GetRowCount;
    property YIQ[ARow: Integer]: Integer read GetYIQ;
  end;

  //----------------------------------------------------------------------------
  // TcaGretagColors                                                            
  //----------------------------------------------------------------------------

  TcaGretagColors = class(TcaCustomColors)
  protected
    procedure AddColorConstants; override;
  end;

  //----------------------------------------------------------------------------
  // TcaNetscapeColors                                                          
  //----------------------------------------------------------------------------

  TcaNetscapeColors = class(TcaCustomColors)
  protected
    procedure AddColorConstants; override;
  end;

  //----------------------------------------------------------------------------
  // IcaColors                                                                  
  //----------------------------------------------------------------------------

  IcaColors = interface(IcaCustomColors)
  ['{BC5363CF-A55A-43FB-A730-F880A6A849E7}']
    procedure AddGretagColors;
    procedure AddNetscapeColors;
    procedure BuildChartColors;
    procedure DeleteSystemColors;
  end;

  //----------------------------------------------------------------------------
  // TcaColors                                                                  
  //----------------------------------------------------------------------------

  TcaColors = class(TcaCustomColors, IcaColors)
  protected
    procedure AddColorConstants; override;
  public
    // Public methods 
    procedure AddGretagColors;
    procedure AddNetscapeColors;
    procedure BuildChartColors;
    procedure DeleteSystemColors;
  end;

  //----------------------------------------------------------------------------
  // TcaGrayColors                                                              
  //----------------------------------------------------------------------------

  TcaGrayColors = class(TcaCustomColors)
  protected
    procedure AddColorConstants; override;
  end;

var
  ColorUtils: IcaColorUtils = nil;

implementation

  //---------------------------------------------------------------------------
  // TcaRect                                                                   
  //---------------------------------------------------------------------------

constructor TcaRect.Create(aRect: TRect);
begin
  inherited Create;
  FRect := ARect;
end;

constructor TcaRect.Create(const aLeft, aTop, aWidth, aHeight: Integer); 
begin
  inherited Create;
  SetBounds(aLeft, aTop, aWidth, aHeight);
end;

  // Public methods 

procedure TcaRect.Adjust(const dL, dT, dR, dB: Integer);
begin
  Utils.AdjustRect(FRect, dL, dT, dR, dB);
end;

procedure TcaRect.Assign(Source: TPersistent);
begin
  if Source is TcaRect then
    begin
      TcaRect(Source).Left := Left;
      TcaRect(Source).Top := Top;
      TcaRect(Source).Right := Right;
      TcaRect(Source).Bottom := Bottom;
    end;
  inherited;
end;

procedure TcaRect.SetBounds(const ALeft, ATop, AWidth, AHeight: Integer);
begin
  FRect := Classes.Rect(ALeft, ATop, ALeft + AWidth, ATop + AHeight);
end;

procedure TcaRect.SetLeftAndWidth(const ALeft, AWidth: Integer);
begin
  FRect.Left := ALeft;
  FRect.Right := ALeft + AWidth;
end;

procedure TcaRect.SetRect(const ALeft, ATop, ARight, ABottom: Integer);
begin
  FRect := Classes.Rect(ALeft, ATop, ARight, ABottom);
end;

procedure TcaRect.SetTopAndHeight(const ATop, AHeight: Integer);
begin
  FRect.Top := ATop;
  FRect.Bottom := ATop + AHeight;
end;

  // Property methods 

function TcaRect.GetBottom: Integer;
begin
  Result := FRect.Bottom;
end;

function TcaRect.GetHeight: Integer;
begin
  Result := FRect.Bottom - FRect.Top;
end;

function TcaRect.GetLeft: Integer;
begin
  Result := FRect.Left;
end;

function TcaRect.GetRect: TRect;
begin
  Result := FRect;
end;

function TcaRect.GetRight: Integer;
begin
  Result := FRect.Right;
end;

function TcaRect.GetTop: Integer;
begin
  Result := FRect.Top;
end;

function TcaRect.GetWidth: Integer;
begin
  Result := FRect.Right - FRect.Left;
end;

procedure TcaRect.SetBottom(const Value: Integer);
begin
  FRect.Bottom := Value;
end;

procedure TcaRect.SetHeight(const Value: Integer);
begin
  FRect.Bottom := FRect.Top + Value;
end;

procedure TcaRect.SetLeft(const Value: Integer);
begin
  FRect.Left := Value;
end;

procedure TcaRect.SetRight(const Value: Integer);
begin
  FRect.Right := Value;
end;

procedure TcaRect.SetTop(const Value: Integer);
begin
  FRect.Top := Value;
end;

procedure TcaRect.SetWidth(const Value: Integer);
begin
  FRect.Right := FRect.Left + Value;
end;

  //---------------------------------------------------------------------------
  // TcaBitmap                                                                 
  //---------------------------------------------------------------------------

function TcaBitmap.BoundsRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

  //---------------------------------------------------------------------------
  // TcaCustomColors                                                           
  //---------------------------------------------------------------------------

constructor TcaCustomColors.Create;
begin
  inherited;
  FMatrix := TcaMatrix.Create;
  Initialize;
end;

destructor TcaCustomColors.Destroy;
begin
  FMatrix.Free;
  inherited;
end;

procedure TcaCustomColors.AddColorConstants;
begin
  FMatrix.ColCount := 8;
end;

procedure TcaCustomColors.AddColorRow(const AColor: TColor; const AColorName: String);
begin
  FMatrix.AddRow;
  FMatrix.Integers[Ord(ccColor), FMatrix.RowCount - 1] := AColor;
  FMatrix.Strings[Ord(ccName), FMatrix.RowCount - 1] := AColorName;
end;

procedure TcaCustomColors.BuildExtraColorData;
var
  ARow: Integer;
  ARGB: TcaRGB;
  AHSV: TcaHSV;
begin
  for ARow := 0 to RowCount - 1 do
    begin
      ARGB := GetRGB(ARow);
      AHSV := ColorUtils.RGBToHSV(ARGB);
      FMatrix.Extendeds[Ord(ccHue), ARow] := AHSV.Hue;
      FMatrix.Extendeds[Ord(ccSaturation), ARow] := AHSV.Saturation;
      FMatrix.Extendeds[Ord(ccValue), ARow] := AHSV.Value;
      FMatrix.Integers[Ord(ccIntensity), ARow] := ColorUtils.RGBToIntensity(ARGB);
      FMatrix.Integers[Ord(ccLightness), ARow] := ColorUtils.RGBToLightness(ARGB);
      FMatrix.Integers[Ord(ccYic), ARow] := ColorUtils.RGBToYIC(ARGB);
    end;
end;

function TcaCustomColors.GetColors(ARow: Integer): TColor;
begin
  Result := FMatrix.Integers[Ord(ccColor), ARow];
end;

function TcaCustomColors.GetColorNames(ARow: Integer): String;
begin
  Result := FMatrix.Strings[Ord(ccName), ARow];
end;

function TcaCustomColors.GetColorHexNums(ARow: Integer): String;
begin
  FmtStr(Result, '$%.8x', [GetColors(ARow)]);
end;

function TcaCustomColors.GetCount: Integer;
begin
  Result := FMatrix.RowCount;
end;

function TcaCustomColors.GetDefaultColor(Index, MaxColors: Integer): TColor;
var
  AIndex: Integer;
begin
  if MaxColors = 0 then MaxColors := FMatrix.RowCount;;
  AIndex := Index mod MaxColors;
  Result := GetColors(AIndex);
end;

function TcaCustomColors.GetHSV(ARow: Integer): TcaHSV;
begin
  Result := ColorUtils.RGBToHSV(GetRGB(ARow));
end;

function TcaCustomColors.GetIntensity(ARow: Integer): Integer;
begin
  Result := ColorUtils.RGBToIntensity(GetRGB(ARow));
end;

function TcaCustomColors.GetLightness(ARow: Integer): Integer;
begin
  Result := ColorUtils.RGBToLightness(GetRGB(ARow));
end;

function TcaCustomColors.GetMatrix: TcaMatrix;
begin
  Result := FMatrix;
end;

function TcaCustomColors.GetRGB(ARow: Integer): TcaRGB;
begin
  Result := ColorUtils.ColorToRGB(GetColors(ARow));
end;

function TcaCustomColors.GetRowCount: Integer;
begin
  Result := FMatrix.RowCount;
end;

function TcaCustomColors.GetYIQ(ARow: Integer): Integer;
begin
  Result := ColorUtils.RGBToYIC(GetRGB(ARow));
end;

procedure TcaCustomColors.Initialize;
begin
  FMatrix.RowCount := 0;
  AddColorConstants;
  BuildExtraColorData;
end;

  //----------------------------------------------------------------------------
  // TcaGretagColors                                                            
  //----------------------------------------------------------------------------

procedure TcaGretagColors.AddColorConstants;
begin
  inherited;
  AddColorRow(clgmDarkSkin, 'clgmDarkSkin');
  AddColorRow(clgmLightSkin, 'clgmLightSkin');
  AddColorRow(clgmBlueSky, 'clgmBlueSky');
  AddColorRow(clgmFoliage, 'clgmFoliage');
  AddColorRow(clgmBlueFlower, 'clgmBlueFlower');
  AddColorRow(clgmBluishGreen, 'clgmBluishGreen');
  AddColorRow(clgmOrange, 'clgmOrange');
  AddColorRow(clgmPurplishBlue, 'clgmPurplishBlue');
  AddColorRow(clgmModerateRed, 'clgmModerateRed');
  AddColorRow(clgmPurple, 'clgmPurple');
  AddColorRow(clgmYellowGreen, 'clgmYellowGreen');
  AddColorRow(clgmOrangeYellow, 'clgmOrangeYellow');
  AddColorRow(clgmBlue, 'clgmBlue');
  AddColorRow(clgmGreen, 'clgmGreen');
  AddColorRow(clgmRed, 'clgmRed');
  AddColorRow(clgmYellow, 'clgmYellow');
  AddColorRow(clgmMagenta, 'clgmMagenta');
  AddColorRow(clgmCyan, 'clgmCyan');
  AddColorRow(clgmWhite, 'clgmWhite');
  AddColorRow(clgmNeutral8, 'clgmNeutral8');
  AddColorRow(clgmNeutral65, 'clgmNeutral65');
  AddColorRow(clgmNeutral5, 'clgmNeutral5');
  AddColorRow(clgmNeutral35, 'clgmNeutral35');
  AddColorRow(clgmBlack, 'clgmBlack');
end;

  //----------------------------------------------------------------------------
  // TcaNetscapeColors                                                          
  //----------------------------------------------------------------------------

procedure TcaNetscapeColors.AddColorConstants;
begin
  inherited;
  AddColorRow(clnsBlack, 'clnsBlack');
  AddColorRow(clnsGrey11, 'clnsGrey11');
  AddColorRow(clnsGrey21, 'clnsGrey21');
  AddColorRow(clnsGrey31, 'clnsGrey31');
  AddColorRow(clnsDimGrey, 'clnsDimGrey');
  AddColorRow(clnsGrey41, 'clnsGrey41');
  AddColorRow(clnsGrey51, 'clnsGrey51');
  AddColorRow(clnsGrey61, 'clnsGrey61');
  AddColorRow(clnsDarkGrey, 'clnsDarkGrey');
  AddColorRow(clnsGrey71, 'clnsGrey71');
  AddColorRow(clnsGrey, 'clnsGrey');
  AddColorRow(clnsGray81, 'clnsGray81');
  AddColorRow(clnsLightGray, 'clnsLightGray');
  AddColorRow(clnsGainsboro, 'clnsGainsboro');
  AddColorRow(clnsGray91, 'clnsGray91');
  AddColorRow(clnsWhiteSmoke, 'clnsWhiteSmoke');
  AddColorRow(clnsWhite, 'clnsWhite');
  AddColorRow(clnsSnow4, 'clnsSnow4');
  AddColorRow(clnsSnow3, 'clnsSnow3');
  AddColorRow(clnsSnow, 'clnsSnow');
  AddColorRow(clnsSnow1, 'clnsSnow1');
  AddColorRow(clnsSnow2, 'clnsSnow2');
  AddColorRow(clnsRosyBrown, 'clnsRosyBrown');
  AddColorRow(clnsRosyBrown1, 'clnsRosyBrown1');
  AddColorRow(clnsRosyBrown3, 'clnsRosyBrown3');
  AddColorRow(clnsRosyBrown2, 'clnsRosyBrown2');
  AddColorRow(clnsRosyBrown4, 'clnsRosyBrown4');
  AddColorRow(clnsLightCoral, 'clnsLightCoral');
  AddColorRow(clnsIndianRed, 'clnsIndianRed');
  AddColorRow(clnsIndianRed4, 'clnsIndianRed4');
  AddColorRow(clnsIndianRed2, 'clnsIndianRed2');
  AddColorRow(clnsIndianRed1, 'clnsIndianRed1');
  AddColorRow(clnsIndianRed3, 'clnsIndianRed3');
  AddColorRow(clnsBrown, 'clnsBrown');
  AddColorRow(clnsBrown4, 'clnsBrown4');
  AddColorRow(clnsBrown1, 'clnsBrown1');
  AddColorRow(clnsBrown3, 'clnsBrown3');
  AddColorRow(clnsBrown2, 'clnsBrown2');
  AddColorRow(clnsFirebrick, 'clnsFirebrick');
  AddColorRow(clnsFirebrick1, 'clnsFirebrick1');
  AddColorRow(clnsFirebrick4, 'clnsFirebrick4');
  AddColorRow(clnsFirebrick3, 'clnsFirebrick3');
  AddColorRow(clnsFirebrick2, 'clnsFirebrick2');
  AddColorRow(clnsRed4, 'clnsRed4');
  AddColorRow(clnsDarkRed, 'clnsDarkRed');
  AddColorRow(clnsRed3, 'clnsRed3');
  AddColorRow(clnsRed2, 'clnsRed2');
  AddColorRow(clnsRed, 'clnsRed');
  AddColorRow(clnsRed1, 'clnsRed1');
  AddColorRow(clnsMistyRose3, 'clnsMistyRose3');
  AddColorRow(clnsMistyRose, 'clnsMistyRose');
  AddColorRow(clnsMistyRose1, 'clnsMistyRose1');
  AddColorRow(clnsSalmon, 'clnsSalmon');
  AddColorRow(clnsMistyRose2, 'clnsMistyRose2');
  AddColorRow(clnsMistyRose4, 'clnsMistyRose4');
  AddColorRow(clnsTomato3, 'clnsTomato3');
  AddColorRow(clnsTomato, 'clnsTomato');
  AddColorRow(clnsTomato1, 'clnsTomato1');
  AddColorRow(clnsTomato2, 'clnsTomato2');
  AddColorRow(clnsTomato4, 'clnsTomato4');
  AddColorRow(clnsCoral3, 'clnsCoral3');
  AddColorRow(clnsCoral4, 'clnsCoral4');
  AddColorRow(clnsCoral1, 'clnsCoral1');
  AddColorRow(clnsCoral2, 'clnsCoral2');
  AddColorRow(clnsSalmon2, 'clnsSalmon2');
  AddColorRow(clnsSalmon4, 'clnsSalmon4');
  AddColorRow(clnsSalmon3, 'clnsSalmon3');
  AddColorRow(clnsSalmon1, 'clnsSalmon1');
  AddColorRow(clnsDarkSalmon, 'clnsDarkSalmon');
  AddColorRow(clnsOrangeRed4, 'clnsOrangeRed4');
  AddColorRow(clnsCoral, 'clnsCoral');
  AddColorRow(clnsOrangeRed3, 'clnsOrangeRed3');
  AddColorRow(clnsOrangeRed2, 'clnsOrangeRed2');
  AddColorRow(clnsOrangeRed, 'clnsOrangeRed');
  AddColorRow(clnsOrangeRed1, 'clnsOrangeRed1');
  AddColorRow(clnsLightSalmon2, 'clnsLightSalmon2');
  AddColorRow(clnsLightSalmon, 'clnsLightSalmon');
  AddColorRow(clnsLightSalmon1, 'clnsLightSalmon1');
  AddColorRow(clnsLightSalmon4, 'clnsLightSalmon4');
  AddColorRow(clnsLightSalmon3, 'clnsLightSalmon3');
  AddColorRow(clnsSienna3, 'clnsSienna3');
  AddColorRow(clnsSienna1, 'clnsSienna1');
  AddColorRow(clnsSienna2, 'clnsSienna2');
  AddColorRow(clnsSienna, 'clnsSienna');
  AddColorRow(clnsSienna4, 'clnsSienna4');
  AddColorRow(clnsSeashell, 'clnsSeashell');
  AddColorRow(clnsSeashell1, 'clnsSeashell1');
  AddColorRow(clnsChocolate3, 'clnsChocolate3');
  AddColorRow(clnsChocolate1, 'clnsChocolate1');
  AddColorRow(clnsChocolate2, 'clnsChocolate2');
  AddColorRow(clnsChocolate, 'clnsChocolate');
  AddColorRow(clnsSaddleBrown, 'clnsSaddleBrown');
  AddColorRow(clnsChocolate4, 'clnsChocolate4');
  AddColorRow(clnsSeashell3, 'clnsSeashell3');
  AddColorRow(clnsSeashell2, 'clnsSeashell2');
  AddColorRow(clnsSeashell4, 'clnsSeashell4');
  AddColorRow(clnsSandyBrown, 'clnsSandyBrown');
  AddColorRow(clnsPeachPuff2, 'clnsPeachPuff2');
  AddColorRow(clnsPeachPuff3, 'clnsPeachPuff3');
  AddColorRow(clnsPeachPuff, 'clnsPeachPuff');
  AddColorRow(clnsPeachPuff1, 'clnsPeachPuff1');
  AddColorRow(clnsPeachPuff4, 'clnsPeachPuff4');
  AddColorRow(clnsTan1, 'clnsTan1');
  AddColorRow(clnsTan4, 'clnsTan4');
  AddColorRow(clnsTan2, 'clnsTan2');
  AddColorRow(clnsPeru, 'clnsPeru');
  AddColorRow(clnsTan3, 'clnsTan3');
  AddColorRow(clnsDarkOrange2, 'clnsDarkOrange2');
  AddColorRow(clnsDarkOrange4, 'clnsDarkOrange4');
  AddColorRow(clnsDarkOrange3, 'clnsDarkOrange3');
  AddColorRow(clnsDarkOrange1, 'clnsDarkOrange1');
  AddColorRow(clnsLinen, 'clnsLinen');
  AddColorRow(clnsBisque3, 'clnsBisque3');
  AddColorRow(clnsBisque, 'clnsBisque');
  AddColorRow(clnsBisque1, 'clnsBisque1');
  AddColorRow(clnsBisque2, 'clnsBisque2');
  AddColorRow(clnsDarkOrange, 'clnsDarkOrange');
  AddColorRow(clnsAntiqueWhite3, 'clnsAntiqueWhite3');
  AddColorRow(clnsAntiqueWhite1, 'clnsAntiqueWhite1');
  AddColorRow(clnsBurlywood4, 'clnsBurlywood4');
  AddColorRow(clnsAntiqueWhite2, 'clnsAntiqueWhite2');
  AddColorRow(clnsBurlywood2, 'clnsBurlywood2');
  AddColorRow(clnsBurlywood1, 'clnsBurlywood1');
  AddColorRow(clnsBisque4, 'clnsBisque4');
  AddColorRow(clnsBurlywood3, 'clnsBurlywood3');
  AddColorRow(clnsBurlywood, 'clnsBurlywood');
  AddColorRow(clnsAntiqueWhite, 'clnsAntiqueWhite');
  AddColorRow(clnsTan, 'clnsTan');
  AddColorRow(clnsAntiqueWhite4, 'clnsAntiqueWhite4');
  AddColorRow(clnsNavajoWhite2, 'clnsNavajoWhite2');
  AddColorRow(clnsNavajoWhite, 'clnsNavajoWhite');
  AddColorRow(clnsNavajoWhite1, 'clnsNavajoWhite1');
  AddColorRow(clnsBlanchedAlmond, 'clnsBlanchedAlmond');
  AddColorRow(clnsNavajoWhite4, 'clnsNavajoWhite4');
  AddColorRow(clnsNavajoWhite3, 'clnsNavajoWhite3');
  AddColorRow(clnsPapayaWhip, 'clnsPapayaWhip');
  AddColorRow(clnsMoccasin, 'clnsMoccasin');
  AddColorRow(clnsOrange4, 'clnsOrange4');
  AddColorRow(clnsOrange2, 'clnsOrange2');
  AddColorRow(clnsOrange, 'clnsOrange');
  AddColorRow(clnsOrange1, 'clnsOrange1');
  AddColorRow(clnsWheat4, 'clnsWheat4');
  AddColorRow(clnsOrange3, 'clnsOrange3');
  AddColorRow(clnsOldLace, 'clnsOldLace');
  AddColorRow(clnsWheat, 'clnsWheat');
  AddColorRow(clnsWheat1, 'clnsWheat1');
  AddColorRow(clnsWheat3, 'clnsWheat3');
  AddColorRow(clnsWheat2, 'clnsWheat2');
  AddColorRow(clnsFloralWhite, 'clnsFloralWhite');
  AddColorRow(clnsDarkGoldenrod1, 'clnsDarkGoldenrod1');
  AddColorRow(clnsDarkGoldenrod3, 'clnsDarkGoldenrod3');
  AddColorRow(clnsDarkGoldenrod2, 'clnsDarkGoldenrod2');
  AddColorRow(clnsDarkGoldenrod, 'clnsDarkGoldenrod');
  AddColorRow(clnsGoldenrod, 'clnsGoldenrod');
  AddColorRow(clnsGoldenrod1, 'clnsGoldenrod1');
  AddColorRow(clnsGoldenrod4, 'clnsGoldenrod4');
  AddColorRow(clnsGoldenrod2, 'clnsGoldenrod2');
  AddColorRow(clnsGoldenrod3, 'clnsGoldenrod3');
  AddColorRow(clnsCornsilk, 'clnsCornsilk');
  AddColorRow(clnsCornsilk1, 'clnsCornsilk1');
  AddColorRow(clnsCornsilk2, 'clnsCornsilk2');
  AddColorRow(clnsCornsilk3, 'clnsCornsilk3');
  AddColorRow(clnsLightGoldenrod2, 'clnsLightGoldenrod2');
  AddColorRow(clnsLightGoldenrod1, 'clnsLightGoldenrod1');
  AddColorRow(clnsLightGoldenrod3, 'clnsLightGoldenrod3');
  AddColorRow(clnsCornsilk4, 'clnsCornsilk4');
  AddColorRow(clnsLightGoldenrod4, 'clnsLightGoldenrod4');
  AddColorRow(clnsGold4, 'clnsGold4');
  AddColorRow(clnsLightGoldenrod, 'clnsLightGoldenrod');
  AddColorRow(clnsGold3, 'clnsGold3');
  AddColorRow(clnsGold, 'clnsGold');
  AddColorRow(clnsGold1, 'clnsGold1');
  AddColorRow(clnsGold2, 'clnsGold2');
  AddColorRow(clnsLemonChiffon2, 'clnsLemonChiffon2');
  AddColorRow(clnsLemonChiffon3, 'clnsLemonChiffon3');
  AddColorRow(clnsLemonChiffon, 'clnsLemonChiffon');
  AddColorRow(clnsLemonChiffon1, 'clnsLemonChiffon1');
  AddColorRow(clnsPaleGoldenrod, 'clnsPaleGoldenrod');
  AddColorRow(clnsKhaki4, 'clnsKhaki4');
  AddColorRow(clnsKhaki1, 'clnsKhaki1');
  AddColorRow(clnsKhaki3, 'clnsKhaki3');
  AddColorRow(clnsKhaki2, 'clnsKhaki2');
  AddColorRow(clnsLemonChiffon4, 'clnsLemonChiffon4');
  AddColorRow(clnsDarkKhaki, 'clnsDarkKhaki');
  AddColorRow(clnsIvory4, 'clnsIvory4');
  AddColorRow(clnsIvory3, 'clnsIvory3');
  AddColorRow(clnsIvory2, 'clnsIvory2');
  AddColorRow(clnsIvory, 'clnsIvory');
  AddColorRow(clnsIvory1, 'clnsIvory1');
  AddColorRow(clnsBeige, 'clnsBeige');
  AddColorRow(clnsLightYellow4, 'clnsLightYellow4');
  AddColorRow(clnsLightYellow3, 'clnsLightYellow3');
  AddColorRow(clnsLightYellow2, 'clnsLightYellow2');
  AddColorRow(clnsLightYellow, 'clnsLightYellow');
  AddColorRow(clnsLightYellow1, 'clnsLightYellow1');
  AddColorRow(clnsLtGoldenrodYello, 'clnsLtGoldenrodYello');
  AddColorRow(clnsYellow4, 'clnsYellow4');
  AddColorRow(clnsYellow3, 'clnsYellow3');
  AddColorRow(clnsYellow2, 'clnsYellow2');
  AddColorRow(clnsYellow, 'clnsYellow');
  AddColorRow(clnsYellow1, 'clnsYellow1');
  AddColorRow(clnsOliveDrab4, 'clnsOliveDrab4');
  AddColorRow(clnsOliveDrab, 'clnsOliveDrab');
  AddColorRow(clnsOliveDrab1, 'clnsOliveDrab1');
  AddColorRow(clnsYellowGreen, 'clnsYellowGreen');
  AddColorRow(clnsOliveDrab3, 'clnsOliveDrab3');
  AddColorRow(clnsOliveDrab2, 'clnsOliveDrab2');
  AddColorRow(clnsDarkOliveGreen, 'clnsDarkOliveGreen');
  AddColorRow(clnsDarkOliveGreen1, 'clnsDarkOliveGreen1');
  AddColorRow(clnsDarkOliveGreen4, 'clnsDarkOliveGreen4');
  AddColorRow(clnsDarkOliveGreen3, 'clnsDarkOliveGreen3');
  AddColorRow(clnsDarkOliveGreen2, 'clnsDarkOliveGreen2');
  AddColorRow(clnsGreenYellow, 'clnsGreenYellow');
  AddColorRow(clnsChartreuse3, 'clnsChartreuse3');
  AddColorRow(clnsChartreuse, 'clnsChartreuse');
  AddColorRow(clnsChartreuse1, 'clnsChartreuse1');
  AddColorRow(clnsChartreuse4, 'clnsChartreuse4');
  AddColorRow(clnsChartreuse2, 'clnsChartreuse2');
  AddColorRow(clnsLawnGreen, 'clnsLawnGreen');
  AddColorRow(clnsHoneydew4, 'clnsHoneydew4');
  AddColorRow(clnsHoneydew3, 'clnsHoneydew3');
  AddColorRow(clnsHoneydew2, 'clnsHoneydew2');
  AddColorRow(clnsHoneydew, 'clnsHoneydew');
  AddColorRow(clnsHoneydew1, 'clnsHoneydew1');
  AddColorRow(clnsDarkSeaGreen, 'clnsDarkSeaGreen');
  AddColorRow(clnsDarkSeaGreen1, 'clnsDarkSeaGreen1');
  AddColorRow(clnsDarkSeaGreen3, 'clnsDarkSeaGreen3');
  AddColorRow(clnsDarkSeaGreen2, 'clnsDarkSeaGreen2');
  AddColorRow(clnsDarkSeaGreen4, 'clnsDarkSeaGreen4');
  AddColorRow(clnsPaleGreen, 'clnsPaleGreen');
  AddColorRow(clnsPaleGreen3, 'clnsPaleGreen3');
  AddColorRow(clnsPaleGreen2, 'clnsPaleGreen2');
  AddColorRow(clnsLightGreen, 'clnsLightGreen');
  AddColorRow(clnsPaleGreen4, 'clnsPaleGreen4');
  AddColorRow(clnsPaleGreen1, 'clnsPaleGreen1');
  AddColorRow(clnsForestGreen, 'clnsForestGreen');
  AddColorRow(clnsLimeGreen, 'clnsLimeGreen');
  AddColorRow(clnsDarkGreen, 'clnsDarkGreen');
  AddColorRow(clnsGreen4, 'clnsGreen4');
  AddColorRow(clnsGreen3, 'clnsGreen3');
  AddColorRow(clnsGreen2, 'clnsGreen2');
  AddColorRow(clnsGreen, 'clnsGreen');
  AddColorRow(clnsGreen1, 'clnsGreen1');
  AddColorRow(clnsSeaGreen1, 'clnsSeaGreen1');
  AddColorRow(clnsSeaGreen2, 'clnsSeaGreen2');
  AddColorRow(clnsSeaGreen, 'clnsSeaGreen');
  AddColorRow(clnsSeaGreen4, 'clnsSeaGreen4');
  AddColorRow(clnsSeaGreen3, 'clnsSeaGreen3');
  AddColorRow(clnsMediumSeaGreen, 'clnsMediumSeaGreen');
  AddColorRow(clnsSpringGreen2, 'clnsSpringGreen2');
  AddColorRow(clnsSpringGreen4, 'clnsSpringGreen4');
  AddColorRow(clnsSpringGreen3, 'clnsSpringGreen3');
  AddColorRow(clnsSpringGreen, 'clnsSpringGreen');
  AddColorRow(clnsSpringGreen1, 'clnsSpringGreen1');
  AddColorRow(clnsMintCream, 'clnsMintCream');
  AddColorRow(clnsMedSpringGreen, 'clnsMedSpringGreen');
  AddColorRow(clnsMediumAquamarine, 'clnsMediumAquamarine');
  AddColorRow(clnsAquamarine3, 'clnsAquamarine3');
  AddColorRow(clnsAquamarine, 'clnsAquamarine');
  AddColorRow(clnsAquamarine1, 'clnsAquamarine1');
  AddColorRow(clnsAquamarine2, 'clnsAquamarine2');
  AddColorRow(clnsAquamarine4, 'clnsAquamarine4');
  AddColorRow(clnsTurquoise, 'clnsTurquoise');
  AddColorRow(clnsLightSeaGreen, 'clnsLightSeaGreen');
  AddColorRow(clnsMediumTurquoise, 'clnsMediumTurquoise');
  AddColorRow(clnsAzure4, 'clnsAzure4');
  AddColorRow(clnsAzure3, 'clnsAzure3');
  AddColorRow(clnsAzure2, 'clnsAzure2');
  AddColorRow(clnsAzure, 'clnsAzure');
  AddColorRow(clnsAzure1, 'clnsAzure1');
  AddColorRow(clnsLightCyan4, 'clnsLightCyan4');
  AddColorRow(clnsLightCyan3, 'clnsLightCyan3');
  AddColorRow(clnsLightCyan2, 'clnsLightCyan2');
  AddColorRow(clnsLightCyan, 'clnsLightCyan');
  AddColorRow(clnsLightCyan1, 'clnsLightCyan1');
  AddColorRow(clnsPaleTurquoise, 'clnsPaleTurquoise');
  AddColorRow(clnsPaleTurquoise4, 'clnsPaleTurquoise4');
  AddColorRow(clnsPaleTurquoise1, 'clnsPaleTurquoise1');
  AddColorRow(clnsPaleTurquoise3, 'clnsPaleTurquoise3');
  AddColorRow(clnsPaleTurquoise2, 'clnsPaleTurquoise2');
  AddColorRow(clnsDarkSlateGray, 'clnsDarkSlateGray');
  AddColorRow(clnsDarkSlateGray2, 'clnsDarkSlateGray2');
  AddColorRow(clnsDarkSlateGray1, 'clnsDarkSlateGray1');
  AddColorRow(clnsDarkSlateGray4, 'clnsDarkSlateGray4');
  AddColorRow(clnsDarkSlateGray3, 'clnsDarkSlateGray3');
  AddColorRow(clnsCyan4, 'clnsCyan4');
  AddColorRow(clnsDarkCyan, 'clnsDarkCyan');
  AddColorRow(clnsCyan3, 'clnsCyan3');
  AddColorRow(clnsCyan2, 'clnsCyan2');
  AddColorRow(clnsCyan, 'clnsCyan');
  AddColorRow(clnsCyan1, 'clnsCyan1');
  AddColorRow(clnsDarkTurquoise, 'clnsDarkTurquoise');
  AddColorRow(clnsCadetBlue, 'clnsCadetBlue');
  AddColorRow(clnsTurquoise4, 'clnsTurquoise4');
  AddColorRow(clnsTurquoise3, 'clnsTurquoise3');
  AddColorRow(clnsTurquoise2, 'clnsTurquoise2');
  AddColorRow(clnsTurquoise1, 'clnsTurquoise1');
  AddColorRow(clnsCadetBlue4, 'clnsCadetBlue4');
  AddColorRow(clnsCadetBlue2, 'clnsCadetBlue2');
  AddColorRow(clnsCadetBlue1, 'clnsCadetBlue1');
  AddColorRow(clnsCadetBlue3, 'clnsCadetBlue3');
  AddColorRow(clnsPowderBlue, 'clnsPowderBlue');
  AddColorRow(clnsLightBlue4, 'clnsLightBlue4');
  AddColorRow(clnsLightBlue, 'clnsLightBlue');
  AddColorRow(clnsDeepSkyBlue3, 'clnsDeepSkyBlue3');
  AddColorRow(clnsLightBlue1, 'clnsLightBlue1');
  AddColorRow(clnsLightBlue2, 'clnsLightBlue2');
  AddColorRow(clnsDeepSkyBlue4, 'clnsDeepSkyBlue4');
  AddColorRow(clnsDeepSkyBlue2, 'clnsDeepSkyBlue2');
  AddColorRow(clnsDeepSkyBlue, 'clnsDeepSkyBlue');
  AddColorRow(clnsDeepSkyBlue1, 'clnsDeepSkyBlue1');
  AddColorRow(clnsLightBlue3, 'clnsLightBlue3');
  AddColorRow(clnsSkyBlue, 'clnsSkyBlue');
  AddColorRow(clnsLightSkyBlue3, 'clnsLightSkyBlue3');
  AddColorRow(clnsLightSkyBlue2, 'clnsLightSkyBlue2');
  AddColorRow(clnsLightSkyBlue1, 'clnsLightSkyBlue1');
  AddColorRow(clnsLightSkyBlue4, 'clnsLightSkyBlue4');
  AddColorRow(clnsLightSkyBlue, 'clnsLightSkyBlue');
  AddColorRow(clnsSkyBlue3, 'clnsSkyBlue3');
  AddColorRow(clnsSkyBlue1, 'clnsSkyBlue1');
  AddColorRow(clnsSkyBlue2, 'clnsSkyBlue2');
  AddColorRow(clnsSkyBlue4, 'clnsSkyBlue4');
  AddColorRow(clnsSteelBlue2, 'clnsSteelBlue2');
  AddColorRow(clnsSteelBlue3, 'clnsSteelBlue3');
  AddColorRow(clnsSteelBlue, 'clnsSteelBlue');
  AddColorRow(clnsSteelBlue1, 'clnsSteelBlue1');
  AddColorRow(clnsSteelBlue4, 'clnsSteelBlue4');
  AddColorRow(clnsAliceBlue, 'clnsAliceBlue');
  AddColorRow(clnsDodgerBlue3, 'clnsDodgerBlue3');
  AddColorRow(clnsDodgerBlue, 'clnsDodgerBlue');
  AddColorRow(clnsDodgerBlue1, 'clnsDodgerBlue1');
  AddColorRow(clnsDodgerBlue2, 'clnsDodgerBlue2');
  AddColorRow(clnsDodgerBlue4, 'clnsDodgerBlue4');
  AddColorRow(clnsSlateGrey, 'clnsSlateGrey');
  AddColorRow(clnsLightSlateGray, 'clnsLightSlateGray');
  AddColorRow(clnsSlateGray3, 'clnsSlateGray3');
  AddColorRow(clnsSlateGray1, 'clnsSlateGray1');
  AddColorRow(clnsSlateGray2, 'clnsSlateGray2');
  AddColorRow(clnsSlateGray4, 'clnsSlateGray4');
  AddColorRow(clnsLightSteelBlue4, 'clnsLightSteelBlue4');
  AddColorRow(clnsLightSteelBlue3, 'clnsLightSteelBlue3');
  AddColorRow(clnsLightSteelBlue2, 'clnsLightSteelBlue2');
  AddColorRow(clnsLightSteelBlue, 'clnsLightSteelBlue');
  AddColorRow(clnsLightSteelBlue1, 'clnsLightSteelBlue1');
  AddColorRow(clnsCornflowerBlue, 'clnsCornflowerBlue');
  AddColorRow(clnsRoyalBlue3, 'clnsRoyalBlue3');
  AddColorRow(clnsRoyalBlue2, 'clnsRoyalBlue2');
  AddColorRow(clnsRoyalBlue1, 'clnsRoyalBlue1');
  AddColorRow(clnsRoyalBlue, 'clnsRoyalBlue');
  AddColorRow(clnsRoyalBlue4, 'clnsRoyalBlue4');
  AddColorRow(clnsGhostWhite, 'clnsGhostWhite');
  AddColorRow(clnsLavender, 'clnsLavender');
  AddColorRow(clnsMidnightBlue, 'clnsMidnightBlue');
  AddColorRow(clnsNavyBlue, 'clnsNavyBlue');
  AddColorRow(clnsBlue4, 'clnsBlue4');
  AddColorRow(clnsDarkBlue, 'clnsDarkBlue');
  AddColorRow(clnsMediumBlue, 'clnsMediumBlue');
  AddColorRow(clnsBlue3, 'clnsBlue3');
  AddColorRow(clnsBlue2, 'clnsBlue2');
  AddColorRow(clnsBlue, 'clnsBlue');
  AddColorRow(clnsBlue1, 'clnsBlue1');
  AddColorRow(clnsSlateBlue, 'clnsSlateBlue');
  AddColorRow(clnsSlateBlue1, 'clnsSlateBlue1');
  AddColorRow(clnsSlateBlue3, 'clnsSlateBlue3');
  AddColorRow(clnsLightSlateBlue, 'clnsLightSlateBlue');
  AddColorRow(clnsSlateBlue2, 'clnsSlateBlue2');
  AddColorRow(clnsSlateBlue4, 'clnsSlateBlue4');
  AddColorRow(clnsDarkSlateBlue, 'clnsDarkSlateBlue');
  AddColorRow(clnsMediumSlateBlue, 'clnsMediumSlateBlue');
  AddColorRow(clnsMediumPurple4, 'clnsMediumPurple4');
  AddColorRow(clnsMediumPurple2, 'clnsMediumPurple2');
  AddColorRow(clnsMediumPurple, 'clnsMediumPurple');
  AddColorRow(clnsMediumPurple3, 'clnsMediumPurple3');
  AddColorRow(clnsMediumPurple1, 'clnsMediumPurple1');
  AddColorRow(clnsPurple1, 'clnsPurple1');
  AddColorRow(clnsBlueViolet, 'clnsBlueViolet');
  AddColorRow(clnsPurple2, 'clnsPurple2');
  AddColorRow(clnsPurple4, 'clnsPurple4');
  AddColorRow(clnsPurple3, 'clnsPurple3');
  AddColorRow(clnsPurple, 'clnsPurple');
  AddColorRow(clnsDarkOrchid4, 'clnsDarkOrchid4');
  AddColorRow(clnsDarkOrchid2, 'clnsDarkOrchid2');
  AddColorRow(clnsDarkOrchid, 'clnsDarkOrchid');
  AddColorRow(clnsDarkOrchid1, 'clnsDarkOrchid1');
  AddColorRow(clnsDarkOrchid3, 'clnsDarkOrchid3');
  AddColorRow(clnsDarkViolet, 'clnsDarkViolet');
  AddColorRow(clnsMediumOrchid3, 'clnsMediumOrchid3');
  AddColorRow(clnsMediumOrchid1, 'clnsMediumOrchid1');
  AddColorRow(clnsMediumOrchid2, 'clnsMediumOrchid2');
  AddColorRow(clnsMediumOrchid4, 'clnsMediumOrchid4');
  AddColorRow(clnsMediumOrchid, 'clnsMediumOrchid');
  AddColorRow(clnsThistle4, 'clnsThistle4');
  AddColorRow(clnsThistle, 'clnsThistle');
  AddColorRow(clnsThistle3, 'clnsThistle3');
  AddColorRow(clnsThistle2, 'clnsThistle2');
  AddColorRow(clnsThistle1, 'clnsThistle1');
  AddColorRow(clnsPlum4, 'clnsPlum4');
  AddColorRow(clnsPlum1, 'clnsPlum1');
  AddColorRow(clnsPlum3, 'clnsPlum3');
  AddColorRow(clnsPlum2, 'clnsPlum2');
  AddColorRow(clnsDarkGoldenrod4, 'clnsDarkGoldenrod4');
  AddColorRow(clnsPlum, 'clnsPlum');
  AddColorRow(clnsViolet, 'clnsViolet');
  AddColorRow(clnsMagenta4, 'clnsMagenta4');
  AddColorRow(clnsDarkMagenta, 'clnsDarkMagenta');
  AddColorRow(clnsMagenta3, 'clnsMagenta3');
  AddColorRow(clnsMagenta2, 'clnsMagenta2');
  AddColorRow(clnsMagenta, 'clnsMagenta');
  AddColorRow(clnsMagenta1, 'clnsMagenta1');
  AddColorRow(clnsOrchid4, 'clnsOrchid4');
  AddColorRow(clnsOrchid, 'clnsOrchid');
  AddColorRow(clnsOrchid1, 'clnsOrchid1');
  AddColorRow(clnsOrchid3, 'clnsOrchid3');
  AddColorRow(clnsOrchid2, 'clnsOrchid2');
  AddColorRow(clnsVioletRed, 'clnsVioletRed');
  AddColorRow(clnsMaroon4, 'clnsMaroon4');
  AddColorRow(clnsMediumVioletRed, 'clnsMediumVioletRed');
  AddColorRow(clnsMaroon3, 'clnsMaroon3');
  AddColorRow(clnsMaroon2, 'clnsMaroon2');
  AddColorRow(clnsMaroon1, 'clnsMaroon1');
  AddColorRow(clnsDeepPink4, 'clnsDeepPink4');
  AddColorRow(clnsDeepPink2, 'clnsDeepPink2');
  AddColorRow(clnsDeepPink3, 'clnsDeepPink3');
  AddColorRow(clnsDeepPink, 'clnsDeepPink');
  AddColorRow(clnsDeepPink1, 'clnsDeepPink1');
  AddColorRow(clnsHotPink, 'clnsHotPink');
  AddColorRow(clnsHotPink4, 'clnsHotPink4');
  AddColorRow(clnsHotPink1, 'clnsHotPink1');
  AddColorRow(clnsHotPink2, 'clnsHotPink2');
  AddColorRow(clnsVioletRed4, 'clnsVioletRed4');
  AddColorRow(clnsVioletRed1, 'clnsVioletRed1');
  AddColorRow(clnsVioletRed2, 'clnsVioletRed2');
  AddColorRow(clnsVioletRed3, 'clnsVioletRed3');
  AddColorRow(clnsHotPink3, 'clnsHotPink3');
  AddColorRow(clnsLavenderBlush4, 'clnsLavenderBlush4');
  AddColorRow(clnsMaroon, 'clnsMaroon');
  AddColorRow(clnsLavenderBlush2, 'clnsLavenderBlush2');
  AddColorRow(clnsLavenderBlush3, 'clnsLavenderBlush3');
  AddColorRow(clnsLavenderBlush, 'clnsLavenderBlush');
  AddColorRow(clnsLavenderBlush1, 'clnsLavenderBlush1');
  AddColorRow(clnsPaleVioletRed1, 'clnsPaleVioletRed1');
  AddColorRow(clnsPaleVioletRed, 'clnsPaleVioletRed');
  AddColorRow(clnsPaleVioletRed3, 'clnsPaleVioletRed3');
  AddColorRow(clnsPaleVioletRed2, 'clnsPaleVioletRed2');
  AddColorRow(clnsPaleVioletRed4, 'clnsPaleVioletRed4');
  AddColorRow(clnsPink4, 'clnsPink4');
  AddColorRow(clnsPink2, 'clnsPink2');
  AddColorRow(clnsPink1, 'clnsPink1');
  AddColorRow(clnsPink3, 'clnsPink3');
  AddColorRow(clnsPink, 'clnsPink');
  AddColorRow(clnsLightPink, 'clnsLightPink');
  AddColorRow(clnsLightPink2, 'clnsLightPink2');
  AddColorRow(clnsLightPink3, 'clnsLightPink3');
  AddColorRow(clnsLightPink4, 'clnsLightPink4');
  AddColorRow(clnsLightPink1, 'clnsLightPink1');
end;

  //----------------------------------------------------------------------------
  // TcaColors                                                                  
  //----------------------------------------------------------------------------

procedure TcaColors.AddColorConstants;
begin
  inherited;
  AddColorRow(clBlack, 'clBlack');
  AddColorRow(clMaroon, 'clMaroon');
  AddColorRow(clGreen, 'clGreen');
  AddColorRow(clOlive, 'clOlive');
  AddColorRow(clNavy, 'clNavy');
  AddColorRow(clPurple, 'clPurple');
  AddColorRow(clTeal, 'clTeal');
  AddColorRow(clGray, 'clGray');
  AddColorRow(clSilver, 'clSilver');
  AddColorRow(clRed, 'clRed');
  AddColorRow(clLime, 'clLime');
  AddColorRow(clYellow, 'clYellow');
  AddColorRow(clBlue, 'clBlue');
  AddColorRow(clFuchsia, 'clFuchsia');
  AddColorRow(clAqua, 'clAqua');
  AddColorRow(clWhite, 'clWhite');
  AddColorRow(clScrollBar, 'clScrollBar');
  AddColorRow(clBackground, 'clBackground');
  AddColorRow(clActiveCaption, 'clActiveCaption');
  AddColorRow(clInactiveCaption, 'clInactiveCaption');
  AddColorRow(clMenu, 'clMenu');
  AddColorRow(clWindow, 'clWindow');
  AddColorRow(clWindowFrame, 'clWindowFrame');
  AddColorRow(clMenuText, 'clMenuText');
  AddColorRow(clWindowText, 'clWindowText');
  AddColorRow(clCaptionText, 'clCaptionText');
  AddColorRow(clActiveBorder, 'clActiveBorder');
  AddColorRow(clInactiveBorder, 'clInactiveBorder');
  AddColorRow(clAppWorkSpace, 'clAppWorkSpace');
  AddColorRow(clHighlight, 'clHighlight');
  AddColorRow(clHighlightText, 'clHighlightText');
  AddColorRow(clBtnFace, 'clBtnFace');
  AddColorRow(clBtnShadow, 'clBtnShadow');
  AddColorRow(clGrayText, 'clGrayText');
  AddColorRow(clBtnText, 'clBtnText');
  AddColorRow(clInactiveCaptionText, 'clInactiveCaptionText');
  AddColorRow(clBtnHighlight, 'clBtnHighlight');
  AddColorRow(cl3DDkShadow, 'cl3DDkShadow');
  AddColorRow(cl3DLight, 'cl3DLight');
  AddColorRow(clInfoText, 'clInfoText');
  AddColorRow(clInfoBk, 'clInfoBk');
  AddColorRow(clNone, 'clNone');
end;

procedure TcaColors.AddGretagColors;
var
  FGretagColors: TcaGretagColors;
begin
  FGretagColors := TcaGretagColors.Create;
  try
    FMatrix.AddFromMatrix(FGretagColors.Matrix);
  finally
    FGretagColors.Free;
  end;
end;

procedure TcaColors.AddNetscapeColors;
var
  FNetscapeColors: TcaGretagColors;
begin
  FNetscapeColors := TcaGretagColors.Create;
  try
    FMatrix.AddFromMatrix(FNetscapeColors.Matrix);
  finally
    FNetscapeColors.Free;
  end;
end;

procedure TcaColors.BuildChartColors;
begin
  Initialize;
  // Adds an extra 24 colors 
  AddGretagColors;
  // Deletes clWindow, clBtnFace etc - avoids duplication with named colors. 
  // We are left with 41 colors                                              
  DeleteSystemColors;
  // Sorts by Saturation and Color value - this gives a good spread of colors. 
  // ccSaturation and ccValue are defines in caTypes.pas                       
  // sdDescending is defined in caMatrix.pas                                   
  Matrix.ClearSortColumns;
  Matrix.AddSortColumn(Ord(ccSaturation), sdDescending);
  Matrix.AddSortColumn(Ord(ccValue), sdDescending);
  Matrix.Sorted := True;
end;

procedure TcaColors.DeleteSystemColors;
var
  ACol: Integer;
  ARow: Integer;
  ACopyRow: Integer;
  CopyMatrix: IcaMatrix;
begin
  CopyMatrix := TcaMatrix.Create;
  CopyMatrix.ColCount := FMatrix.ColCount;
  for ARow := 0 to FMatrix.RowCount - 1 do
    begin
      if FMatrix.Integers[0, ARow] >= 0 then
        begin
          ACopyRow := CopyMatrix.AddRow;
          for ACol := 0 to FMatrix.ColCount - 1 do
            CopyMatrix.AssignCell(FMatrix, ACol, ARow, ACol, ACopyRow);
        end;
    end;
  FMatrix.AssignFromInterface(CopyMatrix);
end;

  //----------------------------------------------------------------------------
  // TcaGrayColors                                                              
  //----------------------------------------------------------------------------

procedure TcaGrayColors.AddColorConstants;
var
  Index: Integer;
  AGray: TColor;
begin
  inherited;
  for Index := 0 to 15 do
    begin
      if Index = 10 then
        AGray := TColor($A4A0A0)
      else
        AGray := TColor(Index * $010101);
      AddColorRow(AGray, ColorToString(AGray));
    end;
end;

  //---------------------------------------------------------------------------
  // TcaColorUtils                                                             
  //---------------------------------------------------------------------------

  // Color conversion methods 

function TcaColorUtils.ColorToRGB(AColor: TColor): TcaRGB;
begin
  Result.Red := AColor and $FF;
  Result.Green := (AColor div 256) and $FF;
  Result.Blue := (AColor div 65536) and $FF;
end;

function TcaColorUtils.ColorToString(AColor: TColor): String;
begin
  if AColor = $E0E0E0 then
    Result := 'clLightGray'
  else
    if AColor = $A8A0A0 then
      Result := 'clMidGray'
    else
      if AColor = $606060 then
        Result := 'clDarkGray'
      else
        Result := Graphics.ColorToString(AColor);
end;

function TcaColorUtils.GetFlatToolbarsColor: COLORREF;
begin
  Result := GetRealColor(OffsetColor(COLOR_BTNFACE, 10, 10, 10));
end;

function TcaColorUtils.GetFlatToolbarsDownedColor: COLORREF;
begin
  Result := GetRealColor(OffsetColor(COLOR_BTNFACE, 0, 0, 16));
end;

function TcaColorUtils.GetFlatToolbarsDownedSelectedColor: COLORREF;
begin
  Result := GetRealColor(OffsetColor(COLOR_BTNFACE, -79, -62, -18));
end;

function TcaColorUtils.GetFlatToolbarsSelectedColor: COLORREF;
begin
  Result := GetRealColor(OffsetColor(COLOR_HIGHLIGHT, 172, 153, 104));
end;

function TcaColorUtils.GetRealColor(AColor: COLORREF): COLORREF;
var
  DC: HDC;
begin
  DC := GetDC(0);
  Result := GetNearestColor(DC, AColor);
  ReleaseDC(0, DC);
end;

function TcaColorUtils.HLSToRGB(const AHLS: TcaHLS): TcaRGB;
var
  H, L, S: Single;
  M1, M2, R, G, B: Single;

    function Value(const N1, N2: Single; Hue: Single): Single;
    begin
      if Hue > 360.0 then
        Hue := Hue - 360.0
      else
        if Hue < 0.0 then Hue := Hue + 360.0;
      if Hue < 60.0 then
        Result := N1 + (N2 - N1) * Hue / 60.0
      else
        begin
          if Hue < 180 then
            Result := N2
          else
            begin
              if Hue < 240.0 then
                Result := N1 + (N2 - N1) * (240.0 - Hue) / 60.0
              else
                Result := N1;
            end;
        end;
    end;

begin
  H := AHLS.Hue; L := AHLS.Lightness; S := AHLS.Saturation;
  if L <= 0.5 then
    M2 := L * (L + S)
  else
    M2 := L + S - L * S;
  M1 := 2.0 * L - M2;
  if S = 0.0 then
    begin      // achromatic -- no hue 
      if IsNAN(H) then
        begin
          R := L;
          G := L;
          B := L
        end
      else
        raise EcaColorError.Create('HLStoRGB:  S = 0 and H has a value');
    end
  else
    begin
      // Chromatic case -- there is a hue 
      R := Value(M1, M2, H + 120.0);
      G := Value(M1, M2, H);
      B := Value(M1, M2, H - 120.0)
    end;
  Result.Red := Round(R);
  Result.Green := Round(G);
  Result.Blue := Round(B);
end;

function TcaColorUtils.HSVToRGB(const AHSV: TcaHSV): TcaRGB;
var
  H, S, V: Single;
  F, P, Q, T: Single;
  R, G, B: Single;
  I: Integer;
  MathUtils: IcaMathUtils;
begin
  MathUtils := Utils as IcaMathUtils;
  H := AHSV.Hue; S := AHSV.Saturation; V := AHSV.Value;
  R := 0; G := 0; B := 0;
  if S = 0.0 then
    begin
      if IsNAN(H) then
        begin
          // achromatic (shades of gray) 
          R := V;
          G := V;
          B := V
        end
      else
        raise EcaColorError.Create('HSVtoRGB:  S = 0 and H has a value');
    end
  else
    begin
      if H = 360.0 then H := 0.0;
      H := H / 60;                // H is now IN [0,6)    
      I := MathUtils.Trunc(H);    // Largest integer <= H 
      F := H - I;                 // Fractional part of H 
      P := V * (1.0 - S);
      Q := V * (1.0 - (S * F));
      T := V * (1.0 - (S * (1.0 - F)));
      case I of
        0:  begin R := V; G := T; B := P end;
        1:  begin R := Q; G := V; B := P end;
        2:  begin R := P; G := V; B := T end;
        3:  begin R := P; G := Q; B := V end;
        4:  begin R := T; G := P; B := V end;
        5:  begin R := V; G := P; B := Q end;
      end;
    end;
  Result.Red := Round(R);
  Result.Green := Round(G);
  Result.Blue := Round(B);
end;

function TcaColorUtils.MakeDarker(AColor: TColor; ADelta: Integer): TColor;
begin
  Result := ChangeHSVValue(AColor, -ADelta);
end;

function TcaColorUtils.MakeLighter(AColor: TColor; ADelta: Integer): TColor;
begin
  Result := ChangeHSVValue(AColor, ADelta);
end;

function TcaColorUtils.OffsetColor(BaseColor: COLORREF; DeltaR, DeltaG, DeltaB: Integer): COLORREF;
var
  R, G, B: Integer;
begin
  Result := GetSysColor(BaseColor);
  R := GetRValue(Result) + DeltaR;
  if R > 255 then R := 255;
  G := GetGValue(Result) + DeltaG;
  if G > 255 then G := 255;
  B := GetBValue(Result) + DeltaB;
  if B > 255 then B := 255;
  Result := RGB(R, G, B);
end;

function TcaColorUtils.RGBToColor(const R, G, B: Integer): TColor;
begin
  Result := R or (G * 256) or (B * 65536);
end;

function TcaColorUtils.RGBToColor(const ARGB: TcaRGB): TColor;
begin
  Result := RGBToColor(ARGB.Red, ARGB.Green, ARGB.Blue);
end;

function TcaColorUtils.RGBToHLS(const ARGB: TcaRGB): TcaHLS;
begin
  Result := RGBToHLS(ARGB.Red, ARGB.Green, ARGB.Blue);
end;

function TcaColorUtils.RGBToHLS(const R, G, B: Integer): TcaHLS;
var
  Delta, Max, Min: Single;
  H, L, S: Single;
begin
  Max := MaxValue([R, G, B]);
  Min := MinValue([R, G, B]);
  H := 0;
  L := (Max + Min) / 2.0;     // Lightness 
  if Max = Min                // Achromatic case since r = g = b 
  then
    begin
      S := 0.0;
      H := NAN;         // Undefined 
    end
  else
    begin
      Delta := Max - Min;
      if L <= 0.5 then
        S := Delta / (Max + Min)
      else
        S := Delta / (2.0 - (Max + Min));
      if R = Max then
        H := (60.0 * (G - B)) / Delta                 // Degrees between yellow and magenta 
      else
        begin
          if G = Max then
            H := 120.0 + (60.0 * (B - R)) / Delta     // Degrees between cyan and yellow 
          else
            if B = Max then                           // Degrees between magenta and cyan 
              H := 240.0 + (60.0 * (R - G)) / Delta;
        end;
      if H < 0 then H := H + 360.0;                   // Keep in interval [0, 360) 
    end;
  Result.Hue := H;
  Result.Lightness := L;
  Result.Saturation := S;
end;

function TcaColorUtils.RGBToHSV(const R, G, B: Integer): TcaHSV;
var
  H, S, V, Delta, Min: Single;
begin
  H := 0;
  Min := MinValue([R, G, B]);
  V := MaxValue([R, G, B]);
  Delta := V - Min;
  // Calculate saturation:  saturation is 0 if r, g and b are all 0 
  if V = 0.0 then S := 0 else S := Delta / V;
  if S = 0.0 then
    H := NAN
  else
    begin // Chromatic 
      // between yellow and magenta [degrees] 
      if R = V then H := 60.0 * (G - B) / Delta else
      // between cyan and yellow 
        if G = V then H := 120.0 + 60.0 * (B - R) / Delta else
      // between magenta and cyan 
          if B = V then H := 240.0 + 60.0 * (R - G) / Delta;
      if H < 0.0 then H := H + 360.0
    end;
  Result.Hue := H;
  Result.Saturation := S;
  Result.Value := V;
end;

function TcaColorUtils.RGBToHSV(const ARGB: TcaRGB): TcaHSV;
begin
  Result := RGBToHSV(ARGB.Red, ARGB.Green, ARGB.Blue);
end;

function TcaColorUtils.RGBToIntensity(const R, G, B: Integer): Integer;
begin
  Result := (R + G + B) div 3;
end;

function TcaColorUtils.RGBToIntensity(const ARGB: TcaRGB): Integer;
begin
  Result := RGBToIntensity(ARGB.Red, ARGB.Green, ARGB.Blue);
end;

function TcaColorUtils.RGBToLightness(const R, G, B: Integer): Integer;
begin
  Result := (MinIntValue([R, G, B]) + MaxIntValue([R, G, B])) div 2;
end;

function TcaColorUtils.RGBToLightness(const ARGB: TcaRGB): Integer;
begin
  Result := RGBToLightness(ARGB.Red, ARGB.Green, ARGB.Blue);
end;

function TcaColorUtils.RGBToYIC(const R, G, B: Integer): Integer;
begin
  Result := Integer(77 * R + 150 * G + 29 * B) shr 8;
end;

function TcaColorUtils.RGBToYIC(const ARGB: TcaRGB): Integer;
begin
  Result := RGBToYIC(ARGB.Red, ARGB.Green, ARGB.Blue);
end;

  // Bitmap utils 

procedure TcaColorUtils.CreateDisabledBitmap(Source, Destination: TBitmap);
const
  ROP_DSPDxax = $00E20746;
var
  DDB, MonoBmp: TBitmap;
  IWidth, IHeight: Integer;
  IRect: TRect;
begin
  IWidth := Source.Width;
  IHeight := Source.Height;

  Destination.Width := IWidth;
  Destination.Height := IHeight;
  IRect := Rect(0, 0, IWidth, IHeight);
  Destination.Canvas.Brush.Color := clBtnFace;
  Destination.Palette := CopyPalette(Source.Palette);
  MonoBmp := nil;
  DDB := nil;
  try
    MonoBmp := TBitmap.Create;
    DDB := TBitmap.Create;
    DDB.Assign(Source);
    DDB.HandleType := bmDDB;

    { Create a disabled version }
    with MonoBmp do
    begin
      Assign(Source);
      HandleType := bmDDB;
      Canvas.Brush.Color := clBlack;
      Width := IWidth;
      if Monochrome then
      begin
        Canvas.Font.Color := clWhite;
        Monochrome := False;
        Canvas.Brush.Color := clWhite;
      end;
      Monochrome := True;
    end;
    with Destination.Canvas do
    begin
      Brush.Color := clBtnFace;
      FillRect(IRect);
      Brush.Color := clBtnHighlight;
      SetTextColor(Handle, clBlack);
      SetBkColor(Handle, clWhite);
      BitBlt(Handle, 1, 1, IWidth, IHeight,
             MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
      Brush.Color := clBtnShadow;
      SetTextColor(Handle, clBlack);
      SetBkColor(Handle, clWhite);
      BitBlt(Handle, 0, 0, IWidth, IHeight,
             MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
    end;
  finally
    DDB.Free;
    MonoBmp.Free;
  end;
  Source.Dormant;
end;

  // Private methods 

function TcaColorUtils.ChangeHSVValue(AColor: TColor; ADelta: Integer): TColor;
var
  RGB: TcaRGB;
  HSV: TcaHSV;
begin
  RGB := ColorToRGB(AColor);
  HSV := RGBToHSV(RGB);
  HSV.Value := HSV.Value + ADelta;
  RGB := HSVToRGB(HSV);
  Result := RGBToColor(RGB)
end;

  //---------------------------------------------------------------------------
  // Initialization / Finalization                                             
  //---------------------------------------------------------------------------

procedure CreateColorUtils;
begin
  if ColorUtils = nil then ColorUtils := TcaColorUtils.Create;
end;

procedure FreeColorUtils;
begin
  ColorUtils := nil;
end;

initialization
  CreateColorUtils;

finalization
  FreeColorUtils;

end.
