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


unit caMemMapImage;

{$INCLUDE ca.inc}

interface

uses

  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,

  caControls;

type

  TcaBMPFilename = type string;

  //----------------------------------------------------------------------------
  // TcaMemMapImage                                                             
  //----------------------------------------------------------------------------

  TcaMemMapImage = class(TcaGraphicControl)
  private
    FPalette: HPalette;
    FData: Pointer;
    FBitmapWidth: Integer;
    FBitmapHeight: Integer;
    FColours: Integer;
    FFileHeader: PBitmapFileHeader;
    FInfoHeader: PBitmapInfoHeader;
    FInfo: PBitmapInfo;
    FPixelStart: Pointer;
    // Property fields
    FActive: Boolean;
    FAutoSize: Boolean;
    FFileName: TcaBMPFilename;
    FOffsetX: Integer;
    FOffsetY: Integer;
    FStretch: Boolean;
    FCentre: Boolean;
    // Property methods
    procedure SetActive(Value: Boolean);
    procedure SetFilename(const Value: TcaBMPFilename);
    procedure SetOffsetX(const Value: Integer);
    procedure SetOffsetY(const Value: Integer);
    procedure SetStretch(Value: Boolean);
    procedure SetCentre(Value: Boolean);
  protected
    // Protected property methods
    procedure SetAutoSize(Value: Boolean); override;
    // Protected methods
    procedure Changed; virtual;
    procedure CloseViewer; virtual;
    procedure GetBitmapPalette;
    procedure OpenViewer; virtual;
    procedure BufferedPaint(C: TCanvas; R: TRect); override; 
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods
    procedure Close;
    procedure Open;
    // Public properties
    property BitmapFileHeader: PBitmapFileHeader read FFileHeader;
    property BitmapInfoHeader: PBitmapInfoHeader read FInfoHeader;
    property BitmapInfo: PBitmapInfo read FInfo;
    property PixelStart: Pointer read FPixelStart;
    property Palette: HPalette read FPalette;
  published
    // Published properties
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property Centre: Boolean read FCentre write SetCentre default False;
    property Filename: TcaBMPFilename read FFilename write SetFilename;
    property OffsetX: Integer read FOffsetX write SetOffsetX;
    property OffsetY: Integer read FOffsetY write SetOffsetY;
    property Stretch: Boolean read FStretch write SetStretch default False;
    // Read-only properties
    property Colours: Integer read FColours;
    property BitmapHeight: Integer read FBitmapHeight;
    property BitmapWidth: Integer read FBitmapWidth;
    // Promoted properties
    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    // This must be streamed last
    property Active: Boolean read FActive write SetActive default False;
  end;

implementation

const
  BitmapSignature = $4D42;

procedure InvalidBitmap;
begin
  raise Exception.Create('Bitmap image is not valid')
end;

procedure NotWhenActive;
begin
  raise Exception.Create('Not on an active big bitmap viewer')
end;

constructor TcaMemMapImage.Create(AOwner: TComponent);
begin
  inherited;
  Width := 150;
  Height := 150;
end;

destructor TcaMemMapImage.Destroy;
begin
  CloseViewer;
  inherited;
end;

  // Public methods

procedure TcaMemMapImage.Close;
begin
  Active := False;
end;

procedure TcaMemMapImage.Open;
begin
  Active := True;
end;

  // Protected methods

procedure TcaMemMapImage.Changed;
begin
  if (BitmapWidth >= Width) and (BitmapHeight >= Height) then
    ControlStyle := ControlStyle + [csOpaque]
  else
    ControlStyle := ControlStyle - [csOpaque];

  if AutoSize and (BitmapWidth > 0) and (BitmapHeight > 0) then
    SetBounds(Left, Top, BitmapWidth, BitmapHeight)
  else
    Invalidate;
end;

procedure TcaMemMapImage.CloseViewer;
begin
  if FActive then
    begin
      FActive := False;
      if FData <> nil then
        begin
          UnmapViewOfFile(FData); 
          FData := nil
        end;
      if FPalette <> 0 then
        DeleteObject(FPalette);
    end;
end;

procedure TcaMemMapImage.GetBitmapPalette;
var
  SysPalSize: Integer;
  Index: Integer;
  LogSize: Integer;
  LogPalette: PLogPalette;
  DC: HDC;
  Focus: HWND;
begin
  if FColours > 2 then
    begin
      LogSize := SizeOf(TLogPalette) + pred(FColours) * SizeOf(TPaletteEntry);
      LogPalette := AllocMem(LogSize);
      try
        LogPalette^.palNumEntries := FColours;
        LogPalette^.palVersion := $0300;
        {$IFOPT R+}
          {$DEFINE R_PLUS}
          {$R-}
        {$ENDIF}
        Focus := GetFocus;
        DC := GetDC(Focus);
        try
          SysPalSize := GetDeviceCaps(DC, SIZEPALETTE);
          if (FColours = 16) and (SysPalSize >= 16) then
            begin
              GetSystemPaletteEntries(DC, 0, 8, LogPalette^.palPalEntry);
              Index := 8;
              GetSystemPaletteEntries(DC, SysPalSize - Index, Index, LogPalette^.palPalEntry[Index])
            end
          else
            begin
              for Index := 0 to pred(FColours) do
                begin
                  LogPalette^.palPalEntry[Index].peRed := FInfo^.bmiColors[Index].rgbRed;
                  LogPalette^.palPalEntry[Index].peGreen := FInfo^.bmiColors[Index].rgbGreen;
                  LogPalette^.palPalEntry[Index].peBlue := FInfo^.bmiColors[Index].rgbBlue
                end
            end;
        finally
          ReleaseDC(Focus, DC)
        end;
        {$IFDEF R_PLUS}
          {$R+}
          {$UNDEF R_PLUS}
        {$ENDIF}
        FPalette := CreatePalette(LogPalette^)
      finally
        FreeMem(LogPalette, LogSize)
      end
    end
end;

procedure TcaMemMapImage.OpenViewer;
var
  FileHandle: THandle;
  MapHandle: THandle;
begin
  if FActive then exit;

  // Open file
  FileHandle := FileOpen(FFileName, fmOpenRead + fmShareDenyNone);
  if FileHandle = INVALID_HANDLE_VALUE then
    raise Exception.Create('Failed to open ' + FFilename);

  // Create file map
  try
    MapHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
    if MapHandle = 0 then
      raise Exception.Create('Failed to map file')
  finally
    CloseHandle(FileHandle)
  end;

  // View file map
  try
    FData := MapViewOfFile(MapHandle, FILE_MAP_READ, 0, 0, 0);
    if FData = nil then
      raise Exception.Create('Failed to view map file')
  finally
    CloseHandle(MapHandle)
  end;

  // Set Pointers into file view
  FFileHeader := FData;

  // Test for valid bitmap file:
  if FFileHeader^.bfType <> BitmapSignature then
  begin
    UnmapViewOfFile(FData);
    FData := nil;
    InvalidBitmap
  end;

  // Set up other Pointers
  FInfoHeader := Pointer(integer(FData) + sizeof(TBitmapFileHeader));
  FInfo := Pointer(FInfoHeader);
  FPixelStart := Pointer(LongWord(FData) + FFileHeader^.bfOffBits);

  // Get number of colours
  if FInfoHeader^.biClrUsed <> 0 then
    FColours := FInfoHeader^.biClrUsed
  else
    begin
      case FInfoHeader^.biBitCount of
        1, 4, 8 : FColours := 1 shl FInfoHeader^.biBitCount
      else
        FColours := 0
      end;
    end;

  // Get bitmap size into easy to access variables
  FBitmapHeight := FInfoHeader^.biHeight;
  FBitmapWidth := FInfoHeader^.biWidth;

  // Fetch the palette
  GetBitmapPalette;

  // Trigger the changes
  FActive := True;
  Changed;
end;

procedure TcaMemMapImage.BufferedPaint(C: TCanvas; R: TRect);
var
  OldPalette: HPalette;
  Dest: TRect;
begin
  if (csDesigning in ComponentState) and not FActive then
    begin
      C.Pen.Style := psDash;
      C.Brush.Style := bsClear;
      C.Rectangle(0, 0, Width, Height)
    end
  else
    begin
      if FPalette <> 0 then
        OldPalette := SelectPalette(C.Handle, FPalette, False)
      else
        OldPalette := 0;

      try
        RealizePalette(C.Handle);

        if FStretch then
          Dest := ClientRect
        else
          begin
            if Centre then
              Dest := Rect((Width - FBitmapWidth) div 2,
                           (Height - FBitmapHeight) div 2,
                            FBitmapWidth, FBitmapHeight)
            else
              Dest := Rect(0, 0, FBitmapWidth, FBitmapHeight);
          end;

        // OffsetRect(Dest, FOffsetX, FOffsetY);
        Dest.Left := Dest.Left + FOffsetX;
        Dest.Right := Dest.Right + FOffsetX;
        Dest.Top := Dest.Top + FOffsetY;
        Dest.Bottom := Dest.Bottom + FOffsetY;

        StretchDIBits(C.Handle,
                      Dest.Left, Dest.Top, Dest.Right, Dest.Bottom,
                      0, 0, FBitmapWidth, FBitmapHeight,
                      FPixelStart, FInfo^,
                      DIB_RGB_COLORS, SRCCOPY)
      finally
        if OldPalette <> 0 then
          SelectPalette(C.Handle, OldPalette, False)
      end;
    end;
end;

  // Property methods

procedure TcaMemMapImage.SetActive(Value : boolean);
begin
  if Value <> FActive then
    if Value then
      OpenViewer
    else
      CloseViewer
end;

procedure TcaMemMapImage.SetAutoSize(Value : boolean);
begin
  if Value <> FAutoSize then
  begin
    FAutoSize := Value;
    Changed
  end
end;

procedure TcaMemMapImage.SetStretch(Value : boolean);
begin
  if Value <> FStretch then
  begin
    FStretch := Value;
    Changed
  end
end;

procedure TcaMemMapImage.SetCentre(Value : boolean);
begin
  if Value <> FCentre then
  begin
    FCentre := Value;
    Changed
  end
end;

procedure TcaMemMapImage.SetFilename(const Value : TcaBMPFilename);
begin
  if Value <> FFilename then
  begin
    if FActive then
      NotWhenActive;
    FFilename := Value
  end
end;

procedure TcaMemMapImage.SetOffsetX(const Value: Integer);
begin
  if Value <> FOffsetX then
    begin
      FOffsetX := Value;
      Invalidate;
    end;
end;

procedure TcaMemMapImage.SetOffsetY(const Value: Integer);
begin
  if Value <> FOffsetY then
    begin
      FOffsetY := Value;
      Invalidate;
    end;
end;

end.
