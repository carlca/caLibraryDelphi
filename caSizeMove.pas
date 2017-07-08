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


unit caSizeMove;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  Menus,

  // ca units 
  caUtils,
  caControls;

type

  TcaSizeMoveOption = (smSizing, smMoving);

  TcaSizeMoveOptions = set of TcaSizeMoveOption;

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizingBand                                                             
  //```````````````````````````````````````````````````````````````````````````

  TcaSizingBand = class(TCustomControl)
  protected
    procedure Paint; override;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizeMovePanelCaptionProperties                                         
  //```````````````````````````````````````````````````````````````````````````

  TcaSizeMovePanelCaptionProperties = class(TPersistent)
  private
    // Property fields 
    FBrushColor: TColor;
    FFont: TFont;
    FText: TCaption;
    FTextBorderWidth: Integer;
    // Event property fields 
    FOnChanged: TNotifyEvent;
    // Property methods 
    procedure SetBrushColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetText(const Value: TCaption);
    procedure SetTextBorderWidth(const Value: Integer);
    // Private methods 
    procedure Changed;
    // Event handlers 
    procedure FontChangedEvent(Sender: TObject);
  protected
    // Event triggers 
    procedure DoChanged; virtual;
  public
    // Create/Destroy 
    constructor Create;
    destructor Destroy; override;
    // Public methods 
    procedure Assign(Source: TPersistent); override;
    // Event properties 
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    // Published properties 
    property BrushColor: TColor read FBrushColor write SetBrushColor;
    property Font: TFont read FFont write SetFont;
    property Text: TCaption read FText write SetText;
    property TextBorderWidth: Integer read FTextBorderWidth write SetTextBorderWidth;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizeMoveSubscriber                                                     
  //```````````````````````````````````````````````````````````````````````````

  TcaSizeMoveSubscriber = class(TCollectionItem)
  private
    // Property fields 
    FControl: TControl;
    FOptions: TcaSizeMoveOptions;
    FSizingPixels: Integer;
    // Property methods 
    procedure SetControl(const Value: TControl);
  protected
    // Protected methods 
    function GetDisplayName: String; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;
    procedure SetDisplayName(const Value: String); override;
  public
    // Create/Destroy 
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    // Public methods 
    procedure Assign(Source: TPersistent); override;
  published
    // Properties 
    property Control: TControl read FControl write SetControl;
    property Options: TcaSizeMoveOptions read FOptions write FOptions;
    property SizingPixels: Integer read FSizingPixels write FSizingPixels;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizeMoveSubscribers                                                    
  //```````````````````````````````````````````````````````````````````````````

  TcaSizeMoveSubscribers = class(TOwnedCollection)
  private
    // Property methods 
    function GetItem(Index: Integer): TcaSizeMoveSubscriber;
    procedure SetItem(Index: Integer; Value: TcaSizeMoveSubscriber);
  protected
    // Protected methods 
    procedure ControlAdded(AControl: TControl);
    procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;
    procedure Update(Item: TCollectionItem); override;
  public
    // Public methods 
    function Add: TcaSizeMoveSubscriber;
    function FindSubscriber(AControl: TControl): TcaSizeMoveSubscriber;
    // Properties 
    property Items[Index: Integer]: TcaSizeMoveSubscriber read GetItem write SetItem; default;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizeMoveController                                                     
  //```````````````````````````````````````````````````````````````````````````

  TcaSizeMoveController = class(TComponent)
  private
    // Private fields 
    FBands: array[1..4] of TcaSizingBand;
    FCurX: Integer;
    FCurY: Integer;
    FFixX: Integer;
    FFixY: Integer;
    FMoving: Boolean;
    FMovX: Integer;
    FMovY: Integer;
    FSizing: Boolean;
    // Property fields 
    FControls: TcaSizeMoveSubscribers;
    // Event property fields 
    FOnMoveControl: TNotifyEvent;
    FOnSizeControl: TNotifyEvent;
    // Property methods 
    procedure SetControls(const Value: TcaSizeMoveSubscribers);
    // Event handlers 
    procedure ControlMouseDownEvent(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ControlMouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ControlMouseUpEvent(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    // Private methods 
    function GetNewHeight: Integer;
    function GetNewWidth: Integer;
    procedure CreateSizingBands;
    procedure FreeSizingBands;
    procedure PositionSizingBands(AControl: TControl; X1, Y1, X2, Y2: Integer);
  protected
    // Protected methods 
    procedure ControlAdded(AControl: TControl);
    procedure DoMoveControl(AControl: TControl); virtual;
    procedure DoSizeControl(AControl: TControl); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Controls: TcaSizeMoveSubscribers read FControls write SetControls;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TControlEx                                                                
  //```````````````````````````````````````````````````````````````````````````

  TControlEx = class(TControl);

implementation

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizingBand                                                             
  //```````````````````````````````````````````````````````````````````````````

procedure TcaSizingBand.Paint;
begin
  inherited;
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizeMovePanelCaptionProperties                                         
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaSizeMovePanelCaptionProperties.Create;
begin
  inherited;
  FFont := TFont.Create;
  FFont.OnChange := FontChangedEvent;
end;

destructor TcaSizeMovePanelCaptionProperties.Destroy;
begin
  FFont.Free;
  inherited;
end;

  // Public methods 

procedure TcaSizeMovePanelCaptionProperties.Assign(Source: TPersistent);
var
  SourceProperties: TcaSizeMovePanelCaptionProperties;
begin
  if Source is TcaSizeMovePanelCaptionProperties then
    begin
      SourceProperties := TcaSizeMovePanelCaptionProperties(Source);
      FBrushColor := SourceProperties.BrushColor;
      FFont.Assign(SourceProperties.Font);
      FText := SourceProperties.Text;
      FTextBorderWidth := SourceProperties.TextBorderWidth;
    end
  else
    inherited;
end;

  // Event triggers 

procedure TcaSizeMovePanelCaptionProperties.DoChanged;
begin
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

  // Private methods 

procedure TcaSizeMovePanelCaptionProperties.Changed;
begin
  DoChanged;
end;

  // Event handlers 

procedure TcaSizeMovePanelCaptionProperties.FontChangedEvent(Sender: TObject);
begin
  Changed;
end;

  // Property methods 

procedure TcaSizeMovePanelCaptionProperties.SetBrushColor(const Value: TColor);
begin
  if Value <> FBrushColor then
    begin
      FBrushColor := Value;
      Changed;
    end;
end;

procedure TcaSizeMovePanelCaptionProperties.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TcaSizeMovePanelCaptionProperties.SetText(const Value: TCaption);
begin
  if Value <> FText then
    begin
      FText := Value;
      Changed;
    end;
end;

procedure TcaSizeMovePanelCaptionProperties.SetTextBorderWidth(const Value: Integer);
begin
  if Value <> FTextBorderWidth then
    begin
      FTextBorderWidth := Value;
      Changed;
    end;
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizeMoveSubscriber                                                     
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaSizeMoveSubscriber.Create(ACollection: TCollection);
begin
  inherited;
end;

destructor TcaSizeMoveSubscriber.Destroy;
begin
  inherited;
end;

  // Public methods 

procedure TcaSizeMoveSubscriber.Assign(Source: TPersistent);
begin
  if Source is TcaSizeMoveSubscriber then
    Utils.ShallowCopy(Source, Self)
  else
    inherited;
end;

  // Protected methods 

function TcaSizeMoveSubscriber.GetDisplayName: String;
begin
  if Assigned(FControl) then
    Result := FControl.Name
  else
    Result := inherited GetDisplayName;
end;

procedure TcaSizeMoveSubscriber.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FControl) then
    FControl := nil;
end;

procedure TcaSizeMoveSubscriber.SetDisplayName(const Value: String);
begin
  inherited;
end;

  // Property methods 

procedure TcaSizeMoveSubscriber.SetControl(const Value: TControl);
begin
  if Value <> FControl then
    begin
      FControl := Value;
      if Assigned(FControl) then
        TcaSizeMoveSubscribers(Collection).ControlAdded(FControl);
    end;
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizeMoveSubscribers                                                    
  //```````````````````````````````````````````````````````````````````````````

  // Public methods 

function TcaSizeMoveSubscribers.Add: TcaSizeMoveSubscriber;
begin
  Result := TcaSizeMoveSubscriber(inherited Add);
end;

function TcaSizeMoveSubscribers.FindSubscriber(AControl: TControl): TcaSizeMoveSubscriber;
var
  Index: Integer;
  Subscriber: TcaSizeMoveSubscriber;
begin
  Result := nil;
  for Index := 0 to Pred(Count) do
    begin
      Subscriber := GetItem(Index);
      if Subscriber.Control = AControl then
        Result := Subscriber;
    end;
end;

  // Protected methods 

procedure TcaSizeMoveSubscribers.ControlAdded(AControl: TControl);
begin
  TcaSizeMoveController(Owner).ControlAdded(AControl);
end;

procedure TcaSizeMoveSubscribers.Notification(AComponent: TComponent; Operation: TOperation);
var
  Index: Integer;
begin
  for Index := 0 to Pred(Count) do
    GetItem(Index).Notification(AComponent, Operation);
end;

procedure TcaSizeMoveSubscribers.Update(Item: TCollectionItem);
begin
  inherited;
end;

  // Property methods 

function TcaSizeMoveSubscribers.GetItem(Index: Integer): TcaSizeMoveSubscriber;
begin
  Result := TcaSizeMoveSubscriber(inherited GetItem(Index));
end;

procedure TcaSizeMoveSubscribers.SetItem(Index: Integer; Value: TcaSizeMoveSubscriber);
begin
  inherited SetItem(Index, Value);
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizeMoveController                                                     
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaSizeMoveController.Create(AOwner: TComponent);
begin
  inherited;
  FControls := TcaSizeMoveSubscribers.Create(Self, TcaSizeMoveSubscriber);
end;

destructor TcaSizeMoveController.Destroy;
begin
  FreeSizingBands;
  FControls.Free;
  inherited;
end;

  // Protected methods 

procedure TcaSizeMoveController.ControlAdded(AControl: TControl);
begin
  TControlEx(AControl).OnMouseDown := ControlMouseDownEvent;
  TControlEx(AControl).OnMouseMove := ControlMouseMoveEvent;
  TControlEx(AControl).OnMouseUp := ControlMouseUpEvent;
end;

procedure TcaSizeMoveController.DoMoveControl(AControl: TControl); 
begin
  if Assigned(FOnMoveControl) then FOnMoveControl(AControl);
end;

procedure TcaSizeMoveController.DoSizeControl(AControl: TControl);
begin
  if Assigned(FOnSizeControl) then FOnSizeControl(AControl);
end;

procedure TcaSizeMoveController.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Assigned(FControls) then
    FControls.Notification(AComponent, Operation);
end;

  // Private methods 

function TcaSizeMoveController.GetNewHeight: Integer;
begin
  Result := FMovY - FFixY + 1;
end;

function TcaSizeMoveController.GetNewWidth: Integer;
begin
  Result := FMovX - FFixX + 1;
end;

procedure TcaSizeMoveController.CreateSizingBands;
var
  Band: TcaSizingBand;
  Index: Integer;
begin
  for Index := 1 to 4 do
    begin
      FBands[Index] := TcaSizingBand.Create(Owner);
      Band := FBands[Index];
      Band.Visible := False;
      Band.Parent := Application.MainForm; // ?? 
      Band.Color := clBlack;
      Band.Height := 1;
      Band.Width := 1;
      Band.Visible := True;
    end;
end;

procedure TcaSizeMoveController.FreeSizingBands;
var
  Index: Integer;
begin
  for Index := 1 to 4 do
    if FBands[Index] <> nil then
      begin
        FBands[Index].Free;
        FBands[Index] := nil;
      end;
end;

procedure TcaSizeMoveController.PositionSizingBands(AControl: TControl; X1, Y1, X2, Y2: Integer);
begin
  // Left 
  FBands[1].SetBounds(X1, Y1, 1, Y2 - Y1 + 1);
  // Top 
  FBands[2].SetBounds(X1, Y1, X2 - X1 + 1, 1);
  // Right 
  FBands[3].SetBounds(X2, Y1, 1, FBands[1].Height);
  // Bottom 
  FBands[4].SetBounds(X1, Y2, FBands[2].Width, 1);
end;

  // Event handlers 

procedure TcaSizeMoveController.ControlMouseDownEvent(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  SC_DRAGMOVE = $F012;
var
  CaptionHeight: Integer;
  Control: TControl;
  OwnerForm: TWinControl;
  Subscriber: TcaSizeMoveSubscriber;
begin
  Control := TControl(Sender);
  Control.BringToFront;
  Subscriber := FControls.FindSubscriber(Control);
  if Assigned(Subscriber) then
    begin
      if smSizing in Subscriber.Options then
        begin
          if (X > Control.Width - Subscriber.SizingPixels)
          and (Y > Control.Height - Subscriber.SizingPixels) then
            begin
              FSizing := True;

              OwnerForm := Application.MainForm;
              CaptionHeight := GetSystemMetrics(SM_CYCAPTION);

              CreateSizingBands;

              FCurX := X;
              FCurY := Y;
              FFixX := Control.ClientOrigin.x - OwnerForm.Left - 4;
              FFixY := Control.ClientOrigin.y - CaptionHeight - OwnerForm.Top - 4;
              FMovX := FFixX + Control.Width - 1;
              FMovY := FFixY + Control.Height - 1;

              PositionSizingBands(Control, FFixX, FFixY, FMovX, FMovY);
            end;
        end;
      if (smMoving in Subscriber.Options) and (not FSizing) then
        begin
          FSizing := False;
          FMoving := True;
          ReleaseCapture;
          Control.Perform(WM_SYSCOMMAND, SC_DRAGMOVE, 0);
        end;
    end;
end;

procedure TcaSizeMoveController.ControlMouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Control: TControl;
begin
  Control := TControl(Sender);
  if FSizing then
    begin
      if not (akRight in Control.Anchors) then
        begin
          Inc(FMovX, X - FCurX);
          FCurX := X;
        end;
      if not (akBottom in Control.Anchors) then
        begin
          Inc(FMovY, Y - FCurY);
          FCurY := Y;
          PositionSizingBands(Control, FFixX, FFixY, FMovX, FMovY);
        end;
      DoSizeControl(Control);
    end;
  if FMoving then
    DoMoveControl(Control);
end;

procedure TcaSizeMoveController.ControlMouseUpEvent(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Control: TControl;
  Subscriber: TcaSizeMoveSubscriber;
begin
  Control := TControl(Sender);
  Subscriber := FControls.FindSubscriber(Control);
  if Assigned(Subscriber) then
    begin
      FMoving := False;
      FreeSizingBands;
      if FSizing and (smSizing in Subscriber.Options) then
        begin
          Control.Width := GetNewWidth;
          Control.Height := GetNewHeight;
          // AlignMain;
          FSizing := False;
        end;
    end;
end;

  // Property methods

procedure TcaSizeMoveController.SetControls(const Value: TcaSizeMoveSubscribers);
begin
  FControls.Assign(Value);
end;

end.
