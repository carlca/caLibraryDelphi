unit caShape;

{$B-}

interface

uses

  // Standard Delphi units 
  Windows,
  Controls,
  Classes,
  Forms,
  ExtCtrls,
  Graphics,
  Messages,
  Math,

  // ca units 
  caClasses,
  caUtils;

type

  TcaTextShape = class;

  //```````````````````````````````````````````````````````````````````````````
  // TcaCustomShape                                                            
  //```````````````````````````````````````````````````````````````````````````

  TcaCustomShape = class(TGraphicControl)
  private
    // Private fields 
    FCanProcessMouseMsg: Boolean;
    FCaption: TcaTextShape;
    FSelected: Boolean;
    FWasCovered: Boolean;
  protected
    // Protected methods 
    function GetCustomShapeAtPos(X, Y: Integer): TcaCustomShape;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetCaption(Value: TcaTextShape); virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetSelected(Value: Boolean); virtual;
    // Protected properties 
    property CanProcessMouseMsg: Boolean read FCanProcessMouseMsg write FCanProcessMouseMsg;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
    procedure AlignCaption(Alignment: TAlignment);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    // Public properties 
    property Selected: Boolean read FSelected write SetSelected;
  published
    // Promoted properties 
    property OnClick;
    property OnDblClick;
    // Properties 
    property Caption: TcaTextShape read FCaption write SetCaption;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaMoveableShape                                                          
  //```````````````````````````````````````````````````````````````````````````

  TcaMoveableShape = class(TcaCustomShape)
  private
    // Private fields 
    FOrigin: TPoint;
    FMoving: Boolean;
  protected
    // Protected methods 
    function IsValidMove(DeltaX, DeltaY: Integer): Boolean;
    procedure EndMove;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Move(DeltaX, DeltaY: Integer);
    procedure MoveShapes(DeltaX, DeltaY: Integer);
    procedure StartMove(X, Y: Integer);
    // Protected properties 
    property Moving: Boolean read FMoving write FMoving;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizeableShape                                                          
  //```````````````````````````````````````````````````````````````````````````

  TcaSizingMode = (smTopLeft, smTop, smTopRight, smLeft, smRight, smBottomLeft, smBottom, smBottomRight, smNone);

  TcaSizeableShape = class(TcaMoveableShape)
  private
    // Private fields 
    FMinHeight: Integer;
    FMinWidth: Integer;
    FSizeOrigin: TPoint;
    FSizeRectHeight: Integer;
    FSizeRectWidth: Integer;
    FSizingMode: TcaSizingMode;
  protected
    // Protected methods 
    function GetSizeRect(SizeRectType: TcaSizingMode): TRect;
    procedure CheckForSizeRects(X, Y: Integer);
    procedure DrawSizingRects;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure ResizeControl(X, Y: Integer);
    procedure SetSelected(Value: Boolean); override;
    // Protected properties 
    property MinHeight: Integer read FMinHeight write FMinHeight;
    property MinWidth: Integer read FMinWidth write FMinWidth;
    property SizeRectHeight: Integer read FSizeRectHeight write FSizeRectHeight;
    property SizeRectWidth: Integer read FSizeRectWidth write FSizeRectWidth;
    property SizingMode: TcaSizingMode read FSizingMode write FSizingMode;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    // Public methods 
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaTextShape                                                              
  //```````````````````````````````````````````````````````````````````````````

  TcaTextShape = class(TcaSizeableShape)
  private
    // Private fields 
    FAutosize: Boolean;
    FFont: TFont;
    FText: string;
    // Property methods 
    procedure SetAutosize(Value: Boolean); reintroduce;
    procedure SetFont(Value: TFont);
    procedure SetText(Value: string);
    // Private methods 
    procedure FontChanged(Sender: TObject);
  protected
    // Protected methods 
    procedure Paint; override;
    procedure RefreshText;
    procedure SetParent(AParent: TWinControl); override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    // Published properties 
    property Autosize: Boolean read FAutosize write SetAutosize;
    property Font: TFont read FFont write SetFont;
    property Text: string read FText write SetText;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaBitmapShape                                                            
  //```````````````````````````````````````````````````````````````````````````

  TcaBitmapShape = class(TcaMoveableShape)
  private
    // Private fields 
    FImages: TImageList;
    FImageIndex: Integer;
    // Property methods 
    procedure SetImages(Value: TImageList);
    procedure SetImageIndex(Value: Integer);
  protected
    // Protected methods 
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure SetSelected(Value: Boolean); override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
  published
    // Published properties 
    property Images: TImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaStandardShape                                                          
  //```````````````````````````````````````````````````````````````````````````

  TcaStandardShape = class(TcaSizeableShape)
  private
    // Private fields 
    FShapeType: TShapeType;
    FLineColour: TColor;
    // Property methods 
    procedure SetShapeType(Value: TShapeType);
  protected
    // Protected methods 
    procedure Paint; override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
  published
    // Published properties 
    property LineColour: TColor read FLineColour write FLineColour default clBlack;
    property ShapeType: TShapeType read FShapeType write SetShapeType;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaConnection                                                             
  //```````````````````````````````````````````````````````````````````````````

  TcaConnectionSide = (csLeft, csRight, csTop, csBottom);

  TcaConnection = class(TPersistent)
  private
    // Private fields 
    FShape: TcaCustomShape;
    FSide: TcaConnectionSide;   // Side to connect to
    FOffset: Integer;           // Distance from top or left of side
  public
    // Create/Destroy 
    constructor Create;
    // Public methods 
    function BottomMost(TerminatorRect: TRect): TPoint;
    function ConnPoint(TerminatorRect: TRect): TPoint;
    function LeftMost(TerminatorRect: TRect): TPoint;
    function RightMost(TerminatorRect: TRect): TPoint;
    function TermPoint(TerminatorRect: TRect): TPoint;
    function TopMost(TerminatorRect: TRect): TPoint;
    procedure Assign(Source: TPersistent); override;
  published
    // Published properties 
    property Shape: TcaCustomShape read FShape write FShape;
    property Side: TcaConnectionSide read FSide write FSide;
    property Offset: Integer read FOffset write FOffset;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaConnector                                                              
  //```````````````````````````````````````````````````````````````````````````

  TcaConnector = class(TcaCustomShape)
  private
    // Private fields 
    FLineWidth: Integer;
    FLineColour: TColor;
    FStartConn: TcaConnection;
    FEndConn: TcaConnection;
    FStartTermRect: TRect;
    FEndTermRect: TRect;
    FMidPoint: TPoint;
    // Property methods 
    function GetConn(Index: Integer): TcaConnection;
    function GetMidPoint: TPoint;
    function GetTermRect(Index: Integer): TRect;
    procedure SetConn(Index: Integer; Value: TcaConnection);
    procedure SetLineWidth(Value: Integer);
    procedure SetTermRect(Index: Integer; Value: TRect);
    // Private methods 
    procedure CheckSize(var AWidth, AHeight: Integer);
  protected
    // Protected methods 
    function Convert(APoint: TPoint): TPoint;
    function IsConnected(ConnectedShape: TcaCustomShape): Boolean;
    procedure DrawEndTerminator; virtual;
    procedure DrawStartTerminator; virtual;
    procedure MoveCaption;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure SetCaption(Value: TcaTextShape); override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
    procedure SetBoundingRect;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetConnections(AStartConn, AEndConn: TcaConnection);
    // Public properties 
    property EndTermRect: TRect index 2 read GetTermRect write SetTermRect;
    property StartTermRect: TRect index 1 read GetTermRect write SetTermRect;
  published
    // Published properties 
    property LineWidth: Integer read FLineWidth write SetLineWidth default 1;
    property LineColour: TColor read FLineColour write FLineColour default clBlack;
    property StartConn: TcaConnection index 1 read GetConn write SetConn;
    property EndConn: TcaConnection index 2 read GetConn write SetConn;
    property MidPoint: TPoint read GetMidPoint;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaSingleHeadArrow                                                        
  //```````````````````````````````````````````````````````````````````````````

  TcaSingleHeadArrow = class(TcaConnector)
  protected
    // Protected methods 
    procedure DrawArrowHead(ConnPt, TermPt: TPoint);
    procedure DrawEndTerminator; override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaBluntSingleHeadArrow                                                   
  //```````````````````````````````````````````````````````````````````````````

  TcaBluntSingleHeadArrow = class(TcaSingleHeadArrow)
  protected
    // Protected methods 
    procedure DrawStartTerminator; override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaDoubleHeadArrow                                                        
  //```````````````````````````````````````````````````````````````````````````

  TcaDoubleHeadArrow = class(TcaSingleHeadArrow)
  protected
    // Protected methods 
    procedure DrawStartTerminator; override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // IcaShapeUtils                                                             
  //```````````````````````````````````````````````````````````````````````````

  IcaShapeUtils = interface
  ['{C5654C71-A9AE-4551-ABCA-21A912A4D173}']
    // Interface methods 
    function ArrayMax(A: array of Integer): Integer;
    function ArrayMin(A: array of Integer): Integer;
    function InRect(X, Y: Integer; ARect: TRect): Boolean;
    function NextShapeSuffix: string;
    function RectHeight(ARect: TRect): Integer;
    function RectWidth(ARect: TRect): Integer;
    procedure NoLessThan(var Value: Integer; ALimit: Integer);
  end;

  //```````````````````````````````````````````````````````````````````````````
  // IcaShapeManager                                                           
  //```````````````````````````````````````````````````````````````````````````

  IcaShapeManager = interface
  ['{7B2E667B-27E2-4494-9695-3D80919A6C6E}']
    // Interface methods 
    procedure DeleteAllShapes(ParentControl: TWinControl);
    procedure DeleteSelectedShapes(ParentControl: TWinControl);
    procedure LoadFromFile(const FileName: string; ParentControl: TWinControl);
    procedure SaveToFile(const FileName: string; ParentControl: TWinControl);
    procedure UnselectAllShapes(ParentControl: TWinControl);
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaShapeUtils                                                             
  //```````````````````````````````````````````````````````````````````````````

  TcaShapeUtils = class(TInterfacedObject, IcaShapeUtils, IcaShapeManager)
  private
    // Private fields 
    FShapeIndex: Integer;
  protected
    // Interface methods - IcaShapeUtils 
    function ArrayMax(A: array of Integer): Integer;
    function ArrayMin(A: array of Integer): Integer;
    function InRect(X, Y: Integer; ARect: TRect): Boolean;
    function NextShapeSuffix: string;
    function RectHeight(ARect: TRect): Integer;
    function RectWidth(ARect: TRect): Integer;
    procedure NoLessThan(var Value: Integer; ALimit: Integer);
    // Interface methods - IcaShapeManager 
    procedure DeleteAllShapes(ParentControl: TWinControl);
    procedure DeleteSelectedShapes(ParentControl: TWinControl);
    procedure LoadFromFile(const FileName: string; ParentControl: TWinControl);
    procedure SaveToFile(const FileName: string; ParentControl: TWinControl);
    procedure UnselectAllShapes(ParentControl: TWinControl);
  end;

  //```````````````````````````````````````````````````````````````````````````
  // CoShapeUtilsFactory                                                       
  //```````````````````````````````````````````````````````````````````````````

  CoShapeUtilsFactory = class
  public
    class function Instance: IcaShapeUtils;
  end;

var
  gShapeManager: IcaShapeManager = nil;

implementation

uses

  // Standard Delphi units 
  SysUtils,
  ImgList,
  Dialogs;

type

  TControlEx = class(TControl);

var
  gShapeUtils: IcaShapeUtils = nil;

  //```````````````````````````````````````````````````````````````````````````
  // TcaCustomShape                                                            
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaCustomShape.Create(AOwner: TComponent);
var
  AlreadyUsed: Boolean;
  Index: Integer;
  TempName: string;
begin
  inherited Create(AOwner);
  FCanProcessMouseMsg := True;
  FCaption := nil;
  FSelected := False;
  FWasCovered := False;
  // Give the component a name and ensure that it is unique
  repeat
    // Use a local variable to hold the name, so that don't get exceptions
    // raised on duplicate names
    TempName := 'Shape' + gShapeUtils.NextShapeSuffix;
    AlreadyUsed := False;
    // Loop through all the components on the form to ensure that this name
    // is not already in use
    for Index := 0 to Owner.ComponentCount - 1 do
      begin
        if Owner.Components[Index].Name = TempName then
          begin
            // Try the next component name as this one is used already
            AlreadyUsed := True;
            Break;
          end;
      end;
  until not AlreadyUsed;
  Name := TempName;
end; 

destructor TcaCustomShape.Destroy;
var
  Index: Integer;
begin
  FCaption.Free;
  // First check that this control has been placed on a form
  if Assigned(Parent) then
    begin
      // Search parent control for TcaConnector components that connect
      // to this component
      Index := 0;
      while Index < Parent.ControlCount do
        begin
          if (Parent.Controls[Index] is TcaConnector) and
            (TcaConnector(Parent.Controls[Index]).IsConnected(Self)) then
              begin
              Parent.Controls[Index].Free;
            end else
            begin
              Inc(Index);
            end;
        end;
    end;
  inherited Destroy;
end;

  // Public methods 

procedure TcaCustomShape.AlignCaption(Alignment: TAlignment);
var
  ALeft, ATop, AWidth, AHeight: Integer;
begin
  if not Assigned(FCaption) then Exit;
  ALeft := Left;
  ATop := Top + Height + 5;
  AWidth := FCaption.Width;
  AHeight := FCaption.Height;
  case Alignment of
    taLeftJustify: ALeft := Left;
    taRightJustify: ALeft := Left + Width - 1;
    taCenter: ALeft := Left + ((Width - FCaption.Width) div 2);
  end;
  FCaption.SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TcaCustomShape.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  Index: Integer;
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if not Assigned(Parent) then Exit;
  // Search parent control for TcaConnector components 
  for Index := 0 to Parent.ControlCount - 1 do
    if Parent.Controls[Index] is TcaConnector then
      if TcaConnector(Parent.Controls[Index]).IsConnected(Self) then
        TcaConnector(Parent.Controls[Index]).SetBoundingRect;
end;

  // Protected methods 

function TcaCustomShape.GetCustomShapeAtPos(X, Y: Integer): TcaCustomShape;
var
  Index: Integer;
  Pt: TPoint;
begin
  Result := nil;
  if Parent <> nil then
    begin
      Pt := Parent.ScreenToClient(ClientToScreen(Point(X, Y)));
      for Index := 0 to Parent.ControlCount - 1 do
        begin
          if (Parent.Controls[Index] <> Self) and
            (Parent.Controls[Index] is TcaCustomShape) and
            TcaCustomShape(Parent.Controls[Index]).CanProcessMouseMsg and
            gShapeUtils.InRect(Pt.X, Pt.Y, Parent.Controls[Index].BoundsRect) then
              begin
              Result := TcaCustomShape(Parent.Controls[Index]);
              Exit;
            end;
        end;
    end;
end;

procedure TcaCustomShape.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TempPt: TPoint;
  CoveredShape: TcaCustomShape;
begin 
  if FCanProcessMouseMsg then
    begin
      BringToFront;
      MouseCapture := True;
      inherited MouseDown(Button, Shift, X, Y);
    end
  else
    begin
      // Pass message on to any covered control capable of handling it
      CoveredShape := GetCustomShapeAtPos(X, Y);
      TempPt := Point(X, Y);
      MouseCapture := False;
      if CoveredShape <> nil then
        begin
          SendToBack;
          // Convert coordinates to covered shape's coordinates
          TempPt := CoveredShape.ScreenToClient(ClientToScreen(TempPt));
          // Send the mouse down message to the covered shape
          CoveredShape.MouseDown(Button, Shift, TempPt.X, TempPt.Y);
          // Flag the control as having been covered because we lose a mouse click
          CoveredShape.FWasCovered := True;
        end else if Assigned(Parent) then
        begin
          // Send mouse down message to Parent. The typecast is purely to gain access
          // to the Parent.MouseDown method. Need to convert coordinates to parent's
          // coordinates
          TempPt := Parent.ScreenToClient(ClientToScreen(TempPt));
          TControlEx(Parent).MouseDown(Button, Shift, TempPt.X, TempPt.Y);
        end;
    end;
end;

procedure TcaCustomShape.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FWasCovered then
    begin
      Click;
      FWasCovered := False;
    end;
end;

procedure TcaCustomShape.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FCaption) then
    FCaption := nil;
end;

procedure TcaCustomShape.SetCaption(Value: TcaTextShape);
begin
  if (Value = nil) and Assigned(FCaption) then
    begin
      FCaption.Free;
      FCaption := nil;
    end
  else
    if (Value <> FCaption) then
      begin
        FCaption := Value;
        FCaption.Parent := Self.Parent;
        // Ensure the caption gets aligned correctly. Ths only needs to happen if
        // the caption has not already been set in place (it will already be in the
        // right place if we are loading this from a file).
        if (FCaption.Left = 0) and (FCaption.Top = 0) then
          AlignCaption(taCenter);
      end;
end;

procedure TcaCustomShape.SetParent(AParent: TWinControl);
begin 
  inherited SetParent(AParent);
  if Assigned(FCaption) then
    FCaption.Parent := AParent;
end; 

procedure TcaCustomShape.SetSelected(Value: Boolean);
begin
  FSelected := Value;
  if Assigned(FCaption) then
    FCaption.SetSelected(Value);
end; 

  //```````````````````````````````````````````````````````````````````````````
  // TcaMoveableShape                                                          
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaMoveableShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetSelected(False);
end;

  // Protected methods 

function TcaMoveableShape.IsValidMove(DeltaX, DeltaY: Integer): Boolean;
var
  R: TRect;
begin
  Result := True;
  if Parent <> nil then
    begin
      if Selected then
        begin
          R := Parent.ClientRect;
          Result := (Left + DeltaX >= 0) and
                    (Top + DeltaY >= 0) and
                    (Left + DeltaX + Width - 1 < R.Right - R.Left) and
                    (Top + DeltaY + Height - 1 < R.Bottom - R.Top);
        end;
    end;
end;

procedure TcaMoveableShape.EndMove;
begin
  FMoving := False;
  FOrigin := Point(0, 0);
end;

procedure TcaMoveableShape.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin 
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
    begin
      if not (ssShift in Shift) then gShapeManager.UnselectAllShapes(Parent);
      StartMove(X, Y);
    end;
end;

procedure TcaMoveableShape.MouseMove(Shift: TShiftState; X, Y: Integer);
begin 
  inherited MouseMove(Shift, X, Y);
  if not (ssLeft in Shift) then
    begin
      Moving := False;
      Exit;
    end;
  if Moving then
    MoveShapes(X - FOrigin.X, Y - FOrigin.Y);
end;

procedure TcaMoveableShape.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  Control: TControl;
begin 
  inherited MouseUp(Button, Shift, X, Y);
  // Only interested in left mouse button events
  if Button = mbLeft then
    begin
      EndMove;
      // If this shape is covering any smaller shapes then send it to the back,
      // so that we can get at the smaller ones
      for Index := 0 to Parent.ControlCount - 1 do
        begin
          Control := Parent.Controls[Index];
          if (Control <> Self)
          and (Control is TcaCustomShape)
          and TcaCustomShape(Control).CanProcessMouseMsg
          and gShapeUtils.InRect(Control.Left, Control.Top, BoundsRect)
          and gShapeUtils.InRect(Control.Left + Control.Width, Control.Top + Control.Height, BoundsRect) then
            begin
              // Control is not this one, it is a custom shape, that can process
              // mouse messages (eg not a connector), and is completely covered by
              // this control. So bring the convered control to the top of the z-order
              // so that we can access it.
              Control.BringToFront;
              Exit;
            end;
        end;
    end;
end;

procedure TcaMoveableShape.Move(DeltaX, DeltaY: Integer);
begin 
  SetBounds(Left + DeltaX, Top + DeltaY, Width, Height);
end; 

procedure TcaMoveableShape.MoveShapes(DeltaX, DeltaY: Integer);
var
  Control: TControl;
  Index: Integer;
  Pass: Integer;
begin
  // Do 2 passes through controls. The first one is to check that all
  // movements are valid
  for Pass := 1 to 2 do
    begin
      for Index := 0 to Parent.ControlCount - 1 do
        begin
          Control := Parent.Controls[Index];
          if Control is TcaMoveableShape then
            begin
              if (Pass = 1) and (not TcaMoveableShape(Control).IsValidMove(DeltaX, DeltaY)) then
                Exit
              else
                if (Pass = 2) and TcaMoveableShape(Control).Selected then
                  TcaMoveableShape(Control).Move(DeltaX, DeltaY);
            end;
        end;
    end;
end;

procedure TcaMoveableShape.StartMove(X, Y: Integer);
begin
  SetSelected(True);
  FMoving := True;
  FOrigin := Point(X, Y);
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaSizeableShape                                                          
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaSizeableShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSizingMode := smNone;
  FSizeOrigin := Point(0, 0);
  FSizeRectHeight := 5;
  FSizeRectWidth := 5;
  FMinHeight := FSizeRectHeight;
  FMinWidth := FSizeRectWidth;
end;

  // Public methods 

procedure TcaSizeableShape.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  // The control must be at least as large as a sizing rectangle 
  gShapeUtils.NoLessThan(ALeft, 0);
  gShapeUtils.NoLessThan(ATop, 0);
  gShapeUtils.NoLessThan(AWidth, FMinWidth);
  gShapeUtils.NoLessThan(AHeight, FMinHeight);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

  // Protected methods 

function TcaSizeableShape.GetSizeRect(SizeRectType: TcaSizingMode): TRect;
begin
  case SizeRectType of
    smTopLeft: Result := Bounds(0, 0, SizeRectWidth, SizeRectHeight);
    smTop: Result := Bounds(((ClientRect.Right - ClientRect.Left) div 2) -
        (SizeRectWidth div 2),
        0,
        SizeRectWidth, SizeRectHeight);
    smTopRight: Result := Bounds(ClientRect.Right - SizeRectWidth, 0,
        SizeRectWidth, SizeRectHeight);
    smLeft: Result := Bounds(0,
        ((ClientRect.Bottom - ClientRect.Top) div 2) -
        (SizeRectHeight div 2),
        SizeRectWidth, SizeRectHeight);
    smRight: Result := Bounds(ClientRect.Right - SizeRectWidth,
        ((ClientRect.Bottom - ClientRect.Top) div 2) -
        (SizeRectHeight div 2),
        SizeRectWidth, SizeRectHeight);
    smBottomLeft: Result := Bounds(0, ClientRect.Bottom - SizeRectHeight,
        SizeRectWidth, SizeRectHeight);
    smBottom: Result := Bounds(((ClientRect.Right - ClientRect.Left) div 2) -
        (SizeRectWidth div 2),
        ClientRect.Bottom - SizeRectHeight,
        SizeRectWidth, SizeRectHeight);
    smBottomRight: Result := Bounds(ClientRect.Right - SizeRectWidth,
        ClientRect.Bottom - SizeRectHeight,
        SizeRectWidth, SizeRectHeight);
    smNone: Result := Bounds(0, 0, 0, 0);
  end;
end;

procedure TcaSizeableShape.CheckForSizeRects(X, Y: Integer);
var
  SMode: TcaSizingMode;
begin 
  FSizingMode := smNone;
  if Selected then
    begin
      for SMode := smTopLeft to smBottomRight do
        begin
          if gShapeUtils.InRect(X, Y, GetSizeRect(SMode)) then
            begin
              FSizingMode := SMode;
              Break;
            end;
        end;
      case FSizingMode of
        smTopLeft:      Cursor := crSizeNWSE;
        smTop:          Cursor := crSizeNS;
        smTopRight:     Cursor := crSizeNESW;
        smLeft:         Cursor := crSizeWE;
        smRight:        Cursor := crSizeWE;
        smBottomLeft:   Cursor := crSizeNESW;
        smBottom:       Cursor := crSizeNS;
        smBottomRight:  Cursor := crSizeNWSE;
      else
        Cursor :=       crDefault;
      end;
    end;
end;

procedure TcaSizeableShape.DrawSizingRects;
var
  OldBrush: TBrush;
  SMode: TcaSizingMode;
begin 
  if FSelected then
    begin
      // Draw the sizing rectangles 
      OldBrush := Auto(TBrush.Create).Instance;
      OldBrush.Assign(Canvas.Brush);
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := clBlack;
      Canvas.Pen.Color := clBlack;
      for SMode := smTopLeft to smBottomRight do
        Canvas.FillRect(GetSizeRect(SMode));
      Canvas.Brush.Assign(OldBrush);
    end;
end;

procedure TcaSizeableShape.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin 
  if (FSizingMode = smNone)
  or (Button <> mbLeft)
  or (ssShift in Shift) then
    begin
      FSizingMode := smNone;
      inherited MouseDown(Button, Shift, X, Y);
    end
  else
    begin
      // If sizing then make this the only selected control
      gShapeManager.UnselectAllShapes(Parent);
      BringToFront;
      FSelected := True;
      FSizeOrigin := Point(X, Y);
    end;
end;

procedure TcaSizeableShape.MouseMove(Shift: TShiftState; X, Y: Integer);
begin 
  if Moving then
    inherited MouseMove(Shift, X, Y)
  else
    begin
      if (FSizingMode <> smNone) and (ssLeft in Shift) then
        ResizeControl(X, Y)
      else
        CheckForSizeRects(X, Y);
    end;
end;

procedure TcaSizeableShape.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin 
  if Button = mbLeft then
    FSizingMode := smNone;
  inherited MouseUp(Button, Shift, X, Y);
end; 

procedure TcaSizeableShape.Paint;
begin 
  inherited Paint;
  DrawSizingRects;
end;

procedure TcaSizeableShape.ResizeControl(X, Y: Integer);
var
  L, T, W, H, DeltaX, DeltaY: Integer;
begin
  L := Left;
  T := Top;
  W := Width;
  H := Height;
  DeltaX := X - FSizeOrigin.X;
  DeltaY := Y - FSizeOrigin.Y;
  // Calculate the new boundaries on the control. Also change FSizeOrigin to
  // reflect change in boundaries if necessary.
  case FSizingMode of
    smTopLeft:
      begin
        // Ensure that don't move the left edge if this would make the
        // control too narrow
        if (L + DeltaX >= 0) and (W - DeltaX > MinWidth) then
          begin
            L := L + DeltaX;
            W := W - DeltaX;
          end;
        // Ensure that don't move the top edge if this would make the
        // control too short
        if (T + DeltaY >= 0) and (H - DeltaY > MinHeight) then
          begin
            T := T + DeltaY;
            H := H - DeltaY;
          end;
      end;
    smTop:
      begin
        if (T + DeltaY >= 0) and (H - DeltaY > MinHeight) then
          begin
            T := T + DeltaY;
            H := H - DeltaY;
          end;
      end;
    smTopRight:
      begin
        W := W + DeltaX;
        if (T + DeltaY >= 0) and (H - DeltaY > MinHeight) then
          begin
            T := T + DeltaY;
            H := H - DeltaY;
          end;
        FSizeOrigin.X := X;
      end;
    smLeft:
      begin
        if (L + DeltaX >= 0) and (W - DeltaX > MinWidth) then
          begin
            L := L + DeltaX;
            W := W - DeltaX;
          end;
      end;
    smRight:
      begin
        W := W + DeltaX;
        FSizeOrigin.X := X;
      end;
    smBottomLeft:
      begin
        if (L + DeltaX >= 0) and (W - DeltaX > MinWidth) then
          begin
            L := L + DeltaX;
            W := W - DeltaX;
          end;

        H := H + DeltaY;
        FSizeOrigin.Y := Y;
      end;
    smBottom:
      begin
        H := H + DeltaY;
        FSizeOrigin.X := X;
        FSizeOrigin.Y := Y;
      end;
    smBottomRight:
      begin
        W := W + DeltaX;
        H := H + DeltaY;
        FSizeOrigin.X := X;
        FSizeOrigin.Y := Y;
      end;
    smNone: ;
  end;
  SetBounds(L, T, W, H);
end; 

procedure TcaSizeableShape.SetSelected(Value: Boolean);
begin 
  if Value <> FSelected then
    begin
      inherited SetSelected(Value);
      Invalidate;
    end;
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaTextShape                                                              
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaTextShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutosize := True;
  FText := '';
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
end;

destructor TcaTextShape.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

  // Public methods 

procedure TcaTextShape.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  gShapeUtils.NoLessThan(AWidth, FMinWidth);
  gShapeUtils.NoLessThan(AHeight, FMinHeight);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

  // Protected methods 

procedure TcaTextShape.Paint;
var
  TempRect: TRect;
  Flags: Cardinal;
begin
  Canvas.Font := Font;
  TempRect := ClientRect;
  Flags := DT_CENTER or DT_NOPREFIX or DT_WORDBREAK;
  DrawText(Canvas.Handle, PChar(FText), Length(FText), TempRect, Flags);
  inherited Paint;
end;

procedure TcaTextShape.RefreshText;
var
  Index, Count: Integer;
  TempStr: string;
begin 
  FMinHeight := FSizeRectHeight;
  FMinWidth := FSizeRectWidth;
  TempStr := '';
  Count := 1;
  if FAutosize and Assigned(Parent) then
    begin
      Canvas.Font := Font;
      for Index := 1 to Length(FText) do
        begin
          if FText[Index] = #10 then
            begin
              FMinWidth := gShapeUtils.ArrayMax([FMinWidth, Canvas.TextWidth(TempStr)]);
              TempStr := '';
              Inc(Count);
            end
          else
            TempStr := TempStr + FText[Index];
        end;
      if Count = 1 then
        FMinWidth := gShapeUtils.ArrayMax([FMinWidth, Canvas.TextWidth(FText)]);
      // Calculate the height of the text rectangle
      FMinHeight := gShapeUtils.ArrayMax([FMinHeight, Canvas.TextHeight(FText) * Count]);
    end;
  SetBounds(Left, Top, FMinWidth, FMinHeight);
end; 

procedure TcaTextShape.SetParent(AParent: TWinControl);
begin 
  inherited SetParent(AParent);
  RefreshText;
end; 

  // Private methods 

procedure TcaTextShape.FontChanged(Sender: TObject);
begin
  RefreshText;
end;

  // Property methods 

procedure TcaTextShape.SetAutosize(Value: Boolean);
begin
  if FAutosize <> Value then
    begin
      FAutosize := Value;
      RefreshText;
    end;
end;

procedure TcaTextShape.SetFont(Value: TFont);
begin 
  FFont.Assign(Value);
end; 

procedure TcaTextShape.SetText(Value: string);
begin
  if FText <> Value then
    begin
      FText := Value;
      RefreshText;
    end;
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaBitmapShape                                                            
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaBitmapShape.Create(AOwner: TComponent);
begin 
  inherited Create(AOwner);
  FImages := nil;
  FImageIndex := 0;
end; 

  // Protected methods 

procedure TcaBitmapShape.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
end;

procedure TcaBitmapShape.Paint;
var
  OldPen: TPen;
begin 
  inherited Paint;
  if (Parent <> nil)
  and Assigned(FImages)
  and (FImageIndex >= 0)
  and (FImageIndex < FImages.Count) then
    begin
      OldPen := Canvas.Pen;
      Canvas.Pen.Style := psDot;
      Canvas.Brush.Style := bsClear;
      if Selected then
        Canvas.Pen.Mode := pmNot
      else
        Canvas.Pen.Mode := pmNop;
      Canvas.Polyline([Point(0, 0),
                       Point(Width - 1, 0),
                       Point(Width - 1, Height - 1),
                       Point(0, Height - 1),
                       Point(0, 0)]);
      Canvas.Pen := OldPen;
      // Draw the bitmap
      FImages.DrawingStyle := dsTransparent;
      FImages.Draw(Canvas, 0, 0, FImageIndex);
    end;
end;

procedure TcaBitmapShape.SetSelected(Value: Boolean);
begin 
  if Value <> Selected then
    begin
      inherited SetSelected(Value);
      Invalidate;
    end;
end; 

  // Property methods 

procedure TcaBitmapShape.SetImages(Value: TImageList);
begin 
  if Value <> FImages then
    begin
      FImages := Value;
      if FImages <> nil then
        SetBounds(Left, Top, FImages.Width, FImages.Height);
    end;
end; 

procedure TcaBitmapShape.SetImageIndex(Value: Integer);
begin 
  if Value <> FImageIndex then
    begin
      FImageIndex := Value;
      Invalidate;
    end;
end; 

  //```````````````````````````````````````````````````````````````````````````
  // TcaStandardShape                                                          
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaStandardShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShapeType := stRectangle;
  Width := 100;
  Height := 60;
  FLineColour := clBlack;
end;

  // Protected methods 

procedure TcaStandardShape.Paint;
var
  TempRect: TRect;
  SideLength: Integer;
begin
  inherited Paint;
  TempRect := ClientRect;
  InflateRect(TempRect, -SizeRectWidth, -SizeRectHeight);
  // Draw shape outline
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := FLineColour;
  SideLength := gShapeUtils.ArrayMin([TempRect.Right - TempRect.Left + 1, TempRect.Bottom - TempRect.Top + 1]);
  if FShapeType in [stSquare, stRoundSquare, stCircle] then
    begin
      TempRect.Right := TempRect.Left + SideLength;
      TempRect.Bottom := TempRect.Top + SideLength;
    end;
  case FShapeType of
    stRectangle, stSquare:
      Canvas.Rectangle(TempRect.Left, TempRect.Top, TempRect.Right, TempRect.Bottom);
    stRoundRect, stRoundSquare:
      Canvas.RoundRect(TempRect.Left, TempRect.Top, TempRect.Right, TempRect.Bottom, SideLength div 4, SideLength div 4);
    stCircle, stEllipse:
      Canvas.Ellipse(TempRect.Left, TempRect.Top, TempRect.Right, TempRect.Bottom);
  end;
end;

  // Property methods 

procedure TcaStandardShape.SetShapeType(Value: TShapeType);
begin
  if FShapeType <> Value then
    begin
      FShapeType := Value;
      Invalidate;
    end;
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaConnection                                                             
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaConnection.Create;
begin
  inherited Create;
  FSide := csRight;
end;

  // Public methods 

function TcaConnection.BottomMost(TerminatorRect: TRect): TPoint;
begin
  Result := TermPoint(TerminatorRect);
  if Assigned(FShape) then
    begin
      case FSide of
        csLeft, csRight:
          Result.Y := FShape.Top + FOffset + (gShapeUtils.RectHeight(TerminatorRect) div 2);
        csTop:
          Result.Y := FShape.Top - 1;
        csBottom:
          Result.Y := FShape.Top + FShape.Height + gShapeUtils.RectWidth(TerminatorRect);
      end;
    end;
end;

function TcaConnection.ConnPoint(TerminatorRect: TRect): TPoint;
var
  X, Y, W: Integer;
begin 
  Result := Point(0, 0);
  if Assigned(FShape) then
    begin
      X := 0;
      Y := 0;
      W := TerminatorRect.Right - TerminatorRect.Left;
      case FSide of
        csLeft:
          begin
            X := FShape.Left - W;
            Y := FShape.Top + FOffset;
          end;
        csRight:
          begin
            X := FShape.Left + FShape.Width - 1 + W;
            Y := FShape.Top + FOffset;
          end;
        csTop:
          begin
            X := FShape.Left + FOffset;
            Y := FShape.Top - W;
          end;
        csBottom:
          begin
            X := FShape.Left + FOffset;
            Y := FShape.Top + FShape.Height - 1 + W;
          end;
      end;
      Result := Point(X, Y);
    end;
end;

function TcaConnection.LeftMost(TerminatorRect: TRect): TPoint;
begin
  Result := TermPoint(TerminatorRect);
  if Assigned(FShape) then
    begin
      case FSide of
        csLeft:
          Result.X := FShape.Left - gShapeUtils.RectWidth(TerminatorRect);
        csRight:
          Result.X := FShape.Left + FShape.Width;
        csTop, csBottom:
          Result.X := FShape.Left + FOffset - (gShapeUtils.RectHeight(TerminatorRect) div 2);
      end;
    end;
end;

function TcaConnection.RightMost(TerminatorRect: TRect): TPoint;
begin 
  Result := TermPoint(TerminatorRect);
  if Assigned(FShape) then
    begin
      case FSide of
        csLeft:
          Result.X := FShape.Left - 1;
        csRight:
          Result.X := FShape.Left + FShape.Width - 1 + gShapeUtils.RectWidth(TerminatorRect);
        csTop, csBottom:
          Result.X := FShape.Left + FOffset + (gShapeUtils.RectHeight(TerminatorRect) div 2);
      end;
    end;
end;

function TcaConnection.TermPoint(TerminatorRect: TRect): TPoint;
var
  X, Y: Integer;
begin 
  Result := Point(0, 0);
  if Assigned(FShape) then
    begin
      X := 0;
      Y := 0;
      case FSide of
        csLeft:
          begin
            X := FShape.Left;
            Y := FShape.Top + FOffset;
          end;
        csRight:
          begin
            X := FShape.Left + FShape.Width - 1;
            Y := FShape.Top + FOffset;
          end;
        csTop:
          begin
            X := FShape.Left + FOffset;
            Y := FShape.Top;
          end;
        csBottom:
          begin
            X := FShape.Left + FOffset;
            Y := FShape.Top + FShape.Height - 1;
          end;
      end;
      Result := Point(X, Y);
    end;
end;

function TcaConnection.TopMost(TerminatorRect: TRect): TPoint;
begin
  Result := TermPoint(TerminatorRect);
  if Assigned(FShape) then
    begin
      case FSide of
        csLeft, csRight:
          Result.Y := FShape.Top + FOffset - (gShapeUtils.RectHeight(TerminatorRect) div 2);
        csTop:
          Result.Y := FShape.Top - gShapeUtils.RectWidth(TerminatorRect) - 1;
        csBottom:
          Result.Y := FShape.Top + FShape.Height;
      end;
    end;
end;

procedure TcaConnection.Assign(Source: TPersistent);
begin 
  if Source is TcaConnection then
    begin
      FShape := TcaConnection(Source).FShape;
      FSide := TcaConnection(Source).FSide;
      FOffset := TcaConnection(Source).FOffset;
    end
  else
    inherited Assign(Source);
end; 

  //```````````````````````````````````````````````````````````````````````````
  // TcaConnector                                                              
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaConnector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLineWidth := 1;
  FLineColour := clBlack;
  FStartConn := TcaConnection.Create;
  FEndConn := TcaConnection.Create;
end; 

destructor TcaConnector.Destroy;
begin 
  FStartConn.Free;
  FEndConn.Free;
  inherited Destroy;
end; 

    // Public methods 

procedure TcaConnector.SetBoundingRect;
var
  ALeft, ATop, AWidth, AHeight: Integer;
begin
  if (FStartConn.Shape <> nil) and (FEndConn.Shape <> nil) then
    begin
      ALeft := gShapeUtils.ArrayMin([FStartConn.LeftMost(FStartTermRect).X,
                                     FEndConn.LeftMost(FEndTermRect).X]);
      ATop := gShapeUtils.ArrayMin([FStartConn.TopMost(FStartTermRect).Y,
                                    FEndConn.TopMost(FEndTermRect).Y]);
      AWidth := gShapeUtils.ArrayMax([FStartConn.RightMost(FStartTermRect).X,
                                      FEndConn.RightMost(FEndTermRect).X]) - ALeft + 2;
      AHeight := gShapeUtils.ArrayMax([FStartConn.BottomMost(FStartTermRect).Y,
                                       FEndConn.BottomMost(FEndTermRect).Y]) - ATop + 2;
      CheckSize(AWidth, AHeight);
      Invalidate;
      UpdateBoundsRect(Rect(ALeft, ATop, ALeft + AWidth - 1, ATop + AHeight - 1));
      MoveCaption;
    end;
end;

procedure TcaConnector.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  CheckSize(AWidth, AHeight);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  MoveCaption;
end;

procedure TcaConnector.SetConnections(AStartConn, AEndConn: TcaConnection);
begin
  SetConn(1, AStartConn);
  SetConn(2, AEndConn);
end; 

  // Protected methods 

function TcaConnector.Convert(APoint: TPoint): TPoint;
begin 
  Result := ScreenToClient(Parent.ClientToScreen(APoint));
end; 

function TcaConnector.IsConnected(ConnectedShape: TcaCustomShape): Boolean;
begin 
  Result := (FStartConn.Shape = ConnectedShape) or (FEndConn.Shape = ConnectedShape);
end; 

procedure TcaConnector.DrawEndTerminator;
begin 
  // Virtual 
end;

procedure TcaConnector.DrawStartTerminator;
begin
  // Virtual 
end;

procedure TcaConnector.MoveCaption;
var
  NewMidPoint: TPoint;
  ALeft, ATop, ARight, ABottom: Integer;
begin 
  if Assigned(FCaption) then
    begin
      if (FMidPoint.X = 0) and (FMidPoint.Y = 0) then
        FMidPoint := GetMidPoint;
      NewMidPoint := GetMidPoint;
      // Move the caption relative to the mid point of the connector
      // Not resizing anything, just moving an unconnected shape, so can use
      // faster update method than SetBounds
      FCaption.Invalidate;
      ALeft := FCaption.Left + NewMidPoint.X - FMidPoint.X;
      ATop := FCaption.Top + NewMidPoint.Y - FMidPoint.Y;
      ARight := ALeft + FCaption.Width;
      ABottom := ATop + FCaption.Height;
      FCaption.UpdateBoundsRect(Rect(ALeft, ATop, ARight, ABottom));
      // Save the new mid point
      FMidPoint := NewMidPoint;
    end;
end;

procedure TcaConnector.Notification(AComponent: TComponent; Operation: TOperation);
begin 
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FStartConn.Shape) then
    FStartConn.Shape := nil;
  if (Operation = opRemove) and (AComponent = FEndConn.Shape) then
    FEndConn.Shape := nil;
end;

procedure TcaConnector.Paint;
var
  EndPt: TPoint;
begin 
  inherited Paint;
  if (FStartConn.Shape <> nil) and (FEndConn.Shape <> nil) then
    begin
      DrawStartTerminator;
      DrawEndTerminator;
      // Draw the connecting line
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Width := FLineWidth;
      Canvas.Pen.Color := FLineColour;
      // Convert from Parent coordinates to control coordinates
      Canvas.PenPos := Convert(FStartConn.ConnPoint(FStartTermRect));
      EndPt := Convert(FEndConn.ConnPoint(FEndTermRect));
      Canvas.LineTo(EndPt.X, EndPt.Y);
    end;
end; 

procedure TcaConnector.SetCaption(Value: TcaTextShape);
begin 
  inherited SetCaption(Value);
  MoveCaption;
end; 

  // Private methods 

procedure TcaConnector.CheckSize(var AWidth, AHeight: Integer);
begin
  // Ensure the control is at least as big as the line width
  gShapeUtils.NoLessThan(AHeight, FLineWidth);
  gShapeUtils.NoLessThan(AWidth, FLineWidth);
  // Ensure the control is at least as big as the start terminator rectangle
  gShapeUtils.NoLessThan(AHeight, gShapeUtils.RectHeight(FStartTermRect));
  gShapeUtils.NoLessThan(AWidth, gShapeUtils.RectWidth(FStartTermRect));
  // Ensure the control is at least as big as the end terminator rectangle
  gShapeUtils.NoLessThan(AHeight, gShapeUtils.RectHeight(FEndTermRect));
  gShapeUtils.NoLessThan(AWidth, gShapeUtils.RectWidth(FEndTermRect));
end;

  // Property methods 

function TcaConnector.GetConn(Index: Integer): TcaConnection;
begin
  Result := nil;
  case Index of
    1: Result := FStartConn;
    2: Result := FEndConn;
  end;
end;

function TcaConnector.GetMidPoint: TPoint;
var
  A, B: TPoint;
begin 
  Result := Point(0, 0);
  if Assigned(FStartConn) and Assigned(FEndConn) then
    begin
      A := FStartConn.ConnPoint(FStartTermRect);
      B := FEndConn.ConnPoint(FEndTermRect);
      Result := Point(gShapeUtils.ArrayMin([A.X, B.X]) + Abs(A.X - B.X) div 2,
                      gShapeUtils.ArrayMin([A.Y, B.Y]) + Abs(A.Y - B.Y) div 2);
    end;
end;

function TcaConnector.GetTermRect(Index: Integer): TRect;
begin 
  case Index of
    1: Result := FStartTermRect;
    2: Result := FEndTermRect;
  end;
end; 

procedure TcaConnector.SetConn(Index: Integer; Value: TcaConnection);
begin 
  case Index of
    1: FStartConn.Assign(Value);
    2: FEndConn.Assign(Value);
  end;
  SetBoundingRect;
end; 

procedure TcaConnector.SetLineWidth(Value: Integer);
begin 
  if Value >= 1 then
    FLineWidth := Value;
end; 

procedure TcaConnector.SetTermRect(Index: Integer; Value: TRect);
begin 
  if (Value.Right - Value.Left >= 0) and (Value.Bottom - Value.Top >= 0) then
    begin
      case Index of
        1: FStartTermRect := Value;
        2: FEndTermRect := Value;
      end;
    end;
end; 

  //```````````````````````````````````````````````````````````````````````````
  // TcaSingleHeadArrow                                                        
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaSingleHeadArrow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EndTermRect := Rect(0, 0, 25, 10);
end;

  // Protected methods 

procedure TcaSingleHeadArrow.DrawArrowHead(ConnPt, TermPt: TPoint);
var
  PointPt, Corner1Pt, Corner2Pt: TPoint;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := FLineColour;
  Canvas.Pen.Color := FLineColour;
  // Draw a line connecting the Conn and Term points
  Canvas.PenPos := ConnPt;
  Canvas.LineTo(TermPt.X, TermPt.Y);
  // Set the basic points (to be modified depending on arrow head direction
  PointPt := TermPt;
  Corner1Pt := ConnPt;
  Corner2Pt := ConnPt;
  if ConnPt.X < TermPt.X then
    begin
      // Draw a right pointing arrow head
      Inc(Corner1Pt.X, 10);
      Inc(Corner2Pt.X, 10);
      Dec(Corner1Pt.Y, gShapeUtils.RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.Y, gShapeUtils.RectHeight(EndTermRect) div 2);
    end else
  if ConnPt.X > TermPt.X then
    begin
      // Draw a left pointing arrow head
      Dec(Corner1Pt.X, 10);
      Dec(Corner2Pt.X, 10);
      Dec(Corner1Pt.Y, gShapeUtils.RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.Y, gShapeUtils.RectHeight(EndTermRect) div 2);
    end else
  if ConnPt.Y < TermPt.Y then
    begin
      // Draw a down pointing arrow head
      Inc(Corner1Pt.Y, 10);
      Inc(Corner2Pt.Y, 10);
      Dec(Corner1Pt.X, gShapeUtils.RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.X, gShapeUtils.RectHeight(EndTermRect) div 2);
    end
  else
    begin
      // Draw a up pointing arrow head
      Dec(Corner1Pt.Y, 10);
      Dec(Corner2Pt.Y, 10);
      Dec(Corner1Pt.X, gShapeUtils.RectHeight(EndTermRect) div 2);
      Inc(Corner2Pt.X, gShapeUtils.RectHeight(EndTermRect) div 2);
    end;
  Canvas.Polygon([PointPt, Corner1Pt, Corner2Pt]);
end;

procedure TcaSingleHeadArrow.DrawEndTerminator;
var
  ConnPt, TermPt: TPoint;
begin 
  inherited DrawEndTerminator;
  if FEndConn.Shape <> nil then
    begin
      ConnPt := Convert(FEndConn.ConnPoint(EndTermRect));
      TermPt := Convert(FEndConn.TermPoint(EndTermRect)); ;
      DrawArrowHead(ConnPt, TermPt);
    end;
end; 

  //```````````````````````````````````````````````````````````````````````````
  // TcaBluntSingleHeadArrow                                                   
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaBluntSingleHeadArrow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StartTermRect := Rect(0, 0, 10, 10);
end;

  // Protected methods 

procedure TcaBluntSingleHeadArrow.DrawStartTerminator;
var
  ConnPt: TPoint;
  TermPt: TPoint;
begin
  inherited DrawStartTerminator;
  if FStartConn.Shape <> nil then
    begin
      ConnPt := Convert(FStartConn.ConnPoint(StartTermRect));
      TermPt := Convert(FStartConn.TermPoint(StartTermRect)); ;
      // Draw a line connecting the Conn and Term points
      Canvas.Pen.Color := FLineColour;
      Canvas.PenPos := ConnPt;
      Canvas.LineTo(TermPt.X, TermPt.Y);
    end;
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaDoubleHeadArrow                                                        
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaDoubleHeadArrow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StartTermRect := EndTermRect;
end;

  // Protected methods 

procedure TcaDoubleHeadArrow.DrawStartTerminator;
var
  ConnPt, TermPt: TPoint;
begin
  inherited DrawStartTerminator;
  if FStartConn.Shape <> nil then
    begin
      ConnPt := Convert(FStartConn.ConnPoint(StartTermRect));
      TermPt := Convert(FStartConn.TermPoint(StartTermRect)); ;
      DrawArrowHead(ConnPt, TermPt);
    end;
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaShapeUtils                                                             
  //```````````````````````````````````````````````````````````````````````````

  // Interface methods - IcaShapeUtils 

function TcaShapeUtils.ArrayMax(A: array of Integer): Integer;
var
  Index: Integer;
begin
  Result := 0;
  if High(A) >= 0 then
    begin
      Result := A[Low(A)];
      for Index := Succ(Low(A)) to High(A) do
        Result := Math.Max(Result, A[Index]);
    end;
end;

function TcaShapeUtils.ArrayMin(A: array of Integer): Integer;
var
  Index: Integer;
begin
  Result := 0;
  if High(A) >= 0 then
    begin
      Result := A[Low(A)];
      for Index := Succ(Low(A)) to High(A) do
        Result := Math.Min(Result, A[Index]);
    end;
end;

function TcaShapeUtils.InRect(X, Y: Integer; ARect: TRect): Boolean;
begin
  Result := (X >= ARect.Left) and (X <= ARect.Right) and
            (Y >= ARect.Top)  and (Y <= ARect.Bottom);
end;

function TcaShapeUtils.NextShapeSuffix: string;
begin
  Inc(FShapeIndex);
  Result := IntToStr(FShapeIndex);
end;

procedure TcaShapeUtils.NoLessThan(var Value: Integer; ALimit: Integer);
begin
  if Value < ALimit then Value := ALimit;
end;

function TcaShapeUtils.RectHeight(ARect: TRect): Integer;
begin
  Result := ARect.Bottom - ARect.Top;
end;

function TcaShapeUtils.RectWidth(ARect: TRect): Integer;
begin
  Result := ARect.Right - ARect.Left;
end;

  // Interface methods - IcaShapeManager 

procedure TcaShapeUtils.DeleteAllShapes(ParentControl: TWinControl);
var
  Index: Integer;
begin 
  Index := 0;
  while Index < ParentControl.ControlCount do
    begin
      if ParentControl.Controls[Index] is TcaCustomShape then
        ParentControl.Controls[Index].Free
      else
        Inc(Index);
    end;
end;

procedure TcaShapeUtils.DeleteSelectedShapes(ParentControl: TWinControl);
var
  Index: Integer;
begin
  Index := 0;
  while Index < ParentControl.ControlCount do
    begin
      if (ParentControl.Controls[Index] is TcaCustomShape)
      and (TcaCustomShape(ParentControl.Controls[Index]).Selected) then
        ParentControl.Controls[Index].Free
      else
        Inc(Index);
    end;
end;

procedure TcaShapeUtils.LoadFromFile(const FileName: string; ParentControl: TWinControl);
var
  Stream: TFileStream;
  Reader: TReader;
  RealName: string;
begin
  DeleteAllShapes(ParentControl);
  Stream := Auto(TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite)).Instance;
  Reader := Auto(TReader.Create(Stream, 1024)).Instance;
  // Save the parent's name, in case we are reading into a different
  // control than we saved the diagram from
  RealName := ParentControl.Name;
  Reader.Root := ParentControl.Owner;
  Reader.BeginReferences;
  Reader.ReadComponent(ParentControl);
  Reader.FixupReferences;
  // Restore the parent's name
  ParentControl.Name := RealName;
  Reader.EndReferences;
end;

procedure TcaShapeUtils.SaveToFile(const FileName: string; ParentControl: TWinControl);
var
  Stream: TFileStream;
  Writer: TWriter;
  RealName: string;
begin 
  Stream := Auto(TFileStream.Create(Filename, fmCreate or fmShareDenyWrite)).Instance;
  Writer := Auto(TWriter.Create(Stream, 1024)).Instance;
  Writer.Root := ParentControl.Owner;
  RealName := ParentControl.Name;
  ParentControl.Name := '';
  Writer.WriteComponent(ParentControl);
  ParentControl.Name := RealName;
end;

procedure TcaShapeUtils.UnselectAllShapes(ParentControl: TWinControl);
var
  Index: Integer;
begin
  for Index := 0 to ParentControl.ControlCount - 1 do
    if ParentControl.Controls[Index] is TcaCustomShape then
      TcaCustomShape(ParentControl.Controls[Index]).Selected := False;
end;

  //```````````````````````````````````````````````````````````````````````````
  // CoShapeUtilsFactory                                                       
  //```````````````````````````````````````````````````````````````````````````

class function CoShapeUtilsFactory.Instance: IcaShapeUtils;
const
  FInstance: IcaShapeUtils = nil;
begin
  if not Assigned(FInstance) then
    FInstance := TcaShapeUtils.Create;
  Result := FInstance;
end;

  //```````````````````````````````````````````````````````````````````````````
  // Initialization/Finalization                                               
  //```````````````````````````````````````````````````````````````````````````

procedure RegisterStorageClasses;
begin
  RegisterClasses([
    TcaCustomShape,
    TcaMoveableShape,
    TcaSizeableShape,
    TcaConnection,
    TcaConnector,
    TcaSingleHeadArrow,
    TcaBluntSingleHeadArrow,
    TcaDoubleHeadArrow,
    TcaBitmapShape,
    TcaTextShape,
    TcaStandardShape
  ]);
end;

initialization
  gShapeUtils := CoShapeUtilsFactory.Instance;
  gShapeManager := gShapeUtils as IcaShapeManager;
  RegisterStorageClasses;

end.

