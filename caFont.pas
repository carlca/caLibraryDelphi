unit caFont;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Windows,
  SysUtils,
  Classes,
  Controls,
  Graphics,
  Forms,
  TypInfo,

  // ca Units
  caClasses,
  caUtils,
  caVector;

type

  TcaUpdateControlFontEvent = procedure(Sender: TObject; AControl: TControl; AFont: TFont; var Handled: Boolean) of object;

  //```````````````````````````````````````````````````````````````````````````
  // TcaFontController                                                         
  //```````````````````````````````````````````````````````````````````````````

  TcaFontController = class(TComponent)
  private
    FFont: TFont;
    FRunTimeOnly: Boolean;
    FOnUpdateControlFont: TcaUpdateControlFontEvent;
    // Property methods
    function GetFont: TFont;
    function GetRunTimeOnly: Boolean;
    procedure SetFont(const Value: TFont);
    procedure SetRunTimeOnly(const Value: Boolean);
    // Event handlers
    procedure FontChangeEvent(Sender: TObject);
    // Private methods
    function CanUpdateControls: Boolean;
    procedure UpdateControls;
  protected
    // Virtual protected methods
    procedure DoUpdateControlFont(AControl: TControl; AFont: TFont; var Handled: Boolean); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // Properties
    property Font: TFont read GetFont write SetFont;
    property RunTimeOnly: Boolean read GetRunTimeOnly write SetRunTimeOnly;
    // Events
    property OnUpdateControlFont: TcaUpdateControlFontEvent read FOnUpdateControlFont write FOnUpdateControlFont;
  end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaFontSizes                                                              
  //```````````````````````````````````````````````````````````````````````````

  IcaFontSizes = interface
  ['{2AD79875-DC3C-43BC-8BBF-6AF468D87B42}']
    // Property methods 
    function GetAsString: string;
    // Public methods 
    procedure SetStrings(ASizeStrings: TStrings);
    // Properties 
    property AsString: string read GetAsString;
  end;

  TcaFontSizes = class(TInterfacedObject, IcaFontSizes)
  private
    // Private fields 
    FSizes: TcaIntegerVector;
    // Property methods 
    function GetAsString: string;
    // Private methods 
    procedure AddDefaultSizes;
    procedure CreateObjects;
    procedure FreeObjects;
  public
    // Create/Destroy 
    constructor Create; overload;
    constructor Create(ASizes: array of Integer); overload;
    destructor Destroy; override;
    // Public methods 
    procedure SetStrings(ASizeStrings: TStrings);
    // Properties 
    property AsString: string read GetAsString;
  end;

implementation

  //```````````````````````````````````````````````````````````````````````````
  // TcaFontController                                                         
  //```````````````````````````````````````````````````````````````````````````

constructor TcaFontController.Create(AOwner: TComponent);
begin
  inherited;
  FFont := TFont.Create;
  FFont.OnChange := FontChangeEvent;
end;

destructor TcaFontController.Destroy;
begin
  FFont.Free;
  inherited;
end;

 // Protected methods

procedure TcaFontController.DoUpdateControlFont(AControl: TControl; AFont: TFont; var Handled: Boolean);
begin
  if Assigned(FOnUpdateControlFont) then
    FOnUpdateControlFont(Self, AControl, AFont, Handled);
end;

procedure TcaFontController.Loaded;
begin
  inherited;
  if CanUpdateControls then UpdateControls;
end;

 // Private methods

function TcaFontController.CanUpdateControls: Boolean;
var
  CompState: IcaComponentState;
begin
  CompState := TcaComponentState.Create(Owner);
  Result := (not FRunTimeOnly) or (FRunTimeOnly and CompState.IsRunTime);
end;

procedure TcaFontController.UpdateControls;
var
  Control: TControl;
  Index: Integer;
  OwnerControl: TControl;
  Handled: Boolean;
  ControlFont: TFont;
begin
  if Owner <> nil then
    if Owner is TControl then
      begin
        ControlFont := TFont.Create;
        try
          OwnerControl := TControl(Owner);
          Handled := False;
          ControlFont.Assign(FFont);
          DoUpdateControlFont(OwnerControl, ControlFont, Handled);
          if not Handled then
            begin
              if IsPublishedProp(OwnerControl, 'Font') then
                SetObjectProp(OwnerControl, 'Font', ControlFont);
            end;
          for Index := 0 to OwnerControl.ComponentCount - 1 do
            begin
              if OwnerControl.Components[Index] is TControl then
                begin
                  Control := TControl(OwnerControl.Components[Index]);
                  Handled := False;
                  ControlFont.Assign(FFont);
                  DoUpdateControlFont(Control, ControlFont, Handled);
                  if not Handled then
                    begin
                      if IsPublishedProp(Control, 'Font') then
                        SetObjectProp(Control, 'Font', ControlFont);
                    end;
                end;
            end;
        finally
          ControlFont.Free;
        end;
      end;
end;

 // Event handlers

procedure TcaFontController.FontChangeEvent(Sender: TObject);
begin
  if CanUpdateControls then UpdateControls;
end;

 // Property methods

function TcaFontController.GetFont: TFont;
begin
  Result := FFont;
end;

function TcaFontController.GetRunTimeOnly: Boolean;
begin
  Result := FRunTimeOnly;
end;

procedure TcaFontController.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  if CanUpdateControls then UpdateControls;
end;

procedure TcaFontController.SetRunTimeOnly(const Value: Boolean);
begin
  FRunTimeOnly := Value;
end;

  //```````````````````````````````````````````````````````````````````````````
  // TcaFontSizes                                                              
  //```````````````````````````````````````````````````````````````````````````

  // Create/Destroy 

constructor TcaFontSizes.Create;
begin
  inherited;
  CreateObjects;
  AddDefaultSizes;
end;

constructor TcaFontSizes.Create(ASizes: array of Integer);
begin
  inherited Create;
  CreateObjects;
  FSizes.AddArray(ASizes);
end;

destructor TcaFontSizes.Destroy;
begin
  FreeObjects;
  inherited;
end;

  // Public methods 

procedure TcaFontSizes.SetStrings(ASizeStrings: TStrings);
var
  Index: Integer;
begin
  ASizeStrings.Clear;
  for Index := 0 to Pred(FSizes.Count) do
    ASizeStrings.Add(IntToStr(FSizes[Index]));
end;

  // Private methods 

procedure TcaFontSizes.AddDefaultSizes;
begin
  FSizes.AddArray([6, 8, 9, 10, 11, 12, 14, 16, 18, 20, 22, 24, 26, 28, 36, 48, 72]);
end;

procedure TcaFontSizes.CreateObjects;
begin
  FSizes := TcaIntegerVector.Create;
end;

procedure TcaFontSizes.FreeObjects;
begin
  FSizes.Free;
end;

  // Property methods 

function TcaFontSizes.GetAsString: string;
var
  Strings: TStrings;
begin
  Strings := Auto(TStringList.Create).Instance;
  Result := Strings.Text;
end;

end.
