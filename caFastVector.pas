unit caFastVector;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  // System 
  Classes,
  Sysutils,
  Math,

  // ca units 
  caClasses,
  caTypes,
  caUtils;

type

  //---------------------------------------------------------------------------
  // IcaFastVector                                                             
  //---------------------------------------------------------------------------

  IcaFastVector = interface
  ['{316C0E0E-0120-46EC-90DB-491452664F8E}']
    // Property methods 
    function GetArrayPointer: Pointer;
    function GetCount: Integer;
    procedure SetArrayPointer(const Value: Pointer);
    procedure SetCount(const Value: Integer);
    // Protected Properties 
    property ArrayPointer: Pointer read GetArrayPointer write SetArrayPointer;
    // Public methods 
    function LastIndex: Integer;
    procedure Clear;
    // Public properties 
    property Count: Integer read GetCount write SetCount;

  end;

  //---------------------------------------------------------------------------
  // TcaFastVector                                                             
  //---------------------------------------------------------------------------

  TcaFastVector = class(TcaInterfacedPersistent, IcaFastVector)
  private
    FArrayPointer: Pointer;
    FCount: Integer;
    // Property methods 
    function GetArrayPointer: Pointer;
    function GetCount: Integer;
    procedure SetArrayPointer(const Value: Pointer);
    procedure SetCount(const Value: Integer);
  protected
    // Protected virtual methods 
    function GetElementSize: Integer; virtual; abstract;
    procedure DoAssignData(SourcePointer: Pointer); virtual; abstract;
    procedure DoSetArrayLength; virtual; abstract;
    // Protected static methods 
    procedure Grow(ADelta: Integer = 1);
    // Protected Properties 
    property ArrayPointer: Pointer read GetArrayPointer write SetArrayPointer;
  public
    // Create/Destroy 
    constructor Create(ACount: Integer = 0);
    // Public methods 
    function LastIndex: Integer;
    procedure AssignFromInterface(Source: IUnknown); override;
    procedure AssignFromObject(Source: TPersistent); override;
    procedure Clear;
    // Properties 
    property Count: Integer read GetCount write SetCount;
  end;

  //---------------------------------------------------------------------------
  // IcaFastPointerVector                                                      
  //---------------------------------------------------------------------------

  IcaFastPointerVector = interface(IcaFastVector)
  ['{55D35C10-C5CB-4029-A8DD-3529186926C2}']
    // Property methods 
    function GetItem(Index: Integer): Pointer;
    procedure SetItem(Index: Integer; const Value: Pointer);
    // Interface methods 
    procedure Add(AValue: Pointer);
    // Properties 
    property Items[Index: Integer]: Pointer read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // TcaFastPointerVector                                                      
  //---------------------------------------------------------------------------

  TcaFastPointerVector = class(TcaFastVector, IcaFastPointerVector)
  private
    FArray: TcaPointerArray;
    // Property methods 
    function GetItem(Index: Integer): Pointer;
    procedure SetItem(Index: Integer; const Value: Pointer);
  protected
    // Protected methods 
    function GetElementSize: Integer; override;
    procedure DoAssignData(SourcePointer: Pointer); override;
    procedure DoSetArrayLength; override;
  public
    // Interface methods - IcaFastPointerVector 
    procedure Add(AValue: Pointer);
    // Properties 
    property Items[Index: Integer]: Pointer read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaFastObjectVector                                                       
  //---------------------------------------------------------------------------

  IcaFastObjectVector = interface(IcaFastVector)
  ['{9AE1363B-F6E6-460A-87A0-BB2909D8A9B7}']
    // Property methods 
    function GetItem(Index: Integer): TObject;
    procedure SetItem(Index: Integer; const Value: TObject);
    // Interface methods 
    procedure Add(AValue: TObject);
    // Properties 
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // TcaFastObjectVector                                                       
  //---------------------------------------------------------------------------

  TcaFastObjectVector = class(TcaFastVector, IcaFastObjectVector)
  private
    FArray: TcaObjectArray;
    // Property methods 
    function GetItem(Index: Integer): TObject;
    procedure SetItem(Index: Integer; const Value: TObject);
  protected
    // Protected methods 
    function GetElementSize: Integer; override;
    procedure DoAssignData(SourcePointer: Pointer); override;
    procedure DoSetArrayLength; override;
  public
    // Interface methods - IcaFastPointerVector 
    procedure Add(AValue: TObject);
    // Properties 
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaFastIntegerVector                                                      
  //---------------------------------------------------------------------------

  IcaFastIntegerVector = interface(IcaFastVector)
  ['{511DE8E4-C3DF-425D-BB84-BCE0BCB2A4A2}']
    // Property methods 
    function GetItem(Index: Integer): Integer;
    procedure SetItem(Index: Integer; const Value: Integer);
    // Interface methods 
    procedure Add(AValue: Integer);
    // Properties 
    property Items[Index: Integer]: Integer read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // TcaFastIntegerVector                                                      
  //---------------------------------------------------------------------------

  TcaFastIntegerVector = class(TcaFastVector, IcaFastIntegerVector)
  private
    FArray: TcaIntegerArray;
    // Property methods 
    function GetItem(Index: Integer): Integer;
    procedure SetItem(Index: Integer; const Value: Integer);
  protected
    // Protected methods 
    function GetElementSize: Integer; override;
    procedure DoAssignData(SourcePointer: Pointer); override;
    procedure DoSetArrayLength; override;
  public
    // Interface methods - IcaFastIntegerVector 
    procedure Add(AValue: Integer);
    // Properties 
    property Items[Index: Integer]: Integer read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaFastDoubleVector                                                       
  //---------------------------------------------------------------------------

  IcaFastDoubleVector = interface(IcaFastVector)
  ['{5768D4E8-BBB5-44DA-9B9D-45FEF8A2AEDA}']
    // Property methods 
    function GetItem(Index: Integer): Double;
    procedure SetItem(Index: Integer; const Value: Double);
    // Interface methods 
    procedure Add(AValue: Double);
    // Properties 
    property Items[Index: Integer]: Double read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // TcaFastDoubleVector                                                       
  //---------------------------------------------------------------------------

  TcaFastDoubleVector = class(TcaFastVector, IcaFastDoubleVector)
  private
    FArray: TcaDoubleArray;
    // Property methods 
    function GetItem(Index: Integer): Double;
    procedure SetItem(Index: Integer; const Value: Double);
  protected
    // Protected methods 
    function GetElementSize: Integer; override;
    procedure DoAssignData(SourcePointer: Pointer); override;
    procedure DoSetArrayLength; override;
  public
    // Interface methods - IcaFastDoubleVector 
    procedure Add(AValue: Double);
    // Properties 
    property Items[Index: Integer]: Double read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaFastExtendedVector                                                     
  //---------------------------------------------------------------------------

  IcaFastExtendedVector = interface(IcaFastVector)
  ['{5768D4E8-BBB5-44DA-9B9D-45FEF8A2AEDA}']
    // Property methods 
    function GetItem(Index: Integer): Extended;
    procedure SetItem(Index: Integer; const Value: Extended);
    // Interface methods 
    procedure Add(AValue: Extended);
    // Properties 
    property Items[Index: Integer]: Extended read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // TcaFastExtendedVector                                                     
  //---------------------------------------------------------------------------

  TcaFastExtendedVector = class(TcaFastVector, IcaFastExtendedVector)
  private
    FArray: TcaExtendedArray;
    // Property methods 
    function GetItem(Index: Integer): Extended;
    procedure SetItem(Index: Integer; const Value: Extended);
  protected
    // Protected methods 
    function GetElementSize: Integer; override;
    procedure DoAssignData(SourcePointer: Pointer); override;
    procedure DoSetArrayLength; override;
  public
    // Interface methods - IcaFastExtendedVector 
    procedure Add(AValue: Extended);
    // Properties 
    property Items[Index: Integer]: Extended read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaFastMatrix                                                             
  //---------------------------------------------------------------------------

  IcaFastMatrix = interface
  ['{A1F6FCD1-CDC4-41D6-8254-54B28DCB96FF}']
    // Property methods 
    function GetColCount: Integer;
    function GetRowCount: Integer;
    procedure SetColCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    // Properties 
    property ColCount: Integer read GetColCount write SetColCount;
    property RowCount: Integer read GetRowCount write SetRowCount;
  end;

  //---------------------------------------------------------------------------
  // TcaFastMatrix                                                             
  //---------------------------------------------------------------------------

  TcaFastMatrix = class(TInterfacedObject)
  private
    // Private fields 
    FColCount: Integer;
    FRowCount: Integer;
  protected
    // Property methods 
    function GetColCount: Integer;
    function GetRowCount: Integer;
    procedure SetColCount(const Value: Integer);
    procedure SetRowCount(const Value: Integer);
    // Virtual abstract methods 
    procedure UpdateArrayDimensions; virtual; abstract;    
  public
    // Create/Destroy 
    constructor Create(AColCount, ARowCount: Integer);
    // Properties 
    property ColCount: Integer read GetColCount write SetColCount;
    property RowCount: Integer read GetRowCount write SetRowCount;
  end;

  //---------------------------------------------------------------------------
  // IcaFastIntegerMatrix                                                      
  //---------------------------------------------------------------------------

  IcaFastIntegerMatrix = interface(IcaFastMatrix)
  ['{F38AA99E-CFCC-44D8-89B8-4F79E53C3B9C}']
    // Property methods 
    function GetItem(ACol, ARow: Integer): Integer;
    procedure SetItem(ACol, ARow: Integer; const Value: Integer);
    // Properties 
    property Items[ACol, ARow: Integer]: Integer read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // TcaFastIntegerMatrix                                                      
  //---------------------------------------------------------------------------

  TcaFastIntegerMatrix = class(TcaFastMatrix, IcaFastIntegerMatrix)
  private
    // Private fields 
    FArray: TcaIntegerArray2;
    // Property methods 
    function GetItem(ACol, ARow: Integer): Integer;
    procedure SetItem(ACol, ARow: Integer; const Value: Integer);
  protected
    // Protected virtual methods 
    procedure UpdateArrayDimensions; override;
  public
    // Properties 
    property Items[ACol, ARow: Integer]: Integer read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaFastDoubleMatrix                                                       
  //---------------------------------------------------------------------------

  IcaFastDoubleMatrix = interface(IcaFastMatrix)
  ['{16DD8AEA-E3DE-4668-A283-1BDFD17EA975}']
    // Property methods 
    function GetItem(ACol, ARow: Integer): Double;
    procedure SetItem(ACol, ARow: Integer; const Value: Double);
    // Properties 
    property Items[ACol, ARow: Integer]: Double read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // TcaFastDoubleMatrix                                                       
  //---------------------------------------------------------------------------

  TcaFastDoubleMatrix = class(TcaFastMatrix, IcaFastDoubleMatrix)
  private
    FArray: TcaDoubleArray2;
    // Property methods 
    function GetItem(ACol, ARow: Integer): Double;
    procedure SetItem(ACol, ARow: Integer; const Value: Double);
  protected
    // Protected virtual methods 
    procedure UpdateArrayDimensions; override;
  public
    // Properties 
    property Items[ACol, ARow: Integer]: Double read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaFastExtendedMatrix                                                     
  //---------------------------------------------------------------------------

  IcaFastExtendedMatrix = interface(IcaFastMatrix)
  ['{16DD8AEA-E3DE-4668-A283-1BDFD17EA975}']
    // Property methods 
    function GetItem(ACol, ARow: Integer): Extended;
    procedure SetItem(ACol, ARow: Integer; const Value: Extended);
    // Properties 
    property Items[ACol, ARow: Integer]: Extended read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // TcaFastExtendedMatrix                                                     
  //---------------------------------------------------------------------------

  TcaFastExtendedMatrix = class(TcaFastMatrix, IcaFastExtendedMatrix)
  private
    FArray: TcaExtendedArray2;
    // Property methods 
    function GetItem(ACol, ARow: Integer): Extended;
    procedure SetItem(ACol, ARow: Integer; const Value: Extended);
  protected
    // Protected virtual methods 
    procedure UpdateArrayDimensions; override;
  public
    // Properties 
    property Items[ACol, ARow: Integer]: Extended read GetItem write SetItem; default;
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaFastVector                                                             
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaFastVector.Create(ACount: Integer = 0);
begin
  inherited Create;
  SetCount(ACount);
end;

  // Public methods 

function TcaFastVector.LastIndex: Integer;
begin
  Result := FCount - 1;
end;

procedure TcaFastVector.AssignFromInterface(Source: IUnknown);
var
  SourceVector: IcaFastVector;
begin
  if Supports(Source, IcaFastVector, SourceVector) then
    begin
      SetCount(SourceVector.Count);
      DoAssignData(SourceVector.ArrayPointer);
    end;
end;

procedure TcaFastVector.AssignFromObject(Source: TPersistent);
var
  SourceVector: TcaFastVector;
begin
  if Source is TcaFastVector then
    begin
      SourceVector := TcaFastVector(Source);
      SetCount(SourceVector.Count);
      DoAssignData(SourceVector.ArrayPointer);
    end;
end;

procedure TcaFastVector.Clear;
begin
  SetCount(0);
end;

  // Protected static methods 

procedure TcaFastVector.Grow(ADelta: Integer = 1);
begin
  SetCount(FCount + ADelta);
end;

  // Property methods 

function TcaFastVector.GetArrayPointer: Pointer;
begin
  Result := FArrayPointer;
end;

function TcaFastVector.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TcaFastVector.SetArrayPointer(const Value: Pointer);
begin
  FArrayPointer := Value;
end;

procedure TcaFastVector.SetCount(const Value: Integer);
begin
  FCount := Value;
  DoSetArrayLength;
end;

  //---------------------------------------------------------------------------
  // TcaFastPointerVector                                                      
  //---------------------------------------------------------------------------

  // Interface methods - IcaFastPointerVector 

procedure TcaFastPointerVector.Add(AValue: Pointer);
begin
  Grow;
  SetItem(LastIndex, AValue);  
end;

  // Protected methods 

function TcaFastPointerVector.GetElementSize: Integer;
begin
  Result := SizeOf(Pointer);
end;

procedure TcaFastPointerVector.DoAssignData(SourcePointer: Pointer);
var
  SourceArray: TcaPointerArray;
begin
  SourceArray := TcaPointerArray(SourcePointer^);
  FArray := SourceArray;
end;

procedure TcaFastPointerVector.DoSetArrayLength;
begin
  SetLength(FArray, Count);
  ArrayPointer := @FArray;
end;

  // Property methods 

function TcaFastPointerVector.GetItem(Index: Integer): Pointer;
begin
  Result := FArray[Index];
end;

procedure TcaFastPointerVector.SetItem(Index: Integer; const Value: Pointer);
begin
  FArray[Index] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaFastObjectVector                                                       
  //---------------------------------------------------------------------------

  // Interface methods - IcaFastPointerVector 

procedure TcaFastObjectVector.Add(AValue: TObject);
begin
  Grow;
  SetItem(LastIndex, AValue);  
end;

  // Protected methods 

function TcaFastObjectVector.GetElementSize: Integer;
begin
  Result := SizeOf(TObject);
end;

procedure TcaFastObjectVector.DoAssignData(SourcePointer: Pointer);
var
  SourceArray: TcaObjectArray;
begin
  SourceArray := TcaObjectArray(SourcePointer^);
  FArray := SourceArray;
end;

procedure TcaFastObjectVector.DoSetArrayLength;
begin
  SetLength(FArray, Count);
  ArrayPointer := @FArray;
end;

  // Property methods 

function TcaFastObjectVector.GetItem(Index: Integer): TObject;
begin
  Result := FArray[Index];
end;

procedure TcaFastObjectVector.SetItem(Index: Integer; const Value: TObject);
begin
  FArray[Index] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaFastIntegerVector                                                      
  //---------------------------------------------------------------------------

  // Interface methods - IcaFastIntegerVector 

procedure TcaFastIntegerVector.Add(AValue: Integer);
begin
  Grow;
  SetItem(LastIndex, AValue);  
end;

  // Protected methods 

function TcaFastIntegerVector.GetElementSize: Integer;
begin
  Result := SizeOf(Integer);
end;

procedure TcaFastIntegerVector.DoAssignData(SourcePointer: Pointer);
var
  SourceArray: TcaIntegerArray;
begin
  SourceArray := TcaIntegerArray(SourcePointer^);
  FArray := SourceArray;
end;

procedure TcaFastIntegerVector.DoSetArrayLength;
begin
  SetLength(FArray, Count);
  ArrayPointer := @FArray;
end;

  // Property methods 

function TcaFastIntegerVector.GetItem(Index: Integer): Integer;
begin
  Result := FArray[Index];
end;

procedure TcaFastIntegerVector.SetItem(Index: Integer; const Value: Integer);
begin
  FArray[Index] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaFastDoubleVector                                                       
  //---------------------------------------------------------------------------

  // Interface methods - IcaFastDoubleVector 

procedure TcaFastDoubleVector.Add(AValue: Double);
begin
  Grow;
  SetItem(LastIndex, AValue);  
end;

  // Protected methods 

procedure TcaFastDoubleVector.DoAssignData(SourcePointer: Pointer);
var
  SourceArray: TcaDoubleArray;
begin
  SourceArray := TcaDoubleArray(SourcePointer^);
  FArray := SourceArray;
end;

function TcaFastDoubleVector.GetElementSize: Integer;
begin
  Result := SizeOf(Double);
end;

procedure TcaFastDoubleVector.DoSetArrayLength;
begin
  SetLength(FArray, Count);
  ArrayPointer := @FArray;
end;

  // Property methods 

function TcaFastDoubleVector.GetItem(Index: Integer): Double;
begin
  Result := FArray[Index];
end;

procedure TcaFastDoubleVector.SetItem(Index: Integer; const Value: Double);
begin
  FArray[Index] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaFastExtendedVector                                                     
  //---------------------------------------------------------------------------

  // Interface methods - IcaFastExtendedVector 

procedure TcaFastExtendedVector.Add(AValue: Extended);
begin
  Grow;
  SetItem(LastIndex, AValue);  
end;

  // Protected methods 

procedure TcaFastExtendedVector.DoAssignData(SourcePointer: Pointer);
var
  SourceArray: TcaExtendedArray;
begin
  SourceArray := TcaExtendedArray(SourcePointer^);
  FArray := SourceArray;
end;

function TcaFastExtendedVector.GetElementSize: Integer;
begin
  Result := SizeOf(Extended);
end;

procedure TcaFastExtendedVector.DoSetArrayLength;
begin
  SetLength(FArray, Count);
  ArrayPointer := @FArray;
end;

  // Property methods 

function TcaFastExtendedVector.GetItem(Index: Integer): Extended;
begin
  Result := FArray[Index];
end;

procedure TcaFastExtendedVector.SetItem(Index: Integer; const Value: Extended);
begin
  FArray[Index] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaFastMatrix                                                             
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaFastMatrix.Create(AColCount, ARowCount: Integer);
begin
  inherited Create;
  FColCount := AColCount;
  FRowCount := ARowCount;
  UpdateArrayDimensions;
end;

  // Property methods 

function TcaFastMatrix.GetColCount: Integer;
begin
  Result := FColCount;
end;

function TcaFastMatrix.GetRowCount: Integer;
begin
  Result := FRowCount;
end;

procedure TcaFastMatrix.SetColCount(const Value: Integer);
begin
  FColCount := Value;
  UpdateArrayDimensions;
end;

procedure TcaFastMatrix.SetRowCount(const Value: Integer);
begin
  FRowCount := Value;
  UpdateArrayDimensions;
end;

  //---------------------------------------------------------------------------
  // TcaFastIntegerMatrix                                                      
  //---------------------------------------------------------------------------

  // Protected virtual methods 

procedure TcaFastIntegerMatrix.UpdateArrayDimensions;
begin
  SetLength(FArray, FColCount, FRowCount);
end;

  // Property methods 

function TcaFastIntegerMatrix.GetItem(ACol, ARow: Integer): Integer;
begin
  Result := FArray[ACol, ARow];
end;

procedure TcaFastIntegerMatrix.SetItem(ACol, ARow: Integer; const Value: Integer);
begin
  FArray[ACol, ARow] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaFastDoubleMatrix                                                       
  //---------------------------------------------------------------------------

  // Protected virtual methods 

procedure TcaFastDoubleMatrix.UpdateArrayDimensions;
begin
  SetLength(FArray, FColCount, FRowCount);
end;

  // Property methods 

function TcaFastDoubleMatrix.GetItem(ACol, ARow: Integer): Double;
begin
  Result := FArray[ACol, ARow];
end;

procedure TcaFastDoubleMatrix.SetItem(ACol, ARow: Integer; const Value: Double);
begin
  FArray[ACol, ARow] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaFastExtendedMatrix                                                     
  //---------------------------------------------------------------------------

  // Protected virtual methods 

procedure TcaFastExtendedMatrix.UpdateArrayDimensions;
begin
  SetLength(FArray, FColCount, FRowCount);
end;

  // Property methods 

function TcaFastExtendedMatrix.GetItem(ACol, ARow: Integer): Extended;
begin
  Result := FArray[ACol, ARow];
end;

procedure TcaFastExtendedMatrix.SetItem(ACol, ARow: Integer; const Value: Extended);
begin
  FArray[ACol, ARow] := Value;
end;

end.


