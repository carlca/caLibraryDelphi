unit caMtxVec;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Classes,
  Sysutils,
  Math,

  // ca units 
  caClasses,
  caTypes,
  caUtils;

type

  TcaMtxVecException = class(Exception);

  //---------------------------------------------------------------------------
  // IcaVec                                                                    
  //---------------------------------------------------------------------------

  IcaVec = interface
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
    procedure Clear(ACount: Integer = 0);
    // Public properties 
    property Count: Integer read GetCount write SetCount;

  end;

  //---------------------------------------------------------------------------
  // TcaVec                                                                    
  //---------------------------------------------------------------------------

  TcaVec = class(TcaInterfacedPersistent, IcaVec)
  private
    FArrayPointer: Pointer;
    // Property methods 
    function GetArrayPointer: Pointer;
    function GetCount: Integer;
    procedure SetArrayPointer(const Value: Pointer);
    procedure SetCount(const Value: Integer);
  protected
    // Protected virtual methods 
    function GetElementSize: Integer; virtual; abstract;
    procedure DoAssignData(SourcePointer: Pointer); virtual; abstract;
    procedure DoSetArrayLength(ACount: Integer); virtual; abstract;
    procedure DoUpdateCount(var ACount: Integer); virtual; abstract;
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
    procedure Clear(ACount: Integer = 0);
    // Properties 
    property Count: Integer read GetCount write SetCount;
  end;

  //---------------------------------------------------------------------------
  // IcaPtrVec                                                                 
  //---------------------------------------------------------------------------

  IcaPtrVec = interface(IcaVec)
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
  // TcaPtrVec                                                                 
  //---------------------------------------------------------------------------

  TcaPtrVec = class(TcaVec, IcaPtrVec)
  private
    FArray: TcaPointerArray;
    // Property methods 
    function GetItem(Index: Integer): Pointer;
    procedure SetItem(Index: Integer; const Value: Pointer);
  protected
    // Protected methods 
    function GetElementSize: Integer; override;
    procedure DoAssignData(SourcePointer: Pointer); override;
    procedure DoSetArrayLength(ACount: Integer); override;
    procedure DoUpdateCount(var ACount: Integer); override;
  public
    // Interface methods - IcaPtrVec 
    procedure Add(AValue: Pointer);
    // Properties 
    property Items[Index: Integer]: Pointer read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaObjVec                                                                 
  //---------------------------------------------------------------------------

  IcaObjVec = interface(IcaVec)
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
  // TcaObjVec                                                                 
  //---------------------------------------------------------------------------

  TcaObjVec = class(TcaVec, IcaObjVec)
  private
    FArray: TcaObjectArray;
    // Property methods 
    function GetItem(Index: Integer): TObject;
    procedure SetItem(Index: Integer; const Value: TObject);
  protected
    // Protected methods 
    function GetElementSize: Integer; override;
    procedure DoAssignData(SourcePointer: Pointer); override;
    procedure DoSetArrayLength(ACount: Integer); override;
    procedure DoUpdateCount(var ACount: Integer); override;
  public
    // Interface methods - IcaPtrVec 
    procedure Add(AValue: TObject);
    // Properties 
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaIntVec                                                                 
  //---------------------------------------------------------------------------

  IcaIntVec = interface(IcaVec)
  ['{511DE8E4-C3DF-425D-BB84-BCE0BCB2A4A2}']
    // Property methods 
    function GetIntArray: TcaIntegerArray;
    function GetItem(Index: Integer): Integer;
    procedure SetIntArray(const Value: TcaIntegerArray);
    procedure SetItem(Index: Integer; const Value: Integer);
    // Interface methods 
    procedure Add(AValue: Integer);
    // Properties 
    property IntArray: TcaIntegerArray read GetIntArray write SetIntArray;
    property Items[Index: Integer]: Integer read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // TcaIntVec                                                                 
  //---------------------------------------------------------------------------

  TcaIntVec = class(TcaVec, IcaIntVec)
  private
    FArray: TcaIntegerArray;
    // Property methods 
    function GetIntArray: TcaIntegerArray;
    function GetItem(Index: Integer): Integer;
    procedure SetIntArray(const Value: TcaIntegerArray);
    procedure SetItem(Index: Integer; const Value: Integer);
  protected
    // Protected methods 
    function GetElementSize: Integer; override;
    procedure DoAssignData(SourcePointer: Pointer); override;
    procedure DoSetArrayLength(ACount: Integer); override;
    procedure DoUpdateCount(var ACount: Integer); override;
  public
    // Interface methods - IcaIntVec 
    procedure Add(AValue: Integer);
    // Properties 
    property IntArray: TcaIntegerArray read GetIntArray write SetIntArray;
    property Items[Index: Integer]: Integer read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaDblVec                                                                 
  //---------------------------------------------------------------------------

  IcaDblVec = interface(IcaVec)
  ['{5768D4E8-BBB5-44DA-9B9D-45FEF8A2AEDA}']
    // Property methods 
    function GetItem(Index: Integer): Double;
    procedure SetItem(Index: Integer; const Value: Double);
    // Interface methods 
    function DivideBy(const ADivisor: IcaDblVec): IcaDblVec;
    function MultiplyBy(const AMultiplicand: IcaDblVec): IcaDblVec;
    procedure Add(AValue: Double); overload;
    procedure Add(AValues: array of Double); overload;
    // Properties 
    property Items[Index: Integer]: Double read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // TcaDblVec                                                                 
  //---------------------------------------------------------------------------

  TcaDblVec = class(TcaVec, IcaDblVec)
  private
    // private fields 
    FArray: TcaDoubleArray;
    FMathUtils: IcaMathUtils;
    // Property methods 
    function GetItem(Index: Integer): Double;
    procedure SetItem(Index: Integer; const Value: Double);
    // private methods 
    procedure CheckCounts(ACount1, ACount2: Integer);
  protected
    // Protected methods 
    function GetElementSize: Integer; override;
    procedure DoAssignData(SourcePointer: Pointer); override;
    procedure DoSetArrayLength(ACount: Integer); override;
    procedure DoUpdateCount(var ACount: Integer); override;
  public
    // public overrides 
    procedure AfterConstruction; override;
    // Interface methods - IcaDblVec 
    function DivideBy(const ADivisor: IcaDblVec): IcaDblVec;
    function MultiplyBy(const AMultiplicand: IcaDblVec): IcaDblVec;
    procedure Add(AValue: Double); overload;
    procedure Add(AValues: array of Double); overload;
    // Properties 
    property Items[Index: Integer]: Double read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaExtVec                                                                 
  //---------------------------------------------------------------------------

  IcaExtVec = interface(IcaVec)
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
  // TcaExtVec                                                                 
  //---------------------------------------------------------------------------

  TcaExtVec = class(TcaVec, IcaExtVec)
  private
    FArray: TcaExtendedArray;
    // Property methods 
    function GetItem(Index: Integer): Extended;
    procedure SetItem(Index: Integer; const Value: Extended);
  protected
    // Protected methods 
    function GetElementSize: Integer; override;
    procedure DoAssignData(SourcePointer: Pointer); override;
    procedure DoSetArrayLength(ACount: Integer); override;
    procedure DoUpdateCount(var ACount: Integer); override;
  public
    // Interface methods - IcaExtVec 
    procedure Add(AValue: Extended);
    // Properties 
    property Items[Index: Integer]: Extended read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaMtx                                                                    
  //---------------------------------------------------------------------------

  IcaMtx = interface
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
  // TcaMtx                                                                    
  //---------------------------------------------------------------------------

  TcaMtx = class(TInterfacedObject)
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
  // IcaIntMtx                                                                 
  //---------------------------------------------------------------------------

  IcaIntMtx = interface(IcaMtx)
  ['{F38AA99E-CFCC-44D8-89B8-4F79E53C3B9C}']
    // Property methods 
    function GetItem(ACol, ARow: Integer): Integer;
    procedure SetItem(ACol, ARow: Integer; const Value: Integer);
    // Properties 
    property Items[ACol, ARow: Integer]: Integer read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // TcaIntMtx                                                                 
  //---------------------------------------------------------------------------

  TcaIntMtx = class(TcaMtx, IcaIntMtx)
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
  // IcaDblMtx                                                                 
  //---------------------------------------------------------------------------

  IcaDblMtx = interface(IcaMtx)
  ['{16DD8AEA-E3DE-4668-A283-1BDFD17EA975}']
    // Property methods 
    function GetDblArray: TcaDoubleArray2;
    function GetItem(ACol, ARow: Integer): Double;
    procedure SetDblArray(const Value: TcaDoubleArray2);
    procedure SetItem(ACol, ARow: Integer; const Value: Double);
    // Interface methods 
    function LUDecompose(out APerms: IcaIntVec): IcaDblMtx;
    function LUSubstitute(const APermutations: IcaIntVec; ARightVector: IcaDblVec): IcaDblVec;
    function Multiply(const AMtx: IcaDblMtx): IcaDblMtx;
    procedure LUInvert;
    procedure FillRandom(AMax: Double);
    // Properties 
    property DblArray: TcaDoubleArray2 read GetDblArray write SetDblArray;
    property Items[ACol, ARow: Integer]: Double read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // TcaDblMtx                                                                 
  //---------------------------------------------------------------------------

  TcaDblMtx = class(TcaMtx, IcaDblMtx)
  private
    FArray: TcaDoubleArray2;
    // Property methods 
    function GetDblArray: TcaDoubleArray2;
    function GetItem(ACol, ARow: Integer): Double;
    procedure SetDblArray(const Value: TcaDoubleArray2);    
    procedure SetItem(ACol, ARow: Integer; const Value: Double);
  protected
    // Protected virtual methods 
    procedure UpdateArrayDimensions; override;
    // Interface methods - IcaDblMtx 
    function LUDecompose(out APerms: IcaIntVec): IcaDblMtx;
    function LUSubstitute(const APermutations: IcaIntVec; ARightVector: IcaDblVec): IcaDblVec;
    function Multiply(const AMtx: IcaDblMtx): IcaDblMtx;
    procedure LUInvert;
    procedure FillRandom(AMax: Double);
  public
    // Properties 
    property DblArray: TcaDoubleArray2 read GetDblArray write SetDblArray;
    property Items[ACol, ARow: Integer]: Double read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // IcaExtMtx                                                                 
  //---------------------------------------------------------------------------

  IcaExtMtx = interface(IcaMtx)
  ['{16DD8AEA-E3DE-4668-A283-1BDFD17EA975}']
    // Property methods 
    function GetItem(ACol, ARow: Integer): Extended;
    procedure SetItem(ACol, ARow: Integer; const Value: Extended);
    // Properties 
    property Items[ACol, ARow: Integer]: Extended read GetItem write SetItem; default;
  end;

  //---------------------------------------------------------------------------
  // TcaExtMtx                                                                 
  //---------------------------------------------------------------------------

  TcaExtMtx = class(TcaMtx, IcaExtMtx)
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
  // TcaVec                                                                    
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaVec.Create(ACount: Integer = 0);
begin
  inherited Create;
  SetCount(ACount);
end;

  // Public methods 

function TcaVec.LastIndex: Integer;
begin
  Result := Pred(GetCount);
end;

procedure TcaVec.AssignFromInterface(Source: IUnknown);
var
  SourceVector: IcaVec;
begin
  if Supports(Source, IcaVec, SourceVector) then
    begin
      SetCount(SourceVector.Count);
      DoAssignData(SourceVector.ArrayPointer);
    end;
end;

procedure TcaVec.AssignFromObject(Source: TPersistent);
var
  SourceVector: TcaVec;
begin
  if Source is TcaVec then
    begin
      SourceVector := TcaVec(Source);
      SetCount(SourceVector.Count);
      DoAssignData(SourceVector.ArrayPointer);
    end;
end;

procedure TcaVec.Clear(ACount: Integer = 0);
begin
  SetCount(0);
  if ACount <> 0 then
    SetCount(ACount);
end;

  // Protected static methods 

procedure TcaVec.Grow(ADelta: Integer = 1);
begin
  SetCount(GetCount + ADelta);
end;

  // Property methods 

function TcaVec.GetArrayPointer: Pointer;
begin
  Result := FArrayPointer;
end;

function TcaVec.GetCount: Integer;
var
  ACount: Integer;
begin
  DoUpdateCount(ACount);
  Result := ACount;
end;

procedure TcaVec.SetArrayPointer(const Value: Pointer);
begin
  FArrayPointer := Value;
end;

procedure TcaVec.SetCount(const Value: Integer);
begin
  DoSetArrayLength(Value);
end;

  //---------------------------------------------------------------------------
  // TcaPtrVec                                                                 
  //---------------------------------------------------------------------------

  // Interface methods - IcaPtrVec 

procedure TcaPtrVec.Add(AValue: Pointer);
begin
  Grow;
  SetItem(LastIndex, AValue);
end;

  // Protected methods 

function TcaPtrVec.GetElementSize: Integer;
begin
  Result := SizeOf(Pointer);
end;

procedure TcaPtrVec.DoAssignData(SourcePointer: Pointer);
var
  SourceArray: TcaPointerArray;
begin
  SourceArray := TcaPointerArray(SourcePointer^);
  FArray := SourceArray;
end;

procedure TcaPtrVec.DoSetArrayLength(ACount: Integer);
begin
  SetLength(FArray, ACount);
  ArrayPointer := @FArray;
end;

procedure TcaPtrVec.DoUpdateCount(var ACount: Integer);
begin
  ACount := Length(FArray);
end;

  // Property methods 

function TcaPtrVec.GetItem(Index: Integer): Pointer;
begin
  Result := FArray[Index];
end;

procedure TcaPtrVec.SetItem(Index: Integer; const Value: Pointer);
begin
  FArray[Index] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaObjVec                                                                 
  //---------------------------------------------------------------------------

  // Interface methods - IcaPtrVec 

procedure TcaObjVec.Add(AValue: TObject);
begin
  Grow;
  SetItem(LastIndex, AValue);  
end;

  // Protected methods 

function TcaObjVec.GetElementSize: Integer;
begin
  Result := SizeOf(TObject);
end;

procedure TcaObjVec.DoAssignData(SourcePointer: Pointer);
var
  SourceArray: TcaObjectArray;
begin
  SourceArray := TcaObjectArray(SourcePointer^);
  FArray := SourceArray;
end;

procedure TcaObjVec.DoSetArrayLength(ACount: Integer);
begin
  SetLength(FArray, ACount);
  ArrayPointer := @FArray;
end;

procedure TcaObjVec.DoUpdateCount(var ACount: Integer);
begin
  ACount := Length(FArray);
end;

  // Property methods 

function TcaObjVec.GetItem(Index: Integer): TObject;
begin
  Result := FArray[Index];
end;

procedure TcaObjVec.SetItem(Index: Integer; const Value: TObject);
begin
  FArray[Index] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaIntVec                                                                 
  //---------------------------------------------------------------------------

  // Interface methods - IcaIntVec 

procedure TcaIntVec.Add(AValue: Integer);
begin
  Grow;
  SetItem(LastIndex, AValue);
end;

  // Protected methods 

function TcaIntVec.GetElementSize: Integer;
begin
  Result := SizeOf(Integer);
end;

procedure TcaIntVec.DoAssignData(SourcePointer: Pointer);
var
  SourceArray: TcaIntegerArray;
begin
  SourceArray := TcaIntegerArray(SourcePointer^);
  FArray := SourceArray;
end;

procedure TcaIntVec.DoSetArrayLength(ACount: Integer);
begin
  SetLength(FArray, ACount);
  ArrayPointer := @FArray;
end;

procedure TcaIntVec.DoUpdateCount(var ACount: Integer);
begin
  ACount := Length(FArray);
end;

  // Property methods 

function TcaIntVec.GetIntArray: TcaIntegerArray;
begin
  Result := FArray;
end;

function TcaIntVec.GetItem(Index: Integer): Integer;
begin
  Result := FArray[Index];
end;

procedure TcaIntVec.SetIntArray(const Value: TcaIntegerArray);
begin
  FArray := Value;
end;

procedure TcaIntVec.SetItem(Index: Integer; const Value: Integer);
begin
  FArray[Index] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaDblVec                                                                 
  //---------------------------------------------------------------------------

  // public overrides 

procedure TcaDblVec.AfterConstruction;
begin
  inherited;
  FMathUtils := Utils as IcaMathUtils;
end;

  // Interface methods - IcaDblVec 

function TcaDblVec.DivideBy(const ADivisor: IcaDblVec): IcaDblVec;
var
  Index: Integer;
begin
  Result := TcaDblVec.Create;
  CheckCounts(Count, ADivisor.Count);
  for Index := 0 to Pred(Count) do
    Result.Add(FMathUtils.FloatDiv(GetItem(Index), ADivisor[Index], 0));
end;

function TcaDblVec.MultiplyBy(const AMultiplicand: IcaDblVec): IcaDblVec;
var
  Index: Integer;
begin
  Result := TcaDblVec.Create;
  CheckCounts(Count, AMultiplicand.Count);
  for Index := 0 to Pred(Count) do
    Result.Add(GetItem(Index) * AMultiplicand[Index]);
end;

procedure TcaDblVec.Add(AValue: Double);
begin
  Grow;
  SetItem(LastIndex, AValue);
end;

procedure TcaDblVec.Add(AValues: array of Double);
var
  Index: Integer;
begin
  for Index := Low(AValues) to High(AValues) do
    Add(AValues[Index]);
end;

  // Protected methods 

procedure TcaDblVec.DoAssignData(SourcePointer: Pointer);
var
  SourceArray: TcaDoubleArray;
begin
  SourceArray := TcaDoubleArray(SourcePointer^);
  FArray := SourceArray;
end;

function TcaDblVec.GetElementSize: Integer;
begin
  Result := SizeOf(Double);
end;

procedure TcaDblVec.DoSetArrayLength(ACount: Integer);
begin
  SetLength(FArray, ACount);
  ArrayPointer := @FArray;
end;

procedure TcaDblVec.DoUpdateCount(var ACount: Integer);
begin
  ACount := Length(FArray);
end;

  // private methods 

procedure TcaDblVec.CheckCounts(ACount1, ACount2: Integer);
begin
  if ACount1 <> ACount2 then
    raise TcaMtxVecException.Create('Vectors must be the same length');
end;

  // Property methods 

function TcaDblVec.GetItem(Index: Integer): Double;
begin
  Result := FArray[Index];
end;

procedure TcaDblVec.SetItem(Index: Integer; const Value: Double);
begin
  FArray[Index] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaExtVec                                                                 
  //---------------------------------------------------------------------------

  // Interface methods - IcaExtVec 

procedure TcaExtVec.Add(AValue: Extended);
begin
  Grow;
  SetItem(LastIndex, AValue);  
end;

  // Protected methods 

procedure TcaExtVec.DoAssignData(SourcePointer: Pointer);
var
  SourceArray: TcaExtendedArray;
begin
  SourceArray := TcaExtendedArray(SourcePointer^);
  FArray := SourceArray;
end;

function TcaExtVec.GetElementSize: Integer;
begin
  Result := SizeOf(Extended);
end;

procedure TcaExtVec.DoSetArrayLength(ACount: Integer);
begin
  SetLength(FArray, ACount);
  ArrayPointer := @FArray;
end;

procedure TcaExtVec.DoUpdateCount(var ACount: Integer);
begin
  ACount := Length(FArray);
end;

  // Property methods 

function TcaExtVec.GetItem(Index: Integer): Extended;
begin
  Result := FArray[Index];
end;

procedure TcaExtVec.SetItem(Index: Integer; const Value: Extended);
begin
  FArray[Index] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaMtx                                                                    
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaMtx.Create(AColCount, ARowCount: Integer);
begin
  inherited Create;
  FColCount := AColCount;
  FRowCount := ARowCount;
  UpdateArrayDimensions;
end;

  // Property methods 

function TcaMtx.GetColCount: Integer;
begin
  Result := FColCount;
end;

function TcaMtx.GetRowCount: Integer;
begin
  Result := FRowCount;
end;

procedure TcaMtx.SetColCount(const Value: Integer);
begin
  FColCount := Value;
  UpdateArrayDimensions;
end;

procedure TcaMtx.SetRowCount(const Value: Integer);
begin
  FRowCount := Value;
  UpdateArrayDimensions;
end;

  //---------------------------------------------------------------------------
  // TcaIntMtx                                                                 
  //---------------------------------------------------------------------------

  // Protected virtual methods 

procedure TcaIntMtx.UpdateArrayDimensions;
begin
  SetLength(FArray, FColCount, FRowCount);
end;

  // Property methods 

function TcaIntMtx.GetItem(ACol, ARow: Integer): Integer;
begin
  Result := FArray[ACol, ARow];
end;

procedure TcaIntMtx.SetItem(ACol, ARow: Integer; const Value: Integer);
begin
  FArray[ACol, ARow] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaDblMtx                                                                 
  //---------------------------------------------------------------------------

  // Interface methods - IcaDblMtx 

function TcaDblMtx.LUDecompose(out APerms: IcaIntVec): IcaDblMtx;
var
  A: TcaDoubleArray2;
  Big, Tmp, Sum: Double;
  C, R: Integer;
  N, I, RMax: Integer;
  Scaling: IcaDblVec;
const
  SmallValue = 1.0E-20;
begin
  // Initialize 
  Result := nil;
  A := FArray;
  N := GetColCount;
  if N <> GetRowCount then Exit;
  Scaling := TcaDblVec.Create;
  Scaling.Count := N;
  // Loop over rows to get the implicit scaling information 
  for R := 0 to Pred(N) do
    begin
      Big := 0.0;
      for C := 0 to Pred(N) do
        Big := Max(Big, Abs(A[C, R]));
      if Big = 0 then Exit;
      Scaling[R] := 1.0 / Big;
    end;
  APerms := TcaIntVec.Create;
  APerms.Count := N;
  // Use Crout's Method column by column 
  for C := 0 to Pred(N) do
    begin
      for R := 0 to Pred(N) do
        begin
          Sum := A[C, R];
          for I := 0 to Pred(R) do
            Sum := Sum - A[I, R] * A[C, I];  // Sum := Sum - A[I, R] * A[R, C];
          A[C, R] := Sum;
        end;
      // Initialize the search the largest pivot element 
      Big := 0.0;
      RMax := -1;
      for R := C to Pred(N) do
        begin
          Sum := A[C, R];
          for I := 0 to Pred(C) do
            Sum := Sum - A[I, R] * A[C, I];  // Sum := Sum - A[I, R] * A[R, C];
          Sum := A[C, R];
          // Is the figure of merit for the pivot better than the best so far? 
          Tmp := Scaling[R] * Abs(Sum);
          if Tmp >= Big then
            begin
              Big := Tmp;
              RMax := R;
            end;
        end;
      // Do we need to interchange rows? 
      if C <> RMax then
        begin
          for I := 0 to Pred(N) do
            begin
              Tmp := A[I, RMax];
              A[I, RMax] := A[I, C];
              A[I, C] := Tmp;
            end;
          Scaling[RMax] := Scaling[C];
        end;
      APerms[C] := RMax;
      if A[C, C] = 0 then A[C, C] := SmallValue;
      // Divide by the pivot element 
      if C <> N then
        begin
          Tmp := 1 / A[C, C];
          for R := Succ(C) to Pred(N) do
            A[C, R] := A[C, R] * Tmp;
        end;
    end;
  Result := TcaDblMtx.Create(N, N);
  Result.DblArray := A;
end;

function TcaDblMtx.LUSubstitute(const APermutations: IcaIntVec; ARightVector: IcaDblVec): IcaDblVec;
var
  A: TcaDoubleArray2;
  C, R, RCheck: Integer;
  N, Perm: Integer;
  Sum: Double;
  Solution: IcaDblVec;
begin
  // Initialize 
  A := FArray;
  N := GetColCount;
  Solution := TcaDblVec.Create;
  Solution.Count := N;
  if N <> GetRowCount then Exit;
  // Forward substitutions by row 
  RCheck := 0;
  for R := 0 to Pred(N) do
    begin
      Perm := APermutations[R];
      Sum := ARightVector[Perm];
      ARightVector[Perm] := ARightVector[R];
      if RCheck <> 0 then
        begin
          for C := RCheck to R do  // for C := RCheck to Pred(R) do
            Sum := Sum - A[C, R] * ARightVector[C];
        end
      else
        begin
          if Sum <> 0.0 then
            RCheck := R;
        end;
      ARightVector[R] := Sum;
    end;
  // Backward substitutions by row 
  for R := Pred(N) downto 0 do
    begin
      Sum := ARightVector[R];
      for C := Succ(R) to Pred(N) do
        Sum := Sum - A[C, R] * ARightVector[C];
      Solution[R] := Sum / A[R, R];
    end;
  FArray := A;
  Result := Solution;
end;

function TcaDblMtx.Multiply(const AMtx: IcaDblMtx): IcaDblMtx;
var
  AA: TcaDoubleArray2;
  MA: TcaDoubleArray2;
  RA: TcaDoubleArray2;
  C, R, I, MCols, ARows: Integer;
  Sum: Double;
begin
  RA := nil;
  MA := nil;
  AA := nil;
  Result := nil;
  MCols := GetColCount;
  ARows := AMtx.RowCount;
  if MCols = ARows then
    begin
      Result := TcaDblMtx.Create(MCols, ARows);
      RA := Result.DblArray;
      MA := FArray;
      AA := AMtx.DblArray;
      for C := 0 to Pred(MCols) do
        for R := 0 to Pred(ARows) do
          begin
            Sum := 0.0;
            for I := 0 to Pred(MCols) do
              Sum := Sum + MA[I, R] * AA[C, I];
            RA[R, C] := Sum;
          end;
      Result.DblArray := RA;
    end;
end;

procedure TcaDblMtx.LUInvert;
var
  A: TcaDoubleArray2;
  C, R, N: Integer;
  Column: IcaDblVec;
  Decomposed: IcaDblMtx;
  Inverted: TcaDoubleArray2;
  Perms: IcaIntVec;
  Solution: IcaDblVec;
begin
  // Initialize 
  A := FArray;
  N := GetColCount;
  SetLength(Inverted, N, N);
  Column := TcaDblVec.Create;
  Column.Count := N;
  // Decompose the matrix just once 
  Decomposed := LUDecompose(Perms);
  if Assigned(Perms) then
    begin
      // Find inverse by columns 
      for C := 0 to Pred(N) do
        begin
          for R := 0 to Pred(N) do
            Column[R] := 0.0;
          Column[C] := 1.0;
          Solution := Decomposed.LUSubstitute(Perms, Column);
          if Assigned(Solution) then
            begin
              for R := 0 to Pred(N) do
                Inverted[C, R] := Solution[R];
            end;
        end;
    end;
  FArray := Inverted;
end;

procedure TcaDblMtx.FillRandom(AMax: Double);
var
  ACol: Integer;
  ARow: Integer;
begin
  for ACol := 0 to Pred(GetColCount) do
    for ARow := 0 to Pred(GetRowCount) do
      SetItem(ACol, ARow, Random * AMax);
end;

  // Protected virtual methods 

procedure TcaDblMtx.UpdateArrayDimensions;
begin
  SetLength(FArray, FColCount, FRowCount);
end;

  // Property methods 

function TcaDblMtx.GetDblArray: TcaDoubleArray2;
begin
  Result := FArray;
end;

function TcaDblMtx.GetItem(ACol, ARow: Integer): Double;
begin
  Result := FArray[ACol, ARow];
end;

procedure TcaDblMtx.SetDblArray(const Value: TcaDoubleArray2);
begin
  FArray := Value;
end;

procedure TcaDblMtx.SetItem(ACol, ARow: Integer; const Value: Double);
begin
  FArray[ACol, ARow] := Value;
end;

  //---------------------------------------------------------------------------
  // TcaExtMtx                                                                 
  //---------------------------------------------------------------------------

  // Protected virtual methods 

procedure TcaExtMtx.UpdateArrayDimensions;
begin
  SetLength(FArray, FColCount, FRowCount);
end;

  // Property methods 

function TcaExtMtx.GetItem(ACol, ARow: Integer): Extended;
begin
  Result := FArray[ACol, ARow];
end;

procedure TcaExtMtx.SetItem(ACol, ARow: Integer; const Value: Extended);
begin
  FArray[ACol, ARow] := Value;
end;

end.


