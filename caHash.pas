unit caHash;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Windows,
  SysUtils;

const
  cLeafSize = 256;  // Don't change 
  cBucketSize = 8;

type

  //---------------------------------------------------------------------------
  // TcaHashLinkedItem                                                         
  //---------------------------------------------------------------------------

  TcaHashLinkedItem = class(TObject)
  public
    // Public fields 
    Data: DWORD;
    Next: TcaHashLinkedItem;
    Value: DWORD;
    // Create/Destroy 
    constructor Create(AValue, AData: DWORD; ANext: TcaHashLinkedItem);
    destructor Destroy; override;
  end;

  //---------------------------------------------------------------------------
  // TcaHashTreeItem                                                           
  //---------------------------------------------------------------------------

  TcaHashTree = class;

  TcaHashTraverseProc = procedure(UserData, UserProc: Pointer; Value, Data: DWORD; var Done: Boolean) of object;

  TcaHashTreeItem = class
  private
    // Property fields 
    FFilled: Integer;
    FLevel: Integer;
    FOwner: TcaHashTree;
    // Private methods 
    procedure FreeItems;
    procedure InitializeItems;
  public
    // Public fields 
    Items: array[0..cLeafSize - 1] of TObject;
    // Create/Destroy 
    constructor Create(AOwner: TcaHashTree);
    destructor Destroy; override;
    // Public Methods 
    function Find(Value, Hash: DWORD; var Data: DWORD): Boolean;
    function ROR(Value: DWORD): DWORD;
    function RORN(Value: DWORD; Level: Integer): DWORD;
    function Traverse(UserData, UserProc: Pointer; TraverseProc: TcaHashTraverseProc): Boolean;
    procedure AddDown(Value, Data, Hash: DWORD);
    procedure Delete(Value, Hash: DWORD);
    // Properties 
    property Filled: Integer read FFilled write FFilled;
    property Level: Integer read FLevel write FLevel;
    property Owner: TcaHashTree read FOwner;
  end;

  //---------------------------------------------------------------------------
  // TcaHashTree                                                               
  //---------------------------------------------------------------------------

  TcaHashTree = class(TInterfacedObject)
  private
    // Property fields 
    FCount: Integer;
    FRoot: TcaHashTreeItem;
    // property methods...
    function GetCount: Integer;
  protected
    // Protected methods 
    function CompareValue(Value1, Value2: DWORD): Boolean; virtual; abstract;
    function Find(Value, Hash: DWORD; var Data: DWORD): Boolean;
    function HashValue(Value: DWORD): DWORD; virtual; abstract;
    procedure AddDown(Value, Data, Hash: DWORD);
    procedure Delete(Value, Hash: DWORD);
    procedure DestroyItem(var Value, Data: DWORD); virtual; abstract;
    procedure Traverse(UserData, UserProc: Pointer; TraverseProc: TcaHashTraverseProc);
  public
    // Create/Destroy 
    constructor Create; virtual;
    destructor Destroy; override;
    // Properties 
    property Count: Integer read GetCount;
    property Root: TcaHashTreeItem read FRoot;
  end;

  //---------------------------------------------------------------------------
  // IcaHash
  //---------------------------------------------------------------------------

  IcaHash = interface
  ['{1F564A82-56EA-4522-B1A8-26CC3641F018}']
    // property methods...
    function GetCount: Integer;
    // interface methods...
    function Find(const AKey: string): TObject; 
    procedure Add(const AKey: string; AData: TObject);
    procedure Delete(const AKey: string);
    // interface properties...
    property Count: Integer read GetCount;
  end;

  //---------------------------------------------------------------------------
  // TcaStringHashTree
  //---------------------------------------------------------------------------

  TcaStringHashTraverseProc = procedure(UserData: Pointer; const Value: string; Data: TObject; var Done: Boolean);

  TcaStringHashTraverseMeth = procedure(UserData: Pointer; const Value: string; Data: TObject; var Done: Boolean) of object;

  TcaHash = class(TcaHashTree, IcaHash)
  private
    // property fields...
    FCaseSensitive: Boolean;
    FAutoFreeObjects: Boolean;
  protected
    // protected methods...
    function CompareValue(Value1, Value2: DWORD): Boolean; override;
    function HashStr(const S: string): DWORD;
    function HashValue(Value: DWORD): DWORD; override;
    procedure DestroyItem(var Value, Data: DWORD); override;
    procedure TraverseMeth(UserData, UserProc: Pointer; Value, Data: DWORD; var Done: Boolean);
    procedure TraverseProc(UserData, UserProc: Pointer; Value, Data: DWORD; var Done: Boolean);
  public
    // lifetime...
    constructor Create; override;
    // interface methods - IcaHash...
    function Find(const AKey: string): TObject; overload;
    procedure Add(const AKey: string; AData: TObject);
    procedure Delete(const AKey: string);
    // other methods...
    procedure Traverse(UserData: Pointer; UserProc: TcaStringHashTraverseMeth); overload;
    procedure Traverse(UserData: Pointer; UserProc: TcaStringHashTraverseProc); overload;
    // properties...
    property AutoFreeObjects: Boolean read FAutoFreeObjects write FAutoFreeObjects;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
  end;

function CalcStrCRC32(const S: string): DWORD;

type
  TcaHashLenStat = array[1..cBucketSize] of Integer;

procedure Stat(AHashTree: TcaHashTree; var MaxLevel, PeakCnt, FillCnt, EmptyCnt: Integer; var LenStat: TcaHashLenStat);

implementation

procedure Stat(AHashTree: TcaHashTree; var MaxLevel, PeakCnt, FillCnt, EmptyCnt: Integer; var LenStat: TcaHashLenStat);

  procedure TreeStat(AHashTree: TcaHashTreeItem);
  var
    Index: Integer;
    LinkedItem: TcaHashLinkedItem;
    StatIndex: Integer;
  begin
    Inc(PeakCnt);
    if AHashTree.Level + 1 > MaxLevel then
      MaxLevel := AHashTree.Level + 1;
    for Index := 0 to Pred(cLeafSize) do
      if Assigned(AHashTree.Items[Index]) then
        begin
          Inc(FillCnt);
          if AHashTree.Items[Index] is TcaHashTreeItem then
            begin
              TreeStat(TcaHashTreeItem(AHashTree.Items[Index]));
            end
          else
            begin
              StatIndex := 0;
              LinkedItem := TcaHashLinkedItem(AHashTree.Items[Index]);
              while Assigned(LinkedItem) do
                begin
                  Inc(StatIndex);
                  LinkedItem := LinkedItem.Next;
                end;
              LenStat[StatIndex] := LenStat[StatIndex] + 1;
            end;
        end
      else
        Inc(EmptyCnt);
  end;

begin
  if Assigned(AHashTree.Root) then
    TreeStat(AHashTree.Root);
end;

  //---------------------------------------------------------------------------
  // TcaHashLinkedItem                                                         
  //---------------------------------------------------------------------------

constructor TcaHashLinkedItem.Create(AValue, AData: DWORD; ANext: TcaHashLinkedItem);
begin
  inherited Create;
  Value := AValue;
  Data := AData;
  Next := ANext;
end;

destructor TcaHashLinkedItem.Destroy;
begin
  Next.Free;
  inherited;
end;

  //---------------------------------------------------------------------------
  // TcaHashTreeItem                                                           
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaHashTreeItem.Create(AOwner: TcaHashTree);
begin
  inherited Create;
  FOwner := AOwner;
  InitializeItems;
end;

destructor TcaHashTreeItem.Destroy;
begin
  FreeItems;
  inherited;
end;

  // Public Methods 

function TcaHashTreeItem.Find(Value, Hash: DWORD; var Data: DWORD): Boolean;
var
  Index: Integer;
  LinkedItem: TcaHashLinkedItem;
begin
  Result := False;
  Index := Hash and $FF;
  if Assigned(Items[Index]) then
    begin
      if Items[Index] is TcaHashTreeItem then
        Result := TcaHashTreeItem(Items[Index]).Find(Value, ROR(Hash), Data)
      else
        begin
          LinkedItem := TcaHashLinkedItem(Items[Index]);
          while Assigned(LinkedItem) do
            begin
              if Owner.CompareValue(LinkedItem.Value, Value) then
                begin
                  Data := LinkedItem.Data;
                  Result := True;
                  Exit;
                end;
              LinkedItem := LinkedItem.Next;
            end;
        end;
    end;
end;

function TcaHashTreeItem.ROR(Value: DWORD): DWORD;
begin
  Result := ((Value and $FF) shl 24) or ((Value shr 8) and $FFFFFF);
end;

function TcaHashTreeItem.RORN(Value: DWORD; Level: Integer): DWORD;
begin
  Result := Value;
  while Level > 0 do
    begin
      Result := ROR(Result);
      Dec(Level);
    end;
end;

function TcaHashTreeItem.Traverse(UserData, UserProc: Pointer; TraverseProc: TcaHashTraverseProc): Boolean;
var
  Index: Integer;
  LinkedItem: TcaHashLinkedItem;
begin
  Result := False;
  for Index := 0 to Pred(cLeafSize) do
    begin
      if Assigned(Items[Index]) then
        begin
          if Items[Index] is TcaHashTreeItem then
            Result := TcaHashTreeItem(Items[Index]).Traverse(UserData, UserProc, TraverseProc)
          else
            begin
              LinkedItem := TcaHashLinkedItem(Items[Index]);
              while Assigned(LinkedItem) do
                begin
                  TraverseProc(UserData, UserProc, LinkedItem.Value, LinkedItem.Data, Result);
                  LinkedItem := LinkedItem.Next;
                end;
            end;
          if Result then Exit;
        end;
    end;
end;

procedure TcaHashTreeItem.AddDown(Value, Data, Hash: DWORD);
var
  Index: Integer;
  LinkedItem: TcaHashLinkedItem;
  LinkIndex: Integer;
  TreeItem: TcaHashTreeItem;
begin
  Index := Hash and $FF;
  if Items[Index] = nil then
    begin
      Items[Index] := TcaHashLinkedItem.Create(Value, Data, nil);
      Inc(FFilled);
    end
  else
    begin
      if Items[Index] is TcaHashTreeItem then
        TcaHashTreeItem(Items[Index]).AddDown(Value, Data, ROR(Hash))
      else
        begin
          LinkIndex := 0;
          LinkedItem := TcaHashLinkedItem(Items[Index]);
          while Assigned(LinkedItem) do
            begin
              if Owner.CompareValue(LinkedItem.Value, Value) then
                begin
                  // found
                  LinkedItem.Data := Data;
                  Exit;
                end;
              LinkedItem := LinkedItem.Next;
              Inc(LinkIndex)
            end;
          if LinkIndex >= cBucketSize then
            begin
              // full
              TreeItem := TcaHashTreeItem.Create(Owner);
              TreeItem.Level := Level + 1;
              LinkedItem := TcaHashLinkedItem(Items[Index]);
              while Assigned(LinkedItem) do
                begin
                  TreeItem.AddDown(LinkedItem.Value, LinkedItem.Data,
                    RORN(Owner.HashValue(LinkedItem.Value), Level + 1));
                  LinkedItem := LinkedItem.Next;
                end;
              TreeItem.AddDown(Value, Data, ROR(Hash));
              TcaHashLinkedItem(Items[Index]).Free;
              Items[Index] := TreeItem;
            end
          else
            Items[Index] := TcaHashLinkedItem.Create(Value, Data, TcaHashLinkedItem(Items[Index]));
        end;
    end;
end;

procedure TcaHashTreeItem.Delete(Value, Hash: DWORD);
var
  Index: Integer;
  LinkedItem: TcaHashLinkedItem;
  PrevLinkedItem: TcaHashLinkedItem;
begin
  Index := Hash and $FF;
  if Assigned(Items[Index]) then
    begin
      if Items[Index] is TcaHashTreeItem then
        begin
          TcaHashTreeItem(Items[Index]).Delete(Value, ROR(Hash));
          if TcaHashTreeItem(Items[Index]).Filled = 0 then
            begin
              TcaHashTreeItem(Items[Index]).Free;
              Items[Index] := nil;
            end;
        end
      else
        begin
          PrevLinkedItem := nil;
          LinkedItem := TcaHashLinkedItem(Items[Index]);
          while Assigned(LinkedItem) do
            begin
              if Owner.CompareValue(LinkedItem.Value, Value) then
                begin
                  // found
                  if PrevLinkedItem = nil then
                    begin
                      Items[Index] := LinkedItem.Next;
                      if Items[Index] = nil then
                        Dec(FFilled);
                    end
                  else
                    PrevLinkedItem.Next := LinkedItem.Next;
                  LinkedItem.Next := nil;
                  Owner.DestroyItem(LinkedItem.Value, LinkedItem.Data);
                  LinkedItem.Free;
                  Exit;
                end;
              PrevLinkedItem := LinkedItem;
              LinkedItem := LinkedItem.Next;
            end;
        end;
    end;
end;

  // Private methods 

procedure TcaHashTreeItem.FreeItems;
var
  Index: Integer;
  LinkedItem: TcaHashLinkedItem;
begin
  for Index := 0 to Pred(cLeafSize) do
    begin
      if Assigned(Items[Index]) then
        begin
          if Items[Index] is TcaHashTreeItem then
            TcaHashTreeItem(Items[Index]).Free
          else
            begin
              LinkedItem := TcaHashLinkedItem(Items[Index]);
              while Assigned(LinkedItem) do
                begin
                  Owner.DestroyItem(LinkedItem.Value, LinkedItem.Data);
                  LinkedItem := LinkedItem.Next;
                end;
              TcaHashLinkedItem(Items[Index]).Free;
            end;
        end;
    end;
end;

procedure TcaHashTreeItem.InitializeItems;
var
  Index: Integer;
begin
  for Index := 0 to Pred(cLeafSize) do
    Items[Index] := nil;
end;

  //---------------------------------------------------------------------------
  // TcaHashTree                                                               
  //---------------------------------------------------------------------------

procedure TcaHashTree.AddDown(Value, Data, Hash: DWORD);
begin
  if FRoot = nil then
    FRoot := TcaHashTreeItem.Create(Self);
  FRoot.AddDown(Value, Data, Hash);
  Inc(FCount);
end;

procedure TcaHashTree.Delete(Value, Hash: DWORD);
begin
  if Assigned(FRoot) then
    FRoot.Delete(Value, Hash);
end;

function TcaHashTree.Find(Value, Hash: DWORD; var Data: DWORD): Boolean;
begin
  if Assigned(FRoot) then
    Result := FRoot.Find(Value, Hash, Data)
  else
    Result := False;
end;

constructor TcaHashTree.Create;
begin
  inherited;
end;

destructor TcaHashTree.Destroy;
begin
  FRoot.Free;
  inherited;
end;

procedure TcaHashTree.Traverse(UserData, UserProc: Pointer; TraverseProc: TcaHashTraverseProc);
begin
  if Assigned(FRoot) then
    FRoot.Traverse(UserData, UserProc, TraverseProc);
end;

  // property methods...

function TcaHashTree.GetCount: Integer;
begin
  Result := FCount;
end;

  //---------------------------------------------------------------------------
  // TcaHash
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaHash.Create;
begin
  inherited;
  FCaseSensitive := False;
  FAutoFreeObjects := False;
end;

  // Public methods 

function TcaHash.Find(const AKey: string): TObject;
var
  Data: TObject;
begin
  Result := nil;
  if inherited Find(DWORD(@AKey), HashStr(AKey), DWORD(Data)) then
    Result := Data;
end;

procedure TcaHash.Add(const AKey: string; AData: TObject);
begin
  AddDown(DWORD(NewStr(AKey)), DWORD(AData), HashStr(AKey));
end;

procedure TcaHash.Delete(const AKey: string);
begin
  inherited Delete(DWORD(@AKey), HashStr(AKey));
end;

procedure TcaHash.Traverse(UserData: Pointer; UserProc: TcaStringHashTraverseProc);
begin
  inherited Traverse(UserData, @UserProc, TraverseProc);
end;

procedure TcaHash.Traverse(UserData: Pointer; UserProc: TcaStringHashTraverseMeth);
begin
  inherited Traverse(UserData, @TMethod(UserProc), TraverseMeth);
end;

  // Protected methods 

function TcaHash.CompareValue(Value1, Value2: DWORD): Boolean;
begin
  if FCaseSensitive then
    Result := PString(Value1)^ = PString(Value2)^
  else
    Result := ANSICompareText(PString(Value1)^, PString(Value2)^) = 0;
end;

function TcaHash.HashStr(const S: string): DWORD;
begin
  if CaseSensitive then
    Result := CalcStrCRC32(S)
  else
    Result := CalcStrCRC32(ANSIUpperCase(S));
end;

function TcaHash.HashValue(Value: DWORD): DWORD;
begin
  Result := HashStr(PString(Value)^);
end;

procedure TcaHash.DestroyItem(var Value, Data: DWORD);
begin
  DisposeStr(PString(Value));
  if FAutoFreeObjects then
    TObject(Data).Free;
  Value := 0;
  Data := 0;
end;

procedure TcaHash.TraverseMeth(UserData, UserProc: Pointer; Value, Data: DWORD; var Done: Boolean);
type
  PTStrHashTraverseMeth = ^TcaStringHashTraverseMeth;
begin
  PTStrHashTraverseMeth(UserProc)^(UserData, PString(Value)^, TObject(Data), Done);
end;

procedure TcaHash.TraverseProc(UserData, UserProc: Pointer; Value, Data: DWORD; var Done: Boolean);
begin
  TcaStringHashTraverseProc(UserProc)(UserData, PString(Value)^, TObject(Data), Done);
end;

  // Dynamic crc32 table 

const
  CRC32_POLYNOMIAL = $EDB88320;
var
  Ccitt32Table: array[0..255] of DWORD;

function CalcStrCRC32(const S: string): DWORD;
var
  Index: Integer;
begin
  Result := $FFFFFFFF;
  for Index := 1 to Length(S) do
    Result := (((Result shr 8) and $00FFFFFF) xor (Ccitt32Table[(Result xor byte(S[Index])) and $FF]));
end;

procedure BuildCRCTable;
var
  Index: longint;
  Index2: longint;
  Value: DWORD;
begin
  for Index := 0 to 255 do
    begin
      Value := Index;
      for Index2 := 8 downto 1 do
        if ((Value and 1) <> 0) then
          Value := (Value shr 1) xor CRC32_POLYNOMIAL
        else
          Value := Value shr 1;
      Ccitt32Table[Index] := Value;
    end
end;

initialization
  BuildCRCTable;

end.

