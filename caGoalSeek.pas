unit caGoalSeek;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Classes,
  Sysutils,
  Math,

  // ca units 
  caClasses,
  caUtils,
  caTypes,
  caConsts,
  caMath,
  caVector,
  caGenetic,
  caNodes,
  caXML;

type

  TcaGoalSeekMethods = (smBruteForce, smBinarySearch, smGenetic);

  TcaGoalSeekEvaluateEvent = procedure(Sender: TObject; AParam: Double; var AResult: Double) of object;

  //---------------------------------------------------------------------------
  // IcaGoalSeekerBruteForce                                                   
  //---------------------------------------------------------------------------

  IcaGoalSeekerBruteForce = interface
  ['{C539D566-EC2B-48D7-9327-A2830D0143C0}']
    // Property methods 
    function GetStepSize: Double;
    procedure SetStepSize(const Value: Double);
    // Properties 
    property StepSize: Double read GetStepSize write SetStepSize;
  end;

  //---------------------------------------------------------------------------
  // TcaGoalSeekerBruteForce                                                   
  //---------------------------------------------------------------------------

  TcaGoalSeekerBruteForce = class(TcaInterfacedPersistent, IcaGoalSeekerBruteForce)
  private
    // Property fields 
    FStepSize: Double;
  protected
    // Property methods 
    function GetStepSize: Double;
    procedure SetStepSize(const Value: Double);
    // Properties 
    property StepSize: Double read GetStepSize write SetStepSize;
  end;

  //---------------------------------------------------------------------------
  // IcaGoalSeekerBinarySearch                                                 
  //---------------------------------------------------------------------------

  IcaGoalSeekerBinarySearch = interface
  ['{3597B661-8B14-4128-B15E-C43607AB579C}']
    // Property methods 
    // Properties 
  end;

  //---------------------------------------------------------------------------
  // TcaGoalSeekerBinarySearch                                                 
  //---------------------------------------------------------------------------

  TcaGoalSeekerBinarySearch = class(TcaInterfacedPersistent, IcaGoalSeekerBinarySearch)
  private
    // Property fields 
  protected
    // Property methods 
    // Properties 
  end;

  //---------------------------------------------------------------------------
  // IcaGoalSeekerGenetic                                                      
  //---------------------------------------------------------------------------

  IcaGoalSeekerGenetic = interface
  ['{03561816-13C4-4256-88F7-B5B9839DB68D}']
    // Property methods 
    function GetAlienExtent: Integer;
    function GetAlienRate: Integer;
    function GetAllowDuplicates: Boolean;
    function GetChildCount: Integer;
    function GetChromozomeSize: Integer;
    function GetCrossoverRate: Integer;
    function GetFitnessMultiplier: Double;
    function GetLogging: Boolean;
    function GetMaxGenerations: Integer;
    function GetMaxPopulation: Integer;
    function GetMutationExtent: Integer;
    function GetMutationRate: Integer;
    function GetPopulation: TcaPopulation;
    function GetRandSeed: Integer;
    function GetTargetFitness: Integer;
    procedure SetAlienExtent(const Value: Integer);
    procedure SetAlienRate(const Value: Integer);
    procedure SetAllowDuplicates(const Value: Boolean);
    procedure SetChildCount(const Value: Integer);
    procedure SetChromozomeSize(const Value: Integer);
    procedure SetCrossoverRate(const Value: Integer);
    procedure SetFitnessMultiplier(const Value: Double);
    procedure SetLogging(const Value: Boolean);
    procedure SetMaxGenerations(const Value: Integer);
    procedure SetMaxPopulation(const Value: Integer);
    procedure SetMutationExtent(const Value: Integer);
    procedure SetMutationRate(const Value: Integer);
    procedure SetPopulation(const Value: TcaPopulation);
    procedure SetRandSeed(const Value: Integer);
    procedure SetTargetFitness(const Value: Integer);
    // Properties 
    property AlienExtent: Integer read GetAlienExtent write SetAlienExtent;
    property AlienRate: Integer read GetAlienRate write SetAlienRate;
    property AllowDuplicates: Boolean read GetAllowDuplicates write SetAllowDuplicates;
    property ChildCount: Integer read GetChildCount write SetChildCount;
    property ChromozomeSize: Integer read GetChromozomeSize write SetChromozomeSize;
    property CrossoverRate: Integer read GetCrossoverRate write SetCrossoverRate;
    property FitnessMultiplier: Double read GetFitnessMultiplier write SetFitnessMultiplier;
    property Logging: Boolean read GetLogging write SetLogging;
    property MaxGenerations: Integer read GetMaxGenerations write SetMaxGenerations;
    property MaxPopulation: Integer read GetMaxPopulation write SetMaxPopulation;
    property MutationExtent: Integer read GetMutationExtent write SetMutationExtent;
    property MutationRate: Integer read GetMutationRate write SetMutationRate;
    property Population: TcaPopulation read GetPopulation write SetPopulation;
    property RandSeed: Integer read GetRandSeed write SetRandSeed;
    property TargetFitness: Integer read GetTargetFitness write SetTargetFitness;
  end;

  //---------------------------------------------------------------------------
  // TcaGoalSeekerGenetic                                                      
  //---------------------------------------------------------------------------

  TcaGoalSeekerGenetic = class(TcaInterfacedPersistent, IcaGoalSeekerGenetic)
  private
    // Property methods 
    FAlienExtent: Integer;
    FAlienRate: Integer;
    FAllowDuplicates: Boolean;
    FChildCount: Integer;
    FChromozomeSize: Integer;
    FCrossoverRate: Integer;
    FFitnessMultiplier: Double;
    FLogging: Boolean;
    FMaxGenerations: Integer;
    FMaxPopulation: Integer;
    FMutationExtent: Integer;
    FMutationRate: Integer;
    FPopulation: TcaPopulation;
    FRandSeed: Integer;
    FTargetFitness: Integer;
    // Property methods 
    function GetAlienExtent: Integer;
    function GetAlienRate: Integer;
    function GetAllowDuplicates: Boolean;
    function GetChildCount: Integer;
    function GetChromozomeSize: Integer;
    function GetCrossoverRate: Integer;
    function GetFitnessMultiplier: Double;
    function GetLogging: Boolean;
    function GetMaxGenerations: Integer;
    function GetMaxPopulation: Integer;
    function GetMutationExtent: Integer;
    function GetMutationRate: Integer;
    function GetPopulation: TcaPopulation;
    function GetRandSeed: Integer;
    function GetTargetFitness: Integer;
    procedure SetAlienExtent(const Value: Integer);
    procedure SetAlienRate(const Value: Integer);
    procedure SetAllowDuplicates(const Value: Boolean);
    procedure SetChildCount(const Value: Integer);
    procedure SetChromozomeSize(const Value: Integer);
    procedure SetCrossoverRate(const Value: Integer);
    procedure SetFitnessMultiplier(const Value: Double);
    procedure SetLogging(const Value: Boolean);
    procedure SetMaxGenerations(const Value: Integer);
    procedure SetMaxPopulation(const Value: Integer);
    procedure SetMutationExtent(const Value: Integer);
    procedure SetMutationRate(const Value: Integer);
    procedure SetPopulation(const Value: TcaPopulation);
    procedure SetRandSeed(const Value: Integer);
    procedure SetTargetFitness(const Value: Integer);
  protected
    // Properties 
    property AlienExtent: Integer read GetAlienExtent write SetAlienExtent;
    property AlienRate: Integer read GetAlienRate write SetAlienRate;
    property AllowDuplicates: Boolean read GetAllowDuplicates write SetAllowDuplicates;
    property ChildCount: Integer read GetChildCount write SetChildCount;
    property ChromozomeSize: Integer read GetChromozomeSize write SetChromozomeSize;
    property CrossoverRate: Integer read GetCrossoverRate write SetCrossoverRate;
    property FitnessMultiplier: Double read GetFitnessMultiplier write SetFitnessMultiplier;
    property Logging: Boolean read GetLogging write SetLogging;
    property MaxGenerations: Integer read GetMaxGenerations write SetMaxGenerations;
    property MaxPopulation: Integer read GetMaxPopulation write SetMaxPopulation;
    property MutationExtent: Integer read GetMutationExtent write SetMutationExtent;
    property MutationRate: Integer read GetMutationRate write SetMutationRate;
    property Population: TcaPopulation read GetPopulation write SetPopulation;
    property RandSeed: Integer read GetRandSeed write SetRandSeed;
    property TargetFitness: Integer read GetTargetFitness write SetTargetFitness;
  end;

  //---------------------------------------------------------------------------
  // IcaGoalSeeker                                                             
  //---------------------------------------------------------------------------

  IcaGoalSeeker = interface
  ['{3F01E6B4-5ADD-44E2-A8EB-5A4B51BEB6EF}']
    // Property methods 
    function GetAutoAdjustParams: Boolean;
    function GetEvalFitnesses: IcaDoubleVector;
    function GetEvalParams: IcaDoubleVector;
    function GetEvalResults: IcaDoubleVector;
    function GetIterations: Integer;
    function GetMaxParam: Double;
    function GetMethod: TcaGoalSeekMethods;
    function GetMinParam: Double;
    function GetTarget: Double;
    function GetTolerance: Double;
    procedure SetAutoAdjustParams(const Value: Boolean);
    procedure SetIterations(const Value: Integer);
    procedure SetMaxParam(const Value: Double);
    procedure SetMethod(const Value: TcaGoalSeekMethods);
    procedure SetMinParam(const Value: Double);
    procedure SetTarget(const Value: Double);
    procedure SetTolerance(const Value: Double);
    // Event property methods 
    function GetOnEvaluate: TcaGoalSeekEvaluateEvent;
    procedure SetOnEvaluate(const Value: TcaGoalSeekEvaluateEvent);
    // Interface methods 
    function Solve: Boolean;
    // Properties 
    property AutoAdjustParams: Boolean read GetAutoAdjustParams write SetAutoAdjustParams;
    property EvalFitnesses: IcaDoubleVector read GetEvalFitnesses;
    property EvalParams: IcaDoubleVector read GetEvalParams;
    property EvalResults: IcaDoubleVector read GetEvalResults;
    property Iterations: Integer read GetIterations write SetIterations;
    property MaxParam: Double read GetMaxParam write SetMaxParam;
    property Method: TcaGoalSeekMethods read GetMethod write SetMethod;
    property MinParam: Double read GetMinParam write SetMinParam;
    property Target: Double read GetTarget write SetTarget;
    property Tolerance: Double read GetTolerance write SetTolerance;
    // Event properties 
    property OnEvaluate: TcaGoalSeekEvaluateEvent read GetOnEvaluate write SetOnEvaluate;
  end;

  //---------------------------------------------------------------------------
  // TcaGoalSeeker                                                             
  //---------------------------------------------------------------------------

  TcaGoalSeeker = class(TcaInterfacedPersistent, IcaGoalSeeker, IcaGoalSeekerBinarySearch,
                                                                IcaGoalSeekerBruteForce,
                                                                IcaGoalSeekerGenetic)
  private
    // Private fields 
    FAutoAdjustParams: Boolean;
    FErrorBenchmark: Double;
    FEvalFitnesses: IcaDoubleVector;
    FEvalParams: IcaDoubleVector;
    FEvalResults: IcaDoubleVector;
    FIterations: Integer;
    FMaxParam: Double;
    FMethod: TcaGoalSeekMethods;
    FMinParam: Double;
    FPopulation: TcaPopulation;
    FPopulationInitialized: Boolean;
    FTarget: Double;
    FTolerance: Double;
    // Genetic algorithm property fields 
    FGenetic: TcaGoalSeekerGenetic;
    // Binary Search property fields 
    FBinarySearch: TcaGoalSeekerBinarySearch;
    // Brute Force property fields 
    FBruteForce: TcaGoalSeekerBruteForce;
    // Event property fields 
    FOnEvaluate: TcaGoalSeekEvaluateEvent;
    // Private methods 
    function Evaluate(AParam: Double): Double;
    function SolveUsingBinarySearch: Boolean;
    function SolveUsingBruteForce: Boolean;
    function SolveUsingGenetic: Boolean;
    procedure AdjustParams;
    procedure CalibrateErrorBenchmark;
    procedure InitializeDefaults;
    procedure Reset;
    procedure UpdateGeneticPopulationParameters;
    // Event handlers 
    procedure GoalSeekEvaluate(Sender: TObject; AOrganism: TcaOrganism; var AFitness: Double);
    procedure GenerationCompletedEvent(Sender: TObject);
    procedure PopulationInitializedEvent(Sender: TObject);
  protected
    // Protected methods 
    procedure DoEvaluate(AParam: Double; var AResult: Double); virtual;
    // Property methods 
    function GetAutoAdjustParams: Boolean;
    function GetEvalFitnesses: IcaDoubleVector;
    function GetEvalParams: IcaDoubleVector;
    function GetEvalResults: IcaDoubleVector;
    function GetIterations: Integer;
    function GetMaxParam: Double;
    function GetMethod: TcaGoalSeekMethods;
    function GetMinParam: Double;
    function GetTarget: Double;
    function GetTolerance: Double;
    procedure SetAutoAdjustParams(const Value: Boolean);
    procedure SetIterations(const Value: Integer);
    procedure SetMaxParam(const Value: Double);
    procedure SetMethod(const Value: TcaGoalSeekMethods);
    procedure SetMinParam(const Value: Double);
    procedure SetTarget(const Value: Double);
    procedure SetTolerance(const Value: Double);
    // Event property methods 
    function GetOnEvaluate: TcaGoalSeekEvaluateEvent;
    procedure SetOnEvaluate(const Value: TcaGoalSeekEvaluateEvent);
    // Interface methods 
    function Solve: Boolean;
    // Properties 
    property AutoAdjustParams: Boolean read GetAutoAdjustParams write SetAutoAdjustParams;
    property EvalFitnesses: IcaDoubleVector read GetEvalFitnesses;
    property EvalParams: IcaDoubleVector read GetEvalParams;
    property EvalResults: IcaDoubleVector read GetEvalResults;
    property Iterations: Integer read GetIterations write SetIterations;
    property MaxParam: Double read GetMaxParam write SetMaxParam;
    property Method: TcaGoalSeekMethods read GetMethod write SetMethod;
    property MinParam: Double read GetMinParam write SetMinParam;
    property Target: Double read GetTarget write SetTarget;
    property Tolerance: Double read GetTolerance write SetTolerance;
    // Genetic algorithm properties 
    property BinarySearch: TcaGoalSeekerBinarySearch read FBinarySearch implements IcaGoalSeekerBinarySearch;
    property BruteForce: TcaGoalSeekerBruteForce read FBruteForce implements IcaGoalSeekerBruteForce;
    property Genetic: TcaGoalSeekerGenetic read FGenetic implements IcaGoalSeekerGenetic;
    // Event properties 
    property OnEvaluate: TcaGoalSeekEvaluateEvent read GetOnEvaluate write SetOnEvaluate;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  TcaGoalSeekerClass = class of TcaGoalSeeker;

implementation

  //---------------------------------------------------------------------------
  // TcaGoalSeekerBruteForce                                                   
  //---------------------------------------------------------------------------

function TcaGoalSeekerBruteForce.GetStepSize: Double;
begin
  Result := FStepSize;
end;

procedure TcaGoalSeekerBruteForce.SetStepSize(const Value: Double);
begin
  FStepSize := Value;
end;

  //---------------------------------------------------------------------------
  // TcaGoalSeekerBinarySearch                                                 
  //---------------------------------------------------------------------------

  // No implementation 

  //---------------------------------------------------------------------------
  // TcaGoalSeekerGenetic                                                      
  //---------------------------------------------------------------------------

  // Property methods 

function TcaGoalSeekerGenetic.GetAlienExtent: Integer;
begin
  Result := FAlienExtent;
end;

function TcaGoalSeekerGenetic.GetAlienRate: Integer;
begin
  Result := FAlienRate;
end;

function TcaGoalSeekerGenetic.GetAllowDuplicates: Boolean;
begin
  Result := FAllowDuplicates;
end;

function TcaGoalSeekerGenetic.GetChildCount: Integer;
begin
  Result := FChildCount;
end;

function TcaGoalSeekerGenetic.GetChromozomeSize: Integer;
begin
  Result := FChromozomeSize;
end;

function TcaGoalSeekerGenetic.GetCrossoverRate: Integer;
begin
  Result := FCrossoverRate;
end;

function TcaGoalSeekerGenetic.GetFitnessMultiplier: Double;
begin
  Result := FFitnessMultiplier;
end;

function TcaGoalSeekerGenetic.GetLogging: Boolean;
begin
  Result := FLogging;
end;

function TcaGoalSeekerGenetic.GetMaxGenerations: Integer;
begin
  Result := FMaxGenerations;
end;

function TcaGoalSeekerGenetic.GetMaxPopulation: Integer;
begin
  Result := FMaxPopulation;
end;

function TcaGoalSeekerGenetic.GetMutationExtent: Integer;
begin
  Result := FMutationExtent;
end;

function TcaGoalSeekerGenetic.GetMutationRate: Integer;
begin
  Result := FMutationRate;
end;

function TcaGoalSeekerGenetic.GetPopulation: TcaPopulation;
begin
  Result := FPopulation;
end;

function TcaGoalSeekerGenetic.GetRandSeed: Integer;
begin
  Result := FRandSeed;
end;

function TcaGoalSeekerGenetic.GetTargetFitness: Integer;
begin
  Result := FTargetFitness;
end;

procedure TcaGoalSeekerGenetic.SetAlienExtent(const Value: Integer);
begin
  FAlienExtent := Value;
end;

procedure TcaGoalSeekerGenetic.SetAlienRate(const Value: Integer);
begin
  FAlienRate := Value;
end;

procedure TcaGoalSeekerGenetic.SetAllowDuplicates(const Value: Boolean);
begin
  FAllowDuplicates := Value;
end;

procedure TcaGoalSeekerGenetic.SetChildCount(const Value: Integer);
begin
  FChildCount := Value;
end;

procedure TcaGoalSeekerGenetic.SetChromozomeSize(const Value: Integer);
begin
  FChromozomeSize := Value;
end;

procedure TcaGoalSeekerGenetic.SetCrossoverRate(const Value: Integer);
begin
  FCrossoverRate := Value;
end;

procedure TcaGoalSeekerGenetic.SetFitnessMultiplier(const Value: Double);
begin
  FFitnessMultiplier := Value;
end;

procedure TcaGoalSeekerGenetic.SetLogging(const Value: Boolean);
begin
  FLogging := Value;
end;

procedure TcaGoalSeekerGenetic.SetMaxGenerations(const Value: Integer);
begin
  FMaxGenerations := Value;
end;

procedure TcaGoalSeekerGenetic.SetMaxPopulation(const Value: Integer);
begin
  FMaxPopulation := Value;
end;

procedure TcaGoalSeekerGenetic.SetMutationExtent(const Value: Integer);
begin
  FMutationExtent := Value;
end;

procedure TcaGoalSeekerGenetic.SetMutationRate(const Value: Integer);
begin
  FMutationRate := Value;
end;

procedure TcaGoalSeekerGenetic.SetPopulation(const Value: TcaPopulation);
begin
  FPopulation := Value;
end;

procedure TcaGoalSeekerGenetic.SetRandSeed(const Value: Integer);
begin
  FRandSeed := Value;
end;

procedure TcaGoalSeekerGenetic.SetTargetFitness(const Value: Integer);
begin
  FTargetFitness := Value;
end;

  //---------------------------------------------------------------------------
  // TcaGoalSeeker                                                             
  //---------------------------------------------------------------------------

procedure TcaGoalSeeker.AfterConstruction;
begin
  inherited;
  FEvalFitnesses := TcaDoubleVector.Create;
  FEvalParams := TcaDoubleVector.Create;
  FEvalResults := TcaDoubleVector.Create;
  FPopulation := TcaPopulation.Create;
  FPopulation.OnEvaluate := GoalSeekEvaluate;
  FPopulation.OnPopulationInitialized := PopulationInitializedEvent;
  FPopulation.OnGenerationCompleted := GenerationCompletedEvent;
  FGenetic := TcaGoalSeekerGenetic.CreateNonRefCounted;
  FGenetic.Population := FPopulation;
  FBinarySearch := TcaGoalSeekerBinarySearch.CreateNonRefCounted;
  FBruteForce := TcaGoalSeekerBruteForce.CreateNonRefCounted;
  InitializeDefaults;
end;

destructor TcaGoalSeeker.Destroy;
begin
  FBinarySearch.Free;
  FBruteForce.Free;
  FGenetic.Free;
  inherited;
end;

  // Interface methods 

function TcaGoalSeeker.Solve: Boolean;
begin
  Reset;
  Result := False;
  case FMethod of
    smBruteForce:   Result := SolveUsingBruteForce;
    smBinarySearch: Result := SolveUsingBinarySearch;
    smGenetic:      Result := SolveUsingGenetic;
  end;
end;

  // Protected methods 

procedure TcaGoalSeeker.DoEvaluate(AParam: Double; var AResult: Double);
begin
  if Assigned(FOnEvaluate) then FOnEvaluate(Self, AParam, AResult);
end;

  // Private methods 

function TcaGoalSeeker.Evaluate(AParam: Double): Double;
begin
  Inc(FIterations);
  DoEvaluate(AParam, Result);
end;

function TcaGoalSeeker.SolveUsingBinarySearch;
var
  EvalResult: Double;
  Lower: Double;
  MaxEvalResult: Double;
  MinEvalResult: Double;
  Param: Double;
  Reversed: Boolean;
  Solving: Boolean;
  Upper: Double;
  UpperNeeded: Boolean;
begin
  Result := False;
  MinEvalResult := Evaluate(FMinParam);
  MaxEvalResult := Evaluate(FMaxParam);
  Reversed := MinEvalResult > MaxEvalResult;
  Lower := FMinParam;
  Upper := FMaxParam;
  Solving := True;
  while Solving do
    begin
      Param := Lower + ((Upper - Lower) / 2);
      EvalResult := Evaluate(Param);
      if Abs(EvalResult - FTarget) < FTolerance then
        begin
          FEvalParams.Add(Param);
          FEvalResults.Add(EvalResult);
          Result := True;
          Break;
        end;
      if Reversed then
        UpperNeeded := EvalResult < (FTarget - FTolerance)
      else
        UpperNeeded := EvalResult > (FTarget + FTolerance);
      if UpperNeeded then
        Upper := Param
      else
        Lower := Param;
    end;
end;

function TcaGoalSeeker.SolveUsingBruteForce;
var
  EvalResult: Double;
  Param: Double;
  StepSize: Double;
begin
  Result := False;
  Param := FMinParam;
  StepSize := FBruteForce.StepSize;
  while Param <= FMaxParam do
    begin
      EvalResult := Evaluate(Param);
      if Abs(EvalResult - FTarget) < FTolerance then
        begin
          FEvalParams.Add(Param);
          FEvalResults.Add(EvalResult);
          Result := True;
          Break;
        end;
      Param := Param + StepSize;
    end;
end;

function TcaGoalSeeker.SolveUsingGenetic;
var
  FitnessGroups: TcaRankedGroups;
  Group: TcaOrganismGroup;
  GroupIndex: Integer;
  Organism: TcaOrganism;
  OrganismIndex: Integer;
begin
  Result := False;
  FPopulationInitialized := False;
  UpdateGeneticPopulationParameters;
  FPopulation.GeneMinValue := FMinParam;
  FPopulation.GeneMaxValue := FMaxParam;
  FPopulation.Run;
  FitnessGroups := FPopulation.FitnessGroups;
  FitnessGroups.Sort(sdAscending);
  for GroupIndex := 0 to Pred(FitnessGroups.Count) do
    begin
      Group := FitnessGroups[GroupIndex];
      for OrganismIndex := 0 to Pred(Group.Count) do
        begin
          Organism := Group[OrganismIndex];
          FEvalFitnesses.Add(Organism.Fitness);
          FEvalParams.Add(Organism.EvalParam);
          FEvalResults.Add(Organism.EvalResult);
          Result := True;
        end;
    end;
end;

procedure TcaGoalSeeker.AdjustParams;
var
  BestOrganism: TcaOrganism;
  FitnessGroups: TcaRankedGroups;
  Group: TcaOrganismGroup;
  GroupIndex: Integer;
  LowestParam: Double;
  Organism: TcaOrganism;
  OrganismIndex: Integer;
begin
  BestOrganism := nil;
  LowestParam := cMaxDouble;
  FitnessGroups := FPopulation.FitnessGroups;
  for GroupIndex := 0 to Pred(FitnessGroups.Count) do
    begin
      Group := FitnessGroups[GroupIndex];
      for OrganismIndex := 0 to Pred(Group.Count) do
        begin
          Organism := Group[OrganismIndex];
          if BestOrganism = nil then BestOrganism := Organism;
          if Organism.EvalResult < FTarget then
            begin
              if Organism.EvalParam < LowestParam then
                LowestParam := Organism.EvalParam;
            end;
        end;
    end;
  if BestOrganism <> nil then
    FPopulation.GeneMaxValue := BestOrganism.EvalParam * 1.1;
  if LowestParam < (cMaxDouble / 10) then
    FPopulation.GeneMinValue := LowestParam * 0.9;
end;

procedure TcaGoalSeeker.CalibrateErrorBenchmark;
var
  BestOrganism: TcaOrganism;
  Error: Double;
  FitnessGroups: TcaRankedGroups;
  Group: TcaOrganismGroup;
  GroupIndex: Integer;
  Organism: TcaOrganism;
  OrganismIndex: Integer;
begin
  BestOrganism := nil;
  FitnessGroups := FPopulation.FitnessGroups;
  for GroupIndex := 0 to Pred(FitnessGroups.Count) do
    begin
      Group := FitnessGroups[GroupIndex];
      for OrganismIndex := 0 to Pred(Group.Count) do
        begin
          Organism := Group[OrganismIndex];
          if BestOrganism = nil then
            begin
              BestOrganism := Organism;
              FErrorBenchmark := Abs(FTarget - Organism.EvalResult) * FGenetic.FitnessMultiplier;
            end;
          Error := Abs(FTarget - Organism.EvalResult);
          Organism.Fitness := Error / FErrorBenchmark * FGenetic.FitnessMultiplier;
        end;
    end;
  FPopulation.RebuildFitnessGroups;
end;

procedure TcaGoalSeeker.InitializeDefaults;
begin
  FMinParam := cMinDouble;
  FMaxParam := cMaxDouble;
  FMethod := smBruteForce;
  FTolerance := 1E-8;
end;

procedure TcaGoalSeeker.Reset;
begin
  FEvalFitnesses.Clear;
  FEvalParams.Clear;
  FEvalResults.Clear;
  FIterations := 0;
end;

procedure TcaGoalSeeker.UpdateGeneticPopulationParameters;
begin
  FPopulation.AlienExtent := FGenetic.AlienExtent;
  FPopulation.AlienRate := FGenetic.AlienRate;
  FPopulation.AllowDuplicates := FGenetic.AllowDuplicates;
  FPopulation.ChildCount := FGenetic.ChildCount;
  FPopulation.ChromozomeSize := FGenetic.ChromozomeSize;
  FPopulation.CrossoverRate := FGenetic.CrossoverRate;
  FPopulation.Logging := FGenetic.Logging;
  FPopulation.MaxGenerations := FGenetic.MaxGenerations;
  FPopulation.MaxPopulation := FGenetic.MaxPopulation;
  FPopulation.MutationExtent := FGenetic.MutationExtent;
  FPopulation.MutationRate := FGenetic.MutationRate;
  FPopulation.RandSeed := FGenetic.RandSeed;
  FPopulation.TargetFitness := FGenetic.TargetFitness;
  FPopulation.Tolerance := FTolerance;
end;

  // Event handlers 

procedure TcaGoalSeeker.GoalSeekEvaluate(Sender: TObject; AOrganism: TcaOrganism; var AFitness: Double);
var
  Error: Double;
  EvalResult: Double;
  FitnessMultiplier: Double;
  Gene: Double;
  Index: Integer;
  Param: Double;
begin
  Param := 0;
  for Index := 0 to Pred(FPopulation.ChromozomeSize) do
    begin
      Gene := AOrganism.Genes[Index];
      Param := Param + Gene;
    end;
  Param := Param / FPopulation.ChromozomeSize;
  EvalResult := Evaluate(Param);
  AOrganism.EvalParam := Param;
  AOrganism.EvalResult := EvalResult;
  FitnessMultiplier := FGenetic.FitnessMultiplier;
  // AFitness := (Max(FTarget, EvalResult) / Min(FTarget, EvalResult)) * FitnessMultiplier - FitnessMultiplier; 
  Error := Abs(FTarget - EvalResult);
  if FPopulationInitialized then
    AFitness := Error / FErrorBenchmark * FitnessMultiplier
  else
    AFitness := Error;
end;

procedure TcaGoalSeeker.GenerationCompletedEvent(Sender: TObject);
begin
  if FAutoAdjustParams then AdjustParams;
end;

procedure TcaGoalSeeker.PopulationInitializedEvent(Sender: TObject);
begin
  FPopulationInitialized := True;
  CalibrateErrorBenchmark;
  if FAutoAdjustParams then AdjustParams;
end;

  // Property methods 

function TcaGoalSeeker.GetAutoAdjustParams: Boolean;
begin
  Result := FAutoAdjustParams;
end;

function TcaGoalSeeker.GetEvalFitnesses: IcaDoubleVector;
begin
  Result := FEvalFitnesses;
end;

function TcaGoalSeeker.GetEvalParams: IcaDoubleVector;
begin
  Result := FEvalParams;
end;

function TcaGoalSeeker.GetEvalResults: IcaDoubleVector;
begin
  Result := FEvalResults;
end;

function TcaGoalSeeker.GetIterations: Integer;
begin
  Result := FIterations;
end;

function TcaGoalSeeker.GetMaxParam: Double;
begin
  Result := FMaxParam;
end;

function TcaGoalSeeker.GetMethod: TcaGoalSeekMethods;
begin
  Result := FMethod;
end;

function TcaGoalSeeker.GetMinParam: Double;
begin
  Result := FMinParam;
end;

function TcaGoalSeeker.GetTarget: Double;
begin
  Result := FTarget;
end;

function TcaGoalSeeker.GetTolerance: Double;
begin
  Result := FTolerance;
end;

procedure TcaGoalSeeker.SetAutoAdjustParams(const Value: Boolean);
begin
  FAutoAdjustParams := Value;
end;

procedure TcaGoalSeeker.SetIterations(const Value: Integer);
begin
  FIterations := Value;
end;

procedure TcaGoalSeeker.SetMaxParam(const Value: Double);
begin
  FMaxParam := Value;
end;

procedure TcaGoalSeeker.SetMethod(const Value: TcaGoalSeekMethods);
begin
  FMethod := Value;
end;

procedure TcaGoalSeeker.SetMinParam(const Value: Double);
begin
  FMinParam := Value;
end;

procedure TcaGoalSeeker.SetTarget(const Value: Double);
begin
  FTarget := Value;
end;

procedure TcaGoalSeeker.SetTolerance(const Value: Double);
begin
  FTolerance := Value;
end;

  // Event property methods 

function TcaGoalSeeker.GetOnEvaluate: TcaGoalSeekEvaluateEvent;
begin
  Result := FOnEvaluate;
end;

procedure TcaGoalSeeker.SetOnEvaluate(const Value: TcaGoalSeekEvaluateEvent);
begin
  FOnEvaluate := Value;
end;

end.


