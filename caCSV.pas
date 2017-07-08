unit caCSV;

interface

uses

  // standard delphi units 
  Classes,
  Sysutils,

  // ca units 
  caLog,
  caTextStream;

type

  //---------------------------------------------------------------------------
  // TcaCSVStream                                                              
  //---------------------------------------------------------------------------

  TcaCSVStreamReadLineEvent = procedure(Sender: TObject; const AFileName: string; AIndex: Integer;
                                        AColumnNames, AValues: TStrings; var AContinue: Boolean) of object;

  TcaCSVStreamProgressEvent = procedure(Sender: TObject; AProgressPct: Integer) of object;

  TcaCSVStream = class(TcaTextStream)
  private
    // private fields 
    FErrorLog: TStrings;
    FFirstLineIsColumnNames: Boolean;
    FValues: TStrings;
    // property fields 
    FColumnNames: TStrings;
    // event property fields 
    FOnProgress: TcaCSVStreamProgressEvent;
    FOnReadLine: TcaCSVStreamReadLineEvent;
    // private methods 
    procedure AssignCommaText(AStrings: TStrings; const ACommaText: string);
  protected
    // protected methods 
    procedure DoProgress(AProgressPct: Integer); virtual;
    procedure DoReadLine(AIndex: Integer; var AContinue: Boolean); virtual;
  public
    // create/destroy 
    procedure AfterConstruction; override;
    destructor Destroy; override;
    // public methods 
    function HasErrors: Boolean;
    procedure Load;
    // public properties 
    property ErrorLog: TStrings read FErrorLog;
    property FirstLineIsColumnNames: Boolean read FFirstLineIsColumnNames write FFirstLineIsColumnNames;
    // public event properties 
    property OnProgress: TcaCSVStreamProgressEvent read FOnProgress write FOnProgress;
    property OnReadLine: TcaCSVStreamReadLineEvent read FOnReadLine write FOnReadLine;
  end;

implementation

const
  cBufferSize: Integer = 8192;

  //---------------------------------------------------------------------------
  // TcaCSVStream                                                              
  //---------------------------------------------------------------------------

  // create/destroy 

procedure TcaCSVStream.AfterConstruction;
begin
  inherited;
  FColumnNames := TStringList.Create;
  FErrorLog := TStringList.Create;
  FFirstLineIsColumnNames := True;
  FValues := TStringList.Create;
end;

destructor TcaCSVStream.Destroy;
begin
  FColumnNames.Free;
  FErrorLog.Free;
  FValues.Free;
  inherited;
end;

  // public methods 

function TcaCSVStream.HasErrors: Boolean;
begin
  Result := FErrorLog.Count > 0;
end;

procedure TcaCSVStream.Load;
var
  ColCount: Integer;
  Continue: Boolean;
  Index: Integer;
  Line: string;
  ValCount: Integer;
  TotalSize: Int64;
  BytesRead: Int64;
  ProgressPct: Integer;
  LastProgressPct: Integer;
begin
  Index := 0;
  Seek(0, soFromBeginning);
  FErrorLog.Clear;
  TotalSize := Size;
  BytesRead := 0;
  LastProgressPct := 0;
  while not IsEOF do
    begin
      Line := GetLine;
      BytesRead := BytesRead + Length(Line) + 2;
      ProgressPct := Round(BytesRead / TotalSize * 100);
      if ProgressPct <> LastProgressPct then
        begin
          DoProgress(ProgressPct);
          LastProgressPct := ProgressPct;
        end;
      if FFirstLineIsColumnNames and (Index = 0) then
        AssignCommaText(FColumnNames, Line)
      else
        begin
          AssignCommaText(FValues, Line);
          Continue := True;
          DoReadLine(Index, Continue);
          if not Continue then Break;
          if FColumnNames.Count <> FValues.Count then
            begin
              ColCount := FColumnNames.Count;
              ValCount := FValues.Count;
              FErrorLog.Add(Sysutils.Format('line: %d, columns: %d, values: %d', [Index, ColCount, ValCount]));
            end;
        end;
      Inc(Index);
    end;
end;

  // protected methods 

procedure TcaCSVStream.DoProgress(AProgressPct: Integer);
begin
  if Assigned(FOnProgress) then FOnProgress(Self, AProgressPct);
end;

procedure TcaCSVStream.DoReadLine(AIndex: Integer; var AContinue: Boolean);
begin
  if Assigned(FOnReadLine) then FOnReadLine(Self, Filename, AIndex, FColumnNames, FValues, AContinue);
end;

  // private methods 

procedure TcaCSVStream.AssignCommaText(AStrings: TStrings; const ACommaText: string);
var
  P: PChar;
  P1: PChar;
  S: string;
begin
  AStrings.BeginUpdate;
  try
    AStrings.Clear;
    P := PChar(ACommaText);
    while P^ <> #0 do
      begin
        if P^ = '"' then
          S := AnsiExtractQuotedStr(P, '"')
        else
          begin
            P1 := P;
            while (P^ <> ',') and (P^ <> #0) do Inc(P);
            SetString(S, P1, P - P1);
          end;
        AStrings.Add(S);
        if P^ = ',' then Inc(P);
        while P^ = #32 do Inc(P);
      end;
  finally
    AStrings.EndUpdate;
  end;    
end;

end.
