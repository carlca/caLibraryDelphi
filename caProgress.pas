unit caProgress;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  Math,

  // ca units
  caButtons,
  caControls,
  caTypes,
  caUtils;

const
  cTimeMultiplier = 1000000;

type

 //---------------------------------------------------------------------------
 // TcaProgressBar
 //---------------------------------------------------------------------------

  TcaProgressBarLayout = (blOneRow, blTwoRows);

  TcaProgressBarTextLayout = (tlLeft, tlTop);

  TcaProgressBar = class(TcaPanel)
  private
    FBarColor: TColor;
    FBarFrameColor: TColor;
    FFitToWidestText: Boolean;
    FLayout: TcaProgressBarLayout;
    FProcessBarActive: Boolean;
    FProcessName: String;
    FProcessPosition: Integer;
    FProcessTarget: Integer;
    FProgressName: String;
    FProgressPosition: Integer;
    FProgressTarget: Integer;
    FTextLayout: TcaProgressBarTextLayout;
    // Property methods
    function GetBarColor: TColor;
    function GetBarFrameColor: TColor;
    function GetFitToWidestText: Boolean;
    function GetLayout: TcaProgressBarLayout;
    function GetProcessBarActive: Boolean;
    function GetProcessName: String;
    function GetProcessPosition: Integer;
    function GetProcessTarget: Integer;
    function GetProgressName: String;
    function GetProgressPosition: Integer;
    function GetProgressTarget: Integer;
    function GetTextLayout: TcaProgressBarTextLayout;
    procedure SetBarColor(const Value: TColor);
    procedure SetBarFrameColor(const Value: TColor);
    procedure SetFitToWidestText(const Value: Boolean);
    procedure SetLayout(const Value: TcaProgressBarLayout);
    procedure SetProcessBarActive(const Value: Boolean);
    procedure SetProcessName(const Value: String);
    procedure SetProcessPosition(const Value: Integer);
    procedure SetProcessTarget(const Value: Integer);
    procedure SetProgressName(const Value: String);
    procedure SetProgressPosition(const Value: Integer);
    procedure SetProgressTarget(const Value: Integer);
    procedure SetTextLayout(const Value: TcaProgressBarTextLayout);
    // Private methods
    procedure ShowHourglassCursor;
    procedure ShowDefaultCursor;
    procedure UpdateMainFormCursor(ACursor: TCursor);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    // Public methods
    procedure Activate;
    procedure BeginProcess(const AProcessName: String; ATarget: Integer = 0);
    procedure BeginProgress(const AProgressName: String; ATarget: Integer = 0);
    procedure Deactivate;
    procedure EndProcess;
    procedure EndProgress;
    procedure Hide;
    procedure Show;
    procedure StepProcess(const AProcessName: String = '');
    procedure StepProgress(const AProgressName: String = '');
  published
    // Promoted properties
    property Font;
    // Properties
    property BarColor: TColor read GetBarColor write SetBarColor;
    property BarFrameColor: TColor read GetBarFrameColor write SetBarFrameColor;
    property FitToWidestText: Boolean read GetFitToWidestText write SetFitToWidestText;
    property Layout: TcaProgressBarLayout read GetLayout write SetLayout;
    property ProcessBarActive: Boolean read GetProcessBarActive write SetProcessBarActive;
    property ProcessName: String read GetProcessName write SetProcessName;
    property ProcessPosition: Integer read GetProcessPosition write SetProcessPosition;
    property ProcessTarget: Integer read GetProcessTarget write SetProcessTarget;
    property ProgressName: String read GetProgressName write SetProgressName;
    property ProgressPosition: Integer read GetProgressPosition write SetProgressPosition;
    property ProgressTarget: Integer read GetProgressTarget write SetProgressTarget;
    property TextLayout: TcaProgressBarTextLayout read GetTextLayout write SetTextLayout;
  end;

 //---------------------------------------------------------------------------
 // IcaProgress
 //---------------------------------------------------------------------------

  IcaProgress = interface
  ['{343E6B6F-8D8C-42BE-AD7A-EF0E51098875}']
    // Property methods
    function GetActive: Boolean;
    function GetProcessNames: TStringList;
    function GetProgressBar: TcaProgressBar;
    function GetProgressNames: TStringList;
    procedure SetActive(const Value: Boolean);
    procedure SetProgressBar(const Value: TcaProgressBar);
    // Public methods
    procedure AddProcessName(const AProcessName: String; ATarget: Integer = 0);
    procedure AddProgressName(const AProgressName: String; ATarget: Integer);
    procedure BeginFirstProgress;
    procedure BeginFirstProcess;
    procedure BeginNextProgress;
    procedure BeginNextProcess;
    procedure BeginProcess(const AProcessName: String; ATarget: Integer = 0);
    procedure BeginProgress(const AProgressName: String; ATarget: Integer = 0);
    procedure ClearProcessNames;
    procedure ClearProgressNames;
    procedure EndProcess;
    procedure EndProgress;
    procedure StepProcess(ADelta: Integer = 0; ADelay: Integer = 0; const AProcessName: String = '');
    procedure StepProcessUntil(ANewPosition, ADelay: Integer; const AProcessName: String = '');
    procedure StepProgress(ADelta: Integer = 0; ADelay: Integer = 0; const AProgressName: String = '');
    procedure StepProgressUntil(ANewPosition, ADelay: Integer; const AProgressName: String = '');
    // Protected properties
    property ProgressBar: TcaProgressBar read GetProgressBar write SetProgressBar;
    property ProcessNames: TStringList read GetProcessNames;
    property ProgressNames: TStringList read GetProgressNames;
    // Public property
    property Active: Boolean read GetActive write SetActive;
  end;

 //---------------------------------------------------------------------------
 // TcaProgress
 //---------------------------------------------------------------------------

  TcaProgress = class(TInterfacedObject, IcaProgress)
  private
    // Private fields
    FActive: Boolean;
    FCurrentProcess: Integer;
    FCurrentProgress: Integer;
    FProgressBar: TcaProgressBar;
    FProgressNames: TStringList;
    FProcessNames: TStringList;
    // Property methods
    function GetActive: Boolean;
    function GetProcessNames: TStringList;
    function GetProgressBar: TcaProgressBar;
    function GetProgressNames: TStringList;
    procedure SetActive(const Value: Boolean);
    procedure SetProgressBar(const Value: TcaProgressBar);
    // Private methods
    function CheckActive: Boolean;
    procedure BeginCurrentProcess(Index: Integer);
    procedure BeginCurrentProgress(Index: Integer);
    procedure CreateListObjects;
    procedure FreeListObjects;
    procedure Initialize;
  protected
    property ProgressBar: TcaProgressBar read GetProgressBar write SetProgressBar;
    property ProcessNames: TStringList read GetProcessNames;
    property ProgressNames: TStringList read GetProgressNames;
  public
    constructor Create;
    destructor Destroy; override;
    // Public methods
    procedure AddProcessName(const AProcessName: String; ATarget: Integer = 0);
    procedure AddProgressName(const AProgressName: String; ATarget: Integer);
    procedure BeginFirstProgress;
    procedure BeginFirstProcess;
    procedure BeginNextProgress;
    procedure BeginNextProcess;
    procedure BeginProcess(const AProcessName: String; ATarget: Integer = 0);
    procedure BeginProgress(const AProgressName: String; ATarget: Integer = 0);
    procedure ClearProcessNames;
    procedure ClearProgressNames;
    procedure EndProcess;
    procedure EndProgress;
    procedure StepProcess(ADelta: Integer = 0; ADelay: Integer = 0; const AProcessName: String = '');
    procedure StepProcessUntil(ANewPosition, ADelay: Integer; const AProcessName: String = '');
    procedure StepProgress(ADelta: Integer = 0; ADelay: Integer = 0; const AProgressName: String = '');
    procedure StepProgressUntil(ANewPosition, ADelay: Integer; const AProgressName: String = '');
    // Public properties
    property Active: Boolean read GetActive write SetActive;
  end;

var
  Progress: IcaProgress = nil;

implementation

 //---------------------------------------------------------------------------
 // TcaProgressBar
 //---------------------------------------------------------------------------

 // Public methods

constructor TcaProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  FBarColor := clHighlight;
  FBarFrameColor := clBtnShadow;
  FProgressTarget := 100;
  FProcessTarget := 100;
end;

procedure TcaProgressBar.Activate;
begin
  Progress.ProgressBar := Self;
end;

procedure TcaProgressBar.BeginProcess(const AProcessName: String; ATarget: Integer);
begin
  Show;
  FProcessName := AProcessName;
  FProcessTarget := ATarget;
  FProcessPosition := 0;
  Invalidate;
  Application.ProcessMessages;
end;

procedure TcaProgressBar.BeginProgress(const AProgressName: String; ATarget: Integer);
begin
  ShowHourglassCursor;
  Show;
  FProgressName := AProgressName;
  FProgressTarget := ATarget;
  FProgressPosition := 0;
  Invalidate;
  Application.ProcessMessages;
end;

procedure TcaProgressBar.Deactivate;
begin
  Progress.ProgressBar := nil;
end;

procedure TcaProgressBar.EndProcess;
begin
  FProcessName := '';
  Invalidate;
end;

procedure TcaProgressBar.EndProgress;
begin
  Sleep(200);
  Hide;
  FProcessName := '';
  ShowDefaultCursor;
  Screen.Cursor := crDefault;
end;

procedure TcaProgressBar.Hide;
begin
  Visible := False;
end;

procedure TcaProgressBar.Show;
begin
  Visible := True;
end;

procedure TcaProgressBar.StepProcess(const AProcessName: String = '');
begin
  Inc(FProcessPosition);
  FProcessPosition := Min(FProcessPosition, FProcessTarget);
  if AProcessName <> '' then FProcessName := AProcessName;
  Invalidate;
  Application.ProcessMessages;
end;

procedure TcaProgressBar.StepProgress(const AProgressName: String = '');
begin
  Inc(FProgressPosition);
  FProgressPosition := Min(FProgressPosition, FProgressTarget);
  if AProgressName <> '' then FProgressName := AProgressName;
  Invalidate;
  Application.ProcessMessages;
end;

 // Protected methods

procedure TcaProgressBar.Paint;
var
  TotalBarWidth: Integer;
  C: TCanvas;
  ProcessRect: TRect;
  ProcessWidth: Integer;
  ProgressRect: TRect;
  ProgressWidth: Integer;
  R: TRect;
  TextWidth: Integer;
  TotalTextWidth: Integer;
  MathUtils: IcaMathUtils;

  function GetWidestWidth(AList: TStringList): Integer;
  var
    Index: Integer;
  begin
    Result := 0;
    for Index := 0 to AList.Count - 1 do
      Result := Max(Result, C.TextWidth(AList[Index]));
  end;

  function GetWidestProcessWidth: Integer;
  begin
    Result := GetWidestWidth(Progress.ProcessNames);
  end;

  function GetWidestProgressWidth: Integer;
  begin
    Result := GetWidestWidth(Progress.ProgressNames);
  end;

  function CalcTextHeight: Integer;
  begin
    Result := C.TextHeight('Ay');
  end;

  procedure CalcTextCoordinates(ARect: TRect; const AName: String; ATextWidth: Integer; var ATextX, ATextY: Integer);
  var
    TextHeight: Integer;
  begin
    TextHeight := CalcTextHeight;
    if FTextLayout = tlLeft then
      begin
        if ATextWidth <> 0 then
          ATextX := ARect.Left + 4 + ATextWidth - C.TextWidth(AName)
        else
          ATextX := ARect.Left + 4;
        ATextY := ((ARect.Top + ARect.Bottom) div 2) - (TextHeight div 2) - 1
      end
    else
      begin
        ATextX := ARect.Left + 4;
        ATextY := Max(((ARect.Top + ARect.Bottom) div 2) - Round(((TextHeight div 2) * 2.5)), 1);
      end;
  end;

  procedure DrawText(ARect: TRect; const AName: String; ATextWidth: Integer);
  var
    TextX: Integer;
    TextY: Integer;
  begin
    CalcTextCoordinates(ARect, AName, ATextWidth, TextX, TextY);
    C.Brush.Color := Color;
    C.TextOut(TextX, TextY, AName);
    C.Brush.Color := FBarFrameColor;
  end;

  function CalcBarRect(ARect: TRect; const AName: String; ATextWidth: Integer; ATextX, ATextY: Integer): TRect;
  var
    TextHeight: Integer;
  begin
    TextHeight := CalcTextHeight;
    Result := ARect;
    Result.Left := ATextX;
    if FTextLayout = tlLeft then
      begin
        Result := ARect;
        Result.Left := ATextX;
        if AName <> '' then
          Result.Left := Result.Left + C.TextWidth(AName) + 4;
        Result.Right := Result.Right - 6;
        Result.Top := ATextY + 2;
        Result.Bottom := Result.Top + TextHeight - 3;
      end
    else
      begin
        Result := ARect;
        Result.Left := ATextX;
        Result.Right := Result.Right - 6;
        Result.Top := Max((((ARect.Top + ARect.Bottom) div 5) * 3) - 2, ATextY + TextHeight + 2);
        Result.Bottom := Result.Top + TextHeight - 5;
      end;
  end;

  procedure DrawBarFrame(ARect: TRect; const AName: String; ATextWidth: Integer);
  var
    BarRect: TRect;
    TextX: Integer;
    TextY: Integer;
  begin
    CalcTextCoordinates(ARect, AName, ATextWidth, TextX, TextY);
    BarRect := CalcBarRect(ARect, AName, ATextWidth, TextX, TextY);
    C.FrameRect(BarRect);
  end;

  procedure DrawBarInterior(ARect: TRect; const AName: String; APosition, ATarget, ATextWidth: Integer);
  var
    BarRect: TRect;
    BarWidth: Integer;
    FullBarWidth: Integer;
    TextX: Integer;
    TextY: Integer;
  begin
    CalcTextCoordinates(ARect, AName, ATextWidth, TextX, TextY);
    BarRect := CalcBarRect(ARect, AName, ATextWidth, TextX, TextY);
    OffsetRect(BarRect, 1, 1);
    BarRect.Right := BarRect.Right - 2;
    FullBarWidth := BarRect.Right - BarRect.Left;
    BarWidth := Min(FullBarWidth, MathUtils.EquiRound(MathUtils.FloatDiv(FullBarWidth * APosition, ATarget, 2)));
    BarRect.Right := BarRect.Left + BarWidth;
    BarRect.Bottom := BarRect.Bottom - 2;
    C.Brush.Color := FBarColor;
    C.FillRect(BarRect);
  end;

  procedure PaintBar(ARect: TRect; const AName: String; APosition, ATarget, ATextWidth: Integer);
  begin
    DrawText(ARect, AName, ATextWidth);
    DrawBarFrame(ARect, AName, ATextWidth);
    DrawBarInterior(ARect, AName, APosition, ATarget, ATextWidth);
  end;

begin
  inherited;
  MathUtils := Utils as IcaMathUtils;
  C := GetOffScreenCanvas;
  C.Font.Assign(Font);
  R := ClientRect;
  ProgressRect := ClientRect;
  ProcessRect := ClientRect;
  if FFitToWidestText then
    begin
      ProcessWidth := GetWidestProcessWidth;
      ProgressWidth := GetWidestProgressWidth;
    end
  else
    begin
      if FTextLayout = tlLeft then
        begin
          ProcessWidth := C.TextWidth(FProcessName);
          ProgressWidth := C.TextWidth(FProgressName);
        end
      else
        begin
          ProcessWidth := ClientWidth div 2;
          ProgressWidth := ProcessWidth;
        end;
    end;
  TextWidth := 0;
  if FProcessBarActive then
    begin
      case FLayout of
        blOneRow:
          begin
            TotalTextWidth := ProcessWidth + ProgressWidth;
            TotalBarWidth := ClientWidth - TotalTextWidth;
            ProgressRect.Right := ProgressWidth + TotalBarWidth div 2;
            ProcessRect.Left := ProgressRect.Right + 1;
          end;
        blTwoRows:
          begin
            ProgressRect.Bottom := ProgressRect.Bottom div 2;
            ProcessRect.Top := ProgressRect.Bottom + 1;
            OffsetRect(ProgressRect, 0, 2);
            OffsetRect(ProcessRect, 0, -2);
            TextWidth := Max(ProcessWidth, ProgressWidth);
          end;
      end;
      PaintBar(ProgressRect, FProgressName, FProgressPosition, FProgressTarget, TextWidth);
      PaintBar(ProcessRect, FProcessName, FProcessPosition, FProcessTarget, TextWidth);
    end
  else
    PaintBar(ProgressRect, FProgressName, FProgressPosition, FProgressTarget, TextWidth);
  UpdateOnScreenBitmap;
end;

 // Private methods

procedure TcaProgressBar.ShowHourglassCursor;
begin
  Screen.Cursor := crHourGlass;
  UpdateMainFormCursor(crHourGlass);
end;

procedure TcaProgressBar.ShowDefaultCursor;
begin
  Screen.Cursor := crDefault;
  UpdateMainFormCursor(crDefault);
end;

procedure TcaProgressBar.UpdateMainFormCursor(ACursor: TCursor);
var
  Form: TForm;
  Index: Integer;
begin
  Form := Application.MainForm;
  if Form <> nil then
    begin
      for Index := 0 to Form.ComponentCount - 1 do
        begin
          if Form.Components[Index] is TControl then
            TControl(Form.Components[Index]).Cursor := ACursor;
        end;
    end;
end;

 // Property methods

function TcaProgressBar.GetBarColor: TColor;
begin
  Result := FBarColor;
end;

function TcaProgressBar.GetBarFrameColor: TColor;
begin
  Result := FBarFrameColor;
end;

function TcaProgressBar.GetFitToWidestText: Boolean;
begin
  Result := FFitToWidestText;
end;

function TcaProgressBar.GetLayout: TcaProgressBarLayout;
begin
  Result := FLayout;
end;

function TcaProgressBar.GetProcessBarActive: Boolean;
begin
  Result := FProcessBarActive;
end;

function TcaProgressBar.GetProcessName: String;
begin
  Result := FProcessName;
end;

function TcaProgressBar.GetProcessPosition: Integer;
begin
  Result := FProcessPosition;
end;

function TcaProgressBar.GetProcessTarget: Integer;
begin
  Result := FProcessTarget;
end;

function TcaProgressBar.GetProgressName: String;
begin
  Result := FProgressName;
end;

function TcaProgressBar.GetProgressPosition: Integer;
begin
  Result := FProgressPosition;
end;

function TcaProgressBar.GetProgressTarget: Integer;
begin
  Result := FProgressTarget;
end;

function TcaProgressBar.GetTextLayout: TcaProgressBarTextLayout;
begin
  Result := FTextLayout;
end;

procedure TcaProgressBar.SetBarColor(const Value: TColor);
begin
  if Value <> FBarColor then
    begin
      FBarColor := Value;
      Invalidate;
    end;                    
end;

procedure TcaProgressBar.SetBarFrameColor(const Value: TColor);
begin
  if Value <> FBarFrameColor then
    begin
      FBarFrameColor := Value;
      Invalidate;
    end;                  
end;

procedure TcaProgressBar.SetFitToWidestText(const Value: Boolean);
begin
  if Value <> FFitToWidestText then
    begin
      FFitToWidestText := Value;
      Invalidate;
    end;                  
end;

procedure TcaProgressBar.SetLayout(const Value: TcaProgressBarLayout);
begin
  if Value <> FLayout then
    begin
      FLayout := Value;
      Invalidate;
    end;
end;

procedure TcaProgressBar.SetProcessBarActive(const Value: Boolean);
begin
  if Value <> FProcessBarActive then
    begin
      FProcessBarActive := Value;
      Invalidate;
    end;                  
end;

procedure TcaProgressBar.SetProcessName(const Value: String);
begin
  if Value <> FProcessName then
    begin
      FProcessName := Value;
      Invalidate;
    end;
end;

procedure TcaProgressBar.SetProcessPosition(const Value: Integer);
begin
  if Value <> FProcessPosition then
    begin
      FProcessPosition := Value;
      Invalidate;
    end;
end;

procedure TcaProgressBar.SetProcessTarget(const Value: Integer);
begin
  if Value <> FProcessTarget then
    begin
      FProcessTarget := Value;
      Invalidate;
    end;
end;

procedure TcaProgressBar.SetProgressName(const Value: String);
begin
  if Value <> FProgressName then
    begin
      FProgressName := Value;
      Invalidate;
    end;
end;

procedure TcaProgressBar.SetProgressPosition(const Value: Integer);
begin
  if Value <> FProgressPosition then
    begin
      FProgressPosition := Value;
      Invalidate;
    end;
end;

procedure TcaProgressBar.SetProgressTarget(const Value: Integer);
begin
  if Value <> FProgressTarget then
    begin
      FProgressTarget := Value;
      Invalidate;
    end;
end;

procedure TcaProgressBar.SetTextLayout(const Value: TcaProgressBarTextLayout);
begin
  if Value <> FTextLayout then
    begin
      FTextLayout := Value;
      Invalidate;
    end;                  
end;

 //---------------------------------------------------------------------------
 // TcaProgress
 //---------------------------------------------------------------------------

constructor TcaProgress.Create;
begin
  inherited;
  CreateListObjects;
  Initialize;
end;

destructor TcaProgress.Destroy;
begin
  FreeListObjects;
  inherited;
end;

 // Public methods

procedure TcaProgress.AddProcessName(const AProcessName: String; ATarget: Integer);
begin
  FProcessNames.AddObject(AProcessName, Pointer(ATarget));
end;

procedure TcaProgress.AddProgressName(const AProgressName: String; ATarget: Integer);
begin
  FProgressNames.AddObject(AProgressName, Pointer(ATarget));
end;

procedure TcaProgress.BeginFirstProcess;
begin
  if FProcessNames.Count > 0 then
    begin
      FCurrentProcess := 0;
      BeginCurrentProcess(FCurrentProcess);
    end;
end;

procedure TcaProgress.BeginFirstProgress;
begin
  if FProgressNames.Count > 0 then
    begin
      FCurrentProgress := 0;
      BeginCurrentProgress(FCurrentProgress);
    end;
end;

procedure TcaProgress.BeginNextProcess;
begin
  if FProcessNames.Count > 0 then
    begin
      if FCurrentProcess < FProcessNames.Count - 1 then
        begin
          Inc(FCurrentProcess);
          BeginCurrentProcess(FCurrentProcess);
        end;
    end;
end;

procedure TcaProgress.BeginNextProgress;
begin
  if FProgressNames.Count > 0 then
    begin
      if FCurrentProgress < FProgressNames.Count - 1 then
        begin
          Inc(FCurrentProgress);
          BeginCurrentProgress(FCurrentProgress);
        end;
    end;
end;

procedure TcaProgress.BeginProcess(const AProcessName: String; ATarget: Integer = 0);
begin
  if CheckActive then FProgressBar.BeginProcess(AProcessName, ATarget);
end;

procedure TcaProgress.BeginProgress(const AProgressName: String; ATarget: Integer);
begin
  if CheckActive then FProgressBar.BeginProgress(AProgressName, ATarget);
end;

procedure TcaProgress.ClearProcessNames;
begin
  FProcessNames.Clear;
end;

procedure TcaProgress.ClearProgressNames;
begin
  FProgressNames.Clear;
end;

procedure TcaProgress.EndProgress;
begin
  if CheckActive then FProgressBar.EndProgress;
end;

procedure TcaProgress.EndProcess;
begin
  if CheckActive then FProgressBar.EndProcess;
end;

procedure TcaProgress.StepProgress(ADelta: Integer = 0; ADelay: Integer = 0; const AProgressName: String = '');
begin
  if CheckActive then
    begin
      if ADelta = 0 then
        FProgressBar.StepProgress(AProgressName)
      else
        StepProgressUntil(FProgressBar.ProgressPosition + ADelta, ADelay, AProgressName);
    end;
end;

procedure TcaProgress.StepProgressUntil(ANewPosition, ADelay: Integer; const AProgressName: String = '');
begin
  if CheckActive then
    while FProgressBar.ProgressPosition < ANewPosition do
      begin
        FProgressBar.StepProgress(AProgressName);
        Sleep(ADelay);
      end;
end;

procedure TcaProgress.StepProcess(ADelta: Integer = 0; ADelay: Integer = 0; const AProcessName: String = '');
begin
  if CheckActive then
    begin
      if ADelta = 0 then
        FProgressBar.StepProcess(AProcessName)
      else
        StepProcessUntil(FProgressBar.ProcessPosition + ADelta, ADelay, AProcessName);
    end;
end;

procedure TcaProgress.StepProcessUntil(ANewPosition, ADelay: Integer; const AProcessName: String = '');
begin
  if CheckActive then
    while FProgressBar.ProcessPosition < ANewPosition do
      begin
        FProgressBar.StepProcess(AProcessName);
        Sleep(ADelay);
      end;
end;

 // Private methods

function TcaProgress.CheckActive: Boolean;
begin
  Result := FActive and (FProgressBar <> nil);
end;

procedure TcaProgress.BeginCurrentProcess(Index: Integer);
begin
  if (FCurrentProcess >= 0) and (FCurrentProcess < FProcessNames.Count) then
    BeginProcess(FProcessNames[FCurrentProcess], Integer(FProcessNames.Objects[FCurrentProcess]));
end;

procedure TcaProgress.BeginCurrentProgress(Index: Integer);
begin
  if (FCurrentProgress >= 0) and (FCurrentProgress < FProgressNames.Count) then
    BeginProgress(FProgressNames[FCurrentProgress], Integer(FProgressNames.Objects[FCurrentProgress]));
end;

procedure TcaProgress.CreateListObjects;
begin
  FProgressNames := TStringList.Create;
  FProcessNames := TStringList.Create;
end;

procedure TcaProgress.FreeListObjects;
begin
  FProgressNames.Free;
  FProcessNames.Free;
end;

procedure TcaProgress.Initialize;
begin
  FActive := True;
  FCurrentProcess := -1;
  FCurrentProgress := -1;
end;

 // Property methods

function TcaProgress.GetActive: Boolean;
begin
  Result := FActive;
end;

function TcaProgress.GetProgressBar: TcaProgressBar;
begin
  Result := FProgressBar;
end;

function TcaProgress.GetProcessNames: TStringList;
begin
  Result := FProcessNames;
end;

procedure TcaProgress.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

function TcaProgress.GetProgressNames: TStringList;
begin
  Result := FProgressNames;
end;

procedure TcaProgress.SetProgressBar(const Value: TcaProgressBar);
begin
  FProgressBar := Value;
end;

 //---------------------------------------------------------------------------
 // Initialization / finalization
 //---------------------------------------------------------------------------

procedure CreateProgress;
begin
  if Progress = nil then Progress := TcaProgress.Create;
end;

procedure FreeProgress;
begin
  Progress := nil;
end;

initialization
  CreateProgress;

finalization
  FreeProgress;

end.
