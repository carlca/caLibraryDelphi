unit caLog;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  Messages,
  Classes,
  Sysutils,
  TypInfo,
  MMSystem,

  // ca units 
  caTypes;

type

  TcaLogControlMessage = (lmClear, lmPause, lmFreeze, lmUnfreeze, lmUnknown);

  //---------------------------------------------------------------------------
  // IcaLoggable                                                               
  //---------------------------------------------------------------------------

  IcaLoggable = interface
  ['{3ED21C41-47B1-4E84-9EF2-2E0328B57E39}']
    procedure SendToLog(const AMsg: String; AClearLog: Boolean = False);
  end;

  //---------------------------------------------------------------------------
  // IcaLog                                                                    
  //---------------------------------------------------------------------------

  IcaLog = interface
  ['{F1D0907E-9996-4618-A8DC-BD40757F070E}']
    // Property methods 
    function GetActive: Boolean;
    function GetDestStrings: TStrings;
    procedure SetActive(const Value: Boolean);
    procedure SetDestStrings(const Value: TStrings);
    // Public methods 
    procedure BeginLine;
    procedure Clear;
    procedure Freeze;
    procedure Indent;
    procedure InitSend(const AMsg: String; AClearLog: Boolean = False);
    procedure Pause;
    procedure Outdent;
    function Send(const Msg: String): string; overload;
    function Send(const Msg, Value: String): string; overload;
    function Send(const Msg: String; Value: Integer): string; overload;
    function Send(const Msg: String; Value: Double): string; overload;
    function Send(const Msg: String; Value: Extended): string; overload;
    function Send(const Msg: String; Value: Boolean): string; overload;
    function Send(const Msg: String; Value: TStrings): string; overload;
    function Send(const Msg: String; Value: TRect): string; overload;
    function Send(AObject: TObject; const APropName: String = ''): string; overload;
    procedure SendWindowClass(const Msg: String; Wnd: HWND);
    procedure SendWindowProcess(const Msg: String; Wnd: HWND);
    procedure SendAllocMem;
    procedure SendFreeMem;
    procedure SendLine;
    procedure SendBreak;
    procedure SendDivider;
    procedure StartClock;
    procedure StopClock(const Msg: String);
    procedure Unfreeze;
    // Properties 
    property Active: Boolean read GetActive write SetActive;
    property DestStrings: TStrings read GetDestStrings write SetDestStrings;
  end;

  //---------------------------------------------------------------------------
  // TcaLog                                                                    
  //---------------------------------------------------------------------------

  TcaLog = class(TInterfacedObject, IcaLog)
  private
    // Property fields 
    FActive: Boolean;
    FDestStrings: TStrings;
    // Private fields 
    FBuildingLine: Boolean;
    FIndentLevel: Integer;
    FStartTime: DWORD;
    FLines: TStringList;
    FLogAppHandle: HWND;
    // FStartTime: TDateTime; 
    // Property methods 
    function GetActive: Boolean;
    function GetDestStrings: TStrings;
    procedure SetActive(const Value: Boolean);
    procedure SetDestStrings(const Value: TStrings);
    // Private methods 
    procedure CheckAppHandle;
    procedure SendLog(const Msg, LogText: String);
  public
    constructor Create;
    destructor Destroy; override;
    // Class function 
    class function EncodeControlMessage(AControlMsg: TcaLogControlMessage): String;
    class function DecodeControlMessage(const AControlMsg: String): TcaLogControlMessage;
    // Public methods 
    procedure BeginLine;
    procedure Clear;
    procedure Freeze;
    procedure Indent;
    procedure InitSend(const AMsg: String; AClearLog: Boolean = False);
    procedure Pause;
    procedure Outdent;
    function Send(const Msg: String): string; overload;
    function Send(const Msg, Value: String): string; overload;
    function Send(const Msg: String; Value: Integer): string; overload;
    function Send(const Msg: String; Value: Double): string; overload;
    function Send(const Msg: String; Value: Extended): string; overload;
    function Send(const Msg: String; Value: Boolean): string; overload;
    function Send(const Msg: String; Value: TStrings): string; overload;
    function Send(const Msg: String; Value: TRect): string; overload;
    function Send(AObject: TObject; const APropName: String = ''): string; overload;    procedure SendWindowClass(const Msg: String; Wnd: HWND);
    procedure SendWindowProcess(const Msg: String; Wnd: HWND);
    procedure SendAllocMem;
    procedure SendFreeMem;
    procedure SendLine;
    procedure SendBreak;
    procedure SendDivider;
    procedure StartClock;
    procedure StopClock(const Msg: String);
    procedure Unfreeze;
    // Properties 
    property Active: Boolean read GetActive write SetActive;
    property DestStrings: TStrings read GetDestStrings write SetDestStrings;
  end;

  //---------------------------------------------------------------------------
  // IcaTrace                                                                  
  //---------------------------------------------------------------------------

  IcaTrace = interface
  ['{72C90185-5B39-4F3C-81AA-F1B4FF945AAE}']
  end;

  //---------------------------------------------------------------------------
  // TcaTrace                                                                  
  //---------------------------------------------------------------------------

  TcaTrace = class(TInterfacedObject, IcaTrace)
  private
    // Private fields 
    FProcName: string;
  public
    // Create/Destroy 
    constructor Create(const Args: array of string);
    destructor Destroy; override;
    // Class function 
    class function Level: Integer;
  end;

  //---------------------------------------------------------------------------
  // TcaLogComponent                                                           
  //---------------------------------------------------------------------------

  TcaLogComponent = class(TComponent)
  private
    // Property fields 
    FActivateLog: Boolean;
    FComponent: TComponent;
    FLogFilenameBase: string;
    // Property methods 
    procedure SetActivateLog(const Value: Boolean);
  protected
    // Protected methods 
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    // Properties fields 
    property ActivateLog: Boolean read FActivateLog write SetActivateLog;
    property Component: TComponent read FComponent write FComponent;
    property LogFilenameBase: string read FLogFilenameBase write FLogFilenameBase;
  end;

var
  TcaTrace_Level: Integer = 0;


const
  cLogOpen      = '<%';
  cLogClose     = '%>';

var
  Log: IcaLog = nil;

implementation

uses
  caClasses,
  caUtils;

  //---------------------------------------------------------------------------
  // TcaLog                                                                    
  //---------------------------------------------------------------------------

constructor TcaLog.Create;
begin
  inherited;
  FLines := TStringList.Create;
  FActive := True;
  CheckAppHandle;
end;

destructor TcaLog.Destroy;
begin
  FLines.Free;
  inherited;
end;

  // Class methods 

class function TcaLog.DecodeControlMessage(const AControlMsg: String): TcaLogControlMessage;
var
  ControlMsg: String;
  EnumMsg: String;
begin
  for Result := Low(TcaLogControlMessage) to High(TcaLogControlMessage) do
    begin
      EnumMsg := EncodeControlMessage(Result);
      ControlMsg := cLogOpen + AControlMsg  + cLogClose;
      if EnumMsg = ControlMsg then Break;
    end;
end;

class function TcaLog.EncodeControlMessage(AControlMsg: TcaLogControlMessage): String;
var
  AControlMsgStr: String;
begin
  AControlMsgStr := GetEnumName(TypeInfo(TcaLogControlMessage), Ord(AControlMsg));
  Result := cLogOpen + AControlMsgStr + cLogClose;
end;

  // Public methods 

procedure TcaLog.BeginLine;
begin
  FBuildingLine := True;
  FLines.Clear;
end;

procedure TcaLog.Clear;
begin
  SendLog(EncodeControlMessage(lmClear), '');
end;

procedure TcaLog.Freeze;
begin
  SendLog(EncodeControlMessage(lmFreeze), '');
end;

procedure TcaLog.Indent;
begin
  Inc(FIndentLevel, 4);
end;

procedure TcaLog.InitSend(const AMsg: String; AClearLog: Boolean = False);
begin
  if AClearLog then Clear;
  if AMsg <> '' then Send(AMsg);
end;

procedure TcaLog.Pause;
begin
  SendLog(EncodeControlMessage(lmPause), '');
end;

procedure TcaLog.Outdent;
begin
  if FIndentLevel >= 4 then
    Dec(FIndentLevel, 4);
end;

function TcaLog.Send(const Msg: String): string;
begin
  SendLog(Msg, '');
  Result := '';
end;

function TcaLog.Send(const Msg: String; Value: Double): string;
begin
  SendLog(Msg, FloatToStr(Value));
  Result := FloatToStr(Value);
end;

function TcaLog.Send(const Msg: String; Value: Integer): string;
begin
  SendLog(Msg, IntToStr(Value));
  Result := IntToStr(Value);
end;

function TcaLog.Send(const Msg, Value: String): string;
begin
  SendLog(Msg, Value);
  Result := Value;
end;

function TcaLog.Send(const Msg: String; Value: TStrings): string;
begin
  SendLog(Format('%s(%d)', [Msg, Value.Count]), Value.Text);
  Result := Value.Text;
end;

function TcaLog.Send(const Msg: String; Value: Boolean): string;
begin
  SendLog(Msg, Utils.BooleanToString(Value));
  Result := Utils.BooleanToString(Value);
end;

function TcaLog.Send(const Msg: String; Value: Extended): string;
begin
  SendLog(Msg, FloatToStr(Value));
  Result := FloatToStr(Value);
end;

function TcaLog.Send(AObject: TObject; const APropName: String = ''): string;
var
  Index: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
  PropName: String;
  PropType: TTypeKind;
  PropTypeInfo: TTypeInfo;
  TypeData: PTypeData;
  TypeInfo: PTypeInfo;
  PropValue: String;
  PropOutList: IcaStringList;
  CompName: String;
begin
  if AObject = nil then Exit;

  PropOutList := TcaStringList.Create;

  if APropName = '' then
    begin
      CompName := '';
      if AObject is TComponent then
        begin
          CompName := '<no name>';
          if TComponent(AObject).Name <> '' then
            CompName := TComponent(AObject).Name;
          PropOutList.Add(CompName + ': ' + AObject.ClassName);
        end
      else
        PropOutList.Add(AObject.ClassName);
    end;

  TypeInfo := AObject.ClassInfo;
  TypeData := GetTypeData(TypeInfo);
  if TypeData.PropCount <> 0 then
    begin
      GetMem(PropList, SizeOf(PPropInfo) * TypeData.PropCount);
      try
        GetPropInfos(TypeInfo, PropList);
        for Index := 0 to TypeData.PropCount - 1 do
          begin
            // Get RTTI for each published object property 
            PropValue := '';
            PropInfo := PropList[Index];
            PropName := PropInfo^.Name;
            PropType := PropInfo^.PropType^.Kind;
            PropTypeInfo := PropList[Index]^.PropType^^;

            case PropType of
              tkInteger:      PropValue := IntToStr(GetOrdProp(AObject, PropInfo));
              tkChar:         PropValue := GetStrProp(AObject, PropInfo);
              tkEnumeration:  PropValue := GetEnumName(@PropTypeInfo, GetOrdProp(AObject, PropInfo));
              tkFloat:        PropValue := FloatToStr(GetFloatProp(AObject, PropInfo));
              tkString:       PropValue := GetStrProp(AObject, PropInfo);
              tkLString:      PropValue := GetStrProp(AObject, PropInfo);
              tkInt64:        PropValue := IntToStr(GetInt64Prop(AObject, PropInfo));
            end;

            if (PropName = APropName) or (APropName = '') then
              PropOutList.Add(PropName + ': ' + PropValue);
          end;
      finally
        FreeMem(PropList);
      end;
    end;
  for Index := 0 to PropOutList.Count - 1 do
    Send(PropOutList[Index]);
  Result := '';
end;

function TcaLog.Send(const Msg: String; Value: TRect): string;
var
  RectStr: String;
begin
  RectStr := Format('Left: %d  Top: %d  Right: %d  Bottom: %d', [Value.Left, Value.Top, Value.Right, Value.Bottom]);
  Send(Msg, RectStr);
  Result := RectStr;
end;

procedure TcaLog.SendAllocMem;
var
  HeapStatus: THeapStatus;
begin
  HeapStatus := GetHeapStatus;
  Send('Allocated memory', HeapStatus.TotalAllocated - SizeOf(THeapStatus));
end;

procedure TcaLog.SendFreeMem;
var
  HeapStatus: THeapStatus;
begin
  HeapStatus := GetHeapStatus;
  Send('Free memory', HeapStatus.TotalAllocated + SizeOf(THeapStatus));
end;

procedure TcaLog.SendLine;
var
  Index: Integer;
  Line: String;
begin
  Line := '';
  for Index := 0 to FLines.Count - 1 do
    Line := Line + FLines[Index] + '  ';
  FBuildingLine := False;
  Send(Line);
  FLines.Clear;
end;

  // Private methods 

procedure TcaLog.CheckAppHandle;
begin
  if FLogAppHandle = 0 then FLogAppHandle := FindWindow('TcaLogAppForm', 'Log');
end;

procedure TcaLog.SendLog(const Msg, LogText: String);
var
  CopyData: TCopyDataStruct;
  CopyText: String;
begin
  if FActive then
    begin
      if FDestStrings <> nil then
        begin
          if LogText = '' then
            CopyText := Msg
          else
            CopyText := Format('%s%s%s', [Msg, #9, LogText]);
          if Pos(cLogOpen, CopyText) = 0 then
            FDestStrings.Add(CopyText);
        end;
      CheckAppHandle;
      if FLogAppHandle <> 0 then
        begin
          if LogText = '' then
            CopyText := Msg
          else
            CopyText := Format('%s [ %s ]', [Msg, LogText]);
          if FBuildingLine then
            FLines.Add(CopyText)
          else
            begin
              CopyText := StringOfChar(#32, FIndentLevel) + CopyText;  
              CopyData.dwData := 0;
              CopyData.cbData := Length(CopyText) + 1;
              GetMem(CopyData.lpData, CopyData.cbData);
              try
                StrCopy(CopyData.lpData, PChar(CopyText));
                SendMessage(FLogAppHandle, WM_COPYDATA, 0, Cardinal(@CopyData));
              finally
                FreeMem(CopyData.lpData);
              end;
            end;
        end;
    end;
end;

procedure TcaLog.SendWindowClass(const Msg: String; Wnd: HWND);
begin
  Send(Msg, Utils.GetWindowClass(Wnd));
end;

procedure TcaLog.SendWindowProcess(const Msg: String; Wnd: HWND);
begin
  Send(Msg, Utils.GetWindowProcess(Wnd));
end;

procedure TcaLog.StartClock;
begin
  FStartTime := TimeGetTime;
end;

procedure TcaLog.SendBreak;
begin
  Send(' ');
end;

procedure TcaLog.SendDivider;
begin
  Send(Utils.BuildString('-', 80));
end;

procedure TcaLog.StopClock(const Msg: String);
var
  HH, MM, SS, MS: Word;
  ElapsedTime: DWORD;
  ElapsedDateTime: TDateTime;
begin
  ElapsedTime := TimeGetTime - FStartTime;
  ElapsedDateTime := ElapsedTime / MSecsPerDay;
  DecodeTime(ElapsedDateTime, HH, MM, SS, MS);
  Send(Msg, Format('%.2d:%.2d:%.2d.%.3d', [HH, MM, SS, MS]));
end;

procedure TcaLog.Unfreeze;
begin
  SendLog(EncodeControlMessage(lmUnfreeze), '');
end;

function TcaLog.GetActive: Boolean;
begin
  Result := FActive;
end;

function TcaLog.GetDestStrings: TStrings;
begin
  Result := FDestStrings;
end;

procedure TcaLog.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TcaLog.SetDestStrings(const Value: TStrings);
begin
  FDestStrings := Value;
end;

  //---------------------------------------------------------------------------
  // TcaTrace                                                                  
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaTrace.Create(const Args: array of string);
var
  Index: Integer;
begin
  inherited Create;
  if Length(Args) > 0 then
    begin
      FProcName := Args[0];
      Log.BeginLine;
      Log.Send(Utils.BuildString(#32, TcaTrace_Level * 2) + '>> ' + FProcName);
      for Index := Succ(Low(Args)) to High(Args) do
        Log.Send(Args[Index]);
      Log.SendLine;
      Inc(TcaTrace_Level);
    end;
end;

destructor TcaTrace.Destroy;
begin
  Dec(TcaTrace_Level);
  Log.Send(Utils.BuildString(#32, TcaTrace_Level * 2) + '<< ' + FProcName);
  inherited;
end;

  // Class function 

class function TcaTrace.Level: Integer;
begin
  Result := TcaTrace_Level;
end;

  //---------------------------------------------------------------------------
  // TcaLogComponent                                                           
  //---------------------------------------------------------------------------

  // Protected methods 

procedure TcaLogComponent.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FComponent) then
    FComponent := nil;
end;

  // Property methods 

procedure TcaLogComponent.SetActivateLog(const Value: Boolean);
var
  LogLines: TStrings;
  Filename: string;
begin
  LogLines := Auto(TStringList.Create).Instance;
  Log.DestStrings := LogLines;
  if Value and Assigned(FComponent) then
    Log.Send(FComponent);
  if FLogFilenameBase <> '' then
    begin
      Filename := FLogFilenameBase + FComponent.Name + '.log';
      LogLines.SaveToFile(Filename);
    end;
  Log.DestStrings := nil;
end;

  //---------------------------------------------------------------------------
  // Initialization                                                            
  //---------------------------------------------------------------------------


initialization
  if Log = nil then Log := TcaLog.Create;

end.


