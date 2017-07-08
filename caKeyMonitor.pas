unit caKeyMonitor;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  Messages,
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  Dialogs,

  // caCollection 
  caClasses,
  caConsts;

type

  TcaFoundKeyEvent = procedure(Sender: TObject; AKey: Word) of object;

  //---------------------------------------------------------------------------
  // IcaKeyMonitor                                                             
  //---------------------------------------------------------------------------

  IcaKeyMonitor = interface
  ['{C3E5F891-927E-4549-AA2B-FE91F2FF04BA}']
    // property methods 
    function GetActive: Boolean;
    function GetKey(Index: Integer): Word;
    function GetKeyCount: Integer;
    function GetOnFoundKey: TcaFoundKeyEvent;
    function GetOnKeyDown: TKeyEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetOnFoundKey(const Value: TcaFoundKeyEvent);
    procedure SetOnKeyDown(const Value: TKeyEvent);
    // interface methods 
    procedure AddKey(AKey: Word);
    procedure ClearKeys;
    // properties 
    property Active: Boolean read GetActive write SetActive;
    property KeyCount: Integer read GetKeyCount;
    property Keys[Index: Integer]: Word read GetKey;
    property OnFoundKey: TcaFoundKeyEvent read GetOnFoundKey write SetOnFoundKey;
    property OnKeyDown: TKeyEvent read GetOnKeyDown write SetOnKeyDown;
  end;

  //---------------------------------------------------------------------------
  // TcaKeyMonitor                                                             
  //---------------------------------------------------------------------------

  TcaKeyMonitor = class(TComponent, IcaKeyMonitor)
  private
    FActive: Boolean;
    FKeys: TList;
    FOnFoundKey: TcaFoundKeyEvent;
    FOnKeyDown: TKeyEvent;
    FOriginalHandler: TMessageEvent;
    // property methods 
    function GetActive: Boolean;
    function GetKey(Index: Integer): Word;
    function GetKeyCount: Integer;
    function GetOnFoundKey: TcaFoundKeyEvent;
    function GetOnKeyDown: TKeyEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetOnFoundKey(const Value: TcaFoundKeyEvent);
    procedure SetOnKeyDown(const Value: TKeyEvent);
    // private methods 
    function CheckKey(AKey: Word): Boolean;
    function IsRunTime: Boolean;
    procedure MessageHandler(var Msg: TMsg; var Handled: Boolean);
  protected
    // event triggers 
    procedure DoFoundKey(AKey: Word); virtual;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); virtual;
  public
    // lifetime 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // interface methods - IcaKeyMonitor 
    procedure AddKey(AKey: Word);
    procedure ClearKeys;
    // interface properties - IcaKeyMonitor 
    property Active: Boolean read GetActive write SetActive;
    property KeyCount: Integer read GetKeyCount;
    property Keys[Index: Integer]: Word read GetKey;
  published
    // public properties 
    property OnFoundKey: TcaFoundKeyEvent read GetOnFoundKey write SetOnFoundKey;
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaKeyMonitor                                                             
  //---------------------------------------------------------------------------

  // lifetime 

constructor TcaKeyMonitor.Create(AOwner: TComponent);
begin
  inherited;
  FKeys := TList.Create;
  FOriginalHandler := Application.OnMessage;
end;

destructor TcaKeyMonitor.Destroy;
begin
  Application.OnMessage := FOriginalHandler;
  FKeys.Free;
  inherited;
end;

  // interface methods - IcaKeyMonitor 

procedure TcaKeyMonitor.AddKey(AKey: Word);
begin
  FKeys.Add(Pointer(AKey));
end;

procedure TcaKeyMonitor.ClearKeys;
begin
  FKeys.Clear;
end;

  // event triggers 

procedure TcaKeyMonitor.DoFoundKey(AKey: Word);
begin
  if Assigned(FOnFoundKey) then FOnFoundKey(Self, AKey);
end;

procedure TcaKeyMonitor.DoKeyDown(var Key: Word; Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then FOnKeyDown(Self, Key, Shift);
end;

  // private methods 

function TcaKeyMonitor.CheckKey(AKey: Word): Boolean;
var
  Index: Integer;
  FoundKey: Word;
begin
  Result := False;
  FoundKey := VK_NONE;
  for Index := 0 to FKeys.Count - 1 do
    begin
      if AKey = Integer(FKeys[Index]) then
        begin
          FoundKey := AKey;
          Break;
        end;
    end;
  if FoundKey <> VK_NONE then
    begin
      DoFoundKey(FoundKey);
      Result := True;
    end;
end;

function TcaKeyMonitor.IsRunTime: Boolean;
begin
  Result := Pos('delphi32', LowerCase(Application.ExeName)) = 0;
end;

procedure TcaKeyMonitor.MessageHandler(var Msg: TMsg; var Handled: Boolean);
var
  Key: Word;
  Shift: TShiftState;
begin
  if Msg.Message = WM_KEYDOWN then
    begin
      Handled := CheckKey(Msg.wParam);
      Shift := KeyDataToShiftState(Msg.lParam);
      Key := Msg.wParam;
      DoKeyDown(Key, Shift);
      Msg.wParam := Key;
    end;
end;

  // property methods 

function TcaKeyMonitor.GetKey(Index: Integer): Word;
begin
  Result := Integer(FKeys[Index]);
end;

function TcaKeyMonitor.GetKeyCount: Integer;
begin
  Result := FKeys.Count;
end;

function TcaKeyMonitor.GetOnFoundKey: TcaFoundKeyEvent;
begin
  Result := FOnFoundKey;
end;

procedure TcaKeyMonitor.SetOnFoundKey(const Value: TcaFoundKeyEvent);
begin
  FOnFoundKey := Value;
end;

function TcaKeyMonitor.GetActive: Boolean;
begin
  Result := FActive;
end;

function TcaKeyMonitor.GetOnKeyDown: TKeyEvent;
begin
  Result := FOnKeyDown;
end;

procedure TcaKeyMonitor.SetActive(const Value: Boolean);
begin
  if IsRunTime then
    begin
      FActive := Value;
      if FActive then
        Application.OnMessage := MessageHandler
      else
        Application.OnMessage := nil;
    end;
end;

procedure TcaKeyMonitor.SetOnKeyDown(const Value: TKeyEvent);
begin
  FOnKeyDown := Value;
end;

end.
