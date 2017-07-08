unit caFormInitializer;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  SysUtils,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  ActnList,

  // ca units 
  caUtils,
  caClasses,
  caLog,
  caMessages,
  caFormHook;

const
  WM_AFTER_SHOW = WM_USER + 1;

type

  //----------------------------------------------------------------------------
  // TcaFormInitializer                                                         
  //----------------------------------------------------------------------------

  TcaFormInitializer = class(TcaFormHook)
  private
    // Property fields 
    FAction: TAction;
    FDestroyDetected: Boolean;
    FHandledNCDestroy: Boolean;
    FHandledShowWindow: Boolean;
    FMessages: IcaMessages;
    FOnFormIsShowing: TNotifyEvent;
    // Property methods 
    function GetOnFormIsShowing: TNotifyEvent;
    // Private methods 
    procedure PostAfterShowMessage(AHandle: HWND);
  protected
    // Virtual protected methods 
    procedure DoFormIsShowing; virtual;
    procedure DoFormReceiveMessage(Msg: TMessage; var Handled: Boolean); override;
    procedure DoReceiveMessage(Msg: TMessage; var Handled: Boolean); override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
  published
    // Published properties 
    property Action: TAction read FAction write FAction;
    property OnFormIsShowing: TNotifyEvent read GetOnFormIsShowing write FOnFormIsShowing;
  end;

implementation

  //----------------------------------------------------------------------------
  // TcaFormInitializer                                                         
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaFormInitializer.Create(AOwner: TComponent);
begin
  inherited;
  FMessages := TcaMessages.Create;
end;

  // Virtual protected methods 

procedure TcaFormInitializer.DoFormIsShowing;
begin
  if Assigned(FAction) then
    begin
      FAction.Enabled := True;
      FAction.Execute;
    end
  else
    begin
      if Assigned(FOnFormIsShowing) then
        FOnFormIsShowing(Self);
    end;
end;

procedure TcaFormInitializer.DoFormReceiveMessage(Msg: TMessage; var Handled: Boolean);
begin
  inherited;
  case Msg.Msg of
    WM_SHOWWINDOW:
      if (not FHandledShowWindow) and Boolean(Msg.WParam) then
        begin
          PostAfterShowMessage(OwnerFormHandle);
          Handled := True;
          FHandledShowWindow := True;
        end;
    WM_DESTROY:
      FDestroyDetected := True;
    WM_NCDESTROY:
      begin
        if FDestroyDetected and (not FHandledNCDestroy) then 
          begin
            PostAfterShowMessage(Handle);
            Handled := True;
            FHandledNCDestroy := True;
          end;
      end;
    WM_AFTER_SHOW:
      begin
        DoFormIsShowing;
        Handled := True;
      end;
  end;
end;

procedure TcaFormInitializer.DoReceiveMessage(Msg: TMessage; var Handled: Boolean);
begin
  if Msg.Msg = WM_AFTER_SHOW then
    begin
      DoFormIsShowing;
      Handled := True;
    end;
end;

  // Private methods 

procedure TcaFormInitializer.PostAfterShowMessage(AHandle: HWND);
begin
  PostMessage(AHandle, WM_AFTER_SHOW, 0, 0);
end;

  // Property methods 

function TcaFormInitializer.GetOnFormIsShowing: TNotifyEvent;
begin
  if Assigned(FOnFormIsShowing) then
    Result := FOnFormIsShowing
  else
    begin
      if Assigned(Action) then
        Result := Action.OnExecute;
    end;
end;

end.
