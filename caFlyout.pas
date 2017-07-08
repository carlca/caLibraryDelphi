unit caFlyout;

{$INCLUDE ca.inc}

interface

uses

  // standard Delphi units 
  Windows,
  Classes,
  Sysutils,
  Controls,
  Forms,
  Messages,

  // ca units 
  caLog,
  caFormHook;

type

  TcaFlyoutPosition = (fpLeft, fpRight, fpTop, fpBottom);

  //---------------------------------------------------------------------------
  // TcaFlyout                                                                 
  //---------------------------------------------------------------------------

  TcaFlyout = class(TcaFormHook)
  private
    // private fields 
    FDefaultApplicationIdleEvent: TIdleEvent;
    FBorderIcons: TBorderIcons;
    FBorderStyle: TBorderStyle;
    FFormCaption: string;
    FFormHeight: Integer;
    FFormWidth: Integer;
    FMouseIsOver: Boolean;
    // property fields 
    FFlyoutDistance: Integer;
    FOffset: Integer;
    FPosition: TcaFlyoutPosition;
    // property methods 
    procedure SetFlyoutDistance(const Value: Integer);
    procedure SetOffset(const Value: Integer);
    procedure SetPosition(const Value: TcaFlyoutPosition);
    // private methods 
    procedure ApplicationIdleEvent(Sender: TObject; var Done: Boolean);
    procedure SaveDefaultState;
    procedure UpdateFormPosition;
    procedure UpdateFormState;
  protected
    // protected methods 
    procedure DoFormReceiveMessage(Msg: TMessage; var Handled: Boolean); override;
    procedure DoReceiveMessage(Msg: TMessage; var Handled: Boolean); override;
    procedure Loaded; override;
  public
    // create/destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // published properties 
    property FlyoutDistance: Integer read FFlyoutDistance write SetFlyoutDistance;
    property Offset: Integer read FOffset write SetOffset;
    property Position: TcaFlyoutPosition read FPosition write SetPosition;
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaFlyout                                                                 
  //---------------------------------------------------------------------------

  // create/destroy 

constructor TcaFlyout.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultApplicationIdleEvent := Application.OnIdle;
  Application.OnIdle := ApplicationIdleEvent;
end;

destructor TcaFlyout.Destroy;
begin

  inherited;
end;

  // protected methods 

procedure TcaFlyout.DoFormReceiveMessage(Msg: TMessage; var Handled: Boolean);
begin
  inherited;
  if (Msg.Msg = CM_MOUSEENTER) or (Msg.Msg = CM_MOUSELEAVE) then
    begin
      if Msg.Msg = CM_MOUSEENTER then
        FMouseIsOver := True
      else
        FMouseIsOver := False;
      UpdateFormState;
      UpdateFormPosition;
    end;
end;

procedure TcaFlyout.DoReceiveMessage(Msg: TMessage; var Handled: Boolean);
begin
  inherited;
end;

procedure TcaFlyout.Loaded;
begin
  inherited;
  if OwnerFormExists then
    begin
      SaveDefaultState;
      UpdateFormPosition;
    end;
end;

  // private methods 

procedure TcaFlyout.ApplicationIdleEvent(Sender: TObject; var Done: Boolean);
begin
  FMouseIsOver := WindowFromPoint(Mouse.CursorPos) = OwnerFormHandle;
  // Log.Send('FMouseIsOver', FMouseIsOver);
  UpdateFormState;
  UpdateFormPosition;
  Done := False;
  if Assigned(FDefaultApplicationIdleEvent) then
    FDefaultApplicationIdleEvent(Sender, Done);
end;

procedure TcaFlyout.SaveDefaultState;
begin
  if OwnerFormExists then
    begin
      FBorderIcons := OwnerForm.BorderIcons;
      FBorderStyle := OwnerForm.BorderStyle;
      FFormCaption := OwnerForm.Caption;
      FFormHeight := OwnerForm.Height;
      FFormWidth := OwnerForm.Width;
    end;
end;

procedure TcaFlyout.UpdateFormPosition;
begin
  if OwnerFormExists then
    begin
      UpdateFormState;
      if FMouseIsOver then
        begin
          case FPosition of
            fpLeft:     OwnerForm.SetBounds(0, FOffset, 1, OwnerForm.Height);
            fpRight:    OwnerForm.SetBounds(Screen.Width - 1, FOffset, 1, OwnerForm.Height);
            fpTop:      OwnerForm.SetBounds(FOffset, 0, OwnerForm.Width, 1);
            fpBottom:   OwnerForm.SetBounds(FOffset, Screen.Height - 1, OwnerForm.Width, 1);
          end;
        end
      else
        begin
          case FPosition of
            fpLeft:     OwnerForm.SetBounds(0, FOffset, FFormWidth, OwnerForm.Height);
            fpRight:    OwnerForm.SetBounds(Screen.Width - 1, FOffset, FFormWidth, OwnerForm.Height);
            fpTop:      OwnerForm.SetBounds(FOffset, 0, OwnerForm.Width, FFormHeight);
            fpBottom:   OwnerForm.SetBounds(FOffset, Screen.Height - 1, OwnerForm.Width, FFormHeight);
          end;
        end;
    end;
end;

procedure TcaFlyout.UpdateFormState;
begin
  if FMouseIsOver then
    begin
      OwnerForm.BorderIcons := FBorderIcons;
      OwnerForm.BorderStyle := FBorderStyle;
      OwnerForm.Caption := FFormCaption;
    end
  else
    begin
      OwnerForm.BorderIcons := [];
      OwnerForm.BorderStyle := bsSizeable;
      OwnerForm.Caption := '';
    end;
end;

  // property methods 

procedure TcaFlyout.SetFlyoutDistance(const Value: Integer);
begin
  if Value <> FFlyoutDistance then
    begin
      FFlyoutDistance := Value;
      UpdateFormPosition;
    end;
end;

procedure TcaFlyout.SetOffset(const Value: Integer);
begin
  if Value <> FOffset then
    begin
      FOffset := Value;
      UpdateFormPosition;
    end;
end;

procedure TcaFlyout.SetPosition(const Value: TcaFlyoutPosition);
begin
  if Value <> FPosition then
    begin
      FPosition := Value;
      UpdateFormPosition;
    end;
end;

end.
