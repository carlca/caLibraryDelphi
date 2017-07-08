unit caRadioButton;

interface

uses

  // standard Delphi units...
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  StdCtrls,
  Forms;

type

  //---------------------------------------------------------------------------
  // TcaRadioButton
  //---------------------------------------------------------------------------

  TcaRadioButton = class(TRadioButton)
  private
    // private members...
    FLabel: TLabel;
    // private methods...
    procedure CreateLabel;
    procedure UpdateLabel;
    procedure UpdateRegion;
    // event handlers...
    procedure LabelClickEventHandler(Sender: TObject);
  protected
    // overridden protected methods...
    procedure CreateWnd; override;
  public
    // lifetime...
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

//---------------------------------------------------------------------------
// TcaRadioButton
//---------------------------------------------------------------------------

// lifetime...

constructor TcaRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreateLabel;
end;

destructor TcaRadioButton.Destroy;
begin
  FreeAndNil(FLabel);
  inherited;
end;

// overridden protected methods...

procedure TcaRadioButton.CreateWnd;
begin
  inherited CreateWnd;
  if not (csDesigning in ComponentState) then
    begin
      UpdateLabel;
      UpdateRegion;
    end;
end;

// private methods...

procedure TcaRadioButton.CreateLabel;
begin
  FLabel := TLabel.Create(Owner);
  FLabel.Transparent := True;
  FLabel.Caption := '';
  FLabel.OnClick := LabelClickEventHandler;
end;

procedure TcaRadioButton.UpdateLabel;
begin
  if FLabel.Caption = '' then
    begin
      FLabel.Caption := Caption;
      Caption := '';
      FLabel.Parent := Parent;
      FLabel.Top := Top + 1;
      FLabel.Left := Left + 20;
      FLabel.Visible := True;
    end;
end;

procedure TcaRadioButton.UpdateRegion;
var
  Region: HRGN;
  HalfHeight: Integer;
begin
  HalfHeight := (Height + 1) div 2;
  Region := CreateEllipticRgn(-1, HalfHeight - 7, 16, HalfHeight + 7);
  SetWindowRgn(Handle, Region, True);
end;

// event handlers...

procedure TcaRadioButton.LabelClickEventHandler(Sender: TObject);
begin
  Checked := True;
end;

end.

