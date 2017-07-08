unit caLabel;

interface

uses

  // Standard Delphi units 
  Windows,
  Classes,
  Messages,
  Controls,
  Sysutils,
  Graphics,
  Forms,
  StdCtrls,
  ExtCtrls,
  ShellAPI,

  // ca units 
  caUtils,
  caTypes,
  caClasses;

type

  //----------------------------------------------------------------------------
  // TcaLabelHyperlink                                                          
  //----------------------------------------------------------------------------

  TcaLabel = class;

  TcaLabelHyperlink = class(TPersistent)
  private
    // Private fields 
    FLabel: TcaLabel;
    // Property fields 
    FActive: Boolean;
    FHyperlinkClickColor: TColor;
    FHyperlinkColor: TColor;
    FHyperlinkHoverColor: TColor;
    FHyperlinkURL: string;
    // Property methods 
    procedure SetActive(const Value: Boolean);
    procedure SetHyperlinkClickColor(const Value: TColor);
    procedure SetHyperlinkColor(const Value: TColor);
    procedure SetHyperlinkHoverColor(const Value: TColor);
    procedure SetHyperlinkURL(const Value: string);
  public
    // Create/Destroy 
    constructor Create(ALabel: TcaLabel);
    // Public methods 
    procedure Assign(Source: TPersistent); override;
    procedure LaunchBrowser;
  published
    // Published properties 
    property Active: Boolean read FActive write SetActive;
    property HyperlinkClickColor: TColor read FHyperlinkClickColor write SetHyperlinkClickColor;
    property HyperlinkColor: TColor read FHyperlinkColor write SetHyperlinkColor;
    property HyperlinkHoverColor: TColor read FHyperlinkHoverColor write SetHyperlinkHoverColor;
    property HyperlinkURL: string read FHyperlinkURL write SetHyperlinkURL;
  end;

  //----------------------------------------------------------------------------
  // TcaLabel                                                                   
  //----------------------------------------------------------------------------

  TcaLabelMode = (lmCaption, lmInteger, lmFloat);

  TcaLabel = class(TLabel)
  private
    // Private fields 
    FMode: TcaLabelMode;
    FMouseDown: Boolean;
    FMouseOver: Boolean;
    // Property fields 
    FFloatFormat: string;
    FFloatValue: Double;
    FHyperlink: TcaLabelHyperlink;
    FIntFormat: string;
    FIntValue: Integer;
    // Property methods 
    function GetCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
    procedure SetFloatFormat(const Value: string);
    procedure SetFloatValue(const Value: Double);
    procedure SetHyperlink(const Value: TcaLabelHyperlink);
    procedure SetIntFormat(const Value: string);
    procedure SetIntValue(const Value: Integer);
    // Private methods 
    procedure UpdateCaption;
    // Event handlers 
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    // Static methods 
    procedure UpdateHyperlink;
    // Protected methods 
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // Promoted properties 
    property Action;
    // Published properties 
    property Caption: TCaption read GetCaption write SetCaption;
    property FloatFormat: string read FFloatFormat write SetFloatFormat;
    property FloatValue: Double read FFloatValue write SetFloatValue;
    property Hyperlink: TcaLabelHyperlink read FHyperlink write SetHyperlink;
    property IntFormat: string read FIntFormat write SetIntFormat;
    property IntValue: Integer read FIntValue write SetIntValue;
  end;

implementation

  //----------------------------------------------------------------------------
  // TcaLabelHyperlink                                                          
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaLabelHyperlink.Create(ALabel: TcaLabel);
begin
  inherited Create;
  FLabel := ALabel;
  FHyperlinkClickColor := clPurple;
  FHyperlinkColor := clBlue;
  FHyperlinkHoverColor := clAqua;
end;

  // Public methods 

procedure TcaLabelHyperlink.Assign(Source: TPersistent);
var
  SourceLink: TcaLabelHyperlink;
begin
  if Source is TcaLabelHyperlink then
    begin
      SourceLink := TcaLabelHyperlink(Source);
      FActive := SourceLink.Active;
      FHyperlinkClickColor := SourceLink.HyperlinkClickColor;
      FHyperlinkColor := SourceLink.HyperlinkColor;
      FHyperlinkHoverColor := SourceLink.HyperlinkHoverColor;
    end;
end;

procedure TcaLabelHyperlink.LaunchBrowser;
begin
  ShellExecute(FLabel.Parent.Handle, nil, PChar(FHyperlinkURL), nil, nil, SW_SHOWMAXIMIZED);
end;

  // Property methods 

procedure TcaLabelHyperlink.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
    begin
      FActive := Value;
      FLabel.UpdateHyperlink;
    end;
end;

procedure TcaLabelHyperlink.SetHyperlinkClickColor(const Value: TColor);
begin
  if Value <> FHyperlinkClickColor then
    begin
      FHyperlinkClickColor := Value;
      FLabel.UpdateHyperlink;
    end;
end;

procedure TcaLabelHyperlink.SetHyperlinkColor(const Value: TColor);
begin
  if Value <> FHyperlinkColor then
    begin
      FHyperlinkColor := Value;
      FLabel.UpdateHyperlink;
    end;
end;

procedure TcaLabelHyperlink.SetHyperlinkHoverColor(const Value: TColor);
begin
  if Value <> FHyperlinkHoverColor then
    begin
      FHyperlinkHoverColor := Value;
      FLabel.UpdateHyperlink;
    end;
end;

procedure TcaLabelHyperlink.SetHyperlinkURL(const Value: string);
begin
  if Value <> FHyperlinkURL then
    begin
      FHyperlinkURL := Value;
      FLabel.UpdateHyperlink;
    end;                    
end;

  //----------------------------------------------------------------------------
  // TcaLabel                                                                   
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaLabel.Create(AOwner: TComponent);
begin
  inherited;
  FHyperlink := TcaLabelHyperlink.Create(Self);
end;

destructor TcaLabel.Destroy;
begin
  FHyperlink.Free;
  inherited;
end;

  // Static methods 

procedure TcaLabel.UpdateHyperlink;
begin
  if FHyperlink.Active then
    begin
      Cursor := crHandPoint;
      if csDesigning in ComponentState then
        Font.Color := FHyperlink.HyperlinkColor
      else
        begin
          if FMouseDown then
            Font.Color := FHyperlink.HyperlinkClickColor else
          if FMouseOver then
            Font.Color := FHyperlink.HyperlinkHoverColor
          else
            Font.Color := FHyperlink.HyperlinkColor;
        end;
      Font.Style := Font.Style + [fsUnderline];
      Invalidate;
    end
  else
    Cursor := crDefault;
end;

  // Protected methods 

procedure TcaLabel.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited;
  FMouseDown := True;
  UpdateHyperlink;
end;

procedure TcaLabel.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited;
  FMouseDown := False;
  UpdateHyperlink;
  if (FHyperlink.Active) and (FHyperlink.HyperlinkURL <> '') then
    FHyperlink.LaunchBrowser;
end;

  // Private methods 

procedure TcaLabel.UpdateCaption;
begin
  case FMode of
    lmCaption:  Pass;
    lmInteger:  inherited Caption := FormatFloat(FIntFormat, FIntValue);
    lmFloat:    inherited Caption := FormatFloat(FFloatFormat, FFloatValue);
  end;
end;

  // Event handlers 

procedure TcaLabel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseOver := True;
  UpdateHyperlink;
end;

procedure TcaLabel.CMMouseLeave(var Message: TMessage);
begin
  FMouseOver := False;
  UpdateHyperlink;
end;

  // Property methods 

function TcaLabel.GetCaption: TCaption;
begin
  Result := inherited Caption;
end;

procedure TcaLabel.SetCaption(const Value: TCaption);
begin
  inherited Caption := Value;
  FMode := lmCaption;
end;

procedure TcaLabel.SetFloatFormat(const Value: string);
begin
  if Value <> FFloatFormat then
    begin
      FFloatFormat := Value;
      UpdateCaption;
    end;
end;

procedure TcaLabel.SetFloatValue(const Value: Double);
begin
  if Value <> FFloatValue then
    begin
      FFloatValue := Value;
      FMode := lmFloat;
      UpdateCaption;
    end;
end;

procedure TcaLabel.SetHyperlink(const Value: TcaLabelHyperlink);
begin
  FHyperlink.Assign(Value);
end;

procedure TcaLabel.SetIntFormat(const Value: string);
begin
  if Value <> FIntFormat then
    begin
      FIntFormat := Value;
      UpdateCaption;
    end;
end;

procedure TcaLabel.SetIntValue(const Value: Integer);
begin
  if Value <> FIntValue then
    begin
      FIntValue := Value;
      FMode := lmInteger;
      UpdateCaption;
    end;
end;

end.
