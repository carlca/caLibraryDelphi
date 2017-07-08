unit caEdit;

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
  Menus,
  Dialogs,
  StdCtrls,
  ExtCtrls,

  // ca units
  caTypes;

type

  //---------------------------------------------------------------------------
  // TcaEdit                                                                   
  //---------------------------------------------------------------------------

  TcaEditProcessKeyEvent = procedure(Sender: TObject; Key: Word; Shift: TShiftState; var Accept: Boolean) of object;

  TcaEdit = class(TEdit)
  private
    // Property fields 
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnProcessKey: TcaEditProcessKeyEvent;
    // Private fields 
    FMouseOver: Boolean;
    FValidKeys: TcaByteSet;
    // Component message handlers 
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    // Component notifier handlers 
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    // Window message handlers 
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure DoProcessKey(Key: Word; Shift: TShiftState; var Accept: Boolean);
    procedure RepaintFrame; virtual;
  public
    property MouseOver: Boolean read FMouseOver write FMouseOver;
    property ValidKeys: TcaByteSet read FValidKeys write FValidKeys;
  published
    // Events 
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnProcessKey: TcaEditProcessKeyEvent read FOnProcessKey write FOnProcessKey;
  end;

  //---------------------------------------------------------------------------
  // TcaComboBox                                                               
  //---------------------------------------------------------------------------

  TcaComboBox = class(TComboBox)
  private
    // Property methods 
    function GetCtl3D: Boolean;
    procedure SetCtl3D(const Value: Boolean);
    // Windows message handlers 
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    // Private methods 
    procedure DrawFlatComboBox;
  published
    // Published properties 
    property Ctl3D: Boolean read GetCtl3D write SetCtl3D;
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaEdit                                                                   
  //---------------------------------------------------------------------------

  // Protected methods 

procedure TcaEdit.DoProcessKey(Key: Word; Shift: TShiftState; var Accept: Boolean);
begin
  if Assigned(FOnProcessKey) then FOnProcessKey(Self, Key, Shift, Accept);
end;

procedure TcaEdit.RepaintFrame;
var
  R: TRect;
begin
  R := ClientRect;
  RedrawWindow( Handle, @R, 0, rdw_Invalidate or rdw_UpdateNow or rdw_NoFrame );
end;

  // Component message handlers 

procedure TcaEdit.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseOver := True;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TcaEdit.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseOver := False;
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

  // Component notifier handlers 

procedure TcaEdit.CNKeyDown(var Message: TWMKeyDown);
var
  Handled: Boolean;
  Shift: TShiftState;
begin
  Handled := Message.Result = 1;
  if (Message.CharCode in FValidKeys) or (FValidKeys = []) then
    begin
      Shift := KeyDataToShiftState(Message.KeyData);
      DoProcessKey(Message.CharCode, Shift, Handled);
    end
  else
    Handled := True;
  Message.Result := Ord(Handled);
end;

  // Window message handlers 

procedure TcaEdit.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if not Ctl3D then RepaintFrame;
end;

  //---------------------------------------------------------------------------
  // TcaComboBox                                                               
  //---------------------------------------------------------------------------

  // Private methods 

procedure TcaComboBox.DrawFlatComboBox;
var
  R: TRect;
  BackColor: TColor;
  Form: TCustomForm;
begin
  BackColor := clBtnFace;
  Form := GetParentForm(Self);
  if Assigned(Form) then
    BackColor := TForm(Form).Color;
  R := GetClientRect;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := BackColor;
  Canvas.Rectangle(R);
  Canvas.Pen.Color := clBlack;
  InflateRect(R, -1, -1);
  Canvas.Rectangle(R);
end;

  // Windows message handlers 

procedure TcaComboBox.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if not Ctl3D then DrawFlatComboBox;
end;

  // Property methods 

function TcaComboBox.GetCtl3D: Boolean;
begin
  Result := inherited Ctl3D;
end;

procedure TcaComboBox.SetCtl3D(const Value: Boolean);
begin
  inherited Ctl3D := Value;
  Invalidate;
end;

end.
