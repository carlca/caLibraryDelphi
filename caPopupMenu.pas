unit caPopupMenu;

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
  Menus;

type

  TcaPopupEvent = procedure(Sender: TObject; var X, Y: Integer) of object;

  //```````````````````````````````````````````````````````````````````````````
  // TcaPopupMenu                                                              
  //```````````````````````````````````````````````````````````````````````````

  TcaPopupMenu = class(TPopupMenu)
  private
    // Property fields 
    FLastPopupPosition: TPoint;
    // Event property fields 
    FOnAfterPopupXY: TcaPopupEvent;
    FOnBeforePopupXY: TcaPopupEvent;
  protected
    // Event trigger methods 
    procedure DoAfterPopupXY(var X, Y: Integer); virtual;
    procedure DoBeforePopupXY(var X, Y: Integer); virtual;
  public
    // Public methods 
    procedure Popup(X, Y: Integer); override;
    // Properties 
    property LastPopupPosition: TPoint read FLastPopupPosition;
  published
    // Event properties 
    property OnBeforePopupXY: TcaPopupEvent read FOnBeforePopupXY write FOnBeforePopupXY;
    property OnAfterPopupXY: TcaPopupEvent read FOnAfterPopupXY write FOnAfterPopupXY;
  end;

implementation

  //```````````````````````````````````````````````````````````````````````````
  // TcaPopupMenu                                                              
  //```````````````````````````````````````````````````````````````````````````

  // Public methods 

procedure TcaPopupMenu.Popup(X, Y: Integer);
begin
  DoBeforePopupXY(X, Y);
  inherited;
  DoAfterPopupXY(X, Y);
  FLastPopupPosition.X := X;
  FLastPopupPosition.Y := Y;
end;

  // Event trigger methods 

procedure TcaPopupMenu.DoAfterPopupXY(var X, Y: Integer);
begin
  if Assigned(FOnAfterPopupXY) then FOnAfterPopupXY(Self, X, Y);
end;

procedure TcaPopupMenu.DoBeforePopupXY(var X, Y: Integer);
begin
  if Assigned(FOnBeforePopupXY) then FOnBeforePopupXY(Self, X, Y);
end;

end.
