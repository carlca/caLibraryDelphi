unit caXPTest;

{$INCLUDE ca.inc}

interface

uses
  Windows, 
  SysUtils, 
  Messages, 
  Classes, 
  Graphics, 
  Controls, 
  Forms, 
  Dialogs, 
  Menus, 
  StdCtrls, 
  ExtCtrls, 
  caXPControls;

type
  TcaXPTest = class(TcaXPGraphicControl)
  private
    FTestProp: string;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property TestProp: string read FTestProp write FTestProp;
  end;

implementation

destructor TcaXPTest.Destroy;
begin
  inherited Destroy;
end;

constructor TcaXPTest.Create(AOwner: TComponent); 
begin
  inherited Create(AOwner);
end;

initialization
finalization
end.
