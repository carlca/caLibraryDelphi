unit caActionsReg;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Classes,
  ActnList,
  StdCtrls,
  Forms,

  // ca units
  caActions;

procedure Register;

implementation

procedure Register;
begin
  RegisterActions('caActions', [
    TcaCloseFormAction,
    TcaStayOnTopFormAction,
    TcaCheckAction,
    TcaOpenDialogAction,
    TcaSaveDialogAction
  ], nil);
end;

end.
