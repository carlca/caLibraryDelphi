unit caActionMgrDsgn;

{$INCLUDE ca.inc}

interface

uses
  Sysutils,
  Classes,
  Forms,
  caActionMgrForm,
  {$IFDEF D7_UP}
  DesignIntf,
  DesignEditors;
  {$ELSE}
  DsgnIntf;
  {$ENDIF}

type

 //---------------------------------------------------------------------------
 // TcaActionMgrCompEd
 //---------------------------------------------------------------------------

  TcaActionMgrCompEd = class(TComponentEditor)
  private
    // Private methods
    function OwnerIsForm: Boolean;
  public
    // Public methods
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaActionMgrCompEd
 //---------------------------------------------------------------------------

 // Public methods

function TcaActionMgrCompEd.GetVerb(Index: Integer): String;
begin
  Result := 'Execute Action Manager ...';
end;

function TcaActionMgrCompEd.GetVerbCount: Integer;
begin
  Result := Ord(OwnerIsForm);
end;

procedure TcaActionMgrCompEd.ExecuteVerb(Index: Integer);
var
  Form: TCustomForm;
  ActionMgr: TcaActionManagerForm;
begin
  if OwnerIsForm then
    begin
      Form := TCustomForm(Component.Owner);
      ActionMgr := TcaActionManagerForm.Create(nil);
      try
        ActionMgr.Form := Form;      
        ActionMgr.ShowModal;
      finally
        ActionMgr.Free;
      end;      
    end;
end;

 // Private methods

function TcaActionMgrCompEd.OwnerIsForm: Boolean;
begin
  Result := Component.Owner is TCustomForm;
end;

end.
