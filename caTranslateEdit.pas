unit caTranslateEdit;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  Messages,
  SysUtils,
  {$IFDEF D7_UP}
  Variants,
  {$ENDIF}
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Grids,
  StdCtrls,
  {$IFDEF D7_UP}
  ValEdit,
  {$ENDIF}
  caLog;

type

  {$IFDEF D5}

  //---------------------------------------------------------------------------
  // TcaEditTextForm                                                           
  //---------------------------------------------------------------------------

  TValueListEditor = class(TMemo)
  private
    // Property methods 
    function GetStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
  public
    // Properties 
    property Strings: TStrings read GetStrings write SetStrings;
  end;

  {$ENDIF}

  TcaEditTextForm = class(TForm)
  private
    // Private fields 
    FListEditor: TValueListEditor;
    // Property methods 
    function GetStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    // Properties 
    property Strings: TStrings read GetStrings write SetStrings;
  end;

var
  caEditTextForm: TcaEditTextForm;

implementation

{$R *.dfm}

  {$IFDEF D5}

  //---------------------------------------------------------------------------
  // TcaEditTextForm                                                           
  //---------------------------------------------------------------------------

function TValueListEditor.GetStrings: TStrings;
begin
  Result := inherited Lines;
end;

procedure TValueListEditor.SetStrings(const Value: TStrings);
begin
  inherited Lines := Value;
end;

  {$ENDIF}

  //---------------------------------------------------------------------------
  // TcaEditTextForm                                                           
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaEditTextForm.Create(AOwner: TComponent);
begin
  inherited;
  FListEditor := TValueListEditor.Create(Self);
  FListEditor.Align := alClient;
  FListEditor.Parent := Self;
end;

  // Property methods 

function TcaEditTextForm.GetStrings: TStrings;
begin
  Result := FListEditor.Strings;
end;

procedure TcaEditTextForm.SetStrings(const Value: TStrings);
begin
  FListEditor.Strings := Value;
end;

end.
