unit caLocalizerForm;

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
  StdCtrls;

type

  //---------------------------------------------------------------------------
  // TcaLocalizerDialog                                                        
  //---------------------------------------------------------------------------

  TcaLocalizerDialog = class(TForm)
    PropListBox: TListBox;
  private
    // Property methods 
    function GetStringProperties: TStrings;
    procedure SetStringProperties(const Value: TStrings);
  public
    // Properties 
    property StringProperties: TStrings read GetStringProperties write SetStringProperties;
  end;

implementation

{$R *.dfm}

{ TcaLocalizerDialog }

function TcaLocalizerDialog.GetStringProperties: TStrings;
begin
  Result := PropListBox.Items;
end;

procedure TcaLocalizerDialog.SetStringProperties(const Value: TStrings);
begin
  PropListBox.Items := Value;
end;

end.
