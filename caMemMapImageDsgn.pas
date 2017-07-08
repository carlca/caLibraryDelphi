unit caMemMapImageDsgn;

{$INCLUDE ca.inc}

interface

uses

  Classes,
  SysUtils,
  Dialogs,
  {$IFDEF D7_UP}
  DesignIntf,
  DesignEditors
  {$ELSE}
  DsgnIntf
  {$ENDIF}
  ;

type

  //----------------------------------------------------------------------------
  // TcaBMPFilenameProperty
  //----------------------------------------------------------------------------

  TcaBMPFilenameProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

implementation

procedure TcaBMPFilenameProperty.Edit;
var
  Dlg: TOpenDialog;
begin
  Dlg := TOpenDialog.Create(nil);
  try
    Dlg.Filename := GetValue;
    Dlg.Filter := 'Windows bitmaps (*.BMP)|*.BMP';
    Dlg.Options := Dlg.Options + [ofPathMustExist, ofFileMustExist, ofHideReadOnly];
    if Dlg.Execute then
      SetValue(Dlg.Filename)
  finally
    Dlg.Free;
  end;
end;

function TcaBMPFilenameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable]
end;

end.
