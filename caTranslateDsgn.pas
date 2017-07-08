unit caTranslateDsgn;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Classes,
  Sysutils,

  {$IFDEF D7_UP}
  DesignIntf,
  DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}

  // ca units 
  caLog,
  caForms,
  caTextStore;

type

  //---------------------------------------------------------------------------
  // TcaTextStoreComponentEditor                                               
  //---------------------------------------------------------------------------

  TcaTextStoreComponentEditor = class(TComponentEditor)
  public
    // Overridden methods 
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  caTranslateEdit;

  //---------------------------------------------------------------------------
  // TcaTextStoreComponentEditor                                               
  //---------------------------------------------------------------------------

procedure TcaTextStoreComponentEditor.ExecuteVerb(Index: Integer);
var
  Editor: TcaEditTextForm;
  TextStore: TcaTextStore;
  OldStrings: string;
begin
  TextStore := TcaTextStore(Component);
  Editor := TcaEditTextForm.Create(nil);
  try
    OldStrings := TextStore.Strings.Text;
    Editor.Strings := TextStore.Strings;
    Editor.ShowModal;
    TextStore.Strings := Editor.Strings;
    if TextStore.Strings.Text <> OldStrings then
      Designer.Modified;
  finally
    Editor.Free;
  end;    
end;

function TcaTextStoreComponentEditor.GetVerb(Index: Integer): String;
begin
  Result := 'Edit...';
end;

function TcaTextStoreComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
