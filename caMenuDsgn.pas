unit caMenuDsgn;

{$INCLUDE ca.inc}

interface

uses
  Sysutils,
  Classes,
  {$IFDEF D7_UP}
  DesignIntf,
  DesignEditors;
  {$ELSE}
  DsgnIntf;
  {$ENDIF}

type

  //---------------------------------------------------------------------------
  // TcaMenuCompEd                                                             
  //---------------------------------------------------------------------------

  TcaMenuCompEd = class(TComponentEditor)
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

uses
  caMenu;

  //---------------------------------------------------------------------------
  // TcaMenuCompEd                                                             
  //---------------------------------------------------------------------------

function TcaMenuCompEd.GetVerbCount: Integer;
begin
  Result := 1;
  if Component is TcaMenuPage then Inc(Result);
end;

function TcaMenuCompEd.GetVerb(Index: Integer): String;
begin
  case Index of
    0:  Result := 'Add Menu Page';
    1:  Result := 'Add Menu Button';
  end;
end;

procedure TcaMenuCompEd.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:  if Component is TcaMenu then
          TcaMenu(Component).AddMenuPage
        else
          TcaMenuPage(Component).Menu.AddMenuPage;
    1:  if Component is TcaMenuPage then
          TcaMenuPage(Component).AddButton('', nil);
  end;
  Designer.Modified;
end;


end.
