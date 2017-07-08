unit caXMLDataDsgn;

{$INCLUDE ca.inc}

interface

uses

  Classes,
  SysUtils,
  Dialogs,
  {$IFDEF D7_UP}
  DesignIntf,
  DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  caXMLData;

type

 //---------------------------------------------------------------------------
 // TcaXMLDataCompEditor
 //---------------------------------------------------------------------------

  TcaXMLDataCompEditor = class(TComponentEditor)
  private
    // Private methods
    procedure LoadXMLData;
    procedure SaveXMLData;
  public
    // Public methods
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaXMLDataCompEditor
 //---------------------------------------------------------------------------

procedure TcaXMLDataCompEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0:  LoadXMLData;
    1:  SaveXMLData;
  end;
end;

function TcaXMLDataCompEditor.GetVerb(Index: Integer): String;
begin
  Result := '';
  case Index of
    0:  Result := 'Load XML Data';
    1:  Result := 'Save XML Data';
  end;
end;

function TcaXMLDataCompEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

 // Private methods

procedure TcaXMLDataCompEditor.LoadXMLData;
var
  OpenDlg: TOpenDialog;
  XMLDataset: TcaXMLDataset;
begin
  OpenDlg := TOpenDialog.Create(nil);
  try
    OpenDlg.Filter := '*.xml|XML files';
    if OpenDlg.Execute then
      begin
        XMLDataset := TcaXMLDataset(Component);
        XMLDataset.FileName := OpenDlg.FileName;
        XMLDataset.Load;
      end;
  finally
    OpenDlg.Free;
  end;
end;

procedure TcaXMLDataCompEditor.SaveXMLData;
var
  SaveDlg: TSaveDialog;
  XMLDataset: TcaXMLDataset;
begin
  SaveDlg := TSaveDialog.Create(nil);
  try
    SaveDlg.Filter := '*.xml|XML files';
    if SaveDlg.Execute then
      begin
        XMLDataset := TcaXMLDataset(Component);
        XMLDataset.FileName := SaveDlg.FileName;
        XMLDataset.Save;
      end;
  finally
    SaveDlg.Free;
  end;
end;

end.
