unit caExcel;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Windows,
  SysUtils,
  Classes,
  Graphics,

  // ca units
  caTypes,
  caConsts,
  caClasses,
  caUtils;

type

 //---------------------------------------------------------------------------
 // IcaExcelFactory
 //---------------------------------------------------------------------------

  IcaExcelFactory = interface
  ['{0082C127-E383-4A92-8CA3-8529C14BBC78}']
    procedure AddHeaderColumn(const AColumnName: String);
    procedure AddRowColumn(const AColumnName, AColumnData: String);
    procedure SaveXLSToFile(const AFileName: String);
    procedure SaveXLSToStream(const AStream: TStream);
    procedure WriteHeader;
    procedure WriteRow;
  end;

 //---------------------------------------------------------------------------
 // TcaExcelFactory
 //---------------------------------------------------------------------------

  TcaExcelFactory = class(TcaStringList, IcaExcelFactory)
  private
    FHeaderColumns: IcaStringList;
    FRowColumns: IcaStringList;
    function Separator: Char;
    function Quote: Char;
  public
    constructor Create;
    procedure AddHeaderColumn(const AColumnName: String);
    procedure AddRowColumn(const AColumnName, AColumnData: String);
    procedure SaveXLSToFile(const AFileName: String);
    procedure SaveXLSToStream(const AStream: TStream);
    procedure WriteHeader;
    procedure WriteRow;
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaExcelFactory
 //---------------------------------------------------------------------------

constructor TcaExcelFactory.Create;
begin
  inherited;
  FHeaderColumns := TcaStringList.Create;
  FRowColumns := TcaStringList.Create;
end;

procedure TcaExcelFactory.AddHeaderColumn(const AColumnName: String);
begin
  if FHeaderColumns.IndexOf(AColumnName) = -1 then
    FHeaderColumns.Add(AColumnName);
end;

procedure TcaExcelFactory.AddRowColumn(const AColumnName, AColumnData: String);
var
  ColumnIndex: Integer;
begin
  ColumnIndex := FHeaderColumns.IndexOf(AColumnName);
  if ColumnIndex = -1 then
    raise EcaException.Create('Column does not exist');
  FRowColumns.Values[IntToStr(ColumnIndex)] := AColumnData;
end;

function TcaExcelFactory.Separator: Char;
begin
  Result := #9;
end;

function TcaExcelFactory.Quote: Char;
begin
  Result := '"';
end;

procedure TcaExcelFactory.SaveXLSToFile(const AFileName: String);
begin
  SaveToFile(AFileName);
end;

procedure TcaExcelFactory.SaveXLSToStream(const AStream: TStream);
begin
  SaveToStream(AStream);
end;

procedure TcaExcelFactory.WriteHeader;
var
  Index: Integer;
  Header: String;
begin
  Header := '';
  for Index := 0 to FHeaderColumns.Count - 1 do
    Header := Header + Quote + FHeaderColumns[Index] + Quote + Separator;
  Utils.DeleteFromEnd(Header, 1);
  Add(Header);
end;

procedure TcaExcelFactory.WriteRow;
var
  Index: Integer;
  Row: String;
  RowColumn: String;
begin
  Row := '';
  for Index := 0 to FHeaderColumns.Count - 1 do
    begin
      RowColumn := FRowColumns.Values[IntToStr(Index)];
      if not Utils.IsNumeric(RowColumn) then
        RowColumn := Quote + RowColumn + Quote;
      Row := Row + RowColumn + Separator;
    end;
  Utils.DeleteFromEnd(Row, 1);
  Add(Row);
  FRowColumns.Clear;
end;

end.
