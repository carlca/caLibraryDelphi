unit caHTMLIndexer;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Windows,
  SysUtils,
  Classes,
  Contnrs,

  // ca units
  caClasses,
  caUtils,
  caHTMLParser;

type

 //---------------------------------------------------------------------------
 // TcaHTMLIndexWordList
 //---------------------------------------------------------------------------

  TcaHTMLIndexWordReference = class;

  TcaHTMLIndexWordList = class(TObject)
  private
    // Property fields
    FList: TObjectList;
    // Property methods
    function GetCount: Integer;
    function GetItem(Index: Integer): TcaHTMLIndexWordReference;
  public
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcaHTMLIndexWordReference read GetItem;
  end;

 //---------------------------------------------------------------------------
 // TcaHTMLIndexWordReference
 //---------------------------------------------------------------------------

  TcaHTMLIndexWordReference = class(TObject)
  private
    // Property fields
    FColumn: Integer;
    FFileName: string;
    FLineNumber: Integer;
  public
    // Properties
    property Column: Integer read FColumn write FColumn;
    property FileName: string read FFileName write FFileName;
    property LineNumber: Integer read FLineNumber write FLineNumber;
  end;

 //---------------------------------------------------------------------------
 // IcaHTMLIndexer
 //---------------------------------------------------------------------------

  IcaHTMLIndexer = interface
  ['{4901F594-1A0E-4150-8156-573AD2A83084}']
    // Property methods
    function GetFileList: TStrings;
    function GetFolder: string;
    procedure setFileList(const Value: TStrings);
    procedure SetFolder(const Value: string);
    // Properties
    property FileList: TStrings read GetFileList write setFileList;
    property Folder: string read GetFolder write SetFolder;
  end;

 //---------------------------------------------------------------------------
 // TcaHTMLIndexer
 //---------------------------------------------------------------------------

  TcaHTMLIndexer = class(TcaInterfacedPersistent, IcaHTMLIndexer)
  private
    FFileList: TStrings;
    FFolder: string;
  protected
    // Property methods
    function GetFileList: TStrings;
    function GetFolder: string;
    procedure SetFileList(const Value: TStrings);
    procedure SetFolder(const Value: string);
    // Private methods
    procedure UpdateFileList;
  public
    // Public methods
    // function Add:
    // Properties
    property FileList: TStrings read GetFileList write setFileList;
    property Folder: string read GetFolder write SetFolder;
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaHTMLIndexWordList
 //---------------------------------------------------------------------------

function TcaHTMLIndexWordList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcaHTMLIndexWordList.GetItem(Index: Integer): TcaHTMLIndexWordReference;
begin
  Result := TcaHTMLIndexWordReference(FList[Index]);
end;

 //---------------------------------------------------------------------------
 // TcaHTMLIndexer
 //---------------------------------------------------------------------------

 // Private methods

procedure TcaHTMLIndexer.UpdateFileList;
begin
  Utils.GetFileList(FFolder, '*.htm*', FFileList);
end;

 // Property methods

function TcaHTMLIndexer.GetFileList: TStrings;
begin
  Result := FFileList;
end;

function TcaHTMLIndexer.GetFolder: string;
begin
  Result := FFolder;
end;

procedure TcaHTMLIndexer.SetFileList(const Value: TStrings);
begin
  FFileList.Assign(Value);
end;

procedure TcaHTMLIndexer.SetFolder(const Value: string);
begin
  FFolder := Value;
  UpdateFileList;
end;

end.
