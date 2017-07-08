unit caConsole;

interface

uses

  // Standard Delphi units
  Classes,
  SysUtils;

type

 //---------------------------------------------------------------------------
 // TcaIOStream
 //---------------------------------------------------------------------------

  TcaIOStream = class(THandleStream)
  private
    FIOHandle: Integer;
  protected
    function GetIOHandle: Integer; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
  end;

 //---------------------------------------------------------------------------
 // TcaConsoleInput
 //---------------------------------------------------------------------------

  TcaConsoleInput = class(TcaIOStream)
  private
  protected
    function GetIOHandle: Integer; override;
  public
  end;

 //---------------------------------------------------------------------------
 // TcaConsoleOutput
 //---------------------------------------------------------------------------

  TcaConsoleOutput = class(TcaIOStream)
  private
  protected
    function GetIOHandle: Integer; override;
  public
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaIOStream
 //---------------------------------------------------------------------------

constructor TcaIOStream.Create;
begin
  FIOHandle := GetIOHandle;
  inherited Create(FIOHandle);
end;

destructor TcaIOStream.Destroy;
begin
  inherited;     
end;

 //---------------------------------------------------------------------------
 // TcaConsoleInput
 //---------------------------------------------------------------------------

function TcaConsoleInput.GetIOHandle: Integer;
begin
  Result := TTextRec(Input).Handle;
end;

 //---------------------------------------------------------------------------
 // TcaConsoleOutput
 //---------------------------------------------------------------------------

function TcaConsoleOutput.GetIOHandle: Integer;
begin
  Result := TTextRec(Output).Handle;
end;

end.
