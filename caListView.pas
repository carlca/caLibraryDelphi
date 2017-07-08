unit caListView;

interface

uses

  // Standard Delphi units 
  Windows,
  SysUtils,
  Classes,
  Controls,
  ComCtrls;

type

  //---------------------------------------------------------------------------
  // TcaListView                                                               
  //---------------------------------------------------------------------------

  TcaListView = class(TListView)
  private
    // Property fields 
    FAutoResizeColumns: Boolean;
    FColumnCount: Integer;
    // Property methods 
    function GetColumnCount: Integer;
    procedure SetAutoResizeColumns(const Value: Boolean);
    procedure SetColumnCount(const Value: Integer);
    // Private methods 
    procedure ResizeColumns;
    procedure UpdateColumns;
  protected
    // Protected methods 
    procedure CreateWnd; override;
    procedure Resize; override;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
  published
    // Published properties 
    property AutoResizeColumns: Boolean read FAutoResizeColumns write SetAutoResizeColumns;

    property ColumnCount: Integer read GetColumnCount write SetColumnCount;
  end;

implementation

  //---------------------------------------------------------------------------
  // TcaListView                                                               
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaListView.Create(AOwner: TComponent);
begin
  inherited;
  ViewStyle := vsReport;
end;

  // Protected methods 

procedure TcaListView.CreateWnd;
begin
  inherited;
  UpdateColumns;
end;

procedure TcaListView.Resize;
begin
  inherited;
  if FAutoResizeColumns then ResizeColumns;
end;

  // Private methods 

procedure TcaListView.ResizeColumns;
var
  AvailWidth: Integer;
  Index: Integer;
begin
  inherited;
  AvailWidth := ClientWidth - GetSystemMetrics(SM_CXVSCROLL);
  for Index := 0 to Pred(Columns.Count) do
    Columns[Index].Width := AvailWidth div Columns.Count;
end;

procedure TcaListView.UpdateColumns;
var
  Index: Integer;
begin
  Columns.Clear;
  for Index := 0 to Pred(FColumnCount) do
    Columns.Add;
end;

  // Properties methods 

function TcaListView.GetColumnCount: Integer;
begin
  Result := Columns.Count;
end;

procedure TcaListView.SetAutoResizeColumns(const Value: Boolean);
begin
  if Value <> FAutoResizeColumns then
    begin
      FAutoResizeColumns := Value;
      Resize;
    end;
end;

procedure TcaListView.SetColumnCount(const Value: Integer);
begin
  if Value <> FColumnCount then
    begin
      FColumnCount := Value;
      if HandleAllocated then
        UpdateColumns;
    end;
end;

end.
