unit caTextStoreEdit;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Grids,
  StdCtrls,
  ValEdit,
  ExtCtrls,
  Clipbrd,

  // ca units 
  caClasses,
  caLog,
  caControls,
  caXPControls;

type

  TcaEditTextForm = class(TForm)
    ListEditor: TValueListEditor;
    TopPanel: TPanel;
    OKButton: TcaXPButton;
    CancelButton: TcaXPButton;
    ClipboardButton: TcaXPButton;
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure ClipboardButtonClick(Sender: TObject);
  private
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

  //---------------------------------------------------------------------------
  // TcaEditTextForm                                                           
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaEditTextForm.Create(AOwner: TComponent);
begin
  inherited;
  ModalResult := mrCancel;
end;

  // Property methods 

function TcaEditTextForm.GetStrings: TStrings;
begin
  Result := ListEditor.Strings;
end;

procedure TcaEditTextForm.SetStrings(const Value: TStrings);
begin
  ListEditor.Strings := Value;
end;

procedure TcaEditTextForm.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TcaEditTextForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TcaEditTextForm.ClipboardButtonClick(Sender: TObject);
var
  Index: Integer;
  Names: TStringList;
  NameValuePairs: TStrings;
begin
  Names := Auto(TStringList.Create).Instance;
  NameValuePairs := GetStrings;
  for Index := 0 to Pred(NameValuePairs.Count) do
    Names.Add(NameValuePairs.Names[Index]);
  Names.Sort;
  Clipboard.AsText := Names.Text;
end;

end.
