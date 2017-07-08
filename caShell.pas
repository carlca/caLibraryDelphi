unit caShell;

{$INCLUDE ca.inc}

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
  StdCtrls,
  ComCtrls,
  ShellCtrls,
  ExtCtrls,
  Buttons,

  // ca units 
  caControls,
  caButtons;

type

  //---------------------------------------------------------------------------
  // TcaSelectFolderForm                                                       
  //---------------------------------------------------------------------------

  TcaSelectFolderForm = class(TForm)
    ShellTree: TShellTreeView;
    ButtonPanel: TPanel;
    OKButton: TButton;
    CancelButton: TButton;
    ToolbarPanel: TPanel;
    DesktopButton: TcaSpeedButton;
    MyComputerButton: TcaSpeedButton;
    NetworkButton: TcaSpeedButton;
    UpperSpacer: TcaPanel;
    LowerSpacer: TcaPanel;
    procedure FormResize(Sender: TObject);
    procedure ShellSelectClick(Sender: TObject);
  private
    // Private methods 
    function GetPath: string;
    function GetRoot: string;
    procedure SetPath(const Value: string);
    procedure SetRoot(const Value: string);
  public
    // Properties 
    property Path: string read GetPath write SetPath;
    property Root: string read GetRoot write SetRoot;
  end;

  //---------------------------------------------------------------------------
  // IcaShellUtils                                                             
  //---------------------------------------------------------------------------

  IcaShellUtils = interface
  ['{125A1A01-AE43-4852-81C3-59004F23B292}']
    // Interface methods 
    function SelectFolder(const APath: String; var ARoot, AFolder: String): Boolean;
  end;

  //---------------------------------------------------------------------------
  // TcaShellUtils                                                             
  //---------------------------------------------------------------------------

  TcaShellUtils = class(TInterfacedObject, IcaShellUtils)
  public
    // Interface methods 
    function SelectFolder(const APath: String; var ARoot, AFolder: String): Boolean;
  end;

  //---------------------------------------------------------------------------
  // CoShellUtilsFactory                                                       
  //---------------------------------------------------------------------------

  CoShellUtilsFactory = class
  public
    class function Instance: IcaShellUtils;
  end;

var
  ShellUtils: IcaShellUtils;

const
  cShellDesktop      = 'rfDesktop';
  cShellMyComputer   = 'rfMyComputer';
  cShellNetwork      = 'rfNetwork';

implementation

{$R *.dfm}

  //---------------------------------------------------------------------------
  // TcaSelectFolderForm                                                       
  //---------------------------------------------------------------------------

  // Event handlers 

procedure TcaSelectFolderForm.FormResize(Sender: TObject);
var
  MidPos: Integer;
begin
  MidPos := ButtonPanel.Width div 2;
  OKButton.Left := MidPos - OKButton.Width - 2;
  CancelButton.Left := MidPos + 2;
end;

procedure TcaSelectFolderForm.ShellSelectClick(Sender: TObject);
begin
  if Sender = DesktopButton then
    ShellTree.Root := cShellDesktop else
  if Sender = MyComputerButton then
    ShellTree.Root := cShellMyComputer else
  if Sender = NetworkButton then
    ShellTree.Root := cShellNetwork;
end;

  // Property methods 

function TcaSelectFolderForm.GetPath: string;
begin
  Result := ShellTree.Path;
end;

function TcaSelectFolderForm.GetRoot: string;
begin
  Result := ShellTree.Root;
end;

procedure TcaSelectFolderForm.SetPath(const Value: string);
begin
  if DirectoryExists(Value) then
    ShellTree.Path := Value;
end;

procedure TcaSelectFolderForm.SetRoot(const Value: string);
begin
  if Value = cShellDesktop then
    DesktopButton.Down := True else
  if Value = cShellMyComputer then
    MyComputerButton.Down := True else
  if Value = cShellNetwork then
    NetworkButton.Down := True
  else
    DesktopButton.Down := True;
  ShellTree.Root := Value;
end;

  //---------------------------------------------------------------------------
  // TcaShellUtils                                                             
  //---------------------------------------------------------------------------

  // Interface methods 

function TcaShellUtils.SelectFolder(const APath: String; var ARoot, AFolder: String): Boolean;
var
  Dlg: TcaSelectFolderForm;
begin
  AFolder := '';
  Dlg := TcaSelectFolderForm.Create(nil);
  try
    if ARoot <> '' then Dlg.Root := ARoot;
    if APath <> '' then Dlg.Path := APath;
    Result := Dlg.ShowModal = mrOk;
    if Result then
      begin
        AFolder := Dlg.Path;
        ARoot := Dlg.Root;
      end;
  finally
    Dlg.Free;
  end;
end;

  //---------------------------------------------------------------------------
  // CoShellUtilsFactory                                                       
  //---------------------------------------------------------------------------

class function CoShellUtilsFactory.Instance: IcaShellUtils;
const
  FInstance: IcaShellUtils = nil;
begin
  if not Assigned(FInstance) then
    FInstance := TcaShellUtils.Create;
  Result := FInstance;
end;

  //---------------------------------------------------------------------------
  // Initialization                                                            
  //---------------------------------------------------------------------------

initialization
  ShellUtils := CoShellUtilsFactory.Instance;

end.
