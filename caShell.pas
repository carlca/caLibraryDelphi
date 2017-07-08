{ This file is part of the caLibrary (for Delphi 7) package

  Copyright (C) 1999-2017 - Carl Caulkett - carl.caulkett@gmail.com

  MODIFIED LGPL Licence - this is the same licence as that used by the Free Pascal Compiler (FPC)
  A copy of the full licence can be found in the file Licence.md in the same folder as this file.

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version
  with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this library with independent
  modules to produce an executable, regardless of the license terms of these independent modules, and to copy and distribute the
  resulting executable under terms of your choice, provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a module which is not derived from or based on this
  library. If you modify this library, you may extend this exception to your version of the library, but you are not obligated
  to do so. If you do not wish to do so, delete this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to the Free
  Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}


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
