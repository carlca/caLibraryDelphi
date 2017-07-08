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


unit caActions;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units 
  Windows,
  Classes,
  ActnList,
  StdCtrls,
  Forms,
  Controls,
  Dialogs,

  // ca units 
  caTypes,
  caUtils,
  caConsts,
  caForms;

type

  TcaCloseFormEvent = procedure(Sender: TObject; SaveNeeded: Boolean) of object;

  //---------------------------------------------------------------------------
  // TcaFormAction                                                             
  //---------------------------------------------------------------------------

  TcaFormAction = class(TAction)
  protected
    // Protected methods 
    function GetForm(Target: TObject): TForm; virtual;
  public
    // Public methods 
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  //---------------------------------------------------------------------------
  // TcaCloseFormAction                                                        
  //---------------------------------------------------------------------------

  TcaCloseFormAction = class(TcaFormAction)
  private
    // Property fields 
    FCloseNeeded: Boolean;
    FHasChanged: Boolean;
    FOnBeforeCloseForm: TcaCloseFormEvent;
    // Event handlers 
    procedure FormCloseEvent(Sender: TObject; var Action: TCloseAction);
  protected
    // Protected methods 
    procedure DoBeforeCloseForm(SaveNeeded: Boolean); virtual;
  public
    // Public methods 
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
    // Public properties 
    property HasChanged: Boolean read FHasChanged write FHasChanged;
  published
    // Published properties 
    property OnBeforeCloseForm: TcaCloseFormEvent read FOnBeforeCloseForm write FOnBeforeCloseForm;
  end;

  //---------------------------------------------------------------------------
  // TcaStayOnTopFormAction                                                    
  //---------------------------------------------------------------------------

  TcaStayOnTopFormAction = class(TcaFormAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

  //---------------------------------------------------------------------------
  // TcaCheckAction                                                            
  //---------------------------------------------------------------------------

  TcaCheckAction = class(TAction)
  private
    // Private fields 
    FChecked: Boolean;
  public
    // Public methods 
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
    // Properties 
    property Checked: Boolean read FChecked write FChecked;
  end;

  //---------------------------------------------------------------------------
  // TcaDialogAction                                                           
  //---------------------------------------------------------------------------

  TcaDialogAction = class(TAction)
  private
    // Property fields 
    FDialog: TOpenDialog;
    FSucceeded: Boolean;
  protected
    // Protected methods 
    function CreateDialog: TOpenDialog; virtual; abstract;
  public
    // Create/Destroy 
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Public methods 
    function Execute: Boolean; override;
    // Properties 
    property Dialog: TOpenDialog read FDialog;
    property Succeeded: Boolean read FSucceeded;
  end;

  //---------------------------------------------------------------------------
  // TcaOpenDialogAction                                                       
  //---------------------------------------------------------------------------

  TcaOpenDialogAction = class(TcaDialogAction)
  protected
    // Protected methods 
    function CreateDialog: TOpenDialog; override;
  end;

  //---------------------------------------------------------------------------
  // TcaSaveDialogAction                                                       
  //---------------------------------------------------------------------------

  TcaSaveDialogAction = class(TcaDialogAction)
  protected
    // Protected methods 
    function CreateDialog: TOpenDialog; override;
  end;

implementation

uses
  TypInfo;

  //---------------------------------------------------------------------------
  // TcaFormAction                                                             
  //---------------------------------------------------------------------------

  // Public methods 

function TcaFormAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := Target is TForm;
end;

procedure TcaFormAction.UpdateTarget(Target: TObject);
begin
  Enabled := Target <> nil;
end;

  // Protected methods 

function TcaFormAction.GetForm(Target: TObject): TForm;
begin
  Result := Target as TForm;
end;

  //---------------------------------------------------------------------------
  // TcaCloseFormAction                                                        
  //---------------------------------------------------------------------------

  // Public methods 

procedure TcaCloseFormAction.ExecuteTarget(Target: TObject);
var
  Response: TcaMsgDialogResponse;
  SaveNeeded: Boolean;
begin
  SaveNeeded := False;
  FCloseNeeded := True;
  if FHasChanged then
    begin
      Response := Utils.QuerySaveData(GetForm(Target).Handle);
      SaveNeeded := Response = mgYes;
      FCloseNeeded := Response <> mgCancel;
    end;
  if FCloseNeeded then
    begin
      DoBeforeCloseForm(SaveNeeded);
      GetForm(Target).OnClose := nil;
      GetForm(Target).Close;
    end;
end;

procedure TcaCloseFormAction.UpdateTarget(Target: TObject);
begin
  inherited;
  if not Assigned(GetForm(Target).OnClose) then
    GetForm(Target).OnClose := FormCloseEvent;
end;

  // Protected methods 

procedure TcaCloseFormAction.DoBeforeCloseForm(SaveNeeded: Boolean);
begin
  if Assigned(FOnBeforeCloseForm) then FOnBeforeCloseForm(Self, SaveNeeded);
end;

  // Event handlers 

procedure TcaCloseFormAction.FormCloseEvent(Sender: TObject; var Action: TCloseAction);
begin
  ExecuteTarget(Sender);
  if not FCloseNeeded then Action := caNone;
end;

  //---------------------------------------------------------------------------
  // TcaStayOnTopFormAction                                                    
  //---------------------------------------------------------------------------

procedure TcaStayOnTopFormAction.ExecuteTarget(Target: TObject);
begin
  Checked := not Checked;
  FormUtils.SetStayOnTopState(GetForm(Target), Checked);
end;

  //---------------------------------------------------------------------------
  // TcaCheckAction                                                            
  //---------------------------------------------------------------------------

function TcaCheckAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := (Target is TControl) and IsPublishedProp(Target, cChecked) and (TControl(Target).Action = Self);
  if Result then
    FChecked := GetPropValue(Target, cChecked, False);
end;

procedure TcaCheckAction.UpdateTarget(Target: TObject);
begin
  Enabled := Target <> nil;
end;

  //---------------------------------------------------------------------------
  // TcaDialogAction                                                           
  //---------------------------------------------------------------------------

constructor TcaDialogAction.Create(AOwner: TComponent);
begin
  inherited;
  FDialog := CreateDialog;
end;

destructor TcaDialogAction.Destroy;
begin
  FDialog.Free;
  inherited;
end;

function TcaDialogAction.Execute: Boolean;
begin
  FSucceeded := FDialog.Execute;
  Result := inherited Execute;
end;

  //---------------------------------------------------------------------------
  // TcaOpenDialogAction                                                       
  //---------------------------------------------------------------------------

  // Protected methods 

function TcaOpenDialogAction.CreateDialog: TOpenDialog;
begin
  Result := TOpenDialog.Create(nil);
end;

  //---------------------------------------------------------------------------
  // TcaSaveDialogAction                                                       
  //---------------------------------------------------------------------------

function TcaSaveDialogAction.CreateDialog: TOpenDialog;
begin
  Result := TSaveDialog.Create(nil);
end;

end.


