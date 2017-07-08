unit caActionMgrForm;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  TypInfo,
  ActnList,
  Buttons,

  // ca units
  caLog,
  caClasses;

type

  TControlEx = class(TControl);

 //---------------------------------------------------------------------------
 // TcaActionManagerForm
 //---------------------------------------------------------------------------

  TcaActionManagerForm = class(TForm)
    ActionPanel: TPanel;
    ActionsLabel: TLabel;
    ActionsList: TListBox;
    ControlsLabel: TLabel;
    ControlsList: TListBox;
    ControlsPanel: TPanel;
    LeftSplitter: TSplitter;
    LinksLabel: TLabel;
    LinksList: TListBox;
    LinksPanel: TPanel;
    RightSplitter: TSplitter;
    ToolbarPanel: TPanel;
    CloseButton: TSpeedButton;
    LinkButton: TSpeedButton;
    UnlinkButton: TSpeedButton;
    CreateActionsButton: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure LinkButtonClick(Sender: TObject);
    procedure UnlinkButtonClick(Sender: TObject);
    procedure CreateActionsButtonClick(Sender: TObject);
  private
    // Property fields
    FForm: TCustomForm;
    // Private methods
    procedure UpdateLists;
    procedure UpdateLinks;
  public
    property Form: TCustomForm read FForm write FForm;
  end;

implementation

{$R *.DFM}

 //---------------------------------------------------------------------------
 // TcaActionManagerForm
 //---------------------------------------------------------------------------

 // Private methods

procedure TcaActionManagerForm.UpdateLists;
var
  Comp: TComponent;
  CompIndex: Integer;
begin
  ActionsList.Clear;
  ControlsList.Clear;
  ActionsList.Items.BeginUpdate; try
  ControlsList.Items.BeginUpdate; try
  for CompIndex := 0 to FForm.ComponentCount - 1 do
    begin
      Comp := FForm.Components[CompIndex];
      if (Comp is TControl) and IsPublishedProp(Comp, 'Action') then
        ControlsList.Items.AddObject(Comp.Name, Comp)
      else
        begin
          if Comp is TCustomAction then
            begin
              Log.Send('Comp.ClassName', Comp.ClassName);
              ActionsList.Items.AddObject(Comp.Name, Comp);
            end;
        end;
    end;
  finally ControlsList.Items.EndUpdate; end;
  finally ActionsList.Items.EndUpdate; end;
end;

procedure TcaActionManagerForm.UpdateLinks;
var
  Action: TCustomAction;
  ActionIndex: Integer;
  ControlIndex: Integer;
  Control: TControl;
begin
  LinksList.Clear;
  LinksList.Items.BeginUpdate; try
  for ActionIndex := 0 to ActionsList.Items.Count - 1 do
    begin
      Action := TCustomAction(ActionsList.Items.Objects[ActionIndex]);
      for ControlIndex := 0 to ControlsList.Items.Count - 1 do
        begin
          Control := TControl(ControlsList.Items.Objects[ControlIndex]);
          if Control.Action = ActionsList.Items.Objects[ActionIndex] then
            LinksList.Items.AddObject(Format('[%s]  ->  [%s]', [Control.Name, Action.Name]), Control);
        end;
    end;
  finally LinksList.Items.EndUpdate; end;
end;

 // Event handlers

procedure TcaActionManagerForm.FormShow(Sender: TObject);
begin
  UpdateLists;
  UpdateLinks;
end;

procedure TcaActionManagerForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TcaActionManagerForm.LinkButtonClick(Sender: TObject);
var
  Action: TCustomAction;
  ControlIndex: Integer;
  Control: TControl;
begin
  if ActionsList.ItemIndex >= 0 then
    begin
      Action := TCustomAction(ActionsList.Items.Objects[ActionsList.ItemIndex]);
      for ControlIndex := 0 to ControlsList.Items.Count - 1 do
        begin
          if ControlsList.Selected[ControlIndex] then
            begin
              Control := TControl(ControlsList.Items.Objects[ControlIndex]);
              Control.Action := Action;
            end;
        end;
    end;
  UpdateLinks;
  FForm.Designer.Modified;
end;

procedure TcaActionManagerForm.UnlinkButtonClick(Sender: TObject);
var
  Control: TControl;
  LinkIndex: Integer;
begin
  for LinkIndex := 0 to LinksList.Items.Count - 1 do
    begin
      if LinksList.Selected[LinkIndex] then
        begin
          Control := TControl(LinksList.Items.Objects[LinkIndex]);
          Control.Action := nil;
        end;
    end;
  UpdateLinks;
  FForm.Designer.Modified;
end;

procedure TcaActionManagerForm.CreateActionsButtonClick(Sender: TObject);
var
  Action: TAction;
  ActionList: TActionList;
  ActionName: IcaString;
  Comp: TComponent;
  CompIndex: Integer;
  Control: TControl;
  ControlIndex: Integer;
begin
  ActionList := nil;
  for CompIndex := 0 to FForm.ComponentCount - 1 do
    begin
      Comp := FForm.Components[CompIndex];
      if Comp is TActionList then
        ActionList := TActionList(Comp);
    end;
  if ActionList <> nil then
    begin
      for ControlIndex := 0 to ControlsList.Items.Count - 1 do
        begin
          if ControlsList.Selected[ControlIndex] then
            begin
              Control := TControl(ControlsList.Items.Objects[ControlIndex]);
              if Control.Action = nil then
                begin
                  ActionName := TcaString.Create(Control.Name);
                  if ActionName.Right(6) = 'Button' then
                    begin
                      ActionName.DeleteFromEnd(6);
                      ActionName.Add('Action');
                      if FForm.FindComponent(ActionName.S) = nil then
                        begin
                          Action := TAction.Create(FForm);
                          Action.Name := ActionName.S;
                          Action.Caption := TControlEx(Control).Caption;
                          Action.ActionList := ActionList;
                          Control.Action := Action;
                        end;
                    end;
                end;
            end;
        end;
    end;
  UpdateLists;
  UpdateLinks;
end;

end.
