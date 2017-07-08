unit caCompPalette;

{$INCLUDE ca.inc}

interface

uses
  Classes, SysUtils, Forms, ComCtrls;

type

  TcaCompPalette = class(TComponent)
  private
    FStyle: TTabStyle;
    FTabIndex: Integer;
    function FindDockedPalette: TTabControl;
    function FindFloatingPalette: TTabControl;
    function FindPalette: TTabControl;
    procedure SetStyle(const Value: TTabStyle);
    procedure SetTabIndex(const Value: Integer);
    procedure UpdateComponentPaletteStyle;
    procedure UpdateComponentPaletteTabIndex;
  published
    property Style: TTabStyle read FStyle write SetStyle;
    property TabIndex: Integer read FTabIndex write SetTabIndex;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TcaCompPalette]);
end;

{ TcaCompPalette }

function TcaCompPalette.FindDockedPalette: TTabControl;
var
  Index: Integer;
  MainForm: TForm;
begin
  Result := nil;
  MainForm := Application.MainForm;
  for Index := 0 to MainForm.ComponentCount - 1 do
    begin
      if MainForm.Components[Index].ClassName = 'TComponentPaletteTabControl' then
        begin
          Result := TTabControl(MainForm.Components[Index]);
          Break;
        end;
    end;
end;

function TcaCompPalette.FindFloatingPalette: TTabControl;
var
  Index: Integer;
  PaletteForm: TForm;
begin
  Result := nil;
  PaletteForm := nil;
  for Index := 0 to Screen.FormCount - 1 do
    begin
      if Screen.Forms[Index].Caption = 'Component Palette' then
        begin
          PaletteForm := Screen.Forms[Index];
          Break;
        end;
    end;
  if PaletteForm <> nil then
    begin
      for Index := 0 to PaletteForm.ComponentCount - 1 do
        begin
          if PaletteForm.Components[Index] is TTabControl then
            begin
              Result := TTabControl(PaletteForm.Components[Index]);
              Break;
            end;
        end;
    end;
end;

function TcaCompPalette.FindPalette: TTabControl;
begin
  Result := nil;
  if csDesigning in ComponentState then
    begin
      Result := FindFloatingPalette;
      if Result = nil then
        Result := FindDockedPalette;
    end;
end;

procedure TcaCompPalette.UpdateComponentPaletteStyle;
var
  Palette: TTabControl;
begin
  Palette := FindPalette;
  if Palette <> nil then
    Palette.Style := FStyle;
end;

procedure TcaCompPalette.UpdateComponentPaletteTabIndex;
var
  Palette: TTabControl;
begin
  Palette := FindPalette;
  if Palette <> nil then
    Palette.TabIndex := FTabIndex;
end;

procedure TcaCompPalette.SetStyle(const Value: TTabStyle);
begin
  if Value <> FStyle then
    begin
      FStyle := Value;
      UpdateComponentPaletteStyle;
    end;
end;

procedure TcaCompPalette.SetTabIndex(const Value: Integer);
begin
  if Value <> FTabIndex then
    begin
      FTabIndex := Value;
      UpdateComponentPaletteTabIndex;
    end;
end;

end.
