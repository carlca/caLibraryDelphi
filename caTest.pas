unit caTest;

{$INCLUDE ca.inc}

interface

uses
  Windows, 
  SysUtils, 
  Messages, 
  Classes, 
  Graphics, 
  Controls, 
  Forms, 
  Dialogs, 
  Menus, 
  StdCtrls, 
  ExtCtrls, 
  CDK_Comp, 
  caTypes,
  caButtons,
  caEdit;

type
  TcaTest = class(TCompoundComponentPanel)
    caSpeedButton1: TcaSpeedButton;
    caEdit1: TcaEdit;
  private
    procedure ReadSpecialProperty(Reader: TReader);
    procedure WriteSpecialProperty(Writer: TWriter);
  protected
    FShouldSaveSpecialProperty: Boolean;		{ Allows descendants to override storage. }
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); Override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

implementation

procedure TcaTest.ReadSpecialProperty(Reader: TReader);
var
  i: Integer;
begin
  with Reader Do
  begin
    ReadListBegin;
    { CDK: Iterate through elements of SpecialProperty array, reading one element at a time,
      or otherwise implement your special property reader. }

    { Example:
    
      For i := 0 to ColumnCount - 1 Do
    		ColumnWidths[i] := ReadInteger;

      Other available "Read" functions defined in TReader:

        ReadBoolean
        ReadChar
        ReadComponent
        ReadFloat
        ReadString
    }
    ReadListEnd;
  end;	{ with }
end;		{ ReadSpecialProperty }

procedure TcaTest.WriteSpecialProperty(Writer: TWriter);
var
  i: Integer;
begin
  with Writer Do
  begin
    WriteListBegin;
    { CDK: Iterate through elements of SpecialProperty array, writing one element at a time,
      or otherwise implement your special property writer. }

    { Example: 
    
      For i := 0 to ColumnCount - 1 Do
    		WriteInteger(ColumnWidths[i]);

      Other available "Write" functions defined in TWriter:

        WriteBoolean
        WriteChar
        WriteComponent
        WriteFloat
        WriteString
    }
    WriteListEnd;
  end;	{ with }
end;		{ WriteSpecialProperty }

procedure TcaTest.DefineProperties(Filer: TFiler);
begin
  Inherited DefineProperties(Filer);
  if FShouldSaveSpecialProperty then
    with Filer do
    begin
      { CDK: Repeat the following DefineProperty call for each property you 
        have that requires special storage. }
  	  DefineProperty('SpecialProperty', ReadSpecialProperty, WriteSpecialProperty, True {Has Data} );
    end;  { with }
end;		{ DefineProperties }

destructor TcaTest.Destroy;
begin
  inherited Destroy;
end;

constructor TcaTest.Create(AOwner: TComponent); 
begin
  inherited Create(AOwner);
  Width := 197;
  Height := 57;

  caSpeedButton1 := TcaSpeedButton.Create(Self);

  caEdit1 := TcaEdit.Create(Self);
  caEdit1.Parent := Self;
  FShouldSaveSpecialProperty := True;
end;

procedure TcaTest.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);

  with caSpeedButton1 do
    begin
      Width := 25;
      Height := 25;
      AllowAllUp := False;
      AutoSizeMargin := 5;
      Color := clBtnFace;
      Color3DLight := cl3DLight;
      ColorBtnHighlight := clBtnHighlight;
      ColorBtnShadow := clBtnShadow;
      ColorWindowFrame := clWindowFrame;
      DisableDown := False;
      Down := False;
      DownFont.Charset := DEFAULT_CHARSET;
      DownFont.Color := clWindowText;
      DownFont.Height := -11;
      DownFont.Name := 'MS Sans Serif';
      DownFont.Style := [];
      GlyphOffsetWhenDown := True;
      GroupIndex := 0;
      Layout := laGlyphLeftCentered;
      MouseOverFont.Charset := DEFAULT_CHARSET;
      MouseOverFont.Color := clWindowText;
      MouseOverFont.Height := -11;
      MouseOverFont.Name := 'MS Sans Serif';
      MouseOverFont.Style := [];
      MouseOverStyle := bsThin;
      Spacing := 0;
      Style := bsDefault;
      SyncDownFont := True;
      SyncMouseOverFont := True;
      TextOffsetWhenDown := True;
      TextStyle := tsNormal;
      XOffset := 0;
      YOffset := 0;
    end;

  with caEdit1 do
    begin
      Left := 20;
      Top := 16;
      Width := 121;
      Height := 21;
      TabOrder := 0;
      Text := 'caEdit1';
    end;
end;

end.
