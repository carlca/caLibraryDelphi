unit caCrypt;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Classes,
  SysUtils;

type

 //---------------------------------------------------------------------------
 // IcaCrypt
 //---------------------------------------------------------------------------

  IcaCrypt = interface
  ['{01BB5DD9-AD0F-4451-A98F-0A23DF500172}']
    // Public methods
    function Encrypt(const S: String; const StartKey: Word): String;
    function Decrypt(const S: String; const StartKey: Word): String;
  end;

 //---------------------------------------------------------------------------
 // TcaCrypt
 //---------------------------------------------------------------------------

  TcaCrypt = class(TInterfacedObject, IcaCrypt)
  private
    FKey1: Word;
    FKey2: Word;
  public
    constructor Create(const AKey1, AKey2: Word);
    // Public methods
    function Encrypt(const S: String; const StartKey: Word): String;
    function Decrypt(const S: String; const StartKey: Word): String;
  end;

implementation

 //---------------------------------------------------------------------------
 // TcaCrypt
 //---------------------------------------------------------------------------

constructor TcaCrypt.Create(const AKey1, AKey2: Word);
begin
  inherited Create;
  FKey1 := AKey1;
  FKey2 := AKey2;
end;

function TcaCrypt.Decrypt(const S: String; const StartKey: Word): String;
var
  B: Byte;
  Index: Integer;
  Key: Word;
begin
  B := 0;
  Key := StartKey;
  Result := '';
  for Index := 1 to Length(S) div 2 do
    begin
      try
        B :=  StrToInt('$' + Copy(S, 2 * Index - 1, 2));
      except
        on EConvertError do B := 0
      end;
      Result := Result + Char(B xor (Key shr 8));
      Key := (B + Key) * FKey1 + FKey2;
    end
end;

function TcaCrypt.Encrypt(const S: String; const StartKey: Word): String;
var
  B: Byte;
  Index: Integer;
  Key: Word;
begin
  Key := StartKey;
  Result := '';
  for Index := 1 to Length(S) do
    begin
      B := Byte(S[Index]) xor (Key shr 8);
      Key := (B + Key) * FKey1 + FKey2;
      Result := Result + IntToHex(B, 2)
    end
end;

end.
