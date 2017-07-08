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


unit caHTMLParser;

{$INCLUDE ca.inc}

interface

uses

  // Standard Delphi units
  Windows,
  SysUtils,
  Classes,

  // ca units
  caClasses,
  caUtils;

type

  TcaOnHTMLTagEvent = procedure(Sender: TObject; const ATag: string; AParams: Tstrings) of Object;

  TcaOnHTMLTextEvent = procedure(Sender: TObject; const AText: string) of Object;

 //---------------------------------------------------------------------------
 // IcaHTMLParser
 //---------------------------------------------------------------------------

  IcaHTMLParser = interface
  ['{0F50D1DA-E006-4DD0-8AF0-6DDF76CA7669}']
    // Property methods
    function GetOnTag: TcaOnHTMLTagEvent;
    function GetOnText: TcaOnHTMLTextEvent;
    procedure SetOnTag(const Value: TcaOnHTMLTagEvent);
    procedure SetOnText(const Value: TcaOnHTMLTextEvent);
    // Interface methods
    procedure Parse(AHTML: TStream);
    // Event properties
    property OnTag: TcaOnHTMLTagEvent read GetOnTag write SetOnTag;
    property OnText: TcaOnHTMLTextEvent read GetOnText write SetOnText;
  end;

 //---------------------------------------------------------------------------
 // TcaHTMLParser
 //---------------------------------------------------------------------------

  TcaHTMLParser = class(TcaInterfacedPersistent, IcaHTMLParser)
  private
    // Private fields
    FOnTag: TcaOnHTMLTagEvent;
    FOnText: TcaOnHTMLTextEvent;
    // Private methods
    function GetTagName(const AText: string): string;
    function RemoveCRLFChars(const Ch: Char): string;
    function RemoveNonBreakingSpaces(const AText: string): string;
    function RemoveQuotes(const AText: string): string;
    procedure ParseTagParams(const AText: string; AParams: Tstrings);
    procedure ProcessText(const AText: string);
    procedure ToLower(var AText: string);
  protected
    // Property methods
    function GetOnTag: TcaOnHTMLTagEvent;
    function GetOnText: TcaOnHTMLTextEvent;
    procedure SetOnTag(const Value: TcaOnHTMLTagEvent);
    procedure SetOnText(const Value: TcaOnHTMLTextEvent);
    // Event triggers
    procedure DoTag(const ATag: string; AParams: Tstrings); virtual;
    procedure DoText(const AText: string); virtual;
  public
    // Interface methods
    procedure Parse(AHTML: TStream);
  published
    // Event properties
    property OnTag: TcaOnHTMLTagEvent read GetOnTag write SetOnTag;
    property OnText: TcaOnHTMLTextEvent read GetOnText write SetOnText;
  end;

implementation

 // Interface methods

procedure TcaHTMLParser.Parse(AHTML: TStream);
var
  Ch: Char;
  ParsedStr: string;
  TagName: string;
  Params: Tstrings;
  OnStr: Boolean;
begin
  if AHTML.Size > 0 then
    begin
      AHTML.Seek(0, soFromBeginning);
      ParsedStr := '';
      Ch := #0;
      OnStr := False;
      repeat
        AHTML.Read(Ch, 1);
        if Ch= '"' then OnStr := not OnStr;
        if (Ch = '<') and (not OnStr) then
          begin
            if Length(ParsedStr) > 0 then
              ProcessText(ParsedStr);
            ParsedStr := '';
          end;
        ParsedStr := ParsedStr + RemoveCRLFChars(Ch);
        if (Ch = '>') and (not OnStr) then
          begin
            if Length(ParsedStr) > 0 then
              begin
                TagName := GetTagName(ParsedStr);
                if Copy(TagName, 1, 1) <> '!' then
                  begin
                    Params := TstringList.Create;
                    try
                      ParseTagParams(ParsedStr, Params);
                      DoTag(GetTagName(ParsedStr), Params);
                    finally
                      Params.Free;
                    end;
                  end
                else
                  ProcessText(ParsedStr);
              end;
            ParsedStr := '';
          end;
      until AHTML.Size <= AHTML.Position;
    end;
end;

 // Event triggers

procedure TcaHTMLParser.DoTag(const ATag: string; AParams: Tstrings);
begin
  if Assigned(FOnTag) then FOnTag(Self, ATag, AParams);
end;

procedure TcaHTMLParser.DoText(const AText: string);
begin
  if Assigned(FOnText) then FOnText(Self, AText);
end;

 // Private methods

function TcaHTMLParser.GetTagName(const AText: string): string;
var
  EndPos: Integer;
begin
  EndPos := Pos(#32, AText);
  if EndPos = 0 then
    EndPos := Pos('>', AText);
  Result := Copy(AText, 2, EndPos - 2);
end;

function TcaHTMLParser.RemoveCRLFChars(const Ch: Char): string;
const
  CRLFChars: set of Char = [#13,#10];
begin
  if (Ch in CRLFChars) then
    Result := ''
  else
    Result := Ch;
end;

function TcaHTMLParser.RemoveNonBreakingSpaces(const AText: string): string;
begin
  Result := AText;
  Utils.Replace(Result, '&nbsp;', '');
end;

function TcaHTMLParser.RemoveQuotes(const AText: string): string;
begin
  Result := AText;
  Utils.StripChar('"', Result);
  Utils.StripChar('''', Result);
end;

procedure TcaHTMLParser.ParseTagParams(const AText: string; AParams: Tstrings);
const
  StartStop: set of Char = [' ', '<', '>'];
  Equals: set of Char = ['='];
var
  Ch: Char;
  Index: Integer;
  InQuote: Boolean;
  TagParam: string;
begin
  TagParam := '';
  AParams.Clear;
  InQuote := False;
  for Index := 1 to Length(AText) do
    begin
      Ch := AText[Index];
      if Ch = '"' then InQuote := not InQuote;
      if (Ch in StartStop) and (not InQuote) then
        begin
          if Length(TagParam) > 0 then
            begin
              ToLower(TagParam);
              AParams.Add(RemoveQuotes(TagParam));
            end;
          TagParam := '';
        end
      else
        TagParam := TagParam + Ch;
    end;
end;

procedure TcaHTMLParser.ProcessText(const AText: string);
var
  OutText: string;
begin
  OutText := RemoveNonBreakingSpaces(AText);
  if OutText <> '' then
    DoText(OutText);
end;

procedure TcaHTMLParser.ToLower(var AText: string);
var
  Index: Integer;
begin
  for Index := 1 to Length(AText) do
    begin
      if AText[Index] = '=' then Break;
      AText[Index] := AnsiLowerCase(AText[Index])[1];
    end;
end;

 // Property methods

function TcaHTMLParser.GetOnTag: TcaOnHTMLTagEvent;
begin
  Result := FOnTag;
end;

function TcaHTMLParser.GetOnText: TcaOnHTMLTextEvent;
begin
  Result := FOnText;
end;

procedure TcaHTMLParser.SetOnTag(const Value: TcaOnHTMLTagEvent);
begin
  FOnTag := Value;
end;

procedure TcaHTMLParser.SetOnText(const Value: TcaOnHTMLTextEvent);
begin
  FOnText := Value;
end;

end.
