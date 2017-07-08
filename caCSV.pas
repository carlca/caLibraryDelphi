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


unit caCSV;

interface

uses

  // standard delphi units 
  Classes,
  Sysutils,

  // ca units 
  caLog,
  caTextStream;

type

  //---------------------------------------------------------------------------
  // TcaCSVStream                                                              
  //---------------------------------------------------------------------------

  TcaCSVStreamReadLineEvent = procedure(Sender: TObject; const AFileName: string; AIndex: Integer;
                                        AColumnNames, AValues: TStrings; var AContinue: Boolean) of object;

  TcaCSVStreamProgressEvent = procedure(Sender: TObject; AProgressPct: Integer) of object;

  TcaCSVStream = class(TcaTextStream)
  private
    // private fields 
    FErrorLog: TStrings;
    FFirstLineIsColumnNames: Boolean;
    FValues: TStrings;
    // property fields 
    FColumnNames: TStrings;
    // event property fields 
    FOnProgress: TcaCSVStreamProgressEvent;
    FOnReadLine: TcaCSVStreamReadLineEvent;
    // private methods 
    procedure AssignCommaText(AStrings: TStrings; const ACommaText: string);
  protected
    // protected methods 
    procedure DoProgress(AProgressPct: Integer); virtual;
    procedure DoReadLine(AIndex: Integer; var AContinue: Boolean); virtual;
  public
    // create/destroy 
    procedure AfterConstruction; override;
    destructor Destroy; override;
    // public methods 
    function HasErrors: Boolean;
    procedure Load;
    // public properties 
    property ErrorLog: TStrings read FErrorLog;
    property FirstLineIsColumnNames: Boolean read FFirstLineIsColumnNames write FFirstLineIsColumnNames;
    // public event properties 
    property OnProgress: TcaCSVStreamProgressEvent read FOnProgress write FOnProgress;
    property OnReadLine: TcaCSVStreamReadLineEvent read FOnReadLine write FOnReadLine;
  end;

implementation

const
  cBufferSize: Integer = 8192;

  //---------------------------------------------------------------------------
  // TcaCSVStream                                                              
  //---------------------------------------------------------------------------

  // create/destroy 

procedure TcaCSVStream.AfterConstruction;
begin
  inherited;
  FColumnNames := TStringList.Create;
  FErrorLog := TStringList.Create;
  FFirstLineIsColumnNames := True;
  FValues := TStringList.Create;
end;

destructor TcaCSVStream.Destroy;
begin
  FColumnNames.Free;
  FErrorLog.Free;
  FValues.Free;
  inherited;
end;

  // public methods 

function TcaCSVStream.HasErrors: Boolean;
begin
  Result := FErrorLog.Count > 0;
end;

procedure TcaCSVStream.Load;
var
  ColCount: Integer;
  Continue: Boolean;
  Index: Integer;
  Line: string;
  ValCount: Integer;
  TotalSize: Int64;
  BytesRead: Int64;
  ProgressPct: Integer;
  LastProgressPct: Integer;
begin
  Index := 0;
  Seek(0, soFromBeginning);
  FErrorLog.Clear;
  TotalSize := Size;
  BytesRead := 0;
  LastProgressPct := 0;
  while not IsEOF do
    begin
      Line := GetLine;
      BytesRead := BytesRead + Length(Line) + 2;
      ProgressPct := Round(BytesRead / TotalSize * 100);
      if ProgressPct <> LastProgressPct then
        begin
          DoProgress(ProgressPct);
          LastProgressPct := ProgressPct;
        end;
      if FFirstLineIsColumnNames and (Index = 0) then
        AssignCommaText(FColumnNames, Line)
      else
        begin
          AssignCommaText(FValues, Line);
          Continue := True;
          DoReadLine(Index, Continue);
          if not Continue then Break;
          if FColumnNames.Count <> FValues.Count then
            begin
              ColCount := FColumnNames.Count;
              ValCount := FValues.Count;
              FErrorLog.Add(Sysutils.Format('line: %d, columns: %d, values: %d', [Index, ColCount, ValCount]));
            end;
        end;
      Inc(Index);
    end;
end;

  // protected methods 

procedure TcaCSVStream.DoProgress(AProgressPct: Integer);
begin
  if Assigned(FOnProgress) then FOnProgress(Self, AProgressPct);
end;

procedure TcaCSVStream.DoReadLine(AIndex: Integer; var AContinue: Boolean);
begin
  if Assigned(FOnReadLine) then FOnReadLine(Self, Filename, AIndex, FColumnNames, FValues, AContinue);
end;

  // private methods 

procedure TcaCSVStream.AssignCommaText(AStrings: TStrings; const ACommaText: string);
var
  P: PChar;
  P1: PChar;
  S: string;
begin
  AStrings.BeginUpdate;
  try
    AStrings.Clear;
    P := PChar(ACommaText);
    while P^ <> #0 do
      begin
        if P^ = '"' then
          S := AnsiExtractQuotedStr(P, '"')
        else
          begin
            P1 := P;
            while (P^ <> ',') and (P^ <> #0) do Inc(P);
            SetString(S, P1, P - P1);
          end;
        AStrings.Add(S);
        if P^ = ',' then Inc(P);
        while P^ = #32 do Inc(P);
      end;
  finally
    AStrings.EndUpdate;
  end;    
end;

end.
