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


unit caMimeStream;

interface

{$INCLUDE ca.inc}

uses
  Classes;

procedure MimeEncodeFile(const InputFileName, OutputFileName: AnsiString);
procedure MimeEncodeFileNoCRLF(const InputFileName, OutputFileName: AnsiString);
procedure MimeDecodeFile(const InputFileName, OutputFileName: AnsiString);
procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
procedure MimeEncodeStreamNoCRLF(const InputStream: TStream; const OutputStream: TStream);
procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);

implementation

uses
  SysUtils,
  caMime;

const
  MIME_BUFFER_SIZE = MIME_DECODED_LINE_BREAK * 3 * 4 * 4;


  //```````````````````````````````````````````````````````````````````````````
  // File Encoding & Decoding                                                  
  //```````````````````````````````````````````````````````````````````````````

procedure MimeEncodeFile(const InputFileName, OutputFileName: AnsiString);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      MimeEncodeStream(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

procedure MimeEncodeFileNoCRLF(const InputFileName, OutputFileName: AnsiString);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      MimeEncodeStreamNoCRLF(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

procedure MimeDecodeFile(const InputFileName, OutputFileName: AnsiString);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      MimeDecodeStream(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

  //```````````````````````````````````````````````````````````````````````````
  // Stream Encoding & Decoding                                                
  //```````````````````````````````````````````````````````````````````````````

procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer: array[0..MIME_BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array[0..(MIME_BUFFER_SIZE + 2) div 3 * 4 + MIME_BUFFER_SIZE div MIME_DECODED_LINE_BREAK * 2 - 1] of Byte;
  BytesRead: Cardinal;
  IDelta, ODelta: Cardinal;
begin
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));

  while BytesRead = SizeOf(InputBuffer) do
    begin
      MimeEncodeFullLines(InputBuffer, SizeOf(InputBuffer), OutputBuffer);
      OutputStream.Write(OutputBuffer, SizeOf(OutputBuffer));
      BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
    end;

  MimeEncodeFullLines(InputBuffer, BytesRead, OutputBuffer);

  IDelta := BytesRead div MIME_DECODED_LINE_BREAK; // Number of lines processed.
  ODelta := IDelta * (MIME_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * MIME_DECODED_LINE_BREAK;
  MimeEncodeNoCRLF(Pointer(Cardinal(@InputBuffer) + IDelta)^, BytesRead - IDelta, Pointer(Cardinal(@OutputBuffer) + ODelta)^);

  OutputStream.Write(OutputBuffer, MimeEncodedSize(BytesRead));
end;

procedure MimeEncodeStreamNoCRLF(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer: array[0..MIME_BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array[0..((MIME_BUFFER_SIZE + 2) div 3) * 4 - 1] of Byte;
  BytesRead: Cardinal;
begin
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  while BytesRead = SizeOf(InputBuffer) do
    begin
      MimeEncodeNoCRLF(InputBuffer, SizeOf(InputBuffer), OutputBuffer);
      OutputStream.Write(OutputBuffer, SizeOf(OutputBuffer));
      BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
    end;

  MimeEncodeNoCRLF(InputBuffer, BytesRead, OutputBuffer);
  OutputStream.Write(OutputBuffer, MimeEncodedSizeNoCRLF(BytesRead));
end;

procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  InputBuffer: array[0..MIME_BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array[0..(MIME_BUFFER_SIZE + 3) div 4 * 3 - 1] of Byte;
  BytesRead: Cardinal;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  while BytesRead > 0 do
    begin
      OutputStream.Write(OutputBuffer, MimeDecodePartial(InputBuffer, BytesRead, OutputBuffer, ByteBuffer, ByteBufferSpace));
      BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
    end;
  OutputStream.Write(OutputBuffer, MimeDecodePartialEnd(OutputBuffer, ByteBuffer, ByteBufferSpace));
end;

end.

