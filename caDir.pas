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


unit caDir;

{$INCLUDE CA.INC}

interface

uses

  // Standard Delphi units 
  Windows,
  Classes,
  SysUtils,
  Contnrs,
  Masks,

  // ca units 
  caClasses,
  caUtils,
  caConsts,
  caNodes;

type

  TcaDirNode = class;

  TcaDirAddNodeEvent = procedure(Sender: TObject; ANode: TcaDirNode; var AAccept: Boolean) of object;

  TcaDirScanFileEvent = procedure(Sender: TObject; const AFileName: string; ASearchRec: TSearchRec) of object;

  //----------------------------------------------------------------------------
  // TcaDirAttributes                                                           
  //----------------------------------------------------------------------------

  TcaDirAttributes = class(TObject)
  private
    // Property fields 
    FArchive: Boolean;
    FHidden: Boolean;
    FIsFolder: Boolean;
    FReadOnly: Boolean;
    FSystemFile: Boolean;
  public
    // Properties 
    property Archive: Boolean read FArchive write FArchive;
    property Hidden: Boolean read FHidden write FHidden;
    property IsFolder: Boolean read FIsFolder write FIsFolder;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property SystemFile: Boolean read FSystemFile write FSystemFile;
  end;

  //----------------------------------------------------------------------------
  // TcaDirNode                                                                 
  //----------------------------------------------------------------------------

  TcaDirNode = class(TcaNode)
  private
    // Property fields 
    FAttributes: TcaDirAttributes;
    FChildFileCount: Integer;
    FContents: string;
    FCreationTime: TDateTime;
    FFilename: string;
    FFileSize: Int64;
    FFolder: string;
    FImageIndex: Integer;
    FLastAccessTime: TDateTime;
    FLastWriteTime: TDateTime;
    // Property methods 
    function GetFullFilename: string;
  public
    // Create/Destroy 
    constructor Create; override;
    destructor Destroy; override;
    // Public methods 
    procedure IncrementChildFileCount;
    procedure ResetChildFileCount;
    // Properties 
    property Attributes: TcaDirAttributes read FAttributes;
    property ChildFileCount: Integer read FChildFileCount;
    property Contents: string read FContents write FContents;
    property CreationTime: TDateTime read FCreationTime write FCreationTime;
    property Filename: string read FFilename write FFilename;
    property FileSize: Int64 read FFileSize write FFileSize;
    property Folder: string read FFolder write FFolder;
    property FullFilename: string read GetFullFilename;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property LastAccessTime: TDateTime read FLastAccessTime write FLastAccessTime;
    property LastWriteTime: TDateTime read FLastWriteTime write FLastWriteTime;
  end;

  //---------------------------------------------------------------------------
  // TcaDirNodes                                                               
  //---------------------------------------------------------------------------

  TcaDirNodes = class(TcaNodes)
  protected
    // Protected methods 
    function CreateNode: TcaNode; override;
  end;

  //---------------------------------------------------------------------------
  // IcaDir                                                                    
  //---------------------------------------------------------------------------

  IcaDir = interface
  ['{EF3FB6ED-F960-4CF8-A970-5CC66A03BC1B}']
    // Property methods 
    function GetCount: Integer;
    function GetExclude: string;
    function GetFileSpec: string;
    function GetFolder: string;
    function GetItem(Index: Integer): TcaDirNode;
    function GetNodes: TcaDirNodes;
    procedure SetExclude(const Value: string);
    procedure SetFileSpec(const Value: string);
    procedure SetFolder(const Value: string);
    // Event property methods 
    function GetOnAddNode: TcaDirAddNodeEvent;
    function GetOnScanFile: TcaDirScanFileEvent;
    procedure SetOnAddNode(const Value: TcaDirAddNodeEvent);
    procedure SetOnScanFile(const Value: TcaDirScanFileEvent);
    // Interface methods 
    procedure Scan(ADeleteEmptyFolders: Boolean = False);
    procedure SortByFilename;
    procedure SortByFolder;
    procedure SortByFolderAndFileName;
    procedure SortByLastWriteTime;
    // Properties 
    property Count: Integer read GetCount;
    property Exclude: string read GetExclude write SetExclude;
    property FileSpec: string read GetFileSpec write SetFileSpec;
    property Folder: string read GetFolder write SetFolder;
    property Items[Index: Integer]: TcaDirNode read GetItem; default;
    property Nodes: TcaDirNodes read GetNodes;
    // Event properties 
    property OnAddNode: TcaDirAddNodeEvent read GetOnAddNode write SetOnAddNode;
    property OnScanFile: TcaDirScanFileEvent read GetOnScanFile write SetOnScanFile;    
  end;

  //----------------------------------------------------------------------------
  // TcaDir                                                                     
  //----------------------------------------------------------------------------

  TcaDir = class(TInterfacedObject, IcaDir)
  private
    // Private fields 
    FFileSpec: string;
    FFileList: TObjectList;
    FExclude: string;
    FExcludeList: TStrings;
    FFolder: string;
    FNodes: TcaDirNodes;
    FParser: IcaParser;
    // Property fields 
    FOnAddNode: TcaDirAddNodeEvent;
    FOnScanFile: TcaDirScanFileEvent;
    // Property methods 
    function GetCount: Integer;
    function GetExclude: string;
    function GetFileSpec: string;
    function GetFolder: string;
    function GetItem(Index: Integer): TcaDirNode;
    function GetNodes: TcaDirNodes;
    procedure SetExclude(const Value: string);
    procedure SetFileSpec(const Value: string);
    procedure SetFolder(const Value: string);
    // Event property methods 
    function GetOnAddNode: TcaDirAddNodeEvent;
    function GetOnScanFile: TcaDirScanFileEvent;
    procedure SetOnAddNode(const Value: TcaDirAddNodeEvent);
    procedure SetOnScanFile(const Value: TcaDirScanFileEvent);
    // Private methods 
    procedure DeleteEmptyFolders;
    procedure RecursedScan(AParentNode: TcaNode; const AFolder, AFileSpec, AExclude: string);
    procedure UpdateFileList;
  protected
    // Protected methods 
    function CreateDirNodes: TcaDirNodes; virtual;
    procedure DoAddNode(ANode: TcaDirNode; var AAccept: Boolean); virtual;
    procedure DoScanFile(const AFileName: string; ASearchRec: TSearchRec); virtual;
  public
    // Create/Destroy 
    constructor Create; overload;
    constructor Create(const AFolder, AFileSpec, AExclude: string); overload;
    destructor Destroy; override;
    // Interface methods - IcaDir 
    procedure Scan(ADeleteEmptyFolders: Boolean = False);
    procedure SortByFilename;
    procedure SortByFolder;
    procedure SortByFolderAndFileName;
    procedure SortByLastWriteTime;
    // Properties 
    property Count: Integer read GetCount;
    property Exclude: string read GetExclude write SetExclude;
    property FileSpec: string read GetFileSpec write SetFileSpec;
    property Folder: string read GetFolder write SetFolder;
    property Items[Index: Integer]: TcaDirNode read GetItem; default;
    property Nodes: TcaDirNodes read GetNodes;
    // Event properties 
    property OnAddNode: TcaDirAddNodeEvent read GetOnAddNode write SetOnAddNode;
    property OnScanFile: TcaDirScanFileEvent read GetOnScanFile write SetOnScanFile;
  end;

implementation

{$IFDEF D7_UP}
uses
  DateUtils;

  {$ENDIF}

  //----------------------------------------------------------------------------
  // TcaDirNode                                                                 
  //----------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaDirNode.Create;
begin
  inherited;
  FAttributes := TcaDirAttributes.Create;
end;

destructor TcaDirNode.Destroy;
begin
  FAttributes.Free;
  inherited;
end;

  // Public methods 

procedure TcaDirNode.IncrementChildFileCount;
begin
  Inc(FChildFileCount);
end;

procedure TcaDirNode.ResetChildFileCount;
begin
  FChildFileCount := 0;
end;

  // Property methods 

function TcaDirNode.GetFullFilename: string;
begin
  Result := FFolder;
  Utils.EnsureLastChar(Result, '\');
  Result := Result + FFilename;
end;

  //---------------------------------------------------------------------------
  // TcaDirNodes                                                               
  //---------------------------------------------------------------------------

  // Protected methods 

function TcaDirNodes.CreateNode: TcaNode;
begin
  Result := TcaDirNode.Create;
end;

  //---------------------------------------------------------------------------
  // TcaDir                                                                    
  //---------------------------------------------------------------------------

  // Create/Destroy 

constructor TcaDir.Create;
begin
  inherited;
  FFileList := TObjectList.Create(False);
  FExcludeList := TStringList.Create;
  FNodes := CreateDirNodes;
end;

constructor TcaDir.Create(const AFolder, AFileSpec, AExclude: string);
begin
  FFolder := AFolder;
  FFileSpec := AFileSpec;
  FExclude := AExclude;
  Create;
end;

destructor TcaDir.Destroy;
begin
  FFileList.Free;
  FExcludeList.Free;
  FNodes.Free;
  inherited;
end;

  // Interface methods - IcaDir 

procedure TcaDir.Scan(ADeleteEmptyFolders: Boolean = False);
begin
  FNodes.Clear;
  FParser := Utils as IcaParser;
  FParser.Initialize;
  FParser.TokenDelimiters := ' ';
  FParser.StringToParse := FExclude;
  FParser.GetTokens(FExcludeList);
  RecursedScan(nil, FFolder, FFileSpec, FExclude);
  UpdateFileList;
  if ADeleteEmptyFolders then DeleteEmptyFolders;
end;

{$IFDEF D5}

function CompareDateTime(const A, B: TDateTime): Integer;
begin
  if Abs(A - B) < (1 / MSecsPerDay) then
    Result := 0
  else if A < B then
    Result := -1
  else
    Result := 1;
end;

{$ENDIF}

function CompareLastWriteTimes(Item1, Item2: Pointer): Integer;
begin
  Result := -CompareDateTime(TcaDirNode(Item1).LastWriteTime, TcaDirNode(Item2).LastWriteTime);
end;

function CompareFilenames(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TcaDirNode(Item1).Filename, TcaDirNode(Item2).Filename);
end;

function CompareFolders(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TcaDirNode(Item1).Folder, TcaDirNode(Item2).Folder);
end;

function CompareFoldersAndFileNames(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TcaDirNode(Item1).Folder + ' ' + TcaDirNode(Item1).Filename,
                        TcaDirNode(Item2).Folder + ' ' + TcaDirNode(Item2).Filename);
end;

procedure TcaDir.SortByLastWriteTime;
begin
  FFileList.Sort(@CompareLastWriteTimes);
end;

procedure TcaDir.SortByFilename;
begin
  FFileList.Sort(@CompareFilenames);
end;

procedure TcaDir.SortByFolder;
begin
  FFileList.Sort(@CompareFolders);
end;

procedure TcaDir.SortByFolderAndFileName;
begin
  FFileList.Sort(@CompareFoldersAndFileNames);
end;

// Protected methods 

function TcaDir.CreateDirNodes: TcaDirNodes;
begin
  Result := TcaDirNodes.Create;
end;

procedure TcaDir.DoAddNode(ANode: TcaDirNode; var AAccept: Boolean);
begin
  if Assigned(FOnAddNode) then FOnAddNode(Self, ANode, AAccept);
end;

procedure TcaDir.DoScanFile(const AFileName: string; ASearchRec: TSearchRec);
begin
  if Assigned(FOnScanFile) then FOnScanFile(Self, AFileName, ASearchRec);    
end;

  // Private methods 

procedure TcaDir.DeleteEmptyFolders;

  procedure ClearChildFileCountForAllNodes;
  var
    Node: TcaDirNode;
  begin
    Node := TcaDirNode(FNodes.Roots[0]);
    while Assigned(Node) do
      begin
        Node.ResetChildFileCount;
        Node := TcaDirNode(Node.Next);
      end;
  end;

  procedure UpdateChildFileCountForAllFolders;
  var
    Node: TcaDirNode;
    Parent: TcaDirNode;
  begin
    Node := TcaDirNode(FNodes.Roots[0]);
    while Assigned(Node) do
      begin
        if not Node.Attributes.IsFolder then
          begin
            // Process file nodes, not folders 
            Parent := TcaDirNode(Node.Parent);
            while Assigned(Parent) do
              begin
                Parent.IncrementChildFileCount;
                Parent := TcaDirNode(Parent.Parent);
              end;
          end;
        Node := TcaDirNode(Node.Next);
      end;
  end;

  procedure DeleteEmptyFolders;
  var
    Node: TcaDirNode;
    NothingDeleted: Boolean;
  begin
    repeat
      NothingDeleted := True;
      Node := TcaDirNode(FNodes.Roots[0]);
      while Assigned(Node) do
        begin
          if Node.Attributes.IsFolder and (Node.ChildFileCount = 0) then
            begin
              FNodes.Delete(Node);
              NothingDeleted := False;
              Break;
            end;
          Node := TcaDirNode(Node.Next);
        end;
    until NothingDeleted;
  end;

begin
  ClearChildFileCountForAllNodes;
  UpdateChildFileCountForAllFolders;
  DeleteEmptyFolders;
end;

procedure TcaDir.RecursedScan(AParentNode: TcaNode; const AFolder, AFileSpec, AExclude: string);
var
  Accept: Boolean;
  Filename: string;
  FileSize: Int64;
  FindStatus: Integer;
  Folder: string;
  FoundFirst: Boolean;
  Index: Integer;
  Masks: TObjectList;
  Node: TcaDirNode;
  SearchRec: TSearchRec;
  SubFolder: string;

  procedure CreateMasks(const AFileSpec: string; AMasks: TObjectList);
  var
    Mask: TMask;
    S: string;
    Parser: IcaParser;
  begin
    S := AFileSpec;
    Utils.Replace(S, cSemiColon, #32);
    Parser := Utils as IcaParser;
    Parser.Initialize;
    Parser.TokenDelimiters := #32;
    Parser.StringToParse := S;
    while Parser.HasMoreTokens do
      begin
        Mask := TMask.Create(Parser.NextToken);
        AMasks.Add(Mask);
      end;
  end;

  function MatchesAMask(const AFileName: string; AMasks: TObjectList): Boolean;
  var
    Index: Integer;
    Mask: TMask;
  begin
    Result := False;
    for Index := 0 to Pred(AMasks.Count) do
      begin
        Mask := TMask(AMasks[Index]);
        if Mask.Matches(AFileName) then
          begin
            Result := True;
            Break;
          end;
      end;
  end;

  function IsDotFile: Boolean;
  begin
    Result := (SearchRec.Name = '.') or (SearchRec.Name = '..');
  end;

  function IsDirectory: Boolean;
  begin
    Result := (SearchRec.Attr and faDirectory > 0);
  end;

  function DateStampToDateTime(ADateStamp: TFileTime): TDateTime;
  var
    LocalFileTime: TFileTime;
    SystemTime: TSystemTime;
  begin
    FileTimeToLocalFileTime(ADateStamp, LocalFileTime);
    FileTimeToSystemTime(LocalFileTime, SystemTime);
    Result := SystemTimeToDateTime(SystemTime);
  end;

  procedure SetNodeAttributes(ANode: TcaDirNode; ASearchRec: TSearchRec; IsFolder: Boolean);
  begin
    ANode.Attributes.Archive := ASearchRec.Attr and faReadOnly <> 0;
    ANode.Attributes.Hidden := ASearchRec.Attr and faHidden <> 0;
    ANode.Attributes.IsFolder := IsFolder;
    ANode.Attributes.ReadOnly := ASearchRec.Attr and faReadOnly <> 0;
    ANode.Attributes.SystemFile := ASearchRec.Attr and faSysFile <> 0;
  end;

begin
  Masks := Auto(TObjectList.Create(True)).Instance;
  CreateMasks(AFileSpec, Masks);
  Folder := AFolder;
  Utils.EnsureLastChar(Folder, '\');
  FindStatus := FindFirst(Folder + '*.*', faAnyFile, SearchRec);
  FoundFirst := FindStatus = WN_SUCCESS;
  if FoundFirst then
    begin
      if not Assigned(AParentNode) then
        begin
          AParentNode := FNodes.AddRoot(AFolder);
          Node := TcaDirNode(AParentNode);
          Node.Folder := Folder;
          Node.Filename := '';
          SetNodeAttributes(Node, SearchRec, True);
          Node.CreationTime := DateStampToDateTime(SearchRec.FindData.ftCreationTime);
          Node.LastAccessTime := DateStampToDateTime(SearchRec.FindData.ftLastAccessTime);
          Node.LastWriteTime := DateStampToDateTime(SearchRec.FindData.ftLastWriteTime);
          Int64Rec(FileSize).Lo := SearchRec.FindData.nFileSizeLow;
          Int64Rec(FileSize).Hi := SearchRec.FindData.nFileSizeHigh;
          Node.FileSize := FileSize;
        end;
    end;
  while FindStatus = WN_SUCCESS do
    begin
      // Process non-directory entries 
      if (not IsDotFile) and (not IsDirectory) then
        begin
          Filename := SearchRec.FindData.cFileName;
          DoScanFile(Folder + Filename, SearchRec);
          // Check file spec 
          Accept := MatchesAMask(Folder + Filename, Masks);
          // Check excludes 
          for Index := 0 to Pred(FExcludeList.Count) do
            if Utils.FastPos(Filename, FExcludeList[Index]) > 0 then
              begin
                Accept := False;
                Break;
              end;
          // Add DirItem 
          if Accept then
            begin
              Node := TcaDirNode(FNodes.AddSub(AParentNode, Filename));
              Node.Folder := Folder;
              Node.Filename := Filename;
              SetNodeAttributes(Node, SearchRec, False);
              Node.CreationTime := DateStampToDateTime(SearchRec.FindData.ftCreationTime);
              Node.LastAccessTime := DateStampToDateTime(SearchRec.FindData.ftLastAccessTime);
              Node.LastWriteTime := DateStampToDateTime(SearchRec.FindData.ftLastWriteTime);
              Int64Rec(FileSize).Lo := SearchRec.FindData.nFileSizeLow;
              Int64Rec(FileSize).Hi := SearchRec.FindData.nFileSizeHigh;
              Node.FileSize := FileSize;
              // Allow event to cancel addtion of item 
                DoAddNode(Node, Accept);
              if not Accept then
                FNodes.Delete(Node);
            end;
        end;
      // Recurse if necessary 
      if IsDirectory and (not IsDotFile) then
        begin
          Filename := SearchRec.FindData.cFileName;
          Node := TcaDirNode(FNodes.AddSub(AParentNode, Filename));
          Node.CreationTime := DateStampToDateTime(SearchRec.FindData.ftCreationTime);
          Node.LastAccessTime := DateStampToDateTime(SearchRec.FindData.ftLastAccessTime);
          Node.LastWriteTime := DateStampToDateTime(SearchRec.FindData.ftLastWriteTime);
          SetNodeAttributes(Node, SearchRec, True);
          SubFolder := SearchRec.Name;
          Utils.EnsureLastChar(SubFolder, '\');
          Node.Folder := Folder + SubFolder;
          Accept := True;
          DoAddNode(Node, Accept);
          if Accept then
            RecursedScan(Node, Folder + SubFolder, AFileSpec, AExclude);
        end;
      FindStatus := FindNext(SearchRec);
    end;
  if FoundFirst then FindClose(SearchRec);
end;

procedure TcaDir.UpdateFileList;
var
  Node: TcaDirNode;
begin
  FFileList.Clear;
  Node := TcaDirNode(FNodes.Roots[0]);
  while Assigned(Node) do
    begin
      FFileList.Add(Node);
      Node := TcaDirNode(Node.Next);
    end;
end;

  // Property methods 

function TcaDir.GetCount: Integer;
begin
  Result := FFileList.Count;
end;

function TcaDir.GetExclude: string;
begin
  Result := FExclude;
end;

function TcaDir.GetFileSpec: string;
begin
  Result := FFileSpec;
end;

function TcaDir.GetFolder: string;
begin
  Result := FFolder;
end;

function TcaDir.GetItem(Index: Integer): TcaDirNode;
begin
  Result := TcaDirNode(FFileList[Index]);
end;

function TcaDir.GetNodes: TcaDirNodes;
begin
  Result := FNodes;
end;

procedure TcaDir.SetExclude(const Value: string);
begin
  FExclude := Value;
end;

procedure TcaDir.SetFileSpec(const Value: string);
begin
  FFileSpec := Value;
end;

procedure TcaDir.SetFolder(const Value: string);
begin
  FFolder := Value;
end;

  // Event property methods 

function TcaDir.GetOnAddNode: TcaDirAddNodeEvent;
begin
  Result := FOnAddNode;
end;

function TcaDir.GetOnScanFile: TcaDirScanFileEvent;
begin
  Result := FOnScanFile;
end;

procedure TcaDir.SetOnAddNode(const Value: TcaDirAddNodeEvent);
begin
  FOnAddNode := Value;
end;

procedure TcaDir.SetOnScanFile(const Value: TcaDirScanFileEvent);
begin
  FOnScanFile := Value;
end;

end.

