unit PlayerExtractors;

{$mode objfpc}{$H+}

interface

uses
  PlayerThreads, Classes, SysUtils, Fgl, Process;

type
  TPlayerFileInfo = packed record
    TrackFile: String;
    CreatedAt: TDateTime;
  end;

  TPlayerFileList = specialize TFPGMap<String, TPlayerFileInfo>;

  { TPlayerInfoExtractor }

  TPlayerInfoExtractor = class
  private
    FList: TPlayerFileList;
    FSessionID: String;
    FTempDir: String;
    function GetCount: Integer;
    function GetFileInfo(const Index: Integer): TPlayerFileInfo;
    function GetFileInfoByName(const AFileName: String): TPlayerFileInfo;
    function GetFileName(const Index: Integer): String;
    procedure PrepareTempDir(const ATempDir: String);
  public
    constructor Create(AFileList: TStringList; const ATempDir: String);
    destructor Destroy; override;

    function LoadData: TThread;

    property Count: Integer read GetCount;
    property FileName[Index: Integer]: String read GetFileName; default;
    property FileInfo[Index: Integer]: TPlayerFileInfo read GetFileInfo;
    property FileInfoByName[AFileName: String]: TPlayerFileInfo read GetFileInfoByName;

    property SessionID: String read FSessionID;
    property TempDir: String read FTempDir;
  end;

  { TPlayerExtractorManager }

  TPlayerExtractorManager = class(TPlayerThreadManager)
  private
    FExtractor: TPlayerInfoExtractor;
    FCount: Integer;
  protected
    function GetNextThread: TPlayerThread; override;
  public
    constructor Create(AExtractor: TPlayerInfoExtractor);

    property Extractor: TPlayerInfoExtractor read FExtractor;
  end;

  { TPlayerExtractorThread }

  TPlayerExtractorThread = class(TPlayerThread)
  private
    FIndex: Integer;
    function GetExtractor: TPlayerInfoExtractor;
    function GetManager: TPlayerExtractorManager;
  protected
    procedure Execute; override;
  public
    constructor Create(AManager: TPlayerExtractorManager; const AIndex: Integer);

    property Extractor: TPlayerInfoExtractor read GetExtractor;
    property Manager: TPlayerExtractorManager read GetManager;
  end;

implementation

{ TPlayerExtractorThread }

function TPlayerExtractorThread.GetManager: TPlayerExtractorManager;
begin
  Result:=inherited Manager as TPlayerExtractorManager;
end;

function TPlayerExtractorThread.GetExtractor: TPlayerInfoExtractor;
begin
  Result:=Manager.Extractor;
end;

procedure TPlayerExtractorThread.Execute;
var
  AProcess: TProcess;
begin
  AProcess:=TProcess.Create(nil);
  try
    with AProcess do
    begin
      // ffmpeg -f concat -i mylist.txt -map 0:s:0 -c copy -f data out.data
      Executable:='/usr/bin/ffmpeg';

      Parameters.Add('-i');
      Parameters.Add(Format('%s', [Extractor[FIndex]]));
      Parameters.Add('-map');
      Parameters.Add('0:s:0');
      Parameters.Add('-c');
      Parameters.Add('copy');
      Parameters.Add('-f');
      Parameters.Add('data');
      Parameters.Add('-v');
      Parameters.Add('quiet');
      Parameters.Add(Format('%ssubtitles_%d.data', [Extractor.TempDir, FIndex]));

      Options:=[poWaitOnExit, poUsePipes];
    end;

    AProcess.Execute;
  finally
    AProcess.Free;
  end;
end;

constructor TPlayerExtractorThread.Create(AManager: TPlayerExtractorManager;
  const AIndex: Integer);
begin
 inherited Create(AManager);
 FIndex:=AIndex;
end;

{ TPlayerExtractorManager }

function TPlayerExtractorManager.GetNextThread: TPlayerThread;
begin
  if not (FCount < Extractor.Count) then Result:=nil else
  begin
    Result:=TPlayerExtractorThread.Create(Self, FCount);
    Inc(FCount);
  end
end;

constructor TPlayerExtractorManager.Create(AExtractor: TPlayerInfoExtractor);
begin
  FExtractor:=AExtractor;
  FCount:=0;
  inherited Create;
end;

{ TPlayerInfoExtractor }

function TPlayerInfoExtractor.GetFileName(const Index: Integer): String;
begin
  Result:=FList.Keys[Index];
end;

procedure TPlayerInfoExtractor.PrepareTempDir(const ATempDir: String);
var
  Guid: TGUID;
begin
  if CreateGUID(Guid) <> 0 then raise Exception.Create('error');

  FSessionID:=LowerCase(GUIDToString(Guid));
  Delete(FSessionID, 1, 1);
  Delete(FSessionID, Length(FSessionID), 1);

  FTempDir:=IncludeTrailingPathDelimiter(ATempDir) + FSessionID;
  FTempDir:=IncludeTrailingPathDelimiter(FTempDir);
  ForceDirectories(FTempDir);
end;

constructor TPlayerInfoExtractor.Create(AFileList: TStringList;
  const ATempDir: String);
var
  FileItemName: String;
  FileItemData: TPlayerFileInfo;
begin
  inherited Create;
  PrepareTempDir(ATempDir);

  FList:=TPlayerFileList.Create;
  for FileItemName in AFileList do
    if FileExists(FileItemName) then
    begin
      FileAge(FileItemName, FileItemData.CreatedAt);
      FileItemData.TrackFile:='';

      FList.Add(FileItemName, FileItemData);
    end;
end;

function TPlayerInfoExtractor.GetCount: Integer;
begin
  Result:=FList.Count;
end;

function TPlayerInfoExtractor.GetFileInfo(const Index: Integer): TPlayerFileInfo;
begin
  Result:=FList.Data[Index];
end;

function TPlayerInfoExtractor.GetFileInfoByName(const AFileName: String
  ): TPlayerFileInfo;
begin
  Result:=FList.KeyData[AFileName];
end;

destructor TPlayerInfoExtractor.Destroy;
begin
  FList.Free;
  inherited;
end;

function TPlayerInfoExtractor.LoadData: TThread;
begin
  Result:=TPlayerExtractorManager.Create(Self);
end;

end.

