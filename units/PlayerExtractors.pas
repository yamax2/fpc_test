unit PlayerExtractors;

{$mode objfpc}{$H+}

interface

uses
{$ifdef unix}
  cthreads,
  cmem,
{$endif}
  Classes, SysUtils, PlayerThreads, PlayerSubtitleExtractors,
  PlayerSessionStorage;

type
  { TPlayerInfoExtractor }

  TPlayerInfoExtractor = class
  private
    FList: TPlayerFileList;
    FLoaded: Boolean;
    FSessionID: String;
    FSubtileExtractorType: TSubtitleExtractorType;
    FTempDir: String;
    FCrc32: String;
    FStorage: TPlayerSessionStorage;
    function FindSession: Boolean;
    function GetCount: Integer;
    function GetFileInfo(const Index: Integer): TPlayerFileInfo;
    function GetFileInfoByName(const AFileName: String): TPlayerFileInfo;
    function GetFileName(const Index: Integer): String;
    procedure PrepareSession;
    procedure PrepareTempDir(const ATempDir: String);
  protected
    function GetSubtileExtractorType: TPlayerSubtitleExtractorClass; virtual;
  public
    constructor Create(AFileList: TStringList; const ATempDir: String);
    destructor Destroy; override;

    function LoadData: TThread;

    property Count: Integer read GetCount;
    property FileName[Index: Integer]: String read GetFileName; default;
    property FileInfo[Index: Integer]: TPlayerFileInfo read GetFileInfo;
    property FileInfoByName[AFileName: String]: TPlayerFileInfo
     read GetFileInfoByName;

    property Crc32: String read FCrc32;
    property Loaded: Boolean read FLoaded;
    property SessionID: String read FSessionID;
    property SubtileExtractorType: TSubtitleExtractorType
     read FSubtileExtractorType write FSubtileExtractorType;
    property TempDir: String read FTempDir;
  end;

  { TPlayerExtractorManager }

  TPlayerExtractorManager = class(TPlayerThreadManager)
  private
    FExtractor: TPlayerInfoExtractor;
    FCount: Integer;
    FCriticalSection: TRTLCriticalSection;
  protected
    function GetNextThread: TPlayerThread; override;
  public
    constructor Create(AExtractor: TPlayerInfoExtractor);
    destructor Destroy; override;

    property Extractor: TPlayerInfoExtractor read FExtractor;
  end;

  { TPlayerExtractorThread }

  TPlayerExtractorThread = class(TPlayerThread)
  private
    FIndex: Integer;
    FDataFile, FTrackFile: String;
    function GetExtractor: TPlayerInfoExtractor;
    function GetManager: TPlayerExtractorManager;
    procedure ExtractTrack;
    procedure ParseTrack;
  protected
    procedure Execute; override;
  public
    constructor Create(AManager: TPlayerExtractorManager; const AIndex: Integer);

    property Extractor: TPlayerInfoExtractor read GetExtractor;
    property Manager: TPlayerExtractorManager read GetManager;
  end;

implementation

uses
  FileUtil, crc;

{ TPlayerExtractorThread }

function TPlayerExtractorThread.GetManager: TPlayerExtractorManager;
begin
  Result:=inherited Manager as TPlayerExtractorManager;
end;

procedure TPlayerExtractorThread.ExtractTrack;
var
  Service: TPlayerSubtitleExtractor;
begin
  FDataFile:=Format('%ssubtitles_%d.data', [Extractor.TempDir, FIndex]);
  Service:=Extractor.GetSubtileExtractorType.Create(Extractor[FIndex], FDataFile);
  try
    Service.Extract;
  finally
    Service.Free;
  end;
end;

procedure TPlayerExtractorThread.ParseTrack;
var
  Service: TPlayerTrackParser;
begin
  FTrackFile:=Format('%strack_%d.data', [Extractor.TempDir, FIndex]);
  Service:=TPlayerNmeaTrackParser.Create(FDataFile, FTrackFile);
  try
    Service.Parse;
  finally
    Service.Free;
  end;
end;

function TPlayerExtractorThread.GetExtractor: TPlayerInfoExtractor;
begin
  Result:=Manager.Extractor;
end;

procedure TPlayerExtractorThread.Execute;
var
  Info: TPlayerFileInfo;
begin
  if not Terminated then ExtractTrack;
  if not Terminated then
  begin
    ParseTrack;
    EnterCriticalsection(Manager.FCriticalSection);
    try
      Info:=Extractor.FileInfo[FIndex];
      Info.TrackFile:=FTrackFile;
      Extractor.FList.Data[FIndex]:=Info;
    finally
      LeaveCriticalsection(Manager.FCriticalSection);
    end;
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
  InitCriticalSection(FCriticalSection);
  inherited Create;
end;

destructor TPlayerExtractorManager.Destroy;
begin
  Extractor.FLoaded:=True;
  DoneCriticalsection(FCriticalSection);
  inherited;
end;

{ TPlayerInfoExtractor }

function TPlayerInfoExtractor.GetFileName(const Index: Integer): String;
begin
  Result:=FList.Keys[Index];
end;

procedure TPlayerInfoExtractor.PrepareSession;
var
  Guid: TGUID;
begin
  if CreateGUID(Guid) <> 0 then raise Exception.Create('error');

  FSessionID:=LowerCase(GUIDToString(Guid));
  Delete(FSessionID, 1, 1);
  Delete(FSessionID, Length(FSessionID), 1);

  FTempDir:=IncludeTrailingPathDelimiter(FTempDir + FSessionID);
  ForceDirectories(FTempDir);
end;

procedure TPlayerInfoExtractor.PrepareTempDir(const ATempDir: String);
begin
  FTempDir:=IncludeTrailingPathDelimiter(ATempDir);
  ForceDirectories(FTempDir);
  FStorage:=TPlayerSessionStorage.Create(FTempDir + 'player.db');
end;

function TPlayerInfoExtractor.GetSubtileExtractorType: TPlayerSubtitleExtractorClass;
begin
  case SubtileExtractorType of
    seFFmpeg: Result:=TPlayerSubtitleFfmpegExtractor;
    seMP4Box: Result:=TPlayerSubtitleMP4BoxExtractor;
  end;
end;

constructor TPlayerInfoExtractor.Create(AFileList: TStringList;
  const ATempDir: String);
var
  FileItemName: String;
  FileItemData: TPlayerFileInfo;
begin
  inherited Create;
  FLoaded:=False;
  FSubtileExtractorType:=seFFmpeg;

  FList:=TPlayerFileList.Create;
  for FileItemName in AFileList do
    if FileExists(FileItemName) then
    begin
      FileAge(FileItemName, FileItemData.CreatedAt);
      FileItemData.TrackFile:='';
      FileItemData.Size:=FileUtil.FileSize(FileItemName);

      FList.Add(FileItemName, FileItemData);
    end;

  PrepareTempDir(ATempDir);
  if not FindSession then PrepareSession else FLoaded:=True;
end;

function TPlayerInfoExtractor.FindSession: Boolean;
var
  List: TStringList;
  Index: Integer;
  Info: TPlayerFileInfo;
  CrcValue: Cardinal;
begin
  List:=TStringList.Create;
  try
    for Index:=0 to Count - 1 do
    begin
      Info:=Self.FileInfo[Index];
      List.Add(Format('%s,%d,%s',
        [Self[Index], Info.Size, FormatDateTime(PLAYER_DATE_FORMAT, Info.CreatedAt)]));
    end;

    List.Sort;

    CrcValue:=0;
    CrcValue:=crc.crc32(CrcValue, PByte(List.Text), Length(List.Text));

    FCrc32:=IntToHex(CrcValue, 8);
    FSessionID:=FStorage.FindSession(FCrc32, FList.Count);

    Result:=FSessionID <> '';
  finally
    List.Free;
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
  FStorage.Free;
  FList.Free;
  inherited;
end;

function TPlayerInfoExtractor.LoadData: TThread;
begin
  Result:=nil;

  if not Loaded then
  begin
    FStorage.AddSession(FSessionID, FCrc32, FList);
    Result:=TPlayerExtractorManager.Create(Self);
  end;
end;

end.

