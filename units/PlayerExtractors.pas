unit PlayerExtractors;

{$mode objfpc}{$H+}

interface

uses
{$ifdef unix}
  cthreads,
  cmem,
{$endif}
  Classes,
  SysUtils,
  PlayerThreads,
  PlayerSubtitleExtractors,
  PlayerSessionStorage,
  PlayerDataExporters;

type
  { TPlayerInfoExtractor }

  TPlayerInfoExtractor = class
  private
    FList: TPlayerFileList;
    FLoaded: Boolean;
    FOnFinish: TNotifyEvent;
    FOnProcess: TPlayerProcessEvent;
    FSessionID: String;
    FTempDir: String;
    FCrc32: String;
    FExporter: TPlayerDataExporter;
    FStorage: TPlayerSessionStorage;
    FProcessedCount: Integer;
    function FindSession: Boolean;
    function GetCount: Integer;
    function GetFileInfo(const Index: Integer): TPlayerFileInfo;
    function GetFileInfoByName(const AFileName: String): TPlayerFileInfo;
    function GetFileName(const Index: Integer): String;
    procedure PrepareSession;
    procedure PrepareTempDir(const ATempDir: String);
  protected
    procedure DoFinish; virtual;
    procedure DoProcess; virtual;
    procedure SetOnFinish(AValue: TNotifyEvent); virtual;
    procedure SetOnProcess(AValue: TPlayerProcessEvent); virtual;
  public
    constructor Create(AFileList: TStrings);
    destructor Destroy; override;

    function ExportData: TPlayerThreadManager;
    function LoadData: TPlayerThreadManager;

    property Count: Integer read GetCount;
    property FileName[Index: Integer]: String read GetFileName; default;
    property FileInfo[Index: Integer]: TPlayerFileInfo read GetFileInfo;
    property FileInfoByName[AFileName: String]: TPlayerFileInfo
     read GetFileInfoByName;

    property Crc32: String read FCrc32;
    property Loaded: Boolean read FLoaded;
    property SessionID: String read FSessionID;
    property TempDir: String read FTempDir;

    property OnFinish: TNotifyEvent read FOnFinish write SetOnFinish;
    property OnProcess: TPlayerProcessEvent read FOnProcess write SetOnProcess;
  end;

  { TPlayerExtractorManager }

  TPlayerExtractorManager = class(TPlayerThreadManager)
  private
    FExtractor: TPlayerInfoExtractor;
    FCount: Integer;
    FCriticalSection: TRTLCriticalSection;
  protected
    procedure Execute; override;
    function GetNextThread: TPlayerThread; override;
  public
    constructor Create(AExtractor: TPlayerInfoExtractor);
    destructor Destroy; override;

    procedure Interrupt(const Force: Boolean = False); override;

    property Extractor: TPlayerInfoExtractor read FExtractor;
  end;

  { TPlayerExtractorThread }

  TPlayerExtractorThread = class(TPlayerThread)
  private
    FIndex: Integer;
    FDataFile: String;
    FService: TPlayerExtractorService;
    function GetExtractor: TPlayerInfoExtractor;
    function GetManager: TPlayerExtractorManager;
    procedure ExtractTrack;
    procedure ParseTrack;
    procedure SavePoints(Sender: TPlayerTrackParser;
      Points: TPlayerPointArray);
  protected
    procedure Execute; override;
  public
    constructor Create(AManager: TPlayerExtractorManager; const AIndex: Integer);

    property Extractor: TPlayerInfoExtractor read GetExtractor;
    property Manager: TPlayerExtractorManager read GetManager;
  end;

implementation

uses
  FileUtil, crc, PlayerLogger, PlayerOptions;

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
  Service:=TPlayerSubtitleFfmpegExtractor.Create(Extractor[FIndex], FDataFile);
  try
    FService:=Service;
    logger.Log('extracting track in session %s, %d(%s) into %s',
      [Extractor.FSessionID, FIndex, Extractor[FIndex], FDataFile]);
    Service.Extract;
  finally
    Service.Free;
    FService:=nil;
  end;
end;

procedure TPlayerExtractorThread.ParseTrack;
var
  Service: TPlayerTrackParser;
begin
  Service:=TPlayerNmeaTrackParser.Create(FDataFile, Self);
  try
    FService:=Service;
    logger.Log('parsing track in session %s, %d (%s)',
      [Extractor.FSessionID, FIndex, FDataFile]);
    Service.OnSave:=@SavePoints;
    Service.Parse;
  finally
    Service.Free;
    FService:=nil;
  end;
end;

procedure TPlayerExtractorThread.SavePoints(Sender: TPlayerTrackParser;
  Points: TPlayerPointArray);
begin
  EnterCriticalsection(Manager.FCriticalSection);
  try
    logger.Log('saving track points in session %s, %d, points: %d',
      [Extractor.FSessionID, FIndex, Length(Points)]);
    Extractor.FStorage.AddPoints(Extractor.SessionID, FIndex, Points);
  finally
    LeaveCriticalsection(Manager.FCriticalSection);
  end;
end;

function TPlayerExtractorThread.GetExtractor: TPlayerInfoExtractor;
begin
  Result:=Manager.Extractor;
end;

procedure TPlayerExtractorThread.Execute;
begin
  try
    if not Terminated then ExtractTrack;
    if not Terminated then ParseTrack;
    if not Terminated then
      Synchronize(@Extractor.DoProcess);
  except
    on E: Exception do
    begin
      logger.Log('error on extractor thread %d, session %s, text: %s',
        [FIndex, Extractor.FSessionID, E.Message]);
      raise;
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

procedure TPlayerExtractorManager.Execute;
begin
  inherited;

  if not (FCount < Extractor.Count) then
  begin
    logger.Log('finalizing session %s', [FExtractor.FSessionID]);
    FExtractor.FStorage.FinalizeSession(FExtractor.FSessionID);
    Extractor.FStorage.GenerateGpx(FExtractor.FSessionID);
    Extractor.FLoaded:=True;
  end;

  Synchronize(@FExtractor.DoFinish);
end;

function TPlayerExtractorManager.GetNextThread: TPlayerThread;
begin
  if not (FCount < Extractor.Count) then Result:=nil else
  begin
    logger.Log('starting new extractor thread: %d for session %s',
      [FCount, Extractor.FSessionID]);
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
  DeleteDirectory(FExtractor.FTempDir, False);
  DoneCriticalsection(FCriticalSection);
  inherited;
end;

procedure TPlayerExtractorManager.Interrupt(const Force: Boolean);
var
  List: TList;
  Index: Integer;

  CurThread: TPlayerExtractorThread;
begin
  inherited;

  if not Force then Exit;
  List:=ThreadList.LockList;
  try
    for Index:=0 to List.Count - 1 do
    begin
      CurThread:=TPlayerExtractorThread(List[Index]);
      if CurThread.FService <> nil then
        CurThread.FService.StopProcess;
    end;
  finally
    ThreadList.UnlockList;
  end;
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

  logger.Log('new session: %s', [FSessionID]);
  FTempDir:=IncludeTrailingPathDelimiter(FTempDir + FSessionID);
  logger.Log('session dir: %s', [FTempDir]);
  ForceDirectories(FTempDir);
end;

procedure TPlayerInfoExtractor.PrepareTempDir(const ATempDir: String);
var
  db: String;
begin
  FTempDir:=IncludeTrailingPathDelimiter(ATempDir);
  logger.Log('temp dir: %s', [FTempDir]);
  ForceDirectories(FTempDir);

  db:=FTempDir + 'player.db';
  logger.Log('database: %s', [db]);

  FStorage:=TPlayerSessionStorage.Create(db);
end;

procedure TPlayerInfoExtractor.SetOnFinish(AValue: TNotifyEvent);
begin
  FOnFinish:=AValue;
  FExporter.OnFinish:=AValue;
end;

procedure TPlayerInfoExtractor.SetOnProcess(AValue: TPlayerProcessEvent);
begin
  FOnProcess:=AValue;
  FExporter.OnProcess:=AValue;
end;

procedure TPlayerInfoExtractor.DoFinish;
begin
  if @FOnFinish <> nil then
    FOnFinish(Self);
end;

procedure TPlayerInfoExtractor.DoProcess;
begin
  Inc(FProcessedCount);
  if @FOnProcess <> nil then
    FOnProcess(Self, FProcessedCount);
end;

constructor TPlayerInfoExtractor.Create(AFileList: TStrings);
var
  FileItemName: String;
  FileItemData: TPlayerFileInfo;

  Index: Integer;
begin
  inherited Create;
  FLoaded:=False;

  logger.Log('extractor started');

  FList:=TPlayerFileList.Create;
  for FileItemName in AFileList do
    if FileExists(FileItemName) then
    begin
      FileAge(FileItemName, FileItemData.CreatedAt);
      FileItemData.Size:=FileUtil.FileSize(FileItemName);

      Index:=FList.Add(FileItemName, FileItemData);

      logger.Log('File added "%s", created_at %s, size: %d, index: %d',
         [FileItemName,
          FormatDateTime(PLAYER_DATE_FORMAT, FileItemData.CreatedAt),
          FileItemData.Size,
          Index
         ]);
    end;

  PrepareTempDir(opts.TempDir);
  FLoaded:=FindSession;
  if not FLoaded then PrepareSession;
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
    logger.Log('find session: %s', [FCrc32]);

    Result:=FSessionID <> '';
    logger.Log('session exists: %s', [BoolToStr(Result, 'yes', 'no')]);
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
  if FExporter <> nil then;
    FExporter.Free;

  FStorage.Free;
  FList.Free;
  logger.Log('extractor finished');

  inherited;
end;

function TPlayerInfoExtractor.ExportData: TPlayerThreadManager;
begin
  Result:=nil;
  if not Loaded then Exit;

  FExporter:=TPlayerDataExporter.Create(SessionID);
  Result:=FExporter.ExportData;
end;

function TPlayerInfoExtractor.LoadData: TPlayerThreadManager;
begin
  Result:=nil;
  if Loaded then Exit;

  FProcessedCount:=0;
  logger.Log('loading session: %s', [FSessionID]);

  FStorage.AddSession(FSessionID, FCrc32, FList);
  Result:=TPlayerExtractorManager.Create(Self);

  Result.Start;
end;

end.

