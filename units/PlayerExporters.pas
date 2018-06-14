unit PlayerExporters;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  SqlDB,
  fgl,
  PlayerThreads;

type

  TPlayerTripInfo = specialize TFPGMap<String, Variant>;

  { TPlayerExporter }

  TPlayerProcessEvent = procedure(Sender: TObject;
    const AProcessedCount: Integer) of object;

  TPlayerExporter = class
  private
    FExported, FFailed: Boolean;
    FOnFinish: TNotifyEvent;
    FOnProcess: TPlayerProcessEvent;
    FSessionID, FDir: String;
    FProcessedCount: Integer;
    function GetCount: Integer;
  protected
    function CreateQuery(const SQLResource: String = ''): TSQLQuery;
    procedure DoFinish; virtual;
    procedure DoProcess; virtual;
  public
    constructor Create(ASessionID: String);
    destructor Destroy; override;
    function ExportData: TPlayerThreadManager;

    property Count: Integer read GetCount;
    property Exported: Boolean read FExported;
    property SessionID: String read FSessionID;

    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnProcess: TPlayerProcessEvent read FOnProcess write FOnProcess;
  end;

  { TPlayerExporterManager }

  TPlayerExporterManager = class(TPlayerThreadManager)
  private
    FExporter: TPlayerExporter;
    FQuery: TSQLQuery;
  protected
    function GetNextThread: TPlayerThread; override;
    procedure Execute; override;
  public
    constructor Create(AExporter: TPlayerExporter);
    destructor Destroy; override;
    procedure Interrupt(const Force: Boolean = False); override;

    property Exporter: TPlayerExporter read FExporter;
  end;

  { TPlayerExporterThread }

  TPlayerExporterThread = class(TPlayerThread)
  private
    FTripInfo: TPlayerTripInfo;
    function GetManager: TPlayerExporterManager;
  protected
    procedure Execute; override;
  public
    constructor Create(AManager: TPlayerThreadManager; AQuery: TSQLQuery);
    destructor Destroy; override;

    property Manager: TPlayerExporterManager read GetManager;
    property TripInfo: TPlayerTripInfo read FTripInfo;
  end;

implementation

uses
  DB, dmxPlayer, PlayerLogger, PlayerOptions, FileUtil, PlayerSessionStorage;

{ TPlayerExporterThread }

function TPlayerExporterThread.GetManager: TPlayerExporterManager;
begin
  Result:=inherited Manager as TPlayerExporterManager;
end;

procedure TPlayerExporterThread.Execute;
begin
  try
    if not Terminated then
      Synchronize(@Manager.Exporter.DoProcess);
  except
    on E: Exception do
    begin
      logger.Log('error on exporter trip %d, session %s, text: %s',
        [Integer(FTripInfo['id']), Manager.Exporter.FSessionID, E.Message]);

      Manager.Exporter.FFailed:=True;
      Manager.Interrupt(True);
    end;
  end;
end;

constructor TPlayerExporterThread.Create(AManager: TPlayerThreadManager;
  AQuery: TSQLQuery);
var
  Field: TField;
begin
  inherited Create(AManager);

  FTripInfo:=TPlayerTripInfo.Create;
  for Field in AQuery.Fields do
    FTripInfo.Add(Field.FieldName.ToLower, Field.Value);
end;

destructor TPlayerExporterThread.Destroy;
begin
  FTripInfo.Free;
  inherited;
end;

{ TPlayerExporterManager }

function TPlayerExporterManager.GetNextThread: TPlayerThread;
var
  id: Integer;
begin
  if FQuery.EOF then Result:=nil else
  begin
    id:=FQuery.FieldByName('id').AsInteger;
    logger.Log('starting new exporter thread: trip %d for session %s',
      [id, Exporter.FSessionID]);
    Result:=TPlayerExporterThread.Create(Self, FQuery);
    FQuery.Next;
  end
end;

procedure TPlayerExporterManager.Execute;
begin
  try
    try
      inherited;

      if FQuery.EOF then
        Exporter.FExported:=True;
    except
      on E: Exception do
      begin
        logger.Log('error on exporting session %s, text: %s',
          [Exporter.FSessionID, E.Message]);

        FExporter.FFailed:=True;
        Interrupt(True);
      end;
    end;
  finally
    Synchronize(@FExporter.DoFinish);
  end;
end;

constructor TPlayerExporterManager.Create(AExporter: TPlayerExporter);
begin
  FExporter:=AExporter;

  FQuery:=Exporter.CreateQuery('GET_TRIPS');
  FQuery.ParamByName('session_id').AsString:=Exporter.SessionID;
  FQuery.Open;

  inherited Create;
end;

destructor TPlayerExporterManager.Destroy;
begin
  FQuery.Close;
  FQuery.Free;

  inherited;
end;

procedure TPlayerExporterManager.Interrupt(const Force: Boolean);
var
  List: TList;
  Index: Integer;

  CurThread: TPlayerExporterThread;
begin
  inherited;

  if not Force then Exit;
  List:=ThreadList.LockList;
  try
    for Index:=0 to List.Count - 1 do
    begin
      CurThread:=TPlayerExporterThread(List[Index]);
      CurThread.Terminate;
    end;
  finally
    ThreadList.UnlockList;
  end;
end;

{ TPlayerExporter }

function TPlayerExporter.GetCount: Integer;
begin
  Result:=0;
end;

function TPlayerExporter.CreateQuery(const SQLResource: String): TSQLQuery;
begin
  Result:=TSQLQuery.Create(dmPlayer);
  Result.DataBase:=dmPlayer.Connection;
  Result.Transaction:=dmPlayer.Transaction;

  if SQLResource <> '' then
    LoadTextFromResource(Result.SQL, SQLResource);
end;

procedure TPlayerExporter.DoFinish;
begin
  if @FOnFinish <> nil then
    FOnFinish(Self);
end;

procedure TPlayerExporter.DoProcess;
begin
  Inc(FProcessedCount);
  if @FOnProcess <> nil then
    FOnProcess(Self, FProcessedCount);
end;

constructor TPlayerExporter.Create(ASessionID: String);
begin
  inherited Create;
  FExported:=False;
  FSessionID:=ASessionID;

  FDir:=IncludeTrailingPathDelimiter(opts.TempDir);
  FDir:=IncludeTrailingPathDelimiter(FDir + 'html');
  FDir:=IncludeTrailingPathDelimiter(FDir + ASessionID);

  DeleteDirectory(FDir, False);
  logger.log('recreating dir: %s', [FDir]);
  ForceDirectories(FDir);

  if dmPlayer <> nil then ;
end;

destructor TPlayerExporter.Destroy;
begin
  logger.Log('export finished: %s', [FSessionID]);

  inherited;
end;

function TPlayerExporter.ExportData: TPlayerThreadManager;
begin
  logger.Log('exporting session: %s', [FSessionID]);

  FProcessedCount:=0;
  FFailed:=False;

  Result:=TPlayerExporterManager.Create(Self);
  Result.Start;
end;

end.

