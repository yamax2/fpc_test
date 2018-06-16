unit PlayerExporters;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  SqlDB,
  fpjson,
  PlayerThreads;

type

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
    FData: TJSONArray;
    procedure AddTrip;
    procedure SaveTrip;
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
    FTripID: Integer;
    function GetManager: TPlayerExporterManager;
  protected
    procedure Execute; override;
  public
    constructor Create(AManager: TPlayerThreadManager;
      const ATripID: Integer);

    property Manager: TPlayerExporterManager read GetManager;
    property TripID: Integer read FTripID;
  end;

implementation

uses
  Math, DB, dmxPlayer, PlayerLogger, PlayerOptions, FileUtil, PlayerSessionStorage;

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
        [FTripID, Manager.Exporter.FSessionID, E.Message]);

      Manager.Exporter.FFailed:=True;
      Manager.Interrupt(True);
    end;
  end;
end;

constructor TPlayerExporterThread.Create(AManager: TPlayerThreadManager;
  const ATripID: Integer);
begin
  inherited Create(AManager);
  FTripID:=ATripID;
end;

{ TPlayerExporterManager }

procedure TPlayerExporterManager.AddTrip;
var
  obj: TJSONObject;
begin
  obj:=TJSONObject.Create;

  obj.Integers['id']:=FQuery.FieldByName('id').AsInteger;
  obj.Strings['started_at']:=FQuery.FieldByName('started_at').AsString;
  obj.Integers['duration']:=FQuery.FieldByName('duration').AsInteger;
  obj.Strings['distance']:=FloatToStr(RoundTo(FQuery.FieldByName('distance').AsFloat, -2));
  obj.Strings['avg_speed']:=FloatToStr(RoundTo(FQuery.FieldByName('avg_speed').AsFloat, -2));
  obj.Strings['size']:=FloatToStr(RoundTo(FQuery.FieldByName('size_mb').AsFloat, -2));

  FData.Add(obj);
  (FQuery.FieldByName('gpx') as TBlobField).SaveToFile(
    Format('%s%d.gpx', [Exporter.FDir, FQuery.FieldByName('id').AsInteger])
  );
end;

procedure TPlayerExporterManager.SaveTrip;
var
  List: TStringList;
begin
  List:=TStringList.Create;
  try
    List.Text:=FData.AsJSON;
    List.SaveToFile(Format('%strips.json', [Exporter.FDir]));
  finally
    List.Free;
  end;
end;

function TPlayerExporterManager.GetNextThread: TPlayerThread;
var
  id: Integer;
begin
  if FQuery.EOF then Result:=nil else
  begin
    id:=FQuery.FieldByName('id').AsInteger;
    AddTrip;

    logger.Log('starting new exporter thread: trip %d for session %s',
      [id, Exporter.FSessionID]);
    Result:=TPlayerExporterThread.Create(Self, id);

    SaveTrip;
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

  FQuery:=Exporter.CreateQuery('GET_TRIPS_LIST');
  FQuery.ParamByName('session_id').AsString:=Exporter.SessionID;
  FQuery.Open;

  FData:=TJSONArray.Create;

  inherited Create;
end;

destructor TPlayerExporterManager.Destroy;
begin
  FQuery.Close;
  FQuery.Free;
  FData.Free;

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

