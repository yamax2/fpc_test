unit PlayerExporters;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  PlayerThreads;

type

  { TPlayerExporter }

  TPlayerProcessEvent = procedure(Sender: TObject;
    const AProcessedCount: Integer) of object;

  TPlayerExporter = class
  private
    FExported: Boolean;
    FOnFinish: TNotifyEvent;
    FOnProcess: TPlayerProcessEvent;
    FSessionID, FDir: String;
  protected
    procedure DoFinish; virtual;
  public
    constructor Create(ASessionID: String);
    destructor Destroy; override;
    function ExportData: TPlayerThreadManager;

    property Exported: Boolean read FExported;
    property SessionID: String read FSessionID;

    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnProcess: TPlayerProcessEvent read FOnProcess write FOnProcess;
  end;

  { TPlayerExporterManager }

  TPlayerExporterManager = class(TPlayerThreadManager)
  private
    FExporter: TPlayerExporter;
  protected
    function GetNextThread: TPlayerThread; override;
    procedure Execute; override;
  public
    constructor Create(AExporter: TPlayerExporter);
    procedure Interrupt(const Force: Boolean = False); override;

    property Exporter: TPlayerExporter read FExporter;
  end;

  { TPlayerExporterThread }

  TPlayerExporterThread = class(TPlayerThread)
  private
    FTripID: Integer;
    function GetManager: TPlayerThreadManager;
  protected
    procedure Execute; override;
  public
    constructor Create(AManager: TPlayerThreadManager; const ATripID: Integer);

    property Manager: TPlayerThreadManager read GetManager;
    property TripID: Integer read FTripID;
  end;

implementation

uses
  dmxPlayer, PlayerLogger, PlayerOptions, FileUtil;

{ TPlayerExporterThread }

function TPlayerExporterThread.GetManager: TPlayerThreadManager;
begin
  Result:=inherited Manager as TPlayerThreadManager;
end;

procedure TPlayerExporterThread.Execute;
begin
  try
    if not Terminated then
      //Synchronize(@Exporter.DoProcess);
  except
    on E: Exception do
    begin
      //logger.Log('error on exporter thread %d, session %s, text: %s',
      //  [FIndex, Extractor.FSessionID, E.Message]);
      raise;
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

function TPlayerExporterManager.GetNextThread: TPlayerThread;
begin
  Result:=nil;
end;

procedure TPlayerExporterManager.Execute;
begin
  inherited;

  if not (False) then
  begin
    Exporter.FExported:=True;
  end;

  Synchronize(@FExporter.DoFinish);
end;

constructor TPlayerExporterManager.Create(AExporter: TPlayerExporter);
begin
 inherited Create;
 FExporter:=AExporter;
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

procedure TPlayerExporter.DoFinish;
begin
  if @FOnFinish <> nil then
    FOnFinish(Self);
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

  Result:=TPlayerExporterManager.Create(Self);
  Result.Start;
end;

end.

