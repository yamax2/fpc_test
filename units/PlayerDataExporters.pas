unit PlayerDataExporters;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  PlayerThreads;

type

  { TPlayerDataExporter }

  TPlayerProcessEvent = procedure(Sender: TObject;
    const AProcessedCount: Integer) of object;

  TPlayerDataExporter = class
  private
    FExported: Boolean;
    FOnFinish: TNotifyEvent;
    FOnProcess: TPlayerProcessEvent;
    FSessionID, FDir: String;
  public
    constructor Create(ASessionID: String);
    destructor Destroy; override;
    function ExportData: TPlayerThreadManager;

    property Exported: Boolean read FExported;
    property SessionID: String read FSessionID;

    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnProcess: TPlayerProcessEvent read FOnProcess write FOnProcess;
  end;

  TPlayerDataExporterManager = class(TPlayerThreadManager)

  end;

implementation

uses
  dmxPlayer, PlayerLogger, PlayerOptions, FileUtil;

{ TPlayerDataExporter }

constructor TPlayerDataExporter.Create(ASessionID: String);
begin
  inherited Create;
  FExported:=False;
  FSessionID:=ASessionID;

  FDir:=IncludeTrailingPathDelimiter(opts.TempDir);
  FDir:=IncludeTrailingPathDelimiter(FDir + 'html');
  FDir:=IncludeTrailingPathDelimiter(FDir + ASessionID);

  ForceDirectories(FDir);
  if dmPlayer <> nil then ;
end;

destructor TPlayerDataExporter.Destroy;
begin
  logger.Log('export finished');
  logger.log('deleting dir: %s', [FDir]);
  DeleteDirectory(FDir, False);

  inherited;
end;

function TPlayerDataExporter.ExportData: TPlayerThreadManager;
begin
  Result:=nil;

  logger.Log('exporting session: %s', [FSessionID]);
end;

end.

