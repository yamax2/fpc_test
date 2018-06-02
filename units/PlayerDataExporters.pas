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
    FOnFinish: TNotifyEvent;
    FOnProcess: TPlayerProcessEvent;
    FSessionID: String;
  public
    constructor Create(ASessionID: String);
    function ExportData: TPlayerThreadManager;

    property SessionID: String read FSessionID;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnProcess: TPlayerProcessEvent read FOnProcess write FOnProcess;
  end;

  TPlayerDataExporterManager = class(TPlayerThreadManager)

  end;

implementation

uses
  dmxPlayer, PlayerLogger, Dialogs;

{ TPlayerDataExporter }

constructor TPlayerDataExporter.Create(ASessionID: String);
begin
  inherited Create;
  FSessionID:=ASessionID;
end;

function TPlayerDataExporter.ExportData: TPlayerThreadManager;
begin
  Result:=nil;

  logger.Log('exporting session: %s', [FSessionID]);
end;

end.

