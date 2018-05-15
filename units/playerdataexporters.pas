unit PlayerDataExporters;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  PlayerThreads,
  PlayerSessionStorage;

type

  { TPlayerDataExporter }

  TPlayerProcessEvent = procedure(Sender: TObject;
    const AProcessedCount: Integer) of object;

  TPlayerDataExporter = class
  private
    FOnFinish: TNotifyEvent;
    FOnProcess: TPlayerProcessEvent;
    FStorage: TPlayerSessionStorage;
  public
    constructor Create(AStorage: TPlayerSessionStorage);
    function ExportData: TPlayerThreadManager;

    property Storage: TPlayerSessionStorage read FStorage;

    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnProcess: TPlayerProcessEvent read FOnProcess write FOnProcess;
  end;

implementation

{ TPlayerDataExporter }

constructor TPlayerDataExporter.Create(AStorage: TPlayerSessionStorage);
begin
  inherited Create;
  FStorage:=AStorage;
end;

function TPlayerDataExporter.ExportData: TPlayerThreadManager;
begin
  Result:=nil;
end;

end.

