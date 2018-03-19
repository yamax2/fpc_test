unit dmxPlayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb;

type

  { TdmPlayer }

  TdmPlayer = class(TDataModule)
    Connection: TSQLite3Connection;
    Script: TSQLScript;
    Query: TSQLQuery;
    Transaction: TSQLTransaction;
    procedure ConnectionLog(Sender: TSQLConnection; EventType: TDBEventType;
      const Msg: String);
  private
    FLogFile: TextFile;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  dmPlayer: TdmPlayer;

implementation
uses
  PlayerSessionStorage, PlayerOptions;

{$R *.lfm}

{ TdmPlayer }

procedure TdmPlayer.ConnectionLog(Sender: TSQLConnection;
  EventType: TDBEventType; const Msg: String);
begin
  if ploDB in opts.LogOptions then
    WriteLn(FLogFile, '[', FormatDateTime(PLAYER_DATE_FORMAT, Now), ']: ',
     'EventType: ', EventType, ' Msg: ', Msg);
end;

constructor TdmPlayer.Create(AOwner: TComponent);
var
  LogFile: String;
begin
  inherited;
  LogFile:=opts.TempDir + 'db.log';
  AssignFile(FLogFile, LogFile);
  if FileExists(LogFile) then Append(FLogFile) else Rewrite(FLogFile);
end;

destructor TdmPlayer.Destroy;
begin
  Connection.Close(True);
  CloseFile(FLogFile);
  inherited;
end;

end.

