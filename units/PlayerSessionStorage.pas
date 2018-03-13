unit PlayerSessionStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Fgl, dmxPlayer;

const
  PLAYER_DATE_FORMAT = 'YYYY-MM-DD HH:MM:SS';

type
  TPlayerFileInfo = packed record
    Size: Int64;
    CreatedAt: TDateTime;
  end;

  { TPlayerPoint }

  TPlayerPoint = packed record
    lat, lon: Double;
    time: TDateTime;
    course, speed: Double;
    ptype: Char;
  end;

  TPlayerFileList = specialize TFPGMap<String, TPlayerFileInfo>;

  { TPlayerSessionStorage }

  TPlayerSessionStorage = class
  private
    FDataFile: String;
    FData: TdmPlayer;
    procedure PrepareDataModule;
    procedure PrepareDatabase;
  public
    constructor Create(ADataFile: String); virtual;
    destructor Destroy; override;

    procedure AddSession(const ASessionId, crc32: String;
      Files: TPlayerFileList);

    function FindSession(const crc32: String; const FilesCount: Integer): String;
    procedure MarkSessionLoaded(const ASessionID: String);
  end;

implementation

{ TPlayerSessionStorage }

procedure TPlayerSessionStorage.PrepareDataModule;
begin
  FData.Connection.DatabaseName:=FDataFile;

  try
    FData.Connection.Open;
  except
    if FileExists(FDataFile) then DeleteFile(FDataFile);
  end;

  with FData do
  begin
    if not Connection.Connected then Connection.Open;
    PrepareDatabase;
  end;
end;

procedure TPlayerSessionStorage.PrepareDatabase;
begin
  with FData do
  begin
    Script.Script.Clear;

    Script.Script.Add('CREATE TABLE IF NOT EXISTS sessions(id TEXT NOT NULL, ' +
     'crc32 TEXT, cc INTEGER NOT NULL DEFAULT 0, '+
     'loaded INTEGER NOT NULL DEFAULT 0, PRIMARY KEY(id));');

    Script.Script.Add('CREATE TABLE IF NOT EXISTS tracks(' +
      'session_id TEXT NOT NULL, rn INTEGER NOT NULL DEFAULT 0,' +
      'filename TEXT NOT NULL, created_at TEXT NOT NULL, ' +
      'size INTEGER NOT NULL DEFAULT 0);');

    Script.Script.Add('CREATE TABLE IF NOT EXISTS points(' +
 	'track_id INTEGER NOT NULL,' +
	'lat REAL,' +
	'lon REAL, ' +
	'time TEXT, ' +
	'course REAL, ' +
	'speed REAL, ' +
	'type TEXT NOT NULL);');

    Script.Script.Add('CREATE INDEX IF NOT EXISTS ' +
      'ix_points_track_id ON points(track_id);');

    Script.Script.Add('CREATE INDEX IF NOT EXISTS ' +
       'ix_tracks ON tracks(session_id, rn);');

    Script.Execute;
    Transaction.Commit;
  end;
end;

constructor TPlayerSessionStorage.Create(ADataFile: String);
begin
  inherited Create;
  FDataFile:=ADataFile;

  FData:=TdmPlayer.Create(nil);
  PrepareDataModule;
end;

destructor TPlayerSessionStorage.Destroy;
begin
  FData.Connection.Close(True);
  FData.Free;
  inherited;
end;

procedure TPlayerSessionStorage.AddSession(const ASessionId, crc32: String;
  Files: TPlayerFileList);
var
  Index: Integer;
begin
  with FData do
  begin
    Script.Script.Clear;

    Script.Script.Add('insert into sessions(id, cc, crc32) values(%s, %d, %s);',
      [QuotedStr(ASessionId), Files.Count, QuotedStr(crc32)]);

    // TODO: sql injection!
    for Index:=0 to Files.Count - 1 do
      Script.Script.Add('insert into tracks(session_id, rn, filename, ' +
        'created_at, size) values(%s, %d, %s, %s, %d);',
        [QuotedStr(ASessionId), Index + 1,
         QuotedStr(Files.Keys[Index]),
         QuotedStr(FormatDateTime(PLAYER_DATE_FORMAT, Files.Data[Index].CreatedAt)),
         Files.Data[Index].Size]);

    Script.Execute;
    Transaction.Commit;
  end;
end;

function TPlayerSessionStorage.FindSession(const crc32: String;
  const FilesCount: Integer): String;
begin
  Result:='';

  with FData do
  try
    Query.SQL.Text:=Format('select id from sessions where crc32 = %s ' +
      'and cc = %d and loaded = 1 limit 1', [QuotedStr(crc32), FilesCount]);
    Query.Open;

    if not Query.EOF then Result:=Query.FieldByName('id').AsString;
  finally
    Query.Close;
  end;
end;

procedure TPlayerSessionStorage.MarkSessionLoaded(const ASessionID: String);
begin
  with FData do
  begin
    Script.Script.Text:=Format('update sessions set loaded = 1 where id = %s',
      [QuotedStr(ASessionID)]);
    Script.Execute;
    Transaction.Commit;
  end;
end;

end.
