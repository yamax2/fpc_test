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

  TPlayerPoint = record
    lat, lon: Double;
    time: TDateTime;
    course, speed: Double;
    ptype: String;
  end;

  TPlayerFileList = specialize TFPGMap<String, TPlayerFileInfo>;
  TPlayerPointArray = array of TPlayerPoint;

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
    procedure AddPoints(const SessionId: String; const TrackRn: Integer;
      Points: TPlayerPointArray);

    function FindSession(const crc32: String; const FilesCount: Integer): String;
    procedure MarkSessionLoaded(const ASessionID: String);
  end;

  operator = (A, B: TPlayerPoint) R: Boolean;

implementation
uses
  sqlite3;

{$R sql.rc}

operator=(A, B: TPlayerPoint)R: Boolean;
begin
  R:=(A.ptype = B.ptype) and (A.lon = B.lon) and (A.lat = B.lat) and
   (A.course = B.course) and (A.speed = B.speed) and (A.time = B.time);
end;

procedure LoadTextFromResource(AList: TStrings;
  const ResourceName: String);
var
  ResourceStream: TResourceStream;
begin
  ResourceStream:=TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  try
    AList.LoadFromStream(ResourceStream);
  finally
    ResourceStream.Free;
  end;
end;

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
    // TODO: Connection.LoadConnection fails?
    sqlite3_enable_load_extension(Connection.Handle, 1);
    sqlite3_load_extension(Connection.Handle, 'libsqlitefunctions.so', nil, nil);

    Script.Script.Clear;
    LoadTextFromResource(Script.Script, 'DB');

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

procedure TPlayerSessionStorage.AddPoints(const SessionId: String;
  const TrackRn: Integer; Points: TPlayerPointArray);
var
  TrackId: Integer;
  Point: TPlayerPoint;
begin
  if Length(Points) = 0 then Exit;

  with FData do
  begin
    Query.SQL.Text:=Format(
      'select rowid id from tracks where session_id = %s and rn = %d limit 1',
      [QuotedStr(SessionId), TrackRn + 1]
    );

    Query.Open;
    if Query.EOF then
      raise Exception.CreateFmt('no track for %s, %d', [SessionId, TrackRn]);

    TrackId:=Query.FieldByName('id').AsInteger;
    Query.Close;

    Script.Script.Clear;
    for Point in Points do
      Script.Script.Add(
        'insert into points(track_id, lat, lon, time, course, speed, type) ' +
        'values(%d, %.6n, %.6n, %s, %n, %n, %s);', [TrackId,
         Point.lat,
         Point.lon,
         QuotedStr(FormatDateTime(PLAYER_DATE_FORMAT, Point.time)),
         Point.course,
         Point.speed, QuotedStr(Point.ptype)]);

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
