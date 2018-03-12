unit PlayerSessionStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dmxPlayer;

type

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
      const Files: TStringArray);

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

    Script.Script.Add('CREATE TABLE IF NOT EXISTS sessions(id TEXT NOT NULL, crc32 TEXT, cc INTEGER NOT NULL DEFAULT 0, loaded INTEGER NOT NULL DEFAULT 0, PRIMARY KEY(id));');
    Script.Script.Add('CREATE TABLE IF NOT EXISTS tracks(session_id TEXT NOT NULL, rn INTEGER NOT NULL DEFAULT 0, filename TEXT NOT NULL);');

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
  const Files: TStringArray);
var
  Index: Integer;
begin
  with FData do
  begin
    Script.Script.Clear;

    Script.Script.Add('insert into sessions(id, cc, crc32) values(%s, %d, %s);',
      [QuotedStr(ASessionId), Length(Files), QuotedStr(crc32)]);

    // TODO: sql injection!
    for Index:=0 to Length(Files) - 1 do
     Script.Script.Add('insert into tracks(session_id, rn, filename) values(%s, %d, %s);',
       [QuotedStr(ASessionId), Index + 1, QuotedStr(Files[Index])]);

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
    Query.SQL.Text:=Format('select id from sessions where crc32 = %s and cc = %d and loaded = 1 limit 1', [QuotedStr(crc32), FilesCount]);
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
    Script.Script.Text:=Format('update sessions set loaded = 1 where id = %s', [QuotedStr(ASessionID)]);
    Script.Execute;
    Transaction.Commit;
  end;
end;

end.
