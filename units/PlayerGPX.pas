unit PlayerGPX;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, PlayerThreads, sqldb;

type

  { TPlayerGPX }

  TPlayerGPX = class
  private
    FTripId: Integer;
    FFile: TextFile;
    FLines: TStringList;
    FThread: TPlayerThreadManager;
    FFileName: String;
    Query: TSQLQuery;
    procedure AddGpxPoint;
    procedure AppendLines;
    function FormatGpxTime(const AValue: String): String;
    function IsTerminated: Boolean;
    function ParseDuration(const AValue: Integer): String;
  public
    constructor Create(const ATripID: Integer; const AGpxFileName: String;
      AThread: TPlayerThreadManager = nil);
    destructor Destroy; override;

    procedure Convert;
  end;

implementation
uses
  dmxPlayer, PlayerLogger, PlayerSessionStorage, DateUtils, Forms, Math,
  StrUtils;

{ TPlayerGPX }

procedure TPlayerGPX.AddGpxPoint;
begin
  with Query do
    FLines.Add(Format('    <trkpt lat="%.6n" lon="%.6n"><time>%s</time><course>%n</course><speed>%n</speed></trkpt>',
      [
        FieldByName('lat').AsFloat, FieldByName('lon').AsFloat,
        FormatGpxTime(FieldByName('time').AsString),
        FieldByName('course').AsFloat,
        FieldByName('speed').AsFloat
      ]));
end;

procedure TPlayerGPX.AppendLines;
begin
  if FLines.Count = 0 then Exit;
  if IsTerminated then Exit;

  Write(FFile, FLines.Text);
  FLines.Clear;
end;

function TPlayerGPX.FormatGpxTime(const AValue: String): String;
var
  dt: TDateTime;
begin
  dt:=ScanDateTime(PLAYER_DATE_FORMAT, AValue);
  Result:=FormatDateTime(PLAYER_DATE_FORMAT, dt).Replace(' ', 'T') + 'Z';
end;

function TPlayerGPX.IsTerminated: Boolean;
begin
 Result:=(FThread <> nil) and FThread.Terminated;
end;

function TPlayerGPX.ParseDuration(const AValue: Integer): String;
var
  Index, v: Integer;
begin
  Result:='';

  for Index:=3 downto 1 do
  begin
    v:=(AValue mod Trunc(Power(60, Index))) div Trunc(Power(60, Pred(Index)));

    if Result <> '' then Result:=Result + ':';
    Result:=Result + AddChar('0', IntToStr(v), 2);
  end;
end;

constructor TPlayerGPX.Create(const ATripID: Integer;
  const AGpxFileName: String; AThread: TPlayerThreadManager);
begin
  inherited Create;

  FFileName:=AGpxFileName;
  FThread:=AThread;
  FLines:=TStringList.Create;
  FTripID:=ATripID;

  AssignFile(FFile, FFileName);
  Rewrite(FFile);

  Query:=TSQLQuery.Create(Application);
  Query.DataBase:=dmPlayer.Connection;
  Query.Transaction:=dmPlayer.Transaction;
end;

destructor TPlayerGPX.Destroy;
begin
  CloseFile(FFile);
  FLines.Free;
  Query.Free;
  inherited;
end;

procedure TPlayerGPX.Convert;
var
  Index: Integer;
begin
  logger.Log('creating gpx for trip %d', [FTripId]);

  Query.SQL.Clear;
  Query.Clear;

  LoadTextFromResource(Query.SQL, 'GPX_INFO');
  Query.ParamByName('trip_id').AsInteger:=FTripId;

  logger.Log('gpx info for trip %d', [FTripId]);
  Query.Open;
  try
    FLines.Add('<?xml version="1.0" encoding="UTF-8" ?>');
    FLines.Add('<gpx version="1.0"');
    FLines.Add(Format('     creator="%s"', [Application.Title]));
    FLines.Add('     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"');
    FLines.Add('     xmlns="http://www.topografix.com/GPX/1/0"');
    FLines.Add('     xsi:schemaLocation="http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd">');
    FLines.Add(Format('<time>%s</time>',
      [FormatGpxTime(Query.FieldByName('started_at').AsString)]));
    FLines.Add(Format('<bounds minlat="%.6n" minlon="%.6n" maxlat="%.6n" maxlon="%.6n"/>',
      [
        Query.FieldByName('minlat').AsFloat, Query.FieldByName('minlon').AsFloat,
        Query.FieldByName('maxlat').AsFloat, Query.FieldByName('maxlon').AsFloat
      ]));
    FLines.Add('<trk>');
    FLines.Add(Format('  <name>Track %s</name>',
      [Query.FieldByName('started_at').AsString]));

    FLines.Add(Format('  <desc>Duration: %s. Length: %.2n km. AvgSpeed: %.2n km/h.</desc>',
      [
        ParseDuration(Query.FieldByName('duration').AsInteger),
        Query.FieldByName('distance').AsFloat / 1000,
        Query.FieldByName('avg_speed').AsFloat
      ]));
    FLines.Add(Format('  <time>%s</time>',
      [FormatGpxTime(Query.FieldByName('started_at').AsString)]));
    FLines.Add('  <trkseg>');
    AppendLines;
  finally
    Query.Close;
  end;

  if IsTerminated then Exit;

  Query.SQL.Clear;
  Query.Clear;

  LoadTextFromResource(Query.SQL, 'GPX');
  Query.ParamByName('trip_id').AsInteger:=FTripId;
  Query.Open;
  try
    Index:=0;
    while not Query.EOF do
    begin
      if IsTerminated then Break;

      logger.Log('saving point %d, trip %d', [Query.FieldByName('id').AsInteger, FTripId]);

      AddGpxPoint;
      Inc(Index);

      if Index = 1000 then
      begin
        AppendLines;
        Index:=0;
      end;

      Query.Next;
    end;
  finally
    Query.Close;
  end;

  FLines.Add('  </trkseg>');
  FLines.Add('</trk>');
  FLines.Add('</gpx>');
  AppendLines;
end;

end.
